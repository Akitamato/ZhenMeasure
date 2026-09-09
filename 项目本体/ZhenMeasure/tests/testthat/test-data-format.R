test_that("ZhenM_parse_data_format parses positions and labels", {
  format_file <- tempfile(fileext = ".txt")
  lines <- c(
    "Data format\tColumn position\tEnglish label (optional)",
    "个体号（ID）\t6\tanimal_id",
    "字符型(Character)\t2,3\tlocation,pen",
    "数值型(Numeric)\t10,13,18\tweight_g,feed_g,daily_feed_g",
    "日期型(Date)\t1,16\trecord_date,start_time"
  )
  writeLines(lines, format_file, useBytes = TRUE)

  parsed <- ZhenM_parse_data_format(format_file)

  expect_equal(parsed$id_col_pos, 6L)
  expect_equal(parsed$character_col_pos, c(2L, 3L))
  expect_equal(parsed$numeric_col_labels, c("weight_g", "feed_g", "daily_feed_g"))
  expect_equal(parsed$date_col_labels, c("record_date", "start_time"))
})

test_that("ZhenM_parse_data_format rejects duplicated positions", {
  format_file <- tempfile(fileext = ".txt")
  lines <- c(
    "Data format\tColumn position",
    "个体号（ID）\t6",
    "字符型(Character)\t2,6",
    "数值型(Numeric)\t10",
    "日期型(Date)\t1"
  )
  writeLines(lines, format_file, useBytes = TRUE)

  expect_error(ZhenM_parse_data_format(format_file), "Duplicate column positions found")
})

test_that(".parse_datetime handles compact Ymd dates correctly (issue #9)", {
  skip_if_not_installed("lubridate")

  # 紧凑日期不得被误判为 Excel 序列号（修复前 20240101 → 约 57355 年）
  res <- ZhenMeasure:::.parse_datetime(c("20240101", "20240229", "20241231"))
  expect_equal(as.Date(res), as.Date(c("2024-01-01", "2024-02-29", "2024-12-31")))

  # 紧凑日期时间
  res2 <- ZhenMeasure:::.parse_datetime("20240101 083000")
  expect_equal(format(res2, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2024-01-01 08:30:00")
})

test_that(".parse_datetime still parses Excel serial numbers in plausible range", {
  skip_if_not_installed("lubridate")

  # Excel 序列号路径保留（期望值用 origin="1899-12-30" 规范换算，避免手算错）
  res <- ZhenMeasure:::.parse_datetime(c("45300", "45000.5"))
  expect_equal(as.Date(res[1]), as.Date(45300, origin = "1899-12-30"))
  # 小数序列号直接比较 POSIXct（as.Date 会截断整天，口径不一致）
  expect_equal(res[2], as.POSIXct((45000.5 - 25569) * 86400, origin = "1970-01-01", tz = "UTC"))

  # 混合列：文本日期 + 序列号各自正确
  res2 <- ZhenMeasure:::.parse_datetime(c("2024-01-01 06:00:00", "45300"))
  expect_equal(as.Date(res2[1]), as.Date("2024-01-01"))
  expect_equal(as.Date(res2[2]), as.Date(45300, origin = "1899-12-30"))
})

test_that(".parse_datetime leaves implausible numbers as NA (not garbage dates)", {
  skip_if_not_installed("lubridate")

  # 远超序列号合理区间的纯数字不得产出荒谬日期
  res <- ZhenMeasure:::.parse_datetime(c("20240101", "99999999"))
  expect_false(is.na(res[1]))
  expect_true(is.na(res[2]))
})

test_that("三套日期解析器统一到 .parse_temporal（issue #20）", {
  skip_if_not_installed("lubridate")
  skip_if_not_installed("data.table")

  x <- c("2024-01-01", "20240101", "2024/01/01 08:00:00", "45300",
         "01/02/2024", "abc", NA)

  # 三者对同一批文本输入结果一致（仅返回类型不同：IDate vs POSIXct）
  expect_identical(
    ZhenMeasure:::ZhenM_safe_to_idate(x),
    data.table::as.IDate(ZhenMeasure:::.parse_datetime(x), tz = "UTC")
  )
  expect_identical(ZhenMeasure:::ZhenM_parse_datetime(x), ZhenMeasure:::.parse_datetime(x))

  # dmy/mdy 文本三者一致识别（修复前只有 .parse_datetime 识别，另两者返回 NA）
  expect_identical(as.character(ZhenMeasure:::ZhenM_safe_to_idate("01/02/2024")), "2024-02-01")
  expect_identical(as.character(ZhenMeasure:::ZhenM_parse_datetime("13/02/2024")), "2024-02-13")

  # Excel 序列号区间守卫对三者一致生效（修复前 safe/parse_datetime 无守卫）
  expect_true(is.na(ZhenMeasure:::ZhenM_safe_to_idate("99999999")))
  expect_true(is.na(ZhenMeasure:::ZhenM_parse_datetime("99999999")))
  expect_equal(as.Date(ZhenMeasure:::ZhenM_safe_to_idate("45300")),
               as.Date(45300, origin = "1899-12-30"))

  # 全 NA / 空向量保持长度（修复前 .parse_datetime 全 NA 返回长度 1）
  expect_length(ZhenMeasure:::.parse_datetime(c(NA, NA)), 2)
  expect_length(ZhenMeasure:::.parse_datetime(character(0)), 0)
  expect_length(ZhenMeasure:::ZhenM_safe_to_idate(c(NA_character_, NA_character_)), 2)
})

test_that(".col_names_by_pos drops invalid positions and warns (issue #15)", {
  nm <- c("a", "b", "c")
  expect_identical(ZhenMeasure:::.col_names_by_pos(nm, c(1, 3)), c("a", "c"))
  expect_identical(ZhenMeasure:::.col_names_by_pos(nm, 2), "b")
  # 0 / 负数 / 越界 / NA 位置：丢弃并告警，不引入 NA 元素
  expect_warning(got <- ZhenMeasure:::.col_names_by_pos(nm, c(1, 5, 0, -1, NA_integer_)))
  expect_identical(got, "a")
  # 全部无效 → NULL（调用方 is.null()/length() 守卫自然走降级分支）
  expect_null(suppressWarnings(ZhenMeasure:::.col_names_by_pos(nm, c(0, 9))))
  expect_null(ZhenMeasure:::.col_names_by_pos(nm, integer(0)))
})

test_that(".map_daily_values_to_records aligns with unsorted idx (issue #15)", {
  skip_if_not_installed("data.table")

  # 动物记录乱序（日期 3,1,2）——旧实现 setkey 重排后按原 idx 写回会静默错位
  dt <- data.table::data.table(
    animal_id = c("A", "A", "A"),
    record_date = as.Date(c("2024-01-03", "2024-01-01", "2024-01-02")),
    val = NA_real_
  )
  daily <- data.table::data.table(
    record_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    daily_value = c(10, 20, 30)
  )
  out <- ZhenMeasure:::.map_daily_values_to_records(
    dt, which(dt$animal_id == "A"), daily, "daily_value", "val")
  expect_identical(as.numeric(out$val), c(30, 10, 20))

  # 日期不在映射表中 → NA；映射表带多余日期 → 不产生额外行
  dt2 <- data.table::data.table(
    record_date = as.Date(c("2024-01-01", "2024-02-01")), val = NA_real_)
  daily2 <- data.table::data.table(
    record_date = as.Date(c("2024-01-01", "2024-03-01")), daily_value = c(10, 99))
  out2 <- ZhenMeasure:::.map_daily_values_to_records(
    dt2, seq_len(nrow(dt2)), daily2, "daily_value", "val")
  expect_identical(as.numeric(out2$val), c(10, NA))
  expect_identical(nrow(out2), 2L)
})

test_that(".format_header_skip reads optional header_skip from format info (issue #16)", {
  # 缺省：format 文件未提供 → 默认值（扬翔 1 / FIRE-NEDAP 2 的历史行为）
  expect_identical(ZhenMeasure:::.format_header_skip(list(), default = 1), 1L)
  expect_identical(ZhenMeasure:::.format_header_skip(list(header_skip = NULL), default = 2), 2L)
  # 覆盖：format 文件提供 header_skip
  expect_identical(ZhenMeasure:::.format_header_skip(list(header_skip = 3), default = 2), 3L)
  expect_identical(ZhenMeasure:::.format_header_skip(list(header_skip = "0"), default = 1), 0L)
  # 无效：告警并回退默认
  expect_warning(got <- ZhenMeasure:::.format_header_skip(list(header_skip = -1), default = 2))
  expect_identical(got, 2L)
  expect_warning(got2 <- ZhenMeasure:::.format_header_skip(list(header_skip = "abc"), default = 1))
  expect_identical(got2, 1L)
})

test_that("jsonlite 声明为 Imports 而非 Suggests（issue #18）", {
  # .json 是唯一受支持的 format 格式（.read_shared_data_format_file 直接拒绝其他后缀），
  # 读取路径硬依赖 jsonlite；若退回 Suggests，干净环境 install 后包能加载但读取必失败
  desc_path <- system.file("DESCRIPTION", package = "ZhenMeasure")
  skip_if(desc_path == "", "无法定位 DESCRIPTION")
  desc <- read.dcf(desc_path)
  imports <- if ("Imports" %in% colnames(desc)) desc[1, "Imports"] else ""
  suggests <- if ("Suggests" %in% colnames(desc)) desc[1, "Suggests"] else ""
  expect_true(grepl("jsonlite", imports, fixed = TRUE))
  expect_false(grepl("jsonlite", suggests, fixed = TRUE))
})

test_that("ZhenM_standard_to_daily 不把缺失 Location 拼成字面 NA（issue #19）", {
  skip_if_not_installed("data.table")

  std <- data.table::data.table(
    ID = c("A001", "A001", "A001"),
    Visit_time = as.POSIXct(c("2024-01-01 08:00:00", "2024-01-01 09:00:00",
                              "2024-01-01 10:00:00"), tz = "UTC"),
    Weight = c(30000, 31000, 32000), Feed_intake = c(500, 600, 700),
    Duration = 300, AGE = 100,
    Location = c("A", NA, "B")
  )
  daily <- ZhenM_standard_to_daily(std)
  expect_identical(daily$location, "A;B")

  # 全 NA → 真 NA（修复前为字面字符串 "NA"）
  std$Location <- NA_character_
  daily2 <- ZhenM_standard_to_daily(std)
  expect_true(is.na(daily2$location))
})

test_that("ZhenM_data_summary 全 NA 时返回 NA 区间而非 Inf（issue #19）", {
  skip_if_not_installed("data.table")

  std <- data.table::data.table(
    ID = "A001", Visit_time = as.POSIXct(NA),
    Weight = NA_real_, Feed_intake = NA_real_, Duration = NA_real_,
    AGE = NA_real_, Location = NA_character_
  )
  res <- ZhenM_data_summary(std)
  for (nm in c("date_range", "weight_range_kg", "feed_range_g", "age_range")) {
    expect_true(all(is.na(res[[nm]])), info = nm)
    expect_false(any(is.infinite(res[[nm]])), info = nm)
  }
})
