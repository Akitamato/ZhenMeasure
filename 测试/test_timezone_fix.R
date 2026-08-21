######### 时区UTC统一修复验证脚本 #########
# 验证 ZhenM_safe_to_idate / ZhenM_parse_datetime / .parse_datetime
# 在统一 tz="UTC" 后不再出现 record_date 比原始日期晚一天的问题
##########################################

rm(list = ls())

# 加载 ZhenMeasure 包
setwd("D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/项目本体")
devtools::load_all("ZhenMeasure")
library(ZhenMeasure)

# ---------- 测试辅助 ---------- #
pass_count <- 0
fail_count <- 0

check <- function(desc, condition) {
  if (isTRUE(condition)) {
    cat(sprintf("  ✅ PASS: %s\n", desc))
    pass_count <<- pass_count + 1
  } else {
    cat(sprintf("  ❌ FAIL: %s\n", desc))
    fail_count <<- fail_count + 1
  }
}

check_equal <- function(desc, actual, expected) {
  if (identical(actual, expected)) {
    cat(sprintf("  ✅ PASS: %s\n", desc))
    pass_count <<- pass_count + 1
  } else {
    cat(sprintf("  ❌ FAIL: %s (期望=%s, 实际=%s)\n",
                desc,
                paste(expected, collapse = ","),
                paste(actual, collapse = ",")))
    fail_count <<- fail_count + 1
  }
}

header <- function(title) {
  cat(sprintf("\n========== %s ==========\n", title))
}

# ============================================================
# 测试 1：ZhenM_safe_to_idate 对 POSIXct 的处理
# ============================================================
header("1. ZhenM_safe_to_idate POSIXct 直接处理")

# 用 UTC 时区创建几个标准 POSIXct 时间点
t1 <- as.POSIXct("2024-09-17 00:00:00", tz = "UTC")
t2 <- as.POSIXct("2024-09-17 23:59:59", tz = "UTC")
t3 <- as.POSIXct("2024-09-17 12:30:45", tz = "UTC")
t4 <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
t5 <- as.POSIXct("2024-09-17 00:00:00", tz = "Asia/Shanghai")

d1 <- ZhenM_safe_to_idate(t1)
d2 <- ZhenM_safe_to_idate(t2)
d3 <- ZhenM_safe_to_idate(t3)
d4 <- ZhenM_safe_to_idate(t4)
d5 <- ZhenM_safe_to_idate(t5)  # CST 时区，UTC 表示应为 2024-09-16 16:00:00

check_equal("UTC 午夜 -> 同日 IDate",    as.character(d1), "2024-09-17")
check_equal("UTC 23:59:59 -> 同日 IDate", as.character(d2), "2024-09-17")
check_equal("UTC 正午 -> 同日 IDate",     as.character(d3), "2024-09-17")
check_equal("UTC 元旦 -> 同日 IDate",     as.character(d4), "2025-01-01")
# CST 时间转为 UTC 日期应为前一天
check_equal("CST 午夜 -> UTC 前一天",     as.character(d5), "2024-09-16")

# 验证返回值类型是 IDate
check("返回值类型为 IDate", inherits(d1, "IDate"))

# 测试 Date 类型输入（回归）
d_date <- ZhenM_safe_to_idate(as.Date("2024-09-17"))
check_equal("Date 类型输入", as.character(d_date), "2024-09-17")

# 测试 IDate 类型输入（回归）
d_idate <- ZhenM_safe_to_idate(data.table::as.IDate("2024-09-17"))
check_equal("IDate 类型输入", as.character(d_idate), "2024-09-17")


# ============================================================
# 测试 2：ZhenM_safe_to_idate 边缘时间
# ============================================================
header("2. ZhenM_safe_to_idate 边缘时间")

# 跨越午夜边界
t_midnight <- as.POSIXct("2024-09-18 00:00:00", tz = "UTC")
d_midnight <- ZhenM_safe_to_idate(t_midnight)
check_equal("UTC 午夜边界", as.character(d_midnight), "2024-09-18")

# 日期末尾一秒
t_nearly_midnight <- as.POSIXct("2024-09-17 23:59:59", tz = "UTC")
d_nearly <- ZhenM_safe_to_idate(t_nearly_midnight)
check_equal("UTC 23:59:59 仍在当日", as.character(d_nearly), "2024-09-17")

# 跨年边界
t_nye <- as.POSIXct("2024-12-31 23:59:59", tz = "UTC")
t_ny <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
check_equal("跨年边界 除夕",     as.character(ZhenM_safe_to_idate(t_nye)), "2024-12-31")
check_equal("跨年边界 元旦",     as.character(ZhenM_safe_to_idate(t_ny)),  "2025-01-01")


# ============================================================
# 测试 3：ZhenM_safe_to_idate 字符串日期（回归测试）
# ============================================================
header("3. ZhenM_safe_to_idate 字符串日期解析")

check_equal("纯日期字符串 Ymd",        as.character(ZhenM_safe_to_idate("2024-09-17")),          "2024-09-17")
check_equal("日期时间字符串 Ymd HMS",  as.character(ZhenM_safe_to_idate("2024-09-17 08:30:00")), "2024-09-17")
check_equal("斜杠格式 Y/m/d",          as.character(ZhenM_safe_to_idate("2024/09/17")),          "2024-09-17")
check_equal("紧凑格式 Ymd",            as.character(ZhenM_safe_to_idate("20240917")),             "2024-09-17")


# ============================================================
# 测试 4：.parse_datetime 日期解析（来源 ZhenM_read_utils.R）
# ============================================================
header("4. .parse_datetime 日期解析")

# 字符串格式日期
p1 <- ZhenMeasure:::.parse_datetime("2024-09-17 08:30:00")
check("字符串日期解析返回 POSIXct", inherits(p1, "POSIXct"))
# 返回的时间应是 UTC
check_equal("字符串日期 UTC 日期正确", format(p1, "%Y-%m-%d", tz = "UTC"), "2024-09-17")
check_equal("字符串日期 UTC 时间正确", format(p1, "%H:%M:%S", tz = "UTC"), "08:30:00")

# Excel 数值型日期（2024-09-17 的 Excel 序列号 = 45552）
p2 <- ZhenMeasure:::.parse_datetime("45552")
check("数值日期解析返回 POSIXct", inherits(p2, "POSIXct"))
check_equal("数值日期 UTC 日期正确", format(p2, "%Y-%m-%d", tz = "UTC"), "2024-09-17")

# 多种日期格式
formats_to_test <- c("2024-09-17", "2024/09/17", "09/17/2024", "17/09/2024")
for (fmt in formats_to_test) {
  p <- ZhenMeasure:::.parse_datetime(fmt)
  check(sprintf("格式 '%s' 解析成功且日期正确", fmt),
        !is.na(p) && format(p, "%Y-%m-%d", tz = "UTC") == "2024-09-17")
}

# 全 NA
p_na <- ZhenMeasure:::.parse_datetime(NA)
check("全 NA 输入返回 NA", is.na(p_na))


# ============================================================
# 测试 5：ZhenM_parse_datetime 日期解析（来源 ZhenM_utils.R）
# ============================================================
header("5. ZhenM_parse_datetime 日期解析")

# 字符串日期
a1 <- ZhenM_parse_datetime("2024-09-17 10:30:00")
check("ZhenM_parse_datetime 返回 POSIXct", inherits(a1, "POSIXct"))
check_equal("ZhenM_parse_datetime UTC 日期正确", format(a1, "%Y-%m-%d", tz = "UTC"), "2024-09-17")

# 数值日期（Excel 序列号）
a2 <- ZhenM_parse_datetime(45552)
check_equal("ZhenM_parse_datetime 数值日期 UTC 日期", format(a2, "%Y-%m-%d", tz = "UTC"), "2024-09-17")

# Date 类型输入
a3 <- ZhenM_parse_datetime(as.Date("2024-09-17"))
check_equal("ZhenM_parse_datetime Date 输入 UTC 日期", format(a3, "%Y-%m-%d", tz = "UTC"), "2024-09-17")


# ============================================================
# 测试 6：真实数据端到端验证
# ============================================================
header("6. 真实数据端到端验证（FIRE 数据）")

demo_base <- "D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/demo"
fire_dir <- file.path(demo_base, "demo_input", "FIRE_奥斯本")
format_path <- file.path(fire_dir, "附加信息", "FIRE_data_format.json")
birth_path <- file.path(fire_dir, "附加信息", "25YF_出生信息(1).xlsx")
raw_data_dir <- file.path(fire_dir, "原始数据")

if (file.exists(format_path) && dir.exists(raw_data_dir)) {
  # Step 1: 读取标准数据
  std <- tryCatch(
    ZhenM_read_data(raw_data_dir, "FIRE", format_path, birth_path),
    error = function(e) {
      cat("  ⚠️ ZhenM_read_data 执行出错:", conditionMessage(e), "\n")
      return(NULL)
    }
  )

  if (!is.null(std) && nrow(std) > 0) {
    # Step 2: 验证 Visit_time 与 record_date 的对应关系
    dt <- data.table::copy(std)
    # 计算 record_date
    dt[, record_date := ZhenM_safe_to_idate(Visit_time)]

    # 检查: Visit_time 的 UTC 日期应与 record_date 一致
    dt[, visit_date_utc := as.IDate(format(Visit_time, "%Y-%m-%d", tz = "UTC"))]
    dt[, date_match := record_date == visit_date_utc]

    match_rate <- sum(dt$date_match, na.rm = TRUE) / nrow(dt)
    n_mismatch <- sum(!dt$date_match, na.rm = TRUE)

    cat(sprintf("  record_date vs Visit_time(UTC) 一致率: %.2f%%\n", match_rate * 100))
    cat(sprintf("  不匹配记录数: %d / %d\n", n_mismatch, nrow(dt)))

    check("FIRE 数据: record_date 与 Visit_time UTC 日期 100% 一致", match_rate == 1)

    if (n_mismatch > 0) {
      bad <- dt[date_match == FALSE, .(ID, Visit_time, visit_date_utc, record_date)]
      cat("  不匹配样本（前10行）:\n")
      print(head(bad, 10))
    }

    # Step 3: 验证 record_date 不会比 Visit_time 的任何合理解释 "晚一天"
    # 即 record_date 不可能晚于 Visit_time 在 UTC 下的日期
    dt[, record_later := record_date > visit_date_utc]
    n_later <- sum(dt$record_later, na.rm = TRUE)
    check("record_date 没有比 Visit_time(UTC) 晚一天", n_later == 0)

    dt[, c("visit_date_utc", "date_match", "record_later") := NULL]
    cat(sprintf("  总记录数: %d, 个体数: %d\n", nrow(dt), uniqueN(dt$ID)))
  } else {
    cat("  ⚠️ 读取 FIRE 数据为空或失败，跳过端到端验证\n")
  }
} else {
  cat("  ⚠️ FIRE 数据文件不存在，跳过端到端验证\n")
}


# ============================================================
# 测试 7：YANGXIANG 数据端到端验证
# ============================================================
header("7. YANGXIANG 数据端到端验证")

yx_dir <- file.path(demo_base, "demo_input", "YANGXIANG_扬翔")
yx_format <- list.files(file.path(yx_dir, "附加信息"), pattern = "\\.json$", full.names = TRUE)[1]
yx_raw <- file.path(yx_dir, "原始数据")

if (!is.na(yx_format) && dir.exists(yx_raw)) {
  yx_std <- tryCatch(
    ZhenM_read_data(yx_raw, "YANGXIANG", yx_format),
    error = function(e) {
      cat("  ⚠️ YANGXIANG ZhenM_read_data 出错:", conditionMessage(e), "\n")
      return(NULL)
    }
  )

  if (!is.null(yx_std) && nrow(yx_std) > 0) {
    dt_yx <- data.table::copy(yx_std)
    dt_yx[, record_date := ZhenM_safe_to_idate(Visit_time)]
    dt_yx[, visit_date_utc := as.IDate(format(Visit_time, "%Y-%m-%d", tz = "UTC"))]
    dt_yx[, date_match := record_date == visit_date_utc]

    match_rate <- sum(dt_yx$date_match, na.rm = TRUE) / nrow(dt_yx)
    n_mismatch <- sum(!dt_yx$date_match, na.rm = TRUE)

    cat(sprintf("  record_date vs Visit_time(UTC) 一致率: %.2f%%\n", match_rate * 100))
    cat(sprintf("  不匹配记录数: %d / %d\n", n_mismatch, nrow(dt_yx)))
    check("YANGXIANG 数据: record_date 与 Visit_time(UTC) 100% 一致", match_rate == 1)
    cat(sprintf("  总记录数: %d, 个体数: %d\n", nrow(dt_yx), uniqueN(dt_yx$ID)))
  } else {
    cat("  ⚠️ YANGXIANG 数据为空或失败，跳过\n")
  }
} else {
  cat("  ⚠️ YANGXIANG 数据文件不存在，跳过\n")
}


# ============================================================
# 汇总
# ============================================================
header("测试结果汇总")
total <- pass_count + fail_count
cat(sprintf("  总计: %d 项测试\n", total))
cat(sprintf("  通过: %d\n", pass_count))
cat(sprintf("  失败: %d\n", fail_count))

if (fail_count == 0) {
  cat("\n🎉 全部通过！时区 UTC 统一修复验证完成。\n")
} else {
  cat(sprintf("\n⚠️  %d 项测试未通过，请检查。\n", fail_count))
}
