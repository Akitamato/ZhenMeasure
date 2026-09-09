test_that("standard_fcr calculates correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 100),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 100),
    daily_weight_g = seq(30000, 120000, length.out = 100),
    daily_feed_g = rep(2000, 100)
  )

  result <- ZhenM_calc_phenotypes(dt, "standard_fcr")

  expect_true("FCR_30_120kg" %in% names(result))
  expect_true("ADG_g" %in% names(result))
  expect_true("ADFI_g" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("report mode calculates stage average", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 50),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 50),
    daily_weight_g = seq(30000, 80000, length.out = 50),
    daily_feed_g = rep(2000, 50)
  )

  result <- ZhenM_calc_phenotypes(dt, "report")

  expect_true("FCR" %in% names(result))
  expect_true("ADG_g" %in% names(result))
})

test_that("monitor mode produces daily output", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("zoo")

  dt <- data.table::data.table(
    animal_id = rep("A001", 20),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 20),
    daily_weight_g = seq(30000, 50000, length.out = 20),
    daily_feed_g = rep(2000, 20)
  )

  result <- ZhenM_calc_phenotypes(dt, "monitor")

  expect_true("ADG_rolling_mean_g" %in% names(result))
  expect_true("FCR_rolling_mean" %in% names(result))
  expect_true(nrow(result) >= 1)
})

test_that("research mode includes statistics", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 50),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 50),
    daily_weight_g = seq(30000, 80000, length.out = 50),
    daily_feed_g = rep(2000, 50)
  )

  result <- ZhenM_calc_phenotypes(dt, "research")

  expect_true("ADG_r2" %in% names(result))
  expect_true("FCR_lm" %in% names(result))
  expect_true("ADG_pval" %in% names(result))
})

test_that("weight stage mode partitions data correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 100),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 100),
    daily_weight_g = seq(30000, 120000, length.out = 100),
    daily_feed_g = rep(2000, 100)
  )

  result <- ZhenM_calc_phenotypes(
    dt,
    phenotype_method = "standard_fcr",
    stage_mode = "weight",
    target_weight_stages = c(30, 60, 90, 120)
  )

  expect_true("stage_label" %in% names(result))
  expect_true(nrow(result) >= 3)  # Should have at least 3 stages
})

test_that("weight stage boundaries are left-closed right-open, no double counting (issue #27)", {
  skip_if_not_installed("data.table")

  # 50/60/80/100kg 各一天，60kg 恰为相邻阶段 30-60 / 60-90 的边界
  # （standard_fcr 内层按 test_weight_range 默认 45-110kg 过滤，故下界用 50kg）
  dt <- data.table::data.table(
    animal_id = "A001",
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 4),
    daily_weight_g = c(50000, 60000, 80000, 100000),
    daily_feed_g = rep(2000, 4)
  )

  result <- ZhenM_calc_phenotypes(
    dt,
    phenotype_method = "standard_fcr",
    stage_mode = "weight",
    target_weight_stages = c(30, 60, 90, 120)
  )
  days <- stats::setNames(result$test_days, result$stage_label)

  # 右开：60kg 归入 60-90kg，不重复落入 30-60kg
  expect_equal(unname(days["30-60kg"]), 1)
  expect_equal(unname(days["60-90kg"]), 2)
  expect_equal(unname(days["90-120kg"]), 1)
  # 四个观测日恰好被分配一次，无重复计数
  expect_equal(sum(result$test_days), 4)
})

test_that("age stage mode requires age_days column", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 100),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 100),
    daily_weight_g = seq(30000, 120000, length.out = 100),
    daily_feed_g = rep(2000, 100),
    age_days = seq(70, 169, length.out = 100)
  )

  result <- ZhenM_calc_phenotypes(
    dt,
    phenotype_method = "report",
    stage_mode = "age",
    target_age_stages = c(70, 100, 130, 160)
  )

  expect_true("stage_label" %in% names(result))
  expect_true(nrow(result) >= 3)  # Should have at least 3 stages
})

test_that("date stage mode partitions by dates", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 100),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 100),
    daily_weight_g = seq(30000, 120000, length.out = 100),
    daily_feed_g = rep(2000, 100)
  )

  result <- ZhenM_calc_phenotypes(
    dt,
    phenotype_method = "report",
    stage_mode = "date",
    target_date_stages = c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01")
  )

  expect_true("stage_label" %in% names(result))
  expect_true("stage_start_date" %in% names(result))
  expect_true("stage_end_date" %in% names(result))
  expect_true(nrow(result) >= 3)  # Should have at least 3 stages
})

test_that("report mode adds FCR stage QC columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 50),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 50),
    daily_weight_g = seq(30000, 80000, length.out = 50),
    daily_feed_g = rep(2000, 50)
  )

  result <- ZhenM_calc_phenotypes(dt, "report")

  expect_true("flag_fcr_stage_invalid" %in% names(result))
  expect_true("n_stages" %in% names(result))
  expect_true("n_valid_stages" %in% names(result))
})

test_that("stage test_days filtering drops short stages", {
  skip_if_not_installed("data.table")

  # 体重在 10 天内从 30kg 涨到 100kg，30-100/115/120 阶段 test_days=10 < 20，应全部被过滤
  dt <- data.table::data.table(
    animal_id = rep("A001", 10),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10),
    daily_weight_g = seq(30000, 100000, length.out = 10),
    daily_feed_g = rep(2000, 10)
  )

  result <- ZhenM_calc_phenotypes(
    dt,
    phenotype_method = "report",
    stage_mode = "weight",
    target_weight_stages = "YANGXIANG"
  )

  expect_true(nrow(result) == 0)
})

test_that("monitor mode guards FCR_rolling_mean against zero ADG (issue #11)", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("zoo")

  dt <- data.table::data.table(
    animal_id = rep("A001", 20),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 20),
    daily_weight_g = rep(50000, 20),   # 体重完全平坦 → ADG 滚动均值 = 0
    daily_feed_g = rep(2000, 20)
  )

  result <- ZhenM_calc_phenotypes(dt, "monitor")

  # 修复前 2000/0 → Inf；守卫后应全部为 NA
  expect_true(all(is.na(result$FCR_rolling_mean)))
  expect_false(any(is.infinite(result$FCR_rolling_mean)))
})

test_that("research mode guards FCR_lm against zero ADG (issue #11)", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 50),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 50),
    daily_weight_g = rep(50000, 50),   # 平坦 → lm 斜率 ADG_g_lm = 0
    daily_feed_g = rep(2000, 50)
  )

  result <- ZhenM_calc_phenotypes(dt, "research")

  expect_true(all(is.na(result$FCR_lm)))
  expect_false(any(is.infinite(result$FCR_lm)))
})

test_that("weight-stage ADFI is NA (not NaN) when stage feed is all NA (issue #11)", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 100),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 100),
    daily_weight_g = seq(30000, 120000, length.out = 100),
    daily_feed_g = NA_real_   # 全程无有效采食
  )

  result <- ZhenM_calc_phenotypes(
    dt,
    phenotype_method = "standard_fcr",
    stage_mode = "weight",
    target_weight_stages = c(30, 60, 90, 120)
  )

  # 修复前：阶段内 mean(na.rm=TRUE) 对全 NA 得 NaN 进结果表
  expect_true(nrow(result) >= 1)
  expect_false(any(sapply(result, function(col) any(is.nan(col)))))
})

test_that("全 NA 体重时阶段解析返回空且不告警（issue #19）", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = "A001",
    record_date = as.Date("2024-01-01") + 0:9,
    daily_feed_g = 1000,
    median_weight_g = NA_real_
  )

  # 修复前：min/max(na.rm=TRUE) 得 ±Inf 并告警，生成 "Inf--Inf kg" 非法阶段
  expect_no_warning(
    result <- ZhenM_calc_phenotypes_stage(
      dt, stage_mode = "weight", target_weight_stages = FALSE)
  )
  expect_true("animal_id" %in% names(result))
  # 无任何阶段列
  expect_equal(setdiff(names(result), "animal_id"), character(0))
})

test_that("日期有缺口时 ADG 以日历天数为分母（issue #19）", {
  skip_if_not_installed("data.table")

  # 4 条记录跨 5 个日历天，增重 9000g
  dt <- data.table::data.table(
    animal_id = "A001",
    record_date = as.Date("2024-01-01") + c(0, 1, 4, 5),
    daily_feed_g = 3000,
    median_weight_g = c(30000, 33000, 36000, 39000)
  )

  result <- ZhenM_calc_phenotypes_stage(
    dt, stage_mode = "weight",
    target_weight_stages = list("30-40kg" = c(30000, 40000)))

  # 修复前：record_index 口径 → 9000/3 = 3000 g/天（高估）
  expect_equal(result[["30-40kg_ADG"]], 1800)
})

test_that("无 age_day 时以 measurement_day 为时间轴算阶段 ADG（issue #23）", {
  skip_if_not_installed("data.table")

  # 100 天，逐日增重 900 g；无 age_day，只有 measurement_day
  dt <- data.table::data.table(
    animal_id = "A001",
    record_date = as.Date("2024-01-01") + 0:99,
    daily_feed_g = 2000,
    median_weight_g = 30000 + 900 * (0:99),
    age_day = NA_real_,
    measurement_day = 1:100
  )

  res <- ZhenM_calc_phenotypes_stage(
    dt, stage_mode = "weight",
    target_weight_stages = list("30-40kg" = c(30000, 40000)))

  # 新口径（逐记录天数索引）→ 时间差 11 天，ADG = 9900/11 = 900
  expect_equal(res[["30-40kg_ADG"]], 900)

  # 旧口径（每头常数「首末日跨度」）时间差恒为 0，ADG 退化为 NA
  dt_const <- data.table::copy(dt)[, measurement_day := 99]
  res_const <- ZhenM_calc_phenotypes_stage(
    dt_const, stage_mode = "weight",
    target_weight_stages = list("30-40kg" = c(30000, 40000)))
  expect_true(is.na(res_const[["30-40kg_ADG"]]))
})
