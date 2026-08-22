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
