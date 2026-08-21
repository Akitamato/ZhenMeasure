# Unit tests for imputation functions

test_that("ZhenM_impute_weight fills missing values", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("zoo")
  skip_if_not_installed("imputeTS")

  dt <- data.table::data.table(
    animal_id = rep("A001", 10),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10),
    daily_weight_g = c(30000, 32000, NA, 36000, NA, 40000, 42000, NA, 46000, 48000)
  )

  result <- ZhenMeasure:::.impute_weight_national(dt, ZhenM_default_config("national_standard"))

  expect_true("is_imputed_wt" %in% names(result))
  expect_true(sum(is.na(result$daily_weight_g)) < sum(is.na(dt$daily_weight_g)))
})

test_that("ZhenM_impute_feed handles discrete missing", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("zoo")

  dt <- data.table::data.table(
    animal_id = rep("A001", 10),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10),
    daily_feed_g = c(1000, NA, 1200, 1300, NA, 1500, 1600, 1700, NA, 1900)
  )

  result <- ZhenMeasure:::.impute_feed_national_v2(dt, ZhenM_default_config("national_standard"))

  expect_true("is_imputed_feed" %in% names(result))
})
