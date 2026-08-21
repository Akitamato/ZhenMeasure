# Unit tests for QC functions

test_that("ZhenM_qc_weight_standard runs national_standard method", {
  skip_if_not_installed("data.table")

  # Create standard_data format test data
  dt <- data.table::data.table(
    animal_id = rep("A001", 30),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10), each = 3),
    weight_g = rep(seq(30000, 50000, length.out = 10), each = 3) + rnorm(30, 0, 500),
    device_type = "YANGXIANG"
  )

  result_national <- ZhenM_qc_weight_standard(dt, "national_standard")

  expect_true("is_outlier_wt" %in% names(result_national))
  expect_true("flag_weight_low" %in% names(result_national))
})

test_that("ZhenM_qc_feed_standard identifies anomalies", {
  skip_if_not_installed("data.table")

  # Create standard_data format test data
  dt <- data.table::data.table(
    animal_id = rep("A001", 30),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10), each = 3),
    feed_g = c(rep(c(300, 400, 350), 9), -50, 200, 8000),  # Contains negative and extreme high values
    duration_sec = rep(c(300, 400, 350), 10),
    weight_g = rep(seq(30000, 50000, length.out = 10), each = 3),
    device_type = "YANGXIANG"
  )

  result <- ZhenM_qc_feed_standard(dt, "national_standard")

  expect_true("flag_feed_negative" %in% names(result))
  expect_true("is_outlier_feed" %in% names(result))
  expect_true(any(result$flag_feed_negative))
})

test_that("ZhenM_standard_to_daily_filtered excludes outliers", {
  skip_if_not_installed("data.table")

  # Create standard_data with QC flags
  dt <- data.table::data.table(
    animal_id = rep("A001", 10),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 5), each = 2),
    feed_g = c(300, 400, 350, 9000, 320, 380, 310, 390, 330, 370),  # 4th is outlier
    weight_g = c(30000, 30100, 32000, 32100, 34000, 34100, 36000, 36100, 38000, 38100),
    duration_sec = rep(300, 10),
    is_outlier_feed = c(rep(FALSE, 3), TRUE, rep(FALSE, 6)),
    is_outlier_wt = rep(FALSE, 10),
    device_type = "YANGXIANG",
    age_day = rep(100:104, each = 2),
    measurement_day = rep(1:5, each = 2),
    source_file = "test.csv",
    daily_feed_g = NA_real_
  )

  result <- ZhenM_standard_to_daily_filtered(dt)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)  # 5 days of data
  expect_true("n_outlier_feed" %in% names(result))

  # Check day 2 feed (should exclude outlier, only 350 remains)
  day2_feed <- result[record_date == as.Date("2024-01-02"), daily_feed_g]
  expect_equal(day2_feed, 350)
})

test_that("ZhenM_generate_qc_summary produces summary", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    flag_out_of_range = c(TRUE, FALSE, TRUE, FALSE),
    flag_weight_low = c(FALSE, TRUE, FALSE, FALSE)
  )

  summary <- ZhenM_generate_qc_summary(dt)

  expect_true(is.data.frame(summary))
  expect_true("error_type" %in% names(summary))
  expect_true("count" %in% names(summary))
  expect_equal(nrow(summary), 2)
})
