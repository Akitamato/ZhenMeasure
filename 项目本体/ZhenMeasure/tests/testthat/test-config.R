# Unit tests for configuration management

test_that("ZhenM_default_config returns correct structure", {
  cfg_national <- ZhenM_default_config("national_standard")

  expect_equal(cfg_national$qc_method, "national_standard")

  expect_true("national_standard" %in% names(cfg_national))
})

test_that("ZhenM_default_config errors on legacy method", {
  expect_error(ZhenM_default_config("legacy"))
})

test_that("national_standard config has required parameters", {
  cfg <- ZhenM_default_config("national_standard")$national_standard

  expect_equal(cfg$weight_range, c(25, 140))
  expect_equal(cfg$feed_intake_range, c(0, 6))
  expect_equal(cfg$impute_r2_min, 0.95)
  expect_equal(cfg$min_test_days, 60)
  expect_equal(cfg$max_missing_rate, 0.15)

  expect_true(is.data.frame(cfg$fcr_ranges))
  expect_equal(nrow(cfg$fcr_ranges), 9)
})

test_that("ZhenM_merge_config merges user config", {
  user_cfg <- list(national_standard = list(weight_range = c(30, 130)))
  merged <- ZhenM_merge_config(user_cfg, "national_standard")

  expect_equal(merged$national_standard$weight_range, c(30, 130))
  expect_equal(merged$national_standard$feed_intake_range, c(0, 6))
})
