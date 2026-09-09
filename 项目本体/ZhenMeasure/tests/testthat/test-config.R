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
  expect_equal(cfg$growth_curve_r2_min, 0.95)
  expect_equal(cfg$feed_intake_range, c(0, 6))
  expect_equal(cfg$impute_r2_min, 0.95)
  expect_equal(cfg$min_test_days, 60)
  expect_equal(cfg$min_stage_days, 20)
  expect_equal(cfg$max_missing_rate, 0.15)

  expect_true(is.data.frame(cfg$fcr_ranges))
  expect_equal(nrow(cfg$fcr_ranges), 9)

  # 校正机制开关默认开启（= V1.1.1 现状行为）
  expect_true(cfg$use_record_feed_correction)
  expect_true(cfg$use_lmm_feed_correction)
})

test_that("ZhenM_merge_config merges user config", {
  user_cfg <- list(national_standard = list(weight_range = c(30, 130)))
  merged <- ZhenM_merge_config(user_cfg, "national_standard")

  expect_equal(merged$national_standard$weight_range, c(30, 130))
  expect_equal(merged$national_standard$feed_intake_range, c(0, 6))
})

test_that("ZhenM_merge_config warns on unknown keys (issue #17)", {
  # 拼写错误键：告警且路径含节名
  expect_warning(
    merged <- ZhenM_merge_config(
      list(national_standard = list(use_record_feed_corection = FALSE)),
      "national_standard"),
    "use_record_feed_corection"
  )
  # 未识别键不生效：读取处走默认值
  expect_true(merged$national_standard$use_record_feed_correction)

  # 未知顶层节同样告警
  expect_warning(
    ZhenM_merge_config(list(legacy = list(weight_threshold = 0.5)), "national_standard"),
    "legacy"
  )

  # 合法键不告警
  expect_silent(ZhenM_merge_config(
    list(national_standard = list(weight_range = c(30, 130),
                                  speed_zero_long_duration_sec = 600)),
    "national_standard"))

  # data.frame 叶子（fcr_ranges）不递归列名，不误报
  expect_silent(ZhenM_merge_config(
    list(national_standard = list(fcr_ranges = data.frame(weight_min = 30, weight_max = 40))),
    "national_standard"))

  # 嵌套路径：未知子树整体报出（不深入未知的下层）
  expect_warning(
    ZhenM_merge_config(list(national_standard = list(gompertz = list(foo = 1))), "national_standard"),
    "national_standard.gompertz"
  )
})
