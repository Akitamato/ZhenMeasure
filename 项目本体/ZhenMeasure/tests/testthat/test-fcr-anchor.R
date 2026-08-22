# Unit tests for FCR anchor correction (.correct_feed_with_fcr_anchor)

test_that("FCR anchor caps high and low daily feed", {
  skip_if_not_installed("data.table")

  # 10 天，体重 30kg→39kg 每天增 1000g，ADG=1000 g/天
  dt <- data.table::data.table(
    animal_id = rep("A001", 10),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10),
    daily_weight_g = seq(30000, 39000, length.out = 10),
    daily_feed_g = c(2000, 2000, 10000, 2000, 2000, 100, 2000, 2000, 2000, 2000)
  )
  config <- ZhenM_default_config("national_standard")
  config$national_standard$use_fcr_anchor <- TRUE

  result <- .correct_feed_with_fcr_anchor(dt, config)

  # 30-40kg 阶段: fcr_min=0.76, fcr_max=3.16；ADG=1000
  upper_cap <- 1000 * 3.16 * 1.5   # 4740
  lower_cap <- 1000 * 0.76 * 0.5   # 380

  # 第3天 feed=10000 > 4740 → cap 到 4740
  expect_equal(result$daily_feed_g[3], upper_cap)
  # 第6天 feed=100 < 380 → cap 到 380
  expect_equal(result$daily_feed_g[6], lower_cap)
  # 正常天 feed=2000 不变
  expect_equal(result$daily_feed_g[2], 2000)
  # flag 正确标记
  expect_true(result$flag_feed_fcr_corrected[3])
  expect_true(result$flag_feed_fcr_corrected[6])
  expect_false(result$flag_feed_fcr_corrected[2])
})

test_that("FCR anchor skips first day and non-positive ADG", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 3),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 3),
    daily_weight_g = c(30000, 31000, 30000),  # 第3天掉重
    daily_feed_g = c(10000, 10000, 10000)
  )
  config <- ZhenM_default_config("national_standard")
  config$national_standard$use_fcr_anchor <- TRUE

  result <- .correct_feed_with_fcr_anchor(dt, config)

  # 首日无前值，跳过
  expect_equal(result$daily_feed_g[1], 10000)
  # 第3天 ADG<0（掉重），跳过
  expect_equal(result$daily_feed_g[3], 10000)
  # 第2天 ADG>0，应被 cap
  expect_lt(result$daily_feed_g[2], 10000)
  expect_true(result$flag_feed_fcr_corrected[2])
})

test_that("FCR anchor skips out-of-range weight", {
  skip_if_not_installed("data.table")

  # 体重从 28kg 起（<30kg 阶段无对应 FCR 范围）
  dt <- data.table::data.table(
    animal_id = rep("A001", 3),
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 3),
    daily_weight_g = c(28000, 28500, 29000),  # 均 <30kg
    daily_feed_g = c(10000, 10000, 10000)
  )
  config <- ZhenM_default_config("national_standard")
  config$national_standard$use_fcr_anchor <- TRUE

  result <- .correct_feed_with_fcr_anchor(dt, config)

  # 体重 <30kg，全部跳过，不矫正
  expect_equal(result$daily_feed_g, c(10000, 10000, 10000))
  expect_false(any(result$flag_feed_fcr_corrected))
})
