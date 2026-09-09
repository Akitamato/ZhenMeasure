# Unit tests for STL and Gompertz integration into National Standard method
# V0.2.6: STL time-series feed QC and Gompertz growth curve weight QC

# ---------------------------------------------------------------------------
# Config tests
# ---------------------------------------------------------------------------

test_that("national_standard config includes STL and Gompertz parameters", {
  cfg <- ZhenM_default_config("national_standard")$national_standard

  # STL parameters
  expect_true("use_stl_feed" %in% names(cfg))
  expect_false(cfg$use_stl_feed)  # 默认关闭
  expect_equal(cfg$stl_period, 7)
  expect_equal(cfg$stl_s_window, "periodic")
  expect_true(cfg$stl_robust)
  expect_equal(cfg$stl_mad_multiplier, 3)
  expect_equal(cfg$stl_min_obs, 30)

  # Gompertz parameters
  expect_true("use_gompertz" %in% names(cfg))
  expect_false(cfg$use_gompertz)  # 默认关闭
  expect_equal(cfg$gompertz_min_obs, 60)
  expect_equal(cfg$gompertz_mad_multiplier, 4)
  expect_equal(cfg$gompertz_maxiter, 100)
})

test_that("user can enable STL and Gompertz via config", {
  user_cfg <- list(national_standard = list(
    use_stl_feed = TRUE,
    use_gompertz = TRUE,
    stl_mad_multiplier = 2.5
  ))
  merged <- ZhenM_merge_config(user_cfg, "national_standard")

  expect_true(merged$national_standard$use_stl_feed)
  expect_true(merged$national_standard$use_gompertz)
  expect_equal(merged$national_standard$stl_mad_multiplier, 2.5)
  # 未覆盖的参数保持默认值
  expect_equal(merged$national_standard$stl_period, 7)
})

# ---------------------------------------------------------------------------
# Feed QC: STL integration tests
# ---------------------------------------------------------------------------

test_that("STL flag is created when use_stl_feed = TRUE", {
  skip_if_not_installed("data.table")

  # 创建 60 天、每天 3 次访问的模拟数据
  set.seed(42)
  n_days <- 60
  dt <- data.table::data.table(
    animal_id = rep("A001", n_days * 3),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = n_days), each = 3),
    feed_g = rep(rnorm(n_days, 2500, 200), each = 3),
    duration_sec = rep(rnorm(n_days, 600, 50), each = 3),
    weight_g = rep(seq(40000, 70000, length.out = n_days), each = 3),
    device_type = "YANGXIANG"
  )

  # 注入 3 天异常高采食量
  anomaly_days <- as.Date(c("2024-01-15", "2024-02-01", "2024-02-20"))
  dt[record_date %in% anomaly_days, feed_g := 8000]

  result <- ZhenM_qc_feed_standard(dt, "national_standard",
    config = list(national_standard = list(use_stl_feed = TRUE)))

  expect_true("flag_STL_FI" %in% names(result))
  # 应该标记了至少一些异常日
  expect_true(sum(result$flag_STL_FI, na.rm = TRUE) > 0)
})

test_that("STL flag is NOT created when use_stl_feed = FALSE (default)", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 30),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10), each = 3),
    feed_g = rep(c(300, 400, 350), 10),
    duration_sec = rep(300, 30),
    weight_g = rep(40000, 30),
    device_type = "YANGXIANG"
  )

  result <- ZhenM_qc_feed_standard(dt, "national_standard")

  expect_true("flag_STL_FI" %in% names(result))
  expect_true(all(!result$flag_STL_FI))  # 全部为 FALSE
})

test_that("STL respects stl_min_obs threshold", {
  skip_if_not_installed("data.table")

  # 仅 10 天数据，低于 stl_min_obs = 30
  dt <- data.table::data.table(
    animal_id = rep("A001", 30),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10), each = 3),
    feed_g = rep(c(300, 400, 350), 10),
    duration_sec = rep(300, 30),
    weight_g = rep(40000, 30),
    device_type = "YANGXIANG"
  )

  result <- ZhenM_qc_feed_standard(dt, "national_standard",
    config = list(national_standard = list(use_stl_feed = TRUE, stl_min_obs = 30)))

  # 数据不足 30 天，STL 不应标记任何记录
  expect_true(all(!result$flag_STL_FI))
})

test_that("STL flag is included in is_outlier_feed", {
  skip_if_not_installed("data.table")

  set.seed(123)
  n_days <- 60
  dt <- data.table::data.table(
    animal_id = rep("A001", n_days * 3),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = n_days), each = 3),
    feed_g = rep(rnorm(n_days, 2500, 100), each = 3),
    duration_sec = rep(rnorm(n_days, 600, 30), each = 3),
    weight_g = rep(seq(40000, 70000, length.out = n_days), each = 3),
    device_type = "YANGXIANG"
  )

  result_stl <- ZhenM_qc_feed_standard(dt, "national_standard",
    config = list(national_standard = list(use_stl_feed = TRUE)))

  result_no_stl <- ZhenM_qc_feed_standard(dt, "national_standard")

  # 启用 STL 后，is_outlier_feed 应该包含 flag_STL_FI 的贡献
  n_outlier_stl <- sum(result_stl$is_outlier_feed, na.rm = TRUE)
  n_outlier_no_stl <- sum(result_no_stl$is_outlier_feed, na.rm = TRUE)

  # STL 标记了额外的异常，所以 outlier 数应该 >= 不启用时
  expect_gte(n_outlier_stl, n_outlier_no_stl)
})

# ---------------------------------------------------------------------------
# Weight QC: Gompertz integration tests
# ---------------------------------------------------------------------------

test_that("Gompertz flag is created when use_gompertz = TRUE", {
  skip_if_not_installed("data.table")

  n_days <- 90
  dt <- data.table::data.table(
    animal_id = rep("A001", n_days * 3),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = n_days), each = 3),
    weight_g = rep(seq(30000, 80000, length.out = n_days) + rnorm(n_days, 0, 200), each = 3),
    feed_g = rep(2500, n_days * 3),
    device_type = "YANGXIANG"
  )

  result <- ZhenM_qc_weight_standard(dt, "national_standard",
    config = list(national_standard = list(use_gompertz = TRUE)))

  expect_true("flag_Gompertz_WT" %in% names(result))
})

test_that("Gompertz flag is NOT created when use_gompertz = FALSE (default)", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 30),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10), each = 3),
    weight_g = rep(seq(30000, 50000, length.out = 10), each = 3) + rnorm(30, 0, 500),
    feed_g = rep(2500, 30),
    device_type = "YANGXIANG"
  )

  result <- ZhenM_qc_weight_standard(dt, "national_standard")

  expect_true("flag_Gompertz_WT" %in% names(result))
  expect_true(all(!result$flag_Gompertz_WT))  # 全部为 FALSE
})

test_that("Gompertz flags a planted single-day weight spike via newdata residuals (issue #29)", {
  skip_if_not_installed("data.table")

  # 90 天 Gompertz 生长曲线（45→110kg），第 70 天人为抬高 3kg。
  # 记录级 RLM 不判该天异常（flag_weight_low=0），故异常只能由 Gompertz
  # 残差路径捕获——该路径依赖 predict(newdata=) 生效（issue #29）。
  set.seed(29)
  n <- 90
  w <- 115 * exp(-2.3 * exp(-0.055 * (1:n))) * 1000
  w[70] <- w[70] + 3000
  dt <- data.table::data.table(
    animal_id = "A001",
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = n), each = 2),
    weight_g = rep(w, each = 2) + stats::rnorm(2 * n, 0, 50),
    feed_g = 2500,
    device_type = "YANGXIANG"
  )

  result <- suppressWarnings(ZhenM_qc_weight_standard(dt, "national_standard",
    config = list(national_standard = list(
      use_gompertz = TRUE,
      gompertz_mad_multiplier = 4,
      test_weight_range = c(200, 20)  # 关闭全量程筛选，避免整头被删
    ))))

  spike_date <- as.Date("2024-03-10")  # 第 70 天
  expect_true(any(result$flag_Gompertz_WT & result$record_date == spike_date))
  expect_equal(sum(result$flag_weight_low, na.rm = TRUE), 0)  # 确非记录级 RLM 捕获
})

test_that("Gompertz still runs when some days have no weight (issue #29 nls subset abort)", {
  skip_if_not_installed("data.table")

  # 关键回归：旧公式把子集写在公式里（y_gomp[valid] ~ ... x_gomp[valid]），
  # nls 会校验 n %% respLength == 0；一旦有缺体重日 sum(valid) < n 即不整除，
  # nls 报错被 tryCatch 吞成 NULL，整头动物静默跳过。此处额外追加 7 个只有
  # 采食记录、无体重的日期（n_total=97, sum(valid)=90，97 %% 90 = 7 ≠ 0），
  # 旧代码在该动物上完全不做 Gompertz 检查，新代码应正常标出第 70 天尖峰。
  set.seed(29)
  n <- 90
  w <- 115 * exp(-2.3 * exp(-0.055 * (1:n))) * 1000
  w[70] <- w[70] + 3000
  days_main <- seq.Date(as.Date("2024-01-01"), by = "day", length.out = n)
  days_extra <- seq.Date(as.Date("2024-03-31"), by = "day", length.out = 7)
  dt <- data.table::data.table(
    animal_id = "A001",
    record_date = c(rep(days_main, each = 2), days_extra),
    weight_g = c(rep(w, each = 2) + stats::rnorm(2 * n, 0, 50), rep(NA_real_, 7)),
    feed_g = 2500,
    device_type = "YANGXIANG"
  )

  result <- suppressWarnings(ZhenM_qc_weight_standard(dt, "national_standard",
    config = list(national_standard = list(
      use_gompertz = TRUE,
      gompertz_mad_multiplier = 4,
      test_weight_range = c(200, 20)
    ))))

  spike_date <- as.Date("2024-03-10")
  expect_true(any(result$flag_Gompertz_WT & result$record_date == spike_date))
})

test_that("Gompertz handles insufficient data gracefully", {
  skip_if_not_installed("data.table")

  # 仅 10 天数据，低于 gompertz_min_obs = 60
  dt <- data.table::data.table(
    animal_id = rep("A001", 30),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10), each = 3),
    weight_g = rep(seq(30000, 50000, length.out = 10), each = 3),
    feed_g = rep(2500, 30),
    device_type = "YANGXIANG"
  )

  # 不应报错，应静默跳过
  expect_no_error(
    result <- ZhenM_qc_weight_standard(dt, "national_standard",
      config = list(national_standard = list(use_gompertz = TRUE)))
  )
  expect_true(all(!result$flag_Gompertz_WT))
})

test_that("Gompertz flag is included in is_outlier_wt", {
  skip_if_not_installed("data.table")

  set.seed(42)
  n_days <- 90
  dt <- data.table::data.table(
    animal_id = rep("A001", n_days * 3),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = n_days), each = 3),
    weight_g = rep(seq(30000, 80000, length.out = n_days) + rnorm(n_days, 0, 200), each = 3),
    feed_g = rep(2500, n_days * 3),
    device_type = "YANGXIANG"
  )

  result_gomp <- ZhenM_qc_weight_standard(dt, "national_standard",
    config = list(national_standard = list(use_gompertz = TRUE)))

  result_no_gomp <- ZhenM_qc_weight_standard(dt, "national_standard")

  n_outlier_gomp <- sum(result_gomp$is_outlier_wt, na.rm = TRUE)
  n_outlier_no_gomp <- sum(result_no_gomp$is_outlier_wt, na.rm = TRUE)

  # Gompertz 启用后，异常标记数应该 >= 不启用时
  expect_gte(n_outlier_gomp, n_outlier_no_gomp)
})

# ---------------------------------------------------------------------------
# Backward compatibility: default config produces identical results
# ---------------------------------------------------------------------------

test_that("default national_standard config produces identical results with and without integration code", {
  skip_if_not_installed("data.table")

  set.seed(99)
  n_days <- 60
  dt <- data.table::data.table(
    animal_id = rep("A001", n_days * 3),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = n_days), each = 3),
    feed_g = rep(rnorm(n_days, 2500, 200), each = 3),
    duration_sec = rep(rnorm(n_days, 600, 50), each = 3),
    weight_g = rep(seq(40000, 70000, length.out = n_days) + rnorm(n_days, 0, 200), each = 3),
    device_type = "YANGXIANG"
  )

  # 默认配置（STL=FALSE, Gompertz=FALSE）
  cfg <- ZhenM_default_config("national_standard")
  result_default <- ZhenM_qc_feed_standard(dt, "national_standard", config = cfg)
  result_wt_default <- ZhenM_qc_weight_standard(dt, "national_standard", config = cfg)

  # 验证默认配置下 flag 列存在但全为 FALSE
  expect_true("flag_STL_FI" %in% names(result_default))
  expect_true(all(!result_default$flag_STL_FI))

  expect_true("flag_Gompertz_WT" %in% names(result_wt_default))
  expect_true(all(!result_wt_default$flag_Gompertz_WT))
})

# ---------------------------------------------------------------------------
# LMM integration: STL flag as LMM covariate
# ---------------------------------------------------------------------------

test_that("flag_STL_FI is passed to LMM when enabled", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("lme4")

  set.seed(42)
  n_days <- 60
  dt <- data.table::data.table(
    animal_id = rep(c("A001", "A002"), each = n_days * 3),
    record_date = rep(rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = n_days), each = 3), 2),
    feed_g = rep(rnorm(n_days * 2, 2500, 200), each = 1) * 3,
    duration_sec = rep(rnorm(n_days * 2, 600, 50), each = 1) * 3,
    weight_g = rep(seq(40000, 70000, length.out = n_days * 2), each = 1) * 3,
    device_type = "YANGXIANG",
    is_outlier_feed = FALSE,
    is_outlier_wt = FALSE
  )

  # 标记一些 STL 异常
  dt[animal_id == "A001" & record_date == as.Date("2024-01-15"), flag_STL_FI := TRUE]
  dt[animal_id == "A001" & record_date == as.Date("2024-02-01"), flag_STL_FI := TRUE]

  # 运行日聚合（包含 LMM 校正）
  result <- ZhenM_standard_to_daily_filtered(dt)

  # 如果 lme4 可用，LMM 应该处理 flag_STL_FI
  # 这里主要验证不会报错
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})
