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

test_that("weight imputation is invariant to input row order (issue #8)", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("zoo")
  skip_if_not_installed("imputeTS")

  set.seed(20260901)
  n <- 20
  miss <- c(3, 7, 12, 16, 19)

  dt_sorted <- data.table::data.table(
    animal_id = "A001",
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = n),
    daily_weight_g = 30000 + 500 * seq_len(n) + round(rnorm(n, 0, 100))
  )
  dt_sorted[miss, daily_weight_g := NA]
  dt_shuffled <- dt_sorted[sample.int(n)]

  cfg <- ZhenM_default_config("national_standard")
  r_s <- ZhenMeasure:::.impute_weight_national(data.table::copy(dt_sorted), cfg)
  r_h <- ZhenMeasure:::.impute_weight_national(data.table::copy(dt_shuffled), cfg)

  # 乱序输入不得错位：同一日期的插补结果与标记必须一致
  expect_equal(r_s[order(record_date)], r_h[order(record_date)])
  expect_equal(which(r_s[order(record_date)]$is_imputed_wt), sort(miss))
})

test_that("weight imputation never overwrites observed weights (issue #26)", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("zoo")
  skip_if_not_installed("imputeTS")

  set.seed(20260909)
  n <- 30
  miss <- c(5, 12, 13, 20)
  dt <- data.table::data.table(
    animal_id = "A001",
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = n),
    daily_weight_g = 30000 + 700 * seq_len(n) + round(rnorm(n, 0, 300))
  )
  dt[miss, daily_weight_g := NA]
  observed <- !is.na(dt$daily_weight_g)
  orig <- dt$daily_weight_g

  res <- ZhenMeasure:::.impute_weight_national(data.table::copy(dt), ZhenM_default_config("national_standard"))

  # 原本有效的观测值必须逐位不变（不被 Kalman 平滑改写）
  expect_equal(res$daily_weight_g[observed], orig[observed])
  # 插补标记与原缺失位严格一致
  expect_equal(which(res$is_imputed_wt), miss)
  # 缺失位全部被填补
  expect_false(any(is.na(res$daily_weight_g)))
})

test_that("feed imputation is invariant to input row order (issue #8)", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("zoo")

  n <- 24
  miss <- c(4, 10, 17)

  dt_sorted <- data.table::data.table(
    animal_id = "A001",
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = n),
    daily_feed_g = 1000 + 50 * seq_len(n)
  )
  dt_sorted[miss, daily_feed_g := NA]
  dt_shuffled <- dt_sorted[sample.int(n)]

  cfg <- ZhenM_default_config("national_standard")
  r_s <- ZhenMeasure:::.impute_feed_national_v2(data.table::copy(dt_sorted), cfg)
  r_h <- ZhenMeasure:::.impute_feed_national_v2(data.table::copy(dt_shuffled), cfg)

  expect_equal(r_s[order(record_date)], r_h[order(record_date)])
  expect_equal(which(r_s[order(record_date)]$is_imputed_feed), sort(miss))
})

test_that("continuous-missing path does not pollute output schema (issue #8)", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("zoo")

  n <- 25
  weight_g <- 50000 + 2000 * (seq_len(n) - 1)   # 50→98 kg：中段缺失场景
  dt <- data.table::data.table(
    animal_id = "A001",
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = n),
    daily_weight_g = weight_g,
    daily_feed_g = 2000 + 20 * (weight_g / 1000 - 50)
  )
  miss <- 10:15   # 连续缺失 6 天（>3 触发外推路径）
  dt[miss, daily_feed_g := NA]

  # fcr_ranges=NULL（modifyList 语义：NULL 删键）→ 跳过 FCR 阶段校验
  cfg <- ZhenM_merge_config(list(national_standard = list(fcr_ranges = NULL)))
  result <- ZhenMeasure:::.impute_feed_national_v2(data.table::copy(dt), cfg)

  # 拟合中间列不得泄漏进输出（无论外推成功或 r2 不达标回退）
  expect_false(any(c("cum_feed", "weight_kg", "pred_cum_feed", "pred_daily_feed") %in% names(result)))
  # 缺失天全部被填（外推或中位数兜底）且打标
  expect_true(all(is.finite(result$daily_feed_g[miss])))
  expect_equal(which(result$is_imputed_feed), sort(miss))
})

test_that(".check_fcr_stages_v2 fails closed on degenerate stages (issue #11)", {
  cfg <- ZhenM_default_config("national_standard")

  # 正常阶段（fcr=1.2 ∈ [1.14,3.30]）+ 0/0 退化阶段（gain=0 且 feed=0 → NaN）
  mixed <- data.table::data.table(
    daily_feed_g = c(3000, 3000, 0, 0),
    weight_kg    = c(55, 60, 65, 65)
  )
  # 修复前：NaN 比较被 na.rm=TRUE 丢弃 → 正常阶段单独放行（fail-open 返回 TRUE）
  expect_false(ZhenMeasure:::.check_fcr_stages_v2(mixed, cfg))

  # 全退化阶段：旧写法 all(logical(0)) 返回 TRUE
  degen <- data.table::data.table(daily_feed_g = rep(0, 4), weight_kg = rep(65, 4))
  expect_false(ZhenMeasure:::.check_fcr_stages_v2(degen, cfg))

  # Inf 路径（feed>0、gain=0）保持拦截
  inf_case <- data.table::data.table(
    daily_feed_g = c(3000, 3000, 2000, 2000),
    weight_kg    = c(55, 60, 65, 65)
  )
  expect_false(ZhenMeasure:::.check_fcr_stages_v2(inf_case, cfg))
})

test_that("feed imputation survives degenerate FCR fit data (issue #11)", {
  # 平坦体重 + 窗口全 0 feed → 旧代码 FCR 守卫放行后带病外推；
  # 新代码 fail-closed → 缺失天由中位数兜底填充，全程无报错
  n <- 20
  dt <- data.table::data.table(
    animal_id = "A001",
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = n),
    daily_weight_g = rep(65000, n),
    daily_feed_g = c(rep(0, 8), rep(NA_real_, 4), rep(0, 8))
  )
  cfg <- ZhenM_default_config("national_standard")
  result <- ZhenMeasure:::.impute_feed_national_v2(data.table::copy(dt), cfg)

  expect_true(all(is.finite(result$daily_feed_g)))
  expect_equal(which(result$is_imputed_feed), 9:12)
})
