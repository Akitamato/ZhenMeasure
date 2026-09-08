# Unit tests for .check_growth_fit (growth curve R² check)
# and .safe_rlm_fit (RLM fitting with error handling)

test_that(".check_growth_fit computes R2 for perfect quadratic", {
  x <- 1:30
  y <- 30000 + 800 * x - 2 * x^2  # 完美二次曲线

  res <- .check_growth_fit(y, x, min_r2 = 0.95)

  expect_true(res$pass)
  expect_gt(res$r2, 0.99)
})

test_that(".check_growth_fit rejects insufficient data", {
  res <- .check_growth_fit(1:5, 1:5, min_r2 = 0.95)

  expect_false(res$pass)
  expect_true(is.na(res$r2))
})

test_that(".check_growth_fit handles constant y without NA pass", {
  x <- 1:20
  y <- rep(100, 20)  # 常数 y → 二次拟合降秩，r2 为 NaN

  res <- .check_growth_fit(y, x, min_r2 = 0.95)

  expect_false(res$pass)
  expect_false(is.na(res$pass))  # NA 崩溃防护：pass 不能是 NA
})

test_that(".check_growth_fit pass respects threshold", {
  x <- 1:30
  y <- 30000 + 800 * x - 2 * x^2  # r2 ≈ 1

  expect_true(.check_growth_fit(y, x, min_r2 = 0.5)$pass)
  expect_false(.check_growth_fit(y, x, min_r2 = 1.5)$pass)  # min_r2 > 1 不可能通过
})

test_that(".safe_rlm_fit returns NULL (not error) on non-finite input", {
  set.seed(1)
  x <- 1:30
  y <- 30000 + 500 * x + rnorm(30, 0, 200)

  # Inf 不被 is.na 过滤，MASS::rlm 对非有限值硬报错；
  # .safe_rlm_fit 必须捕获异常返回 NULL，而非让错误穿透中断 QC 循环
  x_inf <- x; x_inf[5] <- Inf
  expect_error(f1 <- .safe_rlm_fit(y, x_inf, formula_type = "polynomial"), NA)
  expect_null(f1)

  y_inf <- y; y_inf[5] <- Inf
  expect_error(f2 <- .safe_rlm_fit(y_inf, x, formula_type = "polynomial"), NA)
  expect_null(f2)

  # 线性形式同样防护
  expect_error(f3 <- .safe_rlm_fit(y, x_inf, formula_type = "linear"), NA)
  expect_null(f3)
})

test_that(".safe_rlm_fit fits normally on clean data", {
  set.seed(1)
  x <- 1:30
  y <- 30000 + 500 * x + rnorm(30, 0, 200)

  fit <- .safe_rlm_fit(y, x, formula_type = "polynomial")
  expect_false(is.null(fit))
  expect_length(fit$w, 30)          # RLM 权重与输入等长
  expect_type(fit$w, "double")
})
