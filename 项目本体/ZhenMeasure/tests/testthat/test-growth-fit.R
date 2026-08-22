# Unit tests for .check_growth_fit (growth curve R² check)

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
