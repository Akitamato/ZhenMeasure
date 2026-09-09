# Birth info functions are internal (.read_birth_info, not exported).
# They are tested indirectly via integration tests (ZhenM_read_data with birth_info_path).
# See tests/testthat/test-qc-and-phenotype-age.R for age-stage integration tests.

test_that("birth info reading is tested via integration", {
  skip("Birth info functions are internal; tested via integration tests with ZhenM_read_data")
})

test_that("单列出生信息文件给出明确报错（issue #19）", {
  skip_if_not_installed("data.table")

  bf <- tempfile(fileext = ".csv")
  writeLines(c("ID", "A001"), bf)

  # 修复前：birth_col 取 names()[2] 得 NA → 报错 "column not found: [NA]"
  expect_error(
    ZhenMeasure:::.read_birth_info(bf),
    "至少需要两列"
  )
})
