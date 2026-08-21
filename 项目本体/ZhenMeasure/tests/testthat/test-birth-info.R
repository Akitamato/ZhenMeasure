# Birth info functions are internal (.read_birth_info, not exported).
# They are tested indirectly via integration tests (ZhenM_read_data with birth_info_path).
# See tests/testthat/test-qc-and-phenotype-age.R for age-stage integration tests.

test_that("birth info reading is tested via integration", {
  skip("Birth info functions are internal; tested via integration tests with ZhenM_read_data")
})
