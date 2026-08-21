test_that("qc_only returns split QC outputs", {
  skip("ZhenM_run() does not exist; qc_only mode not yet implemented as a standalone function")
})

test_that("qc_only writes structured qc output files", {
  skip("ZhenM_run() does not exist; qc_only mode not yet implemented as a standalone function")
})

test_that("age stage phenotype calculation works with birth info", {
  daily <- data.table::data.table(
    animal_id = rep("pig1", 20),
    record_date = data.table::as.IDate(seq.Date(as.Date("2025-03-01"), by = "day", length.out = 20)),
    daily_feed_g = seq(1000, 1190, by = 10),
    median_weight_g = seq(30000, 49000, by = 1000),
    age_day = 70:89,
    measurement_day = 1:20,
    device_type = "NEDAP"
  )

  # Use stage interface (valid exported function)
  pheno <- ZhenM_calc_phenotypes_stage(
    daily_records = daily,
    stage_mode = "age",
    target_age_stages = list("70-85d" = c(70, 85)),
    target_phenotype = c("ADFI", "ADG", "FCR")
  )

  expect_true("70-85d_ADFI" %in% names(pheno))
  expect_true("70-85d_ADG" %in% names(pheno))
  expect_true("70-85d_FCR" %in% names(pheno))
  expect_true(!is.na(pheno$`70-85d_ADFI`[1]))
})

test_that("new phenotype calculation interface works with age stages", {
  daily <- data.table::data.table(
    animal_id = rep("pig1", 30),
    record_date = data.table::as.IDate(seq.Date(as.Date("2025-03-01"), by = "day", length.out = 30)),
    daily_feed_g = seq(1000, 1290, by = 10),
    daily_weight_g = seq(30000, 59000, by = 1000),
    age_days = 70:99
  )

  # Use new interface (general method)
  pheno <- ZhenM_calc_phenotypes(
    daily_records = daily,
    phenotype_method = "report",
    stage_mode = "age",
    target_age_stages = c(70, 85, 100)
  )

  expect_true("stage_label" %in% names(pheno))
  expect_true("ADG_g" %in% names(pheno))
  expect_true("ADFI_g" %in% names(pheno))
  expect_true("FCR" %in% names(pheno))
  expect_true(nrow(pheno) >= 2)  # Should have at least 2 stages
})
