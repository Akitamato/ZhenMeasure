test_that("ZhenM_write_plot_outputs creates combined and per-animal PDFs", {
  # Raw daily data (original before QC)
  raw_daily <- data.table::data.table(
    animal_id = c("A001", "A001", "A002", "A002"),
    record_date = data.table::as.IDate(c("2025-01-01", "2025-01-02", "2025-01-01", "2025-01-02")),
    daily_feed_g = c(600, 650, 700, 680),
    daily_weight_g = c(30000, 30500, 32000, 32600),
    day_has_outlier_feed = c(FALSE, TRUE, FALSE, FALSE),
    day_has_outlier_wt = c(FALSE, TRUE, FALSE, FALSE),
    day_is_imputed_feed = c(FALSE, TRUE, FALSE, FALSE),
    day_is_imputed_wt = c(FALSE, TRUE, FALSE, FALSE)
  )

  # Processed daily data (after QC and imputation)
  daily_data <- data.table::data.table(
    animal_id = c("A001", "A001", "A002", "A002"),
    record_date = data.table::as.IDate(c("2025-01-01", "2025-01-02", "2025-01-01", "2025-01-02")),
    daily_feed_g = c(600, 620, 700, 680),
    daily_weight_g = c(30000, 30300, 32000, 32600),
    is_imputed_feed = c(FALSE, TRUE, FALSE, FALSE),
    is_imputed_wt = c(FALSE, TRUE, FALSE, FALSE)
  )

  result <- list(
    raw_daily = raw_daily,
    daily_records = daily_data
  )

  plot_writer <- if (exists("ZhenM_write_plot_outputs", mode = "function")) {
    get("ZhenM_write_plot_outputs", mode = "function")
  } else {
    getFromNamespace("ZhenM_write_plot_outputs", "ZhenMeasure")
  }

  output_dir <- tempfile("ZhenM_plot_test_")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  plot_files <- plot_writer(
    result = result,
    output_dir = output_dir,
    growth_curve = TRUE,
    growth_curve_test = TRUE
  )

  expect_true(file.exists(file.path(output_dir, "growth_curves.pdf")))
  expect_true(dir.exists(file.path(output_dir, "growth_curves")))

  per_animal_files <- list.files(file.path(output_dir, "growth_curves"), pattern = "\\.pdf$", full.names = TRUE)
  expect_gte(length(per_animal_files), 2)
  expect_true(length(plot_files) >= 2)
  expect_true(any(grepl("growth_curves.pdf$", plot_files)))
})

test_that(".with_pdf_device 报错时也关闭设备（issue #23）", {
  helper <- getFromNamespace(".with_pdf_device", "ZhenMeasure")
  tmp_pdf <- tempfile(fileext = ".pdf")

  dev_before <- grDevices::dev.cur()
  expect_error(helper(tmp_pdf, stop("boom")), "boom")
  expect_equal(grDevices::dev.cur(), dev_before)
  expect_true(file.exists(tmp_pdf))

  # 正常路径：表达式执行后设备同样被关闭
  helper(tmp_pdf, graphics::plot(1:3, 1:3))
  expect_equal(grDevices::dev.cur(), dev_before)
})
