test_that("ZhenM_validate_standard_records normalizes standard columns", {
  dt <- data.table::data.table(
    animal_id = c("1001", "1001"),
    device_type = c("YANGXIANG", "YANGXIANG"),
    record_date = c("2026-01-01", "2026-01-02"),
    feed_g = c("120", "180"),
    weight_g = c("30000", "30500"),
    source_file = c("a.xlsx", "a.xlsx"),
    age_day = c("70", "71")
  )

  result <- ZhenM_validate_standard_records(dt)

  expect_s3_class(result$record_date, "IDate")
  expect_type(result$feed_g, "double")
  expect_true(all(c("start_time", "measurement_day") %in% names(result)))
})

test_that("ZhenM_standard_to_daily_filtered aggregates daily metrics", {
  dt <- data.table::data.table(
    animal_id = c("1001", "1001", "1002"),
    device_type = c("NEDAP", "NEDAP", "NEDAP"),
    record_date = data.table::as.IDate(c("2026-01-01", "2026-01-01", "2026-01-02")),
    start_time = as.POSIXct(c("2026-01-01 08:00:00", "2026-01-01 12:00:00", "2026-01-02 08:00:00")),
    end_time = as.POSIXct(c("2026-01-01 08:10:00", "2026-01-01 12:05:00", "2026-01-02 08:04:00")),
    duration_sec = c(600, 300, 240),
    feed_g = c(100, 120, 90),
    daily_feed_g = c(220, 220, 90),
    weight_g = c(30000, 30100, 28000),
    location = c("A", "A", "B"),
    source_file = c("a.csv", "a.csv", "b.csv"),
    age_day = c(NA, NA, NA),
    measurement_day = c(1, 1, 2)
  )

  daily <- ZhenM_standard_to_daily_filtered(dt)

  expect_equal(nrow(daily), 2L)
  expect_equal(daily[animal_id == "1001", daily_feed_g], 220)
  expect_equal(daily[animal_id == "1001", visits_n], 2L)
  expect_equal(daily[animal_id == "1001", total_duration_sec], 900)
})
