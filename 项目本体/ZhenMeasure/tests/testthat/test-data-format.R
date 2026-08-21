test_that("ZhenM_parse_data_format parses positions and labels", {
  format_file <- tempfile(fileext = ".txt")
  lines <- c(
    "Data format\tColumn position\tEnglish label (optional)",
    "个体号（ID）\t6\tanimal_id",
    "字符型(Character)\t2,3\tlocation,pen",
    "数值型(Numeric)\t10,13,18\tweight_g,feed_g,daily_feed_g",
    "日期型(Date)\t1,16\trecord_date,start_time"
  )
  writeLines(lines, format_file, useBytes = TRUE)

  parsed <- ZhenM_parse_data_format(format_file)

  expect_equal(parsed$id_col_pos, 6L)
  expect_equal(parsed$character_col_pos, c(2L, 3L))
  expect_equal(parsed$numeric_col_labels, c("weight_g", "feed_g", "daily_feed_g"))
  expect_equal(parsed$date_col_labels, c("record_date", "start_time"))
})

test_that("ZhenM_parse_data_format rejects duplicated positions", {
  format_file <- tempfile(fileext = ".txt")
  lines <- c(
    "Data format\tColumn position",
    "个体号（ID）\t6",
    "字符型(Character)\t2,6",
    "数值型(Numeric)\t10",
    "日期型(Date)\t1"
  )
  writeLines(lines, format_file, useBytes = TRUE)

  expect_error(ZhenM_parse_data_format(format_file), "Duplicate column positions found")
})
