#' Legacy feed imputation (simple)
#' @keywords internal
.impute_feed_legacy <- function(daily_records, cfg) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))

  if (!"is_imputed_feed" %in% names(dt)) dt[, is_imputed_feed := FALSE]

  # 记录原始缺失位置，然后进行填充
  dt[, was_missing_feed := is.na(daily_feed_g)]

  dt[, `:=`(
    daily_feed_g = zoo::na.approx(daily_feed_g, na.rm = FALSE),
    is_imputed_feed = was_missing_feed
  ), by = animal_id]

  # Fix any negative values from interpolation — set to 0 but flag as imputed
  neg_idx <- which(dt$daily_feed_g < 0 & !is.na(dt$daily_feed_g))
  if (length(neg_idx) > 0) {
    dt[neg_idx, c("daily_feed_g", "is_imputed_feed") := .(0, TRUE)]
  }

  # 清理临时列
  dt[, was_missing_feed := NULL]

  dt
}
