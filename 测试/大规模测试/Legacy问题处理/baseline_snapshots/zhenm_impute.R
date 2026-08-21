#' Impute missing data with dual-method support
#'
#' Fills missing weight and feed data using either national standard methods
#' (Loess, linear regression extrapolation) or legacy methods (simple interpolation).
#'
#' @param daily_records Daily-level data.table with columns: animal_id, record_date,
#'   daily_weight_g, daily_feed_g
#' @param impute_method Imputation method: "national_standard" (default) or "legacy"
#' @param config Optional configuration list. If NULL, uses default config.
#' @return Data.table with imputed values and is_imputed_wt, is_imputed_feed flags
#' @export
#' @examples
#' \dontrun{
#' # National standard imputation
#' result <- ZhenM_impute_data(daily_data, impute_method = "national_standard")
#'
#' # Legacy imputation
#' result <- ZhenM_impute_data(daily_data, impute_method = "legacy")
#' }
ZhenM_impute_data <- function(daily_records, impute_method = "national_standard", config = NULL) {
  cfg <- ZhenM_merge_config(config, qc_method = impute_method)

  dt <- data.table::as.data.table(data.table::copy(daily_records))

  # Impute weight
  dt <- ZhenM_impute_weight(dt, impute_method, cfg)

  # Impute feed
  dt <- ZhenM_impute_feed(dt, impute_method, cfg)

  dt
}

#' Impute weight data
#' @keywords internal
ZhenM_impute_weight <- function(daily_records, impute_method = "national_standard", config = NULL) {
  cfg <- ZhenM_merge_config(config, qc_method = impute_method)

  if (impute_method == "national_standard") {
    .impute_weight_national(daily_records, cfg)
  } else {
    .impute_weight_legacy(daily_records, cfg)
  }
}

#' Impute feed data
#' @keywords internal
ZhenM_impute_feed <- function(daily_records, impute_method = "national_standard", config = NULL) {
  cfg <- ZhenM_merge_config(config, qc_method = impute_method)

  if (impute_method == "national_standard") {
    .impute_feed_national_v2(daily_records, cfg)
  } else {
    .impute_feed_legacy(daily_records, cfg)
  }
}

#' National standard weight imputation (spline)
#' @keywords internal
.impute_weight_national <- function(daily_records, cfg) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))

  if (!"is_imputed_wt" %in% names(dt)) dt[, is_imputed_wt := FALSE]

  ids <- unique(dt$animal_id)
  for (id in ids) {
    idx <- which(dt$animal_id == id)
    sub <- dt[idx]

    data.table::setorder(sub, record_date)
    x <- as.numeric(sub$record_date - min(sub$record_date))
    y <- sub$daily_weight_g

    missing_idx <- is.na(y)
    if (!any(missing_idx) || sum(!missing_idx) < 4) next

    # Use Kalman Filter (imputeTS) to model and smooth the growth curve including endpoints.
    # The StructTS (Structural Time Series) model is excellent at capturing local trends 
    # without exploding into massive negative/positive infinity like Cubic Splines do.
    y_interp <- tryCatch({
      # Try Kalman Smoothing
      # Structural models need at least a few points to guess the level/trend
      if (sum(!is.na(y)) >= 4 && requireNamespace("imputeTS", quietly = TRUE)) {
        suppressWarnings(imputeTS::na_kalman(y, model = "StructTS"))
      } else {
        # Fallback if library missing or sequence too short
        zoo::na.approx(y, x, na.rm = FALSE, rule = 2)
      }
    }, error = function(e) {
      zoo::na.approx(y, x, na.rm = FALSE, rule = 2)
    })

    # Optional safeguard check if Kalman somehow still leaves NAs at the extreme edges
    if (any(is.na(y_interp))) {
      y_interp <- zoo::na.approx(y_interp, x, na.rm = FALSE, rule = 2)
    }

    # Physics Engine Safeguards: Even Kalman filter can occasionally overestimate trend if 
    # the last 2 days of records showed an intense (faked/water) localized jump in weight. 
    # We allow the pig to "virtually grow" during missing periods, but clamp the boundaries.
    min_valid_wt <- min(y, na.rm = TRUE)
    max_valid_wt <- max(y, na.rm = TRUE)
    
    # 物理下限界：绝对不可能为负数，也不可能瞬间比进站最小体重的 90% 还轻
    hard_floor <- min_valid_wt * 0.9
    y_interp[y_interp < hard_floor] <- hard_floor

    # 物理上限界：哪怕外推预测再猛，不能比最重那天的体重凭空胖出 20 公斤 (20,000g)
    hard_ceil <- max_valid_wt + 20000
    y_interp[y_interp > hard_ceil] <- hard_ceil

    dt[idx, daily_weight_g := y_interp]
    dt[idx[missing_idx], is_imputed_wt := TRUE]
  }

  dt
}

#' Legacy weight imputation (simple)
#' @keywords internal
.impute_weight_legacy <- function(daily_records, cfg) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))

  if (!"is_imputed_wt" %in% names(dt)) dt[, is_imputed_wt := FALSE]

  # 记录原始缺失位置，然后进行填充
  dt[, was_missing_weight := is.na(daily_weight_g)]

  dt[, `:=`(
    daily_weight_g = zoo::na.approx(daily_weight_g, na.rm = FALSE),
    is_imputed_wt = was_missing_weight
  ), by = animal_id]

  # 清理临时列
  dt[, was_missing_weight := NULL]

  dt
}
