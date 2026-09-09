#' Impute missing data (national standard method)
#'
#' Fills missing weight and feed data using national standard methods
#' (Kalman filter for weight, Loess/linear regression for feed).
#'
#' @param daily_records Daily-level data.table with columns: animal_id, record_date,
#'   daily_weight_g, daily_feed_g
#' @param impute_method Imputation method: "national_standard" (the only supported method since V1.0.0)
#' @param config Optional configuration list. If NULL, uses default config.
#' @return Data.table with imputed values and is_imputed_wt, is_imputed_feed flags
#' @export
#' @examples
#' \dontrun{
#' # National standard imputation
#' result <- ZhenM_impute_data(daily_data, impute_method = "national_standard")
#' }
ZhenM_impute_data <- function(daily_records, impute_method = "national_standard", config = NULL) {
  if (!identical(impute_method, "national_standard")) {
    stop("Legacy imputation method was removed in V1.0.0. Use 'national_standard'.", call. = FALSE)
  }

  cfg <- ZhenM_merge_config(config, qc_method = "national_standard")

  dt <- data.table::as.data.table(data.table::copy(daily_records))

  # Impute weight
  dt <- ZhenM_impute_weight(dt, impute_method = "national_standard", cfg)

  # Impute feed
  dt <- ZhenM_impute_feed(dt, impute_method = "national_standard", cfg)

  dt
}

#' Impute weight data
#' @keywords internal
ZhenM_impute_weight <- function(daily_records, impute_method = "national_standard", config = NULL) {
  cfg <- ZhenM_merge_config(config, qc_method = impute_method)
  .impute_weight_national(daily_records, cfg)
}

#' Impute feed data
#' @keywords internal
ZhenM_impute_feed <- function(daily_records, impute_method = "national_standard", config = NULL) {
  cfg <- ZhenM_merge_config(config, qc_method = impute_method)
  .impute_feed_national_v2(daily_records, cfg)
}

#' National standard weight imputation (Kalman filter)
#' @keywords internal
.impute_weight_national <- function(daily_records, cfg) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))

  # 统一排序（issue #8）：下面按「日期序」算出的插补值要写回同一批行，
  # 若输入未按 animal_id+record_date 排序，idx（原始行序）与日期序结果
  # 会错位，插补值被静默写到错误的日期上。dt 已是副本，原地排序不影响
  # 调用方；返回表因此按个体+日期有序（下游均按个体分组，无行序依赖）。
  data.table::setorder(dt, animal_id, record_date)

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

    # 只对「被插补的位置」取值与钳位（issue #26）。na_kalman / na.approx 实测均不改写
    # 非缺失位置，但显式只写 missing_idx 让「观测值不被改动」成为本函数的不变量，
    # 不再依赖 imputeTS 内部实现；钳位同样只作用于插补值，不碰原始观测。
    y_imputed <- y_interp[missing_idx]

    # Physical floor: can't be negative, can't be less than 90% of minimum recorded weight
    hard_floor <- min_valid_wt * 0.9
    y_imputed[y_imputed < hard_floor] <- hard_floor

    # Physical ceiling: can't exceed max recorded weight by more than 20kg (20,000g)
    hard_ceil <- max_valid_wt + 20000
    y_imputed[y_imputed > hard_ceil] <- hard_ceil

    dt[idx[missing_idx], daily_weight_g := y_imputed]
    dt[idx[missing_idx], is_imputed_wt := TRUE]
  }

  dt
}
