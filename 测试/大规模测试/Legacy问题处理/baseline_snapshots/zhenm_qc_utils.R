#' Create logger helper functions for QC modules
#' @param logger Logger object (from ZhenM_create_logger)
#' @return List with log_info, log_detail, log_subsection functions
#' @keywords internal
.create_logger_helpers <- function(logger) {
  list(
    log_info = function(msg) {
      if (!is.null(logger) && is.list(logger) && "info" %in% names(logger)) 
        logger$info(msg)
    },
    log_detail = function(msg) {
      if (!is.null(logger) && is.list(logger) && "detail" %in% names(logger)) 
        logger$detail(msg)
    },
    log_subsection = function(title) {
      if (!is.null(logger) && is.list(logger) && "subsection" %in% names(logger)) 
        logger$subsection(title)
    }
  )
}

#' Normalize weight range to grams
#' @param weight_range Numeric vector [min, max] in kg or g
#' @return Numeric vector [min_g, max_g] in grams
#' @keywords internal
.normalize_weight_range <- function(weight_range) {
  wr <- suppressWarnings(as.numeric(weight_range))
  if (length(wr) < 2 || any(is.na(wr[1:2]))) return(c(-Inf, Inf))
  # Default config is in kg (e.g., 25-140). Convert to g for weight_g.
  if (max(abs(wr), na.rm = TRUE) <= 500) return(wr * 1000)
  wr
}

#' Normalize feed range to grams
#' @param feed_range Numeric vector [min, max] in kg or g
#' @return Numeric vector [min_g, max_g] in grams
#' @keywords internal
.normalize_feed_range <- function(feed_range) {
  fr <- suppressWarnings(as.numeric(feed_range))
  if (length(fr) < 2 || any(is.na(fr[1:2]))) return(c(-Inf, Inf))
  # Default config is in kg. Convert to g for feed_g.
  if (max(abs(fr), na.rm = TRUE) <= 500) return(fr * 1000)
  fr
}

# ============================================================================
# WEIGHT QC HELPER FUNCTIONS (consolidated from ZhenM_qc_weight_helpers.R)
# ============================================================================

#' Safely fit RLM model with error handling
#'
#' 统一的RLM拟合和错误处理，避免在多个地方重复
#'
#' @param y Response variable (numeric vector)
#' @param x Predictor variable (numeric vector)
#' @param formula_type Type of formula: "linear" or "polynomial"
#' @param maxit Maximum iterations for RLM
#'
#' @return RLM model object or NULL if fitting fails
#' @keywords internal
.safe_rlm_fit <- function(y, x, formula_type = "linear", maxit = 60) {
  valid <- !is.na(y) & !is.na(x)
  if (sum(valid) < 10) {
    return(NULL)
  }
  
  rlm_fit <- switch(formula_type,
    "linear" = MASS::rlm(y[valid] ~ x[valid], maxit = maxit),
    "polynomial" = MASS::rlm(y[valid] ~ x[valid] + I(x[valid]^2), maxit = maxit),
    NULL
  )

  tryCatch(rlm_fit, error = function(e) NULL)
}

#' Map daily values to individual records
#'
#' 统一的日期-值映射逻辑，避免在多处重复
#'
#' @param dt Data table containing records
#' @param idx Row indices for specific animal
#' @param daily_data Data table with daily values (must have record_date column)
#' @param value_col Column name in daily_data containing values to map
#' @param output_col Output column name in dt
#'
#' @return Data table with mapped values
#' @keywords internal
.map_daily_values_to_records <- function(dt, idx, daily_data, value_col, output_col) {
  dt_sub <- dt[idx]
  
  # Create mapping table with duplicate dates removed (keep first occurrence)
  map_data <- unique(daily_data[, c("record_date", value_col), with = FALSE], by = "record_date")
  data.table::setnames(map_data, value_col, "map_value")
  data.table::setkey(map_data, record_date)
  
  # Match and assign values using merge to avoid cartesian join issues
  dt_sub_copy <- data.table::copy(dt_sub)
  data.table::setkey(dt_sub_copy, record_date)
  
  merged <- map_data[dt_sub_copy, on = "record_date"]
  matched_values <- merged$map_value
  
  data.table::set(dt, i = idx, j = output_col, value = matched_values)
  return(dt)
}

#' Initialize QC flag columns
#'
#' 根据质控方法统一初始化QC标志列
#'
#' @param dt Data table
#' @param method QC method: "national_standard" or "legacy"
#'
#' @return Data table with initialized flag columns
#' @keywords internal
.init_qc_flags <- function(dt, method = "national_standard") {
  flags <- switch(method,
    "national_standard" = c(
      "flag_weight_out_of_range", "flag_weight_low",
      "flag_daily_weight_low", "flag_growth_curve_poor",
      "is_outlier_wt"
    ),
    "legacy" = c(
      "flag_weight_out_of_range", "flag_SD_WT", "flag_RLM_WT",
      "flag_Gompertz_WT", "flag_STL_FI", "is_imputed_feed",
      "is_outlier_wt", "is_outlier_fi_stl"
    ),
    NULL
  )
  
  if (!is.null(flags)) {
    dt[, (flags) := FALSE]
  }
  return(dt)
}

#' Check growth curve fit quality
#'
#' 计算和检查生长曲线拟合的R²值
#'
#' @param y Response variable
#' @param x Predictor variable (day sequence or time)
#' @param min_r2 Minimum acceptable R² value
#'
#' @return List with r2, pass (logical), and error message
#' @keywords internal
.check_growth_fit <- function(y, x, min_r2 = 0.5) {
  valid <- !is.na(y) & !is.na(x)
  if (sum(valid) < 10) {
    return(list(r2 = NA_real_, pass = FALSE, error = "数据不足 (< 10条)"))
  }
  
  fit <- tryCatch(
    lm(y[valid] ~ x[valid] + I(x[valid]^2)),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(list(r2 = NA_real_, pass = FALSE, error = "拟合失败"))
  }
  
  r2 <- summary(fit)$r.squared
  pass <- r2 >= min_r2
  
  list(r2 = r2, pass = pass, error = NULL)
}

#' Identify dates with all weight anomalies
#'
#' 找出某日所有体重都是异常的日期
#'
#' @param weight_records Data table with record_date and flag columns
#' @param flag_col Flag column name indicating single-record anomalies
#'
#' @return Vector of dates where all records are anomalous
#' @keywords internal
.identify_bad_dates <- function(weight_records, flag_col = "flag_outlier_single") {
  summary <- weight_records[, .(
    total_count = .N,
    outlier_count = sum(get(flag_col), na.rm = TRUE)
  ), by = record_date]
  
  bad_dates <- summary[outlier_count == total_count, record_date]
  return(bad_dates)
}
