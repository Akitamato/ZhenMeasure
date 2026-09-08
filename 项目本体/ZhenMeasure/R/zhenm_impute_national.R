#' National standard feed imputation (CORRECTED VERSION)
#'
#' Implements feed imputation according to red-header file specifications.
#' Key correction: Uses weight range of missing interval (not position percentage)
#' to determine fitting strategy.
#'
#' @param daily_records Daily-level data.table
#' @param config Configuration list
#' @return Data.table with imputed feed values
#' @keywords internal
.impute_feed_national_v2 <- function(daily_records, config) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))

  if (!"is_imputed_feed" %in% names(dt)) dt[, is_imputed_feed := FALSE]

  # 统一排序（issue #8）：与 .impute_weight_national 同因——idx 取的是
  # 原始行序，而 sub 经 setorder 后按日期序计算，乱序输入会把插补值/
  # 标记写错行。dt 已是副本，原地排序安全。
  data.table::setorder(dt, animal_id, record_date)

  ids <- unique(dt$animal_id)

  for (id in ids) {
    idx <- which(dt$animal_id == id)
    sub <- dt[idx]

    data.table::setorder(sub, record_date)
    y <- sub$daily_feed_g
    missing_idx <- is.na(y)

    if (!any(missing_idx) || sum(!missing_idx) < 10) next

    # Identify discrete vs continuous missing
    missing_runs <- rle(missing_idx)
    max_run <- max(missing_runs$lengths[missing_runs$values])

    if (max_run <= 3) {
      # Discrete missing: Loess
      x <- seq_len(nrow(sub))
      valid <- !missing_idx

      loess_fit <- tryCatch(
        stats::loess(y[valid] ~ x[valid], span = 0.3),
        error = function(e) NULL
      )

      if (!is.null(loess_fit)) {
        y_pred <- stats::predict(loess_fit, x)
        # Fix negative predictions
        y_pred[y_pred < 0] <- pmax(0, stats::median(y[valid], na.rm = TRUE))
        dt[idx[missing_idx], daily_feed_g := y_pred[missing_idx]]
        dt[idx[missing_idx], is_imputed_feed := TRUE]
      }
    } else {
      # Continuous missing: Linear regression extrapolation with FCR QC
      sub_out <- .extrapolate_feed_with_fcr_v2(sub, config)
      
      # 严格保护原有数值：仅更新之前为 NA 的位置
      # 只回填输出契约列（issue #8）：sub_out 携带的 cum_feed/weight_kg/
      # pred_* 是拟合中间列，原实现把整表列名写回 dt 会污染输出 schema
      cols_to_update <- intersect(c("daily_feed_g", "is_imputed_feed"), names(sub_out))
      
      # 识别原本就是 NA 的行（在当前个体 sub 中的索引）
      orig_na_in_sub <- which(is.na(sub$daily_feed_g))
      
      if (length(orig_na_in_sub) > 0) {
        # 只取 sub_out 中原本是 NA 的那些行，并同步只保留契约列
        # （多列 := 按位置配对，RHS 列数必须与 cols_to_update 一致）
        sub_to_apply <- sub_out[orig_na_in_sub, ..cols_to_update]
        # 更新到原 dt 对应的全局索引位置
        dt[idx[orig_na_in_sub], (cols_to_update) := sub_to_apply]
      }
    }

    # 兜底：插补失败残留的 NA，用该个体中位数填补，避免 sum(na.rm=TRUE) 时被当作 0
    current_val <- dt$daily_feed_g[idx]
    still_na <- which(is.na(current_val))
    if (length(still_na) > 0) {
      med_val <- stats::median(current_val, na.rm = TRUE)
      if (!is.na(med_val)) {
        dt[idx[still_na], daily_feed_g := med_val]
        dt[idx[still_na], is_imputed_feed := TRUE]
      }
    }
  }

  dt
}

#' Extrapolate feed with FCR quality control (CORRECTED VERSION)
#'
#' CRITICAL FIX: Determines fitting range based on weight range of missing interval,
#' not position percentage.
#'
#' Red-header file specification (Section 3.3.2):
#' - If missing interval is in start phase (30-45kg): use last 10kg of missing interval
#' - If missing interval is in end phase (110-120kg): use first 15kg of missing interval
#'
#' @keywords internal
.extrapolate_feed_with_fcr_v2 <- function(sub_data, config) {
  dt <- data.table::copy(sub_data)

  # Calculate cumulative feed and weight in kg
  dt[, cum_feed := cumsum(ifelse(is.na(daily_feed_g), 0, daily_feed_g))]
  dt[, weight_kg := daily_weight_g / 1000]

  # Identify missing interval
  missing_idx <- is.na(dt$daily_feed_g)

  if (!any(missing_idx)) return(dt)

  # CRITICAL FIX: Determine weight range of missing interval
  missing_weight_range <- range(dt$weight_kg[missing_idx], na.rm = TRUE)

  if (all(is.na(missing_weight_range)) || all(is.infinite(missing_weight_range))) {
    return(dt)
  }

  # Determine fitting strategy based on missing interval weight range
  fit_range <- rep(FALSE, nrow(dt))

  if (missing_weight_range[2] <= 45) {
    # Start phase missing (30-45kg): use last 10kg of missing interval
    # "始测阶段(30—45kg)缺失以缺失区间后段10kg体重范围内的数据拟合"
    fit_start <- max(35, missing_weight_range[1])
    fit_end <- 45
    fit_range <- dt$weight_kg >= fit_start & dt$weight_kg <= fit_end & !is.na(dt$daily_feed_g)

  } else if (missing_weight_range[1] >= 110) {
    # End phase missing (110-120kg): use first 15kg of missing interval
    # "结测阶段（110—120kg）缺失以缺失区间前段15kg体重范围内的数据"
    fit_start <- 110
    fit_end <- min(125, missing_weight_range[2])
    fit_range <- dt$weight_kg >= fit_start & dt$weight_kg <= fit_end & !is.na(dt$daily_feed_g)

  } else {
    # Middle phase: use adjacent non-missing data
    # Find first and last valid indices
    valid_idx <- which(!missing_idx)
    if (length(valid_idx) < 5) return(dt)

    # Use data before and after missing interval
    first_missing <- min(which(missing_idx))
    last_missing <- max(which(missing_idx))

    before_idx <- valid_idx[valid_idx < first_missing]
    after_idx <- valid_idx[valid_idx > last_missing]

    # Take last 5 before and first 5 after
    fit_idx <- c(
      if (length(before_idx) > 0) tail(before_idx, 5) else integer(0),
      if (length(after_idx) > 0) head(after_idx, 5) else integer(0)
    )

    fit_range[fit_idx] <- TRUE
  }

  fit_data <- dt[fit_range]

  if (nrow(fit_data) < 5) return(dt)

  # Check FCR in weight stages (Table 2)
  fcr_valid <- .check_fcr_stages_v2(fit_data, config)
  if (!fcr_valid) {
    dt[, flag_fcr_stage_invalid := TRUE]
    return(dt)
  }

  # Fit linear regression: cum_feed ~ weight
  lm_fit <- tryCatch(
    stats::lm(cum_feed ~ weight_kg, data = fit_data),
    error = function(e) NULL
  )

  if (is.null(lm_fit)) return(dt)

  # Check R² > 0.95
  r2 <- summary(lm_fit)$r.squared
  if (r2 < 0.95) {
    dt[, flag_low_r2 := TRUE]
    return(dt)
  }

  # Predict cumulative feed
  dt[, pred_cum_feed := stats::predict(lm_fit, newdata = .SD)]

  # Calculate daily feed from cumulative (with negative value protection)
  dt[, pred_daily_feed := c(pred_cum_feed[1], diff(pred_cum_feed))]

  # Fix negative daily feed values: replace with positive interpolation
  if (any(dt$pred_daily_feed < 0, na.rm = TRUE)) {
    # Use median of valid daily feed for negative predictions
    valid_daily_feed <- dt$daily_feed_g[!is.na(dt$daily_feed_g) & dt$daily_feed_g >= 0]
    if (length(valid_daily_feed) > 0) {
      replacement_value <- stats::median(valid_daily_feed, na.rm = TRUE)
      dt[pred_daily_feed < 0, pred_daily_feed := replacement_value]
    } else {
      # Fallback: use a reasonable minimum value
      dt[pred_daily_feed < 0, pred_daily_feed := 100]  # 100g minimum daily feed
    }
  }

  # Fill missing
  dt[missing_idx, daily_feed_g := pred_daily_feed[missing_idx]]
  dt[missing_idx, is_imputed_feed := TRUE]

  dt
}

#' Check FCR in weight stages (Table 2)
#' @keywords internal
.check_fcr_stages_v2 <- function(data, config) {
  dt <- data.table::copy(data)

  # Get FCR ranges from config
  if (!"fcr_ranges" %in% names(config$national_standard)) {
    return(TRUE)  # Skip check if no ranges defined
  }

  fcr_ranges <- data.table::as.data.table(
    data.table::copy(config$national_standard$fcr_ranges)
  )

  # Assign weight stages
  dt[, weight_stage := cut(
    weight_kg,
    breaks = c(fcr_ranges$weight_min, max(fcr_ranges$weight_max)),
    labels = paste0("[", fcr_ranges$weight_min, ",", fcr_ranges$weight_max, ")"),
    include.lowest = TRUE,
    right = FALSE
  )]

  # Calculate FCR for each stage
  stage_fcr <- dt[!is.na(weight_stage), .(
    total_feed = sum(daily_feed_g, na.rm = TRUE),
    weight_gain = max(weight_kg, na.rm = TRUE) - min(weight_kg, na.rm = TRUE)
  ), by = weight_stage]

  stage_fcr[, fcr := total_feed / (weight_gain * 1000)]  # Convert kg to g

  # Merge with ranges
  stage_fcr[, stage_label := as.character(weight_stage)]
  fcr_ranges[, stage_label := paste0("[", weight_min, ",", weight_max, ")")]

  stage_fcr <- merge(stage_fcr, fcr_ranges[, .(stage_label, fcr_min, fcr_max)],
                     by = "stage_label", all.x = TRUE)

  # Check if all stages are within range
  all(stage_fcr$fcr >= stage_fcr$fcr_min & stage_fcr$fcr <= stage_fcr$fcr_max, na.rm = TRUE)
}

#' National standard imputation wrapper
#'
#' @param daily_records Daily-level data.table with weight and feed columns.
#' @param config Optional configuration list. If NULL, uses default national_standard config.
#' @return Data.table with imputed weight and feed values.
#' @export
ZhenM_impute_national <- function(daily_records, config = NULL) {
  if (is.null(config)) {
    config <- ZhenM_default_config("national_standard")
  }

  # First impute weight if needed
  dt <- ZhenM_impute_weight(daily_records, "national_standard", config)

  # Then impute feed
  dt <- .impute_feed_national_v2(dt, config)

  dt
}
