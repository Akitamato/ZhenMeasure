#' Weight QC for standard-level records
#'
#' Perform weight quality control on standard records (standard_data) and flag abnormal records.
#'
#' @details
#' 单轮 RLM 拟合 + 双阈值判定（issue #14 还原原始设计）：
#' \itemize{
#'   \item 记录级：RLM 权重 <= \code{weight_threshold}（默认 0.25）的单条记录判异常（\code{flag_weight_low}）；
#'   \item 日级：当日有效记录数 >= 2 且全部记录的 RLM 权重 < \code{daily_weight_threshold}（默认 0.90）时整日判异常（\code{flag_daily_weight_low}）；单记录天不参与日级共识。
#' }
#'
#' @param standard_records Standard-level data
#' @param qc_method QC method: "national_standard" (the only supported method since V1.0.0).
#' @param config Configuration list
#' @param logger Optional logger object for detailed logging
#' @return QC'd data with flags (is_outlier_wt)
#' @keywords internal
ZhenM_qc_weight_standard <- function(standard_records, qc_method = "national_standard", config = NULL, logger = NULL) {
  cfg <- ZhenM_merge_config(config, qc_method)
  .qc_weight_standard_national(standard_records, cfg, logger)
}

# Apply start/end test-weight-range filtering after weight QC.
# Keep behavior consistent with historical overall QC: remove whole animals when range fails.
.apply_test_weight_range_filter <- function(dt, cfg, log_detail, method_label = "", daily_weight_col = NULL) {
  test_weight_range <- cfg$national_standard$test_weight_range %||% c(45, 110)

  removed_weight_range_records <- 0L
  removed_weight_range_animals <- 0L
  removed_ids_weight <- character()

  source_col <- if (is.null(daily_weight_col)) "weight_g" else daily_weight_col

  if (nrow(dt) > 0 && source_col %in% names(dt)) {
    n_before_animals <- data.table::uniqueN(dt$animal_id)
    data.table::setorder(dt, animal_id, record_date)

    weight_stats <- dt[!is.na(get(source_col)), .(first_date = min(record_date), last_date = max(record_date)), by = animal_id]
    daily_weight <- dt[!is.na(get(source_col)), .(daily_weight_g = stats::median(get(source_col), na.rm = TRUE)), by = .(animal_id, record_date)]

    animal_weight_range <- merge(weight_stats, daily_weight,
      by.x = c("animal_id", "first_date"),
      by.y = c("animal_id", "record_date"),
      all.x = TRUE
    )
    data.table::setnames(animal_weight_range, "daily_weight_g", "start_weight_kg")
    animal_weight_range[, start_weight_kg := start_weight_kg / 1000]

    animal_weight_range <- merge(animal_weight_range, daily_weight,
      by.x = c("animal_id", "last_date"),
      by.y = c("animal_id", "record_date"),
      all.x = TRUE
    )
    data.table::setnames(animal_weight_range, "daily_weight_g", "end_weight_kg")
    animal_weight_range[, end_weight_kg := end_weight_kg / 1000]

    animal_weight_range[, is_valid_range := !is.na(start_weight_kg) & !is.na(end_weight_kg) &
      start_weight_kg <= test_weight_range[1] & end_weight_kg >= test_weight_range[2]]
    invalid_animals <- animal_weight_range[is_valid_range == FALSE, animal_id]
    removed_ids_weight <- invalid_animals

    log_detail(paste0(
      "Post-weight-QC test range check [", method_label,
      "] using ", source_col,
      ": start <= ", test_weight_range[1], "kg, end >= ", test_weight_range[2], "kg"
    ))

    if (length(invalid_animals) > 0) {
      n_before <- nrow(dt)
      dt <- dt[!animal_id %in% invalid_animals]
      removed_weight_range_records <- n_before - nrow(dt)
      removed_weight_range_animals <- n_before_animals - data.table::uniqueN(dt$animal_id)

      log_detail(paste0(
        "Post-weight-QC test range removed animals: ", length(invalid_animals),
        " (Removed records: ", removed_weight_range_records, ")"
      ))
      if (length(invalid_animals) <= 20) {
        log_detail(paste0("Filtered IDs: ", paste(invalid_animals, collapse = ", ")))
      } else {
        log_detail(paste0("Filtered IDs (first 20): ", paste(head(invalid_animals, 20), collapse = ", "), ", ..."))
      }
    } else {
      log_detail("All animals meet post-weight-QC test weight range requirements")
    }
  } else {
    log_detail(paste0("Skipping post-weight-QC test range check: data is empty or missing ", source_col, " column"))
  }

  list(
    dt = dt,
    removed_records = removed_weight_range_records,
    removed_animals = removed_weight_range_animals,
    removed_ids = removed_ids_weight
  )
}

#' National standard weight QC for standard records
#' @keywords internal
.qc_weight_standard_national <- function(standard_records, cfg, logger = NULL) {
  dt <- data.table::as.data.table(data.table::copy(standard_records))
  
  # Create logger helpers
  loggers <- .create_logger_helpers(logger)
  log_info <- loggers$log_info
  log_detail <- loggers$log_detail
  log_subsection <- loggers$log_subsection
  
  log_subsection("Phase 2: Weight QC (National Standard) - Standard Records")
  
  n_total <- nrow(dt)
  n_animals <- data.table::uniqueN(dt$animal_id)
  
  log_detail(paste0("Weight QC input records: ", n_total, ", animals: ", n_animals))
  
  # Initialize QC flag columns
  dt <- .init_qc_flags(dt, method = "national_standard")
  dt[, flag_Gompertz_WT := FALSE]

  # Range check (Task 2.3)
  weight_range <- .normalize_weight_range(cfg$national_standard$weight_range)
  dt[!is.na(weight_g), flag_weight_out_of_range :=
       weight_g < weight_range[1] | weight_g > weight_range[2]]
  
  log_detail(paste0("Weight range threshold: [", weight_range[1], ", ", weight_range[2], "] g"))
  log_detail(paste0("RLM weight threshold (single record): ", cfg$national_standard$weight_threshold))
  log_detail(paste0("Minimum growth curve R²: ", cfg$national_standard$growth_curve_r2_min))
  
  # No need to pre-compute daily average weight, this will be calculated dynamically in the loop
  
  # Robust regression weight-based QC (Task 2.6)
  ids <- unique(dt$animal_id)
  pb <- .zhenm_progress(length(ids), "Weight QC (Standard)") # Progress bar
  
  rlm_processed_count <- 0
  rlm_skipped_count <- 0
  animals_to_delete <- character()
  
  for (id in ids) {
    .zhenm_progress_tick(pb)
    
    idx <- which(dt$animal_id == id)
    sub <- dt[idx]

    # Get all weight records and their dates (not daily average), add row index for exact matching
    weight_records <- sub[, .(row_idx = idx, record_date, weight_g)]
    data.table::setorder(weight_records, record_date)
    
    # Require at least 10 weight records
    if (sum(!is.na(weight_records$weight_g)) < 10) {
      rlm_skipped_count <- rlm_skipped_count + 1
      next
    }
    
    # ===== Step 1: Perform RLM fitting on all weight data of each animal to get weight values =====
    # Measurement days: counting from 1
    day <- seq_len(nrow(weight_records))
    y <- weight_records$weight_g
    
    # Perform robust regression on all weight records
    # Model: weight ~ day + day^2
    rlm_fit_1 <- .safe_rlm_fit(y, day, formula_type = "polynomial", maxit = 60)
    
    if (is.null(rlm_fit_1)) {
      rlm_skipped_count <- rlm_skipped_count + 1
      next
    }
    
    rlm_processed_count <- rlm_processed_count + 1
    
    # Calculate weight values (0-1) for each weight record
    valid <- !is.na(y) & !is.na(day)
    rlm_weights_1 <- rep(NA_real_, length(y))
    rlm_weights_1[valid] <- rlm_fit_1$w
    
    # ===== Step 2: 双阈值判定（issue #14：单轮 RLM + 记录级/日级两道阈值）=====
    # 记录级：w <= weight_threshold（0.25，~5σ）的单条记录判异常
    # 日级：当日 >=2 条有效记录的 w 全部 < daily_weight_threshold（0.90，~1.5σ）
    #       时整日判异常；单记录天无"全部一致"语义，不参与共识
    wt_threshold <- cfg$national_standard$weight_threshold
    wt_threshold_daily <- if (!is.null(cfg$national_standard$daily_weight_threshold)) {
      as.numeric(cfg$national_standard$daily_weight_threshold)
    } else 0.90
    weight_records[, flag_outlier_single := rlm_weights_1 <= wt_threshold]
    weight_records[, flag_low_daily := rlm_weights_1 < wt_threshold_daily]

    day_consensus <- weight_records[!is.na(rlm_weights_1),
      .(n_valid = .N, n_low = sum(flag_low_daily)), by = record_date]
    bad_dates <- day_consensus[n_valid >= 2 & n_low == n_valid, record_date]

    # Create cleaned weight column (rule 1 + rule 2 set to NA)
    weight_records[, cleaned_weight := weight_g]
    weight_records[flag_outlier_single == TRUE, cleaned_weight := NA_real_]
    weight_records[record_date %in% bad_dates, cleaned_weight := NA_real_]
    weight_records[, flag_daily_weight_low := record_date %in% bad_dates]
    
    # ===== Step 4: Calculate daily weight after handling abnormal data =====
    # Merge the rlm weight of each record into the table to ensure alignment with cleaned_weight
      weight_records[, rlm_weight := rlm_weights_1]

      # Weighted average: sum(cleaned_weight * rlm_weight) / sum(w)
      daily_weight_data <- weight_records[, {
        valid <- !is.na(cleaned_weight) & !is.na(rlm_weight)
        sw <- sum(cleaned_weight[valid] * rlm_weight[valid])
        sw_w <- sum(rlm_weight[valid])
        list(
          daily_weight = if (sw_w == 0) NA_real_ else sw / sw_w,
          n_records = sum(valid)
        )
      }, by = record_date]

    data.table::setorder(daily_weight_data, record_date)

    # Map the weighted average daily weight back to the original records
    date_daily_weight_map <- data.table::data.table(
      record_date = daily_weight_data$record_date,
      weighted_avg_weight_per_day = daily_weight_data$daily_weight
    )

    dt <- .map_daily_values_to_records(
      dt, idx,
      daily_data = date_daily_weight_map,
      value_col = "weighted_avg_weight_per_day",
      output_col = "weighted_avg_weight_per_day"
    )
    
    if (nrow(daily_weight_data) < 10 || sum(!is.na(daily_weight_data$daily_weight)) < 10) {
      animals_to_delete <- c(animals_to_delete, id)
      next
    }
    
    # Assign the weight values from step 1 directly back to the original standard records (exact match via row index)
    dt[weight_records$row_idx, rlm_weight := rlm_weights_1]
    dt[weight_records$row_idx, flag_weight_low := weight_records$flag_outlier_single]
    dt[weight_records$row_idx, flag_daily_weight_low := weight_records$flag_daily_weight_low]

    # ===== Step 6: Gompertz 生长曲线检查 (可选) =====
    if (isTRUE(cfg$national_standard$use_gompertz) &&
        "gompertz_min_obs" %in% names(cfg$national_standard) &&
        nrow(daily_weight_data) >= cfg$national_standard$gompertz_min_obs) {

      x_gomp <- seq_len(nrow(daily_weight_data))
      y_gomp <- daily_weight_data$daily_weight / 1000  # 转为 kg
      valid_gomp <- !is.na(y_gomp)

      if (sum(valid_gomp) >= cfg$national_standard$gompertz_min_obs) {
        gompertz_fit <- tryCatch({
          A_init <- max(y_gomp[valid_gomp], na.rm = TRUE) * 1.1
          B_init <- 2
          C_init <- 0.05

          stats::nls(y_gomp[valid_gomp] ~ A * exp(-B * exp(-C * x_gomp[valid_gomp])),
                     start = list(A = A_init, B = B_init, C = C_init),
                     control = stats::nls.control(
                       maxiter = cfg$national_standard$gompertz_maxiter,
                       warnOnly = TRUE))
        }, error = function(e) NULL)

        if (!is.null(gompertz_fit)) {
          pred <- predict(gompertz_fit, newdata = list(x = x_gomp[valid_gomp]))
          resid <- y_gomp[valid_gomp] - pred
          mad_val <- stats::mad(resid, na.rm = TRUE)

          if (mad_val > 0) {
            outliers_logical <- abs(resid) >
              cfg$national_standard$gompertz_mad_multiplier * mad_val
            outlier_dates <- daily_weight_data$record_date[valid_gomp][outliers_logical]

            dt[animal_id == id & record_date %in% outlier_dates,
               flag_Gompertz_WT := TRUE]
          }
        }
      }
    }
  }

  # If data is completely insufficient or RLM fails severely, mark all original data of the animal
  if (length(animals_to_delete) > 0) {
    dt[animal_id %in% animals_to_delete, flag_growth_curve_poor := TRUE]
  }
  
  dt[, is_outlier_wt := flag_weight_out_of_range | flag_weight_low | flag_daily_weight_low | flag_growth_curve_poor | flag_Gompertz_WT]

  range_filter <- .apply_test_weight_range_filter(
    dt,
    cfg,
    log_detail,
    method_label = "national_standard",
    daily_weight_col = "weighted_avg_weight_per_day"
  )
  dt <- range_filter$dt
  
  # Detailed statistics logs
  n_out_of_range <- sum(dt$flag_weight_out_of_range, na.rm = TRUE)
  n_weight_low <- sum(dt$flag_weight_low, na.rm = TRUE)
  n_daily_wt_low <- sum(dt$flag_daily_weight_low, na.rm = TRUE)
  n_growth_poor <- sum(dt$flag_growth_curve_poor, na.rm = TRUE)
  n_gompertz_wt <- sum(dt$flag_Gompertz_WT, na.rm = TRUE)
  n_total_outlier <- sum(dt$is_outlier_wt, na.rm = TRUE)

  log_detail(paste0("Animals successfully fitted with RLM model: ", rlm_processed_count))
  log_detail(paste0("Animals skipped for RLM model (insufficient data): ", rlm_skipped_count))
  log_detail(paste0("Animals deleted due to insufficient growth curve R²: ", length(animals_to_delete)))
  log_detail(paste0("flag_weight_out_of_range: ", n_out_of_range))
  log_detail(paste0("flag_weight_low (RLM weight too low): ", n_weight_low))
  log_detail(paste0("flag_daily_weight_low (all valid records of day below daily threshold): ", n_daily_wt_low))
  log_detail(paste0("flag_growth_curve_poor: ", n_growth_poor))
  log_detail(paste0("flag_Gompertz_WT: ", n_gompertz_wt))
  n_total_after_filter <- nrow(dt)
  outlier_pct <- if (n_total_after_filter == 0) 0 else round(n_total_outlier / n_total_after_filter * 100, 2)
  log_detail(paste0("post_weight_qc_test_range_removed_records: ", range_filter$removed_records,
                    ", removed_animals: ", range_filter$removed_animals))
  log_info(paste0("Weight QC complete: Total abnormal records flagged ", n_total_outlier, " (", outlier_pct, "%)"))

  message(sprintf("Weight QC (National-Standard): out_of_range=%d, weight_low=%d, daily_wt_low=%d, growth_curve_poor=%d, Gompertz=%d, test_range_removed=%d. Total outliers=%d",
                  n_out_of_range, n_weight_low, n_daily_wt_low,
                  n_growth_poor, n_gompertz_wt, range_filter$removed_records, n_total_outlier))

  dt
}

