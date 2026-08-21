#' Weight QC for standard-level records
#'
#' Perform weight quality control on standard records (standard_data) and flag abnormal records.
#'
#' @param standard_records Standard-level data
#' @param qc_method "national_standard" or "legacy"
#' @param config Configuration list
#' @param logger Optional logger object for detailed logging
#' @return QC'd data with flags (is_outlier_wt)
#' @keywords internal
ZhenM_qc_weight_standard <- function(standard_records, qc_method = "national_standard", config = NULL, logger = NULL) {
  cfg <- ZhenM_merge_config(config, qc_method)
  
  if (qc_method == "national_standard") {
    .qc_weight_standard_national(standard_records, cfg, logger)
  } else {
    .qc_weight_standard_legacy(standard_records, cfg, logger)
  }
}

# Apply start/end test-weight-range filtering after weight QC.
# Keep behavior consistent with historical overall QC: remove whole animals when range fails.
.apply_test_weight_range_filter <- function(dt, cfg, log_detail, method_label = "", daily_weight_col = NULL) {
  test_weight_range <- cfg$national_standard$test_weight_range %||%
    cfg$legacy$test_weight_range %||% c(45, 110)

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
    
    # ===== Step 2: Weight records with weight values below the threshold are considered abnormal and set to NA =====
    wt_threshold <- cfg$national_standard$weight_threshold
    weight_records[, flag_outlier_single := rlm_weights_1 <= wt_threshold]
    
    # ===== Step 3: If all weight values on a certain day are below the threshold, all weights on that day are considered abnormal and set to NA =====
    bad_dates <- .identify_bad_dates(weight_records, flag_col = "flag_outlier_single")
    
    # Create cleaned weight column (abnormal and all-day abnormal are set to NA)
    weight_records[, cleaned_weight := weight_g]
    weight_records[flag_outlier_single == TRUE, cleaned_weight := NA_real_]
    weight_records[record_date %in% bad_dates, cleaned_weight := NA_real_]
    
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
    
    # ===== Step 5: Perform regression fitting on the growth curve based on daily weight =====
    # Measurement days: counting from 1
    day_daily <- seq_len(nrow(daily_weight_data))
    y_daily <- daily_weight_data$daily_weight
    
    if (sum(!is.na(y_daily)) < 10) {
      animals_to_delete <- c(animals_to_delete, id)
      next
    }
    
    # Second RLM fit
    # Model: daily_weight ~ day + day^2
    rlm_fit_2 <- .safe_rlm_fit(y_daily, day_daily, formula_type = "polynomial", maxit = 60)

    if (is.null(rlm_fit_2)) {
      animals_to_delete <- c(animals_to_delete, id)
      next
    }

    # Get the weights of the second RLM fit
    valid_daily <- !is.na(y_daily) & !is.na(day_daily)
    rlm_weights_2 <- rep(NA_real_, length(y_daily))
    rlm_weights_2[valid_daily] <- rlm_fit_2$w

    # Mark dates where the second RLM weight is too low
    daily_weight_data[, rlm_weight_daily := rlm_weights_2]
    daily_weight_data[, flag_daily_low := rlm_weight_daily <= wt_threshold]

    # Map the weights and flags of the second RLM fit back to the original records
    date_daily_rlm_map <- data.table::data.table(
      record_date = daily_weight_data$record_date,
      rlm_weight_daily = daily_weight_data$rlm_weight_daily,
      flag_daily_low = daily_weight_data$flag_daily_low
    )

    dt <- .map_daily_values_to_records(
      dt, idx,
      daily_data = date_daily_rlm_map,
      value_col = "rlm_weight_daily",
      output_col = "rlm_weight_daily"
    )

    dt <- .map_daily_values_to_records(
      dt, idx,
      daily_data = date_daily_rlm_map,
      value_col = "flag_daily_low",
      output_col = "flag_daily_weight_low"
    )
    
    # Assign the weight values from step 1 directly back to the original standard records (exact match via row index)
    dt[weight_records$row_idx, rlm_weight := rlm_weights_1]
    dt[weight_records$row_idx, flag_weight_low := weight_records$flag_outlier_single]
  }
  
  # If data is completely insufficient or RLM fails severely, mark all original data of the animal
  if (length(animals_to_delete) > 0) {
    dt[animal_id %in% animals_to_delete, flag_growth_curve_poor := TRUE]
  }
  
  dt[, is_outlier_wt := flag_weight_out_of_range | flag_weight_low | flag_daily_weight_low | flag_growth_curve_poor]

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
  n_total_outlier <- sum(dt$is_outlier_wt, na.rm = TRUE)
  
  log_detail(paste0("Animals successfully fitted with RLM model: ", rlm_processed_count))
  log_detail(paste0("Animals skipped for RLM model (insufficient data): ", rlm_skipped_count))
  log_detail(paste0("Animals deleted due to insufficient growth curve R²: ", length(animals_to_delete)))
  log_detail(paste0("flag_weight_out_of_range: ", n_out_of_range))
  log_detail(paste0("flag_weight_low (RLM weight too low): ", n_weight_low))
  log_detail(paste0("flag_daily_weight_low (Daily RLM weight too low): ", n_daily_wt_low))
  log_detail(paste0("flag_growth_curve_poor: ", n_growth_poor))
  n_total_after_filter <- nrow(dt)
  outlier_pct <- if (n_total_after_filter == 0) 0 else round(n_total_outlier / n_total_after_filter * 100, 2)
  log_detail(paste0("post_weight_qc_test_range_removed_records: ", range_filter$removed_records,
                    ", removed_animals: ", range_filter$removed_animals))
  log_info(paste0("Weight QC complete: Total abnormal records flagged ", n_total_outlier, " (", outlier_pct, "%)"))
  
  message(sprintf("Weight QC (National-Standard): out_of_range=%d, weight_low=%d, daily_wt_low=%d, growth_curve_poor=%d, test_range_removed=%d. Total outliers=%d",
                  n_out_of_range, n_weight_low, n_daily_wt_low,
                  n_growth_poor, range_filter$removed_records, n_total_outlier))

  dt
}

#' Legacy weight QC for standard records
#' @keywords internal
.qc_weight_standard_legacy <- function(standard_records, cfg, logger = NULL) {
  dt <- data.table::as.data.table(data.table::copy(standard_records))
  
  # Create logger helpers
  loggers <- .create_logger_helpers(logger)
  log_info <- loggers$log_info
  log_detail <- loggers$log_detail
  log_subsection <- loggers$log_subsection
  
  log_subsection("Phase 2: Weight QC (Legacy) - Standard Records")
  
  n_total <- nrow(dt)
  n_animals <- data.table::uniqueN(dt$animal_id)
  log_detail(paste0("Weight QC input records: ", n_total, ", animals: ", n_animals))
  
  # Initialize QC flag columns
  dt <- .init_qc_flags(dt, method = "legacy")
  
  # Range check
  weight_range <- .normalize_weight_range(cfg$legacy$weight_range)
  dt[!is.na(weight_g), flag_weight_out_of_range :=
       weight_g < weight_range[1] | weight_g > weight_range[2]]
  
  log_detail(paste0("Weight range threshold: [", weight_range[1], ", ", weight_range[2], "] g"))
  
  # SD threshold method
  sd_threshold <- cfg$legacy$weight_sd_threshold
  log_detail(paste0("SD threshold multiplier: ", sd_threshold))
  
  dt[, `:=`(.mean_wt = mean(weight_g, na.rm = TRUE),
            .sd_wt = stats::sd(weight_g, na.rm = TRUE)), by = animal_id]
  dt[!is.na(weight_g) & !is.na(.sd_wt) & .sd_wt > 0, flag_SD_WT := abs(weight_g - .mean_wt) > sd_threshold * .sd_wt]
  dt[, `:=`(.mean_wt = NULL, .sd_wt = NULL)]
  
  # Calculate daily median weight for advanced QC
  dt[, median_weight_per_day := stats::median(weight_g, na.rm = TRUE), 
     by = .(animal_id, record_date)]
  
  # RLM robust regression (Advanced QC)
  if (cfg$legacy$use_rlm && "rlm_weight_thresh" %in% names(cfg$legacy)) {
    log_detail("Executing advanced RLM robust regression weight QC...")
    
    ids <- unique(dt$animal_id)
    rlm_processed <- 0
    
    for (id in ids) {
      idx <- which(dt$animal_id == id)
      sub <- dt[idx]
      
      # Get daily median weight data
      daily_sub <- unique(sub[, .(record_date, median_weight_per_day)], by = "record_date")
      data.table::setorder(daily_sub, record_date)
      
      if (nrow(daily_sub) < 10) next
      
      x <- as.numeric(daily_sub$record_date - min(daily_sub$record_date))
      y <- daily_sub$median_weight_per_day
      
      # Use safe RLM fit function (adopted from national_standard, replaced "linear" with "polynomial" model for better biological fit)
      rlm_fit <- .safe_rlm_fit(y, x, formula_type = "polynomial", maxit = cfg$legacy$rlm_maxit)
      
      if (!is.null(rlm_fit)) {
        valid <- !is.na(y) & !is.na(x)
        weights <- rep(NA_real_, length(y))
        weights[valid] <- rlm_fit$w
        
        # Create mapping from date to weight
        date_weight_map <- data.table::data.table(
          record_date = daily_sub$record_date,
          rlm_weight = weights
        )
        
        # Distribute weights to all standard records using a generic mapping function
        dt <- .map_daily_values_to_records(
          dt, idx,
          daily_data = date_weight_map,
          value_col = "rlm_weight",
          output_col = "rlm_weight"
        )
        
        # Mark records with weights below threshold
        outliers <- dt[idx, rlm_weight] < cfg$legacy$rlm_weight_thresh
        dt[idx, flag_RLM_WT := outliers]
        rlm_processed <- rlm_processed + 1
      }
    }
    
    log_detail(paste0("Animals processed by RLM QC: ", rlm_processed))
    log_detail(paste0("RLM weight threshold: ", cfg$legacy$rlm_weight_thresh))
    n_rlm <- sum(dt$flag_RLM_WT, na.rm = TRUE)
    log_detail(paste0("flag_RLM_WT (RLM robust regression outlier): ", n_rlm))
  }
  
  
  # Gompertz growth curve (Advanced QC)
  if (cfg$legacy$use_gompertz && "min_obs_for_wt" %in% names(cfg$legacy)) {
    log_detail("Executing advanced Gompertz growth curve weight QC...")
    
    ids <- unique(dt$animal_id)
    gompertz_processed <- 0
    
    for (id in ids) {
      idx <- which(dt$animal_id == id)
      sub <- dt[idx]
      
      # 获取每日的中位数体重数据
      daily_sub <- unique(sub[, .(record_date, median_weight_per_day)], by = "record_date")
      data.table::setorder(daily_sub, record_date)
      
      if (nrow(daily_sub) < cfg$legacy$min_obs_for_wt) next
      
      x <- seq_len(nrow(daily_sub))
      y <- daily_sub$median_weight_per_day / 1000  # 转换为 kg
      valid <- !is.na(y)
      
      if (sum(valid) < cfg$legacy$min_obs_for_wt) next
      
      # Simplified Gompertz: y = A * exp(-B * exp(-C * x))
      gompertz_fit <- tryCatch({
        A_init <- max(y[valid], na.rm = TRUE) * 1.1
        B_init <- 2
        C_init <- 0.05
        
        stats::nls(y[valid] ~ A * exp(-B * exp(-C * x[valid])),
                   start = list(A = A_init, B = B_init, C = C_init),
                   control = stats::nls.control(maxiter = 100, warnOnly = TRUE))
      }, error = function(e) NULL)
      
      if (!is.null(gompertz_fit)) {
        pred <- predict(gompertz_fit, newdata = list(x = x[valid]))
        resid <- y[valid] - pred
        
        mad_val <- stats::mad(resid, na.rm = TRUE)
        if (mad_val > 0) {
          outliers_logical <- abs(resid) > 4 * mad_val
          outlier_dates <- daily_sub$record_date[valid][outliers_logical]
          
          # Mark all records for abnormal dates
          dt[animal_id == id & record_date %in% outlier_dates, flag_Gompertz_WT := TRUE]
          gompertz_processed <- gompertz_processed + 1
        }
      }
    }
      
    log_detail(paste0("Animals processed by Gompertz QC: ", gompertz_processed))
    n_gompertz <- sum(dt$flag_Gompertz_WT, na.rm = TRUE)
    log_detail(paste0("flag_Gompertz_WT (Gompertz growth curve outlier): ", n_gompertz))
  }
    
  dt[, is_outlier_wt := flag_SD_WT | flag_weight_out_of_range | flag_RLM_WT | flag_Gompertz_WT]

  range_filter <- .apply_test_weight_range_filter(dt, cfg, log_detail, method_label = "legacy")
  dt <- range_filter$dt

  n_out_of_range <- sum(dt$flag_weight_out_of_range, na.rm = TRUE)
  n_sd_wt <- sum(dt$flag_SD_WT, na.rm = TRUE)
  n_rlm_wt <- sum(dt$flag_RLM_WT, na.rm = TRUE)
  n_gompertz_wt <- sum(dt$flag_Gompertz_WT, na.rm = TRUE)
  n_total_outlier_wt <- sum(dt$is_outlier_wt, na.rm = TRUE)
  
  log_detail(paste0("flag_weight_out_of_range: ", n_out_of_range))
  log_detail(paste0("flag_SD_WT (SD test): ", n_sd_wt))
  log_detail(paste0("flag_RLM_WT (RLM robust regression outlier): ", n_rlm_wt))
  log_detail(paste0("flag_Gompertz_WT (Gompertz growth curve outlier): ", n_gompertz_wt))
  n_total_after_filter <- nrow(dt)
  outlier_pct <- if (n_total_after_filter == 0) 0 else round(n_total_outlier_wt / n_total_after_filter * 100, 2)
  log_detail(paste0("post_weight_qc_test_range_removed_records: ", range_filter$removed_records,
                    ", removed_animals: ", range_filter$removed_animals))
  log_info(paste0("Weight QC complete: Total abnormal records flagged ", n_total_outlier_wt, " (", outlier_pct, "%)"))
  
  message(sprintf("Weight QC (Legacy-Standard): out_of_range=%d, SD_WT=%d, RLM=%d, Gompertz=%d, test_range_removed=%d. Total outliers=%d", 
                  n_out_of_range, n_sd_wt, n_rlm_wt, n_gompertz_wt, range_filter$removed_records, n_total_outlier_wt))
  dt
}
