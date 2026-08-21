#' Feed QC for standard-level records
#'
#' 对标准记录（standard_data）进行采食量质控，标记异常记录
#'
#' @param standard_records Standard-level data
#' @param qc_method "national_standard" or "legacy"
#' @param config Configuration list
#' @param logger Optional logger object for detailed logging
#' @return QC'd data with flags (is_outlier_feed)
#' @keywords internal
ZhenM_qc_feed_standard <- function(standard_records, qc_method = "national_standard", config = NULL, logger = NULL) {
  cfg <- ZhenM_merge_config(config, qc_method)
  
  if (qc_method == "national_standard") {
    .qc_feed_standard_national(standard_records, cfg, logger)
  } else {
    .qc_feed_standard_legacy(standard_records, cfg, logger)
  }
}

#' National standard feed QC for standard records
#' @keywords internal
.qc_feed_standard_national <- function(standard_records, cfg, logger = NULL) {
  dt <- data.table::as.data.table(data.table::copy(standard_records))
  
  # Create logger helpers
  loggers <- .create_logger_helpers(logger)
  log_info <- loggers$log_info
  log_detail <- loggers$log_detail
  log_subsection <- loggers$log_subsection
  
  log_subsection("Phase 3: Feed QC (National Standard) - Standard Records")
  
  n_total <- nrow(dt)
  log_detail(paste0("Feed QC input records: ", n_total))
  
  # Initialize flags
  dt[, `:=`(
    flag_feed_negative = FALSE,
    flag_feed_too_high = FALSE,
    flag_duration_negative = FALSE,
    flag_duration_too_long = FALSE,
    flag_duration_zero_with_feed = FALSE,
    flag_speed_too_slow = FALSE,
    flag_speed_too_fast = FALSE,
    flag_speed_extreme_low_feed = FALSE,
    flag_speed_zero_long_duration = FALSE
  )]
  
  # 1. Duration negative
  dt[!is.na(duration_sec), flag_duration_negative := duration_sec < 0]
  
  # 2. Duration too long
  dt[!is.na(duration_sec), flag_duration_too_long :=
       duration_sec > cfg$national_standard$duration_max]
  
  log_detail(paste0("Max feeding duration threshold: ", cfg$national_standard$duration_max, " s"))
  
  # 3. Duration zero with feed
  dt[!is.na(duration_sec) & !is.na(feed_g),
     flag_duration_zero_with_feed := duration_sec == 0 & feed_g > 0]
  
  # Calculate speed (g/min)
  dt[, feed_speed := ifelse(duration_sec > 0, feed_g / (duration_sec / 60), NA_real_)]
  
  # 4. Speed too slow
  dt[!is.na(feed_speed), flag_speed_too_slow :=
       feed_speed <= cfg$national_standard$speed_min]
  
  log_detail(paste0("Min feeding speed threshold: ", cfg$national_standard$speed_min, " g/min"))
  
  # 5. Speed too fast
  dt[!is.na(feed_speed) & !is.na(feed_g),
     flag_speed_too_fast := feed_speed > cfg$national_standard$speed_max &
       feed_g >= cfg$national_standard$feed_extreme_threshold]
  
  log_detail(paste0("Max feeding speed threshold: ", cfg$national_standard$speed_max, " g/min"))
  
  # 6. Speed extreme with low feed
  dt[!is.na(feed_speed) & !is.na(feed_g),
     flag_speed_extreme_low_feed := feed_speed > cfg$national_standard$speed_extreme &
       feed_g > 0 & feed_g < cfg$national_standard$feed_extreme_threshold]
  
  # 7. Speed zero with long duration
  dt[!is.na(feed_speed) & !is.na(duration_sec),
     flag_speed_zero_long_duration := feed_speed == 0 & duration_sec > 500]
  

  # 9. Feed negative
  dt[!is.na(feed_g), flag_feed_negative := feed_g < 0]

  # 10. Feed too high (quantile regression - simplified)
  # For each individual, use daily aggregated feed intake to set P99
  if ("weight_g" %in% names(dt)) {
    # First calculate daily total feed intake
    daily_feed <- dt[, .(daily_total_feed = sum(feed_g, na.rm = TRUE)),
                     by = .(animal_id, record_date)]
    daily_feed[, daily_threshold_feed := quantile(daily_total_feed, 0.99, na.rm = TRUE), by = animal_id]

    # Merge back to original data
    dt <- merge(dt, daily_feed[, .(animal_id, record_date, daily_threshold_feed)],
                by = c("animal_id", "record_date"), all.x = TRUE)

    # Flag if single feed record exceeds the individual's daily P99
    dt[!is.na(feed_g) & !is.na(daily_threshold_feed),
       flag_feed_too_high := feed_g > daily_threshold_feed]

    dt[, daily_threshold_feed := NULL]
  } else {
    # If no weight data, just use P99 of individual single feed intakes
    dt[!is.na(feed_g), flag_feed_too_high := feed_g > quantile(feed_g, 0.99, na.rm = TRUE), by = animal_id]
  }

  # Mark any anomaly
  dt[, is_outlier_feed := flag_feed_negative | flag_feed_too_high |
       flag_duration_negative | flag_duration_too_long |
       flag_duration_zero_with_feed | flag_speed_too_slow |
       flag_speed_too_fast | flag_speed_extreme_low_feed |
       flag_speed_zero_long_duration]
  
  # Log outlier statistics
  n_feed_negative <- sum(dt$flag_feed_negative, na.rm = TRUE)
  n_feed_too_high <- sum(dt$flag_feed_too_high, na.rm = TRUE)
  n_duration_negative <- sum(dt$flag_duration_negative, na.rm = TRUE)
  n_duration_too_long <- sum(dt$flag_duration_too_long, na.rm = TRUE)
  n_duration_zero <- sum(dt$flag_duration_zero_with_feed, na.rm = TRUE)
  n_speed_slow <- sum(dt$flag_speed_too_slow, na.rm = TRUE)
  n_speed_fast <- sum(dt$flag_speed_too_fast, na.rm = TRUE)
  n_speed_extreme <- sum(dt$flag_speed_extreme_low_feed, na.rm = TRUE)
  n_speed_zero_long <- sum(dt$flag_speed_zero_long_duration, na.rm = TRUE)
  n_total_outlier <- sum(dt$is_outlier_feed, na.rm = TRUE)
  
  log_detail(paste0("flag_feed_negative: ", n_feed_negative))
  log_detail(paste0("flag_feed_too_high: ", n_feed_too_high))
  log_detail(paste0("flag_duration_negative: ", n_duration_negative))
  log_detail(paste0("flag_duration_too_long: ", n_duration_too_long))
  log_detail(paste0("flag_duration_zero_with_feed: ", n_duration_zero))
  log_detail(paste0("flag_speed_too_slow: ", n_speed_slow))
  log_detail(paste0("flag_speed_too_fast: ", n_speed_fast))
  log_detail(paste0("flag_speed_extreme_low_feed: ", n_speed_extreme))
  log_detail(paste0("flag_speed_zero_long_duration: ", n_speed_zero_long))
  log_info(paste0("Feed QC completed: Total outliers flagged = ", n_total_outlier, " (", round(n_total_outlier/n_total*100, 2), "%)"))
  
  message(sprintf("Feed QC (National-Standard): feed_negative=%d, feed_too_high=%d, duration_negative=%d, duration_too_long=%d, duration_zero=%d, speed_slow=%d, speed_fast=%d, speed_extreme=%d, speed_zero_long=%d. Total outliers=%d", 
                  n_feed_negative, n_feed_too_high, 
                  n_duration_negative, n_duration_too_long, n_duration_zero,
                  n_speed_slow, n_speed_fast, n_speed_extreme,
                  n_speed_zero_long, n_total_outlier))
  dt
}

#' Legacy feed QC for standard records
#' @keywords internal
.qc_feed_standard_legacy <- function(standard_records, cfg, logger = NULL) {
  dt <- data.table::copy(standard_records)
  feed_range <- cfg$legacy$feed_intake_range
  
  # Create logger helpers
  loggers <- .create_logger_helpers(logger)
  log_info <- loggers$log_info
  log_detail <- loggers$log_detail
  log_subsection <- loggers$log_subsection
  
  log_subsection("Phase 3: Feed QC (Legacy) - Standard Records")
  
  n_total <- nrow(dt)
  log_detail(paste0("Feed QC input records: ", n_total))
  
  # Out of range check
  dt[, flag_feed_out_of_range := FALSE]
  dt[!is.na(feed_g), flag_feed_out_of_range := 
       feed_g < feed_range[1] | feed_g > feed_range[2]]
  
  log_detail(paste0("Feed intake range threshold: [", feed_range[1], ", ", feed_range[2], "] g"))
  
  # Percentile method
  dt[, `:=`(
    flag_percentile_low = FALSE,
    flag_percentile_high = FALSE
  )]
  
  p_low <- cfg$legacy$feed_percentile_low
  p_high <- cfg$legacy$feed_percentile_high
  log_detail(paste0("Percentile thresholds: Low P", p_low * 100, ", High P", (1 - p_high) * 100))
  
  # Calculate daily total feed intake
  daily_feed <- dt[, .(daily_total_feed = sum(feed_g, na.rm = TRUE)), 
                   by = .(animal_id, record_date)]
  
  # Apply percentile detection on daily totals
  daily_feed[!is.na(daily_total_feed), `:=`(
    is_low_outlier = daily_total_feed < quantile(daily_total_feed, p_low, na.rm = TRUE),
    is_high_outlier = daily_total_feed > quantile(daily_total_feed, 1 - p_high, na.rm = TRUE)
  ), by = animal_id]
  daily_feed[, is_outlier_day := is_low_outlier | is_high_outlier]

  # Merge back to original data, mark all records of anomalous days
  dt <- merge(dt, daily_feed[, .(animal_id, record_date, is_outlier_day, is_low_outlier, is_high_outlier)],
              by = c("animal_id", "record_date"), all.x = TRUE)

  dt[is_low_outlier == TRUE, flag_percentile_low := TRUE]
  dt[is_high_outlier == TRUE, flag_percentile_high := TRUE]
  
  # Initialize STL flags
  dt[, flag_STL_FI := FALSE]
  
  # STL-based feed anomaly detection (Advanced QC)
  if (cfg$legacy$use_stl && "min_obs_for_ts" %in% names(cfg$legacy)) {
    log_detail("Executing advanced STL time series feed QC...")
    
    # Aggregate to daily level
    daily_for_stl <- dt[, .(daily_feed_g = sum(feed_g, na.rm = TRUE)), 
                        by = .(animal_id, record_date)]
    data.table::setorder(daily_for_stl, animal_id, record_date)
    
    ids <- unique(daily_for_stl$animal_id)
    stl_processed <- 0
    
    for (id in ids) {
      sub <- daily_for_stl[animal_id == id]
      if (nrow(sub) < cfg$legacy$min_obs_for_ts) next
      
      y <- sub$daily_feed_g
      if (sum(!is.na(y)) < cfg$legacy$min_obs_for_ts) next
      
      ts_obj <- tryCatch(
        stats::ts(y, frequency = 7),
        error = function(e) NULL
      )
      
      if (is.null(ts_obj)) next
      
      stl_fit <- tryCatch(
        stats::stl(ts_obj, s.window = "periodic", robust = TRUE),
        error = function(e) NULL
      )
      
      if (!is.null(stl_fit)) {
        resid <- stl_fit$time.series[, "remainder"]
        mad_val <- stats::mad(resid, na.rm = TRUE)
        if (mad_val > 0) {
          outliers <- abs(resid) > 3 * mad_val
          outlier_dates <- sub$record_date[outliers]
          
          # Flag anomalous dates back to standard records
          dt[animal_id == id & record_date %in% outlier_dates, flag_STL_FI := TRUE]
          stl_processed <- stl_processed + 1
        }
      }
    }
    
    log_detail(paste0("STL QC processed individuals: ", stl_processed))
    n_stl <- sum(dt$flag_STL_FI, na.rm = TRUE)
    log_detail(paste0("flag_STL_FI (STL timeseries outlier): ", n_stl))
  }
  
  dt[, is_outlier_feed := flag_feed_out_of_range | flag_percentile_low | 
       flag_percentile_high | flag_STL_FI]
  
  # Clean temporary variables
  dt[, is_outlier_day := NULL]
  
  n_out_of_range <- sum(dt$flag_feed_out_of_range, na.rm = TRUE)
  n_percentile <- sum(dt$flag_percentile_low | dt$flag_percentile_high, na.rm = TRUE)
  n_total_outlier <- sum(dt$is_outlier_feed, na.rm = TRUE)
  
  log_detail(paste0("flag_feed_out_of_range: ", n_out_of_range))
  log_detail(paste0("flag_percentile (Daily feed anomaly): ", n_percentile))
  log_info(paste0("Feed QC completed: Total outliers flagged = ", n_total_outlier, " (", round(n_total_outlier/n_total*100, 2), "%)"))
  
  message(sprintf("Feed QC (Legacy-Standard): out_of_range=%d, percentile=%d, STL=%d. Total outliers=%d",
                  n_out_of_range, n_percentile, sum(dt$flag_STL_FI, na.rm = TRUE), n_total_outlier))
  
  dt
}
