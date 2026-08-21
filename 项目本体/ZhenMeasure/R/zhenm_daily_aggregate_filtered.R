#' Aggregate standard records to daily records with QC filtering
#'
#' Calculates daily aggregated data from quality-controlled standard records,
#' excluding weight and feed records flagged as anomalous.
#' Supports two input formats:
#' 1. Original format (from ZhenM_read_data): ID, AGE, DFI, Visit_time, Feed_intake, Weight, Duration, Location
#' 2. New format (from ZhenM_validate_standard_records): animal_id, record_date, feed_g, weight_g, etc.
#'
#' V0.2.2 Updates:
#' - Weight inheritance: Prioritizes pre-calculated daily weight metrics from Step 3 
#'   (weighted_avg_weight_per_day or median_weight_per_day), outputting as daily_weight_g
#' - Feed QC: If any record on a given day has flag_feed_out_of_range as TRUE, 
#'   the daily_feed_g for that day is set to NA.
#'
#' @param standard_records Standard-record-level data with QC flags
#' @return A daily-level table aggregated by animal and date with daily_weight_g and enhanced feed QC
#' @export
ZhenM_standard_to_daily_filtered <- function(standard_records) {
  if (!data.table::is.data.table(standard_records)) {
    standard_records <- data.table::as.data.table(standard_records)
  }
  
  dt <- data.table::copy(standard_records)
  
  # Detect input format and unify column names
  # Original format column mapping
  col_mapping <- list(
    animal_id = c("animal_id", "ID"),
    record_date = c("record_date"),  # Needs to be calculated from Visit_time
    weight = c("weight_g", "Weight"),
    feed = c("feed_g", "Feed_intake"),
    duration = c("duration_sec", "Duration"),
    age = c("age_day", "AGE"),
    daily_feed = c("daily_feed_g", "DFI"),
    location = c("location", "Location"),
    visit_time = c("start_time", "Visit_time"),
    end_time = c("end_time", "End_time")
  )
  
  # Find actually existing column names
  find_col <- function(candidates) {
    for (col_name in candidates) {
      if (col_name %in% names(dt)) return(col_name)
    }
    return(NULL)
  }
  
  id_col <- find_col(col_mapping$animal_id)
  wt_col <- find_col(col_mapping$weight)
  feed_col <- find_col(col_mapping$feed)
  dur_col <- find_col(col_mapping$duration)
  age_col <- find_col(col_mapping$age)
  dfi_col <- find_col(col_mapping$daily_feed)
  loc_col <- find_col(col_mapping$location)
  visit_col <- find_col(col_mapping$visit_time)
  
  if (is.null(id_col)) {
    stop("Cannot find animal ID column (animal_id or ID)", call. = FALSE)
  }
  
  # Create record_date (if it does not exist)
  if (!"record_date" %in% names(dt)) {
    if (!is.null(visit_col)) {
      dt[, record_date := ZhenM_safe_to_idate(get(visit_col))]
    } else {
      stop("Cannot determine record_date: no Visit_time or record_date column", call. = FALSE)
    }
  }
  
  # Check if pre-calculated daily weight metrics exist (computed in Step 3)
  has_national_weight <- "weighted_avg_weight_per_day" %in% names(dt)

  # Check if feed intake QC flags exist
  has_feed_out_of_range <- "flag_feed_out_of_range" %in% names(dt)

  # If no feed range flag exists, create default (all FALSE)
  if (!has_feed_out_of_range) {
    dt[, flag_feed_out_of_range := FALSE]
  }

  # Check if QC flag columns exist
  has_wt_qc <- "is_outlier_wt" %in% names(dt)
  has_feed_qc <- "is_outlier_feed" %in% names(dt)
  
  # If no QC flags exist, create defaults (all FALSE)
  if (!has_wt_qc) {
    dt[, is_outlier_wt := FALSE]
  }
  if (!has_feed_qc) {
    dt[, is_outlier_feed := FALSE]
  }

  # Check if weighted_avg_weight_per_day exists and if so, filter it directly before aggregation
  if (has_national_weight) {
      dt[is_outlier_wt == TRUE, weighted_avg_weight_per_day := NA_real_]
  }

  # Create filtered weight and feed columns
  if (!is.null(wt_col)) {
    dt[, weight_filtered := ifelse(is_outlier_wt == TRUE, NA_real_, get(wt_col))]
  } else {
    dt[, weight_filtered := NA_real_]
  }
  
  if (!is.null(feed_col)) {
    dt[, feed_filtered := ifelse(is_outlier_feed == TRUE, 0, get(feed_col))]
  } else {
    dt[, feed_filtered := 0]
  }
  
  # Get age column values
  get_age <- if (!is.null(age_col)) dt[[age_col]] else rep(NA_real_, nrow(dt))
  
  # Get duration column values
  get_dur <- if (!is.null(dur_col)) dt[[dur_col]] else rep(NA_real_, nrow(dt))
  
  # Get location column values
  get_loc <- if (!is.null(loc_col)) dt[[loc_col]] else rep(NA_character_, nrow(dt))
  
  # Store these values in dt for usage during aggregation
  dt[, .age_val := get_age]
  dt[, .dur_val := get_dur]
  dt[, .loc_val := get_loc]
  
  # Aggregate to daily level
  result <- dt[
    ,
    .(
      # Daily feed intake: Smart QC filtering logic
      daily_feed_g = if (any(flag_feed_out_of_range == TRUE, na.rm = TRUE)) {
        # Scenario 1: Has out-of-range records, set the whole day to NA (equipment failure)
        NA_real_
      } else {
        # Scenario 2: No out-of-range records, check for valid feed intakes
        valid_feed <- feed_filtered[feed_filtered > 0 & !is.na(feed_filtered)]
        if (length(valid_feed) == 0) {
          # All records for the day were flagged as anomalies (e.g., duration/speed issues), set to NA and wait for imputation
          NA_real_
        } else {
          # Has normal feed records, sum valid values
          sum(valid_feed, na.rm = TRUE)
        }
      },

      # Weight: Use pre-calculated weighted average weight from national standard method
      daily_weight_g = if (has_national_weight) {
        # National method: use pre-calculated weighted average weight
        # Exclude records where the weight is explicitly flagged as an outlier
        valid_ww <- weighted_avg_weight_per_day[is_outlier_wt == FALSE]
        if (length(valid_ww) > 0 && any(!is.na(valid_ww))) {
          data.table::first(stats::na.omit(valid_ww))
        } else {
          NA_real_
        }
      } else {
        # Fallback: recalculate from raw weight column
        if (all(is.na(weight_filtered))) {
          NA_real_
        } else {
          stats::median(weight_filtered, na.rm = TRUE)
        }
      },
      
      # Age: Take the first non-NA value of the day
      age_day = if (all(is.na(.age_val))) NA_real_ else data.table::first(stats::na.omit(.age_val)),
      
      # Visit count statistics
      visits_n = .N,
      visits_n_valid = sum(is_outlier_feed == FALSE, na.rm = TRUE),
      
      # Total duration: Filtered valid duration
      total_duration_sec = sum(ifelse(is_outlier_feed == FALSE, .dur_val, 0), na.rm = TRUE),
      
      # Location: Aggregation
      location = paste(unique(stats::na.omit(.loc_val)), collapse = ";"),

      # QC statistics
      n_outlier_wt = sum(is_outlier_wt == TRUE, na.rm = TRUE),
      n_outlier_feed = sum(is_outlier_feed == TRUE, na.rm = TRUE),
      has_feed_out_of_range_today = any(flag_feed_out_of_range == TRUE, na.rm = TRUE)
    ),
    by = c(id_col, "record_date")
  ]
  
  # Rename ID column to standard name
  if (id_col != "animal_id") {
    data.table::setnames(result, id_col, "animal_id")
  }
  
  data.table::setorder(result, animal_id, record_date)

  # ==== LMM Daily Feed Intake Correction Module ====
  # Reserve breed interface (if breed column exists)
  has_breed <- "breed" %in% names(dt)
  if (has_breed) {
    breed_map <- unique(dt[!is.na(breed), .(animal_id, breed)])
    if (nrow(breed_map) > 0) {
      result <- merge(result, breed_map, by = "animal_id", all.x = TRUE)
    }
  }

  result <- .apply_feed_lmm_correction(result, dt)
  # =================================================

  
  # Add attributes
  attr(result, "qc_filtered") <- TRUE
  attr(result, "source_format") <- if (id_col == "ID") "original" else "new"
  
  result
}

#' LMM Feed Intake Correction Engine
#' @keywords internal
.apply_feed_lmm_correction <- function(daily_dt, raw_dt) {
  dt <- data.table::copy(daily_dt)
  
  # 10 error flags for single record anomalies (including STL time series flag)
  err_flags <- c("flag_duration_negative", "flag_duration_too_long",
                 "flag_duration_zero_with_feed", "flag_speed_too_slow",
                 "flag_speed_too_fast", "flag_speed_extreme_low_feed",
                 "flag_speed_zero_long_duration", "flag_feed_negative",
                 "flag_feed_too_high", "flag_STL_FI")
  
  # ==== 1. Extract feed intake of normal records and anomaly occurrence flags ====
  # Mark "normal" records in raw_dt (i.e., all 9 error flags are FALSE AND not an outlier)
  raw_dt[, is_feed_normal_record := TRUE]
  # Exclude records already flagged as feed outliers by QC
  if ("is_outlier_feed" %in% names(raw_dt)) {
    raw_dt[is_outlier_feed == TRUE, is_feed_normal_record := FALSE]
  }
  for (flg in err_flags) {
    if (flg %in% names(raw_dt)) {
      raw_dt[get(flg) == TRUE, is_feed_normal_record := FALSE]
    }
  }
  
  # Aggregate daily occurrence of the 9 anomalies and sum of normal feed intake
  feed_col <- if ("feed_g" %in% names(raw_dt)) "feed_g" else if ("Feed_intake" %in% names(raw_dt)) "Feed_intake" else NULL
  
  if (is.null(feed_col)) {
    # Return dt directly if feed intake column is missing
    return(dt)
  }
  
  # We do not exclude flag_feed_out_of_range, retaining this rule
  daily_features <- raw_dt[, .(
    normal_feed_sum = sum(.SD[[feed_col]][is_feed_normal_record == TRUE], na.rm = TRUE)
  ), by = .(animal_id, record_date)]
  
  # Add daily occurrence flags for each feature
  for (flg in err_flags) {
    if (flg %in% names(raw_dt)) {
      # 用异常记录条数（次数）而非"是否发生"（布尔），使校正量与异常程度成比例
      flg_agg <- raw_dt[, .(flg_occurred = sum(get(flg) == TRUE, na.rm = TRUE)), by = .(animal_id, record_date)]
      data.table::setnames(flg_agg, "flg_occurred", paste0("has_", flg))
      daily_features <- merge(daily_features, flg_agg, by = c("animal_id", "record_date"), all.x = TRUE)
    } else {
      # Default to 0 if a flag is missing in the input
      daily_features[, paste0("has_", flg) := 0L]
    }
  }
  
  dt <- merge(dt, daily_features, by = c("animal_id", "record_date"), all.x = TRUE)
  
  # ==== 2. Validity interception for 0~6kg range ====
  # 6kg (6000g) 为猪只单日采食量生理上限。超过上限的天标记为 flag_daily_feed_over_limit，
  # 并将超出 [0, 6000] 范围的天置 NA（含 >6kg 超限与 <=0 过低）。
  dt[, flag_daily_feed_over_limit := !is.na(normal_feed_sum) & normal_feed_sum > 6000]
  dt[!is.na(normal_feed_sum) & (normal_feed_sum <= 0 | normal_feed_sum > 6000),
     normal_feed_sum := NA_real_]
  
  # ==== 3. Construct individual daily weight gain (Covariate) ====
  data.table::setorder(dt, animal_id, record_date)
  # Roughly estimate ADG using diff (lag 1 day difference)
  dt[, adg_g := c(NA, diff(daily_weight_g)), by = animal_id]
  # Cannot diff on the first day, pad with 0 to prevent training interference
  dt[is.na(adg_g), adg_g := 0]
  
  # ==== 4. LMM Preparation and Modeling ====
  if (requireNamespace("lme4", quietly = TRUE)) {
    # Only days with valid normal_feed_sum (not NA) are modeled as the dependent variable
    train_idx <- !is.na(dt$normal_feed_sum) & !is.na(dt$daily_weight_g) & !is.na(dt$adg_g)
    
    # Include Location as a fixed effect only if it exists and has > 1 unique value
    has_loc <- "location" %in% names(dt) && length(unique(stats::na.omit(dt$location))) > 1
    has_breed <- "breed" %in% names(dt) && length(unique(stats::na.omit(dt$breed))) > 1
    
    if (sum(train_idx) > 30) {
      train_data <- dt[train_idx]
      # Rescale large covariates (grams to kg) to avoid lme4 optimizer warning: "Some predictor variables are on very different scales"
      formula_str <- "normal_feed_sum ~ I(daily_weight_g / 1000) + I(adg_g / 1000)"
      
      if (has_loc) formula_str <- paste0(formula_str, " + location")
      if (has_breed) formula_str <- paste0(formula_str, " + breed")
      
      # Extract error types that actually varied in this dataset
      active_flags <- character()
      for (flg in err_flags) {
        has_flg_name <- paste0("has_", flg)
        # A valid feature must have both TRUE and FALSE cases in the training set
        if (length(unique(train_data[[has_flg_name]])) > 1) {
          # Convert to 0/1 integer for multiple regression
          dt[, (has_flg_name) := as.integer(get(has_flg_name))]
          formula_str <- paste0(formula_str, " + ", has_flg_name)
          active_flags <- c(active_flags, has_flg_name)
        }
      }
      
      formula_str <- paste0(formula_str, " + (1 | animal_id)")
      
      lmm_fit <- tryCatch({
        lme4::lmer(as.formula(formula_str), data = dt[train_idx])
      }, error = function(e) {
        warning("LMM model failed to converge or encountered an error. Falling back to uncorrected daily feed. Details: ", e$message)
        NULL
      })
      
      if (!is.null(lmm_fit)) {
        # Extract fixed effect coefficients
        fixed_eff <- lme4::fixef(lmm_fit)
        
        # Calculate daily correction value for each record
        # correction = sum_{active_flags} ( - beta_i * I_flag_i)
        dt[, correction_g := 0]
        
        for (i in seq_along(active_flags)) {
          flg_name <- active_flags[i]
          if (flg_name %in% names(fixed_eff)) {
            beta_val <- fixed_eff[flg_name]
            dt[, correction_g := correction_g - beta_val * get(flg_name)]
          }
        }
        
        # Add correction to the normal daily feed intake
        dt[!is.na(normal_feed_sum), daily_feed_g_corrected := normal_feed_sum + correction_g]
        
        # Log the number of successfully corrected daily records (where absolute correction > 0)
        n_corrected <- sum(abs(dt$correction_g) > 0.001 & !is.na(dt$daily_feed_g_corrected), na.rm = TRUE)
        message(sprintf("LMM Feed Correction: Successfully corrected %d daily records.", n_corrected))
        
        # Overwrite the main daily feed_g variable
        # Negative or zero corrected values are treated as invalid → set to NA for imputation
        dt[, daily_feed_g := daily_feed_g_corrected]
        dt[daily_feed_g <= 0 & !is.na(daily_feed_g), daily_feed_g := NA_real_]
        dt[, c("correction_g", "daily_feed_g_corrected") := NULL]
        
      } else {
        message("LMM Feed Correction: Model fitting failed or skipped, 0 records corrected.")
        dt[, daily_feed_g := normal_feed_sum]
      }
    } else {
      # If training samples are too few, use initial normal_feed_sum directly without LMM inference
      message(sprintf("LMM Feed Correction: Insufficient valid samples for training (%d <= 30), 0 records corrected.", sum(train_idx)))
      dt[, daily_feed_g := normal_feed_sum]
    }
  } else {
    warning("Package 'lme4' is not installed. Ignoring LMM feed correction. Proceeding with raw normal daily sum.")
    dt[, daily_feed_g := normal_feed_sum]
  }
  
  # Clean temporary feature columns used in the process
  cols_to_remove <- c("normal_feed_sum", "adg_g", paste0("has_", err_flags))
  dt[, (cols_to_remove) := NULL]
  
  dt
}


#' Build raw daily data from original standard records (no QC filtering)
#'
#' Aggregates original standard-level records to daily-level without any QC
#' filtering, for later comparison with quality-controlled daily data.
#'
#' @param records Standard-level data (output of ZhenM_read_data, before QC)
#' @return data.table with animal_id, record_date, daily_feed_g, daily_weight_g, visits_n
#' @keywords internal
.build_raw_daily <- function(records) {
  dt <- data.table::copy(records)

  id_col <- if ("animal_id" %in% names(dt)) "animal_id"
            else if ("ID" %in% names(dt)) "ID"
            else stop("No ID column found", call. = FALSE)
  wt_col  <- if ("weight_g" %in% names(dt)) "weight_g"
             else if ("Weight" %in% names(dt)) "Weight" else NULL
  feed_col <- if ("feed_g" %in% names(dt)) "feed_g"
              else if ("Feed_intake" %in% names(dt)) "Feed_intake" else NULL
  visit_col <- if ("start_time" %in% names(dt)) "start_time"
               else if ("Visit_time" %in% names(dt)) "Visit_time" else NULL

  if (!"record_date" %in% names(dt)) {
    if (!is.null(visit_col)) {
      dt[, record_date := ZhenM_safe_to_idate(get(visit_col))]
    } else {
      stop("Cannot determine record_date", call. = FALSE)
    }
  }

  dt[, `:=`(
    .tmp_feed = if (!is.null(feed_col)) suppressWarnings(as.numeric(get(feed_col))) else NA_real_,
    .tmp_wt   = if (!is.null(wt_col)) suppressWarnings(as.numeric(get(wt_col))) else NA_real_
  )]

  result <- dt[, .(
    daily_feed_g   = sum(.tmp_feed, na.rm = TRUE),
    daily_weight_g = stats::median(.tmp_wt, na.rm = TRUE),
    visits_n       = .N
  ), by = c(id_col, "record_date")]

  if (id_col != "animal_id") data.table::setnames(result, id_col, "animal_id")
  data.table::setorder(result, animal_id, record_date)
  result
}

#' Annotate raw daily data with QC and imputation flags
#'
#' Marks each day in raw daily data with flags indicating whether any original
#' record on that day was flagged as an outlier, and whether the daily value
#' was later imputed in the processed pipeline.
#'
#' @param raw_daily Raw daily data from .build_raw_daily
#' @param qc_standard Standard-level data with QC flags (is_outlier_wt, is_outlier_feed)
#' @param daily_imputed Daily-level data with imputation flags (is_imputed_wt, is_imputed_feed)
#' @return raw_daily with added flag columns
#' @keywords internal
.annotate_raw_daily_flags <- function(raw_daily, qc_standard, daily_imputed) {
  raw <- data.table::copy(raw_daily)

  raw[, `:=`(
    day_has_outlier_wt   = FALSE,
    day_has_outlier_feed = FALSE,
    day_is_imputed_wt    = FALSE,
    day_is_imputed_feed  = FALSE
  )]

  if ("is_outlier_wt" %in% names(qc_standard)) {
    outlier_wt <- unique(qc_standard[is_outlier_wt == TRUE, .(animal_id, record_date)])
    if (nrow(outlier_wt) > 0) {
      raw[outlier_wt, on = .(animal_id, record_date), day_has_outlier_wt := TRUE]
    }
  }

  if ("is_outlier_feed" %in% names(qc_standard)) {
    outlier_feed <- unique(qc_standard[is_outlier_feed == TRUE, .(animal_id, record_date)])
    if (nrow(outlier_feed) > 0) {
      raw[outlier_feed, on = .(animal_id, record_date), day_has_outlier_feed := TRUE]
    }
  }

  if ("is_imputed_wt" %in% names(daily_imputed)) {
    imp_wt <- daily_imputed[is_imputed_wt == TRUE, .(animal_id, record_date)]
    if (nrow(imp_wt) > 0) {
      raw[imp_wt, on = .(animal_id, record_date), day_is_imputed_wt := TRUE]
    }
  }

  if ("is_imputed_feed" %in% names(daily_imputed)) {
    imp_feed <- daily_imputed[is_imputed_feed == TRUE, .(animal_id, record_date)]
    if (nrow(imp_feed) > 0) {
      raw[imp_feed, on = .(animal_id, record_date), day_is_imputed_feed := TRUE]
    }
  }

  raw
}
