#' Calculate phenotypes with stage-aware support
#'
#' Calculates pig growth phenotypes (ADG, ADFI, FCR) using different calculation methods.
#' Supports optional stage-based data partitioning by weight, age, or date.
#'
#' @param daily_records Daily-level data.table with columns: animal_id, record_date,
#'   daily_weight_g, daily_feed_g, age_days (required if stage_mode="age")
#' @param phenotype_method Calculation method:
#'   \itemize{
#'     \item "standard_fcr": National standard (30-120kg, FCR=feed/90kg)
#'     \item "report": Stage average method (compatible with V0.2.0)
#'     \item "monitor": Rolling window method (daily output)
#'     \item "research": Linear regression method (with R² and p-values)
#'   }
#' @param config Optional configuration list. If NULL, uses default config.
#' @param stage_mode Data partitioning mode (optional):
#'   \itemize{
#'     \item NULL: No partitioning, calculate on full dataset
#'     \item "weight": Partition by weight ranges (requires target_weight_stages)
#'     \item "age": Partition by age ranges (requires target_age_stages)
#'     \item "date": Partition by date ranges (requires target_date_stages)
#'   }
#' @param target_weight_stages For stage_mode="weight": weight ranges in kg.
#'   Can be "YANGXIANG" (preset: 30-100, 30-115, 30-120kg) or numeric vector
#' @param target_age_stages For stage_mode="age": age ranges in days.
#'   Numeric vector defining stage boundaries (e.g., c(70, 100, 130, 160))
#' @param target_date_stages For stage_mode="date": date ranges.
#'   Character vector or Date vector defining stage boundaries
#'   (e.g., c("2024-01-01", "2024-02-01", "2024-03-01"))
#' @return Data.table with calculated phenotypes. Output structure:
#'   \itemize{
#'     \item Without stage_mode: One row per animal
#'     \item With stage_mode: One row per animal per stage (adds stage_label column)
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Standard FCR on full dataset
#' result <- ZhenM_calc_phenotypes(daily_data, "standard_fcr")
#'
#' # Standard FCR by weight stages
#' result <- ZhenM_calc_phenotypes(daily_data, "standard_fcr", 
#'                                stage_mode = "weight",
#'                                target_weight_stages = c(30, 60, 90, 120))
#'
#' # Report mode by age stages
#' result <- ZhenM_calc_phenotypes(daily_data, "report",
#'                                stage_mode = "age",
#'                                target_age_stages = c(70, 100, 130, 160))
#'
#' # Research mode by date stages
#' result <- ZhenM_calc_phenotypes(daily_data, "research",
#'                                stage_mode = "date",
#'                                target_date_stages = c("2024-01-01", "2024-02-01", "2024-03-01"))
#' }
ZhenM_calc_phenotypes <- function(daily_records, 
                                 phenotype_method = "standard_fcr", 
                                 config = NULL,
                                 stage_mode = NULL,
                                 target_weight_stages = NULL,
                                 target_age_stages = NULL,
                                 target_date_stages = NULL) {

  # Validate inputs
  phenotype_method <- match.arg(phenotype_method, c("standard_fcr", "report", "monitor", "research"))
  
  if (!is.null(stage_mode)) {
    stage_mode <- match.arg(stage_mode, c("weight", "age", "date"))
  }

  cfg <- ZhenM_merge_config(config, qc_method = "national_standard")

  # Case 1: No stage partitioning - calculate on full dataset
  if (is.null(stage_mode)) {
    return(.dispatch_phenotype_calc(daily_records, phenotype_method, cfg))
  }

  # Case 2: Stage-based partitioning
  dt <- data.table::as.data.table(data.table::copy(daily_records))
  
  # Partition data by stages
  if (stage_mode == "weight") {
    stage_results <- .calc_phenotypes_by_weight_stages(dt, phenotype_method, cfg, target_weight_stages)
  } else if (stage_mode == "age") {
    stage_results <- .calc_phenotypes_by_age_stages(dt, phenotype_method, cfg, target_age_stages)
  } else if (stage_mode == "date") {
    stage_results <- .calc_phenotypes_by_date_stages(dt, phenotype_method, cfg, target_date_stages)
  }

  return(stage_results)
}

#' Dispatch phenotype calculation to specific method
#' @keywords internal
.dispatch_phenotype_calc <- function(daily_records, phenotype_method, cfg) {
  switch(phenotype_method,
    "standard_fcr" = .calc_phenotypes_standard_fcr(daily_records, cfg),
    "report" = .calc_phenotypes_report(daily_records, cfg),
    "monitor" = .calc_phenotypes_monitor(daily_records, cfg),
    "research" = .calc_phenotypes_research(daily_records, cfg)
  )
}

#' Calculate phenotypes by weight stages
#' @keywords internal
.calc_phenotypes_by_weight_stages <- function(daily_records, phenotype_method, cfg, target_weight_stages) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))
  
  # Parse weight stages
  if (is.character(target_weight_stages) && target_weight_stages == "YANGXIANG") {
    stage_ranges <- list(
      "30-100kg" = c(30, 100),
      "30-115kg" = c(30, 115),
      "30-120kg" = c(30, 120)
    )
  } else if (is.numeric(target_weight_stages)) {
    # Convert numeric vector to stage ranges
    stage_ranges <- list()
    for (i in 1:(length(target_weight_stages) - 1)) {
      stage_name <- paste0(target_weight_stages[i], "-", target_weight_stages[i+1], "kg")
      stage_ranges[[stage_name]] <- c(target_weight_stages[i], target_weight_stages[i+1])
    }
  } else {
    stop("target_weight_stages must be 'YANGXIANG' or numeric vector")
  }
  
  # Convert weight to kg
  dt[, weight_kg := daily_weight_g / 1000]
  
  # Calculate phenotypes for each stage
  stage_results <- lapply(names(stage_ranges), function(stage_name) {
    range_kg <- stage_ranges[[stage_name]]
    dt_stage <- dt[weight_kg >= range_kg[1] & weight_kg <= range_kg[2]]
    
    if (nrow(dt_stage) == 0) return(NULL)
    
    result <- .dispatch_phenotype_calc(dt_stage, phenotype_method, cfg)
    # report 模式的体重阶段：过滤 test_days 过短的病猪（阶段数据不足）
    if (nrow(result) > 0 && identical(phenotype_method, "report") &&
        "test_days" %in% names(result)) {
      min_stage_days <- cfg$national_standard$min_stage_days
      if (!is.null(min_stage_days)) {
        result <- result[test_days >= min_stage_days]
      }
    }
    if (nrow(result) > 0) {
      result[, stage_label := stage_name]
      result[, stage_min := range_kg[1]]
      result[, stage_max := range_kg[2]]
    }
    result
  })
  
  data.table::rbindlist(stage_results, fill = TRUE)
}

#' Calculate phenotypes by age stages
#' @keywords internal
.calc_phenotypes_by_age_stages <- function(daily_records, phenotype_method, cfg, target_age_stages) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))
  
  # Validate age column exists
  if (!"age_days" %in% names(dt)) {
    stop("age_days column required for stage_mode='age'")
  }
  
  # Parse age stages
  if (is.null(target_age_stages) || length(target_age_stages) < 2) {
    stop("target_age_stages must be numeric vector with at least 2 values")
  }
  
  # Create stage ranges
  stage_ranges <- list()
  for (i in 1:(length(target_age_stages) - 1)) {
    stage_name <- paste0(target_age_stages[i], "-", target_age_stages[i+1], "d")
    stage_ranges[[stage_name]] <- c(target_age_stages[i], target_age_stages[i+1])
  }
  
  # Calculate phenotypes for each stage
  stage_results <- lapply(names(stage_ranges), function(stage_name) {
    range_days <- stage_ranges[[stage_name]]
    dt_stage <- dt[age_days >= range_days[1] & age_days <= range_days[2]]
    
    if (nrow(dt_stage) == 0) return(NULL)
    
    result <- .dispatch_phenotype_calc(dt_stage, phenotype_method, cfg)
    if (nrow(result) > 0) {
      result[, stage_label := stage_name]
      result[, stage_min := range_days[1]]
      result[, stage_max := range_days[2]]
    }
    result
  })
  
  data.table::rbindlist(stage_results, fill = TRUE)
}

#' Calculate phenotypes by date stages
#' @keywords internal
.calc_phenotypes_by_date_stages <- function(daily_records, phenotype_method, cfg, target_date_stages) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))
  
  # Validate date column exists
  if (!"record_date" %in% names(dt)) {
    stop("record_date column required for stage_mode='date'")
  }
  
  # Parse date stages
  if (is.null(target_date_stages) || length(target_date_stages) < 2) {
    stop("target_date_stages must be Date or character vector with at least 2 values")
  }
  
  # Convert to Date if character
  if (is.character(target_date_stages)) {
    target_date_stages <- as.Date(target_date_stages)
  }
  
  # Ensure record_date is Date type
  if (!inherits(dt$record_date, "Date")) {
    dt[, record_date := ZhenM_safe_to_idate(record_date)]
  }
  
  # Create stage ranges
  stage_ranges <- list()
  for (i in 1:(length(target_date_stages) - 1)) {
    stage_name <- paste0(target_date_stages[i], "_", target_date_stages[i+1])
    stage_ranges[[stage_name]] <- c(target_date_stages[i], target_date_stages[i+1])
  }
  
  # Calculate phenotypes for each stage
  stage_results <- lapply(names(stage_ranges), function(stage_name) {
    range_dates <- stage_ranges[[stage_name]]
    dt_stage <- dt[record_date >= range_dates[1] & record_date <= range_dates[2]]
    
    if (nrow(dt_stage) == 0) return(NULL)
    
    result <- .dispatch_phenotype_calc(dt_stage, phenotype_method, cfg)
    if (nrow(result) > 0) {
      result[, stage_label := stage_name]
      result[, stage_start_date := range_dates[1]]
      result[, stage_end_date := range_dates[2]]
    }
    result
  })
  
  data.table::rbindlist(stage_results, fill = TRUE)
}

#' Calculate base phenotypes (standard Report format)
#' @keywords internal
.calc_base_phenotypes <- function(daily_records) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))

  empty_dt <- data.table::data.table(
    animal_id = character(),
    start_date = data.table::as.IDate(character()),
    end_date = data.table::as.IDate(character()),
    test_days = numeric(),
    start_weight_g = numeric(),
    end_weight_g = numeric(),
    total_feed_g = numeric(),
    ADG_g = numeric(),
    ADFI_g = numeric(),
    FCR = numeric(),
    AGE = numeric()
  )

  if (nrow(dt) == 0) return(empty_dt)

  phenotypes <- dt[, {
    data.table::setorder(.SD, record_date)
    start_dt <- min(record_date)
    end_dt <- max(record_date)
    
    sub_st <- .SD[record_date <= start_dt + 6]
    sub_ed <- .SD[record_date >= end_dt - 6]
    
    st_wt_g <- stats::median(sub_st$daily_weight_g, na.rm = TRUE)
    ed_wt_g <- stats::median(sub_ed$daily_weight_g, na.rm = TRUE)
    
    st_dt_mid <- stats::median(as.numeric(sub_st$record_date), na.rm = TRUE)
    ed_dt_mid <- stats::median(as.numeric(sub_ed$record_date), na.rm = TRUE)
    
    # 获取最高日龄 (兼容 age_days 或 age_day)
    age_val <- NA_real_
    if ("age_days" %in% names(.SD)) {
      age_val <- suppressWarnings(max(.SD$age_days, na.rm = TRUE))
    } else if ("age_day" %in% names(.SD)) {
      age_val <- suppressWarnings(max(.SD$age_day, na.rm = TRUE))
    }
    if (is.infinite(age_val)) age_val <- NA_real_
    
    .(
      start_date = start_dt,
      end_date = end_dt,
      test_days = as.numeric(end_dt - start_dt) + 1,
      adg_days = as.numeric(ed_dt_mid - st_dt_mid),
      start_weight_g = st_wt_g,
      end_weight_g = ed_wt_g,
      total_feed_g = sum(daily_feed_g, na.rm = TRUE),
      AGE = age_val
    )
  }, by = animal_id]

  phenotypes[, ADG_g := (end_weight_g - start_weight_g) / adg_days]
  phenotypes[adg_days <= 0, ADG_g := NA_real_]
  phenotypes[, ADFI_g := total_feed_g / test_days]
  phenotypes[, FCR := total_feed_g / as.numeric(end_weight_g - start_weight_g)]
  phenotypes[is.infinite(FCR) | is.nan(FCR), FCR := NA_real_]
  phenotypes[, adg_days := NULL]

  phenotypes
}

#' Standard FCR calculation (30-120kg)
#' @keywords internal
.calc_phenotypes_standard_fcr <- function(daily_records, cfg) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))

  # Filter 30-120kg range roughly using daily_weight_g directly
  test_range <- cfg$national_standard$test_weight_range
  test_range_g <- test_range * 1000
  dt_test <- dt[daily_weight_g >= test_range_g[1] & daily_weight_g <= test_range_g[2]]
  
  phenotypes <- .calc_base_phenotypes(dt_test)
  if (nrow(phenotypes) == 0) {
    phenotypes[, `:=`(FCR_30_120kg = numeric(), n_stages = integer(), n_valid_stages = integer(), invalid_stages = character(), flag_fcr_stage_invalid = logical())]
    return(phenotypes)
  }

  # Standard FCR = total_feed / 90kg (so FCR_30_120kg expects total_feed_kg / 90)
  phenotypes[, FCR_30_120kg := (total_feed_g / 1000) / 90]

  # Weight stage FCR QC (Table 2)
  dt_test[, feed_kg := daily_feed_g / 1000]
  dt_test[, weight_kg := daily_weight_g / 1000]
  phenotypes <- .add_stage_fcr_qc(dt_test, phenotypes, cfg)

  phenotypes
}

#' Add weight stage FCR quality control
#' @keywords internal
.add_stage_fcr_qc <- function(daily_data, phenotypes, cfg) {
  dt <- data.table::copy(daily_data)
  fcr_ranges <- data.table::as.data.table(
    data.table::copy(cfg$national_standard$fcr_ranges)
  )

  # 自建 kg 列（standard_fcr 调用方已建，report 等模式未建）
  if (!"weight_kg" %in% names(dt)) dt[, weight_kg := daily_weight_g / 1000]
  if (!"feed_kg" %in% names(dt)) dt[, feed_kg := daily_feed_g / 1000]

  # Calculate FCR for each 10kg stage
  dt[, weight_stage := cut(
    weight_kg,
    breaks = c(fcr_ranges$weight_min, max(fcr_ranges$weight_max)),
    labels = paste0(fcr_ranges$weight_min, "-", fcr_ranges$weight_max, "kg"),
    include.lowest = TRUE,
    right = FALSE
  )]

  stage_fcr <- dt[!is.na(weight_stage), .(
    stage_feed = sum(feed_kg, na.rm = TRUE),
    stage_gain = max(weight_kg, na.rm = TRUE) - min(weight_kg, na.rm = TRUE),
    n_days = .N
  ), by = .(animal_id, weight_stage)]

  stage_fcr[, stage_fcr := ifelse(stage_gain > 0, stage_feed / stage_gain, NA_real_)]

  # Merge with FCR ranges
  stage_fcr[, stage_label := as.character(weight_stage)]
  fcr_ranges[, stage_label := paste0(weight_min, "-", weight_max, "kg")]

  stage_fcr <- merge(stage_fcr, fcr_ranges, by = "stage_label", all.x = TRUE)

  # Flag out-of-range stages
  stage_fcr[, stage_fcr_valid := stage_fcr >= fcr_min & stage_fcr <= fcr_max]

  # Summarize by animal
  stage_summary <- stage_fcr[, .(
    n_stages = .N,
    n_valid_stages = sum(stage_fcr_valid, na.rm = TRUE),
    invalid_stages = paste(weight_stage[!stage_fcr_valid], collapse = ";")
  ), by = animal_id]

  stage_summary[invalid_stages == "", invalid_stages := NA_character_]

  # Merge back to phenotypes
  phenotypes <- merge(phenotypes, stage_summary, by = "animal_id", all.x = TRUE)
  phenotypes[, flag_fcr_stage_invalid := n_valid_stages < n_stages]

  phenotypes
}

#' Report mode: stage average method
#' @keywords internal
.calc_phenotypes_report <- function(daily_records, cfg) {
  phenotypes <- .calc_base_phenotypes(daily_records)
  if (nrow(phenotypes) == 0) return(phenotypes)
  .add_stage_fcr_qc(daily_records, phenotypes, cfg)
}

#' Monitor mode: rolling window method
#' @keywords internal
.calc_phenotypes_monitor <- function(daily_records, cfg, window = 7) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))
  
  phenotypes <- .calc_base_phenotypes(dt)
  if (nrow(phenotypes) == 0) return(phenotypes)

  dt[, weight_kg := daily_weight_g / 1000]
  dt[, feed_kg := daily_feed_g / 1000]

  data.table::setorder(dt, animal_id, record_date)

  # Rolling ADG
  dt[, weight_diff := c(NA, diff(weight_kg)), by = animal_id]
  dt[, ADG_rolling := zoo::rollmean(weight_diff, k = window, fill = NA, align = "right"), by = animal_id]

  # Rolling ADFI
  dt[, ADFI_rolling := zoo::rollmean(feed_kg, k = window, fill = NA, align = "right"), by = animal_id]

  # Summarize to match base format
  monitor_summary <- dt[, .(
    ADG_rolling_mean_g = mean(ADG_rolling, na.rm = TRUE) * 1000,
    ADFI_rolling_mean_g = mean(ADFI_rolling, na.rm = TRUE) * 1000
  ), by = animal_id]

  monitor_summary[, FCR_rolling_mean := ADFI_rolling_mean_g / ADG_rolling_mean_g]

  phenotypes <- merge(phenotypes, monitor_summary, by = "animal_id", all.x = TRUE)
  
  # Remove report-duplicated phenotypes to maintain focus
  phenotypes[, `:=`(ADG_g = NULL, ADFI_g = NULL, FCR = NULL)]
  
  phenotypes
}

#' Research mode: linear regression method
#' @keywords internal
.calc_phenotypes_research <- function(daily_records, cfg) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))
  
  phenotypes <- .calc_base_phenotypes(dt)
  if (nrow(phenotypes) == 0) return(phenotypes)

  dt[, day_num := as.numeric(record_date - min(record_date)), by = animal_id]
  # Calculate cumulative feed safely by treating NAs as 0 during cumsum
  dt[, cum_feed_g := cumsum(ifelse(is.na(daily_feed_g), 0, daily_feed_g)), by = animal_id]
  # Then set cum_feed_g to NA where daily_feed_g is NA so lm() drops these points
  dt[is.na(daily_feed_g), cum_feed_g := NA_real_]

  phenotypes_list <- lapply(unique(dt$animal_id), function(id) {
    sub <- dt[animal_id == id]

    # ADG: lm(weight_g ~ day)
    adg_fit <- tryCatch(lm(daily_weight_g ~ day_num, data = sub), error = function(e) NULL)

    # ADFI: lm(cum_feed_g ~ day)
    adfi_fit <- tryCatch(lm(cum_feed_g ~ day_num, data = sub), error = function(e) NULL)

    result <- data.table::data.table(animal_id = id)

    if (!is.null(adg_fit) && length(coef(adg_fit)) > 1 && !is.na(coef(adg_fit)[2])) {
      result[, ADG_g_lm := coef(adg_fit)[2]]
      result[, ADG_r2 := summary(adg_fit)$r.squared]
      
      coeffs <- summary(adg_fit)$coefficients
      if (nrow(coeffs) >= 2 && ncol(coeffs) >= 4) {
        result[, ADG_pval := coeffs[2, 4]]
      } else {
        result[, ADG_pval := NA_real_]
      }
    }

    if (!is.null(adfi_fit) && length(coef(adfi_fit)) > 1 && !is.na(coef(adfi_fit)[2])) {
      result[, ADFI_g_lm := coef(adfi_fit)[2]]
      result[, ADFI_r2 := summary(adfi_fit)$r.squared]
      
      coeffs <- summary(adfi_fit)$coefficients
      if (nrow(coeffs) >= 2 && ncol(coeffs) >= 4) {
        result[, ADFI_pval := coeffs[2, 4]]
      } else {
        result[, ADFI_pval := NA_real_]
      }
    }

    result
  })

  res_extra <- data.table::rbindlist(phenotypes_list, fill = TRUE)
  phenotypes <- merge(phenotypes, res_extra, by = "animal_id", all.x = TRUE)
  
  if ("ADFI_g_lm" %in% names(phenotypes) && "ADG_g_lm" %in% names(phenotypes)) {
    phenotypes[, FCR_lm := ADFI_g_lm / ADG_g_lm]
  } else {
    phenotypes[, FCR_lm := NA_real_]
  }
  
  # Remove report-duplicated phenotypes to maintain focus
  phenotypes[, `:=`(ADG_g = NULL, ADFI_g = NULL, FCR = NULL)]
  
  phenotypes
}
