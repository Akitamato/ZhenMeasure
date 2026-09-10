#' Overall Quality Control
#'
#' Performs overall quality control on standard feeding records, including removing missing
#' or duplicate records, checking data completeness, continuity, and logical validity.
#'
#' @param standard_records A data.table containing the standard feeding records.
#' @param inactive_days_threshold Maximum allowed consecutive inactive days. Default is 5.
#' @param min_segment_days Minimum required continuous segment days. Default is 30.
#' @param config Optional custom configuration list to override default thresholds.
#' @param qc_method QC method: "national_standard" (the only supported method since V1.0.0).
#' @param logger Optional logger object for recording QC step details.
#' @param keep_ids Path to a text file containing a list of animal_ids to keep (similar to PLINK's --keep). Default is NULL (keep all individuals).
#' @return A list containing the valid `records` and a `summary` of removed records.
#' @export
ZhenM_qc_overall <- function(
    standard_records,
    inactive_days_threshold = 5,
    min_segment_days = 30,
    config = NULL,
    qc_method = "national_standard",
    logger = NULL,
    keep_ids = NULL) {

  qc_method <- match.arg(qc_method)
  if (qc_method != "national_standard") {
    stop("legacy method was removed in V1.0.0. Use national_standard.", call. = FALSE)
  }
  cfg <- ZhenM_merge_config(config, qc_method = qc_method)

  # Create logger helpers
  loggers <- .create_logger_helpers(logger)
  log_info <- loggers$log_info
  log_detail <- loggers$log_detail
  log_subsection <- loggers$log_subsection

  log_subsection("Phase 1: Overall QC Start")

  dt <- data.table::copy(ZhenM_validate_standard_records(standard_records, strict = FALSE))

  n_initial <- nrow(dt)
  n_initial_animals <- data.table::uniqueN(dt$animal_id)
  n_before_animals <- n_initial_animals  # Track previous step animals
  log_detail(paste0("Initial input records: ", n_initial, ", Initial animals: ", n_initial_animals))

  # Track removed animal IDs for each step
  removed_ids_keep <- character()
  removed_ids_missing <- character()
  removed_ids_duplicate <- character()
  removed_ids_incomplete <- character()
  removed_ids_continuity <- character()
  removed_ids_logic <- character()

  # Step 1: Keep specified IDs (like PLINK --keep)
  n_before <- nrow(dt)
  if (!is.null(keep_ids) && file.exists(keep_ids)) {
    ids_to_keep <- data.table::fread(keep_ids, header = FALSE)[[1]]
    ids_to_keep <- base::trimws(as.character(ids_to_keep))
    
    dt_kept <- dt[animal_id %in% ids_to_keep]
    removed_keep_records <- n_before - nrow(dt_kept)
    removed_ids_keep <- setdiff(unique(dt$animal_id), unique(dt_kept$animal_id))
    
    dt <- dt_kept
    n_after_animals <- data.table::uniqueN(dt$animal_id)
    removed_keep_animals <- n_before_animals - n_after_animals
    n_before_animals <- n_after_animals
    
    log_detail(paste0("Kept only specified IDs. Removed records: ", removed_keep_records, " (Animals removed: ", removed_keep_animals, ")"))
  } else {
    removed_keep_records <- 0L
    removed_keep_animals <- 0L
    log_detail("Skipping keep_ids filter: keep_ids is NULL or file missing")
  }

  # Step 2: Remove NA records and invalid IDs
  n_before <- nrow(dt)
  
  # Check if ID consists of identical repeated digits (e.g., "1111111111", "0000")
  # 占位 ID 特征：至少 3 位且全同（issue #19：原 ^(\d)\1*$ 把 "1"/"11" 等
  # 单/双位合法 ID 也判为无效）
  is_valid_id <- !is.na(dt$animal_id) &
                 trimws(dt$animal_id) != "" &
                 !grepl("^(\\d)\\1{2,}$", trimws(dt$animal_id))
  
  removed_ids_missing <- unique(dt[(!is_valid_id | is.na(record_date)) & !is.na(animal_id), animal_id])

  dt <- dt[is_valid_id & !is.na(record_date)]
  removed_missing <- n_before - nrow(dt)
  n_after_animals <- data.table::uniqueN(dt$animal_id)
  removed_missing_animals <- n_before_animals - n_after_animals
  n_before_animals <- n_after_animals
  log_detail(paste0("Removed records with missing/invalid animal_id or missing record_date: ", removed_missing))

  # Step 3: Deduplication
  n_before <- nrow(dt)
  
  removed_ids_duplicate <- dt[duplicated(dt), unique(animal_id)]

  dt <- unique(dt)
  removed_duplicate <- n_before - nrow(dt)
  n_after_animals <- data.table::uniqueN(dt$animal_id)
  removed_duplicate_animals <- n_before_animals - n_after_animals
  n_before_animals <- n_after_animals
  log_detail(paste0("Removed duplicated records: ", removed_duplicate))

  # Step 4: Data completeness check
  min_test_days <- cfg[[qc_method]]$min_test_days %||% cfg$national_standard$min_test_days
  max_missing_rate <- cfg[[qc_method]]$max_missing_rate %||% cfg$national_standard$max_missing_rate

  completeness <- dt[, .(
    n_test_days = as.numeric(max(record_date) - min(record_date)) + 1,
    n_valid_days = data.table::uniqueN(record_date)
  ), by = animal_id]

  completeness[, missing_rate := 1 - n_valid_days / n_test_days]
  completeness[, is_incomplete := n_test_days < min_test_days | missing_rate > max_missing_rate]

  incomplete_ids <- completeness[is_incomplete == TRUE, animal_id]
  removed_ids_incomplete <- incomplete_ids
  
  n_before <- nrow(dt)
  dt <- dt[!(animal_id %in% incomplete_ids)]
  removed_incomplete_records <- n_before - nrow(dt)
  n_after_animals <- data.table::uniqueN(dt$animal_id)
  removed_incomplete_animals <- n_before_animals - n_after_animals
  n_before_animals <- n_after_animals

  log_detail(paste0("Data completeness check: min_test_days = ", min_test_days, ", max_missing_rate = ", max_missing_rate))
  log_detail(paste0("Unqualified animals for completeness: ", length(incomplete_ids), " (Removed records: ", removed_incomplete_records, ")"))


  if (nrow(dt) == 0) {
    log_info("Warning: All data has been filtered out. QC process is terminated.")
    # issue #33：summary 的列名必须与正常路径一致（原为 removed_animal_ids），
    # 否则 qc_overall_summary.csv 的表头随数据状态变化，按列名取值的下游会静默落空
    return(list(
      records = dt,
      summary = data.table::data.table(
        step = c("keep_specified_ids", "missing_id_or_date", "duplicate_records", "incomplete_data", "continuity_removed_records", "logic_invalid"),
        n_removed = c(removed_keep_records, removed_missing, removed_duplicate, removed_incomplete_records, 0L, 0L),
        n_removed_animals = c(removed_keep_animals, removed_missing_animals, removed_duplicate_animals, removed_incomplete_animals, 0L, 0L),
        associated_animal_ids = c(
          paste(removed_ids_keep, collapse = ";"),
          paste(removed_ids_missing, collapse = ";"),
          paste(removed_ids_duplicate, collapse = ";"),
          paste(removed_ids_incomplete, collapse = ";"),
          "",
          ""
        )
      )
    ))
  }

  # Step 5: Continuity check
  dt[, has_feed := !is.na(daily_feed_g) & daily_feed_g > 0]
  dt[, has_wt := !is.na(weight_g) & weight_g > 0]

  daily_status <- dt[, .(is_valid_day = any(has_feed) | any(has_wt)), by = .(animal_id, record_date)]
  data.table::setorder(daily_status, animal_id, record_date)
  valid_days <- daily_status[is_valid_day == TRUE]

  continuity_removed_records <- 0L
  continuity_removed_animals <- 0L
  all_ids_before_continuity <- unique(dt$animal_id)

  if (nrow(valid_days) > 0) {
    valid_days[, date_diff := c(1, diff(record_date)), by = animal_id]
    valid_days[, segment_id := cumsum(date_diff > inactive_days_threshold), by = animal_id]
    segment_stats <- valid_days[, .(
      start_date = min(record_date),
      end_date = max(record_date),
      n_valid_days = .N,
      duration = as.numeric(max(record_date) - min(record_date)) + 1
    ), by = .(animal_id, segment_id)]

    qualified_segments <- segment_stats[n_valid_days >= min_segment_days]

    # =========================
    # Keep only the longest qualified segment per animal
    # =========================
    if (nrow(qualified_segments) > 0) {
      qualified_segments <- qualified_segments[
        qualified_segments[, .I[which.max(duration)], by = animal_id]$V1
      ]
    }

    n_before <- nrow(dt)
    dt[, keep_segment := FALSE]
    # issue #22：段区间回填由「逐段全表扫描」改为一次非等值 join
    # （O(段数 × 记录数) → O(记录数 + 命中行数)），命中集合与旧实现一致
    if (nrow(qualified_segments) > 0) {
      seg_hit <- dt[qualified_segments,
                    on = .(animal_id, record_date >= start_date, record_date <= end_date),
                    which = TRUE, nomatch = NULL]
      if (length(seg_hit) > 0) dt[seg_hit, keep_segment := TRUE]
    }
    dt <- dt[keep_segment == TRUE]
    continuity_removed_records <- n_before - nrow(dt)
    dt[, keep_segment := NULL]
    
    n_after_animals <- data.table::uniqueN(dt$animal_id)
    continuity_removed_animals <- n_before_animals - n_after_animals
    n_before_animals <- n_after_animals

    all_ids_after_continuity <- unique(dt$animal_id)
    removed_ids_continuity <- setdiff(all_ids_before_continuity, all_ids_after_continuity)

    log_detail(paste0("Continuity check criteria: max_inactive_days = ", inactive_days_threshold, 
                      ", min_segment_days = ", min_segment_days,
                      "; kept only the longest segment per animal"))
    log_detail(paste0("Continuity check filtered records: ", continuity_removed_records))
  } else {
    removed_ids_continuity <- all_ids_before_continuity
  }

  dt[, c("has_feed", "has_wt") := NULL]

  # Step 6: Data logical validity check
  n_before <- nrow(dt)
  removed_logic_records <- 0L
  removed_logic_animals <- 0L
  all_ids_before_logic <- unique(dt$animal_id)
  
  if (nrow(dt) > 0) {
    dt[, logic_invalid := FALSE]
    
    # Condition 1: end_time < start_time
    if (all(c("end_time", "start_time") %in% names(dt))) {
      invalid_time <- !is.na(dt$end_time) & !is.na(dt$start_time) & dt$end_time < dt$start_time
      if (any(invalid_time)) {
        dt[invalid_time, logic_invalid := TRUE]
        log_detail(paste0("Detected invalid records with end_time < start_time: ", sum(invalid_time)))
      }
    }
    
    # Condition 2: duration_sec <= 0 AND feed_g <= 0
    if (all(c("duration_sec", "feed_g") %in% names(dt))) {
      both_invalid <- (!is.na(dt$duration_sec) & dt$duration_sec <= 0) &
                      (!is.na(dt$feed_g) & dt$feed_g <= 0)
      if (any(both_invalid)) {
        dt[both_invalid, logic_invalid := TRUE]
        log_detail(paste0("Detected invalid records with duration_sec and feed_g <= 0: ", sum(both_invalid)))
      }
    }
    
    # Remove logically invalid records
    dt <- dt[logic_invalid == FALSE]
    removed_logic_records <- n_before - nrow(dt)
    dt[, logic_invalid := NULL]
    n_after_animals <- data.table::uniqueN(dt$animal_id)
    removed_logic_animals <- n_before_animals - n_after_animals
    n_before_animals <- n_after_animals
    
    all_ids_after_logic <- unique(dt$animal_id)
    removed_ids_logic <- setdiff(all_ids_before_logic, all_ids_after_logic)

    if (removed_logic_records > 0) {
      log_detail(paste0("Logically invalid records removed: ", removed_logic_records))
    } else {
      log_detail("All records passed logical validity check")
    }
  } else {
    removed_ids_logic <- all_ids_before_logic
    log_detail("Skipping logical check: data is empty")
  }

  # Summary statistics
  n_final <- nrow(dt)
  n_final_animals <- data.table::uniqueN(dt$animal_id)
  total_removed <- n_initial - n_final
  total_removed_animals <- n_initial_animals - n_final_animals
  log_info(paste0("Overall QC Completed: removed records ", total_removed, ", remaining records ", n_final))
  log_info(paste0("Remaining animals: ", n_final_animals))

  loggers$log_summary(sprintf(
    "Overall QC: keep=%d, missing=%d, duplicate=%d, incomplete=%d, continuity=%d, logic_invalid=%d",
    removed_keep_records, removed_missing, removed_duplicate, removed_incomplete_records,
    continuity_removed_records, removed_logic_records))

  list(
    records = dt,
    summary = data.table::data.table(
      step = c("keep_specified_ids", "missing_id_or_date", "duplicate_records", "incomplete_data", "continuity_removed_records", "logic_invalid"),
      n_removed = c(removed_keep_records, removed_missing, removed_duplicate, removed_incomplete_records, continuity_removed_records, removed_logic_records),
      n_removed_animals = c(removed_keep_animals, removed_missing_animals, removed_duplicate_animals, removed_incomplete_animals, continuity_removed_animals, removed_logic_animals),
      associated_animal_ids = c(
        paste(removed_ids_keep, collapse = ";"),
        paste(removed_ids_missing, collapse = ";"),
        paste(removed_ids_duplicate, collapse = ";"),
        paste(removed_ids_incomplete, collapse = ";"),
        paste(removed_ids_continuity, collapse = ";"),
        paste(removed_ids_logic, collapse = ";")
      )
    )
  )
}
