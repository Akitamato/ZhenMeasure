#' Feed QC for standard-level records
#'
#' 对标准记录（standard_data）进行采食量质控，标记异常记录
#'
#' @param standard_records Standard-level data
#' @param qc_method QC method: "national_standard" (the only supported method since V1.0.0).
#' @param config Configuration list
#' @param logger Optional logger object for detailed logging
#' @return QC'd data with flags (is_outlier_feed)
#' @keywords internal
ZhenM_qc_feed_standard <- function(standard_records, qc_method = "national_standard", config = NULL, logger = NULL) {
  cfg <- ZhenM_merge_config(config, qc_method)
  .qc_feed_standard_national(standard_records, cfg, logger)
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
  # issue #16：阈值接入 config（原写死 500 秒）
  dt[!is.na(feed_speed) & !is.na(duration_sec),
     flag_speed_zero_long_duration := feed_speed == 0 &
       duration_sec > cfg$national_standard$speed_zero_long_duration_sec]
  

  # 9. Feed negative
  dt[!is.na(feed_g), flag_feed_negative := feed_g < 0]

  # 10. Feed too high (single record exceeds individual's single-feed P99)
  # 单次采食量过高：用该个体单次采食量的 P99 作为阈值（量纲一致，均为单次采食量）。
  # 修复：原先误用"日采食量总和的 P99"去标"单次采食记录"，量纲错位导致该 flag 几乎永不触发。
  dt[!is.na(feed_g), flag_feed_too_high := feed_g > quantile(feed_g, 0.99, na.rm = TRUE), by = animal_id]

  # 11. Feed out of plausible range (issue #40)
  # 记录级量程判定：单次采食量落在 feed_intake_range 之外视为设备故障信号。
  # 下游 ZhenM_standard_to_daily_filtered() 据此把「当天任一条记录出界」的整天
  # 置为 NA（交插补），这是 roxygen / man 里承诺的「设备故障保护」分支——此前
  # 该列在全包内没有任何生产者，分支恒不可达。
  #
  # 量纲陷阱：feed_intake_range 在 config 中以 **kg** 给出（默认 c(0, 6)），
  # 而 feed_g 是 **克**。必须过 .normalize_feed_range() 再比较——V0.2.6 的 C-1
  # 正是漏了这一步，让 c(0,6) 直接与 feed_g 相比，把所有正常记录判为异常。
  # 归一化后默认为 [0, 6000] g；下界侧本已由 flag_feed_negative 覆盖，此处一并
  # 表达以保持「区间」语义完整（config 可给出非 0 下界）。
  feed_rng <- .normalize_feed_range(cfg$national_standard$feed_intake_range)
  dt[, flag_feed_out_of_range := FALSE]
  dt[!is.na(feed_g), flag_feed_out_of_range := feed_g < feed_rng[1] | feed_g > feed_rng[2]]

  # Phase 10: STL 时间序列采食量异常检测 (可选)
  dt[, flag_STL_FI := FALSE]

  if (isTRUE(cfg$national_standard$use_stl_feed) &&
      "stl_min_obs" %in% names(cfg$national_standard)) {

    log_detail("Executing STL time series feed QC (National Standard)...")

    # 聚合到日级别
    daily_for_stl <- dt[, .(daily_feed_g = sum(feed_g, na.rm = TRUE)),
                        by = .(animal_id, record_date)]
    data.table::setorder(daily_for_stl, animal_id, record_date)

    ids <- unique(daily_for_stl$animal_id)
    stl_processed <- 0
    stl_cfg <- cfg$national_standard

    for (id in ids) {
      sub <- daily_for_stl[animal_id == id]
      if (nrow(sub) < stl_cfg$stl_min_obs) next

      y <- sub$daily_feed_g
      if (sum(!is.na(y)) < stl_cfg$stl_min_obs) next

      ts_obj <- tryCatch(
        stats::ts(y, frequency = stl_cfg$stl_period),
        error = function(e) NULL
      )
      if (is.null(ts_obj)) next

      stl_fit <- tryCatch(
        stats::stl(ts_obj, s.window = stl_cfg$stl_s_window,
                   robust = stl_cfg$stl_robust),
        error = function(e) NULL
      )

      if (!is.null(stl_fit)) {
        resid <- stl_fit$time.series[, "remainder"]
        mad_val <- stats::mad(resid, na.rm = TRUE)
        if (mad_val > 0) {
          outliers <- abs(resid) > stl_cfg$stl_mad_multiplier * mad_val
          outlier_dates <- sub$record_date[outliers]
          dt[animal_id == id & record_date %in% outlier_dates,
             flag_STL_FI := TRUE]
          stl_processed <- stl_processed + 1
        }
      }
    }

    log_detail(paste0("STL QC processed individuals: ", stl_processed))
    n_stl <- sum(dt$flag_STL_FI, na.rm = TRUE)
    log_detail(paste0("flag_STL_FI: ", n_stl))
  }

  # Mark any anomaly
  # 注：flag_feed_out_of_range 有意**不**并入 is_outlier_feed（issue #40）。
  # 两者的下游语义不同——is_outlier_feed 表示「这条记录的采食数值不可信，需要
  # QC 纠正/置零」，而 out_of_range 表示「设备故障，整天数据整体不可用」，
  # 由 ZhenM_standard_to_daily_filtered() 的整日置 NA 分支单独消费。
  # 并入 OR 会让「整天置 NA」之外还多出记录级纠正口径的变化，属无谓扩散。
  dt[, is_outlier_feed := flag_feed_negative | flag_feed_too_high |
       flag_duration_negative | flag_duration_too_long |
       flag_duration_zero_with_feed | flag_speed_too_slow |
       flag_speed_too_fast | flag_speed_extreme_low_feed |
       flag_speed_zero_long_duration | flag_STL_FI]

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
  n_stl_fi <- sum(dt$flag_STL_FI, na.rm = TRUE)
  n_out_of_range <- sum(dt$flag_feed_out_of_range, na.rm = TRUE)
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
  log_detail(paste0("flag_STL_FI: ", n_stl_fi))
  log_detail(paste0("flag_feed_out_of_range: ", n_out_of_range,
                    " (range [", feed_rng[1], ", ", feed_rng[2], "] g)"))
  log_info(paste0("Feed QC completed: Total outliers flagged = ", n_total_outlier, " (", round(n_total_outlier/n_total*100, 2), "%)"))

  loggers$log_summary(sprintf("Feed QC (National-Standard): feed_negative=%d, feed_too_high=%d, duration_negative=%d, duration_too_long=%d, duration_zero=%d, speed_slow=%d, speed_fast=%d, speed_extreme=%d, speed_zero_long=%d, STL=%d, out_of_range=%d. Total outliers=%d",
                  n_feed_negative, n_feed_too_high,
                  n_duration_negative, n_duration_too_long, n_duration_zero,
                  n_speed_slow, n_speed_fast, n_speed_extreme,
                  n_speed_zero_long, n_stl_fi, n_out_of_range, n_total_outlier))
  dt
}

