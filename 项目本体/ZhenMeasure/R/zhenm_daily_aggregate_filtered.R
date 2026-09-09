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
#' @param config Optional configuration list (merged via ZhenM_merge_config). Controls the
#'   optional FCR anchor correction (`national_standard$use_fcr_anchor`) and the correction
#'   mechanism switches (`use_record_feed_correction`, `use_lmm_feed_correction`,
#'   experimental stacking switch `use_lmm_stacking`).
#'   NULL keeps default behaviour.
#'   三开关依赖（issue #17）：`use_lmm_feed_correction=TRUE` 仅在记录级纠正
#'   关闭或失败时作为日级兜底运行；记录级纠正成功且未开启 `use_lmm_stacking`
#'   时 LMM 不运行（避免二次校正），该组合下此开关为空操作。
#' @return A daily-level table aggregated by animal and date with daily_weight_g and enhanced feed QC
#' @export
ZhenM_standard_to_daily_filtered <- function(standard_records, config = NULL) {
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
  
  # 解析校正机制开关：config=NULL 直调时保持现状行为（记录级纠正 + LMM 兜底均开启）
  ns_cfg <- if (!is.null(config)) config$national_standard else NULL
  use_record_fix <- if (!is.null(ns_cfg$use_record_feed_correction)) {
    isTRUE(ns_cfg$use_record_feed_correction)
  } else TRUE
  use_lmm_fix <- if (!is.null(ns_cfg$use_lmm_feed_correction)) {
    isTRUE(ns_cfg$use_lmm_feed_correction)
  } else TRUE
  use_lmm_stack <- if (!is.null(ns_cfg$use_lmm_stacking)) {
    isTRUE(ns_cfg$use_lmm_stacking)
  } else FALSE

  # 被 flag 记录 = 事件真实发生但采食量错误，按 flag 类型用物理规则纠正（而非置零）。
  # 纠正失败或被配置关闭时回退为现有「置零 + 日级 LMM 校正」路径。
  feed_correction_success <- FALSE
  if (!is.null(feed_col) && use_record_fix) {
    corrected <- .correct_feed_records(
      dt, speed_max = if (!is.null(ns_cfg$speed_max)) as.numeric(ns_cfg$speed_max) else 170)
    feed_correction_success <- corrected$success
    if (feed_correction_success) {
      dt[, feed_filtered := corrected$feed_corrected]
    } else {
      dt[, feed_filtered := ifelse(is_outlier_feed == TRUE, 0, get(feed_col))]
    }
  } else if (!is.null(feed_col)) {
    # 记录级物理纠正被配置关闭（消融实验对照用）：走 V1.1.0 置零路径
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
        # issue #19：无实测体重的记录不参与「当日是否有可用体重」的判定——否则其
        # flag 由 NA 改为 FALSE（flag 语义修正）后，会把「当日实测体重全部被标异常」
        # 的天重新填回该日均值，改变日级结果
        valid_ww <- weighted_avg_weight_per_day[
          !is.na(weight_filtered) & is_outlier_wt == FALSE]
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

  run_lmm_fallback <- !feed_correction_success && use_lmm_fix
  run_lmm_stack <- feed_correction_success && use_lmm_fix && use_lmm_stack
  if (run_lmm_fallback || run_lmm_stack) {
    # 记录级纠正失败/关闭 → 日级 LMM 兜底（fallback）；
    # 纠正成功且开启叠加 → 在物理纠正结果上串联互补式 LMM（stack，
    # 只补噪声置零类损失，不对已被物理封顶的记录二次补偿）
    result <- .apply_feed_lmm_correction(result, dt, ns_cfg, stack = run_lmm_stack)
  } else {
    # 跳过日级 LMM 校正，仅保留 6kg 日上限校验（保证各路径口径一致）
    result <- .finalize_daily_feed(result)
  }
  # =================================================

  # FCR 锚定矫正（可选）：用国标 FCR 范围双向 cap 日采食量
  if (!is.null(config) && isTRUE(config$national_standard$use_fcr_anchor)) {
    result <- .correct_feed_with_fcr_anchor(result, config)
  }

  # Add attributes
  attr(result, "qc_filtered") <- TRUE
  attr(result, "source_format") <- if (id_col == "ID") "original" else "new"
  
  result
}

#' 日级采食量出口校验（issue #21）
#'
#' 6kg (6000g) 为猪只单日采食量生理上限：超限天打标
#' `flag_daily_feed_over_limit` 并置 NA（等插补）；≤0 的天同样置 NA。三条出口
#' 路径（跳过 LMM / LMM 出口 / 无 feed 列提前返回）统一调用，避免口径漂移。
#' 只做出口把关，不筛 LMM 训练样本。
#'
#' @param dt 日级表（含 daily_feed_g）
#' @return 原地修改并返回 dt
#' @keywords internal
.finalize_daily_feed <- function(dt) {
  dt[, flag_daily_feed_over_limit := !is.na(daily_feed_g) & daily_feed_g > 6000]
  dt[!is.na(daily_feed_g) & (daily_feed_g <= 0 | flag_daily_feed_over_limit == TRUE),
     daily_feed_g := NA_real_]
  dt[]
}

#' Record-level feed intake correction by flag type (physics caps)
#'
#' Corrects the feed intake of flagged records using flag-specific physical
#' rules instead of zeroing them out or predicting from a regression. A flagged
#' record still represents a real feeding event whose recorded amount is at
#' most some physiological upper bound.
#'
#' @param dt Standard-record-level data.table with feed QC flags
#' @param speed_max Physiological feeding-rate cap in g/min, used to cap
#'   speed_too_fast records (feed ≤ speed_max × duration/60). Defaults to 170;
#'   callers should thread `ns_cfg$speed_max` so the record-level cap and the
#'   LMM add-back cap (issue #12) stay on the same config value.
#' @return list(success, feed_corrected). feed_corrected is a numeric vector
#'   aligned with dt rows.
#' @keywords internal
.correct_feed_records <- function(dt, speed_max = 170) {
  dt <- data.table::copy(dt)

  feed_col <- if ("feed_g" %in% names(dt)) "feed_g"
              else if ("Feed_intake" %in% names(dt)) "Feed_intake" else NULL
  if (is.null(feed_col) || !"is_outlier_feed" %in% names(dt)) {
    return(list(success = FALSE, feed_corrected = NULL))
  }

  dur_col <- if ("duration_sec" %in% names(dt)) "duration_sec"
             else if ("Duration" %in% names(dt)) "Duration" else NULL

  # 初始保留原采食量（被 flag 记录不置零，只对「明显离谱」的封顶/归零）
  dt[, feed_corrected := as.numeric(get(feed_col))]

  # speed_max 由调用方从 config 传入（issue #12），缺省 170 与
  # zhenm_config_defaults.R 的 speed_max 保持一致

  # 1) 纯噪声 → 0
  if ("flag_feed_negative" %in% names(dt)) {
    dt[flag_feed_negative == TRUE, feed_corrected := 0]
  }
  if ("flag_speed_extreme_low_feed" %in% names(dt)) {
    dt[flag_speed_extreme_low_feed == TRUE, feed_corrected := 0]
  }
  if ("flag_speed_zero_long_duration" %in% names(dt)) {
    dt[flag_speed_zero_long_duration == TRUE, feed_corrected := 0]
  }

  # 2) 速度过快 → 按生理上限封顶：feed ≤ speed_max × duration/60
  if ("flag_speed_too_fast" %in% names(dt) && !is.null(dur_col)) {
    dt[, .cap := speed_max * as.numeric(get(dur_col)) / 60]
    dt[flag_speed_too_fast == TRUE & !is.na(.cap) & .cap > 0,
       feed_corrected := pmin(feed_corrected, .cap)]
    dt[, .cap := NULL]
  }

  # 3) 单次采食过高 → 封顶到个体 P99（用干净记录计算，避免被异常值抬高）
  if ("flag_feed_too_high" %in% names(dt)) {
    dt[, feed_p99 := stats::quantile(
          feed_corrected[is_outlier_feed == FALSE & feed_corrected > 0],
          0.99, na.rm = TRUE), by = animal_id]
    dt[flag_feed_too_high == TRUE & !is.na(feed_p99),
       feed_corrected := pmin(feed_corrected, feed_p99)]
    dt[, feed_p99 := NULL]
  }

  # 4) 时长类异常 / speed_too_slow / STL → 保留原值（时长错但采食量可能对），无需处理

  n_corrected <- sum(dt$is_outlier_feed == TRUE, na.rm = TRUE)
  message(sprintf("Record-level feed correction: corrected %d flagged records via physics rules.", n_corrected))

  list(success = TRUE, feed_corrected = dt$feed_corrected)
}

#' FCR anchor correction: cap daily feed intake by national FCR ranges
#'
#' 用国标 FCR 范围（Table 2）作为生物学锚点：每天预期采食 = ADG × FCR。
#' 实际采食偏离 [ADG×fcr_min, ADG×fcr_max] 带超过阈值时，双向 cap 回带边界。
#'
#' @param daily_dt Daily-level data.table with daily_weight_g and daily_feed_g
#' @param config Configuration list with national_standard$fcr_ranges
#' @return daily_dt with corrected daily_feed_g and flag_feed_fcr_corrected
#' @keywords internal
.correct_feed_with_fcr_anchor <- function(daily_dt, config) {
  dt <- data.table::copy(daily_dt)
  fcr_ranges <- data.table::as.data.table(data.table::copy(
    config$national_standard$fcr_ranges))
  th <- config$national_standard$fcr_anchor_threshold
  if (is.null(th)) th <- 0.5

  dt[, flag_feed_fcr_corrected := FALSE]
  data.table::setorder(dt, animal_id, record_date)

  ids <- unique(dt$animal_id)
  for (id in ids) {
    idx <- which(dt$animal_id == id)
    w <- dt$daily_weight_g[idx] / 1000            # kg
    f <- dt$daily_feed_g[idx]
    d <- as.numeric(dt$record_date[idx] - min(dt$record_date[idx]))

    # 日增重（g/天），首日无前值为 NA
    adg <- c(NA_real_, diff(dt$daily_weight_g[idx]) / as.numeric(diff(d)))

    for (i in seq_along(idx)) {
      if (i == 1 || is.na(adg[i]) || adg[i] <= 0) next
      if (is.na(f[i]) || is.na(w[i]) || w[i] < 30 || w[i] > 120) next

      stage <- fcr_ranges[w[i] >= weight_min & w[i] < weight_max]
      if (nrow(stage) == 0) next
      fcr_min <- stage$fcr_min[1]
      fcr_max <- stage$fcr_max[1]

      upper <- adg[i] * fcr_max
      lower <- adg[i] * fcr_min

      if (f[i] > upper * (1 + th)) {
        data.table::set(dt, idx[i], "daily_feed_g", upper * (1 + th))
        data.table::set(dt, idx[i], "flag_feed_fcr_corrected", TRUE)
      } else if (f[i] < lower * (1 - th)) {
        data.table::set(dt, idx[i], "daily_feed_g", lower * (1 - th))
        data.table::set(dt, idx[i], "flag_feed_fcr_corrected", TRUE)
      }
    }
  }
  dt
}

#' LMM Feed Intake Correction Engine
#'
#' 日级兜底校正：仅在记录级物理纠正失败或被配置关闭时触发。
#' Phase 1 重构（issue #5，V1.1.2）修复四个统计缺陷：
#' 1. 协变量加入 visits_n——异常条数与当日活动强度机械相关，不控制强度时
#'    flag 系数会把「当天访问多」的效应误吸收进补偿量；
#' 2. 被 flag 记录改用「时长量纲特征」入模：补偿量与被丢采食时长成比例
#'    （近似与丢失的真实克数成比例），而非与异常次数成比例；
#'    无时长列或日级 flag（STL）自动退回计数特征；
#' 3. 训练集不再按 0 < normal_feed_sum ≤ 6000 截断——截断系统性丢弃大采食天，
#'    让系数低估真实损失；生理上限只在出口做校验（打标 + 置 NA），不筛训练样本；
#' 4. 补偿加回量受物理速率约束：add-back ≤ speed_max × 被flag记录总时长 / 60
#'    （把记录级物理规则的先验吸收进 LMM）。
#' 另新增台账列 lmm_correction_g（每日净校正值），全程可追溯。
#'
#' stack 模式（`use_lmm_stacking=TRUE` 且记录级纠正成功时）：在物理纠正后的
#' 日值上做**互补式**校正——只建模「噪声置零类」flag（负值/极高速小采食/
#' 长时间零速被物理规则置 0 的记录）的时长特征；已被物理封顶恢复的
#' speed_too_fast / feed_too_high 不再入模，避免二次补偿。响应为纠正后的
#' 日值本身，校正面为加法（daily_feed_g += correction）；NA 天不复活，
#' 留给插补。
#'
#' @param daily_dt Daily-level data.table (aggregated output of Step 5)
#' @param raw_dt Standard-record-level data.table with QC flags
#' @param ns_cfg Optional national_standard config list (read for `speed_max`)
#' @param stack Logical; TRUE = 互补叠加模式（见上），FALSE = 兜底模式
#' @return Daily-level data.table with corrected daily_feed_g and ledger column
#'   lmm_correction_g
#' @keywords internal
.apply_feed_lmm_correction <- function(daily_dt, raw_dt, ns_cfg = NULL, stack = FALSE) {
  dt <- data.table::copy(daily_dt)
  # issue #30：下面要给 raw_dt 加 is_feed_normal_record 列，先 copy 避免按引用
  # 改写调用方的表（与 daily_dt 的处理一致）
  raw_dt <- data.table::copy(raw_dt)

  # 10 error flags for single record anomalies (including STL time series flag)
  err_flags <- c("flag_duration_negative", "flag_duration_too_long",
                 "flag_duration_zero_with_feed", "flag_speed_too_slow",
                 "flag_speed_too_fast", "flag_speed_extreme_low_feed",
                 "flag_speed_zero_long_duration", "flag_feed_negative",
                 "flag_feed_too_high", "flag_STL_FI")

  # 噪声置零类：物理规则会把这类记录置 0（真实克数完全丢失）——
  # stack 模式下唯一允许 LMM 补偿的损失类别
  noise_flags <- c("flag_feed_negative", "flag_speed_extreme_low_feed",
                   "flag_speed_zero_long_duration")
  
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
    # 无 feed 列时跳过记录级纠正与 LMM，但出口校验仍要走（issue #21：原先直接
    # return 使此路径缺少 flag_daily_feed_over_limit 列，与另两条出口口径不一致）
    return(.finalize_daily_feed(dt))
  }
  
  # We do not exclude flag_feed_out_of_range, retaining this rule
  # issue #23：原用 .SD[[feed_col]] 会按组物化全部列再取一列；get(feed_col) 只取
  # 目标列，结果相同（分组 j 内 get() 取到的即该组的目标列向量）
  daily_features <- raw_dt[, .(
    normal_feed_sum = sum(get(feed_col)[is_feed_normal_record == TRUE], na.rm = TRUE)
  ), by = .(animal_id, record_date)]

  dt <- merge(dt, daily_features, by = c("animal_id", "record_date"), all.x = TRUE)

  # 每 flag 类型构建两个日级特征：
  #   has_<flag> : 异常发生次数（计数口径）
  #   dur_<flag> : 该类型被 flag 记录的累计有效时长（秒，量纲口径，优先使用）
  # 时长保留了单次采食事件的规模信息：同样是 speed_too_fast，丢掉 60s 的真实
  # 采食和丢掉 5s 的不应获得同样的补偿。flag_STL_FI 是日级标记，只用计数。
  dur_col <- if ("duration_sec" %in% names(raw_dt)) "duration_sec"
             else if ("Duration" %in% names(raw_dt)) "Duration" else NULL

  for (flg in err_flags) {
    if (flg %in% names(raw_dt)) {
      if (!is.null(dur_col) && flg != "flag_STL_FI") {
        flg_agg <- raw_dt[get(flg) %in% TRUE, .(
          flg_n = .N,
          flg_dur = sum(pmax(as.numeric(get(dur_col)), 0), na.rm = TRUE)
        ), by = .(animal_id, record_date)]
      } else {
        flg_agg <- raw_dt[get(flg) %in% TRUE, .(
          flg_n = .N,
          flg_dur = 0
        ), by = .(animal_id, record_date)]
      }
      data.table::setnames(flg_agg, c("flg_n", "flg_dur"),
                           c(paste0("has_", flg), paste0("dur_", flg)))
      dt <- merge(dt, flg_agg, by = c("animal_id", "record_date"), all.x = TRUE)
      cnt_name <- paste0("has_", flg)
      dur_name <- paste0("dur_", flg)
      dt[is.na(get(cnt_name)), (cnt_name) := 0L]
      dt[is.na(get(dur_name)), (dur_name) := 0]
    } else {
      # Default to 0 if a flag is missing in the input
      dt[, paste0("has_", flg) := 0L]
      dt[, paste0("dur_", flg) := 0]
    }
  }

  # 被 flag 记录的当日总时长（任意 flag 口径、不重复计多 flag 记录）：
  # 用作补偿加回量的物理速率封顶基数
  if (!is.null(dur_col) && "is_outlier_feed" %in% names(raw_dt)) {
    dur_tot <- raw_dt[is_outlier_feed %in% TRUE, .(
      flagged_dur_total = sum(pmax(as.numeric(get(dur_col)), 0), na.rm = TRUE)
    ), by = .(animal_id, record_date)]
    dt <- merge(dt, dur_tot, by = c("animal_id", "record_date"), all.x = TRUE)
    dt[is.na(flagged_dur_total), flagged_dur_total := 0]
  } else {
    dt[, flagged_dur_total := 0]
  }

  # stack 模式的速率封顶基数：仅噪声置零类的总时长（互补口径——只有这类
  # 损失允许 LMM 补偿，封顶也只对这部分时长生效）
  if (stack && !is.null(dur_col)) {
    has_noise_any <- Reduce(`|`, lapply(noise_flags, function(f) {
      if (f %in% names(raw_dt)) raw_dt[[f]] %in% TRUE else rep(FALSE, nrow(raw_dt))
    }))
    if (any(has_noise_any)) {
      noise_tot <- raw_dt[has_noise_any, .(
        noise_dur_total = sum(pmax(as.numeric(get(dur_col)), 0), na.rm = TRUE)
      ), by = .(animal_id, record_date)]
      dt <- merge(dt, noise_tot, by = c("animal_id", "record_date"), all.x = TRUE)
      dt[is.na(noise_dur_total), noise_dur_total := 0]
    } else {
      dt[, noise_dur_total := 0]
    }
  } else {
    dt[, noise_dur_total := 0]
  }

  # ==== 2. Construct individual daily weight gain (Covariate) ====
  data.table::setorder(dt, animal_id, record_date)
  # 个体日增重 = 相邻两天体重差 / 相邻两天天数差（g/天）
  dt[, adg_g := c(NA, diff(daily_weight_g) / as.numeric(diff(record_date))), by = animal_id]
  # 首日无前值，补 0 避免干扰训练
  dt[is.na(adg_g), adg_g := 0]
  
  # ==== 3. LMM Preparation and Modeling ====
  # 响应变量：fallback = 干净记录和 normal_feed_sum（覆写口径）；
  #          stack = 记录级物理纠正后的日值本身（加法口径）
  response_col <- if (stack) "daily_feed_g" else "normal_feed_sum"
  if (requireNamespace("lme4", quietly = TRUE)) {
    # Only days with valid covariates are modeled as the dependent variable。
    # 注意：训练集不再按 0 < sum ≤ 6000 截断（Phase 1 修复）——截断会系统性
    # 丢弃大采食天、低估损失系数；生理上限只在出口校验（见第 4 步）
    train_idx <- !is.na(dt[[response_col]]) & !is.na(dt$daily_weight_g) & !is.na(dt$adg_g)

    # Include Location as a fixed effect only if it exists and has > 1 unique value
    has_loc <- "location" %in% names(dt) && length(unique(stats::na.omit(dt$location))) > 1
    has_breed <- "breed" %in% names(dt) && length(unique(stats::na.omit(dt$breed))) > 1

    if (sum(train_idx) > 30) {
      # Rescale large covariates (grams to kg) to avoid lme4 optimizer warning: "Some predictor variables are on very different scales"
      formula_str <- paste(response_col, "~ I(daily_weight_g / 1000) + I(adg_g / 1000)")

      # 解混杂关键项：异常条数与当日活动强度机械相关（访问越多的天越容易出现异常
      # 记录），不控制 visits_n 时 flag 系数会把「当天采食活动多」误吸收进补偿量
      if ("visits_n" %in% names(dt)) formula_str <- paste(formula_str, "+ visits_n")
      if (has_loc) formula_str <- paste(formula_str, "+ location")
      if (has_breed) formula_str <- paste(formula_str, "+ breed")

      # 每个 flag 选一个入模特征：优先时长量纲；该特征在训练集中无变异时退回计数。
      # stack 模式只入模噪声置零类——其余类型已被物理规则恢复，二次补偿会重复计数
      active_feats <- character()
      for (flg in err_flags) {
        if (stack && !flg %in% noise_flags) next
        picked <- NULL
        for (cand in c(paste0("dur_", flg), paste0("has_", flg))) {
          vals <- dt[[cand]][train_idx]
          if (length(unique(vals[!is.na(vals)])) > 1) {
            picked <- cand
            break
          }
        }
        if (!is.null(picked)) {
          active_feats <- c(active_feats, picked)
          formula_str <- paste(formula_str, "+", picked)
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
        # correction = sum_active ( - beta_i * feature_i )；
        # β 预期为负（被 flag 时长越长、干净日和越低），故 -β×feature 为正的补偿加回
        dt[, lmm_correction_g := 0]

        for (feat in active_feats) {
          if (feat %in% names(fixed_eff)) {
            beta_val <- fixed_eff[[feat]]
            # issue #13：补偿语义是「加回被丢的真实采食」，-β×feature 依赖
            # β<0 先验；共线性/小样本可能估出 β>0，此时该项会把日值往下减。
            # 跳过该特征并告警，不让方向错误的补偿进入应用环节。
            if (beta_val > 0) {
              warning(sprintf(
                "LMM Feed Correction: coefficient for %s has unexpected sign (beta = %.4f > 0); feature skipped.",
                feat, beta_val), call. = FALSE)
              next
            }
            dt[, lmm_correction_g := lmm_correction_g - beta_val * get(feat)]
          }
        }

        # 物理速率封顶：补偿加回量 ≤ speed_max × 目标类别总时长 / 60，
        # 即加回部分隐含的采食速率不得超过生理上限（吸收记录级物理规则作先验）；
        # stack 模式只对噪声置零类的时长封顶（互补口径）。
        # 下限 0（issue #13）：补偿是「加回」，物理上不为负——pmax 兜底
        # 防止任何未来路径把日值往下减（减穿 0 会被出口校验静默置 NA）。
        speed_max <- if (!is.null(ns_cfg$speed_max)) as.numeric(ns_cfg$speed_max) else 170
        cap_base <- if (stack) dt$noise_dur_total else dt$flagged_dur_total
        cap_g <- speed_max * cap_base / 60
        dt[, lmm_correction_g := pmax(0, pmin(lmm_correction_g, cap_g))]

        n_corrected <- sum(abs(dt$lmm_correction_g) > 0.001, na.rm = TRUE)
        n_capped <- sum(cap_base > 0 &
                          (cap_g - dt$lmm_correction_g) <= 0.001, na.rm = TRUE)
        mean_abs_corr <- if (n_corrected > 0) {
          mean(abs(dt$lmm_correction_g[abs(dt$lmm_correction_g) > 0.001]), na.rm = TRUE)
        } else 0
        mode_tag <- if (stack) "LMM Feed Correction (stack)" else "LMM Feed Correction"
        message(sprintf("%s: corrected %d daily records (%d rate-capped), mean |correction| = %.1f g.",
                        mode_tag, n_corrected, n_capped, mean_abs_corr))

        if (stack) {
          # 加法应用：在物理纠正结果上追加补偿；NA 天不复活（留给插补）
          dt[!is.na(daily_feed_g), daily_feed_g := daily_feed_g + lmm_correction_g]
        } else {
          # 覆写应用：干净记录和 + 统计补偿（台账列 lmm_correction_g 保留在输出中）
          dt[!is.na(normal_feed_sum), daily_feed_g := normal_feed_sum + lmm_correction_g]
        }
      } else {
        message("LMM Feed Correction: Model fitting failed or skipped, 0 records corrected.")
        if (!stack) dt[, daily_feed_g := normal_feed_sum]
        dt[, lmm_correction_g := 0]
      }
    } else {
      # If training samples are too few, skip LMM inference entirely
      message(sprintf("LMM Feed Correction: Insufficient valid samples for training (%d <= 30), 0 records corrected.", sum(train_idx)))
      if (!stack) dt[, daily_feed_g := normal_feed_sum]
      dt[, lmm_correction_g := 0]
    }
  } else {
    warning("Package 'lme4' is not installed. Ignoring LMM feed correction.")
    if (!stack) dt[, daily_feed_g := normal_feed_sum]
    dt[, lmm_correction_g := 0]
  }

  # ==== 4. 出口生理校验（对所有路径统一执行） ====
  dt <- .finalize_daily_feed(dt)

  # Clean temporary feature columns used in the process（台账列 lmm_correction_g 保留）
  cols_to_remove <- c("normal_feed_sum", "adg_g",
                      paste0("has_", err_flags), paste0("dur_", err_flags),
                      "flagged_dur_total", "noise_dur_total", "daily_feed_g_corrected")
  cols_to_remove <- intersect(cols_to_remove, names(dt))
  dt[, (cols_to_remove) := NULL]

  dt[]
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
