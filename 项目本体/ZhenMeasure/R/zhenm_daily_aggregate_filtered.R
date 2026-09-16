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
#'   optional FCR anchor correction (`national_standard$use_fcr_anchor`), the correction
#'   mechanism switches (`use_record_feed_correction`, `use_lmm_feed_correction`) and the
#'   LMM covariate truncation bounds (`lmm_trim_dfie_g`, `lmm_trim_otde_s`).
#'   NULL keeps default behaviour.
#'   开关依赖（issue #5 重写后）：`use_lmm_feed_correction=FALSE`（默认）时不跑 LMM，
#'   `daily_feed_g` 由记录级物理纠正（A）产生；设 TRUE 则日级文献 LMM **恒运行**，
#'   `daily_feed_g` 改由它产生，记录级物理纠正的产物不再进入日值，只作内部对照。
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
  
  # 解析校正机制开关（issue #5 重写后语义）：use_lmm_fix 决定 daily_feed_g 的来源——
  # TRUE 时日级文献 LMM 恒运行；FALSE 时退回记录级物理纠正（A）的产物。
  ns_cfg <- if (!is.null(config)) config$national_standard else NULL
  use_record_fix <- if (!is.null(ns_cfg$use_record_feed_correction)) {
    isTRUE(ns_cfg$use_record_feed_correction)
  } else TRUE
  # 缺键兜底必须与出厂默认一致（FALSE = A）：配置对象是手工拼的、或调用方
  # 没走 ZhenM_merge_config() 时，这里若退回 TRUE 会静默改用文献 LMM，正是
  # 「配置没传到就换了路径」那类坑。
  use_lmm_fix <- if (!is.null(ns_cfg$use_lmm_feed_correction)) {
    isTRUE(ns_cfg$use_lmm_feed_correction)
  } else FALSE

  # 被 flag 记录 = 事件真实发生但采食量错误，按 flag 类型用物理规则纠正（而非置零）。
  # 纠正失败或被配置关闭时退回「置零」。
  # feed_filtered 是 **A 臂（记录级物理纠正）的唯一载体**：下面日级聚合直接对它
  # 求和得 daily_feed_g。use_lmm_feed_correction = FALSE（默认）时它就是日值本身；
  # TRUE 时它先被算出，随后整体被日级文献 LMM 覆写（见本函数后半的 use_lmm_fix
  # 门控），届时才只作 A 臂对照。也正因如此，这里的成败不需要向外传递。
  if (!is.null(feed_col) && use_record_fix) {
    corrected <- .correct_feed_records(
      dt, speed_max = if (!is.null(ns_cfg$speed_max)) as.numeric(ns_cfg$speed_max) else 170)
    if (corrected$success) {
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

  # issue #5 重写：门控与记录级纠正的成败解耦。此前日级 LMM 只在记录级纠正
  # 「关闭或失败」时兜底运行，而记录级纠正成功恰是默认情况——不解耦的话，
  # 文献化的 LMM 在出厂配置下永远不跑。现在 use_lmm_fix 单独决定
  # daily_feed_g 的来源，记录级纠正的产物退居内部对照列。
  if (use_lmm_fix) {
    result <- .apply_feed_lmm_correction(result, dt, ns_cfg)
  } else {
    # 默认路径：daily_feed_g 保持上面聚合出的记录级物理纠正（A）产物，
    # 仅保留日采食量上限校验（保证各路径口径一致）
    result <- .finalize_daily_feed(result, .feed_daily_max_g(ns_cfg))
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
#' `flag_daily_feed_over_limit` 并置 NA（等插补）；≤0 的天打标
#' `flag_daily_feed_nonpositive` 后同样置 NA。三条出口路径（跳过 LMM / LMM 出口
#' / 无 feed 列提前返回）统一调用，避免口径漂移。只做出口把关，不筛 LMM 训练样本
#' ——注意这与 LMM 的协变量截尾是两件事：前者管**响应**的生理上限，后者管
#' **逐错误类型的累计协变量**的极端值。
#'
#' @param dt 日级表（含 daily_feed_g）
#' @param feed_max_g 日级采食量上限（g）。默认 6000（=config 默认
#'   `feed_intake_range = c(0, 6)` kg 的上界），由 `.feed_daily_max_g()` 从
#'   config 接线（issue #40）——此前该值硬编码，配置项改了也不生效。
#' @return 原地修改并返回 dt，新增 `flag_daily_feed_over_limit` /
#'   `flag_daily_feed_nonpositive` 两列
#' @keywords internal
.finalize_daily_feed <- function(dt, feed_max_g = 6000) {
  dt[, flag_daily_feed_over_limit := !is.na(daily_feed_g) & daily_feed_g > feed_max_g]
  # 文献口径下校正量为负（或大到把日值压穿）时不再有 pmax(0,·) 兜底，单独打标
  # 使这条路径可审计，而不是静默变 NA
  dt[, flag_daily_feed_nonpositive := !is.na(daily_feed_g) & daily_feed_g <= 0]
  dt[!is.na(daily_feed_g) & (daily_feed_g <= 0 | flag_daily_feed_over_limit == TRUE),
     daily_feed_g := NA_real_]
  dt[]
}

#' 日级采食量生理上限（g），从 config 的 feed_intake_range 上界接线（issue #40）
#'
#' 与 `zhenm_qc_feed_standard.R` 的记录级量程判定共用同一个配置键：config 中以
#' kg 给出，此处过 `.normalize_feed_range()` 转克（V0.2.6 C-1 的量纲陷阱）。
#' config 缺键或区间非法时退回 6000 g，保持既有行为。
#'
#' @param ns_cfg national_standard 配置列表（可为 NULL）
#' @return 单个数值（g）
#' @keywords internal
.feed_daily_max_g <- function(ns_cfg) {
  if (is.null(ns_cfg) || is.null(ns_cfg$feed_intake_range)) return(6000)
  rng <- .normalize_feed_range(ns_cfg$feed_intake_range)
  if (length(rng) < 2 || !is.finite(rng[2])) 6000 else as.numeric(rng[2])
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
  feed_col <- if ("feed_g" %in% names(dt)) "feed_g"
              else if ("Feed_intake" %in% names(dt)) "Feed_intake" else NULL
  if (is.null(feed_col) || !"is_outlier_feed" %in% names(dt)) {
    return(list(success = FALSE, feed_corrected = NULL))
  }

  dur_col <- if ("duration_sec" %in% names(dt)) "duration_sec"
             else if ("Duration" %in% names(dt)) "Duration" else NULL

  # issue #38：本函数是纯函数——只读 dt、只返回一个向量，从不改写调用方的表。
  # 原实现在入口做 data.table::copy(整表)，只为拿到一块可写的 feed_corrected 列；
  # 实测这次 copy 使进程峰值 RSS 增加 264 MB（1522 → 1786 MB，/usr/bin/time -v
  # 分进程计时，扬翔 668 头 179 万条）。改为直接对被 flag 掩码命中的位置做向量
  # 运算：不改 dt、不产生整表副本，返回值与原实现逐位一致。
  #
  # 初始保留原采食量（被 flag 记录不置零，只对「明显离谱」的封顶/归零）
  fc <- as.numeric(dt[[feed_col]])

  # speed_max 由调用方从 config 传入（issue #12），缺省 170 与
  # zhenm_config_defaults.R 的 speed_max 保持一致

  # 1) 纯噪声 → 0
  if ("flag_feed_negative" %in% names(dt)) {
    fc[dt[["flag_feed_negative"]] %in% TRUE] <- 0
  }
  if ("flag_speed_extreme_low_feed" %in% names(dt)) {
    fc[dt[["flag_speed_extreme_low_feed"]] %in% TRUE] <- 0
  }
  if ("flag_speed_zero_long_duration" %in% names(dt)) {
    fc[dt[["flag_speed_zero_long_duration"]] %in% TRUE] <- 0
  }

  # 2) 速度过快 → 按生理上限封顶：feed ≤ speed_max × duration/60
  if ("flag_speed_too_fast" %in% names(dt) && !is.null(dur_col)) {
    .cap <- speed_max * as.numeric(dt[[dur_col]]) / 60
    hit <- which(dt[["flag_speed_too_fast"]] %in% TRUE & !is.na(.cap) & .cap > 0)
    if (length(hit) > 0) fc[hit] <- pmin(fc[hit], .cap[hit])
  }

  # 3) 单次采食过高 → 封顶到个体 P99（用干净记录计算，避免被异常值抬高）
  if ("flag_feed_too_high" %in% names(dt)) {
    # 池子口径与原实现逐字一致：已过第 1、2 步的 feed_corrected、且非采食异常、
    # 且为正。NA 的比较结果（NA 掩码元素）被 quantile(na.rm=TRUE) 丢弃，这里
    # 直接以 !is.na 显式排除，取值集合相同。
    keep <- !is.na(dt[["is_outlier_feed"]]) &
            dt[["is_outlier_feed"]] == FALSE &
            !is.na(fc) & fc > 0
    idx_hi <- which(dt[["flag_feed_too_high"]] %in% TRUE)
    if (any(keep) && length(idx_hi) > 0) {
      # 只物化 2 列（个体 + 采食量）的临时表算分组 P99，不再复制整张宽表
      pool <- data.table::setDT(list(animal_id = dt[["animal_id"]][keep], .fc = fc[keep]))
      p99 <- pool[, .(.p99 = stats::quantile(.fc, 0.99, na.rm = TRUE)), by = animal_id]
      p99v <- p99$.p99[match(as.character(dt[["animal_id"]][idx_hi]),
                             as.character(p99$animal_id))]
      ok <- !is.na(p99v)
      if (any(ok)) {
        j <- idx_hi[ok]
        fc[j] <- pmin(fc[j], p99v[ok])
      }
    }
  }

  # 4) 时长类异常 / speed_too_slow / STL → 保留原值（时长错但采食量可能对），无需处理

  n_corrected <- sum(dt$is_outlier_feed == TRUE, na.rm = TRUE)
  message(sprintf("Record-level feed correction: corrected %d flagged records via physics rules.", n_corrected))

  list(success = TRUE, feed_corrected = fc)
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
  # issue #36：循环外建「个体 → 行号」查表，取代循环内的全表扫描。
  # 循环体只通过 data.table::set() 改 daily_feed_g 的值，nrow 与行序不变。
  rows_by_id <- .build_row_index(dt)
  for (id in ids) {
    idx <- .row_index_of(rows_by_id, id)
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

#' 逐错误类型的 LMM 协变量指派表（Jiao et al. 2014 的 16 类错误）
#'
#' 文献把协变量**预先**指派给错误类型（不是数据驱动挑选）：
#'   - `ETP_p` 给全部 16 类；
#'   - `OTD_p` 给类型 1,2 与 6–14（共 11 类）；
#'   - `FID_p` 给类型 4,5 与 15,16（共 4 类）。
#' 校验式 16 + 11 + 4 = 31，对应正文 "31 variables created from the 16 error
#' counts"。Jiao et al. (2016) 用 8 类时列的是 FID 给类型 4,5、OTD 给类型
#' 1,2,7,8——均为本文区间的子集，两篇互证。
#'
#' 我们的 QC 只覆盖其中一部分错误类型，对应关系见 `flag` 列；`NA` 表示该类型
#' 我们未采集：LWD/FWD（类型 11–14）需要每次访问**分别**记录入场与离场体重，
#' 而标准格式每次访问只有一个 `Weight` 列（数据格式限制，非实现取舍）；
#' FRV-high-strict（类型 7）需要「看下一条访问」的配对条件，未实现。
#'
#' `flag_STL_FI` 是我们自有的 STL 日级标记，文献 16 类里**没有**对应类型，
#' 按裁定以 ETP-only 形式保留为扩展项。
#'
#' @return data.table，每行一个入模项，列为
#'   `err_type`（文献类型号，扩展项为 NA）、`label`、`flag`、`kind`
#'   （etp/otd/fid）、`term`（= `paste0(kind, "_", flag)`，即模型项名）。
#'   当前共 18 项：ETP 10 + OTD 6 + FID 2。
#' @keywords internal
.lmm_covariate_spec <- function() {
  types <- data.table::data.table(
    err_type = 1:16,
    label = c("FIV-low", "FIV-high", "FIV-0", "OTV-low", "OTV-high",
              "FRV-high-FIV-low", "FRV-high-strict", "FRV-high", "FRV-0",
              "FRV-low", "LWD-low", "LWD-high", "FWD-low", "FWD-high",
              "LTD-low", "FTD-high"),
    flag = c("flag_feed_negative", "flag_feed_too_high",
             "flag_duration_zero_with_feed", "flag_duration_negative",
             "flag_duration_too_long", "flag_speed_extreme_low_feed",
             NA_character_, "flag_speed_too_fast",
             "flag_speed_zero_long_duration", "flag_speed_too_slow",
             NA_character_, NA_character_, NA_character_, NA_character_,
             NA_character_, NA_character_)
  )

  # 文献的指派区间（见函数说明与校验式 16+11+4=31）
  otp_types <- c(1:2, 6:14)
  fid_types <- c(4:5, 15:16)

  spec <- data.table::data.table(
    err_type = rep(types$err_type, 3L),
    kind     = rep(c("etp", "otd", "fid"), each = nrow(types))
  )
  spec <- spec[
    (kind == "etp") |
      (kind == "otd" & err_type %in% otp_types) |
      (kind == "fid" & err_type %in% fid_types)
  ]
  spec <- merge(spec, types, by = "err_type", all.x = TRUE, sort = FALSE)
  # 我们未采集的类型（flag 为 NA）不进模型
  spec <- spec[!is.na(flag)]
  spec[, term := paste0(kind, "_", flag)]

  # 超出文献的扩展项：STL 日级标记，ETP-only
  spec <- data.table::rbindlist(list(
    spec[, .(err_type, label, flag, kind, term)],
    data.table::data.table(
      err_type = NA_integer_, label = "STL (extension)",
      flag = "flag_STL_FI", kind = "etp", term = "etp_flag_STL_FI"
    )
  ))
  data.table::setorder(spec, err_type, kind, na.last = TRUE)
  spec[]
}

#' 个体全期平均日增重（g/天），猪内常数
#'
#' 文献的 `ADG_m` 下标只有 m（猪）、是**全期常数**，与逐日体重差分不是同一个
#' 变量（issue #5 重写前误用了后者）。此处以个体「日体重 ~ 日序」的最小二乘
#' 斜率估计，比「首末两点差 / 天数」稳健——后者完全由两端的称重噪声决定。
#'
#' 有效体重天 < 2 或日期跨度为 0 时返回 NA：该头退出 LMM 训练集，但校正在应用
#' 端照常作用于它（校正量不依赖 ADG）。
#'
#' @param dt 日级表，需含 `animal_id` / `record_date` / `daily_weight_g`
#' @return 与 `nrow(dt)` 等长的数值向量（g/天）
#' @keywords internal
.lmm_adg_per_animal <- function(dt) {
  res <- rep(NA_real_, nrow(dt))
  if (nrow(dt) == 0L) return(res)

  tmp <- data.table::data.table(
    .id = dt[["animal_id"]],
    .d  = dt[["record_date"]],
    .w  = dt[["daily_weight_g"]]
  )

  slopes <- tmp[!is.na(.w), {
    if (.N < 2L) {
      NA_real_
    } else {
      x <- as.numeric(.d - min(.d))
      xc <- x - mean(x)
      den <- sum(xc * xc)
      # den == 0 即全部体重落在同一天（跨度为 0），斜率无定义
      if (den <= 0) NA_real_ else sum(xc * (.w - mean(.w))) / den
    }
  }, by = .id]

  if (nrow(slopes) == 0L) return(res)

  # 同一头的 ADG 是常数，按**动物**回填到该头的每一行——包括体重缺失的行
  # （斜率只用有体重的天估计，但那头动物的 ADG 对它同样成立）
  res[] <- slopes[[2L]][match(tmp$.id, slopes[[1L]])]
  res
}

#' 构建日级 LMM 协变量（文献的 ETP / OTD / FID 与响应）
#'
#' 把记录级表聚合成 `.apply_feed_lmm_correction()` 直接可用的日级宽表。抽出成
#' 独立 internal 函数有两个好处：协变量口径可以脱离 `lme4` 单测；文献的
#' 「response = error-free DFI」「ETP 是占比」「OTD/FID 是逐类型累计量」三条
#' 语义各自有了唯一的定义处。
#'
#' 列含义：
#'   - `ef_dfi_g`   error-free 日和 = 干净记录的采食量之和（**响应** Y）
#'   - `ef_n_visit` 当日干净访问数；为 0 时 Y 无定义（该行退出训练集）
#'   - `n_/dur_/feed_<flag>` 逐 flag 的命中次数 / 累计占据秒数 / 累计采食克数
#'   - `etp_<flag>` = `n_<flag> / 当日全部访问数`
#'   - `otd_<flag>` = `dur_<flag>`（秒）；`fid_<flag>` = `feed_<flag>`（克）
#'
#' 细节口径：
#'   - 「干净记录」= 10 个 error flag 全 FALSE、非 feed 离群，且**排除**
#'     `flag_feed_out_of_range`（语义是设备故障、整天不可用，不该计进 Y）。
#'   - `feed_<flag>` 用**原始** `feed_g`：文献的 FID 就是出错访问的记录值之和，
#'     不做任何纠正。
#'   - `flag_STL_FI` 是日级标记（打在当天全部记录上），没有单条记录时长，
#'     故 `dur_` 取 0；它只作 ETP 项入模。
#'   - 缺失的 flag 列视为该类从未命中，三个派生量恒 0。
#'
#' @param raw_dt 记录级标准表（含 `animal_id` / `record_date` / `feed_g`）
#' @param spec `.lmm_covariate_spec()` 的输出
#' @return 日级 data.table，按 `animal_id` + `record_date` 唯一
#' @keywords internal
.lmm_daily_covariates <- function(raw_dt, spec = .lmm_covariate_spec()) {
  err_flags <- sort(unique(spec$flag))

  normal_rec <- rep(TRUE, nrow(raw_dt))
  if ("is_outlier_feed" %in% names(raw_dt)) {
    normal_rec <- normal_rec & !(raw_dt[["is_outlier_feed"]] %in% TRUE)
  }
  if ("flag_feed_out_of_range" %in% names(raw_dt)) {
    normal_rec <- normal_rec & !(raw_dt[["flag_feed_out_of_range"]] %in% TRUE)
  }
  for (flg in err_flags) {
    if (flg %in% names(raw_dt)) {
      normal_rec <- normal_rec & !(raw_dt[[flg]] %in% TRUE)
    }
  }

  feed_col <- if ("feed_g" %in% names(raw_dt)) "feed_g"
              else if ("Feed_intake" %in% names(raw_dt)) "Feed_intake" else NULL
  if (is.null(feed_col)) stop("no feed column in raw_dt", call. = FALSE)
  # 有 duration 列时才算 OTD；flag_STL_FI 是日级标记，见函数说明
  dur_col <- if ("duration_sec" %in% names(raw_dt)) "duration_sec"
             else if ("Duration" %in% names(raw_dt)) "Duration" else NULL

  feats <- data.table::setDT(list(
    animal_id   = raw_dt[["animal_id"]],
    record_date = raw_dt[["record_date"]],
    .feed       = raw_dt[[feed_col]],
    .normal     = normal_rec
  ))[, .(
    ef_dfi_g     = sum(.feed[.normal], na.rm = TRUE),
    ef_n_visit   = sum(.normal),
    visits_total = .N
  ), by = .(animal_id, record_date)]

  for (flg in err_flags) {
    if (flg %in% names(raw_dt)) {
      if (!is.null(dur_col) && flg != "flag_STL_FI") {
        agg <- raw_dt[get(flg) %in% TRUE, .(
          n    = .N,
          # 占据时长为负正是判错依据本身、不是可用的量：取 0 是定义域处理，
          # 不是被本次重写移除的那类「校正护栏」
          dur  = sum(pmax(as.numeric(get(dur_col)), 0), na.rm = TRUE),
          feed = sum(get(feed_col), na.rm = TRUE)
        ), by = .(animal_id, record_date)]
      } else {
        agg <- raw_dt[get(flg) %in% TRUE, .(
          n = .N, dur = 0, feed = sum(get(feed_col), na.rm = TRUE)
        ), by = .(animal_id, record_date)]
      }
      data.table::setnames(
        agg, c("n", "dur", "feed"),
        c(paste0("n_", flg), paste0("dur_", flg), paste0("feed_", flg))
      )
      feats <- merge(feats, agg, by = c("animal_id", "record_date"), all.x = TRUE)
      for (nm in c(paste0("n_", flg), paste0("dur_", flg), paste0("feed_", flg))) {
        feats[is.na(get(nm)), (nm) := 0]
      }
    } else {
      for (nm in c(paste0("n_", flg), paste0("dur_", flg), paste0("feed_", flg))) {
        feats[, (nm) := 0]
      }
    }
  }

  # 派生入模项。ETP 的分母是**当日全部访问数**（文献 "percentage of visits with
  # error type p"）；分子为 0 时占比恒 0，故分母为 0 也取 0，不产生 NaN。
  denom <- feats[["visits_total"]]
  for (flg in spec[kind == "etp", flag]) {
    n_hit <- feats[[paste0("n_", flg)]]
    feats[, (paste0("etp_", flg)) := data.table::fifelse(denom > 0, n_hit / denom, 0)]
  }
  for (flg in spec[kind == "otd", flag]) {
    feats[, (paste0("otd_", flg)) := feats[[paste0("dur_", flg)]]]
  }
  for (flg in spec[kind == "fid", flag]) {
    feats[, (paste0("fid_", flg)) := feats[[paste0("feed_", flg)]]]
  }

  feats[, visits_total := NULL]
  feats[]
}

#' LMM Feed Intake Correction Engine（Jiao et al. 2014 文献实现）
#'
#' 日级采食量校正的**唯一引擎**（issue #5 重写）。逐字复现 Jiao et al. (2014,
#' *J Anim Sci* 92:2377–2386) 的线性混合模型：
#'
#'   Y = B_i + b1·BW + b2·ADG + Σ_p(b3p·ETP_p + b4p·OTD_p + b5p·FID_p) + P_m + e
#'
#'   - `Y`    = error-free daily feed intake（干净访问的当日采食量和，`ef_dfi_g`）
#'   - `B_i`  = 批次固定效应 ↔ `location`
#'   - `BW`   = 当日体重；`ADG` = **个体全期常数**（`.lmm_adg_per_animal()`）
#'   - `ETP_p` = 类型 p 的访问**占比**（分母为当日全部访问数）
#'   - `OTD_p` = 类型 p 访问的当日**累计占据时长**（秒）
#'   - `FID_p` = 类型 p 访问的当日**累计采食量**（克，用原始记录值）
#'   - `P_m`  = 个体随机截距 `(1 | animal_id)`
#'
#' 协变量按文献预先指派、不做数据驱动挑选，逐类型的对应关系与文献区间校验见
#' `.lmm_covariate_spec()`。我们的 QC 未采集类型 7、11–14，故实际入模 18 项。
#'
#' 应用同文献：`Correction = Σ(α·ETP + γ·OTD + δ·FID)`、
#' `daily_feed_g = ef_dfi_g + Correction`——**字面 +β̂x**。不做单侧截断、
#' 不加物理速率封顶、不按系数符号跳过（文献 Table 1 的系数有正有负，
#' 例如 FIV-high +61.40、OTV-high +1750.0）。
#'
#' 协变量截尾（Casey 2003，经 Jiao et al. 2016 转述）：拟合前剔除
#' `fid_*` / `otd_*` 越界的**训练行**，界见 config 的 `lmm_trim_dfie_g` /
#' `lmm_trim_otde_s`。注意被截的是逐错误类型的**累计协变量**（文献记作
#' DFIe/OTDe，e = error），**不是**当日总采食量、**也不是**响应——响应的生理
#' 上限由出口 `.finalize_daily_feed()` 的 `feed_intake_range` 单独把关。
#' 截尾只作用于训练集；应用端回填永不截尾。
#'
#' 其余口径：
#'   - 训练响应与全部协变量都用**原始**记录，与记录级物理纠正（A）无关；
#'     A 的产物不进日值（见 `ZhenM_standard_to_daily_filtered()` 的门控注释）。
#'   - 拟合失败或样本不足时**什么都不改** `daily_feed_g`（保留上游结果），
#'     绝不回退到 error-free 日和——它系统性丢掉被 flag 记录的采食量，
#'     正是本方法要消除的偏差。
#'   - 台账列 `lmm_ef_g`（error-free 日和）与 `lmm_correction_g`（当日校正量）
#'     保留在输出中，恒满足 `daily_feed_g == lmm_ef_g + lmm_correction_g`。
#'
#' @param daily_dt Daily-level data.table (aggregated output of Step 5)
#' @param raw_dt Standard-record-level data.table with QC flags
#' @param ns_cfg Optional national_standard config list (read for the two
#'   covariate truncation bounds)
#' @return Daily-level data.table with corrected `daily_feed_g` and ledger
#'   columns `lmm_ef_g` / `lmm_correction_g`
#' @keywords internal
.apply_feed_lmm_correction <- function(daily_dt, raw_dt, ns_cfg = NULL) {
  dt <- data.table::copy(daily_dt)
  # issue #38：原实现在此 copy(整表) 的唯一目的是给 raw_dt 按引用加一列
  # is_feed_normal_record（issue #30 为防止改写调用方表的防御性副本）。改为在
  # 局部向量上算「正常记录」掩码，raw_dt 全程只读——既保留 issue #30 的不改写
  # 契约，也不再产生一份 32 列 × 179 万行的整表副本。

  # 逐错误类型的协变量指派（文献预先指定，见 .lmm_covariate_spec()）
  spec      <- .lmm_covariate_spec()
  err_flags <- sort(unique(spec$flag))
  etp_flags <- spec[kind == "etp", flag]
  otd_flags <- spec[kind == "otd", flag]
  fid_flags <- spec[kind == "fid", flag]
  
  # ==== 1. 干净记录掩码与日级协变量聚合 ====
  # 无 feed 列时跳过 LMM，但出口校验仍要走（issue #21：原先直接 return 使此路径
  # 缺少 flag_daily_feed_over_limit 列，与另两条出口口径不一致）；台账列一并补齐，
  # 使输出 schema 不随输入漂移
  feed_col <- if ("feed_g" %in% names(raw_dt)) "feed_g"
              else if ("Feed_intake" %in% names(raw_dt)) "Feed_intake" else NULL

  if (is.null(feed_col)) {
    dt[, lmm_ef_g := NA_real_]
    dt[, lmm_correction_g := NA_real_]
    return(.finalize_daily_feed(dt, .feed_daily_max_g(ns_cfg)))
  }

  # 响应与全部协变量的构建口径集中在 .lmm_daily_covariates()（可脱离 lme4 单测）
  daily_features <- .lmm_daily_covariates(raw_dt, spec)
  dt <- merge(dt, daily_features, by = c("animal_id", "record_date"), all.x = TRUE)

  term_cols <- spec$term
  for (nm in term_cols) {
    # 孤儿行（daily_dt 里有、raw_dt 里没有的 animal-day）不参与训练，
    # 但协变量列不能留 NA 污染模型矩阵
    dt[is.na(get(nm)), (nm) := 0]
  }

  # ==== 2. 个体全期平均日增重（文献 ADG_m，猪内常数） ====
  # issue #5 重写：原用「相邻两天体重差 / 天数差」的**逐日**日增重，那是另一个
  # 变量（文献的 ADG_m 下标只有 m，全期恒定）；「首日补 0」也随之删除。
  data.table::setorder(dt, animal_id, record_date)
  adg_const <- .lmm_adg_per_animal(dt)
  dt[, adg_const_g := adg_const]
  
  # ==== 3. LMM 训练集与拟合 ====
  if (requireNamespace("lme4", quietly = TRUE)) {
    trim_dfie <- if (!is.null(ns_cfg$lmm_trim_dfie_g)) as.numeric(ns_cfg$lmm_trim_dfie_g) else c(0, 3500)
    trim_otde <- if (!is.null(ns_cfg$lmm_trim_otde_s)) as.numeric(ns_cfg$lmm_trim_otde_s) else c(0, 5000)

    # 协变量截尾（Casey 2003）：只在**训练集**上按逐错误类型的累计协变量剔除极端
    # 行。被截的是 fid_* / otd_* 这两个**协变量**——不是响应、也不是日总采食量
    # （文献记作 DFIe/OTDe，e = error，不是 error-free）；响应的生理上限由出口
    # .finalize_daily_feed() 单独把关。应用端回填永不截尾，否则恰好会取消掉最需
    # 要校正的天。
    trim_ok <- rep(TRUE, nrow(dt))
    for (nm in c(paste0("fid_", fid_flags), paste0("otd_", otd_flags))) {
      vals <- dt[[nm]]
      lim  <- if (startsWith(nm, "fid_")) trim_dfie else trim_otde
      trim_ok <- trim_ok & (is.na(vals) | (vals >= lim[1L] & vals <= lim[2L]))
    }

    # 训练行：Y 有定义（当天至少一条干净访问）、体重与 ADG 可得、协变量未越界
    train_idx <- !is.na(dt$ef_dfi_g) & !is.na(dt$daily_weight_g) &
      !is.na(dt$adg_const_g) & !is.na(dt$ef_n_visit) & dt$ef_n_visit >= 1 & trim_ok

    has_loc   <- "location" %in% names(dt) && length(unique(stats::na.omit(dt$location))) > 1
    has_breed <- "breed" %in% names(dt) && length(unique(stats::na.omit(dt$breed))) > 1

    # 逐项剔除训练集内零变异的协变量：lme4 对常量列会秩亏或给 NA 系数，属工程护
    # 栏而非方法偏离——但被剔的项必须报出来，静默改变模型规格是最危险的失效。
    active_terms  <- character()
    dropped_terms <- character()
    for (tm in spec$term) {
      vals <- dt[[tm]][train_idx]
      if (length(unique(vals[!is.na(vals)])) > 1L) {
        active_terms <- c(active_terms, tm)
      } else {
        dropped_terms <- c(dropped_terms, tm)
      }
    }

    # 门槛随入模项数放大：文献式模型有 18 个固定效应 + 随机截距，30 行 / 2 头动物
    # 在数值上无意义（旧阈值是硬编码的 sum(train_idx) > 30）
    n_animal_train <- data.table::uniqueN(dt$animal_id[train_idx])
    min_train <- max(30L, 10L * length(active_terms))

    # Rescale large covariates (grams to kg) to avoid lme4 optimizer warning:
    # "Some predictor variables are on very different scales"
    formula_str <- "ef_dfi_g ~ I(daily_weight_g / 1000) + I(adg_const_g / 1000)"
    if (has_loc)   formula_str <- paste(formula_str, "+ location")
    if (has_breed) formula_str <- paste(formula_str, "+ breed")
    if (length(active_terms) > 0L) {
      formula_str <- paste(formula_str, "+", paste(active_terms, collapse = " + "))
    }
    formula_str <- paste0(formula_str, " + (1 | animal_id)")

    if (sum(train_idx) >= min_train && n_animal_train >= 10L) {
      lmm_fit <- tryCatch({
        lme4::lmer(as.formula(formula_str), data = dt[train_idx],
                   control = lme4::lmerControl(optimizer = "bobyqa"))
      }, error = function(e) {
        warning("LMM Feed Correction: model fitting failed; daily_feed_g left unchanged. Details: ",
                e$message, call. = FALSE)
        NULL
      })
    } else {
      message(sprintf(paste0(
        "LMM Feed Correction: insufficient training samples (%d rows < %d required, ",
        "or %d animals < 10 needed for the random intercept); daily_feed_g left unchanged."),
        sum(train_idx), min_train, n_animal_train))
      lmm_fit <- NULL
    }

    if (!is.null(lmm_fit)) {
      fixed_eff <- lme4::fixef(lmm_fit)

      # 文献的应用口径：Correction = Σ(α·ETP + γ·OTD + δ·FID)，**字面 +β̂x**。
      # 不做单侧截断、不加物理速率封顶、不按系数符号跳过——文献 Table 1 的系数本
      # 就有正有负（FIV-high +61.40、OTV-high +1750.0），跳过正系数项等于把模型
      # 换成另一个东西（V1.1.4 的 β>0 守卫与 pmax(0,·) 随本次重写退役）。
      dt[, lmm_correction_g := 0]
      for (tm in active_terms) {
        if (tm %in% names(fixed_eff)) {
          dt[, lmm_correction_g := lmm_correction_g + fixed_eff[[tm]] * get(tm)]
        }
      }

      # 台账列 + 应用：DFI_corr = DFI_ef + Correction
      dt[, lmm_ef_g := ef_dfi_g]
      dt[, daily_feed_g := ef_dfi_g + lmm_correction_g]

      # 设备故障天不复活：上游「整天置 NA 交插补」的语义要保住
      if ("has_feed_out_of_range_today" %in% names(dt)) {
        dt[has_feed_out_of_range_today %in% TRUE, daily_feed_g := NA_real_]
      }

      n_corrected <- sum(abs(dt$lmm_correction_g) > 0.001, na.rm = TRUE)
      mean_abs_corr <- if (n_corrected > 0) {
        mean(abs(dt$lmm_correction_g[abs(dt$lmm_correction_g) > 0.001]), na.rm = TRUE)
      } else 0
      singular_fit <- isTRUE(lme4::isSingular(lmm_fit, tol = 1e-4))
      message(sprintf(paste0(
        "LMM Feed Correction: corrected %d daily records, mean |correction| = %.1f g ",
        "(train %d rows / %d animals, %d terms, %d dropped%s)."),
        n_corrected, mean_abs_corr, sum(train_idx), n_animal_train,
        length(active_terms), length(dropped_terms),
        if (singular_fit) ", SINGULAR FIT" else ""))
      if (length(dropped_terms) > 0L) {
        message("LMM Feed Correction: zero-variance terms dropped: ",
                paste(dropped_terms, collapse = ", "))
      }
      if (singular_fit) {
        warning(paste0(
          "LMM Feed Correction: singular fit (random-intercept variance at the boundary); ",
          "fixed-effect SEs may be anticonservative. Model specification left unchanged to ",
          "stay faithful to the paper."), call. = FALSE)
      }
    } else {
      # 铁律：拟合失败或样本不足时**什么都不改** daily_feed_g（保留上游的记录级
      # 结果），绝不回退到 error-free 日和——它系统性丢掉被 flag 记录的采食量，
      # 正是本方法要消除的偏差。
      dt[, lmm_ef_g := NA_real_]
      dt[, lmm_correction_g := NA_real_]
    }
  } else {
    warning("Package 'lme4' is not installed. Ignoring LMM feed correction.")
    dt[, lmm_ef_g := NA_real_]
    dt[, lmm_correction_g := NA_real_]
  }

  # ==== 4. 出口生理校验（对所有路径统一执行） ====
  dt <- .finalize_daily_feed(dt, .feed_daily_max_g(ns_cfg))

  # Clean temporary feature columns used in the process
  #（台账列 lmm_ef_g / lmm_correction_g 保留在输出中，满足
  #  daily_feed_g == lmm_ef_g + lmm_correction_g 的逐行对账）
  cols_to_remove <- c("ef_dfi_g", "ef_n_visit", "adg_const_g", "visits_total",
                      paste0("n_", err_flags), paste0("dur_", err_flags),
                      paste0("feed_", err_flags),
                      paste0("etp_", etp_flags), paste0("otd_", otd_flags),
                      paste0("fid_", fid_flags),
                      "daily_feed_g_corrected")
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
