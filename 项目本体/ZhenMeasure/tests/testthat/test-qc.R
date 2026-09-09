# Unit tests for QC functions

test_that("ZhenM_qc_weight_standard runs national_standard method", {
  skip_if_not_installed("data.table")

  # Create standard_data format test data
  dt <- data.table::data.table(
    animal_id = rep("A001", 30),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10), each = 3),
    weight_g = rep(seq(30000, 50000, length.out = 10), each = 3) + rnorm(30, 0, 500),
    device_type = "YANGXIANG"
  )

  result_national <- ZhenM_qc_weight_standard(dt, "national_standard")

  expect_true("is_outlier_wt" %in% names(result_national))
  expect_true("flag_weight_low" %in% names(result_national))
})

test_that("ZhenM_qc_feed_standard identifies anomalies", {
  skip_if_not_installed("data.table")

  # Create standard_data format test data
  dt <- data.table::data.table(
    animal_id = rep("A001", 30),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10), each = 3),
    feed_g = c(rep(c(300, 400, 350), 9), -50, 200, 8000),  # Contains negative and extreme high values
    duration_sec = rep(c(300, 400, 350), 10),
    weight_g = rep(seq(30000, 50000, length.out = 10), each = 3),
    device_type = "YANGXIANG"
  )

  result <- ZhenM_qc_feed_standard(dt, "national_standard")

  expect_true("flag_feed_negative" %in% names(result))
  expect_true("is_outlier_feed" %in% names(result))
  expect_true(any(result$flag_feed_negative))
})

test_that("ZhenM_standard_to_daily_filtered corrects outliers", {
  skip_if_not_installed("data.table")

  # Create standard_data with QC flags
  dt <- data.table::data.table(
    animal_id = rep("A001", 10),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 5), each = 2),
    feed_g = c(300, 400, 350, 9000, 320, 380, 310, 390, 330, 370),  # 4th is outlier
    weight_g = c(30000, 30100, 32000, 32100, 34000, 34100, 36000, 36100, 38000, 38100),
    duration_sec = rep(300, 10),
    is_outlier_feed = c(rep(FALSE, 3), TRUE, rep(FALSE, 6)),
    flag_feed_too_high = c(rep(FALSE, 3), TRUE, rep(FALSE, 6)),
    is_outlier_wt = rep(FALSE, 10),
    device_type = "YANGXIANG",
    age_day = rep(100:104, each = 2),
    measurement_day = rep(1:5, each = 2),
    source_file = "test.csv",
    daily_feed_g = NA_real_
  )

  result <- ZhenM_standard_to_daily_filtered(dt)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)  # 5 days of data
  expect_true("n_outlier_feed" %in% names(result))

  # 被 flag 的 9000g 记录应被「纠正」（封顶到个体 P99），而非整体排除(=350)或原样保留(=9350)
  day2_feed <- result[record_date == as.Date("2024-01-02"), daily_feed_g]
  expect_gt(day2_feed, 350)
  expect_lt(day2_feed, 350 + 9000)
})

test_that("ZhenM_standard_to_daily_filtered respects correction switches", {
  skip_if_not_installed("data.table")

  # 与上一用例相同的 fixture：day2 = 350 + 被 flag 的 9000
  dt <- data.table::data.table(
    animal_id = rep("A001", 10),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 5), each = 2),
    feed_g = c(300, 400, 350, 9000, 320, 380, 310, 390, 330, 370),
    weight_g = c(30000, 30100, 32000, 32100, 34000, 34100, 36000, 36100, 38000, 38100),
    duration_sec = rep(300, 10),
    is_outlier_feed = c(rep(FALSE, 3), TRUE, rep(FALSE, 6)),
    flag_feed_too_high = c(rep(FALSE, 3), TRUE, rep(FALSE, 6)),
    is_outlier_wt = rep(FALSE, 10),
    device_type = "YANGXIANG",
    age_day = rep(100:104, each = 2),
    measurement_day = rep(1:5, each = 2),
    source_file = "test.csv",
    daily_feed_g = NA_real_
  )

  # 关闭记录级纠正 + LMM 兜底（消融实验 C1 变体的路径）
  cfg_off <- list(national_standard = list(
    use_record_feed_correction = FALSE,
    use_lmm_feed_correction = FALSE
  ))
  msgs <- capture_messages(
    result_off <- ZhenM_standard_to_daily_filtered(dt, cfg_off)
  )

  # 记录级纠正关闭后被 flag 记录置零，不再有 P99 封顶：
  # day2 精确等于干净记录之和（350），而非「纠正」后的 >350
  expect_equal(result_off[record_date == as.Date("2024-01-02"), daily_feed_g], 350)

  # LMM 兜底关闭后仍执行 6kg 日上限校验分支
  expect_true("flag_daily_feed_over_limit" %in% names(result_off))

  # 门控接通的运行期证据：不应出现任何 LMM 校正消息
  expect_false(any(grepl("LMM", msgs)))
})

test_that("Improved LMM fallback: duration-proportional compensation + ledger + exit gate", {
  skip_if_not_installed("lme4")

  set.seed(20260826)
  ids <- c("A001", "A002")
  n_days <- 30
  rec_list <- list()
  for (id in seq_along(ids)) {
    w0 <- 30000 + 1500 * (id - 1)
    for (d in 1:n_days) {
      n_vis <- sample(2:4, 1)
      dt_i <- data.table::data.table(
        animal_id = ids[id],
        record_date = as.Date("2024-01-01") + d - 1,
        feed_g = pmax(rnorm(n_vis, 320, 25), 150),
        weight_g = w0 + d * 200 + rnorm(n_vis, 0, 100),
        duration_sec = rep(300, n_vis),
        is_outlier_feed = FALSE,
        flag_feed_too_high = FALSE,
        flag_speed_too_fast = FALSE
      )
      # 注入虚高记录：feed 5000g 但时长仅 100s → speed_too_fast
      # （真实采食量约 320g；置零路径会整条丢弃，LMM 应按时长补偿回近似真实值）
      if (id == 1 && d %in% c(5, 10, 15, 20, 25)) {
        dt_i$is_outlier_feed[1] <- TRUE
        dt_i$flag_speed_too_fast[1] <- TRUE
        dt_i$feed_g[1] <- 5000
        dt_i$duration_sec[1] <- 100
      }
      if (id == 2 && d %in% c(8, 16, 24)) {
        dt_i$is_outlier_feed[1] <- TRUE
        dt_i$flag_speed_too_fast[1] <- TRUE
        dt_i$feed_g[1] <- 4500
        dt_i$duration_sec[1] <- 90
      }
      rec_list[[length(rec_list) + 1]] <- dt_i
    }
  }
  dt <- data.table::rbindlist(rec_list)
  # 构造一个干净但超 6kg 的天（A002 第 28 天）：验证出口生理校验仍生效，
  # 且该天不再被从训练集中截断丢弃
  dt <- data.table::rbindlist(list(dt, data.table::data.table(
    animal_id = "A002",
    record_date = as.Date("2024-01-01") + 27,
    feed_g = c(3300, 3300),
    weight_g = c(31500 + 28 * 200, 31500 + 28 * 200),
    duration_sec = c(300, 300),
    is_outlier_feed = FALSE,
    flag_feed_too_high = FALSE,
    flag_speed_too_fast = FALSE
  )))

  # 记录级纠正关闭（走 V1.1.0 置零路径），LMM 兜底默认开启
  cfg <- list(national_standard = list(use_record_feed_correction = FALSE))
  msgs <- capture_messages(
    res <- suppressWarnings(ZhenM_standard_to_daily_filtered(data.table::copy(dt), cfg))
  )

  # LMM 兜底确实触发，且台账列保留在输出中
  expect_true(any(grepl("LMM Feed Correction: corrected", msgs)))
  expect_true("lmm_correction_g" %in% names(res))

  # 补偿与被丢采食时长成比例：被 flag 天的日值应高于「仅干净记录之和」，
  # 且校正量为正（把被排除记录的真实采食量加回来）
  inj_dates <- as.Date("2024-01-01") + c(4, 9, 14, 19, 24)   # A001 的注入日
  clean_sum_a1 <- dt[animal_id == "A001" & is_outlier_feed == FALSE,
                     .(clean_sum = sum(feed_g)), by = record_date]
  m <- merge(
    res[animal_id == "A001" & record_date %in% inj_dates,
        .(record_date, daily_feed_g, lmm_correction_g)],
    clean_sum_a1, by = "record_date"
  )
  expect_true(all(m$daily_feed_g > m$clean_sum))
  expect_true(all(m$lmm_correction_g > 0))

  # 出口生理校验：>6kg 天打标并置 NA（训练集不截断 ≠ 出口放行）
  d28 <- as.Date("2024-01-01") + 27
  expect_true(res[animal_id == "A002" & record_date == d28, flag_daily_feed_over_limit])
  expect_true(is.na(res[animal_id == "A002" & record_date == d28, daily_feed_g]))
})

test_that("日级采食量出口校验三条路径统一（issue #21）", {
  skip_if_not_installed("data.table")

  # 1) 辅助函数本身：>6000 打标并置 NA，≤0 置 NA，正常值与 NA 不动
  dt <- data.table::data.table(
    animal_id = rep("A001", 5),
    record_date = as.Date("2024-01-01") + 0:4,
    daily_feed_g = c(5000, 7000, 0, -5, NA_real_)
  )
  out <- ZhenMeasure:::.finalize_daily_feed(dt)
  expect_identical(out$flag_daily_feed_over_limit, c(FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(out$daily_feed_g, c(5000, NA, NA, NA, NA))

  # 2) 无 feed 列路径：跳过记录级纠正与 LMM，但出口校验仍执行
  #    （修复前该路径直接 return dt，输出缺 flag_daily_feed_over_limit 列）
  daily <- data.table::data.table(
    animal_id = "A001", record_date = as.Date("2024-01-01"),
    daily_feed_g = NA_real_
  )
  raw <- data.table::data.table(
    animal_id = "A001", record_date = as.Date("2024-01-01"),
    weight_g = 30000
  )
  res <- ZhenMeasure:::.apply_feed_lmm_correction(daily, raw)
  expect_true("flag_daily_feed_over_limit" %in% names(res))
  expect_false(any(res$flag_daily_feed_over_limit))
})

test_that("ZhenM_generate_qc_summary produces summary", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    flag_out_of_range = c(TRUE, FALSE, TRUE, FALSE),
    flag_weight_low = c(FALSE, TRUE, FALSE, FALSE)
  )

  summary <- ZhenM_generate_qc_summary(dt)

  expect_true(is.data.frame(summary))
  expect_true("error_type" %in% names(summary))
  expect_true("count" %in% names(summary))
  expect_equal(nrow(summary), 2)
})

test_that("Stack mode: complementary LMM adds back only noise-zeroed losses", {
  skip_if_not_installed("lme4")

  set.seed(20260826)
  ids <- c("A001", "A002")
  n_days <- 30
  rec_list <- list()
  for (id in seq_along(ids)) {
    w0 <- 30000 + 1500 * (id - 1)
    for (d in 1:n_days) {
      n_vis <- sample(2:4, 1)
      dt_i <- data.table::data.table(
        animal_id = ids[id],
        record_date = as.Date("2024-01-01") + d - 1,
        feed_g = pmax(rnorm(n_vis, 320, 25), 150),
        weight_g = w0 + d * 200 + rnorm(n_vis, 0, 100),
        duration_sec = rep(300, n_vis),
        is_outlier_feed = FALSE,
        flag_speed_too_fast = FALSE,
        flag_speed_zero_long_duration = FALSE
      )
      # 注入「长时间零速」型噪声记录：物理规则会把它置 0，真实约 400g 完全丢失
      # （A 路径损失）；stack 模式应按时长把这部分近似补回来
      if (id == 1 && d %in% c(5, 10, 15, 20, 25)) {
        dt_i$is_outlier_feed[1] <- TRUE
        dt_i$flag_speed_zero_long_duration[1] <- TRUE
        dt_i$feed_g[1] <- 400
        dt_i$duration_sec[1] <- 600
      }
      if (id == 2 && d %in% c(8, 16, 24)) {
        dt_i$is_outlier_feed[1] <- TRUE
        dt_i$flag_speed_zero_long_duration[1] <- TRUE
        dt_i$feed_g[1] <- 350
        dt_i$duration_sec[1] <- 550
      }
      rec_list[[length(rec_list) + 1]] <- dt_i
    }
  }
  dt <- data.table::rbindlist(rec_list)

  cfg_a <- list(national_standard = list())                       # A：现状默认
  cfg_f <- list(national_standard = list(use_lmm_stacking = TRUE)) # F：叠加

  msgs_a <- capture_messages(
    res_a <- suppressWarnings(ZhenM_standard_to_daily_filtered(data.table::copy(dt), cfg_a))
  )
  msgs_f <- capture_messages(
    res_f <- suppressWarnings(ZhenM_standard_to_daily_filtered(data.table::copy(dt), cfg_f))
  )

  # A 路径不触发 LMM；F 路径触发 stack 模式并保留台账列
  expect_false(any(grepl("LMM", msgs_a)))
  expect_true(any(grepl("LMM Feed Correction \\(stack\\)", msgs_f)))
  expect_false("lmm_correction_g" %in% names(res_a))
  expect_true("lmm_correction_g" %in% names(res_f))

  # 噪声置零天：叠加后日值高于纯记录级纠正（把置零丢失的克数补回来）
  inj_dates <- as.Date("2024-01-01") + c(4, 9, 14, 19, 24)
  f_day <- res_f[animal_id == "A001" & record_date %in% inj_dates, daily_feed_g]
  a_day <- res_a[animal_id == "A001" & record_date %in% inj_dates, daily_feed_g]
  expect_true(all(f_day > a_day))
  expect_true(all(res_f[animal_id == "A001" & record_date %in% inj_dates,
                        lmm_correction_g] > 0))

  # J3 不造假：干净天上叠加模式与纯 A 逐值相同（correction 恒为 0）
  clean_dates <- setdiff(unique(res_f$record_date), inj_dates)
  expect_equal(
    res_f[animal_id == "A001" & record_date %in% clean_dates, daily_feed_g],
    res_a[animal_id == "A001" & record_date %in% clean_dates, daily_feed_g]
  )
})

test_that("record-level speed cap threads speed_max from config (issue #12)", {
  skip_if_not_installed("data.table")

  # 同一份数据：60 秒内 5000g 的 speed_too_fast 记录，
  # 封顶值 = speed_max × 60 / 60 = speed_max（g）
  mk <- function() {
    data.table::data.table(
      animal_id = rep("A001", 10),
      record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 5), each = 2),
      feed_g = c(rep(300, 8), 5000, 400),
      weight_g = 30000 + seq(0, 90, length.out = 10) * 100,
      duration_sec = c(rep(300, 8), 60, 300),
      is_outlier_feed = c(rep(FALSE, 8), TRUE, FALSE),
      flag_speed_too_fast = c(rep(FALSE, 8), TRUE, FALSE),
      is_outlier_wt = FALSE,
      device_type = "YANGXIANG",
      age_day = rep(1:5, each = 2),
      measurement_day = rep(1:5, each = 2),
      source_file = "t.csv",
      daily_feed_g = NA_real_
    )
  }

  # 默认 config：speed_max=170 → 该记录封顶 170g；当天干净记录 400g → 日和 570g
  r_def <- ZhenM_standard_to_daily_filtered(mk())
  d5_def <- r_def[record_date == as.Date("2024-01-05"), daily_feed_g]

  # 自定义 speed_max=300 → 封顶 300g → 日和 700g
  r_cfg <- ZhenM_standard_to_daily_filtered(
    mk(), ZhenM_merge_config(list(national_standard = list(speed_max = 300))))
  d5_cfg <- r_cfg[record_date == as.Date("2024-01-05"), daily_feed_g]

  expect_equal(d5_def, 170 + 400)
  expect_equal(d5_cfg, 300 + 400)
})

test_that("LMM compensation sign guard skips positive beta (issue #13)", {
  skip_if_not_installed("lme4")

  set.seed(20260902)
  rec_list <- list()
  for (id in 1:3) {
    idc <- sprintf("A%03d", id)
    for (d in 1:30) {
      flagged <- (d %% 5) == 0
      # 反向关系：被 flag 天的干净记录采食远高于干净天（visits_n 同为 3，
      # 体重平滑无差）→ dur 特征的 β 被估成正号
      rec_list[[length(rec_list) + 1]] <- data.table::data.table(
        animal_id = idc,
        record_date = as.Date("2024-01-01") + d - 1,
        feed_g = if (flagged) c(1000, 1000, 5000) else c(300, 300, 300),
        weight_g = 30000 + id * 1000 + d * 150,
        duration_sec = if (flagged) c(300, 300, 600) else c(300, 300, 300),
        is_outlier_feed = if (flagged) c(FALSE, FALSE, TRUE) else c(FALSE, FALSE, FALSE),
        flag_speed_too_fast = if (flagged) c(FALSE, FALSE, TRUE) else c(FALSE, FALSE, FALSE)
      )
    }
  }
  dt <- data.table::rbindlist(rec_list)

  # 关记录级纠正 → 置零 + LMM 兜底路径
  cfg <- list(national_standard = list(use_record_feed_correction = FALSE))
  warn_hit <- FALSE
  res <- withCallingHandlers(
    ZhenM_standard_to_daily_filtered(data.table::copy(dt), cfg),
    warning = function(w) {
      if (grepl("unexpected sign", conditionMessage(w))) {
        warn_hit <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )

  # 符号守卫触发：β>0 的特征被跳过
  expect_true(warn_hit)

  # 补偿不再向下：被 flag 天的日值 = 干净记录和（1000+1000），而非被 β>0 减小
  flagged_dates <- as.Date("2024-01-01") + seq(4, 29, 5)
  expect_true(all(res[record_date %in% flagged_dates, daily_feed_g] == 2000))
  # 全程无静默 NA/负值（下限护栏 + 符号守卫共同保证）
  expect_true(all(is.finite(res$daily_feed_g)))
})

test_that("day-level consensus rule flags systematically-off days (issue #14)", {
  skip_if_not_installed("data.table")

  set.seed(20260903)
  days <- 15
  base_w <- seq(50000, 105000, length.out = days)
  rec_list <- list()
  for (d in 1:days) {
    rec_list[[length(rec_list) + 1]] <- data.table::data.table(
      animal_id = "A001",
      record_date = as.Date("2024-01-01") + d - 1,
      weight_g = round(base_w[d] + c(150, -120)),   # 每天 2 条记录
      device_type = "YANGXIANG"
    )
  }
  dt <- data.table::rbindlist(rec_list)
  # 第 8 天：两条记录整体 +5kg（天级系统偏移，Huber 权重双双 < 0.9）
  dt[record_date == as.Date("2024-01-08"), weight_g := weight_g + 5000]

  cfg <- ZhenM_merge_config(list(national_standard = list(test_weight_range = c(60, 100))))
  r <- ZhenM_qc_weight_standard(dt, "national_standard", cfg)

  # 共识天被日级 flag 标中，且仅此一天
  expect_true(all(r[record_date == as.Date("2024-01-08"), flag_daily_weight_low]))
  expect_equal(data.table::uniqueN(r[flag_daily_weight_low %in% TRUE, record_date]), 1)
})

test_that("day-level consensus exempts single-record days (issue #14)", {
  skip_if_not_installed("data.table")

  set.seed(20260904)
  days <- 15
  base_w <- seq(50000, 105000, length.out = days)
  rec_list <- list()
  for (d in 1:days) {
    n_rec <- if (d == 8) 1 else 2   # 第 8 天仅 1 条记录
    rec_list[[length(rec_list) + 1]] <- data.table::data.table(
      animal_id = "A001",
      record_date = as.Date("2024-01-01") + d - 1,
      weight_g = round(base_w[d] + if (n_rec == 2) c(150, -120) else 0),
      device_type = "YANGXIANG"
    )
  }
  dt <- data.table::rbindlist(rec_list)
  # 第 8 天唯一记录 +6kg（w≈0.41 < 0.9，若无准入规则本会被标中）
  dt[record_date == as.Date("2024-01-08"), weight_g := weight_g + 6000]

  cfg <- ZhenM_merge_config(list(national_standard = list(test_weight_range = c(60, 100))))
  r <- ZhenM_qc_weight_standard(dt, "national_standard", cfg)

  # 准入条件：单记录天不由日级共识规则管辖
  expect_false(any(r[record_date == as.Date("2024-01-08"), flag_daily_weight_low]))
})

test_that("daily_weight_threshold is honored from config (issue #14)", {
  skip_if_not_installed("data.table")

  set.seed(20260903)
  days <- 15
  base_w <- seq(50000, 105000, length.out = days)
  rec_list <- list()
  for (d in 1:days) {
    rec_list[[length(rec_list) + 1]] <- data.table::data.table(
      animal_id = "A001",
      record_date = as.Date("2024-01-01") + d - 1,
      weight_g = round(base_w[d] + c(150, -120)),
      device_type = "YANGXIANG"
    )
  }
  dt <- data.table::rbindlist(rec_list)
  dt[record_date == as.Date("2024-01-08"), weight_g := weight_g + 5000]

  r_hi <- ZhenM_qc_weight_standard(dt, "national_standard",
    ZhenM_merge_config(list(national_standard = list(test_weight_range = c(60, 100),
                                                    daily_weight_threshold = 0.9))))
  r_lo <- ZhenM_qc_weight_standard(dt, "national_standard",
    ZhenM_merge_config(list(national_standard = list(test_weight_range = c(60, 100),
                                                    daily_weight_threshold = 0.1))))

  # 第 8 天偏移足够大：0.9 下标中；阈值收紧到 0.1 后不再由日级规则标出
  expect_true(all(r_hi[record_date == as.Date("2024-01-08"), flag_daily_weight_low]))
  expect_false(any(r_lo[record_date == as.Date("2024-01-08"), flag_daily_weight_low]))
})

test_that("speed_zero_long_duration threshold is configurable (issue #16)", {
  skip_if_not_installed("data.table")

  mk <- function() {
    data.table::data.table(
      animal_id = "A001",
      record_date = as.Date("2024-01-01"),
      feed_g = c(0, 100),        # QC 内部重算 feed_speed = feed_g/时长 → 第 1 条 speed=0
      duration_sec = c(600, 600),
      weight_g = c(60000, 60000)
    )
  }
  base <- ZhenM_merge_config(list(national_standard = list()))

  # 默认阈值 500s：speed=0 且 600s 记录被标中
  r1 <- ZhenM_qc_feed_standard(mk(), "national_standard", base)
  expect_true(r1$flag_speed_zero_long_duration[1])
  expect_false(r1$flag_speed_zero_long_duration[2])

  # 阈值放宽到 700s：同一条记录不再被标中
  r2 <- ZhenM_qc_feed_standard(mk(), "national_standard",
    ZhenM_merge_config(list(national_standard = list(speed_zero_long_duration_sec = 700))))
  expect_false(r2$flag_speed_zero_long_duration[1])
})

test_that("NA 体重记录的 QC flag 为 FALSE 而非 NA（issue #19）", {
  skip_if_not_installed("data.table")

  n <- 80
  dt <- data.table::data.table(
    animal_id = "A001",
    record_date = as.Date("2024-01-01") + 0:(n - 1),
    weight_g = seq(44000, 115000, length.out = n),
    feed_g = 2000, duration_sec = 600,
    start_time = as.POSIXct("2024-01-01 08:00:00", tz = "UTC")
  )
  dt$weight_g[c(3, 17)] <- NA_real_

  cfg <- ZhenM_default_config("national_standard")
  cfg$national_standard$test_weight_range <- c(1e9, -1e9)  # 关闭整头删除以便观察 flag
  res <- suppressWarnings(ZhenM_qc_weight_standard(dt, "national_standard", cfg))

  na_rows <- res[is.na(weight_g)]
  expect_equal(nrow(na_rows), 2L)
  # 修复前：rlm_weights_1 为 NA → flag 列 NA → is_outlier_wt 传播 NA
  expect_false(anyNA(na_rows$flag_weight_low))
  expect_false(anyNA(na_rows$flag_daily_weight_low))
  expect_false(anyNA(res$is_outlier_wt))
  expect_false(any(na_rows$is_outlier_wt))
})

test_that("空表 QC 汇总的 percentage 为 0 而非 NaN（issue #19）", {
  skip_if_not_installed("data.table")

  empty <- data.table::data.table(animal_id = character(0), flag_weight_low = logical(0))
  summary <- ZhenM_generate_qc_summary(empty)

  expect_true(nrow(summary) >= 1)
  expect_false(any(is.nan(summary$percentage)))
  expect_true(all(summary$percentage == 0))
})

test_that("当日实测体重全部被标异常时 daily_weight_g 仍为 NA（issue #19）", {
  skip_if_not_installed("data.table")

  # day1：一条实测体重被标异常（out of range）+ 一条无体重记录（flag 为 FALSE 但无实测值）
  #       → 当日无可用的实测体重，daily_weight_g 应为 NA
  # day2：两条正常体重 → 有值
  dt <- data.table::data.table(
    animal_id = rep("A001", 4),
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 2), each = 2),
    feed_g = rep(1000, 4),
    weight_g = c(23000, NA, 60000, 61000),
    weighted_avg_weight_per_day = c(23000, 23000, 60500, 60500),
    duration_sec = rep(300, 4),
    is_outlier_feed = rep(FALSE, 4),
    is_outlier_wt = c(TRUE, FALSE, FALSE, FALSE),
    device_type = "YANGXIANG",
    age_day = rep(100:101, each = 2),
    measurement_day = rep(1:2, each = 2),
    source_file = "test.csv",
    daily_feed_g = NA_real_
  )

  result <- ZhenM_standard_to_daily_filtered(dt)

  expect_true(is.na(result[record_date == as.Date("2024-01-01"), daily_weight_g]))
  expect_equal(result[record_date == as.Date("2024-01-02"), daily_weight_g], 60500)
})

test_that("生长曲线批量判定：点数不足/拟合差/合格三类（issue #22）", {
  skip_if_not_installed("data.table")
  set.seed(20260909)

  # A: 合格（30 天，二次曲线 + 小噪声）
  d <- 0:29
  a <- data.table::data.table(
    animal_id = "A",
    record_date = as.Date("2024-01-01") + d,
    daily_weight_g = 30000 + 700 * d + 8 * d^2 + stats::rnorm(30, 0, 50)
  )
  # B: 点数不足（只有 9 个有效体重点）
  b <- data.table::data.table(
    animal_id = "B",
    record_date = as.Date("2024-01-01") + 0:8,
    daily_weight_g = 30000 + 700 * (0:8)
  )
  # C: 点数够但拟合差（阶跃，二次拟合 R² 低于阈值）
  c_dt <- data.table::data.table(
    animal_id = "C",
    record_date = as.Date("2024-01-01") + 0:29,
    daily_weight_g = c(rep(30000, 15), rep(50000, 15))
  )
  daily <- data.table::rbindlist(list(a, b, c_dt))

  res <- ZhenMeasure:::.check_growth_curve_batch(daily, min_r2 = 0.99)

  # 删除集合与顺序与 unique(animal_id) 一致（旧实现按此顺序 c() 追加）
  expect_identical(res$animals_to_delete, c("B", "C"))
  expect_equal(res$n_insufficient, 1L)
  expect_equal(res$n_low_r2, 1L)

  # 空输入返回空结果
  res0 <- ZhenMeasure:::.check_growth_curve_batch(daily[0], min_r2 = 0.99)
  expect_length(res0$animals_to_delete, 0)
  expect_equal(res0$n_insufficient, 0L)
  expect_equal(res0$n_low_r2, 0L)
})

test_that("连续性检查只保留最长合格段（issue #22 非等值 join 回填）", {
  skip_if_not_installed("data.table")

  # 段 1：第 1-5 天（5 个有效天）；空档 10 天；段 2：第 16-25 天（10 个有效天）
  seg1 <- data.table::data.table(
    animal_id = "A",
    record_date = as.Date("2024-01-01") + 0:4,
    weight_g = seq(30000, 32000, length.out = 5)
  )
  seg2 <- data.table::data.table(
    animal_id = "A",
    record_date = as.Date("2024-01-01") + 15:24,
    weight_g = seq(40000, 45000, length.out = 10)
  )
  dt <- data.table::rbindlist(list(seg1, seg2))

  res <- ZhenM_qc_overall(
    dt,
    config = list(national_standard = list(min_test_days = 3, max_missing_rate = 0.6)),
    inactive_days_threshold = 5,
    min_segment_days = 3
  )

  kept <- res$records
  expect_equal(nrow(kept), 10)
  expect_equal(min(kept$record_date), as.Date("2024-01-16"))
  expect_equal(max(kept$record_date), as.Date("2024-01-25"))
  expect_equal(res$summary[step == "continuity_removed_records", n_removed], 5)
})

test_that("test_weight_range 是「覆盖全量程」口径：不满足者整头删除（issue #25）", {
  skip_if_not_installed("data.table")

  mk <- function(id, start_g, end_g, n = 30) {
    data.table::data.table(
      animal_id = id,
      device_type = "YANGXIANG",
      record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = n), each = 2),
      weight_g = rep(seq(start_g, end_g, length.out = n), each = 2)
    )
  }
  dt <- data.table::rbindlist(list(
    mk("A_full_range", 40000, 115000),  # 首日 <=45kg 且 末日 >=110kg → 保留
    mk("B_start_high", 50000, 115000),  # 入栏 50kg > 45kg → 整头删除
    mk("C_end_low", 40000, 105000)      # 出栏 105kg < 110kg → 整头删除
  ))

  res <- suppressWarnings(ZhenM_qc_weight_standard(dt, "national_standard"))

  expect_setequal(unique(res$animal_id), "A_full_range")
})

test_that("汇总行走 logger 落盘，无 logger 时仍走 message（issue #23）", {
  skip_if_not_installed("data.table")

  helpers <- ZhenMeasure:::.create_logger_helpers
  seen <- character(0)
  logger <- list(
    info = function(msg) seen <<- c(seen, paste0("info:", msg)),
    detail = function(msg) seen <<- c(seen, paste0("detail:", msg)),
    subsection = function(msg) NULL
  )

  # 有 logger：汇总行进 detail（控制台 + 日志文件），不再单独 message
  expect_silent(helpers(logger)$log_summary("hello"))
  expect_equal(seen, "detail:hello")

  # 无 logger：保持原有 message 输出，控制台可见性不变
  expect_message(helpers(NULL)$log_summary("world"), "world")
})

test_that("三个 QC 阶段的汇总行都能被 logger 捕获（issue #23）", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 12),
    device_type = "YANGXIANG",
    record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 12), each = 1),
    feed_g = rep(c(300, 400, 350, 380), 3),
    duration_sec = rep(c(300, 400, 350, 380), 3),
    weight_g = rep(seq(30000, 46000, length.out = 12)),
    age_day = seq(70, 81)
  )

  lines <- character(0)
  logger <- list(
    info = function(msg) lines <<- c(lines, msg),
    detail = function(msg) lines <<- c(lines, msg),
    subsection = function(msg) NULL
  )

  invisible(ZhenM_qc_feed_standard(dt, "national_standard", logger = logger))
  # 权重序列为等距线性，二次项退化会让 RLM 报收敛告警；本用例只验证汇总行落盘
  invisible(suppressWarnings(ZhenM_qc_weight_standard(dt, "national_standard", logger = logger)))
  # 阈值放宽，避免个体因完整度不足被剔除（剔除后走提前返回分支，不发汇总行）
  invisible(ZhenM_qc_overall(
    dt,
    config = list(national_standard = list(min_test_days = 3, max_missing_rate = 0.9)),
    min_segment_days = 3,
    logger = logger
  ))

  expect_true(any(grepl("^Feed QC \\(National-Standard\\):", lines)))
  expect_true(any(grepl("^Weight QC \\(National-Standard\\):", lines)))
  expect_true(any(grepl("^Overall QC:", lines)))
})

test_that("LMM 校正不按引用改写调用方传入的 raw_dt（issue #30）", {
  skip_if_not_installed("data.table")

  raw <- data.table::data.table(
    animal_id = "A001",
    record_date = rep(as.Date("2024-01-01"), 2),
    feed_g = c(100, 200),
    duration_sec = c(60, 90),
    is_outlier_feed = FALSE,
    flag_feed_negative = FALSE,
    flag_speed_too_fast = FALSE
  )
  daily <- data.table::data.table(
    animal_id = "A001",
    record_date = as.Date("2024-01-01"),
    daily_feed_g = 300,
    daily_weight_g = 50000
  )
  before <- names(raw)

  invisible(ZhenMeasure:::.apply_feed_lmm_correction(daily, raw, ns_cfg = NULL))

  expect_false("is_feed_normal_record" %in% names(raw))
  expect_identical(names(raw), before)
})
