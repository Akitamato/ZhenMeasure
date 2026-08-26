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
