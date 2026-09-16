# Unit tests for QC functions

# 出厂默认 = 记录级物理纠正（A）：config 默认 use_lmm_feed_correction = FALSE，
# 文献 LMM 是可选增强（注入基准上 L 三设备三档全面低于 A，见 NEWS.md 1.2.0 段）。
# 凡要验证 LMM 实现的用例必须**显式**打开它——不能再依赖「不给 config」隐含等于
# 跑 LMM，那正是默认翻转前的旧语义。只给这一个键即可：use_record_feed_correction
# 缺键时兜底为 TRUE，与旧默认「两个开关都 TRUE」逐值等价。
lmm_cfg <- function() list(national_standard = list(use_lmm_feed_correction = TRUE))

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

  # 裁定 0 的核心契约：记录级物理纠正**成功**时 LMM 也必须运行。
  # 重写前日级 LMM 只在「记录级纠正关闭或失败」时兜底，而记录级成功恰是默认
  # 情况——不解耦的话文献 LMM 在任何配置下都永不执行。此处显式打开 LMM 跑同一份
  # 数据，必须看到 LMM 路径的消息（本 fixture 只有 1 头动物，会被双门槛挡下并
  # 明确报「样本不足」，但这已证明门控被接通）。
  msgs_def <- capture_messages(
    result_def <- suppressWarnings(ZhenM_standard_to_daily_filtered(
      data.table::copy(dt), lmm_cfg()))
  )
  expect_true(any(grepl("LMM Feed Correction", msgs_def)))

  # 反之，关掉 LMM 时记录级纠正照跑（两个开关相互独立，不是串联依赖）
  expect_gt(result_def[record_date == as.Date("2024-01-02"), daily_feed_g], 350)
})

test_that("出厂默认 = 记录级物理纠正 A（use_lmm_feed_correction 默认 FALSE）", {
  skip_if_not_installed("lme4")

  # 12 头 × 20 天，规模足以让文献 LMM **真的拟合成功**——因此若哪天默认被误翻回
  # TRUE，本用例会立刻看到 LMM 消息、日值被覆写、且台账列出现，三重可辨。
  # 这是 2026-09-16「A 转正」裁定的行为锁（test-config.R 只锁了配置字面值）。
  rec_list <- list()
  for (i in 1:12) {
    for (d in 1:20) {
      hi <- (d %% 5) == 0   # 每 5 天一次「高速大采食」损坏，A 会把它封顶回 170×时长
      rec_list[[length(rec_list) + 1]] <- data.table::data.table(
        animal_id = sprintf("A%03d", i),
        record_date = as.Date("2024-01-01") + d - 1,
        feed_g = c(rep(300, 3), if (hi) 5000 else numeric(0)),
        weight_g = 30000 + i * 500 + d * (180 + 10 * i),
        duration_sec = c(rep(300, 3), if (hi) 150 else numeric(0)),
        is_outlier_feed = c(rep(FALSE, 3), if (hi) TRUE else logical(0)),
        flag_speed_too_fast = c(rep(FALSE, 3), if (hi) TRUE else logical(0))
      )
    }
  }
  dt <- data.table::rbindlist(rec_list)

  # 默认：走 A，日级 LMM 不运行
  msgs_def <- capture_messages(
    r_def <- suppressWarnings(ZhenM_standard_to_daily_filtered(data.table::copy(dt)))
  )
  expect_false(any(grepl("LMM", msgs_def)))
  # LMM 没跑 → 两个台账列**根本不该被创建**（与「跑了但失败」的 NA 是两回事）
  expect_false(any(c("lmm_ef_g", "lmm_correction_g") %in% names(r_def)))

  # 缺键兜底也必须是 A：手工拼的 config（不走 ZhenM_merge_config）不能静默改用 LMM
  r_bare <- suppressWarnings(ZhenM_standard_to_daily_filtered(
    data.table::copy(dt), list(national_standard = list(speed_max = 170))))
  expect_identical(r_bare$daily_feed_g, r_def$daily_feed_g)

  # 显式打开 LMM：必须真的跑，且日值确实与 A 不同——否则本用例没有区分力
  msgs_on <- capture_messages(
    r_on <- suppressWarnings(ZhenM_standard_to_daily_filtered(
      data.table::copy(dt), lmm_cfg()))
  )
  expect_true(any(grepl("LMM Feed Correction", msgs_on)))
  expect_true(all(c("lmm_ef_g", "lmm_correction_g") %in% names(r_on)))
  expect_false(isTRUE(all.equal(r_def$daily_feed_g, r_on$daily_feed_g)))
})

test_that("LMM 文献实现：校正按 +β̂x 应用、台账恒等式成立、出口生理校验独立", {
  skip_if_not_installed("lme4")

  # issue #5 重写后的集成契约。fixture 设计（20 头 × 30 天 = 600 训练行，
  # 过 min_train = max(30, 10×入模项数) 与 ≥10 头的双门槛）：
  #   - 每 5 天一个「高频长时访问 + 读数损坏」的天：3 条干净记录 + 1 条
  #     5000g/短时长记录（flag_speed_too_fast）。同一天的真实采食量随该次
  #     停留时长上升（300 + dur/2 每餐）——这正是文献要校正的混淆结构。
  #   - 干净天 3 条记录、无任何 flag → 全部协变量恰为 0。
  set.seed(20260916)
  ids <- sprintf("A%03d", 1:20)
  n_days <- 30L
  rec_list <- list()
  for (i in seq_along(ids)) {
    w0 <- 30000 + i * 500
    slope <- 180 + 10 * i          # 个体间 ADG 有差异，避免 ADG 项零变异
    for (d in 1:n_days) {
      high <- (d %% 5) == 0
      dur  <- if (high) sample(c(100, 150, 200), 1) else NA_real_
      cf   <- pmax(rnorm(3, if (high) 300 + dur / 2 else 300, 20), 150)
      dt_i <- data.table::data.table(
        animal_id    = ids[i],
        record_date  = as.Date("2024-01-01") + d - 1,
        feed_g       = c(cf, if (high) 5000 else numeric(0)),
        weight_g     = w0 + d * slope + rnorm(3 + high, 0, 100),
        duration_sec = c(rep(300, 3), if (high) dur else numeric(0)),
        is_outlier_feed  = c(rep(FALSE, 3), if (high) TRUE else logical(0)),
        flag_speed_too_fast = c(rep(FALSE, 3), if (high) TRUE else logical(0)),
        # 这两个 flag 恒 FALSE：训练集内零变异，应被剔除并在消息里点名
        flag_feed_negative  = FALSE,
        flag_feed_too_high  = FALSE
      )
      rec_list[[length(rec_list) + 1]] <- dt_i
    }
  }
  dt <- data.table::rbindlist(rec_list)
  # 干净但超 6kg 的一天：出口生理校验仍生效，且该天**不**因响应大而被剔出训练集
  # （截尾的对象是逐类型协变量，不是响应——见 .apply_feed_lmm_correction 文档）
  over_date <- as.Date("2024-01-01") + n_days
  dt <- data.table::rbindlist(list(dt, data.table::data.table(
    animal_id = "A001", record_date = over_date,
    feed_g = c(3300, 3300),
    weight_g = 30000 + 500 + (n_days + 1) * 190,
    duration_sec = c(300, 300),
    is_outlier_feed = FALSE, flag_speed_too_fast = FALSE,
    flag_feed_negative = FALSE, flag_feed_too_high = FALSE
  )))

  msgs <- capture_messages(
    res <- suppressWarnings(ZhenM_standard_to_daily_filtered(
      data.table::copy(dt), lmm_cfg()))
  )

  # 显式打开 LMM（use_lmm_feed_correction = TRUE）后它必须真的跑起来——这是
  # 裁定 0 的核心契约：记录级纠正成功不再是 LMM 的门控条件
  expect_true(any(grepl("LMM Feed Correction: corrected", msgs)))
  expect_false(any(grepl("insufficient training samples|model fitting failed", msgs)))
  # 零变异项被剔除时必须点名，不允许静默改模型规格
  expect_true(any(grepl("zero-variance terms dropped", msgs)))

  # 台账列 + 恒等式：daily_feed_g == lmm_ef_g + lmm_correction_g（出口置 NA 的行除外）
  expect_true(all(c("lmm_ef_g", "lmm_correction_g") %in% names(res)))
  fin <- !is.na(res$daily_feed_g)
  expect_gt(sum(fin), 0)
  expect_equal(res$daily_feed_g[fin],
               (res$lmm_ef_g + res$lmm_correction_g)[fin])

  # 零校正不变量：无 flag 的天所有 ETP/OTD/FID 恰为 0 → Correction ≡ 0。
  # 这是相对旧「叠加」路径（干净数据上也会日均改写数百克）最大的增益。
  high_dates  <- as.Date("2024-01-01") + seq(4, by = 5, length.out = 6)
  clean_dates <- setdiff(unique(res$record_date), c(high_dates, over_date))
  expect_gt(length(clean_dates), 0)
  expect_true(all(res$lmm_correction_g[res$record_date %in% clean_dates] == 0))

  # 被 flag 天：校正量为正（该天真实采食随停留时长上升，模型应把它加回来）
  expect_true(all(res[record_date %in% high_dates, lmm_correction_g] > 0))
  # 且确实改变了日值（不再是「仅干净记录之和 + 0」）
  expect_true(all(res[record_date %in% high_dates, daily_feed_g] >
                    res[record_date %in% high_dates, lmm_ef_g]))

  # 出口生理校验：>6kg 天打标并置 NA（训练集不截断 ≠ 出口放行）
  expect_true(res[animal_id == "A001" & record_date == over_date,
                  flag_daily_feed_over_limit])
  expect_true(is.na(res[animal_id == "A001" & record_date == over_date,
                        daily_feed_g]))
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

test_that("校正按字面 +β̂x 应用：正系数不被跳过（issue #13 守卫已退役）", {
  skip_if_not_installed("lme4")

  # 反向关系 fixture：被 flag 天的**干净**记录采食远高于干净天（2000 vs 900），
  # 于是时长/占比类协变量的 β 必然被估成正号。V1.1.4 的符号守卫会把这类项整个跳过
  # （并报 "unexpected sign" 警告），使补偿无法向上；文献 Table 1 的系数本就
  # 有正有负（FIV-high +61.40、OTV-high +1750.0）且照用，故守卫已随 issue #5 重写
  # 退役——本用例是它的反向回归锁。
  set.seed(20260902)
  rec_list <- list()
  for (id in 1:12) {
    idc <- sprintf("A%03d", id)
    for (d in 1:30) {
      flagged <- (d %% 5) == 0
      rec_list[[length(rec_list) + 1]] <- data.table::data.table(
        animal_id = idc,
        record_date = as.Date("2024-01-01") + d - 1,
        feed_g = if (flagged) c(1000, 1000, 5000) else c(300, 300, 300),
        weight_g = 30000 + id * 1000 + d * (150 + 5 * id),
        duration_sec = if (flagged) c(300, 300, 600) else c(300, 300, 300),
        is_outlier_feed = if (flagged) c(FALSE, FALSE, TRUE) else c(FALSE, FALSE, FALSE),
        flag_speed_too_fast = if (flagged) c(FALSE, FALSE, TRUE) else c(FALSE, FALSE, FALSE)
      )
    }
  }
  dt <- data.table::rbindlist(rec_list)

  sign_warn <- FALSE
  msgs <- character()
  res <- withCallingHandlers(
    {
      msgs <- capture_messages(
        out <- suppressWarnings(ZhenM_standard_to_daily_filtered(
          data.table::copy(dt), lmm_cfg()))
      )
      out
    },
    warning = function(w) {
      if (grepl("unexpected sign", conditionMessage(w))) {
        sign_warn <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )

  # 反向锁 1：符号守卫不再存在（不存在该警告，也不再据它跳过任何项）
  expect_false(sign_warn)
  # 反向锁 2：文献 LMM 跑起来了，不是被门槛挡下后留下的空台账
  expect_true(any(grepl("LMM Feed Correction: corrected", msgs)))

  # 反向锁 3：β>0 的项照常入模——被 flag 天的校正量为正（守卫生效时会被跳过，
  # 日值将停留在「干净记录之和」2000）
  flagged_dates <- as.Date("2024-01-01") + seq(4, 29, 5)
  m <- res[record_date %in% flagged_dates, .(daily_feed_g, lmm_ef_g, lmm_correction_g)]
  expect_true(all(m$lmm_correction_g > 0))
  expect_equal(m$lmm_ef_g, rep(2000, nrow(m)))
  expect_true(all(m$daily_feed_g > m$lmm_ef_g))

  # 字面应用的结果仍须是有限值：出口校验（≤0 置 NA）是另一层，不在这里越权
  expect_true(all(is.finite(res$daily_feed_g)))
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

test_that("Overall QC 早退路径与正常路径的 summary 列名一致（issue #33）", {
  skip_if_not_installed("data.table")

  cfg <- list(national_standard = list(min_test_days = 3, max_missing_rate = 0.9))

  # 正常路径：数据保留
  dt_ok <- data.table::data.table(
    animal_id = rep("A001", 80),
    device_type = "YANGXIANG",
    record_date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 80),
    feed_g = 2500,
    duration_sec = 600,
    weight_g = 40000 + 300 * seq_len(80),
    age_day = seq(70, 149)
  )
  r_ok <- suppressWarnings(suppressMessages(
    ZhenM_qc_overall(dt_ok, config = cfg, min_segment_days = 3)
  ))

  # 早退路径：record_date 全缺失 → 记录被清空后提前 return
  dt_empty <- data.table::data.table(
    animal_id = c("A001", "A002"),
    device_type = "YANGXIANG",
    record_date = as.Date(rep(NA_character_, 2)),
    feed_g = 2500,
    duration_sec = 600,
    weight_g = 50000
  )
  r_empty <- suppressWarnings(suppressMessages(
    ZhenM_qc_overall(dt_empty, config = cfg, min_segment_days = 3)
  ))

  expect_equal(nrow(r_empty$records), 0L)  # 确已走早退分支
  expect_identical(names(r_empty$summary), names(r_ok$summary))
  expect_true("associated_animal_ids" %in% names(r_empty$summary))
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

test_that("feed_intake_range 归一化后产出 flag_feed_out_of_range（issue #40）", {
  skip_if_not_installed("data.table")

  # config 里 feed_intake_range 是 kg（默认 c(0, 6)），feed_g 是克。V0.2.6 的 C-1
  # 正是漏了 kg→g 归一化，让 c(0,6) 直接与 feed_g 比较、把全部记录判为异常——
  # 本用例的第一条断言就是该回归的守卫。
  dt <- data.table::data.table(
    animal_id = rep("A001", 6),
    record_date = rep(as.Date("2024-01-01") + 0:2, each = 2),
    feed_g = c(300, 400, 350, 320, 6001, 7000),
    duration_sec = rep(300, 6),
    weight_g = seq(30000, 40000, length.out = 6),
    device_type = "YANGXIANG"
  )

  r <- ZhenM_qc_feed_standard(dt, "national_standard")

  expect_true("flag_feed_out_of_range" %in% names(r))
  # 500 g 级的正常记录绝不能被判出界（C-1 回归守卫）
  expect_false(any(r$flag_feed_out_of_range[r$feed_g < 1000]))
  # 默认上界 6 kg = 6000 g，闭区间：6001 出界、6000 不算
  expect_equal(which(r$flag_feed_out_of_range), c(5L, 6L))
})

test_that("flag_feed_out_of_range 走 config（issue #40）", {
  skip_if_not_installed("data.table")

  mk <- function() {
    data.table::data.table(
      animal_id = rep("A001", 3),
      record_date = as.Date("2024-01-01") + 0:2,
      feed_g = c(300, 1200, 3000),
      duration_sec = rep(300, 3),
      weight_g = c(30000, 31000, 32000),
      device_type = "YANGXIANG"
    )
  }

  # 收紧到 2 kg（= 2000 g）：1200 仍在界内，只有 3000 出界
  r <- ZhenM_qc_feed_standard(mk(), "national_standard",
                              list(national_standard = list(feed_intake_range = c(0, 2))))
  expect_equal(which(r$flag_feed_out_of_range), 3L)

  # 收紧到 1 kg（= 1000 g）：1200 与 3000 都出界
  r1 <- ZhenM_qc_feed_standard(mk(), "national_standard",
                               list(national_standard = list(feed_intake_range = c(0, 1))))
  expect_equal(which(r1$flag_feed_out_of_range), c(2L, 3L))

  # 配置给空区间时不报错、也不误标（归一化助手退化为 (-Inf, Inf)）
  r2 <- ZhenM_qc_feed_standard(mk(), "national_standard",
                               list(national_standard = list(feed_intake_range = numeric(0))))
  expect_false(any(r2$flag_feed_out_of_range))
})

test_that("日级出口上限接 config 的 feed_intake_range（issue #40）", {
  skip_if_not_installed("data.table")

  expect_equal(.feed_daily_max_g(NULL), 6000)
  expect_equal(.feed_daily_max_g(list(feed_intake_range = c(0, 6))), 6000)
  expect_equal(.feed_daily_max_g(list(feed_intake_range = c(0, 4))), 4000)
  expect_equal(.feed_daily_max_g(list(feed_intake_range = NULL)), 6000)
  # 已是克口径的大数值不会被二次放大（归一化助手自身有 <=500 判定）
  expect_equal(.feed_daily_max_g(list(feed_intake_range = c(0, 5500))), 5500)

  d <- data.table::data.table(animal_id = "A", record_date = as.Date("2024-01-01"),
                              daily_feed_g = 4500)
  expect_false(.finalize_daily_feed(data.table::copy(d), 6000)$flag_daily_feed_over_limit)
  r <- .finalize_daily_feed(data.table::copy(d), 4000)
  expect_true(r$flag_daily_feed_over_limit)
  expect_true(is.na(r$daily_feed_g))
})

test_that("任一条记录出界 → 整天 daily_feed_g 置 NA（issue #40 文档承诺的分支）", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = rep("A001", 6),
    record_date = rep(as.Date("2024-01-01") + 0:2, each = 2),
    feed_g = c(300, 400, 350, 320, 600, 7000),   # 第 3 天的 7000 g 出界
    duration_sec = rep(300, 6),
    weight_g = c(30000, 30100, 32000, 32100, 34000, 34100),
    device_type = "YANGXIANG"
  )
  r <- ZhenM_qc_feed_standard(dt, "national_standard")
  expect_equal(which(r$flag_feed_out_of_range), 6L)

  # 关掉两条校正路径，隔离出「整日置 NA」这一个机制：
  # 该天合计 6700 g 其实也会撞上日上限（6000 g）被置 NA，本用例要证明的是
  # 出界记录单独就足以触发，而不是靠上限兜底。
  d <- data.table::as.data.table(ZhenM_standard_to_daily_filtered(r, config = list(
    national_standard = list(use_record_feed_correction = FALSE,
                             use_lmm_feed_correction = FALSE))))

  d3 <- d[d$record_date == as.Date("2024-01-03")]
  expect_true(isTRUE(d3$has_feed_out_of_range_today))
  expect_true(is.na(d3$daily_feed_g))
  # 相邻的正常天不受牵连
  d1 <- d[d$record_date == as.Date("2024-01-01")]
  expect_false(isTRUE(d1$has_feed_out_of_range_today))
  expect_false(is.na(d1$daily_feed_g))
})

test_that(".build_row_index / .row_index_of 与 which(animal_id == id) 等价（issue #36）", {
  skip_if_not_installed("data.table")

  # 故意乱序、含重复 id，且首行不是字典序最小的 id
  dt <- data.table::data.table(
    animal_id = c("B", "A", "B", "C", "A", "B"),
    record_date = as.Date("2024-01-01") + 0:5,
    v = 1:6
  )
  before <- data.table::copy(dt)
  rows <- .build_row_index(dt)

  # 与原实现（每次循环内全表扫 which）逐个体等价——含每组内升序
  for (id in unique(dt$animal_id)) {
    expect_identical(.row_index_of(rows, id), which(dt$animal_id == id))
  }

  # 查表建好后不改动调用方的表（nrow 与行序均不变）
  expect_identical(dt, before)

  # 边界：不存在的 id 与 NA 返回 integer(0)（与 which() 的 NA 口径一致）；
  # 非标量 id 不是本助手的契约，返回 integer(0) 而不是静默取并集
  expect_identical(.row_index_of(rows, "ZZZ"), integer(0))
  expect_identical(.row_index_of(rows, NA_character_), integer(0))
  expect_identical(.row_index_of(rows, c("A", "B")), integer(0))

  # 元素取回的是原表位置，可直接用于 data.table 的 i
  expect_identical(dt[.row_index_of(rows, "A"), v], c(2L, 5L))
})
test_that(".correct_feed_records 是纯函数：不复制整表、不改写调用方的表（issue #38）", {
  skip_if_not_installed("data.table")

  # 两头个体，各自的干净采食量不同（300 vs 600），用来锁死「逐个体 P99」的口径：
  # 若封顶阈值串到别的个体，B 的高值会被压到 A 的 P99。
  dt <- data.table::data.table(
    animal_id = c(rep("A", 100), rep("B", 100), "A", "B"),
    record_date = as.Date("2024-01-01"),
    feed_g = c(rep(300, 100), rep(600, 100), 9000, 9000),
    duration_sec = 300,
    is_outlier_feed = c(rep(FALSE, 200), TRUE, TRUE),
    flag_feed_too_high = c(rep(FALSE, 200), TRUE, TRUE)
  )
  before <- data.table::copy(dt)
  names_before <- names(dt)

  cf <- .correct_feed_records(dt, speed_max = 170)

  # 纯函数契约：调用方的表逐字节不变，且不残留 feed_corrected / feed_p99 / .cap
  expect_identical(dt, before)
  expect_identical(names(dt), names_before)

  expect_true(cf$success)
  expect_length(cf$feed_corrected, nrow(dt))
  expect_equal(cf$feed_corrected[1:100], rep(300, 100))       # 干净记录不动
  expect_equal(cf$feed_corrected[101:200], rep(600, 100))
  expect_equal(cf$feed_corrected[201], 300)                   # A 的 9000 → A 的 P99
  expect_equal(cf$feed_corrected[202], 600)                   # B 的 9000 → B 的 P99
})

test_that(".correct_feed_records 五类 flag 的物理规则（issue #38 重构后口径不变）", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    animal_id = "A",
    record_date = as.Date("2024-01-01"),
    feed_g = c(300, -50, 20, 400, 5000, 9000),
    duration_sec = c(300, 300, 1000, 300, 100, 300),
    is_outlier_feed = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    flag_feed_negative = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    flag_speed_extreme_low_feed = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    flag_speed_zero_long_duration = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    flag_speed_too_fast = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
    flag_feed_too_high = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  cf <- .correct_feed_records(dt, speed_max = 170)
  v <- cf$feed_corrected

  expect_equal(v[2], 0)          # 负值 → 0
  expect_equal(v[3], 0)          # 极高速小采食 → 0
  expect_equal(v[4], 0)          # 长时间零速 → 0
  expect_equal(v[5], 170 * 100 / 60)  # 速度过快 → speed_max × 时长/60
  expect_equal(v[6], 300)        # 单次采食过高 → 干净记录 P99（唯一干净值 300）
})

# ---------------------------------------------------------------------------
# issue #5：日级 LMM 按 Jiao et al. (2014) 重写后的契约
# ---------------------------------------------------------------------------

test_that("LMM 协变量指派表与文献预先指定的区间一致（issue #5）", {
  spec <- ZhenMeasure:::.lmm_covariate_spec()

  # 文献每类错误最多三个协变量（正文 "31 variables created from the 16 error
  # counts" = ETP 16 + OTD 11 + FID 4）。我们的 QC 只采集 9 类（缺类型 7、
  # 11–14），故实际 10 + 6 + 2 = 18 项（ETP 多出的 1 项是 STL 扩展）。
  expect_equal(spec[kind == "etp", .N], 10L)
  expect_equal(spec[kind == "otd", .N], 6L)
  expect_equal(spec[kind == "fid", .N], 2L)
  expect_equal(nrow(spec), 18L)
  expect_equal(anyDuplicated(spec$term), 0L)
  expect_true(all(spec$term == paste0(spec$kind, "_", spec$flag)))

  # 文献的指派区间：ETP 给全部 16 类；OTD 给 1,2 与 6–14；FID 给 4,5 与 15,16。
  # 下表是「文献区间 ∩ 我们采集到的类型」——7、11–14 我们没采集，故 OTD 只剩 6 项；
  # 15、16 没采集，故 FID 只剩 2 项。
  expect_equal(sort(unique(spec[kind == "otd", err_type])), c(1L, 2L, 6L, 8L, 9L, 10L))
  expect_equal(sort(unique(spec[kind == "fid", err_type])), c(4L, 5L))
  expect_equal(spec[kind == "etp" & !is.na(err_type), .N], 9L)
  # 文献区间本身（16+11+4=31）不能被"顺手"改窄：OTD 必是 1,2 与 6–14 的子集，
  # FID 必是 4,5 与 15,16 的子集
  expect_true(all(spec[kind == "otd", err_type] %in% c(1:2, 6:14)))
  expect_true(all(spec[kind == "fid", err_type] %in% c(4:5, 15:16)))

  # 逐条锁住「哪种错误挂哪些协变量」——这是本次重写最容易改错的一处
  expect_equal(sort(spec[err_type == 1L, kind]), c("etp", "otd"))
  expect_equal(sort(spec[err_type == 2L, kind]), c("etp", "otd"))
  expect_equal(sort(spec[err_type == 3L, kind]), "etp")
  expect_equal(sort(spec[err_type == 4L, kind]), c("etp", "fid"))
  expect_equal(sort(spec[err_type == 5L, kind]), c("etp", "fid"))
  expect_equal(sort(spec[err_type == 8L, kind]), c("etp", "otd"))

  # flag_STL_FI 是超出文献的扩展项（我们的 STL 检测没有文献对应），仅 ETP
  stl <- spec[flag == "flag_STL_FI"]
  expect_equal(nrow(stl), 1L)
  expect_equal(stl$kind, "etp")
  expect_true(is.na(stl$err_type))

  # 未采集的类型不得出现在表里
  expect_false(any(spec$err_type %in% c(7L, 11L, 12L, 13L, 14L), na.rm = TRUE))
})

test_that("日级协变量口径：ETP 是占比、OTD/FID 是逐类型累计量（issue #5）", {
  raw <- data.table::data.table(
    animal_id = rep("A001", 3),
    record_date = rep(as.Date("2024-01-01"), 3),
    feed_g = c(300, 400, 500),
    duration_sec = c(100, 200, 150),
    is_outlier_feed = c(TRUE, TRUE, FALSE),
    flag_feed_negative   = c(TRUE, FALSE, FALSE),  # 类型 1 → ETP + OTD
    flag_feed_too_high   = c(FALSE, TRUE, FALSE),  # 类型 2 → ETP + OTD（无 FID）
    flag_duration_negative = c(FALSE, FALSE, FALSE) # 类型 4 → ETP + FID
  )

  feats <- ZhenMeasure:::.lmm_daily_covariates(raw)

  expect_equal(nrow(feats), 1L)
  # 响应 = 干净记录之和（被 flag 的两条不计入），干净访问数 = 1
  expect_equal(feats$ef_dfi_g, 500)
  expect_equal(feats$ef_n_visit, 1)

  # ETP = 命中次数 / 当日**全部**访问数（3），不是有效访问数
  expect_equal(feats$etp_flag_feed_negative, 1 / 3)
  expect_equal(feats$etp_flag_feed_too_high, 1 / 3)
  # OTD = 该类型命中的记录时长之和（被 flag 记录的时长，不是全部时长）
  expect_equal(feats$otd_flag_feed_negative, 100)
  expect_equal(feats$otd_flag_feed_too_high, 200)
  # FID = 该类型命中的记录**原始**采食量之和；类型 2 没有 FID 项，类型 4 有
  expect_false("fid_flag_feed_too_high" %in% names(feats))
  expect_true("fid_flag_duration_negative" %in% names(feats))
  expect_equal(feats$fid_flag_duration_negative, 0)
  # 类型 1 只有 ETP + OTD，没有 FID
  expect_false("fid_flag_feed_negative" %in% names(feats))

  # 类型 4 命中时 FID 取原始值（不做任何物理纠正——raw 里那条 4000 原样进 FID）
  raw2 <- data.table::copy(raw)
  raw2[, flag_duration_negative := c(FALSE, FALSE, TRUE)]
  raw2[3, feed_g := 4000]
  feats2 <- ZhenMeasure:::.lmm_daily_covariates(raw2)
  expect_equal(feats2$fid_flag_duration_negative, 4000)
  # 该天三条记录各带一个 flag → 误差自由访问数为 0，响应为 0。
  # 注意第 3 条的 is_outlier_feed 仍是 FALSE：**任一 error flag 命中即排除**，
  # 响应不依赖 is_outlier_feed 这一列（两者是不同来源的标记）
  expect_equal(feats2$ef_dfi_g, 0)
  expect_equal(feats2$ef_n_visit, 0)
  expect_equal(feats2$etp_flag_duration_negative, 1 / 3)
})

test_that("零校正不变量：无 flag 的天全部协变量为 0（issue #5）", {
  raw <- data.table::data.table(
    animal_id = rep(c("A001", "A002"), each = 3),
    record_date = rep(as.Date("2024-01-01") + 0:1, each = 3),
    feed_g = 300,
    duration_sec = 300,
    is_outlier_feed = FALSE
  )
  feats <- ZhenMeasure:::.lmm_daily_covariates(raw)

  term_cols <- setdiff(names(feats), c("animal_id", "record_date", "ef_dfi_g", "ef_n_visit"))
  expect_gt(length(term_cols), 0)
  expect_true(all(as.matrix(feats[, ..term_cols]) == 0))
  expect_true(all(feats$ef_dfi_g == 900))
})

test_that("ADG 是每头常数，取日体重对日序的最小二乘斜率（issue #5）", {
  dt <- data.table::data.table(
    animal_id = c(rep("A", 5), rep("B", 5), "C", "D", "D", "D"),
    record_date = as.Date("2024-01-01") + c(0:4, 0:4, 0, 0:2),
    daily_weight_g = c(30000 + 0:4 * 250, 40000 + 0:4 * 400, 35000,
                       c(30000, NA, 30600))
  )
  adg <- ZhenMeasure:::.lmm_adg_per_animal(dt)

  expect_equal(adg[1:5], rep(250, 5))      # 完美线性 → 斜率本身
  expect_equal(adg[6:10], rep(400, 5))
  expect_true(is.na(adg[11]))              # 仅 1 个体重天 → 无定义
  expect_equal(adg[12:14], rep(300, 3))    # 缺一天的体重不影响该头的斜率

  # 每头常数：同一头的每一行取值相同（含体重缺失的行，按动物回填）
  expect_equal(length(unique(adg[1:5])), 1L)
  expect_equal(length(unique(adg[12:14])), 1L)

  # 空表 / 全 NA 不报错
  expect_equal(ZhenMeasure:::.lmm_adg_per_animal(
    data.table::data.table(animal_id = character(0), record_date = as.Date(character(0)),
                           daily_weight_g = numeric(0))), numeric(0))
  expect_true(is.na(ZhenMeasure:::.lmm_adg_per_animal(
    data.table::data.table(animal_id = "A", record_date = as.Date("2024-01-01"),
                           daily_weight_g = NA_real_))))
})

test_that("协变量截尾：只剔训练行、只剔逐类型协变量、不剔响应（issue #5）", {
  skip_if_not_installed("lme4")

  # 12 头 × 20 天干净数据（每天 3 条 300g 记录）
  rec_list <- list()
  for (i in 1:12) {
    idc <- sprintf("A%03d", i)
    for (d in 1:20) {
      rec_list[[length(rec_list) + 1]] <- data.table::data.table(
        animal_id = idc,
        record_date = as.Date("2024-01-01") + d - 1,
        feed_g = 300,
        weight_g = 30000 + i * 500 + d * (180 + 10 * i),
        duration_sec = 300,
        is_outlier_feed = FALSE,
        flag_duration_negative = FALSE
      )
    }
  }
  # A004–A012 第 10 天挂一条「时长负」记录（类型 4 → 有 FID），fid=500 未越界
  for (i in 4:12) {
    rec_list[[length(rec_list) + 1]] <- data.table::data.table(
      animal_id = sprintf("A%03d", i),
      record_date = as.Date("2024-01-01") + 9,
      feed_g = 500, weight_g = 30000 + i * 500 + 10 * (180 + 10 * i),
      duration_sec = 60, is_outlier_feed = TRUE, flag_duration_negative = TRUE
    )
  }
  dt <- data.table::rbindlist(rec_list)

  # ① 响应远超 3500 g 的干净天：各类型累计量都是 0，**必须留在训练集里**
  #    （文献截的是 DFIe/OTDe 这两个协变量，不是响应；响应上限归出口管）
  over <- data.table::data.table(
    animal_id = "A001", record_date = as.Date("2024-01-01") + 20,
    feed_g = 8000, weight_g = 30000 + 500 + 21 * 190,
    duration_sec = 300, is_outlier_feed = FALSE, flag_duration_negative = FALSE
  )
  # ② 类型 4 的当日累计采食量 4000 g > lmm_trim_dfie_g 上界 3500 → 该行被剔
  over_fid <- data.table::data.table(
    animal_id = "A002", record_date = as.Date("2024-01-01") + 20,
    feed_g = c(300, 300, 300, 4000),
    weight_g = 30000 + 1000 + 21 * 200,
    duration_sec = 300, is_outlier_feed = c(FALSE, FALSE, FALSE, TRUE),
    flag_duration_negative = c(FALSE, FALSE, FALSE, TRUE)
  )
  dt <- data.table::rbindlist(list(dt, over, over_fid))
  # 日级行数：12 头 × 20 天；类型 4 那条记录落在已有的一天上，不新增天；
  # 两个 over_* 各新增 1 天（第 21 天）
  n_daily <- 12L * 20L + 2L

  msgs <- capture_messages(
    res <- suppressWarnings(ZhenM_standard_to_daily_filtered(
      data.table::copy(dt), lmm_cfg()))
  )
  expect_true(any(grepl("LMM Feed Correction: corrected", msgs)))

  # 训练行数 = 全部日行 − 1（只有 fid 越界那一行被剔）。若截尾错误地作用在响应
  # 上，8000 g 那天也会被剔 → 少一行，本断言正是该回归的守卫。
  expect_true(any(grepl(sprintf("train %d rows", n_daily - 1L), msgs)))
  # 截尾只影响训练集：越界天在输出里照常有值（不被"因越界而跳过校正"）
  fid_day <- res[animal_id == "A002" & record_date == as.Date("2024-01-01") + 20]
  expect_false(is.na(fid_day$daily_feed_g))
})

test_that("校正失败时 daily_feed_g 与关掉 LMM 逐值相同（issue #5 铁律）", {
  skip_if_not_installed("lme4")

  # 单头 5 天：必然过不了「≥10 头」的门槛 → 拟合不发生
  mk <- function() {
    data.table::data.table(
      animal_id = rep("A001", 10),
      record_date = rep(seq.Date(as.Date("2024-01-01"), by = "day", length.out = 5), each = 2),
      feed_g = c(300, 400, 350, 9000, 320, 380, 310, 390, 330, 370),
      weight_g = seq(30000, 40000, length.out = 10),
      duration_sec = rep(300, 10),
      is_outlier_feed = c(rep(FALSE, 3), TRUE, rep(FALSE, 6)),
      flag_feed_too_high = c(rep(FALSE, 3), TRUE, rep(FALSE, 6)),
      is_outlier_wt = FALSE, device_type = "YANGXIANG",
      age_day = rep(100:104, each = 2), measurement_day = rep(1:5, each = 2),
      source_file = "t.csv", daily_feed_g = NA_real_
    )
  }

  msgs <- capture_messages(
    r_lmm <- suppressWarnings(ZhenM_standard_to_daily_filtered(mk(), lmm_cfg()))
  )
  expect_true(any(grepl("insufficient training samples", msgs)))
  # 台账列明确记为 NA（不是 0）——「没跑」与「跑了但校正量为 0」不能混淆
  expect_true(all(is.na(r_lmm$lmm_correction_g)))
  expect_true(all(is.na(r_lmm$lmm_ef_g)))

  r_off <- suppressWarnings(ZhenM_standard_to_daily_filtered(
    mk(), list(national_standard = list(use_lmm_feed_correction = FALSE))))

  # 铁律：样本不足时**什么都不改** daily_feed_g，必须与「根本没跑 LMM」逐值相同。
  # 若哪天有人把失败兜底改成 error-free 日和，这里会立刻变红。
  expect_equal(r_lmm$daily_feed_g, r_off$daily_feed_g)
  expect_gt(r_lmm[record_date == as.Date("2024-01-02"), daily_feed_g], 350)
})

test_that("设备故障天（flag_feed_out_of_range）不被 LMM 复活（issue #5）", {
  skip_if_not_installed("lme4")

  rec_list <- list()
  for (i in 1:12) {
    for (d in 1:20) {
      rec_list[[length(rec_list) + 1]] <- data.table::data.table(
        animal_id = sprintf("A%03d", i),
        record_date = as.Date("2024-01-01") + d - 1,
        feed_g = 300, weight_g = 30000 + i * 500 + d * (180 + 10 * i),
        duration_sec = 300, is_outlier_feed = FALSE,
        flag_feed_out_of_range = FALSE
      )
    }
  }
  # A001 第 5 天整天设备故障：记录出界 → 上游整天置 NA，LMM 不得把它算回来
  fault_date <- as.Date("2024-01-01") + 4
  rec_list[[length(rec_list) + 1]] <- data.table::data.table(
    animal_id = "A001", record_date = fault_date, feed_g = 9000,
    weight_g = 30000 + 500 + 5 * 190, duration_sec = 300,
    is_outlier_feed = TRUE, flag_feed_out_of_range = TRUE
  )
  dt <- data.table::rbindlist(rec_list)

  msgs <- capture_messages(
    res <- suppressWarnings(ZhenM_standard_to_daily_filtered(
      data.table::copy(dt), lmm_cfg()))
  )
  expect_true(any(grepl("LMM Feed Correction: corrected", msgs)))

  day <- res[animal_id == "A001" & record_date == fault_date]
  expect_true(day$has_feed_out_of_range_today)
  expect_true(is.na(day$daily_feed_g))
})

test_that("奇异拟合被报出且不报错（issue #5：只报不治）", {
  skip_if_not_installed("lme4")

  # 所有个体的**逐日采食序列完全相同**（同一个 20 天的确定性波动），只是体重轨迹
  # 不同 → 各头均值恒等 → 随机截距方差落在 0 边界上（σ²_p 无识别），而残差方差
  # 仍 > 0（固定效应解释不掉那圈日内波动）。这正是文献「ADG 猪内恒定 + 猪随机
  # 截距」设定容易退化的失效模式（计划 R5）。
  set.seed(7)
  day_bump <- 30 * sin(1:20)
  rec_list <- list()
  for (i in 1:12) {
    for (d in 1:20) {
      hi <- (d %% 5) == 0
      cf <- rep(300 + day_bump[d] + if (hi) 200 else 0, 3)
      rec_list[[length(rec_list) + 1]] <- data.table::data.table(
        animal_id = sprintf("A%03d", i),
        record_date = as.Date("2024-01-01") + d - 1,
        feed_g = c(cf, if (hi) 5000 else numeric(0)),
        weight_g = 30000 + i * 500 + d * (180 + 10 * i),
        duration_sec = c(rep(300, 3), if (hi) 150 else numeric(0)),
        is_outlier_feed = c(rep(FALSE, 3), if (hi) TRUE else logical(0)),
        flag_speed_too_fast = c(rep(FALSE, 3), if (hi) TRUE else logical(0))
      )
    }
  }
  dt <- data.table::rbindlist(rec_list)

  msgs <- capture_messages(
    res <- suppressWarnings(ZhenM_standard_to_daily_filtered(
      data.table::copy(dt), lmm_cfg()))
  )
  expect_true(any(grepl("SINGULAR FIT", msgs)))
  # 只报不治：不做「奇异时自动降级」的隐形处理，模型规格不变，
  # 结果照常产出有限值
  expect_true(any(grepl("2 terms", msgs)))
  expect_equal(nrow(res), 12L * 20L)
  expect_true(all(is.finite(res$daily_feed_g)))
})

test_that("出口 ≤0 的天打 flag_daily_feed_nonpositive 并置 NA（issue #5）", {
  dt <- data.table::data.table(
    animal_id = "A001", record_date = as.Date("2024-01-01") + 0:2,
    daily_feed_g = c(-5, 0, 3000)
  )
  out <- ZhenMeasure:::.finalize_daily_feed(dt)

  expect_identical(out$flag_daily_feed_nonpositive, c(TRUE, TRUE, FALSE))
  expect_identical(out$flag_daily_feed_over_limit, c(FALSE, FALSE, FALSE))
  expect_equal(out$daily_feed_g, c(NA, NA, 3000))
})
