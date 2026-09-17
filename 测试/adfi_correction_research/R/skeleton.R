######### ADFI 校正研究模块：共享骨架 #########
#
# 来源：从 测试/simulation_benchmark.R 原样搬出（问题 A 的基准脚本）。
# 目的：A 轨（visit 级污染）与 B 轨（整天缺失）共用一套真值构造与评价口径，
#       见 开发方案/Debug/采食量校正模块修改/ ADFI 校正算法开发与比较计划.md §2.5。
#
# 搬迁纪律：inject_errors() 与 eval_metrics() 的**函数体逐字节保持原样**
#   （simulation_benchmark.R 的重放门依赖它们行为不变）。
#   constants 也沿用原名（SET_SEED / TYPE_PROBS / INJECTION_RATES / base_ns），
#   使函数体里的自由变量查找结果与搬迁前一致。

library(data.table)
options(scipen = 999)

# ============================================================
# 常量（原 simulation_benchmark.R:74-78）
# ============================================================
base_ns <- list(test_weight_range = c(200, 20))   # 南沙脚本既有用法，各变体一致

INJECTION_RATES <- c(0.05, 0.10, 0.20)
TYPE_PROBS      <- c(inflate = 0.45, zero = 0.35, negate = 0.20)
SET_SEED        <- 20260826

device_dirs <- c(YANGXIANG = "YANGXIANG_扬翔", FIRE = "FIRE_奥斯本", NEDAP = "Nedap_睿保乐")

# ============================================================
# 仓库根定位
#
# 原脚本在 测试/ 下用 file.path(script_dir, "..") 得到仓库根；本模块的脚本在
# 测试/adfi_correction_research/scripts/ 下，同一写法会算错，故改为向上找 .git。
# 同时兼容 source() 与 Rscript 两种调用（script_dir 只在 source() 下有值）。
# ============================================================
find_project_root <- function(start = NULL) {
  if (is.null(start)) {
    script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
    start <- if (!identical(script_dir, "")) script_dir else getwd()
  }
  d <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, ".git"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) stop("向上走到文件系统根仍未找到 .git", call. = FALSE)
    d <- parent
  }
}

load_zhenmeasure <- function(project_root) {
  pkgload::load_all(file.path(project_root, "项目本体", "ZhenMeasure"),
                    quiet = TRUE, export_all = TRUE)
  invisible(NULL)
}

# ============================================================
# 设备解析（原 simulation_benchmark.R:36-51）
#
# 注意 扬翔 的 原始数据/ 下还有 扬翔全数据_一般测试不跑/（19 GB、2151 个 xlsx），
# 目录名即约定「一般测试不跑」。不加这个分支会把整仓递归读进来。
# ============================================================
resolve_device <- function(DEVICE, project_root) {
  DEVICE <- toupper(DEVICE)
  if (!DEVICE %in% names(device_dirs)) {
    stop("未知设备类型：", DEVICE, "（可选 YANGXIANG / FIRE / NEDAP）", call. = FALSE)
  }
  dev_base <- file.path(project_root, "测试/demo/demo_input", device_dirs[[DEVICE]])
  data_path <- if (DEVICE == "YANGXIANG") {
    file.path(dev_base, "原始数据", "南沙")
  } else {
    file.path(dev_base, "原始数据")
  }
  format_path <- list.files(file.path(dev_base, "附加信息"),
                            pattern = "[.]json$", full.names = TRUE)[1]
  list(device = DEVICE, dev_base = dev_base, data_path = data_path,
       format_path = format_path)
}

# ============================================================
# 共享 Steps 1-4：读一次数据 + 三层 QC（原 simulation_benchmark.R:104-114）
#
# 运行口径铁律（计划 §17.4，违反会静默产生错误结果）：
#   test_weight_range = c(200, 20)  —— 默认 c(45,110) 会把 NEDAP 从 29 头削到 2 头且不报错
#   keep_ids = NULL                 —— keep_fire_ids.txt 只有 9 个 ID，传了会把 FIRE 从 211 头压到 10 头
# ============================================================
# 基准配置：Steps 1-4 与主循环里「污染数据重跑 feed QC」必须用**同一份**，
# 否则两边门控不一致，检出率与分数无法归因。原脚本里它是全局量（:107、:324）。
make_base_config <- function(base_ns) {
  ZhenM_merge_config(list(national_standard = base_ns))
}

run_steps1_4 <- function(data_path, DEVICE, format_path, base_ns) {
  cat(">>> Steps 1-4：读取 + Overall/Weight/Feed QC ...\n")
  t0 <- Sys.time()
  standard_data <- ZhenM_read_data(data_path, DEVICE, format_path, NULL)
  cfg_base <- make_base_config(base_ns)
  qc_result <- ZhenM_qc_overall(standard_data, config = cfg_base, logger = NULL, keep_ids = NULL)
  standard_data <- qc_result$records
  standard_data <- ZhenM_qc_weight_standard(standard_data, "national_standard", cfg_base, NULL)
  standard_data <- ZhenM_qc_feed_standard(standard_data, "national_standard", cfg_base, NULL)
  cat(sprintf("    完成，耗时 %.1f 秒；QC 后记录 %d 行、个体 %d 头\n\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs")),
              nrow(standard_data), uniqueN(standard_data$animal_id)))
  standard_data
}

# ============================================================
# 纯净世界 + 真值（原 simulation_benchmark.R:116-134）
#   无任何 feed flag 且 feed_g > 0 的记录 = 真值
#
# 已知限制（计划 §3.1）：纯净世界是**幸存者样本**——真实数据里被剔掉的那些天，
# 恰恰是难恢复的天。所以两轨的绝对精度都偏乐观，只能用于**横向比较**。
# ============================================================
build_clean_world <- function(qc_dt) {
  feed_flag_cols <- intersect(
    c("is_outlier_feed", "flag_feed_out_of_range", "flag_feed_negative", "flag_feed_too_high",
      "flag_duration_negative", "flag_duration_too_long", "flag_duration_zero_with_feed",
      "flag_speed_too_slow", "flag_speed_too_fast", "flag_speed_extreme_low_feed",
      "flag_speed_zero_long_duration", "flag_STL_FI"),
    names(qc_dt))

  any_flag <- qc_dt[, Reduce(`|`, lapply(.SD, function(x) x %in% TRUE)), .SDcols = feed_flag_cols]
  # 注意：保留全部原始列——Step5 的日聚合与 FCR 锚需要体重列，瘦表会让 D 变体退化
  clean_dt <- data.table::copy(qc_dt[!any_flag & !is.na(feed_g) & feed_g > 0])
  cat(sprintf(">>> 纯净世界：%d 条干净记录（占 QC 后 %.1f%%），%d 头、%d 动物天\n",
              nrow(clean_dt), 100 * nrow(clean_dt) / nrow(qc_dt),
              uniqueN(clean_dt$animal_id),
              uniqueN(clean_dt[, paste(animal_id, record_date)])))

  # 真实日和（ground truth）
  truth_daily <- clean_dt[, .(true_feed = sum(feed_g)), by = .(animal_id, record_date)]
  list(clean_dt = clean_dt, truth_daily = truth_daily)
}

# ============================================================
# 注入函数：对 copy 施加已知损坏，返回（污染数据, 受影响日×类型表）
#   详见 simulation_benchmark.R:1-11 的设计说明。**函数体与原脚本逐字节一致。**
# ============================================================
inject_errors <- function(dt_clean, rate) {
  dt <- data.table::copy(dt_clean)
  dt[, feed_original := feed_g]   # 先存真值再破坏
  set.seed(SET_SEED + round(rate * 1000))

  n_inj <- floor(nrow(dt) * rate)
  idx <- sample.int(nrow(dt), n_inj)
  types <- sample(names(TYPE_PROBS), n_inj, replace = TRUE, prob = TYPE_PROBS)

  i_inf <- idx[types == "inflate"]
  i_zer <- idx[types == "zero"]
  i_neg <- idx[types == "negate"]
  dt[i_inf, feed_g := feed_g * runif(.N, 1.5, 3.0)]
  dt[i_zer, feed_g := 0]
  dt[i_neg, feed_g := -feed_g]
  dt[idx, injected_type := types]

  # 受影响 (动物天 × 损坏类型) 明细；该天完整真值由 truth_daily 提供
  affected <- dt[!is.na(injected_type),
                 .(n_injected_rec = .N,
                   true_injected_sum = sum(feed_original)),
                 by = .(animal_id, record_date, inj_types = injected_type)]

  dt[, feed_original := NULL]
  list(dt_injected = dt, affected = affected, n_inj = n_inj)
}

# ============================================================
# 评价指标（原 simulation_benchmark.R:171-196）
#   accuracy = 1 − Σ|est − true| / Σ true   （NA 天按 0 计入——丢天即损失）
#   bias     = (Σest − Σtrue) / Σtrue
#   **函数体与原脚本逐字节一致。**
#
# ⚠️ 这个函数**只对问题 A 成立**，问题 B 必须用 eval_b.R 的 eval_metrics_b()：
#    `merge(daily_est, truth, all.x = TRUE)` 以**臂自身的输出**为锚，被臂删掉的
#    动物天根本不进 m，fcoalesce(est, 0) 永不触发 —— 「什么都不恢复」的臂
#    accuracy 会虚高到 ≈1.0。A 轨没暴露这个问题，只因 A 的注入器从不删行。
#    改动本函数会破坏 00_capture_baseline.R / 01_smoke_skeleton.R 的重放门。
# ============================================================
eval_metrics <- function(daily_est, truth, affected = NULL) {
  m <- merge(daily_est[, .(animal_id, record_date, est = daily_feed_g)],
             truth, by = c("animal_id", "record_date"), all.x = TRUE)
  m <- m[!is.na(true_feed)]
  m[, est_filled := data.table::fcoalesce(est, 0)]
  acc  <- 1 - sum(abs(m$est_filled - m$true_feed)) / sum(m$true_feed)
  bias <- (sum(m$est_filled) - sum(m$true_feed)) / sum(m$true_feed)
  covg <- mean(!is.na(m$est))

  # 个体层 ADFI 相关度
  by_animal <- m[, .(est_adfi = mean(est_filled), true_adfi = mean(true_feed)), by = animal_id]
  r_pearson <- suppressWarnings(cor(by_animal$est_adfi, by_animal$true_adfi))
  rho_sp <- suppressWarnings(cor(by_animal$est_adfi, by_animal$true_adfi,
                                 method = "spearman"))

  # 分损坏类型的受影响天恢复率（est/true，按 动物天×类型 行计）
  type_tab <- NULL
  if (!is.null(affected)) {
    aff <- merge(affected, m[, .(animal_id, record_date, est_filled, true_feed)],
                 by = c("animal_id", "record_date"), all.x = TRUE)
    aff[, est_filled := data.table::fcoalesce(est_filled, 0)]
    type_tab <- aff[, .(ratio_mean = mean(est_filled / true_feed), n_days = .N),
                    by = inj_types]
  }
  list(acc = acc, bias = bias, coverage = covg, r = r_pearson, rho = rho_sp, by_type = type_tab)
}
