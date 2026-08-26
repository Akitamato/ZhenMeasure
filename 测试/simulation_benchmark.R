######### Phase 0 注入式仿真基准：已知损坏下的校正恢复准确度（issue #5） #########
#
# 设计（复刻 Jiao et al. 2016, JAS 94:824 的模拟思路到南沙扬翔数据）：
#   1. 取通过全部 feed flag 的干净记录构成「纯净世界」——每条 feed_g 视为真值
#   2. 按比例注入三种已知损坏（时长一律不动，模拟真实传感器故障模式）：
#      - inflate : feed_g × U(1.5, 3)   虚高（对应速度超限/单次过高型故障）
#      - zero    : feed_g := 0          漏记型（猪在采食但传感器没记到）
#      - negate  : feed_g := -feed_g    负值翻转（称重信号异常）
#   3. 重跑 ZhenM_qc_feed_standard 让 QC 在被污染的数据上重新打 flag
#      （允许漏检——端到端口径：校正只能处理 QC 检出来的部分）
#   4. 各变体跑 Step5 校正，恢复日和 vs 真实日和
#
# 指标（Jiao 2016 口径）：
#   accuracy = 1 − Σ|est − true| / Σ true   （NA 天按 0 计入——丢天即损失）
#   bias     = (Σest − Σtrue) / Σtrue       （方向性：+高估 / −低估）
#   另报：分损坏类型受影响天的恢复率、个体 ADFI 相关度、可用天覆盖
#
# 变体矩阵（与 compare_correction_variants.R 同一套开关）：
#   E_inj 不校正（污染原样，脚本内直接按天求和，不走 Step5 校正门控）
#   C0 置零不补偿  B 置零+LMM  A 记录级物理  D 记录级+FCR锚
#
# 运行：/data6/home/yhliao/00_Software/conda/miniconda3/envs/yhliao_R/bin/Rscript 测试/simulation_benchmark.R

rm(list = ls())
options(scipen = 999)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
project_root <- if (script_dir == "") getwd() else normalizePath(file.path(script_dir, ".."))

library(data.table)
pkg_dir <- file.path(project_root, "项目本体/ZhenMeasure")
pkgload::load_all(pkg_dir, quiet = TRUE, export_all = TRUE)

# --- 路径与配置 ---
data_path   <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/原始数据/南沙")
format_path <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/附加信息/YANGXIANG_data_format.json")
if (!file.exists(format_path)) {
  format_path <- list.files(file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/附加信息"),
                            pattern = "\\.json$", full.names = TRUE)[1]
}
out_dir <- file.path(project_root, "测试/demo/demo_output/YANGXIANG_扬翔",
                     sprintf("injection_benchmark_%s", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

base_ns <- list(test_weight_range = c(200, 20))   # 南沙脚本既有用法，各变体一致

INJECTION_RATES <- c(0.05, 0.10, 0.20)
TYPE_PROBS      <- c(inflate = 0.45, zero = 0.35, negate = 0.20)
SET_SEED        <- 20260826

variants <- list(
  list(key = "C0", label = "C0_置零不补偿",
       sw = list(use_record_feed_correction = FALSE, use_lmm_feed_correction = FALSE)),
  list(key = "B",  label = "B_V110行为(置零+LMM)",
       sw = list(use_record_feed_correction = FALSE)),
  list(key = "A",  label = "A_V111现状(记录级物理)",
       sw = list()),
  list(key = "D",  label = "D_记录级+FCR锚",
       sw = list(use_fcr_anchor = TRUE))
)

# ============================================================
# 共享 Steps 1-4：读一次数据 + 三层 QC
# ============================================================
cat(">>> Steps 1-4：读取 + Overall/Weight/Feed QC ...\n")
t0 <- Sys.time()
standard_data <- ZhenM_read_data(data_path, "YANGXIANG", format_path, NULL)
cfg_base <- ZhenM_merge_config(list(national_standard = base_ns))
qc_result <- ZhenM_qc_overall(standard_data, config = cfg_base, logger = NULL, keep_ids = NULL)
standard_data <- qc_result$records
standard_data <- ZhenM_qc_weight_standard(standard_data, "national_standard", cfg_base, NULL)
standard_data <- ZhenM_qc_feed_standard(standard_data, "national_standard", cfg_base, NULL)
cat(sprintf("    完成，耗时 %.1f 秒；QC 后记录 %d 行、个体 %d 头\n\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")),
            nrow(standard_data), uniqueN(standard_data$animal_id)))

# ============================================================
# 构建纯净世界：无任何 feed flag 且 feed_g > 0 的记录 = 真值
# ============================================================
feed_flag_cols <- intersect(
  c("is_outlier_feed", "flag_feed_out_of_range", "flag_feed_negative", "flag_feed_too_high",
    "flag_duration_negative", "flag_duration_too_long", "flag_duration_zero_with_feed",
    "flag_speed_too_slow", "flag_speed_too_fast", "flag_speed_extreme_low_feed",
    "flag_speed_zero_long_duration", "flag_STL_FI"),
  names(standard_data))

any_flag <- standard_data[, Reduce(`|`, lapply(.SD, function(x) x %in% TRUE)), .SDcols = feed_flag_cols]
# 注意：保留全部原始列——Step5 的日聚合与 FCR 锚需要体重列，瘦表会让 D 变体退化
clean_dt <- data.table::copy(standard_data[!any_flag & !is.na(feed_g) & feed_g > 0])
cat(sprintf(">>> 纯净世界：%d 条干净记录（占 QC 后 %.1f%%），%d 头、%d 动物天\n",
            nrow(clean_dt), 100 * nrow(clean_dt) / nrow(standard_data),
            uniqueN(clean_dt$animal_id), uniqueN(clean_dt[, paste(animal_id, record_date)])))

# 真实日和（ground truth）
truth_daily <- clean_dt[, .(true_feed = sum(feed_g)), by = .(animal_id, record_date)]

rm(standard_data, qc_result, any_flag); invisible(gc(verbose = FALSE))

# ============================================================
# 注入函数：对 copy 施加已知损坏，返回（污染数据, 受影响日×类型表）
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
# 主循环：rate × variant
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

results <- list(); type_rows <- list()
for (rate in INJECTION_RATES) {
  cat(sprintf(">>> 注入率 %.0f%% ...\n", rate * 100))
  inj <- inject_errors(clean_dt, rate)
  dt_inj <- inj$dt_injected
  affected <- inj$affected   # 动物天×类型明细，eval 时与真值日表 join

  # 重跑 feed QC：在污染数据上重新打 flag（端到端：漏检计入）
  dt_qc <- suppressMessages(
    ZhenM_qc_feed_standard(dt_inj, "national_standard", cfg_base, NULL))
  n_flagged_new <- sum(dt_qc$is_outlier_feed %in% TRUE, na.rm = TRUE)
  detect_rate <- n_flagged_new / max(1, inj$n_inj)
  cat(sprintf("    注入 %d 条 → QC 标出 %d 条（检出率 %.1f%%，含原本即被标出的记录）\n",
              inj$n_inj, n_flagged_new, 100 * detect_rate))

  # E_inj：污染数据原始日和（含负值），不经过任何校正门控
  daily_raw_e <- dt_qc[, .(daily_feed_g = sum(feed_g, na.rm = TRUE)),
                       by = .(animal_id, record_date)]
  met_e <- eval_metrics(daily_raw_e, truth_daily, affected)
  results[[length(results) + 1]] <- data.table(
    rate = rate, variant = "E_inj", label = "E_污染不校正",
    accuracy = met_e$acc, bias = met_e$bias, coverage = met_e$coverage,
    adfi_r = met_e$r, adfi_rho = met_e$rho,
    na_days = 0L)
  if (!is.null(met_e$by_type)) {
    met_e$by_type[, `:=`(rate = rate, variant = "E_inj")]
    type_rows[[length(type_rows) + 1]] <- met_e$by_type
  }
  cat(sprintf("    E_inj %-24s acc=%.3f bias=%+.3f cover=%.3f\n",
              "E_污染不校正", met_e$acc, met_e$bias, met_e$coverage))
  rm(daily_raw_e); invisible(gc(verbose = FALSE))

  for (v in variants) {
    cfg_v <- ZhenM_merge_config(list(national_standard = modifyList(base_ns, v$sw)))
    daily_pre <- suppressMessages(
      ZhenM_standard_to_daily_filtered(data.table::copy(dt_qc), cfg_v))
    met <- eval_metrics(daily_pre, truth_daily, affected)

    results[[length(results) + 1]] <- data.table(
      rate = rate, variant = v$key, label = v$label,
      accuracy = met$acc, bias = met$bias, coverage = met$coverage,
      adfi_r = met$r, adfi_rho = met$rho,
      na_days = sum(is.na(daily_pre$daily_feed_g)))
    if (!is.null(met$by_type)) {
      met$by_type[, `:=`(rate = rate, variant = v$key)]
      type_rows[[length(type_rows) + 1]] <- met$by_type
    }
    cat(sprintf("    %-4s %-24s acc=%.3f bias=%+.3f cover=%.3f\n",
                v$key, v$label, met$acc, met$bias, met$coverage))
    rm(daily_pre); invisible(gc(verbose = FALSE))
  }
  rm(inj, dt_inj, dt_qc, affected); invisible(gc(verbose = FALSE))
}

# ============================================================
# 汇总输出
# ============================================================
summary <- rbindlist(results)
summary[, accuracy := round(accuracy, 4)]
summary[, bias := round(bias, 4)]
summary[, coverage := round(coverage, 4)]
summary[, adfi_r := round(adfi_r, 4)]
summary[, adfi_rho := round(adfi_rho, 4)]

cat("\n=== 注入仿真基准主表（准确度越高越好；bias 正=高估负=低估） ===\n")
print(summary[, .(rate, variant, accuracy, bias, coverage, adfi_r)])

by_type <- rbindlist(type_rows, fill = TRUE)
by_type[, ratio_mean := round(ratio_mean, 3)]
cat("\n=== 分损坏类型：受影响天的 est/true 均值（1=完美恢复） ===\n")
print(by_type[order(rate, variant, inj_types)])

fwrite(summary, file.path(out_dir, "accuracy_by_variant_rate.csv"))
fwrite(by_type, file.path(out_dir, "recovery_ratio_by_type.csv"))

cat(sprintf("\n=== 完成，结果已写入: %s ===\n", out_dir))
