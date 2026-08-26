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
# 运行：/data6/home/yhliao/00_Software/conda/miniconda3/envs/yhliao_R/bin/Rscript 测试/simulation_benchmark.R [YANGXIANG|FIRE|NEDAP]
#   设备参数缺省为 YANGXIANG；泛化复测时传 FIRE / NEDAP（数据在 demo_input 对应厂商目录）

rm(list = ls())
options(scipen = 999)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
project_root <- if (script_dir == "") getwd() else normalizePath(file.path(script_dir, ".."))

library(data.table)
pkg_dir <- file.path(project_root, "项目本体/ZhenMeasure")
pkgload::load_all(pkg_dir, quiet = TRUE, export_all = TRUE)

# --- 设备选择与路径 ---
dev_args <- commandArgs(trailingOnly = TRUE)
DEVICE <- if (length(dev_args) >= 1) toupper(dev_args[1]) else "YANGXIANG"
device_dirs <- c(YANGXIANG = "YANGXIANG_扬翔", FIRE = "FIRE_奥斯本", NEDAP = "Nedap_睿保乐")
if (!DEVICE %in% names(device_dirs)) {
  stop("未知设备类型：", DEVICE, "（可选 YANGXIANG / FIRE / NEDAP）", call. = FALSE)
}
dev_base <- file.path(project_root, "测试/demo/demo_input", device_dirs[[DEVICE]])
data_path   <- file.path(dev_base, "原始数据")
format_path <- list.files(file.path(dev_base, "附加信息"),
                          pattern = "[.]json$", full.names = TRUE)[1]

# G_cens 保守化参数（CLI 可选覆盖）：Rscript sim.R <DEVICE> <quantile> <shrink>
#   quantile：分位数删失界（0~1，传入 0 表示退回纯物理界）；shrink：复活量折扣
G_QUANTILE <- if (length(dev_args) >= 2 && is.finite(as.numeric(dev_args[2]))) {
  as.numeric(dev_args[2])
} else 0
G_SHRINK <- if (length(dev_args) >= 3 && is.finite(as.numeric(dev_args[3]))) {
  as.numeric(dev_args[3])
} else 0.7
settings_tag <- if (length(dev_args) >= 2) {
  sprintf("_q%s_s%.2f", G_QUANTILE, G_SHRINK)
} else ""

out_dir <- file.path(project_root, "测试/demo/demo_output", device_dirs[[DEVICE]],
                     sprintf("injection_benchmark_%s_%s%s", tolower(DEVICE),
                             format(Sys.time(), "%Y%m%d_%H%M%S"), settings_tag))
cat(sprintf(">>> 设备=%s | G 保守化：quantile=%s shrink=%.2f\n",
            DEVICE,
            if (G_QUANTILE > 0) as.character(G_QUANTILE) else "物理界(无分位)",
            G_SHRINK))
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
       sw = list(use_fcr_anchor = TRUE)),
  list(key = "F",  label = "F_记录级+叠加LMM",
       sw = list(use_lmm_stacking = TRUE))
)

# ============================================================
# 共享 Steps 1-4：读一次数据 + 三层 QC
# ============================================================
cat(">>> Steps 1-4：读取 + Overall/Weight/Feed QC ...\n")
t0 <- Sys.time()
standard_data <- ZhenM_read_data(data_path, DEVICE, format_path, NULL)
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

# ============================================================
# Phase 2 PoC：右删失 Tobit + lme4 的 EM 迭代（issue #5 步骤2）
# 被物理规则「噪声置零」的记录，其真实采食量 ∈ (0, speed_max×时长/60]，
# 在对数尺度上是右删失观测。用干净记录拟合 log(feed) ~ log(dur)+(1|animal)
# 混合模型，EM 迭代更新删失记录的截尾条件期望，最后以 exp(ẑ) 复活这些
# 记录（A 路径纠正值打底），按 Step5 同款日规则聚合。近似口径：
# 删失行的当前期望直接当作观测入模（ECM 风格）、σ 只由伪残差估计——
# 方向正确、方差略低估，PoC 可接受。
# ============================================================
.em_censored_daily <- function(dt_qc, seed, max_iter = 5,
                               clean_subsample = 250000L, speed_max = 170,
                               quantile_bound = NULL, shrink = 0.7) {
  noise_flags <- c("flag_feed_negative", "flag_speed_extreme_low_feed",
                   "flag_speed_zero_long_duration")
  has_noise <- Reduce(`|`, lapply(noise_flags, function(f)
    if (f %in% names(dt_qc)) dt_qc[[f]] %in% TRUE else rep(FALSE, nrow(dt_qc))))

  corr <- .correct_feed_records(data.table::copy(dt_qc))
  feed_vec <- corr$feed_corrected

  dur_name <- if ("duration_sec" %in% names(dt_qc)) "duration_sec"
              else if ("Duration" %in% names(dt_qc)) "Duration" else NULL
  if (is.null(dur_name) || !"animal_id" %in% names(dt_qc)) return(NULL)
  dur_v <- suppressWarnings(as.numeric(dt_qc[[dur_name]]))

  is_clean <- !has_noise & !is.na(feed_vec) & feed_vec > 0 &
              !is.na(dur_v) & dur_v > 0
  if (sum(has_noise) == 0 || sum(is_clean) < 1000) return(NULL)
  id_v <- dt_qc$animal_id

  # 删失上界 U（克）：默认取 min(物理上限 speed_max×时长/60,
  #   个体干净速率分位数 × 时长 / 60)。分位数界把期望值从「远处的物理尾部」
  #   拉回该个体真实采食水平，避免过补（保守化杠杆②）；
  #   quantile_bound=NULL 时退回纯物理界（原行为）。
  # 时长不可用者用个体干净中位时长兜底
  med_dur_by <- tapply(dur_v[is_clean], id_v[is_clean], median, na.rm = TRUE)
  gmed <- stats::median(dur_v[is_clean], na.rm = TRUE)
  eff_dur <- dur_v[has_noise]
  miss <- is.na(eff_dur) | eff_dur <= 0
  if (any(miss)) {
    md <- as.numeric(med_dur_by)[match(id_v[has_noise][miss], names(med_dur_by))]
    md[is.na(md)] <- gmed
    eff_dur[miss] <- md
  }
  u_phys <- pmax(speed_max * eff_dur / 60, 1)
  u_bound <- u_phys
  if (!is.null(quantile_bound) && is.finite(quantile_bound)) {
    rate_clean <- feed_vec[is_clean] / dur_v[is_clean]
    rate_q_by <- tapply(rate_clean, id_v[is_clean],
                        function(x) as.numeric(stats::quantile(x, quantile_bound, na.rm = TRUE)))
    rate_q <- as.numeric(rate_q_by)[match(id_v[has_noise], names(rate_q_by))]
    rate_q[is.na(rate_q)] <- as.numeric(stats::quantile(rate_clean, quantile_bound, na.rm = TRUE))
    u_bound <- pmin(u_phys, pmax(rate_q * eff_dur / 60, 1))
  }

  # 建模池：干净子样本（固定种子）+ 全部噪声行
  set.seed(seed)
  pool_clean_i <- which(is_clean)
  if (length(pool_clean_i) > clean_subsample) {
    pool_clean_i <- sample(pool_clean_i, clean_subsample)
  }
  pool_noise_i <- which(has_noise)
  n_c <- length(pool_clean_i)

  durs_pool <- c(dur_v[pool_clean_i], eff_dur)
  ids_pool  <- c(id_v[pool_clean_i], id_v[pool_noise_i])
  x_log <- log(durs_pool)
  sel_noise <- c(rep(FALSE, n_c), rep(TRUE, length(pool_noise_i)))
  u_log_noise <- log(u_bound)

  # 初始化隐变量：个体干净速率 × 时长（物理一致），封顶到 U
  rate_med <- tapply(feed_vec[pool_clean_i] / dur_v[pool_clean_i],
                     id_v[pool_clean_i], median, na.rm = TRUE)
  gr <- as.numeric(rate_med)[match(ids_pool[sel_noise], names(rate_med))]
  gr[is.na(gr)] <- stats::median(feed_vec[pool_clean_i] / dur_v[pool_clean_i],
                                 na.rm = TRUE)
  z <- pmin(log(gr * durs_pool[sel_noise]), u_log_noise)

  animal_f <- factor(ids_pool)
  for (it in seq_len(max_iter)) {
    y_pool <- c(log(feed_vec[pool_clean_i]), z)
    fit <- tryCatch(lme4::lmer(y_pool ~ x_log + (1 | animal_f)),
                    error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    beta <- lme4::fixef(fit); sig <- stats::sigma(fit)
    re_obj <- lme4::ranef(fit)$animal_f
    b <- re_obj[[1]]; names(b) <- rownames(re_obj)
    bvec <- b[as.character(ids_pool[sel_noise])]
    bvec[is.na(bvec)] <- 0
    mu <- beta[[1]] + beta[[2]] * x_log[sel_noise] + bvec
    alpha <- (u_log_noise - mu) / sig
    z_new <- mu - sig * stats::dnorm(alpha) / pmax(stats::pnorm(alpha), 1e-12)
    delta <- mean(abs(z_new - z)); z <- z_new
    cat(sprintf("      [EM iter %d] mean|dz|=%.4f\n", it, delta))
    if (delta < 1e-3) break
  }

  feed_resurrect <- feed_vec
  # 保守化杠杆①：复活量打折扣 shrink（默认 0.7）。宁可少补不可多补——
  # 低估方向安全（残余 −2~−4% 可接受），高估会把个体 ADFI 整体抬高。
  feed_resurrect[pool_noise_i] <- shrink * pmin(pmax(exp(z), 1e-3), u_bound)

  oor <- if ("flag_feed_out_of_range" %in% names(dt_qc)) {
    dt_qc$flag_feed_out_of_range %in% TRUE
  } else rep(FALSE, nrow(dt_qc))
  agg <- data.table::data.table(animal_id = id_v, record_date = dt_qc$record_date,
                                f = feed_resurrect, oor = oor)
  daily <- agg[, .(
    daily_feed_g = if (any(oor)) NA_real_ else {
      v <- f[f > 0 & !is.na(f)]
      if (length(v) == 0) NA_real_ else sum(v)
    }
  ), by = .(animal_id, record_date)]
  daily[!is.na(daily_feed_g) & daily_feed_g > 6000, daily_feed_g := NA_real_]
  daily[]
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

  # G_cens：Phase 2 PoC —— A 路径打底 + 右删失 Tobit EM 复活噪声置零记录
  cat("    G_cens: 拟合删失混合模型（EM）...\n")
  daily_raw_g <- tryCatch(
    .em_censored_daily(dt_qc, SET_SEED + round(rate * 1000),
                       quantile_bound = if (G_QUANTILE > 0) G_QUANTILE else NULL,
                       shrink = G_SHRINK),
    error = function(e) {
      message(sprintf("G_cens failed: %s", conditionMessage(e)))
      NULL
    })
  if (!is.null(daily_raw_g)) {
    met_g <- eval_metrics(daily_raw_g, truth_daily, affected)
    results[[length(results) + 1]] <- data.table(
      rate = rate, variant = "G_cens", label = "G_A路径+删失EM复活",
      accuracy = met_g$acc, bias = met_g$bias, coverage = met_g$coverage,
      adfi_r = met_g$r, adfi_rho = met_g$rho,
      na_days = sum(is.na(daily_raw_g$daily_feed_g)))
    if (!is.null(met_g$by_type)) {
      met_g$by_type[, `:=`(rate = rate, variant = "G_cens")]
      type_rows[[length(type_rows) + 1]] <- met_g$by_type
    }
    cat(sprintf("    %-6s %-24s acc=%.3f bias=%+.3f cover=%.3f\n",
                "G_cens", "G_A路径+删失EM复活", met_g$acc, met_g$bias,
                met_g$coverage))
    rm(daily_raw_g); invisible(gc(verbose = FALSE))
  }

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
