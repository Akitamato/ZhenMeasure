######### 校正机制消融实验：关掉记录级矫正，单独评测 FCR 锚定算法（issue #5） #########
#
# 对照矩阵（共享 Steps 1-4，每变体独立跑 Steps 5 / 5.5 / 6 / 7）：
#   E  纯原始参照    ：qc 后标准记录按天求和，不经任何校正（脚本内直接算）
#   C0 置零不补偿    ：记录级 OFF + LMM OFF + 无锚（C1 的孪生基线）
#   B  V1.1.0 行为   ：记录级 OFF + LMM ON
#   A  V1.1.1 现状   ：记录级 ON（默认）
#   C1 纯 FCR 锚     ：记录级 OFF + LMM OFF + FCR 锚 ON   ← 实验核心
#   C2 兜底加 FCR 锚 ：记录级 OFF + LMM ON + FCR 锚 ON
#   D  方案 B 完全体 ：记录级 ON + FCR 锚 ON
#
# 孪生差分归因：D - A = 锚叠加在记录级纠正之上；C2 - B = 锚叠加在兜底之上；
#               C1 - C0 = FCR 锚在「零干预」之上的净效果。
#
# 运行：/data6/home/yhliao/00_Software/conda/miniconda3/envs/yhliao_R/bin/Rscript 测试/compare_correction_variants.R

rm(list = ls())

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
project_root <- if (script_dir == "") getwd() else normalizePath(file.path(script_dir, ".."))

library(data.table)
pkg_dir <- file.path(project_root, "项目本体/ZhenMeasure")
pkgload::load_all(pkg_dir, quiet = TRUE, export_all = TRUE)

# --- 路径 ---
data_path   <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/原始数据/南沙")
format_path <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/附加信息/YANGXIANG_data_format.json")
if (!file.exists(format_path)) {
  format_path <- list.files(file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/附加信息"),
                            pattern = "\\.json$", full.names = TRUE)[1]
}
out_dir <- file.path(project_root, "测试/demo/demo_output/YANGXIANG_扬翔",
                     sprintf("correction_variant_matrix_%s", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

base_ns <- list(test_weight_range = c(200, 20))  # 南沙脚本既有用法，各变体保持一致

# ============================================================
# 共享 Steps 1-4：读一次数据 + 三层 QC（各变体逐字节相同）
# ============================================================
cat(">>> 共享 Steps 1-4：读取 + Overall/Weight/Feed QC ...\n")
t0 <- Sys.time()
standard_data_original <- ZhenM_read_data(data_path, "YANGXIANG", format_path, NULL)
cfg_base <- ZhenM_merge_config(list(national_standard = base_ns))

standard_data <- data.table::copy(standard_data_original)
qc_result <- ZhenM_qc_overall(standard_data, config = cfg_base, logger = NULL, keep_ids = NULL)
standard_data <- qc_result$records
standard_data <- ZhenM_qc_weight_standard(standard_data, "national_standard", cfg_base, NULL)
standard_data <- ZhenM_qc_feed_standard(standard_data, "national_standard", cfg_base, NULL)
qc_standard_data <- data.table::copy(standard_data)   # 循环前固定，杜绝串染
rm(standard_data, standard_data_original, qc_result); invisible(gc(verbose = FALSE))
cat(sprintf("    完成，耗时 %.1f 秒；QC 后记录 %d 行、个体 %d 头\n\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")),
            nrow(qc_standard_data), uniqueN(qc_standard_data$animal_id)))

# ============================================================
# 干净天基线（对应 issue #5 的 ~2084 g/天）：当天无任何 feed flag 的原始日和
# 注意：flag_feed_too_high 按 P99 定义天然标 ~1% 记录，该基线是保守下界
# ============================================================
feed_flag_cols <- intersect(
  c("is_outlier_feed", "flag_feed_out_of_range", "flag_feed_negative", "flag_feed_too_high",
    "flag_duration_negative", "flag_duration_too_long", "flag_duration_zero_with_feed",
    "flag_speed_too_slow", "flag_speed_too_fast", "flag_speed_extreme_low_feed",
    "flag_speed_zero_long_duration", "flag_STL_FI"),
  names(qc_standard_data))

day_flags <- qc_standard_data[, lapply(.SD, function(x) any(x %in% TRUE)),
                              by = .(animal_id, record_date), .SDcols = feed_flag_cols]
day_flags[, any_feed_flag := Reduce(`|`, .SD), .SDcols = feed_flag_cols]
flag_lookup <- day_flags[, .(animal_id, record_date, any_feed_flag)]

raw_daily_E <- qc_standard_data[, .(raw_day_feed = sum(feed_g, na.rm = TRUE)),
                                by = .(animal_id, record_date)]
raw_daily_E <- merge(raw_daily_E, flag_lookup,
                     by = c("animal_id", "record_date"), all.x = TRUE)
raw_daily_E[is.na(any_feed_flag), any_feed_flag := FALSE]

baseline_clean <- mean(raw_daily_E[any_feed_flag == FALSE, raw_day_feed], na.rm = TRUE)
baseline_by_animal <- raw_daily_E[, .(
  clean_adfi = mean(raw_day_feed[any_feed_flag == FALSE], na.rm = TRUE),
  flagged_adfi = mean(raw_day_feed[any_feed_flag == TRUE], na.rm = TRUE),
  clean_days = sum(any_feed_flag == FALSE),
  flagged_days = sum(any_feed_flag == TRUE)), by = animal_id]

cat(sprintf(">>> 干净天基线（未校正原始日和均值）：%.1f g/天（%d 干净天 / 共 %d 天）\n\n",
            baseline_clean, sum(raw_daily_E$any_feed_flag == FALSE), nrow(raw_daily_E)))

# ============================================================
# 复刻 Step 5.5：生长曲线 R² 剔除（与 run_zhen_measure 逐行一致）
# ============================================================
apply_growth_r2_filter <- function(daily, cfg) {
  ids_to_check <- unique(daily$animal_id)
  animals_to_delete <- character()
  min_r2 <- cfg$national_standard$growth_curve_r2_min
  for (id in ids_to_check) {
    sub_daily <- daily[animal_id == id]
    valid_pts <- sub_daily[!is.na(daily_weight_g)]
    if (nrow(valid_pts) < 10) {
      animals_to_delete <- c(animals_to_delete, id)
      next
    }
    x <- as.numeric(valid_pts$record_date - min(valid_pts$record_date))
    y <- valid_pts$daily_weight_g
    fit_res <- .check_growth_fit(y, x, min_r2 = min_r2)
    if (!fit_res$pass) animals_to_delete <- c(animals_to_delete, id)
  }
  list(daily = daily[!animal_id %in% animals_to_delete], deleted = animals_to_delete)
}

# ============================================================
# 变体定义（E 单独处理；其余走 pipeline）
# ============================================================
variants <- list(
  list(key = "C0", label = "C0_置零不补偿",
       sw = list(use_record_feed_correction = FALSE, use_lmm_feed_correction = FALSE)),
  list(key = "B",  label = "B_V110行为",
       sw = list(use_record_feed_correction = FALSE)),
  list(key = "A",  label = "A_V111现状",
       sw = list()),
  list(key = "C1", label = "C1_纯FCR锚",
       sw = list(use_record_feed_correction = FALSE, use_lmm_feed_correction = FALSE,
                 use_fcr_anchor = TRUE)),
  list(key = "C2", label = "C2_兜底加FCR锚",
       sw = list(use_record_feed_correction = FALSE, use_fcr_anchor = TRUE)),
  list(key = "D",  label = "D_方案B完全体",
       sw = list(use_fcr_anchor = TRUE))
)

twin_pairs <- list(C1 = "C0", C2 = "B", D = "A")   # 锚边际效应的孪生差分

res_daily <- list(); res_pheno_all <- list(); res_pheno_stage <- list()
deleted_sets <- list()

for (v in variants) {
  cat(sprintf(">>> 变体 %s：%s\n", v$key, v$label))
  cfg_v <- ZhenM_merge_config(list(national_standard = modifyList(base_ns, v$sw)))

  # Step 5（三层校正门控全部由 cfg_v 驱动）；捕获消息作为门控接通的运行期证据
  msgs <- capture_messages(
    daily_pre <- ZhenM_standard_to_daily_filtered(data.table::copy(qc_standard_data), cfg_v))
  gate_rec <- any(grepl("Record-level feed correction", msgs))
  gate_lmm <- any(grepl("LMM Feed Correction", msgs))

  # Step 5.5 生长曲线 R² 剔除
  filt <- apply_growth_r2_filter(daily_pre, cfg_v)
  daily_pre <- filt$daily
  deleted_sets[[v$key]] <- sort(filt$deleted)

  # 标记干净天（用 update-join 打标，不污染原表结构）
  daily_pre[flag_lookup, clean_day := !i.any_feed_flag, on = .(animal_id, record_date)]
  daily_pre[is.na(clean_day), clean_day := FALSE]

  # 插补前快照指标（归因主终点：adfi_clean_pre，限未插补干净天口径见 summary 注释）
  n_days <- nrow(daily_pre)
  n_na_feed <- sum(is.na(daily_pre$daily_feed_g))
  n_over_limit <- if ("flag_daily_feed_over_limit" %in% names(daily_pre)) {
    sum(daily_pre$flag_daily_feed_over_limit, na.rm = TRUE)
  } else NA_integer_
  n_fcr_fixed <- if ("flag_feed_fcr_corrected" %in% names(daily_pre)) {
    sum(daily_pre$flag_feed_fcr_corrected, na.rm = TRUE)
  } else 0L

  adfi_all_pre   <- mean(daily_pre$daily_feed_g, na.rm = TRUE)
  adfi_clean_pre <- mean(daily_pre[clean_day == TRUE, daily_feed_g], na.rm = TRUE)
  adfi_flag_pre  <- mean(daily_pre[clean_day == FALSE, daily_feed_g], na.rm = TRUE)

  # Step 6：初始化插补标记后插补
  daily_post <- data.table::copy(daily_pre)
  daily_post[, clean_day := NULL]
  if (!"is_imputed_wt" %in% names(daily_post)) daily_post[, is_imputed_wt := FALSE]
  if (!"is_imputed_feed" %in% names(daily_post)) daily_post[, is_imputed_feed := FALSE]
  daily_post <- ZhenM_impute_national(daily_post, cfg_v)
  n_imputed_feed <- sum(daily_post$is_imputed_feed, na.rm = TRUE)

  # Step 7：双口径表型
  p_all   <- ZhenM_calc_phenotypes(data.table::copy(daily_post), "report", config = cfg_v)
  p_stage <- ZhenM_calc_phenotypes(data.table::copy(daily_post), "report", config = cfg_v,
                                   stage_mode = "weight", target_weight_stages = "YANGXIANG")

  res_daily[[v$key]] <- data.table(
    variant = v$key, label = v$label,
    n_animals = uniqueN(daily_pre$animal_id), n_days = n_days,
    na_feed_days = n_na_feed, over_limit_days = n_over_limit,
    fcr_corrected_days = n_fcr_fixed,
    imputed_feed_days = n_imputed_feed,
    adfi_all_pre = adfi_all_pre, adfi_clean_pre = adfi_clean_pre,
    adfi_flagged_pre = adfi_flag_pre,
    adfi_all_post = mean(daily_post$daily_feed_g, na.rm = TRUE),
    gate_record_msg = gate_rec, gate_lmm_msg = gate_lmm)
  res_pheno_all[[v$key]]  <- data.table(variant = v$key, p_all)
  res_pheno_stage[[v$key]] <- data.table(variant = v$key, p_stage)
  rm(daily_pre, daily_post, p_all, p_stage, filt); invisible(gc(verbose = FALSE))
}

# --- 孪生差分：FCR 锚在各基线上的边际效应 ---
fcr_delta <- rbindlist(lapply(names(twin_pairs), function(k) {
  base_k <- twin_pairs[[k]]
  data.table(anchor_on = k, twin_base = base_k,
             delta_adfi_all = res_daily[[k]]$adfi_all_pre - res_daily[[base_k]]$adfi_all_pre,
             delta_adfi_clean = res_daily[[k]]$adfi_clean_pre - res_daily[[base_k]]$adfi_clean_pre,
             delta_adfi_flagged = res_daily[[k]]$adfi_flagged_pre - res_daily[[base_k]]$adfi_flagged_pre,
             fcr_corrected_days = res_daily[[k]]$fcr_corrected_days)
}))

# --- 汇总表 ---
order_keys <- c("E", "C0", "B", "A", "C1", "C2", "D")
e_row <- data.table(
  variant = "E", label = "E_纯原始参照",
  n_animals = uniqueN(raw_daily_E$animal_id), n_days = nrow(raw_daily_E),
  na_feed_days = 0L, over_limit_days = NA_integer_, fcr_corrected_days = 0L,
  imputed_feed_days = 0L,
  adfi_all_pre = mean(raw_daily_E$raw_day_feed, na.rm = TRUE),
  adfi_clean_pre = baseline_clean,
  adfi_flagged_pre = mean(raw_daily_E[any_feed_flag == TRUE, raw_day_feed], na.rm = TRUE),
  adfi_all_post = NA_real_,
  gate_record_msg = FALSE, gate_lmm_msg = FALSE)

summary <- rbind(e_row, rbindlist(res_daily, fill = TRUE), fill = TRUE)[match(order_keys, variant)]
summary[, `:=`(
  delta_vs_baseline = adfi_clean_pre - baseline_clean,
  pct_vs_baseline = 100 * (adfi_clean_pre - baseline_clean) / baseline_clean)]

# ============================================================
# 一致性断言与自检输出
# ============================================================
cat("\n=== 一致性检查 ===\n")
ref_del <- deleted_sets[[variants[[1]]$key]]
del_ok <- all(vapply(deleted_sets, function(x) identical(x, ref_del), logical(1)))
cat(sprintf("生长曲线 R² 剔除个体集各变体一致: %s（%d 头被剔除）\n", del_ok, length(ref_del)))
if (!del_ok) stop("各变体的生长曲线剔除集合不一致——门控接线存在 bug，禁止继续解读！")

gate_tab <- summary[variant != "E", .(variant, gate_record_msg, gate_lmm_msg)]
cat("门控消息证据（Record-level 应仅 A/D 为 TRUE；LMM 应仅 B/C2 为 TRUE）:\n")
print(gate_tab)

cat("\n=== 校正机制消融对比矩阵（主表：插补前日级；adfi_clean 为干净天口径） ===\n")
print(summary[, .(variant, label, n_animals, na_feed_days, fcr_corrected_days, imputed_feed_days,
                  adfi_all_pre = round(adfi_all_pre, 1),
                  adfi_clean_pre = round(adfi_clean_pre, 1),
                  adfi_flagged_pre = round(adfi_flagged_pre, 1),
                  delta_vs_baseline = round(delta_vs_baseline, 1),
                  pct_vs_baseline = round(pct_vs_baseline, 2))])

cat("\n=== 表型层（stage_mode=NULL 总体口径） ===\n")
ph_all <- rbindlist(res_pheno_all, fill = TRUE)
print(ph_all[, .(ADG = round(mean(ADG_g, na.rm = TRUE), 1),
                 ADFI = round(mean(ADFI_g, na.rm = TRUE), 1),
                 FCR_mean = round(mean(FCR, na.rm = TRUE), 3),
                 FCR_sd = round(sd(FCR, na.rm = TRUE), 3),
                 n = uniqueN(animal_id)), by = variant])

cat("\n=== 表型层（weight 阶段 YANGXIANG 口径） ===\n")
ph_stage_sum <- rbindlist(res_pheno_stage, fill = TRUE)[, .(
  ADG = round(mean(ADG_g, na.rm = TRUE), 1),
  ADFI = round(mean(ADFI_g, na.rm = TRUE), 1),
  FCR_mean = round(mean(FCR, na.rm = TRUE), 3),
  FCR_sd = round(sd(FCR, na.rm = TRUE), 3),
  n_rows = .N), by = variant]
print(ph_stage_sum)

cat("\n=== FCR 锚边际效应（孪生差分，g/天） ===\n")
print(fcr_delta)

cat(sprintf("\n参考：issue #5 观察值为 main 分支口径（ADFI≈1995.5 / 干净基线≈2084），本分支含 R² 修复，数字不可直比，仅看相对关系。\n"))

# --- 落盘 ---
fwrite(summary, file.path(out_dir, "summary_by_variant.csv"))
fwrite(ph_all, file.path(out_dir, "phenotypes_by_variant_all.csv"))
fwrite(ph_stage_sum, file.path(out_dir, "phenotypes_by_variant_stage.csv"))
fwrite(fcr_delta, file.path(out_dir, "fcr_anchor_marginal_effect.csv"))
fwrite(baseline_by_animal, file.path(out_dir, "by_animal_clean_vs_flagged_baseline.csv"))

cat(sprintf("\n=== 完成，结果已写入: %s ===\n", out_dir))
