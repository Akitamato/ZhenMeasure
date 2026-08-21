###############################################################################
# Nedap QC 方法对比测试：national_standard vs legacy
# 输出：per_source/Nedap_metrics.json, per_source/Nedap_qc_flag_stats.csv,
#       phenotypes_compare/Nedap_pheno_*.csv
###############################################################################

rm(list = ls())
library(ZhenMeasure)
library(data.table)

# ── 路径定义 ──────────────────────────────────────────────────────────────────
project_root <- file.path("D:", "My_project", "Cooperation_Project", "横向",
  "长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)",
  "扬翔群体饲喂仪器数据处理脚本开发")

data_path    <- file.path(project_root, "V项目测试与开发", "测试", "demo", "demo_standard", "Farm_B_Nedap")
format_path  <- file.path(data_path, "Data_format", "NEDAP_data_format.json")

out_root     <- file.path(project_root, "V项目测试与开发", "测试", "大规模测试", "Legacy问题处理")
per_source   <- file.path(out_root, "per_source")
pheno_dir    <- file.path(out_root, "phenotypes_compare")

# 确保输出目录存在
dir.create(per_source, recursive = TRUE, showWarnings = FALSE)
dir.create(pheno_dir,  recursive = TRUE, showWarnings = FALSE)

# ── 辅助函数：收集 QC flag 统计 ──────────────────────────────────────────────
collect_qc_flag_stats <- function(result, method_name) {
  cr <- result$corrected_records
  if (is.null(cr) || nrow(cr) == 0) {
    return(data.table(method = method_name, flag = "(no data)", count = 0L))
  }

  # 识别所有 flag 列（is_outlier_* 和 flag_*）
  flag_cols <- grep("^(is_outlier|flag)_", names(cr), value = TRUE)
  if (length(flag_cols) == 0) {
    return(data.table(method = method_name, flag = "(no flag columns)", count = 0L))
  }

  rows <- lapply(flag_cols, function(fc) {
    data.table(
      method = method_name,
      flag   = fc,
      count  = sum(cr[[fc]] == TRUE, na.rm = TRUE)
    )
  })
  rbindlist(rows)
}

# ── 收集表型指标 ──────────────────────────────────────────────────────────────
collect_phenotype_metrics <- function(result, method_name) {
  ph <- result$phenotypes
  if (is.null(ph) || nrow(ph) == 0) {
    return(list(
      method   = method_name,
      animals  = 0L,
      mean_FCR = NA_real_,
      sd_FCR   = NA_real_,
      mean_ADFI = NA_real_,
      mean_ADG  = NA_real_,
      FCR_NA   = NA_integer_
    ))
  }
  list(
    method    = method_name,
    animals   = nrow(ph),
    mean_FCR  = round(mean(ph$FCR, na.rm = TRUE), 4),
    sd_FCR    = round(sd(ph$FCR, na.rm = TRUE), 4),
    mean_ADFI = round(mean(ph$ADFI_g, na.rm = TRUE), 2),
    mean_ADG  = round(mean(ph$ADG_g, na.rm = TRUE), 2),
    FCR_NA    = sum(is.na(ph$FCR))
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 步骤 B: 运行 national_standard
# ══════════════════════════════════════════════════════════════════════════════
cat("\n========================================\n")
cat("  Nedap - national_standard\n")
cat("========================================\n")

nat_output_dir <- file.path(per_source, "national_Nedap")
nat_error <- NULL
result_nat <- NULL

result_nat <- tryCatch(
  run_zhen_measure(
    data_path        = data_path,
    data_type        = "NEDAP",
    format_path      = format_path,
    qc_method        = "national_standard",
    phenotype_method = "report",
    stage_mode       = NULL,
    output_dir       = nat_output_dir
  ),
  error = function(e) {
    nat_error <<- conditionMessage(e)
    cat("ERROR in national_standard:", nat_error, "\n")
    cat("Traceback:\n")
    traceback()
    NULL
  }
)

if (!is.null(nat_error)) {
  cat("national_standard FAILED:", nat_error, "\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# 步骤 C: 运行 legacy（放宽参数）
# ══════════════════════════════════════════════════════════════════════════════
cat("\n========================================\n")
cat("  Nedap - legacy (relaxed)\n")
cat("========================================\n")

legacy_cfg <- list(legacy = list(
  feed_intake_range   = c(0, 6000),
  weight_sd_threshold = 4,
  rlm_weight_thresh   = 0.3,
  min_obs_for_wt      = 40,
  growth_curve_r2_min = 0.95
))

leg_output_dir <- file.path(per_source, "legacy_Nedap")
leg_error <- NULL
result_leg <- NULL

result_leg <- tryCatch(
  run_zhen_measure(
    data_path        = data_path,
    data_type        = "NEDAP",
    format_path      = format_path,
    qc_method        = "legacy",
    phenotype_method = "report",
    stage_mode       = NULL,
    config           = legacy_cfg,
    output_dir       = leg_output_dir
  ),
  error = function(e) {
    leg_error <<- conditionMessage(e)
    cat("ERROR in legacy:", leg_error, "\n")
    cat("Traceback:\n")
    traceback()
    NULL
  }
)

if (!is.null(leg_error)) {
  cat("legacy FAILED:", leg_error, "\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# 步骤 D: 收集对比指标
# ══════════════════════════════════════════════════════════════════════════════
cat("\n========================================\n")
cat("  Collecting metrics\n")
cat("========================================\n")

metrics <- list(
  source  = "Nedap",
  errors  = list(
    national = if (is.null(nat_error)) "OK" else nat_error,
    legacy   = if (is.null(leg_error)) "OK" else leg_error
  ),
  step2_overall = list(
    nat_records = if (!is.null(result_nat)) nrow(result_nat$corrected_records) else NA_integer_,
    leg_records = if (!is.null(result_leg)) nrow(result_leg$corrected_records) else NA_integer_
  ),
  step3_weight = list(
    nat_outlier_wt = if (!is.null(result_nat)) sum(result_nat$corrected_records$is_outlier_wt == TRUE, na.rm=TRUE) else NA_integer_,
    leg_outlier_wt = if (!is.null(result_leg)) sum(result_leg$corrected_records$is_outlier_wt == TRUE, na.rm=TRUE) else NA_integer_
  ),
  step4_feed = list(
    nat_outlier_feed = if (!is.null(result_nat)) sum(result_nat$corrected_records$is_outlier_feed == TRUE, na.rm=TRUE) else NA_integer_,
    leg_outlier_feed = if (!is.null(result_leg)) sum(result_leg$corrected_records$is_outlier_feed == TRUE, na.rm=TRUE) else NA_integer_
  ),
  step5_daily = list(
    nat_daily_rows = if (!is.null(result_nat)) nrow(result_nat$daily_records) else NA_integer_,
    leg_daily_rows = if (!is.null(result_leg)) nrow(result_leg$daily_records) else NA_integer_
  ),
  step7_phenotype = list(
    nat_animals  = if (!is.null(result_nat)) nrow(result_nat$phenotypes) else NA_integer_,
    leg_animals  = if (!is.null(result_leg)) nrow(result_leg$phenotypes) else NA_integer_,
    nat_mean_FCR = if (!is.null(result_nat)) round(mean(result_nat$phenotypes$FCR, na.rm=TRUE), 4) else NA_real_,
    leg_mean_FCR = if (!is.null(result_leg)) round(mean(result_leg$phenotypes$FCR, na.rm=TRUE), 4) else NA_real_,
    nat_sd_FCR   = if (!is.null(result_nat)) round(sd(result_nat$phenotypes$FCR, na.rm=TRUE), 4) else NA_real_,
    leg_sd_FCR   = if (!is.null(result_leg)) round(sd(result_leg$phenotypes$FCR, na.rm=TRUE), 4) else NA_real_,
    nat_mean_ADFI = if (!is.null(result_nat)) round(mean(result_nat$phenotypes$ADFI_g, na.rm=TRUE), 2) else NA_real_,
    leg_mean_ADFI = if (!is.null(result_leg)) round(mean(result_leg$phenotypes$ADFI_g, na.rm=TRUE), 2) else NA_real_,
    nat_mean_ADG  = if (!is.null(result_nat)) round(mean(result_nat$phenotypes$ADG_g, na.rm=TRUE), 2) else NA_real_,
    leg_mean_ADG  = if (!is.null(result_leg)) round(mean(result_leg$phenotypes$ADG_g, na.rm=TRUE), 2) else NA_real_,
    nat_FCR_NA    = if (!is.null(result_nat)) sum(is.na(result_nat$phenotypes$FCR)) else NA_integer_,
    leg_FCR_NA    = if (!is.null(result_leg)) sum(is.na(result_leg$phenotypes$FCR)) else NA_integer_
  )
)

# 保存 metrics JSON
jsonlite::write_json(metrics,
  file.path(per_source, "Nedap_metrics.json"),
  auto_unbox = TRUE, pretty = TRUE)
cat("Saved: Nedap_metrics.json\n")

# 保存 phenotype CSV
if (!is.null(result_nat)) {
  fwrite(result_nat$phenotypes, file.path(pheno_dir, "Nedap_pheno_national.csv"))
  cat("Saved: Nedap_pheno_national.csv\n")
}
if (!is.null(result_leg)) {
  fwrite(result_leg$phenotypes, file.path(pheno_dir, "Nedap_pheno_legacy.csv"))
  cat("Saved: Nedap_pheno_legacy.csv\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# 步骤 E: 收集 QC flag 统计
# ══════════════════════════════════════════════════════════════════════════════
cat("\n========================================\n")
cat("  Collecting QC flag statistics\n")
cat("========================================\n")

flag_nat <- if (!is.null(result_nat)) collect_qc_flag_stats(result_nat, "national_standard") else data.table()
flag_leg <- if (!is.null(result_leg)) collect_qc_flag_stats(result_leg, "legacy") else data.table()
flag_all <- rbind(flag_nat, flag_leg)

if (nrow(flag_all) > 0) {
  fwrite(flag_all, file.path(per_source, "Nedap_qc_flag_stats.csv"))
  cat("Saved: Nedap_qc_flag_stats.csv\n")

  # 宽格式对比
  flag_wide <- dcast(flag_all, flag ~ method, value.var = "count", fill = 0L)
  fwrite(flag_wide, file.path(per_source, "Nedap_qc_flag_compare.csv"))
  cat("Saved: Nedap_qc_flag_compare.csv\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# 控制台输出摘要
# ══════════════════════════════════════════════════════════════════════════════
cat("\n\n")
cat("================================================================\n")
cat("  Nedap QC Comparison Test - Summary\n")
cat("================================================================\n")
cat(sprintf("  Error status: national=%s, legacy=%s\n",
  if (is.null(nat_error)) "OK" else "FAILED",
  if (is.null(leg_error)) "OK" else "FAILED"))

cat("\n  -- Step 2: Overall QC --\n")
cat(sprintf("    national records: %d\n", metrics$step2_overall$nat_records))
cat(sprintf("    legacy   records: %d\n", metrics$step2_overall$leg_records))

cat("\n  -- Step 3: Weight QC --\n")
cat(sprintf("    national outlier_wt: %d\n", metrics$step3_weight$nat_outlier_wt))
cat(sprintf("    legacy   outlier_wt: %d\n", metrics$step3_weight$leg_outlier_wt))

cat("\n  -- Step 4: Feed QC --\n")
cat(sprintf("    national outlier_feed: %d\n", metrics$step4_feed$nat_outlier_feed))
cat(sprintf("    legacy   outlier_feed: %d\n", metrics$step4_feed$leg_outlier_feed))

cat("\n  -- Step 5: Daily Aggregation --\n")
cat(sprintf("    national daily rows: %d\n", metrics$step5_daily$nat_daily_rows))
cat(sprintf("    legacy   daily rows: %d\n", metrics$step5_daily$leg_daily_rows))

cat("\n  -- Step 7: Phenotype --\n")
cat(sprintf("    national animals: %d, mean_FCR=%.4f, sd_FCR=%.4f, mean_ADFI=%.2f, mean_ADG=%.2f, FCR_NA=%d\n",
  metrics$step7_phenotype$nat_animals, metrics$step7_phenotype$nat_mean_FCR,
  metrics$step7_phenotype$nat_sd_FCR, metrics$step7_phenotype$nat_mean_ADFI,
  metrics$step7_phenotype$nat_mean_ADG, metrics$step7_phenotype$nat_FCR_NA))
cat(sprintf("    legacy   animals: %d, mean_FCR=%.4f, sd_FCR=%.4f, mean_ADFI=%.2f, mean_ADG=%.2f, FCR_NA=%d\n",
  metrics$step7_phenotype$leg_animals, metrics$step7_phenotype$leg_mean_FCR,
  metrics$step7_phenotype$leg_sd_FCR, metrics$step7_phenotype$leg_mean_ADFI,
  metrics$step7_phenotype$leg_mean_ADG, metrics$step7_phenotype$leg_FCR_NA))

cat("\n  -- QC Flag Stats (wide) --\n")
if (nrow(flag_all) > 0) {
  print(flag_wide)
}

cat("\n================================================================\n")
cat("  All outputs saved to:\n")
cat("    ", per_source, "\n")
cat("    ", pheno_dir, "\n")
cat("================================================================\n")
