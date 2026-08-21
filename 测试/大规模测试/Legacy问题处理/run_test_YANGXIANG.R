###############################################################################
# YANGXIANG QC 方法对比测试：national_standard vs legacy
# 数据源: Farm_C_YANGXIANG
# 输出目录: V项目测试与开发/测试/大规模测试/Legacy问题处理/
###############################################################################

rm(list = ls())
cat("=== YANGXIANG QC Comparison Test Start ===\n")
cat("Timestamp:", format(Sys.time()), "\n\n")

# ── 路径定义 ──────────────────────────────────────────────────────────────────
base_path <- file.path("D:", "My_project", "Cooperation_Project", "横向",
  "长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)",
  "扬翔群体饲喂仪器数据处理脚本开发",
  "V项目测试与开发")

out_root <- file.path(base_path, "测试", "大规模测试", "Legacy问题处理")
per_source_dir <- file.path(out_root, "per_source")
pheno_compare_dir <- file.path(out_root, "phenotypes_compare")

data_path    <- file.path(base_path, "测试", "demo", "demo_standard", "Farm_C_YANGXIANG")
format_path  <- file.path(data_path, "Data_format", "YANGXIANG_data_format.json")

dir.create(per_source_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pheno_compare_dir, recursive = TRUE, showWarnings = FALSE)

# ── 步骤 A: 加载包 ──────────────────────────────────────────────────────────
cat("Step A: Installing/loading ZhenMeasure...\n")
install.packages(
  file.path(base_path, "项目本体", "ZhenMeasure"),
  repos = NULL, type = "source"
)
library(ZhenMeasure)
library(data.table)

# 检查 jsonlite 是否可用（用于输出 JSON）
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}
library(jsonlite)

# ── 辅助函数 ──────────────────────────────────────────────────────────────────
safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sd   <- function(x) if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)

# ── 步骤 B: 运行 national_standard ──────────────────────────────────────────
cat("\n========================================\n")
cat("Step B: Running national_standard method...\n")
cat("========================================\n")

result_nat <- NULL
nat_error  <- NULL
nat_traceback <- NULL

tryCatch({
  result_nat <- run_zhen_measure(
    data_path             = data_path,
    data_type             = "YANGXIANG",
    format_path           = format_path,
    qc_method             = "national_standard",
    phenotype_method      = "report",
    stage_mode            = "weight",
    target_weight_stages  = "YANGXIANG",
    output_dir            = file.path(per_source_dir, "national_YANGXIANG")
  )
  cat("national_standard completed successfully.\n")
}, error = function(e) {
  nat_error <<- conditionMessage(e)
  nat_traceback <<- paste(capture.output(traceback()), collapse = "\n")
  cat("ERROR in national_standard:", nat_error, "\n")
  cat("Traceback:\n", nat_traceback, "\n")
})

# ── 步骤 C: 运行 legacy（放宽参数）──────────────────────────────────────────
cat("\n========================================\n")
cat("Step C: Running legacy method (relaxed params)...\n")
cat("========================================\n")

legacy_cfg <- list(legacy = list(
  feed_intake_range  = c(0, 6000),
  weight_sd_threshold = 4,
  rlm_weight_thresh   = 0.3,
  min_obs_for_wt      = 40,
  growth_curve_r2_min = 0.95
))

result_leg <- NULL
leg_error  <- NULL
leg_traceback <- NULL

tryCatch({
  result_leg <- run_zhen_measure(
    data_path             = data_path,
    data_type             = "YANGXIANG",
    format_path           = format_path,
    qc_method             = "legacy",
    phenotype_method      = "report",
    stage_mode            = "weight",
    target_weight_stages  = "YANGXIANG",
    config                = legacy_cfg,
    output_dir            = file.path(per_source_dir, "legacy_YANGXIANG")
  )
  cat("legacy completed successfully.\n")
}, error = function(e) {
  leg_error <<- conditionMessage(e)
  leg_traceback <<- paste(capture.output(traceback()), collapse = "\n")
  cat("ERROR in legacy:", leg_error, "\n")
  cat("Traceback:\n", leg_traceback, "\n")
})

# ── 步骤 D: 收集对比指标 ─────────────────────────────────────────────────────
cat("\n========================================\n")
cat("Step D: Collecting comparison metrics...\n")
cat("========================================\n")

# 注意: run_zhen_measure 返回的 corrected_records 只保留前12列（无 flag 列），
# outlier 计数改用 daily_records 中的 n_outlier_wt / n_outlier_feed 汇总。

metrics <- list(
  source    = "YANGXIANG",
  timestamp = format(Sys.time()),
  errors    = list(
    national_standard = if (is.null(nat_error)) "none" else nat_error,
    legacy            = if (is.null(leg_error)) "none" else leg_error
  )
)

# Step 2: Overall QC - corrected_records 行数
metrics$step2_overall <- list(
  nat_records = if (!is.null(result_nat)) nrow(result_nat$corrected_records) else NA,
  leg_records = if (!is.null(result_leg)) nrow(result_leg$corrected_records) else NA
)

# Step 3+4: Outlier counts from daily_records (n_outlier_wt / n_outlier_feed 汇总)
if (!is.null(result_nat)) {
  dr_nat <- result_nat$daily_records
  metrics$step3_weight <- list(
    nat_outlier_wt = as.integer(sum(dr_nat$n_outlier_wt, na.rm = TRUE)),
    leg_outlier_wt = NA
  )
  metrics$step4_feed <- list(
    nat_outlier_feed = as.integer(sum(dr_nat$n_outlier_feed, na.rm = TRUE)),
    leg_outlier_feed = NA
  )
} else {
  metrics$step3_weight <- list(nat_outlier_wt = NA, leg_outlier_wt = NA)
  metrics$step4_feed   <- list(nat_outlier_feed = NA, leg_outlier_feed = NA)
}

if (!is.null(result_leg)) {
  dr_leg <- result_leg$daily_records
  metrics$step3_weight$leg_outlier_wt <- as.integer(sum(dr_leg$n_outlier_wt, na.rm = TRUE))
  metrics$step4_feed$leg_outlier_feed <- as.integer(sum(dr_leg$n_outlier_feed, na.rm = TRUE))
}

# Step 5: Daily records
metrics$step5_daily <- list(
  nat_daily_rows = if (!is.null(result_nat)) nrow(result_nat$daily_records) else NA,
  leg_daily_rows = if (!is.null(result_leg)) nrow(result_leg$daily_records) else NA,
  nat_animals    = if (!is.null(result_nat)) uniqueN(result_nat$daily_records$animal_id) else NA,
  leg_animals    = if (!is.null(result_leg)) uniqueN(result_leg$daily_records$animal_id) else NA
)

# Step 7: Phenotype
if (!is.null(result_nat)) {
  ph_nat <- result_nat$phenotypes
  metrics$step7_phenotype <- list(
    nat_animals   = nrow(ph_nat),
    nat_mean_FCR  = round(safe_mean(ph_nat$FCR), 3),
    nat_sd_FCR    = round(safe_sd(ph_nat$FCR), 3),
    nat_mean_ADFI = round(safe_mean(ph_nat$ADFI_g), 1),
    nat_mean_ADG  = round(safe_mean(ph_nat$ADG_g), 1),
    nat_FCR_NA    = as.integer(sum(is.na(ph_nat$FCR))),
    leg_animals   = NA,
    leg_mean_FCR  = NA,
    leg_sd_FCR    = NA,
    leg_mean_ADFI = NA,
    leg_mean_ADG  = NA,
    leg_FCR_NA    = NA
  )
} else {
  metrics$step7_phenotype <- list(
    nat_animals = NA, nat_mean_FCR = NA, nat_sd_FCR = NA,
    nat_mean_ADFI = NA, nat_mean_ADG = NA, nat_FCR_NA = NA,
    leg_animals = NA, leg_mean_FCR = NA, leg_sd_FCR = NA,
    leg_mean_ADFI = NA, leg_mean_ADG = NA, leg_FCR_NA = NA
  )
}

if (!is.null(result_leg)) {
  ph_leg <- result_leg$phenotypes
  metrics$step7_phenotype$leg_animals   <- nrow(ph_leg)
  metrics$step7_phenotype$leg_mean_FCR  <- round(safe_mean(ph_leg$FCR), 3)
  metrics$step7_phenotype$leg_sd_FCR    <- round(safe_sd(ph_leg$FCR), 3)
  metrics$step7_phenotype$leg_mean_ADFI <- round(safe_mean(ph_leg$ADFI_g), 1)
  metrics$step7_phenotype$leg_mean_ADG  <- round(safe_mean(ph_leg$ADG_g), 1)
  metrics$step7_phenotype$leg_FCR_NA    <- as.integer(sum(is.na(ph_leg$FCR)))
}

# 保存 metrics JSON
json_path <- file.path(per_source_dir, "YANGXIANG_metrics.json")
write_json(metrics, json_path, auto_unbox = TRUE, pretty = TRUE)
cat("Metrics JSON saved to:", json_path, "\n")

# 保存 phenotype 对比 CSV
if (!is.null(result_nat)) {
  fwrite(result_nat$phenotypes, file.path(pheno_compare_dir, "YANGXIANG_pheno_national.csv"))
  cat("National phenotype CSV saved.\n")
}
if (!is.null(result_leg)) {
  fwrite(result_leg$phenotypes, file.path(pheno_compare_dir, "YANGXIANG_pheno_legacy.csv"))
  cat("Legacy phenotype CSV saved.\n")
}

# ── 步骤 E: 收集 QC flag 统计 ───────────────────────────────────────────────
cat("\n========================================\n")
cat("Step E: Collecting QC flag statistics...\n")
cat("========================================\n")

# qc_summary 包含各 flag 类型的计数、百分比和严重程度
collect_qc_flags <- function(result, method_name) {
  if (is.null(result) || is.null(result$qc_summary)) {
    cat(sprintf("  No QC summary available for %s.\n", method_name))
    return(data.table(
      method = character(0), error_type = character(0),
      count = integer(0), percentage = numeric(0), severity = character(0)
    ))
  }
  qs <- copy(result$qc_summary)
  qs[, method := method_name]
  cat(sprintf("  %s: %d flag types found\n", method_name, nrow(qs)))
  return(qs)
}

flag_nat <- collect_qc_flags(result_nat, "national_standard")
flag_leg <- collect_qc_flags(result_leg, "legacy")

# 合并并保存
flag_combined <- rbindlist(list(flag_nat, flag_leg), fill = TRUE)
flag_csv_path <- file.path(per_source_dir, "YANGXIANG_qc_flag_stats.csv")
fwrite(flag_combined, flag_csv_path)
cat("QC flag statistics saved to:", flag_csv_path, "\n")

# 也分别保存（便于独立分析）
if (nrow(flag_nat) > 0) {
  fwrite(flag_nat, file.path(per_source_dir, "YANGXIANG_qc_flags_national.csv"))
}
if (nrow(flag_leg) > 0) {
  fwrite(flag_leg, file.path(per_source_dir, "YANGXIANG_qc_flags_legacy.csv"))
}

# ── 最终汇总输出 ─────────────────────────────────────────────────────────────
cat("\n========================================\n")
cat("=== RESULTS SUMMARY ===\n")
cat("========================================\n")
cat(sprintf("Source: YANGXIANG\n"))
cat(sprintf("Data path: %s\n", data_path))
cat(sprintf("national_standard error: %s\n", if (is.null(nat_error)) "none" else nat_error))
cat(sprintf("legacy error:            %s\n", if (is.null(leg_error)) "none" else leg_error))
cat("\n")

cat("--- Step 2 (Overall QC) ---\n")
cat(sprintf("  national corrected_records: %d rows\n", metrics$step2_overall$nat_records))
cat(sprintf("  legacy    corrected_records: %d rows\n", metrics$step2_overall$leg_records))

cat("\n--- Step 3 (Weight QC outliers in daily) ---\n")
cat(sprintf("  national outlier_wt (daily sum): %s\n", metrics$step3_weight$nat_outlier_wt))
cat(sprintf("  legacy    outlier_wt (daily sum): %s\n", metrics$step3_weight$leg_outlier_wt))

cat("\n--- Step 4 (Feed QC outliers in daily) ---\n")
cat(sprintf("  national outlier_feed (daily sum): %s\n", metrics$step4_feed$nat_outlier_feed))
cat(sprintf("  legacy    outlier_feed (daily sum): %s\n", metrics$step4_feed$leg_outlier_feed))

cat("\n--- Step 5 (Daily aggregation) ---\n")
cat(sprintf("  national daily_rows: %d, animals: %d\n", metrics$step5_daily$nat_daily_rows, metrics$step5_daily$nat_animals))
cat(sprintf("  legacy    daily_rows: %d, animals: %d\n", metrics$step5_daily$leg_daily_rows, metrics$step5_daily$leg_animals))

cat("\n--- Step 7 (Phenotype) ---\n")
ph <- metrics$step7_phenotype
cat(sprintf("  national: %d animals, ADFI=%.1fg, ADG=%.1fg, FCR=%.3f (sd=%.3f), FCR_NA=%d\n",
  ph$nat_animals, ph$nat_mean_ADFI, ph$nat_mean_ADG, ph$nat_mean_FCR, ph$nat_sd_FCR, ph$nat_FCR_NA))
cat(sprintf("  legacy:   %d animals, ADFI=%.1fg, ADG=%.1fg, FCR=%.3f (sd=%.3f), FCR_NA=%d\n",
  ph$leg_animals, ph$leg_mean_ADFI, ph$leg_mean_ADG, ph$leg_mean_FCR, ph$leg_sd_FCR, ph$leg_FCR_NA))

cat("\n--- QC Flag Statistics ---\n")
cat(sprintf("  national: %d flag types\n", nrow(flag_nat)))
cat(sprintf("  legacy:   %d flag types\n", nrow(flag_leg)))

cat("\n=== All outputs written to:", out_root, "===\n")
cat("=== YANGXIANG QC Comparison Test Done ===\n")
