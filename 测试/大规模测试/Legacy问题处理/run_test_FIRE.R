###############################################################################
# FIRE QC 方法对比测试: national_standard vs legacy
# 逐步骤运行管线，收集每步 QC flag 统计与表型指标
# 输出到 per_source/ 目录
###############################################################################

rm(list = ls())
options(scipen = 999)

# ── 加载包 ─────────────────────────────────────────────────────────────────────
install.packages(
  file.path("D:", "My_project", "Cooperation_Project", "横向",
    "长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)",
    "扬翔群体饲喂仪器数据处理脚本开发",
    "V项目测试与开发", "项目本体", "ZhenMeasure"),
  repos = NULL, type = "source"
)
library(ZhenMeasure)
library(data.table)
library(jsonlite)

# ── 路径定义 ──────────────────────────────────────────────────────────────────
base_path <- file.path("D:", "My_project", "Cooperation_Project", "横向",
  "长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)",
  "扬翔群体饲喂仪器数据处理脚本开发",
  "V项目测试与开发")

data_path    <- file.path(base_path, "测试", "demo", "demo_standard", "Farm_A_FIRE")
format_path  <- file.path(data_path, "附加信息", "FIRE_data_format.json")
out_root     <- file.path(base_path, "测试", "大规模测试", "Legacy问题处理")

per_source_dir  <- file.path(out_root, "per_source")
pheno_comp_dir  <- file.path(out_root, "phenotypes_compare")

dir.create(per_source_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pheno_comp_dir, recursive = TRUE, showWarnings = FALSE)

# ── 逐步骤运行函数 ────────────────────────────────────────────────────────────
run_fire_step_by_step <- function(qc_method, override_config = NULL) {
  cat(sprintf("\n====== [FIRE] qc_method=%s ======\n", qc_method))

  cfg <- ZhenMeasure:::ZhenM_merge_config(override_config, qc_method)

  result <- list(metrics = list(), error_log = character())

  # Step 1: Read
  cat("Step 1: Reading data...\n")
  std <- tryCatch(
    ZhenM_read_data(data_path, "FIRE", format_path),
    error = function(e) {
      result$error_log <<- c(result$error_log, paste0("Step1 READ ERROR: ", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(std)) return(result)
  result$metrics$step1_read <- list(n_records = nrow(std), n_animals = uniqueN(std$ID))

  # Step 2: Overall QC
  cat("Step 2: Overall QC...\n")
  qc_result <- ZhenM_qc_overall(std, config = cfg, logger = NULL)
  qc_data <- qc_result$records
  result$metrics$step2_overall <- list(
    n_after = nrow(qc_data),
    n_animals_after = uniqueN(qc_data$animal_id),
    removed = result$metrics$step1_read$n_records - nrow(qc_data)
  )

  # Step 3: Weight QC
  cat("Step 3: Weight QC...\n")
  qc_data_wt <- ZhenMeasure:::ZhenM_qc_weight_standard(qc_data, qc_method, cfg, logger = NULL)
  wt_flags <- grep("^flag_.*WT|^flag_weight|^flag_daily_weight|^flag_growth|^is_outlier", names(qc_data_wt), value = TRUE)
  wt_stats <- list(n_after = nrow(qc_data_wt), n_animals_after = uniqueN(qc_data_wt$animal_id))
  for (f in wt_flags) {
    wt_stats[[f]] <- sum(qc_data_wt[[f]] == TRUE, na.rm = TRUE)
  }
  wt_stats$is_outlier_wt <- sum(qc_data_wt$is_outlier_wt == TRUE, na.rm = TRUE)
  wt_stats$outlier_pct <- round(wt_stats$is_outlier_wt / nrow(qc_data_wt) * 100, 2)
  result$metrics$step3_weight_qc <- wt_stats
  result$qc_data_wt <- qc_data_wt

  # Step 4: Feed QC
  cat("Step 4: Feed QC...\n")
  qc_data_fd <- ZhenMeasure:::ZhenM_qc_feed_standard(qc_data_wt, qc_method, cfg, logger = NULL)
  fd_flags <- grep("^flag_feed|^flag_duration|^flag_speed|^flag_STL|^flag_percentile|^is_outlier", names(qc_data_fd), value = TRUE)
  fd_stats <- list(n_after = nrow(qc_data_fd), n_animals_after = uniqueN(qc_data_fd$animal_id))
  for (f in fd_flags) {
    fd_stats[[f]] <- sum(qc_data_fd[[f]] == TRUE, na.rm = TRUE)
  }
  fd_stats$is_outlier_feed <- sum(qc_data_fd$is_outlier_feed == TRUE, na.rm = TRUE)
  fd_stats$outlier_pct <- round(fd_stats$is_outlier_feed / nrow(qc_data_fd) * 100, 2)
  result$metrics$step4_feed_qc <- fd_stats
  result$qc_data_fd <- qc_data_fd

  # Step 5: Daily aggregation
  cat("Step 5: Daily aggregation...\n")
  daily <- ZhenM_standard_to_daily_filtered(qc_data_fd)
  result$metrics$step5_daily <- list(n_daily = nrow(daily), n_animals = uniqueN(daily$animal_id))

  # Step 5.5: Growth curve check
  cat("Step 5.5: Growth curve check...\n")
  if (!"is_imputed_wt" %in% names(daily)) daily[, is_imputed_wt := FALSE]
  if (!"is_imputed_feed" %in% names(daily)) daily[, is_imputed_feed := FALSE]

  ids_to_check <- unique(daily$animal_id)
  animals_to_delete <- character()
  min_r2 <- cfg[[qc_method]]$growth_curve_r2_min %||% cfg$national_standard$growth_curve_r2_min

  for (id in ids_to_check) {
    sub_daily <- daily[animal_id == id]
    valid_pts <- sub_daily[!is.na(daily_weight_g)]
    if (nrow(valid_pts) < 10) { animals_to_delete <- c(animals_to_delete, id); next }
    x <- as.numeric(valid_pts$record_date - min(valid_pts$record_date))
    y <- valid_pts$daily_weight_g
    fit_res <- tryCatch(ZhenMeasure:::.check_growth_fit(y, x, min_r2 = min_r2),
                        error = function(e) list(pass = FALSE))
    pass_val <- isTRUE(fit_res$pass)
    if (!pass_val) {
      animals_to_delete <- c(animals_to_delete, id)
    }
  }
  daily <- daily[!animal_id %in% animals_to_delete]
  result$metrics$step5_5_growth <- list(
    deleted_animals = length(animals_to_delete),
    remaining_animals = uniqueN(daily$animal_id)
  )

  # Step 6: Imputation
  cat("Step 6: Imputation...\n")
  if (qc_method == "national_standard") {
    daily <- ZhenM_impute_national(daily, cfg)
  } else {
    daily <- ZhenM_impute_data(daily, "legacy", cfg)
  }
  result$metrics$step6_impute <- list(
    imputed_feed = sum(daily$is_imputed_feed == TRUE, na.rm = TRUE),
    imputed_wt = sum(daily$is_imputed_wt == TRUE, na.rm = TRUE),
    n_daily = nrow(daily),
    n_animals = uniqueN(daily$animal_id)
  )

  # Step 7: Phenotypes (research method)
  cat("Step 7: Phenotype calculation (research method)...\n")
  phenos <- ZhenM_calc_phenotypes(daily, phenotype_method = "research", config = cfg)
  result$metrics$step7_phenotype <- list(
    n_animals = nrow(phenos),
    # research 方法使用 _lm 后缀列名
    mean_ADFI_g = round(mean(phenos$ADFI_g_lm, na.rm = TRUE), 1),
    mean_ADG_g  = round(mean(phenos$ADG_g_lm, na.rm = TRUE), 1),
    mean_FCR    = round(mean(phenos$FCR_lm, na.rm = TRUE), 3),
    median_FCR  = round(median(phenos$FCR_lm, na.rm = TRUE), 3),
    sd_FCR      = round(sd(phenos$FCR_lm, na.rm = TRUE), 3),
    FCR_NA_count = sum(is.na(phenos$FCR_lm))
  )

  result$daily <- daily
  result$phenos <- phenos
  result$qc_data <- qc_data_fd

  cat(sprintf("[%s] completed. phenotypes: %d, daily_records: %d\n",
              qc_method, nrow(phenos), nrow(daily)))

  result
}

# ── 配置 ──────────────────────────────────────────────────────────────────────
legacy_cfg <- list(legacy = list(
  feed_intake_range   = c(0, 6000),
  weight_sd_threshold = 4,
  rlm_weight_thresh   = 0.3,
  min_obs_for_wt      = 40,
  growth_curve_r2_min = 0.95
))

# ── 步骤 B: 运行 national_standard ────────────────────────────────────────────
result_nat <- run_fire_step_by_step("national_standard")

# ── 步骤 C: 运行 legacy（放宽参数）────────────────────────────────────────────
result_leg <- run_fire_step_by_step("legacy", override_config = legacy_cfg)

# ── 步骤 D: 收集对比指标 ──────────────────────────────────────────────────────
cat("\n====== Collecting comparison metrics ======\n")

safe_val <- function(x) if (is.null(x) || length(x) == 0) NA else x

metrics <- list(
  source = "FIRE",
  step2_overall = list(
    nat_records = safe_val(result_nat$metrics$step2_overall$n_after),
    leg_records = safe_val(result_leg$metrics$step2_overall$n_after)
  ),
  step3_weight = list(
    nat_outlier_wt = safe_val(result_nat$metrics$step3_weight_qc$is_outlier_wt),
    leg_outlier_wt = safe_val(result_leg$metrics$step3_weight_qc$is_outlier_wt)
  ),
  step4_feed = list(
    nat_outlier_feed = safe_val(result_nat$metrics$step4_feed_qc$is_outlier_feed),
    leg_outlier_feed = safe_val(result_leg$metrics$step4_feed_qc$is_outlier_feed)
  ),
  step5_daily = list(
    nat_daily_rows = safe_val(result_nat$metrics$step5_daily$n_daily),
    leg_daily_rows = safe_val(result_leg$metrics$step5_daily$n_daily)
  ),
  step7_phenotype = list(
    nat_animals   = safe_val(result_nat$metrics$step7_phenotype$n_animals),
    leg_animals   = safe_val(result_leg$metrics$step7_phenotype$n_animals),
    nat_mean_FCR  = safe_val(result_nat$metrics$step7_phenotype$mean_FCR),
    leg_mean_FCR  = safe_val(result_leg$metrics$step7_phenotype$mean_FCR),
    nat_sd_FCR    = safe_val(result_nat$metrics$step7_phenotype$sd_FCR),
    leg_sd_FCR    = safe_val(result_leg$metrics$step7_phenotype$sd_FCR),
    nat_mean_ADFI = safe_val(result_nat$metrics$step7_phenotype$mean_ADFI_g),
    leg_mean_ADFI = safe_val(result_leg$metrics$step7_phenotype$mean_ADFI_g),
    nat_mean_ADG  = safe_val(result_nat$metrics$step7_phenotype$mean_ADG_g),
    leg_mean_ADG  = safe_val(result_leg$metrics$step7_phenotype$mean_ADG_g),
    nat_FCR_NA    = safe_val(result_nat$metrics$step7_phenotype$FCR_NA_count),
    leg_FCR_NA    = safe_val(result_leg$metrics$step7_phenotype$FCR_NA_count)
  ),
  errors = c(result_nat$error_log, result_leg$error_log)
)
if (length(metrics$errors) == 0) metrics$errors <- "none"

# 写入 JSON
json_path <- file.path(per_source_dir, "FIRE_metrics.json")
write_json(metrics, json_path, auto_unbox = TRUE, pretty = TRUE)
cat("Metrics saved to:", json_path, "\n")

# 保存 phenotype 对比 CSV
if (!is.null(result_nat$phenos)) {
  fwrite(result_nat$phenos, file.path(pheno_comp_dir, "FIRE_pheno_national.csv"))
  cat("Saved: FIRE_pheno_national.csv\n")
}
if (!is.null(result_leg$phenos)) {
  fwrite(result_leg$phenos, file.path(pheno_comp_dir, "FIRE_pheno_legacy.csv"))
  cat("Saved: FIRE_pheno_legacy.csv\n")
}

# 保存 QC 数据（用于后续分析）
if (!is.null(result_nat$qc_data)) {
  fwrite(result_nat$qc_data, file.path(per_source_dir, "FIRE_qcdata_national.csv"))
  cat("Saved: FIRE_qcdata_national.csv\n")
}
if (!is.null(result_leg$qc_data)) {
  fwrite(result_leg$qc_data, file.path(per_source_dir, "FIRE_qcdata_legacy.csv"))
  cat("Saved: FIRE_qcdata_legacy.csv\n")
}

# ── 步骤 E: 收集 QC flag 统计 ─────────────────────────────────────────────────
cat("\n====== QC flag statistics ======\n")

collect_flag_stats <- function(qc_data, method_name) {
  if (is.null(qc_data)) return(data.table(method = method_name, flag = "NO_DATA", count = 0))

  flag_cols <- grep("^flag_|^is_outlier", names(qc_data), value = TRUE)
  if (length(flag_cols) == 0) {
    return(data.table(method = method_name, flag = "NO_FLAGS_FOUND", count = 0))
  }

  rows <- lapply(flag_cols, function(fc) {
    data.table(
      method = method_name,
      flag   = fc,
      count  = sum(qc_data[[fc]] == TRUE, na.rm = TRUE)
    )
  })
  rbindlist(rows)
}

flag_nat <- collect_flag_stats(result_nat$qc_data, "national_standard")
flag_leg <- collect_flag_stats(result_leg$qc_data, "legacy")
flag_combined <- rbind(flag_nat, flag_leg)

flag_csv_path <- file.path(per_source_dir, "FIRE_qc_flag_stats.csv")
fwrite(flag_combined, flag_csv_path)
cat("QC flag stats saved to:", flag_csv_path, "\n")

# 打印 flag 统计
cat("\n--- national_standard QC flags ---\n")
print(flag_nat, nrows = 50)
cat("\n--- legacy QC flags ---\n")
print(flag_leg, nrows = 50)

# ── 构建逐步骤对比 diff 表 ────────────────────────────────────────────────────
cat("\n====== Building step-by-step diff table ======\n")

steps <- c("step1_read", "step2_overall", "step3_weight_qc", "step4_feed_qc",
           "step5_daily", "step5_5_growth", "step6_impute", "step7_phenotype")

rows <- list()
for (step in steps) {
  nat_m <- result_nat$metrics[[step]]
  leg_m <- result_leg$metrics[[step]]
  all_keys <- unique(c(names(nat_m), names(leg_m)))
  for (k in all_keys) {
    rows <- c(rows, list(data.table(
      source = "FIRE",
      step = step,
      metric = k,
      national_standard = ifelse(is.null(nat_m[[k]]), NA, as.character(nat_m[[k]])),
      legacy = ifelse(is.null(leg_m[[k]]), NA, as.character(leg_m[[k]]))
    )))
  }
}
diff_dt <- rbindlist(rows)

# 计算差值（数值型指标）
diff_dt[, nat_num := suppressWarnings(as.numeric(national_standard))]
diff_dt[, leg_num := suppressWarnings(as.numeric(legacy))]
diff_dt[!is.na(nat_num) & !is.na(leg_num), diff := leg_num - nat_num]
diff_dt[!is.na(nat_num) & !is.na(leg_num), diff_pct := round((leg_num - nat_num) / ifelse(nat_num == 0, 1, nat_num) * 100, 2)]

diff_csv_path <- file.path(per_source_dir, "FIRE_diff.csv")
fwrite(diff_dt, diff_csv_path)
cat("Diff table saved to:", diff_csv_path, "\n")

# ── 打印最终指标 ───────────────────────────────────────────────────────────────
cat("\n\n========== FIRE Comparison Metrics ==========\n")
cat(jsonlite::toJSON(metrics, auto_unbox = TRUE, pretty = TRUE), "\n")

cat("\n========== Step-by-step Diff ==========\n")
print(diff_dt, nrows = 100)

if (!all(metrics$errors == "none")) {
  cat("\n========== ERRORS ==========\n")
  for (e in metrics$errors) cat(e, "\n")
}

cat("\n=== FIRE test complete ===\n")
