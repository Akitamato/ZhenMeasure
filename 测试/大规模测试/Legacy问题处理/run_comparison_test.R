###############################################################################
# ZhenMeasure 大规模对比测试：national_standard vs legacy
# 每步骤详细对比 QC flag、异常率、表型结果
# 输出：CSV 汇总 + 可视化图表
###############################################################################

rm(list = ls())
library(ZhenMeasure)
library(data.table)

# ── 输出目录 ──────────────────────────────────────────────────────────────────
out_root <- file.path("D:", "My_project", "Cooperation_Project", "横向",
  "长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)",
  "扬翔群体饲喂仪器数据处理脚本开发",
  "V项目测试与开发", "测试", "大规模测试", "Legacy问题处理")

dir.create(file.path(out_root, "per_source"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_root, "phenotypes_compare"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_root, "plots"), recursive = TRUE, showWarnings = FALSE)

# ── 数据源定义 ────────────────────────────────────────────────────────────────
demo_base <- file.path("D:", "My_project", "Cooperation_Project", "横向",
  "长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)",
  "扬翔群体饲喂仪器数据处理脚本开发",
  "V项目测试与开发", "测试", "demo", "demo_standard")

sources <- list(
  list(name = "YANGXIANG", data_type = "YANGXIANG",
       data_path = file.path(demo_base, "Farm_C_YANGXIANG"),
       format_path = file.path(demo_base, "Farm_C_YANGXIANG", "Data_format", "YANGXIANG_data_format.json")),
  list(name = "FIRE", data_type = "FIRE",
       data_path = file.path(demo_base, "Farm_A_FIRE"),
       format_path = file.path(demo_base, "Farm_A_FIRE", "附加信息", "FIRE_data_format.json")),
  list(name = "Nedap", data_type = "NEDAP",
       data_path = file.path(demo_base, "Farm_B_Nedap"),
       format_path = file.path(demo_base, "Farm_B_Nedap", "Data_format", "Nedap_data_format.json"))
)

# ── 逐步骤运行函数 ────────────────────────────────────────────────────────────
run_step_by_step <- function(src, qc_method, override_config = NULL) {
  cat(sprintf("\n====== [%s] qc_method=%s ======\n", src$name, qc_method))

  cfg <- ZhenMeasure:::ZhenM_merge_config(override_config, qc_method)
  loggers <- list(
    log_info   = function(msg) cat("  [INFO]", msg, "\n"),
    log_detail = function(msg) cat("  [DETAIL]", msg, "\n"),
    log_subsection = function(msg) cat("\n  ---", msg, "---\n")
  )

  metrics <- list()

  # Step 1: Read
  cat("Step 1: Reading data...\n")
  std <- ZhenM_read_data(src$data_path, src$data_type, src$format_path)
  metrics$step1_read <- list(n_records = nrow(std), n_animals = uniqueN(std$ID))

  # Step 2: Overall QC
  cat("Step 2: Overall QC...\n")
  qc_result <- ZhenM_qc_overall(std, config = cfg, logger = NULL)
  qc_data <- qc_result$records
  metrics$step2_overall <- list(
    n_after = nrow(qc_data),
    n_animals_after = uniqueN(qc_data$animal_id),
    removed = metrics$step1_read$n_records - nrow(qc_data)
  )

  # Step 3: Weight QC
  cat("Step 3: Weight QC...\n")
  qc_data_wt <- ZhenMeasure:::ZhenM_qc_weight_standard(qc_data, qc_method, cfg, logger = NULL)
  wt_flags <- grep("^flag_.*WT|^flag_weight|^flag_daily_weight|^flag_growth", names(qc_data_wt), value = TRUE)
  wt_stats <- list(n_after = nrow(qc_data_wt), n_animals_after = uniqueN(qc_data_wt$animal_id))
  for (f in wt_flags) {
    wt_stats[[f]] <- sum(qc_data_wt[[f]] == TRUE, na.rm = TRUE)
  }
  wt_stats$is_outlier_wt <- sum(qc_data_wt$is_outlier_wt == TRUE, na.rm = TRUE)
  wt_stats$outlier_pct <- round(wt_stats$is_outlier_wt / nrow(qc_data_wt) * 100, 2)
  metrics$step3_weight_qc <- wt_stats

  # Step 4: Feed QC
  cat("Step 4: Feed QC...\n")
  qc_data_fd <- ZhenMeasure:::ZhenM_qc_feed_standard(qc_data_wt, qc_method, cfg, logger = NULL)
  fd_flags <- grep("^flag_feed|^flag_duration|^flag_speed|^flag_STL|^flag_percentile", names(qc_data_fd), value = TRUE)
  fd_stats <- list(n_after = nrow(qc_data_fd), n_animals_after = uniqueN(qc_data_fd$animal_id))
  for (f in fd_flags) {
    fd_stats[[f]] <- sum(qc_data_fd[[f]] == TRUE, na.rm = TRUE)
  }
  fd_stats$is_outlier_feed <- sum(qc_data_fd$is_outlier_feed == TRUE, na.rm = TRUE)
  fd_stats$outlier_pct <- round(fd_stats$is_outlier_feed / nrow(qc_data_fd) * 100, 2)
  metrics$step4_feed_qc <- fd_stats

  # Step 5: Daily aggregation
  cat("Step 5: Daily aggregation...\n")
  daily <- ZhenM_standard_to_daily_filtered(qc_data_fd)
  metrics$step5_daily <- list(n_daily = nrow(daily), n_animals = uniqueN(daily$animal_id))

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
  metrics$step5_5_growth <- list(deleted_animals = length(animals_to_delete),
                                  remaining_animals = uniqueN(daily$animal_id))

  # Step 6: Imputation
  cat("Step 6: Imputation...\n")
  if (qc_method == "national_standard") {
    daily <- ZhenM_impute_national(daily, cfg)
  } else {
    daily <- ZhenM_impute_data(daily, "legacy", cfg)
  }
  metrics$step6_impute <- list(
    imputed_feed = sum(daily$is_imputed_feed == TRUE, na.rm = TRUE),
    imputed_wt = sum(daily$is_imputed_wt == TRUE, na.rm = TRUE),
    n_daily = nrow(daily),
    n_animals = uniqueN(daily$animal_id)
  )

  # Step 7: Phenotypes (统一用 report，不分阶段)
  cat("Step 7: Phenotype calculation...\n")
  phenos <- ZhenM_calc_phenotypes(daily, phenotype_method = "report", config = cfg)
  metrics$step7_phenotype <- list(
    n_animals = nrow(phenos),
    mean_ADFI_g = round(mean(phenos$ADFI_g, na.rm = TRUE), 1),
    mean_ADG_g = round(mean(phenos$ADG_g, na.rm = TRUE), 1),
    mean_FCR = round(mean(phenos$FCR, na.rm = TRUE), 3),
    median_FCR = round(median(phenos$FCR, na.rm = TRUE), 3),
    sd_FCR = round(sd(phenos$FCR, na.rm = TRUE), 3),
    FCR_NA_count = sum(is.na(phenos$FCR))
  )

  list(metrics = metrics, daily = daily, phenos = phenos, qc_data = qc_data_fd)
}

# ── 主循环 ────────────────────────────────────────────────────────────────────
all_summaries <- list()

for (src in sources) {
  if (!dir.exists(src$data_path)) {
    cat(sprintf("SKIP: %s (path not found: %s)\n", src$name, src$data_path))
    next
  }

  result_nat <- run_step_by_step(src, "national_standard")
  result_leg <- run_step_by_step(src, "legacy",
    override_config = list(legacy = list(
      feed_intake_range = c(0, 6000),
      # 放宽体重QC参数，避免过度过滤导致后续生长曲线检查全军覆没
      weight_sd_threshold = 4,        # 原值3，放宽SD法
      rlm_weight_thresh = 0.3,        # 原值0.5，放宽RLM法
      min_obs_for_wt = 40,            # 原值60，允许更多个体参与Gompertz拟合
      # 降低生长曲线R²阈值（legacy数据经QC后连续性较差）
      growth_curve_r2_min = 0.95      # 原值0.99，对legacy方法放宽
    )))

  # 保存表型对比
  fwrite(result_nat$phenos, file.path(out_root, "phenotypes_compare",
    paste0(src$name, "_pheno_national.csv")))
  fwrite(result_leg$phenos, file.path(out_root, "phenotypes_compare",
    paste0(src$name, "_pheno_legacy.csv")))

  # 构建逐步骤对比表
  steps <- c("step1_read", "step2_overall", "step3_weight_qc", "step4_feed_qc",
             "step5_daily", "step5_5_growth", "step6_impute", "step7_phenotype")

  rows <- list()
  for (step in steps) {
    nat_m <- result_nat$metrics[[step]]
    leg_m <- result_leg$metrics[[step]]
    all_keys <- unique(c(names(nat_m), names(leg_m)))
    for (k in all_keys) {
      rows <- c(rows, list(data.table(
        source = src$name,
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

  fwrite(diff_dt, file.path(out_root, "per_source", paste0(src$name, "_diff.csv")))
  all_summaries[[src$name]] <- diff_dt

  # 保存 QC 数据（用于后续分析）
  fwrite(result_nat$qc_data, file.path(out_root, "per_source", paste0(src$name, "_qcdata_national.csv")))
  fwrite(result_leg$qc_data, file.path(out_root, "per_source", paste0(src$name, "_qcdata_legacy.csv")))
}

# ── 汇总对比表 ────────────────────────────────────────────────────────────────
summary_dt <- rbindlist(all_summaries)
fwrite(summary_dt, file.path(out_root, "compare_summary.csv"))
cat("\n\nSummary saved to:", file.path(out_root, "compare_summary.csv"), "\n")

# ── 可视化图表 ────────────────────────────────────────────────────────────────
cat("Generating plots...\n")

plot_comparison_bar <- function(dt, step_name, metric_name, title, ylab, filename) {
  sub <- dt[step == step_name & metric == metric_name]
  if (nrow(sub) == 0) return()

  vals_nat <- as.numeric(sub$national_standard)
  vals_leg <- as.numeric(sub$legacy)
  names_vals <- sub$source

  if (all(is.na(vals_nat)) && all(is.na(vals_leg))) return()

  png(file.path(out_root, "plots", filename), width = 800, height = 500, res = 100)
  par(mar = c(5, 4, 4, 2))
  mat <- rbind(vals_nat, vals_leg)
  colnames(mat) <- names_vals
  bp <- barplot(mat, beside = TRUE, col = c("#2196F3", "#FF9800"),
    main = title, ylab = ylab, las = 1, legend.text = c("national_standard", "legacy"),
    args.legend = list(x = "topright", bty = "n"))
  if (ncol(bp) == length(vals_nat)) {
    text(bp[1, ], vals_nat, labels = vals_nat, pos = 3, cex = 0.8, col = "#1565C0")
    text(bp[2, ], vals_leg, labels = vals_leg, pos = 3, cex = 0.8, col = "#E65100")
  }
  dev.off()
  cat("  Saved:", filename, "\n")
}

plot_flag_comparison <- function(dt, source_name, step_name, filename) {
  sub <- dt[source == source_name & step == step_name & !metric %in% c("n_after", "n_animals_after", "outlier_pct")]
  sub <- sub[!is.na(nat_num) & !is.na(leg_num)]
  if (nrow(sub) == 0) return()

  png(file.path(out_root, "plots", filename), width = 900, height = 500, res = 100)
  par(mar = c(8, 4, 4, 2))
  mat <- rbind(sub$nat_num, sub$leg_num)
  colnames(mat) <- sub$metric
  bp <- barplot(mat, beside = TRUE, col = c("#2196F3", "#FF9800"),
    main = paste0(source_name, " - ", step_name, " Flag Comparison"),
    ylab = "Count", las = 2, cex.names = 0.7,
    legend.text = c("national_standard", "legacy"),
    args.legend = list(x = "topright", bty = "n"))
  dev.off()
  cat("  Saved:", filename, "\n")
}

plot_phenotype_boxplot <- function(source_name) {
  nat_file <- file.path(out_root, "phenotypes_compare", paste0(source_name, "_pheno_national.csv"))
  leg_file <- file.path(out_root, "phenotypes_compare", paste0(source_name, "_pheno_legacy.csv"))
  if (!file.exists(nat_file) || !file.exists(leg_file)) return()

  nat <- fread(nat_file)
  leg <- fread(leg_file)

  for (var in c("ADFI_g", "ADG_g", "FCR")) {
    nat_vals <- nat[[var]]
    leg_vals <- leg[[var]]
    if (all(is.na(nat_vals)) && all(is.na(leg_vals))) next

    png(file.path(out_root, "plots", paste0(source_name, "_pheno_", var, ".png")),
      width = 700, height = 500, res = 100)
    par(mar = c(5, 4, 4, 2))
    boxplot(nat_vals, leg_vals,
      names = c("national_standard", "legacy"),
      col = c("#2196F3", "#FF9800"),
      main = paste0(source_name, " - ", var, " Distribution"),
      ylab = var)
    mtext(paste0("nat: n=", sum(!is.na(nat_vals)), " | leg: n=", sum(!is.na(leg_vals))),
      side = 1, line = 3, cex = 0.8, col = "gray40")
    dev.off()
    cat("  Saved:", paste0(source_name, "_pheno_", var, ".png"), "\n")
  }
}

# 生成图表
for (src_name in names(all_summaries)) {
  dt <- all_summaries[[src_name]]

  plot_flag_comparison(dt, src_name, "step3_weight_qc",
    paste0(src_name, "_weight_flags.png"))
  plot_flag_comparison(dt, src_name, "step4_feed_qc",
    paste0(src_name, "_feed_flags.png"))
  plot_phenotype_boxplot(src_name)
}

# 全局对比图
plot_comparison_bar(summary_dt, "step6_impute", "imputed_feed",
  "Feed Imputation Count", "Records", "global_impute_feed.png")
plot_comparison_bar(summary_dt, "step6_impute", "imputed_wt",
  "Weight Imputation Count", "Records", "global_impute_wt.png")
plot_comparison_bar(summary_dt, "step7_phenotype", "mean_FCR",
  "Mean FCR Comparison", "FCR", "global_mean_FCR.png")
plot_comparison_bar(summary_dt, "step7_phenotype", "n_animals",
  "Final Animal Count", "Animals", "global_final_animals.png")

cat("\n=== All done! Results in:", out_root, "===\n")
