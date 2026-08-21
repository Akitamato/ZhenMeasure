######### 对比脚本: 未质控 vs V1.1.0 质控后的 ADFI #########
rm(list = ls())
library(data.table)

project_root <- "D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发"
pkg_dir <- file.path(project_root, "项目本体", "ZhenMeasure")
devtools::load_all(pkg_dir)
library(ZhenMeasure)

demo_dir <- file.path(project_root, "测试", "demo", "demo_input")
data_path <- file.path(demo_dir, "YANGXIANG_扬翔", "原始数据")
format_path <- file.path(demo_dir, "YANGXIANG_扬翔", "附加信息", "YANGXIANG_data_format.json")

custom_config <- list(national_standard = list(test_weight_range = c(200, 20)))
cfg <- ZhenM_merge_config(custom_config, "national_standard")

cat("=== V1.1.0 完整流水线 ===\n")
result <- run_zhen_measure(
  data_path = data_path, data_type = "YANGXIANG", format_path = format_path,
  config = custom_config, phenotype_method = "report",
  stage_mode = "weight", target_weight_stages = "YANGXIANG"
)

daily <- as.data.table(result$daily_records)
pheno <- as.data.table(result$phenotypes)

cat("\n=================================================================\n")
cat("ADFI 对比 (单位: g/day)\n")
cat("=================================================================\n")

# 1. 基准 ADFI: 去重后但未做 Weight/Feed QC 排除 (公平基准)
# 原始数据含 7.7 万条重复记录, 直接用会导致日总和虚高。用 Overall QC (去重) 后的数据作基准。
cat("\n【基准】去重后 (未做 QC 排除) 直接日聚合...\n")
base_records <- ZhenM_read_data(data_path, "YANGXIANG", format_path, NULL)
base_qc <- ZhenM_qc_overall(base_records, config = cfg)
base_records <- base_qc$records  # 已去重 + 完整性 + 连续性 + 逻辑检查
feed_col <- if ("feed_g" %in% names(base_records)) "feed_g" else "Feed_intake"
raw_daily <- base_records[, .(
  daily_feed_raw = sum(get(feed_col), na.rm = TRUE),
  n_records = .N
), by = c("animal_id", "record_date")]

# 基准 ADFI (每头猪: 总采食 / 天数)
raw_animals <- raw_daily[, .(
  total_feed_g = sum(daily_feed_raw, na.rm = TRUE),
  n_days = .N
), by = animal_id]
raw_animals[, ADFI_raw := total_feed_g / n_days]

# 2. V1.1.0 质控后 ADFI (从表型结果)
# 表型是分阶段的 (30-100/115/120), 这里用未分阶段的 report 模式取每头猪全期 ADFI
pheno_full <- ZhenM_calc_phenotypes(daily, "report", cfg)
pheno_full <- as.data.table(pheno_full)

# 3. 合并对比
merged <- merge(raw_animals[, .(animal_id, ADFI_raw)],
                pheno_full[, .(animal_id, ADFI_g, total_feed_g, test_days)],
                by = "animal_id", all.x = TRUE)

cat(sprintf("  对比个体数: %d\n", nrow(merged)))

# 均值对比
mean_raw <- mean(merged$ADFI_raw, na.rm = TRUE)
mean_qc <- mean(merged$ADFI_g, na.rm = TRUE)
cat(sprintf("\n  未质控 ADFI 均值: %.1f g/day\n", mean_raw))
cat(sprintf("  质控后 ADFI 均值: %.1f g/day (V1.1.0)\n", mean_qc))
cat(sprintf("  差值: %.1f g/day (%.2f%%)\n", mean_qc - mean_raw,
            (mean_qc - mean_raw) / mean_raw * 100))

cat("\n  按个体 ADFI 变化分布:\n")
merged[, adfi_diff := ADFI_g - ADFI_raw]
merged[, adfi_diff_pct := (ADFI_g - ADFI_raw) / ADFI_raw * 100]
cat(sprintf("    个体数增多(质控后ADFI更高): %d\n", sum(merged$adfi_diff > 0, na.rm = TRUE)))
cat(sprintf("    个体数降低(质控后ADFI更低): %d\n", sum(merged$adfi_diff < 0, na.rm = TRUE)))
cat(sprintf("    变化幅度 P10/P50/P90: %.1f / %.1f / %.1f %%\n",
            quantile(merged$adfi_diff_pct, 0.1, na.rm = TRUE),
            quantile(merged$adfi_diff_pct, 0.5, na.rm = TRUE),
            quantile(merged$adfi_diff_pct, 0.9, na.rm = TRUE)))

cat("\n=================================================================\n")
cat("采食量构成分析 (V1.1.0 中间环节)\n")
cat("=================================================================\n")

# 插补情况
cat(sprintf("\n  daily_feed_g NA 数: %d\n", sum(is.na(daily$daily_feed_g))))
cat(sprintf("  插补标记 is_imputed_feed: %d 天\n", sum(daily$is_imputed_feed, na.rm = TRUE)))
cat(sprintf("  flag_daily_feed_over_limit: %d 天 (6kg 生理上限)\n",
            sum(daily$flag_daily_feed_over_limit, na.rm = TRUE)))

# QC 排除
qc_records <- ZhenM_qc_feed_standard(ZhenM_qc_weight_standard(
  ZhenM_qc_overall(ZhenM_read_data(data_path, "YANGXIANG", format_path, NULL), config = cfg)$records,
  "national_standard", cfg), "national_standard", cfg)
total_feed_all <- sum(qc_records[[feed_col]], na.rm = TRUE)
feed_removed <- sum(qc_records$is_outlier_feed * qc_records[[feed_col]], na.rm = TRUE)
cat(sprintf("\n  Feed QC 排除采食量: %.0f g (%.2f%%)\n",
            feed_removed, feed_removed / total_feed_all * 100))

# LMM 校正
cat(sprintf("  LMM 校正记录数: (见运行日志 LMM Feed Correction)\n"))

cat("\n=================================================================\n")
cat("前 10 头猪的 ADFI 对比明细 (按降低幅度排序)\n")
cat("=================================================================\n")
print(merged[order(adfi_diff_pct, decreasing = TRUE)][1:10,
      .(animal_id, ADFI_raw, ADFI_g, adfi_diff_pct)], digits = 2)

fwrite(merged, file.path(project_root, "测试", "adfi_comparison_result.csv"))
cat("\n  明细已保存: 测试/adfi_comparison_result.csv\n")
