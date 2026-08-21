######### ZhenMeasure 南沙扬翔数据测试 #########
rm(list = ls())

# 检查依赖包
required_pkgs <- c("data.table", "MASS", "readxl", "lubridate", "zoo", "lme4", "imputeTS")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

# 确定项目根目录（V项目测试与开发）
script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
if (script_dir == "") {
    # Rscript 直接调用时，通过工作目录定位
    project_root <- getwd()
} else {
    # source() 调用时，脚本在 测试/ 下，上级就是项目根
    project_root <- normalizePath(file.path(script_dir, ".."))
}

# 加载 V1.1.0 代码
pkg_dir <- file.path(project_root, "项目本体/ZhenMeasure")
library(data.table)
pkgload::load_all(pkg_dir, quiet = TRUE)

cat("=== ZhenMeasure 南沙扬翔数据测试 ===\n")
cat("包版本:", as.character(packageVersion("ZhenMeasure")), "\n")
cat("测试时间:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# --- 路径设置 ---
demo_base_dir <- file.path(project_root, "测试/demo")
raw_data_dir <- file.path(demo_base_dir, "demo_input/YANGXIANG_扬翔/原始数据/南沙")
output_dir   <- file.path(demo_base_dir, "demo_output/YANGXIANG_扬翔/南沙_汇总分析")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

log_file <- file.path(output_dir, paste0("nansha_test_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))

cat("数据路径:", raw_data_dir, "\n")
cat("输出路径:", output_dir, "\n")
cat("日志文件:", log_file, "\n\n")

# --- 附加信息 ---
extra_info_dir <- file.path(demo_base_dir, "demo_input/YANGXIANG_扬翔/附加信息")
format_path <- list.files(extra_info_dir, pattern = "\\.json$", full.names = TRUE)[1]
if (is.na(format_path)) format_path <- NULL

cat("格式配置:", ifelse(is.null(format_path), "无", format_path), "\n")

# --- 配置（与 quickly_start 中 YANGXIANG 一致） ---
custom_config <- list(
    national_standard = list(
        test_weight_range = c(200, 20)
    )
)

# --- 运行主流程 ---
cat(">>> 开始运行 run_zhen_measure ...\n")
t0 <- Sys.time()

result <- run_zhen_measure(
    data_path            = raw_data_dir,
    data_type            = "YANGXIANG",
    format_path          = format_path,
    birth_info_path      = NULL,
    qc_method            = "national_standard",
    phenotype_method     = "report",
    stage_mode           = "weight",
    target_weight_stages = "YANGXIANG",
    target_age_stages    = NULL,
    target_date_stages   = NULL,
    output_dir           = output_dir,
    config               = custom_config,
    keep_ids             = NULL,
    growth_curve         = FALSE,
    growth_curve_test    = TRUE,
    log_file             = log_file
)

elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("\n>>> 运行完成，耗时 %.1f 秒\n\n", elapsed))

# 保存原始日汇总（含异常天，用于对比）
if (!is.null(result$raw_daily_records)) {
    fwrite(result$raw_daily_records, file.path(output_dir, "raw_daily_records.csv"))
    cat("原始日汇总已保存: raw_daily_records.csv\n\n")
}

# --- 结果分析 ---
cat("========== 结果摘要 ==========\n\n")

# 1. 基本统计
if (!is.null(result$raw_daily_records)) {
    raw_daily <- result$raw_daily_records
    cat(sprintf("原始日汇总: %d 行, %d 头个体\n",
                nrow(raw_daily), length(unique(raw_daily$animal_id))))
    cat(sprintf("  异常体重天数: %d\n", sum(raw_daily$day_has_outlier_wt, na.rm = TRUE)))
    cat(sprintf("  异常采食天数: %d\n", sum(raw_daily$day_has_outlier_feed, na.rm = TRUE)))
    cat(sprintf("  填充体重天数: %d\n", sum(raw_daily$day_is_imputed_wt, na.rm = TRUE)))
    cat(sprintf("  填充采食天数: %d\n", sum(raw_daily$day_is_imputed_feed, na.rm = TRUE)))
    cat("\n")
}

# 2. QC 汇总
if (!is.null(result$qc_summary)) {
    cat("QC 汇总:\n")
    print(result$qc_summary)
    cat("\n")
}

# 3. 表型数据
if (!is.null(result$phenotypes)) {
    pheno <- result$phenotypes
    cat(sprintf("表型数据: %d 行\n", nrow(pheno)))
    cat("\n")
}

# 4. ADFI 对比分析（原始 vs 质控后）
if (!is.null(result$raw_daily_records) && !is.null(result$daily_records)) {
    cat("========== ADFI 对比分析 ==========\n\n")

    raw_dt  <- as.data.table(result$raw_daily_records)
    filt_dt <- as.data.table(result$daily_records)

    # 原始 ADFI（全部记录的平均日采食量，按个体）
    raw_adfi <- raw_dt[, .(raw_adfi = mean(daily_feed_g, na.rm = TRUE),
                           raw_days = .N),
                       by = animal_id]

    # 质控后 ADFI（只保留用于表型计算的天）
    filt_adfi <- filt_dt[, .(qc_adfi  = mean(daily_feed_g, na.rm = TRUE),
                             qc_days  = .N),
                         by = animal_id]

    # 合并
    adfi_compare <- merge(raw_adfi, filt_adfi, by = "animal_id", all = TRUE)
    adfi_compare[, adfi_diff := qc_adfi - raw_adfi]
    adfi_compare[, adfi_pct  := adfi_diff / raw_adfi * 100]

    cat("个体 ADFI 统计 (g/天):\n")
    cat(sprintf("  原始平均 ADFI:   %.1f g/天\n", mean(adfi_compare$raw_adfi, na.rm = TRUE)))
    cat(sprintf("  质控后平均 ADFI: %.1f g/天\n", mean(adfi_compare$qc_adfi,  na.rm = TRUE)))
    cat(sprintf("  平均差异:        %.1f g/天\n", mean(adfi_compare$adfi_diff, na.rm = TRUE)))
    cat(sprintf("  平均变化率:      %.2f%%\n",   mean(adfi_compare$adfi_pct,  na.rm = TRUE)))
    cat("\n")

    cat("个体 ADFI 差异分布:\n")
    print(summary(adfi_compare$adfi_diff))
    cat("\n")

    # 保存对比表
    fwrite(adfi_compare, file.path(output_dir, "nansha_adfi_compare.csv"))
    cat("个体 ADFI 对比表已保存: nansha_adfi_compare.csv\n\n")
}

# 5. 表型中 ADFI 详情
if (!is.null(result$phenotypes)) {
    pheno <- as.data.table(result$phenotypes)
    adfi_cols <- grep("adfi|ADFI", names(pheno), value = TRUE)
    if (length(adfi_cols) > 0) {
        cat("========== 表型中 ADFI 列统计 ==========\n\n")
        for (col in adfi_cols) {
            vals <- pheno[[col]]
            if (is.numeric(vals)) {
                cat(sprintf("  %s: mean=%.1f, sd=%.1f, min=%.1f, max=%.1f, N=%d\n",
                            col, mean(vals, na.rm=TRUE), sd(vals, na.rm=TRUE),
                            min(vals, na.rm=TRUE), max(vals, na.rm=TRUE),
                            sum(!is.na(vals))))
            }
        }
        cat("\n")
    }
}

cat("========== 测试完成 ==========\n")
