######### 端到端验证：R² 0.95 + test_days 过滤 + FCR 软标记 #########
rm(list = ls())

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
project_root <- if (script_dir == "") getwd() else normalizePath(file.path(script_dir, ".."))

library(data.table)
pkg_dir <- file.path(project_root, "项目本体/ZhenMeasure")
pkgload::load_all(pkg_dir, quiet = TRUE, export_all = TRUE)

data_path   <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/原始数据/南沙")
format_path <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/附加信息/YANGXIANG_data_format.json")

cat(">>> 运行南沙 pipeline（默认 R²=0.95 + test_days>=20 + FCR 软标记）...\n")
t0 <- Sys.time()
result <- run_zhen_measure(
    data_path            = data_path,
    data_type            = "YANGXIANG",
    format_path          = format_path,
    qc_method            = "national_standard",
    phenotype_method     = "report",
    stage_mode           = "weight",
    target_weight_stages = "YANGXIANG",
    output_dir           = NULL,
    config               = list(national_standard = list(test_weight_range = c(200, 20))),
    growth_curve         = FALSE,
    growth_curve_test    = FALSE
)
cat(sprintf("运行完成，耗时 %.1f 秒\n\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

pheno <- as.data.table(result$phenotypes)

cat("========== 修复后南沙表型验证 ==========\n")
cat(sprintf("表型动物数: %d（基线 0.99 为 324）\n", uniqueN(pheno$animal_id)))
cat(sprintf("表型行数: %d\n", nrow(pheno)))
cat(sprintf("test_days 最小值: %.0f（应 >= 20）\n", min(pheno$test_days, na.rm = TRUE)))
cat(sprintf("test_days < 20 行数: %d（应为 0）\n", nrow(pheno[test_days < 20])))
cat(sprintf("FCR mean / sd: %.3f / %.3f（0.95 无过滤为 3.07/4.28，0.99 基线为 2.47/0.50）\n",
            mean(pheno$FCR, na.rm = TRUE), sd(pheno$FCR, na.rm = TRUE)))
cat(sprintf("FCR > 5 行数: %d（0.95 无过滤为 19）\n", nrow(pheno[FCR > 5])))
cat(sprintf("FCR < 0 行数: %d\n", nrow(pheno[FCR < 0])))
cat(sprintf("flag_fcr_stage_invalid 列存在: %s\n", "flag_fcr_stage_invalid" %in% names(pheno)))
if ("flag_fcr_stage_invalid" %in% names(pheno)) {
    cat(sprintf("flag_fcr_stage_invalid = TRUE 行数: %d\n", sum(pheno$flag_fcr_stage_invalid, na.rm = TRUE)))
}
cat("\n========== 验证完成 ==========\n")
