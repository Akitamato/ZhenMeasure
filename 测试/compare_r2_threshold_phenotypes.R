######### 对比 R² 阈值 0.99 vs 0.95 的表型变化 #########
# 基线(0.99)读已有 phenotypes.csv；0.95 跑完整 pipeline 后对比

rm(list = ls())

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
project_root <- if (script_dir == "") getwd() else normalizePath(file.path(script_dir, ".."))

library(data.table)
pkg_dir <- file.path(project_root, "项目本体/ZhenMeasure")
pkgload::load_all(pkg_dir, quiet = TRUE, export_all = TRUE)

# --- 读 0.99 基线表型 ---
base_file <- file.path(project_root, "测试/demo/demo_output/YANGXIANG_扬翔/南沙_汇总分析/phenotypes.csv")
base <- as.data.table(fread(base_file, encoding = "UTF-8"))

# --- 路径与 config（0.95） ---
data_path   <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/原始数据/南沙")
format_path <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/附加信息/YANGXIANG_data_format.json")
custom_config <- list(national_standard = list(
    test_weight_range = c(200, 20),
    growth_curve_r2_min = 0.95
))

# 打印生效阈值确认
cfg_check <- ZhenM_merge_config(custom_config, "national_standard")
cat(sprintf("生效阈值 growth_curve_r2_min: %s\n", cfg_check$national_standard$growth_curve_r2_min))

# --- 跑 0.95 pipeline ---
cat(">>> 运行 0.95 阈值 pipeline ...\n")
t0 <- Sys.time()
result <- run_zhen_measure(
    data_path            = data_path,
    data_type            = "YANGXIANG",
    format_path          = format_path,
    qc_method            = "national_standard",
    phenotype_method     = "report",
    stage_mode           = "weight",
    target_weight_stages = "YANGXIANG",
    output_dir           = NULL,          # 不写文件，纯内存对比
    config               = custom_config,
    growth_curve         = FALSE,
    growth_curve_test    = FALSE
)
new <- as.data.table(result$phenotypes)
cat(sprintf("运行完成，耗时 %.1f 秒\n\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# --- 汇总函数 ---
summarize <- function(dt) {
    data.table(
        动物数     = uniqueN(dt$animal_id),
        表型行数   = nrow(dt),
        ADG_g_mean = round(mean(dt$ADG_g, na.rm = TRUE), 1),
        ADG_g_sd   = round(sd(dt$ADG_g, na.rm = TRUE), 1),
        ADFI_g_mean= round(mean(dt$ADFI_g, na.rm = TRUE), 1),
        ADFI_g_sd  = round(sd(dt$ADFI_g, na.rm = TRUE), 1),
        FCR_mean   = round(mean(dt$FCR, na.rm = TRUE), 3),
        FCR_sd     = round(sd(dt$FCR, na.rm = TRUE), 3),
        test_days_mean = round(mean(dt$test_days, na.rm = TRUE), 1),
        total_feed_kg_mean = round(mean(dt$total_feed_g, na.rm = TRUE) / 1000, 1)
    )
}

cat("========== 总体表型对比（0.99 vs 0.95） ==========\n")
overall <- rbind(
    cbind(阈值 = "0.99(基线)", summarize(base)),
    cbind(阈值 = "0.95",      summarize(new))
)
print(overall)
cat("\n")

# --- 按 stage_label 对比 ---
cat("========== 按体重阶段对比 ==========\n")
stage_levels <- sort(unique(c(base$stage_label, new$stage_label)))
for (st in stage_levels) {
    b <- base[stage_label == st]
    n <- new[stage_label == st]
    if (nrow(b) == 0 || nrow(n) == 0) next
    cat(sprintf("\n--- stage: %s ---\n", st))
    cat(sprintf("  动物数: 0.99=%d -> 0.95=%d (新增 %d)\n",
                uniqueN(b$animal_id), uniqueN(n$animal_id),
                uniqueN(n$animal_id) - uniqueN(b$animal_id)))
    cat(sprintf("  ADG_g:  0.99=%.1f -> 0.95=%.1f\n",
                mean(b$ADG_g, na.rm=TRUE), mean(n$ADG_g, na.rm=TRUE)))
    cat(sprintf("  ADFI_g: 0.99=%.1f -> 0.95=%.1f\n",
                mean(b$ADFI_g, na.rm=TRUE), mean(n$ADFI_g, na.rm=TRUE)))
    cat(sprintf("  FCR:    0.99=%.3f -> 0.95=%.3f\n",
                mean(b$FCR, na.rm=TRUE), mean(n$FCR, na.rm=TRUE)))
}

# --- 新增动物的表型特征（0.95 有但 0.99 没有的 animal_id） ---
cat("\n========== 新增保留动物（0.95 救回）的表型特征 ==========\n")
new_ids  <- unique(new$animal_id)
base_ids <- unique(base$animal_id)
rescued  <- setdiff(new_ids, base_ids)
cat(sprintf("救回动物数: %d\n", length(rescued)))
if (length(rescued) > 0) {
    rescued_dt <- new[animal_id %in% rescued]
    cat("救回动物的表型分布:\n")
    print(summarize(rescued_dt))
    # 救回动物的 R² 分布（读诊断表）
    diag_file <- file.path(project_root, "测试/demo/demo_output/YANGXIANG_扬翔/南沙_汇总分析/growth_curve_r2_diagnosis.csv")
    if (file.exists(diag_file)) {
        diag <- fread(diag_file, encoding = "UTF-8")
        rescued_r2 <- diag[animal_id %in% rescued]$r2
        cat(sprintf("救回动物的 R² 分布: min=%.4f, median=%.4f, max=%.4f\n",
                    min(rescued_r2), median(rescued_r2), max(rescued_r2)))
    }
}

# --- 保存 0.95 表型（不覆盖基线） ---
out_file <- file.path(project_root, "测试/demo/demo_output/YANGXIANG_扬翔/南沙_汇总分析/phenotypes_r2_095.csv")
fwrite(new, out_file)
cat(sprintf("\n0.95 表型已保存: %s\n", out_file))
cat("\n========== 对比完成 ==========\n")
