######### 完整对比 4 种表型算法（最新代码） #########
rm(list = ls())

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
project_root <- if (script_dir == "") getwd() else normalizePath(file.path(script_dir, ".."))

library(data.table)
pkg_dir <- file.path(project_root, "项目本体/ZhenMeasure")
pkgload::load_all(pkg_dir, quiet = TRUE, export_all = TRUE)

data_path   <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/原始数据/南沙")
format_path <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/附加信息/YANGXIANG_data_format.json")

cat(">>> 跑 pipeline 拿质控后 daily_records ...\n")
t0 <- Sys.time()
result <- run_zhen_measure(
    data_path = data_path, data_type = "YANGXIANG", format_path = format_path,
    qc_method = "national_standard", phenotype_method = "report",
    stage_mode = "weight", target_weight_stages = "YANGXIANG",
    output_dir = NULL,
    config = list(national_standard = list(test_weight_range = c(200, 20))),
    growth_curve = FALSE, growth_curve_test = FALSE
)
daily <- as.data.table(result$daily_records)
cat(sprintf("运行完成，耗时 %.1f 秒，质控后 %d 头\n\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")), uniqueN(daily$animal_id)))

extract <- function(p, method) {
    if (nrow(p) == 0) return(data.table(方法 = method, 行数 = 0L))
    if (method == "standard_fcr" || method == "report") {
        adg <- p$ADG_g; adfi <- p$ADFI_g; fcr <- p$FCR
    } else if (method == "monitor") {
        adg <- p$ADG_rolling_mean_g; adfi <- p$ADFI_rolling_mean_g; fcr <- p$FCR_rolling_mean
    } else {
        adg <- p$ADG_g_lm; adfi <- p$ADFI_g_lm; fcr <- p$FCR_lm
    }
    data.table(
        方法 = method, 行数 = nrow(p),
        ADG_mean = round(mean(adg, na.rm = TRUE), 1),
        ADG_sd   = round(sd(adg, na.rm = TRUE), 1),
        ADFI_mean = round(mean(adfi, na.rm = TRUE), 1),
        FCR_mean = round(mean(fcr, na.rm = TRUE), 3),
        FCR_sd   = round(sd(fcr, na.rm = TRUE), 3),
        FCR_P25  = round(quantile(fcr, 0.25, na.rm = TRUE), 3),
        FCR_P50  = round(quantile(fcr, 0.50, na.rm = TRUE), 3),
        FCR_P75  = round(quantile(fcr, 0.75, na.rm = TRUE), 3)
    )
}

methods <- c("standard_fcr", "report", "monitor", "research")
out <- rbindlist(lapply(methods, function(m) {
    p <- as.data.table(ZhenM_calc_phenotypes(daily, m,
        stage_mode = "weight", target_weight_stages = "YANGXIANG"))
    extract(p, m)
}))

cat("========== 4 种表型算法完整对比（30-100/115/120kg 阶段） ==========\n")
print(out)
cat("\n说明：\n")
cat("  standard_fcr: 国标法 FCR=feed/固定90kg（另算），ADG 仍用首尾差\n")
cat("  report:       首尾体重差 ÷ 天数（对噪声敏感）\n")
cat("  monitor:      滚动窗口 mean（较稳健）\n")
cat("  research:     线性回归斜率（对噪声最稳健）\n")
cat("\n========== 对比完成 ==========\n")
