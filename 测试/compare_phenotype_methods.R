######### 对比 4 种表型算法（standard_fcr/report/monitor/research） #########
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

# 对同一份 daily_records 用 4 种方法算表型（weight stage）
extract <- function(p, method) {
    if (nrow(p) == 0) return(data.table(方法 = method, ADG = NA_real_, ADFI = NA_real_, FCR = NA_real_))
    if (method == "standard_fcr" || method == "report") {
        adg <- mean(p$ADG_g, na.rm = TRUE); adfi <- mean(p$ADFI_g, na.rm = TRUE)
        fcr <- mean(p$FCR, na.rm = TRUE)
    } else if (method == "monitor") {
        adg <- mean(p$ADG_rolling_mean_g, na.rm = TRUE); adfi <- mean(p$ADFI_rolling_mean_g, na.rm = TRUE)
        fcr <- mean(p$FCR_rolling_mean, na.rm = TRUE)
    } else {  # research
        adg <- mean(p$ADG_g_lm, na.rm = TRUE); adfi <- mean(p$ADFI_g_lm, na.rm = TRUE)
        fcr <- mean(p$FCR_lm, na.rm = TRUE)
    }
    data.table(方法 = method,
               ADG = round(adg, 1), ADG_sd = round(sd(p$ADG_g, na.rm = TRUE), 1),
               ADFI = round(adfi, 1),
               FCR = round(fcr, 3))
}

methods <- c("standard_fcr", "report", "monitor", "research")
out <- rbindlist(lapply(methods, function(m) {
    p <- as.data.table(ZhenM_calc_phenotypes(daily, m, stage_mode = "weight",
                                             target_weight_stages = "YANGXIANG"))
    cat(sprintf("  %s: %d 行\n", m, nrow(p)))
    extract(p, m)
}))
cat("\n========== 4 种表型算法对比（同一份质控后数据, 30-100/115/120kg 阶段） ==========\n")
print(out)
cat("\n说明：\n")
cat("  standard_fcr/report 的 ADG 用「首尾体重差÷天数」（对噪声敏感）\n")
cat("  monitor 用滚动窗口 mean\n")
cat("  research 用线性回归斜率（对噪声稳健）\n")
cat("\n========== 对比完成 ==========\n")
