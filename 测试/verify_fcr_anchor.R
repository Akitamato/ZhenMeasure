######### 验证 FCR 锚定矫正效果：开启 vs 关闭 #########
rm(list = ls())

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
project_root <- if (script_dir == "") getwd() else normalizePath(file.path(script_dir, ".."))

library(data.table)
pkg_dir <- file.path(project_root, "项目本体/ZhenMeasure")
pkgload::load_all(pkg_dir, quiet = TRUE, export_all = TRUE)

data_path   <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/原始数据/南沙")
format_path <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/附加信息/YANGXIANG_data_format.json")

cat(">>> 跑 pipeline 拿质控后 daily_records（默认 use_fcr_anchor=FALSE）...\n")
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
cat(sprintf("运行完成，耗时 %.1f 秒，%d 头\n\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")), uniqueN(daily$animal_id)))

cfg <- ZhenM_default_config("national_standard")

# 关闭：直接算表型
p_off <- as.data.table(ZhenM_calc_phenotypes(daily, "report",
    stage_mode = "weight", target_weight_stages = "YANGXIANG"))

# 开启：FCR 锚定矫正后算表型
daily_corrected <- .correct_feed_with_fcr_anchor(daily, cfg)
p_on <- as.data.table(ZhenM_calc_phenotypes(daily_corrected, "report",
    stage_mode = "weight", target_weight_stages = "YANGXIANG"))

cat("========== FCR 锚定矫正效果对比 ==========\n")
cat(sprintf("被矫正的天数: %d / %d (%.2f%%)\n",
            sum(daily_corrected$flag_feed_fcr_corrected), nrow(daily_corrected),
            sum(daily_corrected$flag_feed_fcr_corrected) / nrow(daily_corrected) * 100))

summ <- function(dt, label) {
    data.table(方案 = label,
        ADG = round(mean(dt$ADG_g, na.rm = TRUE), 1),
        ADFI = round(mean(dt$ADFI_g, na.rm = TRUE), 1),
        FCR_mean = round(mean(dt$FCR, na.rm = TRUE), 3),
        FCR_sd = round(sd(dt$FCR, na.rm = TRUE), 3))
}
out <- rbind(summ(p_off, "关闭"), summ(p_on, "开启"))
print(out)
cat("\n")

# 矫正方向统计
up <- sum(daily_corrected$flag_feed_fcr_corrected &
          daily_corrected$daily_feed_g > daily$daily_feed_g, na.rm = TRUE)
down <- sum(daily_corrected$flag_feed_fcr_corrected &
            daily_corrected$daily_feed_g < daily$daily_feed_g, na.rm = TRUE)
cat(sprintf("矫正方向：上调 %d 天, 下调 %d 天\n\n", up, down))

cat("========== 验证完成 ==========\n")
