######### 验证：救回猪的高 FCR 是体重噪声假象，还是真实低效 #########
rm(list = ls())

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
project_root <- if (script_dir == "") getwd() else normalizePath(file.path(script_dir, ".."))

library(data.table)
pkg_dir <- file.path(project_root, "项目本体/ZhenMeasure")
pkgload::load_all(pkg_dir, quiet = TRUE, export_all = TRUE)

data_path   <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/原始数据/南沙")
format_path <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/附加信息/YANGXIANG_data_format.json")

cat(">>> 运行 pipeline 拿 daily_records ...\n")
t0 <- Sys.time()
result <- run_zhen_measure(
    data_path = data_path, data_type = "YANGXIANG", format_path = format_path,
    qc_method = "national_standard", phenotype_method = "report",
    stage_mode = "weight", target_weight_stages = "YANGXIANG",
    output_dir = NULL,
    config = list(national_standard = list(test_weight_range = c(200, 20))),
    growth_curve = FALSE, growth_curve_test = FALSE
)
cat(sprintf("运行完成，耗时 %.1f 秒\n\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

daily <- as.data.table(result$daily_records)
pheno <- as.data.table(result$phenotypes)

# 基线 324 头（0.99 结果）
base_ids <- unique(fread(file.path(project_root,
    "测试/demo/demo_output/YANGXIANG_扬翔/南沙_汇总分析/phenotypes.csv"), encoding = "UTF-8")$animal_id)
rescued_ids <- setdiff(unique(pheno$animal_id), base_ids)
cat(sprintf("基线 %d 头, 救回 %d 头\n\n", length(base_ids), length(rescued_ids)))

# 对每头猪算体重曲线特征（用质控后 daily_weight_g 非 NA 序列）
analyze_curve <- function(sub) {
    sub <- sub[!is.na(daily_weight_g)]
    setorder(sub, record_date)
    if (nrow(sub) < 5) return(NULL)
    w <- sub$daily_weight_g
    d <- as.numeric(sub$record_date - min(sub$record_date))
    dw <- diff(w)
    drop_rate <- mean(dw < 0, na.rm = TRUE)              # 掉重频率
    jump_med  <- median(abs(dw), na.rm = TRUE)           # 相邻天跳变幅度(g)
    simple_adg <- (tail(w, 1) - head(w, 1)) / max(1, tail(d, 1) - head(d, 1))  # 简单 ADG
    reg_adg <- tryCatch(stats::lm(w ~ d)$coefficients[["d"]], error = function(e) NA_real_)  # 回归 ADG
    data.table(animal_id = sub$animal_id[1], n_pts = nrow(sub),
               drop_rate = drop_rate, jump_med = jump_med,
               simple_adg = simple_adg, reg_adg = reg_adg)
}

curve_all <- rbindlist(lapply(unique(daily$animal_id), function(id) {
    analyze_curve(daily[animal_id == id])
}))
curve_all[, group := ifelse(animal_id %in% rescued_ids, "救回", "基线")]

cat("========== 体重曲线噪声特征：救回 vs 基线 ==========\n")
cmp <- curve_all[, .(
    动物数 = .N,
    平均点数 = round(mean(n_pts), 1),
    掉重频率 = round(mean(drop_rate, na.rm = TRUE) * 100, 1),      # %
    跳变幅度_g = round(mean(jump_med, na.rm = TRUE), 0),
    简单ADG_g = round(mean(simple_adg, na.rm = TRUE), 1),
    回归ADG_g = round(mean(reg_adg, na.rm = TRUE), 1)
), by = group]
print(cmp)
cat("\n")

cat("========== 简单 ADG vs 回归 ADG 的差异（噪声放大程度） ==========\n")
curve_all[, adg_gap := simple_adg - reg_adg]  # 负 = 简单 ADG 被噪声压低
gap <- curve_all[, .(ADG差距_g = round(mean(adg_gap, na.rm = TRUE), 1)), by = group]
print(gap)
cat("\n")

# 结合表型：救回猪的简单 FCR vs 稳健 FCR（用回归 ADG）
pheno_30_100 <- pheno[stage_label == "30-100kg"]
pheno_30_100[, adfi_g := ADFI_g]
fcr_compare <- merge(pheno_30_100[, .(animal_id, simple_fcr = FCR, adfi_g)],
                     curve_all[, .(animal_id, reg_adg)], by = "animal_id")
fcr_compare[, robust_fcr := adfi_g / reg_adg]  # 稳健 FCR = ADFI / 回归ADG
fcr_compare[, group := ifelse(animal_id %in% rescued_ids, "救回", "基线")]

cat("========== 简单 FCR vs 稳健 FCR（30-100kg 阶段） ==========\n")
fcr_summ <- fcr_compare[, .(
    简单FCR = round(mean(simple_fcr, na.rm = TRUE), 3),
    稳健FCR = round(mean(robust_fcr, na.rm = TRUE), 3),
    差值 = round(mean(simple_fcr - robust_fcr, na.rm = TRUE), 3)
), by = group]
print(fcr_summ)
cat("\n")

cat("========== 结论判读 ==========\n")
r <- fcr_compare[group == "救回"]
cat(sprintf("救回猪：简单FCR=%.3f, 稳健FCR=%.3f\n",
            mean(r$simple_fcr, na.rm = TRUE), mean(r$robust_fcr, na.rm = TRUE)))
cat(sprintf("若稳健FCR明显低于简单FCR且接近2.5 → 高FCR是噪声假象；若两者接近 → 真实低效\n"))
cat("\n========== 验证完成 ==========\n")
