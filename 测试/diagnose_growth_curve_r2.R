######### 诊断 Step 5.5 生长曲线 R² 删除算法 #########
# 目标：搞清南沙 292 头被删动物的真实构成（<10点 vs R2<0.99 vs 拟合失败）
# 只读诊断，不改包代码；复现 run_zhen_measure Step1-5 拿到删除前日汇总

rm(list = ls())

# --- 定位项目根目录 ---
script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
if (script_dir == "") {
    project_root <- getwd()
} else {
    project_root <- normalizePath(file.path(script_dir, ".."))
}

# --- 加载包（export_all=TRUE 使未导出的 ZhenM_qc_weight_standard 等也可直接调用） ---
library(data.table)
pkg_dir <- file.path(project_root, "项目本体/ZhenMeasure")
pkgload::load_all(pkg_dir, quiet = TRUE, export_all = TRUE)

cat("=== 诊断 Step 5.5 生长曲线 R² 删除算法 ===\n")
cat("包版本:", as.character(packageVersion("ZhenMeasure")), "\n\n")

# --- 路径与配置（与南沙测试完全一致） ---
data_path   <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/原始数据/南沙")
format_path <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/附加信息/YANGXIANG_data_format.json")
custom_config <- list(national_standard = list(test_weight_range = c(200, 20)))
cfg <- ZhenM_merge_config(custom_config, "national_standard")
r2_min <- cfg$national_standard$growth_curve_r2_min

cat("数据路径:", data_path, "\n")
cat("R² 阈值 growth_curve_r2_min:", r2_min, "\n\n")

# --- 复现 Step1-5（run_zhen_measure.R:69-121） ---
cat(">>> 复现 Step1-5 ...\n")
t0 <- Sys.time()

std_orig  <- ZhenM_read_data(data_path, "YANGXIANG", format_path, NULL)
std       <- data.table::copy(std_orig)
qc_result <- ZhenM_qc_overall(std, config = cfg, logger = NULL, keep_ids = NULL)
std       <- qc_result$records
std       <- ZhenM_qc_weight_standard(std, "national_standard", cfg, NULL)
std       <- ZhenM_qc_feed_standard(std, "national_standard", cfg, NULL)
daily_data <- ZhenM_standard_to_daily_filtered(std)   # Step 5.5 删除前的日汇总

cat(sprintf("复现完成，耗时 %.1f 秒\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat(sprintf("Step 5.5 输入日汇总: %d 行, %d 头动物\n\n",
            nrow(daily_data), data.table::uniqueN(daily_data$animal_id)))

# --- 逐头诊断（复现 run_zhen_measure.R:141-161 的删除判定，但记录原因） ---
ids <- unique(daily_data$animal_id)
diag <- lapply(ids, function(id) {
    sub   <- daily_data[animal_id == id]
    valid <- sub[!is.na(daily_weight_g)]
    n     <- nrow(valid)
    if (n < 10) {
        return(data.table(animal_id = id, n_points = n, r2 = NA_real_,
                          reason = "<10点", start_wt = NA_real_, end_wt = NA_real_))
    }
    x  <- as.numeric(valid$record_date - min(valid$record_date))
    y  <- valid$daily_weight_g
    r2 <- tryCatch(summary(lm(y ~ x + I(x^2)))$r.squared,
                   error = function(e) NA_real_)
    reason <- if (is.na(r2)) "拟合失败/NA"
              else if (r2 < r2_min) "R2<0.99"
              else "保留"
    data.table(animal_id = id, n_points = n, r2 = r2, reason = reason,
               start_wt = valid$daily_weight_g[1], end_wt = valid$daily_weight_g[n])
})
diag <- rbindlist(diag)

# --- 诊断报告 ---
cat("========== 诊断报告 ==========\n\n")

# 1. 删除构成
cat("--- 删除构成 ---\n")
tab <- diag[, .N, by = reason][order(-N)]
tab[, pct := N / nrow(diag) * 100]
print(tab)
n_delete <- nrow(diag[reason != "保留"])
cat(sprintf("删除总数: %d / %d (%.1f%%)\n\n", n_delete, nrow(diag), n_delete / nrow(diag) * 100))

# 2. R² 分布（仅 n_points >= 10 的动物）
cat("--- R² 分布（n_points >= 10 的动物） ---\n")
r2_ok <- diag[n_points >= 10 & !is.na(r2)]
print(summary(r2_ok$r2))
r2_bin <- r2_ok[, .N, by = .(bin = cut(r2, breaks = c(-Inf, 0.90, 0.95, 0.98, 0.99, Inf),
                                        labels = c("<0.90", "0.90-0.95", "0.95-0.98", "0.98-0.99", ">=0.99"),
                                        right = FALSE))][order(bin)]
print(r2_bin)
cat("\n")

# 3. 有效点数分布
cat("--- 有效点数分布 ---\n")
print(summary(diag$n_points))
n_bin <- diag[, .N, by = .(bin = cut(n_points, breaks = c(-Inf, 10, 30, 60, 100, Inf),
                                      labels = c("<10", "10-30", "30-60", "60-100", ">100"),
                                      right = FALSE))][order(bin)]
print(n_bin)
cat("\n")

# 4. NA / 拟合失败计数
n_na <- nrow(diag[reason == "拟合失败/NA"])
cat(sprintf("--- NA/拟合失败: %d 头（其中 <10点 已另计） ---\n\n", n_na))

# 5. 被删 vs 保留对比
cat("--- 被删 vs 保留 对比 ---\n")
diag[, grp := ifelse(reason == "保留", "保留", "删除")]
cmp <- diag[, .(动物数 = .N,
                平均有效点数 = round(mean(n_points), 1),
                平均体重跨度_g = round(mean(end_wt - start_wt, na.rm = TRUE), 0)),
            by = grp]
print(cmp)
cat("\n")

# 6. 抽查 R2<0.99 的动物体重曲线
cat("--- 抽查 R2<0.99 动物（前5个）的 daily_weight_g 序列 ---\n")
sample_ids <- head(diag[reason == "R2<0.99"]$animal_id, 5)
for (sid in sample_ids) {
    sub <- daily_data[animal_id == sid][!is.na(daily_weight_g)]
    setorder(sub, record_date)
    wts <- round(head(sub$daily_weight_g, 15) / 1000, 2)  # 转 kg
    cat(sprintf("  %s (R2=%.4f, n=%d): %s%s\n", sid,
                diag[animal_id == sid]$r2, nrow(sub),
                paste(wts, collapse = " "),
                if (nrow(sub) > 15) " ..." else ""))
}
cat("\n")

# 保存 CSV
out_dir <- file.path(project_root, "测试/demo/demo_output/YANGXIANG_扬翔/南沙_汇总分析")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
fwrite(diag, file.path(out_dir, "growth_curve_r2_diagnosis.csv"))
cat(sprintf("诊断表已保存: %s\n", file.path(out_dir, "growth_curve_r2_diagnosis.csv")))

cat("\n========== 诊断完成 ==========\n")
