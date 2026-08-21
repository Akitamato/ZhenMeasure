library(data.table)
pkgload::load_all("项目本体/ZhenMeasure", quiet = TRUE)

custom_config <- list(national_standard = list(test_weight_range = c(200, 20)))

result <- run_zhen_measure(
    data_path = "测试/demo/demo_input/YANGXIANG_扬翔/原始数据/南沙",
    data_type = "YANGXIANG",
    format_path = "测试/demo/demo_input/YANGXIANG_扬翔/附加信息/YANGXIANG_data_format.json",
    qc_method = "national_standard",
    phenotype_method = "report",
    stage_mode = "weight",
    target_weight_stages = "YANGXIANG",
    output_dir = tempfile(),
    config = custom_config,
    growth_curve = FALSE,
    growth_curve_test = TRUE
)

raw  <- as.data.table(result$raw_daily)
filt <- as.data.table(result$daily_records)

cat("\n============================================\n")
cat("  ADFI 下降诊断: 异常天排除 + LMM 校正\n")
cat("============================================\n\n")

# ============================================================
# Part 1: 排除异常天后为什么下降
# ============================================================
cat("======= Part 1: 异常天排除分析 =======\n\n")

raw[, outlier_type := "正常"]
raw[day_has_outlier_wt == TRUE & day_has_outlier_feed == FALSE, outlier_type := "仅体重异常"]
raw[day_has_outlier_wt == FALSE & day_has_outlier_feed == TRUE, outlier_type := "仅采食异常"]
raw[day_has_outlier_wt == TRUE & day_has_outlier_feed == TRUE, outlier_type := "体重+采食异常"]

cat("--- 各类天的采食量分布 ---\n")
outlier_summary <- raw[, .(
    days = .N,
    mean_feed = mean(daily_feed_g, na.rm=TRUE),
    median_feed = median(daily_feed_g, na.rm=TRUE),
    pct = .N / nrow(raw) * 100
), by = outlier_type][order(-days)]
print(outlier_summary)
cat("\n")

cat("--- 异常天 ADFI 贡献 ---\n")
total_feed <- sum(raw$daily_feed_g, na.rm=TRUE)
for (ot in c("仅采食异常", "仅体重异常", "体重+采食异常")) {
    sub_feed <- sum(raw[outlier_type == ot]$daily_feed_g, na.rm=TRUE)
    sub_days <- nrow(raw[outlier_type == ot])
    cat(sprintf("  %s: %d 天 (%.1f%%), 采食量占比 %.1f%%\n",
                ot, sub_days, sub_days/nrow(raw)*100, sub_feed/total_feed*100))
}
cat("\n")

cat("--- 采食异常天 vs 正常天的采食量分位数 ---\n")
for (flag in c(FALSE, TRUE)) {
    sub <- raw[day_has_outlier_feed == flag]
    label <- if (flag) "采食异常天" else "正常天"
    cat(sprintf("  %s: N=%d, mean=%.0f, median=%.0f, P5=%.0f, P25=%.0f, P75=%.0f, P95=%.0f\n",
                label, nrow(sub),
                mean(sub$daily_feed_g, na.rm=TRUE),
                median(sub$daily_feed_g, na.rm=TRUE),
                quantile(sub$daily_feed_g, 0.05, na.rm=TRUE),
                quantile(sub$daily_feed_g, 0.25, na.rm=TRUE),
                quantile(sub$daily_feed_g, 0.75, na.rm=TRUE),
                quantile(sub$daily_feed_g, 0.95, na.rm=TRUE)))
}
cat("\n")

cat("--- 仅体重异常天 vs 正常天的采食量分位数 ---\n")
for (flag in c(FALSE, TRUE)) {
    sub <- raw[day_has_outlier_wt == flag & day_has_outlier_feed == FALSE]
    label <- if (flag) "仅体重异常天" else "正常天(无任何异常)"
    cat(sprintf("  %s: N=%d, mean=%.0f, median=%.0f, P5=%.0f, P25=%.0f, P75=%.0f, P95=%.0f\n",
                label, nrow(sub),
                mean(sub$daily_feed_g, na.rm=TRUE),
                median(sub$daily_feed_g, na.rm=TRUE),
                quantile(sub$daily_feed_g, 0.05, na.rm=TRUE),
                quantile(sub$daily_feed_g, 0.25, na.rm=TRUE),
                quantile(sub$daily_feed_g, 0.75, na.rm=TRUE),
                quantile(sub$daily_feed_g, 0.95, na.rm=TRUE)))
}
cat("\n")

# 个体级: 排除异常天前后 ADFI 变化
raw_adfi_all    <- raw[, .(raw_adfi_all = mean(daily_feed_g, na.rm=TRUE)), by = animal_id]
raw_adfi_normal <- raw[outlier_type == "正常", .(raw_adfi_normal = mean(daily_feed_g, na.rm=TRUE)), by = animal_id]
tmp <- merge(raw_adfi_all, raw_adfi_normal, by = "animal_id")
tmp[, drop_pct := (raw_adfi_normal - raw_adfi_all) / raw_adfi_all * 100]

cat("--- 排除异常天: 个体 ADFI 变化率 (%) ---\n")
print(summary(tmp$drop_pct))
cat("\n")

# 异常天占比
raw[, total_days := .N, by = animal_id]
raw[, outlier_days := sum(day_has_outlier_feed | day_has_outlier_wt), by = animal_id]
raw[, outlier_pct := outlier_days / total_days]
ind_outlier <- unique(raw[, .(animal_id, total_days, outlier_days, outlier_pct)])

cat("--- 异常天占比分布 ---\n")
print(summary(ind_outlier$outlier_pct))
cat(sprintf("\n异常天占比 > 30%%: %d 头\n", nrow(ind_outlier[outlier_pct > 0.3])))
cat(sprintf("异常天占比 > 50%%: %d 头\n\n", nrow(ind_outlier[outlier_pct > 0.5])))

# ============================================================
# Part 2: LMM 校正分析
# ============================================================
cat("======= Part 2: LMM 校正分析 =======\n\n")

# 匹配同一天
matched <- merge(
    raw[, .(animal_id, record_date, raw_feed = daily_feed_g,
            day_has_outlier_wt, day_has_outlier_feed, outlier_type)],
    filt[, .(animal_id, record_date, qc_feed = daily_feed_g,
             is_imputed_feed, is_imputed_wt, n_outlier_wt, n_outlier_feed)],
    by = c("animal_id", "record_date")
)

cat(sprintf("匹配到 %d 天 (raw %d 天, filt %d 天)\n\n", nrow(matched), nrow(raw), nrow(filt)))

matched[, lmm_delta := qc_feed - raw_feed]

cat("--- LMM 校正量分布 (g) ---\n")
print(summary(matched$lmm_delta))
cat("\n")

cat(sprintf("校正量 < 0 (下调): %d 天 (%.1f%%)\n",
            sum(matched$lmm_delta < 0, na.rm=TRUE),
            sum(matched$lmm_delta < 0, na.rm=TRUE) / nrow(matched) * 100))
cat(sprintf("校正量 = 0 (无变化): %d 天 (%.1f%%)\n",
            sum(matched$lmm_delta == 0, na.rm=TRUE),
            sum(matched$lmm_delta == 0, na.rm=TRUE) / nrow(matched) * 100))
cat(sprintf("校正量 > 0 (上调): %d 天 (%.1f%%)\n\n",
            sum(matched$lmm_delta > 0, na.rm=TRUE),
            sum(matched$lmm_delta > 0, na.rm=TRUE) / nrow(matched) * 100))

# 按是否被校正分组
matched[, has_correction := lmm_delta != 0]
cat("--- 有/无校正天的原始采食量 ---\n")
corr_stats <- matched[, .(
    days = .N,
    mean_raw = mean(raw_feed, na.rm=TRUE),
    mean_delta = mean(lmm_delta, na.rm=TRUE),
    sum_delta = sum(lmm_delta, na.rm=TRUE)
), by = has_correction]
print(corr_stats)
cat("\n")

# 按原始采食量分桶
matched[, feed_bin := cut(raw_feed,
    breaks = c(-Inf, 0, 500, 1000, 1500, 2000, 2500, 3000, Inf),
    labels = c("<0", "0-500", "500-1k", "1k-1.5k", "1.5k-2k", "2k-2.5k", "2.5k-3k", ">3k"))]

cat("--- LMM 校正量按原始采食量分桶 ---\n")
bin_stats <- matched[, .(
    days = .N,
    mean_raw = mean(raw_feed, na.rm=TRUE),
    mean_delta = mean(lmm_delta, na.rm=TRUE),
    median_delta = median(lmm_delta, na.rm=TRUE),
    mean_qc = mean(qc_feed, na.rm=TRUE)
), by = feed_bin][order(feed_bin)]
print(bin_stats)
cat("\n")

cat(sprintf("总校正量: %.0f g (%.1f kg)\n", sum(matched$lmm_delta, na.rm=TRUE),
            sum(matched$lmm_delta, na.rm=TRUE)/1000))
cat(sprintf("平均每天校正量: %.1f g\n", mean(matched$lmm_delta, na.rm=TRUE)))
cat(sprintf("校正导致 ADFI 变化: %.1f g/天\n\n", mean(matched$lmm_delta, na.rm=TRUE)))

# 校正量按异常类型
cat("--- LMM 校正量按天的异常类型 ---\n")
outlier_corr <- matched[, .(
    days = .N,
    mean_delta = mean(lmm_delta, na.rm=TRUE),
    sum_delta = sum(lmm_delta, na.rm=TRUE),
    pct_days = .N / nrow(matched) * 100
), by = outlier_type][order(-days)]
print(outlier_corr)
cat("\n")

# 校正最大的 20 天
cat("--- 校正量最负的 20 天 ---\n")
worst <- head(matched[order(lmm_delta)], 20)
print(worst[, .(animal_id, record_date, raw_feed, qc_feed, lmm_delta,
                outlier_type, n_outlier_wt, n_outlier_feed)])
cat("\n")

# 校正最大的 10 天 (上调)
cat("--- 校正量最正的 10 天 ---\n")
best <- head(matched[order(-lmm_delta)], 10)
print(best[, .(animal_id, record_date, raw_feed, qc_feed, lmm_delta,
               outlier_type, n_outlier_wt, n_outlier_feed)])

cat("\n======= Done =======\n")
