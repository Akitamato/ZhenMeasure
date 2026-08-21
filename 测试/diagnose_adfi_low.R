######### 诊断脚本: 定位 ADFI 偏低根因 #########
# 逐方向量化验证, 用 YANGXIANG 数据坐实主因
rm(list = ls())

library(data.table)

project_root <- "D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发"
pkg_dir <- file.path(project_root, "项目本体", "ZhenMeasure")

devtools::load_all(pkg_dir)
library(ZhenMeasure)

demo_dir <- file.path(project_root, "测试", "demo", "demo_input")
data_path <- file.path(demo_dir, "YANGXIANG_扬翔", "原始数据")
format_path <- file.path(demo_dir, "YANGXIANG_扬翔", "附加信息", "YANGXIANG_data_format.json")

custom_config <- list(
  national_standard = list(test_weight_range = c(200, 20))
)
cfg <- ZhenM_merge_config(custom_config, "national_standard")

cat("=== 读取 + QC ===\n")
standard_data <- ZhenM_read_data(data_path, "YANGXIANG", format_path, NULL)
qc_result <- ZhenM_qc_overall(standard_data, config = cfg)
standard_data <- qc_result$records
standard_data <- ZhenM_qc_weight_standard(standard_data, "national_standard", cfg)
standard_data <- ZhenM_qc_feed_standard(standard_data, "national_standard", cfg)

feed_col <- if ("feed_g" %in% names(standard_data)) "feed_g" else "Feed_intake"

cat("\n=================================================================\n")
cat("方向 1: Feed QC 各标志排除的采食量 (量化误杀)\n")
cat("=================================================================\n")

total_feed_all <- sum(standard_data[[feed_col]], na.rm = TRUE)
cat(sprintf("  全部记录采食量总和: %.0f g\n", total_feed_all))

flags <- c("flag_feed_negative", "flag_feed_too_high", "flag_duration_negative",
           "flag_duration_too_long", "flag_duration_zero_with_feed",
           "flag_speed_too_slow", "flag_speed_too_fast", "flag_speed_extreme_low_feed",
           "flag_speed_zero_long_duration", "flag_STL_FI")

cat("\n  各标志排除的采食量:\n")
for (flg in flags) {
  if (flg %in% names(standard_data)) {
    n <- sum(standard_data[[flg]], na.rm = TRUE)
    feed_removed <- sum(standard_data[[flg]] * standard_data[[feed_col]], na.rm = TRUE)
    cat(sprintf("    %-32s: %4d 条, 采食量 %.0f g\n", flg, n, feed_removed))
  }
}

n_outlier <- sum(standard_data$is_outlier_feed, na.rm = TRUE)
feed_removed_total <- sum(standard_data$is_outlier_feed * standard_data[[feed_col]], na.rm = TRUE)
cat(sprintf("\n  合计 is_outlier_feed: %d 条, 被排除采食量 %.0f g (占全部 %.2f%%)\n",
            n_outlier, feed_removed_total, feed_removed_total / total_feed_all * 100))

# 保留记录的采食量
feed_kept <- sum((!standard_data$is_outlier_feed) * standard_data[[feed_col]], na.rm = TRUE)
cat(sprintf("  保留采食量: %.0f g\n", feed_kept))

cat("\n=================================================================\n")
cat("方向 5: flag_feed_too_high 的 P99 量纲检查\n")
cat("=================================================================\n")

# 重新计算 daily_threshold_feed 看量纲
daily_feed <- standard_data[, .(daily_total_feed = sum(get(feed_col), na.rm = TRUE)),
                            by = .(animal_id, record_date)]
daily_feed[, daily_threshold_feed := quantile(daily_total_feed, 0.99, na.rm = TRUE), by = animal_id]
cat(sprintf("  日采食量总和分布: min=%.0f, P50=%.0f, P99=%.0f, max=%.0f g\n",
            quantile(daily_feed$daily_total_feed, 0, na.rm = TRUE),
            quantile(daily_feed$daily_total_feed, 0.5, na.rm = TRUE),
            quantile(daily_feed$daily_total_feed, 0.99, na.rm = TRUE),
            quantile(daily_feed$daily_total_feed, 1, na.rm = TRUE)))
cat(sprintf("  daily_threshold_feed (日总和P99) 范围: %.0f ~ %.0f g\n",
            min(daily_feed$daily_threshold_feed, na.rm = TRUE),
            max(daily_feed$daily_threshold_feed, na.rm = TRUE)))
cat(sprintf("  单次 feed_g 分布: min=%.0f, P99=%.0f, max=%.0f g\n",
            min(standard_data[[feed_col]], na.rm = TRUE),
            quantile(standard_data[[feed_col]], 0.99, na.rm = TRUE),
            max(standard_data[[feed_col]], na.rm = TRUE)))

cat("\n=================================================================\n")
cat("方向 2: LMM 校正的 beta 值 (验证是否接近 0)\n")
cat("=================================================================\n")

# 手动重做 LMM 拟合, 打印 beta
dt <- data.table::copy(standard_data)
err_flags <- c("flag_duration_negative", "flag_duration_too_long",
               "flag_duration_zero_with_feed", "flag_speed_too_slow",
               "flag_speed_too_fast", "flag_speed_extreme_low_feed",
               "flag_speed_zero_long_duration", "flag_feed_negative",
               "flag_feed_too_high", "flag_STL_FI")

# normal_feed_sum
dt[, is_feed_normal_record := TRUE]
dt[is_outlier_feed == TRUE, is_feed_normal_record := FALSE]
for (flg in err_flags) {
  if (flg %in% names(dt)) dt[get(flg) == TRUE, is_feed_normal_record := FALSE]
}
daily_features <- dt[, .(normal_feed_sum = sum(get(feed_col)[is_feed_normal_record == TRUE], na.rm = TRUE)),
                     by = .(animal_id, record_date)]
for (flg in err_flags) {
  if (flg %in% names(dt)) {
    flg_agg <- dt[, .(x = any(get(flg) == TRUE, na.rm = TRUE)), by = .(animal_id, record_date)]
    setnames(flg_agg, "x", paste0("has_", flg))
    daily_features <- merge(daily_features, flg_agg, by = c("animal_id", "record_date"), all.x = TRUE)
  }
}

# 日汇总 weight/adg
daily_dt <- ZhenM_standard_to_daily_filtered(standard_data)
daily_features <- merge(daily_features, daily_dt[, .(animal_id, record_date, daily_weight_g)], by = c("animal_id", "record_date"), all.x = TRUE)
daily_features[, normal_feed_sum := ifelse(normal_feed_sum > 0 & normal_feed_sum <= 6000, normal_feed_sum, NA_real_)]
setorder(daily_features, animal_id, record_date)
daily_features[, adg_g := c(NA, diff(daily_weight_g)), by = animal_id]
daily_features[is.na(adg_g), adg_g := 0]

# active flags
active_flags <- character()
for (flg in err_flags) {
  has_flg_name <- paste0("has_", flg)
  if (has_flg_name %in% names(daily_features) && length(unique(daily_features[[has_flg_name]])) > 1) {
    active_flags <- c(active_flags, has_flg_name)
  }
}

train_idx <- !is.na(daily_features$normal_feed_sum) & !is.na(daily_features$daily_weight_g) & !is.na(daily_features$adg_g)
formula_str <- "normal_feed_sum ~ I(daily_weight_g / 1000) + I(adg_g / 1000)"
for (flg in active_flags) formula_str <- paste0(formula_str, " + ", flg)
formula_str <- paste0(formula_str, " + (1 | animal_id)")

cat(sprintf("  训练样本数: %d, active flags: %d\n", sum(train_idx), length(active_flags)))
cat(sprintf("  公式: %s\n", formula_str))

if (requireNamespace("lme4", quietly = TRUE) && sum(train_idx) > 30) {
  lmm_fit <- tryCatch(lme4::lmer(as.formula(formula_str), data = daily_features[train_idx]),
                      error = function(e) NULL)
  if (!is.null(lmm_fit)) {
    fixed_eff <- lme4::fixef(lmm_fit)
    cat("\n  固定效应系数 (beta):\n")
    for (nm in names(fixed_eff)) {
      cat(sprintf("    %-30s: %10.4f\n", nm, fixed_eff[nm]))
    }
  }
}

cat("\n=================================================================\n")
cat("方向 3: NA 天与插补的对应关系\n")
cat("=================================================================\n")

daily_dt <- ZhenM_standard_to_daily_filtered(standard_data)
n_na_before <- sum(is.na(daily_dt$daily_feed_g))
daily_imputed <- ZhenM_impute_national(daily_dt, cfg)
n_imputed <- sum(daily_imputed$is_imputed_feed, na.rm = TRUE)
n_na_after <- sum(is.na(daily_imputed$daily_feed_g))
cat(sprintf("  插补前 daily_feed_g NA: %d\n", n_na_before))
cat(sprintf("  插补标记 is_imputed_feed=TRUE: %d\n", n_imputed))
cat(sprintf("  插补后 daily_feed_g NA: %d (仍为 NA, 将被 sum 当作 0)\n", n_na_after))

cat("\n=== 诊断完成 ===\n")
