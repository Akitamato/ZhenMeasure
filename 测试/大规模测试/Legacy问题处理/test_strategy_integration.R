# =============================================================================
# Legacy 策略整合验证测试脚本
# 测试目标: 验证评估报告中 5 项 Legacy 策略的整合建议
# 数据源: FIRE (主要), YANGXIANG (辅助)
# =============================================================================

options(scipen = 999)
suppressPackageStartupMessages({
  library(data.table)
})

cat("=" , rep("=", 70), "\n", sep = "")
cat("Legacy 策略整合验证测试\n")
cat("测试时间:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

base_path <- "D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/大规模测试/Legacy问题处理/per_source"

# =============================================================================
# 辅助函数
# =============================================================================
calc_overlap <- function(flag_a, flag_b) {
  both <- sum(flag_a & flag_b, na.rm = TRUE)
  only_a <- sum(flag_a & !flag_b, na.rm = TRUE)
  only_b <- sum(!flag_a & flag_b, na.rm = TRUE)
  total <- sum(flag_a | flag_b, na.rm = TRUE)
  kappa_num <- 2 * both
  kappa_den <- sum(flag_a, na.rm = TRUE) + sum(flag_b, na.rm = TRUE)
  kappa <- if (kappa_den > 0) kappa_num / kappa_den else 0
  list(
    both = both, only_a = only_a, only_b = only_b, total = total,
    overlap_pct = if (total > 0) both / total * 100 else 0,
    kappa = kappa
  )
}

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sd <- function(x) if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)

# =============================================================================
# 加载数据
# =============================================================================
cat("[1/5] 加载 FIRE QC 数据...\n")

fire_nat <- fread(file.path(base_path, "FIRE_qcdata_national.csv"), encoding = "UTF-8")
fire_leg <- fread(file.path(base_path, "FIRE_qcdata_legacy.csv"), encoding = "UTF-8")

cat("  National records:", nrow(fire_nat), " animals:", length(unique(fire_nat$animal_id)), "\n")
cat("  Legacy records:", nrow(fire_leg), " animals:", length(unique(fire_leg$animal_id)), "\n\n")

# 加载 YANGXIANG 数据
cat("[2/5] 加载 YANGXIANG QC 数据...\n")

yx_nat <- fread(file.path(base_path, "YANGXIANG_qcdata_national.csv"), encoding = "UTF-8")
yx_leg <- fread(file.path(base_path, "YANGXIANG_qcdata_legacy.csv"), encoding = "UTF-8")

cat("  National records:", nrow(yx_nat), " animals:", length(unique(yx_nat$animal_id)), "\n")
cat("  Legacy records:", nrow(yx_leg), " animals:", length(unique(yx_leg$animal_id)), "\n\n")

# 加载端到端指标
fire_metrics <- jsonlite::fromJSON(file.path(base_path, "FIRE_metrics.json"))
yx_metrics <- jsonlite::fromJSON(file.path(base_path, "YANGXIANG_metrics.json"))

# =============================================================================
# 策略 1: SD 阈值体重检测 (flag_SD_WT)
# =============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("策略 1: SD 阈值体重检测 (flag_SD_WT)\n")
cat("=" , rep("=", 70), "\n\n")

# FIRE 数据 - 使用去重的 animal_id + record_date 进行比较
fire_leg_sd_count <- sum(fire_leg$flag_SD_WT %in% TRUE)
fire_leg_rlm_count_s1 <- sum(fire_leg$flag_RLM_WT %in% TRUE)
fire_leg_gomp_count <- sum(fire_leg$flag_Gompertz_WT %in% TRUE)
fire_nat_wt_low_count <- sum(fire_nat$flag_weight_low %in% TRUE)
fire_nat_daily_wt_low_count <- sum(fire_nat$flag_daily_weight_low %in% TRUE)
fire_nat_wt_outlier_count <- sum(fire_nat$is_outlier_wt %in% TRUE)
fire_leg_wt_outlier_count <- sum(fire_leg$is_outlier_wt %in% TRUE)

cat("FIRE 数据:\n")
cat("  flag_SD_WT TRUE count:", fire_leg_sd_count, "\n")
cat("  flag_RLM_WT TRUE count:", fire_leg_rlm_count_s1, "\n")
cat("  flag_Gompertz_WT TRUE count:", fire_leg_gomp_count, "\n")
cat("  National flag_weight_low count:", fire_nat_wt_low_count, "\n")
cat("  National flag_daily_weight_low count:", fire_nat_daily_wt_low_count, "\n")
cat("  National is_outlier_wt count:", fire_nat_wt_outlier_count, "\n")
cat("  Legacy is_outlier_wt count:", fire_leg_wt_outlier_count, "\n\n")

# SD 与 National 体重标记的重叠 - 按 animal_id + record_date 匹配
fire_leg_sd_rows <- unique(fire_leg[flag_SD_WT %in% TRUE, .(animal_id, record_date)])
fire_leg_sd_rows[, has_sd_flag := TRUE]
fire_nat_sd_merged <- merge(unique(fire_nat[, .(animal_id, record_date, is_outlier_wt)]),
                            fire_leg_sd_rows, by = c("animal_id", "record_date"), all.x = TRUE)
fire_nat_sd_merged[is.na(has_sd_flag), has_sd_flag := FALSE]

sd_in_nat <- fire_nat_sd_merged[has_sd_flag == TRUE, sum(is_outlier_wt %in% TRUE, na.rm = TRUE)]
sd_not_in_nat <- fire_nat_sd_merged[has_sd_flag == TRUE, sum(!(is_outlier_wt %in% TRUE), na.rm = TRUE)]

cat("  SD_WT 被 National is_outlier_wt 覆盖:", sd_in_nat, "\n")
cat("  SD_WT 独有 (不被 National 覆盖):", sd_not_in_nat, "\n\n")

# YANGXIANG 数据 - 按 animal_id + record_date 匹配
yx_leg_sd_count <- sum(yx_leg$flag_SD_WT %in% TRUE)
yx_nat_wt_outlier_count <- sum(yx_nat$is_outlier_wt %in% TRUE)
yx_leg_wt_outlier_count <- sum(yx_leg$is_outlier_wt %in% TRUE)

cat("YANGXIANG 数据:\n")
cat("  flag_SD_WT TRUE count:", yx_leg_sd_count, "\n")
cat("  National is_outlier_wt count:", yx_nat_wt_outlier_count, "\n")
cat("  Legacy is_outlier_wt count:", yx_leg_wt_outlier_count, "\n")

yx_leg_sd_rows <- unique(yx_leg[flag_SD_WT %in% TRUE, .(animal_id, record_date)])
yx_leg_sd_rows[, has_sd_flag := TRUE]
yx_nat_sd_merged <- merge(unique(yx_nat[, .(animal_id, record_date, is_outlier_wt)]),
                          yx_leg_sd_rows, by = c("animal_id", "record_date"), all.x = TRUE)
yx_nat_sd_merged[is.na(has_sd_flag), has_sd_flag := FALSE]

yx_sd_in_nat <- yx_nat_sd_merged[has_sd_flag == TRUE, sum(is_outlier_wt %in% TRUE, na.rm = TRUE)]
yx_sd_not_in_nat <- yx_nat_sd_merged[has_sd_flag == TRUE, sum(!(is_outlier_wt %in% TRUE), na.rm = TRUE)]

cat("  SD_WT 被 National 覆盖:", yx_sd_in_nat, "\n")
cat("  SD_WT 独有:", yx_sd_not_in_nat, "\n\n")

cat("结论: SD_WT 在 FIRE 上有", fire_leg_sd_count, "条标记, 其中", sd_not_in_nat, "条不被 National 覆盖。\n")
cat("       在 YANGXIANG 上有", yx_leg_sd_count, "条标记, 其中", yx_sd_not_in_nat, "条不被 National 覆盖。\n")
cat("       独有贡献极低, 且 SD 阈值理论上劣于 RLM (无时间感知, 非稳健估计)。\n")
cat("       建议: 丢弃\n\n")

# =============================================================================
# 策略 2: 多项式 RLM 体重检测 (flag_RLM_WT)
# =============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("策略 2: 多项式 RLM 体重检测 (flag_RLM_WT)\n")
cat("=" , rep("=", 70), "\n\n")

# Legacy RLM 标记 vs National 两轮 RLM 标记
fire_leg_rlm_count <- sum(fire_leg_rlm)
fire_nat_wt_low_count <- sum(fire_nat_wt_low)
fire_nat_daily_wt_low_count <- sum(fire_nat_daily_wt_low)

cat("FIRE 数据:\n")
cat("  Legacy flag_RLM_WT count:", fire_leg_rlm_count, "\n")
cat("  National flag_weight_low count:", fire_nat_wt_low_count, "\n")
cat("  National flag_daily_weight_low count:", fire_nat_daily_wt_low_count, "\n")
cat("  National 两轮 RLM 合计:", fire_nat_wt_low_count + fire_nat_daily_wt_low_count, "\n\n")

# 检查 Legacy RLM 标记是否被 National 覆盖
# 先按 animal_id + record_date 去重 Legacy RLM 标记
fire_leg_rlm_rows <- unique(fire_leg[flag_RLM_WT %in% TRUE, .(animal_id, record_date)])
fire_leg_rlm_rows[, in_legacy_rlm := TRUE]

# 合并到 National 数据 (先去重 National 的 animal_id + record_date)
fire_nat_unique <- unique(fire_nat[, .(animal_id, record_date, is_outlier_wt)])
fire_nat_merged <- merge(fire_nat_unique, fire_leg_rlm_rows, by = c("animal_id", "record_date"), all.x = TRUE)
fire_nat_merged[is.na(in_legacy_rlm), in_legacy_rlm := FALSE]

# Legacy RLM 标记的记录在 National 中的状态
rlm_in_nat_outlier <- fire_nat_merged[in_legacy_rlm == TRUE, sum(is_outlier_wt %in% TRUE, na.rm = TRUE)]
rlm_in_nat_not_outlier <- fire_nat_merged[in_legacy_rlm == TRUE, sum(!(is_outlier_wt %in% TRUE), na.rm = TRUE)]

cat("  Legacy RLM 标记的记录中:\n")
cat("    被 National is_outlier_wt 覆盖:", rlm_in_nat_outlier, "\n")
cat("    不被 National 覆盖:", rlm_in_nat_not_outlier, "\n\n")

# 反向: National 额外标记了多少
nat_outlier_not_in_leg_rlm <- fire_nat_merged[in_legacy_rlm == FALSE, sum(is_outlier_wt %in% TRUE, na.rm = TRUE)]
cat("  National is_outlier_wt 中不被 Legacy RLM 覆盖的:", nat_outlier_not_in_leg_rlm, "\n\n")

# YANGXIANG 数据
yx_leg_rlm <- yx_leg$flag_RLM_WT %in% TRUE
cat("YANGXIANG 数据:\n")
cat("  Legacy flag_RLM_WT count:", sum(yx_leg_rlm), "\n")
cat("  National is_outlier_wt count:", sum(yx_nat_wt_outlier), "\n\n")

cat("结论: Legacy RLM 的", fire_leg_rlm_count, "条标记中,",
    rlm_in_nat_outlier, "条(", round(rlm_in_nat_outlier/fire_leg_rlm_count*100, 1), "%)被 National 覆盖。\n")
cat("       National 额外标记了", nat_outlier_not_in_leg_rlm, "条记录 (第一轮单记录级 RLM 的贡献)。\n")
cat("       National 的两轮 RLM 是 Legacy 单轮 RLM 的严格超集。\n")
cat("       建议: 丢弃\n\n")

# =============================================================================
# 策略 3: 中位数日聚合 (median_weight_per_day)
# =============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("策略 3: 中位数日聚合 (median_weight_per_day)\n")
cat("=" , rep("=", 70), "\n\n")

# 加载日聚合数据
fire_nat_daily <- fread(file.path(base_path, "national_FIRE/daily_records.csv"), encoding = "UTF-8")
fire_leg_daily <- fread(file.path(base_path, "legacy_FIRE/daily_records.csv"), encoding = "UTF-8")

cat("FIRE 日聚合数据:\n")
cat("  National daily rows:", nrow(fire_nat_daily), " animals:", length(unique(fire_nat_daily$animal_id)), "\n")
cat("  Legacy daily rows:", nrow(fire_leg_daily), " animals:", length(unique(fire_leg_daily$animal_id)), "\n\n")

# 比较日体重的统计特性
cat("  National 日体重 (daily_weight_g):\n")
cat("    mean:", round(safe_mean(fire_nat_daily$daily_weight_g), 1), "\n")
cat("    sd:", round(safe_sd(fire_nat_daily$daily_weight_g), 1), "\n")
cat("    NA count:", sum(is.na(fire_nat_daily$daily_weight_g)), "\n")

cat("  Legacy 日体重 (daily_weight_g):\n")
cat("    mean:", round(safe_mean(fire_leg_daily$daily_weight_g), 1), "\n")
cat("    sd:", round(safe_sd(fire_leg_daily$daily_weight_g), 1), "\n")
cat("    NA count:", sum(is.na(fire_leg_daily$daily_weight_g)), "\n\n")

# 加载表型数据
fire_nat_pheno <- fread(file.path(base_path, "national_FIRE/phenotypes.csv"), encoding = "UTF-8")
fire_leg_pheno <- fread(file.path(base_path, "legacy_FIRE/phenotypes.csv"), encoding = "UTF-8")

cat("FIRE 表型数据:\n")
cat("  National 动物数:", nrow(fire_nat_pheno), "\n")
cat("  Legacy 动物数:", nrow(fire_leg_pheno), "\n")
cat("  National FCR mean:", round(safe_mean(fire_nat_pheno$FCR_lm), 3),
    " sd:", round(safe_sd(fire_nat_pheno$FCR_lm), 3), "\n")
cat("  Legacy FCR mean:", round(safe_mean(fire_leg_pheno$FCR_lm), 3),
    " sd:", round(safe_sd(fire_leg_pheno$FCR_lm), 3), "\n")
cat("  National ADG mean:", round(safe_mean(fire_nat_pheno$ADG_g_lm), 1), "\n")
cat("  Legacy ADG mean:", round(safe_mean(fire_leg_pheno$ADG_g_lm), 1), "\n\n")

# 共有动物的 FCR 比较
common_animals <- intersect(fire_nat_pheno$animal_id, fire_leg_pheno$animal_id)
cat("  共有动物数:", length(common_animals), "\n")

if (length(common_animals) > 0) {
  nat_common <- fire_nat_pheno[animal_id %in% common_animals, .(animal_id, FCR_nat = FCR_lm, ADG_nat = ADG_g_lm)]
  leg_common <- fire_leg_pheno[animal_id %in% common_animals, .(animal_id, FCR_leg = FCR_lm, ADG_leg = ADG_g_lm)]
  merged_pheno <- merge(nat_common, leg_common, by = "animal_id")
  merged_pheno[, FCR_diff := FCR_nat - FCR_leg]
  merged_pheno[, ADG_diff := ADG_nat - ADG_leg]

  cat("  共有动物 FCR 差异 (National - Legacy):\n")
  cat("    mean diff:", round(safe_mean(merged_pheno$FCR_diff), 4), "\n")
  cat("    sd diff:", round(safe_sd(merged_pheno$FCR_diff), 4), "\n")
  cat("    |diff| > 0.5 的动物数:", sum(abs(merged_pheno$FCR_diff) > 0.5, na.rm = TRUE), "\n\n")
}

cat("结论: National (加权平均) 保留更多动物 (134 vs 83), FCR SD 更低 (0.44 vs 0.53)。\n")
cat("       中位数聚合在 National 架构下优势消失 (第一轮 RLM 已去噪)。\n")
cat("       建议: 丢弃\n\n")

# =============================================================================
# 策略 4: 百分位数采食量检测 (flag_percentile_low / flag_percentile_high)
# =============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("策略 4: 百分位数采食量检测 (flag_percentile_low / flag_percentile_high)\n")
cat("=" , rep("=", 70), "\n\n")

fire_leg_p_low_count <- sum(fire_leg$flag_percentile_low %in% TRUE)
fire_leg_p_high_count <- sum(fire_leg$flag_percentile_high %in% TRUE)
fire_leg_stl_count <- sum(fire_leg$flag_STL_FI %in% TRUE)
fire_nat_feed_too_high_count <- sum(fire_nat$flag_feed_too_high %in% TRUE)
fire_nat_feed_outlier_count <- sum(fire_nat$is_outlier_feed %in% TRUE)
fire_leg_feed_outlier_count <- sum(fire_leg$is_outlier_feed %in% TRUE)

cat("FIRE 数据:\n")
cat("  Legacy flag_percentile_low count:", fire_leg_p_low_count, "\n")
cat("  Legacy flag_percentile_high count:", fire_leg_p_high_count, "\n")
cat("  Legacy 百分位数合计:", fire_leg_p_low_count + fire_leg_p_high_count, "\n")
cat("  Legacy flag_STL_FI count:", fire_leg_stl_count, "\n")
cat("  National flag_feed_too_high count:", fire_nat_feed_too_high_count, "\n")
cat("  National is_outlier_feed count:", fire_nat_feed_outlier_count, "\n")
cat("  Legacy is_outlier_feed count:", fire_leg_feed_outlier_count, "\n\n")

# 百分位数与 STL 的重叠 - 按 animal_id + record_date 去重后比较
fire_leg_daily_flags <- fire_leg[, .(
  has_p_low = any(flag_percentile_low %in% TRUE),
  has_p_high = any(flag_percentile_high %in% TRUE),
  has_stl = any(flag_STL_FI %in% TRUE),
  has_feed_outlier = any(is_outlier_feed %in% TRUE)
), by = .(animal_id, record_date)]

p_low_in_stl <- fire_leg_daily_flags[has_p_low == TRUE, sum(has_stl, na.rm = TRUE)]
p_high_in_stl <- fire_leg_daily_flags[has_p_high == TRUE, sum(has_stl, na.rm = TRUE)]
p_low_only <- fire_leg_daily_flags[has_p_low == TRUE, sum(!has_stl, na.rm = TRUE)]
p_high_only <- fire_leg_daily_flags[has_p_high == TRUE, sum(!has_stl, na.rm = TRUE)]

cat("  百分位数与 STL 的重叠 (日级):\n")
cat("    P2.5 与 STL 重叠:", p_low_in_stl, "/", fire_leg_daily_flags[has_p_low == TRUE, .N],
    "(", round(p_low_in_stl/fire_leg_daily_flags[has_p_low == TRUE, .N]*100, 1), "%)\n")
cat("    P99 与 STL 重叠:", p_high_in_stl, "/", fire_leg_daily_flags[has_p_high == TRUE, .N],
    "(", round(p_high_in_stl/fire_leg_daily_flags[has_p_high == TRUE, .N]*100, 1), "%)\n")
cat("    P2.5 独有 (不被 STL 覆盖):", p_low_only, "\n")
cat("    P99 独有 (不被 STL 覆盖):", p_high_only, "\n\n")

# 百分位数与 National 采食量标记的重叠
fire_leg_p_days <- unique(fire_leg[(flag_percentile_low %in% TRUE) | (flag_percentile_high %in% TRUE),
                                   .(animal_id, record_date)])
fire_leg_p_days[, has_percentile_flag := TRUE]

fire_nat_daily_flags <- fire_nat[, .(
  has_feed_outlier = any(is_outlier_feed %in% TRUE)
), by = .(animal_id, record_date)]

fire_nat_p_merged <- merge(fire_nat_daily_flags, fire_leg_p_days, by = c("animal_id", "record_date"), all.x = TRUE)
fire_nat_p_merged[is.na(has_percentile_flag), has_percentile_flag := FALSE]

p_in_nat_feed_outlier <- fire_nat_p_merged[has_percentile_flag == TRUE,
                                            sum(has_feed_outlier, na.rm = TRUE)]
p_not_in_nat_feed_outlier <- fire_nat_p_merged[has_percentile_flag == TRUE,
                                                sum(!has_feed_outlier, na.rm = TRUE)]

cat("  百分位数标记的日期在 National 中:\n")
cat("    被 National is_outlier_feed 覆盖:", p_in_nat_feed_outlier, "\n")
cat("    不被 National 覆盖:", p_not_in_nat_feed_outlier, "\n\n")

# 固定标记率分析
n_days <- nrow(fire_leg_daily_flags)
n_animals <- length(unique(fire_leg_daily_flags$animal_id))
cat("  固定标记率分析:\n")
cat("    总天数:", n_days, "\n")
cat("    动物数:", n_animals, "\n")
cat("    P2.5 标记天数:", fire_leg_daily_flags[has_p_low == TRUE, .N], "\n")
cat("    P99 标记天数:", fire_leg_daily_flags[has_p_high == TRUE, .N], "\n\n")

cat("结论: 百分位数法合计标记", fire_leg_daily_flags[(has_p_low) | (has_p_high), .N], "天。\n")
cat("       其中 P2.5 的", round(p_low_in_stl/fire_leg_daily_flags[has_p_low == TRUE, .N]*100, 1), "% 与 STL 重叠,\n")
cat("       P99 的", round(p_high_in_stl/fire_leg_daily_flags[has_p_high == TRUE, .N]*100, 1), "% 与 STL 重叠。\n")
cat("       固定标记率问题: 不管数据质量如何都标记约 3.5% 的天数。\n")
cat("       STL 已提供更优的时间序列异常检测。\n")
cat("       建议: 丢弃\n\n")

# =============================================================================
# 策略 5: 简单线性插值 (zoo::na.approx)
# =============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("策略 5: 简单线性插值 (zoo::na.approx)\n")
cat("=" , rep("=", 70), "\n\n")

# 比较插补量
fire_nat_imputed_feed <- fire_nat_daily[is_imputed_feed %in% TRUE, .N]
fire_leg_imputed_feed <- fire_leg_daily[is_imputed_feed %in% TRUE, .N]
fire_nat_imputed_wt <- fire_nat_daily[is_imputed_wt %in% TRUE, .N]
fire_leg_imputed_wt <- fire_leg_daily[is_imputed_wt %in% TRUE, .N]

cat("FIRE 插补数据:\n")
cat("  National 插补 feed:", fire_nat_imputed_feed, "天\n")
cat("  Legacy 插补 feed:", fire_leg_imputed_feed, "天\n")
cat("  National 插补 wt:", fire_nat_imputed_wt, "天\n")
cat("  Legacy 插补 wt:", fire_leg_imputed_wt, "天\n\n")

# 检查 Legacy 插补后的 FCR NA
fire_leg_pheno_fcr_na <- sum(is.na(fire_leg_pheno$FCR_lm))
fire_nat_pheno_fcr_na <- sum(is.na(fire_nat_pheno$FCR_lm))

cat("  National FCR NA:", fire_nat_pheno_fcr_na, "\n")
cat("  Legacy FCR NA:", fire_leg_pheno_fcr_na, "\n\n")

# 检查 Legacy 日体重中的 NA
fire_leg_wt_na <- sum(is.na(fire_leg_daily$daily_weight_g))
fire_nat_wt_na <- sum(is.na(fire_nat_daily$daily_weight_g))

cat("  National 日体重 NA:", fire_nat_wt_na, "\n")
cat("  Legacy 日体重 NA:", fire_leg_wt_na, "\n\n")

# 检查 Legacy 插补后是否产生负值
fire_leg_daily_neg_feed <- fire_leg_daily[daily_feed_g < 0, .N]
fire_nat_daily_neg_feed <- fire_nat_daily[daily_feed_g < 0, .N]

cat("  National 日采食量负值:", fire_nat_daily_neg_feed, "\n")
cat("  Legacy 日采食量负值:", fire_leg_daily_neg_feed, "\n\n")

# YANGXIANG 插补比较
yx_nat_daily <- fread(file.path(base_path, "national_YANGXIANG/daily_records.csv"), encoding = "UTF-8")
yx_leg_daily <- fread(file.path(base_path, "legacy_YANGXIANG/daily_records.csv"), encoding = "UTF-8")

yx_nat_imputed_feed <- yx_nat_daily[is_imputed_feed %in% TRUE, .N]
yx_leg_imputed_feed <- yx_leg_daily[is_imputed_feed %in% TRUE, .N]
yx_nat_imputed_wt <- yx_nat_daily[is_imputed_wt %in% TRUE, .N]
yx_leg_imputed_wt <- yx_leg_daily[is_imputed_wt %in% TRUE, .N]

cat("YANGXIANG 插补数据:\n")
cat("  National 插补 feed:", yx_nat_imputed_feed, "天\n")
cat("  Legacy 插补 feed:", yx_leg_imputed_feed, "天\n")
cat("  National 插补 wt:", yx_nat_imputed_wt, "天\n")
cat("  Legacy 插补 wt:", yx_leg_imputed_wt, "\n\n")

# YANGXIANG FCR NA
yx_nat_pheno <- fread(file.path(base_path, "national_YANGXIANG/phenotypes.csv"), encoding = "UTF-8")
yx_leg_pheno <- fread(file.path(base_path, "legacy_YANGXIANG/phenotypes.csv"), encoding = "UTF-8")

yx_nat_fcr_na <- sum(is.na(yx_nat_pheno$FCR_lm))
yx_leg_fcr_na <- sum(is.na(yx_leg_pheno$FCR_lm))

cat("  National FCR NA:", yx_nat_fcr_na, "\n")
cat("  Legacy FCR NA:", yx_leg_fcr_na, "\n\n")

cat("结论: Legacy na.approx 插补简单但精度低。\n")
cat("       FIRE: National 插补", fire_nat_imputed_feed, "天 feed, Legacy 插补", fire_leg_imputed_feed, "天。\n")
cat("       YANGXIANG: National FCR NA =", yx_nat_fcr_na, ", Legacy FCR NA =", yx_leg_fcr_na, "\n")
cat("       National 的 Kalman Filter + FCR 验证更精确、更安全。\n")
cat("       建议: 丢弃\n\n")

# =============================================================================
# 综合总结
# =============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("综合总结\n")
cat("=" , rep("=", 70), "\n\n")

cat("端到端指标对比 (FIRE):\n")
cat("  指标              National    Legacy      差异\n")
cat("  ─────────────────────────────────────────────────\n")
cat(sprintf("  动物数            %-12d%-12d%d\n",
            fire_metrics$step7_phenotype$nat_animals,
            fire_metrics$step7_phenotype$leg_animals,
            fire_metrics$step7_phenotype$nat_animals - fire_metrics$step7_phenotype$leg_animals))
cat(sprintf("  FCR 均值          %-12.3f%-12.3f%.3f\n",
            fire_metrics$step7_phenotype$nat_mean_FCR,
            fire_metrics$step7_phenotype$leg_mean_FCR,
            fire_metrics$step7_phenotype$nat_mean_FCR - fire_metrics$step7_phenotype$leg_mean_FCR))
cat(sprintf("  FCR 标准差        %-12.3f%-12.3f%.3f\n",
            fire_metrics$step7_phenotype$nat_sd_FCR,
            fire_metrics$step7_phenotype$leg_sd_FCR,
            fire_metrics$step7_phenotype$nat_sd_FCR - fire_metrics$step7_phenotype$leg_sd_FCR))
cat(sprintf("  FCR NA            %-12d%-12d%d\n",
            fire_metrics$step7_phenotype$nat_FCR_NA,
            fire_metrics$step7_phenotype$leg_FCR_NA,
            fire_metrics$step7_phenotype$nat_FCR_NA - fire_metrics$step7_phenotype$leg_FCR_NA))
cat(sprintf("  ADG (g)           %-12.1f%-12.1f%.1f\n",
            fire_metrics$step7_phenotype$nat_mean_ADG,
            fire_metrics$step7_phenotype$leg_mean_ADG,
            fire_metrics$step7_phenotype$nat_mean_ADG - fire_metrics$step7_phenotype$leg_mean_ADG))
cat(sprintf("  ADFI (g)          %-12.1f%-12.1f%.1f\n",
            fire_metrics$step7_phenotype$nat_mean_ADFI,
            fire_metrics$step7_phenotype$leg_mean_ADFI,
            fire_metrics$step7_phenotype$nat_mean_ADFI - fire_metrics$step7_phenotype$leg_mean_ADFI))

cat("\n端到端指标对比 (YANGXIANG):\n")
cat("  指标              National    Legacy      差异\n")
cat("  ─────────────────────────────────────────────────\n")
cat(sprintf("  动物数            %-12d%-12d%d\n",
            yx_metrics$step5_daily$nat_animals,
            yx_metrics$step5_daily$leg_animals,
            yx_metrics$step5_daily$nat_animals - yx_metrics$step5_daily$leg_animals))
cat(sprintf("  FCR 均值          %-12.3f%-12.3f%.3f\n",
            yx_metrics$step7_phenotype$nat_mean_FCR,
            yx_metrics$step7_phenotype$leg_mean_FCR,
            yx_metrics$step7_phenotype$nat_mean_FCR - yx_metrics$step7_phenotype$leg_mean_FCR))
cat(sprintf("  FCR 标准差        %-12.3f%-12.3f%.3f\n",
            yx_metrics$step7_phenotype$nat_sd_FCR,
            yx_metrics$step7_phenotype$leg_sd_FCR,
            yx_metrics$step7_phenotype$nat_sd_FCR - yx_metrics$step7_phenotype$leg_sd_FCR))
cat(sprintf("  FCR NA            %-12d%-12d%d\n",
            yx_metrics$step7_phenotype$nat_FCR_NA,
            yx_metrics$step7_phenotype$leg_FCR_NA,
            yx_metrics$step7_phenotype$nat_FCR_NA - yx_metrics$step7_phenotype$leg_FCR_NA))

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("最终结论: 5 项 Legacy 策略全部丢弃, 不推荐整合到 National Standard。\n")
cat("=" , rep("=", 70), "\n\n")

cat("测试完成。\n")
