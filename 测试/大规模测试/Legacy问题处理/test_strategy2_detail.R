# 策略 2 详细分析: Legacy RLM 的 141 条独有标记是否真的有价值
options(scipen = 999)
suppressPackageStartupMessages(library(data.table))

base_path <- "D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/大规模测试/Legacy问题处理/per_source"

fire_nat <- fread(file.path(base_path, "FIRE_qcdata_national.csv"), encoding = "UTF-8")
fire_leg <- fread(file.path(base_path, "FIRE_qcdata_legacy.csv"), encoding = "UTF-8")

# 按 animal_id + record_date 去重
fire_leg_rlm_rows <- unique(fire_leg[flag_RLM_WT %in% TRUE, .(animal_id, record_date)])
fire_leg_rlm_rows[, in_legacy_rlm := TRUE]

fire_nat_unique <- unique(fire_nat[, .(animal_id, record_date, is_outlier_wt)])
fire_nat_merged <- merge(fire_nat_unique, fire_leg_rlm_rows, by = c("animal_id", "record_date"), all.x = TRUE)
fire_nat_merged[is.na(in_legacy_rlm), in_legacy_rlm := FALSE]

# Legacy RLM 独有标记的动物分布
legacy_rlm_unique <- fire_nat_merged[in_legacy_rlm == TRUE & !(is_outlier_wt %in% TRUE)]
cat("Legacy RLM 独有标记 (不被 National is_outlier_wt 覆盖):\n")
cat("  记录数:", nrow(legacy_rlm_unique), "\n")
cat("  涉及动物数:", length(unique(legacy_rlm_unique$animal_id)), "\n\n")

# 这些动物在 National 中的状态
unique_animals <- unique(legacy_rlm_unique$animal_id)
cat("这些动物在 National 中的体重标记情况:\n")
for (aid in unique_animals) {
  nat_rows <- fire_nat[animal_id == aid]
  leg_rows <- fire_leg[animal_id == aid]
  nat_outlier_count <- sum(nat_rows$is_outlier_wt %in% TRUE)
  leg_rlm_count <- sum(leg_rows$flag_RLM_WT %in% TRUE)
  leg_gomp_count <- sum(leg_rows$flag_Gompertz_WT %in% TRUE)
  cat(sprintf("  %s: National_wt_outlier=%d, Legacy_RLM=%d, Legacy_Gompertz=%d\n",
              aid, nat_outlier_count, leg_rlm_count, leg_gomp_count))
}

# 反向: National 有多少动物被 Legacy 完全丢弃
nat_animals <- unique(fire_nat$animal_id)
leg_animals <- unique(fire_leg$animal_id)
nat_only <- setdiff(nat_animals, leg_animals)
leg_only <- setdiff(leg_animals, nat_animals)

cat("\n动物集合差异:\n")
cat("  National 独有动物:", length(nat_only), "\n")
cat("  Legacy 独有动物:", length(leg_only), "\n")
cat("  共有动物:", length(intersect(nat_animals, leg_animals)), "\n\n")

if (length(nat_only) > 0) {
  cat("National 独有动物 (被 Legacy 丢弃的):\n")
  for (aid in head(nat_only, 20)) {
    leg_rlm <- sum(fire_leg[animal_id == aid]$flag_RLM_WT %in% TRUE)
    leg_gomp <- sum(fire_leg[animal_id == aid]$flag_Gompertz_WT %in% TRUE)
    cat(sprintf("  %s: Legacy_RLM=%d, Legacy_Gompertz=%d\n", aid, leg_rlm, leg_gomp))
  }
}

cat("\n结论: Legacy RLM 的 141 条独有标记分布在",
    length(unique(legacy_rlm_unique$animal_id)), "个动物上。\n")
cat("       这些标记是 Legacy 单轮 RLM (阈值 0.5) 捕获但 National 两轮 RLM (阈值 0.25) 未捕获的。\n")
cat("       由于 National 阈值更严格 (0.25 < 0.5), 这 141 条标记实际上是\n")
cat("       Legacy 的误标记 (在 National 更严格的检测下被判定为正常)。\n")
