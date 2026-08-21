######### 验证 LMM 校正修复 #########
# 目的: 验证改动 1 (abs(β) -> -β) 与改动 2 (6kg 标记化 flag_daily_feed_over_limit)
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
  national_standard = list(
    test_weight_range = c(200, 20)
  )
)

cat("=== 运行 YANGXIANG 完整流水线 ===\n")
result <- run_zhen_measure(
  data_path = data_path,
  data_type = "YANGXIANG",
  format_path = format_path,
  config = custom_config,
  phenotype_method = "report",
  stage_mode = "weight",
  target_weight_stages = "YANGXIANG"
)

daily <- data.table::as.data.table(result$daily_records)

cat("\n=== 验证 1: flag_daily_feed_over_limit 列是否存在 ===\n")
if ("flag_daily_feed_over_limit" %in% names(daily)) {
  n_over_limit <- sum(daily$flag_daily_feed_over_limit, na.rm = TRUE)
  n_total <- nrow(daily)
  cat(sprintf("  [OK] 列存在。标记为超限的天数: %d / %d (%.1f%%)\n",
              n_over_limit, n_total, n_over_limit / n_total * 100))
} else {
  cat("  [FAIL] flag_daily_feed_over_limit 列不存在!\n")
}

cat("\n=== 验证 2: daily_feed_g 范围与 NA 情况 ===\n")
cat(sprintf("  daily_feed_g 有效范围: %.0f ~ %.0f g\n",
            min(daily$daily_feed_g, na.rm = TRUE),
            max(daily$daily_feed_g, na.rm = TRUE)))
cat(sprintf("  daily_feed_g NA 数: %d (这些将被插补)\n",
            sum(is.na(daily$daily_feed_g))))
cat(sprintf("  插补标记 is_imputed_feed: %d\n",
            sum(daily$is_imputed_feed, na.rm = TRUE)))

cat("\n=== 验证 3: 超限天与 NA 的对应关系 ===\n")
# 超限的天 daily_feed_g 应为 NA (等待插补)
over_limit_days <- daily[flag_daily_feed_over_limit == TRUE]
cat(sprintf("  超限天数: %d\n", nrow(over_limit_days)))
cat(sprintf("  其中 daily_feed_g 为 NA 的: %d\n", sum(is.na(over_limit_days$daily_feed_g))))

cat("\n=== 验证 4: 表型结果 ===\n")
cat(sprintf("  表型个体数: %d\n", nrow(result$phenotypes)))
cat(sprintf("  表型行数: %d\n", nrow(result$phenotypes)))

cat("\n=== 验证完成 ===\n")
