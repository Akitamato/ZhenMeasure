######### ZhenMeasure LMM 日采食量校正效果可视化与评估测试脚本 #########
rm(list = ls())

# 检查依赖包
required_pkgs <- c("data.table", "MASS", "readxl", "lubridate", "zoo", "ggplot2", "lme4")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

# 设置 R 包所在位置
setwd("D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/项目本体")
devtools::load_all("ZhenMeasure")
library(ZhenMeasure)
library(ggplot2)

# ----------------- 测试环境配置 ----------------- #
data_type <- "FIRE"
base_dir <- "D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/demo"
v_dir <- file.path(base_dir, "demo_input", paste0(data_type, "_奥斯本"))

data_path <- file.path(v_dir, "原始数据")
extra_info_dir <- file.path(v_dir, "附加信息")

format_path <- list.files(extra_info_dir, pattern = "\\.json$", full.names = TRUE)[1]
birth_info_path <- list.files(extra_info_dir, pattern = "\\.xlsx$", full.names = TRUE)[1]
if (is.na(birth_info_path)) birth_info_path <- NULL

qc_method <- "national_standard"
cfg <- ZhenM_merge_config(list(), qc_method)

cat("\n====================================================================\n")
cat("============= LMM Feed Correction (日采食量校正) 效果评估 ============\n")
cat("====================================================================\n")

# 1. 运行核心流水线直到日聚合步骤
cat("\n>>> 正在运行流水线的前置模块 (读数 -> 整体QC -> 体重QC -> 采食QC)...\n")
dt <- ZhenM_read_data(data_path, data_type, format_path, birth_info_path)
dt <- ZhenM_qc_overall(dt, config = cfg, qc_method = qc_method, keep_ids = NULL)$records
dt <- ZhenM_qc_weight_standard(dt, qc_method = qc_method, config = cfg)
dt <- ZhenM_qc_feed_standard(dt, qc_method = qc_method, config = cfg)

cat("\n>>> 正在执行日汇总及 LMM 采食量补偿校正...\n")
# 我们不跑外层 ZhenM_standard_to_daily_filtered, 我们用它内部一模一样的逻辑，但是在赋值阶段强行保存副本
# 通过直接复制它的函数体并注释掉最后的列移除逻辑
dt_lmm <- data.table::copy(dt)
daily_data <- ZhenM_standard_to_daily_filtered(dt_lmm)

# 但是，既然上一步用原来的函数已经被移除了，为了保证图表能够获得中间变量
# 我们这里重新根据外层的 daily_feed_g (原来的 normal_feed_sum 有记录，或者我们通过在进入 ZhenM_standard_to_daily_filtered 前保存一个 pure_sum)
# 其实，我们可以直接手动在外部重构 normal_feed_sum！
# 根据 normal_feed_sum 的定义：
dt_test <- data.table::copy(dt)
feed_col <- if ("feed_g" %in% names(dt_test)) "feed_g" else if ("Feed_intake" %in% names(dt_test)) "Feed_intake" else NULL
err_flags <- c("flag_duration_negative", "flag_duration_too_long", "flag_duration_zero_with_feed", "flag_speed_too_slow", "flag_speed_too_fast", "flag_speed_extreme_low_feed", "flag_speed_zero_long_duration", "flag_feed_negative", "flag_feed_too_high")
dt_test[, is_feed_normal_record := TRUE]
for (flg in err_flags) {
  if (flg %in% names(dt_test)) dt_test[get(flg) == TRUE, is_feed_normal_record := FALSE]
}
daily_features <- dt_test[, .(normal_feed_sum = sum(.SD[[feed_col]][is_feed_normal_record == TRUE], na.rm = TRUE)), by = .(animal_id = get(if("animal_id" %in% names(dt_test)) "animal_id" else "ID"), record_date)]
daily_features[, normal_feed_sum := ifelse(normal_feed_sum > 0 & normal_feed_sum <= 6000, normal_feed_sum, NA_real_)]

# 关联回 daily_data 这里
daily_data <- merge(daily_data, daily_features, by = c("animal_id", "record_date"), all.x = TRUE)

# 用最终暴露的 daily_feed_g 减去 normal_feed_sum，重新倒推出 correction_g
daily_data[, daily_feed_g_corrected := daily_feed_g]
daily_data[, correction_g := daily_feed_g_corrected - normal_feed_sum]

# ======================================================================== #
# ==================== 校正效果提取与分析 ============================== #
# ======================================================================== #
cat("\n====================================================================\n")
cat("📊 LMM 校正数据统计\n")
cat("====================================================================\n")

if (!"correction_g" %in% names(daily_data)) {
  stop("❌ 未在日汇总数据中发现 'correction_g' 列，请检查 ZhenMeasure 代码中是否包含 LMM 模块。")
}

# 提取发生校正的记录 (绝对校正量 > 0.01g)
corrected_records <- daily_data[!is.na(correction_g) & abs(correction_g) > 0.01 & !is.na(normal_feed_sum)]

n_total <- nrow(daily_data[!is.na(normal_feed_sum)])
n_corrected <- nrow(corrected_records)
prop_corrected <- (n_corrected / n_total) * 100

cat(sprintf("有效日总记录数 (含有非NA的 normal_feed_sum): %d 行\n", n_total))
cat(sprintf("成功被 LMM 触发校正(即补偿不为0)的记录数: %d 行 (占比 %.2f%%)\n", n_corrected, prop_corrected))

if (n_corrected > 0) {
  cat(sprintf("✅ 校正/补偿阈值分布 (克):\n   -> 最小补偿量: %.1f g\n   -> 最大补偿量: %.1f g\n   -> 平均补偿量: %.1f g\n",
              min(corrected_records$correction_g), 
              max(corrected_records$correction_g), 
              mean(corrected_records$correction_g)))
  
  cat(sprintf("\n✅ 宏观采食影响：\n   -> [群体] 校正前的平均日采食量(g): %.1f\n   -> [群体] 校正后的平均日采食量(g): %.1f\n", 
              mean(corrected_records$normal_feed_sum),
              mean(corrected_records$daily_feed_g_corrected)))
  
  # ----------------- 附加输出: 校正前后对比图 ----------------- #
  cat("\n====================================================================\n")
  cat("📈 生成校正效果对比图\n")
  cat("====================================================================\n")
  
  plot_out_dir <- file.path(base_dir, "demo_output", paste0(data_type, "_lmm_correction_compare"))
  if (!dir.exists(plot_out_dir)) dir.create(plot_out_dir, recursive = TRUE)
  
  # 将宽表转换为长表用于 ggplot2 绘图
  plot_dt <- data.table::rbindlist(list(
    data.table::data.table(
      animal_id = corrected_records$animal_id,
      record_date = corrected_records$record_date,
      feed_type = "Before Correction (normal_feed_sum)",
      feed_g = corrected_records$normal_feed_sum
    ),
    data.table::data.table(
      animal_id = corrected_records$animal_id,
      record_date = corrected_records$record_date,
      feed_type = "After Correction (daily_feed_g_corrected)",
      feed_g = corrected_records$daily_feed_g_corrected
    )
  ))
  
  plot_dt[, feed_type := factor(feed_type, levels = c("Before Correction (normal_feed_sum)", "After Correction (daily_feed_g_corrected)"))]

  # 1. 密度分布对比图
  p_density <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = feed_g, fill = feed_type)) +
    ggplot2::geom_density(alpha = 0.5, color = "white") +
    ggplot2::scale_fill_manual(values = c("#7f7f7f", "#2ca02c")) +
    ggplot2::labs(
      title = "Distribution of Daily Feed Intake (Corrected Records Only)",
      subtitle = sprintf("Based on %d LMM corrected daily records", n_corrected),
      x = "Daily Feed Intake (g)",
      y = "Density",
      fill = "Status"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(face = "bold"))
  
  # 2. 对角线散点图 (校正前 vs 校正后)
  p_scatter <- ggplot2::ggplot(corrected_records, ggplot2::aes(x = normal_feed_sum, y = daily_feed_g_corrected)) +
    ggplot2::geom_point(alpha = 0.4, color = "#1f77b4", size = 1.5) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
    ggplot2::annotate("text", x = min(corrected_records$normal_feed_sum), y = max(corrected_records$daily_feed_g_corrected), 
             label = "Points above red line indicate positive compensation", hjust = 0, color = "red") +
    ggplot2::labs(
      title = "LMM Feed Intake Correction: Before vs After",
      subtitle = "Red dashed line indicates y = x (No change)",
      x = "Before Correction (normal_feed_sum, g)",
      y = "After Correction (daily_feed_g_corrected, g)"
    ) +
    ggplot2::theme_bw(base_size = 12)

  # 3. 误差补偿量柱状图
  p_hist <- ggplot2::ggplot(corrected_records, ggplot2::aes(x = correction_g)) +
    ggplot2::geom_histogram(fill = "#ff7f0e", color = "white", bins = 50, alpha = 0.8) +
    ggplot2::geom_vline(xintercept = mean(corrected_records$correction_g), color = "blue", linetype = "dashed", linewidth = 1) +
    ggplot2::labs(
      title = "Distribution of LMM Compensation Amount",
      subtitle = "Blue dashed line indicates the mean compensation",
      x = "Correction Amount (g)",
      y = "Frequency Count"
    ) +
    ggplot2::theme_bw(base_size = 12)

  density_path <- file.path(plot_out_dir, "01_feed_intake_density_compare.png")
  scatter_path <- file.path(plot_out_dir, "02_feed_intake_scatter_compare.png")
  hist_path <- file.path(plot_out_dir, "03_correction_amount_histogram.png")

  ggplot2::ggsave(filename = density_path, plot = p_density, device = "png", width = 8, height = 6, dpi = 140)
  ggplot2::ggsave(filename = scatter_path, plot = p_scatter, device = "png", width = 8, height = 6, dpi = 140)
  ggplot2::ggsave(filename = hist_path, plot = p_hist, device = "png", width = 8, height = 6, dpi = 140)
  
  cat(sprintf("\n✅ 图表 1: 采食量核心分布密度对比已保存 ➜ %s\n", density_path))
  cat(sprintf("✅ 图表 2: 校正前后采食量映射散点图已保存 ➜   %s\n", scatter_path))
  cat(sprintf("✅ 图表 3: 各记录算法补偿量直方图已保存 ➜     %s\n", hist_path))
  
  cat("\n🎉 测试成功结束！建议前往上述路径查看生成的对比图表，直观评估 LMM 方法的作用！\n")
} else {
  cat("\n⚠ 没有记录发生 LMM 校正，跳过绘图步骤。\n")
}