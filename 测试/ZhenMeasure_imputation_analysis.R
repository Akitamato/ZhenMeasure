######### ZhenMeasure 数据填补(缺失值插补) 模块评估与可视化分析脚本 #########
rm(list = ls())

# 检查依赖包
required_pkgs <- c("data.table", "MASS", "readxl", "lubridate", "zoo", "ggplot2", "gridExtra", "imputeTS")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) install.packages(missing, repos="https://mirrors.tuna.tsinghua.edu.cn/CRAN/")

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
cat("============== 缺失值推断与填补 (Imputation) 效果评估 ==============\n")
cat("====================================================================\n")

# 1. 运行核心流水线直到日聚合步骤
cat("\n>>> 正在运行流水线的前置模块 (读数 -> 整体QC -> 体重/采食QC -> 日汇总)...\n")
dt <- ZhenM_read_data(data_path, data_type, format_path, birth_info_path)
dt <- ZhenM_qc_overall(dt, config = cfg, qc_method = qc_method, keep_ids = NULL)$records
dt <- ZhenM_qc_weight_standard(dt, qc_method = qc_method, config = cfg)
dt <- ZhenM_qc_feed_standard(dt, qc_method = qc_method, config = cfg)

# 到日汇总层面，会有部分天数是没有记录(NA)的
daily_data_pre <- ZhenM_standard_to_daily_filtered(dt)

# 2. 执行双填补 (体重 & 采食量)
cat("\n>>> 正在执行缺失数据填补算法 (Kalman Filter 结构时间序列模型 & 物理兜底)...\n")
daily_data_post <- ZhenM_impute_data(daily_data_pre, impute_method = qc_method, config = cfg)


# ======================================================================== #
# ==================== 填补效果提取与分析 ============================== #
# ======================================================================== #
cat("\n====================================================================\n")
cat("📊 数据填补效果统计\n")
cat("====================================================================\n")

if (!"is_imputed_wt" %in% names(daily_data_post) || !"is_imputed_feed" %in% names(daily_data_post)) {
  stop("❌ 填补字段未生成，请检查 ZhenM_impute_data 模块！")
}

n_total <- nrow(daily_data_post)
n_wt_imputed <- sum(daily_data_post$is_imputed_wt, na.rm = TRUE)
n_feed_imputed <- sum(daily_data_post$is_imputed_feed, na.rm = TRUE)

cat(sprintf("全群体总日记录数 (含缺失日): %d 行\n", n_total))
cat(sprintf("✅ 成功填补【空缺体重】的数据量: %d 行 (占整体 %.2f%%)\n", n_wt_imputed, n_wt_imputed / n_total * 100))
cat(sprintf("✅ 成功填补【空缺采食量】的数据量: %d 行 (占整体 %.2f%%)\n", n_feed_imputed, n_feed_imputed / n_total * 100))

neg_wt_count <- sum(daily_data_post$daily_weight_g < 0 & daily_data_post$is_imputed_wt, na.rm=TRUE)
neg_feed_count <- sum(daily_data_post$daily_feed_g < 0 & daily_data_post$is_imputed_feed, na.rm=TRUE)
cat(sprintf("\n⚠️ 异常填埋检查 (是否填补出现负数):\n"))
cat(sprintf("   -> 填因为负数的体重数量: %d 行\n", neg_wt_count))
cat(sprintf("   -> 填因为负数的采食量数量: %d 行\n", neg_feed_count))

# 倒挂分析 (时序允许的合理生理波动)
df_inv <- data.table::copy(daily_data_post)
data.table::setorder(df_inv, animal_id, record_date)
df_inv[, next_real_wt := zoo::na.locf(ifelse(!is_imputed_wt, daily_weight_g, NA), fromLast = TRUE, na.rm = FALSE), by = animal_id]
inv_count <- sum(df_inv$is_imputed_wt == TRUE & !is.na(df_inv$next_real_wt) & df_inv$daily_weight_g > df_inv$next_real_wt, na.rm=TRUE)
cat(sprintf("\n📈 生理震荡/时序倒挂评估 (填补的昨日体重 > 今日实际观测):\n"))
cat(sprintf("   -> 发生合理倒挂弹性值的次数: %d 次\n", inv_count))

# ----------------- 附加输出: 图表可视化分析 ----------------- #
cat("\n====================================================================\n")
cat("📈 生成分布与插值曲线可视化\n")
cat("====================================================================\n")

plot_out_dir <- file.path(base_dir, "demo_output", paste0(data_type, "_imputation_analysis"))
if (!dir.exists(plot_out_dir)) dir.create(plot_out_dir, recursive = TRUE)

# 准备画图数据
daily_data_post[, weight_kg := daily_weight_g / 1000]
daily_data_post[, feed_kg := daily_feed_g / 1000]

plot_dt_wt <- daily_data_post[!is.na(weight_kg), .(weight_kg, is_imputed_wt)]
plot_dt_wt[, Status := ifelse(is_imputed_wt, "Imputed (Filled)", "Actual (Original)")]

plot_dt_fd <- daily_data_post[!is.na(feed_kg), .(feed_kg, is_imputed_feed)]
plot_dt_fd[, Status := ifelse(is_imputed_feed, "Imputed (Filled)", "Actual (Original)")]

# 图1. 体重填补数据的分布图
p_wt_dist <- ggplot2::ggplot(plot_dt_wt, ggplot2::aes(x = weight_kg, fill = Status)) +
  ggplot2::geom_density(alpha = 0.5, color = "white") +
  ggplot2::scale_fill_manual(values = c("Actual (Original)" = "#1f77b4", "Imputed (Filled)" = "#ff7f0e")) +
  ggplot2::labs(
    title = "Body Weight: Actual vs Imputed Distribution",
    x = "Daily Weight (kg)", y = "Density"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

# 图2. 采食量填补数据的分布图
p_fd_dist <- ggplot2::ggplot(plot_dt_fd, ggplot2::aes(x = feed_kg, fill = Status)) +
  ggplot2::geom_density(alpha = 0.5, color = "white") +
  ggplot2::scale_fill_manual(values = c("Actual (Original)" = "#2ca02c", "Imputed (Filled)" = "#d62728")) +
  ggplot2::labs(
    title = "Daily Feed Intake: Actual vs Imputed Distribution",
    x = "Daily Feed Intake (kg)", y = "Density"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")


# 寻找一只发生了大量插值的"代表猪"来绘制成长曲线和采食曲线
imputed_stats <- daily_data_post[, .(n_imp = sum(is_imputed_wt) + sum(is_imputed_feed)), by = animal_id]
top_animal <- imputed_stats[order(-n_imp)]$animal_id[1]

animal_dt <- daily_data_post[animal_id == top_animal]
animal_dt[, Status_Wt := ifelse(is_imputed_wt, "Imputed", "Actual")]
animal_dt[, Status_Fd := ifelse(is_imputed_feed, "Imputed", "Actual")]

# 图3. 单个体体重插值曲线追踪
p_curve_wt <- ggplot2::ggplot(animal_dt[!is.na(weight_kg)], ggplot2::aes(x = record_date, y = weight_kg)) +
  ggplot2::geom_line(color = "gray50", linetype = "dashed") +
  ggplot2::geom_point(ggplot2::aes(color = Status_Wt, size = Status_Wt)) +
  ggplot2::scale_color_manual(values = c("Actual" = "#1f77b4", "Imputed" = "#ff7f0e")) +
  ggplot2::scale_size_manual(values = c("Actual" = 1.5, "Imputed" = 3)) +
  ggplot2::labs(
    title = sprintf("Weight Trajectory for Animal: %s", top_animal),
    subtitle = "Trace showing how missing gaps were mathematically bridged",
    x = "Date", y = "Weight (kg)"
  ) +
  ggplot2::theme_bw()

# 图4. 单个体采食量插值曲线追踪
p_curve_fd <- ggplot2::ggplot(animal_dt[!is.na(feed_kg)], ggplot2::aes(x = record_date, y = feed_kg)) +
  ggplot2::geom_line(color = "gray50", alpha = 0.5) +
  ggplot2::geom_point(ggplot2::aes(color = Status_Fd, shape = Status_Fd), size = 2.5) +
  ggplot2::scale_color_manual(values = c("Actual" = "#2ca02c", "Imputed" = "#d62728")) +
  ggplot2::labs(
    title = sprintf("Feed Trajectory for Animal: %s", top_animal),
    x = "Date", y = "Feed Intake (kg)"
  ) +
  ggplot2::theme_bw()

# 保存图表
dist_wt_path <- file.path(plot_out_dir, "01_weight_imputation_density.png")
dist_fd_path <- file.path(plot_out_dir, "02_feed_imputation_density.png")
curve_wt_path <- file.path(plot_out_dir, "03_single_animal_weight_trajectory.png")
curve_fd_path <- file.path(plot_out_dir, "04_single_animal_feed_trajectory.png")

ggplot2::ggsave(filename = dist_wt_path, plot = p_wt_dist, width = 8, height = 6, dpi = 140)
ggplot2::ggsave(filename = dist_fd_path, plot = p_fd_dist, width = 8, height = 6, dpi = 140)
ggplot2::ggsave(filename = curve_wt_path, plot = p_curve_wt, width = 10, height = 5, dpi = 140)
ggplot2::ggsave(filename = curve_fd_path, plot = p_curve_fd, width = 10, height = 5, dpi = 140)

cat(sprintf("\n✅ 整体记录 [体重] 填补平滑分布图已保存 ➜   %s\n", dist_wt_path))
cat(sprintf("✅ 整体记录 [采食量] 填补平滑分布图已保存 ➜ %s\n", dist_fd_path))
cat(sprintf("✅ 单体猪只 [体重成长的缝补曲线] 已追踪保存 ➜ %s\n", curve_wt_path))
cat(sprintf("✅ 单体猪只 [每日食量空缺插值] 已追踪保存 ➜   %s\n", curve_fd_path))

cat("\n🎉 插补测试圆满结束！生成的个体随日期的连线图，可以极佳地展示模型是如何自动推断缺失值的！\n")
