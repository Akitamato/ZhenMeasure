######### ZhenMeasure 质控方法优劣背靠背对比测试脚本 #########
rm(list = ls())

# 检查依赖包
required_pkgs <- c("data.table", "MASS", "readxl", "lubridate", "zoo", "ggplot2")
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

keep_fire_ids_path <- file.path(extra_info_dir, "keep_fire_ids.txt")
if (!file.exists(keep_fire_ids_path)) keep_fire_ids_path <- NULL

# 使用默认参数，同时确保 legacy 也使用到了 polynomial RLM（已在内部修改）
custom_config <- list()
cfg_nat <- ZhenM_merge_config(custom_config, "national_standard")
cfg_leg <- ZhenM_merge_config(custom_config, "legacy")

cat("\n====================================================================\n")
cat("========== national_standard VS legacy 质控策略优劣对比测试 ==========\n")
cat("====================================================================\n")

# 1. 统一读取基准数据
cat("\n>>> 步骤 1: 读取所有原始数据并统一格式映射\n")
standard_data_original <- ZhenM_read_data(data_path, data_type, format_path, birth_info_path)
n_raw <- nrow(standard_data_original)
n_raw_animals <- data.table::uniqueN(standard_data_original$ID)
cat(sprintf("基准读入完成：总记录条数 %d，包含生猪头数 %d\n", n_raw, n_raw_animals))


# ======================================================================== #
# ==================== 方法 A: national_standard ========================= #
# ======================================================================== #
cat("\n====================================================================\n")
cat("  ▶ 执行方法 A: national_standard (特点：极端干净，容错低，连坐制)\n")
dt_nat <- data.table::copy(standard_data_original)

# Overall QC (国标版)
qc_overall_nat <- ZhenM_qc_overall(dt_nat, config = cfg_nat, qc_method = "national_standard", keep_ids = NULL)
dt_nat <- qc_overall_nat$records
overall_removed_nat <- sum(qc_overall_nat$summary$n_removed)
overall_animals_rem_nat <- sum(qc_overall_nat$summary$n_removed_animals)

# Weight QC (国标版)
dt_nat <- ZhenM_qc_weight_standard(dt_nat, qc_method = "national_standard", config = cfg_nat)
weight_outlier_nat <- sum(dt_nat$is_outlier_wt, na.rm = TRUE)

# 由于 National_Standard 的“连坐制”，一旦 R² 不达标或少于连续10日有效点，该头猪整头作废
animals_lost_nat <- sum(unique(dt_nat$flag_growth_curve_poor))

cat(sprintf("【方法A 战报】\n"))
cat(sprintf("  - Overall 阶段过滤条数: %d (影响完整动物数: %d)\n", overall_removed_nat, overall_animals_rem_nat))
cat(sprintf("  - Weight 阶段标记异常点数: %d\n", weight_outlier_nat))
cat(sprintf("  - 因 R² 不达标或数据过少被【整猪连坐作废】头数: %d\n", animals_lost_nat))


# ======================================================================== #
# ==================== 方法 B: legacy (重构进阶版) ======================= #
# ======================================================================== #
cat("\n====================================================================\n")
cat("  ▶ 执行方法 B: legacy (特点：模块化粗筛+高级稳健容错模型，尽力挽回)\n")
dt_leg <- data.table::copy(standard_data_original)

# Overall QC (科研版)
qc_overall_leg <- ZhenM_qc_overall(dt_leg, config = cfg_leg, qc_method = "legacy", keep_ids = NULL)
dt_leg <- qc_overall_leg$records
overall_removed_leg <- sum(qc_overall_leg$summary$n_removed)
overall_animals_rem_leg <- sum(qc_overall_leg$summary$n_removed_animals)

# Weight QC (科研版 - 使用改良 polynomial 回归及 SD前置粗筛)
dt_leg <- ZhenM_qc_weight_standard(dt_leg, qc_method = "legacy", config = cfg_leg)
weight_outlier_leg <- sum(dt_leg$is_outlier_wt, na.rm = TRUE)

# Legacy 只废弃具体的异常跳点，没有连坐的整猪全删标志
animals_lost_leg <- 0

cat(sprintf("【方法B 战报】\n"))
cat(sprintf("  - Overall 阶段过滤条数: %d (影响完整动物数: %d)\n", overall_removed_leg, overall_animals_rem_leg))
cat(sprintf("  - Weight 阶段标记异常点数(含SD、多项式RLM及Gompertz): %d\n", weight_outlier_leg))
cat(sprintf("  - 因数据不足被【整猪连坐作废】头数: %d (Legacy仅标记异常天，不灭口)\n", animals_lost_leg))


cat("\n====================================================================\n")
cat("📊 最终对比点评\n")
cat("====================================================================\n")
cat(sprintf("总保留有效行数 (去除异常点后): \n  - National: %d 行 \n  - Legacy:   %d 行\n",
            nrow(dt_nat) - weight_outlier_nat,
            nrow(dt_leg) - weight_outlier_leg))

cat("\n【National Standard 总结】: 算法开销大，使用倒数加权的聚合及高阶拟合，数据绝对干净，但样本损耗高（数据质量稍差则某头猪通通不要）。\n")
cat("【Legacy (Updated) 总结】 : 先计算日均并保留局部跳变点的废弃标记，算法速度更快；引入了从国标借鉴的 'polynomial' 多项式 RLM 模型后，容错率和挽回数据的能力得到了极大增强。\n\n")


# ----------------- 附加输出: 两种方法质控后体重记录分布图 ----------------- #
cat("====================================================================\n")
cat("📈 生成体重记录分布图 (质控后有效记录)\n")
cat("====================================================================\n")

plot_out_dir <- file.path(base_dir, "demo_output", "qc_methods_compare")
if (!dir.exists(plot_out_dir)) dir.create(plot_out_dir, recursive = TRUE)

# --- 1. 准备散点图数据 (Age vs Weight)，展示异常值 ---
if (!"flag_growth_curve_poor" %in% names(dt_nat)) dt_nat[, flag_growth_curve_poor := FALSE]

scatter_nat <- dt_nat[!is.na(weight_g) & !is.na(age_day), .(
    method = "national_standard", 
    age_day, 
    weight_kg = weight_g / 1000, 
    Status = ifelse(is_outlier_wt | flag_growth_curve_poor, "Outlier (Removed)", "Valid (Retained)")
)]
scatter_leg <- dt_leg[!is.na(weight_g) & !is.na(age_day), .(
    method = "legacy", 
    age_day, 
    weight_kg = weight_g / 1000, 
    Status = ifelse(is_outlier_wt, "Outlier (Removed)", "Valid (Retained)")
)]
scatter_compare <- data.table::rbindlist(list(scatter_nat, scatter_leg), use.names = TRUE, fill = TRUE)
scatter_compare[, Status := factor(Status, levels = c("Valid (Retained)", "Outlier (Removed)"))]

# --- 2. 准备分布图数据 (仅有效记录) ---
wt_nat <- dt_nat[is_outlier_wt == FALSE & !is.na(weight_g), .(method = "national_standard", weight_kg = weight_g / 1000)]
wt_leg <- dt_leg[is_outlier_wt == FALSE & !is.na(weight_g), .(method = "legacy", weight_kg = weight_g / 1000)]
wt_compare <- data.table::rbindlist(list(wt_nat, wt_leg), use.names = TRUE, fill = TRUE)

if (nrow(scatter_compare) == 0) {
    cat("⚠ 无可用于绘图的体重记录，跳过绘图。\n")
} else {
    # 绘制异常值散点对比图
    p_scatter <- ggplot2::ggplot(scatter_compare, ggplot2::aes(x = age_day, y = weight_kg, color = Status)) +
        ggplot2::geom_point(alpha = 0.5, size = 1) +
        ggplot2::scale_color_manual(values = c("Valid (Retained)" = "#1f77b4", "Outlier (Removed)" = "#d62728")) +
        ggplot2::facet_wrap(~method, ncol = 1) +
        ggplot2::labs(
            title = "QC Outliers Detection: Age vs Weight",
            subtitle = "National Standard vs Legacy",
            x = "Age (Days)",
            y = "Body Weight (kg)",
            color = "Record Status"
        ) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            legend.position = "bottom"
        )

    # 绘制分布直方图 (有效记录)
    p_hist <- ggplot2::ggplot(wt_compare, ggplot2::aes(x = weight_kg, fill = method)) +
        ggplot2::geom_histogram(position = "identity", alpha = 0.45, bins = 60, color = "white") +
        ggplot2::facet_wrap(~method, ncol = 1, scales = "free_y") +
        ggplot2::labs(
            title = "Weight Distribution After QC (Valid Records Only)",
            x = "Body Weight (kg)", y = "Record Count"
        ) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(legend.position = "none")

    scatter_path <- file.path(plot_out_dir, "qc_outliers_scatter.png")
    hist_path <- file.path(plot_out_dir, "weight_distribution_hist_after_qc.png")

    ggplot2::ggsave(filename = scatter_path, plot = p_scatter, width = 10, height = 8, dpi = 140)
    ggplot2::ggsave(filename = hist_path, plot = p_hist, width = 10, height = 8, dpi = 140)

    cat(sprintf("✅ 异常值剔除散点图(红蓝对比)已保存: %s\n", scatter_path))
    cat(sprintf("✅ 有效记录分布直方图已保存: %s\n", hist_path))
}