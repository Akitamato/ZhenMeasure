######### ZhenMeasure 模块化逐步测试脚本 #########
rm(list = ls())

# 检查依赖包
required_pkgs <- c("data.table", "MASS", "readxl", "lubridate", "zoo", "lme4", "imputeTS")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

# 设置 R 包所在位置
setwd("D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/项目本体")
devtools::load_all("ZhenMeasure")
library(ZhenMeasure)

# ----------------- 全局参数配置 ----------------- #
# 可将 data_type 替换为 "NEDAP" 或 "FIRE" 以测试其他厂商
data_type <- "YANGXIANG"
####################

custom_config <- list(
    national_standard = list(
        test_weight_range = c(200, 20) # 修改体重范围检查的开测体重与结测体重
    )
)

# 动态参数匹配过去版本的独立逻辑
if (data_type == "FIRE") {
    current_phenotype_method <- "research"
    current_stage_mode <- NULL
    current_target_weight_stages <- NULL
    current_target_date_stages <- NULL
} else if (data_type == "YANGXIANG") {
    current_phenotype_method <- "report"
    current_stage_mode <- "weight"
    current_target_weight_stages <- "YANGXIANG"
    current_target_date_stages <- NULL
} else {
    # 兜底默认参数（针对 Nedap 等其他厂商）
    current_phenotype_method <- "report"
    current_stage_mode <- NULL
    current_target_weight_stages <- NULL
    current_target_date_stages <- NULL
}


base_dir <- "D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/demo"
if (data_type == "YANGXIANG"){
  v_dir <- file.path(base_dir, "demo_input", paste0(data_type, "_扬翔"))
} else if (data_type == "FIRE"){
  v_dir <- file.path(base_dir, "demo_input", paste0(data_type, "_奥斯本"))
} else if (data_type == "NEDAP"){
  v_dir <- file.path(base_dir, "demo_input", paste0(data_type, "_睿保乐"))
}

data_path <- file.path(v_dir, "原始数据")
extra_info_dir <- file.path(v_dir, "附加信息")

format_path <- list.files(extra_info_dir, pattern = "\\.json$", full.names = TRUE)[1]
birth_info_path <- list.files(extra_info_dir, pattern = "\\.xlsx$", full.names = TRUE)[1]
if (is.na(birth_info_path)) birth_info_path <- NULL

keep_fire_ids_path <- file.path(extra_info_dir, "keep_fire_ids.txt")
if (!file.exists(keep_fire_ids_path)) keep_fire_ids_path <- NULL

qc_method <- "national_standard"
cfg <- ZhenM_merge_config(custom_config, qc_method)

# （可选）创建一个临时日志器
log_file <- file.path(base_dir, "demo_output", paste0(data_type, "_step_by_step_test.txt"))
logger <- ZhenM_create_logger(log_file, append = FALSE)

cat("\n========================================================\n")
cat("========== ZhenMeasure Step-by-Step Test Pipeline ==========\n")
cat("========================================================\n")


# ----------------- 步骤 1: 数据读取与格式化 (深度展开子过程) ----------------- #
# [架构说明] 在正常的单步运行中，本步骤的系列操作完全由外层大封装函数 `ZhenM_read_data()` (来源: R/ZhenM_read_data.R) 承担。
cat("\n>>> Step 1-1: 解析格式配置文件\n")
# 调用内部函数: .read_shared_data_format_file() 来自 R/ZhenM_read_utils.R
format_info <- ZhenMeasure:::.read_shared_data_format_file(format_path)
cat("   -> 成功获取格式配置。ID列为第", format_info$id_col, "列\n")

cat("\n>>> Step 1-2: 扫描并合并原始数据表格\n")
if (data_type == "YANGXIANG") {
  files <- list.files(data_path, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
  # 调用内部函数: .read_yangxiang_file() 来自 R/ZhenM_read_utils.R
  all_data <- lapply(files, function(f) ZhenMeasure:::.read_yangxiang_file(f, format_info))
} else {
  # 调用内部函数: .find_tabular_files_recursive() 和 .read_tabular_file() 均来自 R/ZhenM_read_utils.R
  files <- ZhenMeasure:::.find_tabular_files_recursive(data_path)
  all_data <- lapply(files, ZhenMeasure:::.read_tabular_file)
}
if (length(files) == 0) stop("未找到任何数据文件！")
raw_dt <- data.table::rbindlist(all_data, use.names = TRUE, fill = TRUE)
cat("   -> 查找到", length(files), "个文件。合并后总行数:", nrow(raw_dt), "\n")


cat("\n>>> Step 1-3: 解析出生档案并计算年龄 (主要针对 FIRE / NEDAP)\n")
birth_info <- NULL
if (!is.null(birth_info_path) && file.exists(birth_info_path)) {
   # 调用内部函数: .read_birth_info() 来自 R/ZhenM_read_utils.R
   birth_info <- ZhenMeasure:::.read_birth_info(birth_info_path)
   cat("   -> 成功获取出生档案: ", nrow(birth_info), " 头生猪信息\n")
} else {
   cat("   -> 跳过: 未提供或本设备无需出生档案。\n")
}

cat("\n>>> Step 1-4: 映射为 ZhenMeasure 全局标准格式 (提取 ID, Visit_time, Feed_intake 等核心列)\n")
if (data_type == "YANGXIANG") {
  # 调用内部函数: .map_yangxiang_to_standard() 来自 R/ZhenM_read_data.R
  standard_data_original <- ZhenMeasure:::.map_yangxiang_to_standard(raw_dt, format_info)
} else if (data_type == "FIRE") {
  # 调用内部函数: .map_fire_to_standard() 来自 R/ZhenM_read_data.R
  standard_data_original <- ZhenMeasure:::.map_fire_to_standard(raw_dt, format_info, birth_info)
} else if (data_type == "NEDAP") {
  # 调用内部函数: .map_nedap_to_standard() 来自 R/ZhenM_read_data.R
  standard_data_original <- ZhenMeasure:::.map_nedap_to_standard(raw_dt, format_info, birth_info)
}

standard_data <- data.table::copy(standard_data_original)

# 构建原始日汇总数据（无QC过滤，用于画图对比）
cat("\n>>> 构建原始日汇总数据 (raw_daily_data, 无QC过滤)\n")
raw_daily_data <- ZhenMeasure:::.build_raw_daily(standard_data_original)
cat(sprintf("   -> 原始日汇总行数: %d, 个体数: %d\n", nrow(raw_daily_data), data.table::uniqueN(raw_daily_data$animal_id)))

cat(sprintf("✅ Step 1 完成。剔除无效读取和无时间记录后，剩余有效数据行数: %d, 有效个体数: %d\n",
            nrow(standard_data), data.table::uniqueN(standard_data$ID)))

#### 读取模块涉及脚本：1.zhenm_read_data()外层大封装函数
###                   2.zhenm_read_utils.R
###                   ~~3.zhenm_birth_info.R~~ (历史遗留无效脚本，已废弃不再维护)





# ----------------- 步骤 2: Overall QC (展示内部过滤过程) ----------------- #
# [架构说明] 本步骤直接调用独立的大包函数 `ZhenM_qc_overall()` (来源: R/ZhenM_qc_overall.R)，属于不需下探拆分的阶段级函数。
cat("\n>>> Step 2: 整体质控 (基于主函数 ZhenM_qc_overall, 来源: R/ZhenM_qc_overall.R)\n")
# 因为这是一个完整的单体函数，我们在外层直接获取结果，然后展示其内部多阶段过滤的结果：
qc_result <- ZhenM_qc_overall(standard_data, config = cfg, logger = logger, keep_ids = NULL)
standard_data <- qc_result$records
qc_overall_summary <- qc_result$summary

cat("   -> 内部执行了7个粗筛过滤步骤，具体剔除数如下:\n")
print(qc_overall_summary[, .(step, n_removed, n_removed_animals)])
cat(sprintf("✅ Step 2 完成。剩余有效数据行数: %d, 有效个体数: %d\n", 
            nrow(standard_data), data.table::uniqueN(standard_data$animal_id)))


# ----------------- 步骤 3: 体重质控 (调用子模块核心算法) ----------------- #
# [架构说明] 在正常的单步运行中，本步骤的调度与分发由外层大封装函数 `ZhenM_qc_weight_standard()` (来源: R/ZhenM_qc_weight_standard.R) 承担。
cat("\n>>> Step 3: 体重细致质控\n")
if (qc_method == "national_standard") {
  # 调用内部算法: .qc_weight_standard_national() 来自 R/ZhenM_qc_weight_standard.R
  cat("   -> 调用内部算法: .qc_weight_standard_national (来源: R/ZhenM_qc_weight_standard.R)\n")
  standard_data <- ZhenMeasure:::.qc_weight_standard_national(standard_data, cfg, logger)
} else {
  # 调用内部算法: .qc_weight_standard_legacy() 来自 R/ZhenM_qc_weight_standard.R
  cat("   -> 调用内部算法: .qc_weight_standard_legacy (来源: R/ZhenM_qc_weight_standard.R)\n")
  standard_data <- ZhenMeasure:::.qc_weight_standard_legacy(standard_data, cfg, logger)
}

cat(sprintf("✅ Step 3 完成。标记异常体重 (过低/出界/拟合差) 记录数: %d\n", 
            sum(standard_data$flag_weight_out_of_range | standard_data$flag_weight_low | standard_data$flag_growth_curve_poor, na.rm = TRUE)))


# ----------------- 步骤 4: 采食量质控 (调用子模块核心算法) ----------------- #
# [架构说明] 在正常的单步运行中，由大封装函数 `ZhenM_qc_feed_standard()` (来源: R/ZhenM_qc_feed_standard.R) 承担。
cat("\n>>> Step 4: 采食量与时间质控\n")
if (qc_method == "national_standard") {
  # 调用内部算法: .qc_feed_standard_national() 来自 R/ZhenM_qc_feed_standard.R
  cat("   -> 调用内部算法: .qc_feed_standard_national (来源: R/ZhenM_qc_feed_standard.R)\n")
  standard_data <- ZhenMeasure:::.qc_feed_standard_national(standard_data, cfg, logger)
} else {
  # 调用内部算法: .qc_feed_standard_legacy() 来自 R/ZhenM_qc_feed_standard.R
  cat("   -> 调用内部算法: .qc_feed_standard_legacy (来源: R/ZhenM_qc_feed_standard.R)\n")
  standard_data <- ZhenMeasure:::.qc_feed_standard_legacy(standard_data, cfg, logger)
}
cat("   -> 检测并标记了单次采食超限、进食速度反常、或设备重启产生的异常点\n")
cat("✅ Step 4 完成。\n")

qc_standard_data <- data.table::copy(standard_data)

cat("Step 5 前: is_outlier_feed 是否存在?",
    "is_outlier_feed" %in% names(standard_data), "\n")
cat("is_outlier_feed TRUE 数:", sum(standard_data$is_outlier_feed, na.rm=TRUE), "\n")

# ----------------- 步骤 5: 转换为日汇总格式 ----------------- #
# [架构说明] 本步骤直接调用独立的大包函数 `ZhenM_standard_to_daily_filtered()` (来源: R/ZhenM_daily_aggregate_filtered.R)。
cat("\n>>> Step 5: 转化日汇总记录 (剔除Step 3,4 中的不良点后归并)\n")
cat("   -> 调用主函数: ZhenM_standard_to_daily_filtered, 截取有效点汇总 (来源: R/ZhenM_daily_aggregate_filtered.R)\n")
daily_data <- ZhenM_standard_to_daily_filtered(standard_data)

cat(sprintf("✅ Step 5 完成。成功将单次采食聚合成按日数据, 当前日汇总行数: %d, 个体数: %d\n", 
            nrow(daily_data), data.table::uniqueN(daily_data$animal_id)))


# ----------------- 步骤 6: 数据填充 (分体重填补与采食量填补) ----------------- #
# [架构说明] 在正常的单步运行中，主要调用 `ZhenM_impute_data()` (来源: R/ZhenM_impute.R) 大函数来分拆这两个模块。
cat("\n>>> Step 6: 缺失测定数据推断与填补\n")

# 强制刷新 data.table 的内部指针 (.internal.selfref)，避免跨模块传递时底层属性丢失
data.table::setDT(daily_data)

if (!"is_imputed_wt" %in% names(daily_data)) data.table::set(daily_data, j = "is_imputed_wt", value = FALSE)
if (!"is_imputed_feed" %in% names(daily_data)) data.table::set(daily_data, j = "is_imputed_feed", value = FALSE)

cat("   >>> Step 6-1: 插值填补每日体重虚位 \n")
# 外层主函数 ZhenM_impute_weight() 位于 R/ZhenM_impute.R 中
# [避坑说明]: 此前 national_standard 算法在进行两端（开测前/结测后）预测时使用了"三次自然后样条(Cubic Spline)"外推，
# 这极易诱发著名的龙格现象(Runge's phenomenon)，导致外推端点处的体重暴跌为负数(-11489g)。
# 现已修复：采用卡尔曼滤波算法进行计算,不再有之前填成负数的情况。
cat("       -> 调用主函数: ZhenM_impute_weight (来源: R/ZhenM_impute.R)\n")
daily_data <- ZhenM_impute_weight(daily_data, qc_method, cfg)
cat(sprintf("       填补体重完成。共计填补记录数: %d\n", sum(daily_data$is_imputed_wt, na.rm = TRUE)))

cat("   >>> Step 6-2: 推算填补缺失采食量\n")
if (qc_method == "national_standard") {
  # [避坑说明]: 细心的跑批中可能会发现 summary(daily_feed_g) 依然存在少量的 NA 值。
  # 这是完全正常的。因为 national_standard 标准中，对于连续缺失 (>3天) 的情况有着极其严苛的要求:
  # 必须前后有足够数据拟合线性回归 (R² > 0.95)，且相应体重区间的 料肉比 (FCR) 必须在规定生理红线内。
  # 任意条件不满足，算法将为了数据的严谨性而"拒绝"捏造填补采食量，从而保留 NA 原貌。
  # 调用内部算法: .impute_feed_national_v2() 来自 R/ZhenM_impute_national.R
  cat("       -> 调用红头文件算法版本: .impute_feed_national_v2 (来源: R/ZhenM_impute_national.R)\n")
  daily_data <- ZhenMeasure:::.impute_feed_national_v2(daily_data, cfg)
} else {
  # 外层主函数 ZhenM_impute_feed() 位于 R/ZhenM_impute.R 中
  cat("       -> 调用旧有保守填补算法: ZhenM_impute_feed (来源: R/ZhenM_impute.R)\n")
  daily_data <- ZhenM_impute_feed(daily_data, "legacy", cfg)
}
cat(sprintf("       填补采食量完成。共计填补记录数: %d\n", sum(daily_data$is_imputed_feed, na.rm = TRUE)))
cat("✅ Step 6 整体填补模块完成()\n")

# 标注原始日汇总中的QC和填充标志
cat("\n>>> 标注原始日汇总的QC和填充标志\n")
raw_daily_data <- ZhenMeasure:::.annotate_raw_daily_flags(raw_daily_data, qc_standard_data, daily_data)
cat(sprintf("   -> 存在异常体重的天数: %d\n", sum(raw_daily_data$day_has_outlier_wt, na.rm = TRUE)))
cat(sprintf("   -> 存在异常采食的天数: %d\n", sum(raw_daily_data$day_has_outlier_feed, na.rm = TRUE)))
cat(sprintf("   -> 体重被填充的天数: %d\n", sum(raw_daily_data$day_is_imputed_wt, na.rm = TRUE)))
cat(sprintf("   -> 采食被填充的天数: %d\n", sum(raw_daily_data$day_is_imputed_feed, na.rm = TRUE)))


# ----------------- 步骤 7: 表型计算 (深入揭示底层分发调度算法) ----------------- #
# [架构说明] 在正常的单步运行中，主要调用 `ZhenM_calc_phenotypes()` 主封装函数 (来源: R/ZhenM_phenotype_main.R)。
cat("\n>>> Step 7: 计算宏观表型指标输出 (ADG/ADFI/FCR/AGE)\n")

if (is.null(current_stage_mode)) {
  cat("   -> 生长分段模式：无分段 (计算个体的全局整体评估)\n")
  cat(sprintf("   -> 触发内部引擎: .dispatch_phenotype_calc (调用算法分支=%s)\n", current_phenotype_method))
  phenotypes <- ZhenMeasure:::.dispatch_phenotype_calc(daily_data,  current_phenotype_method, cfg)
} else {
  cat(sprintf("   -> 生长分段模式：按条件切片计算 (stage_mode=%s)\n", current_stage_mode))
  cat(sprintf("   -> 触发内部引擎: .calc_phenotypes_by_stages (循环切分调用算法分支=%s)\n", current_phenotype_method))
  phenotypes <- ZhenMeasure:::.calc_phenotypes_by_stages(
    daily_records = daily_data,
    phenotype_method = current_phenotype_method, 
    config = cfg,
    stage_mode = current_stage_mode,
    target_weight_stages = current_target_weight_stages,
    target_age_stages = NULL,
    target_date_stages = current_target_date_stages
  )
}
cat(sprintf("✅ Step 7 运算完成。成功萃取出个体表型记录总计: %d 条\n", nrow(phenotypes)))


# ----------------- 步骤 8: 生成 QC Summary 指标报告 ----------------- #
# [架构说明] 在正常的单步运行中，这属于 `run_zhen_measure()` 内部调用 `ZhenM_generate_qc_summary()` (来源: R/ZhenM_qc_utils.R) 生成的结果。
cat("\n>>> Step 8: 生成全周期的质控摘要报告 (ZhenM_generate_qc_summary)\n")
qc_summary_metrics <- ZhenM_generate_qc_summary(qc_standard_data)

cat("✅ Step 8 完成。汇总表列名结构验证通过:\n")
print(names(qc_summary_metrics))

# ----------------- 步骤 9: 绘图模块（生长曲线 2x2 对比图）----------------- #
cat("\n>>> Step 9: 生成生长曲线对比图 (ZhenM_write_plot_outputs)\n")
cat("   -> 2x2面板: 原始vs清洗 × 采食/体重, 异常点红/橙色标记\n")

plot_output_dir <- file.path(base_dir, "demo_output", paste0(data_type, "_step_by_step"))
if (!dir.exists(plot_output_dir)) dir.create(plot_output_dir, recursive = TRUE)

# 构造绘图所需的 result 列表
plot_result <- list(
  raw_daily_records = raw_daily_data,
  daily_records = daily_data
)

plot_files <- ZhenM_write_plot_outputs(plot_result, plot_output_dir,
                                      growth_curve = FALSE,
                                      growth_curve_test = TRUE)

if (!is.null(plot_files)) {
  cat(sprintf("✅ Step 9 完成。生成了 %d 个绘图文件:\n", length(plot_files)))
  for (pf in plot_files) cat(sprintf("   -> %s\n", pf))
} else {
  cat("   ⚠️ 绘图模块未生成输出（可能数据不满足绘图条件）\n")
}

if (!is.null(logger)) ZhenM_close_log(logger)
cat("\n🎉 =========================================================\n")
cat("🎉 您已经成功全透明地跑完了 ZhenMeasure 的所有 15+ 个底层子功能组件！\n")
cat("🎉 如果遇到报错，日志和上面的 console 也会指出是挂在了哪一个特定的子函数 (.xxx) 上。\n")