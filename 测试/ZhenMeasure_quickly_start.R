######### ZhenMeasure quickly start #########
rm(list = ls())
# 检查R版本
R.version.string  # 应该 >= 4.4.3

# 检查依赖包
required_pkgs <- c("data.table", "MASS", "readxl", "lubridate", "zoo", "lme4", "imputeTS")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

# R包所在位置
setwd("D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/项目本体")
devtools::load_all("ZhenMeasure")

library(ZhenMeasure) ### 会出现一些警告别在意，因为我还没有修改加载信息

# 2. 设置路径 (自动批量遍历所有格式与批次)
demo_base_dir <- "D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/demo"
demo_input_dir <- file.path(demo_base_dir, "demo_input")
demo_output_dir <- file.path(demo_base_dir, "demo_output")

# 获取所有厂商的目录 (如 YANGXIANG_扬翔, FIRE_奥斯本, Nedap_睿保乐)
vendor_dirs <- list.dirs(demo_input_dir, recursive = FALSE)

if (length(vendor_dirs) == 0) {
  stop("在 demo_input 中未找到厂商文件夹，请检查路径。")
}

##### 正式批量测试 ##########

for (v_dir in vendor_dirs) {
    vendor_folder_name <- basename(v_dir)
    # 从文件夹名提取 data_type（提取下划线前面的英文品牌，强制大写）
    data_type <- toupper(strsplit(vendor_folder_name, "_")[[1]][1])
    
    cat("\n========================================================\n")
    cat(sprintf("🎯 发现待测数据：[%s], 识别设备类型：[%s]\n", vendor_folder_name, data_type))
    cat("========================================================\n")
    
    # --- 1. 获取附加信息配置（格式JSON和出生日志） ---
    extra_info_dir <- file.path(v_dir, "附加信息")
    
    format_path <- list.files(extra_info_dir, pattern = "\\.json$", full.names = TRUE)[1]
    if (is.na(format_path)) {
        format_path <- NULL
        cat("  ⚠️ 警告: 未找到对应的 .json 格式配置文件！\n")
    }
    
    birth_info_path <- list.files(extra_info_dir, pattern = "\\.xlsx$", full.names = TRUE)[1]
    if (is.na(birth_info_path)) {
        birth_info_path <- NULL
    }
    
    keep_ids_path <- file.path(extra_info_dir, "keep_fire_ids.txt")
    if (!file.exists(keep_ids_path)) {
        keep_ids_path <- NULL
    }
    
    # --- 2. 确定数据输入和输出路径 ---
    raw_data_dir <- file.path(v_dir, "原始数据")
    
    # 不再分子批次，所有子文件夹下的数据将被当成一个整体（由内部函数自动递归读取）
    output_dir <- file.path(demo_output_dir, vendor_folder_name, "汇总分析")
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    run_ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- file.path(output_dir, paste0("test_log_", run_ts, ".txt"))
    
    cat(sprintf("\n>>> 【开始处理】 %s 下的所有数据\n", vendor_folder_name))
    cat("  数据路径:", raw_data_dir, "\n")
    cat("  格式配置:", ifelse(is.null(format_path), "无", format_path), "\n")
    cat("  出生日志:", ifelse(is.null(birth_info_path), "无", birth_info_path), "\n")
    cat("  靶向保留:", ifelse(is.null(keep_ids_path), "默认保留全部", keep_ids_path), "\n")
    cat("  日志文件:", log_file, "\n")
    
    # --- 3. 运行完整流程 ---
    # 自定义配置
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
    
    result <- tryCatch({
        run_zhen_measure(data_path = raw_data_dir,
                   data_type = data_type,
                   format_path = format_path,
                   birth_info_path = birth_info_path,
                   qc_method = "national_standard",
                   phenotype_method = current_phenotype_method,
                   stage_mode = current_stage_mode,
                   target_weight_stages = current_target_weight_stages,
                   target_age_stages = NULL,
                   target_date_stages = current_target_date_stages,
                   output_dir = output_dir,
                   config = custom_config,           # 传入自定义 config
                   keep_ids = NULL,         # keep_ids_path传入待保留个体ID
                   growth_curve = FALSE,
                   growth_curve_test = TRUE,
                   log_file = log_file
        ) 
    }, error = function(e) {
        cat("❌ 运行出错 [", vendor_folder_name, "]:\n", conditionMessage(e), "\n")
        return(NULL)
    })
    
    if (is.null(result)) {
        cat("  ⚠️ 本次测试失败跳过，继续下一批。\n")
        # 不直接 quit() 使得脚本能跑完后面所有测试
    } else {
        cat("  ✅ run_zhen_measure 执行成功\n")
        if (!is.null(result$raw_daily_records)) {
            cat(sprintf("  📊 原始日汇总(对比用): %d 行 | 异常体重天数: %d | 异常采食天数: %d | 填充体重天数: %d | 填充采食天数: %d\n",
                nrow(result$raw_daily_records),
                sum(result$raw_daily_records$day_has_outlier_wt, na.rm = TRUE),
                sum(result$raw_daily_records$day_has_outlier_feed, na.rm = TRUE),
                sum(result$raw_daily_records$day_is_imputed_wt, na.rm = TRUE),
                sum(result$raw_daily_records$day_is_imputed_feed, na.rm = TRUE)))
        }
    }
}

cat("\n🎉 所有厂批次的格式文件均测试完毕！\n\n")





