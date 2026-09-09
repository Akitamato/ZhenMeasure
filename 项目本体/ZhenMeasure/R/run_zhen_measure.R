#' ZhenMeasure V1.0.0 pipeline
#'
#' Entry point using national standard QC method.
#'
#' @param data_path Path to raw data directory
#' @param data_type Data source type: "YANGXIANG", "NEDAP", or "FIRE"
#' @param format_path Path to Data_format.json file (only .json is supported)
#' @param birth_info_path Path to birth info file (for NEDAP/FIRE)
#' @param qc_method QC method: "national_standard" (the only supported method since V1.0.0)
#' @param phenotype_method Phenotype calculation method: "standard_fcr", "report", "monitor", "research"
#' @param stage_mode Data partitioning mode (optional): NULL (no partitioning), "weight", "age", or "date"
#' @param target_weight_stages For stage_mode="weight": weight ranges in kg (e.g., c(30, 60, 90, 120) or "YANGXIANG")
#' @param target_age_stages For stage_mode="age": age ranges in days (e.g., c(70, 100, 130, 160))
#' @param target_date_stages For stage_mode="date": date ranges (e.g., c("2024-01-01", "2024-02-01", "2024-03-01"))
#' @param output_dir Output directory path
#' @param config Optional configuration list
#' @param keep_ids Path to a text file containing animal IDs to keep (one per line). Default is NULL (keep all).
#' @param growth_curve Generate individual growth curves
#' @param growth_curve_test Generate combined growth curve
#' @param log_file Optional log file path. If provided, all messages and warnings
#'   during the run are appended to this file.
#' @return List with processed data and phenotypes
#' @export
run_zhen_measure <- function(data_path, data_type, format_path,
                       birth_info_path = NULL,
                       qc_method = "national_standard",
                       phenotype_method = NULL,
                       stage_mode = NULL,
                       target_weight_stages = NULL,
                       target_age_stages = NULL,
                       target_date_stages = NULL,
                       output_dir = NULL,
                       config = NULL,
                       keep_ids = NULL,
                       growth_curve = FALSE,
                       growth_curve_test = FALSE,
                       log_file = NULL) {

  if (!identical(qc_method, "national_standard")) {
    stop("Legacy QC method was removed in V1.0.0. Use 'national_standard'.", call. = FALSE)
  }

  if (is.null(phenotype_method)) {
    phenotype_method <- "standard_fcr"
  }

  # 创建日志器
  logger <- NULL
  if (!is.null(log_file) && nzchar(trimws(log_file))) {
    logger <- ZhenM_create_logger(log_file, append = FALSE)
  }

  cfg <- ZhenM_merge_config(config, qc_method)

  run_impl <- function() {
    start_time <- Sys.time()

    if (!is.null(logger)) {
      logger$section("ZhenMeasure V1.0.0 Pipeline 开始")
      logger$info(paste0("数据类型: ", data_type))
      logger$info(paste0("数据路径: ", data_path))
      logger$info(paste0("质控方法: ", qc_method))
      logger$info(paste0("表型方法: ", phenotype_method))
    }

    message("=== ZhenMeasure V1.0.0 Pipeline ===")
    message("Step 1: Reading data...")
    if (!is.null(logger)) logger$section("Step 1: 数据读取")
    standard_data_original <- ZhenM_read_data(data_path, data_type, format_path, birth_info_path)
    
    # 保存原始列名，用于最终输出时还原
    original_colnames <- names(standard_data_original)

    if (!is.null(logger)) {
      logger$detail(paste0("读取标准记录数: ", nrow(standard_data_original)))
      # 标准格式使用 ID 或 animal_id 列名
      id_col <- if ("ID" %in% names(standard_data_original)) "ID" else "animal_id"
      logger$detail(paste0("读取个体数: ", data.table::uniqueN(standard_data_original[[id_col]])))
    }
    
    # 复制数据用于 QC 处理（validate 会修改列名）
    standard_data <- data.table::copy(standard_data_original)

    message("Step 2: Overall QC on standard data...")
    if (!is.null(logger)) logger$section("Step 2: Overall QC (Standard Records)")
    qc_result <- ZhenM_qc_overall(standard_data, config = cfg, logger = logger, keep_ids = keep_ids)
    standard_data <- qc_result$records

    message("Step 3: Weight QC on standard data...")
    if (!is.null(logger)) logger$section("Step 3: 体重质控 (Standard Records)")
    standard_data <- ZhenM_qc_weight_standard(standard_data, qc_method, cfg, logger)

    message("Step 4: Feed QC on standard data...")
    if (!is.null(logger)) logger$section("Step 4: 采食量质控 (Standard Records)")
    standard_data <- ZhenM_qc_feed_standard(standard_data, qc_method, cfg, logger)

    
    # 保存质控后的标准记录底表（用于生成qc_summary，这里保留全部原始数据和flag，不要物理删除，否则摘要数会变0）
    qc_standard_data <- data.table::copy(standard_data)
    
    # 构建纯净版 corrected_records_output（物理清洗：异常值置空或删行）
    corrected_records_output <- data.table::copy(qc_standard_data)
    if ("is_outlier_wt" %in% names(corrected_records_output)) {
      corrected_records_output[is_outlier_wt == TRUE, c("weight_g", "Weight") := NA_real_]
    }
    if ("is_outlier_feed" %in% names(corrected_records_output)) {
      corrected_records_output[is_outlier_feed == TRUE, c("feed_g", "Feed_intake") := NA_real_]
    }
    # 只输出标准业务列（白名单取自标准 schema，去除 flag 等诊断列与内部列）
    # issue #15：不再按位置截取前 12 列——列序变化或 schema 扩列时会静默截掉业务列
    schema_fields <- ZhenM_standard_record_fields()[field != "source_file", field]
    keep_cols <- intersect(schema_fields, names(corrected_records_output))
    corrected_records_output <- corrected_records_output[, ..keep_cols]
    
    # 物理剔除毫无意义的空测定行（采食和体重都没了的行）
    if ("weight_g" %in% names(corrected_records_output) && "feed_g" %in% names(corrected_records_output)) {
      corrected_records_output <- corrected_records_output[!(is.na(weight_g) & (is.na(feed_g) | feed_g == 0))]
    }
    
    message("Step 5: Converting to daily format (filtered)...")
    if (!is.null(logger)) logger$section("Step 5: 转换为日汇总格式（排除异常记录）")
    daily_data <- ZhenM_standard_to_daily_filtered(standard_data, cfg)
    
    if (!is.null(logger)) {
      logger$detail(paste0("日汇总记录数: ", nrow(daily_data)))
      logger$detail(paste0("日汇总个体数: ", data.table::uniqueN(daily_data$animal_id)))
    }
    
    # 初始化插补标记（在调用插补函数之前）
    data.table::setDT(daily_data) # 强制刷新底层 C 指针，防止报错
    if (!"is_imputed_wt" %in% names(daily_data)) {
      data.table::set(daily_data, j = "is_imputed_wt", value = FALSE)
    }
    if (!"is_imputed_feed" %in% names(daily_data)) {
      data.table::set(daily_data, j = "is_imputed_feed", value = FALSE)
    }

    message("Step 5.5: Growth curve check on aggregated daily data...")
    if (!is.null(logger)) logger$section("Step 5.5: 整体生长曲线质控(基于日龄聚合)")
    
    # Check growth curve R2 for each animal based on daily_weight_g
    # issue #22：逐头判定逻辑抽取为 .check_growth_curve_batch()，循环内不再
    # 用 c() 追加删除名单（O(n²) 复制）、不再每头做一次全表子集
    min_r2 <- cfg$national_standard$growth_curve_r2_min
    gc_batch <- .check_growth_curve_batch(daily_data, min_r2 = min_r2)
    animals_to_delete <- gc_batch$animals_to_delete
    n_insufficient <- gc_batch$n_insufficient
    n_low_r2 <- gc_batch$n_low_r2

    if (length(animals_to_delete) > 0) {
      if (!is.null(logger)) {
        logger$info(sprintf("生长曲线质控删除个体数: %d（点数不足 %d 头, R²低于阈值 %d 头）",
                            length(animals_to_delete), n_insufficient, n_low_r2))
        if (length(animals_to_delete) <= 20) {
          logger$detail(sprintf("被删除的异常个体 ID: %s", paste(animals_to_delete, collapse = ", ")))
        } else {
          logger$detail(sprintf("被删除的异常个体 ID (前20个): %s, ...", paste(head(animals_to_delete, 20), collapse = ", ")))
        }
      }
      
      # 若要在摘要中反馈，需要先把标记打上
      if ("flag_growth_curve_poor" %in% names(qc_standard_data)) {
        qc_standard_data[animal_id %in% animals_to_delete, flag_growth_curve_poor := TRUE]
      }
      
      # 物理剔除数据
      daily_data <- daily_data[!animal_id %in% animals_to_delete]
      qc_standard_data <- qc_standard_data[!animal_id %in% animals_to_delete]
      corrected_records_output <- corrected_records_output[!animal_id %in% animals_to_delete]
    } else {
      if (!is.null(logger)) logger$info("所有留存个体的生长曲线均符合R²质量要求")
    }

    message("Step 6: Feed imputation...")
    if (!is.null(logger)) logger$section("Step 6: 数据填充")
    daily_data <- ZhenM_impute_national(daily_data, cfg)

    if (!is.null(logger)) {
      n_imputed_feed <- sum(daily_data$is_imputed_feed, na.rm = TRUE)
      n_imputed_wt <- sum(daily_data$is_imputed_wt, na.rm = TRUE)
      logger$detail(paste0("采食量填充记录数: ", n_imputed_feed))
      logger$detail(paste0("体重填充记录数: ", n_imputed_wt))
    }

    # 构建原始日汇总（用于绘图模块对比展示质控前后效果）
    # 注意：使用 standard_data（已去重+已QC）而非 standard_data_original（含大量重复），
    # 否则原始日汇总值会因重复记录虚高，与清洗后的值产生虚假差异。
    raw_daily <- .build_raw_daily(standard_data)
    raw_daily <- .annotate_raw_daily_flags(raw_daily, qc_standard_data, daily_data)

    message("Step 7: Calculating phenotypes...")
    if (!is.null(logger)) logger$section("Step 7: 表型计算")
    
    # 调用新版表型计算函数，支持阶段划分
    phenotypes <- ZhenM_calc_phenotypes(
      daily_records = daily_data,
      phenotype_method = phenotype_method,
      config = cfg,
      stage_mode = stage_mode,
      target_weight_stages = target_weight_stages,
      target_age_stages = target_age_stages,
      target_date_stages = target_date_stages
    )

    if (!is.null(logger)) {
      logger$detail(paste0("计算表型个体数: ", nrow(phenotypes)))
      logger$detail(paste0("输出表型列: ", paste(setdiff(names(phenotypes), "animal_id"), collapse = ", ")))
      if (!is.null(stage_mode)) {
        logger$detail(paste0("阶段划分模式: ", stage_mode))
        if ("stage_label" %in% names(phenotypes)) {
          logger$detail(paste0("阶段数量: ", data.table::uniqueN(phenotypes$stage_label)))
        }
      }
    }

    message("Step 8: Generating QC summary...")
    if (!is.null(logger)) logger$info("Step 8: 生成QC摘要")
    qc_summary <- ZhenM_generate_qc_summary(qc_standard_data)

    # 构建返回结果 - 保持与旧版本兼容的命名
    result <- list(
      # 主要输出
      corrected_records = corrected_records_output,
      phenotypes = phenotypes,

      # 内部使用的数据
      daily_records = daily_data,               # 日汇总数据（用于表型计算）
      raw_daily = raw_daily,                    # 原始日汇总（用于绘图对比）

      # QC统计
      qc_summary = qc_summary,
      qc_overall_summary = qc_result$summary,
      config = cfg
    )

    if (!is.null(output_dir)) {
      message("Step 9: Writing outputs...")
      if (!is.null(logger)) logger$section("Step 9: 输出文件")
      ZhenM_write_run_outputs(result, output_dir, logger = logger)

      if (growth_curve || growth_curve_test) {
        if (!is.null(logger)) logger$info("生成生长曲线图...")
        ZhenM_write_plot_outputs(result, output_dir, growth_curve, growth_curve_test)
      }
    }

    end_time <- Sys.time()
    duration <- difftime(end_time, start_time, units = "mins")

    if (!is.null(logger)) {
      logger$section("Pipeline 完成")
      logger$info(paste0("总耗时: ", round(as.numeric(duration), 2), " 分钟"))
      logger$info(paste0("最终记录数: ", nrow(daily_data)))
      logger$info(paste0("最终个体数: ", data.table::uniqueN(daily_data$animal_id)))
      logger$info(paste0("表型个体数: ", nrow(phenotypes)))
    }

    message("=== Pipeline completed ===")
    invisible(result)
  }

  out <- tryCatch(
    run_impl(),
    error = function(e) {
      if (!is.null(logger)) {
        logger$warn(paste0("Pipeline 错误: ", conditionMessage(e)))
      }
      stop(e)
    },
    finally = {
      if (!is.null(logger)) {
        ZhenM_close_log(logger)
      }
    }
  )

  out
}
