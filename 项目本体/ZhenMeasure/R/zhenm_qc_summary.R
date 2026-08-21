#' Generate QC summary report
#'
#' @param qc_data Data with QC flags
#' @return Summary data.table
#' @export
ZhenM_generate_qc_summary <- function(qc_data) {
  dt <- data.table::as.data.table(qc_data)

  # 筛选异常检测标志列：
  # 1. 以 flag_ 或 is_outlier_ 为前缀，或显式包含 logic_invalid
  # 2. 排除掉汇总统计列 is_outlier_wt 和 is_outlier_feed，避免信息重复
  possible_patterns <- c("^flag_", "^is_outlier_", "^logic_invalid$")
  all_potential_flags <- grep(paste(possible_patterns, collapse = "|"), names(dt), value = TRUE)
  
  exclude_cols <- c("is_outlier_wt", "is_outlier_feed")
  flag_cols <- setdiff(all_potential_flags, exclude_cols)

  if (length(flag_cols) == 0) {
    return(data.table::data.table(
      error_type = character(0),
      count = integer(0),
      percentage = numeric(0),
      severity = character(0)
    ))
  }
  
  summary_list <- lapply(flag_cols, function(col) {
    # 统计标记为 TRUE 的行数
    n_flagged <- sum(dt[[col]] == TRUE, na.rm = TRUE)
    pct <- n_flagged / nrow(dt) * 100

    # 根据列名分配严重程度
    severity <- if (grepl("incomplete|growth_curve_poor|out_of_range", col)) {
      "HIGH"
    } else if (grepl("weight_low|daily_weight_low|feed_too_high", col)) {
      "MEDIUM"
    } else {
      "LOW"
    }

    data.table::data.table(
      error_type = col,
      count = n_flagged,
      percentage = round(pct, 2),
      severity = severity
    )
  })

  result <- data.table::rbindlist(summary_list)
  
  # 增加汇总行（可选，但通常汇总列在报告中以独立逻辑体现更直观）
  # 此处按报错数量排序
  data.table::setorder(result, -count)
  
  result
}
