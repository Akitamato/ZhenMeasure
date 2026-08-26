#' Get default configuration for ZhenMeasure
#'
#' Returns default configuration parameters for quality control, imputation,
#' and phenotype calculation using the national standard method.
#'
#' @param qc_method QC method: "national_standard" (the only supported method since V1.0.0)
#' @return A list of configuration parameters
#' @export
#' @examples
#' # Get national standard config
#' cfg <- ZhenM_default_config("national_standard")
ZhenM_default_config <- function(qc_method = "national_standard") {
  if (match.arg(qc_method, choices = "national_standard") != "national_standard") {
    stop("Legacy QC method was removed in V1.0.0. Use 'national_standard'.", call. = FALSE)
  }

  base_config <- list(
    qc_method = "national_standard",
    impute_method = "national_standard",
    phenotype_method = "standard_fcr"
  )

  base_config$national_standard <- list(
    # Weight QC
    weight_range = c(25, 140),
    weight_threshold = 0.25,
    daily_weight_threshold = 0.90,
    growth_curve_r2_min = 0.95,
    test_weight_range = c(45, 110),

    # Feed QC
    feed_intake_range = c(0, 6),
    feed_anomaly_types = c(
      "feed_negative", "feed_too_high", "duration_negative",
      "duration_too_long", "duration_zero_with_feed",
      "speed_too_slow", "speed_too_fast", "speed_extreme_low_feed",
      "speed_zero_long_duration"
    ),
    duration_max = 1800,
    speed_min = 2,
    speed_max = 170,
    speed_extreme = 500,
    feed_extreme_threshold = 50,

    # Imputation
    impute_r2_min = 0.95,
    loess_span = 0.75,
    extrapolation_start_range = 10,
    extrapolation_end_range = 15,

    # Data completeness
    min_test_days = 60,
    min_stage_days = 20,
    max_missing_rate = 0.15,

    # FCR ranges (Table 2)
    fcr_ranges = data.frame(
      weight_min = c(30, 40, 50, 60, 70, 80, 90, 100, 110),
      weight_max = c(40, 50, 60, 70, 80, 90, 100, 110, 120),
      fcr_min = c(0.76, 1.06, 1.14, 1.28, 1.35, 1.36, 1.34, 1.34, 1.31),
      fcr_max = c(3.16, 3.08, 3.30, 3.41, 3.59, 3.86, 4.12, 4.27, 4.50),
      stringsAsFactors = FALSE
    ),

    # FCR anchor correction (optional, disabled by default)
    use_fcr_anchor = FALSE,
    fcr_anchor_threshold = 0.5,

    # 校正机制开关：默认均为 TRUE（= 现状行为）。关闭记录级物理纠正后回退
    # 「置零」路径；关闭日级 LMM 兜底后仅保留 6kg 日上限校验。用于校正机制
    # 消融实验（issue #5）与后续锚点修复的对照评测。
    use_record_feed_correction = TRUE,
    use_lmm_feed_correction = TRUE,

    # LMM 叠加模式（实验性）：记录级物理纠正成功后仍串联运行改良 LMM，
    # 但只补偿物理规则无法恢复的「噪声置零类」损失（负值/极高速小采食/
    # 长时间零速被置 0 的记录），避免对已被封顶纠正的记录二次补偿。
    use_lmm_stacking = FALSE,

    # STL time-series feed QC (optional, disabled by default)
    use_stl_feed = FALSE,
    stl_period = 7,
    stl_s_window = "periodic",
    stl_robust = TRUE,
    stl_mad_multiplier = 3,
    stl_min_obs = 30,

    # Gompertz growth curve QC (optional, disabled by default)
    use_gompertz = FALSE,
    gompertz_min_obs = 60,
    gompertz_mad_multiplier = 4,
    gompertz_maxiter = 100
  )

  base_config
}

#' Merge user config with defaults
#'
#' @param user_config User-provided configuration list
#' @param qc_method QC method (always "national_standard" since V1.0.0)
#' @return Merged configuration
#' @keywords internal
ZhenM_merge_config <- function(user_config = NULL, qc_method = "national_standard") {
  default <- ZhenM_default_config(qc_method)

  if (is.null(user_config)) return(default)

  utils::modifyList(default, user_config)
}
