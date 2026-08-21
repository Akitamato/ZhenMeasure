#' Get default configuration for ZhenMeasure
#'
#' Returns default configuration parameters for quality control, imputation,
#' and phenotype calculation. Supports both national standard (red-header file)
#' and legacy (V0.2.0) methods.
#'
#' @param qc_method QC method: "national_standard" or "legacy"
#' @return A list of configuration parameters
#' @export
#' @examples
#' # Get national standard config
#' cfg <- ZhenM_default_config("national_standard")
#'
#' # Get legacy config
#' cfg_legacy <- ZhenM_default_config("legacy")
ZhenM_default_config <- function(qc_method = c("national_standard", "legacy")) {
  qc_method <- match.arg(qc_method)

  base_config <- list(
    qc_method = qc_method,
    impute_method = qc_method,
    phenotype_method = if (qc_method == "national_standard") "standard_fcr" else "report"
  )

  if (qc_method == "national_standard") {
    base_config$national_standard <- list(
      # Weight QC
      weight_range = c(25, 140),
      weight_threshold = 0.25,
      daily_weight_threshold = 0.90,
      growth_curve_r2_min = 0.99,
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
      max_missing_rate = 0.15,

      # FCR ranges (Table 2)
      fcr_ranges = data.frame(
        weight_min = c(30, 40, 50, 60, 70, 80, 90, 100, 110),
        weight_max = c(40, 50, 60, 70, 80, 90, 100, 110, 120),
        fcr_min = c(0.76, 1.06, 1.14, 1.28, 1.35, 1.36, 1.34, 1.34, 1.31),
        fcr_max = c(3.16, 3.08, 3.30, 3.41, 3.59, 3.86, 4.12, 4.27, 4.50),
        stringsAsFactors = FALSE
      )
    )
  } else {
    base_config$legacy <- list(
      weight_range = c(25, 140),
      weight_sd_threshold = 3,
      test_weight_range = c(45, 110),
      feed_intake_range = c(0, 6),
      feed_percentile_low = 0.025,
      feed_percentile_high = 0.01,
      min_test_days = 60,
      max_missing_rate = 0.15,

      # Advanced legacy QC parameters
      min_obs_for_ts = 100,
      min_obs_for_wt = 60,
      rlm_maxit = 60,
      rlm_weight_thresh = 0.5,
      
      # Advanced QC switches (默认开启)
      use_stl = TRUE,         # STL 时间序列采食量质控
      use_rlm = TRUE,         # RLM 稳健回归体重质控
      use_gompertz = TRUE     # Gompertz 生长曲线体重质控
    )
  }

  base_config
}

#' Merge user config with defaults
#'
#' @param user_config User-provided configuration list
#' @param qc_method QC method
#' @return Merged configuration
#' @keywords internal
ZhenM_merge_config <- function(user_config = NULL, qc_method = "national_standard") {
  default <- ZhenM_default_config(qc_method)

  if (is.null(user_config)) return(default)

  utils::modifyList(default, user_config)
}
