#' Get default configuration for ZhenMeasure
#'
#' Returns default configuration parameters for quality control, imputation,
#' and phenotype calculation using the national standard method.
#'
#' @param qc_method QC method: "national_standard" (the only supported method since V1.0.0)
#' @return A list of configuration parameters
#'
#' @section `test_weight_range`（试验全量程筛选）:
#' `national_standard$test_weight_range`（默认 `c(45, 110)` kg）采用
#' **覆盖全量程** 口径，而不是区间包含过滤：只有「入栏体重 <= 下界 且
#' 出栏体重 >= 上界」的个体才保留，不满足者整头删除（含其全部记录）。
#' 因此入栏已超下界或出栏未达上界的正常个体也会被删除，且该规则对
#' 全量程饲养试验之外的场景并不适用；实测三设备演示数据（默认口径）
#' 分别删除 42% / 20% / 95% 的个体（issue #25）。
#' 调试脚本中常见的 `c(200, 20)` 是反向区间，等效于关闭该筛选。
#' @export
#' @examples
#' # Get national standard config
#' cfg <- ZhenM_default_config("national_standard")
ZhenM_default_config <- function(qc_method = "national_standard") {
  if (!identical(qc_method, "national_standard")) {
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
    # 试验全量程（kg）——「覆盖全量程」筛选口径，非区间包含过滤：
    # 只有 入栏体重 <= 下界(45) 且 出栏体重 >= 上界(110) 的个体才保留，
    # 不满足者整头删除（.apply_test_weight_range_filter()，issue #25）。
    # 想让「起栏>45kg / 出栏<110kg 的正常个体」保留、仅剔除超界记录，需改代码；
    # 调试脚本常用的 c(200, 20) 是反向区间，等效于关闭该筛选。
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
    speed_zero_long_duration_sec = 500,

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
    # 三开关依赖关系（issue #17）：
    #   use_lmm_feed_correction=TRUE 仅在记录级纠正「关闭或失败」时兜底运行；
    #   记录级纠正成功时是否再跑 LMM 由 use_lmm_stacking 决定，此时
    #   use_lmm_feed_correction 为空操作。
    use_record_feed_correction = TRUE,
    use_lmm_feed_correction = TRUE,

    # LMM 叠加模式：记录级物理纠正成功后仍串联运行改良 LMM，但只补偿物理
    # 规则无法恢复的「噪声置零类」损失（负值/极高速小采食/长时间零速被置 0
    # 的记录），避免对已被封顶纠正的记录二次补偿。
    # V1.1.4 起默认 TRUE（issue #5「F 转正」）：注入式基准三设备 9/9 格
    # 优于纯记录级纠正（A），且干净数据上几乎不出手（FIRE 0 天 / NEDAP 1 天 /
    # 扬翔 869 天且平均改动仅 7.7 g），代价是每台设备多一次 lme4 拟合
    # （+0.4~7.7 秒）。设 FALSE 可退回 V1.1.1 的纯记录级物理纠正行为。
    use_lmm_stacking = TRUE,

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

#' Collect key paths in user config that are absent from defaults
#'
#' issue #17：modifyList 不校验键名，拼写错误的键会被静默塞进 config，
#' 读取处命中 NULL 走默认值。此助手递归比对（仅普通 list 递归，
#' data.frame 按叶子处理避免列名误报），返回形如
#' "national_standard.use_record_feed_corection" 的未知键路径。
#'
#' @param default 默认配置（键的权威来源）
#' @param user 用户配置
#' @param prefix 递归用路径前缀
#' @return 字符向量；无未知键时为空
#' @keywords internal
.unknown_config_keys <- function(default, user, prefix = "") {
  unknown <- character(0)
  for (nm in names(user)) {
    path <- if (prefix == "") nm else paste(prefix, nm, sep = ".")
    if (!nm %in% names(default)) {
      unknown <- c(unknown, path)
    } else {
      d <- default[[nm]]
      u <- user[[nm]]
      if (is.list(d) && !is.data.frame(d) && is.list(u) && !is.data.frame(u)) {
        unknown <- c(unknown, .unknown_config_keys(d, u, path))
      }
    }
  }
  unknown
}

#' Merge user config with defaults
#'
#' @param user_config User-provided configuration list
#' @param qc_method QC method (always "national_standard" since V1.0.0)
#' @return Merged configuration。用户配置中存在默认值没有的键时发出
#'   warning（很可能是拼写错误，该键不会生效）——issue #17。
#' @keywords internal
ZhenM_merge_config <- function(user_config = NULL, qc_method = "national_standard") {
  default <- ZhenM_default_config(qc_method)

  if (is.null(user_config)) return(default)

  unknown_keys <- .unknown_config_keys(default, user_config)
  if (length(unknown_keys) > 0) {
    warning(paste0(
      "config 中存在未识别的键（可能是拼写错误，将不会生效）: ",
      paste(unknown_keys, collapse = ", ")
    ), call. = FALSE)
  }

  utils::modifyList(default, user_config)
}
