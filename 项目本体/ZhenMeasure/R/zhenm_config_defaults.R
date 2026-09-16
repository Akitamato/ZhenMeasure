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
    phenotype_method = "standard_fcr",
    # data.table 线程数（issue #37）。默认 1：显式串行，结果不依赖机器核数、
    # 可复现。实测在本机（32 逻辑核，data.table 默认取 16）上，对扬翔 668 头
    #（179 万条）这类规模，多线程的调度与合并开销超过其收益，是**负收益**，
    # 且把 CPU 打满会挤压同机其他任务。
    # 设 dt_threads = 0 恢复 data.table 自身的默认（用满可用核）；
    # 设 dt_threads = NULL 则完全不干预、保持调用时的当前值。
    dt_threads = 1L
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

    # 校正机制开关。日级校正已按 Jiao et al. (2014) 重写（issue #5），
    # 依赖关系随之改变：
    #   use_lmm_feed_correction = FALSE（默认）→ 不跑 LMM，daily_feed_g 由
    #     记录级物理纠正（A）产生，仅保留出口日上限校验。
    #   use_lmm_feed_correction = TRUE → 日级文献 LMM **恒运行**，
    #     daily_feed_g 改由它产生（= error-free 日和 + Σβ̂x）。此时
    #     use_record_feed_correction 的产物不再进入日值，只留在内部列里
    #     作对照臂。
    # 默认取 A 而非文献 LMM 的依据：注入式基准上 L 的 accuracy 在三设备 ×
    # 三档注入率共 9 格中全面低于 A（FIRE@20% 0.5019 vs 0.5559、NEDAP
    # 0.5470 vs 0.6023、扬翔 0.2991 vs 0.3426），bias 也是低估最严重的一档。
    # 文献实现保留为**可选增强**，改这一个键即可切换。详见 NEWS.md 1.2.0 段。
    use_record_feed_correction = TRUE,
    use_lmm_feed_correction = FALSE,

    # LMM 协变量截尾界（Casey 2003，经 Jiao et al. 2016 转述）：拟合前剔除
    # 越界的**训练行**以降低极端值带来的偏差。注意被截的对象是「某一类错误
    # 访问当日的累计量」这一**协变量**——不是当日总采食量，也不是模型响应
    # （响应的生理上限由出口的 feed_intake_range 管，两者不是一回事）。
    #   lmm_trim_dfie_g — FID_p：类型 4,5,15,16 的当日累计采食量 (g)
    #   lmm_trim_otde_s — OTD_p：其余入模类型的当日累计占据时长 (s)
    # 截尾只作用于训练集；应用端回填永不截尾，否则恰好会取消掉最需要校正的天。
    lmm_trim_dfie_g = c(0, 3500),
    lmm_trim_otde_s = c(0, 5000),

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

#' 已移除的配置键及其说明（issue #5 重写为 Jiao et al. (2014) 文献实现）
#'
#' 返回命名列表「键名 → 面向用户的说明」。`ZhenM_merge_config()` 对用户实际
#' 传入的每个已移除键发一条 warning，并把对应路径从「未识别键」列表中剔除——
#' 否则同一件事会报两条互相矛盾的提示（一条说「可能是拼写错误」，另一条说
#' 「已明确移除」）。
#'
#' @param user_config 用户配置
#' @return 命名列表；用户未传任何已移除键时为空列表
#' @keywords internal
.removed_config_keys <- function(user_config) {
  defined <- list(
    use_lmm_stacking = paste0(
      "config 键 national_standard$use_lmm_stacking 已被移除，设置不会生效。",
      "日级 LMM 采食量校正已按 Jiao et al. (2014) 重写，",
      "不再区分「兜底」与「叠加」两种模式。该实现默认关闭，",
      "如需启用请设 national_standard$use_lmm_feed_correction = TRUE。"
    )
  )
  defined[intersect(names(defined), names(user_config$national_standard))]
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

  # 已移除的键先单独说明，并从「未识别键」里剔除（issue #5 重写）
  removed_keys <- .removed_config_keys(user_config)
  for (nm in names(removed_keys)) {
    warning(removed_keys[[nm]], call. = FALSE)
  }

  unknown_keys <- setdiff(
    .unknown_config_keys(default, user_config),
    paste0(qc_method, ".", names(removed_keys))
  )
  if (length(unknown_keys) > 0) {
    warning(paste0(
      "config 中存在未识别的键（可能是拼写错误，将不会生效）: ",
      paste(unknown_keys, collapse = ", ")
    ), call. = FALSE)
  }

  utils::modifyList(default, user_config)
}
