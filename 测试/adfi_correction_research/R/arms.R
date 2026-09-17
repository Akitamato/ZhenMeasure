######### 臂的公共契约 #########
#
# 仓库里此前没有任何 fit/predict 注册表：`测试/simulation_benchmark.R` 的
# `variants <- list(list(key, label, sw))` 是**配置开关**清单（C0/A/L/Ln…），
# 装不下 LOESS 这种函数对象。Phase 4 第一次引入「臂」这个抽象，故先把契约定死。
#
#   臂 = function(arm_input) -> data.table(animal_id, record_date, daily_feed_g, ...)
#
# arm_input 与输出一律是 gold_to_arm_input(gold) 的形态（补齐后的日历网格）。
# 输入预处理（要不要过 as_na_view()）挂在**臂对象的属性**上，由 run_arm() 自动施加，
# 调用方不必记得——记错一次就是静默的对照组失效（见 arm_loess.R 的 raw_zero）。
#
# 四类错误都会让 eval_metrics_b 或下游**静默**给出错数，契约校验是这一层存在的
# 唯一理由：四条都必须在 eval 之前炸掉。
#   ① 行数变了   —— 臂偷偷加了/删了动物天，universe 与分母随之漂移
#   ② 键重复     —— eval_metrics_b 的 merge 是多对多，重复会让 m 膨胀、
#                    n_universe 虚增、accuracy 被稀释，且不报任何错
#   ③ 键越界     —— 产出 universe 之外的新动物天（凭空造天）
#   ④ 日期类不同 —— IDate / Date / character 混用，merge 静默零匹配 → 全 NA

ARM_KEY <- c("animal_id", "record_date")
ARM_REQUIRED <- c(ARM_KEY, "daily_feed_g")

# 按臂声明的 needs_na_view 预处理输入。
#
# 为什么必须有这一步：`as_na_view()` 把 zero / gated / missing 一律改写成 NA，
# 而**只动 daily_feed_g、不动 DFI_status**（daily_gold.R:193-197）。如果某个臂
# 按 DFI_status 判缺口，它在 zero 模式下会拿到与 missing **逐位相同**的掩码，
# 「原样喂 0」的对照臂就退化成空操作。臂一律按 is.na(daily_feed_g) 判缺口。
prepare_arm_input <- function(arm, arm_input) {
  if (isTRUE(attr(arm, "needs_na_view"))) as_na_view(arm_input) else arm_input
}

# 读臂对象声明的参数（kind / span / degree / fallback）。
#
# 为什么不是一个简单的 attr() 取值：`attr(arm, "arm_param")` 缺失时返回 NULL，而
# `data.table(span = NULL, degree = NULL)` 会**静默丢掉那两列**——脚本不报错，
# 只是结果表里少了 span/degree，调参表看起来「就是没这两列」。
# （04/05 第一版真的踩了这个坑，两处被静默吞掉。）
# 缺失即报错，把静默丢列变成响亮失败。
arm_param <- function(arm) {
  p <- attr(arm, "arm_param")
  if (is.null(p)) {
    stop("臂未声明 attr(., \"arm_param\")（kind/span/degree/fallback）", call. = FALSE)
  }
  p
}

# 执行一条臂并校验契约。@return 臂输出的 data.table（带 arm_diag 属性时原样透传）
run_arm <- function(arm, arm_input, allow_row_drop = FALSE) {
  stopifnot(is.function(arm), data.table::is.data.table(arm_input))
  arm_param(arm)   # 先炸在声明缺失上，别等到结果表静默少列
  arm_input <- prepare_arm_input(arm, arm_input)

  out <- arm(arm_input)
  if (!data.table::is.data.table(out)) {
    stop("臂必须返回 data.table", call. = FALSE)
  }

  miss <- setdiff(ARM_REQUIRED, names(out))
  if (length(miss) > 0) {
    stop(sprintf("臂输出缺列：%s", paste(miss, collapse = ", ")), call. = FALSE)
  }

  if (anyDuplicated(out[, ..ARM_KEY]) > 0) {
    stop(paste0("臂输出的 (animal_id, record_date) 有重复——eval_metrics_b 的 merge ",
                "是多对多，n_universe 会虚增而 accuracy 被稀释"), call. = FALSE)
  }

  if (!allow_row_drop && nrow(out) != nrow(arm_input)) {
    stop(sprintf("臂输出行数变了：%d vs 输入 %d（确实要删整天就显式 allow_row_drop = TRUE）",
                 nrow(out), nrow(arm_input)), call. = FALSE)
  }

  if (!identical(class(out$record_date), class(arm_input$record_date))) {
    stop(sprintf("record_date 类不一致：输出 %s / 输入 %s —— merge 会静默零匹配",
                 paste(class(out$record_date), collapse = "/"),
                 paste(class(arm_input$record_date), collapse = "/")), call. = FALSE)
  }

  # 键集：不允许越界造天；删行必须显式声明（后续的「删整天」臂才需要）
  inp_key <- arm_input[, ..ARM_KEY]
  out_key <- out[, ..ARM_KEY]
  data.table::setorderv(inp_key, ARM_KEY)
  data.table::setorderv(out_key, ARM_KEY)
  if (nrow(data.table::fsetdiff(out_key, inp_key)) > 0) {
    stop("臂产出了输入网格之外的动物天（越界造天）", call. = FALSE)
  }
  if (!allow_row_drop && nrow(data.table::fsetdiff(inp_key, out_key)) > 0) {
    stop("臂丢掉了输入里的动物天，却没声明 allow_row_drop = TRUE", call. = FALSE)
  }

  out
}

# 读臂自附的逐头诊断（n_gap / n_loess / n_fallback / branch …）。
# 挂在属性上而不是当列放：这些量是**每头一个常数**，摊成列后任何 sum() 都会重复计数。
arm_diag <- function(arm_out) {
  d <- attr(arm_out, "arm_diag")
  if (is.null(d)) stop("该臂未附诊断表（attr(., \"arm_diag\")）", call. = FALSE)
  d
}
