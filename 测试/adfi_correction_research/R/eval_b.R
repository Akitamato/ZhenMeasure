######### 问题 B 轨评价指标：以**真值**为锚 #########
#
# 为什么不复用 skeleton.R 的 eval_metrics()：
#
#   eval_metrics() 的合并是 merge(daily_est, truth, all.x = TRUE)——**以臂自身的
#   输出为锚**。在问题 A 里这不是问题：A 的注入器只改 feed_g，从不删行，所以
#   每个动物天都还在 daily_est 里，fcoalesce(est, 0) 能按预期触发。
#
#   问题 B 的核心动作恰恰是**删整天**。被删的天不在 daily_est 里，于是根本不进 m，
#   fcoalesce 永不触发 —— accuracy 只在幸存的那些天上算，
#   一个「什么都不恢复」的臂会得到 accuracy ≈ 1.0、coverage ≈ 1.0。
#   这是静默的错误结果，必须在 B 轨另起一个以真值为锚的评价函数。
#
# 打分范围**必须**显式传 `universe`，且在 B 轨里一律取 gold_universe(gold)
#   （daily_gold.R）= 注入前的 observed 天。
#
#   ⚠️ 直接用 truth_daily 当 universe 是错的：truth_daily 里还含 gated 天
#   （FIRE 3 天）——真值存在，但生产 Step 5 的日级门把值置成了 NA，管线自己都产不出。
#   计进分母会给每个臂挂一个恒定 ≈0.04% 亏损，`未注入 → acc == 1` 的契约随之断掉。
#   gated 天与无访问的缺失天一样，**只报告不打分**（计划 §五）。
#
# 两轨可比性：universe 之外的行一律不进分母，与 A 轨 m[!is.na(true_feed)] 口径一致。

#' @param daily_est 臂的输出日级表，需含 animal_id / record_date / daily_feed_g
#' @param truth     真值日表（truth_daily），含 animal_id / record_date / true_feed
#' @param universe  可选。限定打分范围的 animal-day 子集；缺省为 truth 的全部行。
#'   **B 轨请务必传 gold_universe(gold)**，否则 gated 天会被算进分母（见上）
#' @param affected  可选。注入明细表，含 animal_id / record_date / inj_types
#'
#' @return 与 eval_metrics() 同构的 list：acc / bias / coverage / r / rho / by_type，
#'   另加 n_universe / n_na_days / n_extra_rows 三个诊断量。
eval_metrics_b <- function(daily_est, truth, universe = NULL, affected = NULL) {
  stopifnot(all(c("animal_id", "record_date", "true_feed") %in% names(truth)))
  stopifnot(all(c("animal_id", "record_date", "daily_feed_g") %in% names(daily_est)))

  truth_u <- if (is.null(universe)) {
    truth
  } else {
    merge(truth, unique(universe[, .(animal_id, record_date)]),
          by = c("animal_id", "record_date"))
  }

  # 关键：truth 在左。臂删掉的天在这里变成 est = NA，再被 fcoalesce 记 0 —— 丢天即损失。
  m <- merge(truth_u[, .(animal_id, record_date, true_feed)],
             daily_est[, .(animal_id, record_date, est = daily_feed_g)],
             by = c("animal_id", "record_date"), all.x = TRUE)
  m[, est_filled := data.table::fcoalesce(est, 0)]

  acc  <- 1 - sum(abs(m$est_filled - m$true_feed)) / sum(m$true_feed)
  bias <- (sum(m$est_filled) - sum(m$true_feed)) / sum(m$true_feed)
  covg <- mean(!is.na(m$est))

  # 落在 universe 之外、臂却凭空产出的行（例如给 universe 外的天插补了值）——不参与打分
  n_extra <- nrow(daily_est) - nrow(merge(daily_est[, .(animal_id, record_date)],
                                          truth_u[, .(animal_id, record_date)],
                                          by = c("animal_id", "record_date")))

  # 个体层 ADFI 相关度
  by_animal <- m[, .(est_adfi = mean(est_filled), true_adfi = mean(true_feed)),
                 by = animal_id]
  r_pearson <- suppressWarnings(cor(by_animal$est_adfi, by_animal$true_adfi))
  rho_sp <- suppressWarnings(cor(by_animal$est_adfi, by_animal$true_adfi,
                                 method = "spearman"))

  # 分缺失模式的明细。比 A 轨多报 acc 与 coverage：
  # 单看 ratio_mean 会把「填了但填错」和「压根没填」混为一谈。
  type_tab <- NULL
  if (!is.null(affected)) {
    aff <- merge(affected[, .(animal_id, record_date, inj_types)],
                 m[, .(animal_id, record_date, est, est_filled, true_feed)],
                 by = c("animal_id", "record_date"), all.x = TRUE)
    # affected 必须落在 universe 内（越界行没有真值，均值会变 NA）
    aff <- aff[!is.na(true_feed)]
    if (nrow(aff) > 0) {
      type_tab <- aff[, .(
        ratio_mean = mean(est_filled / true_feed, na.rm = TRUE),
        acc        = 1 - sum(abs(est_filled - true_feed)) / sum(true_feed),
        coverage   = mean(!is.na(est)),
        n_days     = .N
      ), by = inj_types]
    }
  }

  list(acc = acc, bias = bias, coverage = covg, r = r_pearson, rho = rho_sp,
       by_type = type_tab,
       n_universe = nrow(m), n_na_days = sum(is.na(m$est)), n_extra_rows = n_extra)
}

#' 把 eval_metrics_b() 的结果摊成一行 scorecard，便于跨臂 rbindlist
scorecard_row <- function(met, rate, variant, label, na_days = NA_integer_) {
  data.table::data.table(
    rate = rate, variant = variant, label = label,
    accuracy = met$acc, bias = met$bias, coverage = met$coverage,
    adfi_r = met$r, adfi_rho = met$rho,
    n_universe = met$n_universe, na_days = met$n_na_days,
    extra_rows = met$n_extra_rows)
}
