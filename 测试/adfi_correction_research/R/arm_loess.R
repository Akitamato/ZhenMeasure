######### Phase 4：LOESS 臂 #########
#
# LOESS **已经在生产里**（zhenm_impute_national.R:57-68），所以本阶段不是「实现
# LOESS」，而是（a）把它原样接入量化、（b）另立一条无路由的纯 LOESS 臂测它**本身**
# 的能力、（c）调 span × degree 看生产默认是否次优。
#
# 三条臂：
#   arm_loess_prod(cfg)                     —— 原样调 .impute_feed_national_v2()，零新代码
#   arm_loess_pure(span, degree, fallback)  —— 无路由，每个缺口都走 LOESS
#   arm_loess_raw_zero(span, degree)        —— 反事实对照：输入**不过** as_na_view()
#
# ---- 读生产代码得来的事实（别凭印象，改动前回去核对行号）----
#
# 1. :52 按 `max_run` 分叉：<=3 → LOESS；>3 → .extrapolate_feed_with_fcr_v2()
#    （cum_feed ~ weight_kg 线性外推 + FCR 阶段门 + R²>0.95 门）。
#    **所以 5d / 14d 这两个最重要的 pattern 上生产根本不跑 LOESS。**
#    测 LOESS 本身必须另立无路由的 pure 臂，不能拿 prod 当 LOESS 的代表。
#
# 2. :58 是 `stats::loess(y[valid] ~ x[valid], span = 0.3)` —— **没传 degree**，
#    走 stats::loess 的缺省 degree = 2（二次局部回归）。
#    开发计划里写的「span=0.3 与 degree=1 硬编码」是看错了代码；实测
#    `formals(stats::loess)$degree` = 2。**等价性门的生产点因此是 (0.3, 2)，
#    不是 (0.3, 1)** —— 门按 (0.3, 1) 跑必然全体不一致。
#
# 3. span 是「参与拟合的**点数**比例」而非日历宽度：拟合在 y[valid] ~ x[valid] 上，
#    所以 span * n_valid 才是有效带宽。n_valid 随注入率上升而下降 ⇒ 同一个 span
#    在 5% 与 20% 下覆盖的日历天数不同，**这会与 rate 轴混淆**，必须报出来。
#
# 4. `x <- seq_len(nrow(sub))` 只在金表已补连续网格时才等于日历索引（daily_gold.R
#    补网格的理由）；稀疏表下生产会把不规则缺口当成等间距。
#
# 5. :34 的 `sum(!missing_idx) < 10` 守卫是 `next`，**在**中位数兜底**之前**：
#    <10 有效点的动物整条留 NA，兜底也不跑。抄错这一条，「残留 NA 集合」会第一个露馅。
#
# 6. :90-107 的兜底判据是「**回写后仍有 NA**」，不是「拟合失败」。loess 可能成功却在
#    个别点上给出 NA，两种残留在生产里走同一段兜底代码——复刻必须按前者判。

# ============================================================
# 生产路由的**外部复算**
#
# 只读输入侧的缺口结构（这是路由的唯一输入），不猜任何值。
# 用途：在失效模式表里标注 prod 实际走了哪条分支。
# ============================================================
.prod_branch <- function(missing_mask) {
  m <- missing_mask
  if (!any(m)) return("no_gap")
  if (sum(!m) < 10L) return("skipped_lt10")   # :34 守卫，在兜底之前
  r <- rle(m)
  if (max(r$lengths[r$values]) <= 3L) "loess" else "fcr-extrap"
}

# ============================================================
# 生产 :52-68 那个「<=3 天」块的**参数化复刻**
#
# 必须复刻而非调用：span / degree 是 :58 的字面量，注入不进生产签名。
# 以注释显式声明它是复刻件、生产函数是权威、等价性门在 04。
#
# 逐条对齐（任何一条抄错都会被 04 的等价性门逮住）：
#   1. `x <- seq_len(nrow(sub))`，`valid <- !missing_idx`
#   2. **单次**拟合，随后一次 `predict(x)` 预测**全部** x（不是只预测缺口点）
#   3. `y_pred[y_pred < 0] <- pmax(0, stats::median(y[valid], na.rm = TRUE))`
#      —— median 取的是**填充前**的 valid 集
#   4. 回写**只**落在 missing_idx 位置
#
# @return 长度 = length(y) 的预测向量；拟合失败（tryCatch 到 NULL）时返回 NULL。
.loess_predict_all <- function(y, span, degree) {
  x <- seq_along(y)
  valid <- !is.na(y)
  loess_fit <- tryCatch(
    stats::loess(y[valid] ~ x[valid], span = span, degree = degree),
    error = function(e) NULL
  )
  if (is.null(loess_fit)) return(NULL)
  y_pred <- as.numeric(stats::predict(loess_fit, x))
  y_pred[y_pred < 0] <- pmax(0, stats::median(y[valid], na.rm = TRUE))
  y_pred
}

# ============================================================
# pure / raw_zero 的共用工作体
#
# 与生产的差别**只有两处**，其余逐行对应 :26-107：
#   · 没有 max_run 分叉（每个缺口都走 LOESS）
#   · span / degree / fallback 由参数给
#
# @param fallback "median" 对齐生产的中位数兜底；"na" 是敏感度臂——
#   eval_metrics_b 把 NA 记 0，所以「不填」必须是被**主动选择并计数**的，不能静默。
# ============================================================
.loess_arm_worker <- function(arm_input, span, degree, fallback) {
  dt <- data.table::copy(arm_input)
  if (!"is_imputed_feed" %in% names(dt)) dt[, is_imputed_feed := FALSE]
  data.table::setorder(dt, animal_id, record_date)

  # 用生产同一套行索引（.build_row_index / .row_index_of），把复刻件与生产可能的
  # 分歧面压到最小：剩下的差异只应来自路由与 span/degree/fallback。
  rows_by_id <- .build_row_index(dt)
  diag <- vector("list", 0)

  for (id in unique(dt$animal_id)) {
    idx <- .row_index_of(rows_by_id, id)
    y <- dt$daily_feed_g[idx]
    missing_idx <- is.na(y)
    n_gap <- sum(missing_idx)
    if (n_gap == 0L) next

    if (sum(!missing_idx) < 10L) {
      # 生产语义：整条留 NA，**兜底也不跑**（:34-46 的 next 在兜底之前）
      diag[[length(diag) + 1L]] <- data.table::data.table(
        animal_id = id, branch = "skipped_lt10", n_gap = n_gap,
        n_loess = 0L, n_fallback = 0L,
        span_pts = span * sum(!missing_idx))
      next
    }

    y_pred <- .loess_predict_all(y, span, degree)
    n_loess <- 0L
    if (!is.null(y_pred)) {
      data.table::set(dt, i = idx[missing_idx], j = "daily_feed_g",
                      value = y_pred[missing_idx])
      data.table::set(dt, i = idx[missing_idx], j = "is_imputed_feed", value = TRUE)
      # n_loess 数的是**真正拿到非 NA 预测**的天数，不是回写次数：
      # predict(loess, x) 在拟合区间**之外**返回 NA（loess 不外推），所以
      # boundary 缺口上 y_pred 全是 NA，随后整批落进中位数兜底。
      # 按「回写次数」数会把这批谎报成 LOESS 的功劳。
      n_loess <- sum(!is.na(y_pred[missing_idx]))
    }

    # 兜底（生产 :90-107 的复刻）：判据是「回写后仍有 NA」，不是「拟合失败」
    cur <- dt$daily_feed_g[idx]
    still_na <- which(is.na(cur))
    n_fb <- 0L
    if (length(still_na) > 0L && identical(fallback, "median")) {
      med_val <- stats::median(cur, na.rm = TRUE)
      if (!is.na(med_val)) {
        data.table::set(dt, i = idx[still_na], j = "daily_feed_g", value = med_val)
        data.table::set(dt, i = idx[still_na], j = "is_imputed_feed", value = TRUE)
        n_fb <- length(still_na)
      }
    }

    diag[[length(diag) + 1L]] <- data.table::data.table(
      animal_id = id,
      branch = if (is.null(y_pred)) "fit_failed" else "loess",
      n_gap = n_gap, n_loess = n_loess, n_fallback = n_fb,
      span_pts = span * sum(!missing_idx))
  }

  attr(dt, "arm_diag") <- if (length(diag) > 0) {
    data.table::rbindlist(diag)
  } else {
    data.table::data.table(animal_id = character(), branch = character(),
                           n_gap = integer(), n_loess = integer(),
                           n_fallback = integer(), span_pts = numeric())
  }
  attr(dt, "arm_param") <- list(kind = "pure", span = span, degree = degree,
                                fallback = fallback)
  dt
}

# ============================================================
# 无路由的纯 LOESS 臂（工厂）
#
# 契约：每个缺口都走 LOESS，缺口多长都一样。输入过 as_na_view()（needs_na_view）。
# ============================================================
arm_loess_pure <- function(span = 0.3, degree = 2, fallback = c("median", "na")) {
  fallback <- match.arg(fallback)
  f <- function(arm_input) .loess_arm_worker(arm_input, span, degree, fallback)
  attr(f, "needs_na_view") <- TRUE
  attr(f, "arm_label") <- sprintf("pure(s%.2f,d%d,%s)", span, degree, fallback)
  attr(f, "arm_param") <- list(kind = "pure", span = span, degree = degree,
                               fallback = fallback)
  f
}

# ============================================================
# 反事实对照臂：**不过 as_na_view()**，原样喂入
#
# 生产契约里 0 不可能出现（.finalize_daily_feed 把 daily_feed_g <= 0 改写成 NA），
# 而生产插补器把 0 当作**有效观测**（zhenm_impute_national.R:31-32）。
# 本臂回答的是：「若臂相信注入的 0 是真观测，会发生什么」——
# zero 模式相对 missing 模式的全部信息量都落在这一条上（inject_seed 不含 mode 项，
# 两种模式注入位置逐位相同）。
#
# 标签与结论里必须写明它是**反事实**，不能当成生产结论。
# ============================================================
arm_loess_raw_zero <- function(span = 0.3, degree = 2) {
  f <- function(arm_input) {
    out <- .loess_arm_worker(arm_input, span, degree, "median")
    attr(out, "arm_param") <- list(kind = "raw_zero", span = span, degree = degree,
                                   fallback = "median")
    out
  }
  attr(f, "needs_na_view") <- FALSE   # ← 唯一的区别
  attr(f, "arm_label") <- sprintf("raw_zero(s%.2f,d%d)", span, degree)
  attr(f, "arm_param") <- list(kind = "raw_zero", span = span, degree = degree,
                               fallback = "median")
  f
}

# ============================================================
# 生产臂（工厂）：原样调 .impute_feed_national_v2()，一行 LOESS 代码都不新写
#
# 与计划的一处签名偏差：计划写的是 `arm_loess_prod(arm_input, cfg)`，但臂的契约是
# 单参数 `function(arm_input)`（run_arm 只传一个），故改成工厂 `arm_loess_prod(cfg)`。
#
# 诊断只能从「输入/输出的差」算，不猜内部走了哪条分支（.prod_branch 只读输入侧结构）。
# 生产不暴露「哪些天是 loess 填的、哪些是中位数兜底填的」，故本臂的 n_fallback 留 NA
# —— 想知道就去看 pure 臂，不要在这里编一个数出来。
# ============================================================
arm_loess_prod <- function(cfg) {
  f <- function(arm_input) {
    n_warn <- 0L
    out <- withCallingHandlers(
      .impute_feed_national_v2(arm_input, cfg),
      warning = function(w) {
        n_warn <<- n_warn + 1L
        invokeRestart("muffleWarning")
      }
    )

    d <- merge(
      arm_input[order(animal_id, record_date),
                .(animal_id, record_date, was_na = is.na(daily_feed_g))],
      out[order(animal_id, record_date),
          .(animal_id, record_date, now_na = is.na(daily_feed_g),
            imp = data.table::fcoalesce(is_imputed_feed, FALSE))],
      by = c("animal_id", "record_date"))
    diag <- d[, .(branch = .prod_branch(was_na),
                  n_gap = sum(was_na),
                  n_filled = sum(was_na & !now_na),
                  n_loess = NA_integer_,      # 生产不暴露，见上
                  n_fallback = NA_integer_,
                  span_pts = 0.3 * sum(!was_na)),
              by = animal_id]
    attr(out, "arm_diag") <- diag[]
    attr(out, "arm_param") <- list(kind = "prod", span = 0.3, degree = 2,
                                   fallback = "median", n_prod_warn = n_warn)
    out
  }
  attr(f, "needs_na_view") <- TRUE   # 生产契约里 0 不可达，必须先归一化
  attr(f, "arm_label") <- "prod(0.3,2,median)"
  attr(f, "arm_param") <- list(kind = "prod", span = 0.3, degree = 2,
                               fallback = "median")
  f
}
