######### Phase 5：GAM 臂 #########
#
# 四条臂：
#   arm_gam_pure(k, bs, family, fallback, min_valid)   —— 逐头 GAM，**无路由**，每个缺口都走 GAM
#   arm_gam_router(k, bs, family, fallback, cfg)       —— 保住生产的 max_run<=3 分叉，只把 <=3 那支换成 GAM
#   arm_gam_pop(k, bs, family, min_valid)              —— 群体模型 s(t) + s(animal_id, bs="re")，一次拟合全表
#   arm_nofill()                                       —— 参照臂：什么都不填（计划 §2.5.2 要的 B 轨 floor）
#
# ---- 实测事实（mgcv 1.9.4 / R 4.5.3，2026-09-17。改动前回来复核，别凭印象）----
#
# 1. **mgcv 的 gam 能外推。** `predict.gam` 在拟合区间之外返回有限值，tp / cr / ps / cs
#    四种基都是。`bs="cr"` 的区间外行为是**严格的直线尾**（自然三次回归样条的定义）。
#    这是本阶段全部价值的来源：Phase 4 已证 `stats::loess` 在区间外返回 NA，向外缺口
#    全部落进 :90-107 的中位数兜底，而那个兜底在向外天上是**负贡献**
#    （FIRE random 的向外子集 acc = -0.3222，比填 0 还差）。
#
# 2. **mgcv 1.9.4 没有单调基。** `bs="moi"` 直接报
#    `no applicable method for 'smooth.construct' applied to an object of class "moi.smooth.spec"`。
#    单调约束需要 `scam`（未安装，用户裁定不装）。别浪费时间在 bs="moi" 上。
#
# 3. **REML 在"有数据的地方"自己选复杂度，但在"没数据的地方"是瞎的。**
#    k∈{6,15} × bs∈{cr,tp,ps} × family∈{gaussian,Gamma} 在**内部天**上平均 EDF
#    全部落在 3.4–5.9——k 在那里确实只是个上限。**但这不等于 k 不重要**：
#    区间外没有数据，REML 无从约束，k 就成了唯一限制基函数边缘自由度的东西。
#    设计复核实测（FIRE 14d 的**向外子集**，同一批 189 头，只动 k）：
#      k=3 → acc_out 0.6714 / k=6 → 0.6955 / k=10 → 0.5669 / k=20 → 0.4321
#    EDF 只从 2.82 爬到 5.50，向外精度却掉了 26pp。**k 在外推区是真旋钮**，
#    网格必须往下探到 3，只报"k 不重要"是把内部结论错当全局结论。
#
# 4. 逐头拟合约 20–27 ms/头 ⇒ FIRE 211 头约 5 s/臂。**k=40 会到 300–740 ms/头**，
#    网格别往上开那么大。
#
# ---- 两条计划约束（是文档写死的，不是可选的谨慎）----
#
# · **不加减负护栏。** 开发方案 §17.5 / issue #44 原话：「新方法**不要**随手加
#   `pmax(0,·)` 护栏，那会引入单侧偏差」。本文件**不做任何负值截断**，只把负预测
#   计数进 diag（`n_negative_pred`）——它若非零，那本身就是要报的发现。
#   （对照：arm_loess.R 里那句 pmax 是**为了逐位复现生产**，是复刻件的义务；
#     两者不要混为一谈，也不要把护栏的理由抄过来。）
#
# · **不偷看答案。** 开发方案 §五 铁律：B 轨算法只能看 observed/missing 的模式 +
#   日期 / 体重 / 阶段，**不得读取 corrupted 行的 ETP/OTD/FID**（那是问题 A 的信息）。
#   所以下面的公式里除了 `s(t)` 不加任何协变量。后人要加协变量，先回去读 §五。

# 构造公式，并把 `s` 绑进**公式自己的环境**。
#
# 为什么不写 `mgcv::s(...)`：mgcv 的平滑项解析器要的是**裸符号** `s`，
# 命名空间限定会直接炸——本机实测：
#   `gam(y ~ mgcv::s(x, k=6, bs="cr"), ...)` ⇒
#   invalid type (list) for variable 'mgcv::s(x, k = 6, bs = "cr")'
# （所以开发计划里"公式里带命名空间调 s"那条是**做不到的**，下面这条是等效且可行的替代。）
#
# 为什么不用 `library(mgcv)`：那会污染调用方的搜索路径，且 **attach 的包会被
# `s()` 这个极常见的名字劫持**——研究模块里 `s` 作为局部变量并不罕见。
# 绑进公式环境既不 attach、也不依赖调用方先 `library(mgcv)`，是本场景最干净的解法。
# `k_eff` / `bs` 一并绑进去：mgcv 求值 `s(x, k = k_eff, bs = bs)` 时就在这个环境里找它们。
#
# 变量 `y` / `x` / `animal_id` 一律来自显式传入的 `data=`，不靠环境查找。
.gam_formula <- function(k, bs, re = FALSE, envir = parent.frame()) {
  e <- new.env(parent = envir)
  e$s <- mgcv::s
  e$k_eff <- k
  e$bs <- bs
  stats::as.formula(
    if (re) "y ~ s(x, k = k_eff, bs = bs) + s(animal_id, bs = \"re\")"
    else    "y ~ s(x, k = k_eff, bs = bs)",
    env = e)
}

# 把族名映射成 family 对象。用字符串传参是为了让 arm_param / 结果表能直接存"用了哪个族"。
.gam_family <- function(family) {
  switch(family,
    gaussian = stats::gaussian(),
    Gamma = stats::Gamma(link = "log"),
    stop(sprintf("未知的 family：%s（只支持 gaussian / Gamma）", family), call. = FALSE)
  )
}

# 单次拟合 + 预测全部 x。
#
# @return 长度 = length(y) 的预测向量；不可拟合时返回 NULL（调用方走 fallback）。
#   `k_eff` 是**实际**用的 k——mgcv 要求 k 不超过唯一协变量值个数，
#   9 个有效点的猪配 k=15 会直接报错，所以要按唯一值个数截断而不是让 gam 炸。
.gam_predict_all <- function(x_all, x_valid, y_valid, k, bs, family_obj) {
  k_eff <- min(k, length(unique(x_valid)))
  if (k_eff < 3L) return(NULL)
  # Gamma(log) 要求响应严格为正。生产契约里 daily_feed_g<=0 已被改写成 NA，
  # 但 arm 的输入不保证（zero 注入模式、或 raw 视图），故这里再查一次。
  # 本机实测不查会直接报 `non-positive values not allowed for the 'Gamma' family'`——
  # 是 error 不是 warning，所以这一句是**功能性**守卫而非防御性装饰。
  if (inherits(family_obj, "Gamma") && !all(y_valid > 0)) return(NULL)

  fit <- tryCatch(
    suppressWarnings(mgcv::gam(
      .gam_formula(k_eff, bs),
      data = data.frame(x = x_valid, y = y_valid),
      family = family_obj, method = "REML"
    )),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  # **必须显式 `type = "response"`**：`predict.gam` 缺省是 `type = "link"`。
  # 对 gaussian 两者恒等（link 就是 identity）**所以这个 bug 不会在 gaussian 上露头**；
  # 对 Gamma(log) 它静默返回**对数尺度**的值（约 7.7 而不是 2200 g），
  # accuracy 会算出一个 0.003 之类的荒谬数却**不报任何错**。
  # 这条是设计复核时被指出的，本机在 gaussian 臂上跑到全绿也照样漏掉——
  # 所以下面这行注释别删，删了以后有人加新 family 会再踩一次。
  p <- tryCatch(
    suppressWarnings(as.numeric(stats::predict(
      fit, newdata = data.frame(x = x_all), type = "response"))),
    error = function(e) NULL
  )
  if (is.null(p) || length(p) != length(x_all)) return(NULL)

  list(pred = p, k_eff = k_eff, edf = unname(summary(fit)$s.table[1, "edf"]),
       bs_dim = unname(fit$smooth[[1]]$bs.dim))
}

# ============================================================
# 逐头臂（pure / router）的共用工作体
#
# router 的实现取巧处（**故意**，因为它把"与生产漂移"这个风险降到了零）：
# 不去重写生产的 FCR 外推，而是先原样跑一次 `.impute_feed_national_v2()`，
# 然后只对 `max_run <= 3` 的那批头，把缺口位**重置回 NA** 再用 GAM 重填。
# 于是：
#   · >3 天的头逐位就是生产行为（FCR 外推 + 中位数兜底），零新代码；
#   · <=3 天的头**逐位等于 pure 臂**（见下），语义精确等于"把 <=3 支的 LOESS 换成 GAM"。
#
# 为什么重置回 NA 是必须的：中位数兜底取的是 `median(cur)`，而 `cur` 是**回写后**的
# 序列。若沿用生产填好的值，`cur` 里就有生产 LOESS 的填充值参与中位数，于是 router
# 的 GAM 路径与 pure 不再逐位相同，"只换了 LOESS" 这句话就不成立了。
# 重置之后两者逐位相同——这条性质在 06 里被显式断言（G5）。
#
# 生产只写 missing_idx 位置（zhenm_impute_national.R:66-67 / 86 / 96-97），
# 非缺口位的值不被触碰，所以重置缺口位后 base 就回到了输入本身。
#
# @param router_cfg NULL ⇒ pure（base 就是输入）；给配置 ⇒ router
# ============================================================
.gam_arm_worker <- function(arm_input, k, bs, family, fallback,
                            min_valid = 10L, router_cfg = NULL) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("GAM 臂需要 mgcv（研究模块外的依赖，不进 DESCRIPTION）", call. = FALSE)
  }
  family_obj <- .gam_family(family)

  inp <- data.table::copy(arm_input)
  if (!"is_imputed_feed" %in% names(inp)) inp[, is_imputed_feed := FALSE]
  data.table::setorder(inp, animal_id, record_date)

  # 缺口判据的唯一来源。绝不用 DFI_status：as_na_view() 只改 daily_feed_g、
  # 不动 DFI_status（daily_gold.R:193-197），按 DFI_status 判缺口在 zero 模式下
  # 会拿到与 missing 逐位相同的掩码，对照臂会退化成空操作。
  inp_y <- inp$daily_feed_g

  if (is.null(router_cfg)) {
    out <- data.table::copy(inp)
  } else {
    prod_out <- withCallingHandlers(
      .impute_feed_national_v2(inp, router_cfg),
      warning = function(w) invokeRestart("muffleWarning")
    )
    data.table::setorder(prod_out, animal_id, record_date)
    out <- prod_out[]
  }

  rows_by_id <- .build_row_index(inp)
  diag <- vector("list", 0)

  for (id in unique(inp$animal_id)) {
    idx <- .row_index_of(rows_by_id, id)
    if (length(idx) == 0L) next

    y <- inp_y[idx]                       # 始终以**输入**的缺口为准
    gap_pos <- which(is.na(y))
    n_gap <- length(gap_pos)
    if (n_gap == 0L) next                 # 与 Phase 4 一致：无缺口不进 diag

    n_valid <- sum(!is.na(y))

    # 生产 :34-45 的守卫是 next，且在中位数兜底**之前**：有效点不足整头留 NA，
    # 兜底也不跑。抄错这一条，"残留 NA 集合"会第一个露馅。
    if (n_valid < min_valid) {
      diag[[length(diag) + 1L]] <- data.table::data.table(
        animal_id = id, branch = "skipped_lt10", n_gap = n_gap,
        n_gam = 0L, n_fallback = 0L, n_self_extrap = 0L,
        n_negative_pred = 0L, k_eff = NA_integer_, edf = NA_real_,
        bs_dim = NA_integer_, n_valid = n_valid, span_pts = NA_real_)
      next
    }

    x <- seq_along(y)                     # 行索引；金表已补连续网格故等于日历索引
    valid_x <- x[!is.na(y)]
    lo_x <- min(valid_x); hi_x <- max(valid_x)
    n_out <- sum(x[gap_pos] < lo_x | x[gap_pos] > hi_x)

    # 路由：>3 天的缺口留给生产（base 已是生产成品），<=3 的走 GAM
    if (!is.null(router_cfg)) {
      r <- rle(is.na(y))
      max_run <- max(r$lengths[r$values])
      if (max_run > 3L) {
        cur <- out$daily_feed_g[idx]
        diag[[length(diag) + 1L]] <- data.table::data.table(
          animal_id = id, branch = "fcr-extrap", n_gap = n_gap,
          n_gam = 0L, n_fallback = sum(is.na(cur[gap_pos])),
          n_self_extrap = n_out, n_negative_pred = 0L,
          k_eff = NA_integer_, edf = NA_real_, bs_dim = NA_integer_,
          n_valid = n_valid, span_pts = NA_real_)
        next
      }
      # <=3：把缺口位重置回 NA，让 GAM 路径与 pure 逐位相同（见上方长注释）
      data.table::set(out, i = idx[gap_pos], j = "daily_feed_g", value = NA_real_)
      data.table::set(out, i = idx[gap_pos], j = "is_imputed_feed", value = FALSE)
    }

    g <- .gam_predict_all(x, x[!is.na(y)], y[!is.na(y)], k, bs, family_obj)

    n_gam <- 0L; n_neg <- 0L
    if (!is.null(g)) {
      filled <- gap_pos[!is.na(g$pred[gap_pos])]
      # 只回写缺口位；**不做负值截断**（§17.5），负值照写、只计数
      data.table::set(out, i = idx[filled], j = "daily_feed_g",
                      value = g$pred[filled])
      data.table::set(out, i = idx[filled], j = "is_imputed_feed", value = TRUE)
      n_gam <- length(filled)
      n_neg <- sum(g$pred[gap_pos] < 0, na.rm = TRUE)
    }

    # 兜底：判据是"回写后仍有 NA"，不是"拟合失败"（生产 :90-107 的语义）
    cur <- out$daily_feed_g[idx]
    still_na <- which(is.na(cur[gap_pos]))
    n_fb <- 0L
    if (length(still_na) > 0L && identical(fallback, "median")) {
      med_val <- stats::median(cur, na.rm = TRUE)   # 取**填充前**的 valid 集
      if (!is.na(med_val)) {
        data.table::set(out, i = idx[gap_pos[still_na]], j = "daily_feed_g",
                        value = med_val)
        data.table::set(out, i = idx[gap_pos[still_na]], j = "is_imputed_feed",
                        value = TRUE)
        n_fb <- length(still_na)
      }
    }

    diag[[length(diag) + 1L]] <- data.table::data.table(
      animal_id = id,
      branch = if (is.null(g)) "fit_failed" else "gam",
      n_gap = n_gap, n_gam = n_gam, n_fallback = n_fb,
      n_self_extrap = n_out, n_negative_pred = n_neg,
      k_eff = if (is.null(g)) NA_integer_ else g$k_eff,
      edf = if (is.null(g)) NA_real_ else g$edf,
      # bs_dim = mgcv **实际**用的基维数。k 低于该基的下限时 mgcv 会**静默抬高**
      # 并只发一条 warning（"basis dimension, k, increased to minimum possible"），
      # 所以 k_eff 单独一个数不足以审计"到底 fit 了什么"。两个都报。
      # （实测：k=3 配 bs="ps" 不是被抬高而是直接报错 —— ps 的下限是 4。
      #   网格里那一格会整格 fit_failed，那是**真结论**不是 bug，照报。）
      bs_dim = if (is.null(g)) NA_integer_ else g$bs_dim,
      n_valid = n_valid,
      span_pts = NA_real_)
  }

  attr(out, "arm_diag") <- if (length(diag) > 0) {
    data.table::rbindlist(diag)
  } else {
    .gam_empty_diag()
  }
  out
}

.gam_empty_diag <- function() {
  data.table::data.table(
    animal_id = character(), branch = character(), n_gap = integer(),
    n_gam = integer(), n_fallback = integer(), n_self_extrap = integer(),
    n_negative_pred = integer(), k_eff = integer(), edf = numeric(),
    bs_dim = integer(), n_valid = integer(), span_pts = numeric())
}

# ============================================================
# 群体臂（GAMM 的单一模型落地形式）
#
# 开发方案 §八 点名**优先**这个形式而不是逐猪 gamm()：
#   「`mgcv::gamm()` 内部走 `nlme`，逐猪循环，扬翔 547 头 / 63805 天会很慢。
#     优先用 `bam()` + `s(t, by=...)` 或 `s(t) + s(animal_id, bs="re")` 的单一模型形式」
# 且计划把 GAM 与 GAMM 放在**同一个 Phase 5**（Phase 6 才是 Kalman），故收在这里。
#
# 科学上它回答的是另一个问题：每头只有约 35 个有效点、缺口长达 14 天时，
# **向群体借强度**（共享 s(t) + 个体随机截距）是不是比逐头各拟合各的更好。
# 注意 `bs="re"` 只给个体一个**水平**平移，不给个体形状——这是它的模型假设，
# 也是它可能赢（少参数、不逐头过拟合）或输（个体轨迹形状不同）的地方。
# ============================================================
.gam_pop_worker <- function(arm_input, k, bs, family, fallback, min_valid = 10L,
                            use_bam = TRUE) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("GAM 臂需要 mgcv（研究模块外的依赖，不进 DESCRIPTION）", call. = FALSE)
  }
  family_obj <- .gam_family(family)

  inp <- data.table::copy(arm_input)
  if (!"is_imputed_feed" %in% names(inp)) inp[, is_imputed_feed := FALSE]
  data.table::setorder(inp, animal_id, record_date)
  inp_y <- inp$daily_feed_g

  rows_by_id <- .build_row_index(inp)
  ids <- unique(inp$animal_id)
  x_all <- integer(nrow(inp)); id_all <- character(nrow(inp))
  for (id in ids) {
    idx <- .row_index_of(rows_by_id, id)
    x_all[idx] <- seq_along(idx)
    id_all[idx] <- as.character(id)
  }

  fit_rows <- !is.na(inp_y)
  if (inherits(family_obj, "Gamma")) fit_rows <- fit_rows & inp_y > 0
  k_eff <- min(k, length(unique(x_all[fit_rows])))

  out <- data.table::copy(inp)
  diag <- vector("list", 0)
  pred_all <- NULL; edf <- NA_real_; bs_dim <- NA_integer_; branch_pop <- "fit_failed"

  if (k_eff >= 3L && sum(fit_rows) >= 3L) {
    d <- data.frame(x = x_all[fit_rows], y = inp_y[fit_rows],
                    animal_id = factor(id_all[fit_rows]))
    # bam 用 fREML：本机实测 `method = "faminla"` 报
    # `un-supported smoothness selection method`（1.9.4 没有它）。fREML 是 bam 的
    # 原生快速 REML，选的就是"用速度换精度"的档位——逐头臂走 gam 的精确 REML，
    # 这条差异在这里显式记一笔，免得后人把两者的 edf 直接并排比。
    fit <- tryCatch(
      suppressWarnings(if (use_bam) {
        mgcv::bam(.gam_formula(k_eff, bs, re = TRUE), data = d,
                  family = family_obj, method = "fREML")
      } else {
        mgcv::gam(.gam_formula(k_eff, bs, re = TRUE), data = d,
                  family = family_obj, method = "REML")
      }),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      pred_all <- tryCatch(
        suppressWarnings(as.numeric(stats::predict(
          fit, newdata = data.frame(x = x_all, animal_id = factor(id_all)),
          type = "response"))),   # 同 .gam_predict_all：缺了它对 Gamma 是错的
        error = function(e) NULL)
      if (!is.null(pred_all)) {
        edf <- unname(summary(fit)$s.table[1, "edf"])
        bs_dim <- unname(fit$smooth[[1]]$bs.dim)
        branch_pop <- "gam"
      }
    }
  }
  if (is.null(pred_all)) pred_all <- rep(NA_real_, nrow(inp))

  for (id in ids) {
    idx <- .row_index_of(rows_by_id, id)
    if (length(idx) == 0L) next
    y <- inp_y[idx]
    gap_pos <- which(is.na(y))
    n_gap <- length(gap_pos)
    if (n_gap == 0L) next
    valid_x <- which(!is.na(y))
    n_out <- if (length(valid_x)) {
      sum(gap_pos < min(valid_x) | gap_pos > max(valid_x))
    } else n_gap

    if (length(valid_x) < min_valid) {
      diag[[length(diag) + 1L]] <- data.table::data.table(
        animal_id = id, branch = "skipped_lt10", n_gap = n_gap, n_gam = 0L,
        n_fallback = 0L, n_self_extrap = n_out, n_negative_pred = 0L,
        k_eff = NA_integer_, edf = NA_real_, bs_dim = NA_integer_,
        n_valid = length(valid_x), span_pts = NA_real_)
      next
    }

    p <- pred_all[idx]
    n_gam <- 0L; n_neg <- 0L
    if (branch_pop == "gam") {
      filled <- gap_pos[!is.na(p[gap_pos])]
      data.table::set(out, i = idx[filled], j = "daily_feed_g", value = p[filled])
      data.table::set(out, i = idx[filled], j = "is_imputed_feed", value = TRUE)
      n_gam <- length(filled)
      n_neg <- sum(p[gap_pos] < 0, na.rm = TRUE)
    }

    cur <- out$daily_feed_g[idx]
    still_na <- which(is.na(cur[gap_pos]))
    n_fb <- 0L
    if (length(still_na) > 0L && identical(fallback, "median")) {
      med_val <- stats::median(cur, na.rm = TRUE)
      if (!is.na(med_val)) {
        data.table::set(out, i = idx[gap_pos[still_na]], j = "daily_feed_g",
                        value = med_val)
        data.table::set(out, i = idx[gap_pos[still_na]], j = "is_imputed_feed",
                        value = TRUE)
        n_fb <- length(still_na)
      }
    }

    diag[[length(diag) + 1L]] <- data.table::data.table(
      animal_id = id, branch = branch_pop, n_gap = n_gap, n_gam = n_gam,
      n_fallback = n_fb, n_self_extrap = n_out, n_negative_pred = n_neg,
      k_eff = k_eff, edf = edf, bs_dim = bs_dim,
      n_valid = length(valid_x), span_pts = NA_real_)
  }

  attr(out, "arm_diag") <- if (length(diag) > 0) {
    data.table::rbindlist(diag)
  } else {
    .gam_empty_diag()
  }
  out
}

# ============================================================
# 工厂
#
# `arm_param` 里的字段名必须与脚本读的**逐一对上**：`data.table(span = NULL)`
# 会静默丢列，Phase 4 真的踩过这个坑（两处被吞）。所以 GAM 臂显式声明
# k / bs / family / min_valid，并且 06/07 读的时候用 pget() 兜 NA 而不是直接取。
# ============================================================
arm_gam_pure <- function(k = 6, bs = "cr", family = "gaussian",
                         fallback = c("median", "na"), min_valid = 10L) {
  fallback <- match.arg(fallback)
  f <- function(arm_input) {
    out <- .gam_arm_worker(arm_input, k, bs, family, fallback, min_valid)
    attr(out, "arm_param") <- list(kind = "gam_pure", k = k, bs = bs,
                                   family = family, fallback = fallback,
                                   min_valid = min_valid)
    out
  }
  attr(f, "needs_na_view") <- TRUE
  attr(f, "arm_label") <- sprintf("gam_pure(k%d,%s,%s)", k, bs, family)
  attr(f, "arm_param") <- list(kind = "gam_pure", k = k, bs = bs, family = family,
                               fallback = fallback, min_valid = min_valid)
  f
}

arm_gam_router <- function(cfg, k = 6, bs = "cr", family = "gaussian",
                           fallback = c("median", "na"), min_valid = 10L) {
  fallback <- match.arg(fallback)
  f <- function(arm_input) {
    out <- .gam_arm_worker(arm_input, k, bs, family, fallback, min_valid,
                           router_cfg = cfg)
    attr(out, "arm_param") <- list(kind = "gam_router", k = k, bs = bs,
                                   family = family, fallback = fallback,
                                   min_valid = min_valid)
    out
  }
  attr(f, "needs_na_view") <- TRUE
  attr(f, "arm_label") <- sprintf("gam_router(k%d,%s,%s)", k, bs, family)
  attr(f, "arm_param") <- list(kind = "gam_router", k = k, bs = bs,
                               family = family, fallback = fallback,
                               min_valid = min_valid)
  f
}

arm_gam_pop <- function(k = 6, bs = "cr", family = "gaussian",
                        fallback = c("median", "na"), min_valid = 10L,
                        use_bam = TRUE) {
  fallback <- match.arg(fallback)
  f <- function(arm_input) {
    out <- .gam_pop_worker(arm_input, k, bs, family, fallback, min_valid, use_bam)
    attr(out, "arm_param") <- list(kind = "gam_pop", k = k, bs = bs,
                                   family = family, fallback = fallback,
                                   min_valid = min_valid, use_bam = use_bam)
    out
  }
  attr(f, "needs_na_view") <- TRUE
  attr(f, "arm_label") <- sprintf("gam_pop(k%d,%s,%s)", k, bs, family)
  attr(f, "arm_param") <- list(kind = "gam_pop", k = k, bs = bs, family = family,
                               fallback = fallback, min_valid = min_valid,
                               use_bam = use_bam)
  f
}

# 参照臂：什么都不填（计划 §2.5.2 要求 B 轨补一个「不填补」参照，B 轨的 C0）。
#
# 它的 affected-only accuracy **恒等于 0**：eval_metrics_b 把 NA 按 0 计
# （eval_b.R:47 `fcoalesce(est, 0)`），于是 acc = 1 - Σtrue/Σtrue = 0。
# 与 Phase 4 的 raw_zero 同值 ⇒ 它不是一条有分辨力的臂，价值只在于把 floor
# 显式画出来：让「boundary 的 0.19 到底离地板多远」一眼可见。
# 注意它**不满足** n_gam + n_fallback == n_gap，这是它该有的样子，不要拿去断言。
arm_nofill <- function() {
  f <- function(arm_input) {
    out <- data.table::copy(arm_input)
    if (!"is_imputed_feed" %in% names(out)) out[, is_imputed_feed := FALSE]
    data.table::setorder(out, animal_id, record_date)
    d <- out[, {
      m <- is.na(daily_feed_g)
      .(branch = "no_fill", n_gap = sum(m), n_gam = 0L, n_fallback = 0L,
        n_self_extrap = 0L, n_negative_pred = 0L, k_eff = NA_integer_,
        edf = NA_real_, bs_dim = NA_integer_, n_valid = sum(!m),
        span_pts = NA_real_)
    }, by = animal_id][n_gap > 0L]
    attr(out, "arm_diag") <- if (nrow(d) > 0) d else .gam_empty_diag()
    attr(out, "arm_param") <- list(kind = "nofill", k = NA_integer_,
                                   bs = NA_character_, family = NA_character_,
                                   fallback = "none", min_valid = NA_integer_)
    out
  }
  attr(f, "needs_na_view") <- TRUE
  attr(f, "arm_label") <- "nofill"
  attr(f, "arm_param") <- list(kind = "nofill", k = NA_integer_,
                               bs = NA_character_, family = NA_character_,
                               fallback = "none", min_valid = NA_integer_)
  f
}
