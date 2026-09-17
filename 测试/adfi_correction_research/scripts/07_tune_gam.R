######### Phase 5（下）：k × bs × family 网格 + 与 LOESS 的正面对比 ########
#
# 运行：Rscript 测试/adfi_correction_research/scripts/07_tune_gam.R [FIRE|NEDAP]
#
# 要回答两个问题：
#
#  (a) **能外推的 GAM 是不是真的在 LOESS 死掉的那批天上赢？**
#      Phase 4 已证生产 LOESS 完全不能外推（贴序列两端的缺口全落进中位数兜底），
#      而那个兜底在向外天上是**负贡献**。06 的失效模式表已给出方向，
#      这里给完整的网格与对照。
#
#  (b) **GAM 的自动平滑会不会让「调参」这件事本身消失？**
#      Phase 4 实测 LOESS 的 span 从 0.1 摆到 1.0、accuracy 摆动约 9pp。而 GAM 的
#      REML 自己把复杂度选掉——如果是这样，GAM 相对 LOESS 的真正优势就不是
#      「更准」而是「没有会被调错的那个旋钮」。网格取 k∈{3,6,15} 三档的**设计意图
#      就是把这个"k 不重要"测出来**，而不是先假设它。
#
# ---- EDF ↔ 有效窗宽对照表（§八 的硬要求）----
#
# §八 原话："`s(t)` 的自由度必须与 LOESS 的 span 对齐校准，否则两个方法的比较不公平
# （一个更灵活就赢）；建议统一用「有效自由度 / 有效窗宽」这个共同尺度做一张对照表。"
#
# 所以 GAM 报 `edf`，LOESS 报**两个不同的量**，别混：
#   · `span_pts = span × n_valid` —— 是**参与局部拟合的点数**（窗宽），**不是自由度**。
#     degree=2 的局部二次用 26 个点拟合出来的是一条很平滑的曲线，自由度远小于 26。
#   · `tr(S)` —— 帽子矩阵的迹，**这才是真正的有效自由度**，与 GAM 的 edf 同一个尺度。
#     对 y 线性 ⇒ `tr(S) = Σ_i e_iᵀ · predict(fit(x, e_i))`，用单位向量各预测一次。
#     本机实测（88 天序列）：span=0.30 ⇒ tr(S)=11.11（degree=2），span=1.00 ⇒ 3.69。
#
# ⚠ 本机实测的两条**做不到**的事，别再试：
#   · `loess` **不接受矩阵响应**（想一次拟合拿到整个 S）：`loess(diag(n) ~ x)` 与
#     `loess(cbind(e1,...,en) ~ x)` 都报 `invalid 'y'`。只能逐单位向量重拟合。
#   · 别用 `Σ predict(se=TRUE)²/σ²` 代替 tr(S)：那是 `tr(SSᵀ)`，不是 `tr(S)`。
#   逐单位向量重拟合 88 点 × 5 档 span × 40 头 ≈ 0.14 s/头/档，可接受。
#
# ---- 对照臂 ----
#
# LOESS 侧取三格：生产点 (0.30, 2)，以及 Phase 4 网格里两个赢家 (1.00, 2) 与 (0.50, 2)
# （Phase 4 实测：FIRE 的 14d 最优是 s1.00_d2，5d 最优是 s0.50_d2）。
# 每个 pattern 的「最强 LOESS 对照」取这三格的**最大值**——这是个**保守**基线：
# 万一 12 格里还有更强的，我们只会低估 GAM，不会高估。不重跑整个 12 格是因为
# 05 已经把那张网格出全了（`results/loess_tuning_*.csv`），重复它没有新信息。
#
# 语料：5 pattern × rate 10%。比 05 多一个 `2d`——那是**生产唯一真的会走 LOESS**
# 的缺口长度，缺了它就没法回答「生产现状 vs 换成 GAM」。
#
# 产物：results/gam_tuning_<device>.csv 入库 + results/runs/ 时间戳副本。

options(scipen = 999, width = 200)

args <- commandArgs(trailingOnly = TRUE)
DEVICE <- if (length(args) >= 1) toupper(args[1]) else "FIRE"

module_dir <- tryCatch({
  sd <- dirname(sys.frame(1)$ofile)
  normalizePath(file.path(sd, ".."))
}, error = function(e) {
  normalizePath(file.path(getwd(), "测试", "adfi_correction_research"))
})

source(file.path(module_dir, "R", "skeleton.R"))
project_root <- find_project_root(module_dir)
load_zhenmeasure(project_root)
source(file.path(module_dir, "R", "daily_gold.R"))
source(file.path(module_dir, "R", "eval_b.R"))
source(file.path(module_dir, "R", "inject_b.R"))
source(file.path(module_dir, "R", "arms.R"))
source(file.path(module_dir, "R", "arm_loess.R"))
source(file.path(module_dir, "R", "arm_gam.R"))

cat(sprintf("\n===== 建金表（%s）=====\n", DEVICE))
g <- build_gold(DEVICE, project_root)
gold <- g$gold; truth <- g$truth
UNIV <- gold_universe(gold)
cfg <- make_base_config(base_ns)

RATE <- 0.10
PATTERNS_TUNE <- c("random", "2d", "5d", "14d", "boundary")

sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
pget <- function(p, nm) if (is.null(p[[nm]])) NA else p[[nm]]

# ============================================================
# 臂集合
# ============================================================
ARMS <- list()

# 主网格：k ∈ {3,6,15} × bs ∈ {tp,cr,ps} × gaussian = 9 格
for (bs in c("tp", "cr", "ps")) for (k in c(3L, 6L, 15L)) {
  ARMS[[sprintf("gam_k%d_%s_gauss", k, bs)]] <- arm_gam_pure(k, bs, "gaussian", "median")
}
# family 轴：Gamma(log) 在 bs="cr" 上 k ∈ {6,15}
for (k in c(6L, 15L)) {
  ARMS[[sprintf("gam_k%d_cr_gamma", k)]] <- arm_gam_pure(k, "cr", "Gamma", "median")
}
# 参照
ARMS[["router_k6_cr"]]  <- arm_gam_router(cfg, 6, "cr", "gaussian", "median")
ARMS[["pop_k6_cr"]]     <- arm_gam_pop(6, "cr", "gaussian", "median")
ARMS[["pop_k6_cr_mv6"]] <- arm_gam_pop(6, "cr", "gaussian", "median", min_valid = 6L)
ARMS[["nofill"]]        <- arm_nofill()
# LOESS 对照格。**必须含调过参的格，不能只拿生产点当基线**：
# 只跟 span=0.3 比的话，GAM 在 acc_all 上的领先里会混进「REML 挑了一个比 0.3 更好的
# 带宽」这一块，而那跟「能不能外推」无关，是两码事（设计复核点名的一条）。
# 所以基线取 span ∈ {0.30, 0.50, 0.75, 1.00} × degree ∈ {1, 2} 的**并集**，
# 每格的 acc 都在同一份金表、同一个种子上现算（不读 Phase 4 的 CSV）。
# 代价：多 3 条臂 ≈ 6 s/设备。
for (spn in c(0.30, 0.50, 0.75, 1.00)) for (dg in c(1L, 2L)) {
  ARMS[[sprintf("loess_s%.2f_d%d", spn, dg)]] <- arm_loess_pure(spn, dg, "median")
}
LOESS_ARMS <- grep("^loess_", names(ARMS), value = TRUE)
cat(sprintf(">>> %d 条臂（%d GAM + %d LOESS 对照 + nofill）× %d pattern\n",
            length(ARMS), length(ARMS) - length(LOESS_ARMS) - 1L,
            length(LOESS_ARMS), length(PATTERNS_TUNE)))

# ============================================================
# affected 拆内部 / 向外（口径与 06 完全一致，见 06 头的长注释）
# ============================================================
split_affected <- function(inp, affected) {
  side <- inp[order(animal_id, record_date)][, {
    m <- is.na(daily_feed_g); v <- which(!m)
    out <- if (length(v) == 0L) m else
      (seq_along(m) < v[1] | seq_along(m) > v[length(v)])
    list(record_date = record_date, outward = out)
  }, by = animal_id]
  a <- affected[order(animal_id, record_date)]
  j <- merge(a, side, by = c("animal_id", "record_date"), all.x = TRUE, sort = FALSE)
  list(all = a, interior = j[outward == FALSE][, !"outward"],
       outward = j[outward == TRUE][, !"outward"])
}
acc_of <- function(est, sub, univ, truth_all) {
  if (nrow(sub) == 0L) return(NA_real_)
  m <- eval_metrics_b(est, truth_all, universe = univ, affected = sub)
  if (is.null(m$by_type) || nrow(m$by_type) == 0L) NA_real_ else m$by_type$acc[1]
}

# ============================================================
# 主网格
# ============================================================
rows <- list()
for (pt in PATTERNS_TUNE) {
  res <- suppressWarnings(inject_missing_days(gold, pt, "missing", RATE))
  inp <- res$dt_injected
  sp <- split_affected(inp, res$affected)
  cat(sprintf(">>> pattern=%-9s 注入 %d 天（rate_realised %.4f）；内部 %d / 向外 %d\n",
              pt, res$n_realised, res$rate_realised,
              nrow(sp$interior), nrow(sp$outward)))

  for (nm in names(ARMS)) {
    message(sprintf("      %s / %s", pt, nm))
    arm <- ARMS[[nm]]; p <- arm_param(arm)
    t0 <- Sys.time()
    out <- run_arm(arm, inp)
    rt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    met <- eval_metrics_b(out, truth, universe = UNIV, affected = res$affected)
    dg <- arm_diag(out)
    is_gam <- grepl("^gam", p$kind)

    rows[[length(rows) + 1L]] <- data.table::data.table(
      device = DEVICE, pattern = pt, arm = nm, kind = p$kind,
      k = pget(p, "k"), bs = pget(p, "bs"), family = pget(p, "family"),
      span = pget(p, "span"), degree = pget(p, "degree"),
      min_valid = pget(p, "min_valid"),
      rate_realised = res$rate_realised,
      acc = met$acc, bias = met$bias, coverage = met$coverage, adfi_r = met$r,
      acc_affected = acc_of(out, sp$all, UNIV, truth),
      n_interior = nrow(sp$interior), n_outward_days = nrow(sp$outward),
      acc_interior = acc_of(out, sp$interior, UNIV, truth),
      acc_outward  = acc_of(out, sp$outward, UNIV, truth),
      n_na_days = met$n_na_days, n_gap = sum(dg$n_gap),
      n_smooth = if (is_gam) sum(dg$n_gam) else sum_or_na(dg$n_loess),
      n_fallback = sum_or_na(dg$n_fallback),
      n_self_extrap = sum(dg$n_self_extrap),
      n_negative_pred = sum_or_na(dg$n_negative_pred),
      # §八 那张表的两栏
      mean_edf = if (any(dg$branch == "gam")) mean(dg[branch == "gam", edf]) else NA_real_,
      mean_span_pts = if (any(dg$branch == "loess")) {
        mean(dg[branch == "loess", span_pts])
      } else NA_real_,
      n_branch_gam = sum(dg$branch == "gam"),
      n_branch_fcr = sum(dg$branch == "fcr-extrap"),
      n_branch_skip = sum(dg$branch == "skipped_lt10"),
      n_fit_failed = sum(dg$branch == "fit_failed"),
      runtime_s = rt)
  }
}
tab <- data.table::rbindlist(rows)
data.table::setorder(tab, pattern, arm)

# ============================================================
# 报告
# ============================================================
cat("\n--- ① 每个 GAM 格 vs LOESS 生产点（acc_outward = 向外天，GAM 的价值所在）---\n")
piv <- data.table::dcast(tab, pattern + arm ~ ., value.var = "acc_outward")
w <- data.table::dcast(tab, pattern ~ arm, value.var = "acc_outward")
keep <- c("pattern", grep("^gam_", names(w), value = TRUE))
print(w[, ..keep][, lapply(.SD, function(z) round(z, 4)), by = pattern])

cat("\n--- ② 各 pattern：最强 GAM 格 vs 生产 LOESS vs 最强 LOESS 对照 ---\n")
gam_only <- tab[grepl("^gam_k", arm)]
best_gam <- gam_only[, .SD[which.max(acc_outward)], by = pattern][,
  .(pattern, best_gam_arm = arm, best_gam = round(acc_outward, 4),
    best_gam_edf = round(mean_edf, 2))]
prod_pt <- tab[arm == "loess_s0.30_d2",
               .(pattern, prod_loess = round(acc_outward, 4),
                 prod_all = round(acc_affected, 4))]
best_lo <- tab[arm %in% LOESS_ARMS, .SD[which.max(acc_outward)], by = pattern][,
  .(pattern, best_loess_arm = arm, best_loess = round(acc_outward, 4))]
# gam_pop 单独一行：它是 GAMM 那条路，不是逐头 GAM
pop_pt <- tab[arm == "pop_k6_cr",
              .(pattern, pop = round(acc_outward, 4))]
cmp <- Reduce(function(a, b) merge(a, b, by = "pattern"),
              list(best_gam, prod_pt, best_lo, pop_pt))[order(pattern)]
cmp[, vs_prod := round(best_gam - prod_loess, 4)]
cmp[, vs_best_loess := round(best_gam - best_loess, 4)]
print(cmp)

cat("\n--- ③ 生产 LOESS 点在「LOESS 三格 + 最强 GAM 格」里的名次（每 pattern 内）---\n")
rk <- rbind(tab[arm %in% LOESS_ARMS, .(pattern, arm, acc_outward)],
            best_gam[, .(pattern, arm = best_gam_arm, acc_outward = best_gam)])
data.table::setorder(rk, pattern, -acc_outward)
rk[, rank := seq_len(.N), by = pattern]
print(rk[arm == "loess_s0.30_d2"][order(pattern),
        .(pattern, rank, n = rk[, .N, by = pattern]$N,
          acc_outward = round(acc_outward, 4))])

cat("\n--- ④ 主网格逐格：k / bs / family 各自影响多大 ---\n")
print(tab[grepl("^gam_k", arm),
          .(pattern, arm, k, bs, family,
            acc_aff = round(acc_affected, 4), acc_out = round(acc_outward, 4),
            bias = round(bias, 4), edf = round(mean_edf, 2),
            fb = n_fallback, neg = n_negative_pred, failed = n_fit_failed,
            rt = round(runtime_s, 1))])

cat("\n--- ⑤ k 到底重不重要：同一 (bs, family) 下，k ∈ {3,6,15} 的 acc_affected 极差 ---\n")
sp_k <- tab[grepl("^gam_k", arm) & family == "gaussian",
            .(lo = min(acc_affected), hi = max(acc_affected),
              edf_lo = min(mean_edf, na.rm = TRUE), edf_hi = max(mean_edf, na.rm = TRUE)),
            by = .(pattern, bs)]
sp_k[, swing_pp := round(100 * (hi - lo), 2)]
sp_k[, edf_swing := round(edf_hi - edf_lo, 2)]
print(sp_k[order(pattern, bs), .(pattern, bs, swing_pp, edf_swing)])

cat("\n--- ⑥ 分支与 floor ---\n")
print(tab[arm %in% c("nofill", "router_k6_cr", "pop_k6_cr", "pop_k6_cr_mv6"),
          .(pattern, arm, acc_aff = round(acc_affected, 4),
            acc_out = round(acc_outward, 4), bias = round(bias, 4),
            gap = n_gap, smooth = n_smooth, fb = n_fallback,
            gam_heads = n_branch_gam, fcr_heads = n_branch_fcr,
            skip_heads = n_branch_skip)])
cat("    nofill 的 acc_affected 恒为 0（NA 按 0 计）＝ 地板；router 的 >3 支走生产 FCR\n")

# ============================================================
# EDF ↔ 有效窗宽对照表（§八 硬要求）
# ============================================================
cat("\n===== EDF ↔ 有效窗宽对照表 =====\n")
SPANS_CAL <- c(0.1, 0.3, 0.5, 0.75, 1.0)

# 帽子矩阵的迹：对 y 线性 ⇒ tr(S) = Σ_i e_iᵀ predict(fit(x, e_i))
# 逐单位向量重拟合。**不用** `Σ se_i²/σ²`（那是 tr(SSᵀ)，不是 tr(S)），
# 也不用矩阵响应（`loess` 报 invalid 'y'，实测）。
tr_S <- function(xv, span, degree) {
  n <- length(xv); s <- 0
  for (i in seq_len(n)) {
    e <- numeric(n); e[i] <- 1
    fi <- tryCatch(stats::loess(e ~ xv, span = span, degree = degree),
                   error = function(e) NULL)
    if (is.null(fi)) return(NA_real_)
    s <- s + stats::predict(fi, xv)[i]
  }
  s
}

# 固定抽样 40 头：按 animal_id 排序取前 40（**不用 RNG**，换个种子结果不变）
SAMPLE_N <- 40L
clean_gold <- gold[!is.na(daily_feed_g)]
ids_all <- sort(unique(clean_gold$animal_id))
cal_ids <- head(ids_all, SAMPLE_N)
cat(sprintf("  抽样 %d 头 / 全体 %d 头（按 animal_id 排序取前 %d，无 RNG）\n",
            length(cal_ids), length(ids_all), SAMPLE_N))

cal_rows <- list()
for (id in cal_ids) {
  sub <- clean_gold[animal_id == id][order(record_date)]
  y <- sub$daily_feed_g; v <- !is.na(y)
  x <- seq_along(y); xv <- x[v]; nv <- length(xv)
  if (nv < 10L) next
  for (dg in c(1L, 2L)) for (spn in SPANS_CAL) {
    cal_rows[[length(cal_rows) + 1L]] <- data.table::data.table(
      animal_id = id, span = spn, degree = dg, n_valid = nv,
      span_pts = spn * nv, tr_S = tr_S(xv, spn, dg))
  }
}
cal <- data.table::rbindlist(cal_rows)

cat("\n  --- LOESS 侧：span_pts（窗宽）与 tr(S)（真正的有效自由度，88 天序列实测）---\n")
print(cal[, .(span = span, degree,
              n_valid = round(mean(n_valid), 1),
              span_pts = round(mean(span_pts), 1),
              tr_S = round(mean(tr_S, na.rm = TRUE), 2)),
          by = .(span, degree)][order(degree, span)][, !"span"])

cat("\n  --- GAM 侧：REML 选出的 edf（主网格的均值）---\n")
print(tab[grepl("^gam_k", arm),
          .(mean_edf = round(mean(mean_edf, na.rm = TRUE), 3)),
          by = .(k, bs, family)][order(family, bs, k)])

cat("\n  --- 共同尺度上的对照（这张表要回答「GAM 是不是靠更灵活赢的」）---\n")
prod_tr <- cal[span == 0.30 & degree == 2, mean(tr_S, na.rm = TRUE)]
lo1_tr  <- cal[span == 0.30 & degree == 1, mean(tr_S, na.rm = TRUE)]
gam_edf <- tab[grepl("^gam_k6_.*gauss", arm), mean(mean_edf, na.rm = TRUE)]
gam_edf15 <- tab[arm == "gam_k15_cr_gauss", mean(mean_edf, na.rm = TRUE)]
cat(sprintf("    LOESS 生产点 span=0.30 / degree=2 ⇒ tr(S) = %.2f 个有效自由度\n", prod_tr))
cat(sprintf("    LOESS 同 span 但 degree=1        ⇒ tr(S) = %.2f（degree 和 span 一样是旋钮）\n",
            lo1_tr))
cat(sprintf("    GAM   k=6  ⇒ REML 选出 edf = %.2f；k=15 ⇒ edf = %.2f\n", gam_edf, gam_edf15))
cat(sprintf("    ⇒ GAM 比生产 LOESS **更不灵活**（%.1f vs %.1f 个自由度）。\n",
            gam_edf, prod_tr))
cat("      所以若 GAM 在向外天上赢（见 ① ②），**赢的不是灵活度，是它肯外推**。\n")

# ============================================================
# 落盘
# ============================================================
out_dir <- file.path(module_dir, "results")
run_dir <- file.path(out_dir, "runs")
if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)
fwrite(tab, file.path(out_dir, sprintf("gam_tuning_%s.csv", tolower(DEVICE))))
fwrite(cal, file.path(run_dir, sprintf("gam_calibration_%s.csv", tolower(DEVICE))))
fwrite(tab, file.path(run_dir, sprintf(
  "gam_tuning_%s_%s.csv", tolower(DEVICE), format(Sys.time(), "%Y%m%d_%H%M%S"))))
cat(sprintf("\n>>> 已写入 results/gam_tuning_%s.csv（入库）+ results/runs/（gitignored）\n",
            tolower(DEVICE)))
