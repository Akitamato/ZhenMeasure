######### Phase 4（下）：LOESS 的 span × degree 网格 ########
#
# 运行：Rscript 测试/adfi_correction_research/scripts/05_tune_loess.R [FIRE|NEDAP]
#
# 要回答的问题只有一个：**生产的 span = 0.3 / degree = 2 是不是次优？**
#
#   `zhenm_config_defaults.R:69` 有个 `loess_span = 0.75`，全包**无人读取**——
#   这坐实了「生产值不是调出来的」，也给网格提供了一个有意思的候选点（0.75 恰好
#   是 stats::loess 自己的缺省 span）。
#
# 网格：span ∈ {0.1, 0.2, 0.3, 0.5, 0.75, 1.0} × degree ∈ {1, 2} = 12 条 pure 臂，
# 外加 `prod`（生产点，带路由）与 `raw_zero(0.3,2)`（反事实对照）两条。
#
# 语料：pattern ∈ {random, 5d, 14d, boundary} × rate 10%——四个原型（离散 / 中连续 /
# 长连续 / 边界）。**不跑满轴**：开发计划 §十八 把满轴（6 pattern × 多 rate）留给
# Phase 7 的 L1 语料，这里只取能分辨方法差异的最小充分集。
#
# 两个必须随每个数字一起报的量（否则数字不可归因）：
#   · `rate_realised` —— 连续 run 受可用空间限制，实际比例 ≠ 名义 10%
#   · `mean_span_pts` —— span 是「参与拟合的**点数**比例」，不是日历宽度。
#     有效带宽 = span × n_valid，而 n_valid 随注入率下降 ⇒ 同一个 span 在不同 rate
#     下覆盖的日历天数不同，这一项把这层混淆显式摊开。
#
# 产物：results/loess_tuning_<device>.csv 入库；逐次运行原始产物在 results/runs/。

options(scipen = 999, width = 200)   # 宽表不要按 80 列折断，否则列名与值对不上

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

cat(sprintf("\n===== 建金表（%s）=====\n", DEVICE))
g <- build_gold(DEVICE, project_root)
gold <- g$gold; truth <- g$truth
UNIV <- gold_universe(gold)
cfg <- make_base_config(base_ns)

RATE <- 0.10
PATTERNS_TUNE <- c("random", "5d", "14d", "boundary")
SPANS <- c(0.1, 0.2, 0.3, 0.5, 0.75, 1.0)
DEGREES <- c(1L, 2L)

# 12 条 pure 臂 + prod + raw_zero
ARMS <- list()
for (dg in DEGREES) for (sp in SPANS) {
  nm <- sprintf("pure_s%.2f_d%d", sp, dg)
  ARMS[[nm]] <- arm_loess_pure(sp, dg, "median")
}
ARMS[["prod"]] <- arm_loess_prod(cfg)
ARMS[["raw_zero_s0.30_d2"]] <- arm_loess_raw_zero(0.3, 2)

sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

rows <- list()
for (pt in PATTERNS_TUNE) {
  res_m <- suppressWarnings(inject_missing_days(gold, pt, "missing", RATE))
  res_z <- suppressWarnings(inject_missing_days(gold, pt, "zero", RATE))
  cat(sprintf(">>> pattern=%-9s 注入 %d 天（占 observed %.4f）\n",
              pt, res_m$n_realised, res_m$rate_realised))

  for (nm in names(ARMS)) {
    # stderr 做进度（不带缓冲，stdout 走管道时会被块缓冲，出错点会被藏住）
    message(sprintf("      %s / %s", pt, nm))
    arm <- ARMS[[nm]]
    p <- arm_param(arm)
    inp <- if (identical(p$kind, "raw_zero")) res_z$dt_injected else res_m$dt_injected

    t0 <- Sys.time()
    out <- run_arm(arm, inp)
    rt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    met <- eval_metrics_b(out, truth, universe = UNIV, affected = res_m$affected)
    dg <- arm_diag(out)

    rows[[length(rows) + 1L]] <- data.table::data.table(
      device = DEVICE, pattern = pt, arm = nm, kind = p$kind,
      span = p$span, degree = p$degree,
      rate_realised = res_m$rate_realised,
      # 平均有效带宽（只统计真的跑了 LOESS 的头）
      mean_span_pts = if (any(dg$branch == "loess")) {
        mean(dg[branch == "loess", span_pts])
      } else NA_real_,
      acc = met$acc, bias = met$bias, coverage = met$coverage, adfi_r = met$r,
      acc_affected = if (is.null(met$by_type)) NA_real_ else met$by_type$acc[1],
      n_gap = sum(dg$n_gap), n_loess = sum_or_na(dg$n_loess),
      n_fallback = sum_or_na(dg$n_fallback),
      # 分支计数（每格 = 多少头走了哪条路）。prod 的 n_loess/n_fallback 是 NA，
      # 想知道它到底跑没跑 LOESS 只能看这个
      n_branch_loess = sum(dg$branch == "loess"),
      n_branch_fcr = sum(dg$branch == "fcr-extrap"),
      n_branch_skip = sum(dg$branch %in% c("skipped_lt10", "no_gap")),
      n_fit_failed = sum(dg$branch == "fit_failed"),
      n_na_days = met$n_na_days,
      runtime_s = rt)
  }
}

tab <- data.table::rbindlist(rows)
# setorder（不是 setorderv）：后者第二参要字符向量，会把 pattern 当列名、kind 当排序列
data.table::setorder(tab, pattern, kind, span, degree)

# ---------- 结果 ----------
cat("\n--- 每一格（acc_affected = 只算被注入的天）---\n")
print(tab[kind == "pure",
          .(pattern, span, degree, acc_affected = round(acc_affected, 4),
            bias = round(bias, 4), mean_span_pts = round(mean_span_pts, 1),
            n_fallback, n_fit_failed, rt = round(runtime_s, 1))])

cat("\n--- 各 pattern 的最优 pure 格 vs 生产点 ---\n")
best <- tab[kind == "pure", .SD[which.max(acc_affected)],
            by = pattern][, .(pattern, best_arm = arm,
                              best_acc = round(acc_affected, 4))]
prod_pt <- tab[arm == "prod" & kind == "prod",
               .(pattern, prod_acc = round(acc_affected, 4),
                 prod_bias = round(bias, 4))]
print(merge(best, prod_pt, by = "pattern")[
  order(pattern)][, gap := round(best_acc - prod_acc, 4)][])

# 生产点 (0.3, 2) 自身在网格里的位置——「0.3 是否次优」的直接答案
cat("\n--- 生产点 (0.3, 2) 在网格内的名次（同 pattern 内按 acc_affected 排序）---\n")
rk <- tab[kind == "pure", .(arm, span, degree, acc_affected),
          by = pattern][order(pattern, -acc_affected)]
rk[, rank := seq_len(.N), by = pattern]
print(rk[span == 0.3 & degree == 2][order(pattern),
        .(pattern, rank, n_cells = rk[, .N, by = pattern]$N,
          acc_affected = round(acc_affected, 4))])

cat("\n--- raw_zero（反事实：相信注入的 0 是真观测）---\n")
print(tab[kind == "raw_zero",
          .(pattern, acc = round(acc, 4), bias = round(bias, 4),
            acc_affected = round(acc_affected, 4), n_gap)])

cat("\n--- 生产分支：prod 在每种 pattern 上有多少头走了 LOESS / FCR 外推 / 跳过 ---\n")
print(tab[kind == "prod", .(pattern, loess_heads = n_branch_loess,
                            fcr_heads = n_branch_fcr, skip_heads = n_branch_skip,
                            acc_affected = round(acc_affected, 4))])

out_dir <- file.path(module_dir, "results")
run_dir <- file.path(out_dir, "runs")
if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)
fwrite(tab, file.path(out_dir, sprintf("loess_tuning_%s.csv", tolower(DEVICE))))
fwrite(tab, file.path(run_dir, sprintf(
  "loess_tuning_%s_%s.csv", tolower(DEVICE), format(Sys.time(), "%Y%m%d_%H%M%S"))))
cat(sprintf("\n>>> 已写入 %s（入库）+ results/runs/（gitignored）\n",
            file.path("results", sprintf("loess_tuning_%s.csv", tolower(DEVICE)))))
