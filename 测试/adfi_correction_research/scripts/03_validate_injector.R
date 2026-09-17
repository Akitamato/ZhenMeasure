######### Phase 3（下）：校验缺失天注入器的生成逻辑 ########
#
# 运行：Rscript 测试/adfi_correction_research/scripts/03_validate_injector.R [FIRE|NEDAP|YANGXIANG]
#
# 计划 §十八 Phase 3 要求「验证生成逻辑」。这里逐条断言：
#   ① affected ⊆ universe
#   ② 实际注入比例 = rate；missing 模式的注入天值为 NA，zero 模式为 0
#   ③ 未注入表过 eval_metrics_b → acc == 1 且 coverage == 1
#   ④ 注入后不恢复 → acc < 1 且 coverage < 1
#   ④b **对照**：同样一份「稀疏」输入过旧 eval_metrics 会严重虚高 —— 这是
#      eval_b.R 必须存在、不能复用 eval_metrics() 的实证
#   ⑤ 同种子重跑，affected 逐位一致
#   ⑥ boundary 模式不越出每头补齐后的 span

options(scipen = 999)

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

N_PASS <- 0L; N_FAIL <- 0L
check <- function(label, expr) {
  ok <- tryCatch({ isTRUE(expr) }, error = function(e) {
    cat(sprintf("      [异常] %s\n", conditionMessage(e))); FALSE
  })
  if (ok) N_PASS <<- N_PASS + 1L else N_FAIL <<- N_FAIL + 1L
  cat(sprintf("  %-4s %s\n", if (ok) "PASS" else "FAIL", label))
  invisible(ok)
}

cat(sprintf("\n===== 建金表（%s）=====\n", DEVICE))
g <- build_gold(DEVICE, project_root)
gold <- g$gold; truth <- g$truth

# 打分 universe = 注入前的 observed 天（gated 只报告不打分，见 daily_gold.R）。
# 注入器可采样的集合与它逐位相同，故「注入率」的分母就是 nrow(UNIV)。
UNIV <- gold_universe(gold)
n_obs <- nrow(UNIV)
cat(sprintf(">>> 打分 universe = observed %d 天（truth_daily %d − gated %d）\n",
            n_obs, nrow(truth), nrow(truth) - n_obs))

# 补齐 generated 状态的初值（build_gold 已设，这里防御性确认）
if (!"injected" %in% names(gold)) gold[, injected := FALSE]
if (!"zero_injected" %in% names(gold)) gold[, zero_injected := FALSE]

# ============================================================
# ① 全 pattern × mode 组合都能跑通，且 affected ⊆ universe
# ============================================================
cat("\n===== ① pattern × mode 跑通 + affected ⊆ universe =====\n")
RATE <- 0.10
combos <- list()
for (pt in PATTERNS) for (md in c("missing", "zero")) {
  res <- inject_missing_days(gold, pt, md, RATE)
  combos[[paste(pt, md)]] <- res
  lab <- sprintf("%-11s %-7s 注入 %5d 天（占 observed %d 的 %.2f%%）",
                 pt, md, res$n_realised, n_obs, 100 * res$rate_realised)
  hit <- nrow(merge(res$affected, truth, by = c("animal_id", "record_date")))
  check(paste0(lab, " | ⊆universe=", hit == res$n_realised),
        hit == res$n_realised)
}
# run 模式（2d/5d/14d）按 run 个数配额，落地天数必是 L 的整数倍且贴近目标
for (pt in names(RUN_LEN)) {
  L <- RUN_LEN[[pt]]
  res <- combos[[paste(pt, "missing")]]
  check(sprintf("%-4s 落地天数是 run 长 %d 的整数倍（%d 天）",
                pt, L, res$n_realised),
        res$n_realised %% L == 0L)
  check(sprintf("%-4s 落地量在目标的一个 run 之内（%d vs 目标 %d）",
                pt, res$n_realised, res$n_target),
        res$n_target - res$n_realised < L)
}

# run 的**原子性**：注入标记的连续段必须恰好 L 天。
# 相邻两个 run 首尾相接（起点恰在前一个的 d+L）时，`.pick_runs` 的冲突判据若只写
# `< L` 会放行，两个 2d run 合并成 4 天缺口——**pattern 名字就不再等于缺口长度**，
# 分 pattern 的失效归因会撒谎。判据必须是 `< L + 1L`（至少隔一天）。
for (pt in names(RUN_LEN)) {
  L <- RUN_LEN[[pt]]
  gm <- combos[[paste(pt, "missing")]]$gold_marked
  seg <- gm[order(animal_id, record_date)][, {
    r <- rle(injected)
    list(len = r$lengths[r$values])
  }, by = animal_id]
  check(sprintf("%-4s 注入标记的连续段长度全部 == %d（%d 段，实测 %s）",
                pt, L, nrow(seg),
                if (nrow(seg) == 0) "无" else
                  sprintf("%d..%d", min(seg$len), max(seg$len))),
        nrow(seg) > 0 && all(seg$len == L))
  # 诊断（不断言）：值层面的 NA 缺口分布。若某 run 与既有的无访问天相邻，NA 缺口会
  # 长于 L —— 这是语料固有的，不是注入器的缺陷；报出来免得日后误判。
  gap <- gm[order(animal_id, record_date)][, {
    r <- rle(is.na(daily_feed_g))
    list(len = r$lengths[r$values])
  }, by = animal_id]
  n_gt <- if (nrow(gap)) sum(gap$len > L) else 0L
  cat(sprintf("       └ NA 缺口 %d 段，其中长于 %d 的 %d 段（run 与既有空洞相邻所致）\n",
              nrow(gap), L, n_gt))
}

# ============================================================
# ② 比例与值语义
# ============================================================
cat("\n===== ② 比例与值语义 =====\n")
r_rand <- combos[["random missing"]]
check(sprintf("random 模式注入天数恰为目标（%d == %d）",
              r_rand$n_realised, r_rand$n_target),
      r_rand$n_realised == r_rand$n_target)

for (key in names(combos)) {
  res <- combos[[key]]
  tol <- max(0.01, RATE * 0.25)   # 连续 run 受可用空间限制，给 25% 相对容差
  ok <- abs(res$rate_realised - RATE) <= tol
  check(sprintf("%-18s 实际比例 %.4f 与目标 %.2f 相符（容差 %.3f）",
                key, res$rate_realised, RATE, tol), ok)
}

# 值语义：missing → NA；zero → 0；未注入的 observed 天在两模式下都不受影响
for (md in c("missing", "zero")) {
  res <- combos[[paste("random", md)]]
  gm <- res$gold_marked
  inj_val_ok <- if (md == "missing") {
    all(is.na(gm[injected == TRUE]$daily_feed_g))
  } else {
    all(gm[injected == TRUE]$daily_feed_g == 0)
  }
  check(sprintf("random/%s：注入天值为 %s", md,
                if (md == "missing") "NA" else "0"), inj_val_ok)
  untouched <- gm[injected == FALSE & DFI_status == "observed"]
  check(sprintf("random/%s：未注入的 observed 天仍等于真值", md),
        identical(untouched$daily_feed_g, untouched$true_feed))
}

# ============================================================
# ③④ 评价口径
# ============================================================
cat("\n===== ③④ eval_metrics_b 契约 =====\n")
clean_view <- gold_to_arm_input(gold)
m_clean <- eval_metrics_b(clean_view, truth, universe = UNIV)
check(sprintf("未注入：acc == 1（实际 %.6f）", m_clean$acc),
      isTRUE(all.equal(m_clean$acc, 1)))
check(sprintf("未注入：coverage == 1（实际 %.6f）", m_clean$coverage),
      isTRUE(all.equal(m_clean$coverage, 1)))
check(sprintf("未注入：n_universe == observed 天数（%d == %d）",
              m_clean$n_universe, n_obs),
      m_clean$n_universe == n_obs)

res_m <- combos[["random missing"]]
m_no <- eval_metrics_b(res_m$dt_injected, truth, universe = UNIV,
                       affected = res_m$affected)
check(sprintf("注入后不恢复：acc < 1（实际 %.4f）", m_no$acc), m_no$acc < 1)
check(sprintf("注入后不恢复：coverage < 1（实际 %.4f）", m_no$coverage),
      m_no$coverage < 1)
# coverage 的精确期望：universe 里被抹掉的天 / universe（不是 / truth_daily）
expect_cov <- (n_obs - res_m$n_realised) / n_obs
check(sprintf("注入后不恢复：coverage == 1 − 注入天/observed（%.6f vs %.6f）",
              m_no$coverage, expect_cov),
      abs(m_no$coverage - expect_cov) < 1e-12)

# ============================================================
# ④b 对照：旧 eval_metrics 对稀疏输入会虚高——eval_metrics_b 的存在理由
# ============================================================
cat("\n===== ④b 对照：旧 eval_metrics 在稀疏输入上虚高 =====\n")
# 生产管线的日级表是**稀疏**的：那天没有数据就没有那一行。
# 问题 B 的臂删掉整天后返回的正是这种表，故这里显式造一份稀疏视图。
sparse <- res_m$dt_injected[!is.na(daily_feed_g)]
old <- eval_metrics(sparse, truth, res_m$affected)
new <- m_no
cat(sprintf("     新 eval_metrics_b : acc=%.4f coverage=%.4f\n", new$acc, new$coverage))
cat(sprintf("     旧 eval_metrics   : acc=%.4f coverage=%.4f\n", old$acc, old$coverage))
check(sprintf("旧口径把「丢了 %.1f%% 的天」读成 acc=%.4f（虚高 %.4f）",
              100 * res_m$rate_realised, old$acc, old$acc - new$acc),
      old$acc > new$acc + 0.05)
check(sprintf("旧口径的 coverage 被钉在 %.4f（应当 ≈ %.4f）",
              old$coverage, new$coverage),
      old$coverage > 0.99)

# ============================================================
# ⑤ 同种子可复现
# ============================================================
cat("\n===== ⑤ 同种子复现 =====\n")
for (key in c("random missing", "5d missing", "boundary zero", "error-assoc missing")) {
  parts <- strsplit(key, " ")[[1]]
  a <- inject_missing_days(gold, parts[1], parts[2], RATE)
  b <- inject_missing_days(gold, parts[1], parts[2], RATE)
  same <- identical(a$affected, b$affected) && identical(a$seed, b$seed)
  check(sprintf("%-20s 两次运行 affected 逐位一致（seed=%d）", key, a$seed), same)
}
# 不同 pattern 不能撞种子
seeds <- vapply(PATTERNS, function(p) inject_seed(RATE, p), numeric(1))
check(sprintf("6 个 pattern 的种子互不相同（%d 个唯一值）", length(unique(seeds))),
      length(unique(seeds)) == length(PATTERNS))

# ============================================================
# ⑥ boundary 不越界
# ============================================================
cat("\n===== ⑥ boundary 不越出 span =====\n")
span <- gold[, .(lo = min(record_date), hi = max(record_date)), by = animal_id]
b_aff <- merge(combos[["boundary missing"]]$affected, span, by = "animal_id")
check(sprintf("boundary 注入的 %d 天全部落在各自 [min, max] 内", nrow(b_aff)),
      all(b_aff$record_date >= b_aff$lo & b_aff$record_date <= b_aff$hi))
# boundary 必须真的贴边（否则它就不是边界场景）
edge <- b_aff[, .(n_edge = sum(record_date == lo | record_date == hi),
                  n = .N), by = animal_id][n > 0]
check(sprintf("boundary 应贴序列两端：%d 头中 %d 头的注入点落在端点上",
              nrow(edge), sum(edge$n_edge > 0)),
      all(edge$n_edge > 0))

cat(sprintf("\n=== 校验完成：%d PASS / %d FAIL ===\n", N_PASS, N_FAIL))
if (N_FAIL > 0) quit(status = 1L)
