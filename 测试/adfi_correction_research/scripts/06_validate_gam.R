######### Phase 5（上）：GAM 臂的契约门 + 能力门 + 失效模式表 ########
#
# 运行：Rscript 测试/adfi_correction_research/scripts/06_validate_gam.R [FIRE|NEDAP]
#
# Phase 4 有一个「逐位复现生产」的等价性门，因为 LOESS 有个生产实现要对齐。
# **GAM 没有可对齐的对象**（生产里没有 GAM），所以这里换成五条**性质**断言。
# 其中 G3 是本阶段的验收条件——它把「GAM 能外推、LOESS 不能」从一句口头结论
# 变成一条会失败的测试：
#
#   G1 无害    无缺口的输入逐位不变（含「有缺口的输入里、无缺口的那批头不动」）
#   G2 确定性  同一输入两遍逐位相同（mgcv 无随机性；断言它，Phase 6/7 才敢依赖）
#   G3 能力    boundary 上 n_self_extrap > 0 且覆盖多数缺口；LOESS 对照恒为 0
#   G4 自洽    n_gam + n_fallback == n_gap（中位数兜底不留残 NA）
#   G5 路由    router 的 <=3 支逐位等于 pure、>3 支逐位等于 prod
#
# G5 不是计划里写的，是写 router 时**顺手拿到的一条免费性质**：生产只写
# missing_idx 位置（zhenm_impute_national.R:66-67 / 86 / 96-97），所以在把 <=3 那批
# 头的缺口位重置回 NA 之后，它们与 pure 的输入逐位相同，两条路径必然同值。
# 这条断言一石二鸟：既证明「router 真的只换了 <=3 支的 LOESS」，又证明
# 「router 的 >3 支零新代码、不可能与生产漂移」。**但它很容易空转**——
# FIRE 跑 5d pattern 时 >3 支是 211 头、<=3 支是 0 头，断言会在退化的输入上
# 全绿。所以下面显式断言两边都非空，**空转即 FAIL**。
#
# 【表】5 pattern × rate 10%，5 条臂，每个格子把 affected-only accuracy **拆成
#   「内部缺口 / 向外外推」两半**。这是本表最重要的列，理由见下。
#
# 为什么必须拆：Phase 4 报的 `boundary` accuracy 0.1881 看着像个「还行」的数，
# 实际上它 100% 是**向外**天（2079/2079），而那 0.1881 是生产的中位数兜底说出来的，
# LOESS 一个字没说。同一份实测里 `random` 的向外子集是 **−0.3222**——比填 0 还差，
# 因为兜底用的是整头中位数而序列开头采食量本就低，系统性高估。
# 混成一个数会被 98% 的内部天平均掉（random 混起来是 0.7529，看着挺好）。
# ⇒ **报表里绝不给 accuracy 加 pmax(0,·) 之类的护栏，负数是真信号。**

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
source(file.path(module_dir, "R", "arm_gam.R"))

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
UNIV <- gold_universe(gold)
cfg <- make_base_config(base_ns)
cat(sprintf(">>> 打分 universe = observed %d 天\n", nrow(UNIV)))

# ============================================================
# 工具
# ============================================================

# 逐头的缺口结构。`n_outward` = 落在 [首个有效天, 末个有效天] 之外的缺口天数。
gap_struct <- function(dt) {
  dt[order(animal_id, record_date)][, {
    m <- is.na(daily_feed_g)
    r <- rle(m)
    first_v <- if (any(!m)) which(!m)[1] else NA_integer_
    last_v  <- if (any(!m)) which(!m)[sum(!m)] else NA_integer_
    list(n_valid = sum(!m),
         max_run = if (any(m)) max(r$lengths[r$values]) else 0L,
         n_gap = sum(m),
         n_outward = if (is.na(first_v)) 0L else
           sum(m & (seq_along(m) < first_v | seq_along(m) > last_v)))
  }, by = animal_id][]
}

# 把 affected 键按「向外 / 内部」切成两半。判据只读**该臂自己的输入**的缺口结构。
#
# 陷阱 1：`boundary` 的内部子集**是空的**（FIRE 实测 2079/2079 全向外）。
#   eval_metrics_b 在 0 行 affected 上返回 by_type = NULL，直接 `$acc[1]` 会静默
#   读成 NA——所以这里显式返回 0 行，由调用方记 NA 并**跳过**那次调用。
# 陷阱 2：accuracy 可以为负。不要加护栏。
split_affected <- function(inp, affected) {
  side <- inp[order(animal_id, record_date)][, {
    m <- is.na(daily_feed_g); v <- which(!m)
    out <- if (length(v) == 0L) m else
      (seq_along(m) < v[1] | seq_along(m) > v[length(v)])
    list(record_date = record_date, outward = out)
  }, by = animal_id]
  a <- affected[order(animal_id, record_date)]
  j <- merge(a, side, by = c("animal_id", "record_date"), all.x = TRUE, sort = FALSE)
  list(all      = a,
       interior = j[outward == FALSE][, !"outward"],
       outward  = j[outward == TRUE][, !"outward"])
}

# affected 子集上的 accuracy。空子集 ⇒ NA（不是 0，也不是 NULL 被吃掉）。
acc_of <- function(est, affected_sub, univ, truth_all) {
  if (nrow(affected_sub) == 0L) return(NA_real_)
  m <- eval_metrics_b(est, truth_all, universe = univ, affected = affected_sub)
  if (is.null(m$by_type) || nrow(m$by_type) == 0L) NA_real_ else m$by_type$acc[1]
}

# 全 NA 的列说明该臂不暴露这个量（如 prod 不区分 loess 填与兜底填），
# na.rm = TRUE 会把它静默读成 0，正是要避免的那种「编一个数出来」
sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# arm_param 缺字段时给 NA 而不是 NULL。Phase 4 踩过 `data.table(span = NULL)`
# 静默丢列的坑（列没了，表照出，值对不上）。
pget <- function(p, nm) if (is.null(p[[nm]])) NA else p[[nm]]

ARMS <- list(
  gam_pure      = arm_gam_pure(6, "cr", "gaussian", "median"),
  gam_router    = arm_gam_router(cfg, 6, "cr", "gaussian", "median"),
  gam_pop       = arm_gam_pop(6, "cr", "gaussian", "median"),
  loess_prod_pt = arm_loess_pure(0.3, 2, "median"),
  nofill        = arm_nofill())

RATE <- 0.10

# ============================================================
# G1 —— 无害
# ============================================================
cat("\n===== G1：无缺口输入逐位不变 =====\n")
clean <- gold_to_arm_input(gold)[!is.na(daily_feed_g)]
cat(sprintf("  零缺口输入 %d 行 / %d 头\n", nrow(clean), length(unique(clean$animal_id))))
for (nm in c("gam_pure", "gam_router", "gam_pop", "nofill")) {
  out <- run_arm(ARMS[[nm]], clean)
  check(sprintf("G1 %-9s 零缺口输入 daily_feed_g 逐位不变（%d 行）", nm, nrow(clean)),
        identical(out$daily_feed_g, clean$daily_feed_g))
  check(sprintf("G1 %-9s 零缺口输入 diag 为空（无缺口不入 diag）", nm),
        nrow(arm_diag(out)) == 0L)
}

# 更强的一版：**非缺口天一个都不许动**。
#
# 最初这里写的是「无缺口的那批头必须不变」，实测直接被证伪成一个**空转断言**：
# FIRE 上 boundary 注入命中全部 211 头，`n_gap == 0` 的头是 0 个，断言在空集上
# 全绿/全红都没有意义。改成逐天的形式后有 20793 天参与，**每个 pattern 都非空**，
# 而且它抓的正是真正危险的那类 bug——平滑器把值写回了非缺口位。
non_gap_unchanged <- function(out, inp) {
  a <- out[order(animal_id, record_date)]
  b <- inp[order(animal_id, record_date)]
  v <- !is.na(b$daily_feed_g)          # 输入里**不是**缺口的天
  list(ok = identical(a$daily_feed_g[v], b$daily_feed_g[v]), n = sum(v))
}

res_g1 <- suppressWarnings(inject_missing_days(gold, "boundary", "missing", RATE))
inp_g1 <- res_g1$dt_injected
st_g1 <- gap_struct(inp_g1)
cat(sprintf("  boundary 输入：%d 头 / %d 天，其中缺口 %d 天、非缺口 %d 天\n",
            nrow(st_g1), nrow(inp_g1), sum(st_g1$n_gap), nrow(inp_g1) - sum(st_g1$n_gap)))
for (nm in c("gam_pure", "gam_router", "gam_pop", "nofill")) {
  out <- run_arm(ARMS[[nm]], inp_g1)
  u <- non_gap_unchanged(out, inp_g1)
  check(sprintf("G1 %-9s 非缺口 %d 天逐位不变（回写越界会在这里露馅）", nm, u$n),
        u$ok && u$n > 0L)
}

# ============================================================
# G2 —— 确定性
# ============================================================
cat("\n===== G2：确定性（同输入两遍逐位相同）=====\n")
for (nm in c("gam_pure", "gam_router", "gam_pop", "nofill")) {
  o1 <- run_arm(ARMS[[nm]], inp_g1)
  o2 <- run_arm(ARMS[[nm]], inp_g1)
  check(sprintf("G2 %-9s 两遍 daily_feed_g 逐位相同", nm),
        identical(o1$daily_feed_g, o2$daily_feed_g))
  check(sprintf("G2 %-9s 两遍 diag 逐位相同", nm),
        identical(arm_diag(o1), arm_diag(o2)))
}

# ============================================================
# G3 —— 能力（本阶段的验收条件）
# ============================================================
cat("\n===== G3：GAM 在 boundary 上真的外推了（LOESS 恒为 0）=====\n")
out_g3 <- run_arm(ARMS[["gam_pure"]], inp_g1)
dg_g3  <- arm_diag(out_g3)
n_gap3 <- sum(dg_g3$n_gap); n_ext3 <- sum(dg_g3$n_self_extrap)
n_fb3  <- sum_or_na(dg_g3$n_fallback)

cat(sprintf("  gam_pure(boundary)：缺口 %d 天，其中向外 %d 天（%.1f%%），兜底 %s 天\n",
            n_gap3, n_ext3, 100 * n_ext3 / n_gap3, n_fb3))
check("G3 gam_pure 在 boundary 上 n_self_extrap > 0（LOESS 在此恒为 0）", n_ext3 > 0L)
check(sprintf("G3 n_self_extrap 覆盖多数缺口（%.1f%% >= 50%%）",
              100 * n_ext3 / n_gap3), n_ext3 / n_gap3 >= 0.5)

# LOESS 对照：同一语料上，兜底填的天数恰等于向外天数 ⇒ 它在边界上全靠兜底
out_l3 <- run_arm(ARMS[["loess_prod_pt"]], inp_g1)
dg_l3  <- arm_diag(out_l3)
n_out3 <- sum(gap_struct(inp_g1)$n_outward)
cat(sprintf("  loess_pure(0.3,2)：缺口 %d 天，向外 %d 天，兜底 %s 天\n",
            n_gap3, n_out3, sum_or_na(dg_l3$n_fallback)))
check(sprintf("G3 对照：LOESS 的 n_fallback 恰等于向外天数（%d）——它一天都没外推",
              n_out3),
      sum_or_na(dg_l3$n_fallback) == n_out3)

# ============================================================
# G4 —— 自洽
# ============================================================
cat("\n===== G4：兜底不留残 NA / 逐分支账目自洽 =====\n")
# 断言 ①（arm 无关，最本质）：fallback = "median" 的臂在**缺口位上不留任何 NA**。
#   `n_gam + n_fallback == n_gap` 这条**不能**直接对全部头断言：router 的
#   `fcr-extrap` 头（>3 天，FIRE boundary 上 14 头）由**生产**填，GAM 一天没填，
#   所以 0 + 0 != n_gap 是**正确行为**而不是缺陷。这条被实测逮住过，别再写回去。
#   逐分支的账目放到断言 ②。
no_residual_na <- function(out, inp) {
  a <- out[order(animal_id, record_date)]
  b <- inp[order(animal_id, record_date)]
  m <- is.na(b$daily_feed_g)
  list(ok = !any(is.na(a$daily_feed_g[m])), n = sum(m))
}
for (nm in c("gam_pure", "gam_router", "gam_pop")) {
  out <- run_arm(ARMS[[nm]], inp_g1)
  r <- no_residual_na(out, inp_g1)
  check(sprintf("G4 %-9s 缺口 %d 天无残留 NA（中位数兜底兜住了）", nm, r$n),
        r$ok && r$n > 0L)
}
# 断言 ②：在**真的由 GAM 经手**的那批头上，逐分支账目闭合。
#
# 语料**必须**换成 2d/random：boundary 的注入集中在序列首尾 ⇒ run 天然长 ⇒
# router 在那批语料上 211 头**全部**走 `fcr-extrap`，`branch == "gam"` 的头是 0 个，
# 断言会空转（实测逮到过）。用 2d 才有 197 头走 GAM。
res_g4 <- suppressWarnings(inject_missing_days(gold, "2d", "missing", RATE))
inp_g4 <- res_g4$dt_injected
for (nm in c("gam_pure", "gam_router", "gam_pop")) {
  dg <- arm_diag(run_arm(ARMS[[nm]], inp_g4))
  f <- dg[branch == "gam"]
  check(sprintf("G4 %-9s (2d) branch==gam 的 %d 头：n_gam + n_fallback == n_gap",
                nm, nrow(f)),
        nrow(f) > 0L && all(f$n_gam + f$n_fallback == f$n_gap))
  # 被排除在外的分支照实报，别让它们悄悄消失
  ex <- dg[branch != "gam", .N, by = branch]
  if (nrow(ex) > 0) {
    cat(sprintf("        （%-9s 另有：%s，不由 GAM 填，故不参与上面的账目）\n", nm,
                paste(sprintf("%s=%d头", ex$branch, ex$N), collapse = " ")))
  }
}
# n_negative_pred **不断言为 0**：它非零是有信息的发现（FIRE boundary 实测 10 天），
# 不是要抹掉的瑕疵——§17.5 明令不加负值截断。
cat(sprintf("  注：n_negative_pred 照实报、不截断（gam_pure boundary 实测 %d 天）\n",
            sum_or_na(dg_g3$n_negative_pred)))

# ============================================================
# G5 —— 路由（本阶段免费拿到的强性质）
# ============================================================
cat("\n===== G5：router 的 <=3 支 ≡ pure、>3 支 ≡ prod =====\n")
# 空转判据放在**整个循环之后**，不是每个 pattern 各自要求两侧非空。
# 理由：NEDAP 只有 42 头，`2d` 上 >3 支恰为 0 头——那是**语料规模**的属性，不是
# 断言该失败的地方（实测 FIRE 全过、NEDAP 在 2d 上假失败）。要防的是「整段脚本
# 一次都没验到某一侧」，所以累计两侧的观测数，最后一次性断言。
seen_le3 <- 0L; seen_gt3 <- 0L
for (pt in c("2d", "random")) {
  res <- suppressWarnings(inject_missing_days(gold, pt, "missing", RATE))
  inp <- res$dt_injected; st <- gap_struct(inp)
  le3 <- st[n_valid >= 10 & max_run <= 3L, animal_id]
  gt3 <- setdiff(st$animal_id, le3)
  seen_le3 <- seen_le3 + length(le3); seen_gt3 <- seen_gt3 + length(gt3)
  cat(sprintf("  %-7s <=3 支 %d 头 / >3 支 %d 头\n", pt, length(le3), length(gt3)))

  pure <- run_arm(ARMS[["gam_pure"]], inp)
  rout <- run_arm(ARMS[["gam_router"]], inp)
  prod_out <- run_arm(arm_loess_prod(cfg), inp)
  sl <- function(d, ids) d[animal_id %in% ids][order(animal_id, record_date)]

  if (length(le3) > 0L) {
    a <- sl(pure, le3); b <- sl(rout, le3)
    check(sprintf("G5a %-7s <=3 支(%d 头)：router ≡ pure（daily_feed_g）", pt, length(le3)),
          identical(a$daily_feed_g, b$daily_feed_g))
    check(sprintf("G5a %-7s <=3 支：router ≡ pure（is_imputed_feed）", pt),
          identical(a$is_imputed_feed, b$is_imputed_feed))
  } else {
    cat(sprintf("        （%s 无 <=3 支的头，本 pattern 跳过 G5a）\n", pt))
  }
  if (length(gt3) > 0L) {
    c1 <- sl(rout, gt3); c2 <- sl(prod_out, gt3)
    check(sprintf("G5b %-7s >3 支(%d 头)：router ≡ prod（零新 FCR 代码）", pt, length(gt3)),
          identical(c1$daily_feed_g, c2$daily_feed_g))
  } else {
    cat(sprintf("        （%s 无 >3 支的头，本 pattern 跳过 G5b）\n", pt))
  }
  dgr <- arm_diag(rout)
  cat(sprintf("        router 分支：%s\n",
              paste(sprintf("%s=%d", names(table(dgr$branch)), table(dgr$branch)),
                    collapse = " ")))
}
# 这一条才是防空转的：两侧**都**得至少被验到一次
check(sprintf("G5 两侧在整个脚本里都被验到（<=3 累计 %d 头 / >3 累计 %d 头）",
              seen_le3, seen_gt3),
      seen_le3 > 0L && seen_gt3 > 0L)

# ============================================================
# 【表】失效模式：acc 拆成内部 / 向外两半
# ============================================================
cat("\n===== 【表】失效模式（rate 10%）：acc_affected 拆内部 / 向外 =====\n")
rows <- list()
for (pt in PATTERNS) {
  res <- suppressWarnings(inject_missing_days(gold, pt, "missing", RATE))
  inp <- res$dt_injected; st <- gap_struct(inp)
  sp  <- split_affected(inp, res$affected)
  n_out_pt <- sum(st$n_outward)

  for (nm in names(ARMS)) {
    arm <- ARMS[[nm]]
    p <- arm_param(arm)
    message(sprintf("      %s / %s", pt, nm))
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
      rate_realised = res$rate_realised,
      acc = met$acc, bias = met$bias, coverage = met$coverage, adfi_r = met$r,
      # 全部受影响天
      acc_affected = acc_of(out, sp$all, UNIV, truth),
      # 拆开的两半——本表的主列
      n_interior = nrow(sp$interior),
      n_outward_days = nrow(sp$outward),
      acc_interior = acc_of(out, sp$interior, UNIV, truth),
      acc_outward  = acc_of(out, sp$outward, UNIV, truth),
      n_na_days = met$n_na_days,
      n_gap = sum(dg$n_gap),
      n_smooth = if (is_gam) sum(dg$n_gam) else sum_or_na(dg$n_loess),
      n_fallback = sum_or_na(dg$n_fallback),
      n_self_extrap = sum(dg$n_self_extrap),
      n_negative_pred = sum_or_na(dg$n_negative_pred),
      # §八 要的那张「共同尺度」表的两栏：GAM 报 edf，LOESS 报有效窗宽
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

  # prod 实际走了哪条分支（只读输入侧结构复算，不猜值）。
  # 注意要读 `inp` 的 daily_feed_g，不是读 `st`——`st` 是 gap_struct() 的**汇总量**，
  # 里面根本没有 daily_feed_g 列（写成 `st[, .(.prod_branch(is.na(daily_feed_g)))]`
  # 会报 `object 'daily_feed_g' not found`，实测踩过）。
  pr <- inp[order(animal_id, record_date)][,
    .(branch = .prod_branch(is.na(daily_feed_g))), by = animal_id][,
    .(n = .N), by = branch][order(-n)]
  cat(sprintf("\n  %-11s 注入 %d 天；prod 分支：%s\n", pt, res$n_realised,
              paste(sprintf("%s=%d头", pr$branch, pr$n), collapse = " ")))
}

tab <- data.table::rbindlist(rows)
data.table::setorder(tab, pattern, arm)

cat("\n--- 主表：affected-only accuracy 拆成内部 / 向外（向外是 GAM 的价值所在）---\n")
print(tab[, .(pattern, arm,
              acc_aff = round(acc_affected, 4),
              n_int = n_interior, acc_int = round(acc_interior, 4),
              n_out = n_outward_days, acc_out = round(acc_outward, 4))])

cat("\n--- 机制列：填了多少天、有多少天是外推、兜底了多少天 ---\n")
cat("    （主表的 n_out 与这儿的 extrap **不是同一个量、也不该相等**：\n")
cat("      n_out 只数**被注入**的天，extrap 数**全部**落在区间外的缺口天，\n")
cat("      含金表里本来就无访问的天。random 上 FIRE 实测 48 vs 52，差的 4 天就是后者。）\n")
print(tab[, .(pattern, arm, gap = n_gap, smooth = n_smooth, fallback = n_fallback,
              extrap = n_self_extrap, neg = n_negative_pred,
              edf = round(mean_edf, 2), span_pts = round(mean_span_pts, 1),
              rt = round(runtime_s, 1))])

cat("\n--- 两设备都适用的两条读数 ---\n")
gp <- tab[arm == "gam_pure"]
cat(sprintf("  ① gam_pure 在 boundary 上 acc_outward = %.4f，loess 生产点 = %.4f；\n",
            gp[pattern == "boundary", acc_outward],
            tab[arm == "loess_prod_pt" & pattern == "boundary", acc_outward]))
cat(sprintf("     gam_pure 的 n_fallback 在 boundary 上 = %d（LOESS 是 %d，即全部向外天）\n",
            gp[pattern == "boundary", n_fallback],
            tab[arm == "loess_prod_pt" & pattern == "boundary", n_fallback]))
cat("  ② random 的向外子集是负数吗：",
    sprintf("gam_pure %s / loess %s / nofill %s\n",
            round(gp[pattern == "random", acc_outward], 4),
            round(tab[arm == "loess_prod_pt" & pattern == "random", acc_outward], 4),
            round(tab[arm == "nofill" & pattern == "random", acc_outward], 4)))
cat("     （负数是真信号：说明「兜底比填 0 还差」，不是要截断的瑕疵）\n")

# nofill 的 floor：affected-only accuracy 恒为 0，且与 Phase 4 的 raw_zero 同值
check("nofill 的 acc_affected 恒为 0（NA 被 fcoalesce 当 0 计，与 raw_zero 同值）",
      all(abs(tab[arm == "nofill", acc_affected]) < 1e-12))

run_dir <- file.path(module_dir, "results", "runs")
if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)
out_csv <- file.path(run_dir, sprintf("gam_failure_modes_%s.csv", tolower(DEVICE)))
data.table::fwrite(tab, out_csv)
cat(sprintf("\n>>> 明细已写入 %s（gitignored）\n", out_csv))

cat(sprintf("\n=== 门与表完成：%d PASS / %d FAIL ===\n", N_PASS, N_FAIL))
if (N_FAIL > 0) quit(status = 1L)
