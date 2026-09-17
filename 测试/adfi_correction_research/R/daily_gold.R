######### 问题 B 轨：日级金表 + 统一输入格式 #########
#
# 金表 = 问题 B 的输入。它由三块拼成：
#   1. gold_truth  —— 纯净世界的逐日真值（skeleton.R::build_clean_world 产出）
#   2. gold_daily  —— 对 clean_dt 跑一次 Step 5 的产物（带 daily_weight_g、日级 flag、
#                     n_outlier_feed、has_feed_out_of_range_today）
#   3. 每头补齐的连续日历网格
#
# 三件必须知道的事：
#
# (1) **必须补日历网格。** 全包没有任何 seq.Date/CJ/complete，Step 5 只产出「有记录的
#     动物天」。而 .impute_feed_national_v2() 只遍历存在的行，且回归自变量是
#     x <- seq_len(nrow(sub))（zhenm_impute_national.R:54，**行序不是日历**）。
#     稀疏表下「删掉一整天」等于那天从序列里消失，Phase 4 的 LOESS 臂会整个空转。
#
# (2) **注入不能靠「删 visit 再跑 Step 5」。** 除了会把 6kg / ≤0 / out-of-range 三道
#     出口门重新施加到邻天上（分数无法归因），更要命的是 `zero` 模式在管线里不可达：
#     .finalize_daily_feed()（zhenm_daily_aggregate_filtered.R:287-289）把任何
#     daily_feed_g <= 0 改写成 NA。所以注入器作用在**日级金表**上。
#
# (3) **`gated` 不是 `corrupted`。** n_outlier_feed > 0 不代表该天值不可用——
#     日聚合是故意对幸存记录求和的（同文件 :171-177）。日级门只认
#     has_feed_out_of_range_today / flag_daily_feed_over_limit / flag_daily_feed_nonpositive。

# ============================================================
# 日历补齐：每头按 [min, max] 补出连续日
#
# 只补到该头自己的观测区间内——区间之外没有「应该有多少」的先验，
# 补出去只会人为放大分母。
# ============================================================
pad_daily_grid <- function(daily, id_col = "animal_id", date_col = "record_date") {
  stopifnot(id_col %in% names(daily), date_col %in% names(daily))
  dt <- data.table::copy(daily)
  data.table::setnames(dt, c(id_col, date_col), c("animal_id", "record_date"))
  dt[, record_date := data.table::as.IDate(record_date)]

  grid <- dt[, .(record_date = seq(min(record_date), max(record_date), by = "day")),
             by = animal_id]
  data.table::setkeyv(grid, c("animal_id", "record_date"))
  out <- merge(grid, dt, by = c("animal_id", "record_date"), all.x = TRUE)
  data.table::setorder(out, animal_id, record_date)
  out[]
}

# ============================================================
# 状态判定（**注入前**；注入后的改写由 inject_b.R 负责）
#
#   observed —— 有真值，且无日级门     → 可打分、可被注入
#   gated    —— 有真值，但日级门触发   → 可打分、不可被注入（当天值已被出口置 NA）
#   missing  —— 无真值（universe 之外）→ 不打分
#
# 注入后新增一个 `zero` 值；而「注入造成的 missing」与「本来就没有的 missing」
# 共用同一个 status 值，靠 `injected` 逻辑列区分——因为两者的**值语义完全一样**
# （都是 NA），分开命名只会造出一个没人会读的第五档。
# 打分只看 true_feed 是否存在（见 eval_b.R），不看 status，所以这个合并不影响正确性。
# ============================================================
GATE_COLS <- c("has_feed_out_of_range_today", "flag_daily_feed_over_limit",
               "flag_daily_feed_nonpositive")

assign_dfi_status <- function(gold) {
  gate_cols <- intersect(GATE_COLS, names(gold))
  gated <- if (length(gate_cols) == 0) {
    rep(FALSE, nrow(gold))
  } else {
    gold[, Reduce(`|`, lapply(.SD, function(x) x %in% TRUE)), .SDcols = gate_cols]
  }
  has_truth <- !is.na(gold$true_feed)

  status <- rep("missing", nrow(gold))
  status[has_truth & gated]  <- "gated"
  status[has_truth & !gated] <- "observed"
  gold[, DFI_status := status]
  gold[]
}

# ============================================================
# 建金表
#
# 返回：
#   gold    —— 补齐网格后的金表（含 DFI_status、injected、zero_injected）
#   truth   —— gold_truth（universe；animal_id / record_date / true_feed）
#   extra   —— 诊断：两边的一致性核对与 universe 之外的生产缺失
# ============================================================
build_gold <- function(device, project_root, ns_cfg = base_ns) {
  dev <- resolve_device(device, project_root)
  qc_dt <- run_steps1_4(dev$data_path, dev$device, dev$format_path, ns_cfg)
  cw <- build_clean_world(qc_dt)
  clean_dt <- cw$clean_dt
  truth_daily <- cw$truth_daily

  cfg <- ZhenM_merge_config(list(national_standard = ns_cfg))
  cat(">>> Step 5 跑 clean_dt（金表基座）...\n")
  gold_daily <- suppressMessages(
    ZhenM_standard_to_daily_filtered(data.table::copy(clean_dt), cfg))
  cat(">>> Step 5 跑全量 QC 表（用于统计 universe 之外的缺失）...\n")
  full_daily <- suppressMessages(
    ZhenM_standard_to_daily_filtered(data.table::copy(qc_dt), cfg))

  # 补齐网格并挂上真值
  gold <- pad_daily_grid(gold_daily)
  gold <- merge(gold, truth_daily[, .(animal_id, record_date, true_feed)],
                by = c("animal_id", "record_date"), all.x = TRUE)
  gold[, injected := FALSE]
  gold[, zero_injected := FALSE]
  gold <- assign_dfi_status(gold)
  data.table::setorder(gold, animal_id, record_date)

  # --- 一致性核对 1：金表观测日的值与真值必须逐位相等 ---
  # clean_dt 无任何 flag → feed_filtered = feed_g，Step 5 的 sum(feed_g>0) 就是真值。
  both <- gold[DFI_status == "observed" & !is.na(daily_feed_g)]
  max_abs_diff <- if (nrow(both) == 0) 0 else
    max(abs(both$daily_feed_g - both$true_feed))
  stopifnot(max_abs_diff == 0)

  # --- 诊断 2：universe 有多大、被门挡掉多少、之外有多少生产缺失 ---
  n_truth <- nrow(truth_daily)
  gated <- gold[DFI_status == "gated"]
  outside <- gold[is.na(true_feed)]
  full_ids <- unique(full_daily[, .(animal_id, record_date)])
  data.table::setkeyv(full_ids, c("animal_id", "record_date"))
  out_key <- unique(outside[, .(animal_id, record_date)])
  data.table::setkeyv(out_key, c("animal_id", "record_date"))
  # universe 之外再分两类：满表里有行（记录全被 flag → 被出口挡掉）vs 压根没行（无访问）
  n_out_rownum <- nrow(merge(out_key, full_ids, by = c("animal_id", "record_date")))

  extra <- list(
    n_universe       = n_truth,
    n_gold_rows      = nrow(gold),
    n_gated          = nrow(gated),
    n_outside        = nrow(outside),
    n_outside_fullrow = n_out_rownum,
    n_outside_novisit = nrow(outside) - n_out_rownum,
    max_abs_diff     = max_abs_diff
  )
  cat(sprintf(paste0(">>> 金表：网格 %d 行 = universe %d（observed %d / gated %d）",
                     " + universe 之外 %d\n",
                     "    之外细分：满表有行 %d（记录全被 flag）、无访问 %d\n"),
              extra$n_gold_rows, extra$n_universe,
              extra$n_universe - extra$n_gated, extra$n_gated,
              extra$n_outside, extra$n_outside_fullrow, extra$n_outside_novisit))

  list(gold = gold, truth = truth_daily[, .(animal_id, record_date, true_feed)],
       extra = extra, qc_dt = qc_dt, clean_dt = clean_dt, full_daily = full_daily)
}

# ============================================================
# 把金表切成「算法看到的输入」
#
# 两条铁律（计划 §五）：
#   · 问题 B 的算法只能看 observed / missing 的模式 + 日期/体重/阶段；
#   · **不得读取 corrupted 行的 ETP/OTD/FID**（那是问题 A 的信息，读了就是偷看答案）。
#
# 输入形态：**始终是补齐后的日历网格**，孔洞用「值 + DFI_status」表达。
# 之所以不真删行：LOESS / GAM / Kalman 都是规则索引上的序列方法，而生产插补器
# .impute_feed_national_v2() 更把 x 取成 seq_len(nrow(sub))（行序不是日历）——
# 稀疏输入会让它把不规则的缺口当成等间距，缺口一多整个模型就跑偏。
# 需要「生产那样真的没有行」的场景，自己过一遍 `dt[!is.na(daily_feed_g)]` 即可。
#
# 「缺失」与「置零」必须可区分（计划 §四-2）：前者值为 NA，后者值为 0。
# 所以这里**保留 0**，让两种模式在输入上真的不同——各臂按契约决定怎么处理
# （能建模零膨胀 / 删失的臂可以用它，见 Phase 5-6）。
#
# 但生产插补器把 0 当作**有效观测**（zhenm_impute_national.R:31-32）：直接把 0 喂给
# .impute_feed_national_v2() 会让它当正常值参与拟合。不能处理 0 的臂必须先过
# as_na_view()。这一条是 Phase 4 接 LOESS 臂时的硬要求。
# ============================================================
ARM_VIEW_COLS <- c("animal_id", "record_date", "daily_feed_g", "daily_weight_g",
                   "DFI_status", "injected", "zero_injected", "visits_n")

gold_to_arm_input <- function(gold) {
  out <- data.table::copy(gold)
  # gated 天本来在 Step 5 出口就被置 NA 了，这里显式确认（防上游改动后静默变化）
  out[DFI_status == "gated", daily_feed_g := NA_real_]
  out[, ..ARM_VIEW_COLS]
}

# ============================================================
# 打分范围（计划 §五：gated / 无访问 **只报告不打分**）
#
# B 轨的打分 universe = 注入前的 observed 天，正好等于注入器可采样的那批天。
#
# 为什么必须排除 gated：那几天（FIRE 3 天）真值存在，但生产 Step 5 的出口门把值
# 置成了 NA——连管线自己都产不出值。把它们计进分母，等于给**每个**臂挂一个恒定的
# ≈0.04% 亏损，`未注入 → acc == 1` 这条干净契约就断了，注入量也无法从 acc 里分离。
# 排除它们不影响任何横向比较，只是把「管线自身放弃的天」移出计分板。
# ============================================================
gold_universe <- function(gold) {
  gold[DFI_status == "observed", .(animal_id, record_date)]
}

# 给「不认 0」的臂用的视图：zero / gated / missing 一律变 NA。
# 判决只认 DFI_status，不看值本身——避免依赖「0 一定是注入的」这种脆弱假设。
as_na_view <- function(arm_input) {
  out <- data.table::copy(arm_input)
  out[DFI_status %in% c("zero", "gated", "missing"), daily_feed_g := NA_real_]
  out[]
}
