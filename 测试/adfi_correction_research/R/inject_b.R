######### 问题 B 轨：整天缺失注入器 #########
#
# 与 A 轨的 inject_errors() 的分工：
#   inject_errors()  改 feed_g（record-level）—— 问题 A：当天有记录但值不对
#   inject_missing_days()  抹掉整天（day-level）—— 问题 B：整天没有数据
# 两者都作用在**已知真值**之上，都产出一张 affected 明细表供分层打分。
#
# 注入只作用在 `DFI_status == "observed"` 的天上——有真值、且没被日级门挡掉。
# `gated` 天（真值 > 6kg 之类）本来就已经是 NA，注入它没有意义也无法归因。
#
# 口径：rate 是「占该设备 observed 天的比例」，不是占日历天。
# 这样 random 模式下的期望删除比例就是 rate，与 A 轨的语义可比。

PATTERNS    <- c("random", "2d", "5d", "14d", "boundary", "error-assoc")
RUN_LEN     <- c(`2d` = 2L, `5d` = 5L, `14d` = 14L)
PATTERN_IDX <- stats::setNames(seq_along(PATTERNS), PATTERNS)

# 种子：原脚本的 SET_SEED + round(rate*1000) 在不同 pattern 间会撞（A 轨只有一个注入器，
# 没暴露这个问题）。这里给 pattern 加一个独立的倍数偏移。
inject_seed <- function(rate, pattern, base = SET_SEED) {
  base + round(rate * 1000) + PATTERN_IDX[[pattern]] * 1e5
}

# 最大余数法：把 total 按 counts 的比例分配成整数配额，总和尽量 = total。
# 直接 round() 的话，211 头 × 5% 的舍入误差可达 ±100 天（≈10%），足以让 rate 失真。
#
# cap：每头上限（缺省 = counts）。run 模式下容量是「能放几个 run」而不是「有多少天」，
# 两者不同，所以必须能分开传；余数逐个发时遇到满额的头就跳过，避免把配额浪费在
# 已经装不下的头上（原先直接 pmin 会让总和悄悄低于 total）。
.apportion <- function(counts, total, cap = counts) {
  if (length(counts) == 0 || total <= 0) return(integer(length(counts)))
  quota <- counts / sum(counts) * total
  out <- floor(quota)
  rem <- total - sum(out)
  if (rem > 0) {
    for (k in order(quota - out, decreasing = TRUE)) {
      if (rem <= 0) break
      if (out[k] < cap[k]) { out[k] <- out[k] + 1L; rem <- rem - 1L }
    }
  }
  # 不能删掉比该头容量更多的天
  pmin(out, cap)
}

# 每头的 observed 天预算
.animal_budget <- function(cand, rate) {
  per <- cand[, .(n_obs = .N), by = animal_id]
  per[, quota := .apportion(n_obs, floor(sum(n_obs) * rate))]
  per[]
}

# 连续 run：候选起点 = 该头某天起连续 L 个日历日都能用
.run_starts <- function(cand, L) {
  cand[order(animal_id, record_date)][, {
    dn <- as.integer(record_date)
    n <- length(dn)
    ok <- logical(n)
    if (n >= L) {
      for (i in seq_len(n - L + 1L)) {
        ok[i] <- all(dn[i:(i + L - 1L)] == dn[i] + (0:(L - 1L)))
      }
    }
    list(record_date = record_date[ok])
  }, by = animal_id]
}

# 从候选起点里贪心挑互不重叠的 run，选满 quota 天为止
.pick_runs <- function(starts, quota, L) {
  if (quota <= 0 || nrow(starts) == 0) return(starts[0])
  s <- data.table::copy(starts)[order(animal_id, record_date)]
  s[, dnum := as.integer(record_date)]
  s <- s[sample.int(.N)]                 # 打乱后贪心，保证确定性（set.seed 已在外层设好）
  taken <- vector("list", 0)
  used <- data.table::data.table(animal_id = character(), dnum = integer())
  n_days <- 0L
  for (i in seq_len(nrow(s))) {
    if (n_days + L > quota) break
    a <- s$animal_id[i]; d <- s$dnum[i]
    clash <- used[animal_id == a & abs(dnum - d) < L]
    if (nrow(clash) > 0) next
    taken[[length(taken) + 1L]] <- data.table::data.table(
      animal_id = a, dnum = d + (0:(L - 1L)))
    used <- rbind(used, data.table::data.table(animal_id = a, dnum = d))
    n_days <- n_days + L
  }
  if (length(taken) == 0) return(starts[0])
  out <- data.table::rbindlist(taken)
  out[, record_date := data.table::as.IDate(dnum, origin = "1970-01-01")]
  out[, .(animal_id, record_date)]
}

# ============================================================
# 主入口
#
# @param gold  build_gold() 产出的金表
# @param pattern  random / 2d / 5d / 14d / boundary / error-assoc
# @param mode     missing（注入后值为 NA）/ zero（注入后值为 0）
# @param rate     占该设备 observed 天的比例
# @param seed     缺省由 inject_seed(rate, pattern) 决定
#
# @return list：
#   gold_marked  —— 金表 + injected / zero_injected / DFI_status / daily_feed_g 已改写
#   dt_injected  —— 交给臂的输入（gold_to_arm_input 视图）
#   affected     —— 注入明细，契约同 A 轨：(animal_id, record_date, inj_types,
#                   true_injected_sum, n_injected_rec)
#   n_target / n_realised / rate_realised / seed
# ============================================================
inject_missing_days <- function(gold, pattern, mode = c("missing", "zero"),
                                rate, seed = NULL) {
  pattern <- match.arg(pattern, PATTERNS)
  mode <- match.arg(mode)
  if (is.null(seed)) seed <- inject_seed(rate, pattern)

  stopifnot("DFI_status" %in% names(gold), "true_feed" %in% names(gold))
  cand <- gold[DFI_status == "observed", .(animal_id, record_date, n_outlier_feed,
                                           true_feed)]
  n_obs <- nrow(cand)
  n_target <- floor(n_obs * rate)

  set.seed(seed)
  picked <- switch(
    pattern,
    "random" = {
      idx <- sample.int(n_obs, min(n_target, n_obs))
      cand[idx, .(animal_id, record_date)]
    },
    "boundary" = {
      per <- .animal_budget(cand, rate)
      cand[order(animal_id, record_date)][, {
        k <- per[animal_id == .BY$animal_id, quota][1]
        if (is.na(k) || k <= 0) .SD[0] else {
          # 两端各取一半：序列头尾缺失是外推最吃紧的场景
          k1 <- ceiling(k / 2); k2 <- k - k1
          n <- .N
          idx <- unique(c(seq_len(min(k1, n)),
                          seq(max(1L, n - k2 + 1L), n)))
          .SD[idx[!is.na(idx)]]
        }
      }, by = animal_id][, .(animal_id, record_date)]
    },
    "error-assoc" = {
      # 抽样权重取自**真实**误差强度：现实中那些大量记录被 flag 的天，
      # 才是「设备/传感器出问题」最可能整天丢数的天。真值仍来自 gold_truth。
      per <- .animal_budget(cand, rate)
      cand[, w := data.table::fcoalesce(as.numeric(n_outlier_feed), 0) + 1]
      cand[, {
        k <- per[animal_id == .BY$animal_id, quota][1]
        if (is.na(k) || k <= 0) .SD[0] else .SD[sample.int(.N, min(k, .N), prob = w)]
      }, by = animal_id][, .(animal_id, record_date)]
    },
    {
      L <- RUN_LEN[[pattern]]
      # 配额必须按 **run 的个数** 分配，不能按天。
      # 反例（14d @ 10%）：每头约 98 个 observed 天 → 按天的配额约 10，比一个 run 的
      # 14 天还短，.pick_runs 的 `n_days + L > quota` 第一步就 break，注入 0 天。
      # 按 run 数分配则天然兼容：名额 = floor(目标天 / L)，再按各头可容纳的 run 数摊。
      starts <- .run_starts(cand, L)
      n_run_total <- floor(n_target / L)
      capacity <- cand[, .(n_obs = .N), by = animal_id]
      cap_runs <- if (nrow(starts) == 0) {
        capacity[, .(animal_id, capacity = 0L)]
      } else {
        merge(capacity, starts[, .(n_start = .N), by = animal_id],
              by = "animal_id", all.x = TRUE)[
                , .(animal_id, capacity = pmin(data.table::fcoalesce(n_start, 0L),
                                               n_obs %/% L))]
      }
      capacity <- merge(capacity, cap_runs, by = "animal_id", all.x = TRUE)
      capacity[is.na(capacity), capacity := 0L]
      # 权重取各头「最多能放几个 run」——observed 天越多，能承受的 run 越多
      capacity[, nruns := .apportion(n_obs %/% L, n_run_total, cap = capacity)]
      res <- lapply(which(capacity$nruns > 0L), function(i) {
        st <- starts[animal_id == capacity$animal_id[i]]
        .pick_runs(st, capacity$nruns[i] * L, L)
      })
      if (length(res) == 0) {
        cand[0, .(animal_id, record_date)]
      } else {
        data.table::rbindlist(res)
      }
    }
  )

  # --- 落到金表上 ---
  gold_marked <- data.table::copy(gold)
  gold_marked[is.na(injected), injected := FALSE]
  gold_marked[is.na(zero_injected), zero_injected := FALSE]
  data.table::setkeyv(gold_marked, c("animal_id", "record_date"))
  picked <- unique(picked[, .(animal_id, record_date)])
  data.table::setkeyv(picked, c("animal_id", "record_date"))

  gold_marked[picked, on = .(animal_id, record_date),
              `:=`(injected = TRUE, zero_injected = (mode == "zero"))]
  gold_marked[picked, on = .(animal_id, record_date),
              DFI_status := if (mode == "zero") "zero" else "missing"]
  gold_marked[picked, on = .(animal_id, record_date),
              daily_feed_g := if (mode == "zero") 0 else NA_real_]

  # --- 明细表（契约与 A 轨一致）---
  affected <- merge(unique(picked[, .(animal_id, record_date)]),
                    gold[, .(animal_id, record_date, true_feed, visits_n)],
                    by = c("animal_id", "record_date"))
  affected[, `:=`(inj_types = pattern,
                  true_injected_sum = true_feed,
                  n_injected_rec = data.table::fcoalesce(visits_n, 0L))]
  affected[, c("true_feed", "visits_n") := NULL]
  data.table::setorder(affected, animal_id, record_date)

  n_realised <- nrow(affected)
  if (n_realised != n_target && pattern %in% c("random", "2d", "5d", "14d")) {
    warning(sprintf("pattern=%s: 目标 %d 天、实际 %d 天（连续 run 放置受可用空间限制）",
                    pattern, n_target, n_realised), call. = FALSE)
  }

  list(gold_marked = gold_marked,
       dt_injected = gold_to_arm_input(gold_marked),
       affected = affected,
       n_target = n_target, n_realised = n_realised,
       rate_realised = n_realised / n_obs, seed = seed)
}
