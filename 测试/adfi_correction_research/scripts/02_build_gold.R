######### Phase 3（上）：建日级金表并校验网格 ########
#
# 运行：Rscript 测试/adfi_correction_research/scripts/02_build_gold.R [FIRE|NEDAP|YANGXIANG]
#
# 产出：
#   results/runs/<时间戳>/gold_<device>.rds    完整金表（不入库）
#   results/gold_summary_<device>.csv          网格与 universe 的汇总（入库）

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

gold_dir <- file.path(module_dir, "results", "runs",
                      format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(gold_dir, recursive = TRUE, showWarnings = FALSE)

g <- build_gold(DEVICE, project_root)

# ---------- 校验 1：网格必须是连续日历，无洞 ----------
chk <- g$gold[, .(n = .N,
                  span = as.integer(max(record_date) - min(record_date)) + 1L),
              by = animal_id]
bad <- chk[n != span]
if (nrow(bad) > 0) {
  stop(sprintf("网格不连续：%d 头动物的行数 != 日期跨度", nrow(bad)), call. = FALSE)
}
cat(sprintf(">>> 网格校验：%d 头全部连续，合计 %d 行\n", nrow(chk), sum(chk$n)))

# ---------- 校验 2：observed 天的金表值 == 真值 ----------
both <- g$gold[DFI_status == "observed" & !is.na(daily_feed_g)]
stopifnot(max(abs(both$daily_feed_g - both$true_feed)) == 0)
cat(sprintf(">>> 一致性：%d 个 observed 天的 daily_feed_g 与 true_feed 逐位相等\n",
            nrow(both)))

# ---------- 校验 3：universe == truth_daily ----------
stopifnot(nrow(g$truth) == g$extra$n_universe)
stopifnot(nrow(g$gold[!is.na(true_feed)]) == g$extra$n_universe)
cat(sprintf(">>> universe：%d 动物天（与 truth_daily 相同）\n", g$extra$n_universe))

# ---------- 保存 ----------
saveRDS(g$gold, file.path(gold_dir, sprintf("gold_%s.rds", tolower(DEVICE))))

summary_dt <- data.table::data.table(
  device = DEVICE,
  n_animals = uniqueN(g$gold$animal_id),
  n_grid_rows = nrow(g$gold),
  n_universe = g$extra$n_universe,
  n_observed = g$extra$n_universe - g$extra$n_gated,
  n_gated = g$extra$n_gated,
  n_outside = g$extra$n_outside,
  n_outside_fullrow = g$extra$n_outside_fullrow,
  n_outside_novisit = g$extra$n_outside_novisit,
  pct_observed_of_grid = round(100 * (g$extra$n_universe - g$extra$n_gated) /
                                 nrow(g$gold), 2),
  max_abs_diff_observed = g$extra$max_abs_diff)
out_csv <- file.path(module_dir, "results",
                     sprintf("gold_summary_%s.csv", tolower(DEVICE)))
data.table::fwrite(summary_dt, out_csv)
cat(sprintf(">>> 汇总已写入 %s\n", out_csv))
cat(sprintf(">>> 金表已写入 %s\n",
            file.path(gold_dir, sprintf("gold_%s.rds", tolower(DEVICE)))))
print(summary_dt)
