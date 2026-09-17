######### Phase 2 重放门：重构后必须逐字节复现 issue #47 的数字 #########
#
# 骨架从 simulation_benchmark.R 抽到 R/skeleton.R 之后，那条脚本变成薄驱动。
# 这个门的作用是证明「抽骨架」是**纯搬迁**：同样的 FIRE 输入、同样的种子，
# 重新跑出来的两张 CSV 必须与 results/baselines/ 里的基线**逐字节相同**。
#
# 任何一处不一致都意味着搬迁改变了行为（最典型的是函数体被顺手"顺手改好"，
# 或常量被重命名后作用域查找失败）。不一致就回退重构。
#
# 运行：Rscript 测试/adfi_correction_research/scripts/01_smoke_skeleton.R [FIRE|NEDAP|YANGXIANG]

options(scipen = 999)

find_project_root <- function(start = getwd()) {
  d <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, ".git"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) stop("向上走到文件系统根仍未找到 .git", call. = FALSE)
    d <- parent
  }
}

project_root <- find_project_root()
device_dirs <- c(YANGXIANG = "YANGXIANG_扬翔", FIRE = "FIRE_奥斯本", NEDAP = "Nedap_睿保乐")

args <- commandArgs(trailingOnly = TRUE)
DEVICE <- if (length(args) >= 1) toupper(args[1]) else "FIRE"
if (!DEVICE %in% names(device_dirs)) {
  stop("未知设备：", DEVICE, "（可选 FIRE / NEDAP / YANGXIANG）", call. = FALSE)
}

baseline_dir <- file.path(project_root, "测试", "adfi_correction_research",
                          "results", "baselines")
bench_script <- file.path(project_root, "测试", "simulation_benchmark.R")
out_root <- file.path(project_root, "测试", "demo", "demo_output", device_dirs[[DEVICE]])
prefix <- sprintf("injection_benchmark_%s_", tolower(DEVICE))

files <- c("accuracy_by_variant_rate.csv", "recovery_ratio_by_type.csv")
for (f in files) {
  b <- file.path(baseline_dir, sprintf("%s_%s", tolower(DEVICE), f))
  if (!file.exists(b)) {
    stop("缺基线 ", basename(b), "—— 先跑 00_capture_baseline.R ", DEVICE, call. = FALSE)
  }
}

before <- if (dir.exists(out_root)) {
  b <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
  b[startsWith(b, prefix)]
} else character(0)

rscript <- file.path(R.home("bin"), "Rscript")
cat(sprintf(">>> 重跑 %s %s ...\n", basename(bench_script), DEVICE))
status <- system2(rscript, args = c(shQuote(bench_script), DEVICE))
if (status != 0) stop("simulation_benchmark.R 退出码 ", status, call. = FALSE)

after <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
new <- setdiff(after[startsWith(after, prefix)], before)
if (length(new) == 0) stop("没有新增的输出目录", call. = FALSE)
newest <- file.path(out_root, new[order(new, decreasing = TRUE)][1])
cat(sprintf(">>> 输出目录：%s\n", basename(newest)))

ok <- TRUE
for (f in files) {
  got_path <- file.path(newest, f)
  base_path <- file.path(baseline_dir, sprintf("%s_%s", tolower(DEVICE), f))
  got <- if (file.exists(got_path)) readBin(got_path, "raw", file.info(got_path)$size) else raw(0)
  base <- readBin(base_path, "raw", file.info(base_path)$size)
  same <- identical(got, base)
  cat(sprintf("    %-36s %s（%d vs %d 字节）\n", f,
              if (same) "PASS 逐字节一致" else "**FAIL 有差异**",
              length(got), length(base)))
  if (!same) {
    ok <- FALSE
    # 差异定位：按行比，把前几处不同的行打出来
    g <- readLines(got_path, warn = FALSE, encoding = "UTF-8")
    b <- readLines(base_path, warn = FALSE, encoding = "UTF-8")
    n <- max(length(g), length(b))
    g <- c(g, rep("<missing>", n - length(g)))
    b <- c(b, rep("<missing>", n - length(b)))
    d <- which(g != b)
    cat(sprintf("    差异行数 %d / %d\n", length(d), n))
    for (i in head(d, 10)) {
      cat(sprintf("      行 %d 新: %s\n", i, g[i]))
      cat(sprintf("      行 %d 基: %s\n", i, b[i]))
    }
  }
}

if (!ok) {
  cat("\n=== 重放门**未通过**：抽骨架改变了行为，应回退重构 ===\n")
  quit(status = 1L)
}
cat("\n=== 重放门通过：抽骨架是纯搬迁，A 轨表可逐字节重放 ===\n")
