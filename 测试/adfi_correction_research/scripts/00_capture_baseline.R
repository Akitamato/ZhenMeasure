######### 抓 issue #47 基线：重构前的 A 轨成绩单 #########
#
# 目的：Phase 2 把 simulation_benchmark.R 的骨架抽到 R/skeleton.R 之后，
#       必须能逐字节重放 issue #47 的数字。为此要先把重构前的输出存下来。
#
# 做法：不 source 那个脚本（它的 rm(list = ls()) 会清空本脚本的环境），
#       而是按它自己文档里的 CLI 口径起一个子进程，再把两张 CSV 拷进
#       results/baselines/。这同时验证了它作为独立脚本仍可运行。
#
# 运行：Rscript 测试/adfi_correction_research/scripts/00_capture_baseline.R [FIRE|NEDAP|YANGXIANG]
#       从仓库根调用。缺省 FIRE（最快，约 1-2 分钟）。

options(scipen = 999)

# 模块脚本在 测试/adfi_correction_research/scripts/ 下，'..' 得到的不是仓库根，
# 所以向上走找 .git（simulation_benchmark.R 里 file.path(script_dir, "..") 的写法
# 只对 测试/ 下成立，搬进子目录就会算错）。
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
module_dir   <- file.path(project_root, "测试", "adfi_correction_research")
baseline_dir <- file.path(module_dir, "results", "baselines")
dir.create(baseline_dir, recursive = TRUE, showWarnings = FALSE)

args <- commandArgs(trailingOnly = TRUE)
DEVICE <- if (length(args) >= 1) toupper(args[1]) else "FIRE"
# 与 skeleton.R 的 device_dirs 保持一致（本脚本故意不 source 骨架：骨架会 library()
# 并在全局建对象，而这里只是起子进程，不需要那些）
device_dirs <- c(YANGXIANG = "YANGXIANG_扬翔", FIRE = "FIRE_奥斯本", NEDAP = "Nedap_睿保乐")
if (!DEVICE %in% names(device_dirs)) {
  stop("未知设备：", DEVICE, "（可选 FIRE / NEDAP / YANGXIANG）", call. = FALSE)
}

bench_script <- file.path(project_root, "测试", "simulation_benchmark.R")
if (!file.exists(bench_script)) stop("找不到 ", bench_script, call. = FALSE)

# 输出目录带时间戳，跑之前先记住已有的，跑完取新增的那个。
# 注意是 demo_output/<设备目录>/injection_benchmark_<dev>_<时间戳>/ 两层，
# 直接 list  demo_output 只能拿到设备目录名。
out_root <- file.path(project_root, "测试", "demo", "demo_output",
                      device_dirs[[DEVICE]])
prefix <- sprintf("injection_benchmark_%s_", tolower(DEVICE))
before <- if (dir.exists(out_root)) {
  b <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
  b[startsWith(b, prefix)]
} else character(0)

rscript <- file.path(R.home("bin"), "Rscript")
cat(sprintf(">>> 以子进程运行 %s %s（基线采集）...\n", basename(bench_script), DEVICE))
t0 <- Sys.time()
status <- system2(rscript, args = c(shQuote(bench_script), DEVICE))
if (status != 0) stop("simulation_benchmark.R 退出码 ", status, call. = FALSE)
cat(sprintf(">>> 子进程完成，耗时 %.1f 分钟\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

after <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
after <- after[startsWith(after, prefix)]
new <- setdiff(after, before)
if (length(new) == 0) stop("没有新增的输出目录，无法采集基线", call. = FALSE)
newest <- file.path(out_root, new[order(new, decreasing = TRUE)][1])
if (length(after) == 0) stop("没有新增的输出目录，无法采集基线", call. = FALSE)
newest <- file.path(out_root, after[order(after, decreasing = TRUE)][1])
cat(sprintf(">>> 取最新输出目录：%s\n", basename(newest)))

files <- c("accuracy_by_variant_rate.csv", "recovery_ratio_by_type.csv")
for (f in files) {
  src <- file.path(newest, f)
  if (!file.exists(src)) stop("输出目录里缺 ", f, call. = FALSE)
  dst <- file.path(baseline_dir, sprintf("%s_%s", tolower(DEVICE), f))
  file.copy(src, dst, overwrite = TRUE)
  cat(sprintf("    已存基线：%s（%d 字节）\n", basename(dst), file.info(dst)$size))
}

# 环境快照：重放门要求同机同版本，版本变了要能一眼看出
env_lines <- c(
  sprintf("captured_at    = %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  sprintf("device         = %s", DEVICE),
  sprintf("benchmark_sha  = %s", system2("git", c("-C", shQuote(project_root),
                                                 "rev-parse", "HEAD"), stdout = TRUE)),
  sprintf("git_dirty      = %s", ifelse(
    length(system2("git", c("-C", shQuote(project_root), "status", "--porcelain"),
                   stdout = TRUE)) > 0, "TRUE", "FALSE")),
  sprintf("R              = %s", R.version.string),
  sprintf("data.table     = %s", as.character(packageVersion("data.table"))),
  sprintf("lme4           = %s", as.character(packageVersion("lme4"))),
  sprintf("imputeTS       = %s", as.character(packageVersion("imputeTS"))),
  sprintf("zoo            = %s", as.character(packageVersion("zoo"))),
  sprintf("mgcv           = %s", if (requireNamespace("mgcv", quietly = TRUE))
    as.character(packageVersion("mgcv")) else "not installed")
)
env_path <- file.path(baseline_dir, sprintf("env_%s.txt", tolower(DEVICE)))
writeLines(env_lines, env_path, useBytes = TRUE)
cat(sprintf("    已存环境快照：%s\n", basename(env_path)))
cat(sprintf("\n=== 基线采集完成（%s）===\n", DEVICE))
