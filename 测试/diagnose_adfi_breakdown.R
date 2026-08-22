######### 诊断 ADFI 下降：口径对齐 + 逐项分解 #########
# 目标：把 ADFI 从「原始全期」到「质控后阶段」的下降，拆成 4 项来源
#   ① 口径差异(全期 vs 体重阶段窗口) ② 动物删除(616→324)
#   ③ 记录级物理纠正 ④ 插补
# 口径统一：个体 ADFI 均值 = sum(daily_feed_g)/天数，与表型一致

rm(list = ls())

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")
project_root <- if (script_dir == "") getwd() else normalizePath(file.path(script_dir, ".."))

library(data.table)
pkg_dir <- file.path(project_root, "项目本体/ZhenMeasure")
pkgload::load_all(pkg_dir, quiet = TRUE, export_all = TRUE)

# --- 路径与 config（与南沙测试一致，0.99 阈值） ---
data_path   <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/原始数据/南沙")
format_path <- file.path(project_root, "测试/demo/demo_input/YANGXIANG_扬翔/附加信息/YANGXIANG_data_format.json")
custom_config <- list(national_standard = list(test_weight_range = c(200, 20)))

cat(">>> 运行 pipeline ...\n")
t0 <- Sys.time()
result <- run_zhen_measure(
    data_path            = data_path,
    data_type            = "YANGXIANG",
    format_path          = format_path,
    qc_method            = "national_standard",
    phenotype_method     = "report",
    stage_mode           = "weight",
    target_weight_stages = "YANGXIANG",
    output_dir           = NULL,
    config               = custom_config,
    growth_curve         = FALSE,
    growth_curve_test    = FALSE
)
cat(sprintf("运行完成，耗时 %.1f 秒\n\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

raw <- as.data.table(result$raw_daily)          # 原始日汇总（616头）
qc  <- as.data.table(result$daily_records)      # 质控后日汇总（324头，插补后）

# --- 个体 ADFI 均值 ---
adfi_mean <- function(dt) {
    if (nrow(dt) == 0) return(NA_real_)
    pa <- dt[, .(adfi = sum(daily_feed_g, na.rm = TRUE) / .N), by = animal_id]
    mean(pa$adfi, na.rm = TRUE)
}

keep324 <- unique(qc$animal_id)

# --- 各锚点 ---
# A. 原始全期（616头，所有天）
A <- adfi_mean(raw)

# B. 原始阶段（616头，原始体重窗口 30-100kg）
raw_stage <- raw[daily_weight_g / 1000 >= 30 & daily_weight_g / 1000 <= 100]
B <- adfi_mean(raw_stage)

# C. 原始阶段（324头，原始体重窗口）
C <- adfi_mean(raw_stage[animal_id %in% keep324])

# D. 原始阶段（324头，质控体重窗口：用 daily_records 确定窗口日期，取 raw 的 feed）
qc_window <- qc[daily_weight_g / 1000 >= 30 & daily_weight_g / 1000 <= 100]
raw_in_qc_window <- raw[qc_window[, .(animal_id, record_date)],
                        on = .(animal_id, record_date), nomatch = 0]
D <- adfi_mean(raw_in_qc_window)

# E. 质控后阶段（324头，含插补，质控体重窗口）
E <- adfi_mean(qc_window)

# E_nonimpute. 质控后阶段（仅非插补天 = 纠正后真实值）
qc_nonimpute <- qc_window[!(is_imputed_feed %in% TRUE)]
E_ni <- adfi_mean(qc_nonimpute)

# --- 输出分解表 ---
cat("========== ADFI 分解报告（30-100kg 阶段窗口） ==========\n\n")

cat("--- 各锚点 ADFI（个体均值, g/天） ---\n")
anchors <- data.table(
    步骤 = c("A 原始全期(616头)",
             "B 原始阶段(616头,原始体重窗口)",
             "C 原始阶段(324头,原始体重窗口)",
             "D 原始阶段(324头,质控体重窗口)",
             "E 质控后阶段(324头,含插补)",
             "E' 质控后阶段(324头,非插补天)"),
    ADFI_g = round(c(A, B, C, D, E, E_ni), 1)
)
print(anchors)
cat("\n")

cat("--- 分解（正数=该项导致 ADFI 下降） ---\n")
breakdown <- data.table(
    来源 = c("① 口径差异(全期→阶段窗口) [A-B]",
             "② 动物删除(616→324) [B-C]",
             "   其中:体重纠正改窗口 [C-D]",
             "③ 记录级物理纠正 [D-E']",
             "④ 插补 [E'-E]",
             "==== 合计 [A-E] ===="),
    下降量_g = round(c(A - B,
                      B - C,
                      C - D,
                      D - E_ni,
                      E_ni - E,
                      A - E), 1)
)
print(breakdown)
cat("\n")

# --- 各锚点的动物数与天数（辅助理解） ---
cat("--- 各锚点样本量 ---\n")
sizes <- data.table(
    步骤 = c("A 原始全期", "B 原始阶段616", "C 原始阶段324", "D 原始窗口324",
             "E 质控后阶段", "E' 非插补天"),
    动物数 = c(uniqueN(raw$animal_id),
               uniqueN(raw_stage$animal_id),
               uniqueN(raw_stage[animal_id %in% keep324]$animal_id),
               uniqueN(raw_in_qc_window$animal_id),
               uniqueN(qc_window$animal_id),
               uniqueN(qc_nonimpute$animal_id)),
    天数 = c(nrow(raw), nrow(raw_stage),
             nrow(raw_stage[animal_id %in% keep324]),
             nrow(raw_in_qc_window), nrow(qc_window), nrow(qc_nonimpute))
)
print(sizes)
cat("\n")

# --- 插补天占比（辅助理解④） ---
n_imp <- nrow(qc_window[is_imputed_feed %in% TRUE])
cat(sprintf("--- 插补天占比: %d / %d 天 (%.1f%%) ---\n\n",
            n_imp, nrow(qc_window), n_imp / nrow(qc_window) * 100))

cat("========== 分解完成 ==========\n")
