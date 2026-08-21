###############################################################################
# ZhenMeasure 深度对比分析：national_standard vs legacy
# 四个维度：QC一致性 / 分歧画像 / 表型追踪 / 严格度与留存率
###############################################################################

rm(list = ls())
library(data.table)

# ── 路径定义 ──────────────────────────────────────────────────────────────────
base_dir <- file.path("D:", "My_project", "Cooperation_Project",
  "横向", "长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)",
  "扬翔群体饲喂仪器数据处理脚本开发",
  "V项目测试与开发", "测试", "大规模测试", "Legacy问题处理")

input_dir   <- base_dir
output_dir  <- file.path(base_dir, "deep_comparison")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

sources <- c("YANGXIANG", "FIRE", "Nedap")

# ── 辅助函数 ──────────────────────────────────────────────────────────────────

# 安全读取CSV，文件不存在则返回NULL
safe_fread <- function(path) {
  if (!file.exists(path)) {
    warning("文件不存在，跳过: ", path)
    return(NULL)
  }
  fread(path, encoding = "UTF-8")
}

# 手动计算Cohen's Kappa
# 输入: 2x2列联表矩阵 [[1,1],[1,2],[2,1],[2,2]] 对应 both_clean, leg_only, nat_only, both_flagged
# 实际上是 matrix(c(a,b,c,d), nrow=2) 其中 a=both_flagged, b=nat_only, c=leg_only, d=both_clean
calc_kappa <- function(tab) {
  # tab 是 table() 的结果，行=national(FALSE/TRUE)，列=legacy(FALSE/TRUE)
  # 转为矩阵: [1,1]=both_clean, [1,2]=leg_only, [2,1]=nat_only, [2,2]=both_flagged
  m <- as.matrix(tab)
  n <- sum(m)
  if (n == 0) return(NA_real_)
  # 观测一致率
  p_o <- (m[1,1] + m[2,2]) / n
  # 期望一致率
  row_margins <- rowSums(m) / n
  col_margins <- colSums(m) / n
  p_e <- row_margins[1] * col_margins[1] + row_margins[2] * col_margins[2]
  if (p_e >= 1) return(1.0)
  kappa <- (p_o - p_e) / (1 - p_e)
  return(kappa)
}

# 从2x2列联表提取各单元格计数
extract_counts <- function(tab) {
  m <- as.matrix(tab)
  # 确保维度是 2x2（可能某些flag全为FALSE导致只有一行/列）
  if (nrow(m) < 2) {
    # 所有值相同，全为FALSE或全为TRUE
    if (rownames(m)[1] == "FALSE") {
      # 全部未标记 -> both_clean = n, 其余 = 0
      return(list(both_flagged = 0, both_clean = sum(m), nat_only = 0, leg_only = 0))
    } else {
      # 全部标记 -> both_flagged = n
      return(list(both_flagged = sum(m), both_clean = 0, nat_only = 0, leg_only = 0))
    }
  }
  if (ncol(m) < 2) {
    if (colnames(m)[1] == "FALSE") {
      return(list(both_flagged = 0, both_clean = sum(m), nat_only = 0, leg_only = 0))
    } else {
      return(list(both_flagged = sum(m), both_clean = 0, nat_only = 0, leg_only = 0))
    }
  }
  # 正常2x2: 行=national(FALSE/TRUE), 列=legacy(FALSE/TRUE)
  list(
    both_flagged = m["TRUE", "TRUE"],
    both_clean   = m["FALSE", "FALSE"],
    nat_only     = m["TRUE", "FALSE"],
    leg_only     = m["FALSE", "TRUE"]
  )
}

cat("===== 深度对比分析开始 =====\n\n")

###############################################################################
# 维度1：QC判定一致性（逐记录级别）
###############################################################################
cat("--- 维度1：QC判定一致性 ---\n")

agree_rows <- list()

for (src in sources) {
  cat(sprintf("  处理: %s\n", src))
  nat_path <- file.path(input_dir, "per_source", paste0(src, "_qcdata_national.csv"))
  leg_path <- file.path(input_dir, "per_source", paste0(src, "_qcdata_legacy.csv"))
  dt_nat <- safe_fread(nat_path)
  dt_leg <- safe_fread(leg_path)
  if (is.null(dt_nat) || is.null(dt_leg)) next

  # 聚合到日级别（同一animal_id+record_date可能有多条采食记录）
  dt_nat_daily <- dt_nat[, .(
    is_outlier_wt   = any(is_outlier_wt == TRUE),
    is_outlier_feed = any(is_outlier_feed == TRUE)
  ), by = .(animal_id, record_date)]
  dt_leg_daily <- dt_leg[, .(
    is_outlier_wt   = any(is_outlier_wt == TRUE),
    is_outlier_feed = any(is_outlier_feed == TRUE)
  ), by = .(animal_id, record_date)]

  # 内连接对齐
  merged <- merge(dt_nat_daily, dt_leg_daily,
                  by = c("animal_id", "record_date"),
                  suffixes = c("_nat", "_leg"))
  cat(sprintf("    内连接记录数: %d\n", nrow(merged)))

  for (flag_col in c("is_outlier_wt", "is_outlier_feed")) {
    nat_col <- paste0(flag_col, "_nat")
    leg_col <- paste0(flag_col, "_leg")

    # 转为logical（fread可能读为 character "TRUE"/"FALSE"）
    if (is.character(merged[[nat_col]])) {
      merged[[nat_col]] <- as.logical(merged[[nat_col]])
    }
    if (is.character(merged[[leg_col]])) {
      merged[[leg_col]] <- as.logical(merged[[leg_col]])
    }

    tab <- table(national = merged[[nat_col]], legacy = merged[[leg_col]])
    counts <- extract_counts(tab)
    n_total <- counts$both_flagged + counts$both_clean + counts$nat_only + counts$leg_only
    agreement_rate <- (counts$both_flagged + counts$both_clean) / n_total
    kappa <- calc_kappa(tab)

    agree_rows[[length(agree_rows) + 1]] <- data.table(
      source        = src,
      flag_type     = flag_col,
      both_flagged  = counts$both_flagged,
      both_clean    = counts$both_clean,
      nat_only      = counts$nat_only,
      leg_only      = counts$leg_only,
      agreement_rate = round(agreement_rate, 6),
      kappa         = round(kappa, 4)
    )
  }
}

dt_agree <- rbindlist(agree_rows)
fwrite(dt_agree, file.path(output_dir, "01_agreement_summary.csv"))
cat("  输出: 01_agreement_summary.csv\n")

# Kappa柱状图
cat("  绘制 Kappa 柱状图...\n")
png(file.path(output_dir, "01_agreement_kappa.png"), width = 800, height = 600, res = 150)
par(mar = c(7, 4, 3, 1))

x_labels <- paste0(dt_agree$source, "\n", dt_agree$flag_type)
y_vals <- dt_agree$kappa
bar_colors <- ifelse(grepl("wt", dt_agree$flag_type), "#4472C4", "#ED7D31")

bp <- barplot(y_vals, names.arg = x_labels, las = 2, cex.names = 0.75,
              col = bar_colors, border = "white",
              ylim = c(0, max(1, max(y_vals, na.rm = TRUE) * 1.2)),
              ylab = "Cohen's Kappa",
              main = "QC判定一致性：Cohen's Kappa 系数")
# 标注数值
text(bp, y_vals, labels = sprintf("%.3f", y_vals), pos = 3, cex = 0.8, font = 2)
# 添加参考线
abline(h = 0.8, lty = 2, col = "gray50")
abline(h = 0.6, lty = 3, col = "gray70")
legend("topright", legend = c("体重异常 (is_outlier_wt)", "采食异常 (is_outlier_feed)"),
       fill = c("#4472C4", "#ED7D31"), bty = "n", cex = 0.8)
dev.off()
cat("  输出: 01_agreement_kappa.png\n\n")


###############################################################################
# 维度2：分歧记录画像
###############################################################################
cat("--- 维度2：分歇记录画像 ---\n")

disagree_rows <- list()
disagree_feat_list <- list()

for (src in sources) {
  cat(sprintf("  处理: %s\n", src))
  nat_path <- file.path(input_dir, "per_source", paste0(src, "_qcdata_national.csv"))
  leg_path <- file.path(input_dir, "per_source", paste0(src, "_qcdata_legacy.csv"))
  dt_nat <- safe_fread(nat_path)
  dt_leg <- safe_fread(leg_path)
  if (is.null(dt_nat) || is.null(dt_leg)) next

  # 聚合到日级别（同一animal_id+record_date可能有多条采食记录）
  # 对特征列取日总和，对flag列取any
  build_daily_agg <- function(dt, extra_flag_cols = character(0)) {
    flag_cols <- intersect(c("is_outlier_wt", "is_outlier_feed"), names(dt))
    feat_cols <- intersect(c("duration_sec", "feed_g", "weight_g"), names(dt))
    extra_cols <- intersect(extra_flag_cols, names(dt))
    all_flag_cols <- c(flag_cols, extra_cols)

    # flag列: any(value == TRUE)
    flag_agg <- if (length(all_flag_cols) > 0) {
      dt[, lapply(.SD, function(x) any(x == TRUE)), by = .(animal_id, record_date), .SDcols = all_flag_cols]
    } else {
      NULL
    }

    # 特征列: sum(value, na.rm = TRUE)
    feat_agg <- if (length(feat_cols) > 0) {
      dt[, lapply(.SD, sum, na.rm = TRUE), by = .(animal_id, record_date), .SDcols = feat_cols]
    } else {
      NULL
    }

    # 合并
    if (!is.null(flag_agg) && !is.null(feat_agg)) {
      merge(flag_agg, feat_agg, by = c("animal_id", "record_date"))
    } else if (!is.null(flag_agg)) {
      flag_agg
    } else if (!is.null(feat_agg)) {
      feat_agg
    } else {
      dt[, .(animal_id, record_date)]
    }
  }

  # 构建national的完整flag列列表（用于后续分歧画像）
  all_nat_flag_cols <- c(
    "flag_weight_out_of_range", "flag_weight_low", "flag_daily_weight_low", "flag_growth_curve_poor",
    "flag_feed_negative", "flag_feed_too_high", "flag_duration_negative",
    "flag_duration_too_long", "flag_duration_zero_with_feed",
    "flag_speed_too_slow", "flag_speed_too_fast",
    "flag_speed_extreme_low_feed", "flag_speed_zero_long_duration"
  )
  dt_nat_daily <- build_daily_agg(dt_nat, extra_flag_cols = all_nat_flag_cols)
  dt_leg_daily <- build_daily_agg(dt_leg)

  merged <- merge(dt_nat_daily, dt_leg_daily,
                  by = c("animal_id", "record_date"),
                  suffixes = c("_nat", "_leg"),
                  all = FALSE)

  # 转logical
  for (col in c("is_outlier_wt_nat", "is_outlier_wt_leg", "is_outlier_feed_nat", "is_outlier_feed_leg")) {
    if (is.character(merged[[col]])) merged[[col]] <- as.logical(merged[[col]])
  }

  for (flag_col in c("is_outlier_wt", "is_outlier_feed")) {
    nat_col <- paste0(flag_col, "_nat")
    leg_col <- paste0(flag_col, "_leg")

    # nat_only: national标记但legacy未标记
    nat_only_idx <- which(merged[[nat_col]] == TRUE & merged[[leg_col]] == FALSE)
    # leg_only: legacy标记但national未标记
    leg_only_idx <- which(merged[[nat_col]] == FALSE & merged[[leg_col]] == TRUE)

    cat(sprintf("    %s: nat_only=%d, leg_only=%d\n", flag_col, length(nat_only_idx), length(leg_only_idx)))

    # 对nat_only组，统计national各flag列的TRUE比例
    if (flag_col == "is_outlier_wt") {
      nat_flag_cols <- c("flag_weight_out_of_range", "flag_weight_low", "flag_daily_weight_low", "flag_growth_curve_poor")
    } else {
      nat_flag_cols <- c("flag_feed_negative", "flag_feed_too_high", "flag_duration_negative",
                         "flag_duration_too_long", "flag_duration_zero_with_feed",
                         "flag_speed_too_slow", "flag_speed_too_fast",
                         "flag_speed_extreme_low_feed", "flag_speed_zero_long_duration")
    }

    # 获取完整的nat_only记录（需要从dt_nat_daily中获取flag列）
    if (length(nat_only_idx) > 0) {
      nat_only_ids <- merged[nat_only_idx, .(animal_id, record_date)]
      dt_nat_flags <- merge(nat_only_ids, dt_nat_daily, by = c("animal_id", "record_date"))
      for (fc in nat_flag_cols) {
        if (fc %in% names(dt_nat_flags)) {
          vals <- dt_nat_flags[[fc]]
          if (is.character(vals)) vals <- as.logical(vals)
          cnt <- sum(vals == TRUE, na.rm = TRUE)
          disagree_rows[[length(disagree_rows) + 1]] <- data.table(
            source = src, group = "nat_only", flag_type = flag_col,
            flag_name = fc, count = cnt, pct = round(cnt / length(nat_only_idx) * 100, 2)
          )
        }
      }
    }

    # 特征分布数据（确保value为numeric原子向量）
    feat_vars <- c("weight_g", "feed_g", "duration_sec")
    feat_vars <- intersect(feat_vars, names(merged))
    for (fv in feat_vars) {
      feat_vals <- as.numeric(merged[[fv]])
      if (length(nat_only_idx) > 0) {
        disagree_feat_list[[length(disagree_feat_list) + 1]] <- data.table(
          source = src, flag_type = flag_col, group = "nat_only",
          feature = fv, value = feat_vals[nat_only_idx]
        )
      }
      if (length(leg_only_idx) > 0) {
        disagree_feat_list[[length(disagree_feat_list) + 1]] <- data.table(
          source = src, flag_type = flag_col, group = "leg_only",
          feature = fv, value = feat_vals[leg_only_idx]
        )
      }
      # 全体记录
      disagree_feat_list[[length(disagree_feat_list) + 1]] <- data.table(
        source = src, flag_type = flag_col, group = "all",
        feature = fv, value = feat_vals
      )
    }
  }
}

dt_disagree <- rbindlist(disagree_rows)
fwrite(dt_disagree, file.path(output_dir, "02_disagreement_profile.csv"))
cat("  输出: 02_disagreement_profile.csv\n")

# 多面板箱线图
dt_feat <- as.data.frame(rbindlist(disagree_feat_list))

cat("  绘制分歇特征箱线图...\n")
png(file.path(output_dir, "02_disagreement_features.png"), width = 800, height = 600, res = 150)

features <- c("weight_g", "feed_g", "duration_sec")
feature_labels <- c("体重 (weight_g)", "采食量 (feed_g)", "时长 (duration_sec)")

par(mfrow = c(length(features), length(sources)), mar = c(3, 3, 2.5, 0.5), oma = c(2, 2, 3, 1))

for (fv in features) {
  for (src in sources) {
    sub_dt <- dt_feat[dt_feat$source == src & dt_feat$flag_type == "is_outlier_feed" & dt_feat$feature == fv, , drop = FALSE]
    if (nrow(sub_dt) == 0) {
      plot.new()
      title(main = paste(src, "-", feature_labels[match(fv, features)]))
      next
    }
    # 构造箱线图数据
    grp_levels <- c("nat_only", "leg_only", "all")
    grp_labels <- c("nat_only", "leg_only", "全体")
    box_data <- list()
    grp_present <- character(0)
    grp_col <- sub_dt$group
    val_col <- as.numeric(sub_dt$value)
    for (g in grp_levels) {
      idx <- which(grp_col == g & !is.na(val_col))
      if (length(idx) > 0) {
        box_data[[length(box_data) + 1]] <- val_col[idx]
        grp_present <- c(grp_present, grp_labels[match(g, grp_levels)])
      }
    }
    if (length(box_data) == 0) {
      plot.new()
      title(main = paste(src, "-", feature_labels[match(fv, features)]))
      next
    }
    boxplot(box_data, names = grp_present, outline = FALSE,
            col = c("#FF6B6B", "#4ECDC4", "#95A5A6")[seq_along(box_data)],
            main = paste(src, "-", feature_labels[match(fv, features)]),
            cex.main = 0.75, cex.axis = 0.7)
  }
}
mtext("分歇记录特征分布（采食异常标记对比）", outer = TRUE, cex = 1.1, font = 2)
dev.off()
cat("  输出: 02_disagreement_features.png\n\n")


###############################################################################
# 维度3：表型个体级别追踪
###############################################################################
cat("--- 维度3：表型个体级别追踪 ---\n")

pheno_merged_list <- list()

for (src in sources) {
  cat(sprintf("  处理: %s\n", src))
  nat_path <- file.path(input_dir, "phenotypes_compare", paste0(src, "_pheno_national.csv"))
  leg_path <- file.path(input_dir, "phenotypes_compare", paste0(src, "_pheno_legacy.csv"))
  dt_nat <- safe_fread(nat_path)
  dt_leg <- safe_fread(leg_path)
  if (is.null(dt_nat) || is.null(dt_leg)) next

  # 只保留共同个体
  common_ids <- intersect(dt_nat$animal_id, dt_leg$animal_id)
  cat(sprintf("    共同个体数: %d (national=%d, legacy=%d)\n",
              length(common_ids), nrow(dt_nat), nrow(dt_leg)))

  dt_nat_common <- dt_nat[animal_id %in% common_ids]
  dt_leg_common <- dt_leg[animal_id %in% common_ids]

  merged <- merge(dt_nat_common[, .(animal_id, ADG_g, ADFI_g, FCR)],
                  dt_leg_common[, .(animal_id, ADG_g, ADFI_g, FCR)],
                  by = "animal_id", suffixes = c("_nat", "_leg"))

  # 去除NA行（legacy可能有缺失）
  merged <- na.omit(merged)
  if (nrow(merged) == 0) {
    warning(sprintf("%s: 共同个体无有效表型数据", src))
    next
  }

  merged[, source := src]
  pheno_merged_list[[length(pheno_merged_list) + 1]] <- merged
}

dt_pheno <- rbindlist(pheno_merged_list)

# 散点图: ADG / ADFI / FCR
cat("  绘制表型散点图...\n")
png(file.path(output_dir, "03_phenotype_scatter.png"), width = 800, height = 600, res = 150)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), oma = c(1, 1, 2, 1))

pheno_vars <- c("ADG_g", "ADFI_g", "FCR")
pheno_labels <- c("ADG (g)", "ADFI (g)", "FCR")
src_colors <- c("#4472C4", "#ED7D31", "#70AD47")
names(src_colors) <- sources

for (pv in pheno_vars) {
  nat_col <- paste0(pv, "_nat")
  leg_col <- paste0(pv, "_leg")

  x_vals <- dt_pheno[[nat_col]]
  y_vals <- dt_pheno[[leg_col]]

  # 去除NA
  valid <- !is.na(x_vals) & !is.na(y_vals)
  x_vals <- x_vals[valid]
  y_vals <- y_vals[valid]

  plot(x_vals, y_vals, pch = 16, cex = 0.8,
       col = adjustcolor(src_colors[dt_pheno$source[valid]], alpha = 0.6),
       xlab = "National", ylab = "Legacy",
       main = pheno_labels[match(pv, pheno_vars)])

  # 1:1参考线
  abline(0, 1, lty = 2, col = "gray50")

  # 回归线
  if (length(x_vals) > 2) {
    fit <- lm(y_vals ~ x_vals)
    abline(fit, col = "red", lwd = 1.5)
    r_val <- cor(x_vals, y_vals)
    legend("topleft",
           legend = sprintf("r = %.3f\nn = %d", r_val, length(x_vals)),
           bty = "n", cex = 0.8)
  }
}
mtext("表型散点图 (National vs Legacy)", outer = TRUE, cex = 1.1, font = 2)
dev.off()
cat("  输出: 03_phenotype_scatter.png\n")

# Bland-Altman 图 (FCR)
cat("  绘制 Bland-Altman 图...\n")
png(file.path(output_dir, "03_bland_altman_FCR.png"), width = 800, height = 600, res = 150)
par(mar = c(5, 5, 4, 2))

x_vals <- dt_pheno$FCR_nat
y_vals <- dt_pheno$FCR_leg
valid <- !is.na(x_vals) & !is.na(y_vals)
x_vals <- x_vals[valid]
y_vals <- y_vals[valid]

mean_vals <- (x_vals + y_vals) / 2
diff_vals <- y_vals - x_vals  # legacy - national
mean_diff <- mean(diff_vals)
sd_diff <- sd(diff_vals)
loa_upper <- mean_diff + 1.96 * sd_diff
loa_lower <- mean_diff - 1.96 * sd_diff

plot(mean_vals, diff_vals, pch = 16, cex = 0.8,
     col = adjustcolor(src_colors[dt_pheno$source[valid]], alpha = 0.6),
     xlab = "两种方法均值 ((National + Legacy) / 2)",
     ylab = "差值 (Legacy - National)",
     main = "Bland-Altman 图：FCR",
     ylim = c(min(diff_vals, loa_lower) * 0.9, max(diff_vals, loa_upper) * 1.1))
abline(h = mean_diff, col = "red", lwd = 2)
abline(h = loa_upper, col = "blue", lty = 2, lwd = 1.5)
abline(h = loa_lower, col = "blue", lty = 2, lwd = 1.5)
abline(h = 0, col = "gray50", lty = 3)

legend("topright",
       legend = c(sprintf("均值差 = %.3f", mean_diff),
                  sprintf("95%% LOA: [%.3f, %.3f]", loa_lower, loa_upper),
                  sprintf("SD差 = %.3f", sd_diff),
                  sprintf("n = %d", length(diff_vals))),
       bty = "n", cex = 0.85)
dev.off()
cat("  输出: 03_bland_altman_FCR.png\n\n")


###############################################################################
# 维度4：QC严格度与数据利用率
###############################################################################
cat("--- 维度4：QC严格度与数据利用率 ---\n")

# 留存率曲线
retention_rows <- list()
for (src in sources) {
  diff_path <- file.path(input_dir, "per_source", paste0(src, "_diff.csv"))
  dt_diff <- safe_fread(diff_path)
  if (is.null(dt_diff)) next

  # 提取 n_after / n_records 用于留存率计算
  # 步骤顺序: step1_read(n_records) -> step2_overall(n_after) -> step3_weight_qc(n_after)
  #            -> step4_feed_qc(n_after) -> step5_daily(n_daily) -> step6_impute(n_daily) -> step7_phenotype(n_animals)
  step_defs <- list(
    list(step = "step1_read",      metric = "n_records"),
    list(step = "step2_overall",   metric = "n_after"),
    list(step = "step3_weight_qc", metric = "n_after"),
    list(step = "step4_feed_qc",   metric = "n_after"),
    list(step = "step5_daily",     metric = "n_daily"),
    list(step = "step6_impute",    metric = "n_daily"),
    list(step = "step7_phenotype", metric = "n_animals")
  )

  for (sd in step_defs) {
    row_nat <- dt_diff[step == sd$step & metric == sd$metric]
    if (nrow(row_nat) == 0) next
    nat_val <- row_nat$national_standard[1]
    leg_val <- row_nat$legacy[1]

    retention_rows[[length(retention_rows) + 1]] <- data.table(
      source = src, step = sd$step, metric = sd$metric,
      national = nat_val, legacy = leg_val
    )
  }
}

dt_retention <- rbindlist(retention_rows)

# 计算留存率（相对于step1的n_records）
cat("  绘制留存率曲线...\n")
png(file.path(output_dir, "04_retention_curve.png"), width = 800, height = 600, res = 150)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), oma = c(1, 1, 2, 1))

step_labels <- c("Read", "Overall", "Wt QC", "Feed QC", "Daily", "Impute", "Phenotype")
line_colors <- c(national = "#4472C4", legacy = "#ED7D31")

for (src in sources) {
  sub <- dt_retention[source == src]
  if (nrow(sub) == 0) {
    plot.new()
    title(main = src)
    next
  }

  # 基准值 = step1的n_records
  base_val <- sub[step == "step1_read" & metric == "n_records", national]
  if (length(base_val) == 0 || is.na(base_val) || base_val == 0) base_val <- 1

  nat_rate <- sub$national / base_val
  leg_rate <- sub$legacy / base_val

  y_max <- max(1, max(c(nat_rate, leg_rate), na.rm = TRUE) * 1.05)

  plot(seq_along(nat_rate), nat_rate, type = "b", pch = 16, col = line_colors["national"],
       lwd = 2, xlab = "处理步骤", ylab = "留存率",
       main = src, ylim = c(0, y_max), xaxt = "n", cex = 1.2)
  lines(seq_along(leg_rate), leg_rate, type = "b", pch = 17, col = line_colors["legacy"],
        lwd = 2, cex = 1.2)
  axis(1, at = seq_along(step_labels), labels = step_labels, las = 2, cex.axis = 0.65)
  grid(col = "gray90")
}
mtext("数据留存率曲线", outer = TRUE, cex = 1.1, font = 2)
legend("bottomleft", legend = c("National", "Legacy"),
       col = line_colors, lwd = 2, pch = c(16, 17), bty = "n", cex = 0.8)
dev.off()
cat("  输出: 04_retention_curve.png\n")

# 表型CV汇总
cat("  计算表型CV...\n")
cv_rows <- list()
for (src in sources) {
  for (method in c("national", "legacy")) {
    pheno_path <- file.path(input_dir, "phenotypes_compare",
                            paste0(src, "_pheno_", method, ".csv"))
    dt_p <- safe_fread(pheno_path)
    if (is.null(dt_p)) next

    for (pv in c("ADFI_g", "ADG_g", "FCR")) {
      vals <- dt_p[[pv]]
      vals <- vals[!is.na(vals)]
      if (length(vals) > 1) {
        cv_val <- sd(vals) / mean(vals)
      } else {
        cv_val <- NA_real_
      }
      cv_rows[[length(cv_rows) + 1]] <- data.table(
        source = src, method = method, phenotype = pv, cv = round(cv_val, 4)
      )
    }
  }
}

dt_cv_long <- rbindlist(cv_rows)
# 转为宽表
dt_cv_wide <- dcast(dt_cv_long, source + method ~ phenotype, value.var = "cv")
setnames(dt_cv_wide, c("ADFI_g", "ADG_g", "FCR"), c("ADFI_cv", "ADG_cv", "FCR_cv"))
fwrite(dt_cv_wide, file.path(output_dir, "04_phenotype_cv_summary.csv"))
cat("  输出: 04_phenotype_cv_summary.csv\n")


###############################################################################
# 完成
###############################################################################
cat("\n===== 分析完成 =====\n")
cat("所有输出文件：\n")
output_files <- list.files(output_dir, full.names = TRUE)
for (f in output_files) {
  cat(sprintf("  %s\n", f))
}
cat("\nDone.\n")
