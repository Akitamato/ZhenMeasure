# Legacy 剩余独有策略评估与决策

**决策日期**: 2026-06-22
**评估范围**: STL 和 Gompertz 整合后，Legacy 剩余的 5 项独有策略
**决策依据**: 源码分析、3 数据源对比测试数据、审计报告、整合设计文档
**决策人**: 决策者 Agent

---

## 1. 评估背景

STL 时间序列采食量检测和 Gompertz 生长曲线体重检测已整合到 National Standard 方法中（V0.2.6），默认关闭。本报告评估 Legacy 剩余的 5 项独有策略，决定哪些值得整合、哪些直接丢弃，最终目标是完全弃用 Legacy 路径。

**评估标准**:

| 维度 | 说明 |
|------|------|
| 唯一性 | 该策略是否提供了 National 已有能力覆盖不到的检测维度 |
| 互补性 | 与 National 现有标记的重叠程度（Kappa 一致性） |
| 计算成本 | 额外的计算开销 |
| 风险 | 误标记、过度过滤、代码维护负担 |
| 数据证据 | 3 数据源（YANGXIANG/FIRE/Nedap）上的实际表现 |

---

## 2. 策略评估

### 2.1 SD 阈值体重检测 (flag_SD_WT)

**实现位置**: `zhenm_qc_weight_standard.R`, `.qc_weight_standard_legacy()`, L387-394

**实现逻辑**:
```r
dt[, .mean_wt := mean(weight_g, na.rm = TRUE), by = animal_id]
dt[, .sd_wt := sd(weight_g, na.rm = TRUE), by = animal_id]
dt[!is.na(weight_g), flag_SD_WT := abs(weight_g - .mean_wt) > sd_threshold * .sd_wt]
```
按动物计算均值和标准差，标记偏离 `sd_threshold`（默认 3）倍标准差的单次体重记录。

**与 National 的对比**:

National 的体重 QC 使用两轮 RLM（`weight ~ day + day^2`），通过 RLM 权重（0-1）识别异常记录。RLM 是基于回归残差的稳健方法，能捕捉时间趋势；SD 阈值是基于分布的简单方法，不考虑时间维度。

**数据证据**:

| 数据源 | flag_SD_WT 标记量 | National is_outlier_wt | 重叠情况 |
|--------|-------------------|----------------------|----------|
| YANGXIANG | 4 | 10,077 | 几乎完全被 National 覆盖 |
| FIRE | 0 | 7,379 | 零贡献 |
| Nedap | 0 | 12 | 零贡献 |

YANGXIANG 仅 4 条被 SD 阈值标记而 National 未标记的记录。flag_SD_WT 的标记量（4 条）在 346,033 条记录中占比 0.001%，可忽略。

**评估**:

| 维度 | 评分 | 说明 |
|------|------|------|
| 唯一性 | **极低** | National 的两轮 RLM 已覆盖 SD 阈值的检测能力，且更精确 |
| 互补性 | **极低** | 仅 4 条记录有差异，Kappa = 0.847（体重整体一致性高） |
| 计算成本 | **极低** | 一次按组 mean + sd 计算 |
| 风险 | **低** | 3 倍 SD 阈值本身不会过度标记 |
| 数据证据 | **无实际贡献** | 3 个数据源合计仅 4 条独有标记 |

**决策: 不推荐整合。** SD 阈值是统计学中最基础的异常检测方法，National 的 RLM 稳健回归在原理上完全包含并超越了 SD 阈值的能力。实测数据证实 flag_SD_WT 几乎没有独立贡献（3 个数据源合计仅 4 条标记）。整合此策略只会增加代码复杂度，不带来任何检测能力的提升。

---

### 2.2 多项式 RLM 体重检测 (flag_RLM_WT)

**实现位置**: `zhenm_qc_weight_standard.R`, `.qc_weight_standard_legacy()`, L400-453

**实现逻辑**:
```r
# 对每日中位数体重拟合多项式 RLM
x <- as.numeric(daily_sub$record_date - min(daily_sub$record_date))
y <- daily_sub$median_weight_per_day
rlm_fit <- .safe_rlm_fit(y, x, formula_type = "polynomial", maxit = 60)
# 标记 RLM 权重 < 0.5 的日期
dt[idx, flag_RLM_WT := rlm_weight < 0.5]
```
Legacy 的 RLM 在**日中位数体重**上拟合 `y ~ x + I(x^2)`，阈值 0.5。

**与 National 的对比**:

National 也使用多项式 RLM（`weight ~ day + day^2`），但有以下关键差异:

| 维度 | National | Legacy |
|------|----------|--------|
| 拟合层级 | **两轮**: 第一轮单记录级，第二轮日体重级 | 一轮，日中位数级 |
| 日体重计算 | RLM 权重加权平均 | 简单中位数 |
| RLM 阈值 | 0.25（更严格） | 0.5（更宽松） |
| 数据清洗 | 第一轮去噪后再算日体重 | 直接用原始中位数 |

National 的两轮 RLM 设计更精密: 第一轮在单记录级去噪，用去噪后的数据计算加权平均日体重，再在日体重级做第二轮 RLM。Legacy 只有一轮，且使用中位数（不利用 RLM 权重信息）。

**数据证据**:

| 数据源 | flag_RLM_WT | National 相当标记 (flag_weight_low + flag_daily_weight_low) |
|--------|-------------|-----------------------------------------------------------|
| YANGXIANG | 4,003 | 5,984 (4,657 + 1,327) |
| FIRE | 2,063 | 6,463 (6,033 + 430) |
| Nedap | 6 | 12 |

National 标记量更多（阈值 0.25 比 0.5 更严格），且两轮去噪后日体重更准确。Legacy 的单轮 RLM 在中位数日体重上拟合，噪声更大，阈值更宽松，标记量反而更少。

**评估**:

| 维度 | 评分 | 说明 |
|------|------|------|
| 唯一性 | **低** | National 的两轮 RLM 完全覆盖 Legacy 单轮 RLM 的能力 |
| 互补性 | **低** | 两者使用相同的多项式 RLM 核心算法，差异仅在预处理和阈值 |
| 计算成本 | **低** | 一次 RLM 拟合 |
| 风险 | **低** | 阈值 0.5 不会过度标记 |
| 数据证据 | **被 National 超越** | National 的两轮 RLM + 加权平均在所有数据源上标记更多且更准确 |

**决策: 不推荐整合。** National 的两轮 RLM 设计在架构上优于 Legacy 的单轮 RLM。核心差异在于: (1) National 在单记录级先去噪再算日体重，噪声更低；(2) National 使用加权平均而非中位数，保留了 RLM 权重信息；(3) National 的 0.25 阈值比 Legacy 的 0.5 更严格，检测能力更强。整合 Legacy 的单轮 RLM 不会增加任何检测维度。

---

### 2.3 中位数日聚合 (median_weight_per_day)

**实现位置**: `zhenm_qc_weight_standard.R`, `.qc_weight_standard_legacy()`, L397-398; `zhenm_daily_aggregate_filtered.R`, `ZhenM_standard_to_daily_filtered()`, L159-165

**实现逻辑**:
```r
# Weight QC 阶段: 计算每日中位数体重
dt[, median_weight_per_day := median(weight_g, na.rm = TRUE), by = .(animal_id, record_date)]

# 日聚合阶段: 使用中位数
if (has_legacy_weight) {
  daily_weight_g = median(weight_filtered, na.rm = TRUE)
}
```

**与 National 的对比**:

National 使用 RLM 权重加权平均:
```r
# Weight QC 阶段: 加权平均日体重
sw <- sum(cleaned_weight * rlm_weight)
sw_w <- sum(rlm_weight)
daily_weight = sw / sw_w
```

| 维度 | National (加权平均) | Legacy (中位数) |
|------|-------------------|----------------|
| 异常值处理 | RLM 权重自动降权异常记录 | 依赖 is_outlier_wt 标记过滤后取中位数 |
| 信息利用 | 利用 RLM 权重（0-1 连续值） | 仅利用二值标记（异常/正常） |
| 精度 | 更高（权重反映拟合质量） | 较低（所有正常记录等权） |

**评估**:

| 维度 | 评分 | 说明 |
|------|------|------|
| 唯一性 | **低** | 中位数是加权平均的简化版，信息量更少 |
| 互补性 | **无** | 两种方法计算同一指标（日体重），取代表不同 |
| 计算成本 | **更低** | 中位数计算比加权平均简单 |
| 风险 | **中** | 中位数在多记录日上可能丢失精度 |
| 数据证据 | **劣于 National** | National 保留更多动物（118 vs 99, 134 vs 83） |

**决策: 不推荐整合。** 加权平均在统计上优于中位数，因为它利用了 RLM 权重这一连续质量指标，而中位数仅使用二值过滤后的等权平均。在 National 的两轮 RLM 架构下，异常记录已被第一轮 RLM 降权或排除，加权平均能更精确地反映真实日体重。中位数的优势（对异常值鲁棒）已被 National 的 RLM 去噪步骤覆盖。

---

### 2.4 百分位数采食量检测 (flag_percentile_low / flag_percentile_high)

**实现位置**: `zhenm_qc_feed_standard.R`, `.qc_feed_standard_legacy()`, L232-258

**实现逻辑**:
```r
# 计算每日总采食量
daily_feed <- dt[, .(daily_total_feed = sum(feed_g, na.rm = TRUE)),
                 by = .(animal_id, record_date)]

# 按动物计算分位数阈值
daily_feed[, `:=`(
  is_low_outlier = daily_total_feed < quantile(daily_total_feed, 0.025, na.rm = TRUE),
  is_high_outlier = daily_total_feed > quantile(daily_total_feed, 0.99, na.rm = TRUE)
), by = animal_id]
```
对每个动物的日总采食量分布，标记低于 P2.5 和高于 P99 的日期。

**与 National 的对比**:

National 的 `flag_feed_too_high` 也使用分位数（P99），但在单记录级别:
```r
# National: 单次采食量 > 个体日采食量 P99
daily_feed[, daily_threshold_feed := quantile(daily_total_feed, 0.99), by = animal_id]
dt[feed_g > daily_threshold_feed, flag_feed_too_high := TRUE]
```

| 维度 | National | Legacy |
|------|----------|--------|
| 检测粒度 | 单记录级 | 日级别 |
| 上尾阈值 | P99（日总采食量） | P99（日总采食量） |
| 下尾检测 | 无 | P2.5 |
| 检测方式 | 单次采食量 vs 日总阈值 | 日总采食量 vs 分布分位数 |

**数据证据**:

| 数据源 | flag_percentile_low | flag_percentile_high | National flag_feed_too_high |
|--------|--------------------|--------------------|-----------------------------|
| YANGXIANG | 2,474 | 4,068 | 15 |
| FIRE | 1,307 | 1,760 | 0 |
| Nedap | 23 | 23 | 0 |

Legacy 的百分位数标记量远高于 National（YANGXIANG: 6,542 vs 15）。这是因为:
1. Legacy 在日级别标记，National 在单记录级别标记
2. Legacy 的 P2.5 下尾标记是 National 没有的
3. Legacy 的标记量固定约为总天数的 2.5% + 1% = 3.5%，这是一种"按设计标记"的策略

**评估**:

| 维度 | 评分 | 说明 |
|------|------|------|
| 唯一性 | **中** | P2.5 下尾标记是 National 没有的维度 |
| 互补性 | **低** | Kappa = 0.03-0.04，与 National 几乎无重叠，但 STL 已覆盖时间序列异常检测 |
| 计算成本 | **极低** | 简单分位数计算 |
| 风险 | **中高** | (1) 固定标记 3.5% 的天数，可能包含正常波动；(2) 小样本不稳定（审计报告 M-1）；(3) STL 已提供更优的时间序列异常检测 |
| 数据证据 | **标记量过大** | YANGXIANG 标记 6,542 条（National 仅标记 15 条），可能过度标记 |

**决策: 不推荐整合。** 理由如下:

1. **STL 已提供更优的替代方案**。STL 时间序列异常检测（已整合到 National）能检测偏离时间趋势的异常日，比固定分位数的方法更智能、更自适应。STL 考虑了时间序列的趋势和季节性，而百分位数法仅看静态分布。

2. **标记量过大且固定**。百分位数法按设计标记 3.5% 的天数（P2.5 + P1%），无论数据质量如何。这意味着在高质量数据上会误标记正常波动，在低数据质量上可能标记不足。

3. **小样本不稳定**。当个体有效日记录 < 30 天时，P2.5 和 P99 的估计受极端值影响大，可能导致误标记（审计报告 M-1）。

4. **National 的 P99 上尾标记已存在**。National 的 `flag_feed_too_high` 使用 P99 阈值（在日总采食量级别），覆盖了上尾检测。下尾检测的价值有限（低采食量日可能是正常的限饲或应激期）。

---

### 2.5 简单线性插值 (zoo::na.approx)

**实现位置**: `zhenm_impute.R`, `.impute_weight_legacy()`, L121-138; `zhenm_impute_feed.R`, `.impute_feed_legacy()`, L1-26

**实现逻辑**:
```r
# 体重插补: 简单线性插值 + rule=2 端点外推
dt[, daily_weight_g := zoo::na.approx(daily_weight_g, na.rm = FALSE, rule = 2), by = animal_id]

# 采食量插补: 简单线性插值 + rule=2 端点外推 + 负值修正
dt[, daily_feed_g := zoo::na.approx(daily_feed_g, na.rm = FALSE, rule = 2), by = animal_id]
dt[daily_feed_g < 0, daily_feed_g := 0]
```

**与 National 的对比**:

| 维度 | National | Legacy |
|------|----------|--------|
| 体重插补 | Kalman Filter (imputeTS::na_kalman, StructTS 模型) + 物理边界保护 | zoo::na.approx 线性插值 + rule=2 |
| 采食量插补 | 离散缺失: Loess 回归；连续缺失: 线性回归外推 + FCR 阶段验证 | zoo::na.approx 线性插值 + rule=2 |
| 端点处理 | Kalman 自动处理 + 物理边界（下限 90% min, 上限 max+20kg） | rule=2 最近邻延伸 |
| 质量验证 | 采食量插补后检查 FCR 是否在合理范围 | 仅负值修正 |
| 复杂度 | 高（Kalman + Loess + FCR 验证） | 低（单行 na.approx） |

**数据证据**:

| 数据源 | 方法 | 插补 feed | 插补 wt | 动物数 | FCR NA |
|--------|------|----------|---------|--------|--------|
| YANGXIANG | National | 47 | 536 | 118 | 0 |
| YANGXIANG | Legacy | 124 | 585 | 99 | 16 |
| FIRE | National | 58 | 274 | 134 | 0 |
| FIRE | Legacy | 13 | 274 | 83 | 0 |
| Nedap | National | 0 | 0 | 3 | 0 |
| Nedap | Legacy | 0 | 4 | 3 | 0 |

**评估**:

| 维度 | 评分 | 说明 |
|------|------|------|
| 唯一性 | **无** | National 的 Kalman + Loess 完全覆盖 na.approx 的能力 |
| 互补性 | **无** | 两者解决同一问题（缺失值填充），方法不同但不互补 |
| 计算成本 | **更低** | na.approx 比 Kalman + Loess 简单得多 |
| 风险 | **中** | (1) 线性插值在非线性趋势上精度低；(2) rule=2 端点外推可能不合理（最后观测值无限延伸）；(3) 无 FCR 验证保护 |
| 数据证据 | **劣于 National** | Legacy 产生 16 个 FCR_NA（YANGXIANG），National 为 0 |

**决策: 不推荐整合。** 理由如下:

1. **National 的插补方法严格优于 na.approx**。Kalman Filter 能捕捉时间序列的动态趋势（StructTS 模型），Loess 能拟合非线性局部趋势，两者都比线性插值更准确。

2. **FCR 阶段验证是 National 独有的质量保护**。National 在采食量插补后检查 FCR 是否在合理范围内（Table 2: 按体重阶段的 FCR 上下界），能防止插补产生不合理的极端 FCR 值。Legacy 没有此保护。

3. **物理边界保护更安全**。National 的体重插补有物理边界（下限 90% min，上限 max + 20kg），防止 Kalman Filter 在端点外推时产生不合理的体重值。Legacy 的 rule=2 只是简单延伸最后观测值。

4. **na.approx 的唯一优势是简单性**，但这不构成整合理由。National 的复杂性是为了插补精度和安全性，这些是有价值的。

---

## 3. 决策汇总

| 策略 | 决策 | 核心理由 |
|------|------|----------|
| SD 阈值体重检测 | **不整合** | National RLM 完全覆盖，实测仅 4 条独有标记 |
| 多项式 RLM 体重检测 | **不整合** | National 两轮 RLM 架构更优，单轮 RLM 是其子集 |
| 中位数日聚合 | **不整合** | 加权平均利用 RLM 权重信息，统计上优于中位数 |
| 百分位数采食量检测 | **不整合** | STL 已提供更优的时间序列异常检测；固定标记率不可取 |
| 简单线性插值 | **不整合** | Kalman + Loess + FCR 验证严格优于 na.approx |

**结论: 5 项 Legacy 剩余独有策略均不推荐整合到 National Standard。**

---

## 4. Legacy 路径弃用计划

### 4.1 弃用理由

基于本评估和之前的整合决策报告，Legacy 路径在所有维度上均不如 National Standard:

| 维度 | National Standard | Legacy |
|------|-------------------|--------|
| 动物保留率 | 118 / 134 / 3 | 99 / 83 / 3 |
| FCR 稳定性 (SD) | 0.325 / 0.433 / 0.197 | 0.519 / 0.533 / 0.179 |
| FCR NA | 0 / 0 / 0 | 16 / 0 / 0 |
| LMM 校正 | 有效 (25~2274 条) | 无效 (0 条) |
| 插补质量 | Kalman + Loess + FCR 验证 | na.approx 线性插值 |
| 体重 QC 架构 | 两轮 RLM + 加权平均 + 可选 Gompertz | SD + 单轮 RLM + Gompertz (过度过滤) |
| 采食量 QC 架构 | 9 种物理标记 + 可选 STL | 百分位数 + STL (标记过多) |

Legacy 的每一项独有策略都已被 National 的对应实现覆盖或超越。保留 Legacy 路径只会增加代码维护负担（~500 行 Legacy 专用代码）和用户选择困惑。

### 4.2 弃用时间线

| 阶段 | 时间 | 行动 |
|------|------|------|
| V0.2.7 | 下一版本 | 在 `ZhenM_default_config("legacy")` 中添加弃用警告；文档中标注 Legacy 为 "deprecated" |
| V0.3.0 | 后续版本 | 将 `qc_method = "legacy"` 路径标记为 `lifecycle::deprecated()`；保留代码但不再维护 |
| V1.0.0 | 未来大版本 | 移除 Legacy 路径代码（~500 行），仅保留 National Standard |

### 4.3 弃用前需完成的清理

| 编号 | 行动 | 优先级 |
|------|------|--------|
| D-1 | 确认无外部用户依赖 `qc_method = "legacy"` | P0 |
| D-2 | 在 `ZhenM_default_config("legacy")` 返回值中添加 `deprecated = TRUE` 属性 | P1 |
| D-3 | 更新用户手册，移除 Legacy 相关文档 | P1 |
| D-4 | 清理 `.init_qc_flags()` 中的 Legacy 分支（审计报告 C-2/L-1） | P2 |
| D-5 | 移除 `ZhenM_merge_config()` 中对 Legacy 配置的特殊处理 | P2 |

---

## 5. 最终推荐配置

### 唯一推荐的 QC 方法配置

```r
# 默认配置（所有用户）
config <- ZhenM_default_config("national_standard")
# qc_method = "national_standard"
# use_stl_feed = FALSE
# use_gompertz = FALSE

# 增强配置（长期试验、大数据集）
config <- ZhenM_merge_config(
  base = ZhenM_default_config("national_standard"),
  override = list(
    national_standard = list(
      use_stl_feed = TRUE,
      use_gompertz = TRUE
    )
  )
)
```

**不再提供 `qc_method = "legacy"` 作为推荐选项。**

---

## 附录: 评估依据汇总

### A. 源码分析覆盖的文件

| 文件 | 分析内容 |
|------|---------|
| `R/zhenm_qc_weight_standard.R` | SD 阈值 (L387-394), RLM (L400-453), Gompertz (L456-509) |
| `R/zhenm_qc_feed_standard.R` | 百分位数 (L232-258), STL (L263-311) |
| `R/zhenm_daily_aggregate_filtered.R` | 中位数聚合 (L159-165), 加权平均 (L150-158), LMM 校正 (L218-362) |
| `R/zhenm_impute.R` | Legacy 体重插补 (L121-138), National 体重插补 (L60-117) |
| `R/zhenm_impute_feed.R` | Legacy 采食量插补 (L1-26) |
| `R/zhenm_impute_national.R` | National 采食量插补 (L11-70) |
| `R/zhenm_config_defaults.R` | Legacy 配置 (L82-103), National 配置 (L26-80) |
| `R/zhenm_qc_utils.R` | RLM 拟合 (L61-74), QC 标志初始化 (L116-136) |

### B. 数据源信息

| 数据源 | 记录数 | 动物数 | 设备类型 |
|--------|--------|--------|---------|
| YANGXIANG | 383,666 | 200 | 扬翔群喂仪 |
| FIRE | 213,176 | 201 | FIRE 采食站 |
| Nedap | 55,568 | 45 | Nedap 采食站 |

### C. 一致性数据 (agreement_summary.csv)

| 数据源 | 标记类型 | Kappa | 一致率 |
|--------|---------|-------|--------|
| YANGXIANG | is_outlier_wt | 0.847 | 97.9% |
| YANGXIANG | is_outlier_feed | 0.031 | 54.1% |
| FIRE | is_outlier_wt | 0.375 | 86.4% |
| FIRE | is_outlier_feed | 0.041 | 78.4% |
| Nedap | is_outlier_wt | -0.032 | 88.2% |
| Nedap | is_outlier_feed | -0.008 | 84.1% |
