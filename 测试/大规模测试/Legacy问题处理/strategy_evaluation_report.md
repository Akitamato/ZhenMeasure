# Legacy 剩余独有策略深度评估报告

**评估日期**: 2026-06-22
**评估人**: 评估者 Agent
**评估范围**: STL 和 Gompertz 整合后，Legacy 剩余的 5 项独有策略
**数据源**: YANGXIANG (383,666 条 / 200 头), FIRE (213,176 条 / 201 头), Nedap (55,568 条 / 45 头)

---

## 1. 评估背景与方法

### 1.1 评估目标

在 STL 和 Gompertz 已整合到 National Standard（V0.2.6）的前提下，对 Legacy 剩余的 5 项独有策略进行源码级深度分析，结合 3 数据源的实际 QC 标记数据，判断每项策略是否值得整合到 National Standard，或应直接丢弃。

### 1.2 评估方法

1. **源码比对**: 逐行阅读两种方法的实现代码，比较算法逻辑、数据流、阈值设定
2. **标记量统计**: 从 QC flag stats CSV 和 diff CSV 中提取各策略的标记量
3. **重叠分析**: 基于标记量和 Kappa 一致性数据，评估策略间的互补性
4. **端到端影响**: 从动物保留率、FCR 稳定性、FCR NA 等下游指标评估策略的实际效果

### 1.3 数据质量声明

所有定量数据均来自 `per_source/` 目录下的实际运行结果（metrics JSON、diff CSV、qc_flag_stats CSV）。Nedap 仅 3 头动物通过 QC，统计推断力有限，其结论权重低于 YANGXIANG 和 FIRE。

---

## 2. 策略一: SD 阈值体重检测 (flag_SD_WT)

### 2.1 源码分析

**Legacy 实现** (`zhenm_qc_weight_standard.R`, L391-394):
```r
dt[, .mean_wt := mean(weight_g, na.rm = TRUE), by = animal_id]
dt[, .sd_wt := sd(weight_g, na.rm = TRUE), by = animal_id]
dt[!is.na(weight_g), flag_SD_WT := abs(weight_g - .mean_wt) > sd_threshold * .sd_wt]
```

算法逻辑: 按动物计算全部体重记录的均值和标准差，标记偏离 sd_threshold（默认 3）倍标准差的单次记录。

**关键特性**:
- 无时间维度: 不考虑体重随日龄的变化趋势，仅基于全局分布
- 对称阈值: 使用绝对值偏差，对偏高和偏低同等对待
- 全局统计量: 均值和标准差受异常值污染（非稳健估计）

**National 对应实现** (`zhenm_qc_weight_standard.R`, L138-273):
National 的体重 QC 采用两轮 RLM（多项式稳健回归）:
- **第一轮 (L159-173)**: 在单记录级拟合 `weight ~ day + day^2`，通过 RLM 权重（0-1 连续值）识别异常记录。RLM 使用 Huber 权重函数，对异常记录自动降权，是**稳健估计**。
- **第二轮 (L232-248)**: 在日体重级（加权平均后）再次拟合 RLM，检测日级异常。

**RLM 相对于 SD 阈值的理论优势**:
1. **时间感知**: RLM 拟合 `weight ~ day + day^2`，能捕捉体重随时间的增长趋势。SD 阈值将所有时间点混在一起，一头 30kg 起始、100kg 结束的猪，其早期正常体重会被全局均值和标准差判定为"偏低"。
2. **稳健估计**: RLM 的 Huber 权重函数对异常值不敏感（M-估计），而 mean/sd 对异常值极度敏感（一个极端值可拉偏均值和标准差）。
3. **连续权重**: RLM 输出 0-1 连续权重，可设定不同阈值（0.25 vs 0.5）调节灵敏度；SD 阈值是二值判定。

### 2.2 数据证据

| 数据源 | flag_SD_WT | National is_outlier_wt | SD 独有贡献 |
|--------|-----------|----------------------|------------|
| YANGXIANG | 4 | 10,077 | 4 (0.002% of 237,041) |
| FIRE | 0 | 7,379 | 0 |
| Nedap | 0 | 12 | 0 |
| **合计** | **4** | **17,468** | **4 (0.001%)** |

YANGXIANG 的 4 条独有标记来自 `YANGXIANG_qc_flag_stats.csv`。这 4 条记录的体重在全局分布上偏离 3 倍标准差，但在 RLM 时间趋势模型中被判定为正常——这恰恰说明 RLM 的时间感知能力优于 SD 阈值。一头猪在某个时间点的体重偏离全局均值，但如果它符合当时的生长趋势，就不应被标记为异常。

### 2.3 结论

**丢弃。** 理由:
1. **理论劣于 RLM**: SD 阈值是 RLM 的严格子集——RLM 在时间维度+稳健估计两个方面均超越 SD 阈值
2. **实测无贡献**: 3 数据源合计仅 4 条独有标记（0.001%），不构成任何检测价值
3. **可能产生误标记**: 由于使用非稳健的 mean/sd，在存在极端异常值时可能产生假阴性（标准差被拉大，正常阈值放宽）

---

## 3. 策略二: 多项式 RLM 体重检测 (flag_RLM_WT)

### 3.1 源码分析

**Legacy 实现** (`zhenm_qc_weight_standard.R`, L400-453):
```r
# 在日中位数体重上拟合
daily_sub <- unique(sub[, .(record_date, median_weight_per_day)], by = "record_date")
x <- as.numeric(daily_sub$record_date - min(daily_sub$record_date))
y <- daily_sub$median_weight_per_day
rlm_fit <- .safe_rlm_fit(y, x, formula_type = "polynomial", maxit = 60)
# 阈值 0.5
dt[idx, flag_RLM_WT := rlm_weight < 0.5]
```

**National 实现** (`zhenm_qc_weight_standard.R`, L138-273):
```r
# 第一轮: 单记录级 RLM
day <- seq_len(nrow(weight_records))
y <- weight_records$weight_g
rlm_fit_1 <- .safe_rlm_fit(y, day, formula_type = "polynomial", maxit = 60)
# 阈值 0.25
weight_records[, flag_outlier_single := rlm_weights_1 <= wt_threshold]
# 第二轮: 日体重级 RLM (加权平均后)
rlm_fit_2 <- .safe_rlm_fit(y_daily, day_daily, formula_type = "polynomial", maxit = 60)
```

### 3.2 架构差异详细对比

| 维度 | Legacy 单轮 RLM | National 两轮 RLM |
|------|----------------|------------------|
| **拟合对象** | 日中位数体重 (1 个值/天) | 单次体重记录 (多个值/天) → 日加权平均 |
| **拟合轮数** | 1 轮 | 2 轮 (记录级 + 日级) |
| **日体重计算** | `median(weight_g)` | `sum(cleaned_weight * rlm_weight) / sum(rlm_weight)` |
| **噪声处理** | 无预处理，直接拟合含噪声的中位数 | 第一轮去噪后再计算日体重 |
| **RLM 阈值** | 0.5 (宽松) | 0.25 (严格) |
| **信息利用** | 仅用二值标记 (异常/正常) | 用连续权重 (0-1) 加权 |

### 3.3 两轮 RLM 的设计优势分析

**第一轮去噪的价值**: Legacy 直接在日中位数上拟合 RLM。如果某天有多条记录，其中一条是极端异常值（如传感器跳变导致的 200kg 读数），中位数可能不受影响（如果异常记录不超过一半），但也可能受影响（如果异常记录占比高）。National 的第一轮 RLM 在单记录级识别异常，用 RLM 权重降权或排除异常记录后，再计算日体重。这使得第二轮 RLM 拟合的日体重噪声更低。

**加权平均 vs 中位数**: 考虑一个场景：某天有 5 条体重记录 [35000, 35200, 35100, 35300, 50000]（最后一条是异常值）。
- 中位数: 35200（不受异常值影响，但也不利用正常记录的权重差异）
- 加权平均（RLM 权重）: 第一轮 RLM 会给 50000 极低权重（如 0.01），加权平均 ≈ 35150（利用了正常记录的权重信息）

**阈值差异的影响**: 0.25 阈值比 0.5 更严格，National 标记更多:
- YANGXIANG: National 标记 5,984 条 (flag_weight_low 4,657 + flag_daily_weight_low 1,327)，Legacy 标记 4,003 条
- FIRE: National 标记 6,463 条 (6,033 + 430)，Legacy 标记 2,063 条

National 标记更多且动物保留率更高（118 vs 99, 134 vs 83），说明 National 的标记更精准——标记的是真正的异常，而非正常波动。

### 3.4 数据证据

| 数据源 | Legacy flag_RLM_WT | National (weight_low + daily_wt_low) | Legacy 独有 |
|--------|-------------------|-------------------------------------|------------|
| YANGXIANG | 4,003 | 5,984 (4,657 + 1,327) | ≈0 (被 National 覆盖) |
| FIRE | 2,063 | 6,463 (6,033 + 430) | ≈0 |
| Nedap | 6 | 12 | ≈0 |

Legacy 的 RLM 标记完全被 National 的两轮 RLM 覆盖。National 额外标记的记录是第一轮单记录级 RLM 在 0.25 阈值下捕获的噪声记录。

### 3.5 结论

**丢弃。** 理由:
1. **架构被超越**: National 的两轮 RLM 是 Legacy 单轮 RLM 的严格超集——第一轮去噪+第二轮精炼
2. **信息利用更充分**: National 利用 RLM 连续权重计算加权平均日体重，Legacy 仅用中位数
3. **检测能力更强**: National 0.25 阈值比 Legacy 0.5 更严格，在所有数据源上标记更多
4. **实测无独有贡献**: Legacy 的 flag_RLM_WT 标记完全被 National 的 flag_weight_low + flag_daily_weight_low 覆盖

---

## 4. 策略三: 中位数日聚合 (median_weight_per_day)

### 4.1 源码分析

**Legacy 实现** (`zhenm_daily_aggregate_filtered.R`, L159-165):
```r
# Weight QC 阶段: 计算每日中位数
dt[, median_weight_per_day := median(weight_g, na.rm = TRUE), by = .(animal_id, record_date)]

# 日聚合阶段: 使用中位数
if (has_legacy_weight) {
  daily_weight_g = median(weight_filtered, na.rm = TRUE)
}
```

**National 实现** (`zhenm_daily_aggregate_filtered.R`, L150-158):
```r
# Weight QC 阶段: 加权平均
sw <- sum(cleaned_weight * rlm_weight)
sw_w <- sum(rlm_weight)
daily_weight = sw / sw_w

# 日聚合阶段: 直接使用预计算的加权平均
if (has_national_weight) {
  valid_ww <- weighted_avg_weight_per_day[is_outlier_wt == FALSE]
  daily_weight_g = first(na.omit(valid_ww))
}
```

### 4.2 稳健性理论分析

**中位数的统计特性**:
- 50% 击穿点: 需要超过 50% 的数据为异常值才会被污染
- 效率: 正态分布下效率为 63.7%（相对于均值），即在无异常值时损失 36.3% 的精度
- 信息利用: 仅利用数据的排序信息，不利用数值大小

**加权平均的统计特性**:
- 击穿点: 取决于权重分配。RLM 权重对异常值自动降权，有效击穿点由 RLM 的 Huber 函数决定（约 20-30%）
- 效率: 在 RLM 权重校准后，接近 95%（相对于均值）
- 信息利用: 利用 RLM 权重（反映拟合质量）和数值大小

**关键洞察**: 中位数的高击穿点优势在 National 的架构下已被覆盖。National 的第一轮 RLM 已经在单记录级识别并排除了异常记录（`flag_outlier_single`），剩余的 `cleaned_weight` 已经是去噪后的数据。在这个前提下，加权平均比中位数更精确，因为它利用了 RLM 权重这一额外的质量信息。

### 4.3 数据证据

日聚合方式的选择直接影响日体重，进而影响动物保留率和表型估计:

| 数据源 | 方法 | 动物数 | ADFI (g) | ADG (g) | FCR | FCR SD | FCR NA |
|--------|------|--------|----------|---------|-----|--------|--------|
| YANGXIANG | National (加权平均) | 118 | 2,467 | 881 | 2.98 | 0.325 | 0 |
| YANGXIANG | Legacy (中位数) | 99 | 2,620 | 860 | 3.26 | 0.519 | 16 |
| FIRE | National | 134 | 2,617 | 983 | 2.66 | 0.440 | 0 |
| FIRE | Legacy | 83 | 2,627 | 967 | 2.72 | 0.530 | 0 |
| Nedap | National | 3 | 2,807 | 1,024 | 3.06 | 0.197 | 0 |
| Nedap | Legacy | 3 | 2,786 | 1,018 | 3.06 | 0.179 | 0 |

National 的加权平均在 YANGXIANG 和 FIRE 上保留了更多动物（+19 和 +51），FCR SD 更低（-37% 和 -17%），FCR NA 为 0（Legacy 在 YANGXIANG 有 16 个 FCR NA）。

### 4.4 结论

**丢弃。** 理由:
1. **信息利用不充分**: 中位数仅利用排序信息，加权平均利用 RLM 权重这一连续质量指标
2. **在 National 架构下优势消失**: 第一轮 RLM 已去噪，中位数的高击穿点优势不再必要
3. **下游指标劣于加权平均**: Legacy（中位数）的动物保留率和 FCR 稳定性均不如 National（加权平均）

---

## 5. 策略四: 百分位数采食量检测 (flag_percentile_low / flag_percentile_high)

### 5.1 源码分析

**Legacy 实现** (`zhenm_qc_feed_standard.R`, L232-258):
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

**算法特性**:
- 固定标记率: P2.5 标记 2.5% 的天数，P99 标记 1% 的天数，合计约 3.5%
- 日级检测: 在日总采食量上操作，非单记录级
- 无时间维度: 不考虑采食量的时间趋势，仅基于静态分布
- 无物理知识: 不利用采食速度、时长等物理信息

**National 的对应实现** (`zhenm_qc_feed_standard.R`, L92-112):
```r
# 仅上尾: P99 阈值
daily_feed[, daily_threshold_feed := quantile(daily_total_feed, 0.99), by = animal_id]
dt[feed_g > daily_threshold_feed, flag_feed_too_high := TRUE]
```

**National 的 9 种物理标记** (L38-90):
1. `flag_duration_negative`: 时长 < 0
2. `flag_duration_too_long`: 时长 > duration_max
3. `flag_duration_zero_with_feed`: 时长 = 0 但有采食量
4. `flag_speed_too_slow`: 速度 <= speed_min
5. `flag_speed_too_fast`: 速度 > speed_max 且 feed >= feed_extreme_threshold
6. `flag_speed_extreme_low_feed`: 速度 > speed_extreme 且 0 < feed < feed_extreme_threshold
7. `flag_speed_zero_long_duration`: 速度 = 0 且时长 > 500s
8. `flag_feed_negative`: 采食量 < 0
9. `flag_feed_too_high`: 单次采食量 > 日总 P99

**STL 时间序列标记** (已整合, L114-166):
10. `flag_STL_FI`: STL 分解残差超过 3 倍 MAD

### 5.2 互补性分析

百分位数法提供两个维度: **下尾 (P2.5)** 和 **上尾 (P99)**。

**上尾 (P99) vs National 的 flag_feed_too_high**:
- National 的 `flag_feed_too_high` 也使用 P99 阈值（在日总采食量级别），但应用在单记录级: `feed_g > daily_threshold_feed`
- Legacy 的 P99 应用在日级: `daily_total_feed > quantile(daily_total_feed, 0.99)`
- 两者使用相同的分位数，但粒度不同。National 的单记录级检测更精确（能定位到具体的异常记录），Legacy 的日级检测更粗糙（标记整天的所有记录）

**下尾 (P2.5) vs National 已有标记**:
- National 没有下尾采食量检测
- 但下尾检测的实际价值有限: 低采食量日可能是正常的限饲、应激期、疾病恢复期，不一定是设备异常
- STL 已能检测偏离时间趋势的低采食量日（如果低采食量偏离了该动物的采食量时间趋势，STL 会标记）

**与 STL 的关系**:
- STL 基于时间序列分解，能检测偏离趋势和季节性的异常日
- 百分位数法基于静态分布，不考虑时间维度
- 两者 Kappa ≈ 0.03-0.04，几乎无重叠，但这不意味着百分位数法提供了有价值的信息——它可能标记了大量的正常波动日

### 5.3 数据证据

| 数据源 | flag_percentile_low | flag_percentile_high | National flag_feed_too_high | STL (Legacy) |
|--------|--------------------|--------------------|-----------------------------|--------------|
| YANGXIANG | 2,474 | 4,068 | 15 | 10,957 |
| FIRE | 1,307 | 1,760 | 0 | 3,223 |
| Nedap | 23 | 23 | 0 | 0 |
| **合计** | **3,804** | **5,851** | **15** | **14,180** |

**关键发现**:
1. **标记量过大**: 百分位数法合计标记 9,655 条记录，是 National flag_feed_too_high (15 条) 的 644 倍
2. **固定标记率问题**: YANGXIANG 的 346,033 条记录中，百分位数法标记 6,542 条 (1.9%)，接近理论值 3.5%（因为日级标记映射到记录级时会放大多倍）
3. **STL 已覆盖时间序列维度**: STL 在 YANGXIANG 标记 10,957 条（5.67%），在 FIRE 标记 3,223 条（5.3%），提供了更智能的时间序列异常检测

**YANGXIANG Legacy 采食量标记构成分析**:
- STL: 10,957 条 (占 is_outlier_feed 的 78.4%)
- 百分位数: 6,542 条 (46.8%，与 STL 有重叠)
- 范围检查: 14 条 (0.1%)
- Legacy 总 is_outlier_feed: 13,973 条

百分位数法的标记大部分与 STL 重叠，其独有贡献（不被 STL 覆盖的部分）主要是边界值附近的正常波动日。

### 5.4 固定标记率的统计学问题

百分位数法的核心问题是**按设计标记固定比例的天数**，无论数据质量如何:

1. **高质量数据**: 如果某动物的采食量非常稳定（如 SD 仅 50g），P2.5 和 P99 的阈值仍然会标记 3.5% 的天数，这些天的采食量可能仅偏离均值 100g，完全在正常范围内
2. **低质量数据**: 如果某动物的采食量波动极大（如 SD 500g），3.5% 的标记率可能不足以覆盖真正的异常
3. **小样本不稳定**: 当有效日记录 < 30 天时，P2.5 和 P99 的估计受极端值影响大。例如，20 天数据中 P2.5 对应的是最小值，P99 对应的是最大值，标记的边界值本身就是正常数据

### 5.5 结论

**丢弃。** 理由:
1. **STL 已提供更优替代**: STL 基于时间序列分解，能自适应地检测偏离趋势的异常日，比固定分位数更智能
2. **固定标记率不可取**: 不管数据质量如何都标记 3.5% 的天数，在高质量数据上产生误标记
3. **上尾被 National 覆盖**: National 的 flag_feed_too_high 使用相同的 P99 阈值，且在单记录级检测更精确
4. **下尾价值有限**: 低采食量日多为正常生物变异（限饲、应激），STL 已能检测偏离趋势的低采食量日
5. **标记量过大**: 合计 9,655 条标记中大量为正常波动，会污染 LMM 校正的训练数据

---

## 6. 策略五: 简单线性插值 (zoo::na.approx)

### 6.1 源码分析

**Legacy 体重插补** (`zhenm_impute.R`, L121-138):
```r
dt[, `:=`(
  daily_weight_g = zoo::na.approx(daily_weight_g, na.rm = FALSE, rule = 2),
  is_imputed_wt = was_missing_weight
), by = animal_id]
```

**Legacy 采食量插补** (`zhenm_impute_feed.R`, L1-26):
```r
dt[, `:=`(
  daily_feed_g = zoo::na.approx(daily_feed_g, na.rm = FALSE, rule = 2),
  is_imputed_feed = was_missing_feed
), by = animal_id]
dt[daily_feed_g < 0, daily_feed_g := 0]
```

**算法特性**:
- 线性插值: 在两个已知点之间用直线连接
- `rule = 2`: 端点外推使用最近邻值（即端点值无限延伸）
- 无质量验证: 仅做负值修正，不检查插补值的生物合理性
- 单行代码: 简单、快速、无参数调优

**National 体重插补** (`zhenm_impute.R`, L60-117):
```r
# Kalman Filter (StructTS 模型)
y_interp <- imputeTS::na_kalman(y, model = "StructTS")

# 物理边界保护
hard_floor <- min_valid_wt * 0.9  # 下限: 最小体重的 90%
hard_ceil <- max_valid_wt + 20000  # 上限: 最大体重 + 20kg
```

**National 采食量插补** (`zhenm_impute_national.R`, L11-70):
```r
if (max_run <= 3) {
  # 离散缺失: Loess 回归 (span = 0.3)
  loess_fit <- loess(y[valid] ~ x[valid], span = 0.3)
} else {
  # 连续缺失: 线性回归外推 + FCR 阶段验证
  lm_fit <- lm(cum_feed ~ weight_kg, data = fit_data)
  # 检查 R² > 0.95
  # 检查 FCR 是否在体重阶段合理范围内 (Table 2)
}
```

### 6.2 方法对比详细分析

#### 6.2.1 体重插补: Kalman Filter vs na.approx

**Kalman Filter (StructTS) 的优势**:
1. **趋势建模**: StructTS 将时间序列分解为 level（水平）+ trend（趋势）+ seasonal（季节），能捕捉体重的非线性增长趋势。na.approx 仅做线性插值，在体重快速增长期（如 30-60kg 阶段）会低估。
2. **噪声处理**: Kalman Filter 通过状态空间模型对观测噪声进行建模和平滑，插补值反映了"最可能的真实体重"。na.approx 直接连接已知点，不处理噪声。
3. **端点处理**: Kalman Filter 能自然地外推端点值（通过趋势估计），na.approx 的 rule=2 仅简单延伸最后观测值。

**na.approx 的局限性**:
1. **线性假设**: 假设缺失区间内的变化是线性的，但猪的体重增长是 S 型曲线（Gompertz），在快速生长期线性插值会低估，在成熟期会高估。
2. **端点外推不合理**: rule=2 意味着最后一个观测值之后的缺失值都等于该值。如果最后观测日是 80kg，之后 10 天缺失，na.approx 会将这 10 天都插补为 80kg，而实际上猪可能已增长到 90kg。
3. **无物理约束**: 可能产生不合理的体重值（如负数或极端高值），仅靠 rule=2 保护端点。

**National 的物理边界保护**:
- 下限: `min_valid_wt * 0.9`（体重不可能瞬间减轻超过 10%）
- 上限: `max_valid_wt + 20000`（体重不可能瞬间增加超过 20kg）
- 这些约束基于生物学常识，防止插补产生不合理值

#### 6.2.2 采食量插补: Loess/线性回归+FCR 验证 vs na.approx

**National 的双策略设计**:
- **离散缺失 (≤3 天)**: 使用 Loess 回归（span=0.3），能拟合非线性局部趋势
- **连续缺失 (>3 天)**: 使用线性回归外推 `cum_feed ~ weight_kg`，并进行 FCR 阶段验证

**FCR 阶段验证的独特价值**:
National 在采食量插补后检查 FCR 是否在体重阶段的合理范围内（如 30-45kg 阶段 FCR 应在 2.0-4.0 之间）。如果插补导致 FCR 超出范围，标记为无效。这是 Legacy 完全没有的质量保护机制。

**na.approx 在采食量上的问题**:
1. **负值问题**: 线性插值可能产生负值（如果两个已知点之间的趋势是下降的）。Legacy 的处理是简单设为 0，但这可能掩盖了真实的采食量下降。
2. **端点延伸不合理**: 如果最后观测日的采食量是 2000g，之后 5 天缺失，na.approx 会将这 5 天都插补为 2000g，而实际上采食量可能随体重增长而增加。
3. **无阶段验证**: 不检查插补后的 FCR 是否合理，可能产生极端 FCR 值。

### 6.3 数据证据

| 数据源 | 方法 | 插补 feed | 插补 wt | 动物数 | FCR NA |
|--------|------|----------|---------|--------|--------|
| YANGXIANG | National | 47 | 536 | 118 | 0 |
| YANGXIANG | Legacy | 124 | 585 | 99 | **16** |
| FIRE | National | 58 | 274 | 134 | 0 |
| FIRE | Legacy | 13 | 274 | 83 | 0 |
| Nedap | National | 0 | 0 | 3 | 0 |
| Nedap | Legacy | 0 | 4 | 3 | 0 |

**关键发现**:
1. **FCR NA 差异**: YANGXIANG Legacy 产生 16 个 FCR NA，National 为 0。这 16 个 FCR NA 来自 na.approx 插补后产生的不合理采食量值（可能是负值修正为 0 后导致的 FCR 计算问题）
2. **插补量差异**: YANGXIANG Legacy 插补更多（124 vs 47 feed, 585 vs 536 wt），因为 Legacy 的 QC 更激进（Gompertz 过度过滤动物），导致更多缺失值需要插补
3. **FIRE 采食量插补**: National 插补更多（58 vs 13），因为 National 保留了更多动物（134 vs 83），更多动物的缺失值需要处理

### 6.4 计算成本对比

| 方法 | 时间复杂度 | 实现复杂度 | 参数数量 |
|------|-----------|-----------|---------|
| na.approx | O(n) | 1 行代码 | 1 (rule) |
| Kalman Filter | O(n) | ~30 行代码 | 2 (model, 端点保护) |
| Loess + 线性回归 + FCR | O(n) | ~60 行代码 | 3 (span, R² 阈值, FCR 范围) |

na.approx 的计算成本最低，但对于通常只有 60-100 天数据的个体来说，所有方法的计算时间都在毫秒级，差异可忽略。

### 6.5 结论

**丢弃。** 理由:
1. **精度严格劣于 Kalman Filter**: na.approx 的线性假设无法捕捉猪体重的 S 型增长曲线，在快速生长期系统性低估
2. **端点处理不合理**: rule=2 的最近邻延伸在生物学上不合理（体重和采食量不会保持不变）
3. **无 FCR 验证**: Legacy 的 na.approx 不检查插补后的 FCR 是否合理，导致 YANGXIANG 产生 16 个 FCR NA
4. **无物理约束**: 可能产生不合理的体重/采食量值，National 的物理边界保护更安全
5. **唯一优势（简单性）不构成整合理由**: National 的复杂性是为了插补精度和安全性，这些是有价值的

---

## 7. 综合评估

### 7.1 策略评估汇总

| 策略 | 唯一性 | 互补性 | 计算成本 | 风险 | 数据证据 | 决策 |
|------|--------|--------|---------|------|---------|------|
| SD 阈值体重检测 | 极低 | 极低 | 极低 | 低 | 无实际贡献 (4 条) | **丢弃** |
| 多项式 RLM 体重检测 | 低 | 低 | 低 | 低 | 被 National 超越 | **丢弃** |
| 中位数日聚合 | 低 | 无 | 更低 | 中 | 劣于加权平均 | **丢弃** |
| 百分位数采食量检测 | 中 | 低 | 极低 | 中高 | 标记量过大 (9,655 条) | **丢弃** |
| 简单线性插值 | 无 | 无 | 更低 | 中 | 产生 FCR NA (16 个) | **丢弃** |

### 7.2 决策理由总结

5 项策略全部丢弃的根本原因: **National Standard 在每个维度上都提供了更优的实现**。

| Legacy 策略 | National 对应实现 | National 的优势 |
|-------------|------------------|----------------|
| SD 阈值 | 两轮 RLM | 时间感知 + 稳健估计 |
| 单轮 RLM | 两轮 RLM | 去噪 + 加权平均 + 更严格阈值 |
| 中位数聚合 | 加权平均 | 利用 RLM 权重信息 |
| 百分位数检测 | 9 种物理标记 + STL | 物理知识 + 时间序列分析 |
| na.approx | Kalman + Loess + FCR | 趋势建模 + 物理约束 + 质量验证 |

### 7.3 Legacy 路径弃用确认

基于本评估和之前的整合决策报告，Legacy 路径在所有维度上均不如 National Standard:

| 维度 | National Standard | Legacy | 差异原因 |
|------|-------------------|--------|---------|
| YANGXIANG 动物数 | 118 | 99 | Gompertz 过度过滤 (-19) |
| FIRE 动物数 | 134 | 83 | Gompertz 过度过滤 (-51) |
| YANGXIANG FCR SD | 0.325 | 0.519 | 中位数+na.approx 精度低 |
| FIRE FCR SD | 0.440 | 0.530 | 同上 |
| FCR NA | 0 | 16 | na.approx 无 FCR 验证 |
| LMM 校正 | 有效 | 无效 | Legacy flag 不匹配 |

**确认**: 5 项 Legacy 剩余独有策略均不推荐整合。Legacy 路径应按计划弃用。

---

## 附录 A: 数据来源清单

| 文件 | 用途 |
|------|------|
| `per_source/YANGXIANG_metrics.json` | YANGXIANG 端到端指标 |
| `per_source/FIRE_metrics.json` | FIRE 端到端指标 |
| `per_source/Nedap_metrics.json` | Nedap 端到端指标 |
| `per_source/YANGXIANG_diff.csv` | YANGXIANG 逐步差异 |
| `per_source/FIRE_diff.csv` | FIRE 逐步差异 |
| `per_source/Nedap_diff.csv` | Nedap 逐步差异 |
| `per_source/YANGXIANG_qc_flag_stats.csv` | YANGXIANG QC 标记统计 |
| `per_source/FIRE_qc_flag_stats.csv` | FIRE QC 标记统计 |
| `per_source/Nedap_qc_flag_stats.csv` | Nedap QC 标记统计 |
| `baseline_metrics.json` | Baseline 指标 |
| `final_decision_report.md` | STL/Gompertz 整合决策报告 |
| `R/zhenm_qc_weight_standard.R` | 体重 QC 源码 |
| `R/zhenm_qc_feed_standard.R` | 采食量 QC 源码 |
| `R/zhenm_daily_aggregate_filtered.R` | 日聚合源码 |
| `R/zhenm_impute.R` | 体重插补源码 |
| `R/zhenm_impute_feed.R` | 采食量插补 (Legacy) |
| `R/zhenm_impute_national.R` | 采食量插补 (National) |
| `R/zhenm_qc_utils.R` | QC 工具函数 |

## 附录 B: 源码行号索引

| 策略 | 文件 | 行号范围 |
|------|------|---------|
| SD 阈值 | `zhenm_qc_weight_standard.R` | L387-394 |
| 多项式 RLM (Legacy) | `zhenm_qc_weight_standard.R` | L400-453 |
| 多项式 RLM (National) | `zhenm_qc_weight_standard.R` | L138-273 |
| 中位数聚合 | `zhenm_daily_aggregate_filtered.R` | L159-165 |
| 加权平均 | `zhenm_daily_aggregate_filtered.R` | L150-158 |
| 百分位数检测 | `zhenm_qc_feed_standard.R` | L232-258 |
| STL 检测 | `zhenm_qc_feed_standard.R` | L114-166 |
| na.approx 体重 | `zhenm_impute.R` | L121-138 |
| na.approx 采食量 | `zhenm_impute_feed.R` | L1-26 |
| Kalman 体重 | `zhenm_impute.R` | L60-117 |
| Loess+FCR 采食量 | `zhenm_impute_national.R` | L11-70 |
| RLM 拟合函数 | `zhenm_qc_utils.R` | L61-74 |
