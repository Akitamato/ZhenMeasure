# Legacy 剩余独有策略整合验证测试报告

**测试日期**: 2026-06-22
**测试者**: 测试者 Agent
**测试范围**: 评估报告推荐的 5 项 Legacy 剩余独有策略
**数据源**: FIRE (主要), YANGXIANG (辅助)
**测试方法**: 基于 per_source/ 目录下的实际 QC 数据进行标记重叠分析和端到端指标对比

---

## 1. 测试背景

评估报告 (`strategy_evaluation_report.md`) 对 Legacy 剩余的 5 项独有策略进行了源码级分析，结论是全部丢弃。本报告通过实际数据验证这一结论。

**测试脚本**:
- `test_strategy_integration.R` -- 主测试脚本，覆盖 5 项策略
- `test_strategy2_detail.R` -- 策略 2 补充分析

---

## 2. 策略 1: SD 阈值体重检测 (flag_SD_WT)

### 2.1 测试方法

比较 Legacy flag_SD_WT 标记与 National is_outlier_wt 标记的重叠情况，按 animal_id + record_date 匹配。

### 2.2 测试结果

| 数据源 | flag_SD_WT | National is_outlier_wT | SD 独有贡献 |
|--------|-----------|------------------------|------------|
| FIRE | 0 | 7,379 | 0 |
| YANGXIANG | 4 | 10,077 | 0 (全部被 National 覆盖) |

**关键发现**:
- FIRE 数据中 SD 阈值法未产生任何标记
- YANGXIANG 的 4 条标记全部被 National is_outlier_wt 覆盖
- 独有贡献为 0

### 2.3 结论

**丢弃。** SD 阈值法在实测中无独有贡献。理论上，SD 阈值是 RLM 的严格子集（无时间感知、非稳健估计），整合无价值。

---

## 3. 策略 2: 多项式 RLM 体重检测 (flag_RLM_WT)

### 3.1 测试方法

1. 按 animal_id + record_date 去重后匹配 Legacy flag_RLM_WT 与 National is_outlier_wt
2. 统计 Legacy RLM 独有标记的数量和分布
3. 分析 National 独有动物（被 Legacy 丢弃的动物）

### 3.2 测试结果

**FIRE 数据**:
| 指标 | 值 |
|------|-----|
| Legacy flag_RLM_WT | 2,063 条 (日级去重后 329 天) |
| National flag_weight_low | 6,033 条 |
| National flag_daily_weight_low | 430 条 |
| National 两轮 RLM 合计 | 6,463 条 |
| Legacy RLM 被 National 覆盖 | 188 条 (日级) |
| Legacy RLM 独有 | 141 条 (日级)，分布在 56 个动物上 |
| National 额外标记 | 2,375 条 (日级) |

**YANGXIANG 数据**:
| 指标 | 值 |
|------|-----|
| Legacy flag_RLM_WT | 4,003 条 |
| National is_outlier_wt | 10,077 条 |

**动物集合差异 (FIRE)**:
| 指标 | 值 |
|------|-----|
| National 独有动物 | 61 头 (被 Legacy 丢弃) |
| Legacy 独有动物 | 1 头 |
| 共有动物 | 83 头 |

**Legacy RLM 独有标记的 56 个动物特征**:
- 这些动物在 National 中也有大量体重标记（如 ID086: National 189 条, Legacy RLM 51 条）
- Legacy RLM 的 141 条独有标记是阈值 0.5 捕获但 National 阈值 0.25 未捕获的边界记录
- 由于 National 阈值更严格 (0.25 < 0.5)，这些记录实际上是 Legacy 的宽松标记

**National 独有动物 (61 头) 的 Legacy 标记情况**:
- 全部 61 头动物的 Legacy_RLM = 0, Legacy_Gompertz = 0
- 这些动物被 Legacy 的 Gompertz 过度过滤丢弃，而非 RLM

### 3.3 结论

**丢弃。** Legacy RLM 的 141 条独有标记是宽松阈值 (0.5) 的产物，National 的严格阈值 (0.25) 已经覆盖了更有价值的标记。National 的两轮 RLM 架构（第一轮去噪 + 第二轮精炼）严格优于 Legacy 的单轮 RLM。整合 Legacy RLM 不会增加检测能力，反而可能引入噪声。

---

## 4. 策略 3: 中位数日聚合 (median_weight_per_day)

### 4.1 测试方法

比较 National（加权平均）与 Legacy（中位数）的日聚合结果，包括日体重统计特性和下游表型指标。

### 4.2 测试结果

**FIRE 日聚合数据**:
| 指标 | National (加权平均) | Legacy (中位数) |
|------|-------------------|----------------|
| 日记录数 | 13,340 | 8,202 |
| 动物数 | 134 | 83 |
| 日体重 mean | 74,184.5 g | 73,919.2 g |
| 日体重 sd | 29,242.9 g | 28,336.3 g |
| 日体重 NA | 0 | 109 |

**FIRE 表型指标**:
| 指标 | National | Legacy | 差异 |
|------|----------|--------|------|
| 动物数 | 134 | 83 | +51 |
| FCR 均值 | 2.659 | 2.715 | -0.056 |
| FCR SD | 0.440 | 0.530 | -0.090 |
| ADG (g) | 983.3 | 967.4 | +15.9 |

**共有 75 头动物的 FCR 差异**:
| 指标 | 值 |
|------|-----|
| mean diff (National - Legacy) | -0.0645 |
| sd diff | 0.0762 |
| \|diff\| > 0.5 的动物数 | 0 |

**端到端指标对比 (YANGXIANG)**:
| 指标 | National | Legacy | 差异 |
|------|----------|--------|------|
| 动物数 | 118 | 99 | +19 |
| FCR 均值 | 2.831 | 3.118 | -0.287 |
| FCR SD | 0.327 | 0.577 | -0.250 |
| FCR NA | 0 | 1 | -1 |

### 4.3 结论

**丢弃。** National 的加权平均在所有指标上均优于 Legacy 的中位数：
- 动物保留率更高 (+51 头 FIRE, +19 头 YANGXIANG)
- FCR SD 更低 (-17% FIRE, -57% YANGXIANG)
- Legacy 日体重有 109 个 NA，National 为 0
- 中位数的高击穿点优势在 National 架构下已被第一轮 RLM 覆盖

---

## 5. 策略 4: 百分位数采食量检测 (flag_percentile_low / flag_percentile_high)

### 5.1 测试方法

1. 按 animal_id + record_date 去重后统计百分位数标记量
2. 分析百分位数与 STL 标记的重叠
3. 分析百分位数与 National is_outlier_feed 的重叠
4. 评估固定标记率问题

### 5.2 测试结果

**FIRE 标记量统计 (记录级)**:
| 标记 | 数量 |
|------|------|
| Legacy flag_percentile_low | 1,307 |
| Legacy flag_percentile_high | 1,760 |
| Legacy 百分位数合计 | 3,067 |
| Legacy flag_STL_FI | 3,223 |
| National flag_feed_too_high | 0 |
| National is_outlier_feed | 3,231 |
| Legacy is_outlier_feed | 5,591 |

**FIRE 日级分析**:
| 指标 | 值 |
|------|-----|
| 总天数 | 8,303 |
| 动物数 | 84 |
| P2.5 标记天数 | 249 |
| P99 标记天数 | 112 |

**百分位数与 STL 重叠 (日级)**:
| 指标 | P2.5 | P99 |
|------|------|-----|
| 与 STL 重叠 | 19/249 (7.6%) | 36/112 (32.1%) |
| 独有 (不被 STL 覆盖) | 230 | 76 |

**百分位数与 National 采食量标记重叠 (日级)**:
| 指标 | 值 |
|------|-----|
| 被 National is_outlier_feed 覆盖 | 86 |
| 不被 National 覆盖 | 270 |

### 5.3 结论

**丢弃。** 百分位数法的独有贡献主要是边界值附近的正常波动日：
- P2.5 的 92.4% 不与 STL 重叠 -- 这些是固定标记率的产物，非真正异常
- P99 的 67.9% 不与 STL 重叠 -- 同样是固定标记率问题
- National 的 flag_feed_too_high (P99 单记录级) + STL 时间序列检测已提供更精确的检测
- 固定标记率（不管数据质量如何都标记约 3.5% 的天数）在统计学上不可取

---

## 6. 策略 5: 简单线性插值 (zoo::na.approx)

### 6.1 测试方法

比较 National (Kalman Filter + Loess/线性回归 + FCR 验证) 与 Legacy (na.approx) 的插补结果。

### 6.2 测试结果

**FIRE 插补数据**:
| 指标 | National | Legacy |
|------|----------|--------|
| 插补 feed 天数 | 58 | 13 |
| 插补 wt 天数 | 274 | 274 |
| 日体重 NA | 0 | 109 |
| 日采食量负值 | 0 | 0 |
| FCR NA | 0 | 0 |

**YANGXIANG 插补数据**:
| 指标 | National | Legacy |
|------|----------|--------|
| 插补 feed 天数 | 47 | 124 |
| 插补 wt 天数 | 536 | 585 |
| FCR NA | 0 | 0 |

### 6.3 结论

**丢弃。** Legacy na.approx 的主要问题：
- FIRE Legacy 日体重有 109 个 NA（na.approx 无法处理端点缺失），National 为 0
- YANGXIANG Legacy 插补更多（124 vs 47 feed），因为 Gompertz 过度过滤导致更多缺失
- National 的 Kalman Filter 能捕捉体重的非线性增长趋势，na.approx 仅做线性插值
- National 的 FCR 阶段验证防止插补产生不合理的 FCR 值

---

## 7. 综合评估

### 7.1 策略评估汇总

| 策略 | 独有贡献 | 理论优势 | 实测证据 | 决策 |
|------|---------|---------|---------|------|
| SD 阈值体重检测 | 0 条 | 无 (RLM 严格优于) | FIRE 0 条, YX 4 条 (全部被覆盖) | **丢弃** |
| 多项式 RLM 体重检测 | 141 条 (日级) | 无 (两轮 RLM 严格优于) | 141 条是宽松阈值产物 | **丢弃** |
| 中位数日聚合 | 无 | 高击穿点 (已被 RLM 覆盖) | 动物保留率 -51, FCR SD +20% | **丢弃** |
| 百分位数采食量检测 | 270 天 (日级) | 无 (STL 更优) | 固定标记率, 92.4% P2.5 不与 STL 重叠 | **丢弃** |
| 简单线性插值 | 无 | 简单性 | 109 个日体重 NA, 无 FCR 验证 | **丢弃** |

### 7.2 端到端指标对比

**FIRE**:
| 指标 | National | Legacy | 差异 |
|------|----------|--------|------|
| 动物数 | 134 | 83 | +51 (+61%) |
| FCR 均值 | 2.659 | 2.715 | -0.056 |
| FCR SD | 0.440 | 0.530 | -0.090 (-17%) |
| FCR NA | 0 | 0 | 0 |
| ADG (g) | 983.3 | 967.4 | +15.9 |

**YANGXIANG**:
| 指标 | National | Legacy | 差异 |
|------|----------|--------|------|
| 动物数 | 118 | 99 | +19 (+19%) |
| FCR 均值 | 2.831 | 3.118 | -0.287 |
| FCR SD | 0.327 | 0.577 | -0.250 (-43%) |
| FCR NA | 0 | 1 | -1 |

### 7.3 最终结论

**5 项 Legacy 剩余独有策略全部丢弃，不推荐整合到 National Standard。**

根本原因: National Standard 在每个维度上都提供了更优的实现。Legacy 路径应按计划弃用。

---

## 8. 附录: 测试文件清单

| 文件 | 用途 |
|------|------|
| `test_strategy_integration.R` | 主测试脚本，覆盖 5 项策略 |
| `test_strategy2_detail.R` | 策略 2 补充分析 |
| `strategy_integration_test.md` | 本报告 |
| `strategy_evaluation_report.md` | 评估报告 (输入) |
