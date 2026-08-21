# Legacy QC 方法逐步骤评估报告

**评估日期**: 2026-06-22
**评估范围**: Legacy vs National Standard 在 3 个数据源、9 个流水线步骤上的对比
**数据源**: YANGXIANG (383K records, 200 animals), FIRE (213K records, 201 animals), Nedap (56K records, 45 animals)
**参考材料**: compare_summary.csv, agreement_summary.csv, disagreement_profile.csv, evaluator_audit_report.md, per-source metrics JSON

---

## 1. 执行摘要

Legacy 方法在数据保留率上显著低于 National Standard。在 FIRE 数据源上，Legacy 最终仅保留 83 头动物（National 保留 134 头），损失率达 38%。核心问题集中在三个环节：(1) 体重 QC 阶段的 Gompertz 曲线过滤过于激进，导致 FIRE 上 60 头动物被整群剔除；(2) 采食量 QC 的 STL 分解标记异常过多，且 Kappa 一致性仅 0.03-0.04，表明两种方法在衡量本质上不同的东西；(3) R-squared 阈值回退到 0.99 的过严标准，进一步删除动物。在小样本 Nedap 数据源上，两种方法表型结果高度一致（FCR 差异 < 0.01），说明 Legacy 在数据质量好时表现可接受。Legacy 存在 2 个 Critical 级代码 Bug（采食量范围单位不匹配、QC 标志越权初始化）需要立即修复。

---

## 2. 逐步骤详细分析

### Step 1 - Read（数据读取）

| 数据源 | National | Legacy | 差异 |
|--------|----------|--------|------|
| YANGXIANG | 383,666 | 383,666 | 0 |
| FIRE | 213,176 | 213,176 | 0 |
| Nedap | 55,568 | 55,568 | 0 |

**结论**: 读取步骤完全一致，无差异。两种方法共享同一读取逻辑。

---

### Step 2 - Overall QC（去重 + 缺失值过滤）

| 数据源 | National | Legacy | 差异 |
|--------|----------|--------|------|
| YANGXIANG | 346,033 (190 animals) | 346,033 (190 animals) | 0 |
| FIRE | 209,190 (188 animals) | 209,190 (188 animals) | 0 |
| Nedap | 27,830 (42 animals) | 27,830 (42 animals) | 0 |

**结论**: Overall QC 步骤完全一致。去重和缺失值过滤逻辑不依赖 QC 方法。

注意: YANGXIANG 测试报告中的 "corrected_records" (223,983 vs 191,404) 是 Step 5 日聚合后的修正记录数，不是 Step 2 的输出。compare_summary 确认 Step 2 输出完全一致。

---

### Step 3 - Weight QC（体重异常检测）

#### 数据保留率

| 数据源 | 指标 | National | Legacy | 差异 | 差异% |
|--------|------|----------|--------|------|-------|
| YANGXIANG | n_after | 237,041 | 194,978 | -42,063 | -17.7% |
| YANGXIANG | n_animals_after | 126 | 100 | -26 | -20.6% |
| FIRE | n_after | 164,483 | 100,918 | -63,565 | -38.7% |
| FIRE | n_animals_after | 144 | 84 | -60 | -41.7% |
| Nedap | n_after | 1,612 | 1,612 | 0 | 0% |
| Nedap | n_animals_after | 3 | 3 | 0 | 0% |

#### 异常标记量

| 数据源 | National outliers | Legacy outliers | 差异% |
|--------|-------------------|-----------------|-------|
| YANGXIANG | 10,077 (4.25%) | 8,815 (4.52%) | -12.5% |
| FIRE | 7,379 (4.49%) | 3,320 (3.29%) | -55.0% |
| Nedap | 12 (0.74%) | 17 (1.05%) | +41.7% |

#### Legacy 各标记分布

| 标记 | YANGXIANG | FIRE | Nedap |
|------|-----------|------|-------|
| flag_weight_out_of_range | 5,113 | 800 | 0 |
| flag_SD_WT | 4 | 0 | 0 |
| flag_RLM_WT | 4,003 | 2,063 | 6 |
| flag_Gompertz_WT | 1,088 | 1,869 | 17 |

#### 分歧根源分析

一致性数据（agreement_summary）:
- YANGXIANG: Kappa = 0.847, 一致率 97.9% -- 高度一致
- FIRE: Kappa = 0.375, 一致率 86.4% -- 中等一致
- Nedap: Kappa = -0.032, 一致率 88.2% -- 无一致性（小样本）

分歧画像（disagreement_profile）显示:
- **nat_only 分歧**中 99.5-100% 来自 `flag_weight_low`（National 的低体重标记），Legacy 没有对应的标记
- YANGXIANG: 197/198 的 nat_only 分歧是 flag_weight_low
- FIRE: 1,059/1,060 的 nat_only 分歧是 flag_weight_low

**判断**: `flag_weight_low` 是体重 QC 分歧的绝对主因。National Standard 使用 "两轮 RLM + 加权平均日体重 + 二次多项式" 路径，其中 flag_weight_low 标记的是 RLM 残差过大或日体重偏低的记录。Legacy 使用 "SD 阈值 + RLM + Gompertz" 路径，没有直接的 low-weight 标记，而是通过 Gompertz 曲线拟合来判断整体生长轨迹是否异常。

**Legacy 是否过度过滤?** 是的，但原因不是标记了更多异常记录（Legacy 标记量反而更少），而是 **Gompertz 曲线过滤导致大量记录被连带删除**。FIRE 数据源上，Legacy 的 Gompertz 标记了 1,869 条记录，但最终 n_after 从 209,190 骤降到 100,918（-51.3%），说明 Gompertz 不仅标记了异常记录，还触发了整群动物的剔除（从 188 头降到 84 头，丢失 60 头动物）。这是因为 Gompertz 曲线对数据质量要求高，当个体数据点不足或噪声大时，NLS 拟合失败或 R-squared 过低，导致该个体被整体剔除。

---

### Step 4 - Feed QC（采食量异常检测）

#### 异常标记量

| 数据源 | National outliers | Legacy outliers | 差异% | Kappa |
|--------|-------------------|-----------------|-------|-------|
| YANGXIANG | 11,092 (4.68%) | 13,973 (7.17%) | +26.0% | 0.031 |
| FIRE | 3,231 (1.96%) | 5,591 (5.54%) | +73.0% | 0.041 |
| Nedap | 31 (1.92%) | 46 (2.85%) | +48.4% | -0.008 |

#### Legacy 各标记分布

| 标记 | YANGXIANG | FIRE | Nedap |
|------|-----------|------|-------|
| flag_STL_FI | 11,084 | 3,223 | 0 |
| flag_feed_out_of_range | 14 | 0 | 0 |
| flag_percentile_low | 2,474 | 1,307 | 23 |
| flag_percentile_high | 4,068 | 1,760 | 23 |

#### National 各标记分布（nat_only 分歧中的构成）

| 标记 | YANGXIANG | FIRE | Nedap |
|------|-----------|------|-------|
| flag_speed_too_fast | 3,514 (76.5%) | 1,208 (89.6%) | 1 (4.2%) |
| flag_speed_too_slow | 1,144 (24.9%) | 61 (4.5%) | 17 (70.8%) |
| flag_duration_too_long | 421 (9.2%) | 94 (7.0%) | 7 (29.2%) |
| flag_duration_zero_with_feed | 16 | 0 | 0 |
| flag_speed_extreme_low_feed | 96 | 0 | 0 |

#### Kappa ≈ 0 的含义

Kappa 值在 0.03-0.04（YANGXIANG/FIRE）和 -0.008（Nedap）表明两种方法在采食量异常判定上几乎没有一致性。这意味着:

1. **两种方法在衡量不同的东西**。National Standard 的标记基于物理量（速度 = 采食量/时长，时长），反映的是单次采食行为的合理性。Legacy 的标记基于统计量（STL 残差、百分位数），反映的是日采食量的时间序列异常。

2. **National 的速度类标记有明确的物理意义**:
   - `flag_speed_too_fast`: 采食速度异常高，可能对应设备故障（重复打卡、传感器误差）
   - `flag_speed_too_slow`: 采食速度异常低，可能对应动物异常行为或设备读数偏移
   - `flag_duration_too_long`: 采食时间异常长，可能对应动物长时间停留在料槽

3. **Legacy 的标记更偏向统计异常**:
   - `flag_STL_FI`: STL 分解残差超出阈值，检测的是偏离时间趋势的异常日
   - `flag_percentile_low/high`: 超出个体分布的 P2.5/P99 分位数，检测的是极端值

**Legacy 标记更多异常是更严格还是更合理?** 这取决于数据质量。在 YANGXIANG 数据上，Legacy 的 STL 标记了 11,084 条（占 5.7%），而 National 的速度标记合计 11,092 条（占 4.7%）。两者标记量相近但重叠极低（Kappa = 0.03），说明它们在捕捉不同类型的异常。National 的速度标记更有可能对应真实的设备/行为异常，而 Legacy 的 STL 标记可能包含更多正常但偏高的采食日（季节性变化、饲料调整期等）。

---

### Step 5 - Daily Aggregation（日聚合）

| 数据源 | National daily | Legacy daily | National animals | Legacy animals |
|--------|---------------|--------------|------------------|----------------|
| YANGXIANG | 14,023 | 11,053 | 126 | 100 |
| FIRE | 14,296 | 8,303 | 144 | 84 |
| Nedap | 201 | 201 | 3 | 3 |

#### LMM 校正

| 数据源 | National LMM corrections | Legacy LMM corrections |
|--------|--------------------------|------------------------|
| YANGXIANG | 有（具体数未报告） | 0 |
| FIRE | 2,274 | 0 |
| Nedap | 25 | 0 |

**分析**: Legacy 的 LMM 校正实质无效（审计报告 H-1）。原因在于 LMM 校正模块依赖 National Standard 的 9 个单记录异常标志（flag_duration_negative, flag_speed_too_slow 等），Legacy 不产生这些标志列，导致所有标志检查跳过，correction 恒为 0。这是一个架构层面的缺陷：LMM 校正模块与 National Standard 的 Feed QC 标志紧耦合，Legacy 的标志体系（flag_STL_FI, flag_percentile_low/high）无法被 LMM 识别。

---

### Step 5.5 - Growth Curve R-squared Check（生长曲线检查）

| 数据源 | National deleted | Legacy deleted | National remaining | Legacy remaining |
|--------|-----------------|----------------|-------------------|-----------------|
| YANGXIANG | 8 | 1 | 118 | 99 |
| FIRE | 10 | 1 | 134 | 83 |
| Nedap | 0 | 0 | 3 | 3 |

**分析**: 这个结果出乎意料。审计报告 H-3 指出 Legacy 回退到 0.99 的 R-squared 阈值（应为 0.95），预期 Legacy 会删除更多动物。但实际数据显示 Legacy 删除更少（1 vs 10, 1 vs 8）。

原因是: Legacy 在 Step 3 的 Gompertz 体重 QC 阶段已经删除了大量低质量动物（FIRE 上从 188 头降到 84 头），剩下的 84 头动物的数据质量已经很高，自然能通过 R-squared 检查。而 National 保留了更多动物（144 头），其中 10 头的数据质量不足以达到 0.99 的 R-squared 阈值。

**这揭示了一个问题**: Legacy 的动物删除主要发生在 Step 3（体重 QC），而非 Step 5.5（生长曲线）。Gompertz 曲线拟合对数据量和数据质量的要求比二次多项式更高，导致大量动物在早期就被剔除。这是一种"前端重过滤"策略，与 National 的"后端精筛选"策略形成对比。

#### Gompertz vs 二次多项式

从生物学角度看:
- **Gompertz 曲线**更符合猪只生长的真实模式（S 型曲线，有渐近线），但需要足够的数据点和较好的数据质量才能可靠拟合
- **二次多项式**是 Gompertz 的局部近似，在短时间窗口内（如本测试的 3-5 个月）通常足够准确，且对数据质量的要求更低

在大规模生产数据中，二次多项式的鲁棒性更优。Gompertz 虽然理论上更准确，但在实际应用中容易因数据噪声导致拟合失败或 R-squared 过低。

---

### Step 6 - Imputation（缺失值插补）

| 数据源 | 方法 | 插补feed | 插补wt | 插补后daily | 插补后animals |
|--------|------|----------|--------|-------------|---------------|
| YANGXIANG | National | 47 | 536 | 13,087 | 118 |
| YANGXIANG | Legacy | 124 | 585 | 10,915 | 99 |
| FIRE | National | 58 | 274 | 13,340 | 134 |
| FIRE | Legacy | 13 | 274 | 8,202 | 83 |
| Nedap | National | 0 | 0 | 201 | 3 |
| Nedap | Legacy | 0 | 4 | 201 | 3 |

**分析**:

1. **YANGXIANG**: Legacy 插补了更多 feed（124 vs 47）和 weight（585 vs 536）。Legacy 的 na.approx 线性插值只能在已知数据点之间插值，不处理端点缺失。更多的插补量可能是因为 Legacy 在 Step 4 标记了更多 feed 异常（STL 标记），导致更多日 feed 被置为 NA 后需要插补。

2. **FIRE**: Legacy 插补了更少的 feed（13 vs 58）。这是因为 Legacy 保留的动物更少（83 vs 134），缺失的绝对量自然更少。

3. **National 的 FCR 验证环节**: National Standard 的采食量插补使用 Loess/线性回归外推，并配合 FCR 阶段验证（检查插补后的 FCR 是否在合理范围内）。这比 Legacy 的简单线性插值更审慎，能避免产生不合理的极端 FCR 值。

4. **端点缺失问题**（审计报告 H-2）: Legacy 的 na.approx 不处理首尾缺失，但在本次测试中影响不大（Nedap 仅 4 条体重插补，YANGXIANG/FIRE 的端点缺失量有限）。

---

### Step 7 - Phenotype（表型计算）

| 数据源 | 方法 | Animals | ADFI (g) | ADG (g) | FCR | SD FCR | FCR NA |
|--------|------|---------|----------|---------|-----|--------|--------|
| YANGXIANG | National | 118 | 2,467 | 881.1 | 2.980 | 0.325 | 0 |
| YANGXIANG | Legacy | 99 | 2,619.8 | 860.2 | 3.264 | 0.519 | 16 |
| FIRE | National | 134 | 2,478 | 952.1 | 2.803 | 0.433 | 0 |
| FIRE | Legacy | 83 | 2,513.9 | 923.6 | 2.943 | 0.533 | 0 |
| Nedap | National | 3 | 2,806.6 | 1,024.0 | 3.062 | 0.197 | 0 |
| Nedap | Legacy | 3 | 2,785.6 | 1,018.1 | 3.057 | 0.179 | 0 |

#### FCR 差异的主要来源

1. **数据组成差异**: Legacy 保留的动物更少且不同（YANGXIANG: 99 vs 118, FIRE: 83 vs 134）。FCR 的均值和标准差差异主要来自两组不同的动物群体，而非同一动物的不同 FCR 值。

2. **采食量偏差**: Legacy 的 ADFI 系统性偏高（YANGXIANG: +153g, FIRE: +36g）。这与审计报告 C-1 相关 -- Legacy 的 `flag_feed_out_of_range` 单位不匹配导致几乎所有日采食量被标记为异常并置为 NA，全部依赖插补值。插补值可能系统性偏高（线性插值在上升趋势中会高估中间值）。

3. **ADG 偏差**: Legacy 的 ADG 系统性偏低（YANGXIANG: -21g, FIRE: -29g）。这与 Gompertz 过滤有关 -- Gompertz 倾向于保留生长曲线更平滑的动物（可能生长较慢），而剔除生长波动大的动物（可能包含快速生长期）。

4. **FCR 标准差**: Legacy 的 SD FCR 显著高于 National（YANGXIANG: 0.519 vs 0.325, +59.7%; FIRE: 0.533 vs 0.433, +23.1%）。这表明 Legacy 的表型结果更分散、更不稳定。

#### Legacy 产生 FCR_NA 的原因

YANGXIANG 数据源上 Legacy 产生了 16 个 FCR_NA。原因可能是:
1. 端点缺失导致起始/终止体重为 NA（审计报告 H-2）
2. 某些动物的日增重为 0（分母为零），Legacy 没有 LMM 校正来修正异常日采食量
3. 插补后某些阶段的 ADG 仍为 0 或负值

#### 哪种方法的表型更稳定

**National Standard 的表型更稳定**:
- FCR SD 更低（3 个数据源一致）
- FCR NA 更少（YANGXIANG: 0 vs 16）
- 在 Nedap 小样本上两种方法一致（FCR 差异 < 0.01），说明 National 的优势在大规模数据上才体现

---

## 3. Legacy 优势清单

1. **体重异常标记更精确**: 在 YANGXIANG 上，Legacy 标记了 8,815 条体重异常（4.52%），National 标记了 10,077 条（4.25%）。Legacy 的标记量更少但异常率略高，说明标记更集中、更精确。Kappa = 0.847 表明两者高度一致，Legacy 的差异主要来自缺少 flag_weight_low。

2. **Gompertz 曲线有理论优势**: Gompertz 曲线比二次多项式更符合猪只生长的生物学模型（S 型增长）。虽然在实践中对数据质量要求更高，但对于高质量数据（如 Nedap），Gompertz 能提供更准确的生长轨迹描述。

3. **STL 分解能捕捉时间序列异常**: STL 标记能检测到偏离时间趋势的异常日，这是 National 的单记录规则无法做到的。对于存在季节性变化或饲料调整的猪场，STL 可能更有价值。

4. **在小样本上表现相当**: Nedap 数据源（3 头动物）上，两种方法的表型结果高度一致（FCR 差异 < 0.01, ADFI 差异 < 25g, ADG 差异 < 6g），说明 Legacy 在数据质量好、样本量小时是可靠的。

5. **代码结构清晰**: 审计报告显示 Legacy 代码结构合理、函数职责分离、可配置性好（use_rlm, use_gompertz, use_stl 开关控制）。

6. **对 National 的 RLM Bug 无感**: V0.2.4 的 Critical Bug C1（`<<-` 赋值）仅影响 National Standard 的 Feed QC，Legacy 使用百分位数法，不受影响。

---

## 4. Legacy 劣势清单

1. **动物保留率过低**: FIRE 上 Legacy 仅保留 83 头动物（National 保留 134 头，-38%），YANGXIANG 上保留 99 头（National 118 头，-16%）。大量动物在 Step 3 的 Gompertz 体重 QC 阶段被整群剔除，导致最终表型样本量大幅缩水。

2. **LMM 校正实质无效**: 审计报告 H-1 确认 Legacy 模式下 LMM 校正恒为 0。这意味着日采食量未经设备效应修正，直接使用原始求和值。

3. **FCR 不稳定**: Legacy 的 SD FCR 系统性高于 National（YANGXIANG +59.7%, FIRE +23.1%），且产生 FCR_NA（YANGXIANG 上 16 个）。

4. **采食量范围单位 Bug**: 审计报告 C-1 发现 `feed_intake_range = c(0, 6)`（千克）与 `feed_g`（克）直接比较，导致几乎所有有效采食记录被错误标记。这是一个静默数据损坏 Bug。

5. **R-squared 阈值回退**: Legacy 没有定义自己的 `growth_curve_r2_min`，回退到 National 的 0.99 阈值。虽然在当前测试中影响不大（因为 Gompertz 已经提前过滤），但在数据质量好时会成为不必要的限制。

6. **首尾缺失无法插补**: na.approx 不进行端点外推，导致试验期首尾的缺失值无法填充。

7. **采食量 QC 标记与 National 无一致性**: Kappa = 0.03-0.04 表明两种方法在衡量不同的东西。Legacy 的 STL/百分位标记与 National 的速度/时长标记几乎不重叠，难以交叉验证。

8. **百分位数法在小样本上不稳定**: 当个体有效日记录 < 20 天时，P2.5/P97.5 的估计受极端值影响大（审计报告 M-1）。

---

## 5. 参数优化建议

### 优先级 1（Critical，必须修复）

| 编号 | 问题 | 建议 | 预期效果 |
|------|------|------|----------|
| C-1 | feed_intake_range 单位不匹配 | 调用 `.normalize_feed_range(cfg$legacy$feed_intake_range)` 或改配置为 `c(0, 6000)` | 消除误标记，恢复正常采食量 QC |

### 优先级 2（High，强烈建议）

| 编号 | 问题 | 建议 | 预期效果 |
|------|------|------|----------|
| H-3 | R-squared 阈值过严 | 在 Legacy 配置中添加 `growth_curve_r2_min = 0.95` | 减少不必要的动物剔除 |
| H-1 | LMM 校正无效 | 将 Legacy 标志映射到 LMM 特征空间，或显式跳过并记录日志 | 消除无意义计算或提供有意义的校正 |
| P-1 | Gompertz 过滤过激 | 降低 `use_gompertz` 默认阈值，或增加 `min_obs_for_wt` 的最低要求（如 30 天） | 减少动物损失 |

### 优先级 3（Medium，建议改进）

| 编号 | 问题 | 建议 | 预期效果 |
|------|------|------|----------|
| H-2 | 端点缺失 | na.approx 添加 `rule = 2` 进行端点外推 | 消除 FCR_NA |
| M-1 | 百分位数小样本 | 添加 `n_valid_days >= 30` 的最小样本量检查 | 减少小样本个体的误标记 |
| P-2 | STL 标记过多 | 调整 STL 异常阈值（当前可能过松） | 减少过度标记 |

### 优先级 4（Low，可选改进）

| 编号 | 问题 | 建议 |
|------|------|------|
| C-2 | QC 标志越权初始化 | 拆分 Weight/Feed 标志初始化 |
| L-1 | is_outlier_fi_stl 死代码 | 从初始化列表中移除 |

---

## 6. 初步推荐

### 按步骤推荐

| 步骤 | 推荐 | 理由 |
|------|------|------|
| Step 1 - Read | 共享 | 两种方法完全一致 |
| Step 2 - Overall QC | 共享 | 两种方法完全一致 |
| Step 3 - Weight QC | **National** | National 保留更多动物和记录（FIRE: 144 vs 84 头），Kappa = 0.37-0.85 表明有一定一致性但 Legacy 过于激进。National 的两轮 RLM + 二次多项式路径更稳健。Legacy 的 Gompertz 在理论上更准确但实践中代价太大。 |
| Step 4 - Feed QC | **National** | National 的速度/时长标记有明确物理意义，且标记量适中。Legacy 的 STL 标记过多且与 National 无一致性（Kappa = 0.03）。但需注意 National 的 `flag_speed_too_fast` 占 nat_only 分歧的 76-90%，需确认阈值是否合理。 |
| Step 5 - Daily Aggregation | **National** | Legacy 的 LMM 校正无效，National 有 25-2,274 条校正记录。 |
| Step 5.5 - Growth Curve | **National**（但需调参） | 二次多项式对数据质量要求更低，更鲁棒。但 R-squared = 0.99 过严，建议降为 0.95。 |
| Step 6 - Imputation | **National** | Kalman + Loess + FCR 验证比 na.approx 更全面，能处理端点缺失。 |
| Step 7 - Phenotype | **National** | FCR 更稳定（SD 更低），无 FCR_NA。 |

### 整体推荐

**推荐使用 National Standard 作为默认方法**，理由:
1. 动物保留率显著更高（FIRE: 134 vs 83, +61%）
2. 表型结果更稳定（FCR SD 更低）
3. LMM 校正有效
4. 插补方法更全面

**Legacy 可作为补充/对照方法**，但需要先修复 Critical Bug（C-1 采食量单位），并在以下条件满足时考虑使用:
- 数据质量高、样本量大（如 Nedap 类型数据）
- 用户明确偏好 Gompertz 生长模型
- 需要 STL 时间序列异常检测能力

**混合方案可行性**: 可考虑将 Legacy 的 STL 采食量检测作为 National 的补充标记（额外的 flag），而非替代方案。这样既能利用 STL 的时间序列异常检测能力，又能保留 National 的速度/时长物理标记和 LMM 校正。

---

## 附录: 三数据源对比总表

| 指标 | YANGXIANG (Nat/Leg) | FIRE (Nat/Leg) | Nedap (Nat/Leg) |
|------|---------------------|----------------|-----------------|
| 原始记录 | 383,666 / 383,666 | 213,176 / 213,176 | 55,568 / 55,568 |
| Step 2 后记录 | 346,033 / 346,033 | 209,190 / 209,190 | 27,830 / 27,830 |
| Step 3 后记录 | 237,041 / 194,978 | 164,483 / 100,918 | 1,612 / 1,612 |
| Step 3 后动物 | 126 / 100 | 144 / 84 | 3 / 3 |
| Step 5 日记录 | 14,023 / 11,053 | 14,296 / 8,303 | 201 / 201 |
| Step 5.5 后动物 | 118 / 99 | 134 / 83 | 3 / 3 |
| Step 7 动物 | 118 / 99 | 134 / 83 | 3 / 3 |
| Mean FCR | 2.980 / 3.264 | 2.803 / 2.943 | 3.062 / 3.057 |
| SD FCR | 0.325 / 0.519 | 0.433 / 0.533 | 0.197 / 0.179 |
| FCR NA | 0 / 16 | 0 / 0 | 0 / 0 |
| Weight QC Kappa | 0.847 | 0.375 | -0.032 |
| Feed QC Kappa | 0.031 | 0.041 | -0.008 |
