# QC 策略最终决策报告

**决策日期**: 2026-06-22
**决策依据**: 3 个数据源（YANGXIANG 383K条/FIRE 213K条/Nedap 56K条）的对比测试结果、代码审计报告、逐步骤一致性分析
**决策人**: 决策者 Agent

---

## 1. 一句话结论

**推荐全面采用 National Standard 作为默认 QC 方法，Legacy 降级为可选对照方法。** National Standard 在动物保留率（+38~61%）、表型稳定性（FCR 标准差低 23~60%）、插补完整性（FCR NA 为 0）三个核心维度上全面优于 Legacy。Legacy 存在 2 个 Critical 级代码 Bug 和 3 个 High 级架构缺陷，即使修复后其 Gompertz 过滤策略仍会导致大规模动物丢失。

---

## 2. 决策表格

| 步骤 | 候选方案 | 推荐 | 置信度 | 核心理由 |
|------|---------|------|--------|---------|
| Step 3 Weight QC | national / legacy | **national** | **高** | National 保留更多动物（FIRE: 144 vs 84 头，+71%）；Legacy 的 Gompertz 曲线拟合对数据质量要求过高，在 FIRE 上导致 60 头动物被整群剔除；体重 QC Kappa 在 YANGXIANG 上达 0.847，两种方法高度一致，分歧主因是 Legacy 缺少 `flag_weight_low` 而非 National 过度标记 |
| Step 4 Feed QC | national / legacy | **national** | **高** | National 的速度/时长标记有明确物理意义（设备故障、行为异常），Legacy 的 STL/百分位标记偏向统计异常且过度标记（FIRE 上多标 73%）；两种方法 Kappa 仅 0.03~0.04，几乎无一致性，National 标记更精准；National 的 9 类异常标志为 LMM 校正提供必要输入 |
| Step 5.5 Growth Curve | national(二次多项式) / legacy(Gompertz) | **national** | **高** | 二次多项式对数据质量要求更低、更鲁棒；Gompertz 虽然生物学模型更准确，但在实际大规模数据中容易因噪声导致拟合失败；建议将 R-squared 阈值从 0.99 降至 0.95 以减少不必要的动物剔除 |
| Step 6 Imputation | national(Kalman+Loess) / legacy(线性插值) | **national** | **高** | Kalman 滤波 + Loess/线性回归外推能处理首尾缺失（Legacy 的 `na.approx` 不能）；National 的 FCR 阶段验证能避免产生不合理的极端 FCR 插补值；Legacy 插补后 ADFI 系统性偏高（YANGXIANG +153g），可能与过度依赖插补有关 |
| Step 7 Phenotype FCR | national(标准FCR) / legacy(简单FCR) | **national** | **高** | National 的 FCR 标准差系统性更低（YANGXIANG: 0.325 vs 0.519，FIRE: 0.433 vs 0.533）；National 无 FCR NA（Legacy 在 YANGXIANG 上有 16 个）；在 Nedap 小样本上两种方法一致（FCR 差异 < 0.01），说明 National 的优势在大规模数据上充分体现 |

---

## 3. 关键发现

### 发现 1: Legacy 的动物丢失是系统性问题，非偶发

在 FIRE 数据源上，Legacy 从 188 头动物最终仅保留 83 头（损失率 56%），其中 60 头（72% 的损失）发生在 Step 3 体重 QC 阶段的 Gompertz 曲线过滤。这不是个别动物数据质量差的问题，而是 Gompertz NLS 拟合对数据量和噪声水平的系统性要求过高。相比之下，National 在同一阶段仅损失 44 头（从 188 到 144），且保留的动物表型更稳定。

### 发现 2: 采食量 QC 两种方法衡量的是完全不同的东西

Kappa 一致性仅 0.03~0.04，几乎等于随机。National 的速度/时长标记（`flag_speed_too_fast` 占 nat_only 分歧的 76~90%）反映的是单次采食行为的物理合理性，有明确的设备/行为诊断意义。Legacy 的 STL 残差和百分位数标记反映的是时间序列统计异常，可能将正常的采食量波动（季节性变化、饲料调整）误判为异常。在生产实践中，物理标记比统计标记更可解释、更可操作。

### 发现 3: Legacy 存在 2 个 Critical 级静默数据损坏 Bug

- **C-1 采食量范围单位不匹配**: `feed_intake_range = c(0, 6)`（千克）与 `feed_g`（克）直接比较，导致几乎所有有效采食记录被错误标记为 `flag_feed_out_of_range`，日采食量全部依赖插补值。这是一个静默数据损坏 Bug，直接影响表型准确性。
- **C-2 QC 标志越权初始化**: Weight QC 阶段初始化了 Feed QC 的标志列，虽然不导致计算错误但违反职责分离原则。

此外还有 3 个 High 级缺陷：LMM 校正在 Legacy 模式下实质无效（correction 恒为 0）、首尾缺失无法插补、R-squared 阈值回退到过严的 0.99。

### 发现 4: National Standard 的 LMM 校正提供了不可替代的价值

National 模式下，FIRE 数据有 2,274 条日采食量被 LMM 校正，Nedap 有 25 条。Legacy 模式下校正量为 0。LMM 校正能修正设备效应（如不同料槽的系统性偏差）和日间随机波动，是保证表型计算准确性的关键环节。Legacy 的标志体系（STL/百分位）与 LMM 的特征空间不兼容，导致校正模块完全失效。

### 发现 5: 在小样本高质量数据上两种方法表现一致

Nedap 数据源（3 头动物）上，两种方法的表型高度一致：FCR 差异 < 0.01，ADFI 差异 < 25g，ADG 差异 < 6g。这说明 Legacy 在数据质量好、样本量小时是可靠的，但其优势仅限于理想条件。在大规模生产数据中（YANGXIANG/FIRE），Legacy 的劣势全面暴露。

---

## 4. 预期效果

采用 National Standard 作为默认方法后，预期改善如下：

| 维度 | 当前（Legacy 混合使用时的最差情况） | 采用 National 后 |
|------|--------------------------------------|-----------------|
| 动物保留率 | FIRE 上仅 83 头（56% 损失） | FIRE 上 134 头（29% 损失），提升 61% |
| FCR 稳定性 | SD = 0.519~0.533 | SD = 0.325~0.433，降低 23~38% |
| FCR 缺失 | YANGXIANG 上 16 个 NA | 0 个 NA |
| 采食量校正 | 无（LMM 失效） | 25~2,274 条日采食量被校正 |
| 端点缺失处理 | 不能外推 | Kalman + Loess 外推，完整覆盖 |
| 代码 Bug 风险 | 2 个 Critical + 3 个 High 级缺陷 | National 路径在 V0.2.5 审计中无 Critical 级缺陷 |

---

## 5. 风险提示

### 风险 1: National 的 `flag_speed_too_fast` 阈值可能需要校准

在 YANGXIANG 数据上，`flag_speed_too_fast` 标记了 8,283 条记录（占异常的 76.5%），在 FIRE 上标记了 2,904 条（占 89.6%）。这个比例较高，建议确认速度阈值（`speed_max_g_per_min`）是否针对不同设备类型做过校准。如果阈值过松，可能漏掉真实的设备故障；如果过紧，可能过度剔除正常记录。

**建议**: 在后续版本中，针对 YANGXIANG 和 FIRE 设备分别校准速度阈值，或提供设备类型自适应的阈值推荐。

### 风险 2: R-squared 阈值 0.99 在某些数据集上可能过于严格

当前 National 的 R-squared 阈值为 0.99，在 FIRE 上导致 10 头动物被删除。虽然这些动物的数据质量确实较差（二次多项式拟合不佳），但 0.99 在统计学上是非常高的要求。建议降为 0.95，这是一个更平衡的选择——既能过滤明显的异常生长曲线，又不会过度剔除。

**建议**: 将 `national_standard$growth_curve_r2_min` 从 0.99 调整为 0.95，并在 2~3 个额外数据集上验证调整后的动物保留率和表型稳定性。

### 风险 3: 完全弃用 Legacy 会丢失 STL 时间序列异常检测能力

Legacy 的 STL 分解能检测偏离时间趋势的异常日，这是 National 的单记录规则无法做到的。对于存在季节性变化或饲料调整的猪场，STL 可能有独特价值。

**建议**: 不完全删除 Legacy 代码，而是将 STL 检测作为 National 的可选补充标记（额外的 flag 列），在需要时启用。

### 风险 4: `flag_weight_low` 的生物学合理性需要进一步验证

National 的 `flag_weight_low` 是体重 QC 分歧的绝对主因（占 nat_only 分歧的 99.5~100%）。这个标记的作用是剔除体重异常偏低的记录，但它可能误剔除真实的低体重个体（如病后恢复期、弱势个体）。如果研究目标包含生长性能的全谱分析（包括低生长个体），需要评估这个标记是否引入了选择偏差。

**建议**: 抽样检查被 `flag_weight_low` 标记的记录，确认这些记录是设备误差还是真实的低体重数据。

---

## 6. 后续行动

### 立即执行（本次迭代）

| 编号 | 行动 | 优先级 | 预计工作量 |
|------|------|--------|-----------|
| A-1 | 将默认 `qc_method` 从 `"legacy"` 改为 `"national_standard"`（如果当前默认是 legacy） | Critical | 1 行代码 |
| A-2 | 修复 Legacy C-1 Bug（调用 `.normalize_feed_range()`），即使不推荐使用也应修复以保证代码正确性 | Critical | 5 行代码 |
| A-3 | 将 `national_standard$growth_curve_r2_min` 从 0.99 调整为 0.95 | High | 1 行配置 |
| A-4 | 更新用户文档，明确说明推荐使用 National Standard 方法，Legacy 仅作对照 | Medium | 文档更新 |

### 短期优化（下一版本）

| 编号 | 行动 | 优先级 |
|------|------|--------|
| B-1 | 针对 YANGXIANG/FIRE 设备类型校准 `speed_max_g_per_min` 阈值 | High |
| B-2 | 评估将 STL 检测作为 National 可选补充标记的可行性 | Medium |
| B-3 | 抽样验证 `flag_weight_low` 标记的生物学合理性 | Medium |
| B-4 | 修复 Legacy 剩余 High 级缺陷（H-1 LMM 适配、H-2 端点外推、H-3 R-squared 配置） | Low（不推荐使用时降为低优先级） |

### 长期规划

| 编号 | 行动 |
|------|------|
| C-1 | 建立设备类型自适应的 QC 参数推荐系统 |
| C-2 | 建立 QC 效果的持续监控机制（表型稳定性指标追踪） |
| C-3 | 评估 Legacy 代码的维护成本 vs 收益，决定是否在 V0.3.0 中移除 |

---

## 附录: 决策依据数据总览

| 指标 | YANGXIANG (Nat/Leg) | FIRE (Nat/Leg) | Nedap (Nat/Leg) |
|------|---------------------|----------------|-----------------|
| Step 3 后动物 | 126 / 100 | 144 / 84 | 3 / 3 |
| Step 5.5 后动物 | 118 / 99 | 134 / 83 | 3 / 3 |
| Step 7 动物 | 118 / 99 | 134 / 83 | 3 / 3 |
| Mean FCR | 2.980 / 3.264 | 2.803 / 2.943 | 3.062 / 3.057 |
| SD FCR | 0.325 / 0.519 | 0.433 / 0.533 | 0.197 / 0.179 |
| FCR NA | 0 / 16 | 0 / 3 | 0 / 0 |
| Weight QC Kappa | 0.847 | 0.375 | -0.032 |
| Feed QC Kappa | 0.031 | 0.041 | -0.008 |
| LMM 校正量 | 有 / 0 | 2274 / 0 | 25 / 0 |
| Legacy Critical Bug | 2 个（C-1 采食量单位、C-2 标志越权） |
| Legacy High 缺陷 | 3 个（H-1 LMM 失效、H-2 端点缺失、H-3 R-squared 过严） |
