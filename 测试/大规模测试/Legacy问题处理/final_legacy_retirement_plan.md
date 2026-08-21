# Legacy 路径最终退休计划

**决策日期**: 2026-06-22
**决策人**: 项目决策者
**依据**: 决策者框架、评估者报告、测试者验证结果三方综合
**当前版本**: V0.2.4 (DESCRIPTION), V0.2.6 (已完成 STL/Gompertz 整合)

---

## 1. 决策汇总表

| 策略 | 结论 | 理由 | 优先级 |
|------|------|------|--------|
| SD 阈值体重检测 (flag_SD_WT) | **丢弃** | National 两轮 RLM 完全覆盖；实测仅 4 条独有标记（0.001%），无实际检测价值 | P3 |
| 多项式 RLM 体重检测 (flag_RLM_WT) | **丢弃** | National 两轮 RLM 架构（去噪+精炼）严格优于 Legacy 单轮 RLM；141 条独有标记是宽松阈值(0.5)产物 | P3 |
| 中位数日聚合 (median_weight_per_day) | **丢弃** | 加权平均利用 RLM 连续权重，信息量和精度均优于中位数；实测动物保留率 -51（FIRE），FCR SD +20% | P3 |
| 百分位数采食量检测 (flag_percentile_low/high) | **丢弃** | STL 已提供更优的时间序列异常检测；固定标记率（~3.5%天数）在高质量数据上产生误标记；P2.5 的 92.4% 不与 STL 重叠 | P3 |
| 简单线性插值 (zoo::na.approx) | **丢弃** | Kalman Filter + Loess + FCR 阶段验证严格优于 na.approx；实测 Legacy 产生 109 个日体重 NA（National 为 0） | P3 |

**最终结论: 5 项 Legacy 剩余独有策略全部丢弃，不整合到 National Standard。Legacy 路径按计划弃用。**

---

## 2. 决策依据

### 2.1 三方一致性

三方报告（决策者框架、评估者深度分析、测试者实测验证）对 5 项策略的结论完全一致：全部丢弃。核心论据可归纳为：

1. **National 在每个维度上都有更优实现**。Legacy 的每项策略都被 National 的对应模块覆盖或超越。
2. **实测数据无独有价值**。SD 阈值仅 4 条独有标记；RLM 的 141 条独有标记是宽松阈值产物；百分位数的独有贡献主要是边界值附近的正常波动日。
3. **端到端指标全面劣于 National**。动物保留率（FIRE: 83 vs 134）、FCR 稳定性（SD +17%~57%）、FCR NA（16 vs 0）均不如 National。

### 2.2 端到端指标对比

| 指标 | YANGXIANG National | YANGXIANG Legacy | FIRE National | FIRE Legacy |
|------|-------------------|-----------------|---------------|-------------|
| 动物数 | 118 | 99 (-16%) | 134 | 83 (-38%) |
| FCR 均值 | 2.831 | 3.118 | 2.659 | 2.715 |
| FCR SD | 0.327 | 0.577 (+76%) | 0.440 | 0.530 (+20%) |
| FCR NA | 0 | 1 | 0 | 0 |
| LMM 校正 | 有效 | 无效 | 有效 | 无效 |

---

## 3. Legacy 弃用计划

### 3.1 弃用理由

- Legacy 的 5 项独有策略全部丢弃，无任何保留价值
- Legacy 路径保留约 500 行专用代码，增加维护负担
- 双方法架构增加用户选择困惑
- LMM 校正在 Legacy 模式下完全失效（flag 不匹配），导致采食量校正形同虚设
- Gompertz 过度过滤动物（FIRE 损失 38%），造成大量样本浪费

### 3.2 弃用时间线

| 阶段 | 版本 | 行动 | 详细内容 |
|------|------|------|----------|
| **Phase 1: 弃用警告** | V0.2.7 | 运行时警告 | 当用户调用 `ZhenM_default_config("legacy")` 或 `run_zhen_measure(qc_method="legacy")` 时，输出 `lifecycle::deprecate_warn()` 或 `warning()` 消息，提示 Legacy 已弃用、将在 V1.0.0 移除 |
| **Phase 2: 功能冻结** | V0.3.0 | 标记为 deprecated | Legacy 代码保留但不再维护；文档中标注 `(deprecated)`；`ZhenM_default_config("legacy")` 返回值添加 `deprecated = TRUE` 属性 |
| **Phase 3: 代码移除** | V1.0.0 | 移除 Legacy 路径 | 移除所有 Legacy 分支代码（~500 行）；`qc_method` 参数仅接受 `"national_standard"`；清理 DESCRIPTION 中的 `nlme`/`quantreg`/`splines` 依赖 |

### 3.3 Phase 1 详细实施 (V0.2.7)

#### 3.3.1 添加弃用警告

**文件**: `R/run_zhen_measure.R`
**位置**: 第 39 行之后（`qc_method <- match.arg(qc_method)` 之后）

```r
# 添加弃用警告
if (qc_method == "legacy") {
  warning(
    "ZhenMeasure: qc_method = 'legacy' is deprecated and will be removed in V1.0.0. ",
    "Please use qc_method = 'national_standard' instead. ",
    "Legacy method has been fully superseded by national_standard in all QC dimensions. ",
    "See ?ZhenM_default_config for migration guidance.",
    call. = FALSE
  )
}
```

**文件**: `R/zhenm_config_defaults.R`
**位置**: `ZhenM_default_config()` 函数的 `else` 分支（第 81 行）

```r
# 在 legacy 配置块开头添加警告
if (qc_method == "legacy") {
  warning(
    "ZhenMeasure: ZhenM_default_config('legacy') is deprecated. ",
    "Use ZhenM_default_config('national_standard') instead. ",
    "Legacy QC method will be removed in V1.0.0.",
    call. = FALSE
  )
}
```

#### 3.3.2 更新文档

**文件**: `R/zhenm_config_defaults.R` (roxygen 注释)
**修改**: 在 `@param qc_method` 中标注 legacy 已弃用

```r
#' @param qc_method QC method: "national_standard" (recommended) or "legacy" (deprecated, will be removed in V1.0.0)
```

**文件**: `R/run_zhen_measure.R` (roxygen 注释)
**修改**: 同样标注 legacy 已弃用

#### 3.3.3 更新 NEWS.md

```markdown
## 0.2.7

- **Deprecation**: `qc_method = "legacy"` is now deprecated and will be removed in V1.0.0.
  Legacy QC method has been fully superseded by `national_standard` in all dimensions:
  weight QC (two-round RLM vs single-round), feed QC (9 physics flags + optional STL vs percentile),
  imputation (Kalman + Loess + FCR validation vs linear interpolation).
  Migration: simply change `qc_method = "legacy"` to `qc_method = "national_standard"`.
```

### 3.4 Phase 2 详细实施 (V0.3.0)

1. **标记 deprecated**: 在 `ZhenM_default_config("legacy")` 返回值中添加 `attr(result, "deprecated") <- TRUE`
2. **文档更新**: 所有 roxygen `@examples` 中移除 legacy 相关示例；man 页面添加 `\lifecycle{deprecated}` 标签
3. **测试更新**: Legacy 相关测试用例标记为 `skip_on_cran()` 或移除

### 3.5 Phase 3 详细实施 (V1.0.0)

**需移除的代码**:

| 文件 | Legacy 代码范围 | 行数 |
|------|----------------|------|
| `R/zhenm_qc_weight_standard.R` | `.qc_weight_standard_legacy()` 函数（L354-535） | ~180 |
| `R/zhenm_qc_feed_standard.R` | `.qc_feed_standard_legacy()` 函数（L218-331） | ~113 |
| `R/zhenm_daily_aggregate_filtered.R` | `has_legacy_weight` 分支（L159-165）+ Legacy flag 检测 | ~15 |
| `R/zhenm_impute.R` | `.impute_weight_legacy()` 函数（L119-138） | ~20 |
| `R/zhenm_impute_feed.R` | 整个文件 `.impute_feed_legacy()` | ~27 |
| `R/zhenm_config_defaults.R` | Legacy 配置块（L81-103）+ 警告代码 | ~25 |
| `R/run_zhen_measure.R` | Legacy 路由逻辑（L42-43, L188-189）+ 警告代码 | ~10 |
| `R/zhenm_qc_utils.R` | `.init_qc_flags()` 中的 Legacy flag 初始化 | ~10 |
| **合计** | | **~400** |

**需修改的接口**:

1. `run_zhen_measure()`: 移除 `qc_method` 参数中的 `"legacy"` 选项，仅保留 `"national_standard"`
2. `ZhenM_default_config()`: 移除 `qc_method` 参数，直接返回 national_standard 配置
3. `ZhenM_merge_config()`: 移除 legacy 配置的特殊处理

**需清理的依赖** (DESCRIPTION Imports):

| 包 | 当前状态 | 移除理由 |
|----|---------|---------|
| `nlme` | 列入 Imports 但未使用 | 无代码引用 |
| `quantreg` | 列入 Imports 但未使用 | 无代码引用 |
| `splines` | 列入 Imports 但未使用 | 无代码引用 |
| `zoo` | Legacy na.approx 使用 | V1.0 移除 Legacy 后检查是否仍有其他引用，若无则移除 |

---

## 4. 需要修改的文件清单

### Phase 1 (V0.2.7) -- 添加弃用警告

| # | 文件路径 | 修改内容 |
|---|---------|---------|
| 1 | `R/run_zhen_measure.R` | L39 后添加 legacy 弃用 `warning()` |
| 2 | `R/zhenm_config_defaults.R` | L81 legacy 分支添加弃用 `warning()`；更新 roxygen `@param` 注释 |
| 3 | `NEWS.md` | 添加 V0.2.7 弃用说明 |

### Phase 2 (V0.3.0) -- 功能冻结

| # | 文件路径 | 修改内容 |
|---|---------|---------|
| 4 | `R/zhenm_config_defaults.R` | Legacy 返回值添加 `deprecated` 属性 |
| 5 | `R/run_zhen_measure.R` | roxygen 添加 `\lifecycle{deprecated}` 标注 |
| 6 | `tests/testthat/*.R` | Legacy 相关测试用例 `skip()` 或移除 |
| 7 | `man/*.Rd` | 更新文档，标注 deprecated |

### Phase 3 (V1.0.0) -- 代码移除

| # | 文件路径 | 修改内容 |
|---|---------|---------|
| 8 | `R/zhenm_qc_weight_standard.R` | 移除 `.qc_weight_standard_legacy()` |
| 9 | `R/zhenm_qc_feed_standard.R` | 移除 `.qc_feed_standard_legacy()` |
| 10 | `R/zhenm_daily_aggregate_filtered.R` | 移除 `has_legacy_weight` 分支 |
| 11 | `R/zhenm_impute.R` | 移除 `.impute_weight_legacy()` |
| 12 | `R/zhenm_impute_feed.R` | 移除整个文件或清空 |
| 13 | `R/zhenm_config_defaults.R` | 移除 Legacy 配置块 |
| 14 | `R/run_zhen_measure.R` | 移除 `qc_method` 参数，简化路由 |
| 15 | `R/zhenm_qc_utils.R` | 清理 `.init_qc_flags()` 中的 Legacy flag |
| 16 | `DESCRIPTION` | 移除 `nlme`、`quantreg`、`splines`；检查 `zoo` |
| 17 | `NAMESPACE` | 重新生成，移除不再需要的 export/import |

---

## 5. 迁移指南（面向用户）

### 从 Legacy 迁移到 National Standard

**最小改动**（一行代码）:

```r
# 旧代码
result <- run_zhen_measure(data_path, data_type, format_path, qc_method = "legacy")

# 新代码
result <- run_zhen_measure(data_path, data_type, format_path, qc_method = "national_standard")
```

**预期变化**:

| 指标 | 变化方向 | 原因 |
|------|---------|------|
| 保留动物数 | **增加** | National 不使用 Gompertz 过度过滤（默认关闭） |
| FCR SD | **降低** | 加权平均 + Kalman 插补精度更高 |
| FCR NA | **减少至 0** | FCR 阶段验证防止极端插补值 |
| 运行时间 | 略增 | Kalman Filter 比 na.approx 稍慢（毫秒级差异） |

**可选增强**（长期试验、大数据集）:

```r
config <- ZhenM_default_config("national_standard")
config$national_standard$use_stl_feed <- TRUE   # 启用 STL 采食量异常检测
config$national_standard$use_gompertz <- TRUE    # 启用 Gompertz 体重异常检测
result <- run_zhen_measure(data_path, data_type, format_path, config = config)
```

---

## 6. 风险与缓解

| 风险 | 概率 | 影响 | 缓解措施 |
|------|------|------|---------|
| 外部用户依赖 Legacy 路径 | 低 | 中 | V0.2.7 添加警告，给用户 2 个版本的迁移窗口 |
| Legacy 代码移除后遗漏引用 | 低 | 高 | V1.0.0 移除前运行 `R CMD check` + 完整测试套件 |
| `zoo` 包移除后影响其他功能 | 极低 | 低 | 移除前 grep 确认无其他 `zoo::` 引用 |
| 用户不理解弃用警告 | 中 | 低 | 警告信息中包含迁移指南链接 |

---

## 附录: 评估数据来源

| 文件 | 内容 |
|------|------|
| `strategy_evaluation_decision.md` | 决策者框架：5 项策略的评估维度和初步决策 |
| `strategy_evaluation_report.md` | 评估者深度分析：源码比对 + 数据证据 + 理论分析 |
| `strategy_integration_test.md` | 测试者验证：实际 QC 数据的标记重叠和端到端指标 |
| `per_source/*.json` | 3 数据源的端到端指标 |
| `per_source/*_qc_flag_stats.csv` | 3 数据源的 QC 标记统计 |
| `per_source/*_diff.csv` | 3 数据源的逐步差异 |
| `baseline_metrics.json` | Baseline 指标（STL/Gompertz 整合前） |
| `final_decision_report.md` | STL/Gompertz 整合决策（V0.2.6） |
