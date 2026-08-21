# Legacy QC 方法全面审计报告

**审计日期**: 2026-06-22
**审计范围**: ZhenMeasure R 包中 Legacy 路径的完整实现
**审计版本**: V0.2.5 (commit 1155d9c)
**审计人**: Evaluator Agent

---

## 审计文件清单

| 文件 | 审计函数 |
|------|---------|
| `R/zhenm_qc_weight_standard.R` | `.qc_weight_standard_legacy()` (L320-493) |
| `R/zhenm_qc_feed_standard.R` | `.qc_feed_standard_legacy()` (L154-275) |
| `R/zhenm_daily_aggregate_filtered.R` | `ZhenM_standard_to_daily_filtered()` + `.apply_feed_lmm_correction()` |
| `R/zhenm_impute.R` | `.impute_weight_legacy()` (L121-138) |
| `R/zhenm_impute_feed.R` | `.impute_feed_legacy()` (L1-26) |
| `R/zhenm_phenotype_calc.R` | 全部表型计算函数（无 Legacy 特殊分支） |
| `R/zhenm_config_defaults.R` | `ZhenM_default_config("legacy")` (L67-89) |
| `R/run_zhen_measure.R` | 主入口 Legacy 分支逻辑 (L39-292) |
| `R/zhenm_qc_utils.R` | `.init_qc_flags()`, `.check_growth_fit()`, `.normalize_feed_range()` |

---

## 发现问题列表

### Critical (2个)

#### C-1: Legacy 采食量范围阈值单位不匹配 -- `flag_feed_out_of_range` 几乎标记所有记录

- **文件**: `R/zhenm_qc_feed_standard.R`, L156, L171-172
- **配置**: `R/zhenm_config_defaults.R`, L73 (`feed_intake_range = c(0, 6)`)

**问题描述**:

Legacy 配置定义 `feed_intake_range = c(0, 6)`，数值含义为千克 (0~6 kg/次访问)。但代码中 `feed_g` 列的单位是克（经 `zhenm_read_utils.R:359` 的 `.convert_unit_to_grams()` 转换后）。代码直接将 `feed_g`（克）与 `feed_range`（千克）比较：

```r
# L156: feed_range <- cfg$legacy$feed_intake_range   # c(0, 6) 即 0~6 kg
# L171-172:
dt[!is.na(feed_g), flag_feed_out_of_range :=
     feed_g < feed_range[1] | feed_g > feed_range[2]]
# 等价于: feed_g < 0 | feed_g > 6   （单位：克）
```

单次采食量典型值为 1000~3000g，全部 > 6，因此几乎所有有效采食记录都被错误标记为 `flag_feed_out_of_range = TRUE`。

**影响**:
- `flag_feed_out_of_range` 传播到日聚合层 (`ZhenM_standard_to_daily_filtered` L134): 一旦当日有任一记录被标记，该日 `daily_feed_g` 直接置为 `NA`
- 导致几乎所有日采食量为 `NA`，全部依赖插补填充
- 后续 LMM 校正、表型计算全部基于插补值，数据质量严重退化

**修复建议**:

```r
# 方案 A: 对齐体重 QC 的做法，调用已有的单位标准化函数
feed_range <- .normalize_feed_range(cfg$legacy$feed_intake_range)

# 方案 B: 在配置中直接使用克为单位
# feed_intake_range = c(0, 6000)  # 克
```

推荐方案 A，与 `weight_range` 的处理方式保持一致。注意 `.normalize_feed_range()` 已在 `zhenm_qc_utils.R:38-44` 中定义但从未被调用。

---

#### C-2: `.init_qc_flags()` Legacy 模式在 Weight QC 阶段越权初始化 Feed QC 标志列

- **文件**: `R/zhenm_qc_utils.R`, L116-135

**问题描述**:

`.init_qc_flags(dt, method = "legacy")` 在 Weight QC（Step 3）中被调用，但初始化的标志列包含：

```r
"legacy" = c(
  "flag_weight_out_of_range", "flag_SD_WT", "flag_RLM_WT",
  "flag_Gompertz_WT", "flag_STL_FI", "is_imputed_feed",   # <-- Feed QC 列
  "is_outlier_wt", "is_outlier_fi_stl"                     # <-- 从未使用的列
)
```

`flag_STL_FI` 和 `is_imputed_feed` 是 Feed QC（Step 4）的标志，在 Weight QC 阶段预初始化虽然不会导致运行时错误（均初始化为 FALSE），但违反了职责分离原则。更关键的是 `is_outlier_fi_stl` 为死代码——该列在 `.init_qc_flags` 中创建后，整个代码库中再无任何位置对其赋值或读取。

**影响**: Low。不导致计算错误，但造成混淆和维护隐患。

**修复建议**:

```r
# 将 .init_qc_flags 的 legacy flags 拆分为仅体重相关
"legacy" = c(
  "flag_weight_out_of_range", "flag_SD_WT", "flag_RLM_WT",
  "flag_Gompertz_WT", "is_outlier_wt"
)
# 移除 "flag_STL_FI", "is_imputed_feed", "is_outlier_fi_stl"
# flag_STL_FI 已在 .qc_feed_standard_legacy() L205 中独立初始化
# is_imputed_feed 已在各 impute 函数中独立初始化
```

---

### High (3个)

#### H-1: LMM 采食量校正在 Legacy 模式下实质无效

- **文件**: `R/zhenm_daily_aggregate_filtered.R`, L218-362 (`.apply_feed_lmm_correction`)

**问题描述**:

LMM 校正模块依赖 9 个来自 National Standard Feed QC 的单记录异常标志（`flag_duration_negative`, `flag_speed_too_slow` 等）。Legacy Feed QC 不产生这些标志列，而是使用 `flag_feed_out_of_range`, `flag_percentile_low/high`, `flag_STL_FI`。

LMM 校正流程：
1. L230-235: 遍历 9 个 err_flags 检查 `is_feed_normal_record` -- Legacy 数据中这些列不存在，全部跳过
2. `is_feed_normal_record` 始终为 `TRUE`，`normal_feed_sum` = 全日所有记录的 feed_g 之和
3. L293-303: 检查 `has_*` 标志的变异性 -- 全部为 `has_flag_xxx = FALSE`（因为 flag 不存在），无变异性
4. 活跃标志 `active_flags` 为空，`correction_g` 始终为 0
5. L339: `daily_feed_g := daily_feed_g_corrected`（= `normal_feed_sum + 0`）

LMM 模型本身会运行（如果样本量 > 30），但 correction 恒为 0，等于没有校正。

**影响**: High。Legacy 模式下日采食量未经 LMM 校正，直接使用原始求和值。对于存在设备异常（如重复打卡、采食中断）的数据，无法自动修正。

**修复建议**:

选项 A（推荐）: 将 Legacy 的 `flag_feed_out_of_range`, `flag_percentile_low/high`, `flag_STL_FI` 映射到 LMM 的 feature 空间：
```r
# 在 .apply_feed_lmm_correction 中增加 Legacy 标志的适配层
legacy_flags <- c("flag_feed_out_of_range", "flag_percentile_low",
                  "flag_percentile_high", "flag_STL_FI")
for (flg in legacy_flags) {
  if (flg %in% names(raw_dt)) {
    # 将 Legacy 标志作为 LMM 的额外协变量
    ...
  }
}
```

选项 B: 在 Legacy 模式下跳过 LMM 校正并记录日志，避免无意义计算：
```r
if (qc_method == "legacy") {
  message("LMM correction skipped in legacy mode (no compatible flags)")
  return(dt)
}
```

---

#### H-2: Legacy 体重和采食量插补无法处理首尾缺失

- **文件**: `R/zhenm_impute.R`, L121-138 (`.impute_weight_legacy`); `R/zhenm_impute_feed.R`, L1-26 (`.impute_feed_legacy`)

**问题描述**:

Legacy 插补使用 `zoo::na.approx(daily_weight_g, na.rm = FALSE)` 进行线性插值。`na.approx` 仅在已知数据点之间插值，不进行外推。如果某个体在试验期首日或末日的 `daily_weight_g` / `daily_feed_g` 为 `NA`，则插补后仍为 `NA`。

对比 National Standard 方法：
- 体重: 使用 Kalman Filter (`imputeTS::na_kalman`) + `rule = 2` 外推，可处理端点缺失
- 采食量: 使用 Loess / 线性回归外推，可处理端点缺失

**影响**: High。端点缺失的个体在表型计算时会丢失起始/终止体重或采食量，导致 `ADG_g`, `ADFI_g`, `FCR` 计算为 `NA`。

**修复建议**:

```r
.impute_weight_legacy <- function(daily_records, cfg) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))
  if (!"is_imputed_wt" %in% names(dt)) dt[, is_imputed_wt := FALSE]

  dt[, was_missing_weight := is.na(daily_weight_g)]

  # 使用 rule = 2 进行端点外推（最近邻延伸）
  dt[, `:=`(
    daily_weight_g = zoo::na.approx(daily_weight_g, na.rm = FALSE, rule = 2),
    is_imputed_wt = was_missing_weight
  ), by = animal_id]

  dt[, was_missing_weight := NULL]
  dt
}
```

注意: 采食量端点外推需要更谨慎（采食量可以为 0），建议使用 `rule = 2` 并将外推值限制为非负。

---

#### H-3: Growth Curve R-squared 阈值在 Legacy 模式下回退到 National Standard 的 0.99

- **文件**: `R/run_zhen_measure.R`, L141

**问题描述**:

```r
min_r2 <- cfg[[qc_method]]$growth_curve_r2_min %||% cfg$national_standard$growth_curve_r2_min
```

Legacy 配置 (`cfg$legacy`) 中没有定义 `growth_curve_r2_min`，因此 `%||%` 回退到 `cfg$national_standard$growth_curve_r2_min = 0.99`。

0.99 是一个非常严格的 R-squared 阈值。National Standard 路径通过两轮 RLM 去噪 + 加权平均日体重来达到此精度。Legacy 路径仅使用 SD 阈值 + 可选 RLM/Gompertz 去噪，日体重为简单中位数，噪声更大，R-squared 通常较低。

**影响**: High。使用 Legacy QC 的动物更容易被 Step 5.5 的 R-squared 检查误删（`flag_growth_curve_poor = TRUE`），尤其在数据质量一般时可能导致大量个体被剔除。

**修复建议**:

在 Legacy 默认配置中定义合适的 R-squared 阈值：
```r
# R/zhenm_config_defaults.R, legacy 配置中添加:
growth_curve_r2_min = 0.95,  # 或根据实际数据分布校准
```

并在 `run_zhen_measure.R` 中确保优先使用方法特定配置：
```r
min_r2 <- cfg[[qc_method]]$growth_curve_r2_min
if (is.null(min_r2)) min_r2 <- cfg$national_standard$growth_curve_r2_min %||% 0.95
```

---

### Medium (3个)

#### M-1: Legacy Feed QC 使用百分位数法可能在小样本个体上产生不稳定结果

- **文件**: `R/zhenm_qc_feed_standard.R`, L186-202

**问题描述**:

```r
daily_feed[!is.na(daily_total_feed), `:=`(
  is_low_outlier = daily_total_feed < quantile(daily_total_feed, p_low, na.rm = TRUE),
  is_high_outlier = daily_total_feed > quantile(daily_total_feed, 1 - p_high, na.rm = TRUE)
), by = animal_id]
```

当某个体的有效日记录数很少（例如 < 20 天），分位数估计不稳定。极端情况下，若个体仅有 40 天数据，P2.5 和 P97.5 各对应 1 天，阈值受极端值影响大。

**影响**: Medium。小样本个体可能被错误标记更多异常日。

**修复建议**:

添加最小样本量检查：
```r
daily_feed[, n_valid_days := sum(!is.na(daily_total_feed)), by = animal_id]
daily_feed[n_valid_days >= 30, `:=`(
  is_low_outlier = daily_total_feed < quantile(daily_total_feed, p_low, na.rm = TRUE),
  is_high_outlier = daily_total_feed > quantile(daily_total_feed, 1 - p_high, na.rm = TRUE)
)]
daily_feed[n_valid_days < 30, `:=`(is_low_outlier = FALSE, is_high_outlier = FALSE)]
```

---

#### M-2: Legacy 体重 QC 的 RLM 和 Gompertz 是可选模块，依赖额外配置参数

- **文件**: `R/zhenm_qc_weight_standard.R`, L359, L415

**问题描述**:

RLM 模块需要 `cfg$legacy$use_rlm == TRUE` 且 `"rlm_weight_thresh" %in% names(cfg$legacy)` 才执行。
Gompertz 模块需要 `cfg$legacy$use_gompertz == TRUE` 且 `"min_obs_for_wt" %in% names(cfg$legacy)` 才执行。

当前默认配置中 `use_rlm = TRUE`, `rlm_weight_thresh = 0.5`, `use_gompertz = TRUE`, `min_obs_for_wt = 60` 均已定义，所以默认情况下两个模块都会执行。

但如果用户传入自定义 `config` 并覆盖了 `legacy` 部分但未包含这些参数，则高级 QC 模块会被静默跳过。

**影响**: Medium。功能降级但不会出错。用户可能不知道高级 QC 被跳过。

**修复建议**:

在函数入口添加警告日志：
```r
if (!cfg$legacy$use_rlm || !("rlm_weight_thresh" %in% names(cfg$legacy))) {
  log_detail("WARNING: RLM weight QC skipped (use_rlm=FALSE or rlm_weight_thresh not configured)")
}
if (!cfg$legacy$use_gompertz || !("min_obs_for_wt" %in% names(cfg$legacy))) {
  log_detail("WARNING: Gompertz weight QC skipped (use_gompertz=FALSE or min_obs_for_wt not configured)")
}
```

---

#### M-3: `.normalize_feed_range()` 已定义但从未被调用

- **文件**: `R/zhenm_qc_utils.R`, L38-44

**问题描述**:

`.normalize_feed_range()` 函数已实现，用于将 feed 范围从 kg 转换为 g（逻辑与 `.normalize_weight_range()` 一致）。但在整个代码库中没有任何调用点。

- Legacy Feed QC (`zhenm_qc_feed_standard.R:156`) 直接使用 `cfg$legacy$feed_intake_range`
- National Standard Feed QC 不使用 `feed_intake_range`（使用速度/时长阈值）

这直接导致了 C-1 问题。

**修复建议**: 在 Legacy Feed QC 中调用 `.normalize_feed_range()`（见 C-1 修复方案）。

---

### Low (3个)

#### L-1: `is_outlier_fi_stl` 为死代码标志列

- **文件**: `R/zhenm_qc_utils.R`, L126

**问题描述**:

`.init_qc_flags()` Legacy 模式初始化了 `is_outlier_fi_stl` 列，但整个代码库中无任何代码对此列赋值或读取。实际使用的 STL 标志列名为 `flag_STL_FI`。

**修复建议**: 从 `.init_qc_flags()` 的 legacy flags 列表中移除 `"is_outlier_fi_stl"`。

---

#### L-2: Legacy Feed QC 的日志消息中百分位数描述不直观

- **文件**: `R/zhenm_qc_feed_standard.R`, L184

**问题描述**:

```r
log_detail(paste0("Percentile thresholds: Low P", p_low * 100, ", High P", (1 - p_high) * 100))
```

当 `p_low = 0.025`, `p_high = 0.01` 时，输出为 `"Low P2.5, High P99"`。虽然数值正确，但 `p_high = 0.01` 表示上尾概率 1%，对应的上分位数是 P99。变量命名 `p_high` 容易被误读为"高分位数"（P99），实际含义是"上尾概率"。

**修复建议**: 改进日志描述或添加注释说明变量含义。

---

#### L-3: `ZhenM_calc_phenotypes()` 在 Legacy QC + `standard_fcr` 模式下会返回空结果

- **文件**: `R/zhenm_phenotype_calc.R`, L309-332

**问题描述**:

如果用户在 Legacy QC 模式下选择 `phenotype_method = "standard_fcr"`，函数访问 `cfg$national_standard$test_weight_range`（在 Legacy 配置中为 `NULL`），导致 `test_range_g` 为 `NULL`，`dt_test` 为空 data.table。

这不是一个运行时错误（R 正确处理了 NULL 比较），但会产生一个无预警的空结果。

**影响**: Low。默认情况下 Legacy 使用 `phenotype_method = "report"`，不会触发此路径。仅在用户显式指定 `standard_fcr` 时出现。

**修复建议**:

```r
.calc_phenotypes_standard_fcr <- function(daily_records, cfg) {
  test_range <- cfg$national_standard$test_weight_range
  if (is.null(test_range)) {
    warning("standard_fcr requires national_standard config. ",
            "Falling back to 'report' method.")
    return(.calc_phenotypes_report(daily_records, cfg))
  }
  # ... existing code
}
```

---

## V0.2.5 已修复 Bug 在 Legacy 路径中的状态

| Bug 编号 | 描述 | Legacy 路径状态 |
|----------|------|----------------|
| V0.2.4-C1 | `<<-` 赋值 Bug (`zhenm_qc_feed_standard.R:98,114`) | **不适用** -- 仅影响 National Standard Feed QC（量化回归代码），Legacy 使用百分位数法 |
| V0.2.4-C2 | 数值索引作逻辑用 (`zhenm_qc_weight_standard.R:458`) | **已修复** -- Legacy Gompertz 模块 L458 使用 `animal_id == id & record_date %in%` 逻辑子集 |
| V0.2.4-C3 | 硬编码 config 访问 (`run_zhen_measure.R:141`) | **已修复** -- 使用 `cfg[[qc_method]]$growth_curve_r2_min %||%` 回退（但触发 H-3 问题） |
| V0.2.4-C4 | Legacy 模式缺少体重插补 | **已修复** -- L189 调用 `ZhenM_impute_data(daily_data, "legacy", cfg)` 同时处理体重和采食量 |
| V0.2.4-H5 | FCR 除零 | **已修复** -- L357 使用 `ifelse(stage_gain > 0, ...)` |
| V0.2.4-L6 | `formula` 变量遮蔽 | **已修复** -- 重命名为 `rlm_fit` |
| V0.2.4-L7 | `c` 变量遮蔽 | **已修复** -- 重命名为 `col_name` |

---

## Legacy 代码整体质量评估

### 优点

1. **结构清晰**: Legacy 函数与 National Standard 函数完全分离，通过 `if/else` 在入口函数中分发，互不干扰
2. **向后兼容**: 保留了 V0.2.0 的 SD 阈值 + 百分位数 + STL 方法，同时增加了 RLM 和 Gompertz 高级模块
3. **可配置性好**: 高级模块通过 `use_stl`, `use_rlm`, `use_gompertz` 开关控制，参数可调
4. **错误处理**: Gompertz NLS 拟合和 STL 分解都有 `tryCatch` 包裹，失败时静默跳过
5. **V0.2.5 修复覆盖**: 7 个已知 Bug 中，与 Legacy 路径相关的 Bug 均已正确修复

### 问题总结

| 严重级别 | 数量 | 关键问题 |
|----------|------|----------|
| Critical | 2 | 采食量范围单位不匹配; QC 标志越权初始化 |
| High | 3 | LMM 校正无效; 首尾缺失无法插补; R-squared 阈值过严 |
| Medium | 3 | 百分位数小样本不稳定; 高级模块静默跳过; 未调用标准化函数 |
| Low | 3 | 死代码标志; 日志描述; 表型方法兼容性 |

### 优先修复建议

1. **最优先 (C-1)**: 修复 `feed_intake_range` 单位不匹配。这是一个静默数据损坏 Bug，会导致所有日采食量依赖插补值，严重影响表型计算准确性。修复方案简单（调用已有的 `.normalize_feed_range()`）。

2. **次优先 (H-3)**: 在 Legacy 配置中添加 `growth_curve_r2_min = 0.95`，避免过严的 R-squared 阈值导致大量个体被误删。

3. **第三优先 (H-1)**: 决定 Legacy 模式下 LMM 校正的策略——要么适配 Legacy 标志到 LMM，要么显式跳过并记录日志。

### 代码可维护性评分: 7/10

代码结构合理、注释充分（中文注释），函数职责划分清晰。主要扣分项为 C-1（单位处理不一致）和跨模块的配置传递耦合（Legacy 模块需要了解 National Standard 的配置结构才能正确回退）。
