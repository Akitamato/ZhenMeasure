# STL & Gompertz 整合到 National Standard 方法的设计方案

**设计日期**: 2026-06-22
**设计依据**: Legacy 代码审计报告、3 数据源对比测试结果、决策报告
**设计人**: 评估者 Agent

---

## 1. 研究摘要

### 1.1 Legacy STL 实现细节

**文件**: `R/zhenm_qc_feed_standard.R`, `.qc_feed_standard_legacy()`, L207-255

- **输入**: 每个动物的日采食量时间序列 (`daily_feed_g = sum(feed_g, na.rm = TRUE)` by `animal_id, record_date`)
- **前置条件**: `cfg$legacy$use_stl == TRUE` 且 `"min_obs_for_ts" %in% names(cfg$legacy)` (默认 `min_obs_for_ts = 100`)
- **STL 参数**: `frequency = 7` (周周期), `s.window = "periodic"`, `robust = TRUE`
- **异常阈值**: 残差的 MAD (Median Absolute Deviation) 的 3 倍, 即 `|resid| > 3 * mad(resid)`
- **输出**: `flag_STL_FI` 列, 标记异常日期的所有记录
- **与日聚合的关系**: STL 在日聚合前运行(标准记录级别), 但计算基于日汇总值; 标记回写到标准记录级别

**关键代码片段**:
```r
ts_obj <- stats::ts(y, frequency = 7)
stl_fit <- stats::stl(ts_obj, s.window = "periodic", robust = TRUE)
resid <- stl_fit$time.series[, "remainder"]
mad_val <- stats::mad(resid, na.rm = TRUE)
outliers <- abs(resid) > 3 * mad_val
```

### 1.2 Legacy Gompertz 实现细节

**文件**: `R/zhenm_qc_weight_standard.R`, `.qc_weight_standard_legacy()`, L414-467

- **输入**: 每个动物的每日中位数体重 (`median_weight_per_day`, 单位 g, 拟合时转为 kg)
- **前置条件**: `cfg$legacy$use_gompertz == TRUE` 且 `"min_obs_for_wt" %in% names(cfg$legacy)` (默认 `min_obs_for_wt = 60`)
- **模型公式**: `y = A * exp(-B * exp(-C * x))` (简化 Gompertz)
- **拟合方法**: `stats::nls()`, 初始值 `A_init = max(y) * 1.1`, `B_init = 2`, `C_init = 0.05`
- **异常阈值**: 残差的 MAD 的 4 倍, 即 `|resid| > 4 * mad(resid)`
- **输出**: `flag_Gompertz_WT` 列, 标记异常日期的所有记录
- **R-squared**: 未显式计算, 仅通过 MAD 阈值判断异常点

**关键代码片段**:
```r
gompertz_fit <- stats::nls(y[valid] ~ A * exp(-B * exp(-C * x[valid])),
                           start = list(A = A_init, B = B_init, C = C_init),
                           control = stats::nls.control(maxiter = 100, warnOnly = TRUE))
pred <- predict(gompertz_fit, newdata = list(x = x[valid]))
resid <- y[valid] - pred
outliers_logical <- abs(resid) > 4 * mad_val
```

### 1.3 National Feed QC 结构

**文件**: `R/zhenm_qc_feed_standard.R`, `.qc_feed_standard_national()`, L22-150

9 种异常标记(按计算顺序):
1. `flag_duration_negative` -- 时长 < 0
2. `flag_duration_too_long` -- 时长 > `duration_max` (1800s)
3. `flag_duration_zero_with_feed` -- 时长 = 0 但有采食量
4. `flag_speed_too_slow` -- 速度 <= `speed_min` (2 g/min)
5. `flag_speed_too_fast` -- 速度 > `speed_max` (170 g/min) 且 feed >= `feed_extreme_threshold` (50g)
6. `flag_speed_extreme_low_feed` -- 速度 > `speed_extreme` (500 g/min) 且 0 < feed < 50g
7. `flag_speed_zero_long_duration` -- 速度 = 0 且时长 > 500s
8. `flag_feed_negative` -- 采食量 < 0
9. `flag_feed_too_high` -- 单次采食量 > 个体日采食量 P99

汇总标记: `is_outlier_feed = flag_feed_negative | flag_feed_too_high | flag_duration_negative | ...`

### 1.4 National Weight QC 结构

**文件**: `R/zhenm_qc_weight_standard.R`, `.qc_weight_standard_national()`, L99-316

两轮 RLM 流程:
1. **第一轮 RLM** (单记录级别): 对每个动物的所有体重记录拟合 `weight ~ day + day^2`, 获取 RLM 权重
2. 权重 <= `weight_threshold` (0.25) 的记录标记为 `flag_weight_low`
3. 若某日所有记录都被标记, 该日全部置为 NA
4. 计算加权平均日体重: `sum(cleaned_weight * rlm_weight) / sum(rlm_weight)`
5. **第二轮 RLM** (日体重级别): 对加权平均日体重拟合 `daily_weight ~ day + day^2`
6. 权重 <= `weight_threshold` 的日标记为 `flag_daily_weight_low`
7. 拟合失败或数据不足的动物标记为 `flag_growth_curve_poor`

汇总标记: `is_outlier_wt = flag_weight_out_of_range | flag_weight_low | flag_daily_weight_low | flag_growth_curve_poor`

### 1.5 LMM 校正模块

**文件**: `R/zhenm_daily_aggregate_filtered.R`, `.apply_feed_lmm_correction()`, L218-362

9 个 err_flags:
```r
err_flags <- c("flag_duration_negative", "flag_duration_too_long",
               "flag_duration_zero_with_feed", "flag_speed_too_slow",
               "flag_speed_too_fast", "flag_speed_extreme_low_feed",
               "flag_speed_zero_long_duration", "flag_feed_negative",
               "flag_feed_too_high")
```

流程:
1. 标记 `is_feed_normal_record`: 9 个 flag 全为 FALSE 的记录为正常
2. 聚合 `normal_feed_sum`: 正常记录的日采食量之和
3. 构建 `has_*` 特征: 每个 flag 在当日是否有发生 (0/1)
4. 构建 LMM 公式: `normal_feed_sum ~ I(daily_weight_g/1000) + I(adg_g/1000) + has_flag_xxx + ... + (1|animal_id)`
5. 计算校正值: `correction_g = sum(|beta_i| * has_flag_i)` for active flags
6. 输出: `daily_feed_g = normal_feed_sum + correction_g`

---

## 2. 整合方案设计

### 方案 A: STL 整合到 National Feed QC

#### 2.1 目标

在 National Standard 的 9 种单记录异常标记之外, 增加第 10 种基于时间序列的异常标记 `flag_STL_FI`, 检测偏离时间趋势的异常日采食量。

#### 2.2 技术可行性: 高

- STL 分解的输入是日采食量时间序列, 与 National 的日聚合步骤自然衔接
- STL 标记作用于日级别, 可以在日聚合后、LMM 校正前执行
- `stats::stl` 是 R 基础包函数, 无额外依赖

#### 2.3 代码修改计划

**文件 1: `R/zhenm_config_defaults.R`**

在 `national_standard` 配置中添加:
```r
# STL 时间序列采食量 QC (可选)
use_stl_feed = FALSE,           # 默认关闭, 需显式启用
stl_period = 7,                 # STL 周期 (周)
stl_s_window = "periodic",      # 季节性窗口
stl_robust = TRUE,              # 鲁棒拟合
stl_mad_multiplier = 3,         # MAD 异常阈值倍数
stl_min_obs = 30                # 最少观测天数
```

**文件 2: `R/zhenm_qc_feed_standard.R`**

在 `.qc_feed_standard_national()` 函数末尾 (L114 `is_outlier_feed` 计算之前), 添加 STL 检测逻辑:

```r
# Phase 10: STL 时间序列采食量异常检测 (可选)
dt[, flag_STL_FI := FALSE]

if (cfg$national_standard$use_stl_feed &&
    "stl_min_obs" %in% names(cfg$national_standard)) {

  log_detail("Executing STL time series feed QC (National Standard)...")

  # 聚合到日级别
  daily_for_stl <- dt[, .(daily_feed_g = sum(feed_g, na.rm = TRUE)),
                      by = .(animal_id, record_date)]
  data.table::setorder(daily_for_stl, animal_id, record_date)

  ids <- unique(daily_for_stl$animal_id)
  stl_processed <- 0
  stl_cfg <- cfg$national_standard

  for (id in ids) {
    sub <- daily_for_stl[animal_id == id]
    if (nrow(sub) < stl_cfg$stl_min_obs) next

    y <- sub$daily_feed_g
    if (sum(!is.na(y)) < stl_cfg$stl_min_obs) next

    ts_obj <- tryCatch(
      stats::ts(y, frequency = stl_cfg$stl_period),
      error = function(e) NULL
    )
    if (is.null(ts_obj)) next

    stl_fit <- tryCatch(
      stats::stl(ts_obj, s.window = stl_cfg$stl_s_window,
                 robust = stl_cfg$stl_robust),
      error = function(e) NULL
    )

    if (!is.null(stl_fit)) {
      resid <- stl_fit$time.series[, "remainder"]
      mad_val <- stats::mad(resid, na.rm = TRUE)
      if (mad_val > 0) {
        outliers <- abs(resid) > stl_cfg$stl_mad_multiplier * mad_val
        outlier_dates <- sub$record_date[outliers]
        dt[animal_id == id & record_date %in% outlier_dates,
           flag_STL_FI := TRUE]
        stl_processed <- stl_processed + 1
      }
    }
  }

  log_detail(paste0("STL QC processed individuals: ", stl_processed))
  n_stl <- sum(dt$flag_STL_FI, na.rm = TRUE)
  log_detail(paste0("flag_STL_FI: ", n_stl))
}
```

更新 `is_outlier_feed` 计算 (L115-119):
```r
dt[, is_outlier_feed := flag_feed_negative | flag_feed_too_high |
     flag_duration_negative | flag_duration_too_long |
     flag_duration_zero_with_feed | flag_speed_too_slow |
     flag_speed_too_fast | flag_speed_extreme_low_feed |
     flag_speed_zero_long_duration | flag_STL_FI]
```

**文件 3: `R/zhenm_daily_aggregate_filtered.R`**

在 `.apply_feed_lmm_correction()` 的 `err_flags` 定义中添加 `flag_STL_FI`:

```r
err_flags <- c("flag_duration_negative", "flag_duration_too_long",
               "flag_duration_zero_with_feed", "flag_speed_too_slow",
               "flag_speed_too_fast", "flag_speed_extreme_low_feed",
               "flag_speed_zero_long_duration", "flag_feed_negative",
               "flag_feed_too_high", "flag_STL_FI")  # 新增
```

同时, 在 `is_feed_normal_record` 的标记逻辑中, STL 标记的记录也应被视为异常 (不计入 `normal_feed_sum`):
```r
# flag_STL_FI 标记的记录也排除出正常记录集合
# (现有循环已自动处理, 因为 flag_STL_FI 已加入 err_flags)
```

**文件 4: `R/zhenm_qc_utils.R`**

在 `.init_qc_flags()` 的 `national_standard` flags 中添加 `flag_STL_FI`:
```r
"national_standard" = c(
  "flag_weight_out_of_range", "flag_weight_low",
  "flag_daily_weight_low", "flag_growth_curve_poor",
  "flag_STL_FI",  # 新增
  "is_outlier_wt"
)
```

#### 2.4 数据流影响

```
标准记录 → Feed QC (9种标记 + STL) → 日聚合(排除异常记录) → LMM校正(10个特征) → 日采食量
```

STL 标记的日采食量记录会被排除出 `normal_feed_sum`, 同时 `has_flag_STL_FI` 作为 LMM 的第 10 个协变量参与校正。

#### 2.5 预期效果

- **正面**: 能检测到偏离时间趋势的异常日(如饲料突变、设备间歇性故障), 这是 National 的单记录规则无法捕捉的
- **风险**: STL 在 YANGXIANG 数据上标记了 11,084 条 (5.7%), 可能过度标记正常波动
- **缓解**: 默认关闭 (`use_stl_feed = FALSE`), 用户需显式启用; `stl_min_obs = 30` 过滤小样本个体

#### 2.6 代码修改量

- 修改 4 个文件
- 新增约 50 行代码
- 配置新增 6 个参数

---

### 方案 B: Gompertz 整合到 National Weight QC

#### 2.7 目标

在 National Standard 的两轮 RLM + 二次多项式路径之后, 增加 Gompertz 曲线拟合作为额外的质量检查, 标记拟合异常的动物。

#### 2.8 技术可行性: 中

- Gompertz 需要足够的数据点 (默认 `min_obs_for_wt = 60` 天), 对数据量有硬性要求
- NLS 拟合可能失败, 需要完善的错误处理
- Gompertz 作用于日体重级别, 可以在第二轮 RLM 之后执行

#### 2.9 代码修改计划

**文件 1: `R/zhenm_config_defaults.R`**

在 `national_standard` 配置中添加:
```r
# Gompertz 生长曲线 QC (可选)
use_gompertz = FALSE,           # 默认关闭
gompertz_min_obs = 60,          # 最少观测天数
gompertz_mad_multiplier = 4,    # MAD 异常阈值倍数
gompertz_maxiter = 100          # NLS 最大迭代次数
```

**文件 2: `R/zhenm_qc_weight_standard.R`**

在 `.qc_weight_standard_national()` 的主循环中, 第二轮 RLM 之后 (L268 `flag_daily_weight_low` 赋值之后), 添加 Gompertz 拟合:

```r
# ===== Step 6: Gompertz 生长曲线检查 (可选) =====
if (cfg$national_standard$use_gompertz &&
    "gompertz_min_obs" %in% names(cfg$national_standard) &&
    nrow(daily_weight_data) >= cfg$national_standard$gompertz_min_obs) {

  x_gomp <- seq_len(nrow(daily_weight_data))
  y_gomp <- daily_weight_data$daily_weight / 1000  # 转为 kg
  valid_gomp <- !is.na(y_gomp)

  if (sum(valid_gomp) >= cfg$national_standard$gompertz_min_obs) {
    gompertz_fit <- tryCatch({
      A_init <- max(y_gomp[valid_gomp], na.rm = TRUE) * 1.1
      B_init <- 2
      C_init <- 0.05

      stats::nls(y_gomp[valid_gomp] ~ A * exp(-B * exp(-C * x_gomp[valid_gomp])),
                 start = list(A = A_init, B = B_init, C = C_init),
                 control = stats::nls.control(
                   maxiter = cfg$national_standard$gompertz_maxiter,
                   warnOnly = TRUE))
    }, error = function(e) NULL)

    if (!is.null(gompertz_fit)) {
      pred <- predict(gompertz_fit, newdata = list(x = x_gomp[valid_gomp]))
      resid <- y_gomp[valid_gomp] - pred
      mad_val <- stats::mad(resid, na.rm = TRUE)

      if (mad_val > 0) {
        outliers_logical <- abs(resid) >
          cfg$national_standard$gompertz_mad_multiplier * mad_val
        outlier_dates <- daily_weight_data$record_date[valid_gomp][outliers_logical]

        dt[animal_id == id & record_date %in% outlier_dates,
           flag_Gompertz_WT := TRUE]
      }
    }
  }
}
```

更新 `is_outlier_wt` 计算 (L280):
```r
dt[, is_outlier_wt := flag_weight_out_of_range | flag_weight_low |
     flag_daily_weight_low | flag_growth_curve_poor | flag_Gompertz_WT]
```

**文件 3: `R/zhenm_qc_utils.R`**

在 `.init_qc_flags()` 的 `national_standard` flags 中添加 `flag_Gompertz_WT`:
```r
"national_standard" = c(
  "flag_weight_out_of_range", "flag_weight_low",
  "flag_daily_weight_low", "flag_growth_curve_poor",
  "flag_Gompertz_WT",  # 新增
  "flag_STL_FI",
  "is_outlier_wt"
)
```

#### 2.10 数据流影响

```
标准记录 → Weight QC (RLM两轮 + Gompertz) → 日聚合(排除异常记录) → 生长曲线R²检查 → 表型计算
```

Gompertz 标记的日期对应的日体重会被排除, 但不会触发整群动物删除 (与 Legacy 不同, National 的 Gompertz 仅标记异常点, 不判断 R-squared 是否过低)。

#### 2.11 预期效果

- **正面**: Gompertz 曲线比二次多项式更符合猪只生长的 S 型生物学模型, 能检测到偏离生长趋势的异常体重
- **风险**: NLS 拟合对初始值敏感, 可能拟合失败; 在噪声数据上可能标记过多异常点
- **缓解**: 默认关闭 (`use_gompertz = FALSE`); 仅标记异常点, 不删除动物 (与 Legacy 的激进策略不同)

#### 2.12 代码修改量

- 修改 3 个文件
- 新增约 40 行代码
- 配置新增 4 个参数

---

### 方案 C: 同时整合 A 和 B

#### 2.13 技术可行性: 高

方案 A 和 B 互不依赖, 可以独立启用/禁用。整合后的配置结构:

```r
national_standard = list(
  # ... 现有参数 ...

  # 新增: STL 采食量 QC
  use_stl_feed = FALSE,
  stl_period = 7,
  stl_s_window = "periodic",
  stl_robust = TRUE,
  stl_mad_multiplier = 3,
  stl_min_obs = 30,

  # 新增: Gompertz 体重 QC
  use_gompertz = FALSE,
  gompertz_min_obs = 60,
  gompertz_mad_multiplier = 4,
  gompertz_maxiter = 100
)
```

#### 2.14 预期效果

| 维度 | 仅 National (现状) | + STL (方案 A) | + Gompertz (方案 B) | + 两者 (方案 C) |
|------|-------------------|----------------|---------------------|-----------------|
| 采食量异常检测 | 9 种物理标记 | 9 种 + 1 种时间序列 | 9 种物理标记 | 9 种 + 1 种时间序列 |
| 体重异常检测 | RLM + 二次多项式 | 同左 | RLM + 二次 + Gompertz | 同左 |
| LMM 校正特征 | 9 个 | 10 个 | 9 个 | 10 个 |
| 动物保留率 | 基准 | 可能略降 | 可能略降 | 可能略降 |
| FCR 稳定性 | 基准 | 可能提升 | 可能提升 | 可能提升 |

#### 2.15 代码修改量汇总

- 修改 4 个文件 (`zhenm_config_defaults.R`, `zhenm_qc_feed_standard.R`, `zhenm_qc_weight_standard.R`, `zhenm_qc_utils.R`)
- 修改 1 个文件的现有逻辑 (`zhenm_daily_aggregate_filtered.R` -- err_flags 扩展)
- 新增约 100 行代码
- 配置新增 10 个参数

---

## 3. 三个方案的评估对比

### 3.1 方案 A (STL 整合到 Feed QC)

| 维度 | 评估 |
|------|------|
| 技术可行性 | **高** -- STL 是纯日级别操作, 与现有 National Feed QC 的单记录标记互补 |
| 预期效果 | **中** -- 能捕捉时间趋势异常, 但大规模测试中 STL 标记过多 (5.7%), 可能降低动物保留率 |
| 代码修改量 | **小** -- 50 行新代码, 4 个文件 |
| 风险点 | (1) STL 默认关闭, 用户启用后可能过度标记; (2) frequency=7 假设周周期, 对非周期性数据无效; (3) MAD 阈值 3 倍可能需要针对不同数据源校准 |
| 对 LMM 的影响 | 新增 `has_flag_STL_FI` 作为第 10 个协变量, 增强 LMM 校正能力 |

### 3.2 方案 B (Gompertz 整合到 Weight QC)

| 维度 | 评估 |
|------|------|
| 技术可行性 | **中** -- NLS 拟合对数据量和质量有硬性要求, 可能大量失败 |
| 预期效果 | **中** -- Gompertz 生物学模型更准确, 但仅标记异常点 (不删除动物), 对动物保留率影响小 |
| 代码修改量 | **小** -- 40 行新代码, 3 个文件 |
| 风险点 | (1) NLS 拟合失败率可能较高 (数据噪声); (2) 初始值选择对拟合结果敏感; (3) 与现有 RLM + 二次多项式可能存在冗余标记 |
| 对 LMM 的影响 | 无直接影响 (体重 QC 不参与 LMM 校正) |

### 3.3 方案 C (同时整合 A + B)

| 维度 | 评估 |
|------|------|
| 技术可行性 | **高** -- A 和 B 互不依赖, 独立开关控制 |
| 预期效果 | **中高** -- 综合优势, 但需注意过度标记叠加效应 |
| 代码修改量 | **中** -- 100 行新代码, 4+1 个文件 |
| 风险点 | (1) 两个可选模块同时启用时, 异常标记可能过度叠加; (2) 配置参数增多, 用户调优复杂度上升 |
| 对 LMM 的影响 | STL 扩展 LMM 特征空间, Gompertz 不影响 |

---

## 4. 推荐方案

**推荐方案 C (同时整合 A + B), 但默认关闭两个新增模块。**

理由:
1. 两个模块互不依赖, 可以独立启用
2. 默认关闭确保不影响现有 National Standard 的行为
3. 用户可以根据数据特点选择性启用 (如季节性数据启用 STL, 长期试验启用 Gompertz)
4. 代码修改量可控 (约 100 行)
5. 为后续的设备类型自适应 QC 参数推荐系统预留了扩展点

---

## 5. LMM 校正适配计划

### 5.1 当前状态

LMM 校正模块的 9 个 err_flags 全部来自 National Standard Feed QC。如果整合方案 A (STL) 启用, 需要将 `flag_STL_FI` 添加到 err_flags。

### 5.2 修改细节

**文件**: `R/zhenm_daily_aggregate_filtered.R`, `.apply_feed_lmm_correction()`

修改 `err_flags` 定义 (L222-226):
```r
err_flags <- c("flag_duration_negative", "flag_duration_too_long",
               "flag_duration_zero_with_feed", "flag_speed_too_slow",
               "flag_speed_too_fast", "flag_speed_extreme_low_feed",
               "flag_speed_zero_long_duration", "flag_feed_negative",
               "flag_feed_too_high", "flag_STL_FI")  # 新增
```

**兼容性分析**:
- `flag_STL_FI` 在 `use_stl_feed = FALSE` 时不存在于 `raw_dt` 中
- 现有代码 L252-259 已处理缺失 flag 的情况: `if (flg %in% names(raw_dt)) ... else ... FALSE`
- 因此无需额外的兼容性处理, 代码自动适配

### 5.3 LMM 公式变化

启用 STL 后, LMM 公式从:
```
normal_feed_sum ~ I(daily_weight_g/1000) + I(adg_g/1000) + has_flag_xxx_1 + ... + has_flag_xxx_9 + (1|animal_id)
```
变为:
```
normal_feed_sum ~ I(daily_weight_g/1000) + I(adg_g/1000) + has_flag_xxx_1 + ... + has_flag_xxx_9 + has_flag_STL_FI + (1|animal_id)
```

`has_flag_STL_FI` 表示当日是否存在 STL 检测到的异常, 作为 LMM 的额外协变量参与校正。

---

## 6. 配置参数变更计划

### 6.1 新增参数清单

| 参数 | 所属配置 | 类型 | 默认值 | 说明 |
|------|---------|------|--------|------|
| `use_stl_feed` | `national_standard` | logical | `FALSE` | 启用 STL 采食量 QC |
| `stl_period` | `national_standard` | integer | `7` | STL 周期 (天) |
| `stl_s_window` | `national_standard` | character | `"periodic"` | STL 季节性窗口 |
| `stl_robust` | `national_standard` | logical | `TRUE` | STL 鲁棒拟合 |
| `stl_mad_multiplier` | `national_standard` | numeric | `3` | STL MAD 异常阈值倍数 |
| `stl_min_obs` | `national_standard` | integer | `30` | STL 最少观测天数 |
| `use_gompertz` | `national_standard` | logical | `FALSE` | 启用 Gompertz 体重 QC |
| `gompertz_min_obs` | `national_standard` | integer | `60` | Gompertz 最少观测天数 |
| `gompertz_mad_multiplier` | `national_standard` | numeric | `4` | Gompertz MAD 异常阈值倍数 |
| `gompertz_maxiter` | `national_standard` | integer | `100` | Gompertz NLS 最大迭代次数 |

### 6.2 配置文件修改

**文件**: `R/zhenm_config_defaults.R`

在 `ZhenM_default_config("national_standard")` 的 `base_config$national_standard` 列表末尾添加上述 10 个参数。

### 6.3 向后兼容性

- 所有新参数都有默认值, 不影响现有用户配置
- `use_stl_feed = FALSE` 和 `use_gompertz = FALSE` 确保默认行为不变
- 用户可通过 `ZhenM_merge_config()` 覆盖新参数

---

## 7. Legacy Bug 修复计划

### 7.1 C-1: feed_intake_range 单位不匹配

**文件**: `R/zhenm_qc_feed_standard.R`, L156
**当前代码**:
```r
feed_range <- cfg$legacy$feed_intake_range   # c(0, 6) 即 0~6 kg
```
**修复**:
```r
feed_range <- .normalize_feed_range(cfg$legacy$feed_intake_range)
```

**影响**: 修复后 `flag_feed_out_of_range` 不再误标记所有记录, 日采食量恢复正常计算 (而非全部依赖插补)。这是对 Legacy 路径最关键的修复。

**验证方法**: 运行 FIRE 数据的 Legacy QC, 确认 `flag_feed_out_of_range` 标记量从几乎 100% 降至接近 0。

### 7.2 H-3: R-squared 阈值配置缺失

**文件**: `R/zhenm_config_defaults.R`, L68-89
**修改**: 在 `base_config$legacy` 列表中添加:
```r
growth_curve_r2_min = 0.95,
```

**文件**: `R/run_zhen_measure.R`, L141
**当前代码**:
```r
min_r2 <- cfg[[qc_method]]$growth_curve_r2_min %||% cfg$national_standard$growth_curve_r2_min
```
**修复** (可选, 简化回退逻辑):
```r
min_r2 <- cfg[[qc_method]]$growth_curve_r2_min
if (is.null(min_r2)) min_r2 <- 0.95  # 安全回退值
```

**影响**: Legacy 模式下 R-squared 阈值从 0.99 降至 0.95, 减少不必要的动物剔除。在当前测试中影响有限 (因为 Gompertz 已提前过滤), 但在数据质量好时能保留更多动物。

### 7.3 H-2: na.approx 端点外推

**文件**: `R/zhenm_impute.R`, `.impute_weight_legacy()`, L121-138
**当前代码**:
```r
dt[, `:=`(
  daily_weight_g = zoo::na.approx(daily_weight_g, na.rm = FALSE),
  is_imputed_wt = was_missing_weight
), by = animal_id]
```
**修复**:
```r
dt[, `:=`(
  daily_weight_g = zoo::na.approx(daily_weight_g, na.rm = FALSE, rule = 2),
  is_imputed_wt = was_missing_weight
), by = animal_id]
```

**文件**: `R/zhenm_impute_feed.R`, `.impute_feed_legacy()`, L11-14
**当前代码**:
```r
dt[, `:=`(
  daily_feed_g = zoo::na.approx(daily_feed_g, na.rm = FALSE),
  is_imputed_feed = was_missing_feed
), by = animal_id]
```
**修复**:
```r
dt[, `:=`(
  daily_feed_g = zoo::na.approx(daily_feed_g, na.rm = FALSE, rule = 2),
  is_imputed_feed = was_missing_feed
), by = animal_id]
```

**注意**: 采食量的端点外推需要额外限制为非负值。现有代码 L17-20 已有负值修正逻辑, 可以复用。

**影响**: 消除端点缺失导致的 FCR_NA (YANGXIANG 上 16 个)。

### 7.4 C-2: QC 标志越权初始化

**文件**: `R/zhenm_qc_utils.R`, `.init_qc_flags()`, L117-128
**修复**: 将 Legacy flags 拆分为仅体重相关:
```r
"legacy" = c(
  "flag_weight_out_of_range", "flag_SD_WT", "flag_RLM_WT",
  "flag_Gompertz_WT", "is_outlier_wt"
)
```
移除 `"flag_STL_FI"`, `"is_imputed_feed"`, `"is_outlier_fi_stl"`。

**影响**: 纯代码清理, 不影响计算结果。

---

## 8. 测试验证计划

### 8.1 单元测试

**文件**: `tests/testthat/test-qc.R` (或新建 `test-integration.R`)

| 测试用例 | 描述 | 预期结果 |
|----------|------|----------|
| STL 标记正确性 | 构造含周期性异常的日采食量序列 | `flag_STL_FI` 正确标记异常日 |
| STL 样本不足 | 个体日记录 < `stl_min_obs` | 跳过 STL, `flag_STL_FI` 全为 FALSE |
| STL 拟合失败 | 全 NA 或常数序列 | `tryCatch` 捕获, 无报错 |
| Gompertz 标记正确性 | 构造含异常体重的日体重序列 | `flag_Gompertz_WT` 正确标记异常日 |
| Gompertz 拟合失败 | 数据点不足或噪声过大 | `tryCatch` 捕获, 无报错 |
| LMM 10 特征 | 启用 STL 后 LMM 公式包含 `has_flag_STL_FI` | `active_flags` 包含 10 个标志 |
| LMM 兼容性 | 关闭 STL 时 LMM 仍使用 9 个特征 | `active_flags` 不包含 `flag_STL_FI` |
| 配置合并 | 用户传入 `use_stl_feed = TRUE` | `ZhenM_merge_config` 正确合并 |

### 8.2 集成测试

**方法**: 使用现有 3 个数据源 (YANGXIANG, FIRE, Nedap) 运行完整 Pipeline

| 测试场景 | 配置 | 验证指标 |
|----------|------|----------|
| 基准 (无 STL/Gompertz) | `use_stl_feed=FALSE, use_gompertz=FALSE` | 结果与当前 National Standard 完全一致 |
| 仅启用 STL | `use_stl_feed=TRUE, use_gompertz=FALSE` | flag_STL_FI 标记量合理, LMM 校正量变化 |
| 仅启用 Gompertz | `use_stl_feed=FALSE, use_gompertz=TRUE` | flag_Gompertz_WT 标记量合理, 动物保留率变化 |
| 同时启用 | `use_stl_feed=TRUE, use_gompertz=TRUE` | 无冲突, 表型结果合理 |
| Legacy Bug 修复验证 | `qc_method="legacy"` + C-1/H-3/H-2 修复 | flag_feed_out_of_range 标记量正常, FCR_NA 为 0 |

### 8.3 回归测试

- 运行 `devtools::test(pkg = "V项目测试与开发/项目本体/ZhenMeasure")` 确认现有测试不受影响
- 运行 `Rscript V项目测试与开发/项目本体/ZhenMeasure/inst/scripts/run_package_build_check.R` 确认 R CMD check 通过
- 运行 `Rscript V项目测试与开发/测试/ZhenMeasure_quickly_start.R` 确认 Pipeline 正常

### 8.4 性能测试

| 场景 | 预期耗时变化 | 原因 |
|------|-------------|------|
| STL 关闭 (默认) | 无变化 | STL 代码不执行 |
| STL 启用 (200 动物) | +10~30 秒 | 每个动物一次 STL 分解, 约 0.05~0.15 秒/动物 |
| Gompertz 关闭 (默认) | 无变化 | Gompertz 代码不执行 |
| Gompertz 启用 (200 动物) | +20~60 秒 | 每个动物一次 NLS 拟合, 约 0.1~0.3 秒/动物 |

---

## 9. 风险评估

### 9.1 方案 A 风险 (STL)

| 风险 | 概率 | 影响 | 缓解措施 |
|------|------|------|----------|
| STL 过度标记正常波动 | 高 | 中 | 默认关闭; 用户启用后可调 `stl_mad_multiplier` |
| STL frequency=7 不适用 | 中 | 低 | 暴露 `stl_period` 参数供用户调整 |
| STL 与现有标记重叠 | 高 | 低 | 重叠标记不影响 LMM 校正 (LMM 处理多重共线性) |

### 9.2 方案 B 风险 (Gompertz)

| 风险 | 概率 | 影响 | 缓解措施 |
|------|------|------|----------|
| NLS 拟合失败率高 | 中 | 低 | `tryCatch` 包裹, 失败时静默跳过 |
| 与 RLM + 二次多项式冗余 | 中 | 低 | 仅标记异常点, 不替代现有检查 |
| 初始值选择不当 | 低 | 中 | 使用 `max(y) * 1.1` 作为 A 的初始值, 经验值 |

### 9.3 Legacy Bug 修复风险

| 风险 | 概率 | 影响 | 缓测措施 |
|------|------|------|----------|
| C-1 修复后 Legacy 结果大幅变化 | 高 | 中 | 预期行为 -- 修复前结果是错误的 |
| H-3 修复后 R-squared 阈值不适用 | 低 | 低 | 0.95 是经验值, 可在后续校准 |
| H-2 rule=2 外推值不合理 | 低 | 低 | 采食量已有负值修正, 体重有物理边界 |

### 9.4 整体风险总结

- **最低风险**: Legacy Bug 修复 (C-1, H-3, H-2, C-2) -- 纯修复, 不引入新功能
- **低风险**: 方案 A (STL) -- 默认关闭, 不影响现有行为
- **中风险**: 方案 B (Gompertz) -- NLS 拟合可能失败, 但有 tryCatch 保护
- **可控风险**: 方案 C (A+B) -- 两个独立模块, 互不影响

---

## 10. 实施优先级

| 优先级 | 任务 | 工作量 | 预期收益 |
|--------|------|--------|----------|
| **P0** | 修复 C-1 (feed_intake_range 单位) | 1 行 | 消除 Legacy 采食量 QC 的静默数据损坏 |
| **P0** | 修复 H-3 (R-squared 阈值) | 1 行配置 | 减少 Legacy 动物误删 |
| **P1** | 修复 H-2 (端点外推) | 2 行 | 消除 FCR_NA |
| **P1** | 修复 C-2 (标志越权初始化) | 5 行 | 代码清理 |
| **P2** | 整合方案 A (STL 到 National) | 50 行 | 增强采食量异常检测 |
| **P2** | 整合方案 B (Gompertz 到 National) | 40 行 | 增强体重异常检测 |
| **P3** | LMM err_flags 扩展 | 1 行 | STL 标记参与 LMM 校正 |
| **P3** | 单元测试 + 集成测试 | 100 行测试代码 | 质量保证 |
