# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**ZhenMeasure** is an R package for pig feeding station data standardization, quality control (QC), and phenotype calculation. It processes data from three commercial device types: YANGXIANG, Nedap, and FIRE. The package was formerly named AFEStat (renamed at V0.2.4).

The active R package source is at `项目本体/ZhenMeasure/`. Note: the git repository root is this directory itself, so all paths below are already relative to it. `开发方案/` (development plans, debug reports) is gitignored — local-only, absent from fresh clones.

## Commands

All commands assume the repo root as working directory.

**Install locally:**
```r
install.packages("项目本体/ZhenMeasure", repos = NULL, type = "source")
```

**Run R CMD build + check (primary validation):**
```r
Rscript 项目本体/ZhenMeasure/inst/scripts/run_package_build_check.R
```
Success requires `Status: OK` in `项目本体/ZhenMeasure/build-output/ZhenMeasure.Rcheck/00check.log`. The script is cwd-independent (it locates itself), so it can be run from anywhere.

**Run unit tests:**
```r
devtools::test(pkg = "项目本体/ZhenMeasure")
devtools::test(pkg = "项目本体/ZhenMeasure", filter = "qc")  # single file: test-qc.R
```

**Run manual integration tests:**
```r
Rscript 测试/ZhenMeasure_quickly_start.R
Rscript 测试/ZhenMeasure_step_by_step_test.R
```

**Regenerate documentation (roxygen2):**
```r
devtools::document(pkg = "项目本体/ZhenMeasure")
```

## Architecture

### Pipeline (9 steps)

Entry point: `run_zhen_measure()` in `R/run_zhen_measure.R`

1. **Read** -- device-specific readers (`zhenm_read_data.R`) normalize raw files to a standard record format
2. **Overall QC** -- dedup, missing-value filtering, completeness check (`zhenm_qc_overall.R`)
3. **Weight QC** -- weight outlier detection (`zhenm_qc_weight_standard.R`)
4. **Feed QC** -- feed intake outlier detection (`zhenm_qc_feed_standard.R`)
5. **Daily aggregation** -- filtered daily sums with LMM correction (`zhenm_daily_aggregate_filtered.R`)
5.5. **Growth curve R² check** -- per-animal quadratic regression fit; animals with R² < threshold are deleted (`run_zhen_measure.R`)
6. **Imputation** -- missing value imputation via national standard or legacy method (`zhenm_impute*.R`)
7. **Phenotype calculation** -- ADFI, ADG, FCR, etc. with optional stage partitioning (`zhenm_phenotype_*.R`)
8. **QC summary** -- aggregated quality flags
9. **Output** -- CSV files + growth curve PDF plots (`zhenm_write_outputs.R`)

### QC Method

Since V1.0.0, only `national_standard` is supported. The legacy method has been removed.

`national_standard`: RLM robust regression for per-record weight QC → weighted average daily weight → quadratic growth curve fit with R² threshold; 9 feed anomaly types (negative, too-high, duration/speed flags); LMM correction on daily feed intake; Kalman filter (imputeTS) for weight imputation, Loess/linear regression extrapolation for feed imputation with FCR stage validation. Optional enhancements: STL time-series feed anomaly detection (`use_stl_feed=TRUE`) and Gompertz growth curve weight anomaly detection (`use_gompertz=TRUE`), both disabled by default.

### Legacy Retirement (Completed in V1.0.0)

Legacy method was removed in V1.0.0:
- Deleted ~400 lines of legacy code from 8 files
- Deleted `R/zhenm_impute_feed.R` (entire file)
- Removed `nlme`, `quantreg`, `splines` unused dependencies
- Fixed 9 broken test files
- All 5 legacy-unique strategies confirmed zero value in V0.2.6 evaluation

See `测试/大规模测试/Legacy问题处理/final_legacy_retirement_plan.md` for the complete retirement plan.

### Naming conventions

- Exported functions: `ZhenM_` prefix (28 total in NAMESPACE; the one exception is `run_zhen_measure`)
- Internal functions: `.` prefix (various patterns: `.zhenm_`, `.check_`, `.normalize_`, `.map_`, `.init_`, `.identify_`, `.apply_`, `.build_`, `.annotate_`, `.plot_`, `.add_`, `.create_`)
- Source files: `zhenm_*.R` for modules

### Key patterns

- All data manipulation uses `data.table`
- Configuration is a nested list merged via `ZhenM_merge_config()`
- Original data is preserved alongside QC flags for traceability
- `options(scipen = 999)` is used throughout to suppress scientific notation
- Source files must be read/written with `encoding = "UTF-8"`
- Comments are in Chinese

## Testing

Test files are in `项目本体/ZhenMeasure/tests/testthat/` (testthat edition 3). 11 test files: birth-info, config, data-format, impute, phenotype, plot-outputs, qc, qc-and-phenotype-age, regression, standard-schema, stl-gompertz.

Manual regression/diagnostic scripts are in `测试/`: quickly_start, step_by_step_test, imputation_analysis, lmm_correction_compare, qc_methods_compare, smoke_test, timezone_fix, plus V1.1.1-era ADFI diagnostics (`ZhenMeasure_nansha_test.R`, `compare_adfi.R`, `diagnose_adfi_drop.R`, `diagnose_adfi_low.R`, `verify_lmm_fix.R`) and a `demo/` folder.

### Comparison with 中农程序

Comparison test scripts are in two folders under `测试/`:
- `与中农程序对比测试相关脚本/` — 6 scripts: 00_run_all, 01_run_zhenmeasure, 02_prepare_comparison, 03_run_comparison, 04_generate_report, plus report template (`COMPARISON_REPORT.md`, plan in `00_对比测试计划.md`)
- `与中农程序对比测试/` — self-contained variant with its own `data/`, `results/`, and the 中农 Python sources

**中农程序** (China Agricultural University program): Python-based FCR correction tool at `测试/与中农程序对比测试/zhongnong_scripts/` (2 scripts: 手动恢复QC.py, 矫正115.py). Uses robust regression (Tukey Biweight) + breed-specific thresholds (YY/LL/DD) + mixed-effects model for feed correction.

**Data format incompatibility**: YANGXIANG data has 10-164 visits/day (high-frequency sampling), while 中农程序 expects 1-10 visits/day. Daily feed totals (3-10 kg) exceed 中农程序's 6 kg threshold. Direct numerical comparison is not possible on YANGXIANG data.

**ZhenMeasure V1.0.0 test results on YANGXIANG (60 pigs):**
- After Overall QC: 52 animals (8 duplicates/incomplete removed)
- After Weight/Feed QC: 52 animals (1,709 weight + 622 feed outlier flags)
- After Growth Curve R² check: 48 animals (4 removed for R² < 0.99)
- Final phenotypes: 144 rows (48 animals × 3 weight stages: 30-100/115/120 kg)

## Key Dependencies

`data.table`, `MASS`, `readxl`, `lubridate`, `zoo`, `lme4`, `imputeTS` (all in Imports)

## Bug Fix Log

### V1.1.4

**LMM 叠加校正（`use_lmm_stacking`）转为出厂默认**（issue #5「F 转正」）。分支 `feat/lmm-stacking-default`，尚未合入 main。

记录级物理纠正成功后，串联运行改良日级 LMM，只补偿物理规则无法恢复的「噪声置零类」损失（负值 / 极高速小采食 / 长时间零速被置 0 的记录）；已被封顶纠正的记录不入模，避免二次补偿。设 `use_lmm_stacking = FALSE` 可退回 V1.1.1 的纯记录级物理纠正行为。

**依据**（注入式基准，三设备 × 三档注入率共 9 格，F 全部优于纯记录级纠正 A）：
- 干净数据上 F 近乎不出手（FIRE 0 天改写、NEDAP 1 天 / 14.2 g、扬翔 869 天平均 7.7 g）；而日级 LMM 兜底路径 B 会在干净数据上日均改写数百克、波及数千天——「不无端改动好数据」是 F 相比例级纠正之外的实质增益。
- 代价为每台设备多一次 `lme4` 拟合：+0.4s / +1.3s / +7.7s，占该设备读取+QC 耗时的 3~4%。
- 更激进的右删失完整似然方案（G_ml）准确率略高（基准 9 格中 6 格第一，但领先仅 0.001~0.003），却慢 3.6~4.8 倍（issue #6），暂不纳入默认，仍留在 `feat/censored-full-likelihood` 分支。

**实数据端到端回归（A=显式 FALSE vs F=显式 TRUE，走完整管线）**：

| 设备 | 个体数 A/F | 日级 `daily_feed_g` 变化 | 总采食偏差 | 表型 ADFI 最大变化 |
|---|---|---|---|---|
| FIRE | 211 / 211 | 0 / 20913 天 | 0 g（逐位一致） | 无 |
| NEDAP | 29 / 29 | 1 / 1915 天 | +14 g（+0.0003%） | 1 头 +0.22 g |
| 扬翔 | 547 / 547 | 743 / 63805 天（1.16%） | +5690 g（+0.0042%） | 623/1613 阶段行，平均 +0.096 g、最大 6.375 g（0.32%） |

扬翔的结构性影响：个体数与阶段行数不变，`flag_fcr_stage_invalid` 分布不变（TRUE 1248 / FALSE 365）；唯一结构变化是 1 头（`998-025021177551`）的三个阶段行 `n_valid_stages` 各减 1，但该行前后都已是 invalid，无 QC 分类翻转。

**验证**：单测 319 → 324 PASS（+5 条断言锁定「默认 = 显式 TRUE」契约与开关可达性），0 FAIL / 0 ERROR / 5 SKIP；`R CMD check --no-build-vignettes --no-manual` 与基线同为 1 ERROR / 3 WARNING / 1 NOTE。证据见 issue #5 评论 5620400631。

### V1.1.3

代码审查批量修复：`fix/code-review-issues` 分支 26 个提交覆盖 issue #8–#33，已快进合入 main。完整回归报告见 issue #34。单元测试 162 → 319 断言（0 FAIL / 0 ERROR / 5 SKIP），`R CMD check --no-build-vignettes --no-manual` 与基线同为本机环境下的 1 ERROR / 3 WARNING / 1 NOTE。

**行为变更（仅两项，其余为纯修复/重构）**：
- **#14 体重 QC 还原单轮 RLM 双阈值**：删除从未生效的第二轮日级 RLM，日级判定改用 `daily_weight_threshold`（0.90 共识）而非误用的记录级 `weight_threshold`（0.25）。日级体重异常标记扩容（NEDAP 8→313、FIRE 837→8567、扬翔 14831→146402 条）；扬翔比原先多保留 43 头个体（505→547），FIRE/NEDAP 个体数不变。这是本次合入唯一的实质业务影响，已在 issue #34 中提请业务确认。
- **#27 体重阶段区间统一为左闭右开 `[min, max)`**：四处口径统一，杨翔实测影响恰 1 个阶段行。

**重点修复**：
- **#8（严重）** 插补函数按日期序计算却按原始行序写回，乱序输入会把插补值静默写到错误日期；同时修复连续缺失路径的中间列 schema 污染。
- **#29** Gompertz 拟合公式含子集表达式（`x_gomp[valid]`）触发 `nls` 的 `n %% respLength == 0` 校验失败，错误被 `tryCatch` 吞掉后整头动物静默跳过检查（FIRE 211 头中 190 头从未被检查）；改为数据框建模 + 公式变量名，并修复 `predict(newdata=)` 被静默忽略。
- **#9** 紧凑日期 `YYYYMMDD` 被误判为 Excel 序列号；**#13** LMM 补偿量符号守卫与下限护栏；**#11** FCR/ADFI 除零与退化守卫；**#19** 边界条件 8 处（空表 NaN、全 NA ±Inf、flag 被 NA 污染、`Location` 拼出字面 `"NA"` 等）。
- **#18** `jsonlite` 移入 Imports（原软守卫会让干净环境核心读取不可用）；**#20** 三套日期解析器合并；**#21** 日级出口校验抽取；**#22** 生长曲线判定去 O(n²)；**#23** QC 汇总行落盘。
- 零行为变化的加固：#25（文档）、#26（插补只写缺失位）、#30（`copy(raw_dt)`）、#31（删冗余检查）、#32（残留 NA 告警）、#33（早退 summary 列名统一）。

**版本号**：Pipeline 启动 banner 的版本号改从 DESCRIPTION 读取（`utils::packageVersion`），此前硬编码为 V1.0.0 且已落后两个版本。

### V1.1.2

Phase 1：日级 LMM 兜底校正重构（issue #5，分支 `feat/feed-correction-switches`）。动机：注入式仿真基准证明旧 LMM 设定为净负贡献。四项修复（`zhenm_daily_aggregate_filtered.R` `.apply_feed_lmm_correction()`）：
- **visits_n 协变量**：异常条数与当日活动强度机械相关，不控制强度则 flag 系数把「当天访问多」误吸收进补偿量。
- **时长量纲特征**：被 flag 记录按 `dur_<flag>`（累计有效秒数）入模，补偿与被丢采食时长成比例而非与次数成比例；无时长列或日级 flag（`flag_STL_FI`）自动退回计数特征。
- **训练集去截断**：不再按 `0 < normal_feed_sum ≤ 6000` 筛训练样本；6kg 生理上限只在出口统一校验（打标 + 置 NA），对所有路径一致。
- **物理速率封顶**：补偿加回量 ≤ `speed_max × 被flag总时长/60`（吸收记录级物理规则作先验）；`speed_max` 从 config 接线（原硬编码 170）。
- **台账列** `lmm_correction_g` 保留在日级输出中可追溯。
- 新增单测覆盖 LMM 兜底路径（test-qc.R）。

基准验证（`测试/simulation_benchmark.R`，同种子复测）：变体 B 净贡献由负转正——accuracy 5%/10%/20% 注入率下 0.9205→0.9470 / 0.8770→0.9278 / 0.8064→0.8986；bias −6.8→−3.5 / −10.7→−4.3 / −17.6→−5.3%；分类型恢复率 inflate≈0.97–0.98、zero/negate≈0.95。

步骤 1 叠加模式（`use_lmm_stacking`，默认 FALSE）：记录级纠正成功后串联互补式 LMM——只建模「噪声置零类」（负值/极高速小采食/长时间零速）的时长特征，已被封顶恢复的类型不再入模（互补不重复计数）；响应为纠正后日值本身，加法应用，干净天恒零校正。基准变体 F 全面优于纯记录级：accuracy 0.9595→0.9634 / 0.9353→0.9418 / 0.8995→0.9099，日级保真不掉（adfi_r ≥0.997）。

步骤 2 删失模型 PoC（`测试/simulation_benchmark.R` `.em_censored_daily()`，未接入包管线）：噪声置零记录按右删失 Tobit 处理（对数尺度上界 speed_max×时长/60），lme4 混合模型 EM 迭代截尾条件期望（3-4 轮收敛）后复活。变体 G_cens 全档第一：accuracy 0.9648/0.9446/0.9140，20% 时总体 bias≈0（+0.04%）；negate 类恢复率 0.99–1.01、zero 类 0.965–0.986。

泛化复测（FIRE 211 头 / NEDAP 42 头，同基准同种子）：改良 LMM 家族（B/F/G）在三台设备上全部优于无-LMM 对照（C0/A）——Phase 1 修复普适。**F（记录级+叠加LMM）三设备全部稳居前二且 bias 最小（FIRE 20% 仅 +0.7%），是当前证据下的推荐默认组合**。G_cens 平均准确率最高但过补倾向在非扬翔设备显现（bias@20%：FIRE +3.6%、NEDAP +6.7%，小样本放大），接入包内前需保守化（复活量 shrink / 分位数删失界 / 过补信号自动回退 F）。设备排序差异印证互补设计：高频访问（扬翔）A 强、低频（Nedap）日级统计借用强（B 在 NEDAP 20% 反超 G 登顶）。

G_cens 保守化（commit 35ee5ff，脚本 CLI 参数 `Rscript simulation_benchmark.R <DEVICE> <quantile> <shrink>`）：①复活量折扣 shrink=0.7 定版（FIRE 校准 s1.0→0.8→0.7：bias@20% +3.6%→+2.2%→+1.5%，acc 仅损失 ~0.2pp，方向从高估转轻微低估）；②分位数删失界实测不绑定——三设备个体干净速率 95%/99% 分位普遍高于 170 g/min 物理上限，min 后形同虚设，保留为可选参数默认关。三设备定版表现（物理界+s0.7）：bias@20% 扬翔 −1.1%、FIRE +1.5%、NEDAP +4.3%（后者与 B +5.2%/F +4.5% 同档，属 42 头小样本下整个 LMM 家族的天花板而非 G 特有）。**前提性警告：手写 ECM 对干净池:删失行比例高度敏感**（clean_subsample 250k→150k 使 FIRE bias +3.6%↔−3.0%），接入包内必须换成完整似然方法。文献检索确认两条策略均有直接谱系并记录于 issue #5 评论（Casey 2005 十六准则 / Jiao 2016 MI vs LMM / Fernando et al. 1987 删失混合模型动物育种祖先 / Hughes 1999 MCEM）。

G_cens 完整似然落地（commit 273af44，分支 `feat/censored-full-likelihood`）：手写 ECM 换成自实现**右删失完整似然** `.em_censored_daily_ml()`——删失记录贡献上尾概率 Φ((logU−μ)/σ)，随机效应经 Gauss-Hermite 积分边缘化（无伪观测、无子采样、无池比例依赖），BFGS 直接最大化，复活用截尾条件期望 μ−σφ(α)/Φ(α)，shrink 保留。neg_ll 全向量化（rowsum 按动物汇总）。三设备同种子复测：过补系统性下降——FIRE 20% bias +3.6%→+1.6%、NEDAP 20% +6.7%→+4.3%、扬翔 20% +0.04%→−1.1%；准确率与 F 并列、日级保真 adfi_r≥0.95。**F 已随 V1.1.2 合入 main**；G_ml 仍保留在分支，等 issue #5 关闭决策后再合。注意：`lmec`（Vaida & Liu 2009）在 R 4.5.3 下只支持左删失、与本场景右删失语义不符，故自实现而非调用。

### V1.1.1

采食量校正模型重构 + 个体日增重口径修复：
- **记录级物理纠正**（`zhenm_daily_aggregate_filtered.R` 新增 `.correct_feed_records()`）：被 flag 的采食记录不再「置零排除」，改为按 flag 类型物理封顶——噪声类（负值/极端速度小采食/长时间零速）置 0，`speed_too_fast` 封顶到 `170×时长/60`，`feed_too_high` 封顶到个体 P99（用干净记录计算），时长类异常/速度过慢保留原值。南沙数据 `ADFI_g` 1745→1995.5，校正量 -442→-139.5 g/天。
- **日级 LMM 校正改为兜底**：记录级纠正成功时跳过 `normal_feed_sum + β×flag` 校正（避免二次校正），仅保留 6kg 日上限校验 `flag_daily_feed_over_limit`；记录级纠正失败时才走原日级 LMM。
- **个体日增重口径修复**：`adg_g` 从 `diff(daily_weight_g)`（未除以天数）改为 `diff(daily_weight_g)/as.numeric(diff(record_date))`，单位 g/天。
- **单测更新**：`test-qc.R` 从「排除异常值」契约改为「纠正异常值」，补 `flag_feed_too_high` 并改属性断言。

### V1.1.0

修复 4 个采食量相关 bug + 1 个可追溯增强：
- **LMM 校正方向**：`abs(β)` → `-β`（`zhenm_daily_aggregate_filtered.R`），丢弃系数符号会往错误方向加（speed_too_fast/speed_extreme 的 β 为正）。
- **`flag_feed_too_high` 量纲错位**：原先用"日采食量总和的 P99"标"单次采食记录"，几乎永不触发。改为单次 feed_g 的个体 P99（`zhenm_qc_feed_standard.R`）。
- **6kg 截断漏插补**：`.impute_feed_national_v2()` 插补失败残留 NA 静默归零，导致 ADFI 偏低。新增个体中位数兜底填补（`zhenm_impute_national.R`）。
- **LMM 布尔补偿精度**：`has_flag` 从 `any()`（是否发生）改为 `sum()`（发生次数），校正量与异常程度成比例（`zhenm_daily_aggregate_filtered.R`）。
- **新增**：`flag_daily_feed_over_limit` 标记超过 6kg 生理上限的天，可追溯。

### V1.0.0

Legacy method removed. Fixed 9 broken test files. Removed unused dependencies (`nlme`, `quantreg`, `splines`). Added `.txt` format support to `ZhenM_parse_data_format()`. Fixed LMM correction bug where outlier feed records were incorrectly included in `normal_feed_sum`. Fixed `as.IDate(character())` namespace issue in `zhenm_phenotype_calc.R`.

### V0.2.6

Integrated Legacy's STL and Gompertz detection into National Standard as optional modules; fixed 3 Legacy bugs; added unit tests for STL/Gompertz integration.

**New features:**
- **STL time-series feed QC** (`use_stl_feed=TRUE`): STL decomposition on daily feed intake, marks anomalous days via 3×MAD threshold on residuals. Acts as 10th LMM covariate. Complementary to existing 9 physics-based flags (Kappa ≈ 0.03).
- **Gompertz growth curve weight QC** (`use_gompertz=TRUE`): Gompertz NLS fit on daily weight, marks anomalous days via 4×MAD threshold. In FIRE test data, enabled Gompertz retained 2 additional animals (134→136) by improving borderline animals' growth curve R².
- Both modules disabled by default, zero diff from baseline when off.

**Bug fixes:**
- **C-1 (Legacy Critical)**: `feed_intake_range = c(0,6)` (kg) was compared directly with `feed_g` (grams), causing all feed records to be flagged as outlier. Fixed by calling `.normalize_feed_range()`.
- **H-3 (Legacy High)**: Legacy config missing `growth_curve_r2_min`, fell back to national's 0.99 (too strict). Fixed by adding `growth_curve_r2_min = 0.95` to legacy config.
- **H-2 (Legacy High)**: `zoo::na.approx` cannot extrapolate at endpoints. Fixed by adding `rule = 2` to `.impute_weight_legacy()` and `.impute_feed_legacy()`.

**Files modified (7):** `zhenm_config_defaults.R`, `zhenm_qc_feed_standard.R`, `zhenm_qc_weight_standard.R`, `zhenm_qc_utils.R`, `zhenm_daily_aggregate_filtered.R`, `zhenm_impute.R`, `zhenm_impute_feed.R` (~115 lines added).

**New test file:** `tests/testthat/test-stl-gompertz.R` (11 test cases covering config, STL flag creation, Gompertz flag creation, edge cases, backward compatibility).

See `测试/大规模测试/Legacy问题处理/final_decision_report.md` for the full evaluation report.

### V0.2.5 (commit 1155d9c)

Full code audit found and fixed 8 hidden bugs, 3 of which were Critical and caused silent QC errors. Also cleaned up stale API references in user manual and man pages. See `开发方案/Debug/V0.2.4_Bug修复总结报告.md` for details.

### V0.2.4

#### CRITICAL fixes

1. **`<<-` in data.table `j` expression** (`zhenm_qc_feed_standard.R:98,114`): The `<<-` operator assigned values to the parent environment instead of creating columns in the data.table. This caused `daily_threshold_feed` and `flag_feed_too_high` to be silently wrong. Fixed by using `:=` assignment.
2. **Numeric index as logical** (`zhenm_qc_weight_standard.R:458`): `dt[idx & record_date %in% ...]` where `idx` was a numeric vector from `which()`, not a logical vector. R's implicit coercion caused wrong row selection. Fixed by using `animal_id == id & record_date %in% ...`.
3. **Hardcoded config access** (`run_zhen_measure.R:141`): `cfg$national_standard$growth_curve_r2_min` was accessed unconditionally, returning NULL in legacy mode and causing ALL animals to be deleted. Fixed by using `cfg[[qc_method]]$... %||%` fallback.
4. **Missing weight imputation in legacy mode** (`run_zhen_measure.R:189`): Legacy mode called `ZhenM_impute_feed` (feed only) instead of `ZhenM_impute_data` (weight + feed). Weight imputation was skipped entirely.

#### HIGH fixes

5. **Division by zero in FCR** (`zhenm_phenotype_calc.R:357`): `stage_feed / stage_gain` produced `Inf` when `stage_gain == 0`. Fixed with `ifelse(stage_gain > 0, ...)`.

#### LOW fixes (variable shadowing)

6. **`formula` shadows `base::formula()`** (`zhenm_qc_utils.R:67`): Renamed to `rlm_fit`.
7. **`c` shadows `base::c()`** (`zhenm_daily_aggregate_filtered.R:42`): Renamed to `col_name`.

## Known Issues (not yet fixed)

### Test suite (V1.0.0)

9 test files fixed in V1.0.0. Remaining skips (5 total):
- `test-birth-info.R`: internal functions tested via integration
- `test-qc-and-phenotype-age.R`: `ZhenM_run()` doesn't exist (2 tests)
- `test-regression.R`: legacy baseline removed, national baseline not implemented (2 tests)

Unit tests: 0 FAIL / 146 PASS / 5 SKIP.

### R CMD check vignette (V1.1.2)

本机环境缺 pandoc，vignette 无法重建；且 vignette 示例使用占位路径 `path/to/raw/data`，执行必报错。`run_package_build_check.R` 在此环境达不到 `Status: OK`（报 vignette 链 1 ERROR + WARNING）；替代校验：`R CMD build --no-build-vignettes` + `R CMD check --no-build-vignettes`，其余检查项通过（non-ASCII/doc-mismatch 为既有遗留）。

### NAMESPACE cleanup needed

- `utils::write.table` imported but unused in R/ source
- 23 exported functions lack matching Rd documentation entries
