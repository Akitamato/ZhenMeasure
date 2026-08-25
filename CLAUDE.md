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

Unit tests: 0 FAIL / 108 PASS / 5 SKIP.

### NAMESPACE cleanup needed

- `utils::write.table` imported but unused in R/ source
- 23 exported functions lack matching Rd documentation entries
