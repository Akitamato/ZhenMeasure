# ZhenMeasure news

## 1.1.0

### Fixed
- **LMM 采食量校正方向错误**：`.apply_feed_lmm_correction()` 原先用 `abs(β)` 计算校正量，丢弃系数符号；当异常标志的 β 为正时（如 `speed_too_fast`、`speed_extreme`）会往错误方向加。改为 `-β`，方向由数据决定。
- **`flag_feed_too_high` 量纲错位**：原先用"日采食量总和的 P99"去标"单次采食记录"，量纲错位导致该 flag 几乎永不触发。改为用该个体单次采食量的 P99 作为阈值。
- **6kg 截断漏插补**：`.impute_feed_national_v2()` 对 Loess / 线性回归外推插补失败后残留的 NA 静默保留，最终在表型计算的 `sum(na.rm=TRUE)` 中被当作 0，导致 ADFI 系统性偏低。新增兜底：用该个体中位日采食量填补残留 NA 并标记 `is_imputed_feed`。
- **LMM 校正布尔补偿精度不足**：`has_flag` 特征原先用 `any()`（是否发生，布尔），校正量与异常程度脱节。改为 `sum()`（发生次数），使校正量与异常记录条数成比例。

### Added
- 6kg 生理极限截断新增可追溯标记 `flag_daily_feed_over_limit`，超过 6kg 的天被显式标记并进入输出，便于审计。

## 1.0.0

### Breaking changes
- **Legacy QC method removed.** The `qc_method = "legacy"` option has been permanently removed. Only `national_standard` is supported. All legacy-specific QC functions (~400 lines across 8 files) have been deleted.
- `ZhenM_default_config("legacy")` now errors. Use `ZhenM_default_config("national_standard")` or `ZhenM_default_config()`.
- `ZhenM_impute_data()` no longer accepts `impute_method = "legacy"`.
- `ZhenM_qc_weight_standard()` and `ZhenM_qc_feed_standard()` no longer accept `qc_method = "legacy"` (parameter kept for backward compat but ignored).

### Removed
- `R/zhenm_impute_feed.R` (entire file) -- legacy feed imputation
- `inst/scripts/run_legacy_regression_demo.R` -- legacy comparison script
- `inst/scripts/run_legacy_regression_matrix.R` -- legacy comparison script
- Unused dependencies: `nlme`, `quantreg`, `splines` removed from DESCRIPTION Imports

### Fixed
- Fixed 7 broken test files to match current API (column names, function names)
- Removed stale globalVariables for legacy-only flags from `zzz.R`
- Added `.txt` format file support to `ZhenM_parse_data_format()` (previously only `.json`)

### Internal
- Simplified dispatcher functions to direct calls (no more `if/else` on qc_method)
- Cleaned up `zzz.R` globalVariables list
- Removed ~174 lines of `.qc_weight_standard_legacy()` and ~122 lines of `.qc_feed_standard_legacy()`
- Removed legacy config block (~22 lines) from `ZhenM_default_config()`

## 0.2.6

Integrated Legacy's STL and Gompertz detection into National Standard as optional modules; fixed 3 Legacy bugs; added unit tests for STL/Gompertz integration.

## 0.2.5

Full code audit found and fixed 8 hidden bugs, 3 of which were Critical and caused silent QC errors. Also cleaned up stale API references in user manual and man pages.

## 0.2.0.9000

- Refactored the main workflow into modular reader, QC, phenotype, and output components.
- Added birth info ingestion and age-stage phenotype calculation support.
- Added structured QC outputs, including `error_type_summary.csv`, `animal_qc_summary.csv`, and `qc_run_summary.txt`.
- Added explanatory legacy regression tooling to diagnose new-vs-legacy animal set divergence.
- Added a repository-level easy-start script that fixes the three bundled demo runs.
- Added multi-device regression matrix coverage for larger YANGXIANG, NEDAP, and FIRE demo samples.
- Added build-first package validation via `R CMD build` followed by tarball-based `R CMD check`.
- Cleaned the main R source files toward an ASCII-only release baseline while preserving compatibility aliases.
