# ZhenMeasure news

## 1.1.4

### Changed

- **LMM 叠加校正（`use_lmm_stacking`）转为出厂默认**（issue #5「F 转正」）。记录级物理纠正成功后，再串联运行改良日级 LMM，只补偿物理规则无法恢复的「噪声置零类」损失（负值 / 极高速小采食 / 长时间零速被置 0 的记录）；已被封顶纠正的记录不入模，避免二次补偿。设 `use_lmm_stacking = FALSE` 可退回 V1.1.1 的纯记录级物理纠正行为。
  **此项会改变表型结果，但幅度极小**：注入式基准上三设备 × 三档注入率 9/9 格优于纯记录级纠正（A）；干净数据上则近乎不出手（FIRE 0 天改写、NEDAP 1 天 / 14.2 g、扬翔 869 天且平均仅 7.7 g）。实数据端到端回归：FIRE 逐位一致（daily_feed_g 0/20913 天变化），NEDAP 1/1915 天变化（总量 +14 g，+0.0003%，涉及 1/29 头个体的 ADFI +0.22 g）。代价为每台设备多一次 `lme4` 拟合（+0.4s / +1.3s / +7.7s，占该设备读取+QC 耗时的 3~4%）。
  注：更激进的右删失完整似然方案（G_ml）准确率略高（9 格中 6 格第一，但领先幅度仅 0.001~0.003），却慢 3.6~4.8 倍，暂不纳入默认。

## 1.1.3

代码审查批量修复（issue #8–#33，共 26 个提交）。详见 issue #34 的分支合入前全量回归报告。

### Changed

- **体重 QC 还原单轮 RLM 双阈值设计**（issue #14）：删除从未生效的第二轮日级 RLM；日级判定改用专用键 `daily_weight_threshold`（0.90，约 1.5σ 的「整日一致偏移」共识），此前误用了记录级 `weight_threshold`（0.25，约 5.4σ），导致日级规则几乎不触发。单记录日不参与日级共识（无「全部一致」语义）。
  **此项会改变表型结果**：日级体重异常标记显著扩容（NEDAP 8→313、FIRE 837→8567、扬翔 14831→146402 条），扬翔因生长曲线 R² 提高而多保留 43 头个体（505→547）。FIRE / NEDAP 个体数不变。
- **体重阶段区间统一为左闭右开 `[min, max)`**（issue #27）：`ZhenM_calc_phenotypes_stage()`、`fcr_ranges`、日聚合与表型计算四处口径统一，消除体重恰等于阶段上界当天的重复计数。
- 记录级物理纠正的 `speed_max` 接入 config（issue #12），不再与 LMM 校正模块的参数来源不一致。
- `jsonlite` 由 Suggests 移入 Imports（issue #18）：JSON 是唯一受支持的 format 配置路径，原软守卫会让干净环境（不装 Suggests）下的核心读取功能不可用。
- 三套重复的日期解析器合并为 `.parse_temporal()` 单一实现（issue #20）。
- 日级采食量出口校验抽取为 `.finalize_daily_feed()`（issue #21），含「无 feed 列」在内的三条出口路径口径统一。
- 生长曲线判定与连续性回填去掉 O(n²)/全表扫描（issue #22）。
- QC 汇总行改为经 logger 落盘、PDF 输出加设备守卫、`measurement_day` 口径统一（issue #23）。
- `test_weight_range` 文档化其「覆盖全量程」语义（issue #25，零行为变化）。
- `ZhenM_parse_data_format()` 文档更正为只支持 `.json`（issue #10）。
- Pipeline 启动 banner 的版本号改从 DESCRIPTION 读取，不再硬编码（此前长期停留在 V1.0.0）。

### Fixed

- **插补函数按日期序计算却按原始行序写回**（issue #8，严重）：未按 `animal_id + record_date` 预排序的输入会把插补值静默写到错误日期。两个插补入口现统一排序。同一处修复连续缺失路径把 `cum_feed` / `weight_kg` / `pred_*` 等拟合中间列写进输出表的 schema 污染。
- **Gompertz 生长曲线检查在多数个体上静默失效**（issue #29）：拟合公式含子集表达式（`y_gomp[valid] ~ ... x_gomp[valid]`），触发 `nls` 的 `n %% respLength == 0` 校验失败并抛 `str2lang("~")` 错误，被 `tryCatch` 吞成 `NULL` 后整头动物跳过检查（FIRE 实测 211 头中 190 头从未被检查）。改为数据框建模 + 公式变量名，同时修复 `predict(newdata=)` 因名称不匹配被静默忽略的问题。
- **紧凑日期 `YYYYMMDD` 被误判为 Excel 序列号**（issue #9）。
- **LMM 补偿量缺少符号守卫与下限护栏**（issue #13）。
- FCR / ADFI 除零与退化数据守卫（issue #11）。
- 边界条件 8 处修正（issue #19）：空表汇总 `percentage` 为 0 而非 NaN、全 NA 序列不再产生 ±Inf、QC flag 列不被 NA 污染、`Location` 不再拼出字面 `"NA"`、单列 birth info 不再报错等。
- 排序隐式依赖与位置索引越界防护（issue #15）；硬编码阈值与表头跳过行数接入 config（issue #16）。
- `ZhenM_merge_config()` 对未知配置键告警，三个校正开关的依赖关系文档化（issue #17）。
- 体重插补只写「被插补的位置」，观测值不被改写成为显式不变量（issue #26）。
- legacy 移除提示改用 `identical()` 判定，恢复三处入口的友好报错可达性（issue #28；原 `match.arg` 使其成为死代码）。
- LMM 校正入口先 `copy(raw_dt)`，不再按引用给调用方的表留下 `is_feed_normal_record` 残留列（issue #30）。
- 采食插补残留 NA 会被下游 `sum(na.rm = TRUE)` 静默当作 0 时，显式告警（issue #32）。
- Overall QC 早退路径的 summary 列名与正常路径统一为 `associated_animal_ids`（issue #33），`qc_overall_summary.csv` 表头不再随数据状态变化。
- 删除 `.impute_weight_national()` 中重复的数据充足性检查（issue #31，零行为变化）。

## 1.1.2

### Added
- 新增校正机制开关 `use_record_feed_correction` 与 `use_lmm_feed_correction`（默认 TRUE，行为与 V1.1.1 完全一致）。关闭记录级物理纠正后回退「置零」路径；关闭日级 LMM 兜底后仅保留 6kg 日上限校验。用于校正机制消融实验（issue #5）。
- 新增实验性叠加校正开关 `use_lmm_stacking`（默认 FALSE）：记录级纠正成功后串联互补式 LMM，只补偿「噪声置零类」丢失的克数，已被物理封顶的类型不再入模（避免二次补偿）。注入式仿真基准三设备验证：变体 F 全面优于纯记录级纠正且 bias 最小。
- 新增注入式仿真基准脚本 `测试/simulation_benchmark.R`（复刻 Jiao et al. 2016 设计，支持 YANGXIANG/FIRE/NEDAP），作为校正机制改动的裁判台。
- 新增消融实验脚本 `测试/compare_correction_variants.R`：以 6 行对照矩阵量化各校正机制的净效果。

## 1.1.1

### Changed
- **采食量校正模型重构**：被 flag 的采食记录不再「置零排除」，改为按 flag 类型物理封顶/归零（新增 `.correct_feed_records()`）——噪声类（负值/极端速度小采食/长时间零速）置 0，`speed_too_fast` 封顶到 `170×时长/60`，`feed_too_high` 封顶到个体 P99（用干净记录计算）。南沙数据 `ADFI_g` 1745→1995.5，校正量 -442→-139.5 g/天。
- **日级 LMM 校正改为兜底**：记录级纠正成功时跳过 `normal_feed_sum + β×flag` 校正（避免二次校正），仅保留 6kg 日上限校验 `flag_daily_feed_over_limit`；记录级纠正失败时才走原日级 LMM。

### Fixed
- **个体日增重口径修复**：`adg_g` 从 `diff(daily_weight_g)`（未除以天数）改为 `diff(daily_weight_g)/as.numeric(diff(record_date))`，单位 g/天。

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
