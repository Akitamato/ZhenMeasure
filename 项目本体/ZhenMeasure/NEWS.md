# ZhenMeasure news

## 1.2.0

### ⚠️ 会改变表型结果（但幅度小）

日级采食量校正（LMM）**整段重写为 Jiao et al. (2014, *J Anim Sci* 92:2377–2386) 的文献实现**（issue #5）。
该实现**默认关闭**：出厂默认的 `daily_feed_g` 仍由记录级物理纠正（A，`use_record_feed_correction`）产生，
相对 V1.1.4 出厂默认（stack）只动少数天、表型 ADFI 变化在 0.32% 以内（见下"A 转正后的实数据影响"）。
需要文献 LMM 时显式设 `use_lmm_feed_correction = TRUE`；届时 `daily_feed_g` 的来源、模型形式与
全部护栏都换成文献口径，**ADFI / FCR 会明显移动**（ADG 不受影响，LMM 只作用于采食量）。

**为什么默认是 A 而不是文献 LMM**：注入式基准上 L 的 accuracy 在三设备 × 三档注入率共 9 格中
全面低于 A（见下"注入式基准"）。文献实现保留为**可选增强**，改一个键即可切换。

**本版还有第二处会改动结果的行为变更**——恢复 `flag_feed_out_of_range` 的生产者（与 issue #5 无关，
扬翔 6/1613 个表型行、ADFI 变化 ≤0.47%），见下「其他行为变更」。

### Breaking changes

- **移除 `use_lmm_stacking` 开关**：stack（记录级纠正成功后串联互补式 LMM）整条分支退役，
  `noise_flags` / `noise_dur_total` 一并删除。用户若仍传该键，`ZhenM_merge_config()` 会给出
  「该键已被移除」的专门提示（不是泛泛的"未识别键"）。
- **门控与记录级纠正的成败解耦**（裁定 0）。原门控是「记录级纠正成功时跳过 LMM」，
  移除 stack 后若保留原门控，文献 LMM 将永不运行，故门控收成
  `if (use_lmm_feed_correction) 跑 LMM else 只做出厂校验`。
  两个键因此**相互独立**：`use_record_feed_correction`（默认 TRUE）管记录级物理纠正 A，
  `use_lmm_feed_correction`（默认 FALSE）管是否用文献 LMM **覆写**日值。
  `.correct_feed_records()` 的产物（内部 `feed_filtered` 列）**就是默认路径下 `daily_feed_g` 的来源**；
  只有打开 LMM 时它才退居 A 臂对照列。
- **`daily_feed_g` 的失败兜底不再回退到"误差自由和"**：拟合失败 / 训练样本不足 / `lme4` 缺失时，
  `daily_feed_g` **保持不变**，`lmm_ef_g` 与 `lmm_correction_g` 置 `NA_real_`，并 message 报明原因。
  （用误差自由和兜底会系统性丢掉被 flag 记录的采食量，正是这套方法要消除的偏差。）
- 新增两个配置键（**必须显式出现在 `ZhenM_default_config()` 中，否则会被未知键告警静默丢弃**）：

  ```r
  lmm_trim_dfie_g = c(0, 3500),   # 逐错误类型的累计采食量（FID_p）截尾界
  lmm_trim_otde_s = c(0, 5000),   # 逐错误类型的累计占据时长（OTD_p）截尾界
  ```

  **截的是协变量，不是响应，也不是当日总采食量。** Casey et al. (2003)/Jiao et al. (2016) 原文的
  `DFIe` / `OTDe` 里的 `e` 是 **error**（出错访问）而非 error-free；界只作用于**训练集**，
  应用端一律不截。这两条与管线自身的日上限 `feed_daily_max_g`（默认 6000 g）管的不是同一个对象，互不冲突。

### 其他行为变更（随本版一并发布，与 issue #5 无关）

- **恢复 `flag_feed_out_of_range` 的生产者**（issue #40，`eb4ebdf`）。roxygen / man 一直承诺「当天任一
  记录出界 → 整天 `daily_feed_g` 置 NA 交插补」这条设备故障保护分支，但全包内**没有任何生产者**
  （唯一赋值点是"列缺失时初始化为 FALSE"），**恒不可达**；`feed_intake_range` 配置项也因此没有消费方。
  现在记录级 `feed_g` 落在 `feed_intake_range` 之外即打标（本版第 11 号 flag）。
  - **量纲**：config 以 **kg** 给出（默认 `c(0, 6)`），`feed_g` 是**克**，故先过 `.normalize_feed_range()`
    再比较——V0.2.6 的 C-1 正是漏了这一步。归一化后默认为 `[0, 6000]` g。
  - **有意不并入 `is_outlier_feed`**：两者下游语义不同。`is_outlier_feed` = 单条数值不可信、需记录级
    纠正/置零；`out_of_range` = 设备故障、整天数据整体不可用，由日聚合的整日置 NA 分支单独消费。
    并入 OR 会让记录级纠正口径无谓扩散。
  - 日级 6 kg 上限同时**不再硬编码**：`.finalize_daily_feed(dt, feed_max_g = 6000)` 新增参数，
    由新增的 `.feed_daily_max_g(ns_cfg)` 从 config 接线，三处调用点统一。
  - **实数据端到端影响**（默认 config，与 `d9807fe` 对比）：FIRE 211 头 **0** 个表型行变化；
    扬翔 547 头 **6/1613** 个表型行变化（2 头 × 3 阶段），ADFI 变化 **≤0.47%**；63805 个日级行中 2 行变化；
    `qc_summary.csv` 三设备各新增一行 `flag_feed_out_of_range,0,0,HIGH`。新增 4 个单测。

### 性能与可复现性（读取 / QC 核心链路，与 issue #5 无关）

四条来自 `fix/issues-35-42` 那轮，均已在真机上量过，并以**逐字节等价的输出**作为等价性证据：

- **扬翔 xlsx 读取去掉探测性的二次读**（issue #35，`cad20d6`）。每个 `.xlsx` 原先被 `readxl` 打开两次：
  先用 `read_xlsx(n_max = 0)` 探一次列数，再全量读一次；而 `n_max = 0` 并不减少解析量。单值
  `col_types = "text"` 会自动循环到实际列数，故探测调用整个删掉。实测单文件 **18.0 s → 12.7 s**
  （约 −30%）；等价性 559,114 × 24 个单元格与改前逐格零差异。顺带在 `.gitignore` 显式排除扬翔全量
  原始数据目录（19 GB / 2159 个 xlsx）——目录名有误导性，只要 `data_path` 指向其父目录就会被递归扫进来。

- **四处「逐头全表扫描取行号」改为循环外建索引**（issue #36，`0083079`）。
  `which(dt$animal_id == id)` 在每头动物的循环里各扫一遍整表（O(n_animal × n_row)）；改为循环外用
  `split()` 建「个体 → 行号」查表、循环内 O(1)，新增内部函数 `.row_index_of()`，四处调用点同步替换
  （体重 QC、FCR 锚定矫正、体重插补、采食量插补）。实测扬翔 668 头 × 179 万行下该阶段占核心链路
  **53–58%**，其中体重 QC 那一句 `which()` 就实测 8.1 s。等价性：FIRE 奥斯本与扬翔南沙共 **10 个输出
  文件逐字节一致**（含 1,591,023 行的 `corrected_records.csv` 与 63,806 行的 `daily_records.csv`）。

- **去掉两处整表副本**（issue #38，`4d7b0bb`）。`.correct_feed_records()` 与
  `.apply_feed_lmm_correction()` 各自在入口做一次 `data.table::copy()`，只为拿一块可写空间 / 加一列临时
  掩码列。前者改为**纯函数**——只读 `dt`、只返回 `feed_corrected` 向量，各 flag 规则在多态位置做向量
  运算，分组 P99 只物化「个体 + 采食量」两列，不再复制整张宽表；后者把掩码列换成局部逻辑向量、
  分组求和只带 4 列。实测扬翔 668 头默认配置峰值 RSS **1974 → 1640 MB**（−334 MB / −17%），
  其中 LMM 路径增量 +188 → +22 MB、记录级纠正增量 +264 → +94 MB；10 个输出文件与 `d9807fe` 逐位一致。

- **管线入口显式设定 data.table 线程数，默认串行**（issue #37，`57d5e15`）。此前包内不碰线程设置，
  即跟随 `data.table::getDTthreads()` 的默认值（本机 32 逻辑核 → 取值 16），而实测这对扬翔 668 头这类
  规模是**负收益**（2 台设备 × 9 个规模档 × 3 次重复里 **9/9 档墙钟变慢**，扬翔 668 头慢 8.6%），
  且结果与机器核数挂钩、不可复现。`ZhenM_default_config()` 新增 `base_config$dt_threads = 1L`
  （1 = 显式串行（默认，可复现）；0 = 恢复 data.table 自身默认；NULL = 完全不干预、保持调用时的当前值）；
  `run_zhen_measure()` 在合并 config 后调用 `data.table::setDTthreads()`，并在 `on.exit` **还原调用方
  原值**，避免把全局状态泄漏给同一进程内的后续调用；启动 banner 与 logger 均打印生效的线程数。

### Changed

- **响应改为 error-free DFI**（`ef_dfi_g`）：当天**全部**干净访问的采食量之和。
  干净 = 不带任何 error flag、非 `is_outlier_feed`、非 `flag_feed_out_of_range`。
  当天没有干净访问时响应无定义（该天由 `ef_n_visit >= 1` 挡在训练集外）。
- **协变量按文献预先固定的指派表构造**，不再是数据驱动的 `has_`/`dur_` 二选一：
  ETP（占比）= 当日带该类错误的访问数 / 当日访问总数，覆盖我们能采集的 9 类 + `flag_STL_FI` 扩展项，共 10 项；
  OTD（当日累计占据时长）给文献的类型 1,2 与 6–14，我们落到 6 项；
  FID（当日累计采食量）给文献的类型 4,5 与 15,16，我们落到 2 项（`flag_duration_negative`、`flag_duration_too_long`）。
  合计 18 项（文献 16+11+4 = 31 项中我们能构造的部分；LWD/FWD 四类需逐次访问的入场/离场体重，
  标准格式每次只有 1 个 `Weight`，属数据格式限制）。
- **ADG 改为每头常数**（`adg_const_g`）：个体日体重序列对日期的 OLS 斜率，不再用逐日差分。
  有效体重天 < 2 或日期跨度为 0 的头为 NA 并退出训练集（校正在应用端仍照常作用于它）。
- **校正按字面 `+β̂x` 应用**，去掉 V1.1.1–V1.1.4 期间加的全部护栏：`β>0` 符号守卫、
  `speed_max × 时长 / 60` 物理封顶、`pmax(0, ·)` 下限护栏、`+ visits_n` 近似项。
  文献 Table 1 的系数本就**有正有负**（FIV-high +61.40、OTV-high +1750.0），单侧化不是文献做法。
- **训练门槛改为可解释的双门槛**：`sum(train_idx) >= max(30, 10 × 入模项数)` 且 `n_animal_train >= 10`。
  零变异项在拟合前剔除（工程护栏），但**剔除清单必须报进 message**。
- 拟合固定 `lmerControl(optimizer = "bobyqa")`，并用 `lme4::isSingular()` 检测边界解。
  **只报不治**：不改模型规格（删 ADG 或删随机截距都偏离文献），也不引入"奇异时自动降级"的隐形开关。
- 日级输出新增两个**台账列**：`lmm_ef_g`（误差自由和）与 `lmm_correction_g`（校正量），
  使 `daily_feed_g - lmm_ef_g == lmm_correction_g` 可逐行对账。
  注意**恒等式在插补之前成立**：Step 6 会覆写被出口置 NA 的天，插补行的两个台账列保持插补前取值。
- `.finalize_daily_feed()` 新增 `flag_daily_feed_nonpositive`，使"校正后 ≤ 0 被置 NA"这条路径可审计
  （此前只有 `flag_daily_feed_over_limit`）。

### 实数据端到端回归（`use_lmm_feed_correction = TRUE` vs V1.1.4 默认）

> 本节描述的是**打开文献 LMM 后**的效果，不是出厂默认行为。出厂默认（A）的影响见下一节。

三设备走完整管线（demo 口径：`test_weight_range = c(200, 20)`、`keep_ids = NULL`；
FIRE 211 头 / 20913 天、NEDAP 29 头 / 1915 天、扬翔（原始数据/南沙）547 头 / 63805 天）。日级逐值 diff：

| 设备 | 个体数 | 日级 `daily_feed_g` 变化 | 平均 \|Δ\| | 最大 \|Δ\| | ADFI 平均 \|Δ\| | ADFI 最大 \|Δ\| |
|---|---|---|---|---|---|---|
| FIRE | 211 / 211 | 4961 / 20913 天（23.7%） | 918 g | 4941 g | 164 g | 712 g |
| NEDAP | 29 / 29 | 451 / 1915 天（23.6%） | 783 g | 2989 g | 154 g | 317 g |
| 扬翔（南沙） | 547 / 547 | 35647 / 63805 天（55.9%） | 788 g | 5020 g | 344 g | 1069 g |

ADG 逐行零变化。阶段行数与个体数三设备均不变，但 `flag_fcr_stage_invalid` 有翻转：
NEDAP 4 行 TRUE→FALSE（无反向）；扬翔 **346 行 TRUE→FALSE、78 行 FALSE→TRUE**——这是真实存在的
QC 分类变化，下游若按该 flag 筛数据需知悉。

**回归锁（确认没有顺手改到记录级路径）**：`use_lmm_feed_correction = FALSE`——**即出厂默认**——时，
日级表与表型表与 V1.1.4 的纯记录级（A）路径**逐值一致**——
FIRE 20913/20913、NEDAP 1915/1915、扬翔 63805/63805 天，所有表型列 `max|Δ| = 0`。
这条锁现在直接覆盖默认路径本身，价值比"只是回退位"更大。

### A 转正后的实数据影响（出厂默认 A vs V1.1.4 出厂默认 stack）

出厂默认的 `daily_feed_g` 由记录级物理纠正（A）产生。与 V1.1.4 的出厂默认（stack，F）
同为 demo 口径的三设备逐值 diff：

| 设备 | 个体数 A/F | 日级 `daily_feed_g` 变化 | 总采食量差异幅度 | 表型 ADFI 最大变化 |
|---|---|---|---|---|
| FIRE | 211 / 211 | 0 / 20913 天 | 0 g（逐位一致） | 无 |
| NEDAP | 29 / 29 | 1 / 1915 天 | 14 g（0.0003%） | 1 头 0.22 g |
| 扬翔 | 547 / 547 | 743 / 63805 天（1.16%） | 5690 g（0.0042%） | 623/1613 阶段行，平均 0.096 g、最大 6.375 g（0.32%） |

扬翔的结构性影响：个体数与阶段行数不变，`flag_fcr_stage_invalid` 分布不变（TRUE 1248 / FALSE 365）；
唯一结构变化是 1 头（`998-025021177551`）的三个阶段行 `n_valid_stages` 各减 1，但该行前后都已是 invalid，
无 QC 分类翻转。**结论：A 转正对表型的影响在 0.32% 以内。**

### 风险量化（文献 LMM 路径的影响半径）

| 项 | FIRE | NEDAP | 扬翔（南沙） |
|---|---|---|---|
| 协变量截尾剔除的训练行占比（R4） | 0.04%（8 行） | 0.00% | 0.01%（7 行） |
| `Σ ETP > 1` 的天占比（R6，我们的 flag 非互斥） | 0.19% | 0.00% | 0.00% |
| `isSingular` / ICC / `kappa(X)`（R5） | FALSE / 0.029 / 9.7e4 | FALSE / 0.089 / 3.5e5 | FALSE / 0.321 / 9.1e5 |

R4 的原始担忧（"3500 g 会在扬翔大面积误伤"）建立在把 3500 g 当成**日总采食量**的误读上；
按正确的对象（逐错误类型的累计量）实测，三设备剔除面都在 0.1% 量级，**默认一律用文献界，不做设备覆盖**。
同理，R6（flag 非互斥导致总占比可超 100%）实测几乎不发生。
R5 唯一的真实代价是 `kappa(X)` 很大（扬翔 9.1e5）——源于 `otd_`/`fid_` 是原始秒/克量纲而
`etp_` 是 0–1 占比，属量纲混用；随机截距方差三设备都**不在边界**（`isSingular` 全 FALSE），
且 `Correction` 不含随机截距，故不影响校正量本身。按裁定"只报不治"，不改模型形式（改即偏离文献）。

### 性能（打开文献 LMM 的边际成本）

18 项固定效应（文献 31 项中我们能构造的部分）比 V1.1.4 默认路径的 3 项 stack 模型大得多，
但 LMM 只占总耗时的零头。同一份 demo 语料、同一台机器上，管线总耗时的「开 LMM − 关 LMM」边际。
**出厂默认不跑 LMM，所以下表是"选择开启"的代价**：

| 设备 | 新默认 LMM 边际 | V1.1.4 LMM 边际 | 占该设备「关 LMM」总耗时的比例 |
|---|---|---|---|
| FIRE（211 头 / 20913 天） | +11.3 s | +6.4 s | 50.3%（基线仅 22.5 s，LMM 是主要成本） |
| NEDAP（29 头 / 1915 天） | +0.4 s | +0.1 s | 13.4% |
| 扬翔（547 头 / 63805 天） | +5.2 s | +4.7 s | 3.6% |

扬翔边际只多了 0.5 s：它的瓶颈在 2159 个 xlsx 的读取与 QC，不在拟合。FIRE 的绝对耗时小
（22.5 s），LMM 占比因此被放大到一半——若 FIRE 批量跑成为瓶颈，这才是需要关注的点。

### 注入式基准：文献 LMM（L）的准确度低于记录级路径（A）——默认取 A 的直接依据

`测试/simulation_benchmark.R` 的变体矩阵随本次重写重定义为
**C0**（无校正）/ **A**（纯记录级物理纠正，V1.1.1 路径）/ **L**（文献 LMM，可选增强、默认关闭）/
**Ln**（同 L 但把协变量截尾界放宽到 ±Inf）。accuracy 越高越好，bias 正 = 高估：

| 设备 | 注入率 | C0 | A | L（= Ln） |
|---|---|---|---|---|
| FIRE | 5% / 10% / 20% | 0.8248 / 0.7185 / 0.5417 | **0.8552 / 0.7403 / 0.5559** | 0.7968 / 0.6872 / 0.5019 |
| NEDAP | 5% / 10% / 20% | 0.8543 / 0.7626 / 0.5964 | **0.8751 / 0.7737 / 0.6023** | 0.8220 / 0.7241 / 0.5470 |
| 扬翔 | 5% / 10% / 20% | 0.7210 / 0.5471 / 0.3289 | **0.7463 / 0.5670 / 0.3426** | 0.6978 / 0.5204 / 0.2991 |

bias 同向且更极端：L 是三档里低估最严重的（FIRE@20% −0.4548 vs A −0.3688 vs C0 −0.4071）。
`Ln ≡ L` 逐位相同——协变量截尾在实测数据上边界不绑定（越界行 ≤0.04%），去掉它毫无影响。

**怎么读这个结果**：基准的"真值"是注入前的干净世界。A 按物理规则把虚高记录封顶回个体 P99、
把噪声类置 0，等于把真值近似写了回去；L 则把被 flag 的记录整条踢出响应，改用"典型出错访问
含多少采食量"的回归系数估回——在本基准的损坏模式（`feed × U(1.5,3)` / 置 0 / 取负，5–20% 注入率）下
系统性估少。**这是文献方法与我们 flag 语义之间的真实落差，不是实现 bug**，也正是把这套
LMM 设为**可选而非默认**（而非当初裁定的"转正"）的量化依据：

- 基准的损坏比现实更极端（现实错误率约 5%，且不是人为放大 1.5–3 倍）；
- 文献的 16 类错误互斥、全进联合模型，我们只有 10 类且其中多项在本数据零变异被剔除，
  模型比文献薄得多；
- 文献对标的指标是试验期 ADFI 的整体无偏，本基准逐日逐值的 accuracy 比它严苛。

**默认值裁定（2026-09-16）**：出厂默认取 **A**（`use_lmm_feed_correction = FALSE`）。两个理由：
① 上表 9 格 A 全胜 L，且 L 是三档里低估最严重的；② L→A 的切换已被逐值验证——
`use_lmm_feed_correction = FALSE` 精确复现 V1.1.4 的 A 路径（三设备 `max|Δ| = 0`），
现有回归锁直接覆盖默认路径；若反过来默认 L，这条锁就失效了。
文献 LMM 仍随包发布，`use_lmm_feed_correction = TRUE` 一行开启，供后续研究使用。

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
