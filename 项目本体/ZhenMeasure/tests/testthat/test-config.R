# Unit tests for configuration management

test_that("ZhenM_default_config returns correct structure", {
  cfg_national <- ZhenM_default_config("national_standard")

  expect_equal(cfg_national$qc_method, "national_standard")

  expect_true("national_standard" %in% names(cfg_national))
})

test_that("ZhenM_default_config errors on legacy method", {
  # issue #28：原用 match.arg(choices="national_standard")，非匹配参数会被 match.arg
  # 先抛出，友好迁移提示永远不可达。改为 identical 后必须给出可读提示。
  expect_error(ZhenM_default_config("legacy"), "Legacy QC method was removed in V1.0.0")
})

test_that("legacy removal message is reachable in all three entry points (issue #28)", {
  skip_if_not_installed("data.table")
  expect_error(ZhenM_default_config("legacy"), "Legacy QC method was removed in V1.0.0")
  expect_error(
    ZhenM_impute_data(
      data.table::data.table(animal_id = "A", record_date = as.Date("2024-01-01"),
                             daily_feed_g = 1, daily_weight_g = 1),
      impute_method = "legacy"
    ),
    "Legacy imputation method was removed in V1.0.0"
  )
  expect_error(
    run_zhen_measure(data_path = "/nonexistent", data_type = "FIRE", format_path = "x",
                     output_dir = tempdir(), qc_method = "legacy"),
    "Legacy QC method was removed in V1.0.0"
  )
})

test_that("national_standard config has required parameters", {
  cfg <- ZhenM_default_config("national_standard")$national_standard

  expect_equal(cfg$weight_range, c(25, 140))
  expect_equal(cfg$growth_curve_r2_min, 0.95)
  expect_equal(cfg$feed_intake_range, c(0, 6))
  expect_equal(cfg$impute_r2_min, 0.95)
  expect_equal(cfg$min_test_days, 60)
  expect_equal(cfg$min_stage_days, 20)
  expect_equal(cfg$max_missing_rate, 0.15)

  expect_true(is.data.frame(cfg$fcr_ranges))
  expect_equal(nrow(cfg$fcr_ranges), 9)

  # 校正机制开关默认开启（issue #5 重写后：use_lmm_feed_correction 是 daily_feed_g
  # 来源的总开关，use_record_feed_correction 降级为对照臂开关）
  expect_true(cfg$use_record_feed_correction)
  expect_true(cfg$use_lmm_feed_correction)

  # LMM 协变量截尾界（Casey 2003）：对象是逐错误类型的**累计协变量**，
  # 不是日总采食量，也不是响应
  expect_equal(cfg$lmm_trim_dfie_g, c(0, 3500))
  expect_equal(cfg$lmm_trim_otde_s, c(0, 5000))
})

test_that("已移除的 use_lmm_stacking 键给出明确提示（issue #5 重写）", {
  # 该键随 stack 分支一并退役。用户若仍传它，必须得到「已移除」的明确说明，
  # 而不是混在「可能是拼写错误」的泛泛提示里。
  ws <- character()
  withCallingHandlers(
    ZhenM_merge_config(
      list(national_standard = list(use_lmm_stacking = TRUE)), "national_standard"
    ),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # 恰好一条 warning——同一件事报两条互相矛盾的提示最误导人
  expect_length(ws, 1L)
  expect_match(ws, "use_lmm_stacking 已被移除")
  expect_false(grepl("未识别的键", ws))

  # 该键不生效：与 issue #17 的其它未知键一致，modifyList 仍会把它塞进列表，
  # 但全仓已无读取处（grep use_lmm_stacking 只剩本助手与测试）。此处锁住的是
  # 「没有任何开关被它带偏」——传 TRUE 后日级校正开关仍是默认值。
  merged <- suppressWarnings(ZhenM_merge_config(
    list(national_standard = list(use_lmm_stacking = TRUE)), "national_standard"
  ))
  expect_true(merged$national_standard$use_lmm_feed_correction)

  # 反向锁：真正的拼写错误仍要被点名
  expect_warning(
    ZhenM_merge_config(
      list(national_standard = list(use_record_feed_corection = TRUE)), "national_standard"
    ),
    "未识别的键"
  )
})

test_that("ZhenM_merge_config merges user config", {
  user_cfg <- list(national_standard = list(weight_range = c(30, 130)))
  merged <- ZhenM_merge_config(user_cfg, "national_standard")

  expect_equal(merged$national_standard$weight_range, c(30, 130))
  expect_equal(merged$national_standard$feed_intake_range, c(0, 6))
})

test_that("ZhenM_merge_config warns on unknown keys (issue #17)", {
  # 拼写错误键：告警且路径含节名
  expect_warning(
    merged <- ZhenM_merge_config(
      list(national_standard = list(use_record_feed_corection = FALSE)),
      "national_standard"),
    "use_record_feed_corection"
  )
  # 未识别键不生效：读取处走默认值
  expect_true(merged$national_standard$use_record_feed_correction)

  # 未知顶层节同样告警
  expect_warning(
    ZhenM_merge_config(list(legacy = list(weight_threshold = 0.5)), "national_standard"),
    "legacy"
  )

  # 合法键不告警
  expect_silent(ZhenM_merge_config(
    list(national_standard = list(weight_range = c(30, 130),
                                  speed_zero_long_duration_sec = 600)),
    "national_standard"))

  # data.frame 叶子（fcr_ranges）不递归列名，不误报
  expect_silent(ZhenM_merge_config(
    list(national_standard = list(fcr_ranges = data.frame(weight_min = 30, weight_max = 40))),
    "national_standard"))

  # 嵌套路径：未知子树整体报出（不深入未知的下层）
  expect_warning(
    ZhenM_merge_config(list(national_standard = list(gompertz = list(foo = 1))), "national_standard"),
    "national_standard.gompertz"
  )
})

test_that("dt_threads 走 config 且管线退出时还原线程数（issue #37）", {
  skip_if_not_installed("data.table")

  expect_equal(ZhenM_default_config("national_standard")$dt_threads, 1L)
  expect_equal(ZhenM_merge_config(NULL, "national_standard")$dt_threads, 1L)
  # 用户可覆盖
  expect_equal(ZhenM_merge_config(list(dt_threads = 4L), "national_standard")$dt_threads, 4L)

  before <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(before), add = TRUE)

  # setDTthreads() 会把请求值截到可用核数，故先在测试里算出「实际会被设成几」
  data.table::setDTthreads(2L)
  expected <- data.table::getDTthreads()
  data.table::setDTthreads(before)

  # 用一个必然失败的路径跑管线：只关心入口是否按 config 设了线程、
  # 以及 on.exit 是否把调用方的原值还回去（不依赖跑通全流程）。
  lf <- tempfile(fileext = ".txt")
  suppressWarnings(try(
    run_zhen_measure(data_path = tempdir(), data_type = "FIRE",
                     format_path = tempfile(), log_file = lf,
                     config = list(dt_threads = 2L)),
    silent = TRUE))

  expect_equal(data.table::getDTthreads(), before)
  if (file.exists(lf)) {
    txt <- paste(readLines(lf, warn = FALSE), collapse = "\n")
    expect_true(grepl(paste0("data.table 线程数: ", expected), txt, fixed = TRUE))
  }
})
