ZhenM_list_source_files <- function(data_path, patterns) {
  subfolders <- list.dirs(data_path, full.names = TRUE, recursive = TRUE)
  subfolders <- unique(c(data_path, subfolders))
  result <- character(0)

  for (subfolder in subfolders) {
    for (pattern in patterns) {
      result <- c(result, list.files(subfolder, pattern = pattern, full.names = TRUE))
    }
  }

  result <- unique(result[file.exists(result)])
  file_names <- basename(result)
  birth_like <- grepl("birth|\u51fa\u751f", file_names, ignore.case = TRUE)
  result[!birth_like]
}

#' 附加「测定天数」列（measurement_day 的唯一口径定义，issue #23）
#'
#' 口径：每条记录属于该个体「测定的第几天」——首条有效记录为 1，逐日递增
#' （即 `record_date - 该头首个 record_date + 1`）。日期缺失的记录为 NA；
#' 整头无有效日期的个体整列为 NA。
#'
#' `ZhenM_validate_standard_records()` 直接调用本函数，读取路径与 schema 校验
#' 共用同一定义（此前 schema 侧按「首末日跨度 max-min」计算，得每头一个常数，
#' 与列名语义不符，且与 phenotype 里把它当时间轴的用法相矛盾）。
#'
#' @param dt 标准记录表（需含 animal_id / record_date）
#' @return 附加/覆盖 measurement_day 后的 dt
#' @keywords internal
ZhenM_attach_measurement_day <- function(dt) {
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
  if (!"record_date" %in% names(dt)) return(dt)

  dt[, measurement_day := NA_real_]
  first_day <- dt[!is.na(record_date), .(first_date = min(record_date, na.rm = TRUE)), by = animal_id]
  dt[first_day, on = "animal_id", measurement_day := as.numeric(record_date - i.first_date) + 1]
  dt
}

#' Read and parse data format file (JSON ONLY)
#' @keywords internal
.read_shared_data_format_file <- function(format_path) {
  if (!file.exists(format_path)) {
    stop(paste0("Format file not found: ", format_path), call. = FALSE)
  }
  
  if (!grepl("\\.json$", tolower(format_path))) {
    stop("Only JSON format (.json) is supported for data format configuration.", call. = FALSE)
  }

  # jsonlite 为 Imports（issue #18）：.json 是唯一受支持的 format 格式，
  # 依赖由 DESCRIPTION 保证，无需运行时软守卫
  parse_positions <- function(pos_str) {
    if (is.null(pos_str) || length(pos_str) == 0 || is.na(pos_str) || trimws(pos_str) == "") return(integer(0))
    vals <- suppressWarnings(as.integer(trimws(unlist(strsplit(as.character(pos_str), ",")))))
    vals[!is.na(vals)]
  }

  normalize_text <- function(x) {
    x <- gsub("（", "(", x, fixed = TRUE)
    x <- gsub("）", ")", x, fixed = TRUE)
    trimws(x)
  }

  to_standard_field <- function(name_value) {
    nv <- normalize_text(name_value)
    code <- sub("^.*\\(([^()]*)\\).*$", "\\1", nv)
    if (!identical(code, nv)) {
      code <- toupper(trimws(code))
      if (code %in% c("ID", "AGE", "DFI", "VISIT_TIME", "END_TIME", "DURATION", "FEED_INTAKE", "WEIGHT", "LOCATION")) {
        return(switch(code,
          VISIT_TIME = "Visit_time",
          END_TIME = "End_time",
          FEED_INTAKE = "Feed_intake",
          DURATION = "Duration",
          ID = "ID",
          AGE = "AGE",
          DFI = "DFI",
          WEIGHT = "Weight",
          LOCATION = "Location"
        ))
      }
    }
    key <- tolower(gsub("[^a-zA-Z]", "", nv))
    switch(key,
      id = "ID",
      age = "AGE",
      dfi = "DFI",
      visittime = "Visit_time",
      endtime = "End_time",
      duration = "Duration",
      feedintake = "Feed_intake",
      weight = "Weight",
      location = "Location",
      NA_character_
    )
  }

  cfg <- jsonlite::fromJSON(format_path, simplifyVector = FALSE)

  coerce_pos <- function(x) {
    if (is.null(x) || length(x) == 0) return(integer(0))
    if (is.list(x)) x <- unlist(x)
    if (is.character(x) && length(x) == 1 && grepl(",", x, fixed = TRUE)) return(parse_positions(x))
    vals <- suppressWarnings(as.integer(x))
    vals[!is.na(vals)]
  }

  read_types <- cfg$read_types
  type_map <- list(
    character_cols = coerce_pos(read_types$character_cols),
    numeric_cols = coerce_pos(read_types$numeric_cols),
    date_cols = coerce_pos(read_types$date_cols)
  )

  std_map <- cfg$standard_mapping
  if (is.null(std_map)) std_map <- cfg$field_map
  field_map <- list()

  if (!is.null(std_map) && length(std_map) > 0) {
    for (nm in names(std_map)) {
      item <- std_map[[nm]]
      std_field <- to_standard_field(nm)
      if (is.na(std_field) && !is.null(item$name)) {
        std_field <- to_standard_field(as.character(item$name)[1])
      }
      if (is.na(std_field)) next

      col_pos <- NA_integer_
      if (!is.null(item$position)) col_pos <- suppressWarnings(as.integer(item$position)[1])
      if (is.na(col_pos) && !is.null(item$column_position)) col_pos <- suppressWarnings(as.integer(item$column_position)[1])
      if (is.na(col_pos)) next

      unit <- NA_character_
      if (!is.null(item$unit)) unit <- as.character(item$unit)[1]

      src_name <- nm
      if (!is.null(item$source_name)) src_name <- as.character(item$source_name)[1]
      if (!is.null(item$name)) src_name <- as.character(item$name)[1]

      field_map[[std_field]] <- list(
        position = col_pos,
        unit = unit,
        source_name = src_name
      )
    }
  }

  id_col <- if (!is.null(field_map$ID)) field_map$ID$position else NA_integer_
  if (is.na(id_col) && length(type_map$character_cols) > 0) id_col <- type_map$character_cols[1]

  # issue #16：可选键 header_skip——表头前的跳过行数（xlsx），缺省由各读取端自定
  list(
    id_col = id_col,
    character_cols = if (!is.null(type_map$character_cols)) type_map$character_cols else integer(0),
    numeric_cols = if (!is.null(type_map$numeric_cols)) type_map$numeric_cols else integer(0),
    date_cols = if (!is.null(type_map$date_cols)) type_map$date_cols else integer(0),
    field_map = field_map,
    header_skip = cfg$header_skip
  )
}

#' 读取 format 配置的表头跳过行数（可选键 header_skip）
#'
#' @param format_info format 文件解析结果
#' @param default format 文件未提供 header_skip 时使用的默认值（扬翔 1、FIRE/NEDAP 2）
#' @return 非负整数行数；header_skip 无效时告警并回退 default
#' @keywords internal
.format_header_skip <- function(format_info, default) {
  if (!is.null(format_info$header_skip)) {
    n <- suppressWarnings(as.integer(format_info$header_skip))
    if (length(n) >= 1 && !is.na(n[1]) && n[1] >= 0) return(n[1])
    warning(sprintf("format 文件 header_skip 无效（需非负整数），回退默认 %s", default), call. = FALSE)
  }
  as.integer(default)
}

#' Unit conversion helper function
#' @keywords internal
.convert_by_unit <- function(x, unit, target) {
  vals <- suppressWarnings(as.numeric(x))
  if (is.null(unit) || is.na(unit) || trimws(unit) == "") return(vals)

  u <- tolower(trimws(unit))
  u <- gsub("μ", "u", u, fixed = TRUE)

  if (target == "mass_g") {
    if (u %in% c("g", "gram", "grams")) return(vals)
    if (u %in% c("kg", "kilogram", "kilograms")) return(vals * 1000)
    if (u %in% c("mg", "milligram", "milligrams")) return(vals / 1000)
    return(vals)
  }

  if (target == "duration_sec") {
    if (u %in% c("s", "sec", "secs", "second", "seconds")) return(vals)
    if (u %in% c("min", "mins", "minute", "minutes")) return(vals * 60)
    if (u %in% c("h", "hr", "hrs", "hour", "hours")) return(vals * 3600)
    return(vals)
  }

  if (target == "age_day") {
    if (u %in% c("day", "days", "d")) return(vals)
    if (u %in% c("week", "weeks", "wk", "w")) return(vals * 7)
    if (u %in% c("month", "months", "mon", "m")) return(vals * 30)
    return(vals)
  }

  vals
}


#' 按位置安全取列名
#'
#' issue #15：names(dt)[pos] 在 pos 为 0/负数/越界时会静默引入 NA 或缩空向量，
#' 后续 dt[[NA]] 的报错难以定位。此助手丢弃无效位置并告警；全部无效时返回 NULL，
#' 使调用方的 length()/is.null() 守卫自然走降级分支。
#'
#' @param nm 列名向量
#' @param pos 位置索引（标量或向量）
#' @return 有效位置对应的列名；无有效位置时返回 NULL
#' @keywords internal
.col_names_by_pos <- function(nm, pos) {
  pos <- as.integer(pos)
  if (length(pos) == 0) return(NULL)
  ok <- !is.na(pos) & pos >= 1L & pos <= length(nm)
  if (!all(ok)) {
    warning(sprintf("忽略无效列位置 %s（有效范围 1..%d）",
                    paste(pos[!ok], collapse = ", "), length(nm)), call. = FALSE)
  }
  if (!any(ok)) return(NULL)
  nm[pos[ok]]
}

#' Read single YANGXIANG xlsx file
#' @keywords internal
.read_yangxiang_file <- function(file, format_info) {
  # issue #16：表头跳过行数可由 format 文件 header_skip 配置，默认 1（历史行为）
  hdr_skip <- .format_header_skip(format_info, default = 1)
  # Read with all text columns first
  tmp <- readxl::read_xlsx(file, sheet = 1, n_max = 0, skip = hdr_skip)
  n_col <- ncol(tmp)
  col_types <- rep("text", n_col)

  raw <- data.table::as.data.table(
    readxl::read_xlsx(file, sheet = 1, col_types = col_types, skip = hdr_skip)
  )

  raw[, source_file := basename(file)]

  # Fill down logic for merged cells
  id_col_name <- .col_names_by_pos(names(raw), format_info$id_col)
  if (is.null(id_col_name)) {
    stop(sprintf("无法按 format 定位 ID 列（id_col=%s 越界，文件 %s 共 %d 列）",
                 paste(format_info$id_col, collapse = ","), basename(file), ncol(raw)), call. = FALSE)
  }
  raw[[id_col_name]] <- trimws(as.character(raw[[id_col_name]]))
  raw[[id_col_name]][raw[[id_col_name]] == ""] <- NA_character_

  # Fill down ID and date columns
  fill_cols <- .col_names_by_pos(names(raw), c(format_info$character_cols, format_info$date_cols))
  fill_cols <- setdiff(fill_cols, c("age_day", "daily_feed_g", "duration_sec",
                                     "feed_g", "weight_g", "当天进分栏器次数"))

  for (col in fill_cols) {
    if (col %in% names(raw)) {
      raw[[col]] <- zoo::na.locf(raw[[col]], na.rm = FALSE)
    }
  }

  raw
}

#' Parse datetime with multiple formats
#' @keywords internal
.parse_datetime <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = "UTC"))
  # issue #20：文本解析与 Excel 序列号回退统一到 .parse_temporal()，
  # 与 ZhenM_parse_datetime / ZhenM_safe_to_idate 共用同一顺序表与序列号区间
  .parse_temporal(x, out = "datetime")
}

#' Recursively find supported tabular data files
#' @keywords internal
.find_tabular_files_recursive <- function(data_path) {
  list.files(
    path = data_path,
    pattern = "\\.(csv|txt|xls|xlsx)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
}

#' Read one tabular source file by extension
#'
#' @param file 文件路径
#' @param xlsx_skip xlsx 表头前跳过行数（issue #16：可由调用方按 format 文件
#'   header_skip 传入；FIRE 历史 2）
#' @keywords internal
.read_tabular_file <- function(file, xlsx_skip = 2L) {
  file_lower <- tolower(file)
  if (grepl("\\.(xls|xlsx)$", file_lower)) {
    return(data.table::as.data.table(readxl::read_excel(file, sheet = 1, na = "", skip = xlsx_skip)))
  }
  # 文本文件一律按字符读入（issue #23 记录设计取舍）：
  # fread 的类型推断在「同一列混有多种日期格式」或「ID 列前导零」时会误判，
  # 最坏情况把整列 datetime 推成全 NA（NEDAP/FIRE 历史数据均出现过），且错误
  # 静默不可恢复。代价是读入阶段内存约为数值型的 2 倍，换来的是列内容保真；
  # 后续各读取器按 format 文件的列类型定义显式转换（as.numeric / .parse_temporal）。
  # 因此此处刻意保留 colClasses = "character"，不做自动类型推断。
  data.table::fread(
    file,
    header = TRUE,
    stringsAsFactors = FALSE,
    colClasses = "character"
  )

}

#' Read birth info file
#' @keywords internal
.read_birth_info <- function(birth_info_path) {
  file_lower <- tolower(birth_info_path)

  if (grepl("\\.(xls|xlsx)$", file_lower)) {
    birth_dt <- data.table::as.data.table(readxl::read_excel(birth_info_path, sheet = 1))
  } else if (grepl("\\.csv$", file_lower)) {
    birth_dt <- data.table::fread(birth_info_path)
  } else {
    stop("Birth info file must be xls/xlsx/csv", call. = FALSE)
  }

  names(birth_dt) <- tolower(names(birth_dt))

  # issue #19：单列文件无法同时提供 ID 与出生日期，原实现取 names()[2] 得 NA
  # 并抛出难懂的 "column not found: [NA]"
  if (ncol(birth_dt) < 2) {
    stop(paste0("出生信息文件至少需要两列（ID 与出生日期）: ", basename(birth_info_path)), call. = FALSE)
  }

  id_col <- names(birth_dt)[grepl("id|耳标|编号", names(birth_dt), ignore.case = TRUE)][1]
  if (is.na(id_col)) id_col <- names(birth_dt)[1]

  birth_col <- names(birth_dt)[grepl("birth|出生|日期", names(birth_dt), ignore.case = TRUE)][1]
  if (is.na(birth_col)) birth_col <- names(birth_dt)[2]

  birth_dt <- birth_dt[, c(id_col, birth_col), with = FALSE]
  data.table::setnames(birth_dt, c("ID", "birth_date"))
  birth_dt[, birth_date := ZhenM_safe_to_idate(birth_date)]

  birth_dt
}

#' Get mapped column name from format metadata
#' @keywords internal
.get_mapped_col <- function(dt, format_info, field_name) {
  safe_col_by_pos <- function(pos) {
    if (is.null(pos) || length(pos) == 0 || is.na(pos) || pos < 1 || pos > ncol(dt)) return(NULL)
    names(dt)[pos]
  }

  fm <- format_info$field_map
  if (!is.null(fm) && !is.null(fm[[field_name]])) {
    mapped <- safe_col_by_pos(fm[[field_name]]$position)
    if (!is.null(mapped)) return(mapped)
  }

  NULL
}

#' Get mapped unit from format metadata
#' @keywords internal
.get_mapped_unit <- function(format_info, field_name) {
  fm <- format_info$field_map
  if (is.null(fm) || is.null(fm[[field_name]])) return(NA_character_)
  fm[[field_name]]$unit
}

#' Safely convert by unit when metadata is available
#' @keywords internal
.convert_by_unit_safe <- function(x, from_unit, to_unit) {
  .convert_by_unit(x, from_unit, to_unit)
}

#' FIRE mass normalization helper (target: g)
#' @keywords internal
.normalize_fire_mass_to_g <- function(x, declared_unit = NA_character_, field_name = "Feed_intake") {
  vals <- suppressWarnings(as.numeric(x))
  unit <- tolower(trimws(ifelse(is.null(declared_unit) || is.na(declared_unit), "", declared_unit)))

  # Trust explicit kg declaration.
  if (unit %in% c("kg", "kilogram", "kilograms")) return(vals * 1000)
  # Trust explicit g declaration unless data scale strongly suggests kg.
  if (unit %in% c("g", "gram", "grams")) {
    nz <- abs(vals[is.finite(vals) & !is.na(vals)])
    if (length(nz) == 0) return(vals)
    med <- stats::median(nz, na.rm = TRUE)
    if (identical(field_name, "Feed_intake") && med <= 20) return(vals * 1000)
    if (identical(field_name, "Weight") && med <= 500) return(vals * 1000)
    return(vals)
  }

  # Unit missing/unknown: infer by typical FIRE scale.
  nz <- abs(vals[is.finite(vals) & !is.na(vals)])
  if (length(nz) == 0) return(vals)
  med <- stats::median(nz, na.rm = TRUE)
  if (identical(field_name, "Feed_intake")) {
    if (med <= 20) return(vals * 1000)
    return(vals)
  }
  if (identical(field_name, "Weight")) {
    if (med <= 500) return(vals * 1000)
    return(vals)
  }
  vals
}
