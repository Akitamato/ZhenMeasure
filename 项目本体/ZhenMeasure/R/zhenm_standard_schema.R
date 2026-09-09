#' Return standard-record schema definition
#'
#' @return A table describing field names, required flags, and target types.
#' @export
ZhenM_standard_record_fields <- function() {
  data.table::data.table(
    field = c(
      "animal_id", "device_type", "record_date", "start_time", "end_time",
      "duration_sec", "feed_g", "daily_feed_g", "weight_g", "location",
      "source_file", "age_day", "measurement_day"
    ),
    required = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    type = c(
      "character", "character", "IDate", "POSIXct", "POSIXct",
      "numeric", "numeric", "numeric", "numeric", "character",
      "character", "numeric", "numeric"
    )
  )
}

#' Validate and standardize standard-record data
#'
#' @param dt Data to validate.
#' @param strict Whether at least one of age_day or measurement_day is required.
#' @return A standardized table with missing fields filled and types converted.
#'   Note: source_file column is removed, and measurement_day is the per-record
#'   day index of the measurement period (first valid record_date = 1, then +1
#'   per day; see `ZhenM_attach_measurement_day()`).
#'   Original columns (ID, Visit_time, etc.) are removed after mapping to avoid redundancy.
#' @export
ZhenM_validate_standard_records <- function(dt, strict = TRUE) {
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)

  # 建立映射：原始列名 -> 标准列名
  # 例：ID -> animal_id, Visit_time -> record_date, Weight -> weight_g 等
  # 策略：只创建必要的标准列，然后删除原始列，避免数据重复
  
  # 第一步：重命名或创建标准列
  mapping <- list(
    animal_id = c("ID"),
    record_date = c("Visit_time"),
    weight_g = c("Weight"),
    feed_g = c("Feed_intake"),
    daily_feed_g = c("DFI"),
    age_day = c("AGE"),
    duration_sec = c("Duration"),
    start_time = c("Visit_time"),
    end_time = c("End_time"),
    location = c("Location")
  )
  
  # 对需要简单重命名的列进行处理（ID->animal_id, Location->location等）
  if ("ID" %in% names(dt) && !"animal_id" %in% names(dt)) {
    data.table::setnames(dt, "ID", "animal_id")
  }
  
  if ("Location" %in% names(dt) && !"location" %in% names(dt)) {
    data.table::setnames(dt, "Location", "location")
  }
  
  if ("End_time" %in% names(dt) && !"end_time" %in% names(dt)) {
    data.table::setnames(dt, "End_time", "end_time")
  }

  # 对需要转换或计算的列进行处理
  if ("Visit_time" %in% names(dt)) {
    if (!"record_date" %in% names(dt)) {
      dt[, record_date := ZhenM_safe_to_idate(Visit_time)]
    }
    if (!"start_time" %in% names(dt)) {
      dt[, start_time := Visit_time]
    }
  }
  
  if ("Weight" %in% names(dt) && !"weight_g" %in% names(dt)) {
    dt[, weight_g := suppressWarnings(as.numeric(Weight))]
  }
  
  if ("Feed_intake" %in% names(dt) && !"feed_g" %in% names(dt)) {
    dt[, feed_g := suppressWarnings(as.numeric(Feed_intake))]
  }
  
  if ("Duration" %in% names(dt) && !"duration_sec" %in% names(dt)) {
    dt[, duration_sec := suppressWarnings(as.numeric(Duration))]
  }
  
  if ("AGE" %in% names(dt) && !"age_day" %in% names(dt)) {
    dt[, age_day := suppressWarnings(as.numeric(AGE))]
  }
  
  if ("DFI" %in% names(dt) && !"daily_feed_g" %in% names(dt)) {
    dt[, daily_feed_g := suppressWarnings(as.numeric(DFI))]
  }
  
  # 兼容 median_weight_g (从 daily 格式)
  if (!"weight_g" %in% names(dt) && "median_weight_g" %in% names(dt)) {
    dt[, weight_g := suppressWarnings(as.numeric(median_weight_g))]
  }

  schema <- ZhenM_standard_record_fields()
  required_fields <- schema[required == TRUE, field]
  if (!strict) {
    required_fields <- setdiff(required_fields, "device_type")
  }
  missing_required <- setdiff(required_fields, names(dt))
  if (length(missing_required) > 0) {
    stop(paste0("Missing required fields at standard-record level: ", paste(missing_required, collapse = ", ")), call. = FALSE)
  }

  optional_fields <- setdiff(schema$field, names(dt))
  for (field in optional_fields) {
    dt[, (field) := NA]
  }

  if ("record_date" %in% names(dt)) dt[, record_date := ZhenM_safe_to_idate(record_date)]
  if ("start_time" %in% names(dt)) dt[, start_time := ZhenM_parse_datetime(start_time)]
  if ("end_time" %in% names(dt)) dt[, end_time := ZhenM_parse_datetime(end_time)]

  numeric_fields <- schema[type == "numeric", field]
  for (field in intersect(numeric_fields, names(dt))) {
    dt[, (field) := suppressWarnings(as.numeric(get(field)))]
  }

  character_fields <- schema[type == "character", field]
  for (field in intersect(character_fields, names(dt))) {
    dt[, (field) := as.character(get(field))]
  }

  if ("device_type" %in% names(dt) && all(is.na(dt$device_type))) {
    inferred_device <- attr(dt, "data_type", exact = TRUE)
    if (is.null(inferred_device)) inferred_device <- attr(dt, "device_type", exact = TRUE)
    if (!is.null(inferred_device) && length(inferred_device) == 1 && !is.na(inferred_device)) {
      dt[, device_type := as.character(inferred_device)]
    }
  }

  # 计算 measurement_day：每条记录所属的「测定天数」（首条有效记录 = 1，逐日递增）
  # issue #23：此前按「首末日跨度 max-min」计算，得到每头一个常数，与列名语义不符
  # （下游 phenotype 把它当时间轴用，常数会让 ADG 的时间差恒为 0）。
  # 现统一委托 ZhenM_attach_measurement_day()，读取路径与 schema 校验共用唯一定义。
  dt <- ZhenM_attach_measurement_day(dt)

  if (!any(!is.na(dt$age_day)) && !any(!is.na(dt$measurement_day)) && strict) {
    stop("At least one of age_day or measurement_day is required at standard-record level.", call. = FALSE)
  }

  # 第二步：删除原始列，避免数据冗余
  # 删除已被映射到标准列的原始列
  original_cols_to_remove <- c("ID", "Visit_time", "End_time", "Duration", 
                                "Feed_intake", "Weight", "AGE", "DFI", "Location",
                                "source_file", "median_weight_g")
  existing_cols_to_remove <- intersect(original_cols_to_remove, names(dt))
  if (length(existing_cols_to_remove) > 0) {
    dt[, (existing_cols_to_remove) := NULL]
  }
  
  # 调整列顺序
  schema_cols <- schema[field != "source_file", field]
  ordered_cols <- c(intersect(schema_cols, names(dt)), 
                    setdiff(names(dt), schema_cols))
  dt[, ordered_cols, with = FALSE]
}

