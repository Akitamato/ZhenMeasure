#' Unified data reading interface
#'
#' Reads raw data from any supported source (YANGXIANG, NEDAP, FIRE) and
#' converts it to ZhenMeasure standard format.
#'
#' @param data_path Path to raw data directory
#' @param data_type Data source type: "YANGXIANG", "NEDAP", or "FIRE"
#' @param format_path Path to Data_format.json file (only .json is supported)
#' @param birth_info_path Path to birth info file (required for NEDAP/FIRE, optional for YANGXIANG)
#' @return A data.table in standard format with columns:
#'   ID, AGE, DFI, Visit_time, End_time, Duration, Feed_intake, Weight, Location
#' @export
#' @examples
#' \dontrun{
#' # Read YANGXIANG data
#' data <- ZhenM_read_data("path/to/data", "YANGXIANG", "path/to/format.json")
#'
#' # Read NEDAP data with birth info
#' data <- ZhenM_read_data("path/to/data", "NEDAP", "path/to/format.json",
#'                       birth_info_path = "path/to/birth.xlsx")
#' }
ZhenM_read_data <- function(data_path, data_type, format_path, birth_info_path = NULL) {
  # Validate inputs
  if (!dir.exists(data_path)) {
    stop(paste0("Data directory not found: ", data_path), call. = FALSE)
  }

  if (!file.exists(format_path)) {
    stop(paste0("Format file not found: ", format_path), call. = FALSE)
  }

  data_type <- toupper(data_type)
  if (!data_type %in% c("YANGXIANG", "NEDAP", "FIRE")) {
    stop("data_type must be one of: YANGXIANG, NEDAP, FIRE", call. = FALSE)
  }

  # Check birth info requirement
  if (data_type %in% c("NEDAP", "FIRE")) {
    if (is.null(birth_info_path)) {
      warning(paste0(data_type, " data typically requires birth_info_path to calculate AGE. ",
                    "AGE column will be NA without it."))
    } else if (!file.exists(birth_info_path)) {
      warning(paste0("Birth info file not found: ", birth_info_path, ". AGE will be NA."))
      birth_info_path <- NULL
    }
  }

  # Call appropriate conversion function
  standard_data <- switch(
    data_type,
    YANGXIANG = ZhenM_convert_yangxiang_to_standard(data_path, format_path),
    NEDAP = ZhenM_convert_nedap_to_standard(data_path, format_path, birth_info_path),
    FIRE = ZhenM_convert_fire_to_standard(data_path, format_path, birth_info_path)
  )

  # Keep a deterministic order: sort IDs by their minimum Visit_time, 
  # then sort records within each ID by Visit_time ascending.
  if (data.table::is.data.table(standard_data) && "Visit_time" %in% names(standard_data)) {
    standard_data[, .visit_time_na_flag := is.na(Visit_time)]
    standard_data[, .id_min_time := {
      if (all(is.na(Visit_time))) {
        as.POSIXct(NA)
      } else {
        min(Visit_time, na.rm = TRUE)
      }
    }, by = ID]
    data.table::setorder(standard_data, .visit_time_na_flag, .id_min_time, Visit_time, ID)
    standard_data[, c(".visit_time_na_flag", ".id_min_time") := NULL]
  }

  # Add metadata
  data.table::setattr(standard_data, "data_type", data_type)
  data.table::setattr(standard_data, "data_path", data_path)
  data.table::setattr(standard_data, "format_path", format_path)
  data.table::setattr(standard_data, "birth_info_path", birth_info_path)

  message(paste0("Successfully read ", nrow(standard_data), " records from ",
                data_type, " data source"))
  message(paste0("  - Unique IDs: ", length(unique(standard_data$ID))))
  visit_dates <- ZhenM_safe_to_idate(standard_data$Visit_time)
  visit_dates <- visit_dates[!is.na(visit_dates)]
  if (length(visit_dates) > 0) {
    message(paste0("  - Date range: ", min(visit_dates), " to ", max(visit_dates)))
  } else {
    message("  - Date range: NA (all Visit_time are NA)")
  }

  standard_data
}

#' Convert standard format to daily aggregated format
#'
#' Aggregates visit-level standard format data to daily level.
#' This is the format expected by QC and phenotype calculation functions.
#'
#' @param standard_data A data.table in standard format (from ZhenM_read_data)
#' @return A data.table with daily aggregated data
#' @export
ZhenM_standard_to_daily <- function(standard_data) {
  dt <- data.table::copy(standard_data)

  # Add record_date
  dt[, record_date := ZhenM_safe_to_idate(Visit_time)]

  # Aggregate to daily level
  daily_dt <- dt[, .(
    median_weight_g = median(Weight, na.rm = TRUE),
    daily_feed_g = sum(Feed_intake, na.rm = TRUE),
    n_visits = .N,
    total_duration_sec = sum(Duration, na.rm = TRUE),
    age_day = median(AGE, na.rm = TRUE),
    location = paste(unique(Location), collapse = ";")
  ), by = .(animal_id = ID, record_date)]

  # Sort
  data.table::setorder(daily_dt, animal_id, record_date)

  daily_dt
}

#' Get data summary statistics
#'
#' @param standard_data A data.table in standard format
#' @return A list with summary statistics
#' @export
ZhenM_data_summary <- function(standard_data) {
  dt <- data.table::copy(standard_data)
  dt[, record_date := ZhenM_safe_to_idate(Visit_time)]

  list(
    n_records = nrow(dt),
    n_animals = length(unique(dt$ID)),
    date_range = range(dt$record_date, na.rm = TRUE),
    weight_range_kg = range(dt$Weight / 1000, na.rm = TRUE),
    feed_range_g = range(dt$Feed_intake, na.rm = TRUE),
    age_range = range(dt$AGE, na.rm = TRUE),
    missing_age = sum(is.na(dt$AGE)),
    missing_weight = sum(is.na(dt$Weight)),
    missing_feed = sum(is.na(dt$Feed_intake)),
    data_type = attr(standard_data, "data_type")
  )
}


#' Convert YANGXIANG data to standard format
#'
#' Reads YANGXIANG raw data and converts it to ZhenMeasure standard format.
#'
#' @param data_path Path to YANGXIANG raw data directory (containing xlsx files)
#' @param format_path Path to Data_format.json file (only .json is supported)
#' @return A data.table in standard format
#' @export
ZhenM_convert_yangxiang_to_standard <- function(data_path, format_path) {
  # Read format info
  format_info <- .read_shared_data_format_file(format_path)

  # Read all xlsx files (including subdirectories)
  files <- list.files(data_path, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) {
    stop("No xlsx files found in YANGXIANG data directory", call. = FALSE)
  }

  all_data <- lapply(files, function(file) {
    .read_yangxiang_file(file, format_info)
  })

  dt <- data.table::rbindlist(all_data, use.names = TRUE, fill = TRUE)

  # Convert to standard format
  standard_dt <- .map_yangxiang_to_standard(dt, format_info)

  # Validate
  ZhenM_validate_standard_format(standard_dt, strict = FALSE)

  standard_dt
}

#' Map YANGXIANG columns to standard format
#' @keywords internal
.map_yangxiang_to_standard <- function(dt, format_info) {
  convert_by_unit <- function(x, unit, target) {
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

  safe_col_by_pos <- function(pos) {
    if (is.null(pos) || length(pos) == 0 || is.na(pos) || pos < 1 || pos > ncol(dt)) return(NULL)
    names(dt)[pos]
  }

  mapped_col <- function(field_name) {
    fm <- format_info$field_map
    if (is.null(fm) || is.null(fm[[field_name]])) return(NULL)
    safe_col_by_pos(fm[[field_name]]$position)
  }

  mapped_unit <- function(field_name) {
    fm <- format_info$field_map
    if (is.null(fm) || is.null(fm[[field_name]])) return(NA_character_)
    fm[[field_name]]$unit
  }

  standard_dt <- data.table::data.table()

  # ID
  id_col_name <- mapped_col("ID")
  if (is.null(id_col_name)) id_col_name <- safe_col_by_pos(format_info$id_col)
  if (is.null(id_col_name)) stop("Cannot locate ID column from format file.", call. = FALSE)
  standard_dt[, ID := trimws(as.character(dt[[id_col_name]]))]

  # Numeric columns mapping
  num_names <- names(dt)[format_info$numeric_cols]
  date_names <- names(dt)[format_info$date_cols]

  # Weight (usually first numeric column)
  weight_col <- mapped_col("Weight")
  if (is.null(weight_col) && length(num_names) >= 1) {
    weight_col <- num_names[1]
    if ("进分栏器体重" %in% names(dt)) weight_col <- "进分栏器体重"
    if ("weight_g" %in% names(dt)) weight_col <- "weight_g"
  }
  if (!is.null(weight_col)) {
    standard_dt[, Weight := convert_by_unit(dt[[weight_col]], mapped_unit("Weight"), "mass_g")]
  } else {
    standard_dt[, Weight := NA_real_]
  }

  # Feed_intake (usually second numeric column)
  feed_col <- mapped_col("Feed_intake")
  if (is.null(feed_col) && length(num_names) >= 2) {
    feed_col <- num_names[2]
    if ("采食量" %in% names(dt)) feed_col <- "采食量"
    if ("feed_g" %in% names(dt)) feed_col <- "feed_g"
  }
  if (!is.null(feed_col)) {
    standard_dt[, Feed_intake := convert_by_unit(dt[[feed_col]], mapped_unit("Feed_intake"), "mass_g")]
  } else {
    standard_dt[, Feed_intake := NA_real_]
  }

  # Duration
  duration_col <- mapped_col("Duration")
  if (is.null(duration_col) && "采食时长" %in% names(dt)) duration_col <- "采食时长"
  if (is.null(duration_col) && "duration_sec" %in% names(dt)) duration_col <- "duration_sec"

  if (!is.null(duration_col)) {
    standard_dt[, Duration := convert_by_unit(dt[[duration_col]], mapped_unit("Duration"), "duration_sec")]
  } else {
    standard_dt[, Duration := NA_real_]
  }

  # Visit_time
  visit_col <- mapped_col("Visit_time")
  if (is.null(visit_col) && "采食开始时间" %in% names(dt)) visit_col <- "采食开始时间"
  if (is.null(visit_col) && "start_time" %in% names(dt)) visit_col <- "start_time"
  if (is.null(visit_col) && length(date_names) >= 1) visit_col <- date_names[1]

  if (!is.null(visit_col)) {
    standard_dt[, Visit_time := .parse_datetime(dt[[visit_col]])]
  } else {
    standard_dt[, Visit_time := as.POSIXct(NA)]
  }

  # End_time
  end_col <- mapped_col("End_time")
  if (is.null(end_col) && "采食结束时间" %in% names(dt)) end_col <- "采食结束时间"
  if (is.null(end_col) && "end_time" %in% names(dt)) end_col <- "end_time"

  if (!is.null(end_col)) {
    standard_dt[, End_time := .parse_datetime(dt[[end_col]])]
  } else {
    standard_dt[, End_time := as.POSIXct(NA)]
  }

  # Derive End_time from Duration when possible
  if (all(is.na(standard_dt$End_time)) && !all(is.na(standard_dt$Visit_time)) && !all(is.na(standard_dt$Duration))) {
    standard_dt[, End_time := Visit_time + Duration]
  }

  # AGE
  age_col <- mapped_col("AGE")
  if (is.null(age_col) && "日龄" %in% names(dt)) age_col <- "日龄"
  if (is.null(age_col) && "age_day" %in% names(dt)) age_col <- "age_day"

  if (!is.null(age_col)) {
    standard_dt[, AGE := convert_by_unit(dt[[age_col]], mapped_unit("AGE"), "age_day")]
  } else {
    standard_dt[, AGE := NA_real_]
  }

  # DFI (prefer explicit mapping from format file)
  dfi_col <- mapped_col("DFI")
  if (!is.null(dfi_col)) {
    standard_dt[, DFI := convert_by_unit(dt[[dfi_col]], mapped_unit("DFI"), "mass_g")]
  } else {
    standard_dt[, DFI := NA_real_]
  }

  # Location
  location_col <- mapped_col("Location")
  if (is.null(location_col) && "测定站" %in% names(dt)) location_col <- "测定站"
  if (is.null(location_col) && "location" %in% names(dt)) location_col <- "location"

  if (!is.null(location_col)) {
    standard_dt[, Location := as.character(dt[[location_col]])]
  } else {
    standard_dt[, Location := NA_character_]
  }

  # Calculate DFI from Feed_intake when mapping does not provide it
  if (all(is.na(standard_dt$DFI)) && !all(is.na(standard_dt$Visit_time))) {
    standard_dt[, record_date := ZhenM_safe_to_idate(Visit_time)]
    standard_dt[, DFI := sum(Feed_intake, na.rm = TRUE), by = .(ID, record_date)]
    standard_dt[, record_date := NULL]
  }

  # Reorder columns
  standard_dt[, .(ID, AGE, DFI, Visit_time, End_time, Duration, Feed_intake, Weight, Location)]
}

#' Convert NEDAP data to standard format
#'
#' Reads NEDAP raw data and converts it to ZhenMeasure standard format.
#' NEDAP data supports csv/txt/xls/xlsx files in nested folders.
#'
#' @param data_path Path to NEDAP raw data directory
#' @param format_path Path to Data_format.json file
#' @param birth_info_path Path to birth info Excel/CSV file (optional)
#' @return A data.table in standard format
#' @export
ZhenM_convert_nedap_to_standard <- function(data_path, format_path, birth_info_path = NULL) {
  format_info <- .read_shared_data_format_file(format_path)

  files <- .find_tabular_files_recursive(data_path)
  if (length(files) == 0) {
    stop("No csv/txt/xls/xlsx files found in NEDAP data directory", call. = FALSE)
  }

  all_data <- lapply(files, .read_tabular_file)
  dt <- data.table::rbindlist(all_data, use.names = TRUE, fill = TRUE)

  birth_info <- NULL
  if (!is.null(birth_info_path) && file.exists(birth_info_path)) {
    birth_info <- .read_birth_info(birth_info_path)
  }

  standard_dt <- .map_nedap_to_standard(dt, format_info, birth_info)
  ZhenM_validate_standard_format(standard_dt, strict = FALSE)
  standard_dt
}

#' Convert FIRE data to standard format
#'
#' Reads FIRE raw data and converts it to ZhenMeasure standard format.
#' FIRE data supports csv/txt/xls/xlsx files in nested folders.
#'
#' @param data_path Path to FIRE raw data directory
#' @param format_path Path to Data_format.json file
#' @param birth_info_path Path to birth info Excel/CSV file (optional)
#' @return A data.table in standard format
#' @export
ZhenM_convert_fire_to_standard <- function(data_path, format_path, birth_info_path = NULL) {
  format_info <- .read_shared_data_format_file(format_path)

  files <- .find_tabular_files_recursive(data_path)
  if (length(files) == 0) {
    stop("No csv/txt/xls/xlsx files found in FIRE data directory", call. = FALSE)
  }

  all_data <- lapply(files, .read_tabular_file)
  dt <- data.table::rbindlist(all_data, use.names = TRUE, fill = TRUE)

  birth_info <- NULL
  if (!is.null(birth_info_path) && file.exists(birth_info_path)) {
    birth_info <- .read_birth_info(birth_info_path)
  }

  standard_dt <- .map_fire_to_standard(dt, format_info, birth_info)
  ZhenM_validate_standard_format(standard_dt, strict = FALSE)
  standard_dt
}

#' Map NEDAP columns to standard format
#' @keywords internal
.map_nedap_to_standard <- function(dt, format_info, birth_info = NULL) {
  standard_dt <- data.table::data.table()

  safe_col_by_pos <- function(pos) {
    if (is.null(pos) || length(pos) == 0 || is.na(pos) || pos < 1 || pos > ncol(dt)) return(NULL)
    names(dt)[pos]
  }

  id_col_name <- .get_mapped_col(dt, format_info, "ID")
  if (is.null(id_col_name)) id_col_name <- safe_col_by_pos(format_info$id_col)
  if (is.null(id_col_name)) stop("Cannot locate ID column from format file.", call. = FALSE)
  standard_dt[, ID := trimws(as.character(dt[[id_col_name]]))]

  visit_col <- .get_mapped_col(dt, format_info, "Visit_time")
  if (is.null(visit_col)) {
    date_names <- names(dt)[format_info$date_cols]
    if (length(date_names) >= 1) visit_col <- date_names[1]
  }
  if (!is.null(visit_col)) {
    standard_dt[, Visit_time := .parse_datetime(dt[[visit_col]])]
  } else {
    standard_dt[, Visit_time := as.POSIXct(NA)]
  }

  duration_col <- .get_mapped_col(dt, format_info, "Duration")
  if (is.null(duration_col)) {
    num_names <- names(dt)[format_info$numeric_cols]
    if (length(num_names) >= 1) duration_col <- num_names[1]
  }
  if (!is.null(duration_col)) {
    standard_dt[, Duration := .convert_by_unit_safe(dt[[duration_col]], .get_mapped_unit(format_info, "Duration"), "duration_sec")]
  } else {
    standard_dt[, Duration := NA_real_]
  }

  standard_dt[, End_time := Visit_time + Duration]

  feed_col <- .get_mapped_col(dt, format_info, "Feed_intake")
  if (is.null(feed_col)) {
    num_names <- names(dt)[format_info$numeric_cols]
    if (length(num_names) >= 2) feed_col <- num_names[2]
  }
  if (!is.null(feed_col)) {
    standard_dt[, Feed_intake := .convert_by_unit_safe(dt[[feed_col]], .get_mapped_unit(format_info, "Feed_intake"), "mass_g")]
  } else {
    standard_dt[, Feed_intake := NA_real_]
  }

  weight_col <- .get_mapped_col(dt, format_info, "Weight")
  if (is.null(weight_col)) {
    num_names <- names(dt)[format_info$numeric_cols]
    if (length(num_names) >= 3) weight_col <- num_names[3]
  }
  if (!is.null(weight_col)) {
    standard_dt[, Weight := .convert_by_unit_safe(dt[[weight_col]], .get_mapped_unit(format_info, "Weight"), "mass_g")]
  } else {
    standard_dt[, Weight := NA_real_]
  }

  location_col <- .get_mapped_col(dt, format_info, "Location")
  if (is.null(location_col) && length(format_info$character_cols) > 0) {
    location_col <- names(dt)[format_info$character_cols[1]]
  }
  if (!is.null(location_col)) {
    standard_dt[, Location := as.character(dt[[location_col]])]
  } else {
    standard_dt[, Location := NA_character_]
  }

  if (!is.null(birth_info)) {
    standard_dt <- merge(standard_dt, birth_info, by = "ID", all.x = TRUE)
    standard_dt[, AGE := as.numeric(ZhenM_safe_to_idate(Visit_time) - birth_date)]
    standard_dt[, birth_date := NULL]
  } else {
    standard_dt[, AGE := NA_real_]
  }

  if (!all(is.na(standard_dt$Visit_time))) {
    standard_dt[, record_date := ZhenM_safe_to_idate(Visit_time)]
    standard_dt[, DFI := sum(Feed_intake, na.rm = TRUE), by = .(ID, record_date)]
    standard_dt[, record_date := NULL]
  } else {
    standard_dt[, DFI := NA_real_]
  }

  standard_dt[, .(ID, AGE, DFI, Visit_time, End_time, Duration, Feed_intake, Weight, Location)]
}

#' Map FIRE columns to standard format
#' @keywords internal
.map_fire_to_standard <- function(dt, format_info, birth_info = NULL) {
  standard_dt <- data.table::data.table()

  safe_col_by_pos <- function(pos) {
    if (is.null(pos) || length(pos) == 0 || is.na(pos) || pos < 1 || pos > ncol(dt)) return(NULL)
    names(dt)[pos]
  }

  id_col_name <- .get_mapped_col(dt, format_info, "ID")
  if (is.null(id_col_name)) id_col_name <- safe_col_by_pos(format_info$id_col)
  if (is.null(id_col_name)) stop("Cannot locate ID column from format file.", call. = FALSE)
  standard_dt[, ID := trimws(as.character(dt[[id_col_name]]))]

  visit_col <- .get_mapped_col(dt, format_info, "Visit_time")
  if (is.null(visit_col)) {
    date_names <- names(dt)[format_info$date_cols]
    if (length(date_names) >= 1) visit_col <- date_names[1]
  }
  if (!is.null(visit_col)) {
    standard_dt[, Visit_time := .parse_datetime(dt[[visit_col]])]
  } else {
    standard_dt[, Visit_time := as.POSIXct(NA)]
  }

  end_col <- .get_mapped_col(dt, format_info, "End_time")
  if (is.null(end_col)) {
    date_names <- names(dt)[format_info$date_cols]
    if (length(date_names) >= 2) end_col <- date_names[2]
  }
  if (!is.null(end_col)) {
    standard_dt[, End_time := .parse_datetime(dt[[end_col]])]
  } else {
    standard_dt[, End_time := as.POSIXct(NA)]
  }

  standard_dt[, Duration := as.numeric(difftime(End_time, Visit_time, units = "secs"))]

  feed_col <- .get_mapped_col(dt, format_info, "Feed_intake")
  if (is.null(feed_col)) {
    num_names <- names(dt)[format_info$numeric_cols]
    if (length(num_names) >= 1) feed_col <- num_names[1]
  }
  if (!is.null(feed_col)) {
    standard_dt[, Feed_intake := .normalize_fire_mass_to_g(
      dt[[feed_col]],
      declared_unit = .get_mapped_unit(format_info, "Feed_intake"),
      field_name = "Feed_intake"
    )]
  } else {
    standard_dt[, Feed_intake := NA_real_]
  }

  weight_col <- .get_mapped_col(dt, format_info, "Weight")
  if (is.null(weight_col)) {
    num_names <- names(dt)[format_info$numeric_cols]
    if (length(num_names) >= 2) weight_col <- num_names[2]
  }
  if (!is.null(weight_col)) {
    standard_dt[, Weight := .normalize_fire_mass_to_g(
      dt[[weight_col]],
      declared_unit = .get_mapped_unit(format_info, "Weight"),
      field_name = "Weight"
    )]
  } else {
    standard_dt[, Weight := NA_real_]
  }

  location_col <- .get_mapped_col(dt, format_info, "Location")
  if (is.null(location_col) && length(format_info$character_cols) > 0) {
    location_col <- names(dt)[format_info$character_cols[1]]
  }
  if (!is.null(location_col)) {
    standard_dt[, Location := as.character(dt[[location_col]])]
  } else {
    standard_dt[, Location := NA_character_]
  }

  if (!is.null(birth_info)) {
    standard_dt <- merge(standard_dt, birth_info, by = "ID", all.x = TRUE)
    standard_dt[, AGE := as.numeric(ZhenM_safe_to_idate(Visit_time) - birth_date)]
    standard_dt[, birth_date := NULL]
  } else {
    standard_dt[, AGE := NA_real_]
  }

  if (!all(is.na(standard_dt$Visit_time))) {
    standard_dt[, record_date := ZhenM_safe_to_idate(Visit_time)]
    standard_dt[, DFI := sum(Feed_intake, na.rm = TRUE), by = .(ID, record_date)]
    standard_dt[, record_date := NULL]
  } else {
    standard_dt[, DFI := NA_real_]
  }

  standard_dt[, .(ID, AGE, DFI, Visit_time, End_time, Duration, Feed_intake, Weight, Location)]
}
