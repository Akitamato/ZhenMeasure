#' Progress bar wrapper for ZhenMeasure operations
#'
#' @param total Total number of iterations
#' @param title Progress bar title
#' @return Progress bar object
#' @keywords internal
.zhenm_progress <- function(total, title = "Processing") {
  if (requireNamespace("progress", quietly = TRUE)) {
    progress::progress_bar$new(
      format = paste0(title, " [:bar] :percent :eta"),
      total = total,
      clear = FALSE,
      width = 60
    )
  } else {
    NULL
  }
}

#' Update progress bar
#' @keywords internal
.zhenm_progress_tick <- function(pb) {
  if (!is.null(pb)) pb$tick()
}

#' Enhanced error messages with suggestions
#' @keywords internal
.zhenm_error <- function(msg, suggestion = NULL) {
  full_msg <- paste0("Error: ", msg)
  if (!is.null(suggestion)) {
    full_msg <- paste0(full_msg, "\n  Suggestion: ", suggestion)
  }
  stop(full_msg, call. = FALSE)
}

#' Enhanced warning messages
#' @keywords internal
.zhenm_warning <- function(msg, suggestion = NULL) {
  full_msg <- paste0("Warning: ", msg)
  if (!is.null(suggestion)) {
    full_msg <- paste0(full_msg, "\n  Suggestion: ", suggestion)
  }
  warning(full_msg, call. = FALSE)
}

#' Validate input data with helpful messages
#' @keywords internal
.zhenm_validate_input <- function(data, required_cols) {
  if (!is.data.frame(data)) {
    .zhenm_error(
      "Input must be a data.frame or data.table",
      "Convert your data using as.data.frame() or data.table::as.data.table()"
    )
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    .zhenm_error(
      paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")),
      paste0("Ensure your data has columns: ", paste(required_cols, collapse = ", "))
    )
  }

  invisible(TRUE)
}

ZhenM_parse_data_format <- function(format_path) {
  if (!file.exists(format_path)) stop(paste0("Format file not found: ", format_path), call. = FALSE)

  if (grepl("\\.json$", format_path, ignore.case = TRUE)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Need jsonlite package for parsing json format files.", call. = FALSE)
    fmt <- jsonlite::fromJSON(format_path)

    make_seq <- function(x) {
      if (is.null(x)) return(integer(0))
      as.integer(x)
    }

    return(list(
      id_col_pos = make_seq(fmt$ID),
      character_col_pos = make_seq(fmt$Character),
      numeric_col_pos = make_seq(fmt$Numeric),
      date_col_pos = make_seq(fmt$Date),
      id_col_labels = fmt$labels$ID,
      character_col_labels = fmt$labels$Character,
      numeric_col_labels = fmt$labels$Numeric,
      date_col_labels = fmt$labels$Date
    ))
  }

  if (grepl("\\.txt$", format_path, ignore.case = TRUE)) {
    lines <- readLines(format_path, warn = FALSE, encoding = "UTF-8")
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) < 2) stop("Format file is empty or has no data rows.", call. = FALSE)

    # Skip header line
    data_lines <- lines[-1]

    parse_row <- function(line) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
      if (length(parts) < 2) return(NULL)
      list(type = trimws(parts[1]), pos_raw = trimws(parts[2]), labels_raw = if (length(parts) >= 3) trimws(parts[3]) else "")
    }

    rows <- lapply(data_lines, parse_row)
    rows <- Filter(function(r) !is.null(r) && nzchar(r$pos_raw), rows)

    parse_pos <- function(pos_raw) {
      if (!nzchar(pos_raw)) return(integer(0))
      as.integer(strsplit(pos_raw, ",", fixed = TRUE)[[1]])
    }

    parse_labels <- function(labels_raw) {
      if (!nzchar(labels_raw)) return(character(0))
      strsplit(labels_raw, ",", fixed = TRUE)[[1]]
    }

    id_col_pos <- integer(0)
    character_col_pos <- integer(0)
    numeric_col_pos <- integer(0)
    date_col_pos <- integer(0)
    id_col_labels <- character(0)
    character_col_labels <- character(0)
    numeric_col_labels <- character(0)
    date_col_labels <- character(0)

    for (r in rows) {
      pos <- parse_pos(r$pos_raw)
      labs <- parse_labels(r$labels_raw)
      type_lower <- tolower(r$type)

      if (grepl("id", type_lower, ignore.case = TRUE)) {
        id_col_pos <- pos
        id_col_labels <- labs
      } else if (grepl("character|字符", type_lower, ignore.case = TRUE)) {
        character_col_pos <- pos
        character_col_labels <- labs
      } else if (grepl("numeric|数值", type_lower, ignore.case = TRUE)) {
        numeric_col_pos <- pos
        numeric_col_labels <- labs
      } else if (grepl("date|日期", type_lower, ignore.case = TRUE)) {
        date_col_pos <- pos
        date_col_labels <- labs
      }
    }

    # Validate: check for duplicate column positions
    all_pos <- c(id_col_pos, character_col_pos, numeric_col_pos, date_col_pos)
    if (any(duplicated(all_pos))) {
      dupes <- all_pos[duplicated(all_pos)]
      stop(paste0("Duplicate column positions found: ", paste(dupes, collapse = ", ")), call. = FALSE)
    }

    return(list(
      id_col_pos = id_col_pos,
      character_col_pos = character_col_pos,
      numeric_col_pos = numeric_col_pos,
      date_col_pos = date_col_pos,
      id_col_labels = id_col_labels,
      character_col_labels = character_col_labels,
      numeric_col_labels = numeric_col_labels,
      date_col_labels = date_col_labels
    ))
  }

  stop(paste0("Unsupported format file extension: ", format_path, ". Use .json or .txt."), call. = FALSE)
}

ZhenM_colname_by_pos <- function(dt, pos) {
  if (length(pos) == 0) return(character(0))
  valid_pos <- pos[pos >= 1 & pos <= ncol(dt)]
  if (length(valid_pos) == 0) return(character(0))
  names(dt)[valid_pos]
}

ZhenM_safe_to_idate <- function(x) {
  if (inherits(x, "IDate")) return(x)
  if (inherits(x, "Date")) return(data.table::as.IDate(x))
  if (inherits(x, "POSIXct")) return(data.table::as.IDate(x, tz = "UTC"))

  if (is.numeric(x)) {
    return(data.table::as.IDate(as.Date(x, origin = "1899-12-30")))
  }

  x_char <- trimws(as.character(x))
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x_char,
    orders = c(
      "Ymd", "Y-m-d", "Y/m/d", "Ymd HMS", "Ymd HM",
      "Y-m-d H:M:S", "Y-m-d H:M", "Y/m/d H:M:S", "Y/m/d H:M"
    ),
    tz = "UTC"
  ))
  out <- data.table::as.IDate(parsed)

  numeric_like <- suppressWarnings(as.numeric(x_char))
  idx_num <- is.na(out) & !is.na(numeric_like)
  if (any(idx_num)) {
    out[idx_num] <- data.table::as.IDate(as.Date(numeric_like[idx_num], origin = "1899-12-30"))
  }

  out
}

ZhenM_parse_datetime <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x))
  if (is.numeric(x)) {
    origin_time <- as.POSIXct("1899-12-30 00:00:00", tz = "UTC")
    return(origin_time + x * 86400)
  }

  x_char <- trimws(as.character(x))
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x_char,
    orders = c(
      "Ymd HMS", "Ymd HM", "Y-m-d H:M:S", "Y-m-d H:M",
      "Y/m/d H:M:S", "Y/m/d H:M", "Ymd", "Y-m-d", "Y/m/d"
    ),
    tz = "UTC"
  ))

  numeric_like <- suppressWarnings(as.numeric(x_char))
  idx_num <- is.na(parsed) & !is.na(numeric_like)
  if (any(idx_num)) {
    parsed[idx_num] <- as.POSIXct("1899-12-30 00:00:00", tz = "UTC") + numeric_like[idx_num] * 86400
  }

  as.POSIXct(parsed)
}

ZhenM_fill_down <- function(dt, cols, by = NULL) {
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)      
  cols <- intersect(cols, names(dt))
  if (length(cols) == 0) return(dt)

  for (col in cols) {
    if (is.null(by)) {
      dt[, (col) := zoo::na.locf(get(col), na.rm = FALSE)]
    } else {
      dt[, (col) := zoo::na.locf(get(col), na.rm = FALSE), by = by]
    }
  }

  dt
}
