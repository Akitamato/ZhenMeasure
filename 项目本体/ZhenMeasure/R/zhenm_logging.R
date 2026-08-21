#' ZhenMeasure logging utilities
#'
#' Enhanced logging system for ZhenMeasure operations with detailed QC statistics.
#'
#' @keywords internal

#' Create a logger object for detailed QC logging
#'
#' @param log_file Path to log file. If NULL, only console output.
#' @param append Whether to append to existing file. Default FALSE.
#' @return Logger object with write/info/detail/warn/close methods
#' @export
ZhenM_create_logger <- function(log_file = NULL, append = FALSE) {
  log_con <- NULL
  log_enabled <- TRUE


  if (!is.null(log_file) && nzchar(trimws(log_file))) {
    log_path <- normalizePath(log_file, winslash = "/", mustWork = FALSE)
    log_dir <- dirname(log_path)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }
    log_con <- tryCatch(
      file(log_path, open = if (append) "a" else "w", encoding = "UTF-8"),
      error = function(e) {
        warning(paste0("无法创建日志文件: ", log_path, ". 仅输出到控制台."), call. = FALSE)
        NULL
      }
    )
  }

  write_line <- function(level, msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_msg <- paste0("[", timestamp, "] [", level, "] ", msg)
    cat(log_msg, "\n")
    if (!is.null(log_con) && isTRUE(log_enabled)) {
      tryCatch({
        cat(log_msg, "\n", file = log_con)
        flush(log_con)
      }, error = function(e) {
        log_enabled <<- FALSE
      })
    }
  }

  list(
    connection = log_con,
    # 通用写入
    write = function(msg) write_line("INFO", msg),
    # 信息级别 - 主要步骤
    info = function(msg) write_line("INFO", msg),
    # 详细级别 - QC统计数据
    detail = function(msg) write_line("DETAIL", msg),
    # 警告级别
    warn = function(msg) write_line("WARN", msg),
    # 分隔线
    section = function(title) {
      write_line("INFO", paste0("========== ", title, " =========="))
    },
    # 子节标题
    subsection = function(title) {
      write_line("INFO", paste0("--- ", title, " ---"))
    },
    # 关闭日志
    close = function() {
      if (!is.null(log_con)) {
        tryCatch(close(log_con), error = function(e) NULL)
      }
    }
  )
}

#' Initialize log file (legacy compatibility)
#' @param log_file Path to log file
#' @return Log connection object
#' @export
ZhenM_init_log <- function(log_file = "ZhenMeasure_log.txt") {
  ZhenM_create_logger(log_file, append = FALSE)
}

#' Write message to log
#' @param logger Logger object from ZhenM_init_log or ZhenM_create_logger
#' @param msg Message to log
#' @export
ZhenM_log <- function(logger, msg) {
  if (is.list(logger) && "write" %in% names(logger)) {
    logger$write(msg)
  } else {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(paste0("[", timestamp, "] ", msg, "\n"))
  }
}

#' Close log file
#' @param logger Logger object from ZhenM_init_log or ZhenM_create_logger
#' @export
ZhenM_close_log <- function(logger) {
  if (is.list(logger) && "close" %in% names(logger)) {
    logger$close()
  }
}

#' Log QC statistics in standardized format
#'
#' @param logger Logger object
#' @param item_name Name of the QC item (e.g., "去除NA记录数")
#' @param count Number of items affected
#' @param total Optional total for percentage calculation
#' @param ids Optional vector of affected IDs to list (max 10 shown)
#' @export
ZhenM_log_qc_stat <- function(logger, item_name, count, total = NULL, ids = NULL) {
  if (is.null(logger) || !is.list(logger)) return(invisible(NULL))

  msg <- paste0(item_name, ": ", count)
  if (!is.null(total) && total > 0) {
    pct <- round(count / total * 100, 2)
    msg <- paste0(msg, " (", pct, "%)")
  }
  if (!is.null(ids) && length(ids) > 0) {
    if (length(ids) <= 10) {
      msg <- paste0(msg, " [", paste(ids, collapse = ", "), "]")
    } else {
      msg <- paste0(msg, " [", paste(head(ids, 10), collapse = ", "), ", ...]")
    }
  }

  if ("detail" %in% names(logger)) {
    logger$detail(msg)
  } else {
    logger$write(msg)
  }
}

#' Log threshold information
#'
#' @param logger Logger object
#' @param threshold_name Name of the threshold

#' @param value Threshold value
#' @param unit Optional unit string
#' @export
ZhenM_log_threshold <- function(logger, threshold_name, value, unit = "") {

  if (is.null(logger) || !is.list(logger)) return(invisible(NULL))

  msg <- paste0(threshold_name, " = ", round(value, 2))
  if (nzchar(unit)) {
    msg <- paste0(msg, " ", unit)
  }

  if ("detail" %in% names(logger)) {
    logger$detail(msg)
  } else {
    logger$write(msg)
  }
}
