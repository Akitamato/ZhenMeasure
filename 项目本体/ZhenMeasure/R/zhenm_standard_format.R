#' Standard data format specification for ZhenMeasure
#'
#' Defines the unified standard format that all data sources (YANGXIANG, NEDAP, FIRE)
#' must be converted to before processing.
#'
#' @return A list containing column specifications
#' @export
#' @examples
#' \dontrun{
#' spec <- ZhenM_standard_columns()
#' print(spec$required)
#' }
ZhenM_standard_columns <- function() {
  list(
    required = c("ID", "Visit_time", "Feed_intake", "Weight"),
    optional = c("AGE", "DFI", "End_time", "Duration", "Location"),
    all = c("ID", "AGE", "DFI", "Visit_time", "End_time", "Duration",
            "Feed_intake", "Weight", "Location"),
    types = list(
      ID = "character",
      AGE = "numeric",
      DFI = "numeric",
      Visit_time = "POSIXct",
      End_time = "POSIXct",
      Duration = "numeric",
      Feed_intake = "numeric",
      Weight = "numeric",
      Location = "character"
    ),
    descriptions = list(
      ID = "Individual ID (15-digit)",
      AGE = "Age in days",
      DFI = "Daily feed intake (g)",
      Visit_time = "Feeding start time (YYYY-MM-DD HH:MM:SS)",
      End_time = "Feeding end time (YYYY-MM-DD HH:MM:SS)",
      Duration = "Feeding duration (seconds)",
      Feed_intake = "Single feeding intake (g)",
      Weight = "Body weight (g)",
      Location = "Feeding station/location"
    )
  )
}

#' Validate standard format data
#'
#' Checks if a data.table conforms to the ZhenMeasure standard format.
#'
#' @param data A data.table to validate
#' @param strict If TRUE, requires all optional columns. If FALSE, only checks required columns.
#' @return The validated data.table (returns input if valid, throws error otherwise)
#' @export
ZhenM_validate_standard_format <- function(data, strict = FALSE) {
  if (!data.table::is.data.table(data)) {
    stop("Input must be a data.table", call. = FALSE)
  }

  spec <- ZhenM_standard_columns()

  # Check required columns
  missing_required <- setdiff(spec$required, names(data))
  if (length(missing_required) > 0) {
    stop(paste0("Missing required columns: ", paste(missing_required, collapse = ", ")),
         call. = FALSE)
  }

  # Check optional columns if strict
  if (strict) {
    missing_optional <- setdiff(spec$optional, names(data))
    if (length(missing_optional) > 0) {
      warning(paste0("Missing optional columns: ", paste(missing_optional, collapse = ", ")))
    }
  }

  # Validate data types for existing columns
  for (col in names(data)) {
    if (col %in% names(spec$types)) {
      expected_type <- spec$types[[col]]
      actual_class <- class(data[[col]])[1]

      valid <- switch(expected_type,
        "character" = is.character(data[[col]]),
        "numeric" = is.numeric(data[[col]]),
        "POSIXct" = inherits(data[[col]], "POSIXct"),
        TRUE
      )

      if (!valid) {
        stop(paste0("Column '", col, "' should be ", expected_type,
                   " but is ", actual_class), call. = FALSE)
      }
    }
  }

  return(data)
}

#' Create empty standard format data.table
#'
#' @return An empty data.table with standard format columns
#' @export
ZhenM_create_standard_template <- function() {
  spec <- ZhenM_standard_columns()

  dt <- data.table::data.table(
    ID = character(0),
    AGE = numeric(0),
    DFI = numeric(0),
    Visit_time = as.POSIXct(character(0)),
    End_time = as.POSIXct(character(0)),
    Duration = numeric(0),
    Feed_intake = numeric(0),
    Weight = numeric(0),
    Location = character(0)
  )

  dt
}
