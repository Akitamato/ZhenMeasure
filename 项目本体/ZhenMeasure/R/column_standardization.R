ZhenM_canonicalize_label <- function(label) {
  if (is.null(label) || length(label) == 0) return(NA_character_)
  value <- trimws(tolower(as.character(label)[1]))
  if (is.na(value) || value == "") return(NA_character_)
  value <- gsub("[[:space:]-]+", "_", value)
  value <- gsub("[()]", "", value)

  alias_to_canonical <- c(
    animal_id = "animal_id", id = "animal_id", pig_id = "animal_id", eartag = "animal_id",
    record_date = "record_date", date = "record_date", feed_date = "record_date",
    feed_g = "feed_g", feed = "feed_g", intake_g = "feed_g", feed_intake = "feed_g",
    daily_feed_g = "daily_feed_g", daily_feed = "daily_feed_g", daily_intake = "daily_feed_g",
    weight_g = "weight_g", weight = "weight_g", body_weight_g = "weight_g",
    duration_sec = "duration_sec", duration = "duration_sec", feeding_duration_sec = "duration_sec",
    start_time = "start_time", visit_time = "start_time", feeding_start_time = "start_time",
    end_time = "end_time", feeding_end_time = "end_time",
    age_day = "age_day", age = "age_day", day_age = "age_day",
    measurement_day = "measurement_day", test_day = "measurement_day",
    source_file = "source_file", file = "source_file"
  )

  if (value %in% names(alias_to_canonical)) return(unname(alias_to_canonical[[value]]))
  if (value %in% unname(alias_to_canonical)) return(value)
  NA_character_
}

ZhenM_apply_data_format_labels <- function(
    dt,
    id_pos = integer(0), id_labels = character(0),
    character_pos = integer(0), character_labels = character(0),
    numeric_pos = integer(0), numeric_labels = character(0),
    date_pos = integer(0), date_labels = character(0)) {
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)

  n_col <- ncol(dt)
  apply_one_group <- function(pos_vec, label_vec) {
    if (length(pos_vec) == 0 || length(label_vec) == 0) return(invisible(NULL))
    group_n <- min(length(pos_vec), length(label_vec))
    for (index in seq_len(group_n)) {
      pos <- pos_vec[index]
      if (is.na(pos) || pos < 1 || pos > n_col) next
      target <- ZhenM_canonicalize_label(label_vec[index])
      if (is.na(target) || target == "") next
      old_name <- names(dt)[pos]
      if (!identical(old_name, target) && !(target %in% names(dt))) {
        data.table::setnames(dt, old_name, target)
      }
    }
  }

  apply_one_group(id_pos, id_labels)
  apply_one_group(character_pos, character_labels)
  apply_one_group(numeric_pos, numeric_labels)
  apply_one_group(date_pos, date_labels)

  dt
}

ZhenM_add_english_aliases <- function(dt) {
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)

  cn_to_en <- c(
    "\u7535\u5b50\u8033\u6807" = "animal_id",
    "\u91c7\u98df\u65e5\u671f" = "record_date",
    "\u91c7\u98df\u91cf(g)" = "feed_g",
    "\u65e5\u91c7\u98df\u91cf(g)" = "daily_feed_g",
    "\u8fdb\u5206\u680f\u5668\u4f53\u91cd(g)" = "weight_g",
    "\u91c7\u98df\u65f6\u957f(\u79d2)" = "duration_sec",
    "\u91c7\u98df\u5f00\u59cb\u65f6\u95f4" = "start_time",
    "\u91c7\u98df\u7ed3\u675f\u65f6\u95f4" = "end_time",
    "\u65e5\u9f84" = "age_day",
    "\u6d4b\u5b9a\u5929\u6570" = "measurement_day",
    "Source_File" = "source_file"
  )

  for (cn in names(cn_to_en)) {
    en <- unname(cn_to_en[[cn]])
    if (cn %in% names(dt) && !(en %in% names(dt))) {
      dt[, (en) := get(cn)]
    }
  }

  dt
}
