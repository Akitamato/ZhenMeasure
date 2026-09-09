#' Write ZhenMeasure run outputs to files
#'
#' Writes phenotypes, QC summary, and optional detailed results to CSV files.
#' Output file naming follows legacy convention: corrected_records.csv for QC'd data.
#'
#' @param result Result list from ZhenM_run containing phenotypes, qc_summary, etc.
#' @param output_dir Output directory path
#' @param logger Optional logger object for detailed logging
#' @return A character vector of created file paths
#' @export
ZhenM_write_run_outputs <- function(result, output_dir, logger = NULL) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  log_info <- function(msg) {
    if (!is.null(logger) && is.list(logger) && "info" %in% names(logger)) logger$info(msg)
  }
  log_detail <- function(msg) {
    if (!is.null(logger) && is.list(logger) && "detail" %in% names(logger)) logger$detail(msg)
  }

  output_files <- character(0)

  # Write phenotypes
  if ("phenotypes" %in% names(result) && !is.null(result$phenotypes)) {
    pheno_file <- file.path(output_dir, "phenotypes.csv")
    data.table::fwrite(result$phenotypes, pheno_file, bom = TRUE)
    output_files <- c(output_files, pheno_file)
    message(paste0("  Written: phenotypes.csv (", nrow(result$phenotypes), " animals)"))
    log_detail(paste0("输出文件: phenotypes.csv (", nrow(result$phenotypes), " 个体)"))
  }

  # Write QC summary
  if ("qc_summary" %in% names(result) && !is.null(result$qc_summary)) {
    qc_file <- file.path(output_dir, "qc_summary.csv")
    data.table::fwrite(result$qc_summary, qc_file, bom = TRUE)
    output_files <- c(output_files, qc_file)
    message(paste0("  Written: qc_summary.csv"))
    log_detail("输出文件: qc_summary.csv")
  }

  # Write corrected_records (主要输出，旧版本命名)
  # 优先使用 corrected_records，如果没有则使用 daily_records
  corrected_data <- NULL
  if ("corrected_records" %in% names(result) && !is.null(result$corrected_records)) {
    corrected_data <- result$corrected_records
  } else if ("daily_records" %in% names(result) && !is.null(result$daily_records)) {
    corrected_data <- result$daily_records
  }

  if (!is.null(corrected_data)) {
    corrected_file <- file.path(output_dir, "corrected_records.csv")
    data.table::fwrite(corrected_data, corrected_file, bom = TRUE)
    output_files <- c(output_files, corrected_file)
    message(paste0("  Written: corrected_records.csv (", nrow(corrected_data), " records)"))
    log_detail(paste0("输出文件: corrected_records.csv (", nrow(corrected_data), " 条记录)"))
  }

  # Write daily records (保留兼容性，但内容与corrected_records相同)
  if ("daily_records" %in% names(result) && !is.null(result$daily_records)) {
    daily_file <- file.path(output_dir, "daily_records.csv")
    data.table::fwrite(result$daily_records, daily_file, bom = TRUE)
    output_files <- c(output_files, daily_file)
    message(paste0("  Written: daily_records.csv (", nrow(result$daily_records), " records)"))
    log_detail(paste0("输出文件: daily_records.csv (", nrow(result$daily_records), " 条记录)"))
  }

  # Write QC flags (optional)
  if ("qc_flags" %in% names(result) && !is.null(result$qc_flags)) {
    flags_file <- file.path(output_dir, "qc_flags.csv")
    data.table::fwrite(result$qc_flags, flags_file, bom = TRUE)
    output_files <- c(output_files, flags_file)
    message(paste0("  Written: qc_flags.csv"))
    log_detail("输出文件: qc_flags.csv")
  }

  # Write overall QC summary
  if ("qc_overall_summary" %in% names(result) && !is.null(result$qc_overall_summary)) {
    overall_file <- file.path(output_dir, "qc_overall_summary.csv")
    data.table::fwrite(result$qc_overall_summary, overall_file, bom = TRUE)
    output_files <- c(output_files, overall_file)
    message(paste0("  Written: qc_overall_summary.csv"))
    log_detail("输出文件: qc_overall_summary.csv")
  }

  log_info(paste0("共输出 ", length(output_files), " 个文件"))

  invisible(output_files)
}

#' Write plot outputs (growth curves) — 2x2 panel layout
#'
#' Generates and saves growth curve plots as PDF files.
#' Uses 2x2 panel layout showing original vs cleaned data:
#' Row 1: Original (raw daily) data with outlier/imputed coloring
#' Row 2: Cleaned (processed) daily data with imputed coloring
#'
#' @param result Result list from run_zhen_measure (must contain daily_records and raw_daily)
#' @param output_dir Output directory path
#' @param growth_curve If TRUE, create individual PDFs for each animal
#' @param growth_curve_test If TRUE, create a combined PDF with all animals
#' @return A character vector of created file paths
#' @export
ZhenM_write_plot_outputs <- function(result, output_dir, growth_curve = FALSE,
                                    growth_curve_test = FALSE) {
  if (!growth_curve && !growth_curve_test) {
    return(NULL)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_files <- character(0)

  # Get raw_daily and daily_data from result
  raw_daily <- result$raw_daily_records
  if (is.null(raw_daily)) raw_daily <- result$raw_daily

  daily_data <- NULL
  if ("qc_daily_records" %in% names(result)) {
    daily_data <- result$qc_daily_records
  } else if ("daily_records" %in% names(result)) {
    daily_data <- result$daily_records
  }

  if (is.null(raw_daily) || is.null(daily_data)) {
    warning("Missing raw_daily or daily_records for plotting")
    return(NULL)
  }

  raw_daily <- data.table::as.data.table(raw_daily)
  daily_data <- data.table::as.data.table(daily_data)

  # Ensure required columns exist
  required_cols <- c("animal_id", "record_date", "daily_feed_g", "daily_weight_g")
  if (!all(required_cols %in% names(raw_daily)) || nrow(raw_daily) == 0 ||
      !all(required_cols %in% names(daily_data)) || nrow(daily_data) == 0) {
    return(NULL)
  }

  # Ensure flag columns exist with defaults
  raw_flag_cols <- c("day_has_outlier_feed", "day_has_outlier_wt",
                      "day_is_imputed_feed", "day_is_imputed_wt")
  for (col in raw_flag_cols) {
    if (!col %in% names(raw_daily)) raw_daily[, (col) := FALSE]
  }

  daily_imp_cols <- c("is_imputed_feed", "is_imputed_wt")
  for (col in daily_imp_cols) {
    if (!col %in% names(daily_data)) daily_data[, (col) := FALSE]
  }

  # Determine common animal IDs
  animal_ids <- intersect(
    unique(daily_data$animal_id),
    unique(raw_daily$animal_id)
  )
  animal_ids <- animal_ids[!is.na(animal_ids) & trimws(animal_ids) != ""]

  if (length(animal_ids) == 0) {
    return(NULL)
  }

  # Combined PDF (growth_curve_test)
  if (growth_curve_test) {
    combined_file <- file.path(output_dir, "growth_curves.pdf")
    # issue #23：绘图设备由 .with_pdf_device() 打开并在退出时保证关闭，
    # 中途报错不再残留打开的 PDF 设备
    .with_pdf_device(combined_file, {
      for (id in animal_ids) {
        .plot_animal_2x2(
          raw_sub = raw_daily[animal_id == id],
          daily_sub = daily_data[animal_id == id],
          animal_id = id
        )
      }
    })
    output_files <- c(output_files, combined_file)
    message(paste0("  Written: growth_curves.pdf (", length(animal_ids), " animals)"))
  }

  # Individual PDFs (growth_curve)
  if (growth_curve) {
    curves_dir <- file.path(output_dir, "growth_curves")
    if (!dir.exists(curves_dir)) {
      dir.create(curves_dir, recursive = TRUE)
    }

    message(paste0("Generating ", length(animal_ids), " individual growth curve PDFs..."))

    for (id in animal_ids) {
      safe_id <- gsub("[/\\\\:*?\"<>|]", "_", as.character(id))
      pdf_file <- file.path(curves_dir, paste0(safe_id, ".pdf"))
      .with_pdf_device(pdf_file, .plot_animal_2x2(
        raw_sub = raw_daily[animal_id == id],
        daily_sub = daily_data[animal_id == id],
        animal_id = id
      ))
      output_files <- c(output_files, pdf_file)
    }

    message(paste0("  Written: ", length(animal_ids), " PDFs in growth_curves/"))
  }

  invisible(output_files)
}

#' 在 PDF 设备上执行绘图表达式并保证设备关闭（issue #23）
#'
#' `grDevices::pdf()` 与 `dev.off()` 分离书写时，中间报错会残留打开的绘图设备、
#' 产出损坏的 PDF 文件。此辅助函数用 `on.exit()` 保证设备无论正常结束还是报错
#' 都会被关闭。
#'
#' @param file PDF 输出路径
#' @param expr 绘图表达式（惰性求值，在调用方环境中执行）
#' @param width,height 画布尺寸（英寸）
#' @return 不可见地返回文件路径
#' @keywords internal
.with_pdf_device <- function(file, expr, width = 15, height = 10) {
  grDevices::pdf(file, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
  invisible(file)
}

#' Plot single animal with 2x2 panel layout
#'
#' Row 1: Raw daily data (original before QC)
#'   Left: record_date vs daily_feed_g (#f5616f=QC Removed, #f7b13f=Partial Anomaly, black=Normal)
#'   Right: record_date vs daily_weight_g (#f5616f=QC Removed, #f7b13f=Partial Anomaly, black=Normal)
#' Row 2: Cleaned daily data (after QC and imputation)
#'   Left: record_date vs daily_feed_g (green triangle=Imputed, #3685fe circle=Corrected, black circle=Normal)
#'   Right: record_date vs daily_weight_g (green triangle=Imputed, #3685fe circle=Corrected, black circle=Normal)
#'
#' @keywords internal
.plot_animal_2x2 <- function(raw_sub, daily_sub, animal_id) {
  if (is.null(raw_sub) || nrow(raw_sub) == 0 ||
      is.null(daily_sub) || nrow(daily_sub) == 0) {
    return(invisible(NULL))
  }

  # Disable scientific notation for y-axis weights (e.g., 200000 not 2e+05)
  old_scipen <- options(scipen = 9999)
  on.exit(options(old_scipen), add = TRUE)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(2, 2), oma = c(5, 1, 2, 1))

  # === Row 1: Raw Daily Data (Original before QC) ===

  # Panel 1: Raw Feed Intake
  .plot_panel_tricolor(
    x = raw_sub$record_date,
    y = raw_sub$daily_feed_g,
    flag_imputed = raw_sub$day_is_imputed_feed,
    flag_outlier = raw_sub$day_has_outlier_feed,
    main = "Original: Feed Intake",
    xlab = "Record Date",
    ylab = "Feed Intake (g)"
  )

  # Panel 2: Raw Body Weight
  .plot_panel_tricolor(
    x = raw_sub$record_date,
    y = raw_sub$daily_weight_g,
    flag_imputed = raw_sub$day_is_imputed_wt,
    flag_outlier = raw_sub$day_has_outlier_wt,
    main = "Original: Body Weight",
    xlab = "Record Date",
    ylab = "Weight (g)"
  )

  # === Row 2: Cleaned Daily Data (After QC and Imputation) ===

  # Merge raw outlier flags into daily_sub for "Corrected" marking
  daily_sub <- data.table::as.data.table(daily_sub)
  raw_has_outlier <- c("day_has_outlier_feed", "day_has_outlier_wt") %in% names(raw_sub)
  if (all(raw_has_outlier)) {
    raw_info <- unique(raw_sub[, .(record_date, day_has_outlier_feed, day_has_outlier_wt)])
    daily_sub <- merge(daily_sub, raw_info, by = "record_date", all.x = TRUE)
    daily_sub[is.na(day_has_outlier_feed), day_has_outlier_feed := FALSE]
    daily_sub[is.na(day_has_outlier_wt), day_has_outlier_wt := FALSE]
  } else {
    daily_sub[, `:=`(day_has_outlier_feed = FALSE, day_has_outlier_wt = FALSE)]
  }

  # Panel 3: Cleaned Feed Intake
  .plot_panel_cleaned(
    x = daily_sub$record_date,
    y = daily_sub$daily_feed_g,
    flag_imputed = daily_sub$is_imputed_feed,
    flag_corrected = daily_sub$day_has_outlier_feed,
    main = "Cleaned: Feed Intake",
    xlab = "Record Date",
    ylab = "Feed Intake (g)"
  )

  # Panel 4: Cleaned Body Weight
  .plot_panel_cleaned(
    x = daily_sub$record_date,
    y = daily_sub$daily_weight_g,
    flag_imputed = daily_sub$is_imputed_wt,
    flag_corrected = daily_sub$day_has_outlier_wt,
    main = "Cleaned: Body Weight",
    xlab = "Record Date",
    ylab = "Weight (g)"
  )

  # Overall title
  graphics::mtext(paste("ID:", animal_id), outer = TRUE, cex = 1.5)

  invisible(NULL)
}

#' Plot panel with three-color classification (original data)
#'
#' Color scheme: #f5616f = QC Removed, #f7b13f = Partial Anomaly, black = Normal
#'
#' @keywords internal
.plot_panel_tricolor <- function(x, y, flag_imputed, flag_outlier,
                                  main, xlab, ylab) {
  valid <- !is.na(x) & !is.na(y)

  if (!any(valid)) {
    graphics::plot(1, 1, type = "n", main = main, xlab = xlab, ylab = ylab, xaxt = "n")
    graphics::text(1, 1, "No Valid Data")
    return(invisible(NULL))
  }

  # Color priority: imputed > outlier > normal
  col <- rep("black", length(y))
  col[!is.na(flag_imputed) & flag_imputed & valid] <- "#f5616f"
  col[!is.na(flag_outlier) & flag_outlier & !flag_imputed & valid] <- "#f7b13f"

  graphics::plot(x[valid], y[valid], col = col[valid], pch = 20,
                 main = main, xlab = xlab, ylab = ylab, xaxt = "n")
  .add_date_axis(x[valid])

  graphics::legend("topleft",
                   legend = c("Normal", "Partial Anomaly", "QC Removed"),
                   col = c("black", "#f7b13f", "#f5616f"),
                   pch = c(20, 20, 20), bty = "n", cex = 0.8)

  invisible(NULL)
}

#' Plot panel with three-color classification (cleaned data)
#'
#' Color scheme:
#'   #3685fe circle  = day had anomaly but was corrected
#'   green triangle = imputed (no valid data, algorithm-filled)
#'   black circle = normal (no anomaly, not imputed)
#'
#' @keywords internal
.plot_panel_cleaned <- function(x, y, flag_imputed, flag_corrected = NULL,
                                 main, xlab, ylab) {
  valid <- !is.na(x) & !is.na(y)

  if (!any(valid)) {
    graphics::plot(1, 1, type = "n", main = main, xlab = xlab, ylab = ylab, xaxt = "n")
    graphics::text(1, 1, "No Valid Data")
    return(invisible(NULL))
  }

  if (is.null(flag_corrected)) flag_corrected <- rep(FALSE, length(y))

  # Color priority: imputed > corrected > normal
  col <- rep("black", length(y))
  col[!is.na(flag_corrected) & flag_corrected & !flag_imputed & valid] <- "#3685fe"
  col[!is.na(flag_imputed) & flag_imputed & valid] <- "#50c48f"

  pch <- rep(20, length(y))
  pch[!is.na(flag_imputed) & flag_imputed & valid] <- 17

  graphics::plot(x[valid], y[valid], col = col[valid], pch = pch[valid],
                 main = main, xlab = xlab, ylab = ylab, xaxt = "n")
  .add_date_axis(x[valid])

  graphics::legend("topleft",
                   legend = c("Normal", "Corrected", "Imputed"),
                   col = c("black", "#3685fe", "#50c48f"),
                   pch = c(20, 20, 17), bty = "n", cex = 0.8)

  invisible(NULL)
}

#' Add a date axis with ~5 evenly spaced labels
#'
#' Suppresses default x-axis and adds date labels (including first and last date).
#'
#' @param x IDate or Date vector used as x-axis values
#' @keywords internal
.add_date_axis <- function(x) {
  x_dates <- as.Date(x)
  x_dates <- x_dates[!is.na(x_dates)]
  if (length(x_dates) == 0) return(invisible(NULL))

  unique_dates <- sort(unique(x_dates))
  n_unique <- length(unique_dates)

  if (n_unique <= 5) {
    tick_dates <- unique_dates
  } else {
    indices <- round(seq(1, n_unique, length.out = 5))
    tick_dates <- unique_dates[indices]
    tick_dates[1] <- unique_dates[1]
    tick_dates[length(tick_dates)] <- unique_dates[n_unique]
    tick_dates <- unique(tick_dates)
  }

  # Draw tick marks only
  graphics::axis(1, at = as.numeric(tick_dates), labels = FALSE)

  # Add 45-degree angled labels, offset below the plot area
  graphics::text(x = as.numeric(tick_dates),
                 y = graphics::par("usr")[3] - graphics::strheight("0", cex = 0.7) * 1.5,
                 labels = format(tick_dates, "%Y-%m-%d"),
                 srt = 45, adj = 1, xpd = TRUE, cex = 0.7)
}

#' Generate QC summary statistics
#'
