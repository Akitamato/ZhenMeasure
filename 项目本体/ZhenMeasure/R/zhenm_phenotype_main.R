ZhenM_apply_bio_constraints <- function(final_pheno,
                                      data_type = NULL,
                                      yx_fcr_min = 2.0,
                                      yx_fcr_max = 3.2) {
  pheno_cols <- setdiff(names(final_pheno), "animal_id")
  adg_cols <- pheno_cols[grepl("ADG$", pheno_cols)]
  for (adg_col in adg_cols) {
    bad_idx <- which(!is.na(final_pheno[[adg_col]]) & final_pheno[[adg_col]] < 0)
    if (length(bad_idx) > 0) final_pheno[bad_idx, (adg_col) := NA_real_]
  }

  fcr_cols <- pheno_cols[grepl("FCR$", pheno_cols)]
  for (fcr_col in fcr_cols) {
    bad_idx <- which(!is.na(final_pheno[[fcr_col]]) & final_pheno[[fcr_col]] < 1)
    if (length(bad_idx) > 0) {
      adg_col <- sub("FCR$", "ADG", fcr_col)
      adfi_col <- sub("FCR$", "ADFI", fcr_col)
      final_pheno[bad_idx, (fcr_col) := NA_real_]
      if (adg_col %in% names(final_pheno)) final_pheno[bad_idx, (adg_col) := NA_real_]
      if (adfi_col %in% names(final_pheno)) final_pheno[bad_idx, (adfi_col) := NA_real_]
    }

    if (!is.null(data_type) && identical(toupper(data_type), "YANGXIANG")) {
      bad_yx_idx <- which(!is.na(final_pheno[[fcr_col]]) & (final_pheno[[fcr_col]] < yx_fcr_min | final_pheno[[fcr_col]] > yx_fcr_max))
      if (length(bad_yx_idx) > 0) {
        adg_col <- sub("FCR$", "ADG", fcr_col)
        adfi_col <- sub("FCR$", "ADFI", fcr_col)
        final_pheno[bad_yx_idx, (fcr_col) := NA_real_]
        if (adg_col %in% names(final_pheno)) final_pheno[bad_yx_idx, (adg_col) := NA_real_]
        if (adfi_col %in% names(final_pheno)) final_pheno[bad_yx_idx, (adfi_col) := NA_real_]
      }
    }
  }

  final_pheno
}

ZhenM_resolve_stage_ranges <- function(stage_mode, target_weight_stages, target_age_stages, daily_records) {
  if (identical(stage_mode, "weight")) {
    if (isFALSE(target_weight_stages)) {
      # issue #19：全 NA 时 min/max(na.rm=TRUE) 得 ±Inf，生成 "Inf--Inf kg" 非法阶段
      if (all(is.na(daily_records$median_weight_g))) return(list())
      min_wt <- min(daily_records$median_weight_g, na.rm = TRUE)
      max_wt <- max(daily_records$median_weight_g, na.rm = TRUE)
      return(list(stats::setNames(list(c(min_wt, max_wt)), paste0(round(min_wt / 1000), "-", round(max_wt / 1000), "kg"))))
    }
    if (identical(target_weight_stages, "YANGXIANG")) {
      return(list("30-100kg" = c(30000, 100000), "30-115kg" = c(30000, 115000), "30-120kg" = c(30000, 120000)))
    }
    if (is.list(target_weight_stages)) return(target_weight_stages)
    stop("When stage_mode=weight, target_weight_stages must be FALSE, 'YANGXIANG', or a list.", call. = FALSE)
  }

  if (is.null(target_age_stages)) {
    stop("target_age_stages must be provided when stage_mode=age.", call. = FALSE)
  }
  if (is.list(target_age_stages)) return(target_age_stages)
  stop("target_age_stages must be a list, for example list('70-120d'=c(70,120)).", call. = FALSE)
}

#' Calculate stage-level phenotypes (stage-based interface)
#'
#' @param daily_records Daily-level data.
#' @param stage_mode Stage mode. Supported values are weight and age.
#' @param target_weight_stages Weight stage definition.
#' @param target_age_stages Age stage definition.
#' @param target_phenotype Target phenotype set.
#' @param data_type Device type for extra biological constraints.
#' @return A phenotype table keyed by animal_id.
#' @export
ZhenM_calc_phenotypes_stage <- function(
    daily_records,
    stage_mode = c("weight", "age"),
    target_weight_stages = "YANGXIANG",
    target_age_stages = NULL,
    target_phenotype = "ALL",
    data_type = NULL) {
  dt <- data.table::as.data.table(data.table::copy(daily_records))
  stage_mode <- match.arg(stage_mode)
  if (is.character(target_phenotype) && length(target_phenotype) == 1 && target_phenotype == "ALL") {
    target_phenotype <- c("ALL")
  }

  required_cols <- c("animal_id", "record_date", "daily_feed_g", "median_weight_g")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) stop(paste0("Missing required fields for phenotype calculation: ", paste(missing_cols, collapse = ", ")), call. = FALSE)

  if (!"age_day" %in% names(dt)) dt[, age_day := NA_real_]
  if (!"measurement_day" %in% names(dt)) dt[, measurement_day := NA_real_]

  # 时间轴：优先 age_day，其次 measurement_day；两者全缺时退化为「日历天数」
  # （issue #19：原 record_index 口径在日期有缺口时把 ADG 分母算成记录条数，
  #   导致日增重被高估——如 4 条记录跨 5 天时 ADG 被算成 gain/3 而非 gain/5）
  time_axis <- if (any(!is.na(dt$age_day))) "age_day" else if (any(!is.na(dt$measurement_day))) "measurement_day" else "calendar_day"

  stages <- ZhenM_resolve_stage_ranges(stage_mode, target_weight_stages, target_age_stages, dt)
  phenotype_list <- list()

  for (stage_name in names(stages)) {
    range_vals <- stages[[stage_name]]
    if (identical(stage_mode, "weight")) {
      stage_data <- dt[median_weight_g >= range_vals[1] & median_weight_g < range_vals[2]]
      min_days_required <- ceiling((range_vals[2] - range_vals[1]) / 10000)
    } else {
      stage_data <- dt[!is.na(age_day) & age_day >= range_vals[1] & age_day <= range_vals[2]]
      min_days_required <- max(7, ceiling((range_vals[2] - range_vals[1] + 1) * 0.2))
    }

    id_stage_days <- stage_data[, .(n_days_in_stage = data.table::uniqueN(record_date)), by = animal_id]
    valid_ids <- id_stage_days[n_days_in_stage >= min_days_required, animal_id]
    stage_data <- stage_data[animal_id %in% valid_ids]
    if (nrow(stage_data) == 0) next

    res <- stage_data[order(record_date), {
      out <- list()
      daily_recs <- unique(.SD[, .(record_date, daily_feed_g, median_weight_g, age_day, measurement_day)])
      # issue #11：阶段内采食全 NA 时 mean(na.rm=TRUE) 得 NaN，守卫置 NA
      val_adfi <- if (all(is.na(daily_recs$daily_feed_g))) NA_real_
        else mean(daily_recs$daily_feed_g, na.rm = TRUE)
      start_wt <- daily_recs$median_weight_g[1]
      end_wt <- daily_recs$median_weight_g[.N]
      axis_values <- switch(time_axis,
        age_day = daily_recs$age_day,
        measurement_day = daily_recs$measurement_day,
        calendar_day = as.numeric(daily_recs$record_date - daily_recs$record_date[1])
      )
      axis_start <- axis_values[1]
      axis_end <- axis_values[length(axis_values)]
      axis_diff <- axis_end - axis_start
      wt_diff <- end_wt - start_wt
      val_adg <- if (!is.na(axis_diff) && axis_diff > 0) wt_diff / axis_diff else NA_real_
      if (!is.na(val_adg) && val_adg < 0) val_adg <- NA_real_
      val_tfi <- sum(daily_recs$daily_feed_g, na.rm = TRUE)
      val_fcr <- NA_real_
      if (!is.na(val_adg) && val_adg > 0 && !is.na(val_adfi)) {
        wt_gain_kg <- (end_wt - start_wt) / 1000
        tfi_kg <- val_tfi / 1000
        if (!is.na(wt_gain_kg) && wt_gain_kg > 0) val_fcr <- tfi_kg / wt_gain_kg
      }

      if ("ALL" %in% target_phenotype || "ADFI" %in% target_phenotype) out$ADFI <- val_adfi
      if ("ALL" %in% target_phenotype || "ADG" %in% target_phenotype) out$ADG <- val_adg
      if ("ALL" %in% target_phenotype || "FCR" %in% target_phenotype) out$FCR <- val_fcr
      out
    }, by = animal_id]

    if (nrow(res) > 0) {
      data.table::setnames(res, old = names(res)[-1], new = paste0(stage_name, "_", names(res)[-1]))
      phenotype_list[[stage_name]] <- res
    }
  }

  if (length(phenotype_list) > 0) {
    final_pheno <- Reduce(function(x, y) merge(x, y, by = "animal_id", all = TRUE), phenotype_list)
  } else {
    final_pheno <- data.table::data.table(animal_id = unique(dt$animal_id))
  }

  if (("ALL" %in% target_phenotype || "AGE" %in% target_phenotype) && identical(stage_mode, "weight") && any(!is.na(dt$age_day))) {
    target_weights <- if (identical(target_weight_stages, "YANGXIANG")) {
      c(100000, 115000, 120000)
    } else if (is.list(target_weight_stages)) {
      unlist(lapply(target_weight_stages, function(x) x[2]))
    } else {
      numeric(0)
    }

    if (length(target_weights) > 0) {
      daily_wt <- dt[!is.na(median_weight_g), .(med_wt = stats::median(median_weight_g, na.rm = TRUE), age = stats::median(age_day, na.rm = TRUE)), by = .(animal_id, record_date)]
      data.table::setorder(daily_wt, animal_id, record_date)
      age_res <- daily_wt[, {
        out <- list()
        for (target_weight in target_weights) {
          hit_index <- which(med_wt >= target_weight)[1]
          out[[paste0(round(target_weight / 1000), "kg_AGE")]] <- if (!is.na(hit_index)) age[hit_index] else NA_real_
        }
        out
      }, by = animal_id]
      final_pheno <- merge(final_pheno, age_res, by = "animal_id", all = TRUE)
    }
  }

  ZhenM_apply_bio_constraints(data.table::as.data.table(final_pheno), data_type = data_type)
}
