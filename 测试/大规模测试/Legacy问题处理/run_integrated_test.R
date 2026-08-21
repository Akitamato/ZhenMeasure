library(ZhenMeasure)
library(data.table)
library(jsonlite)

sources <- list(
  list(name="YANGXIANG", type="YANGXIANG",
       path="D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/demo/demo_standard/Farm_C_YANGXIANG",
       format="D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/demo/demo_standard/Farm_C_YANGXIANG/Data_format/YANGXIANG_data_format.json"),
  list(name="FIRE", type="FIRE",
       path="D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/demo/demo_standard/Farm_A_FIRE",
       format="D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/demo/demo_standard/Farm_A_FIRE/附加信息/FIRE_data_format.json"),
  list(name="Nedap", type="NEDAP",
       path="D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/demo/demo_standard/Farm_B_Nedap",
       format="D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/demo/demo_standard/Farm_B_Nedap/Data_format/NEDAP_data_format.json")
)

# Test 1: Default config (STL and Gompertz OFF) - should match baseline exactly
cat("=== TEST 1: Default config (STL=FALSE, Gompertz=FALSE) ===\n")
default_results <- list()
for (src in sources) {
  cat("Running default for:", src[["name"]], "\n")
  result <- tryCatch({
    run_zhen_measure(data_path=src[["path"]], data_type=src[["type"]], format_path=src[["format"]],
                     qc_method="national_standard")
  }, error=function(e) list(error=e$message))
  default_results[[src[["name"]]]] <- result
}

# Test 2: With STL enabled
cat("\n=== TEST 2: STL enabled (use_stl_feed=TRUE) ===\n")
stl_results <- list()
stl_config <- list(national_standard=list(use_stl_feed=TRUE))
for (src in sources) {
  cat("Running STL for:", src[["name"]], "\n")
  result <- tryCatch({
    run_zhen_measure(data_path=src[["path"]], data_type=src[["type"]], format_path=src[["format"]],
                     qc_method="national_standard", config=stl_config)
  }, error=function(e) list(error=e$message))
  stl_results[[src[["name"]]]] <- result
}

# Test 3: With Gompertz enabled
cat("\n=== TEST 3: Gompertz enabled (use_gompertz=TRUE) ===\n")
gomp_results <- list()
gomp_config <- list(national_standard=list(use_gompertz=TRUE))
for (src in sources) {
  cat("Running Gompertz for:", src[["name"]], "\n")
  result <- tryCatch({
    run_zhen_measure(data_path=src[["path"]], data_type=src[["type"]], format_path=src[["format"]],
                     qc_method="national_standard", config=gomp_config)
  }, error=function(e) list(error=e$message))
  gomp_results[[src[["name"]]]] <- result
}

# Test 4: Both enabled
cat("\n=== TEST 4: Both STL + Gompertz enabled ===\n")
both_results <- list()
both_config <- list(national_standard=list(use_stl_feed=TRUE, use_gompertz=TRUE))
for (src in sources) {
  cat("Running both for:", src[["name"]], "\n")
  result <- tryCatch({
    run_zhen_measure(data_path=src[["path"]], data_type=src[["type"]], format_path=src[["format"]],
                     qc_method="national_standard", config=both_config)
  }, error=function(e) list(error=e$message))
  both_results[[src[["name"]]]] <- result
}

# Collect metrics
collect_metrics <- function(result, name) {
  if (!is.null(result[["error"]])) {
    return(list(source=name, error=result[["error"]]))
  }
  pheno <- result[["phenotypes"]]
  daily <- result[["daily_records"]]

  n_animals <- nrow(pheno)
  n_daily_records <- nrow(daily)

  adfi <- mean(pheno[["ADFI_g"]], na.rm=TRUE)
  adg <- mean(pheno[["ADG_g"]], na.rm=TRUE)
  fcr <- mean(pheno[["FCR"]], na.rm=TRUE)
  fcr_sd <- sd(pheno[["FCR"]], na.rm=TRUE)
  fcr_na <- sum(is.na(pheno[["FCR"]]))

  list(
    source=name,
    n_animals=n_animals,
    n_daily_records=n_daily_records,
    ADFI_g=round(adfi, 2),
    ADG_g=round(adg, 2),
    FCR=round(fcr, 4),
    FCR_sd=round(fcr_sd, 4),
    FCR_NA=fcr_na
  )
}

# Build comparison
default_metrics <- list()
stl_metrics <- list()
gomp_metrics <- list()
both_metrics <- list()

src_names <- sapply(sources, function(s) s[["name"]])
for (src_name in src_names) {
  default_metrics[[src_name]] <- collect_metrics(default_results[[src_name]], src_name)
  stl_metrics[[src_name]] <- collect_metrics(stl_results[[src_name]], src_name)
  gomp_metrics[[src_name]] <- collect_metrics(gomp_results[[src_name]], src_name)
  both_metrics[[src_name]] <- collect_metrics(both_results[[src_name]], src_name)
}

# Load baseline
baseline_path <- "D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/大规模测试/Legacy问题处理/baseline_metrics.json"
baseline_metrics <- fromJSON(baseline_path)

# Build final comparison
comparison <- list(
  baseline=baseline_metrics,
  integrated_default=default_metrics,
  integrated_stl=stl_metrics,
  integrated_gompertz=gomp_metrics,
  integrated_both=both_metrics,
  diff=list()
)

# Calculate diffs for each source
for (nm in names(default_metrics)) {
  bl <- if (nm %in% names(baseline_metrics)) baseline_metrics[[nm]] else default_metrics[[nm]]
  df <- default_metrics[[nm]]
  comparison[["diff"]][[nm]] <- list(
    animal_diff=df[["n_animals"]] - bl[["n_animals"]],
    daily_diff=df[["n_daily_records"]] - bl[["n_daily_records"]],
    ADFI_diff=round(df[["ADFI_g"]] - bl[["ADFI_g"]], 2),
    ADG_diff=round(df[["ADG_g"]] - bl[["ADG_g"]], 2),
    FCR_diff=round(df[["FCR"]] - bl[["FCR"]], 4),
    FCR_sd_diff=round(df[["FCR_sd"]] - bl[["FCR_sd"]], 4),
    FCR_NA_diff=df[["FCR_NA"]] - bl[["FCR_NA"]]
  )
}

cat("\n=== COMPARISON SUMMARY ===\n")
for (nm in names(comparison[["diff"]])) {
  cat(nm, ":", toJSON(comparison[["diff"]][[nm]], auto_unbox=TRUE, pretty=TRUE), "\n")
}

# Save comparison
output_path <- "D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/大规模测试/Legacy问题处理/integration_comparison.json"
write_json(comparison, output_path, auto_unbox=TRUE, pretty=TRUE)
cat("\nComparison saved to:", output_path, "\n")
