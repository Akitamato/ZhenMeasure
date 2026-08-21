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

baseline_results <- list()
for (src in sources) {
  cat("Running baseline for:", src[["name"]], "\n")
  result <- tryCatch({
    run_zhen_measure(data_path=src[["path"]], data_type=src[["type"]], format_path=src[["format"]],
                     qc_method="national_standard")
  }, error=function(e) list(error=e$message))
  baseline_results[[src[["name"]]]] <- result
}

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

baseline_metrics <- list()
for (nm in names(baseline_results)) {
  baseline_metrics[[nm]] <- collect_metrics(baseline_results[[nm]], nm)
}

cat("\n=== BASELINE METRICS ===\n")
for (nm in names(baseline_metrics)) {
  cat(nm, ":", toJSON(baseline_metrics[[nm]], auto_unbox=TRUE, pretty=TRUE), "\n")
}

output_path <- "D:/My_project/Cooperation_Project/横向/长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)/扬翔群体饲喂仪器数据处理脚本开发/V项目测试与开发/测试/大规模测试/Legacy问题处理/baseline_metrics.json"
write_json(baseline_metrics, output_path, auto_unbox=TRUE, pretty=TRUE)
cat("\nBaseline metrics saved to:", output_path, "\n")
