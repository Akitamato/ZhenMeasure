library(ZhenMeasure)

data_path <- file.path("D:", "My_project", "Cooperation_Project", "横向",
  "长期_2025.5.16_扬翔群喂仪算法开发（周光亮）(1)",
  "扬翔群体饲喂仪器数据处理脚本开发",
  "V项目测试与开发", "测试", "demo", "demo_standard", "Farm_C_YANGXIANG")

format_path <- file.path(data_path, "Data_format", "YANGXIANG_data_format.json")

cat("=== Smoke Test Start ===\n")
cat("Data path:", data_path, "\n")
cat("Format path:", format_path, "\n")
cat("Path exists:", dir.exists(data_path), "\n")
cat("Format exists:", file.exists(format_path), "\n\n")

result <- run_zhen_measure(
  data_path = data_path,
  data_type = "YANGXIANG",
  format_path = format_path,
  output_dir = NULL,
  growth_curve = FALSE,
  growth_curve_test = FALSE
)

cat("\n=== Smoke Test Result ===\n")
cat("SUCCESS\n")
cat("Daily records:", nrow(result$daily_records), "\n")
cat("Phenotypes:", nrow(result$phenotypes), "\n")
cat("Animals:", data.table::uniqueN(result$daily_records$animal_id), "\n")
cat("Phenotype columns:", paste(names(result$phenotypes), collapse=", "), "\n")
