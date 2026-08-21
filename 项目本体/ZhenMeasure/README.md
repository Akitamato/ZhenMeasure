# ZhenMeasure

ZhenMeasure is an R package for standardization, quality control, and phenotype
calculation on pig feeding station data.

The current version focuses on unified reading for three device types,
modular QC, birth info mapping, phenotype calculation by weight or age stage,
and structured QC summary outputs.

## Current capabilities

- Supports YANGXIANG, NEDAP, and FIRE readers
- Supports `read_only`, `qc_only`, and `full_run` modes
- Supports structured QC outputs: `error_type_summary.csv`, `animal_qc_summary.csv`
- Includes build-first validation scripts for release work

## Installation

```r
install.packages(".", repos = NULL, type = "source")
```

## Quick start

```r
library(ZhenMeasure)

res <- run_zhen_measure(
  data_path = "path/to/raw/data",
  data_type = "YANGXIANG",
  format_path = "path/to/data_format.json",
  output_dir = "output/run1",
  growth_curve = TRUE,
  growth_curve_test = TRUE,
  target_weight_stages = "YANGXIANG",
  phenotype_method = "report"
)
```

When `output_dir` is provided, `qc_only` or `full_run` automatically writes:

- `corrected_standard_records.csv`
- `corrected_records.csv`
- `corrected_daily_records.csv`
- `error_type_summary.csv`
- `animal_qc_summary.csv`
- `qc_run_summary.txt`
- `phenotypes.csv` (`full_run` only)
- `growth_curves.pdf` (`growth_curve_test = TRUE`)
- `growth_curves/*.pdf` (`growth_curve = TRUE`, one PDF per animal)

Visualization controls:

- `growth_curve_test = TRUE`: writes a combined PDF report.
- `growth_curve = TRUE`: writes per-animal PDF reports.

## Build and check

Use [inst/scripts/run_package_build_check.R](inst/scripts/run_package_build_check.R)
to run `R CMD build` first and then `R CMD check` on the generated source tarball.

```r
Rscript inst/scripts/run_package_build_check.R
```

This follows the standard package workflow and avoids checking the source
directory directly.

## Documentation set

- [vignettes/ZhenMeasure-quick-workflow.Rmd](vignettes/ZhenMeasure-quick-workflow.Rmd)
- [NEWS.md](NEWS.md)

## Current scope

The current version has completed modular refactoring for the main reader,
core QC, phenotype pipeline, structured QC outputs, and tarball-based package
validation. Only the `national_standard` QC method is supported since V1.0.0
(legacy method removed).
