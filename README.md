# NGOReportR

An R package for exploratory analysis of NGO funding data with automated visualizations, regression diagnostics, and text-based theme detection.

## Installation
```r
# Install from GitHub
devtools::install_github("yourusername/NGOReportR")
```

## Features

- **Data validation** - Quick quality checks with `validate_data()`
- **Automated plots** - Generate and export funding visualizations to PDF
- **Regression analysis** - Linear and logistic models with built-in diagnostics (VIF, Cook's distance, LINE assumptions, AUC)
- **Theme detection** - Classify grant purposes using keyword dictionaries (education, health, climate, etc.)

## Quick Start
```r
library(NGOReportR)

# Validate data
validate_data(grant_data)

# Generate visual exploratory analysis
export_auto_plots_pdf(grant_data, "analysis.pdf")

# Run comprehensive regression report
report <- generate_ngo_report(
  data = grant_data,
  linear_response = "funding_amount",
  logistic_response = "funded",
  predictors = c("org_size", "location", "sector")
)

# Detect and visualize grant themes
themed_data <- detect_funding_themes_qdap(grant_data, "grant_purpose")
ggplot_funding_themes(themed_data)
```

## Dependencies

Built on tidyverse: dplyr, ggplot2, patchwork, cli, skimr, car, qdap

## License

MIT 

---


