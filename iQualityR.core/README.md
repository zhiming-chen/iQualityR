# iQualityR.core

Core components for the iQualityR framework, including base classes, metadata management, reporting services, and utility functions.

## Overview

`iQualityR.core` provides the foundational components for the iQualityR framework, including:

- **Base Classes**: Abstract classes for plans, tasks, analyzers, and plotters
- **Theme Management**: Unified theme system for consistent visualization
- **Reporting Services**: Excel and R Markdown report generation
- **Utility Functions**: Helper functions for data validation, error handling, and configuration
- **Metadata Management**: Structured storage for 4M1E (Man, Machine, Material, Method, Environment) metadata

## Installation

```r
# Install from GitHub (future)
# devtools::install_github("yourusername/iQualityR.core")

# Install from local directory
remotes::install_local("path/to/iQualityR.core")
```

## Usage

### Base Classes

#### IqrPlanBase

```r
library(iQualityR.core)

# Create a plan
plan <- IqrPlanBase$new(
  task_tag = "capability",
  conf_level = 0.95
)

# Set criteria
plan$set_criteria(cpk = 1.33, ppk = 1.0)

# Set metadata
plan$set_meta("man", operator = "John", experience = 5)
plan$set_meta("machine", model = "Machine A", calibration_date = "2024-01-01")

# Validate plan
plan$validate()

# Convert to list
plan_list <- plan$to_list()
```

#### IqrTaskBase

```r
# Create a task
data <- data.frame(x = 1:10, y = rnorm(10))
task <- IqrTaskBase$new(data, theme = "academic")

# Note: IqrTaskBase is an abstract class, you should use specific subclasses
```

#### IqrAnalyzerBase

```r
# Create an analyzer
analyzer <- IqrAnalyzerBase$new()

# Set parameters
analyzer$setup(list(alpha = 0.05, method = "t-test"))

# Set results
analyzer$set_statistic("mean", 10)
analyzer$set_diagnostic("normality", "normal")

# Get results
results <- analyzer$get_results()
```

#### IqrTheme

```r
# Create a theme
theme <- IqrTheme$new(theme_style = "academic")

# Change theme
theme$set_theme("tech")

# Get color palette
palette <- theme$get_pal("discrete")

# Use with ggplot2
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  theme$theme_iqr() +
  theme$scale_color_iqr()
```

### Reporting Services

#### ExcelExporter

```r
# Create Excel exporter
theme <- IqrTheme$new(theme_style = "academic")
exporter <- ExcelExporter$new(theme$config)

# Export data to Excel
data <- list(
  Summary = data.frame(statistic = c("Mean", "SD"), value = c(10, 2)),
  Details = data.frame(x = 1:5, y = rnorm(5))
)
exporter$export_excel(data, path = "report.xlsx", title = "Analysis Report")

# Customize Excel theme
exporter$set_excel_theme(title = "#FF0000", table_header_bg = "#00FF00")
exporter$export_excel(data, path = "report_custom.xlsx")

# Reset to default theme
exporter$reset_excel_theme()
```

#### IqrReporter

```r
# Create reporter
theme <- IqrTheme$new(theme_style = "academic")
reporter <- IqrReporter$new(theme)

# Register template
reporter$register(
  task_tag = "capability",
  rmd_template = "path/to/capability_template.Rmd"
)

# Export to Excel
results <- list(
  statistics = list(mean = 10, sd = 2, cpk = 1.33),
  data = data.frame(x = 1:10, y = rnorm(10))
)
reporter$export(results, task_tag = "capability", format = "excel", path = "capability_report.xlsx")

# Export to HTML
reporter$export(results, task_tag = "capability", format = "html", path = "capability_report.html")

# Export to PDF
reporter$export(results, task_tag = "capability", format = "pdf", path = "capability_report.pdf")
```

### Utility Functions

```r
# Validate metadata
metadata <- validate_metadata(list(man = list(name = "John")))

# Validate inputs
data <- data.frame(x = 1:10, y = rnorm(10))
validate_inputs(data, c("x", "y"))

# Create error message
error_msg <- create_error_message("Invalid input", "input")

# Generate anonymous IDs
ids <- generate_anon_id(5)

# Calculate moving range statistics
data <- 1:10
mr_stats <- moving_range_stats(data, 2)

# Format p-values
p_values <- c(0.0001, 0.005, 0.05, 0.1, 0.5)
formatted <- format_p_value(p_values)

# Safe tolerance calculation
tolerance <- safe_tolerance(10, 5)

# Coalesce operator
value <- NULL %||% "default"
```

## Package Structure

```
iQualityR.core/
├── R/
│   ├── IqrPlanBase.R       # Base class for plans
│   ├── IqrTaskBase.R       # Base class for tasks
│   ├── IqrAnalyzerBase.R   # Base class for analyzers
│   ├── IqrPlotterBase.R    # Base class for plotters
│   ├── IqrTheme.R          # Theme management
│   ├── ExcelExporter.R     # Excel export utility
│   ├── IqrReporter.R       # Global reporting service
│   └── utils.R             # Utility functions
├── man/                    # Documentation
├── tests/                  # Tests
├── DESCRIPTION             # Package description
└── NAMESPACE               # Namespace
```

## Development

### Running Tests

```r
# Run tests
devtools::test("iQualityR.core")

# Build documentation
devtools::document("iQualityR.core")

# Check CRAN compatibility
devtools::check("iQualityR.core")
```

### Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests
5. Submit a pull request

## License

MIT
