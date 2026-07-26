# iQualityR Vignette Master Plan

> Target: Aggregated vignettes form a professional book —
> **"Quality Management with R: The iQualityR Framework"**
> Scope of this document: meta package, .core, .plot, .stat (4 packages)
> Created: 2026-07-26

## Design Principles

1. **Unified format**: All vignettes follow the MSA template — YAML frontmatter, single setup chunk (`include=FALSE` with opts + library + helpers), `rmarkdown::html_vignette` with `toc: true, toc_depth: 3`.
2. **Theory first**: Every professional/statistical vignette must include theoretical foundations, principles, and formulas before code.
3. **Quality scenarios**: All executable examples must use real quality-engineering contexts (manufacturing, pharmaceutical, service, etc.). Pseudocode-only demos are allowed only for pure API reference.
4. **The essence — Plan → Task → Analysis → Summary/Plot → Report**: The meta package and .core must clearly articulate this unified methodology as the series' core design philosophy.
5. **Book coherence**: Vignettes read sequentially as book chapters; cross-references tie them together.

## Unified YAML Frontmatter (all vignettes)

```yaml
---
title: "Chapter Title"
author: "iQualityR.<subpackage> Team"
date: "`r Sys.Date()`"
output:
  rmarkdown::html_vignette:
    toc: true
    toc_depth: 3
vignette: >
  %\VignetteIndexEntry{Chapter Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
```

## Unified Setup Chunk (MSA style, single chunk)

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  fig.align = "center",
  warning = FALSE,
  message = FALSE
)

library(iQualityR.<subpackage>)
library(ggplot2)

# Helpers
fmt_num <- function(x, digits = 4) formatC(x, format = "f", digits = digits)
fmt_pct <- function(x, digits = 2) paste0(formatC(x, format = "f", digits = digits), "%")
```

---

# Part I: Foundation & Methodology

## Chapter 1 — Meta Package: The iQualityR Methodology

**Package**: `iQualityR` (meta)
**Vignette count**: 2
**Role**: Book preface + methodology cornerstone. Introduces the Plan→Task→Analysis→Summary/Plot→Report philosophy that unifies all member packages.

### 1.1 `iQualityR.Rmd` — "The iQualityR Framework: A Unified Methodology for Quality Engineering"

**Chapter role**: Book introduction and the essence of the series.

**Section outline**:
```
## 1. Introduction
   ### 1.1 Why iQualityR? The fragmented landscape of quality tools
   ### 1.2 Design philosophy: one workflow, ten modules
   ### 1.3 Who should read this book
## 2. The Unified Methodology: Plan → Task → Analysis → Summary/Plot → Report
   ### 2.1 Plan — capturing 4M1E context (Man, Machine, Material, Method, Environment)
   ### 2.2 Task — the analysis object that orchestrates everything
   ### 2.3 Analysis — the compute engine (IqrAnalyzerBase)
   ### 2.4 Summary/Plot — the visualization engine (IqrPlotterBase)
   ### 2.5 Report — the export engine (IqrReporter, ExcelExporter)
   ### 2.6 How the five stages flow together (lifecycle diagram)
## 3. Package Architecture
   ### 3.1 The dependency graph: core → plot → stat → domain packages
   ### 3.2 Member package index (table: name, purpose, key classes)
   ### 3.3 The 4M1E metadata model shared across all packages
## 4. Installation and Quick Start
   ### 4.1 Installing the meta-package
   ### 4.2 Attaching the framework (auto-attach behavior)
   ### 4.3 A 5-minute walkthrough: capability analysis end-to-end
## 5. Conventions Used in This Book
   ### 5.1 Code style and theme defaults
   ### 5.2 Quality scenario notation
## 6. References
```

**Theory/principles to cover**:
- The 4M1E cause-classification framework from Ishikawa
- The analysis lifecycle pattern (plan→execute→visualize→report) as a generalization of DMAIC
- Why a unified R6 task object beats scattered function calls

**Quality scenario**: A bearing diameter capability study — from plan (4M1E), to data, to capability indices, to plot, to Excel report. Uses `.stat` + `.plot` + `.core` together.

### 1.2 `iQualityR-workflow.Rmd` (new) — "The Plan-Task-Analysis-Report Workflow in Practice"

**Chapter role**: Extended end-to-end case study showing the methodology across multiple subpackages.

**Section outline**:
```
## 1. Business Context
   ### 1.1 Scenario: semiconductor wafer thickness SPC + capability study
   ### 1.2 Objectives and acceptance criteria
## 2. Planning Phase — Building the IqrPlan
   ### 2.1 Defining 4M1E metadata
   ### 2.2 Specifying the analysis task registry
## 3. Execution Phase — Running the Task
   ### 3.1 Creating the task object
   ### 3.2 Running descriptive statistics (iqr_desc)
   ### 3.3 Running SPC analysis
   ### 3.4 Running capability analysis
## 4. Visualization Phase — Summarizing and Plotting
   ### 4.1 The $plot() interface
   ### 4.2 Combining plots with patchwork
   ### 4.3 Theme switching (academic / workbench / tech)
## 5. Reporting Phase — Exporting Results
   ### 5.1 Console summary
   ### 5.2 Excel report (ExcelExporter)
   ### 5.3 Multi-sheet workbook assembly
## 6. Interpretation and Decision
## 7. References
```

**Theory/principles**: How the task object decouples computation from presentation; how the registry pattern maps task names to analyzer/plotter/reporter classes.

**Quality scenario**: Semiconductor wafer thickness — SPC control chart + Cpk capability + Excel report to QA manager.

---

## Chapter 2 — .core: Core Infrastructure

**Package**: `iQualityR.core`
**Vignette count**: 2
**Role**: The foundation layer — R6 base classes, theme system, utilities.

### 2.1 `iQualityR-core.Rmd` — "Core Infrastructure: The R6 Base Classes and Workflow Engine"

**Chapter role**: Detailed reference for the 9 R6 base classes and the plan→task→analysis→report architecture.

**Section outline**:
```
## 1. Introduction
   ### 1.1 Why a layered R6 architecture?
   ### 1.2 The class hierarchy at a glance (table)
## 2. The Plan Layer — IqrPlanBase
   ### 2.1 4M1E metadata model
   ### 2.2 Creating and validating a plan
   ### 2.3 Method reference table
## 3. The Task Layer — IqrTaskBase
   ### 3.1 Task as the orchestrator (lifecycle diagram)
   ### 3.2 The task registry: mapping task names to engines
   ### 3.3 Public API: $new(), $run(), $plot(), $report(), $summary()
## 4. The Analysis Layer — IqrAnalyzerBase
   ### 4.1 The analyze() contract
   ### 4.2 Result protocol: app_result / app_success / app_error
   ### 4.3 Parameter normalization
## 5. The Visualization Layer — IqrPlotterBase
   ### 5.1 The render() contract
   ### 5.2 The color toolbox: .pal_* and .scale_* methods
   ### 5.3 Subclassing IqrPlotterBase (extension example)
## 6. The Report Layer — IqrReporter and ExcelExporter
   ### 6.1 IqrReporter: multi-format export dispatcher
   ### 6.2 ExcelExporter: themed multi-sheet workbooks
   ### 6.3 Method reference table
## 7. Utility Functions
   ### 7.1 Validation: validate_metadata, validate_inputs, validate_output_dir
   ### 7.2 Formatting: format_p_value, format_scientific
   ### 7.3 Sigma helpers: moving_range_stats, safe_tolerance
   ### 7.4 IDs and config: generate_anon_id, get_config
## 8. Internationalization — iqr_t, iqr_locale, iqr_set_locale
## 9. Best Practices for Extending .core
## 10. References
```

**Theory/principles**:
- R6 vs S4/S3 trade-offs for quality engineering APIs
- The separation of concerns: Plan (context) / Analyzer (compute) / Plotter (visual) / Reporter (export)
- The result protocol (app_result) for serializable, frontend-friendly outputs

**Quality scenario**: Building a custom "measurement system task" by subclassing the four base classes, using a real gage R&R dataset.

### 2.2 `theme-system.Rmd` (new) — "The Unified Theme and Color Pipeline"

**Chapter role**: Complete reference for the theme system — the design system that makes all iQualityR visualizations consistent.

**Section outline**:
```
## 1. Introduction
   ### 1.1 Why a unified theme system? The color chaos problem
   ### 1.2 Design goals: one switch, everything recolors
## 2. Theory: Color in Quality Visualization
   ### 2.1 The four palette classes and their visual jobs
       - discrete (categorical) — factor levels, groups
       - sequential (magnitude) — density, count, risk scores
       - diverging (signed) — correlations, residuals, deviations
       - semantic (verdict) — pass/fail/watch, significant/not
   ### 2.2 WCAG contrast and accessibility
   ### 2.3 Color-blind safety considerations
## 3. The IqrTheme Facade
   ### 3.1 IqrTheme, ThemeConfig, PlotTheme — the three-layer model
   ### 3.2 Creating a theme: IqrTheme$new("academic")
   ### 3.3 Switching themes globally: options(iqr.default_theme = ...)
## 4. Built-in Theme Presets
   ### 4.1 academic, workbench, tech (comparison plot)
   ### 4.2 External themes: economist, wsj, gdocs, tufte, few, solarized, prism
   ### 4.3 Side-by-side preset gallery (all 10 presets)
## 5. The Four-Class Palette System
   ### 5.1 Discrete palettes and auto-extension via colorRampPalette
   ### 5.2 Sequential and diverging gradients
   ### 5.3 Semantic colors: pass/fail/watch/good/bad/neutral
   ### 5.4 Paired mode: coordinated fill + outline
## 6. The IqrPlotterBase Color Toolbox
   ### 6.1 .pal_discrete(), .pal_sequential(), .pal_diverging(), .pal_semantic(), .pal_ui()
   ### 6.2 .scale_fill_discrete(), .scale_color_discrete(), ... (12 scale methods)
   ### 6.3 .contrast_text() for accessible text on colored backgrounds
   ### 6.4 API reference table
## 7. Color Utility Functions
   ### 7.1 lighten(), darken(), mix()
   ### 7.2 is_dark(), contrast_ratio()
   ### 7.3 Worked examples: deriving a palette from a brand color
## 8. Customizing and Extending Themes
   ### 8.1 Overriding individual slots
   ### 8.2 Registering a custom preset
   ### 8.3 Feeding a custom palette through the toolbox
## 9. ExcelExporter Theming
   ### 9.1 How workbook colors track the active theme
   ### 9.2 Customizing table headers, stripes, borders
## 10. Best Practices
## 11. References
```

**Theory/principles**:
- Color theory for statistical graphics (categorical vs sequential vs diverging vs semantic)
- WCAG 2.x contrast ratio formula and accessibility thresholds
- The HCL color space and why colorRampPalette interpolation works

**Quality scenario**: A multi-panel SPC dashboard rendered in 4 different themes (academic, economist, prism, solarized) showing how the same data recolors consistently.

---

# Part II: Visualization for Quality

## Chapter 3 — .plot: The Visualization Infrastructure

**Package**: `iQualityR.plot`
**Vignette count**: 9 (reorganized from existing 9)
**Role**: Complete function catalog with quality scenarios, organized by visual job.

**Design decision**: Reorganize the 9 existing vignettes into a coherent book part. Consolidate overlapping content; fill the ANOVA gap.

### 3.1 `plotting-overview.Rmd` (replaces basic_plotting_theme.Rmd) — "Visualization Infrastructure: base_plot and the Theme Pipeline"

**Chapter role**: Entry point to .plot — how every plot function sources colors from the unified theme.

**Section outline**:
```
## 1. Introduction
   ### 1.1 The grammar of iQualityR graphics
   ### 1.2 How plot functions pick colors (the IqrPlotterBase toolbox)
## 2. Quick Start
   ### 2.1 Your first base_plot
   ### 2.2 Switching themes with one line
## 3. Theory: The Unified Color Pipeline
   ### 3.1 The .iqr_plotter singleton
   ### 3.2 How a plot_* function resolves colors (flowchart)
   ### 3.3 The four palette classes matched to visual jobs
## 4. base_plot() — The Foundation
   ### 4.1 Signature and parameters
   ### 4.2 Theme input types: NULL / string / function / IqrTheme
   ### 4.3 Theme error behavior (no silent fallback)
## 5. as_iqr_theme() and as_iqr_theme_object()
## 6. set_default_theme() and the global option
## 7. combine_plots() with patchwork
## 8. save_diagram() — exporting to PNG/SVG/PDF
## 9. Theme Switching Demonstration
   ### 9.1 Same plot, four themes (academic / economist / prism / solarized)
## 10. Summary
## 11. Further Reading
```

**Quality scenario**: Steel plate thickness scatter plot rendered in 4 themes.

### 3.2 `layer-builders.Rmd` — "Composable Layer Builders for Quality Charts"

**Section outline**:
```
## 1. Introduction
   ### 1.1 Why composable layers? Reuse and consistency
## 2. Theory: The Grammar of Graphics and Layer Composition
   ### 2.1 ggplot2 layer model
   ### 2.2 How layers_* functions encapsulate best-practice defaults
## 3. Distribution Layers
   ### 3.1 layers_histogram_density() — histogram + density overlay
   ### 3.2 layers_qq() — quantile-quantile layers
## 4. Comparison Layers
   ### 4.1 layers_boxplot() — with jitter option
   ### 4.2 layers_violin() — with optional boxplot overlay
## 5. Trend and Band Layers
   ### 5.1 layers_trend_line() — regression smoothing
   ### 5.2 layers_percentile_band() — reference bands
## 6. Quality-Reference Layers
   ### 6.1 layers_spec_limits() — LSL/USL annotation (theory: specification vs control limits)
   ### 6.2 layers_control_chart() — center line + control limits + warning limits
       #### Theory: ±3σ limits, Western Electric zones
## 7. Combining Layers with base_plot
## 8. Parameter Reference Table
## 9. Summary
## 10. Further Reading
```

**Quality scenario**: Building a custom control chart from layers_control_chart + layers_spec_limits to show process stability vs capability.

### 3.3 `distribution-plots.Rmd` (new, consolidates) — "Distribution and Normality Visualization"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Assessing Distributional Assumptions
   ### 2.1 P-P plots vs Q-Q plots — what each diagnoses
   ### 2.2 Expected quantile calculation
   ### 2.3 Common distribution families in quality engineering
## 3. plot_pp() — Probability-Probability Plots
## 4. plot_qq() — Quantile-Quantile Plots
   ### 4.1 Normal Q-Q
   ### 4.2 Fitting other distributions
## 5. layers_qq() — Composable Q-Q layers
## 6. Quality Scenario: Normality Assessment of Injection-Molded Parts
## 7. Interpreting Distribution Plots — Decision Guide
## 8. Parameter Reference
## 9. Summary
## 10. Further Reading
```

### 3.4 `scatter-plots.Rmd` — "Scatter Plots for Process Correlation Analysis"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Correlation vs Causation in Process Analysis
## 3. plot_scatter_basic() — with regression and confidence band
## 4. plot_scatter_grouped() — by categorical factor
## 5. plot_scatter_bubble() — three-variable visualization
## 6. plot_scatter_density() — handling overplotting
   ### 6.1 alpha, jitter, bins, hex methods compared
## 7. Quality Scenario: Cycle Time vs Defect Rate
## 8. Parameter Reference
## 9. Summary
## 10. Further Reading
```

### 3.5 `hypothesis-plots.Rmd` — "Hypothesis Testing Visualization"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Visualizing the Logic of Hypothesis Testing
   ### 2.1 The rejection region
   ### 2.2 Critical values vs test statistics
   ### 2.3 One-sided vs two-sided alternatives
## 3. plot_hypothesis_curve() — rejection region plot
## 4. plot_hypothesis_box() — H0 line with confidence interval
## 5. plot_hypothesis_combined() — curve + box
## 6. Quality Scenario: verifying a supplier's mean fill weight claim
## 7. Decision Guide — reading the plots
## 8. Parameter Reference
## 9. Summary
## 10. Further Reading
```

### 3.6 `anova-plots.Rmd` (new, fills gap) — "ANOVA Visualization"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Visualizing ANOVA Results
   ### 2.1 The F-distribution and rejection region
   ### 2.2 Main effects and interactions
   ### 2.3 Residual diagnostics for ANOVA assumptions
## 3. plot_f_curve() — global F-test visualization
## 4. plot_anova_effects() — main effects with SE bars
## 5. plot_anova_comparison() — multiple comparison forest plot
## 6. plot_anova_residuals() — residual diagnostic panel
## 7. plot_anova_summary() — ANOVA dashboard
## 8. plot_anova_diagnostic() — assumption checks
## 9. create_anova_table() — table grob for reports
## 10. Quality Scenario: comparing three suppliers' tensile strength
## 11. Parameter Reference
## 12. Summary
## 13. Further Reading
```

### 3.7 `pareto-charts.Rmd` — "Pareto Charts for Defect Prioritization"

**Section outline**:
```
## 1. Introduction
## 2. Theory: The 80/20 Rule and Pareto Analysis
   ### 2.1 The Pareto principle in quality management
   ### 2.2 Constructing a Pareto chart (cumulative percentage)
## 3. plot_pareto_enhanced() — with grouping and weights
## 4. quick_pareto() — rapid input variants
## 5. plot_pareto_image_labels() / plot_pareto_emoji_labels() — visual labels
## 6. Quality Scenario: defect types in PCB manufacturing
## 7. Parameter Reference
## 8. Summary
## 9. Further Reading
```

### 3.8 `fishbone-diagrams.Rmd` — "Ishikawa Fishbone Diagrams for Root Cause Analysis"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Cause-and-Effect Analysis
   ### 2.1 The 5M1E framework (Man/Machine/Material/Method/Measurement/Environment)
   ### 2.2 The 5P framework for service industries
   ### 2.3 Weighting and importance coding
## 3. plot_fishbone_basic() — standard Ishikawa diagram
## 4. plot_fishbone_weighted() — line width = weight, color = importance
## 5. plot_fishbone_faceted() — comparing across departments/shifts
## 6. Quality Scenario: root cause analysis of plating defects
## 7. Parameter Reference
## 8. Summary
## 9. Further Reading
```

### 3.9 `turtle-diagram.Rmd` — "Turtle Diagrams for Process Mapping"

**Section outline**:
```
## 1. Introduction
## 2. Theory: The Process Approach (ISO 9001)
   ### 2.1 What a turtle diagram represents
   ### 2.2 The seven elements: Inputs, Outputs, Process, Resources, Responsibility, Measures, Methods
## 3. plot_turtle_diagram() — single process
## 4. Customizing the turtle diagram
## 5. Quality Scenario: mapping the calibration process
## 6. Parameter Reference
## 7. Summary
## 8. Further Reading
```

### 3.10 `advanced-plots.Rmd` — "Advanced Statistical Plots"

**Section outline**:
```
## 1. Introduction
## 2. plot_correlation_heatmap() — multivariate correlation
   ### 2.1 Theory: Pearson/Spearman correlation and diverging palettes
## 3. plot_interaction_line() — interaction effects
   ### 3.1 Theory: two-way interactions in factorial designs
## 4. plot_acf() and plot_pacf() — time series autocorrelation
   ### 4.1 Theory: ACF/PACF for process stability
## 5. plot_roc_curve() — classifier evaluation
   ### 5.1 Theory: ROC, AUC, sensitivity vs specificity
## 6. plot_variance_components() — variance decomposition
   ### 6.1 Theory: components of variation in MSA
## 7. Quality Scenario: multivariate process monitoring
## 8. Parameter Reference
## 9. Summary
## 10. Further Reading
```

---

# Part III: Statistical Methods for Quality

## Chapter 4 — .stat: Statistical Methods for Quality Engineering

**Package**: `iQualityR.stat`
**Vignette count**: 10 (consolidated from 11; 4 probability vignettes → 1)
**Role**: Comprehensive statistical methods with theory, formulas, and quality scenarios.

**Consolidation decisions**:
- Merge 4 probability vignettes → 1 (`probability-distributions.Rmd`)
- Split `model-diagnostics` into diagnostics + outlier detection
- Add new vignettes: `descriptive-statistics`, `anova`, `capability-metrics`
- Add overview vignette

### 4.1 `stat-overview.Rmd` (new) — "Statistical Methods for Quality: An Overview"

**Section outline**:
```
## 1. Introduction
   ### 1.1 The role of statistics in quality engineering
   ### 1.2 The iqr_* entry-class pattern (iqr_desc, iqr_htest, iqr_normality, iqr_prob, iqr_anova)
## 2. Module Map
   ### 2.1 Descriptive statistics
   ### 2.2 Hypothesis testing
   ### 2.3 Normality assessment
   ### 2.4 Probability distributions
   ### 2.5 ANOVA
   ### 2.6 Sample size and power
   ### 2.7 Sigma estimation and SPC constants
   ### 2.8 Capability metrics
   ### 2.9 Distribution fitting and transformation
   ### 2.10 Model diagnostics and outliers
## 3. The Unified iqr_* Workflow
   ### 3.1 $new() → $run() → $plot() → $report() → $summary()
   ### 3.2 The Analyzer/Plotter/Reporter triad behind each entry class
## 4. Quick Start: A Complete Analysis
## 5. References
```

### 4.2 `descriptive-statistics.Rmd` (new) — "Descriptive Statistics for Process Characterization"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Measures of Central Tendency, Dispersion, and Shape
   ### 2.1 Mean, median, mode — when each is appropriate
   ### 2.2 Standard deviation, variance, range, IQR
   ### 2.3 Skewness and kurtosis — formulas and interpretation
   ### 2.4 Coefficient of variation for cross-process comparison
   ### 2.5 Confidence intervals for the mean (t-distribution)
## 3. The iqr_desc Class
   ### 3.1 Lifecycle: $new() → $run() → $plot() → $report()
## 4. Functional API: desc_* family
   ### 4.1 desc_calc(), desc_analyze()
   ### 4.2 desc_hist(), desc_box(), desc_box_with_stats(), desc_plot()
   ### 4.3 desc_summary_table(), desc_to_excel()
## 5. Quality Scenario: Characterizing a CNC machining process
## 6. Interpreting Descriptive Statistics — Decision Guide
## 7. Parameter Reference
## 8. References
```

**Formulas**: mean, sd, SE, CI (t-interval), skewness (Fisher), kurtosis (excess), CV.

### 4.3 `hypothesis-testing.Rmd` — "Hypothesis Testing for Quality Decisions"

**Section outline**:
```
## 1. Introduction
## 2. Theory: The Logic of Hypothesis Testing
   ### 2.1 Null and alternative hypotheses
   ### 2.2 Type I and Type II errors, significance level, power
   ### 2.3 Test statistics and sampling distributions
   ### 2.4 p-value interpretation
   ### 2.5 One-sided vs two-sided alternatives
## 3. Test Types and Their Statistics
   ### 3.1 z-test and t-test (one-sample, two-sample) — formulas
   ### 3.2 Proportion tests (one-sample, two-sample) — formulas
   ### 3.3 Chi-square test — formula and contingency tables
   ### 3.4 Variance tests (F-test, Bartlett, Levene)
## 4. The iqr_htest Class
## 5. Functional API: htest_run(), htest_plot(), htest_interpret()
## 6. Workflow: test type → data → parameters (alternative semantics)
## 7. Quality Scenario: verifying a supplier's mean fill weight claim
## 8. Interpreting Test Results — Decision Guide
## 9. Parameter Reference
## 10. References
```

**Formulas**: z = (x̄-μ₀)/(σ/√n), t = (x̄-μ₀)/(s/√n), pooled vs unpooled SE, χ² statistic, p-value calculation.

### 4.4 `normality-testing.Rmd` (split) — "Normality Assessment"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Why Normality Matters in Quality
   ### 2.1 Assumptions underlying SPC, capability, t-tests
   ### 2.2 Anderson-Darling test — statistic and critical values
   ### 2.3 Shapiro-Wilk test — W statistic
   ### 2.4 Other tests (Cramer-von Mises, Lilliefors)
## 3. The iqr_normality Class
## 4. Functional API: normality_test(), normality_plot(), normality_interpret()
## 5. Visual Assessment: Q-Q plot, histogram, density overlay
## 6. Quality Scenario: assessing normality of coating thickness
## 7. What To Do When Data Is Non-Normal — decision tree
## 8. Parameter Reference
## 9. References
```

### 4.5 `probability-distributions.Rmd` (consolidates 4) — "Probability Distributions for Quality Engineering"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Probability Distributions in Quality
   ### 2.1 Continuous distributions (Normal, Exponential, Gamma, Weibull, Lognormal, Beta, Uniform, Cauchy, t, F, Chi-square)
   ### 2.2 Discrete distributions (Binomial, Poisson, Geometric, Hypergeometric, Negative Binomial)
   ### 2.3 PDF/PMF, CDF, quantile functions — formulas
   ### 2.4 When to use each distribution in quality (mapping table)
## 3. The iqr_prob Class
## 4. The 12 Registered Distributions — reference table
## 5. Functional API: prob_calc(), prob_plot()
## 6. list_prob_distributions(), get_prob_dist_info()
## 7. Distribution Registry: register_dist(), unregister_dist()
## 8. Quality Scenario: modeling time-to-failure with Weibull
## 9. Parameter Reference
## 10. References
```

**Formulas**: PDF/CDF for each of the 12 core distributions; the quality-application mapping (e.g., Weibull for reliability, Poisson for defect counts, Beta for proportions).

### 4.6 `anova.Rmd` (new) — "Analysis of Variance for Process Comparison"

**Section outline**:
```
## 1. Introduction
## 2. Theory: The Analysis of Variance
   ### 2.1 Partitioning total variation (SST = SSB + SSW) — formula
   ### 2.2 The F-statistic and F-distribution
   ### 2.3 One-way ANOVA model and assumptions
   ### 2.4 Two-way ANOVA and interactions
   ### 2.5 Multiple comparisons (Tukey HSD) — formula
## 3. The iqr_anova Class
   ### 3.1 Lifecycle: $new() → $run() → $plot() → $report()
## 4. Functional API: anova_run(), anova_report()
## 5. ANOVA Types: oneway, twoway, multifactor, repeated, mixed, MANOVA
## 6. Assumption Checking
   ### 6.1 Normality of residuals
   ### 6.2 Homogeneity of variance
## 7. Quality Scenario: comparing three suppliers' tensile strength
## 8. Interpreting ANOVA — Decision Guide
## 9. Parameter Reference
## 10. References
```

**Note**: Requires re-running `devtools::document()` to export the ANOVA classes first.

### 4.7 `sample-size-power.Rmd` — "Sample Size and Power Analysis"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Power and Sample Size
   ### 2.1 The power curve
   ### 2.2 Effect size (Cohen's d, f) — formulas
   ### 2.3 Type II error (β) and power (1-β)
   ### 2.4 Sample size formulas for means, proportions, ANOVA
## 3. sample_size_mean(), sample_size_two_means()
## 4. sample_size_proportion(), sample_size_two_proportions()
## 5. sample_size_anova()
## 6. calc_power(), effect_size(), power_table()
## 7. Quality Scenario: sizing a capability study to detect a 0.5σ shift
## 8. Parameter Reference
## 9. References
```

### 4.8 `sigma-and-constants.Rmd` (merge) — "Process Sigma Estimation and SPC Constants"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Estimating Process Standard Deviation
   ### 2.1 Within-subgroup vs between-subgroup variation
   ### 2.2 σ from R-bar (σ = R-bar / d2) — derivation
   ### 2.3 σ from S-bar (σ = S-bar / c4) — derivation
   ### 2.4 σ from moving range (I-MR charts)
   ### 2.5 sigma_decomposition: total = √(σ²_within + σ²_between)
## 3. sigma_estimate() — all methods
## 4. sigma_decomposition()
## 5. Theory: SPC Control Chart Constants
   ### 5.1 d2, d3 — distribution of range
   ### 5.2 c4 — unbiased SD estimator
   ### 5.3 A2, A3 — X-bar chart factors
   ### 5.4 D3, D4 — R chart limits
   ### 5.5 B3, B4 — S chart limits
   ### 5.6 E2 — I-MR chart factor
   ### 5.7 Dependence on subgroup size n (table)
## 6. get_d2(), get_d3(), get_d4(), get_c4(), get_c4_prime()
## 7. get_A2(), get_A3(), get_D3(), get_D4(), get_B3(), get_B4(), get_E2()
## 8. Quality Scenario: setting up an X-bar/R chart for a turning operation
## 9. Parameter Reference
## 10. References
```

**Formulas**: d2 = E[R]/σ, c4 = √(2/(n-1)) · Γ(n/2)/Γ((n-1)/2), A2 = 3/(d2·√n), A3 = 3/(c4·√n), D4 = 1 + 3·d3/d2, B4 = 1 + 3·√(1-c4²)/c4.

### 4.9 `capability-metrics.Rmd` (new) — "Process Capability Metrics and PPM/Sigma Conversion"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Process Capability and Six Sigma
   ### 2.1 Cp, Cpk, Pp, Ppk — formulas and interpretation
   ### 2.2 The 1.5σ shift assumption in Six Sigma
   ### 2.3 PPM and DPMO conversion
   ### 2.4 Z.Bench — the combined process sigma level
   ### 2.5 Rolled throughput yield (RTY) for multi-stage processes
## 3. capability_to_ppm() — Cpk → expected PPM
## 4. capability_interpret() — Cpk level interpretation (Six Sigma, automotive)
## 5. ppm_to_sigma(), sigma_to_ppm()
## 6. yield_to_dpmo(), dpmo_to_yield()
## 7. z_bench(), throughput_yield()
## 8. reliability(), availability()
## 9. benchmark_compare(), quality_dashboard()
## 10. Quality Scenario: benchmarking a plating line against industry standards
## 11. Parameter Reference
## 12. References
```

**Formulas**: Cp = (USL-LSL)/(6σ), Cpk = min((USL-μ),(μ-LSL))/(3σ), PPM = Φ((LSL-μ)/σ)·10⁶ + (1-Φ((USL-μ)/σ))·10⁶, Z.bench = Φ⁻¹(1-PPM/10⁶) + 1.5.

### 4.10 `distribution-fitting-transformation.Rmd` (merge) — "Distribution Fitting and Data Transformation"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Fitting Distributions to Data
   ### 2.1 Maximum likelihood estimation
   ### 2.2 Goodness-of-fit assessment
   ### 2.3 The need for transformation in quality
## 3. fit_distribution() — single distribution
## 4. auto_fit_distribution() — multi-candidate selection
## 5. compare_fits() — comparing fits
## 6. empirical_distribution(), calc_qq_data()
## 7. Theory: Data Transformation Methods
   ### 7.1 Box-Cox transformation — formula and λ optimization
   ### 7.2 Yeo-Johnson — for negative values
   ### 7.3 Johnson system (SU/SB/SL)
## 8. box_cox_transform(), yeo_johnson_transform(), johnson_transform()
## 9. log_transform(), sqrt_transform(), reciprocal_transform()
## 10. auto_transform() — automatic selection
## 11. inverse_transform()
## 12. Quality Scenario: transforming cycle time data for capability analysis
## 13. Parameter Reference
## 14. References
```

**Formulas**: Box-Cox y(λ) = (y^λ-1)/λ for λ≠0, ln(y) for λ=0; Yeo-Johnson piecewise; MLE objective.

### 4.11 `model-diagnostics-outliers.Rmd` (merge) — "Regression Model Diagnostics and Outlier Detection"

**Section outline**:
```
## 1. Introduction
## 2. Theory: Linear Regression Assumptions
   ### 2.1 LINE — Linearity, Independence, Normality, Equal variance
   ### 2.2 Diagnostic plots and what they reveal
## 3. diagnose_lm() — comprehensive diagnostics
## 4. test_residual_normality()
## 5. test_heteroscedasticity() — Breusch-Pagan, NCV, White
## 6. diagnose_multicollinearity() — VIF, condition index
## 7. diagnose_influential_points() — leverage, Cook's distance
## 8. summarize_assumptions()
## 9. Theory: Outlier Detection Methods
   ### 9.1 IQR method — fence rules
   ### 9.2 Z-score and modified Z-score (MAD)
   ### 9.3 Grubbs' test — formula
   ### 9.4 Dixon's Q test — for small samples
   ### 9.5 Mahalanobis distance — multivariate
## 10. detect_outliers_iqr(), detect_outliers_zscore()
## 11. detect_outliers_grubbs(), detect_outliers_dixon()
## 12. detect_outliers_mahalanobis(), detect_outliers_all()
## 13. Quality Scenario: diagnosing a calibration regression model
## 14. Parameter Reference
## 15. References
```

**Formulas**: VIF_j = 1/(1-R²_j), Cook's D = (e²/(p·MSE))·(h/(1-h)²), Grubbs G = max|x_i-x̄|/s, Mahalanobis D² = (x-μ)ᵀΣ⁻¹(x-μ).

---

# Summary: Book Structure at a Glance

| Part | Chapter | Package | Vignette | Pages (est.) |
|---|---|---|---|---|
| I | 1 | iQualityR (meta) | iQualityR.Rmd + iQualityR-workflow.Rmd | 2 |
| I | 2 | iQualityR.core | iQualityR-core.Rmd + theme-system.Rmd | 2 |
| II | 3 | iQualityR.plot | 10 vignettes (reorganized) | 10 |
| III | 4 | iQualityR.stat | 11 vignettes (consolidated from 11) | 11 |
| **Total** | | | **25 vignettes** | |

## Migration Notes

### Deletions (consolidations)
- `basic_plotting_theme.Rmd` → split into `plotting-overview.Rmd` (infra) + content moves to `theme-system.Rmd` (.core)
- `probability-analysis.Rmd` + `probability-and-sigma.Rmd` + `probability-distributions-intro.Rmd` + `probability-distributions.Rmd` → 1 `probability-distributions.Rmd`
- `sigma-estimation.Rmd` + `constants.Rmd` → 1 `sigma-and-constants.Rmd`
- `getting-started_error.Rmd` → absorbed into `stat-overview.Rmd`
- `model-diagnostics.Rmd` → expanded to `model-diagnostics-outliers.Rmd`

### Additions (new)
- meta: `iQualityR-workflow.Rmd`
- .core: `theme-system.Rmd`
- .plot: `distribution-plots.Rmd`, `anova-plots.Rmd`
- .stat: `stat-overview.Rmd`, `descriptive-statistics.Rmd`, `anova.Rmd`, `capability-metrics.Rmd`

### Renames
- `hypothesis_visualization.Rmd` → `hypothesis-plots.Rmd`
- `quality_tools.Rmd` → content merged into relevant chapters
- `advanced_plots.Rmd` → retained as advanced catch-all

## Cross-Reference Conventions

- Each vignette ends with `## References` (academic citations) and `## Further Reading` (links to other vignettes in the book).
- Cross-references use: "See Chapter 3.6 for ANOVA visualization" and `vignette("anova-plots", package = "iQualityR.plot")`.
- The meta vignette includes a "book map" table linking all chapters.

## Quality Scenario Library (reused across vignettes)

To maintain coherence, a shared library of quality scenarios:
1. **Bearing diameter** (capability) — meta + .stat capability
2. **Semiconductor wafer thickness** (SPC + capability) — meta workflow
3. **CNC machining** (descriptive statistics) — .stat descriptive
4. **Supplier fill weight** (hypothesis testing) — .stat hypothesis + .plot hypothesis
5. **Coating thickness** (normality) — .stat normality
6. **Time-to-failure** (Weibull) — .stat probability
7. **Supplier tensile strength** (ANOVA) — .stat ANOVA + .plot ANOVA
8. **PCB defects** (Pareto) — .plot Pareto
9. **Plating defects** (fishbone) — .plot fishbone
10. **Calibration process** (turtle) — .plot turtle
11. **Turning operation** (SPC constants) — .stat sigma + constants
12. **Plating line benchmark** (capability metrics) — .stat capability metrics
13. **Cycle time transformation** (Box-Cox) — .stat transformation
14. **Calibration regression** (diagnostics) — .stat diagnostics

## Next Steps

1. Confirm this outline meets expectations
2. Begin rewriting per package, starting with meta + .core (foundation)
3. Follow MSA template strictly for every vignette
4. Each vignette: theory → formulas → API → quality scenario → decision guide → references
