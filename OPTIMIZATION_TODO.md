# iQualityR Series Packages Optimization TODO

> Systematic optimization of the iQualityR meta-package: unified theme/color pipeline, vignette standardization, and cross-package consistency.
> Created: 2026-07-26 | Last Updated: 2026-07-26

## Background

The `.core` subpackage now provides a unified `IqrTheme` + `IqrPlotterBase` toolbox (four-class palette: discrete / sequential / diverging / semantic). However, only `.plot` has been migrated to the toolbox. The other 9 subpackages still hardcode colors, duplicate theme-initialization boilerplate, and have inconsistent vignette formats. This document tracks the full optimization plan.

## Current State Snapshot (Audit Results)

### Hardcoded Color Audit (R code + vignettes)

| Subpackage | R hex | R named | R total | Vignette hex | Vignette named | Vignette total | Toolbox usage | Migration status |
|---|---:|---:|---:|---:|---:|---:|---:|---|
| iQualityR.core | 411 | 4 | 415 | 34 | 4 | 38 | 15 (definitions) | Toolbox source |
| iQualityR.plot | 28 | 29 | 57 | 64 | 15 | 79 | 295 | **Migrated** |
| iQualityR.stat | 4 | 31 | 35 | 0 | 1 | 1 | 0 | Not started |
| iQualityR.msa | 97 | 5 | 102 | 8 | 1 | 9 | 0 | Not started |
| iQualityR.capa | 6 | 1 | 7 | - | - | - | 0 | Not started |
| iQualityR.doe | 26 | 7 | 33 | 29 | 58 | 87 | 0 | Not started |
| iQualityR.sampling | 8 | 0 | 8 | 0 | 0 | 0 | 0 | Not started |
| iQualityR.reliability | 2 | 4 | 6 | 0 | 0 | 0 | 0 | Not started |
| iQualityR.predict | 11 | 20 | 31 | 0 | 0 | 0 | 0 | Not started |
| iQualityR.spc | 18 | 0 | 18 | 0 | 3 | 3 | 0 | Not started |
| **Total** | **611** | **101** | **712** | **135** | **82** | **217** | **310** | - |

True violations (excluding toolbox definitions and `default=` fallbacks): ~213 in R code, ~150 in vignettes.

### Vignette Format Audit

- **52 .Rmd files** across 10 subpackages (iQualityR.capa has none)
- **7 different author values**, **3 date styles**, **4 output configs** — inconsistent YAML frontmatter
- **6 setup-chunk patterns** — inconsistent knitr bootstrap
- **4 structural paradigms**: doe 4-section, msa 11-section case study, plot function catalog, spc chart catalog
- **iQualityR.stat is the most inconsistent** (4 sub-paradigms, mixed Chinese/English headings)
- Reference templates: `doe_rsm_workflow.Rmd` (technical), `gage-rr-crossed.Rmd` (case study), `scatter_plots.Rmd` (function catalog), `spc-shewhart.Rmd` (chart catalog)

---

## Phase 0: Unify Default Theme (Highest Priority)

**Problem**: Default `theme` parameter is inconsistent across subpackages — the root cause of "messy" visuals.
- `iQualityR.plot/R/plot_factory.R:939` `set_default_theme(theme = "prism")` — defaults to prism
- `iQualityR.stat/R/desc.R` 6 functions default `theme = "prism"` (desc_hist, desc_box, desc_box_with_stats, desc_plot, desc_stats_table, iqr_desc)
- All other subpackages (spc, msa, predict, doe) default `theme = "academic"`

**Principle**: Unless explicitly demonstrating user customization, all default themes MUST be unified to `"academic"` (the .core default and the majority convention). Users who want a different look set it once via `options(iqr.default_theme = "xxx")` or pass `theme = "xxx"` explicitly.

### 0.1 Unify R code defaults
- [x] `iQualityR.plot/R/plot_factory.R:939`: `set_default_theme(theme = "prism")` → `"academic"`
- [x] `iQualityR.stat/R/desc.R`: 6 functions `theme = "prism"` → `"academic"`
- [ ] Audit all other subpackage `theme =` defaults — confirm none use prism/tech/workbench as default
- [ ] Audit vignettes — all should use `theme = "academic"` unless explicitly demonstrating theme switching

### 0.2 Unify vignette defaults
- [ ] All vignette setup chunks set `options(iqr.default_theme = "academic")` once
- [ ] All `plot_*()` / `task$plot()` calls in vignettes omit `theme=` (use the global default) unless demonstrating theme switching
- [ ] Dedicated "Theme Switching" section only in `basic_plotting_theme.Rmd` and `iQualityR-core.Rmd` (the canonical theme docs)

### 0.3 Verification
- [ ] `grep -r 'theme = "prism"' packages/*/R/` returns 0 matches (except explicit theme-switching demos)
- [ ] `grep -r 'theme = "tech"' packages/*/R/` returns 0 matches (except tests and demos)
- [ ] All vignettes render with consistent academic palette by default

---

## Phase 1: .stat Subpackage Full Migration

### 1.1 anova.R comprehensive fix
- [ ] Delete `create_stat_table` dead code block (anova.R:577-578). `create_anova_table` is ANOVA-table-specific (highlights p-values, adds Source column) and does NOT fit the main-effects summary table; keep `gridExtra::tableGrob(df_plot)` directly.
- [ ] Fix syntax error (anova.R:181 has stray `)`)
- [ ] Translate 217 lines of Chinese comments/roxygen to English (CRAN requirement)
- [ ] Convert 6 `exists() + ::` defensive patterns to direct `::` calls (.plot is in Imports, guaranteed available)
- [ ] Migrate 13 hardcoded colors (`steelblue`, `red`, `blue`) to IqrPlotterBase toolbox

### 1.2 .stat package-wide infrastructure
- [ ] Add `.iqr_plotter <- IqrPlotterBase$new()` singleton in `package.R` (mirrors .plot pattern)
- [ ] Import `IqrPlotterBase` from .core in NAMESPACE/DESCRIPTION
- [ ] Deduplicate theme-init boilerplate in 4 Plotter classes (HTestPlotter, NormalityPlotter, AnovaPlotter, ProbPlotter) — extract common `initialize()`/`set_theme()` logic

### 1.3 .stat other files color migration
- [ ] `NormalityPlotter.R`: 3 hardcoded colors (steelblue, red, white)
- [ ] `desc.R`: 5 hardcoded colors (#1259aa, #A9C4E3, #F2F2F2, red)
- [ ] `quality_metrics.R`: 5-level quality grade colors (green/blue/yellow/orange/red) → `.pal_semantic()`
- [ ] `model_diag.R`: 4 named colors
- [ ] `iqr_htest.R`, `iqr_normality.R`, `iqr_prob.R`: theme-init boilerplate dedup

### 1.4 NAMESPACE cleanup
- [ ] Remove redundant importFrom: `iqr_t`, `plot_qq`, `plot_pp` (use `::` instead)
- [ ] Add missing importFrom: `as_iqr_theme`, `plot_scatter_basic`, `plot_interaction_line`, `plot_variance_components`, `create_anova_table`
- [ ] Standardize: all .plot functions via importFrom + direct call (no `::`), OR all via `::` (no importFrom). Pick one style and apply consistently.

### 1.5 Unify theme access pattern
- [ ] Standardize on Pattern A: pass `IqrTheme` object to .plot functions via `theme = self$theme_obj`
- [ ] Refactor `ProbPlotter` (currently uses `themeobj$plot$theme_iqr()` / `scale_color_iqr()` / `scale_fill_iqr()` directly) to use `.plot` functions + toolbox like other Plotters
- [ ] Replace `desc.R` default `theme = "prism"` string-param style with consistent IqrTheme-object pattern

### 1.6 Verification
- [ ] `devtools::document('packages/iQualityR.stat')`
- [ ] `R CMD check --as-cran` on iQualityR.stat
- [ ] All existing tests pass
- [ ] No hardcoded color names remain (automated test)

---

## Phase 2: .plot Vignette Rewrite (6 files)

### 2.1 Remove hardcoded colors from vignettes
- [ ] `layer_builders.Rmd`: 28 hardcoded colors → use theme params
- [ ] `basic_plotting_theme.Rmd`: 17 hardcoded colors (ironic — this is the theme vignette)
- [ ] `scatter_plots.Rmd`: 14 hardcoded colors
- [ ] `quality_tools.Rmd`: 7 hardcoded colors
- [ ] `turtle_diagram.Rmd`: 7 hardcoded colors
- [ ] `fishbone_diagrams.Rmd`: 4 hardcoded colors

### 2.2 Vignette format standardization
- [ ] Unify YAML frontmatter: `author: "iQualityR Team"`, `date: "\`r Sys.Date()\`"`, `output: rmarkdown::html_vignette` with `toc: true, toc_depth: 3`
- [ ] Unify setup chunk to Pattern A (include=FALSE opts_chunk + setup library two-chunk)
- [ ] Apply function-catalog structure: Overview → Load Data → numbered function sections (Basic Usage / Custom Options / Interpreting / Practical Application) → Summary → Further Reading

---

## Phase 3: Other Subpackages Migration (P0-P2)

### 3.1 P0: iQualityR.msa (97 hex + 5 named — heaviest)
- [ ] `Type1Plotter.R`: 36 hex (6-color palette hardcoded repeatedly)
- [ ] `AttrGagePlotter.R`: 35 hex + 5 named (kappa rating color scale)
- [ ] `00_msa_classes.R`: 26 hex
- [ ] Vignette: `gage-rr-batch.Rmd` (3 hex), `attribute-gage-analytic.Rmd` (3 hex), `gage-linearity.Rmd` (1 named)
- [ ] Introduce `.iqr_plotter` singleton, migrate all hardcoded to `.pal_*` / `.scale_*`

### 3.2 P0: iQualityR.spc (18 hex — all true violations)
- [ ] `SpcPlotter.R`: UCL/LCL/CL/out-of-control point colors all hardcoded
- [ ] Deduplicate with .plot's `layers_control_chart` (which already uses toolbox)
- [ ] Vignette: 3 `bg = "white"` in `ggsave` calls (acceptable, but verify)

### 3.3 P1: iQualityR.doe (26 hex R + 58 named vignette)
- [ ] `DoePlotter.R`: 21 hex + 7 named (self-built primary/danger/muted fields — should use toolbox semantic)
- [ ] `TimeEffectModeler.R`: 3 hex
- [ ] `MultiResponseOptimizer.R`: 2 hex
- [ ] Vignettes: 6 files with hardcoded colors (time_effect_modeling, doe_modern, multi_response_optimization, doe_taguchi, doe_rsm, doe_theory)

### 3.4 P1: iQualityR.predict (20 named — all true violations)
- [ ] `PredictivePlotter.R`: replace `.safe_theme_color()` with `.pal_*` / `.pal_semantic()`
- [ ] 20 named colors in geom_*/scale_* calls

### 3.5 P2: iQualityR.capa (7 R, no vignettes)
- [ ] `DistributionFitter.R`: 6 hex (#3498DB, #E74C3C)
- [ ] `CapabilityPlotter.R`: 1 named (darkgreen)

### 3.6 P2: iQualityR.sampling (8 R — all fallback defaults)
- [ ] `package.R`: 4 `.safe_*` functions duplicate toolbox semantic colors — replace with `.pal_semantic()`

### 3.7 P2: iQualityR.reliability (6 R)
- [ ] `ReliabilityPlotter.R`: 2 hex + 4 named (self-built `.safe_*` + scale_*_manual hardcoded)

---

## Phase 4: Vignette Standardization (All Subpackages)

### 4.1 Establish unified templates
- [ ] **Technical template** (based on `doe_rsm_workflow.Rmd`): Introduction → API Overview → Visualising Results → Conclusion → References. For: doe, predict, reliability.
- [ ] **Case study template** (based on `gage-rr-crossed.Rmd`): Business Context → Method/Formulas → Data → Quick Start → Step-by-Step → Deep-Dive → Visualization Gallery → Report Export → Interpretation/Decision → Reference. For: msa, spc, sampling.
- [ ] **Function catalog template** (based on `scatter_plots.Rmd`): Overview → Load Data → numbered function sections → Practical Cases → Parameter Details → Best Practices → FAQ → Summary. For: plot, stat, capa.
- [ ] **Overview template** (based on `iQualityR-core.Rmd`): Overview → Quick Start → Design Rationale → Member Packages. For: meta, core.

### 4.2 Apply unified YAML frontmatter
- [ ] All vignettes: `author: "iQualityR Team"`, `date: "\`r Sys.Date()\`"`, `output: rmarkdown::html_vignette` with `toc: true, toc_depth: 3`
- [ ] Remove hardcoded dates (spc, stat sigma-estimation, probability-analysis)
- [ ] Add missing author/date fields (5 doe vignettes)

### 4.3 Apply unified setup chunk (Pattern A)
- [ ] Two-chunk pattern: `include=FALSE` opts_chunk + `setup` library chunk
- [ ] Replace 5 other patterns (spc helper, predict single-chunk, sampling simplified, stat mixed, core simplified)

### 4.4 Subpackage-specific rewrites
- [ ] **iQualityR.stat**: Rewrite 11 vignettes to function-catalog template (currently 4 sub-paradigms, mixed Chinese/English headings)
- [ ] **iQualityR.doe early 5**: Rewrite `analysis-visualization`, `advanced-ai-ml`, `rsm-optimization`, `design-generation`, `taguchi-robust-design` to match `doe_rsm_workflow.Rmd` (use `**Decision.**` paragraphs instead of `### Decision` subsections)
- [ ] **iQualityR.spc**: Promote `iqr_plot()` helper to cross-package utility (ggsave + include_graphics pattern)

---

## Phase 5: Cross-Package Infrastructure

### 5.1 Promote IqrPlotterBase utilities
- [ ] Consider moving `iqr_plot()` helper from spc to .core or .plot (for cross-package reuse)
- [ ] Consolidate `.safe_*` fallback functions (sampling, reliability, predict) into IqrPlotterBase

### 5.2 Dependency graph cleanup
- [ ] Verify all subpackages follow: depend on .core, .plot, .stat only (per user requirement)
- [ ] Remove any cross-dependencies beyond these three

### 5.3 Final CRAN preparation
- [ ] Remove all `Remotes:` fields before CRAN submission (CRAN rejects Remotes)
- [ ] Verify no non-ASCII characters in any R code (Chinese comments/roxygen)
- [ ] `R CMD check --as-cran` on all 11 packages

---

## Progress Log

| Date | Phase | Task | Status | Notes |
|---|---|---|---|---|
| 2026-07-26 | - | Initial audit | Done | 3 parallel search agents completed |
| 2026-07-26 | - | TODO document created | Done | This file |
| 2026-07-26 | Phase 0 | Unify default theme in R code | Done | set_default_theme + desc.R 6 functions → "academic" |
| 2026-07-26 | Phase 0 | Fix as_iqr_theme_object default | Done | "workbench" → "academic", added @export |
| 2026-07-26 | Phase 1.1 | anova.R comprehensive rewrite | Done | 1342→1242 lines; syntax fix, dead code removed, 217 CN→EN, 13 colors migrated, 6 exists()→:: |
| 2026-07-26 | Phase 1.2 | .stat .iqr_plotter singleton | Done | Added in package.R, IqrPlotterBase imported |
| 2026-07-26 | Phase 1.3 | desc.R color migration | Done | 7 hardcoded → toolbox, 6 defaults → academic |
| 2026-07-26 | Phase 1.3 | NormalityPlotter.R color migration | Done | 3 hardcoded → toolbox |
| 2026-07-26 | Phase 1.3 | quality_metrics.R | Skipped | API return values (semantic level), needs API design eval |
| 2026-07-26 | Phase 1.6 | R CMD check --as-cran | Done | 0 errors, 0 warnings, 0 notes |

---

## Key Constraints (from project_memory)

- All documentation (vignettes, tests, comments) must be in English (CRAN)
- Non-ASCII characters in R code must be translated to ASCII English
- iQualityR.doe vignettes must follow standardized 4-section structure
- All task$plot() calls must be wrapped in print()
- Vignettes must include 'Decision' paragraphs after each plot
- R6 class-level @field tags and field-level @field tags are mutually exclusive
- Must reuse existing functions from .core, .stat, .plot subpackages
- ML enhancement functions must use requireNamespace() for soft dependencies
