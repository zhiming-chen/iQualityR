# iQualityR

> Integrated Quality Engineering Framework for R

[English](README.md) | [中文](README.zh-CN.md)

`iQualityR` is a meta-package that bundles nine cooperating R packages covering
the full quality engineering workflow: core infrastructure, statistical
foundations, visualization, measurement system analysis, process capability,
design of experiments, sampling plans, reliability analysis, and predictive
quality modeling.

Loading `iQualityR` attaches every member package so their APIs are immediately
available, mirroring the tidyverse convention.

## Repository layout

This is a **monorepo**: every member package lives as a top-level folder, and
the meta-package `iQualityR/` ties them together.

```
iQualityR/                  # Meta-package: install/load all members at once
iQualityR.core/             # R6 base classes, theme system, i18n, utilities
iQualityR.plot/             # ggplot2 layers, Pareto, fishbone, turtle, ANOVA plots
iQualityR.stat/             # Descriptive stats, hypothesis tests, distributions, SPC rules
iQualityR.msa/              # Type 1, Linearity, Gage R&R (crossed/nested), attribute agreement
iQualityR.capa/             # Normal / non-normal / nonparametric capability analysis
iQualityR.doe/              # Factorial, RSM, Taguchi, Bayesian optimization, time-effect modeling
iQualityR.sampling/         # Single / double / multiple sampling plans, OC curves, ASN
iQualityR.reliability/      # Kaplan-Meier, Cox model, parametric reliability
iQualityR.predict/          # ML model training, diagnostics, explainability (SHAP)
iQualityR.spc/              # Shewhart, time-weighted, multivariate, rare-event, ML-enhanced charts
```

## Requirements

- R >= 4.1.0
- System toolchain for source packages (Rtools on Windows; gcc/clang on Linux/macOS)
- CRAN dependencies declared in each package's `DESCRIPTION`

## Installation

Because the member packages depend on each other, install them through the
meta-package so the dependency order is resolved automatically. The
`Remotes:` field in `iQualityR/DESCRIPTION` points `remotes::install_github()`
at the correct subdirectory of this repository for each member.

```r
# install.packages("remotes")
remotes::install_github(
  "zhiming-chen/iQualityR",
  subdir = "iQualityR"        # the meta-package folder
)
```

This single call will:

1. Fetch every member package from its `subdir` (e.g. `iQualityR.core`,
   `iQualityR.stat`, ...) thanks to the `Remotes:` field.
2. Build and install them in dependency order.
3. Install the meta-package `iQualityR` itself.

After installation:

```r
library(iQualityR)
iQualityR_packages()   # list attached member packages and versions
```

### Installing individual member packages

If you only need one member package, install it directly by specifying its
`subdir`:

```r
remotes::install_github("zhiming-chen/iQualityR", subdir = "iQualityR.msa")
```

Note: every member package already declares its `iQualityR.*` dependencies in
its own `DESCRIPTION`, so `remotes` / `pak` will pull the required siblings
automatically.

## Usage

Each member package exports its own API. See the package-level help and
vignettes for details:

```r
help(package = "iQualityR.msa")
vignette(package = "iQualityR.doe")
```

## License

MIT + file LICENSE. See `LICENSE` in each package folder.

## Author

Zhiming Chen <zhimingc383@gmail.com>
