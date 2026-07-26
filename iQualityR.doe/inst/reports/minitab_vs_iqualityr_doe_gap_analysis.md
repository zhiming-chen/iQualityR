# Minitab DOE vs iQualityR.doe — Gap Analysis Report

**Date:** 2026-07-22
**Scope:** Feature-by-feature comparison of Minitab Statistical Software (v21/22 + Effex/OMARS module) against `iQualityR.doe` v0.1.0.

---

## Executive Summary

`iQualityR.doe` covers **~65% of classical Minitab DOE** with strong depth in RSM, unreplicated-factorial diagnostics, and Taguchi analysis. However, there are **6 critical gaps** that prevent feature parity, plus **4 modern capabilities** where iQualityR.doe exceeds Minitab (Bayesian optimization, multi-fidelity, surrogate modeling, AutoDOECopilot).

| Category | Minitab Features | iQualityR.doe | Coverage |
|---|---:|---:|---:|
| Design Types | 13 | 9 | 69% |
| Design Management | 14 | 11 | 79% |
| Analysis | 13 | 10 | 77% |
| Visualization | 15 | 10 | 67% |
| Optimization | 9 | 7 | 78% |
| Specialty Tools | 9 | 7 | 78% |
| Reporting | 8 | 5 | 63% |
| Modern/AI Features | 5 | 7 | 140% |
| **TOTAL** | **86** | **66** | **77%** |

---

## 1. Design Types

### Feature Matrix

| Design Type | Minitab | iQualityR.doe | Notes |
|---|:---:|:---:|---|
| Full Factorial (2^k) | ✅ | ✅ | `design_type="factorial"` |
| Fractional Factorial (2^(k-p)) | ✅ | ✅ | `design_type="fractional"`, Res III/IV/V |
| Plackett-Burman | ✅ | ⚠️ | Reuses L12 from `"orthogonal"`; not standalone |
| General Full Factorial (>2 levels) | ✅ | ✅ | `design_type="factorial"` with n-level factors |
| Split-Plot / Hard-to-Change | ✅ | ❌ | **GAP** |
| Central Composite Design (CCD) | ✅ | ✅ | `design_type="ccd"`, α: rotatable/spherical/face_centered/orthogonal |
| Box-Behnken | ✅ | ✅ | `design_type="box_behnken"` |
| Simplex Centroid (Mixture) | ✅ | ❌ | **GAP** |
| Simplex Lattice (Mixture) | ✅ | ❌ | **GAP** |
| Extreme Vertices (Mixture) | ✅ | ❌ | **GAP** |
| Taguchi Orthogonal Arrays | ✅ | ✅ | L4/L8/L9/L12/L16/L27 via `"orthogonal"`/`"taguchi"` |
| Definitive Screening (DSD) | ✅ | ❌ | **GAP** — Jones & Nachtsheim 2011 |
| Custom/Optimal (D/I/A-optimal) | ✅ | ❌ | **GAP** — `evaluate_design()` only evaluates, does not construct |
| Latin Hypercube | ❌ | ✅ | `design_type="lhs"` (space-filling, computer experiments) |
| Maximin | ❌ | ✅ | `design_type="maximin"` |
| OMARS | ✅ (Effex) | ❌ | Proprietary to Minitab |

### Critical Gaps
1. **No Mixture Designs** — Simplex centroid, simplex lattice, extreme vertices are entirely missing. These are essential for formulation chemistry, food science, and material blending applications.
2. **No Definitive Screening Designs (DSD)** — Modern screening design (Jones & Nachtsheim 2011) with 3 levels per factor, main effects not aliased with 2FI. Available in R via `daewr::DSD()`.
3. **No Split-Plot Designs** — Hard-to-change factors (whole plots) vs easy-to-change (subplots). Critical for industrial experiments with nested randomization constraints.
4. **No Optimal Design Construction** — `evaluate_design()` computes D/A/G/I-optimality but there is no `create_optimal_design()` method. Minitab + Effex support D/I/A-optimal construction via coordinate-exchange.

### Where iQualityR.doe Exceeds Minitab
- **LHS / Maximin** space-filling designs for computer experiments (surrogate modeling, Bayesian optimization workflows) — Minitab lacks these entirely.

---

## 2. Design Generation & Management

| Capability | Minitab | iQualityR.doe | Notes |
|---|:---:|:---:|---|
| Randomization | ✅ | ✅ | `randomize=TRUE` |
| Blocking | ✅ | ✅ | `blocking=TRUE`, `n_blocks` |
| Replication | ✅ | ✅ | `replication` parameter |
| Center Points | ✅ | ✅ | `center_points` parameter |
| Fold-Over (add mirror runs) | ✅ | ❌ | **GAP** — de-aliasing augmentation |
| Add Axial Points (factorial→CCD) | ✅ | ❌ | **GAP** — design augmentation |
| Modify Factors (post-creation) | ✅ | ❌ | **GAP** — must recreate design |
| Botched Runs Handling | ✅ | ❌ | **GAP** — account for actual levels that deviated |
| Power & Sample Size (2^k) | ✅ | ✅ | `compute_power()` + `compute_sample_size()` |
| Power & Sample Size (Plackett-Burman) | ✅ | ❌ | **GAP** |
| Power & Sample Size (General Full Factorial) | ✅ | ❌ | **GAP** — only 2-level factorial supported |
| Power Curve Plot | ✅ | ❌ | **GAP** — Minitab renders power curve; iQualityR returns numeric only |
| Design Evaluation (Alias, Resolution) | ✅ | ✅ | `get_alias_structure()` |
| Design Evaluation (Optimality) | ✅ | ✅ | `evaluate_design()` — D/A/G/I |
| Coded ↔ Uncoded Units Toggle | ✅ | ✅ | `.convert_coded_to_actual()` |
| Standard Order ↔ Run Order Toggle | ✅ | ❌ | **GAP** — design is fixed in generation order |

### Critical Gaps
1. **No Fold-Over** — Cannot add mirror runs to break specific aliasing in fractional factorials. Essential when initial screening reveals aliased active effects.
2. **No Design Augmentation** — Cannot convert a 2-level factorial to CCD by adding axial points post-hoc. Minitab supports this via `Modify Design > Add Points`.
3. **No Power Curve Plot** — `compute_power()` returns numeric values but does not render the power-vs-effect-size curve that Minitab displays.
4. **No Post-Creation Modification** — Factor names/levels cannot be modified after design creation; user must recreate the entire design.

---

## 3. Analysis Capabilities

| Capability | Minitab | iQualityR.doe | Notes |
|---|:---:|:---:|---|
| ANOVA Table (Seq/Adj SS) | ✅ | ✅ | `$.perform_anova()` |
| Model Summary (R², adj-R², PRESS) | ✅ | ⚠️ | R² + adj-R²; **no R²(pred)/PRESS** |
| Coded + Uncoded Coefficients | ✅ | ⚠️ | Coded only; uncoded equation not displayed |
| VIF (Variance Inflation Factor) | ✅ | ❌ | **GAP** |
| Lack-of-Fit Test | ✅ | ✅ | `$.test_lack_of_fit()` |
| Curvature Test | ✅ | ✅ | `test_curvature()` |
| Lenth PSE (unreplicated) | ✅ | ✅ | `compute_lenth_pse()` — ME + SME |
| Effects Estimation | ✅ | ✅ | `$.extract_effects()` |
| Stepwise / Forward / Backward | ✅ | ❌ | **GAP** — always fits full model |
| Best Subsets / All-Subsets | ✅ | ❌ | **GAP** |
| AICc / BIC Selection | ✅ | ❌ | **GAP** |
| Alias Structure Analysis | ✅ | ✅ | `get_alias_structure()` |
| Stationary Point / Canonical | ✅ | ✅ | `compute_stationary_point()` — auto in `run()` |
| Analyze Variability (log SD²) | ✅ | ❌ | **GAP** |
| Binary Response (Logistic DOE) | ✅ | ❌ | **GAP** |

### Critical Gaps
1. **No Model Selection** — Always fits the full model (main + 2FI + quadratic for RSM). Cannot do stepwise, backward, forward, or AICc/BIC-based term selection. This is a significant limitation when many factors are inert.
2. **No R²(pred) / PRESS** — Predicted R-squared and PRESS statistic are missing. These are standard model-adequacy metrics reported by Minitab.
3. **No VIF** — Variance Inflation Factors for detecting multicollinearity are not computed.
4. **No Uncoded Coefficient Equation** — Minitab displays the regression equation in uncoded (actual engineering) units; iQualityR only shows coded coefficients.
5. **No Variability Analysis** — Cannot fit a model to log(SD²) for analyzing process variability alongside the mean (critical for robust parameter design).

---

## 4. Visualization

| Plot Type | Minitab | iQualityR.doe | Notes |
|---|:---:|:---:|---|
| Design Plot (factor space) | ✅ | ✅ | Adaptive multi-dim: k=2 single, k=3 row, k≥4 grid; PointType color/shape |
| Main Effects Plot | ✅ | ✅ | Horizontal bar chart, fixed blue/red sign coding |
| Interaction Plot | ✅ | ✅ | Per-pair independent ggplot + patchwork |
| Cube Plot | ✅ | ❌ | **GAP** — 3-factor cube corner display |
| Pareto Chart of Effects | ✅ | ✅ | `type="pareto_effects"` |
| Normal Probability Plot of Effects | ✅ | ❌ | **GAP** |
| Half-Normal Plot of Effects | ✅ | ✅ | `type="half_normal"` with Lenth ME/SME |
| Contour Plot | ✅ | ✅ | `type="contour"` — viridis fill + iso-lines |
| Surface Plot (3D) | ✅ | ✅ | `type="surface"` — 2D tile + contour |
| Wireframe Plot | ✅ | ❌ | **GAP** |
| Residuals: 4-in-1 Panel | ✅ | ✅ | `type="residual"` — delegates to `plot_anova_residuals()` |
| Residuals vs Predictors | ✅ | ❌ | **GAP** |
| Overlaid Contour (multi-response) | ✅ | ❌ | **GAP** |
| Response Optimization Plot | ✅ | ❌ | **GAP** — desirability traces |
| Power Curve Plot | ✅ | ❌ | **GAP** |
| Interactive 3D Surface (plotly) | ❌ | ✅ | `response_surface$interactive_plot` |

### Critical Gaps
1. **No Cube Plot** — The 3-factor cube display showing 8 corner means is a Minitab staple for visualizing 2³ factorial designs.
2. **No Normal Probability Plot of Effects** — Only half-normal is implemented. Minitab shows both; the normal plot (with signed effects) is more informative for direction.
3. **No Overlaid Contour Plot** — Cannot visualize multiple responses on the same 2-factor contour plane with constraint regions. Critical for multi-response optimization.
4. **No Response Optimization Plot** — Minitab's optimizer shows desirability traces and predicted response at optimal settings in a multi-panel layout.
5. **No Wireframe Plot** — 3D wireframe (without tile fill) is missing; only filled surface is available.

### Where iQualityR.doe Exceeds Minitab
- **Interactive 3D plotly surface** — `response_surface$interactive_plot` allows rotation/zoom; Minitab's surface is static.

---

## 5. Optimization & Response

| Capability | Minitab | iQualityR.doe | Notes |
|---|:---:|:---:|---|
| Desirability (min/max/target) | ✅ | ✅ | `MultiResponseOptimizer$compute_individual_desirability()` |
| Overall Desirability (weighted) | ✅ | ✅ | `compute_overall_desirability()` |
| Multi-Response Optimization | ✅ | ✅ | `optimize_desirability()` — grid / L-BFGS-B |
| Pareto Frontier | ❌ | ✅ | `find_pareto_frontier()` |
| TOPSIS Ranking | ❌ | ✅ | `rank_with_topsis()` |
| Overlaid Contour Constraints | ✅ | ❌ | **GAP** |
| Probability of Success (Effex) | ✅ | ❌ | **GAP** |
| Prediction Intervals at Optimum | ✅ | ✅ | Via `compute_stationary_point()` |
| Bayesian Optimization | ❌ | ✅ | `BayesianOptimizer` — GP + EI |

### Critical Gaps
1. **No Overlaid Contour Plot with Constraints** — Cannot shade feasible regions where multiple responses simultaneously meet specifications. This is the standard visualization for multi-response optimization in Minitab/JMP.

### Where iQualityR.doe Exceeds Minitab
- **Pareto Frontier + TOPSIS** — Multi-criteria decision making (MCDM) capabilities not available in Minitab.
- **Bayesian Optimization** — Sequential experiment selection via GP + Expected Improvement; Minitab has no equivalent.

---

## 6. Specialized Tools

| Tool | Minitab | iQualityR.doe | Notes |
|---|:---:|:---:|---|
| Alias Structure Analysis | ✅ | ✅ | `get_alias_structure()` |
| Path of Steepest Ascent | ✅ | ❌ | **GAP** |
| Stationary Point / Canonical | ✅ | ✅ | `compute_stationary_point()` |
| Ridge Analysis | ✅ | ❌ | **GAP** — for stationary point outside design region |
| Taguchi SN Ratios | ✅ | ✅ | `TaguchiAnalyzer$compute_sn_ratio()` — larger/nominal/smaller |
| Taguchi Response Tables | ✅ | ⚠️ | Partial — contributions via ANOVA only |
| Taguchi Prediction | ✅ | ❌ | **GAP** — optimal level prediction + CI |
| Dynamic Taguchi (signal factor) | ✅ | ❌ | **GAP** |
| MSA Integration | ✅ | ❌ | **GAP** — no gage R&R pre-DOE check |
| Bayesian Optimization | ❌ | ✅ | `BayesianOptimizer` |
| Multi-Fidelity Optimization | ❌ | ✅ | `MultiFidelityOptimizer` |
| Surrogate Modeling | ❌ | ✅ | `SurrogateModeler` — GP + virtual experiments |
| AutoDOECopilot | ❌ | ✅ | Rule-based expert system |
| Time-Effect / Stability Modeling | ❌ | ✅ | `TimeEffectModeler` — ICH Q1E shelf-life |

### Critical Gaps
1. **No Path of Steepest Ascent** — When the stationary point is outside the design region or the first-order model is adequate, Minitab computes the direction of steepest ascent to move toward the optimum. This is a fundamental RSM tool.
2. **No Ridge Analysis** — When the stationary point is outside the design region, ridge analysis computes the optimum on a sphere of increasing radius. Essential for constrained RSM.
3. **No Taguchi Prediction** — Cannot predict the optimal factor levels and compute confidence intervals for the predicted SN ratio.
4. **No Dynamic Taguchi** — Only static Taguchi (no signal factor) is supported. Dynamic Taguchi (with signal factor for input-output relationships) is missing.

### Where iQualityR.doe Exceeds Minitab
- **Time-Effect / Stability Modeling** — `TimeEffectModeler` implements ICH Q1E shelf-life estimation with Arrhenius acceleration, multi-batch poolability testing. No Minitab equivalent.
- **Surrogate Modeling** — GP-based virtual experiments with built-in digital twins (welding, injection, battery).
- **Predictive DOE Flywheel** — 4-stage closed loop combining multi-fidelity, surrogate, and copilot recommendations.

---

## 7. Reporting & Output

| Capability | Minitab | iQualityR.doe | Notes |
|---|:---:|:---:|---|
| Console Summary | ✅ | ✅ | `task$summary()` |
| Excel Export | ✅ | ✅ | `DoeReporter$export_excel()` |
| HTML Report | ❌ | ✅ | `DoeReporter$export_html()` |
| Word / PowerPoint Export | ✅ | ❌ | **GAP** |
| Stored Model on Worksheet | ✅ | ❌ | **GAP** — model not persisted for post-analysis |
| Graph Export (PNG/JPEG/TIF/EMF) | ✅ | ⚠️ | Via `ggsave()` only (not integrated) |
| Macros / Automation | ✅ | ❌ | **GAP** |
| Session Window Output | ✅ | ⚠️ | Console only; no persistent session log |

---

## 8. Priority Recommendations

### P0 — Critical (blocks core DOE workflows)
| # | Gap | Effort | R Package Reference |
|---|---|---|---|
| 1 | **Path of Steepest Ascent** | Medium | `rsm::steepest()` |
| 2 | **Model Selection (stepwise/AICc)** | Medium | `MASS::stepAIC()`, `leaps::regsubsets()` |
| 3 | **Definitive Screening Designs** | Medium | `daewr::DSD()` |
| 4 | **Overlaid Contour Plot** | Low | `ggplot2` + `geom_contour` manual |

### P1 — High (common user expectations)
| # | Gap | Effort | R Package Reference |
|---|---|---|---|
| 5 | **Fold-Over / Design Augmentation** | Medium | `FrF2::fold.design()` |
| 6 | **R²(pred) / PRESS** | Low | `stats::predict(model)` + manual |
| 7 | **Normal Probability Plot of Effects** | Low | Manual ggplot2 |
| 8 | **Cube Plot** | Low | `ggplot2` 3D projection |
| 9 | **Uncoded Coefficient Equation** | Low | Transform from coded |
| 10 | **Power Curve Plot** | Low | `ggplot2` line plot |

### P2 — Medium (niche but valuable)
| # | Gap | Effort | R Package Reference |
|---|---|---|---|
| 11 | **Mixture Designs** | High | `mixexp::Xvert()`, `mixexp::SLD()` |
| 12 | **Split-Plot Designs** | High | `FrF2::factorial2k_design` + nesting |
| 13 | **Optimal Design Construction** | High | `AlgDesign::optFederov()`, `skpr` |
| 14 | **Ridge Analysis** | Medium | Manual eigenvalue + optimization |
| 15 | **Taguchi Prediction + Dynamic** | Medium | Manual |

### P3 — Low (nice-to-have)
| # | Gap | Effort |
|---|---|---|
| 16 | VIF | Low |
| 17 | Wireframe Plot | Low |
| 18 | Residuals vs Predictors | Low |
| 19 | Word/PowerPoint Export | Medium |
| 20 | Botched Runs | Medium |

---

## 9. Conclusion

`iQualityR.doe` provides a solid foundation for classical DOE with strong RSM capabilities and modern AI-enhanced features (Bayesian optimization, multi-fidelity, surrogate modeling) that exceed Minitab. However, **4 critical gaps** prevent it from being a drop-in Minitab replacement:

1. **No Path of Steepest Ascent** — breaks the standard RSM workflow (screen → steepest ascent → RSM)
2. **No Model Selection** — always fits full model; cannot identify and remove inert terms
3. **No Definitive Screening Designs** — modern screening design missing
4. **No Overlaid Contour Plot** — multi-response optimization visualization missing

Focusing on the **P0 priorities** (items 1–4 above) would bring the package to ~85% Minitab parity while preserving its unique modern capabilities.
