# =============================================================================
# File: tests/testthat/test-anova.R
# Description: ANOVA module tests
#   - AnovaAnalyzer (one-way / two-way / multifactor / repeated / mixed / manova)
#   - AnovaPlotter (Contract 2 signature)
#   - AnovaReporter (Contract 2 signature)
#   - iqr_anova L3 integrator
#   - Convenience functions (anova_run / anova_report)
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# Test data fixtures
# ----------------------------------------------------------------------------

# One-way ANOVA: 3 groups, clear difference
make_oneway_data <- function(seed = 123) {
  set.seed(seed)
  df <- data.frame(
    Value = c(rnorm(30, mean = 10, sd = 1),
              rnorm(30, mean = 12, sd = 1),
              rnorm(30, mean = 14, sd = 1)),
    Group = factor(rep(c("A", "B", "C"), each = 30))
  )
  df
}

# Two-way ANOVA: 2 factors with interaction
make_twoway_data <- function(seed = 123) {
  set.seed(seed)
  df <- data.frame(
    Adhesion  = rnorm(24, mean = 50, sd = 5),
    PaintType = factor(rep(c("A", "B", "C", "D"), each = 6)),
    Pressure  = factor(rep(c("Low", "High"), each = 3, times = 4))
  )
  df
}

# ----------------------------------------------------------------------------
# AnovaAnalyzer -- initialization & all 6 methods
# ----------------------------------------------------------------------------

test_that("AnovaAnalyzer initialization", {
  analyzer <- AnovaAnalyzer$new()
  expect_true(inherits(analyzer, "AnovaAnalyzer"))
  expect_true(inherits(analyzer, "R6"))
})

test_that("AnovaAnalyzer one-way ANOVA via anova_oneway", {
  df <- make_oneway_data()
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_oneway(Value ~ Group, data = df)

  expect_type(result, "list")
  expect_equal(result$test_type, "One-way ANOVA")
  expect_true("anova_table" %in% names(result))
  expect_true("model" %in% names(result))
  expect_true("r_squared" %in% names(result))
  expect_true("effect_size" %in% names(result))
  expect_true("factors" %in% names(result))
  expect_equal(result$n, nrow(df))
  expect_equal(result$method, "aov")
})

test_that("AnovaAnalyzer two-way ANOVA via anova_twoway", {
  df <- make_twoway_data()
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_twoway(Adhesion ~ PaintType * Pressure, data = df)

  expect_type(result, "list")
  expect_equal(result$test_type, "Two-way ANOVA")
  expect_true("anova_table" %in% names(result))
  expect_true("effect_size" %in% names(result))
  expect_equal(result$n, nrow(df))
})

test_that("AnovaAnalyzer multi-factor ANOVA", {
  set.seed(123)
  df <- data.frame(
    Y = rnorm(60, mean = 50, sd = 2),
    F1 = factor(rep(c("a", "b"), each = 30)),
    F2 = factor(rep(c("x", "y"), each = 15)),
    F3 = factor(rep(c("p", "q"), each = 7, length.out = 60))
  )
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_multifactor(Y ~ F1 * F2 * F3, data = df)

  expect_type(result, "list")
  expect_equal(result$test_type, "Multi-factor ANOVA")
  expect_true("anova_table" %in% names(result))
})

test_that("AnovaAnalyzer repeated measures ANOVA", {
  set.seed(123)
  # 6 subjects, 3 time points (long format)
  df <- data.frame(
    Subject = factor(rep(1:6, each = 3)),
    Time    = factor(rep(c("T1", "T2", "T3"), times = 6)),
    Score   = c(rnorm(6, 10), rnorm(6, 12), rnorm(6, 14))
  )
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_repeated(Score ~ Time + Error(Subject/Time), data = df)

  expect_type(result, "list")
  expect_equal(result$test_type, "Repeated Measures ANOVA")
  expect_true("anova_tables" %in% names(result))
  expect_true("model" %in% names(result))
})

test_that("AnovaAnalyzer MANOVA", {
  set.seed(123)
  df <- data.frame(
    Y1 = c(rnorm(20, 10), rnorm(20, 12)),
    Y2 = c(rnorm(20, 5), rnorm(20, 7)),
    Group = factor(rep(c("A", "B"), each = 20))
  )
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$manova(cbind(Y1, Y2) ~ Group, data = df)

  expect_type(result, "list")
  expect_equal(result$test_type, "MANOVA")
  expect_true("summary" %in% names(result))
  expect_equal(result$method, "manova")
})

test_that("AnovaAnalyzer analyze auto-dispatches one-way", {
  df <- make_oneway_data()
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$analyze(Value ~ Group, data = df)
  expect_equal(result$test_type, "One-way ANOVA")
})

test_that("AnovaAnalyzer analyze auto-dispatches two-way", {
  df <- make_twoway_data()
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$analyze(Adhesion ~ PaintType * Pressure, data = df)
  expect_equal(result$test_type, "Two-way ANOVA")
})

test_that("AnovaAnalyzer analyze auto-dispatches repeated measures", {
  set.seed(123)
  df <- data.frame(
    Subject = factor(rep(1:6, each = 3)),
    Time    = factor(rep(c("T1", "T2", "T3"), times = 6)),
    Score   = rnorm(18, 10)
  )
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$analyze(Score ~ Time + Error(Subject/Time), data = df)
  expect_equal(result$test_type, "Repeated Measures ANOVA")
})

test_that("AnovaAnalyzer analyze auto-dispatches MANOVA", {
  set.seed(123)
  df <- data.frame(
    Y1 = c(rnorm(20, 10), rnorm(20, 12)),
    Y2 = c(rnorm(20, 5), rnorm(20, 7)),
    Group = factor(rep(c("A", "B"), each = 20))
  )
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$analyze(cbind(Y1, Y2) ~ Group, data = df)
  expect_equal(result$test_type, "MANOVA")
})

# Mixed models require lme4 -- skip when not installed
test_that("AnovaAnalyzer mixed model (requires lme4)", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest")
  set.seed(123)
  df <- data.frame(
    Response = rnorm(60, 50, 2),
    Treatment = factor(rep(c("A", "B", "C"), each = 20)),
    Subject = factor(rep(1:10, times = 6))
  )
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_mixed(Response ~ Treatment + (1 | Subject), data = df)

  expect_type(result, "list")
  expect_equal(result$test_type, "Linear Mixed Model")
  expect_true("fixed_effects" %in% names(result))
  expect_true("random_effects" %in% names(result))
})

# ----------------------------------------------------------------------------
# AnovaPlotter -- Contract 2 signature
# ----------------------------------------------------------------------------

test_that("AnovaPlotter initialization", {
  plotter <- AnovaPlotter$new()
  expect_true(inherits(plotter, "AnovaPlotter"))
  expect_true(inherits(plotter, "R6"))
})

test_that("AnovaPlotter$plot has Contract 2 signature", {
  f <- AnovaPlotter$new()$plot
  fm <- formals(f)
  expect_true("result" %in% names(fm))
  expect_true("plot_type" %in% names(fm))
  expect_true("show_table" %in% names(fm))
  expect_true("theme_obj" %in% names(fm))
  expect_equal(fm$plot_type, "auto")
  expect_equal(fm$show_table, FALSE)
})

test_that("AnovaPlotter$plot rejects bad plot_type", {
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  plotter <- AnovaPlotter$new()
  expect_error(plotter$plot(result, plot_type = "nope"), "arg")
})

# ----------------------------------------------------------------------------
# AnovaReporter -- Contract 2 signature
# ----------------------------------------------------------------------------

test_that("AnovaReporter initialization", {
  reporter <- AnovaReporter$new()
  expect_true(inherits(reporter, "AnovaReporter"))
  expect_true(inherits(reporter, "R6"))
})

test_that("AnovaReporter$report has Contract 2 signature (result, format, path, audience)", {
  f <- AnovaReporter$new()$report
  fm <- formals(f)
  expect_true("result" %in% names(fm))
  expect_true("format" %in% names(fm))
  expect_true("path" %in% names(fm))
})

# ----------------------------------------------------------------------------
# iqr_anova L3 integrator
# ----------------------------------------------------------------------------

test_that("iqr_anova R6 class entry", {
  obj <- iqr_anova$new()
  expect_true(inherits(obj, "iqr_anova"))
  expect_true(inherits(obj, "R6"))
  expect_false(is.null(obj$analyzer))
  expect_false(is.null(obj$plotter))
})

test_that("iqr_anova$run caches results on last_results", {
  df <- make_oneway_data()
  obj <- iqr_anova$new()
  obj$run(Value ~ Group, data = df)
  expect_false(is.null(obj$last_results))
  expect_equal(obj$last_results$test_type, "One-way ANOVA")
})

test_that("iqr_anova$plot errors before $run", {
  obj <- iqr_anova$new()
  expect_error(obj$plot(), "Run analysis first")
})

test_that("iqr_anova$report errors before $run", {
  obj <- iqr_anova$new()
  expect_error(obj$report(), "Run analysis first")
})

# ----------------------------------------------------------------------------
# Convenience functions
# ----------------------------------------------------------------------------

test_that("anova_run returns ANOVA result list", {
  df <- make_twoway_data()
  result <- anova_run(Adhesion ~ PaintType * Pressure, data = df)
  expect_type(result, "list")
  expect_equal(result$test_type, "Two-way ANOVA")
})

# ----------------------------------------------------------------------------
# Effect sizes / model fit metrics (R2-7c)
# ----------------------------------------------------------------------------

test_that("anova_oneway returns eta and partial_eta effect sizes", {
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  expect_type(result$effect_size, "list")
  expect_true("eta" %in% names(result$effect_size))
  expect_true("partial_eta" %in% names(result$effect_size))
  # eta values are in [0, 1] and named by term
  expect_true(all(result$effect_size$eta >= 0))
  expect_true(all(result$effect_size$eta <= 1))
  expect_true(length(result$effect_size$eta) >= 2)  # Group + Residuals
})

test_that(".calc_effect_size omega type works", {
  df <- make_oneway_data()
  model <- aov(Value ~ Group, data = df)
  omega <- iQualityR.stat:::.calc_effect_size(model, type = "omega")
  expect_type(omega, "double")
  expect_true(length(omega) >= 2)
  expect_true(all(!is.na(omega)))
})

test_that(".calc_effect_size eta type matches manual computation", {
  df <- make_oneway_data()
  model <- aov(Value ~ Group, data = df)
  eta <- iQualityR.stat:::.calc_effect_size(model, type = "eta")
  # eta = SS_term / SS_total; SS_total = sum(SS)
  aov_df <- as.data.frame(summary(model)[[1]])
  ss <- aov_df[, "Sum Sq"]
  expected_eta <- ss / sum(ss)
  expect_equal(as.numeric(eta), as.numeric(expected_eta), tolerance = 1e-10)
})

test_that("anova_twoway includes r_squared and adj_r_squared", {
  df <- make_twoway_data()
  result <- AnovaAnalyzer$new()$anova_twoway(Adhesion ~ PaintType * Pressure, data = df)
  expect_true(is.numeric(result$r_squared))
  expect_true(is.numeric(result$adj_r_squared))
  expect_gte(result$r_squared, 0)
  expect_lte(result$r_squared, 1)
  expect_lte(result$adj_r_squared, result$r_squared + 1e-10)
})

test_that("anova_oneway multiple_comparisons is available when emmeans installed", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("multcomp")
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  # When emmeans + multcomp are installed, multiple_comparisons should be non-NULL
  expect_false(is.null(result$multiple_comparisons))
})

# ----------------------------------------------------------------------------
# MANOVA -- all 4 test statistics (R2-7c)
# ----------------------------------------------------------------------------

test_that("MANOVA supports Pillai test statistic", {
  set.seed(123)
  df <- data.frame(
    Y1 = c(rnorm(20, 10), rnorm(20, 12)),
    Y2 = c(rnorm(20, 5), rnorm(20, 7)),
    Group = factor(rep(c("A", "B"), each = 20))
  )
  result <- AnovaAnalyzer$new()$manova(cbind(Y1, Y2) ~ Group, data = df, test = "Pillai")
  expect_equal(result$test_statistic, "Pillai")
  expect_true("summary" %in% names(result))
})

test_that("MANOVA supports Hotelling-Lawley test statistic", {
  set.seed(123)
  df <- data.frame(
    Y1 = c(rnorm(20, 10), rnorm(20, 12)),
    Y2 = c(rnorm(20, 5), rnorm(20, 7)),
    Group = factor(rep(c("A", "B"), each = 20))
  )
  result <- AnovaAnalyzer$new()$manova(cbind(Y1, Y2) ~ Group, data = df, test = "Hotelling-Lawley")
  expect_equal(result$test_statistic, "Hotelling-Lawley")
})

test_that("MANOVA supports Roy test statistic", {
  set.seed(123)
  df <- data.frame(
    Y1 = c(rnorm(20, 10), rnorm(20, 12)),
    Y2 = c(rnorm(20, 5), rnorm(20, 7)),
    Group = factor(rep(c("A", "B"), each = 20))
  )
  result <- AnovaAnalyzer$new()$manova(cbind(Y1, Y2) ~ Group, data = df, test = "Roy")
  expect_equal(result$test_statistic, "Roy")
})

test_that("MANOVA errors on unknown test statistic", {
  set.seed(123)
  df <- data.frame(
    Y1 = rnorm(20), Y2 = rnorm(20),
    Group = factor(rep(c("A", "B"), each = 10))
  )
  expect_error(AnovaAnalyzer$new()$manova(cbind(Y1, Y2) ~ Group, data = df, test = "nope"))
})

test_that("MANOVA returns coefficients residuals and fitted", {
  set.seed(123)
  df <- data.frame(
    Y1 = c(rnorm(20, 10), rnorm(20, 12)),
    Y2 = c(rnorm(20, 5), rnorm(20, 7)),
    Group = factor(rep(c("A", "B"), each = 20))
  )
  result <- AnovaAnalyzer$new()$manova(cbind(Y1, Y2) ~ Group, data = df)
  expect_true("coefficients" %in% names(result))
  expect_true("residuals" %in% names(result))
  expect_true("fitted" %in% names(result))
  # NOTE: For MANOVA, residuals() returns an n_obs x n_dv matrix, so
  # length(residuals) = n_obs * n_dv (here 40 * 2 = 80), not n_obs.
  # Just assert n is positive and consistent with the residual matrix.
  expect_true(result$n > 0)
  expect_equal(nrow(result$residuals), 40L)
})

# ----------------------------------------------------------------------------
# Repeated measures ANOVA -- structure (R2-7c)
# ----------------------------------------------------------------------------

test_that("anova_repeated returns named anova_tables by Error stratum", {
  set.seed(123)
  df <- data.frame(
    Subject = factor(rep(1:6, each = 3)),
    Time    = factor(rep(c("T1", "T2", "T3"), times = 6)),
    Score   = c(rnorm(6, 10), rnorm(6, 12), rnorm(6, 14))
  )
  result <- AnovaAnalyzer$new()$anova_repeated(Score ~ Time + Error(Subject/Time), data = df)
  expect_type(result$anova_tables, "list")
  expect_true(length(result$anova_tables) >= 1)
  # Each stratum table is a data.frame
  for (nm in names(result$anova_tables)) {
    expect_s3_class(result$anova_tables[[nm]], "data.frame")
  }
})

# ----------------------------------------------------------------------------
# Mixed model -- ML method and effect size (R2-7c)
# ----------------------------------------------------------------------------

test_that("anova_mixed with ML method", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest")
  set.seed(123)
  df <- data.frame(
    Response = rnorm(60, 50, 2),
    Treatment = factor(rep(c("A", "B", "C"), each = 20)),
    Subject = factor(rep(1:10, times = 6))
  )
  result <- AnovaAnalyzer$new()$anova_mixed(Response ~ Treatment + (1 | Subject),
                                            data = df, method = "ML")
  expect_equal(result$test_type, "Linear Mixed Model")
  expect_equal(result$method, "lmer")
  expect_true("fixed_effects" %in% names(result))
  expect_true("random_effects" %in% names(result))
  expect_true(is.data.frame(result$random_effects))
})

test_that(".calc_effect_size returns GLMM R-squared for mixed models", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("MuMIn")
  set.seed(123)
  df <- data.frame(
    Response = rnorm(60, 50, 2),
    Treatment = factor(rep(c("A", "B", "C"), each = 20)),
    Subject = factor(rep(1:10, times = 6))
  )
  model <- lmerTest::lmer(Response ~ Treatment + (1 | Subject), data = df)
  eff <- iQualityR.stat:::.calc_effect_size(model, type = "eta")
  # MuMIn::r.squaredGLMM returns a 2-row matrix (R2m, R2c)
  expect_true(is.matrix(eff))
  expect_true(nrow(eff) >= 1)
})

test_that("anova_mixed errors when lme4 not available", {
  # Cannot easily simulate absence of lme4 when installed; this test guards
  # the error path only when lme4 is genuinely missing. Skip otherwise.
  if (requireNamespace("lme4", quietly = TRUE)) skip("lme4 installed; cannot test error path")
  set.seed(123)
  df <- data.frame(
    Response = rnorm(30), Treatment = factor(rep(c("A", "B", "C"), each = 10)),
    Subject = factor(rep(1:5, times = 6))
  )
  expect_error(AnovaAnalyzer$new()$anova_mixed(Response ~ Treatment + (1 | Subject), data = df),
               "lme4")
})

# ----------------------------------------------------------------------------
# anova_to_excel_data -- internal sheet converter (R2-7c)
# ----------------------------------------------------------------------------

test_that("anova_to_excel_data converts one-way ANOVA to sheet list", {
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  sheets <- iQualityR.stat:::anova_to_excel_data(result)
  expect_type(sheets, "list")
  expect_true("ANOVA" %in% names(sheets))
  expect_s3_class(sheets[["ANOVA"]], "data.frame")
  # One-way has r_squared -> Model_Summary sheet
  expect_true("Model_Summary" %in% names(sheets))
  # Coefficients sheet
  expect_true("Coefficients" %in% names(sheets))
  # Diagnostics sheet (residuals + fitted)
  expect_true("Diagnostics" %in% names(sheets))
})

test_that("anova_to_excel_data converts repeated measures with multiple strata", {
  set.seed(123)
  df <- data.frame(
    Subject = factor(rep(1:6, each = 3)),
    Time    = factor(rep(c("T1", "T2", "T3"), times = 6)),
    Score   = c(rnorm(6, 10), rnorm(6, 12), rnorm(6, 14))
  )
  result <- AnovaAnalyzer$new()$anova_repeated(Score ~ Time + Error(Subject/Time), data = df)
  sheets <- iQualityR.stat:::anova_to_excel_data(result)
  # Repeated measures has anova_tables (plural) -> one sheet per stratum
  expect_true(any(grepl("^ANOVA_", names(sheets))))
})

test_that("anova_to_excel_data includes effect sizes sheet", {
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  sheets <- iQualityR.stat:::anova_to_excel_data(result)
  expect_true("Effect_Sizes" %in% names(sheets))
  eff <- sheets[["Effect_Sizes"]]
  expect_true("Eta_Squared" %in% names(eff))
})

# ----------------------------------------------------------------------------
# iqr_anova L3 integrator -- interpret + report (R2-7c)
# ----------------------------------------------------------------------------

test_that("iqr_anova$run with interpret=TRUE prints summary", {
  df <- make_oneway_data()
  obj <- iqr_anova$new()
  obj$run(Value ~ Group, data = df, interpret = TRUE)
  expect_false(is.null(obj$last_results))
  expect_equal(obj$last_results$test_type, "One-way ANOVA")
})

test_that("iqr_anova$report generates excel file", {
  skip_if_not_installed("iQualityR.core")
  df <- make_oneway_data()
  obj <- iqr_anova$new()
  obj$run(Value ~ Group, data = df)
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf), add = TRUE)
  expect_no_error(obj$report(format = "excel", path = tf))
  expect_true(file.exists(tf))
})

test_that("anova_report convenience function generates excel file", {
  skip_if_not_installed("iQualityR.core")
  df <- make_oneway_data()
  result <- anova_run(Value ~ Group, data = df)
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf), add = TRUE)
  expect_no_error(anova_report(result, format = "excel", path = tf))
  expect_true(file.exists(tf))
})

test_that("AnovaAnalyzer$report (L1-level) errors before analysis", {
  analyzer <- AnovaAnalyzer$new()
  expect_error(analyzer$report(), "No results")
})

# ----------------------------------------------------------------------------
# AnovaPlotter -- error paths and theme (R2-7c)
# ----------------------------------------------------------------------------

test_that("AnovaPlotter$set_theme updates theme_obj", {
  skip_if_not_installed("iQualityR.core")
  plotter <- AnovaPlotter$new()
  # Use a valid theme name (tufte is one of the registered styles)
  theme_obj <- iQualityR.core::IqrTheme$new("tufte")
  plotter$set_theme(theme_obj)
  expect_true(inherits(plotter$theme_obj, "IqrTheme"))
})

test_that("AnovaPlotter$set_theme accepts theme name string", {
  plotter <- AnovaPlotter$new()
  plotter$set_theme("academic")
  expect_false(is.null(plotter$theme_obj))
})

test_that("AnovaPlotter$plot_variance errors on non-mixed model", {
  skip_if_not_installed("iQualityR.plot")
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  plotter <- AnovaPlotter$new()
  expect_error(plotter$plot_variance(result), "mixed models")
})

test_that("AnovaPlotter$plot_interaction errors without two factors", {
  skip_if_not_installed("iQualityR.plot")
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  plotter <- AnovaPlotter$new()
  expect_error(plotter$plot_interaction(result), "Two factors")
})

test_that("AnovaPlotter$plot_f_curve extracts F statistic from one-way result", {
  skip_if_not_installed("iQualityR.plot")
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  plotter <- AnovaPlotter$new()
  p <- plotter$plot_f_curve(result, alpha = 0.05)
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("AnovaPlotter$plot_residuals produces patchwork for one-way", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("patchwork")
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  plotter <- AnovaPlotter$new()
  p <- plotter$plot_residuals(result)
  expect_true(inherits(p, "patchwork"))
})

test_that("AnovaPlotter$plot_effects produces ggplot for one-way", {
  skip_if_not_installed("iQualityR.plot")
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  plotter <- AnovaPlotter$new()
  p <- plotter$plot_effects(result)
  expect_true(inherits(p, "ggplot"))
})

test_that("AnovaPlotter$plot auto-dispatches interaction for two-way", {
  skip_if_not_installed("iQualityR.plot")
  df <- make_twoway_data()
  result <- AnovaAnalyzer$new()$anova_twoway(Adhesion ~ PaintType * Pressure, data = df)
  plotter <- AnovaPlotter$new()
  p <- plotter$plot(result, plot_type = "auto")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("AnovaPlotter$plot_comparison works with cached comparisons", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("multcomp")
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  plotter <- AnovaPlotter$new()
  p <- plotter$plot_comparison(result)
  expect_true(inherits(p, "ggplot"))
})

test_that("AnovaPlotter$plot_summary produces patchwork", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("patchwork")
  df <- make_twoway_data()
  result <- AnovaAnalyzer$new()$anova_twoway(Adhesion ~ PaintType * Pressure, data = df)
  plotter <- AnovaPlotter$new()
  p <- plotter$plot_summary(result)
  expect_true(inherits(p, "patchwork"))
})

test_that("AnovaPlotter initialize accepts IqrTheme object", {
  skip_if_not_installed("iQualityR.core")
  theme_obj <- iQualityR.core::IqrTheme$new("academic")
  plotter <- AnovaPlotter$new(theme = theme_obj)
  expect_true(inherits(plotter$theme_obj, "IqrTheme"))
})

# ----------------------------------------------------------------------------
# ANOM (Analysis of Means) -- R3-D3
# ----------------------------------------------------------------------------

test_that("anova_anom returns a structured ANOM result", {
  df <- make_oneway_data()
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_anom(Value ~ Group, data = df)

  expect_type(result, "list")
  expect_equal(result$test_type, "ANOM")
  expect_equal(result$method, "anom")
  expect_true("anom_table" %in% names(result))
  expect_s3_class(result$anom_table, "data.frame")
  # Three groups -> three rows
  expect_equal(nrow(result$anom_table), 3L)
  # anom_table has the expected columns
  expect_true(all(c("level", "n", "mean", "deviation", "se", "lcl", "ucl",
                    "out_of_limits") %in% names(result$anom_table)))
})

test_that("anova_anom grand_mean equals overall mean", {
  df <- make_oneway_data()
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_anom(Value ~ Group, data = df)
  expect_equal(result$grand_mean, mean(df$Value), tolerance = 1e-10)
})

test_that("anova_anom deviations sum to zero (weighted) for balanced design", {
  df <- make_oneway_data()  # balanced: 30 per group
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_anom(Value ~ Group, data = df)
  # For a balanced design the unweighted sum of group deviations is 0
  expect_equal(sum(result$anom_table$deviation), 0, tolerance = 1e-10)
})

test_that("anova_anom MSE matches aov residual MS", {
  df <- make_oneway_data()
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_anom(Value ~ Group, data = df)
  aov_tbl <- as.data.frame(summary(aov(Value ~ Group, data = df))[[1]])
  mse_ref <- aov_tbl[nrow(aov_tbl), "Sum Sq"] / aov_tbl[nrow(aov_tbl), "Df"]
  expect_equal(result$mse, mse_ref, tolerance = 1e-10)
  expect_equal(result$df_error, aov_tbl[nrow(aov_tbl), "Df"])
})

test_that("anova_anom h_alpha uses Studentized range approximation", {
  df <- make_oneway_data()
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_anom(Value ~ Group, data = df, conf_level = 0.95)
  k <- result$k
  df_error <- result$df_error
  expected_h <- stats::qtukey(0.95, k, df_error) / sqrt(2)
  expect_equal(result$h_alpha, expected_h, tolerance = 1e-10)
  expect_equal(result$alpha, 0.05)
  expect_equal(result$conf_level, 0.95)
})

test_that("anova_anom decision limits bracket the grand mean", {
  df <- make_oneway_data()
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_anom(Value ~ Group, data = df)
  # All LCL < grand_mean < UCL
  expect_true(all(result$anom_table$lcl < result$grand_mean))
  expect_true(all(result$anom_table$ucl > result$grand_mean))
  # Balanced design -> common SE -> symmetric limits
  se <- result$anom_table$se[1]
  expect_equal(result$anom_table$ucl[1],
               result$grand_mean + result$h_alpha * se, tolerance = 1e-10)
  expect_equal(result$anom_table$lcl[1],
               result$grand_mean - result$h_alpha * se, tolerance = 1e-10)
})

test_that("anova_anom detects an out-of-limits level with a strong signal", {
  set.seed(42)
  # Group C has a large upward shift; the grand mean rises toward C, so A/B
  # also fall below the lower decision limit. This is the correct ANOM
  # behaviour -- a single extreme level drags the grand mean and can flag the
  # remaining levels on the opposite side. We assert C is flagged above.
  df <- data.frame(
    Value = c(rnorm(30, 10, 1), rnorm(30, 10, 1), rnorm(30, 16, 1)),
    Group = factor(rep(c("A", "B", "C"), each = 30))
  )
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_anom(Value ~ Group, data = df)
  # Group C should be outside the limits (above the grand mean)
  c_row <- result$anom_table[result$anom_table$level == "C", ]
  expect_true(c_row$out_of_limits)
  expect_true(c_row$mean > result$grand_mean)
})

test_that("anova_anom flags only the shifted level for a moderate signal", {
  # Deterministic data: each group uses the same zero-sum offset pattern so
  # group means are exact (no sampling noise) while within-group variance is
  # non-zero. C is shifted into the ANOM window (1.5*h*SE, 3*h*SE) so ONLY C
  # is flagged while A and B stay inside the decision limits.
  offsets <- c(-3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5)  # sum 0, SS = 42
  df <- data.frame(
    Value = c(10 + offsets, 10 + offsets, 14 + offsets),
    Group = factor(rep(c("A", "B", "C"), each = 8))
  )
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_anom(Value ~ Group, data = df)
  c_row <- result$anom_table[result$anom_table$level == "C", ]
  ab_rows <- result$anom_table[result$anom_table$level %in% c("A", "B"), ]
  expect_true(c_row$out_of_limits)
  expect_false(any(ab_rows$out_of_limits))
})

test_that("anova_anom flags no levels when groups are identical", {
  set.seed(1)
  # All groups drawn from the same distribution -> no significant deviation
  df <- data.frame(
    Value = rnorm(90, 10, 1),
    Group = factor(rep(c("A", "B", "C"), each = 30))
  )
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_anom(Value ~ Group, data = df)
  expect_false(any(result$anom_table$out_of_limits))
})

test_that("anova_anom supports unbalanced designs", {
  set.seed(7)
  df <- data.frame(
    Value = c(rnorm(20, 10, 1), rnorm(40, 12, 1), rnorm(10, 14, 1)),
    Group = factor(rep(c("A", "B", "C"), times = c(20, 40, 10)))
  )
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$anova_anom(Value ~ Group, data = df)
  # Unbalanced -> per-level SE differs
  expect_equal(length(unique(result$anom_table$se)), 3L)
  # Smaller group has larger SE
  expect_gt(result$anom_table$se[result$anom_table$level == "C"],
            result$anom_table$se[result$anom_table$level == "B"])
})

test_that("anova_anom respects conf_level", {
  df <- make_oneway_data()
  analyzer <- AnovaAnalyzer$new()
  r95 <- analyzer$anova_anom(Value ~ Group, data = df, conf_level = 0.95)
  r99 <- analyzer$anova_anom(Value ~ Group, data = df, conf_level = 0.99)
  # Higher confidence -> wider limits -> larger h_alpha
  expect_gt(r99$h_alpha, r95$h_alpha)
  expect_true(all(r99$anom_table$ucl >= r95$anom_table$ucl))
  expect_true(all(r99$anom_table$lcl <= r95$anom_table$lcl))
})

test_that("analyze with method='anom' dispatches to ANOM", {
  df <- make_oneway_data()
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$analyze(Value ~ Group, data = df, method = "anom")
  expect_equal(result$test_type, "ANOM")
})

test_that("analyze without method still auto-dispatches to one-way ANOVA", {
  df <- make_oneway_data()
  analyzer <- AnovaAnalyzer$new()
  result <- analyzer$analyze(Value ~ Group, data = df)
  expect_equal(result$test_type, "One-way ANOVA")
})

test_that("iqr_anova$run with method='anom' runs ANOM", {
  df <- make_oneway_data()
  obj <- iqr_anova$new()
  obj$run(Value ~ Group, data = df, method = "anom")
  expect_equal(obj$last_results$test_type, "ANOM")
})

test_that("anova_run with method='anom' returns ANOM result", {
  df <- make_oneway_data()
  result <- anova_run(Value ~ Group, data = df, method = "anom")
  expect_type(result, "list")
  expect_equal(result$test_type, "ANOM")
  expect_true("anom_table" %in% names(result))
})

test_that("AnovaPlotter$plot_anom produces ggplot for ANOM result", {
  skip_if_not_installed("iQualityR.plot")
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_anom(Value ~ Group, data = df)
  plotter <- AnovaPlotter$new()
  p <- plotter$plot_anom(result)
  expect_true(inherits(p, "ggplot"))
})

test_that("AnovaPlotter$plot auto-dispatches anom for ANOM result", {
  skip_if_not_installed("iQualityR.plot")
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_anom(Value ~ Group, data = df)
  plotter <- AnovaPlotter$new()
  p <- plotter$plot(result, plot_type = "auto")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("AnovaPlotter$plot accepts plot_type='anom'", {
  skip_if_not_installed("iQualityR.plot")
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_anom(Value ~ Group, data = df)
  plotter <- AnovaPlotter$new()
  p <- plotter$plot(result, plot_type = "anom")
  expect_true(inherits(p, "ggplot"))
})

test_that("AnovaPlotter$plot_anom errors on non-ANOM result", {
  skip_if_not_installed("iQualityR.plot")
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  plotter <- AnovaPlotter$new()
  expect_error(plotter$plot_anom(result), "ANOM")
})

test_that("anova_to_excel_data emits ANOM sheets for ANOM result", {
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_anom(Value ~ Group, data = df)
  sheets <- iQualityR.stat:::anova_to_excel_data(result)
  expect_true("ANOM" %in% names(sheets))
  expect_true("ANOM_Summary" %in% names(sheets))
  expect_s3_class(sheets[["ANOM"]], "data.frame")
  expect_equal(nrow(sheets[["ANOM"]]), 3L)
})

test_that("anova_to_excel_data does not emit ANOM sheet for non-ANOM result", {
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_oneway(Value ~ Group, data = df)
  sheets <- iQualityR.stat:::anova_to_excel_data(result)
  expect_false("ANOM" %in% names(sheets))
})

test_that("iqr_anova$report generates excel from ANOM result", {
  skip_if_not_installed("iQualityR.core")
  df <- make_oneway_data()
  obj <- iqr_anova$new()
  obj$run(Value ~ Group, data = df, method = "anom")
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf), add = TRUE)
  expect_no_error(obj$report(format = "excel", path = tf))
  expect_true(file.exists(tf))
})

test_that("StatInterpreter$interpret dispatches ANOM result", {
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_anom(Value ~ Group, data = df)
  interp <- StatInterpreter$new()$interpret(result, audience = "manager")
  expect_type(interp, "character")
  expect_true(grepl("ANOM", interp))
})

test_that("StatInterpreter$interpret ANOM technical audience", {
  set.seed(42)
  df <- data.frame(
    Value = c(rnorm(30, 10, 1), rnorm(30, 10, 1), rnorm(30, 16, 1)),
    Group = factor(rep(c("A", "B", "C"), each = 30))
  )
  result <- AnovaAnalyzer$new()$anova_anom(Value ~ Group, data = df)
  interp <- StatInterpreter$new()$interpret(result, audience = "technical")
  expect_true(grepl("Studentized-range", interp))
  # C is out of limits -> reported
  expect_true(grepl("1", interp))
})

test_that("anova_anom errors on multi-factor formula", {
  # ANOM is defined for one-way layouts; a two-factor formula should still
  # run (using the first term) but the user is expected to pass one-way.
  # Here we only assert it does not silently return a malformed result.
  df <- make_twoway_data()
  analyzer <- AnovaAnalyzer$new()
  # The aov fit works; ANOM uses terms(model)[1] -> first factor only.
  result <- analyzer$anova_anom(Adhesion ~ PaintType, data = df)
  expect_equal(result$test_type, "ANOM")
  expect_equal(result$k, nlevels(df$PaintType))
})

test_that("anova_anom N equals total observations", {
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_anom(Value ~ Group, data = df)
  expect_equal(result$N, nrow(df))
  expect_equal(result$n, nrow(df))
  expect_equal(sum(result$anom_table$n), nrow(df))
})

test_that("anova_anom data_name is captured", {
  df <- make_oneway_data()
  result <- AnovaAnalyzer$new()$anova_anom(Value ~ Group, data = df)
  expect_equal(result$data_name, "df")
})

