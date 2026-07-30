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
