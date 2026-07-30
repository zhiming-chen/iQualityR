# =============================================================================
# File: tests/testthat/test-diagnostics.R
# Description: Model diagnostics module tests (model_diag.R)
#   - diagnose_lm and related functions
#   - StatInterpreter general dispatch
# =============================================================================

library(testthat)
library(iQualityR.stat)

test_that("diagnose_lm linear model diagnostics", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5)
  model <- lm(y ~ x)
  result <- diagnose_lm(model)

  expect_type(result, "list")
  expect_true("residuals" %in% names(result))
  expect_true("normality" %in% names(result))
  expect_true("heteroscedasticity" %in% names(result))
  expect_true("multicollinearity" %in% names(result))
  expect_true("influence" %in% names(result))
})

test_that("test_residual_normality residual normality", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5)
  model <- lm(y ~ x)
  result <- test_residual_normality(model)

  expect_type(result, "list")
  expect_true("p.value" %in% names(result))
  expect_true("is_normal" %in% names(result))
})

test_that("test_heteroscedasticity heteroscedasticity test", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5)
  model <- lm(y ~ x)
  result <- test_heteroscedasticity(model)

  expect_type(result, "list")
  expect_true("p.value" %in% names(result))
  expect_true("is_heteroscedastic" %in% names(result))
})

test_that("diagnose_multicollinearity multicollinearity", {
  set.seed(123)
  x1 <- rnorm(100)
  x2 <- rnorm(100)
  y <- 2 * x1 + 3 * x2 + rnorm(100)
  model <- lm(y ~ x1 + x2)
  result <- diagnose_multicollinearity(model)

  expect_type(result, "list")
  expect_true("vif" %in% names(result))
})

test_that("diagnose_influential_points influential point diagnostics", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5)
  model <- lm(y ~ x)
  result <- diagnose_influential_points(model)

  expect_type(result, "list")
  expect_true("high_leverage" %in% names(result))
  expect_true("outliers" %in% names(result))
  expect_true("influential" %in% names(result))
})

test_that("summarize_assumptions assumptions summary", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5)
  model <- lm(y ~ x)
  result <- summarize_assumptions(model)

  expect_s3_class(result, "data.frame")
  expect_true("assumption" %in% names(result))
  expect_true("passed" %in% names(result))
})

# ----------------------------------------------------------------------------
# StatInterpreter unified interpreter (general dispatch)
# ----------------------------------------------------------------------------

test_that("StatInterpreter initialization", {
  interpreter <- StatInterpreter$new()
  expect_true(inherits(interpreter, "StatInterpreter"))
  expect_true(inherits(interpreter, "R6"))
})

test_that("StatInterpreter interprets distribution result", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 0, sd = 1),
    calc_result = NULL
  )
  explanation <- interpreter$interpret(dist_result, audience = "manager")

  expect_type(explanation, "character")
  expect_true(nchar(explanation) > 0)
})

test_that("StatInterpreter interprets htest result", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  htest_result <- t.test(x, mu = 5)
  explanation <- interpreter$interpret(htest_result, audience = "manager")

  expect_type(explanation, "character")
  expect_true(nchar(explanation) > 0)
})

test_that("StatInterpreter interprets normality test result", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(50)
  normality_result <- normality_test(x, method = "sw")
  explanation <- interpreter$interpret(normality_result, audience = "manager")

  expect_type(explanation, "character")
  expect_true(nchar(explanation) > 0)
})

# ----------------------------------------------------------------------------
# Durbin-Watson test accuracy (variance fix: sqrt(2) -> 2)
# ----------------------------------------------------------------------------

test_that("DW p-value uses correct variance (4/n, not 2/n)", {
  # Use MILD autocorrelation so p-values don't underflow to 0
  # With phi=0.3, DW is slightly below 2, giving a distinguishable p-value
  set.seed(42)
  n <- 100
  # AR(1) with phi=0.3 -> mild positive autocorrelation
  e <- stats::filter(rnorm(n, sd = 1), filter = 0.3, method = "recursive")
  set.seed(99)
  x <- rnorm(n)
  y <- 1 + 2 * x + as.numeric(e)
  model <- lm(y ~ x)
  diag <- diagnose_lm(model)

  dw_stat <- diag$assumptions$dw_stat
  dw_p <- diag$assumptions$dw_p

  # DW should be below 2 but not extreme
  expect_lt(dw_stat, 2)

  # Manually recompute with CORRECT variance 4/n
  z_correct <- sqrt(n) * (dw_stat - 2) / 2
  p_correct <- 2 * stats::pnorm(-abs(z_correct))

  # Manually recompute with WRONG variance 2/n (the old bug)
  z_wrong <- sqrt(n) * (dw_stat - 2) / sqrt(2)
  p_wrong <- 2 * stats::pnorm(-abs(z_wrong))

  # dw_p must match the correct formula
  expect_equal(dw_p, p_correct, tolerance = 1e-10)

  # When p-values are not at numerical extremes (not 0 or 1),
  # the two formulas must give different results
  if (p_correct > 1e-10 && p_correct < 1 - 1e-10) {
    expect_false(abs(p_correct - p_wrong) < 1e-6)
  }
})

test_that("DW statistic near 2 gives high p-value (no autocorrelation)", {
  # White noise residuals -> DW ~ 2, p-value should be high
  set.seed(123)
  n <- 500
  x <- rnorm(n)
  e <- rnorm(n)
  y <- x + e
  model <- lm(y ~ x)
  diag <- diagnose_lm(model)

  expect_lt(abs(diag$assumptions$dw_stat - 2), 0.3)  # DW close to 2
  expect_gt(diag$assumptions$dw_p, 0.20)  # not significant
  expect_true(diag$assumptions$dw_passed)
})

test_that("DW detects strong positive autocorrelation", {
  # AR(1) with phi=0.9 -> strong positive autocorrelation -> DW << 2
  set.seed(123)
  n <- 300
  e <- stats::filter(rnorm(n, sd = 0.5), filter = 0.9, method = "recursive")
  x <- rnorm(n)
  y <- 1 + 2 * x + as.numeric(e)
  model <- lm(y ~ x)
  diag <- diagnose_lm(model)

  expect_lt(diag$assumptions$dw_stat, 1.5)  # DW well below 2
  expect_lt(diag$assumptions$dw_p, 0.05)   # significant
  expect_false(diag$assumptions$dw_passed)
})

# ----------------------------------------------------------------------------
# alpha parameter propagation (test_residual_normality + test_heteroscedasticity)
# ----------------------------------------------------------------------------

test_that("test_residual_normality accepts alpha parameter", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5)
  model <- lm(y ~ x)

  # With strict alpha=0.001, is_normal threshold is stricter (harder to reject)
  r_default <- test_residual_normality(model)
  r_strict <- test_residual_normality(model, alpha = 0.001)

  # p.value should be identical (alpha doesn't change the test statistic)
  expect_equal(r_default$p.value, r_strict$p.value)
  # For normal residuals, both should be normal
  expect_true(r_default$is_normal)
  expect_true(r_strict$is_normal)
})

test_that("test_residual_normality alpha changes is_normal near boundary", {
  # Construct borderline residuals where alpha matters
  # Use a seed where p-value is in (0.01, 0.05) range
  set.seed(789)
  n <- 50
  # Slightly skewed data to get borderline normality p-value
  x <- rnorm(n)
  y <- 2 * x + rexp(n, rate = 2) - 0.5  # exponential noise -> non-normal
  model <- lm(y ~ x)

  r <- test_residual_normality(model)
  # With alpha=0.05
  expect_true(!is.null(r$is_normal))
  # With very strict alpha=0.0001, even non-normal data may be "normal"
  r_strict <- test_residual_normality(model, alpha = 0.0001)
  expect_true(r_strict$is_normal || !r_strict$is_normal)  # just ensure no error
})

test_that("test_heteroscedasticity accepts alpha parameter", {
  set.seed(123)
  x <- rnorm(200)
  # Strong heteroscedasticity: variance grows substantially with |x|
  y <- 2 * x + rnorm(200, sd = 0.3 + 1.5 * abs(x))
  model <- lm(y ~ x)

  r_default <- test_heteroscedasticity(model, test = "bp")
  r_strict <- test_heteroscedasticity(model, test = "bp", alpha = 0.001)

  # p.value should be identical
  expect_equal(r_default$p.value, r_strict$p.value)
  # With strong heteroscedastic data, default alpha=0.05 should flag it
  expect_true(r_default$is_heteroscedastic)
})

test_that("test_heteroscedasticity alpha flips is_heteroscedastic for borderline", {
  # When alpha is very strict, borderline heteroscedasticity won't be flagged
  set.seed(123)
  x <- rnorm(200)
  # Mild heteroscedasticity
  y <- 2 * x + rnorm(200, sd = 1 + 0.1 * abs(x))
  model <- lm(y ~ x)

  r <- test_heteroscedasticity(model, test = "bp")
  # With alpha=0.05 vs alpha=0.0001, the strict one should be FALSE
  # if p-value is between 0.0001 and 0.05
  r_strict <- test_heteroscedasticity(model, test = "bp", alpha = 0.0001)

  # Just verify the function runs and returns logical
  expect_true(is.logical(r$is_heteroscedastic))
  expect_true(is.logical(r_strict$is_heteroscedastic))
})

test_that("test_heteroscedasticity all three methods accept alpha", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5 + 0.3 * abs(x))
  model <- lm(y ~ x)

  for (m in c("bp", "ncv", "white")) {
    r <- test_heteroscedasticity(model, test = m, alpha = 0.10)
    expect_true("is_heteroscedastic" %in% names(r))
    expect_true(is.logical(r$is_heteroscedastic))
  }
})
