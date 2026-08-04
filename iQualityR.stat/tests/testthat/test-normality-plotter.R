# =============================================================================
# File: tests/testthat/test-normality-plotter.R
# Description: Unit tests for the NormalityPlotter R6 class
#   (R/NormalityPlotter.R). Covers initialize / theme handling, plot() auto
#   dispatch, explicit plot_type calls (hist/qq/pp/combined), add_confidence,
#   layout variants, theme_obj overrides and error paths. All plotting tests
#   skip when iQualityR.plot is absent.
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# Shared fixtures
# ----------------------------------------------------------------------------

# A normal sample large enough for histogram binning and QQ/PP plots.
make_normal_sample <- function(n = 100, seed = 123) {
  set.seed(seed)
  rnorm(n)
}

# A non-normal (exponential) sample used to exercise result-aware dispatch.
make_nonnormal_sample <- function(n = 200, seed = 123) {
  set.seed(seed)
  rexp(n, rate = 1)
}

# A NormalityAnalyzer result for auto-select coverage.
make_normality_result <- function(x = make_normal_sample()) {
  NormalityAnalyzer$new()$test(x, method = "sw")
}

# ----------------------------------------------------------------------------
# initialize / theme handling
# ----------------------------------------------------------------------------

test_that("NormalityPlotter initializes with default academic theme", {
  plotter <- NormalityPlotter$new()
  expect_true(inherits(plotter, "NormalityPlotter"))
  expect_true(inherits(plotter, "R6"))
  expect_false(is.null(plotter$theme_obj))
  expect_true(inherits(plotter$theme_obj, "IqrTheme"))
})

test_that("NormalityPlotter initializes with explicit theme name", {
  plotter <- NormalityPlotter$new(theme = "academic")
  expect_false(is.null(plotter$theme_obj))
  expect_true(inherits(plotter$theme_obj, "IqrTheme"))
})

test_that("NormalityPlotter falls back to NULL theme for unknown theme name", {
  plotter <- NormalityPlotter$new(theme = "totally_unknown_theme_xyz")
  expect_null(plotter$theme_obj)
})

test_that("NormalityPlotter accepts a custom IqrTheme object", {
  skip_if_not_installed("iQualityR.core")
  th <- iQualityR.core::IqrTheme$new("academic")
  plotter <- NormalityPlotter$new(theme = th)
  expect_identical(plotter$theme_obj, th)
})

# ----------------------------------------------------------------------------
# plot() -- auto dispatch
# ----------------------------------------------------------------------------

test_that("NormalityPlotter$plot auto-selects 'hist' when result is NULL", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot(result = NULL, x = x, plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot auto-selects 'combined' when result is provided", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  result <- make_normality_result(x)
  p <- plotter$plot(result = result, x = x, plot_type = "auto")
  # combined returns a patchwork object.
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot default plot_type is 'auto'", {
  f <- NormalityPlotter$new()$plot
  fm <- formals(f)
  expect_equal(fm$plot_type, "auto")
})

# ----------------------------------------------------------------------------
# plot() -- explicit plot_type dispatch
# ----------------------------------------------------------------------------

test_that("NormalityPlotter$plot explicit plot_type='hist' returns ggplot", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot(x, plot_type = "hist")
  expect_true(inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot explicit plot_type='qq' returns ggplot", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot(x, plot_type = "qq")
  expect_true(inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot explicit plot_type='pp' returns ggplot", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot(x, plot_type = "pp")
  expect_true(inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot explicit plot_type='combined' returns patchwork", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot(x, plot_type = "combined")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot rejects invalid plot_type", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  expect_error(plotter$plot(x, plot_type = "nope"), "arg")
})

# ----------------------------------------------------------------------------
# plot_hist -- direct method coverage
# ----------------------------------------------------------------------------

test_that("NormalityPlotter$plot_hist returns ggplot with normal curve overlay", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot_hist(x)
  expect_true(inherits(p, "ggplot"))
  # Title should reflect histogram with normal curve.
  expect_true(grepl("Histogram", p$labels$title))
})

test_that("NormalityPlotter$plot_hist accepts custom bins argument", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot_hist(x, bins = 15)
  expect_true(inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot_hist auto-computes bins via Sturges-like rule", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample(n = 50)
  expect_no_error(plotter$plot_hist(x))
})

test_that("NormalityPlotter$plot_hist filters NA values before plotting", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- c(make_normal_sample(n = 50), NA, NA)
  p <- plotter$plot_hist(x)
  expect_true(inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot_hist errors when n < 3 non-missing values", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  expect_error(plotter$plot_hist(c(1, 2)), "at least 3")
  expect_error(plotter$plot_hist(c(1, NA, NA)), "at least 3")
})

# ----------------------------------------------------------------------------
# plot_qq -- direct method coverage
# ----------------------------------------------------------------------------

test_that("NormalityPlotter$plot_qq returns ggplot", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot_qq(x)
  expect_true(inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot_qq supports add_confidence = TRUE", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  expect_no_error(plotter$plot_qq(x, add_confidence = TRUE))
})

test_that("NormalityPlotter$plot_qq default add_confidence is FALSE", {
  f <- NormalityPlotter$new()$plot_qq
  fm <- formals(f)
  expect_equal(fm$add_confidence, FALSE)
})

# ----------------------------------------------------------------------------
# plot_pp -- direct method coverage
# ----------------------------------------------------------------------------

test_that("NormalityPlotter$plot_pp returns ggplot", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot_pp(x)
  expect_true(inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# plot_combined -- layout variants
# ----------------------------------------------------------------------------

test_that("NormalityPlotter$plot_combined default layout (1x3) returns patchwork", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot_combined(x)
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot_combined layout='1x3' returns patchwork", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot_combined(x, layout = "1x3")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot_combined layout='2x2' returns patchwork", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot_combined(x, layout = "2x2")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot_combined unknown layout falls back to 1x3", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  p <- plotter$plot_combined(x, layout = "weird")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot_combined forwards add_confidence to qq sub-plot", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_normal_sample()
  expect_no_error(plotter$plot_combined(x, add_confidence = TRUE))
})

# ----------------------------------------------------------------------------
# plot() -- theme_obj override
# ----------------------------------------------------------------------------

test_that("NormalityPlotter$plot overrides self$theme_obj via theme_obj argument", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("iQualityR.core")
  plotter <- NormalityPlotter$new(theme = NULL)
  custom_theme <- iQualityR.core::IqrTheme$new("academic")
  x <- make_normal_sample()
  p <- plotter$plot(x, plot_type = "hist", theme_obj = custom_theme)
  expect_true(inherits(p, "ggplot"))
  # theme_obj should be assigned back to self$theme_obj.
  expect_identical(plotter$theme_obj, custom_theme)
})

# ----------------------------------------------------------------------------
# plot() -- non-normal data (exercises the same code paths with skew)
# ----------------------------------------------------------------------------

test_that("NormalityPlotter$plot works on non-normal (exponential) data", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_nonnormal_sample()
  p <- plotter$plot(x, plot_type = "hist")
  expect_true(inherits(p, "ggplot"))
})

test_that("NormalityPlotter$plot combined on non-normal data with result", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- NormalityPlotter$new()
  x <- make_nonnormal_sample()
  result <- make_normality_result(x)
  p <- plotter$plot(result = result, x = x, plot_type = "auto")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# Integration with iqr_normality integrator (end-to-end)
# ----------------------------------------------------------------------------

test_that("NormalityPlotter renders via iqr_normality$plot end-to-end", {
  skip_if_not_installed("iQualityR.plot")
  normality <- iqr_normality$new()
  x <- make_normal_sample()
  normality$test(x, method = "sw")
  p <- normality$plot()
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("NormalityPlotter plot via iqr_normality accepts plot_type='qq'", {
  skip_if_not_installed("iQualityR.plot")
  normality <- iqr_normality$new()
  x <- make_normal_sample()
  normality$test(x, method = "sw")
  p <- normality$plot(plot_type = "qq")
  expect_true(inherits(p, "ggplot"))
})
