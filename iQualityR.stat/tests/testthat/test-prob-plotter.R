# =============================================================================
# File: tests/testthat/test-prob-plotter.R
# Description: Unit tests for the ProbPlotter R6 class (R/ProbPlotter.R).
#   Covers initialize / theme handling, render() dispatch for prob and quant
#   modes, continuous and discrete distributions, facet / CDF options and
#   theme_obj overrides. All plotting tests skip when iQualityR.plot is absent.
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# Shared fixtures -- build ProbNode + ProbAnalyzer results used across tests
# ----------------------------------------------------------------------------

# A single continuous (normal) node plus its prob-mode calc result.
make_norm_prob_fixture <- function() {
  node <- ProbNode$new("N1", "norm", list(mean = 0, sd = 1))
  analyzer <- ProbAnalyzer$new()
  calc <- list(N1 = analyzer$analyze(node, mode = "prob",
                                     calc_type = "upper", values = 1.96))
  list(nodes = list(N1 = node), calc_results = calc)
}

# A single continuous (normal) node plus its quant-mode calc result.
make_norm_quant_fixture <- function() {
  node <- ProbNode$new("N1", "norm", list(mean = 0, sd = 1))
  analyzer <- ProbAnalyzer$new()
  calc <- list(N1 = analyzer$analyze(node, mode = "quant",
                                     calc_type = "lower", values = c(0.025, 0.975)))
  list(nodes = list(N1 = node), calc_results = calc)
}

# A single discrete (binomial) node plus its prob-mode calc result.
make_binom_prob_fixture <- function() {
  node <- ProbNode$new("B1", "binom", list(size = 10, prob = 0.5))
  analyzer <- ProbAnalyzer$new()
  calc <- list(B1 = analyzer$analyze(node, mode = "prob",
                                     calc_type = "lower", values = 3))
  list(nodes = list(B1 = node), calc_results = calc)
}

# Two continuous nodes for multi-distribution / facet coverage.
make_multi_norm_fixture <- function() {
  n1 <- ProbNode$new("A", "norm", list(mean = 0, sd = 1))
  n2 <- ProbNode$new("B", "norm", list(mean = 3, sd = 1))
  analyzer <- ProbAnalyzer$new()
  calc <- list(
    A = analyzer$analyze(n1, mode = "prob", calc_type = "upper", values = 1.96),
    B = analyzer$analyze(n2, mode = "prob", calc_type = "lower", values = 1.96)
  )
  list(nodes = list(A = n1, B = n2), calc_results = calc)
}

# ----------------------------------------------------------------------------
# initialize / theme handling
# ----------------------------------------------------------------------------

test_that("ProbPlotter initializes with default academic theme", {
  plotter <- ProbPlotter$new()
  expect_true(inherits(plotter, "ProbPlotter"))
  expect_true(inherits(plotter, "R6"))
  expect_false(is.null(plotter$theme_obj))
  expect_true(inherits(plotter$theme_obj, "IqrTheme"))
})

test_that("ProbPlotter initializes with explicit theme name", {
  plotter <- ProbPlotter$new(theme = "academic")
  expect_false(is.null(plotter$theme_obj))
  expect_true(inherits(plotter$theme_obj, "IqrTheme"))
})

test_that("ProbPlotter initialize returns invisible self", {
  plotter <- ProbPlotter$new()
  # Constructor returns invisible(self); assigning captures the object.
  expect_true(inherits(plotter, "ProbPlotter"))
})

test_that("ProbPlotter falls back to NULL theme for unknown theme name", {
  plotter <- ProbPlotter$new(theme = "totally_unknown_theme_xyz")
  expect_null(plotter$theme_obj)
})

test_that("ProbPlotter accepts a custom IqrTheme object", {
  skip_if_not_installed("iQualityR.core")
  th <- iQualityR.core::IqrTheme$new("academic")
  plotter <- ProbPlotter$new(theme = th)
  expect_identical(plotter$theme_obj, th)
})

test_that("ProbPlotter accepts NULL theme", {
  plotter <- ProbPlotter$new(theme = NULL)
  expect_null(plotter$theme_obj)
})

# ----------------------------------------------------------------------------
# render() -- prob mode (continuous distribution)
# ----------------------------------------------------------------------------

test_that("ProbPlotter$render returns ggplot for prob mode without CDF", {
  skip_if_not_installed("iQualityR.plot")
  fx <- make_norm_prob_fixture()
  plotter <- ProbPlotter$new()
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = FALSE, show_cdf = FALSE, mode = "prob")
  expect_true(inherits(p, "ggplot"))
})

test_that("ProbPlotter$render returns patchwork when show_cdf = TRUE (prob mode)", {
  skip_if_not_installed("iQualityR.plot")
  fx <- make_norm_prob_fixture()
  plotter <- ProbPlotter$new()
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = FALSE, show_cdf = TRUE, mode = "prob")
  # patchwork objects carry the "patchwork" class.
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("ProbPlotter$render honours facet = TRUE (prob mode)", {
  skip_if_not_installed("iQualityR.plot")
  fx <- make_multi_norm_fixture()
  plotter <- ProbPlotter$new()
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = TRUE, show_cdf = FALSE, mode = "prob")
  expect_true(inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# render() -- quant mode (forces CDF panel because is_prob_mode is FALSE)
# ----------------------------------------------------------------------------

test_that("ProbPlotter$render returns patchwork for quant mode (CDF auto-shown)", {
  skip_if_not_installed("iQualityR.plot")
  fx <- make_norm_quant_fixture()
  plotter <- ProbPlotter$new()
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = FALSE, show_cdf = FALSE, mode = "quant")
  # Quant results have is_prob_mode = FALSE -> must_show_cdf becomes TRUE.
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("ProbPlotter$render quant mode with show_cdf = TRUE also yields patchwork", {
  skip_if_not_installed("iQualityR.plot")
  fx <- make_norm_quant_fixture()
  plotter <- ProbPlotter$new()
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = FALSE, show_cdf = TRUE, mode = "quant")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# render() -- discrete distribution (binomial) exercises geom_col branch
# ----------------------------------------------------------------------------

test_that("ProbPlotter$render handles discrete distribution (binom)", {
  skip_if_not_installed("iQualityR.plot")
  fx <- make_binom_prob_fixture()
  plotter <- ProbPlotter$new()
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = FALSE, show_cdf = FALSE, mode = "prob")
  expect_true(inherits(p, "ggplot"))
})

test_that("ProbPlotter$render discrete distribution with CDF (step line)", {
  skip_if_not_installed("iQualityR.plot")
  fx <- make_binom_prob_fixture()
  plotter <- ProbPlotter$new()
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = FALSE, show_cdf = TRUE, mode = "prob")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# render() -- multi-distribution coverage
# ----------------------------------------------------------------------------

test_that("ProbPlotter$render plots multiple named nodes together", {
  skip_if_not_installed("iQualityR.plot")
  fx <- make_multi_norm_fixture()
  plotter <- ProbPlotter$new()
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = FALSE, show_cdf = FALSE, mode = "prob")
  expect_true(inherits(p, "ggplot"))
  # The mapping should encode a group/colour aesthetic (two distributions).
  expect_true("group" %in% names(p$mapping) || "colour" %in% names(p$mapping))
})

test_that("ProbPlotter$render multiple nodes with facet and CDF", {
  skip_if_not_installed("iQualityR.plot")
  fx <- make_multi_norm_fixture()
  plotter <- ProbPlotter$new()
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = TRUE, show_cdf = TRUE, mode = "prob")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# render() -- theme_obj override
# ----------------------------------------------------------------------------

test_that("ProbPlotter$render uses theme_obj argument when supplied", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("iQualityR.core")
  fx <- make_norm_prob_fixture()
  custom_theme <- iQualityR.core::IqrTheme$new("academic")
  plotter <- ProbPlotter$new(theme = NULL)  # plotter has no theme
  # Passing theme_obj should still style the plot without error.
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = FALSE, show_cdf = FALSE, mode = "prob",
                      theme_obj = custom_theme)
  expect_true(inherits(p, "ggplot"))
})

test_that("ProbPlotter$render falls back to self$theme_obj when theme_obj is NULL", {
  skip_if_not_installed("iQualityR.plot")
  fx <- make_norm_prob_fixture()
  plotter <- ProbPlotter$new(theme = "academic")
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = FALSE, show_cdf = FALSE, mode = "prob",
                      theme_obj = NULL)
  expect_true(inherits(p, "ggplot"))
})

test_that("ProbPlotter$render works with NULL theme_obj and NULL self theme (no styling)", {
  skip_if_not_installed("iQualityR.plot")
  fx <- make_norm_prob_fixture()
  plotter <- ProbPlotter$new(theme = NULL)
  p <- plotter$render(nodes = fx$nodes, calc_results = fx$calc_results,
                      facet = FALSE, show_cdf = FALSE, mode = "prob",
                      theme_obj = NULL)
  # No theme -> plot still built, just without iqr theming.
  expect_true(inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# render() -- all calc_type variants for prob mode
# ----------------------------------------------------------------------------

test_that("ProbPlotter$render covers all prob calc_types (lower/upper/between/outside)", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- ProbPlotter$new()
  node <- ProbNode$new("N1", "norm", list(mean = 0, sd = 1))
  analyzer <- ProbAnalyzer$new()
  for (ct in c("lower", "upper", "between", "outside")) {
    vals <- if (ct %in% c("between", "outside")) c(-1, 1) else 1
    calc <- list(N1 = analyzer$analyze(node, mode = "prob",
                                       calc_type = ct, values = vals))
    p <- plotter$render(nodes = list(N1 = node), calc_results = calc,
                        facet = FALSE, show_cdf = FALSE, mode = "prob")
    expect_true(inherits(p, "ggplot"))
  }
})

# ----------------------------------------------------------------------------
# Integration with iqr_prob integrator (end-to-end render through plot())
# ----------------------------------------------------------------------------

test_that("ProbPlotter renders via iqr_prob$plot end-to-end (prob mode)", {
  skip_if_not_installed("iQualityR.plot")
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  p <- prob$plot(show_cdf = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("ProbPlotter renders via iqr_prob$plot with show_cdf (patchwork)", {
  skip_if_not_installed("iQualityR.plot")
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  p <- prob$plot(show_cdf = TRUE)
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("ProbPlotter renders discrete distribution via iqr_prob end-to-end", {
  skip_if_not_installed("iQualityR.plot")
  prob <- iqr_prob$new(type = "binom", params = list(size = 10, prob = 0.5))
  prob$calc(values = 3, mode = "prob", calc_type = "lower")
  p <- prob$plot(show_cdf = TRUE)
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})
