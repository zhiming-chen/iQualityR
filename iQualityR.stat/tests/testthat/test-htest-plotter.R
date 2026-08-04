# =============================================================================
# File: tests/testthat/test-htest-plotter.R
# Description: Unit tests for the HTestPlotter R6 class (R/HTestPlotter.R).
#   Covers initialize / set_theme, plot() auto dispatch, plot_curve / plot_box /
#   plot_combined for every supported test_type, show_table, theme_obj
#   overrides, fallback / warning branches, and the .compute_critical helper.
#   Plotting tests skip when iQualityR.plot is absent. Tests already covered
#   in test-htest.R (basic init, Contract 2 signature, bad plot_type, box error
#   on prop_test) are not duplicated here.
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# Shared fixtures -- real stat_result objects from HTestAnalyzer
# ----------------------------------------------------------------------------

make_t1s_result <- function() {
  set.seed(123)
  HTestAnalyzer$new()$t_test_1s(rnorm(30, mean = 5, sd = 1), mu = 5)
}

make_t2s_result <- function() {
  set.seed(123)
  HTestAnalyzer$new()$t_test_2s(rnorm(30, mean = 5, sd = 1),
                                rnorm(30, mean = 5.5, sd = 1))
}

make_tpaired_result <- function() {
  set.seed(123)
  x <- rnorm(20, mean = 10, sd = 2)
  y <- x + rnorm(20, mean = 0.5, sd = 0.5)
  HTestAnalyzer$new()$t_test_paired(x, y)
}

make_z1s_result <- function() {
  set.seed(123)
  HTestAnalyzer$new()$z_test_1s(rnorm(30, mean = 100, sd = 5),
                                mu = 100, sigma = 5)
}

make_z1s_summary_result <- function() {
  # Summary-only Z-test -> data$x is NULL (exercises NULL-x branches).
  HTestAnalyzer$new()$z_test_1s(list(mean = 102, n = 30, sd = 5),
                                mu = 100, sigma = 5)
}

make_prop1s_result <- function() {
  HTestAnalyzer$new()$prop_test_1s(x = 45, n = 100, p0 = 0.5)
}

make_prop2s_result <- function() {
  HTestAnalyzer$new()$prop_test_2s(x1 = 30, n1 = 100, x2 = 45, n2 = 100)
}

make_ftest_result <- function() {
  set.seed(123)
  HTestAnalyzer$new()$f_test(rnorm(20, sd = 2), rnorm(20, sd = 3))
}

make_chisq_gof_result <- function() {
  # Goodness-of-fit: vector input -> observed/expected populated.
  HTestAnalyzer$new()$chisq_test(c(20, 30, 50))
}

make_chisq_table_result <- function() {
  # Contingency table: matrix input.
  HTestAnalyzer$new()$chisq_test(matrix(c(10, 20, 30, 40), nrow = 2))
}

# A chi-square result with observed/expected stripped, to exercise the
# text-panel fallback branch in .chisq_curve.
make_chisq_no_counts_result <- function() {
  res <- make_chisq_gof_result()
  res$observed <- NULL
  res$expected <- NULL
  res
}

# ----------------------------------------------------------------------------
# initialize / set_theme -- additional theme coverage
# ----------------------------------------------------------------------------

test_that("HTestPlotter accepts a custom IqrTheme object", {
  skip_if_not_installed("iQualityR.core")
  th <- iQualityR.core::IqrTheme$new("academic")
  plotter <- HTestPlotter$new(theme = th)
  expect_identical(plotter$theme_obj, th)
})

test_that("HTestPlotter falls back to NULL theme for unknown theme name", {
  plotter <- HTestPlotter$new(theme = "totally_unknown_theme_xyz")
  expect_null(plotter$theme_obj)
})

test_that("HTestPlotter accepts NULL theme", {
  plotter <- HTestPlotter$new(theme = NULL)
  expect_null(plotter$theme_obj)
})

test_that("HTestPlotter$set_theme replaces the active theme (chaining)", {
  skip_if_not_installed("iQualityR.core")
  plotter <- HTestPlotter$new(theme = NULL)
  th <- iQualityR.core::IqrTheme$new("academic")
  result <- plotter$set_theme(th)
  # Returns invisible self.
  expect_true(inherits(result, "HTestPlotter"))
  expect_identical(plotter$theme_obj, th)
})

test_that("HTestPlotter$set_theme accepts a theme name string", {
  plotter <- HTestPlotter$new(theme = NULL)
  plotter$set_theme("academic")
  expect_false(is.null(plotter$theme_obj))
  expect_true(inherits(plotter$theme_obj, "IqrTheme"))
})

test_that("HTestPlotter$set_theme handles invalid theme gracefully", {
  plotter <- HTestPlotter$new(theme = "academic")
  plotter$set_theme("totally_unknown_theme_xyz")
  expect_null(plotter$theme_obj)
})

# ----------------------------------------------------------------------------
# plot() -- auto dispatch for every test_type
# ----------------------------------------------------------------------------

test_that("HTestPlotter$plot auto-dispatches t_test_1s to combined", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  p <- plotter$plot(make_t1s_result(), plot_type = "auto")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot auto-dispatches t_test_2s to combined", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  p <- plotter$plot(make_t2s_result(), plot_type = "auto")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot auto-dispatches t_test_paired to combined", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  p <- plotter$plot(make_tpaired_result(), plot_type = "auto")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot auto-dispatches z_test_1s to combined", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  p <- plotter$plot(make_z1s_result(), plot_type = "auto")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot auto-dispatches z_test_1s summary-only to curve", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  p <- plotter$plot(make_z1s_summary_result(), plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot auto-dispatches prop_test_1s to curve", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  p <- plotter$plot(make_prop1s_result(), plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot auto-dispatches prop_test_2s to curve", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  p <- plotter$plot(make_prop2s_result(), plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot auto-dispatches f_test to combined", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  p <- plotter$plot(make_ftest_result(), plot_type = "auto")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot auto-dispatches chisq goodness-of-fit to curve", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  p <- plotter$plot(make_chisq_gof_result(), plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot auto-dispatches chisq contingency table to curve", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  p <- plotter$plot(make_chisq_table_result(), plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# plot_curve -- explicit dispatch for every test_type
# ----------------------------------------------------------------------------

test_that("HTestPlotter$plot_curve renders t_test_1s", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_curve(make_t1s_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_curve renders t_test_2s", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_curve(make_t2s_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_curve renders t_test_paired", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_curve(make_tpaired_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_curve renders z_test_1s (norm dist)", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_curve(make_z1s_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_curve renders prop_test_1s", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_curve(make_prop1s_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_curve renders prop_test_2s", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_curve(make_prop2s_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_curve renders f_test (F-distribution curve)", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_curve(make_ftest_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_curve renders chisq goodness-of-fit", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_curve(make_chisq_gof_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_curve renders chisq contingency table", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_curve(make_chisq_table_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_curve renders chisq fallback when counts are NULL", {
  skip_if_not_installed("iQualityR.plot")
  # observed/expected stripped -> text-panel fallback in .chisq_curve.
  p <- HTestPlotter$new()$plot_curve(make_chisq_no_counts_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_curve forwards theme_obj override", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("iQualityR.core")
  th <- iQualityR.core::IqrTheme$new("academic")
  p <- HTestPlotter$new(theme = NULL)$plot_curve(make_t1s_result(), theme_obj = th)
  expect_true(inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# plot_box -- explicit dispatch for applicable test_types
# ----------------------------------------------------------------------------

test_that("HTestPlotter$plot_box renders t_test_1s", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_box(make_t1s_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_box renders z_test_1s", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_box(make_z1s_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_box renders t_test_paired (paired before/after)", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_box(make_tpaired_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_box renders t_test_2s (two-group box)", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_box(make_t2s_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_box renders f_test (two-group box)", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_box(make_ftest_result())
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_box forwards show_table = TRUE", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_box(make_t1s_result(), show_table = TRUE)
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_box forwards theme_obj override", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("iQualityR.core")
  th <- iQualityR.core::IqrTheme$new("academic")
  p <- HTestPlotter$new(theme = NULL)$plot_box(make_t1s_result(), theme_obj = th)
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_box errors for prop_test_1s (no raw data)", {
  skip_if_not_installed("iQualityR.plot")
  expect_error(HTestPlotter$new()$plot_box(make_prop1s_result()), "not available")
})

test_that("HTestPlotter$plot_box errors for chisq_test", {
  skip_if_not_installed("iQualityR.plot")
  expect_error(HTestPlotter$new()$plot_box(make_chisq_gof_result()), "not available")
})

# ----------------------------------------------------------------------------
# plot_combined
# ----------------------------------------------------------------------------

test_that("HTestPlotter$plot_combined returns patchwork for t_test_1s", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_combined(make_t1s_result())
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_combined returns patchwork for t_test_2s", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_combined(make_t2s_result())
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_combined returns patchwork for f_test", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot_combined(make_ftest_result())
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_combined forwards theme_obj override", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("iQualityR.core")
  th <- iQualityR.core::IqrTheme$new("academic")
  p <- HTestPlotter$new(theme = NULL)$plot_combined(make_t1s_result(), theme_obj = th)
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_combined falls back to curve when box fails", {
  skip_if_not_installed("iQualityR.plot")
  # prop_test has no raw x data -> plot_box fails -> combined returns curve only.
  p <- HTestPlotter$new()$plot_combined(make_prop1s_result())
  expect_true(inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# plot() -- explicit plot_type dispatch and fallback warnings
# ----------------------------------------------------------------------------

test_that("HTestPlotter$plot explicit plot_type='curve' for t_test_1s", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot(make_t1s_result(), plot_type = "curve")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot explicit plot_type='box' for t_test_1s", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot(make_t1s_result(), plot_type = "box")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot explicit plot_type='combined' for t_test_2s", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot(make_t2s_result(), plot_type = "combined")
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot show_table=TRUE forwarded to box plot", {
  skip_if_not_installed("iQualityR.plot")
  p <- HTestPlotter$new()$plot(make_t1s_result(), plot_type = "box", show_table = TRUE)
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot warns and falls back to curve for prop_test_1s box", {
  skip_if_not_installed("iQualityR.plot")
  # prop_test_1s cannot do box -> .resolve_plot_type warns and returns "curve".
  expect_warning(
    p <- HTestPlotter$new()$plot(make_prop1s_result(), plot_type = "box"),
    "not available"
  )
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot warns and falls back to curve for prop_test_2s combined", {
  skip_if_not_installed("iQualityR.plot")
  expect_warning(
    p <- HTestPlotter$new()$plot(make_prop2s_result(), plot_type = "combined"),
    "not available"
  )
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot forwards theme_obj override", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("iQualityR.core")
  th <- iQualityR.core::IqrTheme$new("academic")
  p <- HTestPlotter$new(theme = NULL)$plot(make_t1s_result(), theme_obj = th)
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter$plot_curve errors for unsupported test_type", {
  skip_if_not_installed("iQualityR.plot")
  fake <- make_t1s_result()
  fake$test_type <- "unknown_test"
  expect_error(HTestPlotter$new()$plot_curve(fake), "Unsupported test_type")
})

# ----------------------------------------------------------------------------
# .compute_critical module-scope helper
# ----------------------------------------------------------------------------

test_that(".compute_critical returns norm two-sided critical value", {
  cv <- iQualityR.stat:::.compute_critical("norm", "two.sided", 0.05)
  expect_equal(round(cv, 4), round(stats::qnorm(0.975), 4))
})

test_that(".compute_critical returns norm one-sided critical value", {
  cv <- iQualityR.stat:::.compute_critical("norm", "greater", 0.05)
  expect_equal(round(cv, 4), round(stats::qnorm(0.95), 4))
})

test_that(".compute_critical returns t two-sided critical value", {
  cv <- iQualityR.stat:::.compute_critical("t", "two.sided", 0.05, df = 10)
  expect_equal(round(cv, 4), round(stats::qt(0.975, df = 10), 4))
})

test_that(".compute_critical returns t one-sided critical value", {
  # One-sided alternatives return the upper-tail critical value qt(1 - alpha).
  cv <- iQualityR.stat:::.compute_critical("t", "less", 0.05, df = 10)
  expect_equal(round(cv, 4), round(stats::qt(0.95, df = 10), 4))
})

test_that(".compute_critical errors when df missing for t distribution", {
  expect_error(iQualityR.stat:::.compute_critical("t", "two.sided", 0.05),
               "df required")
})

test_that(".compute_critical errors for unsupported dist_type", {
  expect_error(iQualityR.stat:::.compute_critical("weibull", "two.sided", 0.05),
               "Unsupported dist_type")
})

# ----------------------------------------------------------------------------
# .require_data module-scope helper
# ----------------------------------------------------------------------------

test_that(".require_data is silent when both x and y are present", {
  expect_no_error(iQualityR.stat:::.require_data(1:3, 1:3, "t_test_2s"))
})

test_that(".require_data errors when x is NULL", {
  expect_error(iQualityR.stat:::.require_data(NULL, 1:3, "t_test_2s"),
               "requires both")
})

test_that(".require_data errors when y is NULL", {
  expect_error(iQualityR.stat:::.require_data(1:3, NULL, "t_test_2s"),
               "requires both")
})

# ----------------------------------------------------------------------------
# Integration with iqr_htest integrator (end-to-end)
# ----------------------------------------------------------------------------

test_that("HTestPlotter renders via iqr_htest$plot end-to-end (t_test_1s)", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  htest <- iqr_htest$new()
  htest$run("t_test_1s", x = x, mu = 5)
  p <- htest$plot()
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter renders via iqr_htest$plot with plot_type='curve'", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  htest <- iqr_htest$new()
  htest$run("t_test_1s", x = x, mu = 5)
  p <- htest$plot(plot_type = "curve")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter renders prop_test_1s via iqr_htest end-to-end", {
  skip_if_not_installed("iQualityR.plot")
  htest <- iqr_htest$new()
  htest$run("prop_test_1s", x = 45, n = 100, p0 = 0.5)
  p <- htest$plot()
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter renders f_test via iqr_htest end-to-end", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  htest <- iqr_htest$new()
  htest$run("f_test", x = rnorm(20, sd = 2), y = rnorm(20, sd = 3))
  p <- htest$plot()
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("HTestPlotter renders chisq_test via iqr_htest end-to-end", {
  skip_if_not_installed("iQualityR.plot")
  htest <- iqr_htest$new()
  htest$run("chisq_test", x = c(20, 30, 50))
  p <- htest$plot()
  expect_true(inherits(p, "ggplot"))
})
