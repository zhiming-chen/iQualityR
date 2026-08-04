# =============================================================================
# File: tests/testthat/test-intervals.R
# Description: Interval estimation module tests (R3-A6)
#   - IntervalAnalyzer (all 7 interval types)
#   - IntervalPlotter (Contract 2 signature)
#   - IntervalReporter (Contract 2 signature)
#   - iqr_intervals L3 integrator
#   - Convenience functions (intervals_run/plot/interpret/report)
#   - StatInterpreter interval_result dispatch
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# IntervalAnalyzer: ci_mean
# ----------------------------------------------------------------------------

test_that("IntervalAnalyzer ci_mean (t-interval) returns stat_result", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$ci_mean(x, conf_level = 0.95)
  expect_s3_class(result, "stat_result")
  expect_s3_class(result, "interval_result")
  expect_equal(result$domain, "interval")
  expect_equal(result$test_type, "ci_mean")
  expect_equal(result$dist_type, "t")
  expect_false(result$sigma_known)
  expect_length(result$conf.int, 2)
  expect_true(result$conf.int[1] < result$conf.int[2])
  expect_equal(result$n, 30L)
  expect_equal(result$conf.level, 0.95)
  # CI should bracket the sample mean
  expect_true(result$conf.int[1] < result$statistic[1])
  expect_true(result$conf.int[2] > result$statistic[1])
})

test_that("IntervalAnalyzer ci_mean (z-interval) when sigma supplied", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$ci_mean(x, sigma = 5, conf_level = 0.95)
  expect_equal(result$dist_type, "norm")
  expect_true(result$sigma_known)
  expect_null(result$parameter)
})

test_that("IntervalAnalyzer ci_mean one-sided alternatives", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  res_less <- analyzer$ci_mean(x, alternative = "less")
  res_greater <- analyzer$ci_mean(x, alternative = "greater")
  expect_equal(res_less$alternative, "less")
  expect_equal(res_greater$alternative, "greater")
  expect_true(is.infinite(res_less$conf.int[1]))
  expect_true(is.infinite(res_greater$conf.int[2]))
})

test_that("IntervalAnalyzer ci_mean errors on n < 2", {
  analyzer <- IntervalAnalyzer$new()
  expect_error(analyzer$ci_mean(c(1)), "need at least 2")
})

test_that("IntervalAnalyzer ci_mean matches stats::t.test", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  ref <- t.test(x, conf.level = 0.95)
  result <- analyzer$ci_mean(x, conf_level = 0.95)
  expect_equal(as.numeric(result$conf.int), as.numeric(ref$conf.int),
               tolerance = 1e-6)
})

# ----------------------------------------------------------------------------
# IntervalAnalyzer: ci_proportion
# ----------------------------------------------------------------------------

test_that("IntervalAnalyzer ci_proportion (wald) returns stat_result", {
  analyzer <- IntervalAnalyzer$new()
  result <- analyzer$ci_proportion(x = 12, n = 200, method = "wald")
  expect_s3_class(result, "stat_result")
  expect_s3_class(result, "interval_result")
  expect_equal(result$test_type, "ci_proportion")
  expect_equal(result$dist_type, "norm")
  expect_equal(result$x_success, 12L)
  expect_equal(result$n, 200L)
  expect_equal(as.numeric(result$estimate[1]), 12 / 200)
  expect_length(result$conf.int, 2)
  expect_true(result$conf.int[1] >= 0)
  expect_true(result$conf.int[2] <= 1)
})

test_that("IntervalAnalyzer ci_proportion (exact) via Clopper-Pearson", {
  analyzer <- IntervalAnalyzer$new()
  result <- analyzer$ci_proportion(x = 12, n = 200, method = "exact")
  expect_equal(result$dist_type, "binom")
  ref <- binom.test(12, 200)
  expect_equal(as.numeric(result$conf.int), as.numeric(ref$conf.int),
               tolerance = 1e-6)
})

test_that("IntervalAnalyzer ci_proportion accepts 0/1 vector", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rbinom(100, 1, 0.1)
  result <- analyzer$ci_proportion(x = x)
  expect_equal(result$n, 100L)
  expect_equal(result$x_success, sum(x))
})

test_that("IntervalAnalyzer ci_proportion errors on bad x/n", {
  analyzer <- IntervalAnalyzer$new()
  expect_error(analyzer$ci_proportion(x = 5), "n.*required")
  expect_error(analyzer$ci_proportion(x = -1, n = 100), "0 <= x <= n")
  expect_error(analyzer$ci_proportion(x = 50, n = 10), "0 <= x <= n")
})

# ----------------------------------------------------------------------------
# IntervalAnalyzer: ci_variance
# ----------------------------------------------------------------------------

test_that("IntervalAnalyzer ci_variance returns stat_result", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$ci_variance(x, conf_level = 0.95)
  expect_s3_class(result, "stat_result")
  expect_s3_class(result, "interval_result")
  expect_equal(result$test_type, "ci_variance")
  expect_equal(result$dist_type, "chisq")
  expect_length(result$conf.int, 2)
  expect_true(result$conf.int[1] < result$conf.int[2])
  expect_true(result$var_lower < result$var_upper)
  expect_true(result$sd_lower < result$sd_upper)
  # CI should bracket the sample variance
  expect_true(result$var_lower < result$statistic[1])
  expect_true(result$var_upper > result$statistic[1])
})

test_that("IntervalAnalyzer ci_variance errors on n < 2", {
  analyzer <- IntervalAnalyzer$new()
  expect_error(analyzer$ci_variance(c(1)), "need at least 2")
})

# ----------------------------------------------------------------------------
# IntervalAnalyzer: ci_diff_mean
# ----------------------------------------------------------------------------

test_that("IntervalAnalyzer ci_diff_mean (Welch) returns stat_result", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  y <- rnorm(30, mean = 102, sd = 5)
  result <- analyzer$ci_diff_mean(x, y, var.equal = FALSE)
  expect_s3_class(result, "interval_result")
  expect_equal(result$test_type, "ci_diff_mean")
  expect_false(result$var_equal)
  expect_equal(result$n1, 30L)
  expect_equal(result$n2, 30L)
  expect_length(result$conf.int, 2)
})

test_that("IntervalAnalyzer ci_diff_mean (pooled) matches t.test var.equal", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  y <- rnorm(30, mean = 102, sd = 5)
  ref <- t.test(x, y, var.equal = TRUE)
  result <- analyzer$ci_diff_mean(x, y, var.equal = TRUE)
  expect_equal(as.numeric(result$conf.int), as.numeric(ref$conf.int),
               tolerance = 1e-6)
  # Compare df values (strip names to avoid name-mismatch noise)
  expect_equal(as.numeric(result$parameter[1]), as.numeric(ref$parameter[1]))
})

test_that("IntervalAnalyzer ci_diff_mean errors on small samples", {
  analyzer <- IntervalAnalyzer$new()
  expect_error(analyzer$ci_diff_mean(c(1), c(2)), "at least 2")
})

# ----------------------------------------------------------------------------
# IntervalAnalyzer: tolerance_interval
# ----------------------------------------------------------------------------

test_that("IntervalAnalyzer tolerance_interval (two-sided) returns stat_result", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  result <- analyzer$tolerance_interval(x, p = 0.99, conf_level = 0.95,
                                        side = "two-sided")
  expect_s3_class(result, "interval_result")
  expect_equal(result$test_type, "tolerance_interval")
  expect_equal(result$p_content, 0.99)
  expect_equal(result$side, "two-sided")
  expect_true(result$k_factor > 0)
  expect_length(result$conf.int, 2)
  # TI should be wider than the corresponding CI for the mean
  ci <- analyzer$ci_mean(x, conf_level = 0.95)
  expect_true((result$conf.int[2] - result$conf.int[1]) >
              (ci$conf.int[2] - ci$conf.int[1]))
})

test_that("IntervalAnalyzer tolerance_interval one-sided bounds", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  res_lower <- analyzer$tolerance_interval(x, side = "lower")
  res_upper <- analyzer$tolerance_interval(x, side = "upper")
  expect_true(is.infinite(res_lower$conf.int[2]))
  expect_true(is.infinite(res_upper$conf.int[1]))
})

test_that("IntervalAnalyzer tolerance_interval errors on bad p", {
  analyzer <- IntervalAnalyzer$new()
  x <- rnorm(10)
  expect_error(analyzer$tolerance_interval(x, p = 0), "p must be in")
  expect_error(analyzer$tolerance_interval(x, p = 1), "p must be in")
})

# ----------------------------------------------------------------------------
# IntervalAnalyzer: margin_of_error
# ----------------------------------------------------------------------------

test_that("IntervalAnalyzer margin_of_error (mean, t) returns stat_result", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$margin_of_error(x, type = "mean")
  expect_s3_class(result, "interval_result")
  expect_equal(result$test_type, "margin_of_error")
  expect_equal(result$type, "mean")
  expect_equal(result$dist_type, "t")
  expect_true(result$statistic[1] > 0)
  # MOE should equal half the CI width
  expect_equal(as.numeric(result$statistic[1]),
               (result$conf.int[2] - result$conf.int[1]) / 2,
               tolerance = 1e-6)
})

test_that("IntervalAnalyzer margin_of_error (mean, z) when sigma supplied", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$margin_of_error(x, sigma = 5, type = "mean")
  expect_equal(result$dist_type, "norm")
})

test_that("IntervalAnalyzer margin_of_error (proportion) returns stat_result", {
  analyzer <- IntervalAnalyzer$new()
  result <- analyzer$margin_of_error(x = 12, n = 200, type = "proportion")
  expect_equal(result$type, "proportion")
  expect_equal(result$dist_type, "norm")
  expect_true(result$statistic[1] > 0)
})

test_that("IntervalAnalyzer margin_of_error (proportion) errors without n", {
  analyzer <- IntervalAnalyzer$new()
  expect_error(analyzer$margin_of_error(x = 12, type = "proportion"),
               "n.*required")
})

# ----------------------------------------------------------------------------
# IntervalAnalyzer: pi_mean
# ----------------------------------------------------------------------------

test_that("IntervalAnalyzer pi_mean returns stat_result", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$pi_mean(x, conf_level = 0.95)
  expect_s3_class(result, "interval_result")
  expect_equal(result$test_type, "pi_mean")
  expect_equal(result$dist_type, "t")
  expect_length(result$conf.int, 2)
  # PI should be wider than the CI for the mean
  ci <- analyzer$ci_mean(x, conf_level = 0.95)
  expect_true((result$conf.int[2] - result$conf.int[1]) >
              (ci$conf.int[2] - ci$conf.int[1]))
})

test_that("IntervalAnalyzer pi_mean one-sided alternatives", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  res_less <- analyzer$pi_mean(x, alternative = "less")
  expect_true(is.infinite(res_less$conf.int[1]))
})

# ----------------------------------------------------------------------------
# IntervalAnalyzer: analyze dispatch
# ----------------------------------------------------------------------------

test_that("IntervalAnalyzer analyze dispatches all 7 interval types", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  y <- rnorm(30, mean = 102, sd = 5)

  expect_s3_class(analyzer$analyze("ci_mean", x = x), "interval_result")
  expect_s3_class(analyzer$analyze("ci_proportion", x = 12, n = 200),
                  "interval_result")
  expect_s3_class(analyzer$analyze("ci_variance", x = x), "interval_result")
  expect_s3_class(analyzer$analyze("ci_diff_mean", x = x, y = y),
                  "interval_result")
  expect_s3_class(analyzer$analyze("tolerance_interval", x = x),
                  "interval_result")
  expect_s3_class(analyzer$analyze("margin_of_error", x = x, type = "mean"),
                  "interval_result")
  expect_s3_class(analyzer$analyze("pi_mean", x = x), "interval_result")
})

test_that("IntervalAnalyzer analyze rejects unknown type", {
  analyzer <- IntervalAnalyzer$new()
  expect_error(analyzer$analyze("nope"), "Unknown interval type")
})

# ----------------------------------------------------------------------------
# format / print for interval_result
# ----------------------------------------------------------------------------

test_that("format.stat_result works for interval_result", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$ci_mean(x)
  out <- format(result)
  expect_type(out, "character")
  expect_true(grepl("CI for mean", out))
  expect_true(grepl("confidence interval", out))
  # Footer carries domain=interval (the stat_result class tag is interval_result)
  expect_true(grepl("domain=interval", out))
})

test_that("print.stat_result returns invisibly for interval_result", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$ci_mean(x)
  out <- capture.output(invisible(print(result)))
  expect_true(any(grepl("CI for mean", out)))
})

# ----------------------------------------------------------------------------
# IntervalReporter
# ----------------------------------------------------------------------------

test_that("IntervalReporter initialization", {
  reporter <- IntervalReporter$new()
  expect_true(inherits(reporter, "IntervalReporter"))
  expect_true(inherits(reporter, "R6"))
})

test_that("IntervalReporter$report has Contract 2 signature", {
  f <- IntervalReporter$new()$report
  fm <- formals(f)
  expect_named(fm, c("result", "format", "path", "audience"))
})

test_that("IntervalReporter to_dataframe returns tidy data.frame", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$ci_mean(x)
  reporter <- IntervalReporter$new()
  df <- reporter$report(result, format = "data.frame")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
  expect_equal(df$Domain, "interval")
  expect_equal(df$Interval_Type, "ci_mean")
  expect_true(!is.na(df$Lower))
  expect_true(!is.na(df$Upper))
})

test_that("IntervalReporter console output works", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$ci_mean(x)
  reporter <- IntervalReporter$new()
  out <- capture.output(reporter$report(result, format = "console",
                                        audience = "manager"))
  expect_true(length(out) > 0L)
  expect_true(any(grepl("CI for mean|confidence interval", out)))
})

# ----------------------------------------------------------------------------
# StatInterpreter interval_result dispatch
# ----------------------------------------------------------------------------

test_that("StatInterpreter handles ci_mean (manager)", {
  analyzer <- IntervalAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$ci_mean(x)
  out <- interpreter$interpret(result, audience = "manager")
  expect_type(out, "character")
  expect_true(grepl("Interval Estimate", out))
  expect_true(grepl("Confidence interval for the population mean", out))
  expect_true(grepl("Conclusion", out))
})

test_that("StatInterpreter handles tolerance_interval (manager)", {
  analyzer <- IntervalAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  result <- analyzer$tolerance_interval(x, p = 0.99, conf_level = 0.95)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("Tolerance interval", out))
  expect_true(grepl("Tolerance Interval Parameters", out))
  expect_true(grepl("k factor", out))
})

test_that("StatInterpreter handles ci_diff_mean (technical)", {
  analyzer <- IntervalAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  y <- rnorm(30, mean = 102, sd = 5)
  result <- analyzer$ci_diff_mean(x, y)
  out <- interpreter$interpret(result, audience = "technical")
  expect_true(grepl("Two-Sample Difference", out))
  expect_true(grepl("Welch", out))
})

test_that("StatInterpreter handles pi_mean (manager)", {
  analyzer <- IntervalAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$pi_mean(x)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("Prediction Interval", out))
  expect_true(grepl("NEXT single measurement", out))
})

test_that("StatInterpreter handles margin_of_error (manager)", {
  analyzer <- IntervalAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$margin_of_error(x, type = "mean")
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("Margin of Error", out))
  expect_true(grepl("MOE", out))
})

test_that("StatInterpreter handles ci_variance (manager)", {
  analyzer <- IntervalAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$ci_variance(x)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("Variance", out))
  expect_true(grepl("SD CI", out))
})

# ----------------------------------------------------------------------------
# iqr_intervals L3 integrator
# ----------------------------------------------------------------------------

test_that("iqr_intervals runs ci_mean end-to-end", {
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  iv <- iqr_intervals$new()
  iv$run("ci_mean", x = x, conf_level = 0.95)
  expect_s3_class(iv$last_results, "interval_result")
  expect_equal(iv$last_results$test_type, "ci_mean")
})

test_that("iqr_intervals runs tolerance_interval end-to-end", {
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  iv <- iqr_intervals$new()
  iv$run("tolerance_interval", x = x, p = 0.99)
  expect_equal(iv$last_results$test_type, "tolerance_interval")
})

test_that("iqr_intervals$report returns data.frame", {
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  iv <- iqr_intervals$new()
  iv$run("ci_mean", x = x)
  df <- iv$report(format = "data.frame")
  expect_s3_class(df, "data.frame")
})

test_that("iqr_intervals$plot / $interpret error before $run", {
  iv <- iqr_intervals$new()
  expect_error(iv$plot(), "run.*first")
  expect_error(iv$interpret(), "run.*first")
  expect_error(iv$report(), "run.*first")
})

test_that("iqr_intervals set_theme works", {
  iv <- iqr_intervals$new()
  iv$set_theme("academic")
  expect_true(inherits(iv$theme_obj, "IqrTheme") || is.null(iv$theme_obj))
})

# ----------------------------------------------------------------------------
# Convenience functions
# ----------------------------------------------------------------------------

test_that("intervals_run returns stat_result", {
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- intervals_run("ci_mean", x = x)
  expect_s3_class(result, "interval_result")
})

test_that("intervals_interpret returns character", {
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- intervals_run("ci_mean", x = x)
  out <- capture.output(intervals_interpret(result, audience = "manager"))
  expect_true(length(out) > 0L)
})

test_that("intervals_report returns data.frame", {
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- intervals_run("ci_mean", x = x)
  df <- intervals_report(result, format = "data.frame")
  expect_s3_class(df, "data.frame")
})

# ----------------------------------------------------------------------------
# IntervalPlotter
# ----------------------------------------------------------------------------

test_that("IntervalPlotter initialization", {
  plotter <- IntervalPlotter$new(theme = "academic")
  expect_true(inherits(plotter, "IntervalPlotter"))
  expect_true(inherits(plotter, "R6"))
})

test_that("IntervalPlotter$plot has Contract 2 signature", {
  f <- IntervalPlotter$new()$plot
  fm <- formals(f)
  expect_named(fm, c("result", "plot_type", "show_table", "theme_obj"))
  expect_equal(fm$plot_type, "auto")
  expect_equal(fm$show_table, FALSE)
  expect_equal(fm$theme_obj, NULL)
})

test_that("IntervalPlotter renders histogram for ci_mean", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- IntervalAnalyzer$new()$ci_mean(x)
  plotter <- IntervalPlotter$new()
  p <- plotter$plot(result, plot_type = "histogram")
  expect_true(inherits(p, "ggplot"))
})

test_that("IntervalPlotter renders bar for ci_proportion", {
  skip_if_not_installed("iQualityR.plot")
  result <- IntervalAnalyzer$new()$ci_proportion(x = 12, n = 200)
  plotter <- IntervalPlotter$new()
  p <- plotter$plot(result, plot_type = "bar")
  expect_true(inherits(p, "ggplot"))
})

test_that("IntervalPlotter auto-selects plot type", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- IntervalAnalyzer$new()$ci_mean(x)
  plotter <- IntervalPlotter$new()
  p <- plotter$plot(result, plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})

test_that("IntervalPlotter rejects unknown plot_type", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- IntervalAnalyzer$new()$ci_mean(x)
  plotter <- IntervalPlotter$new()
  expect_error(plotter$plot(result, plot_type = "nope"), "unknown plot_type")
})

# ============================================================================
# R3-D2: pi_mean unification (single-sample + model-based)
# ============================================================================

# ----------------------------------------------------------------------------
# Mode 1: single-sample normal PI (backward compatibility & pi_mode tag)
# ----------------------------------------------------------------------------

test_that("pi_mean sample mode tags pi_mode='sample'", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$pi_mean(x, conf_level = 0.95)
  expect_equal(result$pi_mode, "sample")
  expect_null(result$prediction_table)
  expect_null(result$model_call)
  expect_equal(result$method,
               "Prediction interval for one future observation")
})

test_that("pi_mean sample mode matches theoretical formula", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(42)
  x <- rnorm(20, mean = 50, sd = 4)
  res <- analyzer$pi_mean(x, conf_level = 0.95)
  n <- length(x)
  df <- n - 1
  crit <- stats::qt(1 - 0.05 / 2, df = df)
  se_pred <- stats::sd(x) * sqrt(1 + 1 / n)
  expected_low <- mean(x) - crit * se_pred
  expected_upp <- mean(x) + crit * se_pred
  expect_equal(as.numeric(res$conf.int[1]), expected_low, tolerance = 1e-9)
  expect_equal(as.numeric(res$conf.int[2]), expected_upp, tolerance = 1e-9)
  expect_equal(as.numeric(res$se_pred), se_pred, tolerance = 1e-9)
})

test_that("pi_mean sample mode errors on n < 2", {
  analyzer <- IntervalAnalyzer$new()
  expect_error(analyzer$pi_mean(c(1)), "need at least 2")
})

test_that("pi_mean dispatches to sample mode when only x is supplied", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(1)
  x <- rnorm(10)
  res <- analyzer$pi_mean(x)
  expect_equal(res$pi_mode, "sample")
})

# ----------------------------------------------------------------------------
# Mode 2: model-based PI (explicit model argument)
# ----------------------------------------------------------------------------

test_that("pi_mean model mode with explicit lm returns stat_result", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(123)
  df <- data.frame(x = rnorm(30, mean = 50, sd = 5))
  df$y <- 2 + 1.5 * df$x + rnorm(30, sd = 2)
  fit <- lm(y ~ x, data = df)
  res <- analyzer$pi_mean(model = fit, conf_level = 0.95)
  expect_s3_class(res, "interval_result")
  expect_equal(res$pi_mode, "model")
  expect_equal(res$test_type, "pi_mean")
  expect_match(res$method, "model-based.*predict\\.lm")
  expect_equal(res$model_call, "y ~ x")
  expect_equal(res$n, 30L)  # in-sample by default
})

test_that("pi_mean model mode matches predict.lm interval='prediction'", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(2024)
  df <- data.frame(x = rnorm(40, mean = 0, sd = 3))
  df$y <- 1 + 0.8 * df$x + rnorm(40, sd = 1.5)
  fit <- lm(y ~ x, data = df)
  newdata <- data.frame(x = c(0, 1, 2))
  res <- analyzer$pi_mean(model = fit, newdata = newdata, conf_level = 0.95)
  ref <- as.data.frame(predict(fit, newdata = newdata,
                               interval = "prediction", level = 0.95))
  # prediction_table should mirror predict.lm columns
  expect_equal(res$prediction_table$fit,  ref$fit,  tolerance = 1e-9)
  expect_equal(res$prediction_table$lwr,  ref$lwr,  tolerance = 1e-9)
  expect_equal(res$prediction_table$upr,  ref$upr,  tolerance = 1e-9)
  expect_equal(res$prediction_table$.row, c(1L, 2L, 3L))
})

test_that("pi_mean model mode single-row newdata surfaces conf.int", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(7)
  df <- data.frame(x = rnorm(20, mean = 5, sd = 2))
  df$y <- 10 + 2 * df$x + rnorm(20, sd = 1)
  fit <- lm(y ~ x, data = df)
  newdata <- data.frame(x = 5)
  res <- analyzer$pi_mean(model = fit, newdata = newdata, conf_level = 0.90)
  ref <- predict(fit, newdata = newdata,
                 interval = "prediction", level = 0.90)
  expect_equal(as.numeric(res$conf.int),
               as.numeric(c(ref[1, "lwr"], ref[1, "upr"])),
               tolerance = 1e-9)
  expect_equal(as.numeric(res$statistic[1]), as.numeric(ref[1, "fit"]),
               tolerance = 1e-9)
  # se_pred is well-defined for single row
  expect_false(is.na(res$se_pred))
})

test_that("pi_mean model mode multi-row newdata stores prediction_table", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(11)
  df <- data.frame(x = rnorm(25, mean = 10, sd = 3))
  df$y <- 3 - 0.5 * df$x + rnorm(25, sd = 2)
  fit <- lm(y ~ x, data = df)
  newdata <- data.frame(x = seq(0, 10, length.out = 5))
  res <- analyzer$pi_mean(model = fit, newdata = newdata)
  expect_equal(nrow(res$prediction_table), 5L)
  expect_true(all(c("fit", "lwr", "upr", ".row") %in%
                  names(res$prediction_table)))
  # summary conf.int spans min lwr and max upr
  expect_equal(as.numeric(res$conf.int[1]),
               min(res$prediction_table$lwr), tolerance = 1e-9)
  expect_equal(as.numeric(res$conf.int[2]),
               max(res$prediction_table$upr), tolerance = 1e-9)
})

test_that("pi_mean model mode defaults to in-sample when newdata is NULL", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(99)
  df <- data.frame(x = rnorm(15, mean = 0, sd = 1))
  df$y <- 0.5 * df$x + rnorm(15, sd = 0.5)
  fit <- lm(y ~ x, data = df)
  res <- analyzer$pi_mean(model = fit)
  # In-sample: number of rows equals model frame rows
  expect_equal(res$n, 15L)
  ref <- suppressWarnings(predict(fit, interval = "prediction", level = 0.95))
  expect_equal(res$prediction_table$fit, as.numeric(ref[, "fit"]),
               tolerance = 1e-9)
})

# ----------------------------------------------------------------------------
# Mode 2: model-based PI (formula + data)
# ----------------------------------------------------------------------------

test_that("pi_mean model mode accepts formula + data", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(33)
  df <- data.frame(x = rnorm(20, mean = 0, sd = 1))
  df$y <- 1 + 2 * df$x + rnorm(20, sd = 1)
  res <- analyzer$pi_mean(formula = y ~ x, data = df,
                          newdata = data.frame(x = c(0, 1)),
                          conf_level = 0.95)
  expect_equal(res$pi_mode, "model")
  expect_equal(res$model_call, "y ~ x")
  expect_equal(nrow(res$prediction_table), 2L)
  # Verify against a manually fitted model
  fit <- lm(y ~ x, data = df)
  ref <- predict(fit, newdata = data.frame(x = c(0, 1)),
                 interval = "prediction", level = 0.95)
  expect_equal(res$prediction_table$lwr, as.numeric(ref[, "lwr"]),
               tolerance = 1e-9)
})

test_that("pi_mean model mode attaches response column when newdata has it", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(55)
  df <- data.frame(x = rnorm(20, mean = 0, sd = 1))
  df$y <- 1 + 2 * df$x + rnorm(20, sd = 1)
  newdata <- data.frame(x = c(0, 1), y = c(1, 3))
  res <- analyzer$pi_mean(formula = y ~ x, data = df, newdata = newdata)
  expect_true("y" %in% names(res$prediction_table))
  expect_equal(res$prediction_table$y, c(1, 3))
})

# ----------------------------------------------------------------------------
# Mode 2: input validation & alternative coercion
# ----------------------------------------------------------------------------

test_that("pi_mean model mode errors without formula/data when model is NULL", {
  analyzer <- IntervalAnalyzer$new()
  expect_error(analyzer$pi_mean(model = NULL, formula = NULL),
               "provide 'x'.*'model' / 'formula'")
})

test_that("pi_mean model mode errors when data is not a data.frame", {
  analyzer <- IntervalAnalyzer$new()
  expect_error(analyzer$pi_mean(formula = y ~ x, data = list(y = 1, x = 1)),
               "must be a data frame")
})

test_that("pi_mean model mode errors when model is not lm", {
  analyzer <- IntervalAnalyzer$new()
  not_a_model <- list(coefficients = c(1, 2))
  expect_error(analyzer$pi_mean(model = not_a_model), "must be an lm object")
})

test_that("pi_mean model mode coerces one-sided alternative with warning", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(8)
  df <- data.frame(x = rnorm(20))
  df$y <- 1 + 2 * df$x + rnorm(20, sd = 1)
  fit <- lm(y ~ x, data = df)
  expect_warning(
    res <- analyzer$pi_mean(model = fit,
                            newdata = data.frame(x = 0),
                            alternative = "greater"),
    "two.sided"
  )
  expect_equal(res$alternative, "two.sided")
})

# ----------------------------------------------------------------------------
# Mode 2: dispatch through analyze()
# ----------------------------------------------------------------------------

test_that("analyze('pi_mean', model=...) dispatches to model mode", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(44)
  df <- data.frame(x = rnorm(20, mean = 5))
  df$y <- 1 + df$x + rnorm(20, sd = 0.5)
  fit <- lm(y ~ x, data = df)
  res <- analyzer$analyze("pi_mean", model = fit,
                          newdata = data.frame(x = 5))
  expect_equal(res$pi_mode, "model")
})

# ----------------------------------------------------------------------------
# L3 integrator & convenience functions for model mode
# ----------------------------------------------------------------------------

test_that("iqr_intervals runs pi_mean model mode end-to-end", {
  set.seed(123)
  df <- data.frame(x = rnorm(30, mean = 10, sd = 2))
  df$y <- 5 + 1.2 * df$x + rnorm(30, sd = 1)
  fit <- lm(y ~ x, data = df)
  iv <- iqr_intervals$new()
  iv$run("pi_mean", model = fit, newdata = data.frame(x = 10))
  expect_s3_class(iv$last_results, "interval_result")
  expect_equal(iv$last_results$pi_mode, "model")
})

test_that("intervals_run returns model-mode stat_result", {
  set.seed(202)
  df <- data.frame(x = rnorm(20))
  df$y <- 0.7 * df$x + rnorm(20, sd = 0.3)
  fit <- lm(y ~ x, data = df)
  res <- intervals_run("pi_mean", model = fit,
                       newdata = data.frame(x = 0))
  expect_s3_class(res, "interval_result")
  expect_equal(res$pi_mode, "model")
})

# ----------------------------------------------------------------------------
# StatInterpreter dispatch for model mode
# ----------------------------------------------------------------------------

test_that("StatInterpreter handles pi_mean model mode (manager)", {
  analyzer <- IntervalAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(321)
  df <- data.frame(x = rnorm(20))
  df$y <- 1 + 2 * df$x + rnorm(20, sd = 1)
  fit <- lm(y ~ x, data = df)
  res <- analyzer$pi_mean(model = fit, newdata = data.frame(x = 0))
  out <- interpreter$interpret(res, audience = "manager")
  expect_true(grepl("Prediction Interval", out))
  expect_true(grepl("model-based", out))
  expect_true(grepl("y ~ x", out))
  expect_true(grepl("prediction_table", out))
})

test_that("StatInterpreter handles pi_mean model mode (technical)", {
  analyzer <- IntervalAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(31)
  df <- data.frame(x = rnorm(20))
  df$y <- 1 + 2 * df$x + rnorm(20, sd = 1)
  fit <- lm(y ~ x, data = df)
  res <- analyzer$pi_mean(model = fit, newdata = data.frame(x = c(0, 1)))
  out <- interpreter$interpret(res, audience = "technical")
  expect_true(grepl("model-based|predict\\.lm|Interval type", out))
})

# ----------------------------------------------------------------------------
# IntervalPlotter dispatch for model mode
# ----------------------------------------------------------------------------

test_that("IntervalPlotter auto-selects errorbar for pi_mean model mode", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- IntervalAnalyzer$new()
  set.seed(77)
  df <- data.frame(x = rnorm(20))
  df$y <- 1 + 2 * df$x + rnorm(20, sd = 1)
  fit <- lm(y ~ x, data = df)
  res <- analyzer$pi_mean(model = fit, newdata = data.frame(x = 0))
  plotter <- IntervalPlotter$new()
  # auto should NOT fall into the histogram path (which would fail on a
  # data.frame stored in result$data$x); it should produce a ggplot.
  p <- plotter$plot(res, plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})

test_that("IntervalPlotter errorbar works for multi-row pi_mean model mode", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- IntervalAnalyzer$new()
  set.seed(66)
  df <- data.frame(x = rnorm(25, mean = 5, sd = 2))
  df$y <- 1 + 1.5 * df$x + rnorm(25, sd = 1)
  fit <- lm(y ~ x, data = df)
  newdata <- data.frame(x = seq(2, 8, length.out = 4))
  res <- analyzer$pi_mean(model = fit, newdata = newdata)
  plotter <- IntervalPlotter$new()
  p <- plotter$plot(res, plot_type = "errorbar")
  expect_true(inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# IntervalReporter compatibility with model mode
# ----------------------------------------------------------------------------

test_that("IntervalReporter to_dataframe works for pi_mean model mode", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(88)
  df <- data.frame(x = rnorm(20))
  df$y <- 1 + 2 * df$x + rnorm(20, sd = 1)
  fit <- lm(y ~ x, data = df)
  res <- analyzer$pi_mean(model = fit, newdata = data.frame(x = 0))
  reporter <- IntervalReporter$new()
  out <- reporter$report(res, format = "data.frame")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_equal(out$Interval_Type, "pi_mean")
  expect_true(!is.na(out$Lower))
  expect_true(!is.na(out$Upper))
})

test_that("IntervalReporter console works for pi_mean model mode", {
  analyzer <- IntervalAnalyzer$new()
  set.seed(13)
  df <- data.frame(x = rnorm(20))
  df$y <- 1 + 2 * df$x + rnorm(20, sd = 1)
  fit <- lm(y ~ x, data = df)
  res <- analyzer$pi_mean(model = fit, newdata = data.frame(x = 0))
  reporter <- IntervalReporter$new()
  out <- capture.output(reporter$report(res, format = "console",
                                        audience = "manager"))
  expect_true(length(out) > 0L)
  expect_true(any(grepl("Prediction|pi_mean", out)))
})
