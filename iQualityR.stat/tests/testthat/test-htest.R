# =============================================================================
# File: tests/testthat/test-htest.R
# Description: Hypothesis testing module tests
#   - stat_result S3 class
#   - HTestAnalyzer (all 8 test types)
#   - HTestPlotter (Contract 2 signature)
#   - HTestReporter (Contract 2 signature)
#   - iqr_htest L3 integrator
#   - Convenience functions (htest_run/interpret/report)
#   - StatInterpreter htest_result dispatch
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# stat_result S3 class (new_stat_result / print / format)
# ----------------------------------------------------------------------------

test_that("new_stat_result wraps a list with stat_result + htest_result class", {
  res <- new_stat_result(list(test_type = "t_test_1s", method = "t"), "htest")
  expect_s3_class(res, "stat_result")
  expect_s3_class(res, "htest_result")
  expect_equal(res$domain, "htest")
})

test_that("new_stat_result rejects non-list input", {
  expect_error(new_stat_result(1L, "htest"), "must be a list")
})

test_that("format.stat_result produces a multi-line string with key fields", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  out <- format(result)
  expect_type(out, "character")
  expect_true(length(out) == 1L)
  expect_true(grepl("One Sample t-test", out))
  expect_true(grepl("p-value", out))
  expect_true(grepl("stat_result", out))
})

test_that("print.stat_result returns the object invisibly", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  out <- capture.output(invisible(print(result)))
  expect_true(any(grepl("One Sample t-test", out)))
})

# ----------------------------------------------------------------------------
# HTestAnalyzer -- all 8 test types return stat_result
# ----------------------------------------------------------------------------

test_that("HTestAnalyzer initialization", {
  analyzer <- HTestAnalyzer$new()
  expect_true(inherits(analyzer, "HTestAnalyzer"))
  expect_true(inherits(analyzer, "R6"))
})

test_that("HTestAnalyzer one-sample t-test returns stat_result", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- analyzer$t_test_1s(x, mu = 5)
  expect_s3_class(result, "stat_result")
  expect_s3_class(result, "htest_result")
  expect_equal(result$domain, "htest")
  expect_equal(result$test_type, "t_test_1s")
  expect_true("p.value" %in% names(result))
  expect_true("statistic" %in% names(result))
  expect_true("n" %in% names(result))
  expect_true("data" %in% names(result))
  expect_false(is.null(result$data$x))
})

test_that("HTestAnalyzer two-sample t-test returns stat_result with x and y data", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  y <- rnorm(30, mean = 5.5, sd = 1)
  result <- analyzer$t_test_2s(x, y)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "t_test_2s")
  expect_false(is.null(result$data$x))
  expect_false(is.null(result$data$y))
})

test_that("HTestAnalyzer analyze dispatch by test_type string", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- analyzer$analyze("t_test_1s", x = x, mu = 5)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "t_test_1s")
})

test_that("HTestAnalyzer one-sample Z-test with raw data", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 100, sd = 5)
  result <- analyzer$z_test_1s(x, mu = 100, sigma = 5)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "z_test_1s")
  expect_equal(result$dist_type, "norm")
})

test_that("HTestAnalyzer one-sample Z-test with summary stats (no raw data)", {
  analyzer <- HTestAnalyzer$new()
  result <- analyzer$z_test_1s(list(mean = 102, n = 30, sd = 5), mu = 100, sigma = 5)
  expect_s3_class(result, "stat_result")
  expect_true(is.null(result$data$x))
})

test_that("HTestAnalyzer paired t-test", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(20, mean = 10, sd = 2)
  y <- x + rnorm(20, mean = 0.5, sd = 0.5)
  result <- analyzer$t_test_paired(x, y)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "t_test_paired")
})

test_that("HTestAnalyzer one-sample proportion test (summary-only, no raw data)", {
  analyzer <- HTestAnalyzer$new()
  result <- analyzer$prop_test_1s(x = 45, n = 100, p0 = 0.5)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "prop_test_1s")
  expect_true(is.null(result$data$x))
})

test_that("HTestAnalyzer two-sample proportion test", {
  analyzer <- HTestAnalyzer$new()
  result <- analyzer$prop_test_2s(x1 = 30, n1 = 100, x2 = 45, n2 = 100)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "prop_test_2s")
})

test_that("HTestAnalyzer F-test (variance equality)", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(20, sd = 2)
  y <- rnorm(20, sd = 3)
  result <- analyzer$f_test(x, y)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "f_test")
  expect_equal(result$dist_type, "f")
})

test_that("HTestAnalyzer chi-square test (goodness of fit)", {
  analyzer <- HTestAnalyzer$new()
  result <- analyzer$chisq_test(c(20, 30, 50))
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "chisq_test")
})

test_that("HTestAnalyzer chi-square test (contingency table)", {
  analyzer <- HTestAnalyzer$new()
  tbl <- matrix(c(10, 20, 30, 40), nrow = 2)
  result <- analyzer$chisq_test(tbl)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "chisq_test")
})

test_that("HTestAnalyzer unknown test_type raises error", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$analyze("nope"), "Unknown test type")
})

# ----------------------------------------------------------------------------
# HTestAnalyzer -- uncovered scenarios (alternative directions, var.equal)
# ----------------------------------------------------------------------------

test_that("HTestAnalyzer t_test_1s with alternative='less'", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 4, sd = 1)
  result <- analyzer$t_test_1s(x, mu = 5, alternative = "less")
  expect_s3_class(result, "stat_result")
  expect_equal(result$alternative, "less")
  # Mean is below 5, so one-sided 'less' should give a small p-value
  expect_lt(result$p.value, 0.05)
})

test_that("HTestAnalyzer t_test_1s with alternative='greater'", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 6, sd = 1)
  result <- analyzer$t_test_1s(x, mu = 5, alternative = "greater")
  expect_s3_class(result, "stat_result")
  expect_equal(result$alternative, "greater")
  expect_lt(result$p.value, 0.05)
})

test_that("HTestAnalyzer t_test_2s with var.equal=TRUE", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  y <- rnorm(30, mean = 5.5, sd = 1)
  result <- analyzer$t_test_2s(x, y, var.equal = TRUE)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "t_test_2s")
  # Pooled t-test should have df = n1 + n2 - 2 = 58
  expect_equal(as.numeric(result$parameter["df"]), 58)
})

test_that("HTestAnalyzer z_test_1s with alternative='greater'", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 102, sd = 5)
  result <- analyzer$z_test_1s(x, mu = 100, sigma = 5, alternative = "greater")
  expect_s3_class(result, "stat_result")
  expect_equal(result$alternative, "greater")
  expect_equal(result$dist_type, "norm")
})

test_that("HTestAnalyzer prop_test_1s with alternative='less'", {
  analyzer <- HTestAnalyzer$new()
  result <- analyzer$prop_test_1s(x = 35, n = 100, p0 = 0.5, alternative = "less")
  expect_s3_class(result, "stat_result")
  expect_equal(result$alternative, "less")
})

test_that("HTestAnalyzer f_test with alternative='less'", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(20, sd = 2)
  y <- rnorm(20, sd = 4)
  result <- analyzer$f_test(x, y, alternative = "less")
  expect_s3_class(result, "stat_result")
  expect_equal(result$alternative, "less")
  expect_equal(result$dist_type, "f")
})

test_that("HTestAnalyzer t_test_2s with alternative='greater'", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 6, sd = 1)
  y <- rnorm(30, mean = 5, sd = 1)
  result <- analyzer$t_test_2s(x, y, alternative = "greater")
  expect_s3_class(result, "stat_result")
  expect_equal(result$alternative, "greater")
  expect_lt(result$p.value, 0.05)
})

test_that("HTestAnalyzer prop_test_2s with alternative='greater'", {
  analyzer <- HTestAnalyzer$new()
  result <- analyzer$prop_test_2s(x1 = 60, n1 = 100, x2 = 40, n2 = 100,
                                   alternative = "greater")
  expect_s3_class(result, "stat_result")
  expect_equal(result$alternative, "greater")
})

test_that("HTestAnalyzer chisq_test with custom probabilities", {
  analyzer <- HTestAnalyzer$new()
  # Test if a die is fair with custom expected probabilities
  result <- analyzer$chisq_test(c(10, 20, 30, 40, 50, 60),
                                 p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "chisq_test")
})

test_that("HTestAnalyzer analyze dispatches all 8 test types", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, 5, 1)
  y <- rnorm(30, 5.5, 1)

  # Each should return stat_result without error
  expect_s3_class(analyzer$analyze("t_test_1s", x = x, mu = 5), "stat_result")
  expect_s3_class(analyzer$analyze("t_test_2s", x = x, y = y), "stat_result")
  expect_s3_class(analyzer$analyze("t_test_paired", x = x, y = y), "stat_result")
  expect_s3_class(analyzer$analyze("z_test_1s", x = x, mu = 5, sigma = 1), "stat_result")
  expect_s3_class(analyzer$analyze("prop_test_1s", x = 45, n = 100, p0 = 0.5), "stat_result")
  expect_s3_class(analyzer$analyze("prop_test_2s", x1 = 30, n1 = 100, x2 = 45, n2 = 100), "stat_result")
  expect_s3_class(analyzer$analyze("f_test", x = x, y = y), "stat_result")
  expect_s3_class(analyzer$analyze("chisq_test", x = c(20, 30, 50)), "stat_result")
})

# ----------------------------------------------------------------------------
# HTestPlotter -- Contract 2 signature
# ----------------------------------------------------------------------------

test_that("HTestPlotter initialization with theme name", {
  plotter <- HTestPlotter$new(theme = "academic")
  expect_true(inherits(plotter, "HTestPlotter"))
  expect_true(inherits(plotter, "R6"))
})

test_that("HTestPlotter$plot has Contract 2 signature (result, plot_type, show_table, theme_obj)", {
  f <- HTestPlotter$new()$plot
  fm <- formals(f)
  expect_named(fm, c("result", "plot_type", "show_table", "theme_obj"))
  expect_equal(fm$plot_type, "auto")
  expect_equal(fm$show_table, FALSE)
  expect_equal(fm$theme_obj, NULL)
})

test_that("HTestPlotter$plot rejects bad plot_type", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  plotter <- HTestPlotter$new()
  expect_error(plotter$plot(result, plot_type = "nope"), "arg")
})

test_that("HTestPlotter$plot_box errors when result has no raw x data", {
  result <- HTestAnalyzer$new()$prop_test_1s(x = 45, n = 100, p0 = 0.5)
  plotter <- HTestPlotter$new()
  expect_error(plotter$plot_box(result), "not available")
})

# ----------------------------------------------------------------------------
# HTestReporter -- Contract 2 signature
# ----------------------------------------------------------------------------

test_that("HTestReporter initialization", {
  reporter <- HTestReporter$new()
  expect_true(inherits(reporter, "HTestReporter"))
  expect_true(inherits(reporter, "R6"))
})

test_that("HTestReporter$report has Contract 2 signature", {
  f <- HTestReporter$new()$report
  fm <- formals(f)
  expect_named(fm, c("result", "format", "path", "audience"))
})

test_that("HTestReporter$report format='data.frame' returns a data.frame", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  reporter <- HTestReporter$new()
  df <- reporter$report(result, format = "data.frame")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
  expect_equal(df$Domain, "htest")
  expect_equal(df$Test_Type, "t_test_1s")
})

test_that("HTestReporter$report format='console' prints without error", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  reporter <- HTestReporter$new()
  out <- capture.output(reporter$report(result, format = "console", audience = "manager"))
  expect_true(length(out) > 0L)
})

test_that("HTestReporter$to_dataframe includes estimates and sample size", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  reporter <- HTestReporter$new()
  df <- reporter$to_dataframe(result)
  expect_true("Estimate_mean_of_x" %in% names(df))
  expect_true("N" %in% names(df))
})

# ----------------------------------------------------------------------------
# iqr_htest L3 integrator -- Contract 2 5-method surface
# ----------------------------------------------------------------------------

test_that("iqr_htest R6 class entry", {
  htest <- iqr_htest$new()
  expect_true(inherits(htest, "iqr_htest"))
  expect_true(inherits(htest, "R6"))
})

test_that("iqr_htest$run caches a stat_result on last_results", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  htest <- iqr_htest$new()
  htest$run("t_test_1s", x = x, mu = 5)
  expect_s3_class(htest$last_results, "stat_result")
  expect_equal(htest$last_results$test_type, "t_test_1s")
})

test_that("iqr_htest$plot errors before $run", {
  htest <- iqr_htest$new()
  expect_error(htest$plot(), "run\\(\\) first")
})

test_that("iqr_htest$report errors before $run", {
  htest <- iqr_htest$new()
  expect_error(htest$report(), "run\\(\\) first")
})

test_that("iqr_htest$report returns data.frame after $run", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  htest <- iqr_htest$new()
  htest$run("t_test_1s", x = x, mu = 5)
  df <- htest$report(format = "data.frame")
  expect_s3_class(df, "data.frame")
})

# ----------------------------------------------------------------------------
# Convenience functions -- Contract 2 (result as first arg)
# ----------------------------------------------------------------------------

test_that("htest_run returns a stat_result", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- htest_run("t_test_1s", x = x, mu = 5)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "t_test_1s")
})

test_that("htest_interpret accepts a stat_result (Contract 2)", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- htest_run("t_test_1s", x = x, mu = 5)
  interp <- htest_interpret(result, audience = "manager")
  expect_type(interp, "character")
  expect_true(nchar(interp) > 0)
})

test_that("htest_report accepts a stat_result (Contract 2)", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- htest_run("t_test_1s", x = x, mu = 5)
  df <- htest_report(result, format = "data.frame")
  expect_s3_class(df, "data.frame")
  expect_equal(df$Test_Type, "t_test_1s")
})

# ----------------------------------------------------------------------------
# StatInterpreter recognises stat_result/htest_result
# ----------------------------------------------------------------------------

test_that("StatInterpreter interprets a stat_result (htest_result class)", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  interpreter <- StatInterpreter$new()
  explanation <- interpreter$interpret(result, audience = "manager")
  expect_type(explanation, "character")
  expect_false(grepl("Unable to recognize", explanation))
})
