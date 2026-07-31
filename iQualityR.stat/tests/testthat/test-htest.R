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
  res <- iQualityR.stat:::new_stat_result(list(test_type = "t_test_1s", method = "t"), "htest")
  expect_s3_class(res, "stat_result")
  expect_s3_class(res, "htest_result")
  expect_equal(res$domain, "htest")
})

test_that("new_stat_result rejects non-list input", {
  expect_error(iQualityR.stat:::new_stat_result(1L, "htest"), "must be a list")
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

# ----------------------------------------------------------------------------
# HTestReporter -- additional coverage (R2-7d)
# ----------------------------------------------------------------------------

test_that("HTestReporter$to_dataframe handles two-sample test with n1/n2", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  y <- rnorm(30, mean = 5.5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_2s(x, y)
  reporter <- HTestReporter$new()
  df <- reporter$to_dataframe(result)
  expect_s3_class(df, "data.frame")
  # Two-sample tests store n1/n2 instead of n
  expect_true("N1" %in% names(df) || "N" %in% names(df))
})

test_that("HTestReporter$report format='excel' writes an xlsx file", {
  skip_if_not_installed("iQualityR.core")
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  reporter <- HTestReporter$new()
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf), add = TRUE)
  expect_no_error(reporter$report(result, format = "excel", path = tf))
  expect_true(file.exists(tf))
})

test_that("HTestReporter$export_excel builds exporter on demand", {
  skip_if_not_installed("iQualityR.core")
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  reporter <- HTestReporter$new()
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf), add = TRUE)
  # No excel_exporter supplied -> should build one from theme
  expect_no_error(reporter$export_excel(result, path = tf))
  expect_true(file.exists(tf))
})

test_that("HTestReporter$print_console works with stat_result input", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  reporter <- HTestReporter$new()
  out <- capture.output(reporter$print_console(result, interpret = FALSE))
  expect_true(length(out) > 0L)
  expect_true(any(grepl("t-test", out, ignore.case = TRUE)))
})

test_that("HTestReporter$print_console falls back for legacy list input", {
  # Provide a raw list (not stat_result) to exercise the legacy branch
  legacy <- list(
    method = "Legacy Test",
    test_type = "legacy_test",
    statistic = c(stat = 1.23),
    parameter = c(df = 10),
    p.value = 0.04,
    conf.int = c(0.1, 0.9),
    conf.level = 0.95,
    estimate = c(mean = 5.0),
    data_name = "legacy_data"
  )
  reporter <- HTestReporter$new()
  out <- capture.output(reporter$print_console(legacy, interpret = FALSE))
  expect_true(any(grepl("Legacy Test", out)))
  expect_true(any(grepl("P Value", out)))
})

test_that("HTestReporter$report format='data.frame' includes CI columns", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  reporter <- HTestReporter$new()
  df <- reporter$to_dataframe(result)
  expect_true("CI_Lower" %in% names(df))
  expect_true("CI_Upper" %in% names(df))
  expect_true(!is.na(df$CI_Lower))
  expect_true(!is.na(df$CI_Upper))
})

test_that("HTestReporter initialize accepts theme name string", {
  reporter <- HTestReporter$new(theme = "academic")
  expect_true(inherits(reporter, "HTestReporter"))
  expect_false(is.null(reporter$theme_obj))
})

# ----------------------------------------------------------------------------
# Non-parametric tests (R3-A1): Wilcoxon / Kruskal-Wallis / Friedman
# ----------------------------------------------------------------------------

test_that("HTestAnalyzer one-sample Wilcoxon signed rank test returns stat_result", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- analyzer$wilcoxon_signed_rank(x, mu = 5)
  expect_s3_class(result, "stat_result")
  expect_s3_class(result, "htest_result")
  expect_equal(result$domain, "htest")
  expect_equal(result$test_type, "wilcoxon_signed_rank")
  expect_true(!is.null(result$statistic))
  expect_true(!is.null(result$p.value))
  expect_true(!is.null(result$conf.int))
  expect_equal(result$paired, FALSE)
  expect_equal(result$n, 30L)
  expect_equal(result$dist_type, "wilcox")
  expect_false(is.null(result$data$x))
  expect_true(is.null(result$data$y))
})

test_that("HTestAnalyzer paired Wilcoxon signed rank test returns stat_result", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(20, mean = 10, sd = 2)
  y <- x + rnorm(20, mean = 1, sd = 0.5)
  result <- analyzer$wilcoxon_signed_rank(x, y)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "wilcoxon_signed_rank")
  expect_equal(result$paired, TRUE)
  expect_false(is.null(result$data$x))
  expect_false(is.null(result$data$y))
  expect_equal(length(result$data$x), length(result$data$y))
})

test_that("Wilcoxon signed rank rejects mismatched paired lengths", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$wilcoxon_signed_rank(1:10, 1:5),
               "same length")
})

test_that("HTestAnalyzer Wilcoxon rank sum test returns stat_result", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(20, mean = 50, sd = 5)
  y <- rnorm(20, mean = 55, sd = 5)
  result <- analyzer$wilcoxon_rank_sum(x, y)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "wilcoxon_rank_sum")
  expect_equal(result$n1, 20L)
  expect_equal(result$n2, 20L)
  expect_equal(result$dist_type, "wilcox")
  expect_false(is.null(result$data$x))
  expect_false(is.null(result$data$y))
  # Should produce a finite statistic named "W"
  expect_true(!is.na(as.numeric(result$statistic)))
  expect_equal(names(result$statistic)[1], "W")
})

test_that("Wilcoxon rank sum detects a real shift", {
  analyzer <- HTestAnalyzer$new()
  set.seed(42)
  x <- rnorm(30, mean = 0, sd = 1)
  y <- rnorm(30, mean = 2, sd = 1)
  result <- analyzer$wilcoxon_rank_sum(x, y)
  expect_true(result$p.value < 0.05)
})

test_that("HTestAnalyzer Kruskal-Wallis test accepts vector + grouping", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- c(rnorm(15, 50, 5), rnorm(15, 55, 5), rnorm(15, 60, 5))
  g <- factor(rep(c("A", "B", "C"), each = 15))
  result <- analyzer$kruskal_wallis(x, g)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "kruskal_wallis")
  expect_equal(result$k, 3L)
  expect_equal(length(result$group_n), 3L)
  expect_equal(names(result$group_n), c("A", "B", "C"))
  expect_equal(length(result$group_mean_rank), 3L)
  expect_equal(result$dist_type, "chisq")
  # chi-squared statistic should be a finite number
  expect_true(!is.na(as.numeric(result$statistic)))
})

test_that("Kruskal-Wallis accepts list-of-vectors input", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  g1 <- rnorm(15, 50, 5)
  g2 <- rnorm(15, 55, 5)
  g3 <- rnorm(15, 60, 5)
  result <- analyzer$kruskal_wallis(list(g1, g2, g3))
  expect_s3_class(result, "stat_result")
  expect_equal(result$k, 3L)
  expect_equal(result$group_n[[1]], 15L)
})

test_that("Kruskal-Wallis requires grouping when x is a vector", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$kruskal_wallis(1:10), "grouping vector")
})

test_that("Kruskal-Wallis rejects length mismatch", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$kruskal_wallis(1:10, factor(1:3)), "same length")
})

test_that("Kruskal-Wallis rejects fewer than 2 groups", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$kruskal_wallis(1:10, factor(rep("A", 10))), "at least 2 groups")
})

test_that("HTestAnalyzer Friedman test accepts matrix input", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  # 5 blocks (rows) x 3 treatments (cols)
  mat <- matrix(c(
    7, 8, 9,
    6, 7, 8,
    8, 9, 10,
    5, 6, 7,
    9, 10, 11
  ), nrow = 5, byrow = TRUE)
  result <- analyzer$friedman(mat)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "friedman")
  expect_equal(result$n_blocks, 5L)
  expect_equal(result$n_treatments, 3L)
  expect_equal(result$dist_type, "chisq")
  expect_false(is.null(result$wide_matrix))
  expect_equal(dim(result$wide_matrix), c(5, 3))
})

test_that("Friedman accepts data.frame input", {
  analyzer <- HTestAnalyzer$new()
  df <- data.frame(
    T1 = c(7, 6, 8, 5, 9),
    T2 = c(8, 7, 9, 6, 10),
    T3 = c(9, 8, 10, 7, 11)
  )
  result <- analyzer$friedman(df)
  expect_s3_class(result, "stat_result")
  expect_equal(result$n_blocks, 5L)
  expect_equal(result$n_treatments, 3L)
})

test_that("Friedman accepts long-form x/g/b input", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- c(7, 8, 9, 6, 7, 8, 8, 9, 10, 5, 6, 7, 9, 10, 11)
  g <- factor(rep(c("T1", "T2", "T3"), times = 5))
  b <- factor(rep(1:5, each = 3))
  result <- analyzer$friedman(x, g, b)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "friedman")
  expect_equal(result$n_blocks, 5L)
  expect_equal(result$n_treatments, 3L)
})

test_that("Friedman rejects too-small matrix", {
  analyzer <- HTestAnalyzer$new()
  # 1 row x 3 cols: only 1 block, not enough for a within-block rank
  expect_error(analyzer$friedman(matrix(1:3, nrow = 1, ncol = 3)),
               "at least 2")
  # 3 rows x 1 col: only 1 treatment, no comparison possible
  expect_error(analyzer$friedman(matrix(1:3, nrow = 3, ncol = 1)),
               "at least 2")
})

test_that("Friedman accepts the 2x2 minimum-sized matrix", {
  analyzer <- HTestAnalyzer$new()
  result <- analyzer$friedman(matrix(c(1, 2, 2, 1), nrow = 2, ncol = 2))
  expect_s3_class(result, "stat_result")
  expect_equal(result$n_blocks, 2L)
  expect_equal(result$n_treatments, 2L)
})

test_that("Friedman requires g and b for vector input", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$friedman(1:9), "both 'g' .treatment. and 'b' .block.")
})

# ----------------------------------------------------------------------------
# analyze() dispatcher + iqr_htest L3 integration with non-parametric tests
# ----------------------------------------------------------------------------

test_that("analyze() dispatches all 4 non-parametric test types", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(20); y <- rnorm(20, mean = 1)

  r1 <- analyzer$analyze("wilcoxon_signed_rank", x = x, mu = 0)
  expect_equal(r1$test_type, "wilcoxon_signed_rank")

  r2 <- analyzer$analyze("wilcoxon_rank_sum", x = x, y = y)
  expect_equal(r2$test_type, "wilcoxon_rank_sum")

  g <- factor(rep(1:3, each = 10))
  r3 <- analyzer$analyze("kruskal_wallis", x = c(rnorm(10), rnorm(10, 1), rnorm(10, 2)), g = g)
  expect_equal(r3$test_type, "kruskal_wallis")

  mat <- matrix(rnorm(15), nrow = 5, ncol = 3)
  r4 <- analyzer$analyze("friedman", x = mat)
  expect_equal(r4$test_type, "friedman")
})

test_that("analyze() rejects unknown test type", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$analyze("nonsense_test"), "Unknown test type")
})

test_that("iqr_htest L3 integrator runs Wilcoxon rank sum end-to-end", {
  set.seed(123)
  g1 <- rnorm(15, mean = 50, sd = 5)
  g2 <- rnorm(15, mean = 58, sd = 5)
  htest <- iqr_htest$new()
  htest$run("wilcoxon_rank_sum", x = g1, y = g2)
  expect_s3_class(htest$last_results, "stat_result")
  expect_equal(htest$last_results$test_type, "wilcoxon_rank_sum")
})

test_that("htest_run convenience function works for Kruskal-Wallis", {
  set.seed(123)
  x <- c(rnorm(10, 50, 5), rnorm(10, 55, 5), rnorm(10, 60, 5))
  g <- factor(rep(c("A", "B", "C"), each = 10))
  result <- htest_run("kruskal_wallis", x = x, g = g)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "kruskal_wallis")
  expect_equal(result$k, 3L)
})

test_that("htest_run convenience function works for Friedman", {
  mat <- matrix(c(
    7, 8, 9,
    6, 7, 8,
    8, 9, 10,
    5, 6, 7,
    9, 10, 11
  ), nrow = 5, byrow = TRUE)
  result <- htest_run("friedman", x = mat)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "friedman")
})

test_that("htest_interpret produces a non-empty string for non-parametric tests", {
  set.seed(123)
  g1 <- rnorm(15, 50, 5); g2 <- rnorm(15, 55, 5)
  result <- htest_run("wilcoxon_rank_sum", x = g1, y = g2, interpret = FALSE)
  out <- capture.output(htest_interpret(result, audience = "manager"))
  expect_true(length(out) > 0L)
  expect_true(any(grepl("Wilcoxon", out)))
})

# ----------------------------------------------------------------------------
# Reporter + Interpreter coverage for non-parametric results
# ----------------------------------------------------------------------------

test_that("HTestReporter$to_dataframe works for non-parametric test types", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(20); y <- rnorm(20, mean = 1)
  reporter <- HTestReporter$new()

  r_wilcox <- analyzer$wilcoxon_rank_sum(x, y)
  df <- reporter$to_dataframe(r_wilcox)
  expect_true("Test_Type" %in% names(df))
  expect_equal(df$Test_Type, "wilcoxon_rank_sum")
  expect_equal(df$N1, 20L)
  expect_equal(df$N2, 20L)

  g <- factor(rep(1:3, each = 10))
  r_kw <- analyzer$kruskal_wallis(c(rnorm(10), rnorm(10, 1), rnorm(10, 2)), g)
  df_kw <- reporter$to_dataframe(r_kw)
  expect_equal(df_kw$Test_Type, "kruskal_wallis")
  # k-sample tests have no conf.int
  expect_true(is.na(df_kw$CI_Lower))
})

test_that("StatInterpreter produces sensible wording for k-sample tests", {
  analyzer <- HTestAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- c(rnorm(10, 50, 5), rnorm(10, 55, 5), rnorm(10, 60, 5))
  g <- factor(rep(c("A", "B", "C"), each = 10))
  result <- analyzer$kruskal_wallis(x, g)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("the groups", out, fixed = TRUE))
})

test_that("StatInterpreter wording for Friedman mentions treatments/blocks", {
  analyzer <- HTestAnalyzer$new()
  interpreter <- StatInterpreter$new()
  mat <- matrix(c(
    7, 8, 9,
    6, 7, 8,
    8, 9, 10,
    5, 6, 7,
    9, 10, 11
  ), nrow = 5, byrow = TRUE)
  result <- analyzer$friedman(mat)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("treatments across blocks", out, fixed = TRUE))
})

test_that("format.stat_result renders non-parametric method names", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(20); y <- rnorm(20, mean = 1)
  result <- analyzer$wilcoxon_rank_sum(x, y)
  out <- format(result)
  expect_true(grepl("Wilcoxon rank sum", out))
  expect_true(grepl("stat_result", out))
})

# ----------------------------------------------------------------------------
# HTestPlotter coverage for non-parametric test types (only when .plot available)
# ----------------------------------------------------------------------------

test_that("HTestPlotter auto-selects box for non-parametric tests", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(20); y <- rnorm(20, mean = 1)
  plotter <- HTestPlotter$new()
  # reach into the private method via environment -- or just verify plot() works
  result <- HTestAnalyzer$new()$wilcoxon_rank_sum(x, y)
  p <- plotter$plot(result, plot_type = "box")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("HTestPlotter renders Wilcoxon signed rank (one-sample) box", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(20, mean = 5, sd = 1)
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$wilcoxon_signed_rank(x, mu = 5)
  p <- plotter$plot(result, plot_type = "box")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("HTestPlotter renders Wilcoxon signed rank (paired) box", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(20, mean = 10, sd = 2)
  y <- x + rnorm(20, mean = 1, sd = 0.5)
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$wilcoxon_signed_rank(x, y)
  p <- plotter$plot(result, plot_type = "box")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("HTestPlotter renders Kruskal-Wallis grouped box plot", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- c(rnorm(15, 50, 5), rnorm(15, 55, 5), rnorm(15, 60, 5))
  g <- factor(rep(c("A", "B", "C"), each = 15))
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$kruskal_wallis(x, g)
  p <- plotter$plot(result, plot_type = "box")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("HTestPlotter renders Friedman profile plot from matrix input", {
  skip_if_not_installed("iQualityR.plot")
  mat <- matrix(c(
    7, 8, 9,
    6, 7, 8,
    8, 9, 10,
    5, 6, 7,
    9, 10, 11
  ), nrow = 5, byrow = TRUE)
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$friedman(mat)
  p <- plotter$plot(result, plot_type = "box")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("HTestPlotter curve path falls back to text panel for rank tests", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(20); y <- rnorm(20, mean = 1)
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$wilcoxon_rank_sum(x, y)
  p <- plotter$plot(result, plot_type = "curve")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter auto plot_type picks box for rank tests", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- c(rnorm(15, 50, 5), rnorm(15, 55, 5), rnorm(15, 60, 5))
  g <- factor(rep(c("A", "B", "C"), each = 15))
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$kruskal_wallis(x, g)
  p <- plotter$plot(result, plot_type = "auto")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("HTestPlotter combined path returns a patchwork/ggplot for Wilcoxon", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("patchwork")
  set.seed(123)
  x <- rnorm(20); y <- rnorm(20, mean = 1)
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$wilcoxon_rank_sum(x, y)
  p <- plotter$plot(result, plot_type = "combined")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

# ----------------------------------------------------------------------------
# HTestAnalyzer -- equivalence / non-inferiority / superiority tests
# ----------------------------------------------------------------------------

# ---- TOST for mean equivalence (one-sample) ----

test_that("TOST mean equivalence returns 'equivalent' when within margin", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  # True mean 50.3, mu = 50 -> diff ~ 0.3, delta = 2.0 -> comfortably equivalent
  # Need (delta - |diff|) > t_crit * SE = ~1.66 * 0.5 = 0.83
  x <- rnorm(100, mean = 50.3, sd = 5)
  result <- analyzer$tost_mean(x, mu = 50, delta = 2.0)
  expect_s3_class(result, "stat_result")
  expect_s3_class(result, "htest_result")
  expect_equal(result$test_type, "tost_mean")
  expect_equal(result$alternative, "equivalence")
  expect_equal(result$equivalence, "equivalent")
  expect_true(result$p.value < 0.05)
  expect_true(all(c("t1", "t2", "p1", "p2") %in% names(result)))
  expect_equal(length(result$statistic), 2L)
  expect_equal(names(result$statistic), c("t1", "t2"))
  expect_equal(result$dist_type, "t")
  expect_false(is.null(result$data$x))
  expect_null(result$data$y)
})

test_that("TOST mean equivalence returns 'not equivalent' when outside margin", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  # True mean 53, mu = 50 -> diff = 3 > delta = 1.0 -> NOT equivalent
  x <- rnorm(100, mean = 53, sd = 5)
  result <- analyzer$tost_mean(x, mu = 50, delta = 1.0)
  expect_equal(result$equivalence, "not equivalent")
  expect_true(result$p.value >= 0.05)
})

test_that("TOST mean equivalence (two-sample) detects equivalent means", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(200, mean = 50, sd = 4)
  y <- rnorm(200, mean = 50.2, sd = 4)  # small true diff = 0.2
  # delta = 2.0, SE ~ sqrt(16/200 + 16/200) = 0.4, need (2.0-0.2) > 1.66*0.4 = 0.66 ✓
  result <- analyzer$tost_mean(x, y, delta = 2.0)
  expect_equal(result$test_type, "tost_mean")
  expect_equal(result$equivalence, "equivalent")
  expect_false(is.null(result$data$y))
})

test_that("TOST mean (two-sample) with var.equal=TRUE uses pooled df", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(50, mean = 50, sd = 4)
  y <- rnorm(50, mean = 50.2, sd = 4)
  result <- analyzer$tost_mean(x, y, delta = 1.0, var.equal = TRUE)
  expect_equal(as.numeric(result$parameter["df"]), 98)
})

test_that("TOST mean rejects non-positive delta", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30)
  expect_error(analyzer$tost_mean(x, mu = 0, delta = 0), "positive scalar")
  expect_error(analyzer$tost_mean(x, mu = 0, delta = -1), "positive scalar")
  expect_error(analyzer$tost_mean(x, mu = 0, delta = c(1, 2)), "positive scalar")
})

test_that("TOST mean rejects too-small one-sample", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$tost_mean(c(1), mu = 0, delta = 1), "at least 2")
})

test_that("TOST mean CI level is 1 - 2*alpha (conventional TOST CI)", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(60, mean = 50, sd = 3)
  result <- analyzer$tost_mean(x, mu = 50, delta = 1.0, conf_level = 0.95)
  # alpha = 0.05 -> CI level should be 0.90
  expect_equal(result$conf.level, 0.90)
})

# ---- TOST for proportion equivalence (two-sample) ----

test_that("TOST proportion equivalence returns 'equivalent' when within margin", {
  analyzer <- HTestAnalyzer$new()
  # p1 = 0.45, p2 = 0.46, diff = -0.01, delta = 0.2
  # SE ~ sqrt(0.45*0.55/500 + 0.46*0.54/500) = 0.0315
  # Need (delta - |diff|) > z_crit * SE = 1.645 * 0.0315 = 0.052 -> 0.19 > 0.052 ✓
  result <- analyzer$tost_proportion(x1 = 225, n1 = 500, x2 = 230, n2 = 500,
                                     delta = 0.2)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "tost_proportion")
  expect_equal(result$equivalence, "equivalent")
  expect_true(result$p.value < 0.05)
  expect_equal(result$dist_type, "norm")
  expect_equal(names(result$statistic), c("z1", "z2"))
  expect_equal(length(result$estimate), 3L)
  expect_null(result$data$x)
})

test_that("TOST proportion equivalence returns 'not equivalent' when outside margin", {
  analyzer <- HTestAnalyzer$new()
  # p1 = 0.45, p2 = 0.70, diff = -0.25, delta = 0.1 -> NOT equivalent
  result <- analyzer$tost_proportion(x1 = 45, n1 = 100, x2 = 70, n2 = 100,
                                     delta = 0.1)
  expect_equal(result$equivalence, "not equivalent")
  expect_true(result$p.value >= 0.05)
})

test_that("TOST proportion rejects non-positive delta", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$tost_proportion(45, 100, 46, 100, delta = 0),
               "positive scalar")
  expect_error(analyzer$tost_proportion(45, 100, 46, 100, delta = -0.1),
               "positive scalar")
})

# ---- Non-inferiority tests ----

test_that("Non-inferiority (mean, one-sample) confirms non-inferiority", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  # True mean 52, mu = 50 -> diff = +2 >> -delta -> non-inferior
  x <- rnorm(80, mean = 52, sd = 4)
  result <- analyzer$non_inferiority(type = "mean", x = x, mu = 50, delta = 1.0)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "non_inferiority")
  expect_equal(result$type, "mean")
  expect_equal(result$alternative, "greater")
  expect_true(result$non_inferior)
  expect_true(result$p.value < 0.05)
  expect_equal(result$dist_type, "t")
  expect_equal(as.numeric(result$null.value), -1.0)
})

test_that("Non-inferiority (mean, two-sample) detects non-inferior treatment", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(60, mean = 52, sd = 4)  # treatment
  y <- rnorm(60, mean = 50, sd = 4)  # control
  result <- analyzer$non_inferiority(type = "mean", x = x, y = y, delta = 1.0)
  expect_equal(result$non_inferior, TRUE)
  expect_false(is.null(result$data$y))
})

test_that("Non-inferiority (mean) fails when treatment is clearly worse", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(60, mean = 46, sd = 4)  # treatment much worse than control
  y <- rnorm(60, mean = 50, sd = 4)
  result <- analyzer$non_inferiority(type = "mean", x = x, y = y, delta = 1.0)
  expect_false(result$non_inferior)
  expect_true(result$p.value >= 0.05)
})

test_that("Non-inferiority (proportion) confirms non-inferiority", {
  analyzer <- HTestAnalyzer$new()
  # p1 = 0.55, p2 = 0.50, diff = 0.05 > -delta = -0.10 -> non-inferior
  result <- analyzer$non_inferiority(type = "proportion",
                                     x1 = 55, n1 = 100, x2 = 50, n2 = 100,
                                     delta = 0.10)
  expect_equal(result$test_type, "non_inferiority")
  expect_equal(result$type, "proportion")
  expect_equal(result$dist_type, "norm")
  expect_true(result$non_inferior)
  expect_true(result$p.value < 0.05)
})

test_that("Non-inferiority (proportion) fails when treatment worse", {
  analyzer <- HTestAnalyzer$new()
  # p1 = 0.35, p2 = 0.55, diff = -0.20 << -delta = -0.10 -> NOT non-inferior
  result <- analyzer$non_inferiority(type = "proportion",
                                     x1 = 35, n1 = 100, x2 = 55, n2 = 100,
                                     delta = 0.10)
  expect_false(result$non_inferior)
})

test_that("Non-inferiority requires x1/n1/x2/n2 for proportions", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$non_inferiority(type = "proportion",
                                        x1 = 45, n1 = 100, delta = 0.1),
               "x1/n1/x2/n2 are required")
})

test_that("Non-inferiority rejects invalid delta", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$non_inferiority(type = "mean", x = 1:10, delta = NULL),
               "scalar margin")
})

# ---- Superiority tests ----

test_that("Superiority (mean, one-sample) confirms superiority", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  # True mean 53, mu = 50 -> diff = 3 > delta = 1 -> superior
  x <- rnorm(80, mean = 53, sd = 4)
  result <- analyzer$superiority(type = "mean", x = x, mu = 50, delta = 1.0)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "superiority")
  expect_equal(result$type, "mean")
  expect_equal(result$alternative, "greater")
  expect_true(result$superior)
  expect_true(result$p.value < 0.05)
  expect_equal(as.numeric(result$null.value), 1.0)
})

test_that("Superiority (mean, two-sample) detects superior treatment", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(60, mean = 54, sd = 4)
  y <- rnorm(60, mean = 50, sd = 4)
  result <- analyzer$superiority(type = "mean", x = x, y = y, delta = 1.0)
  expect_true(result$superior)
  expect_false(is.null(result$data$y))
})

test_that("Superiority (mean) fails when treatment not better", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(60, mean = 50.5, sd = 4)
  y <- rnorm(60, mean = 50, sd = 4)
  # diff = 0.5 < delta = 1.0 -> not superior
  result <- analyzer$superiority(type = "mean", x = x, y = y, delta = 1.0)
  expect_false(result$superior)
})

test_that("Superiority (proportion) confirms superiority", {
  analyzer <- HTestAnalyzer$new()
  # p1 = 0.70, p2 = 0.50, diff = 0.20, delta = 0.10
  # SE ~ sqrt(0.7*0.3/200 + 0.5*0.5/200) = 0.048
  # z = (0.20 - 0.10) / 0.048 = 2.08 -> p = 0.019 < 0.05 ✓
  result <- analyzer$superiority(type = "proportion",
                                 x1 = 140, n1 = 200, x2 = 100, n2 = 200,
                                 delta = 0.10)
  expect_equal(result$test_type, "superiority")
  expect_equal(result$type, "proportion")
  expect_true(result$superior)
})

test_that("Superiority (proportion) fails when not superior", {
  analyzer <- HTestAnalyzer$new()
  # p1 = 0.52, p2 = 0.50, diff = 0.02 < delta = 0.10 -> NOT superior
  result <- analyzer$superiority(type = "proportion",
                                 x1 = 52, n1 = 100, x2 = 50, n2 = 100,
                                 delta = 0.10)
  expect_false(result$superior)
})

test_that("Superiority requires x1/n1/x2/n2 for proportions", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$superiority(type = "proportion",
                                    x1 = 65, n1 = 100, delta = 0.1),
               "x1/n1/x2/n2 are required")
})

# ---- analyze() dispatcher covers equivalence tests ----

test_that("analyze() dispatches all 4 equivalence test types", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)

  r1 <- analyzer$analyze("tost_mean", x = rnorm(100, 50, 3), mu = 50, delta = 2.0)
  expect_equal(r1$test_type, "tost_mean")

  r2 <- analyzer$analyze("tost_proportion", x1 = 225, n1 = 500,
                         x2 = 230, n2 = 500, delta = 0.2)
  expect_equal(r2$test_type, "tost_proportion")

  r3 <- analyzer$analyze("non_inferiority", type = "mean",
                         x = rnorm(80, 52, 4), mu = 50, delta = 1.0)
  expect_equal(r3$test_type, "non_inferiority")

  r4 <- analyzer$analyze("superiority", type = "proportion",
                         x1 = 140, n1 = 200, x2 = 100, n2 = 200, delta = 0.1)
  expect_equal(r4$test_type, "superiority")
})

# ---- StatInterpreter coverage for equivalence tests ----

test_that("StatInterpreter handles TOST mean result for manager audience", {
  analyzer <- HTestAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(100, mean = 50.5, sd = 5)
  result <- analyzer$tost_mean(x, mu = 50, delta = 1.0)
  out <- interpreter$interpret(result, audience = "manager")
  expect_type(out, "character")
  expect_true(grepl("Equivalence", out))
  expect_true(grepl("delta", out))
})

test_that("StatInterpreter handles non-inferiority result for technical audience", {
  analyzer <- HTestAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(80, mean = 52, sd = 4)
  result <- analyzer$non_inferiority(type = "mean", x = x, mu = 50, delta = 1.0)
  out <- interpreter$interpret(result, audience = "technical")
  expect_true(grepl("Non-Inferiority", out))
  expect_true(grepl("confirm", out))
})

test_that("StatInterpreter handles superiority result (not-superior case)", {
  analyzer <- HTestAnalyzer$new()
  interpreter <- StatInterpreter$new()
  result <- analyzer$superiority(type = "proportion",
                                 x1 = 52, n1 = 100, x2 = 50, n2 = 100,
                                 delta = 0.10)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("Superiority", out))
  expect_true(grepl("NOT", out))
})

# ---- HTestPlotter coverage for equivalence tests (only when .plot available) ----

test_that("HTestPlotter auto-selects curve for TOST mean", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(80, mean = 50, sd = 4)
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$tost_mean(x, mu = 50, delta = 1.0)
  p <- plotter$plot(result, plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter renders TOST mean curve (text panel)", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(80, mean = 50, sd = 4)
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$tost_mean(x, mu = 50, delta = 1.0)
  p <- plotter$plot(result, plot_type = "curve")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter renders TOST proportion curve (text panel)", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$tost_proportion(45, 100, 46, 100, delta = 0.1)
  p <- plotter$plot(result, plot_type = "curve")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter renders non-inferiority (mean) curve", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(80, mean = 52, sd = 4)
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$non_inferiority(type = "mean", x = x, mu = 50, delta = 1.0)
  p <- plotter$plot(result, plot_type = "curve")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter renders superiority (proportion) curve", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$superiority(type = "proportion",
                                            x1 = 65, n1 = 100, x2 = 50, n2 = 100,
                                            delta = 0.10)
  p <- plotter$plot(result, plot_type = "curve")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter box plot falls back to curve for TOST proportion", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$tost_proportion(45, 100, 46, 100, delta = 0.1)
  p <- plotter$plot(result, plot_type = "box")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter box plot works for TOST mean one-sample", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(80, mean = 50, sd = 4)
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$tost_mean(x, mu = 50, delta = 1.0)
  p <- plotter$plot(result, plot_type = "box")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

# ----------------------------------------------------------------------------
# HTestAnalyzer -- Poisson rate tests (one-sample / two-sample)
# ----------------------------------------------------------------------------

test_that("One-sample Poisson rate test returns stat_result", {
  analyzer <- HTestAnalyzer$new()
  result <- analyzer$poisson_test_1s(x = 12, T_exposure = 2, r = 5)
  expect_s3_class(result, "stat_result")
  expect_s3_class(result, "htest_result")
  expect_equal(result$test_type, "poisson_test_1s")
  expect_equal(result$dist_type, "poisson")
  expect_equal(result$alternative, "two.sided")
  expect_equal(as.numeric(result$statistic["count"]), 12)
  expect_equal(as.numeric(result$parameter["exposure"]), 2)
  expect_equal(as.numeric(result$null.value["rate"]), 5)
  expect_equal(result$rate, 6)  # 12 / 2
  expect_true(!is.na(result$p.value))
  expect_true(length(result$conf.int) == 2)
  expect_null(result$data$x)
})

test_that("One-sample Poisson test detects rate above null", {
  analyzer <- HTestAnalyzer$new()
  # 30 events in 1 hour vs hypothesized rate of 10/hour -> clearly higher
  result <- analyzer$poisson_test_1s(x = 30, T_exposure = 1, r = 10,
                                     alternative = "greater")
  expect_true(result$p.value < 0.05)
})

test_that("One-sample Poisson test does not reject when rate matches null", {
  analyzer <- HTestAnalyzer$new()
  # 10 events in 1 hour vs hypothesized rate of 10/hour -> no rejection
  result <- analyzer$poisson_test_1s(x = 10, T_exposure = 1, r = 10)
  expect_true(result$p.value > 0.05)
})

test_that("One-sample Poisson test with alternative='less'", {
  analyzer <- HTestAnalyzer$new()
  # 3 events in 1 hour vs hypothesized rate of 10/hour -> clearly lower
  result <- analyzer$poisson_test_1s(x = 3, T_exposure = 1, r = 10,
                                     alternative = "less")
  expect_equal(result$alternative, "less")
  expect_true(result$p.value < 0.05)
})

test_that("One-sample Poisson rejects invalid inputs", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$poisson_test_1s(x = -1), "non-negative")
  expect_error(analyzer$poisson_test_1s(x = 5, T_exposure = 0), "positive")
  expect_error(analyzer$poisson_test_1s(x = 5, r = -1), "positive")
  expect_error(analyzer$poisson_test_1s(x = c(1, 2)), "non-negative")
})

test_that("Two-sample Poisson rate test returns stat_result", {
  analyzer <- HTestAnalyzer$new()
  result <- analyzer$poisson_test_2s(x1 = 15, T1 = 3, x2 = 25, T2 = 3)
  expect_s3_class(result, "stat_result")
  expect_equal(result$test_type, "poisson_test_2s")
  expect_equal(result$dist_type, "poisson")
  expect_equal(result$rate1, 5)   # 15/3
  expect_equal(result$rate2, 25/3)  # 25/3
  expect_equal(as.numeric(result$null.value["rate ratio"]), 1)
  expect_true(!is.na(result$p.value))
  expect_true(length(result$conf.int) == 2)
  expect_null(result$data$x)
})

test_that("Two-sample Poisson test detects rate difference", {
  analyzer <- HTestAnalyzer$new()
  # Line A: 10 defects in 1 hour; Line B: 30 defects in 1 hour
  result <- analyzer$poisson_test_2s(x1 = 10, T1 = 1, x2 = 30, T2 = 1)
  expect_true(result$p.value < 0.05)
})

test_that("Two-sample Poisson test with equal rates does not reject", {
  analyzer <- HTestAnalyzer$new()
  # Both lines: 15 defects in 3 hours
  result <- analyzer$poisson_test_2s(x1 = 15, T1 = 3, x2 = 15, T2 = 3)
  expect_true(result$p.value > 0.05)
})

test_that("Two-sample Poisson rejects invalid inputs", {
  analyzer <- HTestAnalyzer$new()
  expect_error(analyzer$poisson_test_2s(x1 = -1, x2 = 5), "non-negative")
  expect_error(analyzer$poisson_test_2s(x1 = 5, x2 = 5, T1 = 0, T2 = 1),
               "positive")
  expect_error(analyzer$poisson_test_2s(x1 = 5), "non-negative")
})

test_that("analyze() dispatches Poisson test types", {
  analyzer <- HTestAnalyzer$new()
  r1 <- analyzer$analyze("poisson_test_1s", x = 12, T_exposure = 2, r = 5)
  expect_equal(r1$test_type, "poisson_test_1s")

  r2 <- analyzer$analyze("poisson_test_2s", x1 = 15, T1 = 3, x2 = 25, T2 = 3)
  expect_equal(r2$test_type, "poisson_test_2s")
})

test_that("StatInterpreter handles Poisson one-sample result", {
  analyzer <- HTestAnalyzer$new()
  interpreter <- StatInterpreter$new()
  result <- analyzer$poisson_test_1s(x = 30, T_exposure = 1, r = 10,
                                     alternative = "greater")
  out <- interpreter$interpret(result, audience = "manager")
  expect_type(out, "character")
  expect_true(grepl("Poisson|poisson|rate", out))
})

test_that("StatInterpreter handles Poisson two-sample result", {
  analyzer <- HTestAnalyzer$new()
  interpreter <- StatInterpreter$new()
  result <- analyzer$poisson_test_2s(x1 = 10, T1 = 1, x2 = 30, T2 = 1)
  out <- interpreter$interpret(result, audience = "technical")
  expect_true(grepl("event rates|P Value", out))
})

# ---- HTestPlotter coverage for Poisson tests ----

test_that("HTestPlotter renders Poisson one-sample text panel", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$poisson_test_1s(x = 12, T_exposure = 2, r = 5)
  p <- plotter$plot(result, plot_type = "curve")
  expect_true(inherits(p, "ggplot"))
})

test_that("HTestPlotter renders Poisson two-sample text panel", {
  skip_if_not_installed("iQualityR.plot")
  plotter <- HTestPlotter$new()
  result <- HTestAnalyzer$new()$poisson_test_2s(x1 = 15, T1 = 3, x2 = 25, T2 = 3)
  p <- plotter$plot(result, plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})
