# =============================================================================
# File: tests/testthat/test-normality.R
# Description: Normality test module tests
#   - NormalityAnalyzer
#   - NormalityPlotter
#   - NormalityReporter
#   - iqr_normality
#   - Convenience functions (normality_test / normality_interpret)
# =============================================================================

library(testthat)
library(iQualityR.stat)

test_that("NormalityAnalyzer initialization", {
  analyzer <- NormalityAnalyzer$new()
  expect_true(inherits(analyzer, "NormalityAnalyzer"))
  expect_true(inherits(analyzer, "R6"))
})

test_that("NormalityAnalyzer Shapiro-Wilk test", {
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- rnorm(50)
  result <- analyzer$test(x, method = "sw")

  expect_type(result, "list")
  expect_true("p.value" %in% names(result))
  expect_true("is_normal" %in% names(result))
  expect_equal(result$method, "Shapiro-Wilk")
})

test_that("NormalityAnalyzer auto-select method", {
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- rnorm(100)
  result <- analyzer$test(x, method = "auto")

  expect_type(result, "list")
  expect_true("method" %in% names(result))
})

test_that("NormalityPlotter initialization", {
  plotter <- NormalityPlotter$new()
  expect_true(inherits(plotter, "NormalityPlotter"))
  expect_true(inherits(plotter, "R6"))
})

test_that("NormalityReporter initialization", {
  reporter <- NormalityReporter$new()
  expect_true(inherits(reporter, "NormalityReporter"))
  expect_true(inherits(reporter, "R6"))
})

test_that("iqr_normality R6 class entry", {
  normality <- iqr_normality$new()
  expect_true(inherits(normality, "iqr_normality"))
  expect_true(inherits(normality, "R6"))
})

test_that("iqr_normality test method", {
  set.seed(123)
  x <- rnorm(50)
  normality <- iqr_normality$new()
  normality$test(x, method = "sw")
  expect_false(is.null(normality$last_results))
  expect_true("p.value" %in% names(normality$last_results))
})

test_that("normality_test convenience function", {
  set.seed(123)
  x <- rnorm(50)
  result <- normality_test(x, method = "sw")

  expect_type(result, "list")
  expect_true("p.value" %in% names(result))
  expect_true("is_normal" %in% names(result))
})

test_that("normality_interpret convenience function", {
  set.seed(123)
  x <- rnorm(50)
  interp <- normality_interpret(x, method = "sw", audience = "manager")

  expect_type(interp, "character")
  expect_true(nchar(interp) > 0)
})

# ----------------------------------------------------------------------------
# NormalityAnalyzer -- all 5 methods (R2-7b)
# ----------------------------------------------------------------------------

test_that("NormalityAnalyzer Anderson-Darling test", {
  skip_if_not_installed("nortest")
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- rnorm(100)
  result <- analyzer$test(x, method = "ad")

  expect_type(result, "list")
  expect_equal(result$method, "Anderson-Darling")
  expect_true("p.value" %in% names(result))
  expect_true("is_normal" %in% names(result))
  expect_true("statistic" %in% names(result))
  expect_equal(result$n, 100L)
})

test_that("NormalityAnalyzer Lilliefors test", {
  skip_if_not_installed("nortest")
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- rnorm(100)
  result <- analyzer$test(x, method = "lillie")

  expect_equal(result$method, "Lilliefors (Kolmogorov-Smirnov)")
  expect_true("p.value" %in% names(result))
  expect_gte(result$p.value, 0)
  expect_lte(result$p.value, 1)
})

test_that("NormalityAnalyzer Cramer-von Mises test", {
  skip_if_not_installed("nortest")
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- rnorm(100)
  result <- analyzer$test(x, method = "cvm")

  expect_equal(result$method, "Cramer-von Mises")
  expect_true("p.value" %in% names(result))
})

test_that("NormalityAnalyzer Shapiro-Francia test", {
  skip_if_not_installed("nortest")
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- rnorm(100)
  result <- analyzer$test(x, method = "sf")

  expect_equal(result$method, "Shapiro-Francia")
  expect_true("p.value" %in% names(result))
})

test_that("NormalityAnalyzer auto-selects sw for small n", {
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- rnorm(50)
  result <- analyzer$test(x, method = "auto")
  # n <= 5000 -> Shapiro-Wilk
  expect_equal(result$method, "Shapiro-Wilk")
})

test_that("NormalityAnalyzer auto-selects ad for large n", {
  skip_if_not_installed("nortest")
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  # n > 5000 -> Anderson-Darling
  x <- rnorm(5001)
  result <- analyzer$test(x, method = "auto")
  expect_equal(result$method, "Anderson-Darling")
  expect_equal(result$n, 5001L)
})

test_that("NormalityAnalyzer errors when n < 3", {
  analyzer <- NormalityAnalyzer$new()
  expect_error(analyzer$test(c(1, 2)), "at least 3")
})

test_that("NormalityAnalyzer filters NA values", {
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- c(rnorm(50), NA, NA)
  result <- analyzer$test(x, method = "sw")
  expect_equal(result$n, 50L)
})

test_that("NormalityAnalyzer detects non-normal data", {
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  # Exponential data is heavily right-skewed -> not normal
  x <- rexp(200, rate = 1)
  result <- analyzer$test(x, method = "sw")
  expect_false(result$is_normal)
  expect_lt(result$p.value, 0.05)
})

test_that("NormalityAnalyzer respects custom alpha", {
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- rnorm(50)
  res_05 <- analyzer$test(x, method = "sw", alpha = 0.05)
  res_01 <- analyzer$test(x, method = "sw", alpha = 0.01)
  expect_equal(res_05$alpha, 0.05)
  expect_equal(res_01$alpha, 0.01)
  # Same p-value, but decision boundary differs
  expect_equal(res_05$p.value, res_01$p.value)
})

test_that("NormalityAnalyzer errors on unknown method", {
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- rnorm(50)
  # match.arg throws on invalid method
  expect_error(analyzer$test(x, method = "nope"))
})

# ----------------------------------------------------------------------------
# NormalityAnalyzer -- test_multiple / diagnose (R2-7b)
# ----------------------------------------------------------------------------

test_that("NormalityAnalyzer test_multiple processes data.frame columns", {
  skip_if_not_installed("nortest")
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  df <- data.frame(
    a = rnorm(50),
    b = rnorm(50),
    c = sample(letters[1:3], 50, replace = TRUE)  # non-numeric, should be skipped
  )
  results <- analyzer$test_multiple(df, method = "sw")
  expect_true("a" %in% names(results))
  expect_true("b" %in% names(results))
  expect_false("c" %in% names(results))
  expect_equal(results$a$variable, "a")
})

test_that("NormalityAnalyzer test_multiple errors on non-data.frame", {
  analyzer <- NormalityAnalyzer$new()
  expect_error(analyzer$test_multiple(1:10), "data frame")
})

test_that("NormalityAnalyzer test_multiple respects vars argument", {
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  df <- data.frame(a = rnorm(50), b = rnorm(50), d = rnorm(50))
  results <- analyzer$test_multiple(df, vars = c("a", "d"), method = "sw")
  expect_true("a" %in% names(results))
  expect_true("d" %in% names(results))
  expect_false("b" %in% names(results))
})

test_that("NormalityAnalyzer diagnose returns descriptive statistics", {
  skip_if_not_installed("moments")
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- rnorm(100)
  diag <- analyzer$diagnose(x)

  expect_type(diag, "list")
  expect_true(all(c("n", "mean", "sd", "median", "skewness", "kurtosis",
                    "excess_kurtosis", "skewness_direction", "kurtosis_type",
                    "min", "max", "range", "q1", "q3", "iqr") %in% names(diag)))
  expect_equal(diag$n, 100L)
})

test_that("NormalityAnalyzer diagnose classifies skewness direction", {
  skip_if_not_installed("moments")
  analyzer <- NormalityAnalyzer$new()
  # Right-skewed data (exponential)
  set.seed(123)
  x <- rexp(500, rate = 1)
  diag <- analyzer$diagnose(x)
  expect_true(grepl("Right-skewed", diag$skewness_direction))
  # Exponential has excess kurtosis = 6 (leptokurtic)
  expect_true(grepl("Leptokurtic", diag$kurtosis_type))
})

test_that("NormalityAnalyzer diagnose classifies symmetric data", {
  skip_if_not_installed("moments")
  analyzer <- NormalityAnalyzer$new()
  set.seed(123)
  x <- rnorm(2000)
  diag <- analyzer$diagnose(x)
  expect_true(grepl("symmetric", diag$skewness_direction, ignore.case = TRUE))
})

test_that("NormalityAnalyzer diagnose errors when n < 4", {
  analyzer <- NormalityAnalyzer$new()
  expect_error(analyzer$diagnose(c(1, 2, 3)), "at least 4")
})

# ----------------------------------------------------------------------------
# iqr_normality L3 integrator -- full method surface (R2-7b)
# ----------------------------------------------------------------------------

test_that("iqr_normality test caches data and diagnose", {
  set.seed(123)
  x <- rnorm(50)
  normality <- iqr_normality$new()
  normality$test(x, method = "sw")
  expect_false(is.null(normality$last_results))
  expect_false(is.null(normality$last_data))
  expect_false(is.null(normality$last_diagnose))
  expect_equal(normality$last_data, x)
})

test_that("iqr_normality test_multiple caches results", {
  set.seed(123)
  df <- data.frame(a = rnorm(50), b = rnorm(50))
  normality <- iqr_normality$new()
  results <- normality$test_multiple(df, method = "sw")
  expect_false(is.null(normality$last_results))
  expect_true("a" %in% names(results))
  expect_true("b" %in% names(results))
})

test_that("iqr_normality interpret after test", {
  set.seed(123)
  x <- rnorm(50)
  normality <- iqr_normality$new()
  normality$test(x, method = "sw")
  interp <- normality$interpret(audience = "technical")
  expect_type(interp, "character")
  expect_true(nchar(interp) > 0)
})

test_that("iqr_normality interpret errors before test", {
  normality <- iqr_normality$new()
  expect_error(normality$interpret(), "No results")
})

test_that("iqr_normality interpret accepts explicit result", {
  set.seed(123)
  x <- rnorm(50)
  analyzer <- NormalityAnalyzer$new()
  res <- analyzer$test(x, method = "sw")
  normality <- iqr_normality$new()
  interp <- normality$interpret(result = res, audience = "manager")
  expect_type(interp, "character")
})

test_that("iqr_normality plot errors before test (no cached data)", {
  normality <- iqr_normality$new()
  expect_error(normality$plot(), "No data")
})

test_that("iqr_normality to_dataframe after test", {
  set.seed(123)
  x <- rnorm(50)
  normality <- iqr_normality$new()
  normality$test(x, method = "sw")
  df <- normality$to_dataframe()
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) >= 1)
})

test_that("iqr_normality to_dataframe errors before test", {
  normality <- iqr_normality$new()
  expect_error(normality$to_dataframe(), "No results")
})

test_that("iqr_normality to_excel errors before test", {
  normality <- iqr_normality$new()
  expect_error(normality$to_excel(), "No results")
})

test_that("iqr_normality with custom theme object", {
  skip_if_not_installed("iQualityR.core")
  theme_obj <- iQualityR.core::IqrTheme$new("academic")
  normality <- iqr_normality$new(theme = theme_obj)
  expect_false(is.null(normality$theme_obj))
})

# ----------------------------------------------------------------------------
# Convenience functions -- full surface (R2-7b)
# ----------------------------------------------------------------------------

test_that("normality_test with interpret = TRUE", {
  set.seed(123)
  x <- rnorm(50)
  result <- normality_test(x, method = "sw", interpret = TRUE, audience = "manager")
  expect_type(result, "list")
  expect_true("p.value" %in% names(result))
})

test_that("normality_test detects non-normal data", {
  set.seed(123)
  x <- rexp(200, rate = 1)
  result <- normality_test(x, method = "sw")
  expect_false(result$is_normal)
})

test_that("normality_test respects alpha", {
  set.seed(123)
  x <- rnorm(50)
  res <- normality_test(x, method = "sw", alpha = 0.10)
  expect_equal(res$alpha, 0.10)
})

# ----------------------------------------------------------------------------
# NormalityReporter -- L2 report engine (R2-7d)
# ----------------------------------------------------------------------------

test_that("NormalityReporter$print_console prints a single result", {
  set.seed(123)
  x <- rnorm(50)
  result <- NormalityAnalyzer$new()$test(x, method = "sw")
  reporter <- NormalityReporter$new()
  out <- capture.output(reporter$print_console(result, interpret = FALSE))
  expect_true(length(out) > 0L)
  expect_true(any(grepl("Normality Test", out)))
  expect_true(any(grepl("P Value", out)))
})

test_that("NormalityReporter$print_console includes diagnose when provided", {
  set.seed(123)
  x <- rnorm(50)
  result <- NormalityAnalyzer$new()$test(x, method = "sw")
  diagnose <- list(skewness_direction = "right-skewed", kurtosis_type = "leptokurtic")
  reporter <- NormalityReporter$new()
  out <- capture.output(reporter$print_console(result, diagnose = diagnose, interpret = FALSE))
  expect_true(any(grepl("Distribution Shape Diagnosis", out)))
  expect_true(any(grepl("right-skewed", out)))
})

test_that("NormalityReporter$to_dataframe converts a single result", {
  set.seed(123)
  x <- rnorm(50)
  result <- NormalityAnalyzer$new()$test(x, method = "sw")
  reporter <- NormalityReporter$new()
  df <- reporter$to_dataframe(result)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
  expect_true("Method" %in% names(df))
  expect_true("P_Value" %in% names(df))
  expect_true("Is_Normal" %in% names(df))
})

test_that("NormalityReporter$to_dataframe converts batch results", {
  set.seed(123)
  df_in <- data.frame(a = rnorm(50), b = rnorm(50))
  normality <- iqr_normality$new()
  results <- normality$test_multiple(df_in, method = "sw")
  reporter <- NormalityReporter$new()
  df <- reporter$to_dataframe(results)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_true("a" %in% df$Variable)
  expect_true("b" %in% df$Variable)
})

test_that("NormalityReporter$to_dataframe handles error results", {
  error_results <- list(
    BadVar = list(error = "Insufficient data")
  )
  reporter <- NormalityReporter$new()
  df <- reporter$to_dataframe(error_results)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
  expect_equal(df$Method[1], "error")
  expect_equal(df$Error[1], "Insufficient data")
})

test_that("NormalityReporter$to_dataframe returns empty data.frame for empty input", {
  reporter <- NormalityReporter$new()
  df <- reporter$to_dataframe(list())
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
})

test_that("NormalityReporter$to_excel writes an xlsx file", {
  skip_if_not_installed("iQualityR.core")
  set.seed(123)
  x <- rnorm(50)
  result <- NormalityAnalyzer$new()$test(x, method = "sw")
  reporter <- NormalityReporter$new()
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf), add = TRUE)
  config <- iQualityR.core::IqrTheme$new("academic")$config
  exporter <- iQualityR.core::ExcelExporter$new(config)
  reporter$to_excel(result, path = tf, excel_exporter = exporter)
  expect_true(file.exists(tf))
})

test_that("NormalityReporter$to_excel errors without excel_exporter", {
  set.seed(123)
  x <- rnorm(50)
  result <- NormalityAnalyzer$new()$test(x, method = "sw")
  reporter <- NormalityReporter$new()
  expect_error(reporter$to_excel(result, excel_exporter = NULL), "excel_exporter")
})
