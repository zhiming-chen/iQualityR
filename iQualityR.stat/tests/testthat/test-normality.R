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
