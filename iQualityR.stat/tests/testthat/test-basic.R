# =============================================================================
# File: tests/testthat/test-basic.R
# Description: Basic statistics module tests
#   - Descriptive statistics (desc.R)
#   - Data transformation (transform.R)
#   - Outlier detection (outlier.R)
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# Descriptive statistics (desc.R)
# ----------------------------------------------------------------------------

test_that("desc_calc single variable computation", {
  set.seed(123)
  x <- rnorm(100, mean = 50, sd = 5)
  result <- desc_calc(x)

  expect_type(result, "list")
  expect_true("n" %in% names(result))
  expect_true("mean" %in% names(result))
  expect_true("stdev" %in% names(result))
  expect_equal(result$n, 100)
  expect_equal(result$mean, mean(x))
  expect_equal(result$stdev, sd(x))
})

test_that("desc_analyze batch analysis", {
  set.seed(123)
  data <- data.frame(
    x = rnorm(100, 50, 5),
    y = rnorm(100, 30, 3),
    z = runif(100, 0, 100)
  )
  results <- desc_analyze(data, vars = c("x", "y"))

  expect_type(results, "list")
  expect_equal(length(results), 2)
})

test_that("desc_summary_table generates summary table", {
  set.seed(123)
  data <- data.frame(x = rnorm(50), y = rnorm(50))
  results <- desc_analyze(data)
  summary_df <- desc_summary_table(results)

  expect_s3_class(summary_df, "data.frame")
  expect_true(nrow(summary_df) >= 2)
})

# ----------------------------------------------------------------------------
# Descriptive statistics plotting (desc.R) -- requires iQualityR.plot
# ----------------------------------------------------------------------------

test_that("desc_hist returns a ggplot object", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  s <- desc_calc(x)
  s$var_name <- "TestVar"
  p <- desc_hist(s, theme = "academic")
  expect_true(inherits(p, "ggplot"))
})

test_that("desc_box returns a ggplot object", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  s <- desc_calc(x)
  s$var_name <- "TestVar"
  p <- desc_box(s, theme = "academic")
  expect_true(inherits(p, "ggplot"))
})

test_that("desc_box_with_stats returns a ggplot object", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  s <- desc_calc(x)
  p <- desc_box_with_stats(s, theme = "academic")
  expect_true(inherits(p, "ggplot"))
})

test_that("desc_plot returns a patchwork object", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  s <- desc_calc(x)
  s$var_name <- "TestVar"
  p <- desc_plot(s, theme = "academic")
  expect_true(inherits(p, "patchwork"))
})

test_that("desc_stats_table returns a ggplot object", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  s <- desc_calc(x)
  p <- desc_stats_table(s, theme = "academic")
  expect_true(inherits(p, "ggplot"))
})

test_that("desc_hist works with custom theme object", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("iQualityR.core")
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  s <- desc_calc(x)
  theme_obj <- iQualityR.core::IqrTheme$new("academic")
  p <- desc_hist(s, theme = theme_obj)
  expect_true(inherits(p, "ggplot"))
})

test_that("iqr_desc quick entry returns invisible results list", {
  skip_if_not_installed("iQualityR.plot")
  set.seed(123)
  data <- data.frame(Val = rnorm(50, 100, 5))
  results <- iqr_desc(data, conf = 0.95)
  expect_type(results, "list")
  expect_true("Val" %in% names(results))
})

# ----------------------------------------------------------------------------
# Data transformation (transform.R)
# ----------------------------------------------------------------------------

test_that("box_cox_transform Box-Cox transformation", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  result <- box_cox_transform(x)

  expect_type(result, "list")
  expect_true("transformed" %in% names(result))
  expect_true("lambda" %in% names(result))
  expect_equal(result$method, "Box-Cox")
})

test_that("yeo_johnson_transform Yeo-Johnson transformation", {
  set.seed(123)
  x <- rnorm(100, mean = -1, sd = 2)
  result <- yeo_johnson_transform(x)

  expect_type(result, "list")
  expect_true("transformed" %in% names(result))
  expect_true("lambda" %in% names(result))
  expect_equal(result$method, "Yeo-Johnson")
})

test_that("log_transform log transformation", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  result <- log_transform(x, base = "natural")

  expect_type(result, "list")
  expect_true("transformed" %in% names(result))
  expect_equal(result$base, "natural")
})

test_that("sqrt_transform square root transformation", {
  set.seed(123)
  x <- rpois(100, lambda = 5)
  result <- sqrt_transform(x)

  expect_type(result, "list")
  expect_true("transformed" %in% names(result))
  expect_equal(result$method, "Square Root")
})

test_that("reciprocal_transform reciprocal transformation", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 1)
  result <- reciprocal_transform(x)

  expect_type(result, "list")
  expect_true("transformed" %in% names(result))
  expect_equal(result$method, "Reciprocal")
})

test_that("auto_transform auto-select transformation", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  result <- auto_transform(x)

  expect_type(result, "list")
  expect_true("best_method" %in% names(result))
  expect_true("best_result" %in% names(result))
})

test_that("inverse_transform inverse transformation", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  result <- box_cox_transform(x)
  original <- inverse_transform(result$transformed, result$method,
                                list(lambda = result$lambda))

  expect_type(original, "double")
  expect_equal(length(original), length(x))
})

# ----------------------------------------------------------------------------
# Outlier detection (outlier.R)
# ----------------------------------------------------------------------------

test_that("detect_outliers_iqr IQR method", {
  set.seed(123)
  x <- c(rnorm(100), 10, -10)
  result <- detect_outliers_iqr(x)

  expect_type(result, "list")
  expect_true("outliers" %in% names(result))
  expect_true("n_outliers" %in% names(result))
  expect_gte(result$n_outliers, 0)
})

test_that("detect_outliers_zscore Z-Score method", {
  set.seed(123)
  x <- c(rnorm(100), 10, -10)
  result <- detect_outliers_zscore(x)

  expect_type(result, "list")
  expect_true("outliers" %in% names(result))
  expect_true("threshold" %in% names(result))
})

test_that("detect_outliers_grubbs Grubbs method", {
  set.seed(123)
  x <- c(rnorm(100), 10)
  result <- detect_outliers_grubbs(x)

  expect_type(result, "list")
  expect_true("outlier" %in% names(result))
  expect_true("p.value" %in% names(result))
})

test_that("detect_outliers_dixon Dixon method", {
  set.seed(123)
  x <- c(rnorm(20), 10)
  result <- detect_outliers_dixon(x)

  expect_type(result, "list")
  expect_true("outlier" %in% names(result))
})

test_that("detect_outliers_all consensus method", {
  set.seed(123)
  x <- c(rnorm(100), 10, -10)
  result <- detect_outliers_all(x)

  expect_type(result, "list")
  expect_true("consensus" %in% names(result))
  expect_true("all_results" %in% names(result))
})
