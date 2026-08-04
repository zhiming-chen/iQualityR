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

# ----------------------------------------------------------------------------
# Data transformation -- additional coverage (R2-7e)
# ----------------------------------------------------------------------------

test_that("box_cox_transform with explicit lambda (optimize=FALSE)", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  result <- box_cox_transform(x, lambda = 0.5, optimize = FALSE)
  expect_equal(result$lambda, 0.5)
  expect_equal(length(result$transformed), length(x))
})

test_that("box_cox_transform errors on non-positive data", {
  expect_error(box_cox_transform(c(1, 2, -1, 3)), "positive")
})

test_that("box_cox_transform errors on too few values", {
  expect_error(box_cox_transform(c(1, 2)), "3 non-missing")
})

test_that("yeo_johnson_transform with explicit lambda", {
  set.seed(123)
  x <- rnorm(50, mean = -1, sd = 2)
  result <- yeo_johnson_transform(x, lambda = 1, optimize = FALSE)
  expect_equal(result$lambda, 1)
  expect_equal(length(result$transformed), length(x))
})

test_that("yeo_johnson_transform handles all-negative data", {
  x <- c(-5, -4, -3, -2, -1)
  result <- yeo_johnson_transform(x, lambda = 0.5, optimize = FALSE)
  expect_equal(length(result$transformed), 5)
})

test_that("johnson_transform requires SuppDists", {
  skip_if_not_installed("SuppDists")
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  result <- johnson_transform(x)
  expect_type(result, "list")
  expect_true("transformed" %in% names(result))
  expect_true("type" %in% names(result))
})

test_that("log_transform with base 10 and 2", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  res10 <- log_transform(x, base = "10")
  res2  <- log_transform(x, base = "2")
  expect_equal(res10$base, "10")
  expect_equal(res2$base, "2")
  # log10(x) = log(x) / log(10)
  expect_equal(res10$transformed[1], log10(x[1]))
})

test_that("log_transform with add_constant for zero data", {
  x <- c(0, 1, 2, 3, 4)
  result <- log_transform(x, add_constant = 1)
  expect_equal(length(result$transformed), 5)
  expect_equal(result$add_constant, 1)
})

test_that("log_transform errors on non-positive without add_constant", {
  expect_error(log_transform(c(-1, 1, 2)), "non-positive")
})

test_that("sqrt_transform with add_constant for negative data", {
  x <- c(-2, -1, 0, 1, 2)
  result <- sqrt_transform(x, add_constant = 2)
  expect_equal(length(result$transformed), 5)
})

test_that("sqrt_transform errors on negative data without add_constant", {
  expect_error(sqrt_transform(c(-1, 0, 1)), "negative")
})

test_that("reciprocal_transform errors on zero values", {
  expect_error(reciprocal_transform(c(0, 1, 2)), "zero")
})

test_that("auto_transform with criterion='ad' selects best method", {
  skip_if_not_installed("nortest")
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  result <- auto_transform(x, criterion = "ad")
  expect_equal(result$criterion, "ad")
  expect_true(nchar(result$best_method) > 0)
})

test_that("inverse_transform round-trips log transform", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  res <- log_transform(x, base = "natural")
  restored <- inverse_transform(res$transformed, res$method,
                                list(base = "natural", add_constant = 0))
  expect_equal(restored, x, tolerance = 1e-8)
})

test_that("inverse_transform round-trips sqrt transform", {
  set.seed(123)
  x <- rpois(100, lambda = 5)
  res <- sqrt_transform(x)
  restored <- inverse_transform(res$transformed, res$method,
                                list(add_constant = 0))
  expect_equal(restored, x, tolerance = 1e-8)
})

test_that("inverse_transform round-trips reciprocal transform", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 1)
  res <- reciprocal_transform(x)
  restored <- inverse_transform(res$transformed, res$method, list())
  expect_equal(restored, x, tolerance = 1e-8)
})

test_that("inverse_transform round-trips yeo-johnson transform", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 1)
  res <- yeo_johnson_transform(x, lambda = 0.5, optimize = FALSE)
  restored <- inverse_transform(res$transformed, res$method,
                                list(lambda = 0.5))
  expect_equal(restored, x, tolerance = 1e-6)
})

test_that("inverse_transform errors on unknown method", {
  expect_error(inverse_transform(c(1, 2), "Unknown Method", list()),
               "Unknown transform method")
})

# ----------------------------------------------------------------------------
# Outlier detection -- additional coverage (R2-7e)
# ----------------------------------------------------------------------------

test_that("detect_outliers_iqr with k=3 detects fewer outliers", {
  set.seed(123)
  x <- c(rnorm(100), 10, -10)
  res_k15 <- detect_outliers_iqr(x, k = 1.5)
  res_k3  <- detect_outliers_iqr(x, k = 3)
  expect_lte(res_k3$n_outliers, res_k15$n_outliers)
})

test_that("detect_outliers_iqr handles NA values", {
  x <- c(1, 2, NA, 4, 100, 3)
  result <- detect_outliers_iqr(x, na.rm = TRUE)
  expect_true(result$n_total == 5)  # NA removed
})

test_that("detect_outliers_zscore with robust=TRUE uses MAD", {
  set.seed(123)
  x <- c(rnorm(100), 10, -10)
  result <- detect_outliers_zscore(x, robust = TRUE)
  expect_equal(result$method, "Z-Score (Robust)")
  expect_true("z_scores" %in% names(result))
})

test_that("detect_outliers_zscore handles constant data (scale=0)", {
  x <- rep(5, 10)
  result <- detect_outliers_zscore(x)
  expect_equal(result$n_outliers, 0)
  expect_equal(result$proportion, 0)
})

test_that("detect_outliers_grubbs with type='min'", {
  set.seed(123)
  x <- c(rnorm(100), -10)  # clear low outlier
  result <- detect_outliers_grubbs(x, type = "min")
  expect_true("G" %in% names(result))
  expect_true(result$is_outlier)
})

test_that("detect_outliers_grubbs with type='max'", {
  set.seed(123)
  x <- c(rnorm(100), 10)  # clear high outlier
  result <- detect_outliers_grubbs(x, type = "max")
  expect_true("G" %in% names(result))
  expect_true(result$is_outlier)
})

test_that("detect_outliers_grubbs handles constant data (sd=0)", {
  x <- rep(5, 10)
  result <- detect_outliers_grubbs(x)
  expect_false(result$is_outlier)
  expect_equal(result$p.value, 1)
})

test_that("detect_outliers_grubbs errors on n < 3", {
  expect_error(detect_outliers_grubbs(c(1, 2)), "at least 3")
})

test_that("detect_outliers_dixon with type='min'", {
  set.seed(123)
  x <- c(rnorm(20), -10)
  result <- detect_outliers_dixon(x, type = "min")
  expect_true("results" %in% names(result))
  expect_true("min" %in% names(result$results))
})

test_that("detect_outliers_dixon with type='max'", {
  set.seed(123)
  x <- c(rnorm(20), 10)
  result <- detect_outliers_dixon(x, type = "max")
  expect_true("max" %in% names(result$results))
})

test_that("detect_outliers_dixon errors on n > 30", {
  x <- rnorm(31)
  expect_error(detect_outliers_dixon(x), "3 <= n <= 30")
})

test_that("detect_outliers_dixon errors on n < 3", {
  expect_error(detect_outliers_dixon(c(1, 2)), "3 <= n <= 30")
})

test_that("detect_outliers_mahalanobis detects multivariate outliers", {
  set.seed(123)
  df <- data.frame(x = rnorm(100), y = rnorm(100))
  df <- rbind(df, c(10, 10))  # clear outlier
  result <- detect_outliers_mahalanobis(df)
  expect_type(result, "list")
  expect_true("mahalanobis_dist" %in% names(result))
  expect_true("critical_value" %in% names(result))
  expect_gte(result$n_outliers, 1)
})

test_that("detect_outliers_mahalanobis errors when n <= p", {
  df <- data.frame(x = c(1, 2), y = c(3, 4))
  expect_error(detect_outliers_mahalanobis(df), "more observations than variables")
})

test_that("detect_outliers_all includes summary data.frame", {
  set.seed(123)
  x <- c(rnorm(100), 10, -10)
  result <- detect_outliers_all(x)
  expect_s3_class(result$summary, "data.frame")
  expect_true("method" %in% names(result$summary))
  expect_true("n_outliers" %in% names(result$summary))
})

test_that("detect_outliers_all with dixon method for small samples", {
  set.seed(123)
  x <- c(rnorm(20), 10)
  result <- detect_outliers_all(x, methods = c("iqr", "zscore", "dixon"))
  expect_true("dixon" %in% names(result$all_results))
})
