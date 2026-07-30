# =============================================================================
# File: tests/testthat/test-distributions.R
# Description: Probability distribution module tests
#   - Distribution registry (dist_registry.R)
#   - Probability calculation (iqr_prob.R, Prob*.R)
#   - Distribution fitting (dist_fit.R)
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# Distribution registry (dist_registry.R)
# ----------------------------------------------------------------------------

test_that("Distribution registry exists", {
  expect_true(exists("DIST_REGISTRY"))
  expect_true(is.list(DIST_REGISTRY))
  expect_true(length(DIST_REGISTRY) > 0)
  expect_true("norm" %in% names(DIST_REGISTRY))
})

test_that("list_available_dists returns a data.frame", {
  dists <- list_available_dists()
  expect_s3_class(dists, "data.frame")
  expect_true(nrow(dists) > 0)
  expect_true("type" %in% names(dists))
  expect_true("norm" %in% dists$type)
})

test_that("get_dist_info returns distribution info", {
  info <- get_dist_info("norm")
  expect_type(info, "list")
  expect_true("description" %in% names(info))
})

# ----------------------------------------------------------------------------
# Probability calculation (prob_calc / iqr_prob)
# ----------------------------------------------------------------------------

test_that("prob_calc normal distribution (upper tail)", {
  result <- prob_calc(type = "norm", params = list(mean = 0, sd = 1),
                      x = 1.96, calc_type = "upper")
  expect_type(result, "list")
  expect_true(length(result) > 0)
  node_res <- result[[1]]
  expect_true("all_res" %in% names(node_res))
  expect_true("mode" %in% names(node_res))
  expect_equal(node_res$mode, "prob")
  p_val <- node_res$all_res[[1]]$result_p
  expect_gte(p_val, 0)
  expect_lte(p_val, 1)
  expect_lt(p_val, 0.05)
})

test_that("prob_calc binomial distribution", {
  result <- prob_calc(type = "binom", params = list(size = 10, prob = 0.5),
                      x = 5, calc_type = "lower")
  expect_type(result, "list")
  p_val <- result[[1]]$all_res[[1]]$result_p
  expect_gte(p_val, 0)
  expect_lte(p_val, 1)
})

test_that("iqr_prob R6 class entry", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  expect_true(inherits(prob, "iqr_prob"))
  expect_true(inherits(prob, "R6"))
  expect_true(length(prob$nodes) > 0)
})

test_that("iqr_prob calc + interpret chain", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  expect_false(is.null(prob$last_results))
  expect_equal(prob$last_results[[1]]$mode, "prob")
})

# ----------------------------------------------------------------------------
# Distribution fitting (dist_fit.R)
# ----------------------------------------------------------------------------

test_that("fit_distribution normal fit", {
  set.seed(123)
  x <- rnorm(100)
  result <- fit_distribution(x, "norm")

  expect_type(result, "list")
  expect_equal(result$dist, "norm")
  expect_true("params" %in% names(result))
  expect_true("AIC" %in% names(result))
  expect_true("BIC" %in% names(result))
})

test_that("fit_distribution exponential fit", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  result <- fit_distribution(x, "exp")

  expect_type(result, "list")
  expect_equal(result$dist, "exp")
  expect_gt(result$params$rate, 0)
})

test_that("auto_fit_distribution auto-select", {
  set.seed(123)
  x <- rnorm(100)
  result <- auto_fit_distribution(x)

  expect_type(result, "list")
  expect_true("best_dist" %in% names(result))
  expect_true("ranking" %in% names(result))
  expect_s3_class(result$ranking, "data.frame")
})

test_that("auto_fit_distribution positive-only data", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  result <- auto_fit_distribution(x, positive_only = TRUE)

  expect_type(result, "list")
  expect_true(result$best_dist %in% c("exp", "gamma", "weibull", "lnorm", "logis"))
})

test_that("compare_fits comparison", {
  set.seed(123)
  x <- rnorm(100)
  f1 <- fit_distribution(x, "norm")
  f2 <- fit_distribution(x, "logis")
  result <- compare_fits(list(norm = f1, logis = f2))

  expect_type(result, "list")
  expect_true("test_results" %in% names(result))
  expect_true("best_by_ks" %in% names(result))
})

test_that("empirical_distribution", {
  set.seed(123)
  x <- rnorm(100)
  result <- empirical_distribution(x)

  expect_type(result, "list")
  expect_true("ecdf" %in% names(result))
  expect_true("points" %in% names(result))
})

test_that("calc_qq_data QQ data", {
  set.seed(123)
  x <- rnorm(100)
  result <- calc_qq_data(x, "norm", list(mean = 0, sd = 1))

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_true("theoretical" %in% names(result))
  expect_true("sample" %in% names(result))
})
