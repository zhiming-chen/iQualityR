# =============================================================================
# File: tests/testthat/test-dist-fit.R
# Description: Tests for the distribution fitting module (R/dist_fit.R).
#   Covers fit_distribution, auto_fit_distribution, compare_fits,
#   empirical_distribution and calc_qq_data across distribution families,
#   goodness-of-fit helpers and error paths.
# =============================================================================

library(testthat)
library(iQualityR.stat)

# Small helper: assert a fit result has the standard structure returned by
# every private_fit_* helper.
expect_valid_fit <- function(fit, dist_name, n_expected) {
  expect_type(fit, "list")
  expect_equal(fit$dist, dist_name)
  expect_type(fit$params, "list")
  expect_true(is.finite(fit$logLik))
  expect_true(is.finite(fit$AIC))
  expect_true(is.finite(fit$BIC))
  expect_true("ks_test" %in% names(fit))
  expect_equal(fit$n, n_expected)
  expect_equal(length(fit$data), n_expected)
}

# ----------------------------------------------------------------------------
# fit_distribution -- happy path across distribution families
# ----------------------------------------------------------------------------

test_that("fit_distribution fits normal distribution and returns full result", {
  set.seed(123)
  x <- rnorm(200, mean = 5, sd = 2)
  fit <- fit_distribution(x, "norm")
  expect_valid_fit(fit, "norm", 200)
  expect_named(fit$params, c("mean", "sd"))
  expect_equal(fit$params$mean, 5, tolerance = 0.3)
  expect_equal(fit$params$sd, 2, tolerance = 0.3)
  expect_true(is.finite(fit$ks_test$statistic))
  expect_true(is.finite(fit$ks_test$p.value))
})

test_that("fit_distribution fits exponential distribution", {
  set.seed(123)
  x <- rexp(200, rate = 0.5)
  fit <- fit_distribution(x, "exp")
  expect_valid_fit(fit, "exp", 200)
  expect_named(fit$params, "rate")
  expect_equal(fit$params$rate, 0.5, tolerance = 0.1)
})

test_that("fit_distribution fits gamma distribution", {
  set.seed(123)
  x <- rgamma(200, shape = 2, rate = 0.5)
  fit <- fit_distribution(x, "gamma")
  expect_valid_fit(fit, "gamma", 200)
  expect_named(fit$params, c("shape", "rate"))
  expect_true(fit$params$shape > 0)
  expect_true(fit$params$rate > 0)
})

test_that("fit_distribution fits weibull distribution", {
  set.seed(123)
  x <- rweibull(200, shape = 2, scale = 5)
  fit <- fit_distribution(x, "weibull")
  expect_valid_fit(fit, "weibull", 200)
  expect_named(fit$params, c("shape", "scale"))
  expect_true(fit$params$shape > 0)
  expect_true(fit$params$scale > 0)
})

test_that("fit_distribution fits log-normal distribution", {
  set.seed(123)
  x <- rlnorm(200, meanlog = 0, sdlog = 0.5)
  fit <- fit_distribution(x, "lnorm")
  expect_valid_fit(fit, "lnorm", 200)
  expect_named(fit$params, c("meanlog", "sdlog"))
  expect_equal(fit$params$meanlog, 0, tolerance = 0.1)
  expect_true(fit$params$sdlog > 0)
})

test_that("fit_distribution fits beta distribution", {
  set.seed(123)
  x <- rbeta(200, shape1 = 2, shape2 = 5)
  fit <- fit_distribution(x, "beta")
  expect_valid_fit(fit, "beta", 200)
  expect_named(fit$params, c("shape1", "shape2"))
  expect_true(fit$params$shape1 > 0)
  expect_true(fit$params$shape2 > 0)
})

test_that("fit_distribution fits logistic distribution", {
  set.seed(123)
  x <- rlogis(200, location = 3, scale = 1.5)
  fit <- fit_distribution(x, "logis")
  expect_valid_fit(fit, "logis", 200)
  expect_named(fit$params, c("location", "scale"))
  expect_equal(fit$params$location, 3, tolerance = 0.3)
  expect_true(fit$params$scale > 0)
})

test_that("fit_distribution fits cauchy distribution", {
  set.seed(123)
  x <- rcauchy(200, location = 0, scale = 1)
  fit <- fit_distribution(x, "cauchy")
  expect_valid_fit(fit, "cauchy", 200)
  expect_named(fit$params, c("location", "scale"))
  expect_true(is.finite(fit$params$location))
  expect_true(fit$params$scale > 0)
})

test_that("fit_distribution fits (scaled) t distribution", {
  set.seed(123)
  x <- rt(200, df = 5)
  fit <- fit_distribution(x, "t")
  expect_valid_fit(fit, "t", 200)
  expect_named(fit$params, c("df", "location", "scale"))
  expect_true(fit$params$df > 0)
  expect_true(is.finite(fit$params$location))
  expect_true(fit$params$scale > 0)
})

test_that("fit_distribution fits F distribution", {
  set.seed(123)
  x <- rf(200, df1 = 5, df2 = 10)
  fit <- fit_distribution(x, "f")
  expect_valid_fit(fit, "f", 200)
  expect_named(fit$params, c("df1", "df2"))
  expect_true(fit$params$df1 > 0)
  expect_true(fit$params$df2 > 0)
})

test_that("fit_distribution fits chi-squared distribution", {
  set.seed(123)
  x <- rchisq(200, df = 4)
  fit <- fit_distribution(x, "chisq")
  expect_valid_fit(fit, "chisq", 200)
  expect_named(fit$params, "df")
  expect_true(fit$params$df > 0)
})

test_that("fit_distribution accepts method = mme (matched, no error)", {
  set.seed(123)
  x <- rnorm(50)
  fit <- fit_distribution(x, "norm", method = "mme")
  expect_valid_fit(fit, "norm", 50)
})

test_that("fit_distribution removes NA values before fitting", {
  set.seed(123)
  x <- c(rnorm(50), NA_real_, NA_real_)
  fit <- fit_distribution(x, "norm")
  expect_valid_fit(fit, "norm", 50)
})

# ----------------------------------------------------------------------------
# fit_distribution -- error paths
# ----------------------------------------------------------------------------

test_that("fit_distribution errors when fewer than 3 non-missing values", {
  expect_error(fit_distribution(c(1, 2), "norm"), "at least 3")
  expect_error(fit_distribution(c(1, NA, NA), "norm"), "at least 3")
})

test_that("fit_distribution errors on non-positive data for exp", {
  expect_error(fit_distribution(c(0, 1, 2), "exp"), "positive data")
  expect_error(fit_distribution(c(-1, 2, 3), "exp"), "positive data")
})

test_that("fit_distribution errors on non-positive data for gamma", {
  expect_error(fit_distribution(c(0, 1, 2), "gamma"), "positive data")
})

test_that("fit_distribution errors on non-positive data for weibull", {
  expect_error(fit_distribution(c(-1, 2, 3), "weibull"), "positive data")
})

test_that("fit_distribution errors on non-positive data for lnorm", {
  expect_error(fit_distribution(c(0, 1, 2), "lnorm"), "positive data")
})

test_that("fit_distribution errors on out-of-(0,1) data for beta", {
  expect_error(fit_distribution(c(0, 0.5, 1), "beta"), "0, 1")
  expect_error(fit_distribution(c(-0.1, 0.5, 0.6), "beta"), "0, 1")
  expect_error(fit_distribution(c(0.4, 0.5, 1.2), "beta"), "0, 1")
})

test_that("fit_distribution errors on non-positive data for f", {
  expect_error(fit_distribution(c(0, 1, 2), "f"), "positive data")
})

test_that("fit_distribution errors on non-positive data for chisq", {
  expect_error(fit_distribution(c(0, 1, 2), "chisq"), "positive data")
})

# ----------------------------------------------------------------------------
# private_fit_* -- internal fallback branches
# ----------------------------------------------------------------------------

test_that("beta fit falls back to shape1 = shape2 = 1 when MoM estimates invalid", {
  # Bimodal data near the boundaries yields negative method-of-moments shapes,
  # exercising the shape1 <= 0 || shape2 <= 0 fallback in private_fit_beta().
  x <- c(0.01, 0.02, 0.98, 0.99)
  fit <- fit_distribution(x, "beta")
  expect_valid_fit(fit, "beta", 4)
  expect_true(fit$params$shape1 > 0)
  expect_true(fit$params$shape2 > 0)
})

test_that("cauchy fit falls back to IQR when mad is zero", {
  # Repeated median value makes mad() == 0, exercising the
  # scale <- IQR(x) / 1.349 fallback in private_fit_cauchy().
  # Use c(1, 1, 1, 5, 10) so that mad == 0 (median of |x - median| is 0)
  # but IQR > 0 (25th pct = 1, 75th pct = 5), yielding a positive scale.
  x <- c(1, 1, 1, 5, 10)
  fit <- fit_distribution(x, "cauchy")
  expect_valid_fit(fit, "cauchy", 5)
  expect_true(is.finite(fit$params$scale))
  expect_true(fit$params$scale > 0)
})

# When MASS is installed, passing an invalid start makes fitdistr() error,
# which is swallowed by tryCatch and falls through to the moment estimates.
# These tests cover the is.null(fit) branch of each MASS-backed helper.

test_that("gamma fit falls back when MASS::fitdistr fails on bad start", {
  skip_if_not_installed("MASS")
  set.seed(123)
  x <- rgamma(100, shape = 2, rate = 0.5)
  fit <- fit_distribution(x, "gamma", start = list(badparam = 1))
  expect_valid_fit(fit, "gamma", 100)
  expect_true(fit$params$shape > 0)
  expect_true(fit$params$rate > 0)
})

test_that("beta fit falls back when MASS::fitdistr fails on bad start", {
  skip_if_not_installed("MASS")
  x <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
  fit <- fit_distribution(x, "beta", start = list(badparam = 1))
  expect_valid_fit(fit, "beta", 7)
  expect_true(fit$params$shape1 > 0)
  expect_true(fit$params$shape2 > 0)
})

test_that("cauchy fit falls back when MASS::fitdistr fails on bad start", {
  skip_if_not_installed("MASS")
  set.seed(123)
  x <- rcauchy(100)
  fit <- fit_distribution(x, "cauchy", start = list(badparam = 1))
  expect_valid_fit(fit, "cauchy", 100)
  expect_true(is.finite(fit$params$location))
})

test_that("t fit falls back when MASS::fitdistr fails on bad start", {
  skip_if_not_installed("MASS")
  set.seed(123)
  x <- rt(100, df = 5)
  fit <- fit_distribution(x, "t", start = list(badparam = 1))
  expect_valid_fit(fit, "t", 100)
  expect_true(fit$params$df > 0)
  expect_true(fit$params$scale > 0)
})

test_that("f fit falls back when MASS::fitdistr fails on bad start", {
  skip_if_not_installed("MASS")
  set.seed(123)
  x <- rf(100, df1 = 5, df2 = 10)
  fit <- fit_distribution(x, "f", start = list(badparam = 1))
  expect_valid_fit(fit, "f", 100)
  expect_true(fit$params$df1 > 0)
  expect_true(fit$params$df2 > 0)
})

# ----------------------------------------------------------------------------
# auto_fit_distribution
# ----------------------------------------------------------------------------

test_that("auto_fit_distribution selects norm for normal data by default", {
  set.seed(123)
  x <- rnorm(300)
  result <- auto_fit_distribution(x)
  expect_type(result, "list")
  expect_named(result, c("best_dist", "best_result", "all_results",
                         "ranking", "criterion", "n"))
  expect_equal(result$criterion, "aic")
  expect_equal(result$n, 300)
  expect_equal(result$best_dist, "norm")
  expect_equal(result$best_result$dist, "norm")
  expect_true("norm" %in% names(result$all_results))
})

test_that("auto_fit_distribution infers positive_only = TRUE for positive data", {
  set.seed(123)
  x <- rexp(300, rate = 0.5)
  result <- auto_fit_distribution(x)  # positive_only = NULL -> all(x > 0) TRUE
  cand <- names(result$all_results)
  expect_true(all(cand %in% c("exp", "gamma", "weibull", "lnorm", "logis")))
  expect_true(result$best_dist %in% cand)
})

test_that("auto_fit_distribution ranks candidates by BIC when criterion = bic", {
  set.seed(123)
  x <- rexp(300, rate = 0.5)
  result <- auto_fit_distribution(x, criterion = "bic")
  expect_equal(result$criterion, "bic")
  expect_s3_class(result$ranking, "data.frame")
  expect_true(all(diff(result$ranking$score) >= 0))  # ascending
  expect_equal(result$ranking$dist[1], result$best_dist)
  expect_equal(result$ranking$score, result$ranking$BIC)
})

test_that("auto_fit_distribution ranks candidates by KS statistic when criterion = ks", {
  set.seed(123)
  x <- rnorm(300)
  result <- auto_fit_distribution(x, criterion = "ks")
  expect_equal(result$criterion, "ks")
  expect_true(all(diff(result$ranking$score) >= 0))
  expect_equal(result$ranking$score, result$ranking$ks_stat)
})

test_that("auto_fit_distribution ranking has expected columns", {
  set.seed(123)
  x <- rnorm(100)
  result <- auto_fit_distribution(x)
  expect_named(result$ranking,
               c("dist", "score", "AIC", "BIC", "ks_stat", "ks_p"))
})

test_that("auto_fit_distribution errors when fewer than 3 values", {
  expect_error(auto_fit_distribution(c(1, 2)), "at least 3")
})

test_that("auto_fit_distribution errors when no candidate matches the data support", {
  # positive_only = TRUE but only norm offered -> intersect with positive
  # support set is empty -> No valid candidate distributions.
  set.seed(123)
  x <- rexp(50)
  expect_error(
    auto_fit_distribution(x, candidates = "norm", positive_only = TRUE),
    "No valid candidate"
  )
})

test_that("auto_fit_distribution with positive_only = FALSE keeps negative-support dists", {
  set.seed(123)
  x <- rnorm(200)
  result <- auto_fit_distribution(x, positive_only = FALSE)
  cand <- names(result$all_results)
  expect_true(all(cand %in% c("norm", "logis", "cauchy", "t")))
})

# ----------------------------------------------------------------------------
# compare_fits
# ----------------------------------------------------------------------------

test_that("compare_fits compares norm and logis, returning KS and AD winners", {
  set.seed(123)
  x <- rnorm(200)
  f1 <- fit_distribution(x, "norm")
  f2 <- fit_distribution(x, "logis")
  result <- compare_fits(list(norm = f1, logis = f2))
  expect_type(result, "list")
  expect_named(result, c("test_results", "best_by_ks", "best_by_ad", "n_fits"))
  expect_equal(result$n_fits, 2)
  expect_true(result$best_by_ks %in% c("norm", "logis"))
  # nortest is an Imports dependency, so AD is computed for norm.
  expect_true(result$best_by_ad %in% c("norm", "logis"))
  expect_true("ks_test" %in% names(result$test_results$norm))
  expect_false(is.null(result$test_results$norm$ad_test))
})

test_that("compare_fits uses the scaled-t closure for t distribution", {
  set.seed(123)
  x <- rt(200, df = 5)
  fit_t <- fit_distribution(x, "t")
  result <- compare_fits(list(t = fit_t))
  expect_equal(result$best_by_ks, "t")
  ks <- result$test_results$t$ks_test
  expect_true(is.finite(unname(ks$statistic)))
  expect_true(is.finite(ks$p.value))
})

test_that("compare_fits returns NA for best_by_ad when no norm fit is present", {
  # AD is only computed for norm; with only non-norm fits, ad_performed is
  # all FALSE and best_by_ad should be NA.
  set.seed(123)
  x <- rnorm(200)
  fl <- fit_distribution(x, "logis")
  result <- compare_fits(list(logis = fl))
  expect_true(is.na(result$best_by_ad))
  expect_null(result$test_results$logis$ad_test)
})

test_that("compare_fits carries AIC/BIC through per-distribution test results", {
  set.seed(123)
  x <- rnorm(100)
  f1 <- fit_distribution(x, "norm")
  f2 <- fit_distribution(x, "logis")
  result <- compare_fits(list(norm = f1, logis = f2))
  expect_equal(result$test_results$norm$AIC, f1$AIC)
  expect_equal(result$test_results$logis$BIC, f2$BIC)
})

# ----------------------------------------------------------------------------
# empirical_distribution
# ----------------------------------------------------------------------------

test_that("empirical_distribution returns ecdf, points and sample size", {
  set.seed(123)
  x <- rnorm(100)
  edf <- empirical_distribution(x)
  expect_type(edf, "list")
  expect_named(edf, c("ecdf", "points", "n"))
  expect_equal(edf$n, 100)
  expect_s3_class(edf$points, "data.frame")
  expect_equal(nrow(edf$points), 100)
  expect_named(edf$points, c("x", "prob"))
  # ECDF endpoints: 0 below min, 1 at/above max.
  expect_equal(edf$ecdf(min(x) - 1), 0)
  expect_equal(edf$ecdf(max(x) + 1), 1)
  # Probabilities are monotonically non-decreasing.
  expect_true(all(diff(edf$points$prob) >= 0))
  expect_equal(edf$points$x, sort(x))
})

test_that("empirical_distribution removes NA values", {
  x <- c(1, 2, 3, NA_real_, 4)
  edf <- empirical_distribution(x)
  expect_equal(edf$n, 4)
  expect_equal(edf$points$x, c(1, 2, 3, 4))
})

test_that("empirical_distribution errors when fewer than 2 values", {
  expect_error(empirical_distribution(c(1)), "at least 2")
  expect_error(empirical_distribution(NA_real_), "at least 2")
})

# ----------------------------------------------------------------------------
# calc_qq_data
# ----------------------------------------------------------------------------

test_that("calc_qq_data returns theoretical and sample quantiles for norm", {
  set.seed(123)
  x <- rnorm(100)
  qq <- calc_qq_data(x, "norm", list(mean = 0, sd = 1))
  expect_s3_class(qq, "data.frame")
  expect_equal(ncol(qq), 2)
  expect_named(qq, c("theoretical", "sample"))
  expect_equal(nrow(qq), 100)
  expect_equal(qq$sample, sort(x))
  probs <- (1:100 - 0.5) / 100
  expect_equal(qq$theoretical, qnorm(probs, mean = 0, sd = 1))
})

test_that("calc_qq_data uses manual qt for scaled-t distribution", {
  set.seed(123)
  x <- rt(100, df = 6)
  params <- list(df = 6, location = 1, scale = 2)
  qq <- calc_qq_data(x, "t", params)
  expect_equal(nrow(qq), 100)
  probs <- (1:100 - 0.5) / 100
  expected <- 1 + 2 * qt(probs, df = 6)
  expect_equal(qq$theoretical, expected)
})

test_that("calc_qq_data dispatches to qfunc for non-t distributions", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  qq <- calc_qq_data(x, "exp", list(rate = 0.5))
  probs <- (1:100 - 0.5) / 100
  expect_equal(qq$theoretical, qexp(probs, rate = 0.5))
  expect_equal(qq$sample, sort(x))
})

test_that("calc_qq_data removes NA values", {
  x <- c(1, 2, 3, NA_real_, 4, 5)
  qq <- calc_qq_data(x, "norm", list(mean = 0, sd = 1))
  expect_equal(nrow(qq), 5)
  expect_equal(qq$sample, c(1, 2, 3, 4, 5))
})

test_that("calc_qq_data errors when fewer than 2 values", {
  expect_error(calc_qq_data(c(1), "norm", list()), "at least 2")
  expect_error(calc_qq_data(NA_real_, "norm", list()), "at least 2")
})

# ----------------------------------------------------------------------------
# Additional coverage: auto_fit error paths, compare_fits edge cases,
# calc_qq_data across more distribution families
# ----------------------------------------------------------------------------

test_that("auto_fit_distribution errors when all candidate fits fail", {
  # Data with a zero value: exp and gamma both require strictly positive data.
  # positive_only = TRUE keeps only exp/gamma/weibull/lnorm/logis candidates,
  # but we restrict to c("exp", "gamma") -> both error -> results empty.
  x <- c(0, 1, 2, 3, 4)
  expect_error(
    auto_fit_distribution(x, candidates = c("exp", "gamma"), positive_only = TRUE),
    "All distribution fits failed"
  )
})

test_that("auto_fit_distribution with positive_only = NULL and negative data uses negative-support set", {
  # rnorm produces negative values -> all(x > 0) is FALSE -> positive_only = FALSE
  # -> candidates intersected with c("norm", "logis", "cauchy", "t")
  set.seed(42)
  x <- rnorm(200)
  result <- auto_fit_distribution(x, candidates = c("norm", "logis", "cauchy", "t", "exp"))
  cand <- names(result$all_results)
  # "exp" should be filtered out because positive_only = FALSE
  expect_false("exp" %in% cand)
  expect_true(all(cand %in% c("norm", "logis", "cauchy", "t")))
})

test_that("auto_fit_distribution best_result matches best_dist entry in all_results", {
  set.seed(123)
  x <- rnorm(100)
  result <- auto_fit_distribution(x)
  expect_identical(result$best_result, result$all_results[[result$best_dist]])
})

test_that("compare_fits handles three or more distributions", {
  set.seed(123)
  x <- rnorm(200)
  f1 <- fit_distribution(x, "norm")
  f2 <- fit_distribution(x, "logis")
  f3 <- fit_distribution(x, "cauchy")
  result <- compare_fits(list(norm = f1, logis = f2, cauchy = f3))
  expect_equal(result$n_fits, 3)
  expect_equal(length(result$test_results), 3)
  expect_true(result$best_by_ks %in% c("norm", "logis", "cauchy"))
  # Only norm has AD test (requires nortest)
  expect_false(is.null(result$test_results$norm$ad_test))
  expect_null(result$test_results$cauchy$ad_test)
})

test_that("compare_fits works with a single fit", {
  set.seed(123)
  x <- rnorm(100)
  f1 <- fit_distribution(x, "norm")
  result <- compare_fits(list(norm = f1))
  expect_equal(result$n_fits, 1)
  expect_equal(result$best_by_ks, "norm")
  expect_equal(result$best_by_ad, "norm")
})

test_that("compare_fits handles mix of t and norm distributions", {
  set.seed(123)
  x <- rnorm(200)
  f1 <- fit_distribution(x, "norm")
  f2 <- fit_distribution(x, "t")
  result <- compare_fits(list(norm = f1, t = f2))
  expect_equal(result$n_fits, 2)
  # norm has AD test; t does not
  expect_false(is.null(result$test_results$norm$ad_test))
  expect_null(result$test_results$t$ad_test)
  # best_by_ad should be "norm" (only distribution with AD computed)
  expect_equal(result$best_by_ad, "norm")
})

test_that("compare_fits KS test uses do.call for standard distributions", {
  # Verify the ks.test is called with proper unpacked params for norm
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 2)
  f1 <- fit_distribution(x, "norm")
  result <- compare_fits(list(norm = f1))
  ks <- result$test_results$norm$ks_test
  expect_true(is.finite(unname(ks$statistic)))
  expect_true(is.finite(ks$p.value))
})

test_that("calc_qq_data works for gamma distribution", {
  set.seed(123)
  x <- rgamma(100, shape = 2, rate = 0.5)
  qq <- calc_qq_data(x, "gamma", list(shape = 2, rate = 0.5))
  expect_equal(nrow(qq), 100)
  probs <- (1:100 - 0.5) / 100
  expect_equal(qq$theoretical, qgamma(probs, shape = 2, rate = 0.5))
  expect_equal(qq$sample, sort(x))
})

test_that("calc_qq_data works for weibull distribution", {
  set.seed(123)
  x <- rweibull(100, shape = 2, scale = 5)
  qq <- calc_qq_data(x, "weibull", list(shape = 2, scale = 5))
  expect_equal(nrow(qq), 100)
  probs <- (1:100 - 0.5) / 100
  expect_equal(qq$theoretical, qweibull(probs, shape = 2, scale = 5))
})

test_that("calc_qq_data works for log-normal distribution", {
  set.seed(123)
  x <- rlnorm(100, meanlog = 0, sdlog = 0.5)
  qq <- calc_qq_data(x, "lnorm", list(meanlog = 0, sdlog = 0.5))
  expect_equal(nrow(qq), 100)
  probs <- (1:100 - 0.5) / 100
  expect_equal(qq$theoretical, qlnorm(probs, meanlog = 0, sdlog = 0.5))
})

test_that("calc_qq_data works for logistic distribution", {
  set.seed(123)
  x <- rlogis(100, location = 3, scale = 1.5)
  qq <- calc_qq_data(x, "logis", list(location = 3, scale = 1.5))
  expect_equal(nrow(qq), 100)
  probs <- (1:100 - 0.5) / 100
  expect_equal(qq$theoretical, qlogis(probs, location = 3, scale = 1.5))
})

test_that("calc_qq_data uses standard qt for t without location/scale params", {
  # When params lacks location/scale, the scaled-t branch is skipped
  # and the do.call(qfunc, ...) path is used with qt().
  set.seed(123)
  x <- rt(100, df = 5)
  qq <- calc_qq_data(x, "t", list(df = 5))
  expect_equal(nrow(qq), 100)
  probs <- (1:100 - 0.5) / 100
  expect_equal(qq$theoretical, qt(probs, df = 5))
})

test_that("calc_qq_data works for chi-squared distribution", {
  set.seed(123)
  x <- rchisq(100, df = 4)
  qq <- calc_qq_data(x, "chisq", list(df = 4))
  expect_equal(nrow(qq), 100)
  probs <- (1:100 - 0.5) / 100
  expect_equal(qq$theoretical, qchisq(probs, df = 4))
})

test_that("empirical_distribution handles constant data", {
  x <- c(5, 5, 5, 5)
  edf <- empirical_distribution(x)
  expect_equal(edf$n, 4)
  expect_equal(edf$ecdf(5), 1)
  expect_equal(edf$ecdf(4), 0)
  expect_equal(edf$ecdf(6), 1)
  # Points have incremental probabilities (1:n)/n even for constant data
  expect_equal(edf$points$prob, c(0.25, 0.5, 0.75, 1.0))
  expect_equal(edf$points$x, c(5, 5, 5, 5))
})

test_that("empirical_distribution with two values produces correct ecdf", {
  x <- c(1, 2)
  edf <- empirical_distribution(x)
  expect_equal(edf$n, 2)
  expect_equal(edf$ecdf(0.5), 0)
  expect_equal(edf$ecdf(1), 0.5)
  expect_equal(edf$ecdf(1.5), 0.5)
  expect_equal(edf$ecdf(2), 1)
  expect_equal(edf$points$x, c(1, 2))
  expect_equal(edf$points$prob, c(0.5, 1))
})

test_that("fit_distribution with method = mme works for non-norm distributions", {
  # method = "mme" is accepted (match.arg) even though it maps to same code path
  set.seed(123)
  x <- rexp(50, rate = 0.5)
  fit <- fit_distribution(x, "exp", method = "mme")
  expect_valid_fit(fit, "exp", 50)
})

test_that("auto_fit_distribution ranking is sorted by score ascending", {
  set.seed(123)
  x <- rexp(200, rate = 0.5)
  result <- auto_fit_distribution(x, criterion = "aic")
  # Ranking should be sorted by AIC ascending (lower is better)
  expect_true(all(diff(result$ranking$AIC) >= 0))
  expect_equal(result$ranking$AIC[1], min(result$ranking$AIC))
})

test_that("auto_fit_distribution with positive_only = FALSE and positive-only candidates filters correctly", {
  # Even with positive data, positive_only = FALSE forces negative-support set
  set.seed(123)
  x <- rexp(200, rate = 0.5)  # All positive
  result <- auto_fit_distribution(x, positive_only = FALSE,
                                  candidates = c("norm", "exp", "gamma"))
  # Only "norm" survives the intersect with c("norm", "logis", "cauchy", "t")
  cand <- names(result$all_results)
  expect_true("norm" %in% cand)
  expect_false("exp" %in% cand)
  expect_false("gamma" %in% cand)
})
