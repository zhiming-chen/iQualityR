# =============================================================================
# File: tests/testthat/test-sample-size.R
# Description: Sample size and power module tests (sample_size.R)
# =============================================================================

library(testthat)
library(iQualityR.stat)

test_that("sample_size_mean mean sample size", {
  result <- sample_size_mean(mu0 = 10, mu1 = 10.5, sigma = 1, power = 0.80)

  expect_type(result, "list")
  expect_true("n" %in% names(result))
  expect_gt(result$n, 0)
})

test_that("sample_size_proportion proportion sample size", {
  result <- sample_size_proportion(p0 = 0.5, p1 = 0.6, power = 0.80)

  expect_type(result, "list")
  expect_true("n" %in% names(result))
  expect_gt(result$n, 0)
})

test_that("sample_size_anova ANOVA sample size", {
  result <- sample_size_anova(k = 3, means = c(10, 11, 12), sigma = 1, power = 0.80)

  expect_type(result, "list")
  expect_true("n_per_group" %in% names(result))
  expect_gt(result$n_per_group, 0)
})

test_that("calc_power power calculation", {
  power <- calc_power(n = 50, effect_size = 0.5, alpha = 0.05)

  expect_type(power, "double")
  expect_gte(power, 0)
  expect_lte(power, 1)
})

test_that("effect_size cohens_d", {
  d <- effect_size(type = "cohens_d", mean1 = 11, mean2 = 10, sd_pooled = 1)
  expect_equal(d, 1)
})

# ----------------------------------------------------------------------------
# Comprehensive effect_size tests (R2-6: all 5 types)
# ----------------------------------------------------------------------------

test_that("effect_size cohens_d negative direction", {
  d <- effect_size(type = "cohens_d", mean1 = 9, mean2 = 11, sd_pooled = 2)
  expect_equal(d, -1)
})

test_that("effect_size cohens_d zero effect", {
  d <- effect_size(type = "cohens_d", mean1 = 10, mean2 = 10, sd_pooled = 1)
  expect_equal(d, 0)
})

test_that("effect_size hedges_g applies small-sample correction", {
  # Hedges' g = d * J where J = 1 - 3/(4*(n1+n2-2) - 1) < 1
  g <- effect_size(type = "hedges_g", mean1 = 11, mean2 = 10,
                   sd_pooled = 1, n1 = 20, n2 = 20)
  d <- effect_size(type = "cohens_d", mean1 = 11, mean2 = 10, sd_pooled = 1)
  # |g| should be slightly smaller than |d| due to the correction factor
  expect_lt(abs(g), abs(d))
  expect_gt(g, 0)
})

test_that("effect_size hedges_g equals d for large n (correction -> 1)", {
  # With very large n, J -> 1, so g -> d
  g <- effect_size(type = "hedges_g", mean1 = 11, mean2 = 10,
                   sd_pooled = 1, n1 = 1000, n2 = 1000)
  d <- effect_size(type = "cohens_d", mean1 = 11, mean2 = 10, sd_pooled = 1)
  expect_equal(g, d, tolerance = 1e-3)
})

test_that("effect_size eta_squared via F statistic", {
  # F = 10, df_between = 2, df_within = 60
  # eta^2 = F*df_b / (F*df_b + df_w) = 20 / (20 + 60) = 0.25
  eta <- effect_size(type = "eta_squared", F = 10, df_between = 2, df_within = 60)
  expect_equal(eta, 0.25)
})

test_that("effect_size eta_squared via SS values", {
  # ss_between / ss_total
  eta <- effect_size(type = "eta_squared", ss_between = 30, ss_total = 120)
  expect_equal(eta, 0.25)
})

test_that("effect_size eta_squared errors without required args", {
  expect_error(effect_size(type = "eta_squared"), "eta_squared requires")
})

test_that("effect_size cohens_h for proportions", {
  # Cohen's h = 2*arcsin(sqrt(p1)) - 2*arcsin(sqrt(p2))
  h <- effect_size(type = "cohens_h", p1 = 0.7, p2 = 0.5)
  expected <- 2 * asin(sqrt(0.7)) - 2 * asin(sqrt(0.5))
  expect_equal(h, expected)
  expect_gt(h, 0)
})

test_that("effect_size cohens_h zero effect for equal proportions", {
  h <- effect_size(type = "cohens_h", p1 = 0.5, p2 = 0.5)
  expect_equal(h, 0)
})

test_that("effect_size r via t statistic", {
  # r = sqrt(t^2 / (t^2 + df))
  r <- effect_size(type = "r", t = 3, df = 30)
  expected <- sqrt(9 / (9 + 30))
  expect_equal(r, expected)
  expect_gt(r, 0)
  expect_lt(r, 1)
})

test_that("effect_size r via z statistic", {
  # r = z / sqrt(n)
  r <- effect_size(type = "r", z = 2.5, n = 100)
  expect_equal(r, 0.25)
})

test_that("effect_size r errors without required args", {
  expect_error(effect_size(type = "r"), "effect_size_r requires")
})

test_that("effect_size rejects unknown type", {
  expect_error(effect_size(type = "nope"), "arg")
})

test_that("power_table returns data.frame with power column", {
  pt <- power_table(effect_size = 0.5, alpha = 0.05)
  expect_s3_class(pt, "data.frame")
  expect_true("n" %in% names(pt))
  expect_true("power" %in% names(pt))
  expect_true("meets_80" %in% names(pt))
  # Power should increase with n
  expect_true(pt$power[nrow(pt)] > pt$power[1])
})

# ----------------------------------------------------------------------------
# sample_size_anova correctness (Bug fix: * k -> / k + iterative refinement)
# ----------------------------------------------------------------------------

test_that("sample_size_anova achieves target power", {
  r <- sample_size_anova(k = 3, means = c(10, 11, 12), sigma = 1.5, power = 0.80)
  expect_gte(r$actual_power, 0.80)
  # n-1 should NOT meet the target power (minimum n found)
  lambda <- (r$n_per_group - 1) * sum((c(10, 11, 12) - 11)^2) / 1.5^2
  df2 <- 3 * (r$n_per_group - 2)
  f_crit <- stats::qf(0.95, 2, df2)
  power_below <- 1 - stats::pf(f_crit, 2, df2, ncp = lambda)
  expect_lt(power_below, 0.80)
})

test_that("sample_size_anova larger effect needs fewer samples", {
  r_small <- sample_size_anova(k = 3, means = c(10, 11, 12), sigma = 1.5, power = 0.80)
  r_large <- sample_size_anova(k = 3, means = c(10, 13, 16), sigma = 1.5, power = 0.80)
  expect_lt(r_large$n_per_group, r_small$n_per_group)
})

test_that("sample_size_anova matches pwr package when available", {
  skip_if_not_installed("pwr")
  means <- c(10, 11, 12)
  f <- sqrt(sum((means - mean(means))^2) / 3) / 1.5
  pwr_res <- pwr::pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.80)
  r <- sample_size_anova(k = 3, means = means, sigma = 1.5, power = 0.80)
  expect_equal(r$n_per_group, ceiling(pwr_res$n))
})

test_that("sample_size_anova errors when all means equal", {
  expect_error(sample_size_anova(k = 3, means = c(10, 10, 10), sigma = 1))
})

# ----------------------------------------------------------------------------
# sample_size_mean actual_power uses ceiling(n) (Bug fix)
# ----------------------------------------------------------------------------

test_that("sample_size_mean t-test achieves target power", {
  r <- sample_size_mean(mu0 = 10, mu1 = 10.5, sigma = 1, power = 0.80, test_type = "t")
  expect_gte(r$actual_power, 0.80)
})

test_that("sample_size_mean z-test actual_power matches integer-n computation", {
  r <- sample_size_mean(mu0 = 10, mu1 = 10.5, sigma = 1, power = 0.80, test_type = "z")
  n_int <- r$n
  ncp <- (abs(10.5 - 10) / 1) * sqrt(n_int)
  z_alpha <- stats::qnorm(1 - 0.05 / 2)
  expected <- stats::pnorm(-z_alpha + ncp) + (1 - stats::pnorm(z_alpha + ncp))
  expect_equal(r$actual_power, expected, tolerance = 1e-10)
})

test_that("sample_size_mean z-test achieves target power", {
  r <- sample_size_mean(mu0 = 10, mu1 = 11, sigma = 1, power = 0.90, test_type = "z")
  expect_gte(r$actual_power, 0.90)
})

# ----------------------------------------------------------------------------
# Proportion power: "less" alternative uses correct critical value (Bug fix)
# ----------------------------------------------------------------------------

test_that("proportion power: greater > less when p1 > p0", {
  n <- 200
  pow_greater <- iQualityR.stat:::private_calc_power_proportion(n, 0.5, 0.6, 0.05, "greater")
  pow_less <- iQualityR.stat:::private_calc_power_proportion(n, 0.5, 0.6, 0.05, "less")
  expect_gt(pow_greater, pow_less)
  expect_lt(pow_less, 0.50)
})

test_that("proportion power: less > greater when p1 < p0", {
  n <- 200
  pow_greater <- iQualityR.stat:::private_calc_power_proportion(n, 0.6, 0.5, 0.05, "greater")
  pow_less <- iQualityR.stat:::private_calc_power_proportion(n, 0.6, 0.5, 0.05, "less")
  expect_gt(pow_less, pow_greater)
  expect_lt(pow_greater, 0.50)
})

test_that("sample_size_proportion less alternative achieves target power", {
  r <- sample_size_proportion(p0 = 0.5, p1 = 0.35, power = 0.80, alternative = "less")
  expect_gte(r$actual_power, 0.80)
})

test_that("sample_size_proportion greater alternative achieves target power", {
  r <- sample_size_proportion(p0 = 0.5, p1 = 0.65, power = 0.80, alternative = "greater")
  expect_gte(r$actual_power, 0.80)
})

# ----------------------------------------------------------------------------
# Two-proportion power: "less" alternative uses -crit (Bug fix)
# ----------------------------------------------------------------------------

test_that("two-proportion power: greater > less when p1 > p2", {
  pow_greater <- iQualityR.stat:::private_calc_power_two_proportion(200, 200, 0.5, 0.4, 0.05, "greater")
  pow_less <- iQualityR.stat:::private_calc_power_two_proportion(200, 200, 0.5, 0.4, 0.05, "less")
  expect_gt(pow_greater, pow_less)
  expect_lt(pow_less, 0.50)
})

test_that("two-proportion power: less > greater when p1 < p2", {
  pow_greater <- iQualityR.stat:::private_calc_power_two_proportion(200, 200, 0.4, 0.5, 0.05, "greater")
  pow_less <- iQualityR.stat:::private_calc_power_two_proportion(200, 200, 0.4, 0.5, 0.05, "less")
  expect_gt(pow_less, pow_greater)
  expect_lt(pow_greater, 0.50)
})

test_that("sample_size_two_proportions less alternative achieves target power", {
  r <- sample_size_two_proportions(p1 = 0.35, p2 = 0.5, power = 0.80, alternative = "less")
  expect_gte(r$actual_power, 0.80)
})

test_that("sample_size_two_proportions greater alternative achieves target power", {
  r <- sample_size_two_proportions(p1 = 0.5, p2 = 0.35, power = 0.80, alternative = "greater")
  expect_gte(r$actual_power, 0.80)
})

# ----------------------------------------------------------------------------
# Additional coverage: two-sample mean and input validation
# ----------------------------------------------------------------------------

test_that("sample_size_two_means returns valid per-group sizes", {
  r <- sample_size_two_means(mu1 = 10, mu2 = 11, sigma = 1.5, power = 0.90)
  expect_gt(r$n1, 0)
  expect_gt(r$n2, 0)
  expect_equal(r$total_n, r$n1 + r$n2)
})

test_that("sample_size_mean rejects zero delta", {
  expect_error(sample_size_mean(mu0 = 10, mu1 = 10, sigma = 1))
})

test_that("sample_size_mean rejects non-positive sigma", {
  expect_error(sample_size_mean(mu0 = 10, mu1 = 11, sigma = 0))
  expect_error(sample_size_mean(mu0 = 10, mu1 = 11, sigma = -1))
})

test_that("sample_size_proportion rejects boundary proportions", {
  expect_error(sample_size_proportion(p0 = 0, p1 = 0.5))
  expect_error(sample_size_proportion(p0 = 0.5, p1 = 1))
})
