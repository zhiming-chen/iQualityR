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
# R3-C2: omega_squared effect size
# ----------------------------------------------------------------------------

test_that("effect_size omega_squared via F statistic", {
  # ω² = (F*df_b - df_b) / (F*df_b + df_w + 1)
  F <- 10; df_b <- 2; df_w <- 60
  omega <- effect_size(type = "omega_squared", F = F,
                       df_between = df_b, df_within = df_w)
  expected <- (F * df_b - df_b) / (F * df_b + df_w + 1)
  expect_equal(omega, expected)
  expect_gt(omega, 0)
  expect_lt(omega, 1)
})

test_that("omega_squared is less biased (smaller) than eta_squared", {
  # For the same F/df, ω² ≤ η²
  F <- 5; df_b <- 3; df_w <- 50
  eta <- effect_size(type = "eta_squared", F = F,
                     df_between = df_b, df_within = df_w)
  omega <- effect_size(type = "omega_squared", F = F,
                       df_between = df_b, df_within = df_w)
  expect_lte(omega, eta)
})

test_that("effect_size omega_squared via SS values", {
  # ω² = (SS_b - df_b * MS_w) / (SS_total + MS_w)
  ss_between <- 30; ss_within <- 90; ss_total <- 120
  df_b <- 2; df_w <- 60
  ms_w <- ss_within / df_w
  omega <- effect_size(type = "omega_squared",
                       ss_between = ss_between, ss_within = ss_within,
                       ss_total = ss_total, df_between = df_b, df_within = df_w)
  expected <- (ss_between - df_b * ms_w) / (ss_total + ms_w)
  expect_equal(omega, expected)
})

test_that("effect_size omega_squared accepts ms_within directly", {
  ss_between <- 30; ss_total <- 120; df_b <- 2; ms_w <- 1.5
  omega <- effect_size(type = "omega_squared",
                       ss_between = ss_between, ss_total = ss_total,
                       ms_within = ms_w, df_between = df_b)
  expected <- (ss_between - df_b * ms_w) / (ss_total + ms_w)
  expect_equal(omega, expected)
})

test_that("effect_size omega_squared errors without required args", {
  expect_error(effect_size(type = "omega_squared"),
               "omega_squared requires")
})

test_that("omega_squared is zero when F = 1 (no effect)", {
  omega <- effect_size(type = "omega_squared", F = 1,
                       df_between = 2, df_within = 60)
  # (1*2 - 2) / (1*2 + 60 + 1) = 0 / 63 = 0
  expect_equal(omega, 0)
})

# ----------------------------------------------------------------------------
# R3-C2: odds_ratio
# ----------------------------------------------------------------------------

test_that("odds_ratio computes correctly", {
  # OR = (a*d) / (b*c)
  r <- odds_ratio(a = 45, b = 25, c = 20, d = 60)
  expect_equal(r$odds_ratio, (45 * 60) / (25 * 20), tolerance = 1e-9)
  expect_length(r$conf.int, 2L)
  expect_true(r$conf.int[1] < r$odds_ratio)
  expect_true(r$conf.int[2] > r$odds_ratio)
  expect_equal(r$conf.level, 0.95)
  expect_true(isFALSE(r$corrected))
})

test_that("odds_ratio equals 1 when distribution is symmetric", {
  # a=50, b=50, c=50, d=50 -> OR = (50*50)/(50*50) = 1
  r <- odds_ratio(a = 50, b = 50, c = 50, d = 50)
  expect_equal(r$odds_ratio, 1)
})

test_that("odds_ratio applies Haldane-Anscombe correction for zero cells", {
  # With a zero cell, the correction adds 0.5 to all cells
  r <- odds_ratio(a = 0, b = 50, c = 20, d = 60)
  expect_true(r$corrected)
  expect_true(is.finite(r$odds_ratio))
  # Corrected OR = (0.5 * 60.5) / (50.5 * 20.5)
  expected <- (0.5 * 60.5) / (50.5 * 20.5)
  expect_equal(r$odds_ratio, expected, tolerance = 1e-9)
})

test_that("odds_ratio CI brackets the estimate", {
  r <- odds_ratio(a = 45, b = 25, c = 20, d = 60, conf_level = 0.99)
  expect_equal(r$conf.level, 0.99)
  expect_true(r$conf.int[1] < r$odds_ratio)
  expect_true(r$conf.int[2] > r$odds_ratio)
})

test_that("odds_ratio log_or and se are consistent", {
  r <- odds_ratio(a = 30, b = 40, c = 15, d = 60)
  expect_equal(r$log_or, log(r$odds_ratio), tolerance = 1e-9)
  # CI = exp(log_or +/- z * se)
  z <- stats::qnorm(1 - (1 - r$conf.level) / 2)
  expect_equal(r$conf.int[1],
               exp(r$log_or - z * r$se_log_or), tolerance = 1e-9)
  expect_equal(r$conf.int[2],
               exp(r$log_or + z * r$se_log_or), tolerance = 1e-9)
})

test_that("odds_ratio errors on invalid inputs", {
  expect_error(odds_ratio(a = -1, b = 1, c = 1, d = 1), "non-negative")
  expect_error(odds_ratio(a = 0, b = 0, c = 1, d = 1), "exposure group")
  expect_error(odds_ratio(a = 0, b = 1, c = 0, d = 1), "outcome group")
})

# ----------------------------------------------------------------------------
# R3-C2: relative_risk
# ----------------------------------------------------------------------------

test_that("relative_risk computes correctly", {
  # RR = (a/(a+b)) / (c/(c+d))
  a <- 30; b <- 70; c <- 10; d <- 90
  r <- relative_risk(a = a, b = b, c = c, d = d)
  expected <- (a / (a + b)) / (c / (c + d))
  expect_equal(r$relative_risk, expected, tolerance = 1e-9)
  expect_length(r$conf.int, 2L)
  expect_true(r$conf.int[1] < r$relative_risk)
  expect_true(r$conf.int[2] > r$relative_risk)
  expect_equal(r$risk_exposed, a / (a + b), tolerance = 1e-9)
  expect_equal(r$risk_unexposed, c / (c + d), tolerance = 1e-9)
})

test_that("relative_risk equals 1 when risks are equal", {
  r <- relative_risk(a = 30, b = 70, c = 30, d = 70)
  expect_equal(r$relative_risk, 1)
})

test_that("relative_risk CI brackets the estimate", {
  r <- relative_risk(a = 30, b = 70, c = 10, d = 90, conf_level = 0.90)
  expect_equal(r$conf.level, 0.90)
  expect_true(r$conf.int[1] < r$relative_risk)
  expect_true(r$conf.int[2] > r$relative_risk)
})

test_that("relative_risk log_rr and se are consistent", {
  r <- relative_risk(a = 30, b = 70, c = 10, d = 90)
  expect_equal(r$log_rr, log(r$relative_risk), tolerance = 1e-9)
  z <- stats::qnorm(1 - (1 - r$conf.level) / 2)
  expect_equal(r$conf.int[1],
               exp(r$log_rr - z * r$se_log_rr), tolerance = 1e-9)
  expect_equal(r$conf.int[2],
               exp(r$log_rr + z * r$se_log_rr), tolerance = 1e-9)
})

test_that("relative_risk handles zero events with correction", {
  # a=0 -> RR = 0; the function should warn but still return a finite CI
  expect_warning(
    r <- relative_risk(a = 0, b = 100, c = 10, d = 90),
    "zero event"
  )
  expect_true(is.finite(r$se_log_rr))
})

test_that("relative_risk errors on invalid inputs", {
  expect_error(relative_risk(a = -1, b = 1, c = 1, d = 1), "non-negative")
  expect_error(relative_risk(a = 0, b = 0, c = 1, d = 1), "exposure group")
})

# ----------------------------------------------------------------------------
# R3-C2: OR vs RR relationship
# ----------------------------------------------------------------------------

test_that("odds_ratio and relative_risk are close for rare outcomes", {
  # When the outcome is rare (small a/c relative to b/d), OR ≈ RR
  a <- 5; b <- 995; c <- 2; d <- 998
  or <- odds_ratio(a, b, c, d)$odds_ratio
  rr <- relative_risk(a, b, c, d)$relative_risk
  # Within 20% of each other for this rare-outcome case
  expect_true(abs(or - rr) / rr < 0.20)
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

# ============================================================================
# R3-B4: Sample size extensions tests
#   - sample_size_paired
#   - sample_size_regression
#   - sample_size_ci
#   - sample_size_tolerance
#   - sample_size_reliability
# ============================================================================

# ----------------------------------------------------------------------------
# sample_size_paired
# ----------------------------------------------------------------------------

test_that("sample_size_paired returns a valid list", {
  r <- sample_size_paired(mu0 = 0, mu1 = 0.5, sigma_d = 1, power = 0.80)
  expect_type(r, "list")
  expect_true("n" %in% names(r))
  expect_true("actual_power" %in% names(r))
  expect_true("effect_size" %in% names(r))
  expect_gt(r$n, 0)
})

test_that("sample_size_paired achieves target power (two-sided)", {
  r <- sample_size_paired(mu0 = 0, mu1 = 0.5, sigma_d = 1, power = 0.80)
  expect_gte(r$actual_power, 0.80)
})

test_that("sample_size_paired achieves target power (greater)", {
  r <- sample_size_paired(mu0 = 0, mu1 = 0.6, sigma_d = 1,
                          power = 0.90, alternative = "greater")
  expect_gte(r$actual_power, 0.90)
})

test_that("sample_size_paired matches one-sample t with sigma = sigma_d", {
  # Paired test on differences is a one-sample t-test with sigma = sigma_d
  paired <- sample_size_paired(mu0 = 0, mu1 = 0.5, sigma_d = 1, power = 0.80)
  one_samp <- sample_size_mean(mu0 = 0, mu1 = 0.5, sigma = 1, power = 0.80)
  expect_equal(paired$n, one_samp$n)
  expect_equal(paired$effect_size, one_samp$effect_size)
})

test_that("sample_size_paired rejects zero delta", {
  expect_error(sample_size_paired(mu0 = 0, mu1 = 0, sigma_d = 1), "mu1 must differ")
})

test_that("sample_size_paired rejects non-positive sigma_d", {
  expect_error(sample_size_paired(mu0 = 0, mu1 = 0.5, sigma_d = 0), "positive")
  expect_error(sample_size_paired(mu0 = 0, mu1 = 0.5, sigma_d = -1), "positive")
})

test_that("sample_size_paired larger effect => smaller n", {
  small <- sample_size_paired(mu0 = 0, mu1 = 0.3, sigma_d = 1, power = 0.80)
  large <- sample_size_paired(mu0 = 0, mu1 = 0.8, sigma_d = 1, power = 0.80)
  expect_lt(large$n, small$n)
})

# ----------------------------------------------------------------------------
# sample_size_regression
# ----------------------------------------------------------------------------

test_that("sample_size_regression returns a valid list", {
  r <- sample_size_regression(p = 3, r_squared = 0.30, power = 0.80)
  expect_type(r, "list")
  expect_true(all(c("n", "actual_power", "f_squared", "r_squared") %in% names(r)))
  expect_gt(r$n, 0)
})

test_that("sample_size_regression achieves target power", {
  r <- sample_size_regression(p = 3, r_squared = 0.30, power = 0.80)
  expect_gte(r$actual_power, 0.80)
})

test_that("sample_size_regression n >= p + 2", {
  r <- sample_size_regression(p = 5, r_squared = 0.50, power = 0.80)
  expect_gte(r$n, 5 + 2)
})

test_that("sample_size_regression f_squared = R^2 / (1 - R^2)", {
  r <- sample_size_regression(p = 2, r_squared = 0.25, power = 0.80)
  expect_equal(r$f_squared, 0.25 / (1 - 0.25))
})

test_that("sample_size_regression larger R^2 => smaller n", {
  weak <- sample_size_regression(p = 3, r_squared = 0.10, power = 0.80)
  strong <- sample_size_regression(p = 3, r_squared = 0.50, power = 0.80)
  expect_lt(strong$n, weak$n)
})

test_that("sample_size_regression more predictors => larger n", {
  few <- sample_size_regression(p = 2, r_squared = 0.30, power = 0.80)
  many <- sample_size_regression(p = 10, r_squared = 0.30, power = 0.80)
  expect_gte(many$n, few$n)
})

test_that("sample_size_regression rejects invalid inputs", {
  expect_error(sample_size_regression(p = 0, r_squared = 0.3), "at least 1")
  expect_error(sample_size_regression(p = 3, r_squared = 0), "\\(0, 1\\)")
  expect_error(sample_size_regression(p = 3, r_squared = 1), "\\(0, 1\\)")
})

test_that("sample_size_regression power matches noncentral F at returned n", {
  r <- sample_size_regression(p = 4, r_squared = 0.25, power = 0.85)
  n <- r$n; p <- 4; f2 <- r$f_squared
  df1 <- p; df2 <- n - p - 1
  f_crit <- stats::qf(1 - 0.05, df1, df2)
  expected_power <- 1 - stats::pf(f_crit, df1, df2, ncp = n * f2)
  expect_equal(r$actual_power, expected_power, tolerance = 1e-10)
})

# ----------------------------------------------------------------------------
# sample_size_ci
# ----------------------------------------------------------------------------

test_that("sample_size_ci mean returns valid list", {
  r <- sample_size_ci(type = "mean", h = 0.3, sigma = 1, conf_level = 0.95)
  expect_type(r, "list")
  expect_true(all(c("n", "half_width", "full_width", "conf_level") %in% names(r)))
  expect_gt(r$n, 0)
  expect_equal(r$half_width, 0.3)
  expect_equal(r$full_width, 0.6)
})

test_that("sample_size_ci mean formula matches n = (z*sigma/h)^2", {
  r <- sample_size_ci(type = "mean", h = 0.5, sigma = 2, conf_level = 0.95)
  z <- stats::qnorm(1 - 0.025)
  expected_n <- ceiling((z * 2 / 0.5)^2)
  expect_equal(r$n, expected_n)
})

test_that("sample_size_ci proportion formula matches n = z^2*p*(1-p)/h^2", {
  r <- sample_size_ci(type = "proportion", h = 0.05, p = 0.5, conf_level = 0.95)
  z <- stats::qnorm(1 - 0.025)
  expected_n <- ceiling((z^2) * 0.5 * 0.5 / (0.05^2))
  expect_equal(r$n, expected_n)
})

test_that("sample_size_ci smaller half-width => larger n", {
  wide <- sample_size_ci(type = "mean", h = 0.5, sigma = 1, conf_level = 0.95)
  narrow <- sample_size_ci(type = "mean", h = 0.1, sigma = 1, conf_level = 0.95)
  expect_gt(narrow$n, wide$n)
})

test_that("sample_size_ci higher confidence => larger n", {
  lo <- sample_size_ci(type = "mean", h = 0.3, sigma = 1, conf_level = 0.90)
  hi <- sample_size_ci(type = "mean", h = 0.3, sigma = 1, conf_level = 0.99)
  expect_gt(hi$n, lo$n)
})

test_that("sample_size_ci rejects invalid inputs", {
  expect_error(sample_size_ci(type = "mean", h = 0, sigma = 1), "positive")
  expect_error(sample_size_ci(type = "mean", h = 0.3, sigma = NULL), "sigma")
  expect_error(sample_size_ci(type = "mean", h = 0.3, sigma = -1), "sigma")
  expect_error(sample_size_ci(type = "proportion", h = 0.05, p = 2), "\\[0, 1\\]")
  expect_error(sample_size_ci(h = 0.3, conf_level = 1.5), "\\(0, 1\\)")
})

# ----------------------------------------------------------------------------
# sample_size_tolerance
# ----------------------------------------------------------------------------

test_that("sample_size_tolerance returns valid list", {
  r <- sample_size_tolerance(p = 0.95, conf_level = 0.95, max_half_width = 3.0)
  expect_type(r, "list")
  expect_true(all(c("n", "k_factor", "half_width", "half_width_in_sigma") %in% names(r)))
  expect_gt(r$n, 0)
})

test_that("sample_size_tolerance n grows as content p increases", {
  # Higher content requires a larger k-factor, hence more samples to satisfy
  # the same width constraint
  low_p <- sample_size_tolerance(p = 0.90, conf_level = 0.95, max_half_width = 3.0)
  high_p <- sample_size_tolerance(p = 0.99, conf_level = 0.95, max_half_width = 3.0)
  expect_gte(high_p$n, low_p$n)
})

test_that("sample_size_tolerance n grows as confidence increases", {
  lo <- sample_size_tolerance(p = 0.95, conf_level = 0.90, max_half_width = 3.0)
  hi <- sample_size_tolerance(p = 0.95, conf_level = 0.99, max_half_width = 3.0)
  expect_gte(hi$n, lo$n)
})

test_that("sample_size_tolerance tighter width => larger n", {
  wide <- sample_size_tolerance(p = 0.95, conf_level = 0.95, max_half_width = 5.0)
  tight <- sample_size_tolerance(p = 0.95, conf_level = 0.95, max_half_width = 2.5)
  expect_gte(tight$n, wide$n)
})

test_that("sample_size_tolerance half_width_in_sigma <= max_half_width", {
  r <- sample_size_tolerance(p = 0.95, conf_level = 0.95, max_half_width = 3.0)
  expect_lte(r$half_width_in_sigma, 3.0)
})

test_that("sample_size_tolerance rejects invalid inputs", {
  expect_error(sample_size_tolerance(p = 0, conf_level = 0.95, max_half_width = 3), "\\(0, 1\\)")
  expect_error(sample_size_tolerance(p = 0.95, conf_level = 0, max_half_width = 3), "\\(0, 1\\)")
  expect_error(sample_size_tolerance(p = 0.95, conf_level = 0.95, max_half_width = 0), "positive")
  expect_error(sample_size_tolerance(p = 0.95, conf_level = 0.95, max_half_width = 3, sigma = -1), "positive")
})

# ----------------------------------------------------------------------------
# sample_size_reliability
# ----------------------------------------------------------------------------

test_that("sample_size_reliability zero-failure returns valid list", {
  r <- sample_size_reliability(reliability = 0.95, conf_level = 0.90)
  expect_type(r, "list")
  expect_true(all(c("n", "reliability", "conf_level", "n_failures") %in% names(r)))
  expect_gt(r$n, 0)
  expect_equal(r$n_failures, 0L)
})

test_that("sample_size_reliability matches success-run formula", {
  R <- 0.95; conf <- 0.90
  r <- sample_size_reliability(reliability = R, conf_level = conf)
  expected_n <- ceiling(log(1 - conf) / log(R))
  expect_equal(r$n, expected_n)
})

test_that("sample_size_reliability achieved confidence >= target", {
  r <- sample_size_reliability(reliability = 0.95, conf_level = 0.90)
  expect_gte(r$achieved_conf_level, 0.90)
})

test_that("sample_size_reliability higher reliability => larger n (0 failures)", {
  # Success-run: n = log(1-conf)/log(R); as R -> 1, log(R) -> 0, so n grows.
  # Demonstrating higher reliability requires MORE samples.
  lo <- sample_size_reliability(reliability = 0.90, conf_level = 0.95)
  hi <- sample_size_reliability(reliability = 0.99, conf_level = 0.95)
  expect_gt(hi$n, lo$n)
})

test_that("sample_size_reliability higher confidence => larger n (0 failures)", {
  lo <- sample_size_reliability(reliability = 0.95, conf_level = 0.80)
  hi <- sample_size_reliability(reliability = 0.95, conf_level = 0.99)
  expect_gt(hi$n, lo$n)
})

test_that("sample_size_reliability allowing failures => larger n", {
  zero <- sample_size_reliability(reliability = 0.95, conf_level = 0.90, n_failures = 0)
  one <- sample_size_reliability(reliability = 0.95, conf_level = 0.90, n_failures = 1)
  expect_gt(one$n, zero$n)
})

test_that("sample_size_reliability with failures controls consumer risk", {
  # P(X <= r | n, 1 - R) <= 1 - conf_level
  r <- sample_size_reliability(reliability = 0.95, conf_level = 0.90, n_failures = 2)
  prob <- stats::pbinom(2, size = r$n, prob = 1 - 0.95)
  expect_lte(prob, 1 - 0.90 + 1e-9)
})

test_that("sample_size_reliability rejects invalid inputs", {
  expect_error(sample_size_reliability(reliability = 0, conf_level = 0.9), "\\(0, 1\\)")
  expect_error(sample_size_reliability(reliability = 0.95, conf_level = 0), "\\(0, 1\\)")
  expect_error(sample_size_reliability(reliability = 0.95, conf_level = 0.9, n_failures = -1), "non-negative")
})
