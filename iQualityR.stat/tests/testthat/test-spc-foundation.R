# =============================================================================
# File: tests/testthat/test-spc-foundation.R
# Description: SPC foundation module tests
#   - Sigma estimation (sigma_estimate.R)
#   - SPC constants (constant.R)
#   - SPC out-of-control rules (spc_rules.R)
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# Sigma estimation (sigma_estimate.R)
# ----------------------------------------------------------------------------

test_that("sigma_estimate total method returns numeric", {
  set.seed(123)
  x <- rnorm(100, mean = 10, sd = 2)
  result <- sigma_estimate(x, method = "total")
  expect_type(result, "double")
  expect_gt(result, 0)
})

test_that("sigma_estimate subgroup methods", {
  set.seed(123)
  x <- rnorm(100, mean = 10, sd = 2)
  subgroup <- rep(1:20, each = 5)

  methods <- c("r_bar", "s_bar", "pooled_s", "mr_bar", "mr_median", "mssd")
  for (m in methods) {
    result <- sigma_estimate(x, subgroup = subgroup, method = m)
    expect_type(result, "double")
    expect_gt(result, 0)
  }
})

test_that("sigma_estimate moving range without subgroup", {
  set.seed(123)
  x <- rnorm(100, mean = 10, sd = 2)
  result <- sigma_estimate(x, method = "mr_bar")
  expect_type(result, "double")
  expect_gt(result, 0)
})

test_that("sigma_estimate with n_size parameter", {
  set.seed(123)
  x <- rnorm(50, mean = 10, sd = 2)
  result <- sigma_estimate(x, n_size = 5, method = "r_bar")
  expect_type(result, "double")
  expect_gt(result, 0)
})

test_that("sigma_estimate with data.frame input", {
  set.seed(123)
  x <- rnorm(50, mean = 10, sd = 2)
  subgroup <- rep(1:10, each = 5)
  df <- data.frame(value = x, group = subgroup)
  result <- sigma_estimate(df, x_col = "value", subgroup_col = "group",
                           method = "pooled_s")
  expect_type(result, "double")
  expect_gt(result, 0)
})

test_that("sigma_estimate data.frame input errors without column names", {
  df <- data.frame(value = 1:10, group = rep(1:2, each = 5))
  expect_error(sigma_estimate(df, method = "r_bar"), "x_col")
})

test_that("sigma_estimate errors on subgroup length mismatch", {
  x <- rnorm(10)
  subgroup <- rep(1:2, each = 4)  # length 8, not 10
  expect_error(sigma_estimate(x, subgroup = subgroup, method = "r_bar"),
               "mismatch")
})

test_that("sigma_estimate with use_unbiased=FALSE", {
  set.seed(123)
  x <- rnorm(50, mean = 10, sd = 2)
  subgroup <- rep(1:10, each = 5)
  res_unbiased <- sigma_estimate(x, subgroup = subgroup, method = "s_bar",
                                  use_unbiased = TRUE)
  res_biased <- sigma_estimate(x, subgroup = subgroup, method = "s_bar",
                                use_unbiased = FALSE)
  # Unbiased should generally be larger (dividing by c4 < 1)
  expect_true(is.numeric(res_unbiased))
  expect_true(is.numeric(res_biased))
})

test_that("sigma_decomposition returns data.frame with all components", {
  set.seed(123)
  between_var <- rnorm(10, mean = 0, sd = 1.5)
  data <- unlist(lapply(between_var, function(x) rnorm(5, mean = x, sd = 1)))
  subgroup <- rep(1:10, each = 5)
  result <- sigma_decomposition(data, subgroup = subgroup)
  expect_s3_class(result, "data.frame")
  expect_true("sigma_within" %in% names(result))
  expect_true("sigma_between" %in% names(result))
  expect_true("sigma_between_within" %in% names(result))
  expect_true("sigma_total" %in% names(result))
  expect_true("n_subgroup" %in% names(result))
  expect_equal(result$n_subgroup, 5)
})

test_that("sigma_decomposition with n_size parameter", {
  set.seed(123)
  x <- rnorm(50, mean = 10, sd = 2)
  result <- sigma_decomposition(x, n_size = 5)
  expect_s3_class(result, "data.frame")
  expect_equal(result$n_subgroup, 5)
})

# ----------------------------------------------------------------------------
# SPC constants (constant.R)
# ----------------------------------------------------------------------------

test_that("SPC constant functions return positive values", {
  expect_gt(get_d2(2), 0)
  expect_gt(get_d3(2), 0)
  expect_gt(get_c4(2), 0)
  expect_gt(get_A2(2), 0)
  expect_gte(get_B3(2), 0)
  expect_gt(get_B4(2), 0)
})

test_that("SPC constants vary with sample size", {
  d2_2 <- get_d2(2)
  d2_5 <- get_d2(5)
  expect_true(d2_2 != d2_5)
})

test_that("get_D4 and get_D3 constants", {
  expect_gt(get_D4(5), 1)
  expect_gte(get_D3(5), 0)
})

# ----------------------------------------------------------------------------
# Comprehensive SPC constants coverage (R2-4: 8+ functions)
# ----------------------------------------------------------------------------

test_that("get_d2 known values", {
  # d2(2) = 1.128, d2(5) = 2.326 (ASTM E2587 / standard tables)
  expect_equal(round(get_d2(2), 3), 1.128)
  expect_equal(round(get_d2(5), 3), 2.326)
  expect_equal(round(get_d2(10), 3), 3.078)
})

test_that("get_d3 known values", {
  # d3(2) = 0.8525, d3(5) = 0.8641
  expect_equal(round(get_d3(2), 4), 0.8525)
  expect_gt(get_d3(5), 0.8)
  expect_lt(get_d3(5), 0.9)
})

test_that("get_c4 known values", {
  # c4(2) = 0.7979, c4(5) = 0.94
  expect_equal(round(get_c4(2), 4), 0.7979)
  expect_gt(get_c4(5), 0.93)
  expect_lt(get_c4(5), 0.95)
})

test_that("get_c4_prime returns valid values for n >= 2", {
  v2 <- get_c4_prime(2)
  v5 <- get_c4_prime(5)
  v10 <- get_c4_prime(10)
  expect_type(v2, "double")
  expect_gt(v2, 0)
  expect_lt(v2, 1)
  expect_gt(v5, 0)
  expect_lt(v5, 1)
  expect_gt(v10, 0)
  expect_lt(v10, 1)
})

test_that("get_c4_prime returns NA for n < 2", {
  expect_true(is.na(get_c4_prime(1)))
})

test_that("get_c4_prime_cpp matches get_c4_prime", {
  # Both use Monte Carlo integration, so they agree to ~3 decimal places but
  # not exactly (different random draws per call). Use approximate equality.
  r_val <- get_c4_prime(5)
  cpp_val <- get_c4_prime_cpp(5L, 1000000L)
  expect_gt(r_val, 0.92)
  expect_lt(r_val, 0.93)
  expect_gt(cpp_val, 0.92)
  expect_lt(cpp_val, 0.93)
  expect_equal(round(r_val, 2), round(cpp_val, 2))
})

test_that("get_A2 known values", {
  # A2(2) = 1.880, A2(5) = 0.577
  expect_equal(round(get_A2(2), 3), 1.88)
  expect_equal(round(get_A2(5), 3), 0.577)
})

test_that("get_A3 returns positive values", {
  expect_gt(get_A3(2), 0)
  expect_gt(get_A3(5), 0)
  # A3(5) ~= 1.427
  expect_equal(round(get_A3(5), 3), 1.427)
})

test_that("get_D3 returns 0 for small n (no lower limit)", {
  # D3 is 0 for n <= 6 (lower range limit doesn't exist)
  expect_equal(get_D3(2), 0)
  expect_equal(get_D3(5), 0)
  # D3(7) > 0
  expect_gt(get_D3(7), 0)
})

test_that("get_D4 known values", {
  # D4(2) = 3.267, D4(5) = 2.114
  expect_equal(round(get_D4(2), 3), 3.267)
  expect_equal(round(get_D4(5), 3), 2.114)
})

test_that("get_B3 returns valid values", {
  # B3(2) = 0, B3(5) = 0
  expect_equal(get_B3(2), 0)
  expect_equal(get_B3(5), 0)
  # B3(10) > 0
  expect_gt(get_B3(10), 0)
})

test_that("get_B4 known values", {
  # B4(2) = 3.267, B4(5) = 2.089
  expect_equal(round(get_B4(2), 3), 3.267)
  expect_equal(round(get_B4(5), 3), 2.089)
})

test_that("get_E2 returns positive values", {
  # E2 = k / d2; d2(2) = 1.12838, so E2(2) = 3 / 1.12838 = 2.659
  expect_gt(get_E2(2), 0)
  expect_gt(get_E2(5), 0)
  expect_equal(round(get_E2(2), 3), 2.659)
})

test_that("get_d4 returns positive values", {
  expect_gt(get_d4(2), 0)
  expect_gt(get_d4(5), 0)
  expect_gt(get_d4(10), 0)
})

test_that("SPC constants are vectorized over n", {
  n_vec <- c(2, 5, 10)
  d2_vec <- get_d2(n_vec)
  expect_length(d2_vec, 3)
  expect_equal(d2_vec[1], get_d2(2))
  expect_equal(d2_vec[2], get_d2(5))
})

test_that("SPC constants D3/D4 relationship with d2/d3", {
  # D4 = 1 + k*(d3/d2), D3 = max(0, 1 - k*(d3/d2))
  n <- 10
  k <- 3
  expected_D4 <- 1 + k * (get_d3(n) / get_d2(n))
  expected_D3 <- max(0, 1 - k * (get_d3(n) / get_d2(n)))
  expect_equal(get_D4(n), expected_D4)
  expect_equal(get_D3(n), expected_D3)
})

# ----------------------------------------------------------------------------
# SPC out-of-control rules (spc_rules.R)
# ----------------------------------------------------------------------------

test_that("detect_spc_violations out-of-control detection", {
  set.seed(123)
  x <- c(rnorm(20, mean = 10, sd = 1), 13.5, rnorm(10, mean = 10, sd = 1))
  result <- detect_spc_violations(x, center = 10, sigma = 1)

  expect_type(result, "list")
  expect_true("violations" %in% names(result))
  expect_true("is_in_control" %in% names(result))
  expect_true("rules_triggered" %in% names(result))
})

test_that("detect_spc_violations in-control data", {
  set.seed(123)
  x <- rnorm(30, mean = 10, sd = 1)
  result <- detect_spc_violations(x, center = 10, sigma = 1)

  expect_true(result$is_in_control)
})

test_that("list_spc_rules rules data.frame", {
  rules <- list_spc_rules()
  expect_s3_class(rules, "data.frame")
  expect_gte(nrow(rules), 8)
  expect_true("Rule" %in% names(rules))
  expect_true("Description" %in% names(rules))
})

# ----------------------------------------------------------------------------
# Per-rule detection with controlled data (check_spc_rule isolates one rule)
# ----------------------------------------------------------------------------

test_that("Rule 1 detects a point beyond 3-sigma", {
  x <- c(10, 10, 10, 10, 20)
  r <- check_spc_rule(x, rule = 1, center = 10, sigma = 1)
  expect_true(r$triggered)
  expect_equal(r$indices, 5)
})

test_that("Rule 1 does not trigger when all points within 3-sigma", {
  x <- c(10, 11, 9, 10.5, 9.5)
  r <- check_spc_rule(x, rule = 1, center = 10, sigma = 1)
  expect_false(r$triggered)
})

test_that("Rule 2 detects 9 consecutive points on same side", {
  x <- c(rep(10.5, 9), 10, 10)
  r <- check_spc_rule(x, rule = 2, center = 10, sigma = 1)
  expect_true(r$triggered)
  expect_equal(r$indices, 9)
})

test_that("Rule 2 does not trigger with only 8 consecutive same side", {
  x <- c(rep(10.5, 8), 9.5, 10.5, 10.5)
  r <- check_spc_rule(x, rule = 2, center = 10, sigma = 1)
  expect_false(r$triggered)
})

# --- Rule 3 (6-point trend) -- window-size fix verification ---

test_that("Rule 3 detects 6 strictly increasing points", {
  x <- 1:6
  r <- check_spc_rule(x, rule = 3, center = 3.5, sigma = 1)
  expect_true(r$triggered)
  expect_equal(r$indices, 6)
})

test_that("Rule 3 detects 6 strictly decreasing points", {
  x <- 6:1
  r <- check_spc_rule(x, rule = 3, center = 3.5, sigma = 1)
  expect_true(r$triggered)
  expect_equal(r$indices, 6)
})

test_that("Rule 3 does not trigger with only 5 increasing points", {
  # Pre-fix this passed spuriously because the window required 7 points;
  # post-fix the window is exactly 6, so 5 points cannot trigger.
  x <- 1:5
  r <- check_spc_rule(x, rule = 3, center = 3, sigma = 1)
  expect_false(r$triggered)
})

test_that("Rule 3 does not trigger on non-strict (flat) trend", {
  # A repeated value breaks strict monotonicity.
  x <- c(1, 2, 2, 3, 4, 5)
  r <- check_spc_rule(x, rule = 3, center = 3, sigma = 1)
  expect_false(r$triggered)
})

test_that("Rule 3 detects trend in a longer series at correct indices", {
  # Windows ending at 6, 7, 8 are all strictly increasing:
  # x[1:6]=1..6, x[2:7]=2..7, x[3:8]=3,4,5,6,7,10 => violations at 6,7,8.
  x <- c(1, 2, 3, 4, 5, 6, 7, 10, 10, 10)
  r <- check_spc_rule(x, rule = 3, center = 5, sigma = 1)
  expect_true(r$triggered)
  expect_equal(r$indices, c(6, 7, 8))
})

# --- Rule 4 (14-point alternation) -- window-size fix verification ---

test_that("Rule 4 detects 14 alternating points", {
  x <- rep(c(1, 3), 7)  # 14 points: 1,3,1,3,...,1,3
  r <- check_spc_rule(x, rule = 4, center = 2, sigma = 1)
  expect_true(r$triggered)
  expect_equal(r$indices, 14)
})

test_that("Rule 4 detects 14 alternating points (opposite phase)", {
  x <- rep(c(3, 1), 7)  # 14 points: 3,1,3,1,...,3,1
  r <- check_spc_rule(x, rule = 4, center = 2, sigma = 1)
  expect_true(r$triggered)
  expect_equal(r$indices, 14)
})

test_that("Rule 4 does not trigger with only 13 alternating points", {
  x <- rep(c(1, 3), 7)[1:13]  # 13 points alternating
  r <- check_spc_rule(x, rule = 4, center = 2, sigma = 1)
  expect_false(r$triggered)
})

test_that("Rule 4 does not trigger when alternation breaks (flat segment)", {
  # 14 points but positions 8->9 are equal, breaking the alternation.
  x <- c(1, 3, 1, 3, 1, 3, 1, 3, 3, 1, 3, 1, 3, 1)
  r <- check_spc_rule(x, rule = 4, center = 2, sigma = 1)
  expect_false(r$triggered)
})

test_that("Rule 4 does not trigger when alternation breaks (same direction)", {
  # 14 points with two consecutive increases in the middle.
  x <- c(1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 3, 1, 3, 1)
  r <- check_spc_rule(x, rule = 4, center = 2, sigma = 1)
  expect_false(r$triggered)
})

# --- Rules 5-8 controlled-data coverage ---

test_that("Rule 5 detects 2 of 3 beyond 2-sigma (same side)", {
  x <- c(10, 10, 13, 10, 13, 10)
  r <- check_spc_rule(x, rule = 5, center = 10, sigma = 1)
  expect_true(r$triggered)
  expect_equal(r$indices, 5)
})

test_that("Rule 5 does not trigger with only 1 of 3 beyond 2-sigma", {
  x <- c(10, 10, 13, 10, 10, 10)
  r <- check_spc_rule(x, rule = 5, center = 10, sigma = 1)
  expect_false(r$triggered)
})

test_that("Rule 6 detects 4 of 5 beyond 1-sigma (same side)", {
  x <- c(10, 12, 12, 12, 12, 10)
  r <- check_spc_rule(x, rule = 6, center = 10, sigma = 1)
  expect_true(r$triggered)
  expect_equal(r$indices, c(5, 6))
})

test_that("Rule 6 does not trigger with only 3 of 5 beyond 1-sigma", {
  x <- c(10, 12, 12, 12, 10, 10)
  r <- check_spc_rule(x, rule = 6, center = 10, sigma = 1)
  expect_false(r$triggered)
})

test_that("Rule 7 detects 15 consecutive points within 1-sigma", {
  x <- rep(10.2, 15)
  r <- check_spc_rule(x, rule = 7, center = 10, sigma = 1)
  expect_true(r$triggered)
  expect_equal(r$indices, 15)
})

test_that("Rule 7 does not trigger with 14 points within 1-sigma", {
  x <- rep(10.2, 14)
  r <- check_spc_rule(x, rule = 7, center = 10, sigma = 1)
  expect_false(r$triggered)
})

test_that("Rule 8 detects 8 consecutive points outside 1-sigma", {
  x <- c(12, 8, 12, 8, 12, 8, 12, 8)  # all outside [9,11]
  r <- check_spc_rule(x, rule = 8, center = 10, sigma = 1)
  expect_true(r$triggered)
  expect_equal(r$indices, 8)
})

test_that("Rule 8 does not trigger with only 7 points outside 1-sigma", {
  x <- c(12, 8, 12, 8, 12, 8, 12, 10)  # last point within band
  r <- check_spc_rule(x, rule = 8, center = 10, sigma = 1)
  expect_false(r$triggered)
})

# --- detect_spc_violations integration for Rule 3 and Rule 4 fixes ---

test_that("detect_spc_violations flags Rule 3 on 6 increasing points", {
  x <- 1:6
  result <- detect_spc_violations(x, center = 3.5, sigma = 1)
  expect_false(result$is_in_control)
  expect_true("Rule 3" %in% result$rules_triggered)
})

test_that("detect_spc_violations flags Rule 4 on 14 alternating points", {
  x <- rep(c(1, 3), 7)
  result <- detect_spc_violations(x, center = 2, sigma = 1)
  expect_false(result$is_in_control)
  expect_true("Rule 4" %in% result$rules_triggered)
})

test_that("detect_spc_violations handles sigma = 0 gracefully", {
  x <- rep(5, 10)
  result <- detect_spc_violations(x, center = 5, sigma = 0)
  expect_true(result$is_in_control)
  expect_equal(result$n_violations, 0)
})

test_that("summarize_spc_rules text and data.frame output", {
  x <- c(1:6, 10, 10)
  result <- detect_spc_violations(x, center = 5, sigma = 1)
  txt <- summarize_spc_rules(result, format = "text")
  expect_type(txt, "character")
  df <- summarize_spc_rules(result, format = "data.frame")
  if (nrow(df) > 0) expect_s3_class(df, "data.frame")
})

test_that("check_spc_rule rejects invalid rule numbers", {
  x <- rnorm(10)
  expect_error(check_spc_rule(x, rule = 0, center = 0, sigma = 1))
  expect_error(check_spc_rule(x, rule = 9, center = 0, sigma = 1))
})
