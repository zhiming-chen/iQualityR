# =============================================================================
# File: tests/testthat/test-stat-interpreter.R
# Description: Unit tests for R/StatInterpreter.R
#   Covers all public methods, all audience levels, all distribution/test
#   result types, significance-level branches, and error/fallback paths.
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# Shared helpers for building calc_result structures
# ----------------------------------------------------------------------------

# Build a prob-mode calc_result (mirrors ProbAnalyzer$.compute_prob output)
make_prob_calc_result <- function(target_x, result_p) {
  list(
    all_res = list(list(
      is_prob_mode = TRUE,
      target_x     = target_x,
      result_p     = result_p
    )),
    mode = "prob"
  )
}

# Build a quant-mode calc_result (mirrors ProbAnalyzer$.compute_quant output)
make_quant_calc_result <- function(target_p, result_x) {
  list(
    all_res = list(list(
      is_prob_mode = FALSE,
      target_p     = target_p,
      result_x     = result_x,
      target_x     = result_x
    )),
    mode = "quant"
  )
}

# Build a normality test result list (mirrors NormalityAnalyzer output)
make_normality_result <- function(p.value, is_normal, statistic = c(W = 0.95),
                                  method = "Shapiro-Wilk", alpha = 0.05,
                                  n = 50, skewness = NA_real_,
                                  excess_kurtosis = NA_real_) {
  list(
    test_type       = "Normality Test",
    method          = method,
    statistic       = statistic,
    p.value         = p.value,
    alpha           = alpha,
    is_normal       = is_normal,
    n               = n,
    skewness        = skewness,
    excess_kurtosis = excess_kurtosis
  )
}

# ============================================================================
# interpret() dispatcher
# ============================================================================

test_that("interpret dispatches htest objects", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  ht <- t.test(x, mu = 5)
  out <- interpreter$interpret(ht, audience = "manager")
  expect_type(out, "character")
  expect_true(nchar(out) > 0)
  expect_false(grepl("Unable to recognize", out))
})

test_that("interpret dispatches stat_result with htest_result class", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- HTestAnalyzer$new()$t_test_1s(x, mu = 5)
  out <- interpreter$interpret(result, audience = "manager")
  expect_type(out, "character")
  expect_false(grepl("Unable to recognize", out))
})

test_that("interpret dispatches aov objects", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  df <- data.frame(y = rnorm(60), g = factor(rep(1:3, each = 20)))
  fit <- aov(y ~ g, data = df)
  out <- interpreter$interpret(fit, audience = "manager")
  expect_type(out, "character")
  expect_true(grepl("ANOVA", out))
})

test_that("interpret dispatches anova objects (data.frame)", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  df <- data.frame(y = rnorm(60), g = factor(rep(1:3, each = 20)))
  fit <- lm(y ~ g, data = df)
  atab <- anova(fit)
  out <- interpreter$interpret(atab, audience = "technical")
  expect_type(out, "character")
  expect_true(grepl("ANOVA", out))
})

test_that("interpret dispatches ProbNode objects", {
  interpreter <- StatInterpreter$new()
  node <- ProbNode$new("n1", "norm", list(mean = 0, sd = 1))
  out <- interpreter$interpret(node, audience = "manager")
  expect_type(out, "character")
  expect_true(grepl("Distribution Analysis", out))
})

test_that("interpret dispatches dist list with type and calc_result", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 100, sd = 5),
    calc_result = make_prob_calc_result(105, pnorm(105, 100, 5))
  )
  out <- interpreter$interpret(dist_result, audience = "manager")
  expect_type(out, "character")
  expect_true(grepl("Distribution Analysis", out))
})

test_that("interpret dispatches normality test results via test_type", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.3, is_normal = TRUE)
  out <- interpreter$interpret(result, audience = "manager")
  expect_type(out, "character")
  expect_true(grepl("Normality Test", out))
})

test_that("interpret returns fallback message for unrecognized input", {
  interpreter <- StatInterpreter$new()
  out <- interpreter$interpret(123)
  expect_match(out, "Unable to recognize")
  out2 <- interpreter$interpret(list(foo = "bar"))
  expect_match(out2, "Unable to recognize")
})

# ============================================================================
# interpret_dist / .interpret_dist
# ============================================================================

test_that("interpret_dist handles ProbNode object (no calc_result)", {
  interpreter <- StatInterpreter$new()
  node <- ProbNode$new("n1", "binom", list(size = 10, prob = 0.5))
  out <- interpreter$interpret_dist(node, audience = "manager")
  expect_type(out, "character")
  expect_true(grepl("Binomial", out))
})

test_that("interpret_dist handles list with type + calc_result (prob mode)", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 0, sd = 1),
    calc_result = make_prob_calc_result(1.96, pnorm(1.96))
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_true(grepl("Distribution Analysis", out))
  expect_true(grepl("Calculation Result", out))
})

test_that("interpret_dist handles legacy list with type only (no calc_result field)", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 0, sd = 1)
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_type(out, "character")
  expect_true(grepl("Distribution Analysis", out))
})

test_that("interpret_dist returns error message for unparseable input", {
  interpreter <- StatInterpreter$new()
  out <- interpreter$interpret_dist(list(notype = 1), audience = "manager")
  expect_match(out, "Unable to parse distribution information")
})

test_that("interpret_dist handles unknown distribution type gracefully", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "unknown_dist_xyz",
    params = list(a = 1),
    calc_result = make_prob_calc_result(1, 0.5)
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_type(out, "character")
  # dist_desc is empty string but header still present
  expect_true(grepl("Distribution Analysis", out))
})

test_that("interpret_dist falls back to manager for unknown audience", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 0, sd = 1),
    calc_result = make_prob_calc_result(1.96, pnorm(1.96))
  )
  out <- interpreter$interpret_dist(dist_result, audience = "unknown_audience")
  expect_true(grepl("Manager Version", out))
})

# ----------------------------------------------------------------------------
# .dist_manager_explain - prob mode with various distribution types
# ----------------------------------------------------------------------------

test_that("dist manager explain: binom prob mode", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "binom",
    params = list(size = 50, prob = 0.05),
    calc_result = make_prob_calc_result(3, pbinom(3, 50, 0.05))
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_true(grepl("independent trials", out))
  expect_true(grepl("Business Recommendation", out))
})

test_that("dist manager explain: pois prob mode", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "pois",
    params = list(lambda = 2),
    calc_result = make_prob_calc_result(3, ppois(3, 2))
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_true(grepl("unit time/space", out))
})

test_that("dist manager explain: norm prob mode", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 100, sd = 5),
    calc_result = make_prob_calc_result(105, pnorm(105, 100, 5))
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_true(grepl("indicator falling within", out))
})

test_that("dist manager explain: other distribution prob mode (generic format)", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "exp",
    params = list(rate = 1),
    calc_result = make_prob_calc_result(1, pexp(1))
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_true(grepl("Calculation result", out))
})

# ----------------------------------------------------------------------------
# .dist_manager_explain - quant mode
# ----------------------------------------------------------------------------

test_that("dist manager explain: quant mode produces quantile text", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 0, sd = 1),
    calc_result = make_quant_calc_result(0.95, qnorm(0.95))
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_true(grepl("Cumulative probability", out))
  expect_true(grepl("quantile", out))
})

# ----------------------------------------------------------------------------
# .get_business_advice - all probability-level branches
# ----------------------------------------------------------------------------

test_that("dist manager advice: p_val >= 0.95 (high probability)", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 0, sd = 1),
    calc_result = make_prob_calc_result(3, pnorm(3))  # ~0.9987
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_true(grepl("process is in good condition", out))
})

test_that("dist manager advice: p_val in [0.90, 0.95)", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 0, sd = 1),
    calc_result = make_prob_calc_result(1.5, pnorm(1.5))  # ~0.9332
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_true(grepl("basically under control", out))
})

test_that("dist manager advice: p_val in [0.80, 0.90)", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 0, sd = 1),
    calc_result = make_prob_calc_result(1.0, pnorm(1.0))  # ~0.8413
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_true(grepl("acceptable but there is some risk", out))
})

test_that("dist manager advice: p_val in [0.50, 0.80)", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 0, sd = 1),
    calc_result = make_prob_calc_result(0.5, pnorm(0.5))  # ~0.6915
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_true(grepl("some quality risks", out))
})

test_that("dist manager advice: p_val < 0.50 (low probability)", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 0, sd = 1),
    calc_result = make_prob_calc_result(-0.5, pnorm(-0.5))  # ~0.3085
  )
  out <- interpreter$interpret_dist(dist_result, audience = "manager")
  expect_true(grepl("quality risk is high", out))
})

# ----------------------------------------------------------------------------
# .dist_technical_explain
# ----------------------------------------------------------------------------

test_that("dist technical explain: binom with prob mode", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "binom",
    params = list(size = 10, prob = 0.5),
    calc_result = make_prob_calc_result(3, pbinom(3, 10, 0.5))
  )
  out <- interpreter$interpret_dist(dist_result, audience = "technical")
  expect_true(grepl("Technical Version", out))
  expect_true(grepl("Cumulative Probability", out))
  expect_true(grepl("Bernoulli", out))
})

test_that("dist technical explain: norm with prob mode", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 100, sd = 5),
    calc_result = make_prob_calc_result(105, pnorm(105, 100, 5))
  )
  out <- interpreter$interpret_dist(dist_result, audience = "technical")
  expect_true(grepl("N\\(mu=", out))
  expect_true(grepl("cumulative probability", out))
})

test_that("dist technical explain: other distribution (generic CDF text)", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "exp",
    params = list(rate = 1),
    calc_result = make_prob_calc_result(1, pexp(1))
  )
  out <- interpreter$interpret_dist(dist_result, audience = "technical")
  expect_true(grepl("cumulative distribution function", out))
})

test_that("dist technical explain: without calc_result only shows info header", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(type = "norm", params = list(mean = 0, sd = 1))
  out <- interpreter$interpret_dist(dist_result, audience = "technical")
  expect_true(grepl("Technical Version", out))
  expect_true(grepl("Parameters:", out))
})

# ----------------------------------------------------------------------------
# .dist_client_explain
# ----------------------------------------------------------------------------

test_that("dist client explain: prob mode produces quality assurance report", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 100, sd = 5),
    calc_result = make_prob_calc_result(105, pnorm(105, 100, 5))
  )
  out <- interpreter$interpret_dist(dist_result, audience = "client")
  expect_true(grepl("Quality Assurance Report", out))
  expect_true(grepl("Analysis Conclusion", out))
  expect_true(grepl("process is under control", out))
})

test_that("dist client explain: without calc_result shows method only", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(type = "norm", params = list(mean = 0, sd = 1))
  out <- interpreter$interpret_dist(dist_result, audience = "client")
  expect_true(grepl("Quality Assurance Report", out))
  expect_true(grepl("Statistical Method", out))
})

# ============================================================================
# interpret_htest / .interpret_htest
# ============================================================================

test_that("interpret_htest manager: highly significant (p < 0.01)", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 7, sd = 1)
  ht <- t.test(x, mu = 5)  # p-value very small
  out <- interpreter$interpret_htest(ht, audience = "manager")
  expect_true(grepl("highly significant", out))
  expect_true(grepl("< 0.01", out))
})

test_that("interpret_htest manager: significant (p < 0.05 but >= 0.01)", {
  interpreter <- StatInterpreter$new()
  # Construct an htest-like list with p.value between 0.01 and 0.05
  ht <- list(
    method = "Dummy Test",
    p.value = 0.03,
    statistic = c(t = 2.3),
    conf.int = c(-1.5, -0.2)
  )
  class(ht) <- "htest"
  out <- interpreter$interpret_htest(ht, audience = "manager")
  expect_true(grepl("statistically significant", out))
  expect_false(grepl("highly significant", out))
})

test_that("interpret_htest manager: not significant (p >= 0.05)", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  ht <- t.test(x, mu = 5)  # p-value large
  out <- interpreter$interpret_htest(ht, audience = "manager")
  expect_true(grepl("not significant", out))
})

test_that("interpret_htest technical: with confidence interval, reject", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 7, sd = 1)
  ht <- t.test(x, mu = 5)
  out <- interpreter$interpret_htest(ht, audience = "technical")
  expect_true(grepl("Confidence Interval", out))
  expect_true(grepl("reject the null hypothesis", out))
  expect_false(grepl("do not reject", out))
})

test_that("interpret_htest technical: without confidence interval", {
  interpreter <- StatInterpreter$new()
  ht <- list(
    method = "Chi-square test",
    p.value = 0.3,
    statistic = c(X.squared = 2.4)
    # no conf.int
  )
  class(ht) <- "htest"
  out <- interpreter$interpret_htest(ht, audience = "technical")
  expect_false(grepl("Confidence Interval", out))
  expect_true(grepl("do not reject", out))
})

test_that("interpret_htest client: includes quality assurance header", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  ht <- t.test(x, mu = 5)
  out <- interpreter$interpret_htest(ht, audience = "client")
  expect_true(grepl("Quality Assurance Report", out))
})

test_that("interpret_htest unknown audience falls back to default header", {
  interpreter <- StatInterpreter$new()
  ht <- list(
    method = "Test",
    p.value = 0.3,
    statistic = c(t = 1.0),
    conf.int = c(-0.5, 0.5)
  )
  class(ht) <- "htest"
  out <- interpreter$interpret_htest(ht, audience = "weird")
  expect_true(grepl("Hypothesis Test Result Interpretation", out))
})

test_that("interpret_htest works with chi-square test (no conf.int, manager)", {
  interpreter <- StatInterpreter$new()
  ht <- chisq.test(c(20, 30, 50))
  out <- interpreter$interpret_htest(ht, audience = "manager")
  expect_type(out, "character")
  expect_true(nchar(out) > 0)
})

# ============================================================================
# interpret_anova / .interpret_anova
# ============================================================================

test_that("interpret_anova manager: significant result (p < 0.05)", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  df <- data.frame(
    y = c(rnorm(20, 10), rnorm(20, 15), rnorm(20, 20)),
    g = factor(rep(1:3, each = 20))
  )
  fit <- aov(y ~ g, data = df)
  out <- interpreter$interpret_anova(fit, audience = "manager")
  expect_true(grepl("significant difference", out))
  expect_true(grepl("Tukey HSD", out))
})

test_that("interpret_anova manager: not significant (p >= 0.05)", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  df <- data.frame(
    y = c(rnorm(20, 10), rnorm(20, 10), rnorm(20, 10)),
    g = factor(rep(1:3, each = 20))
  )
  fit <- aov(y ~ g, data = df)
  out <- interpreter$interpret_anova(fit, audience = "manager")
  expect_true(grepl("insufficient evidence", out))
})

test_that("interpret_anova technical: reject null hypothesis", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  df <- data.frame(
    y = c(rnorm(20, 10), rnorm(20, 15), rnorm(20, 20)),
    g = factor(rep(1:3, each = 20))
  )
  fit <- aov(y ~ g, data = df)
  out <- interpreter$interpret_anova(fit, audience = "technical")
  expect_true(grepl("reject the null hypothesis", out))
  expect_false(grepl("do not reject", out))
})

test_that("interpret_anova technical: do not reject null hypothesis", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  df <- data.frame(
    y = c(rnorm(20, 10), rnorm(20, 10), rnorm(20, 10)),
    g = factor(rep(1:3, each = 20))
  )
  fit <- aov(y ~ g, data = df)
  out <- interpreter$interpret_anova(fit, audience = "technical")
  expect_true(grepl("do not reject", out))
})

test_that("interpret_anova client: produces quality assurance report", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  df <- data.frame(
    y = c(rnorm(20, 10), rnorm(20, 15), rnorm(20, 20)),
    g = factor(rep(1:3, each = 20))
  )
  fit <- aov(y ~ g, data = df)
  out <- interpreter$interpret_anova(fit, audience = "client")
  expect_true(grepl("ANOVA Quality Assurance Report", out))
})

test_that("interpret_anova accepts anova data.frame directly", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  df <- data.frame(
    y = c(rnorm(20, 10), rnorm(20, 15), rnorm(20, 20)),
    g = factor(rep(1:3, each = 20))
  )
  fit <- lm(y ~ g, data = df)
  atab <- anova(fit)
  out <- interpreter$interpret_anova(atab, audience = "manager")
  expect_true(grepl("ANOVA", out))
})

test_that("interpret_anova unknown audience falls back to default header", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  df <- data.frame(
    y = c(rnorm(20, 10), rnorm(20, 10), rnorm(20, 10)),
    g = factor(rep(1:3, each = 20))
  )
  fit <- aov(y ~ g, data = df)
  out <- interpreter$interpret_anova(fit, audience = "weird")
  expect_true(grepl("ANOVA Result Interpretation", out))
})

# ============================================================================
# interpret_normality / .interpret_normality
# ============================================================================

test_that("interpret_normality manager: normal data (is_normal = TRUE)", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.45, is_normal = TRUE)
  out <- interpreter$interpret_normality(result, audience = "manager")
  expect_true(grepl("follows normal distribution", out))
  expect_true(grepl("Continue to maintain", out))
})

test_that("interpret_normality manager: non-normal data (is_normal = FALSE)", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.001, is_normal = FALSE)
  out <- interpreter$interpret_normality(result, audience = "manager")
  expect_true(grepl("does not follow normal distribution", out))
  expect_true(grepl("Box-Cox", out))
})

test_that("interpret_normality manager: p < 0.001 displays <0.001", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.0001, is_normal = FALSE)
  out <- interpreter$interpret_normality(result, audience = "manager")
  expect_true(grepl("<0.001", out))
})

test_that("interpret_normality manager: p >= 0.001 displays numeric value", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.03, is_normal = FALSE)
  out <- interpreter$interpret_normality(result, audience = "manager")
  expect_true(grepl("0.0300", out))
  expect_false(grepl("<0.001", out))
})

test_that("interpret_normality technical: normal, includes sample info", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.4, is_normal = TRUE,
                                  n = 50, skewness = 0.1, excess_kurtosis = 0.1)
  out <- interpreter$interpret_normality(result, audience = "technical")
  expect_true(grepl("do not reject", out))
  expect_true(grepl("Sample size: 50", out))
  expect_true(grepl("Approximately symmetric", out))
  expect_true(grepl("Kurtosis close to normal", out))
})

test_that("interpret_normality technical: right-skewed and leptokurtic", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.001, is_normal = FALSE,
                                  n = 80, skewness = 1.2, excess_kurtosis = 0.8)
  out <- interpreter$interpret_normality(result, audience = "technical")
  expect_true(grepl("Right-skewed", out))
  expect_true(grepl("Leptokurtic", out))
})

test_that("interpret_normality technical: left-skewed and platykurtic", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.001, is_normal = FALSE,
                                  n = 80, skewness = -1.2, excess_kurtosis = -0.8)
  out <- interpreter$interpret_normality(result, audience = "technical")
  expect_true(grepl("Left-skewed", out))
  expect_true(grepl("Platykurtic", out))
})

test_that("interpret_normality technical: with diagnose argument", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.001, is_normal = FALSE,
                                  n = 80, skewness = 0.3, excess_kurtosis = 0.3)
  diag <- list(
    skewness_direction = "Right-skewed (positive)",
    kurtosis_type = "Leptokurtic (heavy-tailed)"
  )
  out <- interpreter$interpret_normality(result, audience = "technical", diagnose = diag)
  expect_true(grepl("Diagnostic Details", out))
  expect_true(grepl("Right-skewed", out))
  expect_true(grepl("Leptokurtic", out))
})

test_that("interpret_normality technical: missing n/skewness/kurtosis are skipped", {
  interpreter <- StatInterpreter$new()
  result <- list(
    test_type = "Normality Test",
    method = "Shapiro-Wilk",
    statistic = c(W = 0.9),
    p.value = 0.001,
    is_normal = FALSE
    # no n, skewness, excess_kurtosis, alpha -> defaults used
  )
  out <- interpreter$interpret_normality(result, audience = "technical")
  expect_true(grepl("Statistical Interpretation", out))
  expect_false(grepl("Sample Information", out))
  expect_false(grepl("Distribution Shape", out))
})

test_that("interpret_normality client: normal data", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.4, is_normal = TRUE)
  out <- interpreter$interpret_normality(result, audience = "client")
  expect_true(grepl("Quality Assurance Statement", out))
  expect_true(grepl("meets the normality assumption", out))
})

test_that("interpret_normality client: non-normal data", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.001, is_normal = FALSE)
  out <- interpreter$interpret_normality(result, audience = "client")
  expect_true(grepl("does not meet the normality assumption", out))
  expect_true(grepl("non-parametric", out))
})

test_that("interpret_normality unknown audience falls back to default header", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.4, is_normal = TRUE)
  out <- interpreter$interpret_normality(result, audience = "weird")
  expect_true(grepl("Normality Test Result Interpretation", out))
})

test_that("interpret_normality dispatches via interpret() with diagnose in ...", {
  interpreter <- StatInterpreter$new()
  result <- make_normality_result(p.value = 0.001, is_normal = FALSE,
                                  n = 80, skewness = 0.3, excess_kurtosis = 0.3)
  diag <- list(skewness_direction = "Symmetric", kurtosis_type = "Mesokurtic")
  out <- interpreter$interpret(result, audience = "technical", diagnose = diag)
  expect_true(grepl("Diagnostic Details", out))
})

# ============================================================================
# End-to-end via real analyzers
# ============================================================================

test_that("end-to-end: interpret real normality test (normal data)", {
  interpreter <- StatInterpreter$new()
  set.seed(42)
  x <- rnorm(100)
  result <- NormalityAnalyzer$new()$test(x, method = "sw")
  out <- interpreter$interpret(result, audience = "manager")
  expect_type(out, "character")
  expect_true(nchar(out) > 50)
})

test_that("end-to-end: interpret real normality test (non-normal data)", {
  interpreter <- StatInterpreter$new()
  set.seed(42)
  x <- rexp(100)  # exponential is non-normal
  result <- NormalityAnalyzer$new()$test(x, method = "ad")
  out <- interpreter$interpret(result, audience = "technical")
  expect_type(out, "character")
  expect_true(nchar(out) > 50)
})
