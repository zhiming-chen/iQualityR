# =============================================================================
# File: tests/testthat/test_stat.R
# Description: iQualityR.stat complete test suite aligned with actual API
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ============================================================================
# 1. Descriptive statistics module (desc.R)
# ============================================================================

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

# ============================================================================
# 2. Probability distribution module (iqr_prob.R, Prob*.R, dist_registry.R)
# ============================================================================

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

test_that("prob_calc normal distribution (upper tail)", {
  result <- prob_calc(type = "norm", params = list(mean = 0, sd = 1),
                      x = 1.96, calc_type = "upper")
  expect_type(result, "list")
  expect_true(length(result) > 0)
  # result is a list of node results; access first node
  node_res <- result[[1]]
  expect_true("all_res" %in% names(node_res))
  expect_true("mode" %in% names(node_res))
  expect_equal(node_res$mode, "prob")
  p_val <- node_res$all_res[[1]]$result_p
  expect_gte(p_val, 0)
  expect_lte(p_val, 1)
  # P(X > 1.96) ~= 0.025
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

# ============================================================================
# 3. Sigma estimation module (sigma_estimate.R)
# ============================================================================

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

# ============================================================================
# 4. SPC constants module (constant.R)
# ============================================================================

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

# ============================================================================
# 5. Quality metrics module (quality_metrics.R)
# ============================================================================

test_that("capability_to_ppm calculation", {
  result <- capability_to_ppm(cpk = 1.33, usl = 10, lsl = 0)
  expect_type(result, "list")
  expect_true("within" %in% names(result))
  expect_true("overall" %in% names(result))
  expect_gt(result$within$total, 0)
  expect_lt(result$within$total, 1000)
})

test_that("sigma_to_ppm conversion", {
  ppm <- sigma_to_ppm(sigma = 3)
  expect_gt(ppm, 0)
  expect_lt(ppm, 100000)
})

test_that("ppm_to_sigma conversion", {
  sigma <- ppm_to_sigma(ppm = 2700)
  expect_gt(sigma, 2)
  expect_lt(sigma, 5)
})

test_that("yield_to_dpmo calculation", {
  dpmo <- yield_to_dpmo(yield = 0.99, opportunities = 10)
  expect_gt(dpmo, 0)
  expect_lt(dpmo, 10000)
})

test_that("z_bench calculation", {
  z <- z_bench(p_total = 0.003)
  expect_gt(z, 2)
  expect_lt(z, 5)
})

test_that("throughput_yield calculation", {
  ytp <- throughput_yield(yield = c(0.95, 0.98, 0.99))
  expect_gt(ytp, 0.9)
  expect_lt(ytp, 1)
})

test_that("throughput_yield via dpu", {
  ytp <- throughput_yield(dpu = c(0.01, 0.02, 0.015))
  expect_gt(ytp, 0.9)
  expect_lt(ytp, 1)
})

test_that("reliability calculation", {
  r <- reliability(lambda = 0.001, t = 100)
  expect_gt(r, 0)
  expect_lt(r, 1)
})

test_that("availability calculation", {
  a <- availability(mtbf = 1000, mttr = 10)
  expect_gt(a, 0.9)
  expect_lt(a, 1)
})

test_that("capability_interpret explanation", {
  interp <- capability_interpret(cpk = 1.33)
  expect_type(interp, "list")
  expect_true("level" %in% names(interp))
})

test_that("benchmark_compare comparison", {
  result <- benchmark_compare(metric = "cpk", value = 1.5, industry = "automotive")
  expect_type(result, "list")
  expect_true("rating" %in% names(result))
  expect_true("benchmark" %in% names(result))
  expect_true("percentile" %in% names(result))
})

test_that("quality_dashboard comprehensive panel", {
  dashboard <- quality_dashboard(
    cpk = 1.33, ppm = 500, yield_val = 0.995, availability = 0.98
  )
  expect_type(dashboard, "list")
  expect_true("cpk" %in% names(dashboard))
  expect_true("ppm" %in% names(dashboard))
})

# ============================================================================
# 6. Hypothesis testing module (htest/)
# ============================================================================

test_that("HTestAnalyzer initialization", {
  analyzer <- HTestAnalyzer$new()
  expect_true(inherits(analyzer, "HTestAnalyzer"))
  expect_true(inherits(analyzer, "R6"))
})

test_that("HTestAnalyzer one-sample t-test via direct method", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- analyzer$t_test_1s(x, mu = 5)

  expect_type(result, "list")
  expect_true("p.value" %in% names(result))
  expect_true("statistic" %in% names(result))
  expect_true("n" %in% names(result))
})

test_that("HTestAnalyzer two-sample t-test via direct method", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  y <- rnorm(30, mean = 5.5, sd = 1)
  result <- analyzer$t_test_2s(x, y)

  expect_type(result, "list")
  expect_true("p.value" %in% names(result))
})

test_that("HTestAnalyzer analyze dispatch by test_type string", {
  analyzer <- HTestAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- analyzer$analyze("t_test_1s", x = x, mu = 5)
  expect_type(result, "list")
  expect_true("p.value" %in% names(result))
})

test_that("HTestPlotter initialization", {
  plotter <- HTestPlotter$new()
  expect_true(inherits(plotter, "HTestPlotter"))
  expect_true(inherits(plotter, "R6"))
})

test_that("HTestReporter initialization", {
  reporter <- HTestReporter$new()
  expect_true(inherits(reporter, "HTestReporter"))
  expect_true(inherits(reporter, "R6"))
})

test_that("iqr_htest R6 class entry", {
  htest <- iqr_htest$new()
  expect_true(inherits(htest, "iqr_htest"))
  expect_true(inherits(htest, "R6"))
})

test_that("iqr_htest run t_test_1s", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  htest <- iqr_htest$new()
  htest$run("t_test_1s", x = x, mu = 5)
  expect_false(is.null(htest$last_results))
  expect_true("p.value" %in% names(htest$last_results))
})

test_that("htest_run convenience function", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  result <- htest_run("t_test_1s", x = x, mu = 5)

  expect_type(result, "list")
  expect_true("p.value" %in% names(result))
})

test_that("htest_interpret convenience function", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  interp <- htest_interpret("t_test_1s", x = x, mu = 5, audience = "manager")

  expect_type(interp, "character")
  expect_true(nchar(interp) > 0)
})

# ============================================================================
# 7. Normality test module (normality/)
# ============================================================================

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

# ============================================================================
# 8. Distribution fitting module (dist_fit.R)
# ============================================================================

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

# ============================================================================
# 9. Model diagnostics module (model_diag.R)
# ============================================================================

test_that("diagnose_lm linear model diagnostics", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5)
  model <- lm(y ~ x)
  result <- diagnose_lm(model)

  expect_type(result, "list")
  expect_true("residuals" %in% names(result))
  expect_true("normality" %in% names(result))
  expect_true("heteroscedasticity" %in% names(result))
  expect_true("multicollinearity" %in% names(result))
  expect_true("influence" %in% names(result))
})

test_that("test_residual_normality residual normality", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5)
  model <- lm(y ~ x)
  result <- test_residual_normality(model)

  expect_type(result, "list")
  expect_true("p.value" %in% names(result))
  expect_true("is_normal" %in% names(result))
})

test_that("test_heteroscedasticity heteroscedasticity test", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5)
  model <- lm(y ~ x)
  result <- test_heteroscedasticity(model)

  expect_type(result, "list")
  expect_true("p.value" %in% names(result))
  expect_true("is_heteroscedastic" %in% names(result))
})

test_that("diagnose_multicollinearity multicollinearity", {
  set.seed(123)
  x1 <- rnorm(100)
  x2 <- rnorm(100)
  y <- 2 * x1 + 3 * x2 + rnorm(100)
  model <- lm(y ~ x1 + x2)
  result <- diagnose_multicollinearity(model)

  expect_type(result, "list")
  expect_true("vif" %in% names(result))
})

test_that("diagnose_influential_points influential point diagnostics", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5)
  model <- lm(y ~ x)
  result <- diagnose_influential_points(model)

  expect_type(result, "list")
  expect_true("high_leverage" %in% names(result))
  expect_true("outliers" %in% names(result))
  expect_true("influential" %in% names(result))
})

test_that("summarize_assumptions assumptions summary", {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100, sd = 0.5)
  model <- lm(y ~ x)
  result <- summarize_assumptions(model)

  expect_s3_class(result, "data.frame")
  expect_true("assumption" %in% names(result))
  expect_true("passed" %in% names(result))
})

# ============================================================================
# 10. Data transformation module (transform.R)
# ============================================================================

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

# ============================================================================
# 11. Sample size and power module (sample_size.R)
# ============================================================================

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
  # Cohen's d is signed: (mean1 - mean2) / sd_pooled
  d <- effect_size(type = "cohens_d", mean1 = 11, mean2 = 10, sd_pooled = 1)
  expect_equal(d, 1)
})

# ============================================================================
# 12. Outlier detection module (outlier.R)
# ============================================================================

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

# ============================================================================
# 13. SPC out-of-control rules module (spc_rules.R)
# ============================================================================

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

# ============================================================================
# 14. StatInterpreter unified interpreter
# ============================================================================

test_that("StatInterpreter initialization", {
  interpreter <- StatInterpreter$new()
  expect_true(inherits(interpreter, "StatInterpreter"))
  expect_true(inherits(interpreter, "R6"))
})

test_that("StatInterpreter interprets distribution result", {
  interpreter <- StatInterpreter$new()
  dist_result <- list(
    type = "norm",
    params = list(mean = 0, sd = 1),
    calc_result = NULL
  )
  explanation <- interpreter$interpret(dist_result, audience = "manager")

  expect_type(explanation, "character")
  expect_true(nchar(explanation) > 0)
})

test_that("StatInterpreter interprets htest result", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 1)
  htest_result <- t.test(x, mu = 5)
  explanation <- interpreter$interpret(htest_result, audience = "manager")

  expect_type(explanation, "character")
  expect_true(nchar(explanation) > 0)
})

test_that("StatInterpreter interprets normality test result", {
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(50)
  normality_result <- normality_test(x, method = "sw")
  explanation <- interpreter$interpret(normality_result, audience = "manager")

  expect_type(explanation, "character")
  expect_true(nchar(explanation) > 0)
})
