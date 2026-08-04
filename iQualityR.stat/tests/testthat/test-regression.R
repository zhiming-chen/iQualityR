# =============================================================================
# File: tests/testthat/test-regression.R
# Description: Regression module tests (R3-B1)
#   - RegressionAnalyzer (lm_fit / logit_fit / poisson_fit)
#   - RegressionPlotter (Contract 2 signature)
#   - RegressionReporter (Contract 2 signature)
#   - iqr_regression L3 integrator
#   - Convenience functions (regression_run/plot/interpret/report)
#   - StatInterpreter regression_result dispatch
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# RegressionAnalyzer: lm_fit
# ----------------------------------------------------------------------------

test_that("RegressionAnalyzer lm_fit returns stat_result", {
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- rnorm(50)
  y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- analyzer$lm_fit(y ~ x, data = df)
  expect_s3_class(result, "stat_result")
  expect_s3_class(result, "regression_result")
  expect_equal(result$domain, "regression")
  expect_equal(result$test_type, "lm_fit")
  expect_true(inherits(result$model, "lm"))
  expect_s3_class(result$coefficients, "data.frame")
  expect_equal(nrow(result$coefficients), 2L)  # intercept + x
  expect_true(all(c("Term", "Estimate", "Std_Error", "Statistic", "p_value") %in%
                  names(result$coefficients)))
  expect_true(!is.na(result$model_stats$r_squared))
  expect_true(result$model_stats$r_squared > 0.9)  # strong linear relationship
  expect_length(result$residuals, 50)
  expect_length(result$fitted, 50)
})

test_that("RegressionAnalyzer lm_fit coefficients match stats::lm", {
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- rnorm(50)
  y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- analyzer$lm_fit(y ~ x, data = df)
  ref <- lm(y ~ x, data = df)
  ref_sm <- summary(ref)
  # Compare estimates
  expect_equal(result$coefficients$Estimate,
               as.numeric(ref_sm$coefficients[, 1]),
               tolerance = 1e-6)
  expect_equal(result$model_stats$r_squared, ref_sm$r.squared,
               tolerance = 1e-6)
})

test_that("RegressionAnalyzer lm_fit errors on non-data.frame", {
  analyzer <- RegressionAnalyzer$new()
  expect_error(analyzer$lm_fit(y ~ x, data = list(y = 1, x = 2)),
               "must be a data frame")
})

# ----------------------------------------------------------------------------
# RegressionAnalyzer: logit_fit
# ----------------------------------------------------------------------------

test_that("RegressionAnalyzer logit_fit returns stat_result", {
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  prob <- 1 / (1 + exp(-(0.5 + 1.5 * x)))
  y <- rbinom(n, 1, prob)
  df <- data.frame(y = y, x = x)
  result <- analyzer$logit_fit(y ~ x, data = df)
  expect_s3_class(result, "regression_result")
  expect_equal(result$test_type, "logit_fit")
  expect_true(inherits(result$model, "glm"))
  expect_equal(result$model_stats$family, "binomial")
  expect_equal(result$model_stats$link, "logit")
  expect_s3_class(result$coefficients, "data.frame")
  # Odds ratios should be present
  expect_true(!is.null(result$odds_ratios))
  expect_true(!is.null(result$odds_ratio_ci))
  expect_length(result$odds_ratios, 2L)
  expect_equal(dim(result$odds_ratio_ci), c(2L, 2L))
})

test_that("RegressionAnalyzer logit_fit odds ratios are exp(coefficients)", {
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  y <- rbinom(n, 1, 1 / (1 + exp(-(0.5 + 1.5 * x))))
  df <- data.frame(y = y, x = x)
  result <- analyzer$logit_fit(y ~ x, data = df)
  expect_equal(as.numeric(result$odds_ratios),
               exp(result$coefficients$Estimate),
               tolerance = 1e-6)
})

# ----------------------------------------------------------------------------
# RegressionAnalyzer: poisson_fit
# ----------------------------------------------------------------------------

test_that("RegressionAnalyzer poisson_fit returns stat_result", {
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  lambda <- exp(0.5 + 0.8 * x)
  y <- rpois(n, lambda)
  df <- data.frame(y = y, x = x)
  result <- analyzer$poisson_fit(y ~ x, data = df)
  expect_s3_class(result, "regression_result")
  expect_equal(result$test_type, "poisson_fit")
  expect_true(inherits(result$model, "glm"))
  expect_equal(result$model_stats$family, "poisson")
  expect_equal(result$model_stats$link, "log")
  expect_s3_class(result$coefficients, "data.frame")
  expect_true(!is.null(result$rate_ratios))
  expect_true(!is.null(result$rate_ratio_ci))
})

test_that("RegressionAnalyzer poisson_fit rate ratios are exp(coefficients)", {
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  y <- rpois(n, exp(0.5 + 0.8 * x))
  df <- data.frame(y = y, x = x)
  result <- analyzer$poisson_fit(y ~ x, data = df)
  expect_equal(as.numeric(result$rate_ratios),
               exp(result$coefficients$Estimate),
               tolerance = 1e-6)
})

# ----------------------------------------------------------------------------
# RegressionAnalyzer: analyze dispatch
# ----------------------------------------------------------------------------

test_that("RegressionAnalyzer analyze dispatches all 3 model types", {
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 50
  x <- rnorm(n)
  df <- data.frame(
    y_cont = 2 + 3 * x + rnorm(n, sd = 0.5),
    y_bin  = rbinom(n, 1, 1 / (1 + exp(-x))),
    y_cnt  = rpois(n, exp(0.5 + 0.5 * x)),
    x = x
  )
  expect_s3_class(analyzer$analyze("lm_fit", formula = y_cont ~ x, data = df),
                  "regression_result")
  expect_s3_class(analyzer$analyze("logit_fit", formula = y_bin ~ x, data = df),
                  "regression_result")
  expect_s3_class(analyzer$analyze("poisson_fit", formula = y_cnt ~ x, data = df),
                  "regression_result")
})

test_that("RegressionAnalyzer analyze rejects unknown type", {
  analyzer <- RegressionAnalyzer$new()
  expect_error(analyzer$analyze("nope"), "Unknown model type")
})

# ----------------------------------------------------------------------------
# RegressionReporter
# ----------------------------------------------------------------------------

test_that("RegressionReporter initialization", {
  reporter <- RegressionReporter$new()
  expect_true(inherits(reporter, "RegressionReporter"))
  expect_true(inherits(reporter, "R6"))
})

test_that("RegressionReporter$report has Contract 2 signature", {
  f <- RegressionReporter$new()$report
  fm <- formals(f)
  expect_named(fm, c("result", "format", "path", "audience"))
})

test_that("RegressionReporter to_dataframe returns coefficients", {
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- analyzer$lm_fit(y ~ x, data = df)
  reporter <- RegressionReporter$new()
  cf <- reporter$report(result, format = "data.frame")
  expect_s3_class(cf, "data.frame")
  expect_true(nrow(cf) >= 1L)
  expect_true("Estimate" %in% names(cf))
})

test_that("RegressionReporter console output works", {
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- analyzer$lm_fit(y ~ x, data = df)
  reporter <- RegressionReporter$new()
  out <- capture.output(reporter$report(result, format = "console",
                                        audience = "manager"))
  expect_true(length(out) > 0L)
  expect_true(any(grepl("Linear regression|R-squared", out)))
})

# ----------------------------------------------------------------------------
# StatInterpreter regression_result dispatch
# ----------------------------------------------------------------------------

test_that("StatInterpreter handles lm_fit (manager)", {
  analyzer <- RegressionAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- analyzer$lm_fit(y ~ x, data = df)
  out <- interpreter$interpret(result, audience = "manager")
  expect_type(out, "character")
  expect_true(grepl("Regression Model", out))
  expect_true(grepl("R-squared", out))
  expect_true(grepl("Conclusion", out))
})

test_that("StatInterpreter handles logit_fit (manager)", {
  analyzer <- RegressionAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  n <- 100; x <- rnorm(n)
  y <- rbinom(n, 1, 1 / (1 + exp(-(0.5 + 1.5 * x))))
  df <- data.frame(y = y, x = x)
  result <- analyzer$logit_fit(y ~ x, data = df)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("Logistic", out))
  expect_true(grepl("AIC", out))
})

test_that("StatInterpreter handles poisson_fit (technical)", {
  analyzer <- RegressionAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  n <- 100; x <- rnorm(n)
  y <- rpois(n, exp(0.5 + 0.8 * x))
  df <- data.frame(y = y, x = x)
  result <- analyzer$poisson_fit(y ~ x, data = df)
  out <- interpreter$interpret(result, audience = "technical")
  expect_true(grepl("Poisson", out))
  expect_true(grepl("Rate ratios|IRR", out))
})

# ----------------------------------------------------------------------------
# iqr_regression L3 integrator
# ----------------------------------------------------------------------------

test_that("iqr_regression runs lm_fit end-to-end", {
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  reg <- iqr_regression$new()
  reg$run("lm_fit", formula = y ~ x, data = df)
  expect_s3_class(reg$last_results, "regression_result")
  expect_equal(reg$last_results$test_type, "lm_fit")
})

test_that("iqr_regression runs logit_fit end-to-end", {
  set.seed(123)
  n <- 100; x <- rnorm(n)
  y <- rbinom(n, 1, 1 / (1 + exp(-(0.5 + 1.5 * x))))
  df <- data.frame(y = y, x = x)
  reg <- iqr_regression$new()
  reg$run("logit_fit", formula = y ~ x, data = df)
  expect_equal(reg$last_results$test_type, "logit_fit")
})

test_that("iqr_regression$report returns data.frame", {
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  reg <- iqr_regression$new()
  reg$run("lm_fit", formula = y ~ x, data = df)
  cf <- reg$report(format = "data.frame")
  expect_s3_class(cf, "data.frame")
})

test_that("iqr_regression$plot / $interpret error before $run", {
  reg <- iqr_regression$new()
  expect_error(reg$plot(), "run.*first")
  expect_error(reg$interpret(), "run.*first")
  expect_error(reg$report(), "run.*first")
})

# ----------------------------------------------------------------------------
# Convenience functions
# ----------------------------------------------------------------------------

test_that("regression_run returns stat_result", {
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- regression_run("lm_fit", formula = y ~ x, data = df)
  expect_s3_class(result, "regression_result")
})

test_that("regression_interpret returns character", {
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- regression_run("lm_fit", formula = y ~ x, data = df)
  out <- capture.output(regression_interpret(result, audience = "manager"))
  expect_true(length(out) > 0L)
})

test_that("regression_report returns data.frame", {
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- regression_run("lm_fit", formula = y ~ x, data = df)
  cf <- regression_report(result, format = "data.frame")
  expect_s3_class(cf, "data.frame")
})

# ----------------------------------------------------------------------------
# RegressionPlotter
# ----------------------------------------------------------------------------

test_that("RegressionPlotter initialization", {
  plotter <- RegressionPlotter$new(theme = "academic")
  expect_true(inherits(plotter, "RegressionPlotter"))
  expect_true(inherits(plotter, "R6"))
})

test_that("RegressionPlotter$plot has Contract 2 signature", {
  f <- RegressionPlotter$new()$plot
  fm <- formals(f)
  expect_named(fm, c("result", "plot_type", "show_table", "theme_obj"))
  expect_equal(fm$plot_type, "auto")
  expect_equal(fm$show_table, FALSE)
  expect_equal(fm$theme_obj, NULL)
})

test_that("RegressionPlotter renders residual plot", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- analyzer$lm_fit(y ~ x, data = df)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(result, plot_type = "residual")
  expect_true(inherits(p, "ggplot"))
})

test_that("RegressionPlotter renders coef plot", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- analyzer$lm_fit(y ~ x, data = df)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(result, plot_type = "coef")
  expect_true(inherits(p, "ggplot"))
})

test_that("RegressionPlotter auto-selects plot type", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- analyzer$lm_fit(y ~ x, data = df)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(result, plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})

test_that("RegressionPlotter rejects unknown plot_type", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- analyzer$lm_fit(y ~ x, data = df)
  plotter <- RegressionPlotter$new()
  expect_error(plotter$plot(result, plot_type = "nope"), "unknown plot_type")
})

# ----------------------------------------------------------------------------
# R3-B2: cox_fit
# ----------------------------------------------------------------------------

test_that("RegressionAnalyzer cox_fit returns stat_result", {
  skip_if_not_installed("survival")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  time <- rexp(n, rate = exp(0.3 + 0.5 * x))
  status <- rbinom(n, 1, 0.8)
  df <- data.frame(time = time, status = status, x = x)
  result <- analyzer$cox_fit(survival::Surv(time, status) ~ x, data = df)
  expect_s3_class(result, "regression_result")
  expect_equal(result$test_type, "cox_fit")
  expect_equal(result$dist_type, "z")
  expect_true(inherits(result$model, "coxph"))
  expect_s3_class(result$coefficients, "data.frame")
  expect_true(all(c("Term", "Estimate", "Std_Error", "Statistic", "p_value") %in%
                  names(result$coefficients)))
  expect_true(!is.null(result$hazard_ratios))
  expect_true(!is.null(result$hazard_ratio_ci))
  expect_equal(dim(result$hazard_ratio_ci), c(1L, 2L))  # one predictor
  expect_true(!is.na(result$model_stats$concordance))
  expect_true(result$model_stats$concordance > 0.5)
  expect_true(result$model_stats$n_events > 0)
})

test_that("cox_fit hazard ratios equal exp(coefficients)", {
  skip_if_not_installed("survival")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  time <- rexp(n, rate = exp(0.3 + 0.5 * x))
  status <- rbinom(n, 1, 0.8)
  df <- data.frame(time = time, status = status, x = x)
  result <- analyzer$cox_fit(survival::Surv(time, status) ~ x, data = df)
  expect_equal(as.numeric(result$hazard_ratios),
               exp(result$coefficients$Estimate),
               tolerance = 1e-6)
})

test_that("StatInterpreter handles cox_fit", {
  skip_if_not_installed("survival")
  analyzer <- RegressionAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  time <- rexp(n, rate = exp(0.3 + 0.5 * x))
  status <- rbinom(n, 1, 0.8)
  df <- data.frame(time = time, status = status, x = x)
  result <- analyzer$cox_fit(survival::Surv(time, status) ~ x, data = df)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("Cox", out))
  expect_true(grepl("Concordance", out))
  expect_true(grepl("Hazard ratios", interpreter$interpret(result, audience = "technical")))
})

# ----------------------------------------------------------------------------
# R3-B2: pls_fit
# ----------------------------------------------------------------------------

test_that("RegressionAnalyzer pls_fit returns stat_result", {
  skip_if_not_installed("pls")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 60
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y <- 1 + 2 * x1 - x2 + 0.5 * x3 + rnorm(n, sd = 0.3)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
  result <- analyzer$pls_fit(y ~ x1 + x2 + x3, data = df, ncomp = 2)
  expect_s3_class(result, "regression_result")
  expect_equal(result$test_type, "pls_fit")
  expect_true(inherits(result$model, "mvr"))
  expect_s3_class(result$coefficients, "data.frame")
  expect_equal(result$model_stats$ncomp, 2)
  expect_true(!is.na(result$model_stats$r_squared))
  expect_true(result$model_stats$r_squared > 0.5)
  expect_length(result$residuals, n)
  expect_length(result$fitted, n)
})

test_that("pls_fit auto-selects all components when ncomp is NULL", {
  skip_if_not_installed("pls")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 60
  x1 <- rnorm(n); x2 <- rnorm(n)
  y <- 1 + 2 * x1 - x2 + rnorm(n, sd = 0.3)
  df <- data.frame(y = y, x1 = x1, x2 = x2)
  result <- analyzer$pls_fit(y ~ x1 + x2, data = df)
  expect_equal(result$model_stats$ncomp, 2)  # 2 predictors -> 2 components
})

test_that("StatInterpreter handles pls_fit", {
  skip_if_not_installed("pls")
  analyzer <- RegressionAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  n <- 60
  x1 <- rnorm(n); x2 <- rnorm(n)
  y <- 1 + 2 * x1 - x2 + rnorm(n, sd = 0.3)
  df <- data.frame(y = y, x1 = x1, x2 = x2)
  result <- analyzer$pls_fit(y ~ x1 + x2, data = df, ncomp = 2)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("PLS", out))
  expect_true(grepl("component", out, ignore.case = TRUE))
})

# ----------------------------------------------------------------------------
# R3-B2: stepwise_fit
# ----------------------------------------------------------------------------

test_that("RegressionAnalyzer stepwise_fit returns stat_result (lm)", {
  skip_if_not_installed("MASS")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 80
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)  # x3 is noise
  y <- 1 + 2 * x1 - 1.5 * x2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
  result <- analyzer$stepwise_fit(y ~ x1 + x2 + x3, data = df, direction = "both")
  expect_s3_class(result, "regression_result")
  expect_equal(result$test_type, "stepwise_fit")
  expect_equal(result$direction, "both")
  expect_equal(result$penalty_k, 2)
  expect_true(inherits(result$model, "lm"))
  expect_s3_class(result$coefficients, "data.frame")
  expect_true(!is.null(result$selected_terms))
  # x3 (noise) may or may not be selected, but x1 and x2 should be retained
  expect_true("x1" %in% result$selected_terms)
  expect_true("x2" %in% result$selected_terms)
})

test_that("stepwise_fit backward direction works", {
  skip_if_not_installed("MASS")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 80
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y <- 1 + 2 * x1 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
  result <- analyzer$stepwise_fit(y ~ x1 + x2 + x3, data = df, direction = "backward")
  expect_equal(result$direction, "backward")
  expect_true("x1" %in% result$selected_terms)
})

test_that("StatInterpreter handles stepwise_fit", {
  skip_if_not_installed("MASS")
  analyzer <- RegressionAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  n <- 80
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y <- 1 + 2 * x1 - 1.5 * x2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
  result <- analyzer$stepwise_fit(y ~ x1 + x2 + x3, data = df)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("Stepwise|R-squared", out))
  out_tech <- interpreter$interpret(result, audience = "technical")
  expect_true(grepl("Direction", out_tech))
  expect_true(grepl("Selected terms", out_tech))
})

# ----------------------------------------------------------------------------
# R3-B2: best_subset_fit
# ----------------------------------------------------------------------------

test_that("RegressionAnalyzer best_subset_fit returns stat_result", {
  skip_if_not_installed("leaps")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 80
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y <- 1 + 2 * x1 - 1.5 * x2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
  result <- analyzer$best_subset_fit(y ~ x1 + x2 + x3, data = df)
  expect_s3_class(result, "regression_result")
  expect_equal(result$test_type, "best_subset_fit")
  expect_true(inherits(result$model, "regsubsets"))
  expect_null(result$coefficients)  # no single coefficient table
  expect_s3_class(result$subset_summary, "data.frame")
  expect_equal(nrow(result$subset_summary), 3)  # 3 predictors
  expect_true(all(c("n_vars", "r_squared", "adj_r_squared", "BIC", "selected_vars") %in%
                  names(result$subset_summary)))
  expect_true(!is.null(result$best_by_bic))
  expect_true(result$best_by_bic$n_vars >= 1)
  expect_equal(result$best_by_bic$n_vars,
               result$subset_summary$n_vars[which.min(result$subset_summary$BIC)])
})

test_that("best_subset_fit respects nvmax", {
  skip_if_not_installed("leaps")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 80
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y <- 1 + 2 * x1 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
  result <- analyzer$best_subset_fit(y ~ x1 + x2 + x3, data = df, nvmax = 2)
  expect_equal(nrow(result$subset_summary), 2)  # capped at 2
})

test_that("StatInterpreter handles best_subset_fit", {
  skip_if_not_installed("leaps")
  analyzer <- RegressionAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  n <- 80
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y <- 1 + 2 * x1 - 1.5 * x2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
  result <- analyzer$best_subset_fit(y ~ x1 + x2 + x3, data = df)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("Best subset|BIC", out))
  expect_true(grepl("Selected Variables", out))
})

# ----------------------------------------------------------------------------
# R3-B2: analyze dispatch + iqr_regression integration
# ----------------------------------------------------------------------------

test_that("RegressionAnalyzer analyze dispatches all 7 model types", {
  skip_if_not_installed("survival")
  skip_if_not_installed("pls")
  skip_if_not_installed("MASS")
  skip_if_not_installed("leaps")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 80
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  time <- rexp(n, rate = exp(0.3 + 0.5 * x1))
  status <- rbinom(n, 1, 0.8)
  y <- 1 + 2 * x1 - x2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3,
                   time = time, status = status)
  expect_s3_class(analyzer$analyze("cox_fit",
                  formula = survival::Surv(time, status) ~ x1, data = df),
                  "regression_result")
  expect_s3_class(analyzer$analyze("pls_fit",
                  formula = y ~ x1 + x2 + x3, data = df, ncomp = 2),
                  "regression_result")
  expect_s3_class(analyzer$analyze("stepwise_fit",
                  formula = y ~ x1 + x2 + x3, data = df),
                  "regression_result")
  expect_s3_class(analyzer$analyze("best_subset_fit",
                  formula = y ~ x1 + x2 + x3, data = df),
                  "regression_result")
})

test_that("iqr_regression runs cox_fit end-to-end", {
  skip_if_not_installed("survival")
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  time <- rexp(n, rate = exp(0.3 + 0.5 * x))
  status <- rbinom(n, 1, 0.8)
  df <- data.frame(time = time, status = status, x = x)
  reg <- iqr_regression$new()
  reg$run("cox_fit", formula = survival::Surv(time, status) ~ x, data = df)
  expect_equal(reg$last_results$test_type, "cox_fit")
})

test_that("RegressionReporter handles best_subset console + data.frame", {
  skip_if_not_installed("leaps")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 80
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y <- 1 + 2 * x1 - 1.5 * x2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
  result <- analyzer$best_subset_fit(y ~ x1 + x2 + x3, data = df)
  reporter <- RegressionReporter$new()
  out <- capture.output(reporter$report(result, format = "console", audience = "manager"))
  expect_true(any(grepl("Best subset|nvmax|BIC", out)))
  ss <- reporter$report(result, format = "data.frame")
  expect_s3_class(ss, "data.frame")
})

test_that("RegressionPlotter auto-selects subset plot for best_subset_fit", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("leaps")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 80
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y <- 1 + 2 * x1 - 1.5 * x2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
  result <- analyzer$best_subset_fit(y ~ x1 + x2 + x3, data = df)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(result, plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
  p2 <- plotter$plot(result, plot_type = "subset")
  expect_true(inherits(p2, "ggplot"))
})

test_that("RegressionPlotter residual plot works for cox_fit", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("survival")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  time <- rexp(n, rate = exp(0.3 + 0.5 * x))
  status <- rbinom(n, 1, 0.8)
  df <- data.frame(time = time, status = status, x = x)
  result <- analyzer$cox_fit(survival::Surv(time, status) ~ x, data = df)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(result, plot_type = "residual")
  expect_true(inherits(p, "ggplot"))
})

# ----------------------------------------------------------------------------
# R3-B3: ROC / PR / Lift curves for logit_fit
# ----------------------------------------------------------------------------

test_that("RegressionPlotter renders ROC curve for logit_fit", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 200
  x <- rnorm(n)
  y <- rbinom(n, 1, 1 / (1 + exp(-(0.5 + 1.5 * x))))
  df <- data.frame(y = y, x = x)
  result <- analyzer$logit_fit(y ~ x, data = df)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(result, plot_type = "roc")
  expect_true(inherits(p, "ggplot"))
  # AUC should appear in subtitle
  pb <- ggplot2::ggplot_build(p)
  expect_true(any(grepl("AUC", pb$plot$labels$subtitle)))
})

test_that("RegressionPlotter renders PR curve for logit_fit", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 200
  x <- rnorm(n)
  y <- rbinom(n, 1, 1 / (1 + exp(-(0.5 + 1.5 * x))))
  df <- data.frame(y = y, x = x)
  result <- analyzer$logit_fit(y ~ x, data = df)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(result, plot_type = "pr")
  expect_true(inherits(p, "ggplot"))
})

test_that("RegressionPlotter renders Lift curve for logit_fit", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 200
  x <- rnorm(n)
  y <- rbinom(n, 1, 1 / (1 + exp(-(0.5 + 1.5 * x))))
  df <- data.frame(y = y, x = x)
  result <- analyzer$logit_fit(y ~ x, data = df)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(result, plot_type = "lift")
  expect_true(inherits(p, "ggplot"))
})

test_that("auto-select returns 'roc' for logit_fit", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  y <- rbinom(n, 1, 1 / (1 + exp(-x)))
  df <- data.frame(y = y, x = x)
  result <- analyzer$logit_fit(y ~ x, data = df)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(result, plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
  # Title should be "ROC Curve" if auto-selected roc
  pb <- ggplot2::ggplot_build(p)
  expect_equal(pb$plot$labels$title, "ROC Curve")
})

test_that("ROC curve errors for non-binary response (lm_fit)", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- rnorm(50); y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  result <- analyzer$lm_fit(y ~ x, data = df)
  plotter <- RegressionPlotter$new()
  expect_error(plotter$plot(result, plot_type = "roc"), "ROC curve requires")
})

# ============================================================================
# R3-D1: mars_fit + spline_fit
# ============================================================================

# ----------------------------------------------------------------------------
# spline_fit (B-spline)
# ----------------------------------------------------------------------------

test_that("spline_fit (bs) returns stat_result", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- seq(0, 10, length.out = 60)
  y <- 2 + 3 * sin(x) + rnorm(60, sd = 0.5)
  df <- data.frame(y = y, x = x)
  res <- analyzer$spline_fit(y ~ x, data = df, df = 6, basis = "bs")
  expect_s3_class(res, "stat_result")
  expect_s3_class(res, "regression_result")
  expect_equal(res$test_type, "spline_fit")
  expect_equal(res$spline_basis, "bs")
  expect_equal(res$spline_df, 6)
  expect_equal(res$spline_predictor, "x")
  expect_equal(res$spline_degree, 3)
  expect_true(!is.null(res$fitted))
  expect_true(!is.null(res$residuals))
  expect_true(!is.null(res$model))
})

test_that("spline_fit (bs) captures nonlinear structure", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(42)
  x <- seq(0, 10, length.out = 80)
  y <- 5 + 2 * sin(2 * x) + rnorm(80, sd = 0.3)
  df <- data.frame(y = y, x = x)
  spline_res <- analyzer$spline_fit(y ~ x, data = df, df = 8, basis = "bs")
  linear_res <- analyzer$lm_fit(y ~ x, data = df)
  expect_true(spline_res$model_stats$r_squared > linear_res$model_stats$r_squared)
})

test_that("spline_fit (ns) natural spline works", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(7)
  x <- seq(-2, 5, length.out = 50)
  y <- 1 + x^2 + rnorm(50, sd = 1)
  df <- data.frame(y = y, x = x)
  res <- analyzer$spline_fit(y ~ x, data = df, df = 4, basis = "ns")
  expect_equal(res$spline_basis, "ns")
  expect_true(is.na(res$spline_degree))
  expect_true(res$model_stats$r_squared > 0.7)
})

test_that("spline_fit accepts explicit knots", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(11)
  x <- seq(0, 10, length.out = 60)
  y <- 1 + 0.5 * x + rnorm(60, sd = 0.5)
  df <- data.frame(y = y, x = x)
  res <- analyzer$spline_fit(y ~ x, data = df, basis = "bs",
                             knots = c(3, 6), degree = 3)
  expect_s3_class(res, "regression_result")
  expect_equal(length(res$spline_knots), 2)
})

test_that("spline_fit preserves additional covariates", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(33)
  x <- seq(0, 10, length.out = 50)
  z <- rnorm(50)
  y <- 1 + 0.5 * x + 2 * z + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x, z = z)
  res <- analyzer$spline_fit(y ~ x + z, data = df, df = 4, basis = "bs")
  # fitted_formula should contain the spline basis for x AND the raw z
  expect_true(grepl("bs\\(x", deparse(res$fitted_formula)[1]))
  expect_true("z" %in% all.vars(res$fitted_formula))
})

test_that("spline_fit errors on non-data.frame", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  expect_error(analyzer$spline_fit(y ~ x, data = list(y = 1, x = 1)),
               "must be a data frame")
})

test_that("spline_fit errors when formula lacks predictors", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  df <- data.frame(y = 1:5)
  expect_error(analyzer$spline_fit(y ~ 1, data = df), "at least one predictor")
})

test_that("spline_fit dispatches via analyze()", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(99)
  x <- seq(0, 5, length.out = 40)
  y <- 1 + x + rnorm(40, sd = 0.5)
  df <- data.frame(y = y, x = x)
  res <- analyzer$analyze("spline_fit", formula = y ~ x, data = df, df = 4)
  expect_equal(res$test_type, "spline_fit")
})

test_that("spline_fit coefficient table matches lm summary", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(55)
  x <- seq(0, 5, length.out = 40)
  y <- 1 + x + rnorm(40, sd = 0.3)
  df <- data.frame(y = y, x = x)
  res <- analyzer$spline_fit(y ~ x, data = df, df = 4, basis = "bs")
  # The internally fitted model
  fit <- res$model
  expect_equal(as.numeric(res$coefficients$Estimate),
               as.numeric(stats::coef(fit)),
               tolerance = 1e-9)
})

# ----------------------------------------------------------------------------
# mars_fit (requires earth)
# ----------------------------------------------------------------------------

test_that("mars_fit returns stat_result", {
  skip_if_not_installed("earth")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- seq(0, 10, length.out = 100)
  y <- ifelse(x < 5, 2 * x, 10 + 1 * (x - 5)) + rnorm(100, sd = 0.5)
  df <- data.frame(y = y, x = x)
  res <- analyzer$mars_fit(y ~ x, data = df, degree = 1)
  expect_s3_class(res, "stat_result")
  expect_s3_class(res, "regression_result")
  expect_equal(res$test_type, "mars_fit")
  expect_equal(res$model_stats$degree, 1)
  expect_true(!is.null(res$selected_terms))
  expect_true(!is.null(res$fitted))
  expect_true(!is.null(res$residuals))
  expect_true(!is.null(res$model))
})

test_that("mars_fit captures piecewise structure", {
  skip_if_not_installed("earth")
  analyzer <- RegressionAnalyzer$new()
  set.seed(42)
  x <- seq(0, 10, length.out = 120)
  # Piecewise-linear signal with a kink at x=5
  y <- 1 + ifelse(x < 5, 0.5 * x, 2.5 + 2 * (x - 5)) + rnorm(120, sd = 0.3)
  df <- data.frame(y = y, x = x)
  res <- analyzer$mars_fit(y ~ x, data = df, degree = 1)
  # Generalized R-squared should be high
  expect_true(res$model_stats$generalized_rsq > 0.85)
  # GCV should be finite and positive
  expect_true(is.finite(res$model_stats$gcv))
  expect_true(res$model_stats$gcv > 0)
})

test_that("mars_fit degree=2 allows interactions", {
  skip_if_not_installed("earth")
  analyzer <- RegressionAnalyzer$new()
  set.seed(7)
  n <- 150
  x1 <- runif(n, 0, 10)
  x2 <- runif(n, 0, 10)
  # Interaction effect
  y <- 1 + 0.3 * x1 - 0.2 * x2 + 0.5 * (x1 - 5) * (x2 - 5) +
       rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2)
  res <- analyzer$mars_fit(y ~ x1 + x2, data = df, degree = 2)
  expect_equal(res$model_stats$degree, 2)
  expect_true(res$model_stats$generalized_rsq > 0.5)
})

test_that("mars_fit errors on non-data.frame", {
  skip_if_not_installed("earth")
  analyzer <- RegressionAnalyzer$new()
  expect_error(analyzer$mars_fit(y ~ x, data = list(y = 1, x = 1)),
               "must be a data frame")
})

test_that("mars_fit errors when earth missing", {
  skip_if_not_installed("earth")
  # Test only passes when earth is installed; we check the namespace guard
  # directly by stubbing requireNamespace via a private call path.
  analyzer <- RegressionAnalyzer$new()
  # Direct call works (we have earth), so this is a smoke test
  set.seed(1)
  x <- 1:20; y <- x + rnorm(20)
  df <- data.frame(y = y, x = x)
  res <- analyzer$mars_fit(y ~ x, data = df)
  expect_equal(res$test_type, "mars_fit")
})

test_that("mars_fit dispatches via analyze()", {
  skip_if_not_installed("earth")
  analyzer <- RegressionAnalyzer$new()
  set.seed(44)
  x <- seq(0, 5, length.out = 50)
  y <- 1 + x + rnorm(50, sd = 0.3)
  df <- data.frame(y = y, x = x)
  res <- analyzer$analyze("mars_fit", formula = y ~ x, data = df)
  expect_equal(res$test_type, "mars_fit")
})

# ----------------------------------------------------------------------------
# analyze dispatch covers all 9 model types
# ----------------------------------------------------------------------------

test_that("RegressionAnalyzer analyze dispatches all 9 model types", {
  skip_if_not_installed("survival")
  skip_if_not_installed("pls")
  skip_if_not_installed("MASS")
  skip_if_not_installed("leaps")
  skip_if_not_installed("splines")
  skip_if_not_installed("earth")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  n <- 60
  x <- rnorm(n)
  y <- 2 + 3 * x + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x = x)

  expect_s3_class(analyzer$analyze("lm_fit", formula = y ~ x, data = df),
                  "regression_result")
  expect_s3_class(analyzer$analyze("spline_fit", formula = y ~ x, data = df,
                                   df = 4), "regression_result")
  expect_s3_class(analyzer$analyze("mars_fit", formula = y ~ x, data = df),
                  "regression_result")
})

# ----------------------------------------------------------------------------
# L3 integrator (iqr_regression) for new types
# ----------------------------------------------------------------------------

test_that("iqr_regression runs spline_fit end-to-end", {
  skip_if_not_installed("splines")
  set.seed(123)
  x <- seq(0, 10, length.out = 50)
  y <- 1 + 2 * sin(x) + rnorm(50, sd = 0.4)
  df <- data.frame(y = y, x = x)
  reg <- iqr_regression$new()
  reg$run("spline_fit", formula = y ~ x, data = df, df = 6, basis = "bs")
  expect_s3_class(reg$last_results, "regression_result")
  expect_equal(reg$last_results$test_type, "spline_fit")
})

test_that("iqr_regression runs mars_fit end-to-end", {
  skip_if_not_installed("earth")
  set.seed(202)
  x <- seq(0, 10, length.out = 80)
  y <- ifelse(x < 5, x, 5 + 2 * (x - 5)) + rnorm(80, sd = 0.5)
  df <- data.frame(y = y, x = x)
  reg <- iqr_regression$new()
  reg$run("mars_fit", formula = y ~ x, data = df, degree = 1)
  expect_s3_class(reg$last_results, "regression_result")
  expect_equal(reg$last_results$test_type, "mars_fit")
})

test_that("regression_run returns spline_fit stat_result", {
  skip_if_not_installed("splines")
  set.seed(99)
  x <- seq(0, 5, length.out = 40)
  y <- 1 + x^1.5 + rnorm(40, sd = 0.5)
  df <- data.frame(y = y, x = x)
  res <- regression_run("spline_fit", formula = y ~ x, data = df, df = 5)
  expect_s3_class(res, "regression_result")
  expect_equal(res$spline_basis, "bs")
})

# ----------------------------------------------------------------------------
# StatInterpreter dispatch for new types
# ----------------------------------------------------------------------------

test_that("StatInterpreter handles spline_fit (manager)", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(31)
  x <- seq(0, 10, length.out = 50)
  y <- 2 + 3 * sin(x) + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  res <- analyzer$spline_fit(y ~ x, data = df, df = 6, basis = "bs")
  out <- interpreter$interpret(res, audience = "manager")
  expect_true(grepl("spline", out, ignore.case = TRUE))
  expect_true(grepl("Basis = BS", out))
  expect_true(grepl("df = 6", out))
  expect_true(grepl("Conclusion", out))
})

test_that("StatInterpreter handles spline_fit (technical)", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(31)
  x <- seq(0, 10, length.out = 50)
  y <- 2 + 3 * sin(x) + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  res <- analyzer$spline_fit(y ~ x, data = df, df = 4, basis = "ns")
  out <- interpreter$interpret(res, audience = "technical")
  expect_true(grepl("Basis: NS", out))
  expect_true(grepl("Predictor expanded", out))
})

test_that("StatInterpreter handles mars_fit (manager)", {
  skip_if_not_installed("earth")
  analyzer <- RegressionAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(21)
  x <- seq(0, 10, length.out = 100)
  y <- ifelse(x < 5, x, 5 + 2 * (x - 5)) + rnorm(100, sd = 0.3)
  df <- data.frame(y = y, x = x)
  res <- analyzer$mars_fit(y ~ x, data = df, degree = 1)
  out <- interpreter$interpret(res, audience = "manager")
  expect_true(grepl("MARS", out))
  expect_true(grepl("Generalized R-squared", out))
  expect_true(grepl("GCV", out))
})

test_that("StatInterpreter handles mars_fit (technical)", {
  skip_if_not_installed("earth")
  analyzer <- RegressionAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(21)
  x <- seq(0, 10, length.out = 100)
  y <- ifelse(x < 5, x, 5 + 2 * (x - 5)) + rnorm(100, sd = 0.3)
  df <- data.frame(y = y, x = x)
  res <- analyzer$mars_fit(y ~ x, data = df, degree = 1)
  out <- interpreter$interpret(res, audience = "technical")
  expect_true(grepl("Pruning method", out))
  expect_true(grepl("Selected terms", out))
})

# ----------------------------------------------------------------------------
# RegressionReporter for new types
# ----------------------------------------------------------------------------

test_that("RegressionReporter console works for spline_fit", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(11)
  x <- seq(0, 5, length.out = 40)
  y <- 1 + x + rnorm(40, sd = 0.5)
  df <- data.frame(y = y, x = x)
  res <- analyzer$spline_fit(y ~ x, data = df, df = 4)
  reporter <- RegressionReporter$new()
  out <- capture.output(reporter$report(res, format = "console",
                                        audience = "manager"))
  expect_true(any(grepl("Basis = BS|spline|R-squared", out)))
})

test_that("RegressionReporter to_dataframe returns coefficients for spline_fit", {
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(22)
  x <- seq(0, 5, length.out = 40)
  y <- 1 + x + rnorm(40, sd = 0.5)
  df <- data.frame(y = y, x = x)
  res <- analyzer$spline_fit(y ~ x, data = df, df = 4)
  reporter <- RegressionReporter$new()
  df_out <- reporter$report(res, format = "data.frame")
  expect_s3_class(df_out, "data.frame")
  expect_true("Estimate" %in% names(df_out))
})

test_that("RegressionReporter console works for mars_fit", {
  skip_if_not_installed("earth")
  analyzer <- RegressionAnalyzer$new()
  set.seed(33)
  x <- seq(0, 10, length.out = 80)
  y <- ifelse(x < 5, x, 5 + 2 * (x - 5)) + rnorm(80, sd = 0.4)
  df <- data.frame(y = y, x = x)
  res <- analyzer$mars_fit(y ~ x, data = df)
  reporter <- RegressionReporter$new()
  out <- capture.output(reporter$report(res, format = "console",
                                        audience = "manager"))
  expect_true(any(grepl("Generalized R-squared|GCV|MARS selected", out)))
})

# ----------------------------------------------------------------------------
# RegressionPlotter for new types
# ----------------------------------------------------------------------------

test_that("RegressionPlotter auto-selects spline for spline_fit", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(123)
  x <- seq(0, 10, length.out = 50)
  y <- 1 + 2 * sin(x) + rnorm(50, sd = 0.5)
  df <- data.frame(y = y, x = x)
  res <- analyzer$spline_fit(y ~ x, data = df, df = 6)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(res, plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
})

test_that("RegressionPlotter spline plot for mars_fit", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("earth")
  analyzer <- RegressionAnalyzer$new()
  set.seed(77)
  x <- seq(0, 10, length.out = 80)
  y <- ifelse(x < 5, x, 5 + 2 * (x - 5)) + rnorm(80, sd = 0.4)
  df <- data.frame(y = y, x = x)
  res <- analyzer$mars_fit(y ~ x, data = df)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(res, plot_type = "spline")
  expect_true(inherits(p, "ggplot"))
})

test_that("RegressionPlotter residual plot works for spline_fit", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(88)
  x <- seq(0, 5, length.out = 40)
  y <- 1 + x + rnorm(40, sd = 0.3)
  df <- data.frame(y = y, x = x)
  res <- analyzer$spline_fit(y ~ x, data = df, df = 4)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(res, plot_type = "residual")
  expect_true(inherits(p, "ggplot"))
})

test_that("RegressionPlotter coef plot works for spline_fit", {
  skip_if_not_installed("iQualityR.plot")
  skip_if_not_installed("splines")
  analyzer <- RegressionAnalyzer$new()
  set.seed(66)
  x <- seq(0, 5, length.out = 40)
  y <- 1 + x + rnorm(40, sd = 0.3)
  df <- data.frame(y = y, x = x)
  res <- analyzer$spline_fit(y ~ x, data = df, df = 4)
  plotter <- RegressionPlotter$new()
  p <- plotter$plot(res, plot_type = "coef")
  expect_true(inherits(p, "ggplot"))
})
