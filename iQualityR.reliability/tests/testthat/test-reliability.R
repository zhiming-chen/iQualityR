# =============================================================================
# Tests for iQualityR.reliability
# =============================================================================

test_that("ReliabilityPlan initializes and validates enums", {
  plan <- ReliabilityPlan$new(
    time_var     = "time",
    distribution = "weibull",
    method       = "parametric"
  )
  expect_s3_class(plan, "ReliabilityPlan")
  expect_equal(plan$distribution, "weibull")
  expect_equal(plan$method, "parametric")
  expect_equal(plan$censoring_type, "right")

  # Invalid distribution should error
  expect_error(
    ReliabilityPlan$new(time_var = "t", distribution = "gamma"),
    class = "simpleError"
  )

  # Invalid method should error
  expect_error(
    ReliabilityPlan$new(time_var = "t", method = "bayesian"),
    class = "simpleError"
  )
})

test_that("ReliabilityPlan$validate checks data integrity", {
  plan <- ReliabilityPlan$new(time_var = "time", method = "parametric")
  df <- data.frame(time = c(10, 20, 30, 40, 50))
  expect_silent(plan$validate(df))

  # Missing variable
  expect_error(plan$validate(data.frame(x = 1:5)), class = "simpleError")

  # Non-positive time
  bad <- data.frame(time = c(10, -5, 30, 40, 50))
  expect_error(plan$validate(bad), class = "simpleError")

  # Insufficient sample size
  tiny <- data.frame(time = c(1, 2))
  expect_error(plan$validate(tiny), class = "simpleError")
})

test_that("ReliabilityPlan$validate detects bad censoring_type", {
  plan <- ReliabilityPlan$new(time_var = "time", method = "parametric")
  plan$censoring_type <- "bogus"
  df <- data.frame(time = 1:5)
  expect_error(plan$validate(df), class = "simpleError")
})

test_that("Parametric Weibull analysis works", {
  withr::local_seed(42)
  times <- rweibull(80, shape = 2, scale = 100)
  data  <- data.frame(time = times)

  result <- reliability_analysis(data, time_var = "time",
                                  distribution = "weibull")
  expect_equal(result$method, "parametric")
  expect_equal(result$distribution, "weibull")
  expect_equal(result$n, 80)
  expect_equal(result$n_events, 80)
  expect_equal(result$n_censored, 0)
  expect_true(result$reliability_metrics$mttf > 50)
  expect_true(result$reliability_metrics$mttf < 150)
  expect_true(nrow(result$survival_function) == 200)
  expect_true(nrow(result$hazard_function) == 200)
  expect_length(result$diagnostics$recommendations, 1L)
})

test_that("Parametric exponential analysis works", {
  withr::local_seed(7)
  times <- rexp(100, rate = 0.01)
  data  <- data.frame(time = times)

  result <- reliability_analysis(data, time_var = "time",
                                  distribution = "exponential")
  expect_equal(result$distribution, "exponential")
  expect_true(result$reliability_metrics$failure_rate > 0)
  # MTTF should be close to 1/rate = 100
  expect_gt(result$reliability_metrics$mttf, 50)
})

test_that("Parametric lognormal analysis works", {
  withr::local_seed(11)
  times <- rlnorm(100, meanlog = log(80), sdlog = 0.4)
  data  <- data.frame(time = times)

  result <- reliability_analysis(data, time_var = "time",
                                  distribution = "lognormal")
  expect_equal(result$distribution, "lognormal")
  expect_true(!is.na(result$reliability_metrics$b50_life))
})

test_that("ReliabilityAnalyzer inherits IqrAnalyzerBase", {
  a <- ReliabilityAnalyzer$new()
  expect_s3_class(a, "IqrAnalyzerBase")
  expect_s3_class(a, "ReliabilityAnalyzer")
})

test_that("ReliabilityPlotter produces ggplot for Weibull", {
  withr::local_seed(99)
  times <- rweibull(50, shape = 2, scale = 100)
  data  <- data.frame(time = times)
  result <- reliability_analysis(data, time_var = "time")

  plotter <- ReliabilityPlotter$new()
  expect_s3_class(plotter, "IqrPlotterBase")

  theme <- IqrTheme$new("academic")
  p <- plotter$render(result, theme, type = "survival")
  expect_s3_class(p, "ggplot")

  p2 <- plotter$render(result, theme, type = "probability")
  expect_s3_class(p2, "ggplot")

  p3 <- plotter$render(result, theme, type = "hazard")
  expect_s3_class(p3, "ggplot")
})

test_that("ReliabilityPlotter errors on NULL results", {
  plotter <- ReliabilityPlotter$new()
  expect_error(plotter$render(NULL, NULL), class = "simpleError")
})

test_that("ReliabilityReporter prints without error", {
  withr::local_seed(3)
  times <- rweibull(40, shape = 2, scale = 100)
  data  <- data.frame(time = times)
  result <- reliability_analysis(data, "time")

  reporter <- ReliabilityReporter$new()
  # print_console() deliberately writes to stdout; just verify no error.
  expect_no_error(reporter$print_console(result))
  df <- reporter$to_dataframe(result)
  expect_s3_class(df, "data.frame")
})

test_that("IqrReliabilityTask end-to-end", {
  withr::local_seed(13)
  data <- data.frame(time = rweibull(60, shape = 2, scale = 100))
  plan <- ReliabilityPlan$new(time_var = "time",
                              distribution = "weibull",
                              method = "parametric")

  task <- IqrReliabilityTask$new(data, plan)
  expect_s3_class(task, "IqrTaskBase")
  expect_s3_class(task, "IqrReliabilityTask")

  expect_silent(task$compute())
  expect_false(is.null(task$results))

  # summary() deliberately prints to console; just verify no error.
  expect_no_error(task$summary())

  # plot returns a ggplot or patchwork
  p <- task$plot(type = "survival")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("cox_model convenience function works with survival", {
  skip_if_not_installed("survival")
  withr::local_seed(5)
  data <- data.frame(
    time   = rexp(80, rate = 0.01),
    status = rbinom(80, 1, 0.7),
    age    = rnorm(80, 50, 10),
    treat  = rbinom(80, 1, 0.5)
  )
  result <- cox_model(data, "time", "status", factors = c("age", "treat"))
  expect_equal(result$method, "cox")
  expect_equal(result$n, 80)
  expect_s3_class(result$cox_model$coefficients, "data.frame")
  expect_equal(nrow(result$cox_model$coefficients), 2L)
})

test_that("kaplan_meier_estimate convenience function works", {
  skip_if_not_installed("survival")
  withr::local_seed(21)
  data <- data.frame(
    time   = rexp(60, rate = 0.01),
    status = rbinom(60, 1, 0.8)
  )
  result <- kaplan_meier_estimate(data, "time", "status")
  expect_equal(result$method, "kaplan_meier")
  expect_equal(result$n, 60)
  expect_true(nrow(result$survival_curve) > 0)
})
