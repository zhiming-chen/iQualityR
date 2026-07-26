# =============================================================================
# File: tests/testthat/test-predictive.R
# Description: Unit tests for quality prediction modeling module
# =============================================================================

library(testthat)
library(iQualityR)

# =============================================================================
# Test Data Preparation
# =============================================================================

# Create mock regression data
create_mock_regression_data <- function(n = 200) {
  set.seed(123)
  tibble(
    temperature = runif(n, 100, 200),
    pressure = runif(n, 50, 100),
    time = runif(n, 10, 30),
    humidity = runif(n, 30, 70),
    product_strength = 50 + 0.3 * temperature + 0.2 * pressure - 0.1 * humidity +
                         rnorm(n, 0, 5)
  )
}

# Create mock classification data
create_mock_classification_data <- function(n = 200) {
  set.seed(123)

  # Stepwise generation to ensure class balance
  temperature <- runif(n, 100, 200)
  pressure <- runif(n, 50, 100)
  time <- runif(n, 10, 30)

  # Use logit model to ensure balanced class distribution
  logit <- -50 + 0.3 * temperature + 0.2 * pressure
  prob <- 1 / (1 + exp(-logit))
  pass_fail <- ifelse(runif(n) < prob, "Pass", "Fail")

  tibble::tibble(
    temperature = temperature,
    pressure = pressure,
    time = time,
    pass_fail = factor(pass_fail)
  )
}

# Create mock time series data
create_mock_time_series_data <- function(n = 100) {
  set.seed(123)
  tibble(
    date = seq.Date(as.Date("2024-01-01"), by = "month", length.out = n),
    month_index = 1:n,
    defect_rate = 5 + 0.02 * (1:n) + rnorm(n, 0, 0.5)
  )
}

# =============================================================================
# PredictivePlan Tests
# =============================================================================

test_that("PredictivePlan initializes correctly (Business Mode)", {
  plan <- PredictivePlan$new(
    task_tag = "regression",
    target = "product_strength",
    factors = c("temperature", "pressure")
  )

  expect_equal(plan$task_tag, "regression")
  expect_equal(plan$target_var, "product_strength")
  expect_equal(plan$factor_vars, c("temperature", "pressure"))
  expect_equal(plan$model_type, "auto")
  expect_false(plan$is_expert_mode())
})

test_that("PredictivePlan initializes correctly (Expert Mode)", {
  plan <- PredictivePlan$new(
    task_tag = "regression",
    target = "product_strength",
    factors = c("temperature", "pressure"),
    expert_config = list(
      tune_grid = expand.grid(trees = c(100, 500))
    )
  )

  expect_true(plan$is_expert_mode())
  expect_equal(plan$expert_config$tune_grid$trees, c(100, 500))
})

test_that("PredictivePlan validate detects missing target variable", {
  plan <- PredictivePlan$new(
    task_tag = "regression",
    target = "nonexistent",
    factors = c("temperature")
  )

  data <- create_mock_regression_data()

  expect_error(plan$validate(data), "Target variable.*not in data")
})

test_that("PredictivePlan validate auto-infers factor variables", {
  plan <- PredictivePlan$new(
    task_tag = "regression",
    target = "product_strength",
    factors = NULL
  )

  data <- create_mock_regression_data()
  plan$validate(data)

  expect_equal(plan$factor_vars, c("temperature", "pressure", "time", "humidity"))
})

test_that("PredictivePlan supports classification task validation", {
  plan <- PredictivePlan$new(
    task_tag = "classification",
    target = "pass_fail",
    factors = c("temperature", "pressure")
  )

  data <- create_mock_classification_data()
  expect_no_error(plan$validate(data))
})

test_that("PredictivePlan detects class imbalance", {
  # Create severely unbalanced data
  set.seed(123)
  unbalanced_data <- data.frame(
    x = rnorm(100),
    y = factor(c(rep("A", 95), rep("B", 5)))
  )

  plan <- PredictivePlan$new(
    task_tag = "classification",
    target = "y",
    factors = "x"
  )

  expect_warning(plan$validate(unbalanced_data), "Class distribution is severely imbalanced")
})

# =============================================================================
# ModelTrainer Tests
# =============================================================================

test_that("ModelTrainer can train linear regression model", {
  data <- create_mock_regression_data()
  plan <- PredictivePlan$new(
    task_tag = "regression",
    target = "product_strength",
    factors = c("temperature", "pressure"),
    model_type = "linear"
  )

  trainer <- ModelTrainer$new()
  trainer$train_auto(data, plan)

  expect_true(inherits(trainer$raw_model, "lm"))
  expect_true(!is.null(trainer$model_metrics))
  expect_true(!is.null(trainer$fitted_values))
  expect_equal(length(trainer$fitted_values), nrow(data))
})

test_that("ModelTrainer model rating is reasonable", {
  trainer <- ModelTrainer$new()

  # Test different R-squared values (using public interface)
  high_r2 <- trainer$rate_model(list(r_squared = 0.85))
  expect_equal(high_r2$level, "Excellent")
  expect_equal(high_r2$stars, 5)

  med_r2 <- trainer$rate_model(list(r_squared = 0.60))
  expect_equal(med_r2$level, "Acceptable")
  expect_equal(med_r2$stars, 3)

  low_r2 <- trainer$rate_model(list(r_squared = 0.20))
  expect_equal(low_r2$level, "Not Recommended")
  expect_equal(low_r2$stars, 1)
})

test_that("ModelTrainer can train classification model (Logistic Regression)", {
  data <- create_mock_classification_data()
  plan <- PredictivePlan$new(
    task_tag = "classification",
    target = "pass_fail",
    factors = c("temperature", "pressure"),
    model_type = "logistic"
  )

  trainer <- ModelTrainer$new()
  trainer$train_auto(data, plan)

  expect_true(inherits(trainer$raw_model, "glm"))
  expect_true(!is.null(trainer$model_metrics))
  expect_true(!is.null(trainer$model_metrics$accuracy))
})

test_that("ModelTrainer intelligent algorithm selection is reasonable", {
  trainer <- ModelTrainer$new()

  # Small data should select linear model
  small_data <- create_mock_regression_data(n = 50)
  plan_small <- PredictivePlan$new(
    task_tag = "regression",
    target = "product_strength",
    factors = c("temperature", "pressure"),
    model_type = "auto"
  )

  # Use public interface
  algo <- trainer$select_algorithm(small_data, plan_small)
  expect_equal(algo, "linear")

  # Multi-factor data should select ridge regression
  set.seed(123)
  wide_data <- as.data.frame(matrix(rnorm(150 * 30), ncol = 30))
  names(wide_data) <- paste0("f", 1:30)
  wide_data$target <- rowSums(wide_data[, 1:5]) + rnorm(150)

  plan_wide <- PredictivePlan$new(
    task_tag = "regression",
    target = "target",
    factors = paste0("f", 1:25),
    model_type = "auto"
  )

  # Use public interface
  algo <- trainer$select_algorithm(wide_data, plan_wide)
  expect_equal(algo, "ridge")
})

# =============================================================================
# DiagnosticAnalyzer Tests
# =============================================================================

test_that("DiagnosticAnalyzer can execute residual analysis", {
  data <- create_mock_regression_data()
  plan <- PredictivePlan$new(
    task_tag = "regression",
    target = "product_strength",
    factors = c("temperature", "pressure")
  )

  trainer <- ModelTrainer$new()
  trainer$train_auto(data, plan)

  analyzer <- DiagnosticAnalyzer$new()
  analyzer$analyze(trainer, data, plan)

  expect_true(!is.null(analyzer$diagnostics$residuals))
  expect_true(!is.null(analyzer$diagnostics$normality))
})

test_that("DiagnosticAnalyzer can generate improvement suggestions", {
  data <- create_mock_regression_data()
  plan <- PredictivePlan$new(
    task_tag = "regression",
    target = "product_strength",
    factors = c("temperature", "pressure")
  )

  trainer <- ModelTrainer$new()
  trainer$train_auto(data, plan)

  analyzer <- DiagnosticAnalyzer$new()
  analyzer$analyze(trainer, data, plan)

  expect_true(is.character(analyzer$recommendations))
})

# =============================================================================
# IqrPredictiveTask Tests
# =============================================================================

test_that("IqrPredictiveTask can complete full workflow", {
  data <- create_mock_regression_data()

  task <- regression_analysis(
    data = data,
    target = "product_strength",
    factors = c("temperature", "pressure")
  )

  expect_true(inherits(task, "IqrPredictiveTask"))

  # Execute computation
  expect_no_error(task$compute())

  # Check results
  expect_true(!is.null(task$results))
  expect_true(!is.null(task$results$model))
  expect_true(!is.null(task$results$metrics))

  # summary should return self reference
  expect_invisible(task$summary())
})

test_that("IqrPredictiveTask plot can generate charts", {
  skip_if_not_installed("patchwork")

  data <- create_mock_regression_data()
  task <- regression_analysis(
    data = data,
    target = "product_strength",
    factors = c("temperature", "pressure")
  )
  task$compute()

  # Should be able to generate basic charts
  expect_no_error(task$plot(type = "basic"))
})

test_that("IqrPredictiveTask can handle missing values", {
  data <- create_mock_regression_data()
  # Artificially add missing values
  data$temperature[1:5] <- NA
  data$pressure[6:10] <- NA

  task <- regression_analysis(
    data = data,
    target = "product_strength",
    factors = c("temperature", "pressure")
  )

  expect_no_error(task$compute())
})

# =============================================================================
# Entry Function Tests
# =============================================================================

test_that("regression_analysis creates correct task object", {
  data <- create_mock_regression_data()

  task <- regression_analysis(
    data = data,
    target = "product_strength",
    factors = c("temperature", "pressure")
  )

  expect_true(inherits(task, "IqrPredictiveTask"))
  expect_equal(task$plan$task_tag, "regression")
})

test_that("classification_analysis creates correct task object", {
  data <- create_mock_classification_data()

  task <- classification_analysis(
    data = data,
    target = "pass_fail",
    factors = c("temperature", "pressure")
  )

  expect_true(inherits(task, "IqrPredictiveTask"))
  expect_equal(task$plan$task_tag, "classification")
})

test_that("time_series_forecast creates correct task object", {
  data <- create_mock_time_series_data()

  task <- time_series_forecast(
    data = data,
    target = "defect_rate",
    time_column = "date",
    horizon = 12
  )

  expect_true(inherits(task, "IqrPredictiveTask"))
  expect_equal(task$plan$task_tag, "time_series")
  expect_equal(task$plan$meta_data$method$forecast_horizon, 12)
})

# =============================================================================
# Pipe Operation Tests
# =============================================================================

test_that("Tasks support pipe operations", {
  skip_if_not_installed("dplyr")

  data <- create_mock_regression_data()

  result <- data |>
    regression_analysis(
      target = "product_strength",
      factors = c("temperature", "pressure")
    ) |>
    (\(x) x$compute())()

  expect_true(inherits(result, "IqrPredictiveTask"))
  expect_true(!is.null(result$results))
})

# =============================================================================
# Boundary Condition Tests
# =============================================================================

test_that("Should error when data volume is too small", {
  small_data <- data.frame(
    x = 1:5,
    y = c(10, 12, 11, 13, 12)
  )

  expect_error(
    regression_analysis(
      data = small_data,
      target = "y",
      factors = "x"
    )$compute(),
    "Insufficient data"
  )
})

test_that("Calling plot before compute should error", {
  data <- create_mock_regression_data()
  task <- regression_analysis(
    data = data,
    target = "product_strength",
    factors = c("temperature", "pressure")
  )

  expect_error(task$plot(), "Computation not yet executed")
})

test_that("Calling summary before compute should give hint", {
  data <- create_mock_regression_data()
  task <- regression_analysis(
    data = data,
    target = "product_strength",
    factors = c("temperature", "pressure")
  )

  expect_output(task$summary(), "Computation not yet executed")
})
