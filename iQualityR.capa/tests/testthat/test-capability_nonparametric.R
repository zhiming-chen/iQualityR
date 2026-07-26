# =============================================================================
# File: tests/testthat/test-capability_nonparametric.R
# Description: Unit tests for non-parametric capability analysis
# =============================================================================

# Test data
set.seed(123)
test_data <- data.frame(
  measurement = c(
    rweibull(50, shape = 2, scale = 50),
    rlnorm(50, meanlog = 3, sdlog = 0.5)
  )
)

test_that("Basic non-parametric capability analysis works", {
  result <- capability_nonparametric(
    data = test_data, measurement = "measurement",
    lsl = 10, usl = 100
  )

  expect_s3_class(result, "IqrCapabilityTask")
  expect_true(!is.null(result$results))
  expect_true(!is.null(result$results$statistics))
  expect_true(!is.null(result$results$diagnostics))

  # Check statistics
  stats <- result$results$statistics
  expect_true(!is.null(stats$cp))
  expect_true(!is.null(stats$cpk))
  expect_true(!is.null(stats$pp))
  expect_true(!is.null(stats$ppk))
  expect_true(is.numeric(stats$cp))
  expect_true(is.numeric(stats$cpk))
  expect_true(is.numeric(stats$pp))
  expect_true(is.numeric(stats$ppk))

  # Check diagnostics
  diag <- result$results$diagnostics
  expect_true(!is.null(diag$method))
  expect_equal(diag$method, "kernel")
})

test_that("Non-parametric analysis with specified method works", {
  result <- capability_nonparametric(
    data = test_data, measurement = "measurement",
    lsl = 10, usl = 100,
    nonparametric_method = "empirical"
  )

  expect_s3_class(result, "IqrCapabilityTask")
  expect_true(!is.null(result$results))

  # Check that the method is correctly set
  diag <- result$results$diagnostics
  expect_true(!is.null(diag$method))
  expect_equal(diag$method, "empirical")
})

test_that("Non-parametric analysis with bootstrap CI works", {
  result <- capability_nonparametric(
    data = test_data, measurement = "measurement",
    lsl = 10, usl = 100,
    use_bootstrap = TRUE, bootstrap_samples = 100
  )

  expect_s3_class(result, "IqrCapabilityTask")
  expect_true(!is.null(result$results))

  # Check Bootstrap results
  expect_true(!is.null(result$results$data_tables$bootstrap_ci))
  expect_s3_class(result$results$data_tables$bootstrap_ci, "data.frame")
  expect_true("Statistic" %in% names(result$results$data_tables$bootstrap_ci))
  expect_true("Estimate" %in% names(result$results$data_tables$bootstrap_ci))
  expect_true("Lower_CI" %in% names(result$results$data_tables$bootstrap_ci))
  expect_true("Upper_CI" %in% names(result$results$data_tables$bootstrap_ci))
})

test_that("Non-parametric analysis with target works", {
  result <- capability_nonparametric(
    data = test_data, measurement = "measurement",
    lsl = 10, usl = 100, target = 50
  )

  expect_s3_class(result, "IqrCapabilityTask")
  expect_true(!is.null(result$results))

  # Check that Cpm is calculated
  stats <- result$results$statistics
  expect_true(!is.null(stats$cpm))
  expect_true(is.numeric(stats$cpm))
})

test_that("Non-parametric analysis handles small sample size", {
  small_data <- data.frame(
    measurement = rweibull(20, shape = 2, scale = 50)
  )

  # Should be able to run
  result <- capability_nonparametric(
    data = small_data, measurement = "measurement",
    lsl = 10, usl = 100
  )

  expect_s3_class(result, "IqrCapabilityTask")
  expect_true(!is.null(result$results))
})

test_that("Non-parametric analysis summary method works", {
  result <- capability_nonparametric(
    data = test_data, measurement = "measurement",
    lsl = 10, usl = 100
  )

  # Test that summary method does not error and produces output
  expect_output(result$summary())
})

test_that("Non-parametric analysis plot method works", {
  result <- capability_nonparametric(
    data = test_data, measurement = "measurement",
    lsl = 10, usl = 100
  )

  # Test that plot method does not error
  p <- result$plot(type = "basic")
  expect_true(!is.null(p))
})

test_that("Non-parametric analysis input validation works", {
  # Non-data-frame input
  expect_error(capability_nonparametric(
    data = "not a data.frame", measurement = "measurement",
    lsl = 10, usl = 100
  ))

  # Non-existent column
  expect_error(capability_nonparametric(
    data = test_data, measurement = "nonexistent",
    lsl = 10, usl = 100
  ))

  # Non-numeric column
  non_numeric_data <- data.frame(measurement = c("a", "b", "c"))
  expect_error(capability_nonparametric(
    data = non_numeric_data, measurement = "measurement",
    lsl = 10, usl = 100
  ))

  # lsl >= usl
  expect_error(capability_nonparametric(
    data = test_data, measurement = "measurement",
    lsl = 100, usl = 10
  ))

  # Invalid non-parametric method
  expect_error(capability_nonparametric(
    data = test_data, measurement = "measurement",
    lsl = 10, usl = 100,
    nonparametric_method = "invalid"
  ))
})
