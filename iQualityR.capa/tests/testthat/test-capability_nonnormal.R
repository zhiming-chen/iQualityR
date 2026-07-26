# =============================================================================
# File: tests/testthat/test-capability_nonnormal.R
# Description: Unit tests for non-normal capability analysis
# =============================================================================

test_that("Auto distribution identification works", {
  set.seed(123)
  # Generate Weibull distribution data
  data <- data.frame(
    measurement = rweibull(100, shape = 2, scale = 50)
  )

  result <- capability_nonnormal(
    data = data, measurement = "measurement",
    lsl = 10, usl = 100,
    distribution = "auto"
  )

  expect_s3_class(result, "IqrCapabilityTask")
  expect_true(is.list(result$results))
  expect_true(!is.null(result$results$statistics))
  expect_true(!is.null(result$results$diagnostics))
  expect_true(!is.null(result$results$statistics$cpk))
  expect_true(!is.null(result$results$statistics$ppk))
  # Check that Weibull distribution is identified
  expect_equal(result$results$diagnostics$distribution, "weibull")
})

test_that("Specified distribution works", {
  set.seed(123)
  # Generate Lognormal distribution data
  data <- data.frame(
    measurement = rlnorm(100, meanlog = 3, sdlog = 0.5)
  )

  result <- capability_nonnormal(
    data = data, measurement = "measurement",
    lsl = 5, usl = 50,
    distribution = "lognormal"
  )

  expect_s3_class(result, "IqrCapabilityTask")
  expect_true(is.list(result$results))
  expect_true(!is.null(result$results$statistics))
  expect_true(!is.null(result$results$diagnostics))
  expect_true(!is.null(result$results$statistics$cpk))
  expect_true(!is.null(result$results$statistics$ppk))
  # Check that the specified Lognormal distribution is used
  expect_equal(result$results$diagnostics$distribution, "lognormal")
})

test_that("Bootstrap CI works", {
  set.seed(123)
  data <- data.frame(
    measurement = rweibull(100, shape = 2, scale = 50)
  )

  result <- capability_nonnormal(
    data = data, measurement = "measurement",
    lsl = 10, usl = 100,
    distribution = "auto",
    use_bootstrap = TRUE, bootstrap_samples = 100
  )

  expect_s3_class(result, "IqrCapabilityTask")
  expect_true(!is.null(result$results$data_tables$bootstrap_ci))
  expect_true(nrow(result$results$data_tables$bootstrap_ci) > 0)
})

test_that("Edge cases are handled", {
  # Test small sample
  set.seed(123)
  data_small <- data.frame(
    measurement = rweibull(15, shape = 2, scale = 50)
  )

  # Should run but produce a warning diagnostic
  result_small <- capability_nonnormal(
    data = data_small, measurement = "measurement",
    lsl = 10, usl = 100,
    distribution = "auto"
  )

  expect_s3_class(result_small, "IqrCapabilityTask")

  # Test data boundary: lsl > usl should error
  data_boundary <- data.frame(
    measurement = c(1, 2, 3, 4, 5)
  )

  expect_error({
    capability_nonnormal(
      data = data_boundary, measurement = "measurement",
      lsl = 5, usl = 1,
      distribution = "auto"
    )
  })

  # Non-existent column should error
  expect_error({
    capability_nonnormal(
      data = data_boundary, measurement = "nonexistent",
      lsl = 1, usl = 5,
      distribution = "auto"
    )
  })

  # Non-data-frame input should error
  expect_error({
    capability_nonnormal(
      data = "not a data frame",
      measurement = "measurement",
      lsl = 1, usl = 5,
      distribution = "auto"
    )
  })
})

test_that("All supported distributions work", {
  set.seed(123)
  data <- data.frame(
    measurement = rweibull(100, shape = 2, scale = 50)
  )

  distributions <- c("weibull", "lognormal", "gamma", "exponential", "logistic")

  for (dist in distributions) {
    # For exponential distribution, use positive data
    if (dist == "exponential") {
      data$measurement <- rexp(100, rate = 0.02)
    }

    result <- capability_nonnormal(
      data = data, measurement = "measurement",
      lsl = 10, usl = 100,
      distribution = dist
    )

    expect_s3_class(result, "IqrCapabilityTask")
    expect_true(!is.null(result$results$statistics))
    expect_true(!is.null(result$results$diagnostics))
  }
})

test_that("Task methods work", {
  set.seed(123)
  data <- data.frame(
    measurement = rweibull(100, shape = 2, scale = 50)
  )

  result <- capability_nonnormal(
    data = data, measurement = "measurement",
    lsl = 10, usl = 100,
    distribution = "auto"
  )

  # Test summary method
  expect_output(result$summary())

  # Test plot method (check it does not error)
  p <- result$plot(type = "basic")
  expect_true(!is.null(p))

  # Test report method exists
  expect_true("report" %in% names(result))
})
