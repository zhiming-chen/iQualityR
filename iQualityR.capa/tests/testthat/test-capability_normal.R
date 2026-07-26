# =============================================================================
# File: tests/testthat/test-capability_normal.R
# Description: Unit tests for normal capability analysis
# =============================================================================

test_that("capability_normal returns correct indices for normal data", {
  set.seed(123)
  df <- data.frame(value = rnorm(100, mean = 100, sd = 2))
  task <- capability_normal(df, "value", lsl = 90, usl = 110)
  expect_s3_class(task, "IqrCapabilityTask")
  expect_true(!is.null(task$results))
  # Cp = (110-90)/(6*2) = 20/12 = 1.6667
  expect_equal(round(task$results$statistics$cp, 2), 1.67, tolerance = 0.02)
  expect_equal(round(task$results$statistics$cpk, 2), 1.67, tolerance = 0.02)
})

test_that("capability_normal handles subgroups correctly", {
  set.seed(123)
  df <- data.frame(
    value = c(rnorm(50, 100, 2), rnorm(50, 102, 2)),
    batch = rep(1:10, each = 10)
  )
  task <- capability_normal(df, "value", lsl = 90, usl = 110, subgroup = "batch")
  expect_true(task$results$statistics$sd_within < task$results$statistics$sd_overall)
})

test_that("capability_normal flags small sample in diagnostics", {
  df <- data.frame(value = rnorm(10, 100, 2))
  task <- capability_normal(df, "value", lsl = 90, usl = 110)
  expect_true(length(task$results$diagnostics$warnings) > 0)
})

test_that("capability_normal returns correct PPM", {
  set.seed(123)
  df <- data.frame(value = rnorm(1000, 100, 2))
  task <- capability_normal(df, "value", lsl = 98, usl = 102)
  # Expected defect rate is approximately 3173 ppm
  expect_true(task$results$data_tables$ppm_overall$PPM[3] < 5000)
})

test_that("capability_normal accepts temporary theme in plot", {
  set.seed(123)
  df <- data.frame(value = rnorm(100, 100, 2))
  task <- capability_normal(df, "value", lsl = 90, usl = 110)
  p1 <- task$plot(type = "basic")
  p2 <- task$plot(type = "basic", theme = "tech")
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  # The two plots should differ (different themes)
  expect_false(identical(p1$theme, p2$theme))
})
