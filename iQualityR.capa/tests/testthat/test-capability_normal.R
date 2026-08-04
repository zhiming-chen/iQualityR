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

  s <- task$results$statistics
  tol <- 110 - 90

  # Cp uses within-subgroup sigma (moving range, via iQualityR.stat);
  # Pp uses the overall sigma. Verify each index is applied to the correct
  # sigma component (the within/overall distinction is the whole point of
  # Cp vs Pp).
  expect_equal(s$cp, tol / (6 * s$sd_within), tolerance = 1e-6)
  expect_equal(s$pp, tol / (6 * s$sd_overall), tolerance = 1e-6)

  # Centering: realized performance cannot exceed potential capability.
  expect_true(s$cpk <= s$cp)
  expect_true(s$ppk <= s$pp)

  # Deterministic values for this seed
  # (mean ~ 100.18, overall sd ~ 1.83, MR-based within sd ~ 1.79).
  expect_equal(round(s$cp, 2), 1.87, tolerance = 0.02)
  expect_equal(round(s$cpk, 2), 1.83, tolerance = 0.02)
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
  expect_true(any(grepl("Sample size", task$results$diagnostics$warnings)))
})

test_that("capability_normal returns correct PPM", {
  set.seed(123)
  df <- data.frame(value = rnorm(1000, 100, 2))
  # ~3-sigma capable process (lsl/usl ~= mean +/- 3*sd).
  task <- capability_normal(df, "value", lsl = 94, usl = 106)
  ppm_total <- task$results$data_tables$ppm_overall$PPM[3]
  mu <- mean(df$value); sigma <- stats::sd(df$value)
  expected_ppm <- (stats::pnorm(94, mu, sigma) +
                    1 - stats::pnorm(106, mu, sigma)) * 1e6
  # Overall PPM must match the normal-distribution tail probability.
  expect_equal(ppm_total, expected_ppm, tolerance = 1)
  # ~3-sigma capable process: expected PPM well below 5000.
  expect_true(ppm_total < 5000)
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

# =============================================================================
# Transformation path (Box-Cox / Johnson / auto) - Minitab convention
# =============================================================================

test_that("capability_normal applies Box-Cox transform and improves normality", {
  set.seed(123)
  # Right-skewed positive data (exponential) - ideal for Box-Cox
  df <- data.frame(value = rexp(200, rate = 0.1))  # mean ~ 10
  task <- capability_normal(df, "value", lsl = 1, usl = 50,
                            transform = "box_cox")

  diag <- task$results$diagnostics
  stats <- task$results$statistics

  # Transform was applied and recorded
  expect_equal(diag$transform_applied, "Box-Cox")
  expect_false(is.null(diag$transform_lambda))
  expect_true(is.finite(diag$transform_lambda))

  # Normality should improve after transform
  expect_true(diag$transform_normality_after > diag$transform_normality_before)

  # Transformed spec limits are stored and finite
  expect_true(is.finite(diag$transformed_lsl))
  expect_true(is.finite(diag$transformed_usl))
  expect_true(diag$transformed_lsl < diag$transformed_usl)

  # Indices are computed on the transformed scale
  expect_true(is.finite(stats$cp))
  expect_true(is.finite(stats$cpk))
  expect_true(is.finite(stats$pp))
  expect_true(is.finite(stats$ppk))
  expect_true(stats$cpk <= stats$cp)

  # Sixpack histogram carries the transformed data and transformed specs
  hist <- task$results$data_tables$sixpack$histogram
  expect_true(mean(hist$values) != mean(df$value))  # transformed, not original
  expect_equal(hist$lsl, diag$transformed_lsl)
  expect_equal(hist$usl, diag$transformed_usl)

  # Original plan is not mutated
  expect_equal(task$plan$lsl, 1)
  expect_equal(task$plan$usl, 50)
  expect_equal(task$plan$transform, "box_cox")
})

test_that("capability_normal with transform=NULL does not apply any transform", {
  set.seed(123)
  df <- data.frame(value = rnorm(100, 100, 2))
  task <- capability_normal(df, "value", lsl = 90, usl = 110)

  # No transform diagnostics should be present
  expect_null(task$results$diagnostics$transform_applied)
  expect_null(task$results$diagnostics$transform_lambda)
  expect_null(task$results$diagnostics$transformed_lsl)
})

test_that("capability_normal Box-Cox falls back gracefully for non-positive data", {
  # Data with zero/negative values cannot be Box-Cox transformed
  df <- data.frame(value = c(-1, 0, 1, 2, 3, 4, 5, rnorm(50, 5, 2)))
  expect_warning(
    task <- capability_normal(df, "value", lsl = -5, usl = 15,
                              transform = "box_cox")
  )
  # Should fall back to untransformed analysis
  expect_null(task$results$diagnostics$transform_applied)
  expect_true(is.finite(task$results$statistics$cp))
})

test_that("capability_normal applies Johnson transform", {
  skip_if_not_installed("SuppDists")
  set.seed(123)
  # Skewed positive data for Johnson transform
  df <- data.frame(value = rf(200, df1 = 5, df2 = 20) * 10 + 5)
  task <- capability_normal(df, "value", lsl = 0, usl = 30,
                            transform = "johnson")

  diag <- task$results$diagnostics
  expect_equal(diag$transform_applied, "Johnson")
  expect_false(is.null(diag$transform_type))
  expect_true(is.finite(diag$transformed_lsl))
  expect_true(is.finite(diag$transformed_usl))
  expect_true(is.finite(task$results$statistics$cpk))
})

test_that("capability_normal transform produces plottable sixpack", {
  set.seed(123)
  df <- data.frame(value = rexp(150, rate = 0.1))
  task <- capability_normal(df, "value", lsl = 1, usl = 50,
                            transform = "box_cox", sixpack = TRUE)
  # Full sixpack should render without error on transformed data
  p <- task$plot(type = "full")
  expect_s3_class(p, "ggplot")
  # Basic histogram should also render
  p2 <- task$plot(type = "basic")
  expect_s3_class(p2, "ggplot")
})

test_that("capability_normal rejects invalid transform value", {
  df <- data.frame(value = rnorm(50, 100, 2))
  expect_error(
    capability_normal(df, "value", lsl = 90, usl = 110, transform = "invalid"),
    "transform must be"
  )
})
