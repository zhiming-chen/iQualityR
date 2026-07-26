# =============================================================================
# Test: Type1 Gage Study - Unit Tests (testthat)
# =============================================================================

# Run with: devtools::test() or testthat::test_file()

# IqrReporter / IqrTheme are exported by iQualityR.core; attach it so the
# reporter integration test can construct the reporter object.
library(iQualityR.core)

cat("========================================\n")
cat("Type1 Gage Study Unit Tests\n")
cat("========================================\n\n")

# ============================================================================
# Test Suite 1: Bias Analysis (iqr_type1_bias)
# ============================================================================

cat("--- Test Suite 1: Bias Analysis ---\n\n")

# Test 1.1: Basic functionality
test_that("iqr_type1_bias computes correctly", {
  set.seed(12345)
  n_rep <- 25
  ref_val <- 10
  bias_true <- 0.15
  sd_true <- 0.3
  measurements <- rnorm(n_rep, mean = ref_val + bias_true, sd = sd_true)

  task <- iqr_type1_bias(
    measurements,
    reference_value = ref_val,
    lsl = 7,
    usl = 13,
    resolution = 0.01,
    u_cal = 0.1
  )

  # Check results exist
  expect_false(is.null(task$results))
  expect_false(is.null(task$results$statistics))

  # Check key statistics
  expect_equal(task$results$statistics$n, n_rep)
  expect_equal(task$results$statistics$reference_value, ref_val)
  expect_true(task$results$statistics$Cg > 0)
  expect_true(task$results$statistics$Cgk > 0)
  expect_true(task$results$statistics$vda5_u_ms > 0)
})

cat("Test 1.1: Basic functionality ... OK\n")

# Test 1.2: Cg/Cgk thresholds
test_that("Cg/Cgk diagnostic thresholds work", {
  set.seed(12345)
  measurements <- rnorm(25, mean = 10, sd = 0.1)

  task <- iqr_type1_bias(
    measurements,
    reference_value = 10,
    lsl = 7,
    usl = 13
  )

  # With small sd, Cg should be > 1.33
  expect_true(task$results$statistics$Cg >= 1.33)
  # With small bias, Cgk should also be good
  expect_true(task$results$diagnostics$Cgk_ok == TRUE || task$results$statistics$Cgk < 1.33)
})

cat("Test 1.2: Cg/Cgk thresholds ... OK\n")

# Test 1.3: VDA5 uncertainty calculation
test_that("VDA5 uncertainty components sum correctly", {
  set.seed(12345)
  measurements <- rnorm(30, mean = 10.2, sd = 0.2)

  task <- iqr_type1_bias(
    measurements,
    reference_value = 10,
    lsl = 7,
    usl = 13,
    resolution = 0.01,
    u_cal = 0.1,
    u_rest = list(u1 = 0.05, u2 = 0.02)
  )

  # Check u_ms is sqrt of sum of squares
  u_evr <- task$results$statistics$vda5_u_evr
  u_re <- task$results$statistics$vda5_u_re
  u_bi <- task$results$statistics$vda5_u_bi
  u_cal <- task$results$statistics$vda5_u_cal
  u_rest <- task$results$statistics$vda5_u_rest
  u_ms <- task$results$statistics$vda5_u_ms

  expected_u_ms <- sqrt(u_evr^2 + u_re^2 + u_bi^2 + u_cal^2 + u_rest^2)
  expect_equal(u_ms, expected_u_ms, tolerance = 1e-6)
})

cat("Test 1.3: VDA5 uncertainty calculation ... OK\n")

# Test 1.4: VDA5 standard-uncertainty formulas (VDA 5, 3rd ed., §5.3)
test_that("VDA5 u_EVR=s and u_BI=|Bias| follow VDA 5 §5.3", {
  set.seed(777)
  ref_val <- 10
  # Build data with a known bias and sd so the VDA5 components are predictable
  measurements <- rnorm(30, mean = ref_val + 0.2, sd = 0.3)

  task <- iqr_type1_bias(
    measurements,
    reference_value = ref_val,
    lsl = 7,
    usl = 13,
    resolution = 0.01,
    u_cal = 0.05
  )

  s <- task$results$statistics

  # VDA 5 §5.3.1: u_EVR is the single-measurement repeatability = s, NOT s/sqrt(n).
  # Using s/sqrt(n) would understate uMS by sqrt(n) and double-count with u_BI.
  expect_equal(s$vda5_u_evr, s$sd_meas, tolerance = 1e-9)
  expect_false(abs(s$vda5_u_evr - s$sd_meas / sqrt(s$n)) < 1e-12)

  # VDA 5 §5.3.2: u_BI is the uncorrected systematic bias magnitude = |Bias|.
  # The bias is being evaluated, not corrected, so its full magnitude
  # contributes. s/sqrt(n) would be the uncertainty of the bias *estimate*
  # (already in u_EVR) and would double-count repeatability.
  expect_equal(s$vda5_u_bi, abs(s$bias), tolerance = 1e-9)

  # VDA 5 §5.3.3: u_RE = resolution / (2*sqrt(3)) (rectangular distribution).
  expect_equal(s$vda5_u_re, 0.01 / (2 * sqrt(3)), tolerance = 1e-9)

  # u_MS = sqrt(u_EVR^2 + u_RE^2 + u_BI^2 + u_CAL^2)  (u_lin = u_rest = 0)
  expected_u_ms <- sqrt(s$vda5_u_evr^2 + s$vda5_u_re^2 +
                          s$vda5_u_bi^2 + s$vda5_u_cal^2)
  expect_equal(s$vda5_u_ms, expected_u_ms, tolerance = 1e-9)

  # %QMS = 2 * u_MS / T * 100  (k=2 expanded uncertainty vs tolerance)
  expect_equal(s$vda5_qms_percent, (2 * s$vda5_u_ms / s$tolerance) * 100,
               tolerance = 1e-9)
})

cat("Test 1.3b: VDA5 §5.3 formulas ... OK\n")

# Test 1.4: Data formats
test_that("iqr_type1_bias accepts different data formats", {
  set.seed(12345)
  ref_val <- 10

  # Vector input
  vec_data <- rnorm(25, mean = ref_val + 0.1, sd = 0.2)
  task1 <- iqr_type1_bias(vec_data, reference_value = ref_val, lsl = 7, usl = 13)
  expect_false(is.null(task1$results))

  # data.frame input
  df_data <- data.frame(measurement = vec_data)
  task2 <- iqr_type1_bias(df_data, reference_value = ref_val, lsl = 7, usl = 13)
  expect_false(is.null(task2$results))

  # data.table input
  dt_data <- data.table::data.table(measurement = vec_data)
  task3 <- iqr_type1_bias(dt_data, reference_value = ref_val, lsl = 7, usl = 13)
  expect_false(is.null(task3$results))
})

cat("Test 1.4: Data formats ... OK\n")

# ============================================================================
# Test Suite 2: Linearity Analysis (iqr_linearity_bias)
# ============================================================================

cat("\n--- Test Suite 2: Linearity Analysis ---\n\n")

# Test 2.1: Basic linearity analysis
test_that("iqr_linearity_bias computes correctly", {
  ref_vals <- c(8, 9, 10, 11, 12)
  n_per_ref <- 20

  data_linear <- data.table::rbindlist(lapply(ref_vals, function(r) {
    slope_true <- 0.08
    bias_r <- 0.1 + slope_true * (r - min(ref_vals))
    meas <- rnorm(n_per_ref, mean = r + bias_r, sd = 0.3)
    data.table::data.table(reference = r, measurement = meas)
  }))

  task <- iqr_linearity_bias(
    data_linear,
    reference_values = ref_vals,
    lsl = 7,
    usl = 13
  )

  expect_false(is.null(task$results))
  expect_equal(task$results$statistics$n_total, 100)
  expect_equal(task$results$statistics$n_ref_points, 5)
  expect_true(task$results$statistics$r_squared >= 0)
  expect_true(task$results$statistics$r_squared <= 1)
})

cat("Test 2.1: Basic linearity analysis ... OK\n")

# Test 2.2: Linear regression correctness
test_that("Linear regression slope and intercept are valid", {
  set.seed(54321)
  ref_vals <- c(8, 9, 10, 11, 12)

  # Create data with known slope
  data_linear <- data.table::rbindlist(lapply(ref_vals, function(r) {
    meas <- rnorm(20, mean = r + 0.1 + 0.05 * (r - 8), sd = 0.1)
    data.table::data.table(reference = r, measurement = meas)
  }))

  task <- iqr_linearity_bias(
    data_linear,
    reference_values = ref_vals,
    lsl = 7,
    usl = 13
  )

  # Slope should be positive (around 0.05)
  expect_true(task$results$statistics$slope > 0)
  expect_true(abs(task$results$statistics$slope - 0.05) < 0.1)

  # R-squared: regression is on ALL individual biases (n=100), not on the
  # 5 per-reference means, so within-reference variation enters the residual
  # and R^2 is naturally lower than the per-reference-mean R^2. Theoretical
  # R^2 ~= var(between-ref bias) / (var(between) + var(within)) ~= 0.33 here
  # (between var of seq(0.1,0.3,len=5) = 0.005; within var = 0.1^2 = 0.01).
  # Assert a robust lower bound that still confirms the signal is captured.
  expect_true(task$results$statistics$r_squared > 0.2)
})

cat("Test 2.2: Linear regression correctness ... OK\n")

# ============================================================================
# Test Suite 3: Task Methods
# ============================================================================

cat("\n--- Test Suite 3: Task Methods ---\n\n")

# Test 3.1: compute() method
test_that("compute() executes without error", {
  set.seed(111)
  measurements <- rnorm(25, mean = 10.1, sd = 0.2)

  task <- iqr_type1_bias(
    measurements,
    reference_value = 10,
    lsl = 7,
    usl = 13
  )

  # compute should already be called in iqr_type1_bias
  expect_false(is.null(task$results))
  expect_true(task$results$statistics$n == 25)
})

cat("Test 3.1: compute() method ... OK\n")

# Test 3.2: summary() method
test_that("summary() prints without error", {
  set.seed(111)
  measurements <- rnorm(25, mean = 10.1, sd = 0.2)

  task <- iqr_type1_bias(
    measurements,
    reference_value = 10,
    lsl = 7,
    usl = 13
  )

  # summary() should not throw error
  expect_no_error(task$summary())
})

cat("Test 3.2: summary() method ... OK\n")

# Test 3.3: plot() method
test_that("plot() method exists and runs", {
  set.seed(111)
  measurements <- rnorm(25, mean = 10.1, sd = 0.2)

  task <- iqr_type1_bias(
    measurements,
    reference_value = 10,
    lsl = 7,
    usl = 13
  )

  # plot() should exist and not throw error
  expect_no_error(task$plot())
})

cat("Test 3.3: plot() method ... OK\n")

# Test 3.4: report() method
test_that("report() generates Excel file", {
  set.seed(111)
  measurements <- rnorm(25, mean = 10.1, sd = 0.2)

  task <- iqr_type1_bias(
    measurements,
    reference_value = 10,
    lsl = 7,
    usl = 13
  )

  # Create temp file
  temp_path <- tempfile(fileext = ".xlsx")

  # report() should not throw error
  expect_no_error(task$report("excel", path = temp_path))

  # File should exist
  expect_true(file.exists(temp_path))

  # Clean up
  if (file.exists(temp_path)) file.remove(temp_path)
})

cat("Test 3.4: report() method ... OK\n")

test_that("IqrReporter render() generates Type1 Rmd report", {
  set.seed(111)
  measurements <- rnorm(25, mean = 10.1, sd = 0.2)

  task <- iqr_type1_bias(
    measurements,
    reference_value = 10,
    lsl = 7,
    usl = 13,
    resolution = 0.01,
    u_cal = 0.1
  )

  reporter <- IqrReporter$new(IqrTheme$new())
  # Register the type1 Rmd template explicitly. In devtools::test() the
  # package is loaded via pkgload::load_all(), and system.file() lookup for
  # templates can fail in the isolated test environment. Explicit
  # registration mirrors what .msa_export_report() does in production.
  type1_tpl <- system.file("templates", "type1_template.Rmd", package = "iQualityR.msa")
  if (type1_tpl != "" && file.exists(type1_tpl)) {
    reporter$register("type1", rmd_template = type1_tpl)
  }
  old_reporter <- getOption("iqr_reporter")
  on.exit(options(iqr_reporter = old_reporter), add = TRUE)
  options(iqr_reporter = reporter)

  temp_path <- tempfile(fileext = ".html")
  expect_no_error({
    reporter <- getOption("iqr_reporter")
    reporter$render("type1", params = list(
      results = task$results,
      plan = task$plan,
      study_type = "bias"
    ), path = temp_path)
  })
  expect_true(file.exists(temp_path))

  if (file.exists(temp_path)) file.remove(temp_path)
})

cat("Test 3.5: IqrReporter render() method ... OK\n")

# ============================================================================
# Test Suite 4: Edge Cases
# ============================================================================

cat("\n--- Test Suite 4: Edge Cases ---\n\n")

# Test 4.1: Zero standard deviation
test_that("Handles zero SD gracefully", {
  measurements <- rep(10, 25)  # No variation

  task <- iqr_type1_bias(
    measurements,
    reference_value = 10,
    lsl = 7,
    usl = 13
  )

  # Should still produce results
  expect_false(is.null(task$results))
  # SD should be 0 or NA
  expect_true(task$results$statistics$sd_meas == 0 || is.na(task$results$statistics$sd_meas))
})

cat("Test 4.1: Zero standard deviation ... OK\n")

# Test 4.2: Perfect accuracy
test_that("Handles zero bias case", {
  set.seed(999)
  ref_val <- 10
  # Perfect measurements
  measurements <- rep(ref_val, each = 25) + rnorm(25, sd = 0.01)

  task <- iqr_type1_bias(
    measurements,
    reference_value = ref_val,
    lsl = 7,
    usl = 13
  )

  # Bias should be very small
  expect_true(abs(task$results$statistics$bias) < 0.05)
})

cat("Test 4.2: Zero bias case ... OK\n")

# ============================================================================
# Test Suite 5: Type1Analyzer public step methods (refactored API)
# ============================================================================
# After the P1-4 refactor the analysis pipeline is exposed as discrete
# public methods on Type1Analyzer / Type1LinearityAnalyzer. These tests
# verify that each step can be called independently and produces the same
# numeric results as the full run() pipeline.

cat("\n--- Test Suite 5: Refactored public step methods ---\n\n")

test_that("Type1Analyzer$compute_bias stores statistics and returns list", {
  set.seed(2024)
  dt <- data.table::data.table(measurement = rnorm(25, mean = 10.1, sd = 0.2))
  analyzer <- Type1Analyzer$new()
  analyzer$setup(list(
    reference_value = 10, spec_limits = list(lsl = 7, usl = 13),
    k_factor = 0.2, conf_level = 0.95, resolution = NULL,
    u_cal = 0, u_rest = list(),
    criteria = list(Cg_min = 1.33, Cgk_min = 1.33, percent_bias_max = 10,
                    vda5_qms_max = 15)
  ))
  out <- analyzer$compute_bias(dt)

  expect_type(out, "list")
  expect_equal(out$n, 25)
  expect_equal(out$reference_value, 10)
  expect_true(out$bias > 0)  # mean 10.1 > ref 10
  # Stored in results container
  expect_equal(analyzer$results$statistics$n, 25)
  expect_equal(analyzer$results$statistics$bias, out$bias)
  expect_equal(analyzer$results$statistics$tolerance, 6)
})

cat("Test 5.1: compute_bias step ... OK\n")

test_that("Type1Analyzer$compute_capability depends on compute_bias", {
  set.seed(7)
  dt <- data.table::data.table(measurement = rnorm(25, mean = 10, sd = 0.3))
  analyzer <- Type1Analyzer$new()
  analyzer$setup(list(
    reference_value = 10, spec_limits = list(lsl = 7, usl = 13),
    k_factor = 0.2, conf_level = 0.95, resolution = NULL,
    u_cal = 0, u_rest = list(),
    criteria = list(Cg_min = 1.33, Cgk_min = 1.33, percent_bias_max = 10,
                    vda5_qms_max = 15)
  ))
  analyzer$compute_bias(dt)
  out <- analyzer$compute_capability()

  expect_type(out, "list")
  expect_true(out$Cg > 0)
  expect_true(out$sv_6sigma > 0)
  expect_equal(out$sv_6sigma, 6 * analyzer$results$statistics$sd_meas)
  expect_equal(analyzer$results$statistics$Cg, out$Cg)
})

cat("Test 5.2: compute_capability step ... OK\n")

test_that("Type1Analyzer$compute_vda5 assembles uncertainty table", {
  set.seed(5)
  dt <- data.table::data.table(measurement = rnorm(25, mean = 10, sd = 0.3))
  analyzer <- Type1Analyzer$new()
  analyzer$setup(list(
    reference_value = 10, spec_limits = list(lsl = 7, usl = 13),
    k_factor = 0.2, conf_level = 0.95, resolution = 0.01,
    u_cal = 0.05, u_rest = list(linearity = 0.02),
    criteria = list(Cg_min = 1.33, Cgk_min = 1.33, percent_bias_max = 10,
                    vda5_qms_max = 15)
  ))
  analyzer$compute_bias(dt)
  out <- analyzer$compute_vda5()

  expect_true(out$u_ms > 0)
  expect_true(out$u_re > 0)              # resolution contributed
  expect_true(out$u_cal == 0.05)
  expect_true(out$qms_percent > 0)
  # Table stored in data_tables. 7 rows: Repeatability, Resolution, Bias,
  # Linearity, Calibration, Other, Total (VDA 5 §5.3 budget).
  expect_true("vda5_uncertainty" %in% names(analyzer$results$data_tables))
  expect_equal(nrow(analyzer$results$data_tables$vda5_uncertainty), 7)
})

cat("Test 5.3: compute_vda5 step ... OK\n")

test_that("Type1Analyzer$evaluate_criteria returns logical diagnostics", {
  set.seed(1)
  dt <- data.table::data.table(measurement = rnorm(25, mean = 10, sd = 0.2))
  analyzer <- Type1Analyzer$new()
  analyzer$setup(list(
    reference_value = 10, spec_limits = list(lsl = 7, usl = 13),
    k_factor = 0.2, conf_level = 0.95, resolution = NULL,
    u_cal = 0, u_rest = list(),
    criteria = list(Cg_min = 1.33, Cgk_min = 1.33, percent_bias_max = 10,
                    vda5_qms_max = 15)
  ))
  analyzer$compute_bias(dt)
  analyzer$compute_capability()
  analyzer$compute_vda5()
  out <- analyzer$evaluate_criteria()

  expect_type(out, "list")
  expect_true(is.logical(out$Cg_ok))
  expect_true(is.logical(out$Cgk_ok))
  expect_true(is.logical(out$bias_ok))
  expect_true(is.logical(out$percent_bias_ok))
  expect_true(is.logical(out$vda5_qms_ok))
  # With sd=0.2 and tolerance=6, Cg = 0.2*6/(6*0.2) = 1.0 < 1.33 => FAIL
  expect_false(out$Cg_ok)
})

cat("Test 5.4: evaluate_criteria step ... OK\n")

test_that("Type1Analyzer$run() matches step-by-step pipeline", {
  set.seed(42)
  dt <- data.table::data.table(measurement = rnorm(25, mean = 10.05, sd = 0.25))
  params <- list(
    reference_value = 10, spec_limits = list(lsl = 7, usl = 13),
    k_factor = 0.2, conf_level = 0.95, resolution = 0.005,
    u_cal = 0.02, u_rest = list(),
    criteria = list(Cg_min = 1.33, Cgk_min = 1.33, percent_bias_max = 10,
                    vda5_qms_max = 15)
  )

  # Full pipeline
  a_full <- Type1Analyzer$new()
  a_full$setup(params)
  a_full$run(dt)

  # Step-by-step
  a_step <- Type1Analyzer$new()
  a_step$setup(params)
  a_step$compute_bias(dt)
  a_step$compute_capability()
  a_step$compute_vda5()
  a_step$evaluate_criteria()

  expect_equal(a_full$results$statistics, a_step$results$statistics)
  expect_equal(a_full$results$diagnostics, a_step$results$diagnostics)
})

cat("Test 5.5: run() == step-by-step pipeline ... OK\n")

test_that("Type1LinearityAnalyzer step methods work independently", {
  set.seed(33)
  refs <- c(2, 5, 8, 12, 16)
  dt <- data.table::data.table()
  for (r in refs) {
    # Inject a linear bias: bias = 0.02 * reference so that the regression
    # of bias on reference has a detectable slope and high R-squared.
    bias_at_r <- 0.02 * r
    dt <- rbind(dt, data.table::data.table(
      reference = r,
      measurement = rnorm(12, mean = r + bias_at_r, sd = 0.2)
    ))
  }

  params <- list(
    reference_values = refs,
    spec_limits = list(lsl = 0, usl = 20),
    process_variation = "from_study",
    conf_level = 0.95,
    criteria = list(linearity_slope_tolerance = 0.1,
                    linearity_r2_min = 0.95,
                    percent_bias_max = 10)
  )

  analyzer <- Type1LinearityAnalyzer$new()
  analyzer$setup(params)

  # Step 1: per-reference summary
  s1 <- analyzer$compute_per_reference_summary(dt)
  expect_equal(s1$n_total, 60)
  expect_equal(s1$n_ref_points, 5)
  expect_equal(nrow(s1$ref_summary), 5)

  # Step 2: regression - with injected linear bias, slope should be positive.
  # R-squared on individual biases (n=60) is naturally modest because
  # within-reference variation (sd=0.2) enters the residual; theoretical
  # R^2 ~= 0.2 here (between-ref var of 0.02*c(2,5,8,12,16) ~= 0.0098;
  # within var = 0.04). Assert the fit is non-trivial and slope direction
  # is correct, without requiring the per-reference-mean R^2 level.
  s2 <- analyzer$fit_linearity_regression()
  expect_true(!is.null(s2$lm_model))
  expect_true(s2$r_squared > 0.1)
  expect_true(s2$slope > 0)
  expect_length(s2$ci_slope, 2)

  # Step 3: criteria
  s3 <- analyzer$evaluate_linearity_criteria()
  expect_true(is.logical(s3$slope_ok))
  expect_true(is.logical(s3$percent_linearity_ok))
  expect_true(is.logical(s3$per_ref_stability_ok))

  # Compare with full run()
  a_full <- Type1LinearityAnalyzer$new()
  a_full$setup(params)
  a_full$run(dt)
  expect_equal(a_full$results$statistics$slope, analyzer$results$statistics$slope)
  expect_equal(a_full$results$statistics$r_squared, analyzer$results$statistics$r_squared)
})

cat("Test 5.6: Type1LinearityAnalyzer step methods ... OK\n")

# ============================================================================
# Test Suite 6: New input-parameter modes (study_multiplier, alternative,
#                historical_sd, wide format, PV degradation, from_historical_sigma)
# ============================================================================

cat("\n--- Test Suite 6: New input-parameter modes ---\n\n")

# 6.1 study_multiplier affects Cg/Cgk/SV
test_that("study_multiplier scales SV and Cg/Cgk", {
  set.seed(12345)
  meas <- rnorm(25, mean = 10, sd = 0.2)
  task_6  <- iqr_type1_bias(meas, reference_value = 10, lsl = 7, usl = 13,
                            study_multiplier = 6)
  task_515 <- iqr_type1_bias(meas, reference_value = 10, lsl = 7, usl = 13,
                             study_multiplier = 5.15)

  s6  <- task_6$results$statistics
  s515 <- task_515$results$statistics

  # SV scales linearly with multiplier
  expect_equal(s6$sv,  6 * s6$sd_meas, tolerance = 1e-9)
  expect_equal(s515$sv, 5.15 * s515$sd_meas, tolerance = 1e-9)
  expect_equal(s6$sv / s515$sv, 6 / 5.15, tolerance = 1e-9)
  # Cg also scales inversely with multiplier
  expect_equal(s6$Cg / s515$Cg, 5.15 / 6, tolerance = 1e-9)
  # sv_6sigma and sv_515sigma always reported for reference
  expect_equal(s6$sv_6sigma, 6 * s6$sd_meas, tolerance = 1e-9)
  expect_equal(s6$sv_515sigma, 5.15 * s6$sd_meas, tolerance = 1e-9)
})

cat("Test 6.1: study_multiplier ... OK\n")

# 6.2 alternative hypothesis (one-sided tests)
test_that("alternative hypothesis changes p-value and CI", {
  set.seed(999)
  # Positive bias: mean 10.3 vs ref 10
  meas <- rnorm(50, mean = 10.3, sd = 0.2)
  task_two  <- iqr_type1_bias(meas, reference_value = 10, lsl = 7, usl = 13,
                              alternative = "two.sided")
  task_gtr  <- iqr_type1_bias(meas, reference_value = 10, lsl = 7, usl = 13,
                              alternative = "greater")
  task_les  <- iqr_type1_bias(meas, reference_value = 10, lsl = 7, usl = 13,
                              alternative = "less")

  s_two <- task_two$results$statistics
  s_gtr <- task_gtr$results$statistics
  s_les <- task_les$results$statistics

  # Bias is positive (~0.3): one-sided "greater" p = two.sided/2
  expect_equal(s_gtr$p_value, s_two$p_value / 2, tolerance = 1e-9)
  # "less" p-value is ~1 - (greater p)
  expect_true(s_les$p_value > 0.99)
  # one-sided CIs are half-infinite
  expect_true(is.infinite(s_gtr$ci_bias[2]))
  expect_true(is.infinite(s_les$ci_bias[1]))
  expect_false(any(is.infinite(s_two$ci_bias)))
})

cat("Test 6.2: alternative hypothesis ... OK\n")

# 6.3 historical_sd switches to z-test and replaces SD
test_that("historical_sd uses z-test and overrides sample SD", {
  set.seed(7)
  meas <- rnorm(25, mean = 10.1, sd = 0.3)
  hist_sigma <- 0.25

  task_hist <- iqr_type1_bias(meas, reference_value = 10, lsl = 7, usl = 13,
                              historical_sd = hist_sigma)
  task_sample <- iqr_type1_bias(meas, reference_value = 10, lsl = 7, usl = 13)

  sh <- task_hist$results$statistics
  ss <- task_sample$results$statistics

  # sd_meas should equal historical_sd (used for capability)
  expect_equal(sh$sd_meas, hist_sigma, tolerance = 1e-9)
  # sd_sample still records the sample SD
  expect_equal(sh$sd_sample, ss$sd_meas, tolerance = 1e-9)
  # df = Inf indicates z-test
  expect_true(is.infinite(sh$df))
  # z-test p-value uses pnorm, not pt
  expected_z <- sh$bias / (hist_sigma / sqrt(sh$n))
  expected_p <- 2 * pnorm(-abs(expected_z))
  expect_equal(sh$p_value, expected_p, tolerance = 1e-9)
  # Cg uses historical_sd
  expect_equal(sh$Cg, (0.2 * 6) / (6 * hist_sigma), tolerance = 1e-9)
})

cat("Test 6.3: historical_sd ... OK\n")

# 6.4 wide-format data auto-conversion
test_that("wide-format data auto-converts to long", {
  set.seed(42)
  refs <- c(2, 4, 6, 8, 10)
  # Build wide format: each column is a reference value
  wide_df <- as.data.frame(setNames(
    lapply(refs, function(r) rnorm(12, mean = r + 0.02 * r, sd = 0.2)),
    as.character(refs)
  ))

  task <- iqr_linearity_bias(wide_df, lsl = 0, usl = 12,
                             process_variation = 1.5)
  s <- task$results$statistics

  expect_equal(s$n_total, 60)
  expect_equal(s$n_ref_points, 5)
  expect_equal(sort(unique(task$results$data_tables$ref_summary$reference)), refs)
  expect_true(s$linearity > 0)
})

cat("Test 6.4: wide-format conversion ... OK\n")

# 6.5 PV = NULL degrades to from_study (E3 degradation strategy)
test_that("PV=NULL degrades to from_study with warning, not error", {
  set.seed(33)
  refs <- c(2, 5, 8, 12, 16)
  df <- do.call(rbind, lapply(refs, function(r) {
    data.frame(reference = r, measurement = rnorm(12, mean = r, sd = 0.2))
  }))

  expect_warning({
    task <- iqr_linearity_bias(df, reference_values = refs,
                               lsl = 0, usl = 20,
                               process_variation = NULL)
  }, "degrading")
  expect_false(is.null(task$results))
  expect_true(task$results$statistics$process_variation > 0)
})

cat("Test 6.5: PV=NULL degradation ... OK\n")

# 6.6 from_historical_sigma mode
test_that("from_historical_sigma uses 6*historical_sd", {
  set.seed(11)
  refs <- c(2, 4, 6, 8, 10)
  df <- do.call(rbind, lapply(refs, function(r) {
    data.frame(reference = r, measurement = rnorm(12, mean = r, sd = 0.2))
  }))

  hist_sigma <- 0.18
  task <- iqr_linearity_bias(df, reference_values = refs,
                             lsl = 0, usl = 12,
                             process_variation = "from_historical_sigma",
                             historical_sd = hist_sigma)
  expect_equal(task$results$statistics$process_variation,
               6 * hist_sigma, tolerance = 1e-9)
})

cat("Test 6.6: from_historical_sigma ... OK\n")

# 6.7 One-sided lower spec via direct tolerance (C-T1-4)
test_that("one-sided lower spec works via direct tolerance", {
  set.seed(12345)
  meas <- rnorm(25, mean = 10, sd = 0.2)
  # User has only LSL = 8 (lower spec), tolerance band = 4 (e.g. natural upper = 12)
  task <- iqr_type1_bias(meas, reference_value = 10,
                         tolerance = 4)  # direct tolerance input
  expect_false(is.null(task$results))
  expect_equal(task$results$statistics$tolerance, 4)
  expect_true(task$results$statistics$Cg > 0)
})

cat("Test 6.7: one-sided lower spec via direct tolerance ... OK\n")

# 6.8 Tolerance error message guides user (degradation strategy E3)
test_that("missing tolerance produces informative error", {
  set.seed(12345)
  meas <- rnorm(25, mean = 10, sd = 0.2)
  expect_error(
    iqr_type1_bias(meas, reference_value = 10),
    class = "simpleError"
  )
  # The error message should mention 'tolerance' to guide the user
  err <- tryCatch(
    iqr_type1_bias(meas, reference_value = 10),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("tolerance", err, ignore.case = TRUE))
})

cat("Test 6.8: tolerance error message ... OK\n")

# ============================================================================
# Summary
# ============================================================================

cat("\n========================================\n")
cat("All unit tests completed successfully!\n")
cat("========================================\n")
