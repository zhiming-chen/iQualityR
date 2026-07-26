# =============================================================================
# File: tests/testthat/test-spc.R
# Description: Unit tests for iQualityR.spc control charts
# =============================================================================

# ---------------------------------------------------------------------------
# Variables charts: Xbar-R, Xbar-S, I-MR, I-MR-R/S
# ---------------------------------------------------------------------------

test_that("run_spc_xbar_r returns IqrSpcTask with correct structure", {
  set.seed(123)
  df <- data.frame(
    measurement = rnorm(50, mean = 100, sd = 2),
    subgroup = rep(1:10, each = 5)
  )
  task <- run_spc_xbar_r(df, "measurement",
                         subgroup = "subgroup", subgroup_size = 5)
  expect_s3_class(task, "IqrSpcTask")
  expect_true(!is.null(task$results))
  expect_equal(task$results$statistics$chart_type, "xbar_r")
  expect_named(task$results$statistics,
    c("center", "sigma", "sigma_method", "ucl", "lcl",
      "n_points", "n_subgroups", "n_violations", "is_in_control",
      "chart_type", "r_bar", "ucl_r", "lcl_r"))
  expect_true(task$results$statistics$sigma > 0)
  expect_true(task$results$statistics$ucl > task$results$statistics$center)
})

test_that("run_spc_xbar_s works with subgroup column", {
  set.seed(42)
  df <- data.frame(
    measurement = rnorm(60, mean = 50, sd = 1.5),
    subgroup = rep(1:10, each = 6)
  )
  task <- run_spc_xbar_s(df, "measurement",
                         subgroup = "subgroup", subgroup_size = 6)
  expect_equal(task$results$statistics$chart_type, "xbar_s")
  expect_true(task$results$statistics$sigma_method == "s_bar")
  expect_true(task$results$statistics$n_subgroups == 10)
})

test_that("run_spc_imr computes correct center and sigma", {
  set.seed(2026)
  df <- data.frame(measurement = rnorm(30, mean = 100, sd = 1))
  task <- run_spc_imr(df, "measurement")
  expect_equal(task$results$statistics$chart_type, "imr")
  expect_equal(task$results$statistics$sigma_method, "mr_bar")
  # Center should be near 100
  expect_lt(abs(task$results$statistics$center - 100), 1)
  # Sigma should be near 1
  expect_lt(abs(task$results$statistics$sigma - 1), 0.5)
  # MR-bar limit should be positive
  expect_true(task$results$statistics$mr_bar > 0)
})

test_that("run_spc_imr_rs handles grouped data", {
  set.seed(7)
  df <- data.frame(
    measurement = rnorm(40, mean = 50, sd = 1.2),
    subgroup = rep(1:8, each = 5)
  )
  task <- run_spc_imr_rs(df, "measurement",
                         subgroup = "subgroup", subgroup_size = 5)
  expect_equal(task$results$statistics$chart_type, "imr_rs")
})

test_that("run_spc_xbar_r detects injected out-of-control points", {
  set.seed(1)
  df <- data.frame(
    measurement = c(rnorm(40, 100, 1), rep(110, 5), rnorm(5, 100, 1)),
    subgroup = rep(1:10, each = 5)
  )
  task <- run_spc_xbar_r(df, "measurement",
                         subgroup = "subgroup", subgroup_size = 5)
  # The injected subgroup (all 110) should trigger at least Rule 1
  expect_gt(task$results$statistics$n_violations, 0)
})

test_that("run_spc_imr generates a valid ggplot", {
  set.seed(123)
  df <- data.frame(measurement = rnorm(30, 100, 1))
  task <- run_spc_imr(df, "measurement")
  p <- task$plot(type = "full")
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# Attributes charts: P, NP, U, C, Laney P', Laney U'
# ---------------------------------------------------------------------------

test_that("run_spc_p computes proportion chart correctly", {
  set.seed(123)
  df <- data.frame(
    defectives = rbinom(20, 100, 0.05),
    sample_size = rep(100, 20)
  )
  task <- run_spc_p(df, "defectives", "sample_size")
  expect_equal(task$results$statistics$chart_type, "p")
  expect_true(task$results$statistics$p_bar > 0)
  expect_true(task$results$statistics$p_bar < 1)
  expect_true(task$results$statistics$ucl > task$results$statistics$lcl)
})

test_that("run_spc_np works for equal sample sizes", {
  set.seed(456)
  df <- data.frame(
    defectives = rbinom(15, 50, 0.04),
    sample_size = rep(50, 15)
  )
  task <- run_spc_np(df, "defectives", "sample_size")
  expect_equal(task$results$statistics$chart_type, "np")
})

test_that("run_spc_u handles varying sample sizes", {
  set.seed(789)
  df <- data.frame(
    defects = rpois(20, lambda = 4),
    sample_size = rep(c(1, 2, 3, 4), 5)
  )
  task <- run_spc_u(df, "defects", "sample_size")
  expect_equal(task$results$statistics$chart_type, "u")
  expect_true(task$results$statistics$u_bar > 0)
})

test_that("run_spc_c works without sample_size", {
  set.seed(101)
  df <- data.frame(defects = rpois(20, lambda = 5))
  task <- run_spc_c(df, "defects")
  expect_equal(task$results$statistics$chart_type, "c")
  expect_true(task$results$statistics$c_bar > 0)
})

test_that("run_spc_p_laney adjusts for over-dispersion", {
  set.seed(2026)
  df <- data.frame(
    defectives = rbinom(25, 500, 0.05),
    sample_size = rep(500, 25)
  )
  task <- run_spc_p_laney(df, "defectives", "sample_size")
  expect_equal(task$results$statistics$chart_type, "p_laney")
  expect_true(!is.null(task$results$statistics$sigma_z))
})

test_that("run_spc_u_laney adjusts for over-dispersion", {
  set.seed(2026)
  df <- data.frame(
    defects = rpois(25, lambda = 30),
    sample_size = rep(100, 25)
  )
  task <- run_spc_u_laney(df, "defects", "sample_size")
  expect_equal(task$results$statistics$chart_type, "u_laney")
  expect_true(!is.null(task$results$statistics$sigma_z))
})

# ---------------------------------------------------------------------------
# Time-weighted charts: EWMA, CUSUM, MA
# ---------------------------------------------------------------------------

test_that("run_spc_ewma smooths data and detects shifts", {
  set.seed(123)
  df <- data.frame(
    measurement = c(rnorm(20, 100, 1), rnorm(10, 102, 1))
  )
  task <- run_spc_ewma(df, "measurement", lambda = 0.2)
  expect_equal(task$results$statistics$chart_type, "ewma")
  expect_equal(task$results$statistics$lambda, 0.2)
  # EWMA points should be in points_df
  expect_equal(nrow(task$results$data_tables$points), 30)
})

test_that("run_spc_cusum computes positive and negative statistics", {
  set.seed(123)
  df <- data.frame(
    measurement = c(rnorm(20, 50, 1), rnorm(10, 51.5, 1))
  )
  task <- run_spc_cusum(df, "measurement", k = 0.5, h = 4.77)
  expect_equal(task$results$statistics$chart_type, "cusum")
  expect_equal(task$results$statistics$k, 0.5)
  expect_equal(task$results$statistics$h, 4.77)
  # CUSUM positive column should exist
  expect_true("cusum_neg" %in% names(task$results$data_tables$points))
})

test_that("run_spc_ma computes moving average", {
  set.seed(999)
  df <- data.frame(measurement = rnorm(30, 100, 1))
  task <- run_spc_ma(df, "measurement", ma_window = 3)
  expect_equal(task$results$statistics$chart_type, "ma")
  expect_equal(task$results$statistics$ma_window, 3L)
  # First ma_window - 1 points should be NA
  expect_true(is.na(task$results$data_tables$points$value[1]))
})

# ---------------------------------------------------------------------------
# Multivariate charts: T2, MEWMA
# ---------------------------------------------------------------------------

test_that("run_spc_t2 detects multivariate out-of-control", {
  set.seed(123)
  df <- data.frame(
    x1 = c(rnorm(25, 50, 1), rnorm(5, 53, 1)),
    x2 = c(rnorm(25, 30, 0.8), rnorm(5, 32.5, 0.8))
  )
  task <- run_spc_t2(df)
  expect_equal(task$results$statistics$chart_type, "t2")
  expect_equal(task$results$statistics$n_variables, 2)
  expect_true(task$results$statistics$ucl > 0)
})

test_that("run_spc_mewma computes MEWMA statistic", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(30, 50, 1),
    x2 = rnorm(30, 30, 0.8)
  )
  task <- run_spc_mewma(df, lambda = 0.2)
  expect_equal(task$results$statistics$chart_type, "mewma")
  expect_true(task$results$statistics$ucl > 0)
})

# ---------------------------------------------------------------------------
# Rare-event charts: G, T
# ---------------------------------------------------------------------------

test_that("run_spc_g handles geometric data", {
  set.seed(123)
  df <- data.frame(opportunities = rgeom(30, prob = 0.05) + 1)
  task <- run_spc_g(df, "opportunities")
  expect_equal(task$results$statistics$chart_type, "g")
  expect_true(task$results$statistics$center > 0)
})

test_that("run_spc_t handles exponential time-between-events", {
  set.seed(2026)
  df <- data.frame(time_between = rexp(30, rate = 0.1))
  task <- run_spc_t(df, "time_between")
  expect_equal(task$results$statistics$chart_type, "t")
  expect_true(task$results$statistics$ucl > task$results$statistics$center)
})

# ---------------------------------------------------------------------------
# Phase analysis
# ---------------------------------------------------------------------------

test_that("run_spc_phase splits data into phases correctly", {
  set.seed(123)
  df <- data.frame(
    measurement = c(rnorm(20, 100, 1), rnorm(15, 102, 1))
  )
  phases <- run_spc_phase(df, "measurement", "imr",
                           phase_boundaries = c(21))
  expect_s3_class(phases, "spc_phase_list")
  expect_equal(length(phases), 2)
  expect_equal(phases[[1]]$results$statistics$n_points, 20)
  expect_equal(phases[[2]]$results$statistics$n_points, 15)
  # Phase 2 center should be higher (102 vs 100)
  expect_gt(phases[[2]]$results$statistics$center,
            phases[[1]]$results$statistics$center)
})

# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------

test_that("run_spc_imr rejects non-numeric measurement", {
  df <- data.frame(measurement = letters[1:10])
  expect_error(run_spc_imr(df, "measurement"), "must be numeric")
})

test_that("run_spc_xbar_r rejects missing column", {
  df <- data.frame(value = rnorm(10))
  expect_error(run_spc_xbar_r(df, "missing_col"), "not found in data")
})

test_that("run_spc_p requires sample_size", {
  df <- data.frame(defectives = rbinom(10, 100, 0.05))
  expect_error(run_spc_p(df, "defectives", NULL), "sample_size is required")
})

test_that("SpcPlan rejects invalid chart_type", {
  expect_error(SpcPlan$new(chart_type = "bogus"), "should be one of")
})

test_that("SpcPlan validates sigma_method via validate()", {
  plan <- SpcPlan$new(chart_type = "imr", sigma_method = "mr_bar")
  expect_error(plan$validate(), NA)
})

test_that("SpcPlan rejects invalid lambda", {
  expect_error(SpcPlan$new(chart_type = "ewma", lambda = 0))
  expect_error(SpcPlan$new(chart_type = "ewma", lambda = 1.5))
})

# ---------------------------------------------------------------------------
# Serialization (run_*() contract: results must be JSON-serializable)
# ---------------------------------------------------------------------------

test_that("run_spc_imr results are JSON-serializable", {
  skip_if_not_installed("jsonlite")
  set.seed(123)
  df <- data.frame(measurement = rnorm(20, 100, 1))
  task <- run_spc_imr(df, "measurement")
  # Drop raw_output if present (may contain non-serializable R6 objects)
  res <- task$results
  res$raw_output <- NULL
  json_str <- jsonlite::toJSON(res, auto_unbox = TRUE, null = "null")
  expect_true(nchar(as.character(json_str)) > 100)
})

# ---------------------------------------------------------------------------
# v0.2: Adaptive / statistical enhancement charts
# ---------------------------------------------------------------------------

test_that("run_spc_adaptive computes rolling-window limits", {
  set.seed(123)
  df <- data.frame(measurement = c(rnorm(30, 100, 1),
                                   rnorm(20, 102, 1)))
  task <- run_spc_adaptive(df, "measurement", window_size = 15)
  expect_equal(task$results$statistics$chart_type, "adaptive")
  expect_true(task$results$statistics$window_size == 15L)
  expect_true(task$results$statistics$sigma > 0)
  # Each point has its own center/sigma
  pts <- task$results$data_tables$points
  expect_equal(nrow(pts), 50)
  expect_true(all(c("index", "value", "cl", "ucl", "lcl") %in% names(pts)))
})

test_that("run_spc_aewma adapts lambda to forecast errors", {
  set.seed(123)
  df <- data.frame(measurement = c(rnorm(30, 100, 1),
                                   rnorm(15, 103, 1)))
  task <- run_spc_aewma(df, "measurement",
                        aewma_lambda = 0.2, aewma_k = 1.5)
  expect_equal(task$results$statistics$chart_type, "aewma")
  expect_equal(task$results$statistics$aewma_lambda, 0.2)
  expect_equal(task$results$statistics$aewma_k, 1.5)
  # The lambda vector should vary (not constant)
  pts <- task$results$data_tables$points
  expect_true("lambda" %in% names(pts))
  expect_gt(diff(range(pts$lambda, na.rm = TRUE)), 0)
})

test_that("run_spc_arima_resid fits ARIMA and monitors residuals", {
  set.seed(123)
  ar1 <- stats::arima.sim(list(ar = 0.6), n = 60, sd = 1) + 100
  df <- data.frame(measurement = as.numeric(ar1))
  task <- run_spc_arima_resid(df, "measurement", arima_order = c(1, 0, 0))
  expect_equal(task$results$statistics$chart_type, "arima_resid")
  expect_true(!is.null(task$results$diagnostics$arima_aic))
  expect_true(!is.null(task$results$diagnostics$arima_sigma2))
  # Residuals should have mean near 0
  expect_lt(abs(task$results$statistics$center), 1)
})

test_that("run_spc_changepoint detects injected mean shift", {
  set.seed(123)
  df <- data.frame(measurement = c(rnorm(30, 100, 1),
                                   rnorm(30, 105, 1)))
  task <- run_spc_changepoint(df, "measurement")
  expect_equal(task$results$statistics$chart_type, "changepoint")
  expect_gte(task$results$statistics$n_change_points, 1)
  # Change-point data table
  cp_df <- task$results$data_tables$change_points
  expect_true("mean_before" %in% names(cp_df))
  expect_true("mean_after" %in% names(cp_df))
  expect_true("mean_diff" %in% names(cp_df))
})

test_that("run_spc_kde produces nonparametric limits", {
  set.seed(123)
  df <- data.frame(measurement = rexp(50, rate = 0.1))
  task <- run_spc_kde(df, "measurement")
  expect_equal(task$results$statistics$chart_type, "kde")
  expect_true(!is.null(task$results$statistics$bandwidth))
  # LCL should be >= 0 for exponential data
  expect_gte(task$results$statistics$lcl, 0)
  # Density data table
  dens <- task$results$data_tables$density
  expect_true(all(c("x", "y") %in% names(dens)))
  expect_equal(nrow(dens), 512)
})

test_that("run_spc_t2_mewma computes combined multivariate statistic", {
  set.seed(123)
  df <- data.frame(
    x1 = c(rnorm(25, 50, 1), rnorm(10, 53, 1)),
    x2 = c(rnorm(25, 30, 0.8), rnorm(10, 32.5, 0.8))
  )
  task <- run_spc_t2_mewma(df, lambda = 0.2)
  expect_equal(task$results$statistics$chart_type, "t2_mewma")
  expect_equal(task$results$statistics$center, 1)
  expect_equal(task$results$statistics$ucl, 1)
  # Points contain both T2 and MEWMA columns
  pts <- task$results$data_tables$points
  expect_true(all(c("t2", "mewma", "ucl_t2", "ucl_mewma") %in% names(pts)))
})

test_that("SpcPlan rejects invalid v0.2 parameters", {
  expect_error(SpcPlan$new(chart_type = "adaptive", window_size = 2),
               "window_size")
  expect_error(SpcPlan$new(chart_type = "arima_resid",
                           arima_order = c(-1, 0, 0)), "arima_order")
  expect_error(SpcPlan$new(chart_type = "aewma", aewma_lambda = 0),
               "aewma_lambda")
  expect_error(SpcPlan$new(chart_type = "kde", kde_bandwidth = -1),
               "kde_bandwidth")
})

test_that("SpcPlan accepts all v0.2 chart types", {
  for (ct in c("adaptive", "arima_resid", "aewma",
               "changepoint", "kde", "t2_mewma")) {
    expect_error(plan <- SpcPlan$new(chart_type = ct), NA)
    expect_equal(plan$chart_type, ct)
  }
})
