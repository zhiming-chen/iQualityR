# =============================================================================
# File: tests/testthat/test-quality-metrics.R
# Description: Quality metrics module tests (quality_metrics.R)
# =============================================================================

library(testthat)
library(iQualityR.stat)

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

# ----------------------------------------------------------------------------
# Conversion roundtrips and known values
# ----------------------------------------------------------------------------

test_that("ppm_to_sigma / sigma_to_ppm roundtrip", {
  for (ppm in c(3.4, 66807, 2700, 100)) {
    sigma <- ppm_to_sigma(ppm)
    ppm_back <- sigma_to_ppm(sigma)
    expect_equal(ppm_back, ppm, tolerance = 1)
  }
})

test_that("sigma_to_ppm known six-sigma value", {
  # 6 sigma with 1.5 shift => ~3.4 PPM
  expect_equal(round(sigma_to_ppm(6)), 3)
  # 3 sigma with 1.5 shift => ~66807 PPM
  expect_lt(abs(sigma_to_ppm(3) - 66807), 100)
})

test_that("ppm_to_sigma known values", {
  expect_equal(round(ppm_to_sigma(3.4), 1), 6.0)
  expect_equal(round(ppm_to_sigma(3.4, shift = 0), 1), 4.5)
})

test_that("yield_to_dpmo / dpmo_to_yield roundtrip", {
  for (y in c(0.95, 0.99, 0.999)) {
    dpmo <- yield_to_dpmo(y)
    y_back <- dpmo_to_yield(dpmo)
    expect_equal(y_back, y, tolerance = 1e-10)
  }
})

test_that("yield_to_dpmo accepts percentage input", {
  expect_equal(yield_to_dpmo(95), yield_to_dpmo(0.95))
  expect_equal(yield_to_dpmo(99, opportunities = 10), yield_to_dpmo(0.99, opportunities = 10))
})

test_that("capability_to_ppm known Cpk values", {
  # Cpk = 1.0 centered => ~2700 PPM (within)
  r <- capability_to_ppm(cpk = 1.0, usl = 10, lsl = 0)
  expect_lt(abs(r$within$total - 2700), 50)
  # Cpk = 1.33 centered => ~63 PPM
  r <- capability_to_ppm(cpk = 1.33, usl = 10, lsl = 0)
  expect_lt(abs(r$within$total - 63), 10)
  # Cpk = 1.67 centered => ~0.57 PPM
  r <- capability_to_ppm(cpk = 1.67, usl = 10, lsl = 0)
  expect_lt(r$within$total, 1)
})

test_that("capability_to_ppm within <= overall when ppk < cpk", {
  r <- capability_to_ppm(cpk = 1.67, ppk = 1.33, usl = 10, lsl = 0)
  expect_gt(r$overall$total, r$within$total)
})

test_that("capability_interpret levels", {
  expect_equal(capability_interpret(2.0)$level, "Excellent")
  expect_equal(capability_interpret(1.5)$level, "Good")
  expect_equal(capability_interpret(1.2)$level, "Acceptable")
  expect_equal(capability_interpret(0.8)$level, "Marginal")
  expect_equal(capability_interpret(0.5)$level, "Unacceptable")
})

test_that("capability_interpret custom thresholds", {
  r <- capability_interpret(1.5, standard = "custom",
                            custom_thresholds = list(excellent = 2.0, good = 1.5, acceptable = 1.0, marginal = 0.5))
  expect_equal(r$level, "Good")
})

test_that("benchmark_compare lower-is-better metrics", {
  # PPM: lower is better. aerospace world_class = 1
  r <- benchmark_compare("ppm", 0.5, "aerospace")  # 0.5 <= 1 -> World Class
  expect_equal(r$rating, "World Class")
  r <- benchmark_compare("ppm", 50000, "aerospace")  # above minimum = 100
  expect_equal(r$rating, "Below Minimum")
})

test_that("benchmark_compare higher-is-better metrics", {
  # Cpk: higher is better
  r <- benchmark_compare("cpk", 2.5, "manufacturing")  # world_class = 2.0
  expect_equal(r$rating, "World Class")
  r <- benchmark_compare("cpk", 0.5, "manufacturing")  # minimum = 1.0
  expect_equal(r$rating, "Below Minimum")
})

test_that("benchmark_compare errors on unknown metric/industry", {
  expect_error(benchmark_compare("unknown", 1, "manufacturing"))
  expect_error(benchmark_compare("cpk", 1.0, "software"))  # no cpk benchmark for software
})

test_that("reliability weibull distribution", {
  r <- reliability(t = 100, shape = 2, scale = 1000, dist = "weibull")
  expect_gt(r, 0)
  expect_lt(r, 1)
  # R(t) = exp(-(t/scale)^shape) = exp(-(100/1000)^2) = exp(-0.01)
  expect_equal(r, exp(-0.01), tolerance = 1e-10)
})

test_that("throughput_yield dpu and yield give same result", {
  dpu_vals <- c(0.01, 0.02, 0.015)
  ytp_dpu <- throughput_yield(dpu = dpu_vals)
  ytp_yield <- throughput_yield(yield = exp(-dpu_vals))
  expect_equal(ytp_dpu, ytp_yield, tolerance = 1e-10)
})

test_that("throughput_yield errors when both or neither provided", {
  expect_error(throughput_yield(dpu = 0.1, yield = 0.9))
  expect_error(throughput_yield())
})

test_that("quality_dashboard ppm_from_cpk field name (typo fix)", {
  d <- quality_dashboard(cpk = 1.33)
  expect_true("ppm_from_cpk" %in% names(d))
  expect_false("cpm_from_cpk" %in% names(d))
})

test_that("quality_dashboard normalizes percentage yield/availability", {
  # yield_val = 95 (percentage) should be treated as 0.95
  d_pct <- quality_dashboard(yield_val = 95)
  d_dec <- quality_dashboard(yield_val = 0.95)
  expect_equal(d_pct$yield$dpmo, d_dec$yield$dpmo)
  # availability = 98 (percentage) should be treated as 0.98
  d_a_pct <- quality_dashboard(availability = 98, industry = "manufacturing")
  d_a_dec <- quality_dashboard(availability = 0.98, industry = "manufacturing")
  expect_equal(d_a_pct$availability$value, d_a_dec$availability$value)
})

# ----------------------------------------------------------------------------
# Reliability edge cases (Weibull + exponential)
# ----------------------------------------------------------------------------

test_that("reliability weibull at t=0 returns 1", {
  # R(0) = exp(-(0/scale)^shape) = exp(0) = 1
  expect_equal(reliability(t = 0, shape = 2, scale = 1000, dist = "weibull"), 1)
})

test_that("reliability weibull shape=1 equals exponential", {
  # Weibull with shape=1 reduces to exponential with lambda = 1/scale
  set.seed(42)
  t_vals <- runif(10, 1, 500)
  r_weibull <- reliability(t = t_vals, shape = 1, scale = 1000, dist = "weibull")
  r_exp <- reliability(t = t_vals, lambda = 1/1000, dist = "exp")
  expect_equal(r_weibull, r_exp, tolerance = 1e-10)
})

test_that("reliability weibull monotone decreasing in t", {
  t_seq <- seq(10, 1000, by = 50)
  r_seq <- reliability(t = t_seq, shape = 2, scale = 500, dist = "weibull")
  expect_true(all(diff(r_seq) < 0))  # strictly decreasing
})

test_that("reliability weibull errors when shape/scale missing", {
  expect_error(reliability(t = 100, shape = 2, dist = "weibull"))
  expect_error(reliability(t = 100, scale = 1000, dist = "weibull"))
})

test_that("reliability exponential errors without lambda or mtbf", {
  expect_error(reliability(t = 100, dist = "exp"))
})

test_that("reliability exponential via mtbf matches lambda", {
  # lambda = 1/mtbf, so results should match
  r_mtbf <- reliability(t = 100, mtbf = 1000)
  r_lambda <- reliability(t = 100, lambda = 0.001)
  expect_equal(r_mtbf, r_lambda, tolerance = 1e-10)
})

test_that("availability edge cases", {
  # mtbf=0, mttr=0 -> defined as 1 (avoid 0/0)
  expect_equal(availability(mtbf = 0, mttr = 0), 1)
  # mtbf=0, mttr>0 -> 0 (system always down)
  expect_equal(availability(mtbf = 0, mttr = 10), 0)
  # mttr=0 -> 1 (instant repair)
  expect_equal(availability(mtbf = 1000, mttr = 0), 1)
  # negative inputs error
  expect_error(availability(mtbf = -1, mttr = 10))
  expect_error(availability(mtbf = 100, mttr = -1))
})
