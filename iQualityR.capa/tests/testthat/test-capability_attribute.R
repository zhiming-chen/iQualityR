# =============================================================================
# File: tests/testthat/test-capability_attribute.R
# Description: Unit tests for attribute (Binomial / Poisson) capability
# =============================================================================

test_that("capability_binomial estimates p-bar and PPM correctly", {
  set.seed(42)
  df <- data.frame(
    batch = 1:30,
    defectives = rbinom(30, size = 100, prob = 0.02),
    inspected  = rep(100, 30)
  )
  task <- capability_binomial(df, defects = "defectives",
                             sample_sizes = "inspected")
  expect_s3_class(task, "IqrAttributeCapabilityTask")

  s <- task$results$statistics
  expect_equal(s$distribution, "binomial")
  expect_equal(s$rate, sum(df$defectives) / sum(df$inspected),
               tolerance = 1e-12)
  expect_equal(s$ppm_expected, s$rate * 1e6, tolerance = 1e-6)
  expect_equal(s$yield, 1 - s$rate, tolerance = 1e-12)
  expect_true(s$z_bench > 0)
  expect_true(s$sigma_level > s$z_bench)  # + shift

  # CI sanity: lower <= estimate <= upper
  expect_true(s$rate_lower <= s$rate && s$rate <= s$rate_upper)

  # Z.Bench (short-term, shift=0) = qnorm(1 - p_bar)
  expect_equal(s$z_bench, stats::qnorm(1 - s$rate), tolerance = 1e-6)
  expect_equal(s$sigma_level, s$z_bench + s$z_shift, tolerance = 1e-9)

  # Data tables present
  expect_true("points" %in% names(task$results$data_tables))
  expect_true("ppm_summary" %in% names(task$results$data_tables))
  expect_equal(nrow(task$results$data_tables$points), 30)
  expect_equal(ncol(task$results$data_tables$points), 8)

  # Dispersion test was computed
  disp <- task$results$diagnostics$dispersion_test
  expect_true(!is.null(disp$p_value))
  expect_true(disp$p_value >= 0 && disp$p_value <= 1)
})

test_that("capability_binomial rejects counts > sample_sizes", {
  df <- data.frame(d = c(5, 10), n = c(10, 5))  # row 2 has count > n
  expect_error(capability_binomial(df, "d", "n"),
              "counts must not exceed sample_sizes")
})

test_that("capability_binomial supports Clopper-Pearson CI", {
  set.seed(1)
  df <- data.frame(
    defects = c(rep(0, 20), rep(1, 5)),
    n = rep(100, 25)
  )
  task_w <- capability_binomial(df, "defects", "n", ci_method = "wilson")
  task_c <- capability_binomial(df, "defects", "n",
                                 ci_method = "clopper_pearson")
  expect_equal(task_w$results$statistics$ci_method, "wilson")
  expect_equal(task_c$results$statistics$ci_method, "clopper_pearson")
  # Clopper-Pearson is more conservative -> wider CI
  width_w <- task_w$results$statistics$rate_upper -
             task_w$results$statistics$rate_lower
  width_c <- task_c$results$statistics$rate_upper -
             task_c$results$statistics$rate_lower
  expect_true(width_c >= width_w - 1e-6)
})

test_that("capability_binomial respects target_proportion", {
  set.seed(7)
  df <- data.frame(
    defects = rbinom(30, 100, 0.05),  # 5% defective
    n = rep(100, 30)
  )
  # Target 1%: rate (5%) > target (1%) -> target_status = fail
  task <- capability_binomial(df, "defects", "n",
                              target_proportion = 0.01)
  expect_equal(task$results$diagnostics$capability_judgment$target_status,
               "fail")
  # Target 10%: rate (5%) <= target (10%) -> target_status = pass
  task2 <- capability_binomial(df, "defects", "n",
                               target_proportion = 0.10)
  expect_equal(task2$results$diagnostics$capability_judgment$target_status,
               "pass")
})

test_that("capability_poisson estimates u-bar and DPMO correctly", {
  set.seed(99)
  df <- data.frame(
    roll = 1:30,
    defects = rpois(30, lambda = 3),
    meters = rep(50, 30)
  )
  task <- capability_poisson(df, defects = "defects",
                            sample_sizes = "meters")
  expect_s3_class(task, "IqrAttributeCapabilityTask")

  s <- task$results$statistics
  expect_equal(s$distribution, "poisson")
  expect_equal(s$rate, sum(df$defects) / sum(df$meters),
               tolerance = 1e-12)
  expect_equal(s$dpmo_expected, s$rate * 1e6, tolerance = 1e-6)
  expect_equal(s$yield, exp(-s$rate), tolerance = 1e-9)
  # Z.Bench (short-term, shift=0) = qnorm(1 - p_total)
  # where p_total = 1 - exp(-u_bar) (probability of >=1 defect on a unit)
  p_total <- 1 - exp(-s$rate)
  expect_equal(s$z_bench, stats::qnorm(1 - p_total), tolerance = 1e-6)
  # Sigma level = Z.Bench + shift
  expect_equal(s$sigma_level, s$z_bench + s$z_shift, tolerance = 1e-9)
})

test_that("attribute plot returns ggplot objects", {
  set.seed(33)
  df_b <- data.frame(defects = rbinom(25, 50, 0.03), n = rep(50, 25))
  task <- capability_binomial(df_b, "defects", "n")
  for (t in c("control", "histogram", "defects", "summary", "full")) {
    p <- task$plot(type = t)
    expect_s3_class(p, c("ggplot", "gg", "patchwork"))
  }
})

test_that("attribute report exports Excel", {
  skip_if_not_installed("openxlsx")
  set.seed(3)
  df <- data.frame(defects = rbinom(20, 100, 0.02), n = rep(100, 20))
  task <- capability_binomial(df, "defects", "n")
  path <- tempfile(fileext = ".xlsx")
  task$report(format = "excel", path = path)
  expect_true(file.exists(path))
  expect_true(file.size(path) > 0)
})
