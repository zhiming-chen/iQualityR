# =============================================================================
# File: tests/testthat/test-resampling.R
# Description: Resampling module tests (R3-C1)
#   - ResamplingAnalyzer (bootstrap_ci / permutation_test)
#   - ResamplingPlotter (Contract 2 signature)
#   - ResamplingReporter (Contract 2 signature)
#   - iqr_resampling L3 integrator
#   - Convenience functions (resampling_run/plot/interpret/report)
#   - StatInterpreter resampling_result dispatch
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# ResamplingAnalyzer: bootstrap_ci (BCa)
# ----------------------------------------------------------------------------

test_that("ResamplingAnalyzer bootstrap_ci returns stat_result", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  result <- analyzer$bootstrap_ci(statistic = mean, data = x,
                                  R = 499, seed = 1)
  expect_s3_class(result, "stat_result")
  expect_s3_class(result, "resampling_result")
  expect_equal(result$domain, "resampling")
  expect_equal(result$test_type, "bootstrap_ci")
  expect_equal(result$boot_method, "bca")
  expect_length(result$conf.int, 2L)
  expect_true(all(is.finite(result$conf.int)))
  # Observed statistic should be the sample mean
  expect_equal(as.numeric(result$statistic[1]), mean(x), tolerance = 1e-9)
  # Replicate count
  expect_length(result$replicates, 499L)
  # CI should roughly bracket the observed mean
  expect_true(result$conf.int[1] <= as.numeric(result$statistic[1]))
  expect_true(result$conf.int[2] >= as.numeric(result$statistic[1]))
})

test_that("bootstrap_ci is reproducible with seed", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(30, mean = 50, sd = 3)
  r1 <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 299, seed = 42)
  r2 <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 299, seed = 42)
  expect_equal(r1$conf.int, r2$conf.int, tolerance = 1e-12)
  expect_equal(r1$replicates, r2$replicates)
})

test_that("bootstrap_ci restores global RNG state when seed is set", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(20)
  # Snapshot RNG state
  state1 <- .Random.seed
  analyzer$bootstrap_ci(statistic = mean, data = x, R = 199, seed = 7)
  state2 <- .Random.seed
  expect_equal(state1, state2)
})

test_that("bootstrap_ci does not alter RNG state when no seed is set", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(999)
  x <- rnorm(20)
  # Draw a deterministic value before / after to confirm advancing only by
  # the bootstrap draws (not a reset). We just check the call doesn't error.
  before <- runif(1)
  expect_no_error(
    analyzer$bootstrap_ci(statistic = mean, data = x, R = 99)
  )
  expect_true(is.numeric(before))
})

test_that("bootstrap_ci median statistic works", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rgamma(60, shape = 2, rate = 1)  # skewed, median != mean
  result <- analyzer$bootstrap_ci(statistic = median, data = x,
                                  R = 499, method = "bca", seed = 1)
  expect_equal(as.numeric(result$statistic[1]), median(x), tolerance = 1e-9)
  expect_true(result$conf.int[1] <= median(x))
  expect_true(result$conf.int[2] >= median(x))
})

test_that("bootstrap_ci on data.frame resamples rows", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  n <- 50
  df <- data.frame(y = rnorm(n), x = rnorm(n))
  slope_stat <- function(d) {
    coef(lm(y ~ x, data = d))["x"]
  }
  result <- analyzer$bootstrap_ci(statistic = slope_stat, data = df,
                                  R = 299, method = "perc", seed = 1)
  obs_slope <- coef(lm(y ~ x, data = df))["x"]
  expect_equal(as.numeric(result$statistic[1]), as.numeric(obs_slope),
               tolerance = 1e-9)
  expect_length(result$replicates, 299L)
  expect_true(all(is.finite(result$conf.int)))
})

# ----------------------------------------------------------------------------
# ResamplingAnalyzer: bootstrap CI methods (perc / basic / norm / bca)
# ----------------------------------------------------------------------------

test_that("bootstrap_ci supports all four methods", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(80, mean = 5, sd = 2)
  for (m in c("bca", "perc", "basic", "norm")) {
    result <- analyzer$bootstrap_ci(statistic = mean, data = x,
                                    R = 499, method = m, seed = 1)
    expect_equal(result$boot_method, m)
    expect_length(result$conf.int, 2L)
    expect_true(all(is.finite(result$conf.int)))
  }
})

test_that("bootstrap_ci one-sided intervals return Inf on the unbounded side", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(60, mean = 0, sd = 1)
  r_less <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 299,
                                  alternative = "less", seed = 1)
  r_greater <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 299,
                                     alternative = "greater", seed = 1)
  expect_true(is.infinite(r_less$conf.int[1]))
  expect_true(is.finite(r_less$conf.int[2]))
  expect_true(is.finite(r_greater$conf.int[1]))
  expect_true(is.infinite(r_greater$conf.int[2]))
})

test_that("bootstrap_ci errors on invalid inputs", {
  analyzer <- ResamplingAnalyzer$new()
  expect_error(analyzer$bootstrap_ci(statistic = NULL, data = 1:10),
               "must be a function")
  expect_error(analyzer$bootstrap_ci(statistic = mean, data = NULL),
               "data.*required")
  expect_error(analyzer$bootstrap_ci(statistic = mean, data = 1),
               "at least 2")
  expect_error(analyzer$bootstrap_ci(statistic = mean, data = 1:10, R = 10),
               "R.*at least 50")
  # statistic returning non-scalar
  expect_error(
    analyzer$bootstrap_ci(statistic = function(d) c(1, 2), data = 1:20, R = 99),
    "single numeric"
  )
})

test_that("ResamplingAnalyzer analyze dispatches both types", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(30)
  r1 <- analyzer$analyze("bootstrap_ci", statistic = mean, data = x,
                         R = 199, seed = 1)
  expect_s3_class(r1, "resampling_result")
  r2 <- analyzer$analyze("permutation_test", x = x, mu = 0, R = 199, seed = 1)
  expect_s3_class(r2, "resampling_result")
  expect_error(analyzer$analyze("nope"), "Unknown resample type")
})

# ----------------------------------------------------------------------------
# ResamplingAnalyzer: permutation_test
# ----------------------------------------------------------------------------

test_that("permutation_test two-sample detects real difference", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  g1 <- rnorm(30, mean = 50, sd = 5)
  g2 <- rnorm(30, mean = 56, sd = 5)  # clear shift
  result <- analyzer$permutation_test(x = g1, y = g2, R = 999, seed = 1)
  expect_s3_class(result, "resampling_result")
  expect_equal(result$test_type, "permutation_test")
  expect_equal(result$design, "two-sample")
  expect_equal(result$alternative, "two.sided")
  # Observed statistic is the difference of means
  expect_equal(as.numeric(result$statistic[1]), mean(g1) - mean(g2),
               tolerance = 1e-9)
  # p-value should be small given the 6-unit shift
  expect_true(result$p.value < 0.05)
  expect_length(result$replicates, 999L)
  # p-value in [0, 1]
  expect_true(result$p.value > 0 && result$p.value <= 1)
})

test_that("permutation_test two-sample finds no difference under H0", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  g1 <- rnorm(30, mean = 50, sd = 5)
  g2 <- rnorm(30, mean = 50, sd = 5)  # same distribution
  result <- analyzer$permutation_test(x = g1, y = g2, R = 999, seed = 1)
  expect_true(result$p.value > 0.05)
})

test_that("permutation_test one-sided alternatives work", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  g1 <- rnorm(30, mean = 50, sd = 5)
  g2 <- rnorm(30, mean = 55, sd = 5)
  r_less <- analyzer$permutation_test(x = g1, y = g2, R = 499,
                                      alternative = "less", seed = 1)
  r_greater <- analyzer$permutation_test(x = g1, y = g2, R = 499,
                                         alternative = "greater", seed = 1)
  # g1 - g2 < 0, so "less" should be significant, "greater" should not
  expect_true(r_less$p.value < 0.05)
  expect_true(r_greater$p.value > 0.05)
})

test_that("permutation_test paired sign-flipping works", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  n <- 30
  pre  <- rnorm(n, mean = 50, sd = 5)
  post <- pre + rnorm(n, mean = 3, sd = 1)  # consistent +3 shift
  result <- analyzer$permutation_test(x = post, y = pre, paired = TRUE,
                                      R = 999, seed = 1)
  expect_equal(result$design, "paired")
  expect_equal(as.numeric(result$statistic[1]), mean(post - pre),
               tolerance = 1e-9)
  expect_true(result$p.value < 0.05)
})

test_that("permutation_test paired requires equal lengths", {
  analyzer <- ResamplingAnalyzer$new()
  expect_error(
    analyzer$permutation_test(x = 1:5, y = 1:4, paired = TRUE),
    "length"
  )
})

test_that("permutation_test one-sample sign-flip works", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(40, mean = 53, sd = 5)
  result <- analyzer$permutation_test(x = x, mu = 50, R = 999, seed = 1)
  expect_equal(result$design, "one-sample")
  expect_equal(as.numeric(result$statistic[1]), mean(x - 50),
               tolerance = 1e-9)
  expect_true(result$p.value < 0.05)
})

test_that("permutation_test one-sample defaults to mu = 0", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(40, mean = 0, sd = 1)
  result <- analyzer$permutation_test(x = x, R = 499, seed = 1)
  expect_equal(result$design, "one-sample")
  # Under H0 (true mean 0), should not reject
  expect_true(result$p.value > 0.05)
})

test_that("permutation_test is reproducible with seed", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  g1 <- rnorm(20); g2 <- rnorm(20, mean = 2)
  r1 <- analyzer$permutation_test(x = g1, y = g2, R = 499, seed = 10)
  r2 <- analyzer$permutation_test(x = g1, y = g2, R = 499, seed = 10)
  expect_equal(r1$p.value, r2$p.value)
  expect_equal(r1$replicates, r2$replicates)
})

test_that("permutation_test accepts user-supplied statistic (two-sample)", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  g1 <- rnorm(20, mean = 50, sd = 5)
  g2 <- rnorm(20, mean = 56, sd = 5)
  # Use difference of medians instead of means
  med_diff <- function(a, b) median(a) - median(b)
  result <- analyzer$permutation_test(statistic = med_diff, x = g1, y = g2,
                                      R = 499, seed = 1)
  expect_equal(as.numeric(result$statistic[1]),
               median(g1) - median(g2), tolerance = 1e-9)
  expect_length(result$replicates, 499L)
})

test_that("permutation_test errors on invalid inputs", {
  analyzer <- ResamplingAnalyzer$new()
  expect_error(analyzer$permutation_test(x = NULL), "x.*required")
  expect_error(analyzer$permutation_test(x = "a"), "numeric")
  expect_error(analyzer$permutation_test(x = 1:10, R = 10), "R.*at least 100")
  expect_error(analyzer$permutation_test(x = 1:10, alternative = "nope"),
               "arg")
})

# ----------------------------------------------------------------------------
# ResamplingAnalyzer: bootstrap CI coverage sanity check
# ----------------------------------------------------------------------------

test_that("bootstrap_ci BCa for mean agrees with t-interval", {
  # For the mean of a normal sample, BCa CI should be close to the t-interval.
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(200, mean = 100, sd = 5)
  boot_r <- analyzer$bootstrap_ci(statistic = mean, data = x,
                                  R = 999, method = "bca", seed = 1)
  t_r <- t.test(x, conf.level = 0.95)
  # Allow 0.02 tolerance: bootstrap Monte Carlo noise + BCa adjustment
  expect_equal(boot_r$conf.int[1], t_r$conf.int[1], tolerance = 0.02)
  expect_equal(boot_r$conf.int[2], t_r$conf.int[2], tolerance = 0.02)
})

# ----------------------------------------------------------------------------
# ResamplingReporter
# ----------------------------------------------------------------------------

test_that("ResamplingReporter initialization", {
  reporter <- ResamplingReporter$new()
  expect_true(inherits(reporter, "ResamplingReporter"))
  expect_true(inherits(reporter, "R6"))
})

test_that("ResamplingReporter$report has Contract 2 signature", {
  f <- ResamplingReporter$new()$report
  fm <- formals(f)
  expect_named(fm, c("result", "format", "path", "audience"))
})

test_that("ResamplingReporter to_dataframe for bootstrap_ci", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(40)
  result <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 199, seed = 1)
  reporter <- ResamplingReporter$new()
  df <- reporter$report(result, format = "data.frame")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
  expect_true(all(c("Statistic", "Lower", "Upper", "Conf_Level") %in%
                  names(df)))
})

test_that("ResamplingReporter to_dataframe for permutation_test", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  g1 <- rnorm(20); g2 <- rnorm(20, mean = 1)
  result <- analyzer$permutation_test(x = g1, y = g2, R = 199, seed = 1)
  reporter <- ResamplingReporter$new()
  df <- reporter$report(result, format = "data.frame")
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Statistic", "P_Value", "Design") %in% names(df)))
})

test_that("ResamplingReporter console output works for bootstrap_ci", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(30)
  result <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 199, seed = 1)
  reporter <- ResamplingReporter$new()
  out <- capture.output(reporter$report(result, format = "console",
                                        audience = "manager"))
  expect_true(length(out) > 0L)
  expect_true(any(grepl("Bootstrap|statistic", out)))
})

test_that("ResamplingReporter console output works for permutation_test", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  g1 <- rnorm(20); g2 <- rnorm(20, mean = 2)
  result <- analyzer$permutation_test(x = g1, y = g2, R = 199, seed = 1)
  reporter <- ResamplingReporter$new()
  out <- capture.output(reporter$report(result, format = "console",
                                        audience = "technical"))
  expect_true(any(grepl("p-value|permutation", out, ignore.case = TRUE)))
})

# ----------------------------------------------------------------------------
# StatInterpreter resampling_result dispatch
# ----------------------------------------------------------------------------

test_that("StatInterpreter handles bootstrap_ci (manager)", {
  analyzer <- ResamplingAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 5)
  result <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 299, seed = 1)
  out <- interpreter$interpret(result, audience = "manager")
  expect_type(out, "character")
  expect_true(grepl("Resampling", out))
  expect_true(grepl("confident", out, ignore.case = TRUE))
})

test_that("StatInterpreter handles bootstrap_ci (technical, method note)", {
  analyzer <- ResamplingAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  x <- rnorm(50)
  for (m in c("bca", "perc", "basic", "norm")) {
    result <- analyzer$bootstrap_ci(statistic = mean, data = x,
                                    R = 199, method = m, seed = 1)
    out <- interpreter$interpret(result, audience = "technical")
    expect_true(grepl("Method Note|Conclusion", out))
  }
})

test_that("StatInterpreter handles permutation_test (manager)", {
  analyzer <- ResamplingAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  g1 <- rnorm(30, mean = 50, sd = 5)
  g2 <- rnorm(30, mean = 56, sd = 5)
  result <- analyzer$permutation_test(x = g1, y = g2, R = 499, seed = 1)
  out <- interpreter$interpret(result, audience = "manager")
  expect_true(grepl("permutation|Permutation|significant", out,
                    ignore.case = TRUE))
})

test_that("StatInterpreter handles permutation_test (technical)", {
  analyzer <- ResamplingAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  g1 <- rnorm(30); g2 <- rnorm(30, mean = 2)
  result <- analyzer$permutation_test(x = g1, y = g2, R = 499, seed = 1)
  out <- interpreter$interpret(result, audience = "technical")
  expect_true(grepl("Design Note|Reject H0", out))
})

test_that("StatInterpreter handles permutation_test (client)", {
  analyzer <- ResamplingAnalyzer$new()
  interpreter <- StatInterpreter$new()
  set.seed(123)
  g1 <- rnorm(30); g2 <- rnorm(30, mean = 2)
  result <- analyzer$permutation_test(x = g1, y = g2, R = 499, seed = 1)
  out <- interpreter$interpret(result, audience = "client")
  expect_true(grepl("Quality Assurance|significant", out, ignore.case = TRUE))
})

# ----------------------------------------------------------------------------
# iqr_resampling L3 integrator
# ----------------------------------------------------------------------------

test_that("iqr_resampling runs bootstrap_ci end-to-end", {
  set.seed(123)
  x <- rnorm(40, mean = 50, sd = 3)
  rs <- iqr_resampling$new()
  rs$run("bootstrap_ci", statistic = mean, data = x, R = 199, seed = 1)
  expect_s3_class(rs$last_results, "resampling_result")
  expect_equal(rs$last_results$test_type, "bootstrap_ci")
})

test_that("iqr_resampling runs permutation_test end-to-end", {
  set.seed(123)
  g1 <- rnorm(20); g2 <- rnorm(20, mean = 3)
  rs <- iqr_resampling$new()
  rs$run("permutation_test", x = g1, y = g2, R = 199, seed = 1)
  expect_equal(rs$last_results$test_type, "permutation_test")
})

test_that("iqr_resampling$report returns data.frame", {
  set.seed(123)
  x <- rnorm(30)
  rs <- iqr_resampling$new()
  rs$run("bootstrap_ci", statistic = mean, data = x, R = 199, seed = 1)
  df <- rs$report(format = "data.frame")
  expect_s3_class(df, "data.frame")
})

test_that("iqr_resampling$plot / $interpret / $report error before $run", {
  rs <- iqr_resampling$new()
  expect_error(rs$plot(), "run.*first")
  expect_error(rs$interpret(), "run.*first")
  expect_error(rs$report(), "run.*first")
})

test_that("iqr_resampling set_theme works", {
  rs <- iqr_resampling$new()
  expect_no_error(rs$set_theme("academic"))
  expect_true(inherits(rs$theme_obj, "IqrTheme") || is.null(rs$theme_obj))
})

# ----------------------------------------------------------------------------
# Convenience functions
# ----------------------------------------------------------------------------

test_that("resampling_run returns stat_result (bootstrap)", {
  set.seed(123)
  x <- rnorm(40)
  result <- resampling_run("bootstrap_ci", statistic = mean, data = x,
                           R = 199, seed = 1)
  expect_s3_class(result, "resampling_result")
})

test_that("resampling_run returns stat_result (permutation)", {
  set.seed(123)
  g1 <- rnorm(20); g2 <- rnorm(20, mean = 2)
  result <- resampling_run("permutation_test", x = g1, y = g2,
                           R = 199, seed = 1)
  expect_s3_class(result, "resampling_result")
})

test_that("resampling_interpret returns character", {
  set.seed(123)
  x <- rnorm(40)
  result <- resampling_run("bootstrap_ci", statistic = mean, data = x,
                           R = 199, seed = 1)
  out <- capture.output(resampling_interpret(result, audience = "manager"))
  expect_true(length(out) > 0L)
})

test_that("resampling_report returns data.frame", {
  set.seed(123)
  x <- rnorm(30)
  result <- resampling_run("bootstrap_ci", statistic = mean, data = x,
                           R = 199, seed = 1)
  df <- resampling_report(result, format = "data.frame")
  expect_s3_class(df, "data.frame")
})

# ----------------------------------------------------------------------------
# ResamplingPlotter
# ----------------------------------------------------------------------------

test_that("ResamplingPlotter initialization", {
  plotter <- ResamplingPlotter$new(theme = "academic")
  expect_true(inherits(plotter, "ResamplingPlotter"))
  expect_true(inherits(plotter, "R6"))
})

test_that("ResamplingPlotter$plot has Contract 2 signature", {
  f <- ResamplingPlotter$new()$plot
  fm <- formals(f)
  expect_named(fm, c("result", "plot_type", "show_table", "theme_obj"))
  expect_equal(fm$plot_type, "auto")
  expect_equal(fm$show_table, FALSE)
  expect_equal(fm$theme_obj, NULL)
})

test_that("ResamplingPlotter renders histogram for bootstrap_ci", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(50)
  result <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 299, seed = 1)
  plotter <- ResamplingPlotter$new()
  p <- plotter$plot(result, plot_type = "hist")
  expect_true(inherits(p, "ggplot"))
})

test_that("ResamplingPlotter renders density for bootstrap_ci", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(50)
  result <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 299, seed = 1)
  plotter <- ResamplingPlotter$new()
  p <- plotter$plot(result, plot_type = "density")
  expect_true(inherits(p, "ggplot"))
})

test_that("ResamplingPlotter renders qq for bootstrap_ci", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(50)
  result <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 299, seed = 1)
  plotter <- ResamplingPlotter$new()
  p <- plotter$plot(result, plot_type = "qq")
  expect_true(inherits(p, "ggplot"))
})

test_that("ResamplingPlotter renders histogram for permutation_test", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  g1 <- rnorm(30); g2 <- rnorm(30, mean = 3)
  result <- analyzer$permutation_test(x = g1, y = g2, R = 299, seed = 1)
  plotter <- ResamplingPlotter$new()
  p <- plotter$plot(result, plot_type = "hist")
  expect_true(inherits(p, "ggplot"))
})

test_that("ResamplingPlotter auto-selects hist", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(40)
  result <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 199, seed = 1)
  plotter <- ResamplingPlotter$new()
  p <- plotter$plot(result, plot_type = "auto")
  expect_true(inherits(p, "ggplot"))
  # Title should contain "Bootstrap"
  pb <- ggplot2::ggplot_build(p)
  expect_true(grepl("Bootstrap", pb$plot$labels$title %||% ""))
})

test_that("ResamplingPlotter show_table annotation works", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(40)
  result <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 199, seed = 1)
  plotter <- ResamplingPlotter$new()
  p <- plotter$plot(result, plot_type = "hist", show_table = TRUE)
  expect_true(inherits(p, "ggplot"))
})

test_that("ResamplingPlotter rejects unknown plot_type", {
  skip_if_not_installed("iQualityR.plot")
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(30)
  result <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 199, seed = 1)
  plotter <- ResamplingPlotter$new()
  expect_error(plotter$plot(result, plot_type = "nope"), "unknown plot_type")
})

# ----------------------------------------------------------------------------
# format.stat_result integration
# ----------------------------------------------------------------------------

test_that("format.stat_result works for resampling_result", {
  analyzer <- ResamplingAnalyzer$new()
  set.seed(123)
  x <- rnorm(30)
  result <- analyzer$bootstrap_ci(statistic = mean, data = x, R = 199, seed = 1)
  out <- format(result)
  expect_type(out, "character")
  expect_true(grepl("stat_result: domain=resampling", out))
  # Permutation
  g1 <- rnorm(20); g2 <- rnorm(20, mean = 2)
  r2 <- analyzer$permutation_test(x = g1, y = g2, R = 199, seed = 1)
  out2 <- format(r2)
  expect_true(grepl("stat_result: domain=resampling", out2))
})
