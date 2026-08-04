# =============================================================================
# File: tests/testthat/test-distributions.R
# Description: Probability distribution module tests
#   - Distribution registry (dist_registry.R)
#   - Probability calculation (iqr_prob.R, Prob*.R)
#   - Distribution fitting (dist_fit.R)
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# Distribution registry (dist_registry.R)
# ----------------------------------------------------------------------------

test_that("Distribution registry exists", {
  # DIST_REGISTRY is not exported; access via :::
  reg <- iQualityR.stat:::DIST_REGISTRY
  expect_true(is.list(reg))
  expect_true(length(reg) > 0)
  expect_true("norm" %in% names(reg))
})

test_that("list_available_dists returns a data.frame", {
  dists <- list_available_dists()
  expect_s3_class(dists, "data.frame")
  expect_true(nrow(dists) > 0)
  expect_true("type" %in% names(dists))
  expect_true("norm" %in% dists$type)
})

test_that("get_dist_info returns distribution info", {
  info <- get_dist_info("norm")
  expect_type(info, "list")
  expect_true("description" %in% names(info))
})

# ----------------------------------------------------------------------------
# Probability calculation (prob_calc / iqr_prob)
# ----------------------------------------------------------------------------

test_that("prob_calc normal distribution (upper tail)", {
  result <- prob_calc(type = "norm", params = list(mean = 0, sd = 1),
                      x = 1.96, calc_type = "upper")
  expect_type(result, "list")
  expect_true(length(result) > 0)
  node_res <- result[[1]]
  expect_true("all_res" %in% names(node_res))
  expect_true("mode" %in% names(node_res))
  expect_equal(node_res$mode, "prob")
  p_val <- node_res$all_res[[1]]$result_p
  expect_gte(p_val, 0)
  expect_lte(p_val, 1)
  expect_lt(p_val, 0.05)
})

test_that("prob_calc binomial distribution", {
  result <- prob_calc(type = "binom", params = list(size = 10, prob = 0.5),
                      x = 5, calc_type = "lower")
  expect_type(result, "list")
  p_val <- result[[1]]$all_res[[1]]$result_p
  expect_gte(p_val, 0)
  expect_lte(p_val, 1)
})

test_that("iqr_prob R6 class entry", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  expect_true(inherits(prob, "iqr_prob"))
  expect_true(inherits(prob, "R6"))
  expect_true(length(prob$nodes) > 0)
})

test_that("iqr_prob calc + interpret chain", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  expect_false(is.null(prob$last_results))
  expect_equal(prob$last_results[[1]]$mode, "prob")
})

# ----------------------------------------------------------------------------
# Distribution fitting (dist_fit.R)
# ----------------------------------------------------------------------------

test_that("fit_distribution normal fit", {
  set.seed(123)
  x <- rnorm(100)
  result <- fit_distribution(x, "norm")

  expect_type(result, "list")
  expect_equal(result$dist, "norm")
  expect_true("params" %in% names(result))
  expect_true("AIC" %in% names(result))
  expect_true("BIC" %in% names(result))
})

test_that("fit_distribution exponential fit", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  result <- fit_distribution(x, "exp")

  expect_type(result, "list")
  expect_equal(result$dist, "exp")
  expect_gt(result$params$rate, 0)
})

test_that("auto_fit_distribution auto-select", {
  set.seed(123)
  x <- rnorm(100)
  result <- auto_fit_distribution(x)

  expect_type(result, "list")
  expect_true("best_dist" %in% names(result))
  expect_true("ranking" %in% names(result))
  expect_s3_class(result$ranking, "data.frame")
})

test_that("auto_fit_distribution positive-only data", {
  set.seed(123)
  x <- rexp(100, rate = 0.5)
  result <- auto_fit_distribution(x, positive_only = TRUE)

  expect_type(result, "list")
  expect_true(result$best_dist %in% c("exp", "gamma", "weibull", "lnorm", "logis"))
})

test_that("compare_fits comparison", {
  set.seed(123)
  x <- rnorm(100)
  f1 <- fit_distribution(x, "norm")
  f2 <- fit_distribution(x, "logis")
  result <- compare_fits(list(norm = f1, logis = f2))

  expect_type(result, "list")
  expect_true("test_results" %in% names(result))
  expect_true("best_by_ks" %in% names(result))
})

test_that("empirical_distribution", {
  set.seed(123)
  x <- rnorm(100)
  result <- empirical_distribution(x)

  expect_type(result, "list")
  expect_true("ecdf" %in% names(result))
  expect_true("points" %in% names(result))
})

test_that("calc_qq_data QQ data", {
  set.seed(123)
  x <- rnorm(100)
  result <- calc_qq_data(x, "norm", list(mean = 0, sd = 1))

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_true("theoretical" %in% names(result))
  expect_true("sample" %in% names(result))
})

# ----------------------------------------------------------------------------
# validate_dist_params -- all 16 distributions + error cases (R2-7a)
# ----------------------------------------------------------------------------

test_that("validate_dist_params fills defaults for norm", {
  p <- iQualityR.stat:::validate_dist_params("norm", list())
  expect_equal(p$mean, 0)
  expect_equal(p$sd, 1)
})

test_that("validate_dist_params fills defaults for all continuous distributions", {
  cont_dists <- list(
    list("weibull", list(shape = 2, scale = 3)),
    list("lnorm", list(meanlog = 0, sdlog = 0.5)),
    list("gamma", list(shape = 2, scale = 2)),
    list("exp", list(rate = 0.5)),
    list("t", list(df = 5)),
    list("f", list(df1 = 3, df2 = 10)),
    list("chisq", list(df = 4)),
    list("beta", list(shape1 = 2, shape2 = 3)),
    list("unif", list(min = 0, max = 10)),
    list("logis", list(location = 0, scale = 1)),
    list("cauchy", list(location = 0, scale = 1))
  )
  for (d in cont_dists) {
    p <- iQualityR.stat:::validate_dist_params(d[[1]], d[[2]])
    expect_type(p, "list")
  }
})

test_that("validate_dist_params fills defaults for all discrete distributions", {
  disc_dists <- list(
    list("binom", list(size = 20, prob = 0.3)),
    list("pois", list(lambda = 5)),
    list("nbinom", list(size = 5, prob = 0.4)),
    list("hyper", list(m = 15, n = 10, k = 5)),
    list("geom", list(prob = 0.2))
  )
  for (d in disc_dists) {
    p <- iQualityR.stat:::validate_dist_params(d[[1]], d[[2]])
    expect_type(p, "list")
  }
})

test_that("validate_dist_params errors on unknown distribution", {
  expect_error(
    iQualityR.stat:::validate_dist_params("nope", list()),
    class = "iqr_prob_error"
  )
})

test_that("validate_dist_params errors on negative sd for norm", {
  expect_error(
    iQualityR.stat:::validate_dist_params("norm", list(sd = -1)),
    class = "iqr_prob_error"
  )
})

test_that("validate_dist_params errors on non-numeric param", {
  expect_error(
    iQualityR.stat:::validate_dist_params("norm", list(mean = "abc")),
    class = "iqr_prob_error"
  )
})

test_that("validate_dist_params errors on zero rate for exp", {
  expect_error(
    iQualityR.stat:::validate_dist_params("exp", list(rate = 0)),
    class = "iqr_prob_error"
  )
})

test_that("validate_dist_params errors on non-integer size for binom", {
  expect_error(
    iQualityR.stat:::validate_dist_params("binom", list(size = 5.5)),
    class = "iqr_prob_error"
  )
})

test_that("validate_dist_params errors on prob > 1 for binom", {
  expect_error(
    iQualityR.stat:::validate_dist_params("binom", list(prob = 1.5)),
    class = "iqr_prob_error"
  )
})

test_that("validate_dist_params errors on min >= max for unif", {
  expect_error(
    iQualityR.stat:::validate_dist_params("unif", list(min = 5, max = 3)),
    class = "iqr_prob_error"
  )
})

test_that("validate_dist_params errors on missing required params for hyper", {
  expect_error(
    iQualityR.stat:::validate_dist_params("hyper", list(m = 5)),
    class = "iqr_prob_error"
  )
})

test_that("validate_dist_params errors on k > m+n for hyper", {
  expect_error(
    iQualityR.stat:::validate_dist_params("hyper", list(m = 3, n = 2, k = 10)),
    class = "iqr_prob_error"
  )
})

test_that("validate_dist_params handles non-list params input", {
  p <- iQualityR.stat:::validate_dist_params("norm", NULL)
  expect_equal(p$mean, 0)
  expect_equal(p$sd, 1)
})

test_that("validate_dist_params lower/upper bounds for nbinom prob", {
  # prob = 0 should error (lower_open = TRUE, 0 <= 0 invalid)
  expect_error(iQualityR.stat:::validate_dist_params("nbinom", list(prob = 0)),
               class = "iqr_prob_error")
  # prob = 1 is valid for nbinom (R accepts prob in (0, 1]; upper bound inclusive)
  p <- iQualityR.stat:::validate_dist_params("nbinom", list(size = 5, prob = 1))
  expect_equal(p$prob, 1)
  # prob > 1 should error (upper = 1, upper_open = FALSE)
  expect_error(iQualityR.stat:::validate_dist_params("nbinom", list(prob = 1.5)),
               class = "iqr_prob_error")
  # Valid prob in (0, 1)
  p <- iQualityR.stat:::validate_dist_params("nbinom", list(size = 5, prob = 0.5))
  expect_equal(p$prob, 0.5)
})

# ----------------------------------------------------------------------------
# dist_registry -- register_dist / unregister_dist (R2-7a)
# ----------------------------------------------------------------------------

test_that("register_dist adds a custom distribution", {
  my_d <- function(x, p) pmax(0, 1 - abs(x - p$center) / p$half_width)
  my_p <- function(q, p) pmax(0, pmin(1, 0.5 + (q - p$center) / p$half_width))
  my_q <- function(p_vec, p) p$center + (p_vec - 0.5) * p$half_width

  register_dist("test_tri",
    d_func = my_d, p_func = my_p, q_func = my_q,
    defaults = list(center = 0, half_width = 1),
    is_discrete = FALSE, support = "R",
    description = "Test triangular distribution")

  expect_true("test_tri" %in% names(iQualityR.stat:::DIST_REGISTRY))
  info <- get_dist_info("test_tri")
  expect_equal(info$description, "Test triangular distribution")

  # Cleanup
  unregister_dist("test_tri")
  expect_false("test_tri" %in% names(iQualityR.stat:::DIST_REGISTRY))
})

test_that("register_dist warns when overwriting existing distribution", {
  my_d <- function(x, p) dnorm(x)
  my_p <- function(q, p) pnorm(q)
  my_q <- function(p_vec, p) qnorm(p_vec)

  expect_warning(
    register_dist("norm",
      d_func = my_d, p_func = my_p, q_func = my_q,
      defaults = list(), is_discrete = FALSE,
      support = "R", description = "Override norm")
  )
  # Restore original by re-registering with standard params
  register_dist("norm",
    d_func = function(x, p) dnorm(x, p$mean, p$sd),
    p_func = function(q, p, lower.tail = TRUE) pnorm(q, p$mean, p$sd, lower.tail = lower.tail),
    q_func = function(p_val, p, lower.tail = TRUE) qnorm(p_val, p$mean, p$sd, lower.tail = lower.tail),
    defaults = list(mean = 0, sd = 1),
    is_discrete = FALSE, support = "(-Inf, +Inf)",
    description = "Normal distribution (Gaussian)")
})

test_that("unregister_dist warns on non-existent distribution", {
  expect_warning(unregister_dist("does_not_exist"))
})

test_that("get_dist_info errors on unknown distribution", {
  expect_error(get_dist_info("nope"), "Unknown distribution")
})

test_that("list_available_dists includes all 16 distributions", {
  dists <- list_available_dists()
  expect_gte(nrow(dists), 16)
  # Check key columns
  for (col in c("type", "description", "is_discrete", "support")) {
    expect_true(col %in% names(dists))
  }
  # Continuous and discrete both present
  expect_true(any(dists$is_discrete))
  expect_false(all(dists$is_discrete))
})

# ----------------------------------------------------------------------------
# ProbNode -- all distribution types + d/p/q methods (R2-7a)
# ----------------------------------------------------------------------------

test_that("ProbNode initializes for continuous distributions", {
  for (d in c("norm", "weibull", "lnorm", "gamma", "exp", "t", "f", "chisq",
              "beta", "unif", "logis", "cauchy")) {
    node <- ProbNode$new("N1", d, list())
    expect_equal(node$type, d)
    expect_false(node$is_discrete)
    expect_true(length(node$params) > 0)
  }
})

test_that("ProbNode initializes for discrete distributions", {
  disc <- list(
    list("binom", list(size = 10, prob = 0.5)),
    list("pois", list(lambda = 3)),
    list("nbinom", list(size = 5, prob = 0.3)),
    list("hyper", list(m = 10, n = 5, k = 3)),
    list("geom", list(prob = 0.2))
  )
  for (d in disc) {
    node <- ProbNode$new("N1", d[[1]], d[[2]])
    expect_equal(node$type, d[[1]])
    expect_true(node$is_discrete)
  }
})

test_that("ProbNode d/p/q methods work for normal", {
  node <- ProbNode$new("N", "norm", list(mean = 0, sd = 1))
  expect_equal(node$d(0), dnorm(0), tolerance = 1e-10)
  expect_equal(node$p(0), 0.5, tolerance = 1e-10)
  expect_equal(node$q(0.5), 0, tolerance = 1e-10)
})

test_that("ProbNode d/p/q methods work for binomial", {
  node <- ProbNode$new("B", "binom", list(size = 10, prob = 0.5))
  expect_equal(node$d(5), dbinom(5, 10, 0.5), tolerance = 1e-10)
  expect_equal(node$p(5), pbinom(5, 10, 0.5), tolerance = 1e-10)
  expect_equal(node$q(0.5), qbinom(0.5, 10, 0.5), tolerance = 1e-10)
})

test_that("ProbNode execute in prob mode returns correct structure", {
  node <- ProbNode$new("N", "norm", list(mean = 0, sd = 1))
  res <- node$execute(1.96, mode = "prob", calc_type = "upper")
  expect_equal(res$mode, "prob")
  expect_true(res$is_prob_mode)
  expect_equal(res$x_val, 1.96)
  expect_lt(abs(res$result_p - 0.025), 0.001)
  expect_true(nchar(res$pdf_lbl) > 0)
})

test_that("ProbNode execute in quant mode returns correct structure", {
  node <- ProbNode$new("N", "norm", list(mean = 0, sd = 1))
  res <- node$execute(0.975, mode = "quant", calc_type = "lower")
  expect_equal(res$mode, "quant")
  expect_false(res$is_prob_mode)
  expect_lt(abs(res$result_x - 1.96), 0.001)
})

test_that("ProbNode execute lower tail calculation", {
  node <- ProbNode$new("N", "norm", list(mean = 0, sd = 1))
  res <- node$execute(-1.96, mode = "prob", calc_type = "lower")
  expect_lt(abs(res$result_p - 0.025), 0.001)
})

test_that("ProbNode loc offset shifts continuous distributions", {
  node <- ProbNode$new("N", "norm", list(mean = 0, sd = 1), loc = 5)
  # d at x=5 should equal dnorm(0) because 5 - loc(5) = 0
  expect_equal(node$d(5), dnorm(0), tolerance = 1e-10)
  # q at 0.5 should be 0 + loc = 5
  expect_equal(node$q(0.5), 5, tolerance = 1e-10)
})

test_that("ProbNode gen_label generates correct text", {
  node <- ProbNode$new("N", "norm", list(mean = 0, sd = 1))
  lbl <- node$gen_label(1.5, 0.067, "lower")
  expect_true(grepl("1.5000", lbl))
  expect_true(grepl("0.0670", lbl))
  expect_equal(node$last_label_text, lbl)
})

test_that("ProbNode gen_label between/outside types", {
  node <- ProbNode$new("N", "norm", list(mean = 0, sd = 1))
  expect_true(nchar(node$gen_label(c(1, 2), 0.13, "between")) > 0)
  expect_true(nchar(node$gen_label(c(1, 2), 0.13, "outside")) > 0)
})

test_that("ProbNode get_node_info returns metadata", {
  node <- ProbNode$new("N", "norm", list(mean = 0, sd = 1))
  info <- node$get_node_info()
  expect_equal(info$type, "norm")
  expect_true("description" %in% names(info))
  expect_true("support" %in% names(info))
  expect_false(info$is_discrete)
  expect_true("mean" %in% names(info$params))
})

test_that("ProbNode errors on unknown distribution type", {
  expect_error(ProbNode$new("N", "unknown_dist", list()), "Unknown distribution")
})

# ----------------------------------------------------------------------------
# iqr_prob L3 integrator -- full method surface (R2-7a)
# ----------------------------------------------------------------------------

test_that("iqr_prob add_dist single distribution", {
  prob <- iqr_prob$new()
  prob$add_dist(type = "norm", params = list(mean = 10, sd = 2))
  expect_equal(length(prob$nodes), 1)
  expect_equal(prob$nodes[[1]]$params$mean, 10)
})

test_that("iqr_prob add_dist batch via dist_list", {
  prob <- iqr_prob$new()
  prob$add_dist(dist_list = list(
    list(id = "A", type = "norm", params = list(mean = 0, sd = 1)),
    list(id = "B", type = "norm", params = list(mean = 5, sd = 1))
  ))
  expect_equal(length(prob$nodes), 2)
  expect_true("A" %in% names(prob$nodes))
  expect_true("B" %in% names(prob$nodes))
})

test_that("iqr_prob add_dist reset clears existing nodes", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  expect_equal(length(prob$nodes), 1)
  prob$add_dist(type = "exp", params = list(rate = 1), reset = TRUE)
  expect_equal(length(prob$nodes), 1)
  expect_equal(prob$nodes[[1]]$type, "exp")
})

test_that("iqr_prob calc quantile mode", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 0.975, mode = "quant", calc_type = "lower")
  expect_false(is.null(prob$last_results))
  expect_equal(prob$last_results[[1]]$mode, "quant")
  # Result is wrapped in all_res by ProbAnalyzer
  res_x <- prob$last_results[[1]]$all_res[[1]]$result_x
  expect_lt(abs(res_x - 1.96), 0.001)
})

test_that("iqr_prob calc between mode", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = c(-1, 1), mode = "prob", calc_type = "between")
  expect_false(is.null(prob$last_results))
  # P(-1 < Z < 1) ~ 0.6827
  res_p <- prob$last_results[[1]]$all_res[[1]]$result_p
  expect_lt(abs(res_p - 0.6827), 0.01)
})

test_that("iqr_prob calc outside mode", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = c(-1.96, 1.96), mode = "prob", calc_type = "outside")
  expect_false(is.null(prob$last_results))
  # P(Z < -1.96 or Z > 1.96) ~ 0.05
  res_p <- prob$last_results[[1]]$all_res[[1]]$result_p
  expect_lt(abs(res_p - 0.05), 0.01)
})

test_that("iqr_prob calc errors without nodes", {
  prob <- iqr_prob$new()
  expect_error(prob$calc(values = 1), "not configured")
})

test_that("iqr_prob calc errors without values", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  expect_error(prob$calc(values = NULL), "provide values")
})

test_that("iqr_prob interpret after calc", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 100, sd = 5))
  prob$calc(values = 108, mode = "prob", calc_type = "upper")
  interp <- prob$interpret(audience = "manager")
  # interpret() returns a list of per-node interpretation strings
  expect_type(interp, "list")
  expect_true(length(interp) >= 1)
  expect_type(interp[[1]], "character")
  expect_true(nchar(interp[[1]]) > 0)
})

test_that("iqr_prob interpret errors before calc", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  expect_error(prob$interpret(), "calc")
})

test_that("iqr_prob plot errors before calc", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  expect_error(prob$plot(), "calc")
})

test_that("iqr_prob report data.frame format", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  # report() takes no arguments and returns a data.frame
  df <- prob$report()
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) >= 1)
})

test_that("iqr_prob report returns NULL before calc", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  # report() returns NULL when last_results is NULL (no error)
  expect_null(prob$report())
})

test_that("iqr_prob with custom theme object", {
  skip_if_not_installed("iQualityR.core")
  theme_obj <- iQualityR.core::IqrTheme$new("academic")
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1), theme = theme_obj)
  expect_false(is.null(prob$theme_obj))
})

test_that("iqr_prob multi-distribution calc", {
  prob <- iqr_prob$new(dist_list = list(
    list(id = "A", type = "norm", params = list(mean = 0, sd = 1)),
    list(id = "B", type = "norm", params = list(mean = 5, sd = 1))
  ))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  expect_equal(length(prob$last_results), 2)
  # Both nodes computed
  expect_true("A" %in% names(prob$last_results))
  expect_true("B" %in% names(prob$last_results))
})

# ----------------------------------------------------------------------------
# ProbReporter -- L2 report engine (R2-7d)
# ----------------------------------------------------------------------------

test_that("ProbReporter initialization", {
  reporter <- ProbReporter$new()
  expect_true(inherits(reporter, "ProbReporter"))
  expect_true(inherits(reporter, "R6"))
})

test_that("ProbReporter$print_console prints prob-mode results without error", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  reporter <- ProbReporter$new()
  out <- capture.output(reporter$print_console(prob$last_results))
  expect_true(length(out) > 0L)
  expect_true(any(grepl("Node ID", out)))
})

test_that("ProbReporter$print_console prints quant-mode results without error", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = c(0.025, 0.975), mode = "quant", calc_type = "lower")
  reporter <- ProbReporter$new()
  out <- capture.output(reporter$print_console(prob$last_results))
  expect_true(length(out) > 0L)
})

test_that("ProbReporter$print_console skips nodes with abnormal structure", {
  # Provide a malformed bundle to exercise the warning + skip branch
  bad_results <- list(BadNode = list(mode = "prob", all_res = NULL))
  reporter <- ProbReporter$new()
  out <- capture.output(suppressWarnings(reporter$print_console(bad_results)))
  # Should not crash; output may be just the header
  expect_true(length(out) >= 1L)
})

test_that("ProbReporter$to_dataframe returns data.frame for prob mode", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  reporter <- ProbReporter$new()
  df <- reporter$to_dataframe(prob$last_results)
  expect_s3_class(df, "data.frame")
  expect_true("node_id" %in% names(df))
  expect_true("mode" %in% names(df))
  expect_equal(df$mode[1], "prob")
})

test_that("ProbReporter$to_dataframe returns data.frame for quant mode", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = c(0.025, 0.975), mode = "quant", calc_type = "lower")
  reporter <- ProbReporter$new()
  df <- reporter$to_dataframe(prob$last_results)
  expect_s3_class(df, "data.frame")
  expect_equal(df$mode[1], "quant")
})

test_that("ProbReporter$to_dataframe returns NULL for NULL input", {
  reporter <- ProbReporter$new()
  expect_null(reporter$to_dataframe(NULL))
})

test_that("ProbReporter$to_dataframe errors on non-list input", {
  reporter <- ProbReporter$new()
  expect_error(reporter$to_dataframe(42), "must be a list")
})

test_that("ProbReporter$to_dataframe skips nodes missing all_res", {
  bad_results <- list(NodeA = list(mode = "prob"),  # no all_res
                      NodeB = list(mode = "prob", pdf_lbl = "P(X > 1)",
                                   all_res = list(
                        list(is_prob_mode = TRUE, target_x = 1, result_p = 0.5,
                             target_p = NULL, result_x = NULL)
                      )))
  reporter <- ProbReporter$new()
  df <- reporter$to_dataframe(bad_results)
  expect_s3_class(df, "data.frame")
  # Only NodeB should be in the result
  expect_equal(nrow(df), 1L)
  expect_equal(df$node_id[1], "NodeB")
})

test_that("ProbReporter$to_dataframe returns NULL when all nodes are bad", {
  bad_results <- list(NodeA = list(mode = "prob"))  # no all_res
  reporter <- ProbReporter$new()
  expect_null(reporter$to_dataframe(bad_results))
})

test_that("ProbReporter$export_excel writes an xlsx file", {
  skip_if_not_installed("iQualityR.core")
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  reporter <- ProbReporter$new()
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf), add = TRUE)
  config <- iQualityR.core::IqrTheme$new("academic")$config
  exporter <- iQualityR.core::ExcelExporter$new(config)
  reporter$export_excel(prob$last_results, prob$nodes, tf, exporter)
  expect_true(file.exists(tf))
})

test_that("ProbReporter$export_excel errors without excel_exporter", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  reporter <- ProbReporter$new()
  expect_error(reporter$export_excel(prob$last_results, prob$nodes,
                                     tempfile(fileext = ".xlsx"), NULL),
               "ExcelExporter")
})
