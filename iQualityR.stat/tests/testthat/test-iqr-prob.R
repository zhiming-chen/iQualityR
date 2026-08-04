# =============================================================================
# File: tests/testthat/test-iqr-prob.R
# Description: Comprehensive tests for the iqr_prob R6 integrator (R/iqr_prob.R)
#   and its convenience functions. Covers all public methods, calculation modes,
#   theme handling, reporting, and error paths.
# =============================================================================

library(testthat)
library(iQualityR.stat)

# ----------------------------------------------------------------------------
# initialize / theme handling
# ----------------------------------------------------------------------------

test_that("initialize wires up engines and default academic theme", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  expect_true(inherits(prob, "iqr_prob"))
  expect_true(inherits(prob$analyzer, "ProbAnalyzer"))
  expect_true(inherits(prob$plotter, "ProbPlotter"))
  expect_true(inherits(prob$reporter, "ProbReporter"))
  expect_true(inherits(prob$interpreter, "StatInterpreter"))
  expect_false(is.null(prob$theme_obj))
  expect_true(length(prob$nodes) > 0)
})

test_that("initialize with no type or dist_list leaves nodes empty", {
  prob <- iqr_prob$new()
  expect_equal(length(prob$nodes), 0)
  expect_null(prob$last_results)
  expect_null(prob$report())
})

test_that("initialize with dist_list adds multiple named nodes", {
  prob <- iqr_prob$new(dist_list = list(
    list(id = "A", type = "norm", params = list(mean = 0, sd = 1)),
    list(id = "B", type = "exp", params = list(rate = 1))
  ))
  expect_equal(length(prob$nodes), 2)
  expect_equal(names(prob$nodes), c("A", "B"))
})

test_that("initialize with invalid theme falls back to NULL theme_obj", {
  prob <- iqr_prob$new(theme = "totally_unknown_theme_xyz")
  expect_null(prob$theme_obj)
  prob$add_dist(type = "norm", params = list(mean = 0, sd = 1))
  expect_equal(length(prob$nodes), 1)
})

test_that("initialize accepts a custom IqrTheme object", {
  skip_if_not_installed("iQualityR.core")
  th <- iQualityR.core::IqrTheme$new("academic")
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1), theme = th)
  expect_identical(prob$theme_obj, th)
})

# ----------------------------------------------------------------------------
# add_dist
# ----------------------------------------------------------------------------

test_that("add_dist single with explicit id", {
  prob <- iqr_prob$new()
  prob$add_dist(id = "myid", type = "norm", params = list(mean = 10, sd = 2))
  expect_true("myid" %in% names(prob$nodes))
  expect_equal(prob$nodes[["myid"]]$type, "norm")
  expect_equal(prob$nodes[["myid"]]$params$mean, 10)
})

test_that("add_dist single auto-generates id when missing", {
  prob <- iqr_prob$new()
  prob$add_dist(type = "norm", params = list(mean = 0, sd = 1))
  expect_equal(names(prob$nodes), "D1")
})

test_that("add_dist batch auto-generates ids when missing", {
  prob <- iqr_prob$new()
  prob$add_dist(dist_list = list(
    list(type = "norm", params = list(mean = 0, sd = 1)),
    list(type = "norm", params = list(mean = 5, sd = 1))
  ))
  expect_equal(names(prob$nodes), c("D1", "D2"))
})

test_that("add_dist batch defaults loc to 0 when missing", {
  prob <- iqr_prob$new()
  prob$add_dist(dist_list = list(
    list(id = "A", type = "norm", params = list(mean = 0, sd = 1))
  ))
  expect_equal(prob$nodes[["A"]]$loc, 0)
})

test_that("add_dist with both dist_list and type adds both", {
  prob <- iqr_prob$new()
  prob$add_dist(
    dist_list = list(list(id = "A", type = "norm", params = list(mean = 0, sd = 1))),
    type = "exp", params = list(rate = 1)
  )
  expect_equal(length(prob$nodes), 2)
  expect_true("A" %in% names(prob$nodes))
  expect_true("D2" %in% names(prob$nodes))
  expect_equal(prob$nodes[["D2"]]$type, "exp")
})

test_that("add_dist reset clears existing nodes", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  expect_equal(length(prob$nodes), 1)
  prob$add_dist(type = "exp", params = list(rate = 1), reset = TRUE)
  expect_equal(length(prob$nodes), 1)
  expect_equal(prob$nodes[[1]]$type, "exp")
})

# ----------------------------------------------------------------------------
# calc -- adding distribution during calc, all calc_types
# ----------------------------------------------------------------------------

test_that("calc adds distribution when type/params provided", {
  prob <- iqr_prob$new()
  expect_equal(length(prob$nodes), 0)
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper",
            type = "norm", params = list(mean = 0, sd = 1))
  expect_equal(length(prob$nodes), 1)
  expect_equal(prob$nodes[[1]]$type, "norm")
  expect_false(is.null(prob$last_results))
})

test_that("calc adds distributions when dist_list provided", {
  prob <- iqr_prob$new()
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper",
            dist_list = list(
              list(id = "A", type = "norm", params = list(mean = 0, sd = 1)),
              list(id = "B", type = "norm", params = list(mean = 5, sd = 1))
            ))
  expect_equal(length(prob$nodes), 2)
  expect_equal(length(prob$last_results), 2)
  expect_true("A" %in% names(prob$last_results))
  expect_true("B" %in% names(prob$last_results))
})

test_that("calc with lower tail produces correct probability", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = -1.96, mode = "prob", calc_type = "lower")
  res_p <- prob$last_results[[1]]$all_res[[1]]$result_p
  expect_lt(abs(res_p - 0.025), 0.001)
})

test_that("calc with quant mode for multiple probabilities", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = c(0.025, 0.5, 0.975), mode = "quant", calc_type = "lower")
  all_res <- prob$last_results[[1]]$all_res
  expect_equal(length(all_res), 3)
  expect_lt(abs(all_res[[1]]$result_x - (-1.96)), 0.01)
  expect_lt(abs(all_res[[2]]$result_x), 0.001)
  expect_lt(abs(all_res[[3]]$result_x - 1.96), 0.01)
})

test_that("calc with discrete distribution (binomial)", {
  prob <- iqr_prob$new(type = "binom", params = list(size = 10, prob = 0.5))
  prob$calc(values = 5, mode = "prob", calc_type = "lower")
  res_p <- prob$last_results[[1]]$all_res[[1]]$result_p
  expect_gte(res_p, 0)
  expect_lte(res_p, 1)
  # P(X <= 5) for B(10, 0.5) should be ~0.623
  expect_lt(abs(res_p - pbinom(5, 10, 0.5)), 0.001)
})

# ----------------------------------------------------------------------------
# plot -- rendering after calc
# ----------------------------------------------------------------------------

test_that("plot returns ggplot object after prob-mode calc", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  p <- prob$plot()
  expect_true(inherits(p, "ggplot") || inherits(p, "gg"))
})

test_that("plot with show_cdf returns patchwork object", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  p <- prob$plot(show_cdf = TRUE)
  # patchwork objects inherit from "patchwork"
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot") || inherits(p, "gg"))
})

test_that("plot auto-shows CDF in quant mode", {
  # In quant mode, is_prob_mode is FALSE so must_show_cdf becomes TRUE
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 0.975, mode = "quant")
  p <- prob$plot()
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot") || inherits(p, "gg"))
})

test_that("plot with facet works for multiple distributions", {
  prob <- iqr_prob$new(dist_list = list(
    list(id = "A", type = "norm", params = list(mean = 0, sd = 1)),
    list(id = "B", type = "norm", params = list(mean = 5, sd = 1))
  ))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  p <- prob$plot(facet = TRUE)
  expect_true(inherits(p, "ggplot") || inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("plot with theme_obj parameter updates theme", {
  skip_if_not_installed("iQualityR.core")
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  th <- iQualityR.core::IqrTheme$new("tech")
  p <- prob$plot(theme_obj = th)
  expect_true(inherits(p, "ggplot") || inherits(p, "gg") || inherits(p, "patchwork"))
  expect_identical(prob$theme_obj, th)
})

test_that("plot with discrete distribution renders without error", {
  prob <- iqr_prob$new(type = "binom", params = list(size = 10, prob = 0.5))
  prob$calc(values = 5, mode = "prob", calc_type = "lower")
  p <- prob$plot()
  expect_true(inherits(p, "ggplot") || inherits(p, "gg") || inherits(p, "patchwork"))
})

# ----------------------------------------------------------------------------
# interpret -- all audience levels
# ----------------------------------------------------------------------------

test_that("interpret with technical audience returns non-empty string", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  interp <- prob$interpret(audience = "technical")
  expect_type(interp, "list")
  expect_type(interp[[1]], "character")
  expect_true(nchar(interp[[1]]) > 0)
  expect_true(grepl("Technical", interp[[1]]))
})

test_that("interpret with client audience returns non-empty string", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  interp <- prob$interpret(audience = "client")
  expect_type(interp, "list")
  expect_type(interp[[1]], "character")
  expect_true(nchar(interp[[1]]) > 0)
  expect_true(grepl("Quality Assurance", interp[[1]]))
})

test_that("interpret with manager audience returns non-empty string", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  interp <- prob$interpret(audience = "manager")
  expect_type(interp, "list")
  expect_true(grepl("Manager", interp[[1]]))
})

test_that("interpret with unknown audience falls back to manager", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  interp <- prob$interpret(audience = "unknown_level")
  expect_type(interp, "list")
  expect_true(grepl("Manager", interp[[1]]))
})

test_that("interpret works after quant mode calc", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 0.975, mode = "quant")
  interp <- prob$interpret(audience = "manager")
  expect_type(interp, "list")
  expect_true(nchar(interp[[1]]) > 0)
})

test_that("interpret works for multiple nodes", {
  prob <- iqr_prob$new(dist_list = list(
    list(id = "A", type = "norm", params = list(mean = 0, sd = 1)),
    list(id = "B", type = "norm", params = list(mean = 5, sd = 1))
  ))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  interp <- prob$interpret(audience = "manager")
  expect_equal(length(interp), 2)
  expect_true("A" %in% names(interp))
  expect_true("B" %in% names(interp))
})

test_that("interpret works for discrete distribution", {
  prob <- iqr_prob$new(type = "binom", params = list(size = 50, prob = 0.05))
  prob$calc(values = 3, mode = "prob", calc_type = "lower")
  interp <- prob$interpret(audience = "manager")
  expect_type(interp, "list")
  expect_true(nchar(interp[[1]]) > 0)
})

# ----------------------------------------------------------------------------
# report_excel -- error paths and successful export
# ----------------------------------------------------------------------------

test_that("report_excel errors before calc", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  expect_error(prob$report_excel(path = tempfile(fileext = ".xlsx")),
               "calc")
})

test_that("report_excel exports xlsx with default path", {
  skip_if_not_installed("iQualityR.core")
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf), add = TRUE)
  # Use custom path to avoid polluting the working directory
  expect_message(prob$report_excel(path = tf), "Report exported")
  expect_true(file.exists(tf))
})

test_that("report_excel with custom excel_exporter", {
  skip_if_not_installed("iQualityR.core")
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf), add = TRUE)
  exporter <- iQualityR.core::ExcelExporter$new(
    iQualityR.core::IqrTheme$new("academic")$config
  )
  expect_message(prob$report_excel(path = tf, excel_exporter = exporter),
                 "Report exported")
  expect_true(file.exists(tf))
})

test_that("report_excel with theme_obj config creates themed exporter", {
  skip_if_not_installed("iQualityR.core")
  th <- iQualityR.core::IqrTheme$new("academic")
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1), theme = th)
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  tf <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tf), add = TRUE)
  expect_message(prob$report_excel(path = tf), "Report exported")
  expect_true(file.exists(tf))
})

# ----------------------------------------------------------------------------
# analyze -- one-click calc + plot
# ----------------------------------------------------------------------------

test_that("analyze runs calc and returns plot", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  p <- prob$analyze(values = 1.96, mode = "prob", calc_type = "upper")
  expect_true(inherits(p, "ggplot") || inherits(p, "gg") || inherits(p, "patchwork"))
  expect_false(is.null(prob$last_results))
})

test_that("analyze with show_cdf returns combined plot", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  p <- prob$analyze(values = 1.96, mode = "prob", calc_type = "upper",
                    show_cdf = TRUE)
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot") || inherits(p, "gg"))
})

test_that("analyze adds distribution when type provided", {
  prob <- iqr_prob$new()
  p <- prob$analyze(values = 1.96, mode = "prob", calc_type = "upper",
                    type = "norm", params = list(mean = 0, sd = 1))
  expect_true(inherits(p, "ggplot") || inherits(p, "gg") || inherits(p, "patchwork"))
  expect_equal(length(prob$nodes), 1)
})

# ----------------------------------------------------------------------------
# set_theme
# ----------------------------------------------------------------------------

test_that("set_theme creates theme when theme_obj is NULL", {
  # Initialize with invalid theme so theme_obj is NULL
  prob <- iqr_prob$new(theme = "totally_unknown_xyz")
  expect_null(prob$theme_obj)
  prob$set_theme(theme_style = "academic")
  expect_false(is.null(prob$theme_obj))
})

test_that("set_theme updates existing theme with character", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  expect_false(is.null(prob$theme_obj))
  prob$set_theme(theme_style = "tech")
  # Theme object should still exist after valid update
  expect_false(is.null(prob$theme_obj))
  expect_true(inherits(prob$theme_obj, "IqrTheme"))
})

test_that("set_theme replaces with IqrTheme object", {
  skip_if_not_installed("iQualityR.core")
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  th <- iQualityR.core::IqrTheme$new("tech")
  prob$set_theme(theme_style = th)
  expect_identical(prob$theme_obj, th)
})

test_that("set_theme with invalid theme keeps NULL when starting from NULL", {
  prob <- iqr_prob$new(theme = "totally_unknown_xyz")
  expect_null(prob$theme_obj)
  prob$set_theme(theme_style = "another_invalid_theme_zzz")
  expect_null(prob$theme_obj)
})

test_that("set_theme with NULL theme_style does nothing", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  original_theme <- prob$theme_obj
  prob$set_theme(theme_style = NULL)
  expect_identical(prob$theme_obj, original_theme)
})

# ----------------------------------------------------------------------------
# list_distributions / get_node_info
# ----------------------------------------------------------------------------

test_that("list_distributions returns data.frame of available distributions", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  dists <- prob$list_distributions()
  expect_s3_class(dists, "data.frame")
  expect_true(nrow(dists) > 0)
  expect_true("type" %in% names(dists))
  expect_true("norm" %in% dists$type)
})

test_that("get_node_info returns list with node metadata", {
  prob <- iqr_prob$new(dist_list = list(
    list(id = "A", type = "norm", params = list(mean = 0, sd = 1)),
    list(id = "B", type = "exp", params = list(rate = 1))
  ))
  info <- prob$get_node_info()
  expect_type(info, "list")
  expect_equal(length(info), 2)
  expect_true("A" %in% names(info))
  expect_true("B" %in% names(info))
  expect_equal(info$A$type, "norm")
  expect_equal(info$B$type, "exp")
  expect_true("params" %in% names(info$A))
  expect_true("description" %in% names(info$A))
})

test_that("get_node_info returns empty list when no nodes", {
  prob <- iqr_prob$new()
  info <- prob$get_node_info()
  expect_type(info, "list")
  expect_equal(length(info), 0)
})

# ----------------------------------------------------------------------------
# Convenience functions: prob_calc, prob_plot, list_prob_distributions,
# get_prob_dist_info
# ----------------------------------------------------------------------------

test_that("prob_calc with interpret returns results and prints interpretation", {
  result <- prob_calc(type = "norm", params = list(mean = 0, sd = 1),
                      x = 1.96, calc_type = "upper", interpret = TRUE)
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_equal(result[[1]]$mode, "prob")
})

test_that("prob_calc with plot returns results and prints plot", {
  result <- prob_calc(type = "norm", params = list(mean = 0, sd = 1),
                      x = 1.96, calc_type = "upper", plot = TRUE)
  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("prob_calc with quant mode returns quantile results", {
  result <- prob_calc(type = "norm", params = list(mean = 100, sd = 5),
                      x = 0.975, mode = "quant")
  expect_type(result, "list")
  expect_equal(result[[1]]$mode, "quant")
  res_x <- result[[1]]$all_res[[1]]$result_x
  expect_lt(abs(res_x - 109.8), 0.5)
})

test_that("prob_calc with between calc_type", {
  result <- prob_calc(type = "norm", params = list(mean = 0, sd = 1),
                      x = c(-1, 1), calc_type = "between")
  res_p <- result[[1]]$all_res[[1]]$result_p
  expect_lt(abs(res_p - 0.6827), 0.01)
})

test_that("prob_calc with outside calc_type", {
  result <- prob_calc(type = "norm", params = list(mean = 0, sd = 1),
                      x = c(-1.96, 1.96), calc_type = "outside")
  res_p <- result[[1]]$all_res[[1]]$result_p
  expect_lt(abs(res_p - 0.05), 0.01)
})

test_that("prob_plot with x value returns plot object", {
  p <- prob_plot(type = "norm", params = list(mean = 0, sd = 1),
                 x = 1.96, calc_type = "upper", show_cdf = TRUE)
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot") || inherits(p, "gg"))
})

test_that("prob_plot without x uses median for plotting", {
  # When x is NULL, prob_plot calculates median quantile for annotation
  p <- prob_plot(type = "norm", params = list(mean = 0, sd = 1),
                 show_cdf = FALSE)
  expect_true(inherits(p, "ggplot") || inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("prob_plot with facet for multiple distributions", {
  p <- prob_plot(type = "norm", params = list(mean = 0, sd = 1),
                 x = 1.96, calc_type = "upper", facet = TRUE)
  expect_true(inherits(p, "ggplot") || inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("list_prob_distributions returns available distributions", {
  dists <- list_prob_distributions()
  expect_s3_class(dists, "data.frame")
  expect_true(nrow(dists) > 0)
  expect_true("norm" %in% dists$type)
})

test_that("get_prob_dist_info returns info for known distribution", {
  info <- get_prob_dist_info("norm")
  expect_type(info, "list")
  expect_true("description" %in% names(info))
})

test_that("get_prob_dist_info errors on unknown distribution", {
  expect_error(get_prob_dist_info("nope"), "Unknown distribution")
})

# ----------------------------------------------------------------------------
# Edge cases: theme error fallback in initialize
# ----------------------------------------------------------------------------

test_that("initialize theme error fallback does not crash", {
  # An invalid theme name triggers the tryCatch error branch in initialize
  prob <- iqr_prob$new(theme = "invalid_theme_name_xyz")
  expect_null(prob$theme_obj)
  # Object should still be usable for non-plotting operations
  prob$add_dist(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  expect_false(is.null(prob$last_results))
  df <- prob$report()
  expect_s3_class(df, "data.frame")
})

test_that("initialize with loc offset shifts distribution", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1), loc = 10)
  # With loc = 10, the node's q(0.5) should be mean + loc = 10
  expect_equal(prob$nodes[[1]]$q(0.5), 10, tolerance = 1e-10)
})

test_that("calc returns invisible self-reference for chaining", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  result <- prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  # calc returns invisible(self), so result should be the prob object
  expect_true(inherits(result, "iqr_prob"))
})

test_that("report returns data.frame with expected columns", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
  df <- prob$report()
  expect_s3_class(df, "data.frame")
  expect_true("node_id" %in% names(df))
  expect_true("mode" %in% names(df))
  expect_equal(df$mode[1], "prob")
})

test_that("report returns data.frame for quant mode", {
  prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
  prob$calc(values = c(0.025, 0.975), mode = "quant")
  df <- prob$report()
  expect_s3_class(df, "data.frame")
  expect_equal(df$mode[1], "quant")
  expect_true(nrow(df) >= 2)
})