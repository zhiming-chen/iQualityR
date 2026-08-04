# =============================================================================
# File: R/iqr_regression.R
# Description: Regression user entry point (L3 integrator).
# Per Contract 2: 5-method R6 + 4 convenience functions.
# =============================================================================

#' @title iqr_regression: Regression entry class
#' @description L3 integrator for regression.
#' Supported: lm_fit, logit_fit, poisson_fit, cox_fit, pls_fit, stepwise_fit,
#' best_subset_fit, mars_fit, spline_fit.
#' @examples
#' set.seed(123)
#' x <- rnorm(30); y <- 2 + 3 * x + rnorm(30, sd = 0.5)
#' df <- data.frame(y = y, x = x)
#' reg <- iqr_regression$new()
#' reg$run("lm_fit", formula = y ~ x, data = df)
#' reg$interpret(audience = "manager")
#' @export
iqr_regression <- R6::R6Class("iqr_regression",
  public = list(
    last_results = NULL, analyzer = NULL, plotter = NULL, reporter = NULL, interpreter = NULL, theme_obj = NULL,
    initialize = function(theme = "academic") {
      self$analyzer <- RegressionAnalyzer$new()
      self$plotter <- RegressionPlotter$new(theme = theme)
      self$reporter <- RegressionReporter$new(theme = theme)
      self$interpreter <- StatInterpreter$new()
      self$theme_obj <- .resolve_theme(theme)
      invisible(self)
    },
    run = function(model_type, ..., plot = FALSE, plot_type = "auto", interpret = FALSE, audience = "manager") {
      self$last_results <- self$analyzer$analyze(model_type, ...)
      self$reporter$print_console(self$last_results, interpret = FALSE)
      if (plot) print(self$plot(plot_type = plot_type))
      if (interpret) self$interpret(audience = audience)
      invisible(self)
    },
    plot = function(plot_type = "auto", show_table = FALSE, theme_obj = NULL) {
      if (is.null(self$last_results)) stop("[iqr_regression] Please run $run() first.", call. = FALSE)
      self$plotter$plot(result = self$last_results, plot_type = plot_type, show_table = show_table, theme_obj = theme_obj %||% self$theme_obj)
    },
    interpret = function(audience = "manager") {
      if (is.null(self$last_results)) stop("[iqr_regression] Please run $run() first.", call. = FALSE)
      explanation <- self$interpreter$interpret(self$last_results, audience = audience)
      cat(explanation, "\n"); invisible(explanation)
    },
    report = function(format = c("data.frame", "console", "excel"), path = NULL, audience = "manager") {
      if (is.null(self$last_results)) stop("[iqr_regression] Please run $run() first.", call. = FALSE)
      self$reporter$report(result = self$last_results, format = format, path = path, audience = audience)
    },
    analyze = function(model_type, ..., plot_type = "auto", audience = "manager") {
      self$run(model_type, ..., plot = TRUE, plot_type = plot_type, interpret = TRUE, audience = audience); invisible(self)
    },
    set_theme = function(theme_style = NULL) {
      self$theme_obj <- .resolve_theme(theme_style %||% "academic")
      self$plotter$set_theme(self$theme_obj); self$reporter$theme_obj <- self$theme_obj; invisible(self)
    }
  )
)

#' @title Convenience regression function
#' @param model_type Model type code (see [iqr_regression]).
#' @param ... Parameters forwarded to RegressionAnalyzer$analyze().
#' @param plot Logical; print a plot immediately.
#' @param plot_type Plot type.
#' @param interpret Logical; print interpretation.
#' @param audience Audience level.
#' @param theme Theme name or IqrTheme.
#' @return A stat_result S3 object (invisibly).
#' @export
#' @examples
#' set.seed(123); x <- rnorm(30); y <- 2 + 3*x + rnorm(30, sd=0.5)
#' result <- regression_run("lm_fit", formula = y ~ x, data = data.frame(y=y, x=x))
regression_run <- function(model_type, ..., plot = FALSE, plot_type = "auto", interpret = FALSE, audience = "manager", theme = "academic") {
  reg <- iqr_regression$new(theme = theme)
  reg$run(model_type, ..., plot = plot, plot_type = plot_type, interpret = interpret, audience = audience)
  invisible(reg$last_results)
}

#' @title Convenience regression plotting function
#' @param result A stat_result from [regression_run()].
#' @param plot_type One of "auto", "residual", "coef".
#' @param show_table Logical.
#' @param theme Theme name or IqrTheme.
#' @return A ggplot object.
#' @export
regression_plot <- function(result, plot_type = "auto", show_table = FALSE, theme = "academic") {
  plotter <- RegressionPlotter$new(theme = theme)
  plotter$plot(result, plot_type = plot_type, show_table = show_table)
}

#' @title Convenience regression interpretation function
#' @param result A stat_result from [regression_run()].
#' @param audience Audience level.
#' @return Interpretation string (invisibly).
#' @export
regression_interpret <- function(result, audience = "manager") {
  interpreter <- StatInterpreter$new()
  explanation <- interpreter$interpret(result, audience = audience)
  cat(explanation, "\n"); invisible(explanation)
}

#' @title Convenience regression report function
#' @param result A stat_result from [regression_run()].
#' @param format Output format.
#' @param path File path for excel.
#' @param audience Audience level.
#' @param theme Theme name or IqrTheme.
#' @return For data.frame: a data frame. Otherwise invisible NULL.
#' @export
regression_report <- function(result, format = c("data.frame", "console", "excel"), path = NULL, audience = "manager", theme = "academic") {
  reporter <- RegressionReporter$new(theme = theme)
  reporter$report(result, format = format, path = path, audience = audience)
}
