# =============================================================================
# File: R/iqr_intervals.R
# Description: Interval estimation user entry point (L3 integrator).
#              Per Contract 2: exposes the unified 5-method R6 surface
#              ($new / $run / $plot / $interpret / $report) plus 4 convenience
#              functions (intervals_run / intervals_plot / intervals_interpret /
#              intervals_report). $run returns a stat_result (interval_result);
#              $plot / $report delegate to L2 with the fixed signature.
# =============================================================================

#' @title iqr_intervals: Interval estimation entry class
#' @description
#' Top-level L3 integrator for the iQualityR interval-estimation module,
#' coordinating computation (L1 `IntervalAnalyzer`), plotting
#' (L2 `IntervalPlotter`), reporting (L2 `IntervalReporter`), and
#' interpretation (`StatInterpreter`).
#'
#' **Supported interval types**:
#' - `ci_mean`: Confidence interval for a population mean (t / z)
#' - `ci_proportion`: Confidence interval for a population proportion
#' - `ci_variance`: Confidence interval for a population variance (chi-sq)
#' - `ci_diff_mean`: Confidence interval for the difference of two means
#' - `tolerance_interval`: Tolerance interval (k-content, p-coverage)
#' - `margin_of_error`: Margin of error (mean / proportion)
#' - `pi_mean`: Prediction interval for one future observation
#'
#' **Dual interface design** (per Contract 2):
#' - R6 class interface: `iqr_intervals$new()$run()$plot()` (chainable)
#' - Convenience function interface: `intervals_run()`, `intervals_plot()`,
#'   `intervals_interpret()`, `intervals_report()` (one-time use)
#'
#' @examples
#' # 95% CI for the mean of a process sample
#' set.seed(123)
#' x <- rnorm(30, mean = 100, sd = 5)
#' iv <- iqr_intervals$new()
#' iv$run("ci_mean", x = x, conf_level = 0.95)
#' iv$interpret(audience = "manager")
#'
#' # 99% / 95% tolerance interval (capture 99% of the population, 95% confidence)
#' iv$run("tolerance_interval", x = x, p = 0.99, conf_level = 0.95)
#'
#' # CI for a defect proportion (12 defects out of 200 units)
#' iv$run("ci_proportion", x = 12, n = 200, method = "wald")
#'
#' # Margin of error for the mean
#' iv$run("margin_of_error", x = x, type = "mean")
#'
#' # Prediction interval for the next single measurement
#' iv$run("pi_mean", x = x, conf_level = 0.95)
#'
#' # CI for the difference of two means (two production lines)
#' set.seed(123)
#' y <- rnorm(30, mean = 102, sd = 5)
#' iv$run("ci_diff_mean", x = x, y = y)
#'
#' # Plotting requires the iQualityR.plot Suggests package
#' if (requireNamespace("iQualityR.plot", quietly = TRUE)) {
#'   iv$plot()
#' }
#'
#' @export
iqr_intervals <- R6::R6Class("iqr_intervals",
  public = list(
    #' @field last_results Cached computation result (a `stat_result` S3 object).
    last_results = NULL,
    #' @field analyzer L1 computation engine (`IntervalAnalyzer`).
    analyzer = NULL,
    #' @field plotter L2 plotting engine (`IntervalPlotter`).
    plotter = NULL,
    #' @field reporter L2 reporting engine (`IntervalReporter`).
    reporter = NULL,
    #' @field interpreter L2 interpreter (`StatInterpreter`).
    interpreter = NULL,
    #' @field theme_obj Active `IqrTheme` object.
    theme_obj = NULL,

    #' @description Initialize the interval-estimation module
    #' @param theme Theme name or `IqrTheme` object.
    #' @return An `iqr_intervals` object (invisibly).
    initialize = function(theme = "academic") {
      self$analyzer    <- IntervalAnalyzer$new()
      self$plotter     <- IntervalPlotter$new(theme = theme)
      self$reporter    <- IntervalReporter$new(theme = theme)
      self$interpreter <- StatInterpreter$new()
      self$theme_obj   <- .resolve_theme(theme)
      invisible(self)
    },

    #' @description Execute an interval estimation
    #'
    #' Runs the L1 analyzer and caches the `stat_result` on
    #' `self$last_results`. Optionally prints a console report and/or plots.
    #'
    #' @param interval_type Interval type code (see class description).
    #' @param ... Parameters forwarded to `IntervalAnalyzer$analyze()`.
    #' @param plot Logical; print a plot immediately after running.
    #' @param plot_type Plot type forwarded to `$plot()`.
    #' @param interpret Logical; print an interpretation immediately.
    #' @param audience Audience level for the interpretation.
    #' @return Invisible self (for chaining).
    run = function(interval_type, ..., plot = FALSE, plot_type = "auto",
                   interpret = FALSE, audience = "manager") {
      self$last_results <- self$analyzer$analyze(interval_type, ...)
      self$reporter$print_console(self$last_results, interpret = FALSE)
      if (plot) {
        p <- self$plot(plot_type = plot_type)
        print(p)
      }
      if (interpret) {
        self$interpret(audience = audience)
      }
      invisible(self)
    },

    #' @description Plot the last result (Contract 2 signature)
    #' @param plot_type One of `"auto"`, `"errorbar"`, `"histogram"`, `"bar"`.
    #' @param show_table Logical; annotate interval bounds.
    #' @param theme_obj Optional `IqrTheme` override.
    #' @return A `ggplot` object.
    plot = function(plot_type = "auto", show_table = FALSE, theme_obj = NULL) {
      if (is.null(self$last_results)) {
        stop("[iqr_intervals] Please run $run() first.", call. = FALSE)
      }
      self$plotter$plot(
        result     = self$last_results,
        plot_type  = plot_type,
        show_table = show_table,
        theme_obj  = theme_obj %||% self$theme_obj
      )
    },

    #' @description Interpret the last result
    #' @param audience Audience level (`"manager"`, `"technical"`, `"client"`).
    #' @return Interpretation string (invisibly; also cat'd to stdout).
    interpret = function(audience = "manager") {
      if (is.null(self$last_results)) {
        stop("[iqr_intervals] Please run $run() first.", call. = FALSE)
      }
      explanation <- self$interpreter$interpret(
        self$last_results, audience = audience
      )
      cat(explanation, "\n")
      invisible(explanation)
    },

    #' @description Report the last result (Contract 2 signature)
    #' @param format Output format: `"data.frame"` (default), `"console"`,
    #'   `"excel"`.
    #' @param path File path for `format = "excel"`.
    #' @param audience Audience level for console interpretation.
    #' @return For `"data.frame"`: a data frame. Otherwise invisible NULL.
    report = function(format = c("data.frame", "console", "excel"),
                      path = NULL, audience = "manager") {
      if (is.null(self$last_results)) {
        stop("[iqr_intervals] Please run $run() first.", call. = FALSE)
      }
      self$reporter$report(
        result   = self$last_results,
        format   = format,
        path     = path,
        audience = audience
      )
    },

    #' @description One-click analysis (run + plot + interpret)
    #' @param interval_type Interval type code.
    #' @param ... Interval parameters.
    #' @param plot_type Plot type.
    #' @param audience Audience level.
    #' @return Invisible self (for chaining).
    analyze = function(interval_type, ..., plot_type = "auto",
                       audience = "manager") {
      self$run(interval_type, ..., plot = TRUE, plot_type = plot_type,
               interpret = TRUE, audience = audience)
      invisible(self)
    },

    #' @description Set / replace the active theme
    #' @param theme_style Theme name or `IqrTheme` object.
    #' @return Invisible self.
    set_theme = function(theme_style = NULL) {
      self$theme_obj <- .resolve_theme(theme_style %||% "academic")
      self$plotter$set_theme(self$theme_obj)
      self$reporter$theme_obj <- self$theme_obj
      invisible(self)
    }
  )
)


# =============================================================================
# Convenience functions (stateless interface, suitable for one-time use)
# Per Contract 2: 4 convenience functions per domain.
# =============================================================================

#' @title Convenience interval-estimation function
#' @description
#' Execute an interval estimation without creating an R6 object. Returns a
#' `stat_result` S3 object so the caller can still feed it to
#' `intervals_plot()` / `intervals_interpret()` / `intervals_report()`.
#'
#' @param interval_type Interval type code (see [iqr_intervals]).
#' @param ... Parameters forwarded to `IntervalAnalyzer$analyze()`.
#' @param plot Logical; print a plot immediately.
#' @param plot_type Plot type forwarded to `intervals_plot()`.
#' @param interpret Logical; print an interpretation immediately.
#' @param audience Audience level for the interpretation.
#' @param theme Theme name or `IqrTheme` object.
#' @return A `stat_result` S3 object (invisibly).
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 100, sd = 5)
#' result <- intervals_run("ci_mean", x = x)
#' print(result)
intervals_run <- function(interval_type, ..., plot = FALSE, plot_type = "auto",
                          interpret = FALSE, audience = "manager",
                          theme = "academic") {
  iv <- iqr_intervals$new(theme = theme)
  iv$run(interval_type, ..., plot = plot, plot_type = plot_type,
         interpret = interpret, audience = audience)
  invisible(iv$last_results)
}

#' @title Convenience interval-estimation plotting function
#' @description
#' Plot a `stat_result` returned by [intervals_run()] (or by
#' `IntervalAnalyzer` directly).
#'
#' @param result A `stat_result` from [intervals_run()] or `IntervalAnalyzer`.
#' @param plot_type One of `"auto"`, `"errorbar"`, `"histogram"`, `"bar"`.
#' @param show_table Logical; annotate interval bounds.
#' @param theme Theme name or `IqrTheme` object.
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("iQualityR.plot", quietly = TRUE)) {
#'   set.seed(123)
#'   x <- rnorm(30, mean = 100, sd = 5)
#'   result <- intervals_run("ci_mean", x = x)
#'   intervals_plot(result, plot_type = "histogram")
#' }
intervals_plot <- function(result, plot_type = "auto", show_table = FALSE,
                           theme = "academic") {
  plotter <- IntervalPlotter$new(theme = theme)
  plotter$plot(result, plot_type = plot_type, show_table = show_table)
}

#' @title Convenience interval-estimation interpretation function
#' @description
#' Interpret a `stat_result` returned by [intervals_run()] without creating
#' an R6 object.
#'
#' @param result A `stat_result` from [intervals_run()] or `IntervalAnalyzer`.
#' @param audience Audience level (`"manager"`, `"technical"`, `"client"`).
#' @return Interpretation string (invisibly; also cat'd to stdout).
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 100, sd = 5)
#' result <- intervals_run("ci_mean", x = x)
#' intervals_interpret(result, audience = "manager")
intervals_interpret <- function(result, audience = "manager") {
  interpreter <- StatInterpreter$new()
  explanation <- interpreter$interpret(result, audience = audience)
  cat(explanation, "\n")
  invisible(explanation)
}

#' @title Convenience interval-estimation report function
#' @description
#' Report a `stat_result` returned by [intervals_run()] without creating an
#' R6 object. Dispatches on `format` to console / data.frame / excel output.
#'
#' @param result A `stat_result` from [intervals_run()] or `IntervalAnalyzer`.
#' @param format Output format: `"data.frame"` (default), `"console"`, `"excel"`.
#' @param path File path for `format = "excel"`.
#' @param audience Audience level for console interpretation.
#' @param theme Theme name or `IqrTheme` object (for Excel styling).
#' @return For `"data.frame"`: a data frame. Otherwise invisible NULL.
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 100, sd = 5)
#' result <- intervals_run("ci_mean", x = x)
#' intervals_report(result, format = "data.frame")
intervals_report <- function(result, format = c("data.frame", "console", "excel"),
                             path = NULL, audience = "manager",
                             theme = "academic") {
  reporter <- IntervalReporter$new(theme = theme)
  reporter$report(result, format = format, path = path, audience = audience)
}
