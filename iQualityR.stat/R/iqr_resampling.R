# =============================================================================
# File: R/iqr_resampling.R
# Description: Resampling user entry point (L3 integrator).
#              Per Contract 2: exposes the unified 5-method R6 surface
#              ($new / $run / $plot / $interpret / $report) plus 4 convenience
#              functions (resampling_run / resampling_plot /
#              resampling_interpret / resampling_report). $run returns a
#              stat_result (resampling_result); $plot / $report delegate to L2
#              with the fixed signature.
# =============================================================================

#' @title iqr_resampling: Resampling entry class
#' @description
#' Top-level L3 integrator for the iQualityR resampling module, coordinating
#' computation (L1 `ResamplingAnalyzer`), plotting (L2 `ResamplingPlotter`),
#' reporting (L2 `ResamplingReporter`), and interpretation (`StatInterpreter`).
#'
#' **Supported resampling types**:
#' - `bootstrap_ci`: Bootstrap confidence interval for any scalar statistic
#'   (BCa / percentile / basic / normal). Pass a `statistic` function and the
#'   `data` it operates on.
#' - `permutation_test`: Permutation (randomization) test for two-sample /
#'   paired / one-sample designs.
#'
#' **Dual interface design** (per Contract 2):
#' - R6 class interface: `iqr_resampling$new()$run()$plot()` (chainable)
#' - Convenience function interface: `resampling_run()`, `resampling_plot()`,
#'   `resampling_interpret()`, `resampling_report()` (one-time use)
#'
#' @examples
#' # 95% BCa bootstrap CI for the median of a process sample
#' set.seed(123)
#' x <- rnorm(50, mean = 100, sd = 5)
#' rs <- iqr_resampling$new()
#' rs$run("bootstrap_ci", statistic = median, data = x, R = 999, seed = 1)
#' rs$interpret(audience = "manager")
#'
#' # Bootstrap CI for a regression coefficient (data-frame resampling)
#' df <- data.frame(y = 2 + 3 * x + rnorm(50, sd = 1), x = x)
#' rs$run("bootstrap_ci",
#'        statistic = function(d) coef(lm(y ~ x, data = d))["x"],
#'        data = df, R = 999, method = "perc", seed = 1)
#'
#' # Two-sample permutation test: do two lines have the same mean?
#' set.seed(123)
#' g1 <- rnorm(20, mean = 50, sd = 5)
#' g2 <- rnorm(20, mean = 54, sd = 5)
#' rs$run("permutation_test", x = g1, y = g2, R = 999, seed = 1)
#'
#' # Paired permutation test (sign-flipping on paired differences)
#' rs$run("permutation_test", x = g1, y = g2, paired = TRUE, R = 999, seed = 1)
#'
#' # One-sample permutation test against mu = 50
#' rs$run("permutation_test", x = g1, mu = 50, R = 999, seed = 1)
#'
#' # Plotting requires the iQualityR.plot Suggests package
#' if (requireNamespace("iQualityR.plot", quietly = TRUE)) {
#'   rs$plot()
#' }
#'
#' @export
iqr_resampling <- R6::R6Class("iqr_resampling",
  public = list(
    #' @field last_results Cached computation result (a `stat_result` S3 object).
    last_results = NULL,
    #' @field analyzer L1 computation engine (`ResamplingAnalyzer`).
    analyzer = NULL,
    #' @field plotter L2 plotting engine (`ResamplingPlotter`).
    plotter = NULL,
    #' @field reporter L2 reporting engine (`ResamplingReporter`).
    reporter = NULL,
    #' @field interpreter L2 interpreter (`StatInterpreter`).
    interpreter = NULL,
    #' @field theme_obj Active `IqrTheme` object.
    theme_obj = NULL,

    #' @description Initialize the resampling module
    #' @param theme Theme name or `IqrTheme` object.
    #' @return An `iqr_resampling` object (invisibly).
    initialize = function(theme = "academic") {
      self$analyzer    <- ResamplingAnalyzer$new()
      self$plotter     <- ResamplingPlotter$new(theme = theme)
      self$reporter    <- ResamplingReporter$new(theme = theme)
      self$interpreter <- StatInterpreter$new()
      self$theme_obj   <- .resolve_theme(theme)
      invisible(self)
    },

    #' @description Execute a resampling procedure
    #'
    #' Runs the L1 analyzer and caches the `stat_result` on
    #' `self$last_results`. Optionally prints a console report and/or plots.
    #'
    #' @param resample_type Resample type code (`"bootstrap_ci"` or
    #'   `"permutation_test"`).
    #' @param ... Parameters forwarded to `ResamplingAnalyzer$analyze()`.
    #' @param plot Logical; print a plot immediately after running.
    #' @param plot_type Plot type forwarded to `$plot()`.
    #' @param interpret Logical; print an interpretation immediately.
    #' @param audience Audience level for the interpretation.
    #' @return Invisible self (for chaining).
    run = function(resample_type, ..., plot = FALSE, plot_type = "auto",
                   interpret = FALSE, audience = "manager") {
      self$last_results <- self$analyzer$analyze(resample_type, ...)
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
    #' @param plot_type One of `"auto"`, `"hist"`, `"density"`, `"qq"`.
    #' @param show_table Logical; annotate observed statistic / CI / p-value.
    #' @param theme_obj Optional `IqrTheme` override.
    #' @return A `ggplot` object.
    plot = function(plot_type = "auto", show_table = FALSE, theme_obj = NULL) {
      if (is.null(self$last_results)) {
        stop("[iqr_resampling] Please run $run() first.", call. = FALSE)
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
        stop("[iqr_resampling] Please run $run() first.", call. = FALSE)
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
        stop("[iqr_resampling] Please run $run() first.", call. = FALSE)
      }
      self$reporter$report(
        result   = self$last_results,
        format   = format,
        path     = path,
        audience = audience
      )
    },

    #' @description One-click analysis (run + plot + interpret)
    #' @param resample_type Resample type code.
    #' @param ... Resampling parameters.
    #' @param plot_type Plot type.
    #' @param audience Audience level.
    #' @return Invisible self (for chaining).
    analyze = function(resample_type, ..., plot_type = "auto",
                       audience = "manager") {
      self$run(resample_type, ..., plot = TRUE, plot_type = plot_type,
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

#' @title Convenience resampling function
#' @description
#' Execute a resampling procedure without creating an R6 object. Returns a
#' `stat_result` S3 object so the caller can still feed it to
#' `resampling_plot()` / `resampling_interpret()` / `resampling_report()`.
#'
#' @param resample_type Resample type code (see [iqr_resampling]).
#' @param ... Parameters forwarded to `ResamplingAnalyzer$analyze()`.
#' @param plot Logical; print a plot immediately.
#' @param plot_type Plot type forwarded to `resampling_plot()`.
#' @param interpret Logical; print an interpretation immediately.
#' @param audience Audience level for the interpretation.
#' @param theme Theme name or `IqrTheme` object.
#' @return A `stat_result` S3 object (invisibly).
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(50, mean = 100, sd = 5)
#' result <- resampling_run("bootstrap_ci", statistic = median,
#'                          data = x, R = 999, seed = 1)
#' print(result)
resampling_run <- function(resample_type, ..., plot = FALSE,
                           plot_type = "auto", interpret = FALSE,
                           audience = "manager", theme = "academic") {
  rs <- iqr_resampling$new(theme = theme)
  rs$run(resample_type, ..., plot = plot, plot_type = plot_type,
         interpret = interpret, audience = audience)
  invisible(rs$last_results)
}

#' @title Convenience resampling plotting function
#' @description
#' Plot a `stat_result` returned by [resampling_run()] (or by
#' `ResamplingAnalyzer` directly).
#'
#' @param result A `stat_result` from [resampling_run()] or
#'   `ResamplingAnalyzer`.
#' @param plot_type One of `"auto"`, `"hist"`, `"density"`, `"qq"`.
#' @param show_table Logical; annotate observed statistic / CI / p-value.
#' @param theme Theme name or `IqrTheme` object.
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("iQualityR.plot", quietly = TRUE)) {
#'   set.seed(123)
#'   x <- rnorm(50, mean = 100, sd = 5)
#'   result <- resampling_run("bootstrap_ci", statistic = mean,
#'                            data = x, R = 499, seed = 1)
#'   resampling_plot(result, plot_type = "hist")
#' }
resampling_plot <- function(result, plot_type = "auto", show_table = FALSE,
                            theme = "academic") {
  plotter <- ResamplingPlotter$new(theme = theme)
  plotter$plot(result, plot_type = plot_type, show_table = show_table)
}

#' @title Convenience resampling interpretation function
#' @description
#' Interpret a `stat_result` returned by [resampling_run()] without creating
#' an R6 object.
#'
#' @param result A `stat_result` from [resampling_run()] or
#'   `ResamplingAnalyzer`.
#' @param audience Audience level (`"manager"`, `"technical"`, `"client"`).
#' @return Interpretation string (invisibly; also cat'd to stdout).
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(50, mean = 100, sd = 5)
#' result <- resampling_run("bootstrap_ci", statistic = mean,
#'                          data = x, R = 499, seed = 1)
#' resampling_interpret(result, audience = "manager")
resampling_interpret <- function(result, audience = "manager") {
  interpreter <- StatInterpreter$new()
  explanation <- interpreter$interpret(result, audience = audience)
  cat(explanation, "\n")
  invisible(explanation)
}

#' @title Convenience resampling report function
#' @description
#' Report a `stat_result` returned by [resampling_run()] without creating an
#' R6 object. Dispatches on `format` to console / data.frame / excel output.
#'
#' @param result A `stat_result` from [resampling_run()] or
#'   `ResamplingAnalyzer`.
#' @param format Output format: `"data.frame"` (default), `"console"`,
#'   `"excel"`.
#' @param path File path for `format = "excel"`.
#' @param audience Audience level for console interpretation.
#' @param theme Theme name or `IqrTheme` object (for Excel styling).
#' @return For `"data.frame"`: a data frame. Otherwise invisible NULL.
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(50, mean = 100, sd = 5)
#' result <- resampling_run("bootstrap_ci", statistic = mean,
#'                          data = x, R = 499, seed = 1)
#' resampling_report(result, format = "data.frame")
resampling_report <- function(result,
                              format = c("data.frame", "console", "excel"),
                              path = NULL, audience = "manager",
                              theme = "academic") {
  reporter <- ResamplingReporter$new(theme = theme)
  reporter$report(result, format = format, path = path, audience = audience)
}
