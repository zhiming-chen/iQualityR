# =============================================================================
# File: R/htest/iqr_htest.R
# Description: Hypothesis test module user entry point (integrator + convenience functions)
# =============================================================================

#' @title iqr_htest: Hypothesis test entry class
#' @description
#' Top-level interface for iQualityR hypothesis test module, coordinating computation, plotting, reporting, and interpretation.
#'
#' **Supported test types**:
#' - `z_test_1s`: One-sample Z test (population standard deviation known)
#' - `t_test_1s`: One-sample t test (population standard deviation unknown)
#' - `t_test_2s`: Two-sample t test (independent samples)
#' - `t_test_paired`: Paired t test
#' - `prop_test_1s`: One-sample proportion test
#' - `prop_test_2s`: Two-sample proportion test
#' - `f_test`: Variance equality test (F test)
#' - `chisq_test`: Chi-square test
#'
#' **Output system**:
#' - Calculation: Test statistic, P-value, confidence interval
#' - Graphics: Rejection region plot, box plot, combined plot
#' - Interpretation: Manager/technical/client three versions
#' - Report: Console, data frame, Excel
#'
#' **Dual interface design**:
#' - R6 class interface: `iqr_htest$new()$run()$plot()` (suitable for chaining)
#' - Convenience function interface: `htest_run()`, `htest_plot()` (suitable for one-time use)
#'
#' @examples
#' \dontrun{
#' # === R6 class interface ===
#' # One-sample t test
#' set.seed(123)
#' x <- rnorm(30, mean = 102, sd = 5)
#' htest <- iqr_htest$new()
#' htest$run("t_test_1s", x = x, mu = 100, alternative = "two.sided")
#' htest$plot()
#' htest$interpret(audience = "manager")
#'
#' # One-sample Z test (sigma known)
#' htest$run("z_test_1s", x = x, mu = 100, sigma = 5)
#'
#' # Two-sample t test
#' y <- rnorm(25, mean = 98, sd = 6)
#' htest$run("t_test_2s", x = x, y = y)
#'
#' # === Convenience function interface ===
#' htest_run("t_test_1s", x = x, mu = 100)
#' }
#'
#' @export
iqr_htest <- R6::R6Class("iqr_htest",
  public = list(
    #' @field last_results Cached computation results
    last_results = NULL,
    #' @field last_data Cached sample data (needed for plotting)
    last_data = list(),
    #' @field analyzer Computation engine
    analyzer = NULL,
    #' @field plotter Plotting engine
    plotter = NULL,
    #' @field reporter Report engine
    reporter = NULL,
    #' @field interpreter Interpreter engine
    interpreter = NULL,
    #' @field theme_obj Theme object
    theme_obj = NULL,

    #' @description Initialize hypothesis test module
    #' @param theme Theme name or IqrTheme object
    #' @param ... Other parameters
    #' @return iqr_htest object
    initialize = function(theme = "academic", ...) {
      self$analyzer     <- HTestAnalyzer$new()
      self$plotter      <- HTestPlotter$new(theme = theme)
      self$reporter     <- HTestReporter$new()
      self$interpreter  <- StatInterpreter$new()

      if (inherits(theme, "IqrTheme")) {
        self$theme_obj <- theme
      } else {
        tryCatch({
          self$theme_obj <- IqrTheme$new(theme, ...)
        }, error = function(e) {
          self$theme_obj <<- NULL
        })
      }
    },

    #' @description Execute hypothesis test
    #' @param test_type Test type
    #' @param ... Test parameters
    #' @param plot Whether to plot
    #' @param plot_type Plot type
    #' @param interpret Whether to output interpretation
    #' @param audience Audience level
    #' @return Invisible self-reference
    #' @examples
    #' \dontrun{
    #' htest$run("t_test_1s", x = x, mu = 100)
    #' htest$run("z_test_1s", x = x, mu = 100, sigma = 5, plot = TRUE)
    #' }
    run = function(test_type, ..., plot = FALSE, plot_type = "auto",
                   interpret = FALSE, audience = "manager") {
      args <- list(...)

      # Cache data (needed for plotting)
      self$last_data <- list(x = args$x, y = args$y)

      # Execute computation
      self$last_results <- self$analyzer$analyze(test_type, ...)

      # Print console report
      self$reporter$print_console(
        self$last_results,
        interpret = FALSE
      )

      # Optional plotting
      if (plot) {
        p <- self$plot(plot_type = plot_type)
        print(p)
      }

      # Optional interpretation
      if (interpret) {
        self$interpret(audience = audience)
      }

      invisible(self)
    },

    #' @description Plot graphics
    #' @param plot_type Plot type ("auto", "curve", "box", "combined")
    #' @param show_table Whether to display statistics table
    #' @param theme_obj Theme object
    #' @return ggplot2 or patchwork object
    #' @examples
    #' \dontrun{
    #' htest$run("t_test_1s", x = x, mu = 100)
    #' htest$plot(plot_type = "combined")
    #' htest$plot(plot_type = "curve")
    #' }
    plot = function(plot_type = "auto", show_table = TRUE, theme_obj = NULL) {
      if (is.null(self$last_results)) {
        stop("[iqr_htest] Please run run() first.", call. = FALSE)
      }

      if (!is.null(theme_obj)) {
        self$plotter$set_theme(theme_obj)
      }

      self$plotter$plot(
        result = self$last_results,
        x = self$last_data$x,
        y = self$last_data$y,
        plot_type = plot_type,
        show_table = show_table
      )
    },

    #' @description Interpret test results
    #' @param audience Audience level ("manager", "technical", "client")
    #' @param context Business context description
    #' @return Interpretation string
    #' @examples
    #' \dontrun{
    #' htest$run("t_test_1s", x = x, mu = 100)
    #' htest$interpret(audience = "manager")
    #' }
    interpret = function(audience = "manager", context = NULL) {
      if (is.null(self$last_results)) {
        stop("[iqr_htest] Please run run() first.", call. = FALSE)
      }

      explanation <- self$interpreter$interpret(
        self$last_results,
        audience = audience,
        context = context
      )

      cat(explanation, "\n")
      invisible(explanation)
    },

    #' @description Structured data export
    #' @return Data frame
    #' @examples
    #' \dontrun{
    #' htest$run("t_test_1s", x = x, mu = 100)
    #' df <- htest$report()
    #' }
    report = function() {
      if (is.null(self$last_results)) return(NULL)
      self$reporter$to_dataframe(self$last_results)
    },

    #' @description Export to Excel report
    #' @param path Output path
    #' @param audience Audience level
    #' @return Invisible self-reference
    #' @examples
    #' \dontrun{
    #' htest$run("t_test_1s", x = x, mu = 100)
    #' htest$report_excel("test_report.xlsx")
    #' }
    report_excel = function(path = NULL, audience = "manager") {
      if (is.null(self$last_results)) {
        stop("[iqr_htest] Please run run() first.", call. = FALSE)
      }

      self$reporter$export_excel(
        result = self$last_results,
        path = path,
        interpreter = self$interpreter,
        audience = audience
      )

      invisible(self)
    },

    #' @description One-click analysis (calculation + plotting + interpretation)
    #' @param test_type Test type
    #' @param ... Test parameters
    #' @param plot_type Plot type
    #' @param audience Audience level
    #' @param context Business context
    #' @return ggplot2 or patchwork object
    #' @examples
    #' \dontrun{
    #' htest$analyze("t_test_1s", x = x, mu = 100,
    #'               plot_type = "combined", audience = "manager")
    #' }
    analyze = function(test_type, ..., plot_type = "auto",
                       audience = "manager", context = NULL) {
      self$run(test_type, ..., plot = TRUE, plot_type = plot_type,
               interpret = TRUE, audience = audience)

      p <- self$plot(plot_type = plot_type)
      invisible(p)
    },

    #' @description Set theme
    #' @param theme_style Theme name or IqrTheme object
    #' @param ... Other parameters
    #' @return Invisible self-reference
    set_theme = function(theme_style = NULL, ...) {
      if (is.null(self$theme_obj) && !is.null(theme_style)) {
        tryCatch({
          self$theme_obj <- IqrTheme$new(theme_style %||% "academic", ...)
        }, error = function(e) {
          self$theme_obj <<- NULL
        })
      }
      if (!is.null(self$theme_obj) && !is.null(theme_style)) {
        tryCatch({
          if (is.character(theme_style)) {
            self$theme_obj$set_theme(theme_style, ...)
          } else if (inherits(theme_style, "IqrTheme")) {
            self$theme_obj <- theme_style
          }
        }, error = function(e) {})
      }
      self$plotter$set_theme(self$theme_obj)
      invisible(self)
    }
  )
)


# =============================================================================
# Convenience functions (stateless interface, suitable for one-time use)
# =============================================================================

#' @title Convenience hypothesis test function
#' @description
#' Execute hypothesis test without creating R6 object.
#' Suitable for quick analysis scenarios.
#'
#' @param test_type Test type
#' @param ... Test parameters
#' @param plot Whether to plot
#' @param plot_type Plot type
#' @param interpret Whether to output interpretation
#' @param audience Audience level
#' @param theme Theme
#'
#' @return Test results list (or plot object if plot = TRUE)
#' @export
#'
#' @examples
#' # One-sample t test
#' set.seed(123)
#' x <- rnorm(30, mean = 102, sd = 5)
#' htest_run("t_test_1s", x = x, mu = 100)
#'
#' # One-sample Z test (sigma known)
#' htest_run("z_test_1s", x = x, mu = 100, sigma = 5, plot = TRUE)
#'
#' # Two-sample t test
#' y <- rnorm(25, mean = 98, sd = 6)
#' htest_run("t_test_2s", x = x, y = y, interpret = TRUE)
htest_run <- function(test_type, ..., plot = FALSE, plot_type = "auto",
                      interpret = FALSE, audience = "manager", theme = "academic") {
  htest <- iqr_htest$new(theme = theme)
  htest$run(test_type, ..., plot = plot, plot_type = plot_type,
            interpret = interpret, audience = audience)

  result <- htest$last_results
  invisible(result)
}

#' @title Convenience hypothesis test plotting function
#' @description
#' Directly plot hypothesis test graphics without creating R6 object.
#'
#' @param test_type Test type
#' @param ... Test parameters
#' @param plot_type Plot type
#' @param show_table Whether to display statistics table
#' @param theme Theme
#'
#' @return ggplot2 or patchwork object
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 102, sd = 5)
#' htest_plot("t_test_1s", x = x, mu = 100, plot_type = "combined")
htest_plot <- function(test_type, ..., plot_type = "auto",
                       show_table = TRUE, theme = "academic") {
  htest <- iqr_htest$new(theme = theme)
  htest$run(test_type, ...)
  htest$plot(plot_type = plot_type, show_table = show_table)
}

#' @title Convenience hypothesis test interpretation function
#' @description
#' Directly output hypothesis test interpretation without creating R6 object.
#'
#' @param test_type Test type
#' @param ... Test parameters
#' @param audience Audience level
#' @param context Business context description
#'
#' @return Interpretation string
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 102, sd = 5)
#' htest_interpret("t_test_1s", x = x, mu = 100, audience = "manager")
htest_interpret <- function(test_type, ..., audience = "manager", context = NULL) {
  htest <- iqr_htest$new()
  htest$run(test_type, ...)
  htest$interpret(audience = audience, context = context)
}

