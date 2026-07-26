# =============================================================================
# File: R/normality/iqr_normality.R
# Description: Normality test module user entry point
# =============================================================================

#' @title iqr_normality: Normality test entry point
#' @description
#' Top-level interface for iQualityR normality test module, coordinating
#' computation, plotting, reporting, and interpretation.
#'
#' **Supported test methods**:
#' - `auto`: Auto-select (n <= 5000 -> Shapiro-Wilk, n > 5000 -> Anderson-Darling)
#' - `sw`: Shapiro-Wilk test (highest power for small samples)
#' - `ad`: Anderson-Darling test (usable for large samples, sensitive to tails)
#' - `lillie`: Lilliefors-corrected KS test
#' - `cvm`: Cramer-von Mises test
#' - `sf`: Shapiro-Francia test (sensitive to skewness)
#'
#' **Output system**:
#' - Calculation: Test statistic, P-value, normality decision
#' - Graphics: Histogram+normal curve, QQ plot, PP plot, combined plot
#' - Interpretation: Manager/technician/customer three versions
#' - Report: Console, data frame, Excel
#'
#' **Dual interface design**:
#' - R6 class interface: `iqr_normality$new()$test()$plot()$interpret()` (suitable for chaining)
#' - Convenience function interface: `normality_test()`, `normality_plot()`, `normality_interpret()` (for one-time use)
#'
#' @export
iqr_normality <- R6::R6Class("iqr_normality",
  public = list(
    #' @field last_results Cached computation results
    last_results = NULL,
    #' @field last_data Cached sample data (needed for plotting)
    last_data = NULL,
    #' @field last_diagnose Cached diagnostic results
    last_diagnose = NULL,
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

    #' @description Initialize normality test module
    #' @param theme Theme name or IqrTheme object
    #' @param ... Other parameters
    #' @return iqr_normality object
    initialize = function(theme = "academic", ...) {
      self$analyzer     <- NormalityAnalyzer$new()
      self$plotter      <- NormalityPlotter$new(theme = theme)
      self$reporter     <- NormalityReporter$new()
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

    #' @description Execute normality test
    #' @param x Numeric vector
    #' @param method Test method
    #' @param alpha Significance level
    #' @param plot Whether to plot
    #' @param plot_type Plot type
    #' @param interpret Whether to output interpretation
    #' @param audience Audience level
    #' @param context Business context description
    #' @return Test results list
    test = function(x, method = "auto", alpha = 0.05,
                    plot = FALSE, plot_type = "auto",
                    interpret = FALSE, audience = "manager",
                    context = NULL) {
      # Compute
      self$last_results <- self$analyzer$test(x, method = method, alpha = alpha)
      self$last_data <- x
      self$last_diagnose <- self$analyzer$diagnose(x)

      # Console output
      self$reporter$print_console(
        self$last_results,
        diagnose = self$last_diagnose,
        interpret = interpret,
        interpreter = self$interpreter,
        audience = audience
      )

      # Plotting
      if (plot) {
        self$plot(plot_type = plot_type)
      }

      invisible(self$last_results)
    },

    #' @description Batch test for multiple columns in data frame
    #' @param data Data frame
    #' @param vars Column name vector
    #' @param method Test method
    #' @param alpha Significance level
    #' @return Test results list
    test_multiple = function(data, vars = NULL, method = "auto", alpha = 0.05) {
      results <- self$analyzer$test_multiple(data, vars = vars, method = method, alpha = alpha)
      self$last_results <- results
      invisible(results)
    },

    #' @description Plot normality diagnostic plots
    #' @param x Sample data (NULL to use cached data)
    #' @param plot_type Plot type
    #' @param add_confidence Whether to add confidence band to QQ plot
    #' @return ggplot2 or patchwork object
    plot = function(x = NULL, plot_type = "auto", add_confidence = FALSE) {
      if (is.null(x)) {
        if (is.null(self$last_data)) {
          stop("No data available. Run test() first or provide x.")
        }
        x <- self$last_data
      }

      self$plotter$plot(x, result = self$last_results,
                        plot_type = plot_type,
                        add_confidence = add_confidence)
    },

    #' @description Interpret test results
    #' @param result Test results (NULL to use cached results)
    #' @param audience Audience level
    #' @param context Business context description
    #' @return Interpretation string
    interpret = function(result = NULL, audience = "manager", context = NULL) {
      if (is.null(result)) {
        if (is.null(self$last_results)) {
          stop("No results available. Run test() first or provide result.")
        }
        result <- self$last_results
      }

      self$interpreter$interpret(result, diagnose = self$last_diagnose,
                                  audience = audience, context = context)
    },

    #' @description Output data frame
    #' @param results Test results (NULL to use cached results)
    #' @return Data frame
    to_dataframe = function(results = NULL) {
      if (is.null(results)) {
        if (is.null(self$last_results)) {
          stop("No results available. Run test() first or provide results.")
        }
        results <- self$last_results
      }

      self$reporter$to_dataframe(results)
    },

    #' @description Output Excel file
    #' @param results Test results
    #' @param path File path
    #' @param include_diagnose Whether to include diagnostic results
    #' @return File path
    to_excel = function(results = NULL, path = "normality_report.xlsx",
                        include_diagnose = FALSE) {
      if (is.null(results)) {
        if (is.null(self$last_results)) {
          stop("No results available. Run test() first or provide results.")
        }
        results <- self$last_results
      }

      if (include_diagnose) {
        attr(results, "diagnose") <- list("Data" = self$last_diagnose)
      }

      self$reporter$to_excel(results, path = path, include_diagnose = include_diagnose)
    }
  )
)


# =============================================================================
# Convenience functions (stateless interface, suitable for one-time use)
# =============================================================================

#' @title Convenience normality test function
#' @description
#' Execute normality test without creating an R6 object.
#' Suitable for quick analysis scenarios.
#'
#' @param x Numeric vector
#' @param method Test method
#' @param alpha Significance level
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
#' set.seed(123)
#' x <- rnorm(50)
#' normality_test(x)
#' normality_test(x, plot = TRUE)
normality_test <- function(x, method = "auto", alpha = 0.05,
                            plot = FALSE, plot_type = "auto",
                            interpret = FALSE, audience = "manager",
                            theme = "academic") {
  normality <- iqr_normality$new(theme = theme)
  normality$test(x, method = method, alpha = alpha,
                 plot = plot, plot_type = plot_type,
                 interpret = interpret, audience = audience)

  result <- normality$last_results
  invisible(result)
}

#' @title Convenience normality plot function
#' @description
#' Directly plot normality diagnostic charts.
#'
#' @param x Numeric vector
#' @param plot_type Plot type ("auto", "hist", "qq", "pp", "combined")
#' @param add_confidence Whether to add confidence band to QQ plot
#' @param theme Theme
#'
#' @return ggplot2 or patchwork object
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(50)
#' normality_plot(x)
#' normality_plot(x, plot_type = "qq", add_confidence = TRUE)
normality_plot <- function(x, plot_type = "auto", add_confidence = FALSE,
                            theme = "academic") {
  normality <- iqr_normality$new(theme = theme)
  normality$plot(x, plot_type = plot_type, add_confidence = add_confidence)
}

#' @title Convenience normality interpretation function
#' @description
#' Directly output interpretation of normality test results.
#'
#' @param x Numeric vector
#' @param method Test method
#' @param alpha Significance level
#' @param audience Audience level
#' @param context Business context description
#'
#' @return Interpretation string
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(50)
#' normality_interpret(x, audience = "manager")
normality_interpret <- function(x, method = "auto", alpha = 0.05,
                                 audience = "manager", context = NULL) {
  normality <- iqr_normality$new()
  normality$test(x, method = method, alpha = alpha)
  normality$interpret(audience = audience, context = context)
}
