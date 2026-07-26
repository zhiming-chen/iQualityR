# =============================================================================
# File: R/IqrReliabilityTask.R
# Description: Reliability analysis task coordinator (inherits IqrTaskBase)
# =============================================================================

#' @title IqrReliabilityTask
#'
#' @description
#' Task coordinator for reliability and survival analysis. Inherits
#' [IqrTaskBase] and orchestrates the [ReliabilityAnalyzer],
#' [ReliabilityPlotter], and [ReliabilityReporter].
#'
#' @field plan [ReliabilityPlan] object holding analysis configuration.
#'
#' @param data Data frame containing the time/status columns and covariates.
#' @param plan [ReliabilityPlan] object.
#' @param theme Theme name (character) or [IqrTheme] object.
#' @param ... Additional arguments passed to [IqrTaskBase]$initialize().
#'
#' @export
IqrReliabilityTask <- R6::R6Class("IqrReliabilityTask",
  inherit = IqrTaskBase,

  public = list(
    plan = NULL,

    #' @description Create a task instance.
    #' @param data Data frame.
    #' @param plan [ReliabilityPlan] object.
    #' @param theme Theme name or [IqrTheme] object.
    #' @param ... Additional arguments.
    initialize = function(data, plan, theme = "academic", ...) {
      super$initialize(data, theme, ...)
      self$plan <- plan

      # Instantiate executors
      self$executor$analyzer <- ReliabilityAnalyzer$new()
      self$executor$plotter  <- ReliabilityPlotter$new()
      self$executor$reporter <- ReliabilityReporter$new()
    },

    #' @description Execute reliability analysis computation.
    compute = function() {
      self$results <- self$executor$analyzer$analyze(self$data, self$plan)
      invisible(self)
    },

    #' @description Print a structured summary to the console.
    summary = function() {
      if (is.null(self$results)) {
        cat("No results yet. Call $compute() first.\n")
        return(invisible(self))
      }
      self$executor$reporter$print_console(self$results, self$plan)
      invisible(self)
    },

    #' @description Draw a reliability plot.
    #' @param type Character. Plot type (see [ReliabilityPlotter]$render()).
    #' @param ... Additional arguments (ignored).
    plot = function(type = "full", ...) {
      if (is.null(self$results)) {
        stop("No results yet. Call $compute() first.", call. = FALSE)
      }
      self$executor$plotter$render(self$results, self$theme_obj, type = type)
    },

    #' @description Generate a report file.
    #' @param format Character. Output format (`"excel"`).
    #' @param path Output file path.
    #' @param ... Additional arguments passed to the reporter.
    report = function(format = "excel", path = NULL, ...) {
      if (is.null(self$results)) {
        stop("No results yet. Call $compute() first.", call. = FALSE)
      }
      if (format == "excel") {
        if (is.null(path)) {
          path <- tempfile(fileext = ".xlsx")
        }
        self$executor$reporter$export_excel(self$results, self$plan, path, ...)
      } else {
        stop("[IqrReliabilityTask] Unsupported report format: ", format,
             call. = FALSE)
      }
      invisible(self)
    }
  )
)
