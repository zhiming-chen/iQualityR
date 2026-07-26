# =============================================================================
# File: R/IqrSamplingTask.R
# Description: Unified sampling task orchestrator (inherits IqrTaskBase)
# =============================================================================

#' @title IqrSamplingTask: Unified Sampling Analysis Task
#'
#' @description
#' Inherits from [IqrTaskBase] and orchestrates the Analyzer / Plotter /
#' Reporter trio for an end-to-end sampling plan analysis:
#'
#' 1. `compute()` - run theoretical OC curve / risk / power / ASN analysis
#' 2. `summary()` - print a console report
#' 3. `plot()` - render visualizations
#' 4. `report()` - export to Excel (other formats reserved for future)
#'
#' @export
IqrSamplingTask <- R6::R6Class("IqrSamplingTask",
  inherit = IqrTaskBase,

  public = list(
    #' @field plan [SamplingPlan] object.
    plan = NULL,

    #' @description Initialize a sampling task.
    #'
    #' @param data Data frame (optional; required only for actual sampling).
    #' @param plan [SamplingPlan] object.
    #' @param theme Theme name (character), IqrTheme object, or ThemeConfig.
    #' @param ... Additional arguments passed to [IqrTaskBase].
    initialize = function(data = NULL, plan, theme = "academic", ...) {
      super$initialize(data = data, theme = theme, ...)
      self$plan <- plan
      self$executor$analyzer <- SamplingAnalyzer$new()
      self$executor$plotter  <- SamplingPlotter$new()
      self$executor$reporter <- SamplingReporter$new()
      invisible(self)
    },

    #' @description Execute the sampling analysis.
    #' @return This task (invisibly). Results are stored in `self$results`.
    compute = function() {
      analyzer <- self$executor$analyzer
      self$results <- analyzer$analyze(data = self$data, plan = self$plan)
      invisible(self)
    },

    #' @description Print a console summary of results.
    summary = function() {
      self$executor$reporter$print_console(self$results, self$plan)
      invisible(self)
    },

    #' @description Render a plot.
    #' @param type Character. Plot type: `"full"`, `"oc"`, `"power"`,
    #'   `"asn"`, `"risk"`.
    #' @param ... Additional arguments passed to the plotter.
    #' @return A `ggplot` or `patchwork` object.
    plot = function(type = "full", ...) {
      self$executor$plotter$render(
        results = self$results,
        theme_obj = self$theme_obj,
        type = type,
        ...
      )
    },

    #' @description Generate a report file.
    #' @param format Character. Output format: `"excel"`.
    #' @param path Character. Output file path (auto-generated if NULL).
    #' @param ... Additional arguments passed to the reporter.
    #' @return Invisible path to the written file.
    report = function(format = "excel", path = NULL, ...) {
      if (is.null(path)) {
        timestamp <- base::format(Sys.time(), "%Y%m%d_%H%M%S")
        path <- paste0("report_sampling_", timestamp, ".xlsx")
      }
      self$executor$reporter$to_excel(self$results, path)
      invisible(path)
    }
  )
)
