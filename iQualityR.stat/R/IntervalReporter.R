# =============================================================================
# File: R/IntervalReporter.R
# Description: Interval estimation report output (L2 presentation layer).
#              Per Contract 2: exposes $report(result, format, path, audience)
#              dispatching to console / data.frame / excel outputs.
# =============================================================================

#' @title IntervalReporter: Interval Estimation Report Output
#' @description
#' Convert a `stat_result` from `IntervalAnalyzer` into user-facing outputs:
#' console text, a tidy data frame, or a themed Excel workbook.
#'
#' **Contract 2 signature**:
#' ```
#' $report(result, format = c("data.frame","console","excel"),
#'         path = NULL, audience = "manager")
#' ```
#'
#' @export
IntervalReporter <- R6::R6Class("IntervalReporter",
  inherit = StatReporter,
  public = list(

    #' @description Initialize with a theme
    #' @param theme Theme name or `IqrTheme` object. NULL is allowed.
    initialize = function(theme = NULL) {
      super$initialize(theme)
    },

    #' @description Unified report entry point (Contract 2)
    #' @param result A `stat_result` from `IntervalAnalyzer`.
    #' @param format Output format: `"data.frame"` (default), `"console"`,
    #'   or `"excel"`.
    #' @param path File path for `format = "excel"`.
    #' @param audience Audience level for the interpretation when
    #'   `format = "console"`: `"manager"`, `"technical"`, or `"client"`.
    #' @return For `"data.frame"`: a data frame. Otherwise invisible NULL.
    report = function(result, format = c("data.frame", "console", "excel"),
                      path = NULL, audience = "manager") {
      format <- match.arg(format)
      switch(format,
        "console"    = self$print_console(result, audience = audience),
        "data.frame" = self$to_dataframe(result),
        "excel"      = self$export_excel(result, path = path, audience = audience),
        stop("Unknown format: ", format)
      )
    },

    #' @description Print a console report
    #' @param result A `stat_result` from `IntervalAnalyzer`.
    #' @param interpret Logical; append plain-language interpretation.
    #' @param interpreter Optional `StatInterpreter` instance.
    #' @param audience Audience level for the interpretation.
    #' @return Invisible NULL.
    print_console = function(result, interpret = TRUE, interpreter = NULL,
                             audience = "manager") {
      if (inherits(result, "stat_result")) {
        cat(format(result), "\n")
      } else {
        cat(sprintf("\n  %s\n", result$method %||% result$test_type))
        cat(sprintf("  %s\n", paste(rep("-", 50), collapse = "")))
        ci <- result$conf.int
        cl <- (result$conf.level %||% 0.95) * 100
        cat(sprintf("  %.0f%% interval: [%.4f, %.4f]\n", cl, ci[1], ci[2]))
      }
      if (interpret) {
        if (is.null(interpreter)) interpreter <- StatInterpreter$new()
        cat("\n")
        cat(interpreter$interpret(result, audience = audience))
      }
      cat("\n")
      invisible(NULL)
    },

    #' @description Convert a stat_result to a tidy data frame
    #' @param result A `stat_result` from `IntervalAnalyzer`.
    #' @return One-row data frame.
    to_dataframe = function(result) {
      ci <- result$conf.int
      data.frame(
        Domain      = result$domain %||% "interval",
        Interval_Type = result$test_type %||% NA,
        Method      = result$method %||% NA,
        Estimate    = if (!is.null(result$estimate)) as.numeric(result$estimate[1]) else NA,
        Lower       = ci[1],
        Upper       = ci[2],
        Conf_Level  = result$conf.level %||% NA,
        N           = result$n %||% NA,
        stringsAsFactors = FALSE
      )
    },

    #' @description Export to a themed Excel workbook
    #' @param result A `stat_result` from `IntervalAnalyzer`.
    #' @param path File path. Auto-timestamped if NULL.
    #' @param audience Audience level (unused for excel, kept for API parity).
    #' @return Invisible path.
    export_excel = function(result, path = NULL, audience = "manager") {
      df <- self$to_dataframe(result)
      if (is.null(path)) {
        ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
        path <- sprintf("interval_report_%s.xlsx", ts)
      }
      # Soft dependency on openxlsx (declared in DESCRIPTION Imports)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Interval")
      openxlsx::writeData(wb, "Interval", df)
      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
      message(sprintf("[IntervalReporter] Excel report written to: %s", path))
      invisible(path)
    }
  )
)
