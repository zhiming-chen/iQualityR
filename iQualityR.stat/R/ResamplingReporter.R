# =============================================================================
# File: R/ResamplingReporter.R
# Description: Resampling report output (L2 presentation layer).
#              Per Contract 2: exposes
#              $report(result, format, path, audience) dispatching to
#              console / data.frame / excel outputs.
# =============================================================================

#' @title ResamplingReporter: Resampling Report Output
#' @description
#' Convert a `stat_result` from `ResamplingAnalyzer` into user-facing outputs:
#' console text, a tidy data frame, or a themed Excel workbook.
#'
#' **Contract 2 signature**:
#' ```
#' $report(result, format = c("data.frame","console","excel"),
#'         path = NULL, audience = "manager")
#' ```
#'
#' @export
ResamplingReporter <- R6::R6Class("ResamplingReporter",
  inherit = StatReporter,
  public = list(

    #' @description Initialize with a theme
    #' @param theme Theme name or `IqrTheme` object. NULL is allowed.
    initialize = function(theme = NULL) {
      super$initialize(theme)
    },

    #' @description Unified report entry point (Contract 2)
    #' @param result A `stat_result` from `ResamplingAnalyzer`.
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
        "excel"      = self$export_excel(result, path = path,
                                         audience = audience),
        stop("Unknown format: ", format)
      )
    },

    #' @description Print a console report
    #' @param result A `stat_result` from `ResamplingAnalyzer`.
    #' @param interpret Logical; append plain-language interpretation.
    #' @param interpreter Optional `StatInterpreter` instance.
    #' @param audience Audience level for the interpretation.
    #' @return Invisible NULL.
    print_console = function(result, interpret = TRUE, interpreter = NULL,
                             audience = "manager") {
      cat(sprintf("\n  %s\n", result$method %||% result$test_type))
      cat(sprintf("  %s\n", paste(rep("-", 50), collapse = "")))
      theta <- as.numeric(result$statistic[1])
      tt <- result$test_type
      if (tt == "bootstrap_ci") {
        ci <- result$conf.int
        cl <- (result$conf.level %||% 0.95) * 100
        cat(sprintf("  statistic = %.4f\n", theta))
        cat(sprintf("  %.0f%% CI (%s): [%.4f, %.4f]\n", cl,
                    toupper(result$boot_method %||% "BCA"),
                    ci[1], ci[2]))
        cat(sprintf("  bias = %.4f, bootstrap se = %.4f\n",
                    result$boot_bias %||% NA, result$boot_se %||% NA))
        cat(sprintf("  R = %d, n = %d\n", result$R %||% NA,
                    result$n %||% NA))
      } else {
        p <- result$p.value
        p_str <- if (is.na(p)) "NA" else
                 if (p < 1e-4) "<1e-04" else sprintf("%.4f", p)
        cat(sprintf("  observed statistic = %.4f\n", theta))
        cat(sprintf("  p-value = %s (alternative = %s)\n",
                    p_str, result$alternative %||% "two.sided"))
        cat(sprintf("  R = %d, n = %d, design = %s\n",
                    result$R %||% NA, result$n %||% NA,
                    result$design %||% "?"))
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
    #' @param result A `stat_result` from `ResamplingAnalyzer`.
    #' @return One-row data frame.
    to_dataframe = function(result) {
      tt <- result$test_type
      theta <- as.numeric(result$statistic[1])
      if (tt == "bootstrap_ci") {
        ci <- result$conf.int
        data.frame(
          Domain      = "resampling",
          Type        = "bootstrap_ci",
          Method      = result$boot_method %||% NA,
          Statistic   = theta,
          Lower       = ci[1],
          Upper       = ci[2],
          Conf_Level  = result$conf.level %||% NA,
          Bias        = result$boot_bias %||% NA,
          SE          = result$boot_se %||% NA,
          R           = result$R %||% NA,
          N           = result$n %||% NA,
          stringsAsFactors = FALSE
        )
      } else {
        p <- result$p.value
        data.frame(
          Domain      = "resampling",
          Type        = "permutation_test",
          Design      = result$design %||% NA,
          Statistic   = theta,
          P_Value     = p,
          Alternative = result$alternative %||% NA,
          R           = result$R %||% NA,
          N           = result$n %||% NA,
          stringsAsFactors = FALSE
        )
      }
    },

    #' @description Export to a themed Excel workbook
    #' @param result A `stat_result` from `ResamplingAnalyzer`.
    #' @param path File path. Auto-timestamped if NULL.
    #' @param audience Audience level (unused for excel, kept for API parity).
    #' @return Invisible path.
    export_excel = function(result, path = NULL, audience = "manager") {
      df <- self$to_dataframe(result)
      if (is.null(path)) {
        ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
        path <- sprintf("resampling_report_%s.xlsx", ts)
      }
      # Soft dependency on openxlsx (declared in DESCRIPTION Imports)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Resampling")
      openxlsx::writeData(wb, "Resampling", df)
      # Also dump the replicate distribution on a second sheet for diagnostics
      if (!is.null(result$replicates)) {
        rep_df <- data.frame(Replicate = seq_along(result$replicates),
                             Value = as.numeric(result$replicates))
        openxlsx::addWorksheet(wb, "Replicates")
        openxlsx::writeData(wb, "Replicates", rep_df)
      }
      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
      message(sprintf("[ResamplingReporter] Excel report written to: %s", path))
      invisible(path)
    }
  )
)
