# =============================================================================
# File: R/HTestReporter.R
# Description: Hypothesis test report output engine (L2 presentation layer).
#              Per Contract 2 (STAT_ANALYSIS_PLAN.md v2.0): exposes a unified
#              $report(result, format, path, audience) entry point that
#              dispatches to console / data.frame / excel outputs.
#              Legacy $print_console / $to_dataframe / $export_excel are kept
#              as thin wrappers for back-compat with existing tests & callers.
# =============================================================================

#' @title HTestReporter: Hypothesis Test Report Output
#' @description
#' Convert a `stat_result` from `HTestAnalyzer` into user-facing outputs:
#' console text, a data frame, or a themed Excel workbook.
#'
#' **Contract 2 signature** (fixed across all L2 Reporters in .stat):
#' ```
#' $report(result, format = c("data.frame","console","excel"),
#'         path = NULL, audience = "manager")
#' ```
#'
#' Excel export uses `iQualityR.core::ExcelExporter` (soft dependency via
#' `requireNamespace`) so the reporter still works in environments that only
#' need console / data.frame output.
#'
#' @export
HTestReporter <- R6::R6Class("HTestReporter",
  public = list(
    #' @field theme_obj Active `IqrTheme` for Excel styling (NULL = default).
    theme_obj = NULL,

    #' @description Initialize with a theme
    #' @param theme Theme name or `IqrTheme` object. NULL is allowed -- Excel
    #'   export will then fall back to the default ThemeConfig.
    initialize = function(theme = NULL) {
      self$theme_obj <- .resolve_theme(theme)
    },

    #' @description Unified report entry point (Contract 2)
    #'
    #' Dispatches on `format`:
    #' - `"console"`: prints a human-readable summary to stdout.
    #' - `"data.frame"`: returns a tidy one-row data frame.
    #' - `"excel"`: writes a themed xlsx file via `ExcelExporter`.
    #'
    #' @param result A `stat_result` from `HTestAnalyzer`
    #' @param format Output format: `"data.frame"` (default), `"console"`, or `"excel"`.
    #' @param path File path for `format = "excel"`. Auto-timestamped if NULL.
    #' @param audience Audience level forwarded to the interpreter when
    #'   `format = "console"`: `"manager"`, `"technical"`, or `"client"`.
    #' @return For `"data.frame"`: a data frame. For `"console"`/`"excel"`:
    #'   invisible(NULL) (side-effect output).
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
    #' @param result A `stat_result` from `HTestAnalyzer`
    #' @param interpret Logical; whether to append a plain-language interpretation.
    #' @param interpreter Optional `StatInterpreter` instance. If NULL and
    #'   `interpret = TRUE`, a transient `StatInterpreter` is created.
    #' @param audience Audience level for the interpretation.
    #' @return Invisible NULL (writes to stdout).
    print_console = function(result, interpret = TRUE, interpreter = NULL, audience = "manager") {
      # Prefer the stat_result format() method -- single source of formatting truth.
      if (inherits(result, "stat_result")) {
        cat(format(result), "\n")
      } else {
        # Legacy fallback for raw lists (pre-stat_result callers)
        cat("\n")
        cat(sprintf("  %s\n", result$method %||% result$test_type %||% "Hypothesis Test"))
        cat(sprintf("  %s\n", paste(rep("-", 50), collapse = "")))
        cat(sprintf("  Data: %s\n", result$data_name %||% ""))
        cat(sprintf("  %s = %.4f\n", names(result$statistic)[1], as.numeric(result$statistic[1])))
        if (!is.null(result$parameter)) {
          cat(sprintf("  df = %s\n", paste(result$parameter, collapse = ", ")))
        }
        p_str <- if (result$p.value < 0.001) "<0.001" else sprintf("%.4f", result$p.value)
        cat(sprintf("  P Value = %s\n", p_str))
        if (!is.null(result$conf.int)) {
          ci <- result$conf.int
          cl <- (result$conf.level %||% 0.95) * 100
          lo <- if (is.infinite(ci[1])) "-Inf" else sprintf("%.4f", ci[1])
          hi <- if (is.infinite(ci[2]))  "Inf" else sprintf("%.4f", ci[2])
          cat(sprintf("  %.0f%% CI: [%s, %s]\n", cl, lo, hi))
        }
        if (!is.null(result$estimate)) {
          cat("\n  Sample Estimate:\n")
          for (nm in names(result$estimate)) {
            cat(sprintf("    %s = %.4f\n", nm, result$estimate[nm]))
          }
        }
      }

      if (interpret) {
        if (is.null(interpreter)) {
          interpreter <- StatInterpreter$new()
        }
        cat("\n")
        cat(interpreter$interpret(result, audience = audience))
      }

      cat("\n")
      invisible(NULL)
    },

    #' @description Convert a stat_result to a tidy data frame
    #' @param result A `stat_result` from `HTestAnalyzer`
    #' @return One-row data frame with test metadata + estimates.
    to_dataframe = function(result) {
      df <- data.frame(
        Domain          = result$domain %||% "htest",
        Test_Type       = result$test_type %||% NA,
        Method          = result$method %||% NA,
        Statistic_Name  = if (!is.null(result$statistic)) names(result$statistic)[1] else NA,
        Statistic_Value = if (!is.null(result$statistic)) as.numeric(result$statistic[1]) else NA,
        P_Value         = result$p.value,
        Alternative     = result$alternative %||% NA,
        Conf_Level      = result$conf.level %||% NA,
        CI_Lower        = if (!is.null(result$conf.int) && !is.infinite(result$conf.int[1])) result$conf.int[1] else NA,
        CI_Upper        = if (!is.null(result$conf.int) && !is.infinite(result$conf.int[2])) result$conf.int[2] else NA,
        stringsAsFactors = FALSE
      )

      # Append sample estimates as named columns
      if (!is.null(result$estimate)) {
        for (nm in names(result$estimate)) {
          df[[paste0("Estimate_", gsub("[^A-Za-z0-9_]", "_", nm))]] <- result$estimate[nm]
        }
      }

      # Append sample sizes (n for one-sample, n1/n2 for two-sample)
      if (!is.null(result$n)) {
        df$N <- result$n
      } else if (!is.null(result$n1) && !is.null(result$n2)) {
        df$N1 <- result$n1
        df$N2 <- result$n2
      }

      df
    },

    #' @description Export to a themed Excel workbook
    #' @param result A `stat_result` from `HTestAnalyzer`
    #' @param path Output file path. Auto-timestamped when NULL.
    #' @param excel_exporter Optional pre-built `ExcelExporter` instance.
    #'   When NULL, one is built on demand from the reporter's theme (or the
    #'   default ThemeConfig if no theme is set).
    #' @param audience Audience level (reserved for future interpretation sheet).
    #' @return Invisible path of the written file.
    export_excel = function(result, path = NULL, excel_exporter = NULL, audience = "manager") {
      if (!requireNamespace("iQualityR.core", quietly = TRUE)) {
        stop("[HTestReporter] iQualityR.core is required for Excel export.", call. = FALSE)
      }

      # Build an ExcelExporter on demand if the caller didn't supply one.
      if (is.null(excel_exporter)) {
        config <- if (!is.null(self$theme_obj)) self$theme_obj$config else NULL
        if (is.null(config)) {
          # Fall back to a default theme config so export still works.
          default_theme <- IqrTheme$new("academic")
          config <- default_theme$config
        }
        excel_exporter <- iQualityR.core::ExcelExporter$new(config)
      }

      df_result <- self$to_dataframe(result)
      data_list <- list("Test Result" = df_result)

      excel_exporter$export_excel(data_list, path = path, sheet_names = "Test Result")
      invisible(path)
    }
  )
)
