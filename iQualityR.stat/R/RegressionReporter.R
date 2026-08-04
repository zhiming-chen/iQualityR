# =============================================================================
# File: R/RegressionReporter.R
# Description: Regression report output (L2). console / data.frame / excel.
# =============================================================================
#' @title RegressionReporter: Regression Report Output
#' @description Convert a regression stat_result into console / data.frame / excel.
#' $report(result, format = c("data.frame","console","excel"), path = NULL, audience = "manager")
#' @export
RegressionReporter <- R6::R6Class("RegressionReporter",
  inherit = StatReporter,
  public = list(
    initialize = function(theme = NULL) { super$initialize(theme) },
    report = function(result, format = c("data.frame", "console", "excel"), path = NULL, audience = "manager") {
      format <- match.arg(format)
      switch(format,
        "console"    = self$print_console(result, audience = audience),
        "data.frame" = self$to_dataframe(result),
        "excel"      = self$export_excel(result, path = path),
        stop("Unknown format: ", format)
      )
    },
    print_console = function(result, interpret = TRUE, interpreter = NULL, audience = "manager") {
      cat(sprintf("\n  %s\n", result$method %||% result$test_type))
      cat(sprintf("  %s\n", paste(rep("-", 50), collapse = "")))
      ms <- result$model_stats
      tt <- result$test_type
      if (tt == "cox_fit") {
        cat(sprintf("  Concordance = %.4f, R-squared = %.4f\n",
                    ms$concordance %||% NA, ms$r_squared %||% NA))
        cat(sprintf("  n = %d, n_events = %d, AIC = %.4f\n",
                    ms$n %||% NA, ms$n_events %||% NA, ms$aic %||% NA))
      } else if (tt == "pls_fit") {
        cat(sprintf("  Components = %d, R-squared = %.4f, RMSEP(CV) = %.4f\n",
                    ms$ncomp %||% NA, ms$r_squared %||% NA, ms$sigma %||% NA))
        cat(sprintf("  n = %d\n", ms$n %||% NA))
      } else if (tt == "best_subset_fit") {
        cat(sprintf("  Predictors = %d, nvmax = %d\n",
                    ms$n_predictors %||% NA, ms$nvmax %||% NA))
        cat(sprintf("  Best (by BIC): %d vars, BIC = %.4f, Adj R-squared = %.4f\n",
                    result$best_by_bic$n_vars, result$best_by_bic$bic,
                    result$best_by_bic$adj_r_squared))
        cat(sprintf("  n = %d\n", ms$n %||% NA))
      } else if (tt == "mars_fit") {
        cat(sprintf("  Generalized R-squared = %.4f, R-squared = %.4f\n",
                    ms$generalized_rsq %||% NA, ms$rsq %||% NA))
        cat(sprintf("  GCV = %.4f, terms = %d, degree = %d\n",
                    ms$gcv %||% NA, ms$n_terms %||% NA, ms$degree %||% NA))
        cat(sprintf("  n = %d\n", ms$n %||% NA))
      } else if (tt == "spline_fit") {
        cat(sprintf("  Basis = %s, df = %d, predictor = %s\n",
                    toupper(result$spline_basis %||% "BS"),
                    result$spline_df %||% NA,
                    result$spline_predictor %||% NA))
        cat(sprintf("  R-squared = %.4f, Adj R-squared = %.4f\n",
                    ms$r_squared %||% NA, ms$adj_r_squared %||% NA))
        if (!is.null(ms$f_statistic))
          cat(sprintf("  F = %.4f, p = %.4f\n", as.numeric(ms$f_statistic[1]), ms$f_p_value))
        cat(sprintf("  n = %d, AIC = %.4f\n", ms$n, ms$aic))
      } else if (!is.null(ms$r_squared) && !is.na(ms$r_squared)) {
        cat(sprintf("  R-squared = %.4f, Adj R-squared = %.4f\n", ms$r_squared, ms$adj_r_squared))
        if (!is.null(ms$f_statistic))
          cat(sprintf("  F = %.4f, p = %.4f\n", as.numeric(ms$f_statistic[1]), ms$f_p_value))
        cat(sprintf("  n = %d, AIC = %.4f\n", ms$n, ms$aic))
      } else {
        cat(sprintf("  Deviance = %.4f, Null deviance = %.4f\n", ms$deviance, ms$null_deviance))
        cat(sprintf("  n = %d, AIC = %.4f\n", ms$n %||% NA, ms$aic %||% NA))
      }
      if (tt == "best_subset_fit" && !is.null(result$subset_summary)) {
        cat("\n  Subset selection summary:\n")
        print(result$subset_summary, row.names = FALSE)
      } else if (!is.null(result$coefficients)) {
        cat("\n  Coefficients:\n")
        print(result$coefficients, row.names = FALSE)
      }
      if (tt == "stepwise_fit" && !is.null(result$selected_terms)) {
        cat(sprintf("  Selected terms: %s\n",
                    paste(result$selected_terms, collapse = ", ")))
      }
      if (tt == "mars_fit" && !is.null(result$selected_terms)) {
        cat(sprintf("  MARS selected terms: %s\n",
                    paste(result$selected_terms, collapse = ", ")))
      }
      if (interpret) {
        if (is.null(interpreter)) interpreter <- StatInterpreter$new()
        cat("\n"); cat(interpreter$interpret(result, audience = audience))
      }
      cat("\n"); invisible(NULL)
    },
    to_dataframe = function(result) {
      if (!is.null(result$coefficients)) return(result$coefficients)
      if (!is.null(result$subset_summary)) return(result$subset_summary)
      NULL
    },
    export_excel = function(result, path = NULL) {
      df <- self$to_dataframe(result)
      if (is.null(df)) {
        message("[RegressionReporter] No tabular data to export.")
        return(invisible(NULL))
      }
      if (is.null(path)) { ts <- format(Sys.time(), "%Y%m%d_%H%M%S"); path <- sprintf("regression_report_%s.xlsx", ts) }
      wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Data")
      openxlsx::writeData(wb, "Data", df); openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
      message(sprintf("[RegressionReporter] Excel report written to: %s", path)); invisible(path)
    }
  )
)
