# =============================================================================
# File: R/htest/HTestReporter.R
# Description: Hypothesis test report output engine
# =============================================================================

#' @title HTestReporter: Hypothesis Test Report Output
#' @description
#' Output test results as console report, data frame, or Excel file.
#'
#' @export
HTestReporter <- R6::R6Class("HTestReporter",
  public = list(
    #' @description Print console report
    #' @param result Test result
    #' @param interpret Whether to output interpretation
    #' @param interpreter StatInterpreter instance
    #' @param audience Audience level
    print_console = function(result, interpret = TRUE, interpreter = NULL, audience = "manager") {
      cat("\n")
      cat(sprintf("  %s\n", result$test_type %||% "Hypothesis Test"))
      cat(sprintf("  %s\n", paste(rep("-", 50), collapse = "")))
      cat(sprintf("  Data: %s\n", result$data_name %||% ""))
      cat("\n")

      # Statistics
      cat(sprintf("  %s = %.4f\n", names(result$statistic)[1], as.numeric(result$statistic[1])))

      if (!is.null(result$parameter)) {
        cat(sprintf("  df = %s\n", paste(result$parameter, collapse = ", ")))
      }

      cat(sprintf("  P Value = %s\n", if (result$p.value < 0.001) "<0.001" else sprintf("%.4f", result$p.value)))

      # Confidence interval
      if (!is.null(result$conf.int)) {
        if (is.infinite(result$conf.int[1]) && is.infinite(result$conf.int[2])) {
          # skip
        } else if (is.infinite(result$conf.int[1])) {
          cat(sprintf("  %.0f%% CI: (-Inf, %.4f]\n",
                      (result$conf.level %||% 0.95) * 100, result$conf.int[2]))
        } else if (is.infinite(result$conf.int[2])) {
          cat(sprintf("  %.0f%% CI: [%.4f, Inf)\n",
                      (result$conf.level %||% 0.95) * 100, result$conf.int[1]))
        } else {
          cat(sprintf("  %.0f%% CI: [%.4f, %.4f]\n",
                      (result$conf.level %||% 0.95) * 100, result$conf.int[1], result$conf.int[2]))
        }
      }

      # Estimate
      if (!is.null(result$estimate)) {
        cat("\n  Sample Estimate:\n")
        for (nm in names(result$estimate)) {
          cat(sprintf("    %s = %.4f\n", nm, result$estimate[nm]))
        }
      }

      # Interpretation
      if (interpret && !is.null(interpreter)) {
        cat("\n")
        cat(interpreter$interpret(result, audience = audience))
      }

      cat("\n")
    },

    #' @description Convert to data frame
    #' @param result Test result
    #' @return Data frame
    to_dataframe = function(result) {
      df <- data.frame(
        Test_Type = result$test_type %||% NA,
        Statistic_Name = names(result$statistic)[1],
        Statistic_Value = as.numeric(result$statistic[1]),
        P_Value = result$p.value,
        Alternative = result$alternative,
        Conf_Level = result$conf.level %||% NA,
        CI_Lower = if (!is.null(result$conf.int) && !is.infinite(result$conf.int[1])) result$conf.int[1] else NA,
        CI_Upper = if (!is.null(result$conf.int) && !is.infinite(result$conf.int[2])) result$conf.int[2] else NA,
        Method = result$method %||% NA,
        stringsAsFactors = FALSE
      )

      # Add estimates
      if (!is.null(result$estimate)) {
        for (nm in names(result$estimate)) {
          df[[paste0("Estimate_", nm)]] <- result$estimate[nm]
        }
      }

      # Add sample size
      if (!is.null(result$n)) {
        if (is.null(result$n2)) {
          df$N <- result$n
        } else {
          df$N1 <- result$n
          df$N2 <- result$n2
        }
      }

      df
    },

    #' @description Export to Excel
    #' @param result Test result
    #' @param path Output path
    #' @param excel_exporter ExcelExporter instance
    #' @param audience Audience level
    export_excel = function(result, path = NULL, excel_exporter = NULL, audience = "manager") {
      if (is.null(excel_exporter)) {
        stop("excel_exporter must be provided. Create an ExcelExporter instance from iQualityR.core.")
      }

      df_result <- self$to_dataframe(result)

      data_list <- list("Test Result" = df_result)
      sheet_names <- "Test Result"

      if (!is.null(path)) {
        excel_exporter$export_excel(data_list, path = path, sheet_names = sheet_names)
      } else {
        excel_exporter$export_excel(data_list, sheet_names = sheet_names)
      }
    }
  )
)
