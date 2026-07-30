# =============================================================================
# File: R/NormalityReporter.R
# Description: Normality test report output engine
# =============================================================================

#' @title NormalityReporter: Normality test report output
#' @description
#' Output test results as console report, data frame, or Excel file.
#'
#' @export
NormalityReporter <- R6::R6Class("NormalityReporter",
  public = list(
    #' @description Print console report
    #' @param result Test result
    #' @param diagnose Diagnostic result (optional)
    #' @param interpret Whether to output interpretation
    #' @param interpreter StatInterpreter instance
    #' @param audience Audience level
    print_console = function(result, diagnose = NULL, interpret = TRUE,
                              interpreter = NULL, audience = "manager") {
      cat("\n")
      cat(sprintf("  %s\n", result$test_type %||% "Normality Test"))
      cat(sprintf("  %s\n", paste(rep("-", 50), collapse = "")))
      cat(sprintf("  Method: %s\n", result$method %||% ""))
      cat(sprintf("  Data: %s\n", result$data_name %||% ""))
      cat("\n")

      # Statistics
      stat_name <- names(result$statistic)[1]
      stat_val <- as.numeric(result$statistic[1])
      cat(sprintf("  %s = %.4f\n", stat_name, stat_val))
      cat(sprintf("  P Value = %s\n", if (result$p.value < 0.001) "<0.001" else sprintf("%.4f", result$p.value)))
      cat(sprintf("  alpha = %.2f\n", result$alpha %||% 0.05))
      cat("\n")

      # Conclusion
      is_normal <- result$is_normal %||% FALSE
      cat(sprintf("  Conclusion: %s\n", if (is_normal) "Data follows normal distribution" else "Data does not follow normal distribution"))
      cat("\n")

      # Sample statistics
      if (!is.null(result$n)) {
        cat(sprintf("  Sample size: %d\n", result$n))
      }
      if (!is.null(result$sample_mean)) {
        cat(sprintf("  Mean: %.4f\n", result$sample_mean))
      }
      if (!is.null(result$sample_sd)) {
        cat(sprintf("  Standard deviation: %.4f\n", result$sample_sd))
      }
      if (!is.null(result$skewness)) {
        cat(sprintf("  Skewness: %.4f\n", result$skewness))
      }
      if (!is.null(result$excess_kurtosis)) {
        cat(sprintf("  Excess kurtosis: %.4f\n", result$excess_kurtosis))
      }
      cat("\n")

      # Diagnostic result
      if (!is.null(diagnose)) {
        cat("  [Distribution Shape Diagnosis]\n")
        cat(sprintf("    Skewness direction: %s\n", diagnose$skewness_direction %||% ""))
        cat(sprintf("    Kurtosis type: %s\n", diagnose$kurtosis_type %||% ""))
        cat("\n")
      }

      # Interpretation
      if (interpret && !is.null(interpreter)) {
        cat(interpreter$interpret(result, diagnose = diagnose, audience = audience))
        cat("\n")
      }
    },

    #' @description Convert results to data frame
    #' @param results Test result list (supports multiple variables)
    #' @return Data frame
    to_dataframe = function(results) {
      if (!is.list(results) || length(results) == 0) {
        return(data.frame())
      }

      # Single variable result
      if (is.null(names(results)) || all(names(results) == "")) {
        results <- list("Variable_1" = results)
      }

      rows <- list()
      for (name in names(results)) {
        r <- results[[name]]
        if (!is.null(r$error)) {
          rows[[name]] <- data.frame(
            Variable = name,
            Method = "error",
            Statistic = NA,
            P_Value = NA,
            Alpha = NA,
            Is_Normal = NA,
            N = NA,
            Mean = NA,
            SD = NA,
            Skewness = NA,
            Excess_Kurtosis = NA,
            Error = r$error,
            stringsAsFactors = FALSE
          )
        } else {
          rows[[name]] <- data.frame(
            Variable = name,
            Method = r$method %||% NA,
            Statistic = as.numeric(r$statistic[1]),
            P_Value = r$p.value,
            Alpha = r$alpha %||% 0.05,
            Is_Normal = r$is_normal %||% NA,
            N = r$n %||% NA,
            Mean = r$sample_mean %||% NA,
            SD = r$sample_sd %||% NA,
            Skewness = r$skewness %||% NA,
            Excess_Kurtosis = r$excess_kurtosis %||% NA,
            Error = NA,
            stringsAsFactors = FALSE
          )
        }
      }

      do.call(rbind, rows)
    },

    #' @description Export results to Excel file
    #' @param results Test result list
    #' @param path File path
    #' @param excel_exporter ExcelExporter instance
    #' @param include_diagnose Whether to include diagnostic results
    #' @return File path (invisible)
    to_excel = function(results, path = "normality_report.xlsx", excel_exporter = NULL, include_diagnose = FALSE) {
      if (is.null(excel_exporter)) {
        stop("excel_exporter must be provided. Create an ExcelExporter instance from iQualityR.core.")
      }

      df <- self$to_dataframe(results)

      data_list <- list("Normality Test Results" = df)
      sheet_names <- "Normality Test Results"

      if (include_diagnose && !is.null(attr(results, "diagnose"))) {
        diag_list <- attr(results, "diagnose")
        if (length(diag_list) > 0) {
          diag_rows <- list()
          for (name in names(diag_list)) {
            d <- diag_list[[name]]
            diag_rows[[name]] <- data.frame(
              Variable = name,
              N = d$n %||% NA,
              Mean = d$mean %||% NA,
              SD = d$sd %||% NA,
              Median = d$median %||% NA,
              Skewness = d$skewness %||% NA,
              Skewness_Direction = d$skewness_direction %||% NA,
              Kurtosis = d$kurtosis %||% NA,
              Excess_Kurtosis = d$excess_kurtosis %||% NA,
              Kurtosis_Type = d$kurtosis_type %||% NA,
              Min = d$min %||% NA,
              Max = d$max %||% NA,
              Q1 = d$q1 %||% NA,
              Q3 = d$q3 %||% NA,
              IQR = d$iqr %||% NA,
              stringsAsFactors = FALSE
            )
          }
          diag_df <- do.call(rbind, diag_rows)
          data_list[["Diagnostic Summary"]] <- diag_df
          sheet_names <- c(sheet_names, "Diagnostic Summary")
        }
      }

      if (!is.null(path)) {
        excel_exporter$export_excel(data_list, path = path, sheet_names = sheet_names)
      } else {
        excel_exporter$export_excel(data_list, sheet_names = sheet_names)
      }
    }
  )
)
