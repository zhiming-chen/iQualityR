# =============================================================================
# File: R/ReliabilityReporter.R
# Description: Reliability and survival analysis report export engine
# =============================================================================

#' @title ReliabilityReporter: Reliability Report Export Engine
#'
#' @description
#' Exports reliability analysis results to structured formats:
#' - Console printing
#' - Data frame conversion
#' - Excel workbook (via [openxlsx], with `requireNamespace()` guard)
#'
#' @export
ReliabilityReporter <- R6::R6Class("ReliabilityReporter",
  public = list(
    #' @description Print a console report.
    #' @param results Analysis results list (from [ReliabilityAnalyzer]).
    #' @param plan [ReliabilityPlan] object (unused, reserved for future).
    print_console = function(results, plan = NULL) {
      cat("\n[iQualityR Reliability Analysis Report]\n")
      cat(rep("=", 50), "\n")

      # Basic information
      cat(sprintf("Method:        %s\n", results$method))
      cat(sprintf("Sample size:   %d\n", results$n))
      if (!is.null(results$n_events)) {
        cat(sprintf("Events:        %d\n", results$n_events))
      }
      if (!is.null(results$n_censored)) {
        cat(sprintf("Censored:      %d\n", results$n_censored))
      }
      cat("\n")

      # Distribution fit results
      if (results$method == "parametric") {
        cat("[Distribution Fit]\n")
        fit <- results$distribution_fit
        cat(sprintf("  Distribution: %s\n", fit$distribution))
        cat(sprintf("  Parameters:   %s\n",
                    paste(names(fit$parameters), "=",
                          round(unlist(fit$parameters), 4), collapse = ", ")))

        # Goodness of fit
        gof <- fit$goodness_of_fit
        if (!is.null(gof) && length(gof) > 0) {
          p_val <- gof$ks_p_value
          if (!is.null(p_val) && length(p_val) > 0 && !is.na(p_val)) {
            cat(sprintf("  KS statistic: %.4f (p = %.4f)\n",
                        .safe_num(gof$ks_statistic), p_val))
          }
          if (!is.null(gof$interpretation)) {
            cat(sprintf("  Fit verdict:  %s\n", gof$interpretation))
          }
        }
        cat("\n")

        # Reliability metrics
        cat("[Reliability Metrics]\n")
        metrics <- results$reliability_metrics
        if (!is.null(metrics$mttf))               cat(sprintf("  MTTF:               %.2f\n", metrics$mttf))
        if (!is.null(metrics$b10_life))           cat(sprintf("  B10 life:           %.2f\n", metrics$b10_life))
        if (!is.null(metrics$b50_life))           cat(sprintf("  B50 life:           %.2f\n", metrics$b50_life))
        if (!is.null(metrics$characteristic_life)) cat(sprintf("  Characteristic life: %.2f\n", metrics$characteristic_life))
        if (!is.null(metrics$shape_parameter))    cat(sprintf("  Shape (beta):       %.2f\n", metrics$shape_parameter))
        if (!is.null(metrics$failure_rate))       cat(sprintf("  Failure rate (lambda): %.6f\n", metrics$failure_rate))
        cat("\n")
      }

      # Cox model results
      if (results$method == "cox" && !is.null(results$cox_model)) {
        cat("[Cox Proportional Hazards Model]\n")
        cat(sprintf("  Concordance:       %.3f\n", results$cox_model$concordance))
        cat(sprintf("  Likelihood ratio:  chisq = %.2f, p = %.4f\n",
                    results$cox_model$likelihood_ratio_test$chisq,
                    results$cox_model$likelihood_ratio_test$p_value))
        cat("\n")

        cat("[Coefficient Estimates]\n")
        coef_df <- results$cox_model$coefficients
        print(coef_df[, c("factor", "coefficient", "hazard_ratio", "p_value")],
              row.names = FALSE)
        cat("\n")
      }

      # Diagnostics
      if (!is.null(results$diagnostics)) {
        if (length(results$diagnostics$warnings) > 0) {
          cat("[Warnings]\n")
          for (w in results$diagnostics$warnings) {
            cat(sprintf("  ! %s\n", w))
          }
          cat("\n")
        }
        if (length(results$diagnostics$recommendations) > 0) {
          cat("[Recommendations]\n")
          for (r in results$diagnostics$recommendations) {
            cat(sprintf("  - %s\n", r))
          }
          cat("\n")
        }
      }

      cat(rep("=", 50), "\n")
      invisible(self)
    },

    #' @description Convert results to a data frame.
    #' @param results Analysis results list.
    #' @return Data frame.
    to_dataframe = function(results) {
      if (results$method == "parametric") {
        private$.to_dataframe_parametric(results)
      } else if (results$method == "kaplan_meier") {
        private$.to_dataframe_km(results)
      } else if (results$method == "cox") {
        private$.to_dataframe_cox(results)
      } else {
        NULL
      }
    },

    #' @description Export results to an Excel workbook.
    #' @param results Analysis results list.
    #' @param plan [ReliabilityPlan] object (unused, reserved for future).
    #' @param path Output file path.
    #' @param ... Additional arguments (ignored; reserved for compatibility).
    export_excel = function(results, plan = NULL, path, ...) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("[ReliabilityReporter] The 'openxlsx' package is required for Excel export. ",
             "Please install it with install.packages('openxlsx').", call. = FALSE)
      }

      sheets <- list()

      # 1. Summary
      sheets[["Summary"]] <- private$.summary_sheet(results)

      # 2. Distribution parameters
      if (results$method == "parametric") {
        sheets[["Distribution Parameters"]] <- private$.dist_params_sheet(results)
        sheets[["Survival Table"]]          <- results$survival_function
        sheets[["Hazard Table"]]            <- results$hazard_function
      }

      # 3. Kaplan-Meier
      if (results$method == "kaplan_meier") {
        sheets[["Survival Curve"]] <- results$survival_curve
      }

      # 4. Cox model
      if (results$method == "cox") {
        sheets[["Cox Coefficients"]] <- results$cox_model$coefficients
        sheets[["Model Tests"]] <- data.frame(
          Test = c("Concordance", "Likelihood ratio"),
          Value = c(
            round(results$cox_model$concordance, 4),
            sprintf("chisq=%.2f, p=%.4f",
                    results$cox_model$likelihood_ratio_test$chisq,
                    results$cox_model$likelihood_ratio_test$p_value)
          ),
          stringsAsFactors = FALSE
        )
      }

      # 5. Diagnostics and recommendations
      sheets[["Diagnostics"]] <- data.frame(
        Type = c(rep("Warning", length(results$diagnostics$warnings)),
                 rep("Recommendation", length(results$diagnostics$recommendations))),
        Content = c(results$diagnostics$warnings, results$diagnostics$recommendations),
        stringsAsFactors = FALSE
      )

      # Write workbook
      wb <- openxlsx::createWorkbook()
      for (nm in names(sheets)) {
        df <- sheets[[nm]]
        if (is.null(df) || nrow(df) == 0) next
        openxlsx::addWorksheet(wb, nm)
        openxlsx::writeData(wb, nm, df)
      }
      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

      message("[ReliabilityReporter] Report exported to: ", path)
      invisible(self)
    }
  ),

  private = list(
    .to_dataframe_parametric = function(results) {
      surv_df <- results$survival_function
      attr(surv_df, "metrics")     <- results$reliability_metrics
      attr(surv_df, "distribution") <- results$distribution
      surv_df
    },

    .to_dataframe_km = function(results) {
      results$survival_curve
    },

    .to_dataframe_cox = function(results) {
      results$cox_model$coefficients
    },

    .summary_sheet = function(results) {
      rows <- list(
        "Method"      = results$method,
        "Sample size" = results$n
      )
      if (!is.null(results$n_events))   rows[["Events"]]   <- results$n_events
      if (!is.null(results$n_censored)) rows[["Censored"]] <- results$n_censored
      if (results$method == "parametric") {
        rows[["Distribution"]] <- results$distribution
        rows[["MTTF"]]         <- results$reliability_metrics$mttf
        rows[["B10 life"]]    <- results$reliability_metrics$b10_life
      }

      data.frame(
        Item  = names(rows),
        Value = unlist(rows),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    },

    .dist_params_sheet = function(results) {
      fit    <- results$distribution_fit
      params <- fit$parameters
      ci     <- fit$confidence_intervals

      data.frame(
        Parameter   = names(params),
        Estimate    = unlist(params),
        CI_lower    = unlist(lapply(ci, `[`, 1)),
        CI_upper    = unlist(lapply(ci, `[`, 2)),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  )
)

# Helper: safe numeric coercion
.safe_num <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) NA_real_ else as.numeric(x)
}
