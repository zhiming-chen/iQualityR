# =============================================================================
# File: R/SamplingReporter.R
# Description: Sampling plan report export engine (console / data frame / Excel)
# =============================================================================

#' @title SamplingReporter: Sampling Plan Report Export Engine
#'
#' @description
#' Exports sampling analysis results to structured formats:
#' - Console printing
#' - Data frame conversion
#' - Excel workbook (via openxlsx, with `requireNamespace()` guard)
#'
#' @export
SamplingReporter <- R6::R6Class("SamplingReporter",
  public = list(
    #' @description Print a console report.
    #' @param results Analysis results list (from [SamplingAnalyzer]).
    #' @param plan [SamplingPlan] object (optional).
    print_console = function(results, plan = NULL) {
      cat("\n[iQualityR Sampling Plan Analysis Report]\n")
      cat(rep("=", 50), "\n", sep = "")

      meta <- results$meta
      if (!is.null(meta)) {
        cat(sprintf("  Sampling type:     %s\n", meta$sampling_type))
        cat(sprintf("  Sample size (n):   %s\n", .safe_num(meta$sample_size, 0)))
        cat(sprintf("  Acceptance no (c): %s\n", .safe_num(meta$acceptance_number, 0)))
        cat(sprintf("  AQL:               %s\n", .safe_num(meta$aql, 4)))
        cat(sprintf("  RQL:               %s\n", .safe_num(meta$rql, 4)))
        cat(sprintf("  Producer risk (alpha): %s\n", .safe_num(meta$alpha, 4)))
        cat(sprintf("  Consumer risk (beta):  %s\n", .safe_num(meta$beta, 4)))
      }
      cat("\n")

      rk <- results$risk_analysis
      if (!is.null(rk)) {
        cat("[Risk Analysis]\n")
        cat(sprintf("  Producer's risk (actual): %s (target: %s)\n",
                    .safe_num(rk$producer_risk, 4),
                    .safe_num(rk$risk_profile$target_alpha, 4)))
        cat(sprintf("  Consumer's risk (actual): %s (target: %s)\n",
                    .safe_num(rk$consumer_risk, 4),
                    .safe_num(rk$risk_profile$target_beta, 4)))
        cat("\n")
      }

      pw <- results$power_analysis
      if (!is.null(pw)) {
        cat("[Power Analysis]\n")
        cat(sprintf("  Achieved power at RQL: %s\n",
                    .safe_num(pw$achieved_power, 4)))
        cat(sprintf("  Required n (80%% power): %s\n",
                    .safe_num(pw$required_sample_size, 0)))
        cat("\n")
      }

      asn <- results$asn_curve
      if (!is.null(asn)) {
        cat("[ASN Curve]\n")
        cat(sprintf("  Single-stage n:  %s\n", .safe_num(asn$single_sample_n, 0)))
        cat(sprintf("  Mean ASN:        %s\n",
                    .safe_num(mean(asn$asn_values, na.rm = TRUE), 2)))
        cat("\n")
      }

      ac <- results$actual_sampling
      if (!is.null(ac)) {
        cat("[Actual Sampling]\n")
        if (!is.null(ac$warning)) {
          cat(sprintf("  Warning: %s\n", ac$warning))
          cat(sprintf("  Available: %s, Required: %s\n",
                      .safe_num(ac$n_available, 0),
                      .safe_num(ac$n_required, 0)))
        } else {
          cat(sprintf("  Sampled:        %s\n", .safe_num(ac$n_sampled, 0)))
          cat(sprintf("  Defectives:     %s\n", .safe_num(ac$n_defective, 0)))
          cat(sprintf("  Acceptance no:  %s\n",
                      .safe_num(ac$acceptance_number, 0)))
          cat(sprintf("  Decision:       %s\n",
                      if (ac$accepted) "ACCEPT" else "REJECT"))
          cat(sprintf("  Defect rate:    %s\n", .safe_num(ac$defect_rate, 4)))
        }
        cat("\n")
      }

      invisible(self)
    },

    #' @description Convert results to a data frame (one row per key metric).
    #' @param results Analysis results list.
    #' @return A data frame.
    to_dataframe = function(results) {
      meta <- results$meta %||% list()
      rk <- results$risk_analysis %||% list()
      pw <- results$power_analysis %||% list()

      data.frame(
        metric = c("sample_size", "acceptance_number", "aql", "rql",
                   "producer_risk", "consumer_risk",
                   "achieved_power", "required_n_80pct"),
        value = c(
          .safe_num(meta$sample_size, 0),
          .safe_num(meta$acceptance_number, 0),
          .safe_num(meta$aql, 6),
          .safe_num(meta$rql, 6),
          .safe_num(rk$producer_risk, 6),
          .safe_num(rk$consumer_risk, 6),
          .safe_num(pw$achieved_power, 6),
          .safe_num(pw$required_sample_size, 0)
        ),
        stringsAsFactors = FALSE
      )
    },

    #' @description Export results to an Excel workbook.
    #' @param results Analysis results list.
    #' @param path Character. Output file path.
    #' @return Invisible path to the written file.
    to_excel = function(results, path) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("[SamplingReporter] Package 'openxlsx' is required for Excel export. ",
             "Please install it with install.packages('openxlsx').",
             call. = FALSE)
      }

      meta <- results$meta %||% list()
      rk <- results$risk_analysis %||% list()
      pw <- results$power_analysis %||% list()

      summary_df <- data.frame(
        field = c("sampling_type", "sample_size", "acceptance_number",
                  "aql", "rql", "alpha", "beta",
                  "producer_risk", "consumer_risk",
                  "achieved_power", "required_n_80pct"),
        value = c(
          meta$sampling_type %||% NA,
          .safe_num(meta$sample_size, 0),
          .safe_num(meta$acceptance_number, 0),
          .safe_num(meta$aql, 6),
          .safe_num(meta$rql, 6),
          .safe_num(meta$alpha, 6),
          .safe_num(meta$beta, 6),
          .safe_num(rk$producer_risk, 6),
          .safe_num(rk$consumer_risk, 6),
          .safe_num(pw$achieved_power, 6),
          .safe_num(pw$required_sample_size, 0)
        ),
        stringsAsFactors = FALSE
      )

      oc_df <- data.frame()
      if (!is.null(results$oc_curve)) {
        oc_df <- data.frame(
          p = results$oc_curve$p_values,
          acceptance_probability = results$oc_curve$acceptance_probabilities
        )
      }

      power_df <- data.frame()
      if (!is.null(pw) && length(pw$p_values) > 0) {
        power_df <- data.frame(
          p = pw$p_values,
          power = pw$powers
        )
      }

      asn_df <- data.frame()
      if (!is.null(results$asn_curve)) {
        asn_df <- data.frame(
          p = results$asn_curve$p_values,
          asn = results$asn_curve$asn_values
        )
      }

      sheets <- list(
        Summary = summary_df,
        `OC Curve` = oc_df,
        `Power Curve` = power_df,
        `ASN Curve` = asn_df
      )
      sheets <- sheets[sapply(sheets, function(x) nrow(x) > 0)]

      wb <- openxlsx::createWorkbook()
      for (sheet_name in names(sheets)) {
        openxlsx::addWorksheet(wb, sheet_name)
        openxlsx::writeData(wb, sheet_name, sheets[[sheet_name]])
      }
      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
      invisible(path)
    }
  )
)
