# =============================================================================
# File: R/package.R
# Description: Package-level documentation and import directives
# =============================================================================

#' iQualityR.capa
#'
#' Process capability analysis tools for the iQualityR ecosystem.
#'
#' @keywords internal
#' @import iQualityR.core
#' @import ggplot2
#' @importFrom R6 R6Class
#' @importFrom stats sd var median quantile dnorm pnorm qnorm dweibull pweibull
#'   qweibull dlnorm plnorm qlnorm dgamma pgamma qgamma dexp pexp qexp dlogis
#'   plogis qlogis dbeta pbeta qbeta ks.test rnorm
#' @importFrom utils head tail
#' @importFrom data.table data.table as.data.table
#' @importFrom MASS fitdistr
#' @importFrom patchwork plot_annotation wrap_plots
#' @importFrom mvtnorm pmvnorm
#' @importFrom iQualityR.stat sigma_decomposition sigma_estimate normality_test
#'   capability_interpret capability_to_ppm get_D4 fit_distribution z_bench
#'   box_cox_transform johnson_transform auto_transform
#' @importFrom iQualityR.plot base_plot as_iqr_theme as_iqr_theme_object
#'   layers_histogram_density layers_spec_limits layers_qq layers_control_chart
"_PACKAGE"

utils::globalVariables("self")

.onLoad <- function(libname, pkgname) {
  invisible()
}

# =============================================================================
# capability_to_excel_data: convert capability results to Excel sheet list
#
# Returned to IqrReporter$export_excel() when the user calls
# task$report(format = "excel"). Each list element becomes one worksheet.
# Sheets are organized to mirror the Rmd template's sections so that Excel
# and HTML reports present a consistent story.
# =============================================================================

#' Convert capability results to an Excel sheet list (internal)
#'
#' @param results Result list returned by `CapabilityAnalyzer`.
#' @param plan Optional `CapabilityPlan` object (used for spec limits / labels).
#' @return Named list; each element is a data frame corresponding to one
#'   Excel worksheet.
#' @keywords internal
capability_to_excel_data <- function(results, plan = NULL) {
  sheets <- list()

  stats <- results$statistics
  diag  <- results$diagnostics
  tbls  <- results$data_tables

  # ---- Sheet 1: Overview ------------------------------------------------
  overview <- data.frame(
    Parameter = c("Analysis type", "LSL", "USL", "Target",
                  "Sample size (n)", "Process mean",
                  "StdDev (Within)", "StdDev (Overall)",
                  "Confidence level"),
    Value = c(
      if (!is.null(plan$analysis_type)) plan$analysis_type else "normal",
      if (!is.null(plan$lsl))  format(plan$lsl,  digits = 6)  else "NA",
      if (!is.null(plan$usl))  format(plan$usl,  digits = 6)  else "NA",
      if (!is.null(plan$target)) format(plan$target, digits = 6) else "NA",
      stats$n,
      format(stats$mean, digits = 6),
      if (!is.null(stats$sd_within))  format(stats$sd_within,  digits = 6)  else "N/A",
      format(stats$sd_overall, digits = 6),
      if (!is.null(plan$conf_level)) format(plan$conf_level, digits = 4) else "0.95"
    ),
    stringsAsFactors = FALSE
  )
  sheets$Overview <- overview

  # ---- Sheet 2: Capability Indices --------------------------------------
  idx_df <- data.frame(
    Index = c("Cp", "Cpk", "Cpl", "Cpu", "Pp", "Ppk", "Ppl", "Ppu"),
    Value = c(stats$cp, stats$cpk, stats$cpl, stats$cpu,
              stats$pp, stats$ppk, stats$ppl, stats$ppu),
    stringsAsFactors = FALSE
  )
  if (!is.null(stats$cpm)) {
    idx_df <- rbind(idx_df, data.frame(Index = "Cpm", Value = stats$cpm))
  }
  sheets$Indices <- idx_df

  # ---- Sheet 3: Judgment ------------------------------------------------
  judgment <- diag$capability_judgment
  if (!is.null(judgment)) {
    j_df <- data.frame(
      Metric = c("Cpk status", "Ppk status", "Cp status", "Pp status",
                 "Overall verdict"),
      Status = c(judgment$cpk_status, judgment$ppk_status,
                 judgment$cp_status,  judgment$pp_status,
                 judgment$overall_verdict),
      stringsAsFactors = FALSE
    )
    sheets$Judgment <- j_df
  }

  # ---- Sheet 4: PPM ------------------------------------------------------
  if (!is.null(tbls$ppm_within) && nrow(tbls$ppm_within) > 0) {
    sheets$PPM_Within <- tbls$ppm_within
  }
  if (!is.null(tbls$ppm_overall) && nrow(tbls$ppm_overall) > 0) {
    sheets$PPM_Overall <- tbls$ppm_overall
  }

  # ---- Sheet 5: Diagnostics ----------------------------------------------
  diag_rows <- list(Parameter = character(), Value = character())
  add_diag <- function(name, value) {
    diag_rows$Parameter <<- c(diag_rows$Parameter, name)
    diag_rows$Value     <<- c(diag_rows$Value,
                              if (is.null(value)) "NA" else as.character(value))
  }
  if (!is.null(diag$normality_method)) add_diag("Normality method", diag$normality_method)
  if (!is.null(diag$normality_p_value)) {
    add_diag("Normality p-value", format(diag$normality_p_value, digits = 4))
  }
  if (!is.null(diag$distribution))  add_diag("Fitted distribution", diag$distribution)
  if (!is.null(diag$aic))          add_diag("AIC", format(diag$aic, digits = 4))
  if (!is.null(diag$bic))          add_diag("BIC", format(diag$bic, digits = 4))
  if (!is.null(diag$ks_statistic)) add_diag("KS statistic", format(diag$ks_statistic, digits = 4))
  if (!is.null(diag$ks_p_value))   add_diag("KS p-value",  format(diag$ks_p_value,  digits = 4))
  if (!is.null(diag$method))       add_diag("Nonparametric method", diag$method)
  if (length(diag_rows$Parameter) > 0) {
    sheets$Diagnostics <- data.frame(diag_rows, stringsAsFactors = FALSE)
  }

  # ---- Sheet 6: Warnings -------------------------------------------------
  warnings <- diag$warnings
  if (length(warnings) > 0) {
    sheets$Warnings <- data.frame(Warning = warnings, stringsAsFactors = FALSE)
  }

  # ---- Sheet 7: Advice ---------------------------------------------------
  advice <- diag$process_advice
  if (length(advice) > 0) {
    sheets$Advice <- data.frame(Recommendation = advice, stringsAsFactors = FALSE)
  }

  # ---- Sheet 8: Bootstrap CI --------------------------------------------
  if (!is.null(tbls$bootstrap_ci) && nrow(tbls$bootstrap_ci) > 0) {
    sheets$Bootstrap_CI <- tbls$bootstrap_ci
  }

  # ---- Sheet 9: Raw data ------------------------------------------------
  if (!is.null(tbls$raw_data)) {
    sheets$Raw_Data <- tbls$raw_data
  }

  sheets
}
