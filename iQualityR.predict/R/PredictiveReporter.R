# =============================================================================
# File: R/predict/PredictiveReporter.R
# Description: Quality Prediction Modeling Report Executor
# =============================================================================

#' @title PredictiveReporter: Quality Prediction Modeling Report Executor
#' @description
#' Responsible for exporting prediction modeling results as Excel or HTML format reports.
#' Inherits design pattern from IqrReporter, but optimized specifically for prediction modeling scenarios.
#'
#' **Report Content**:
#' - Model overview (type, parameters, rating)
#' - Performance metrics (R2, RMSE, MAE, etc.)
#' - Factor influence ranking
#' - Diagnostic conclusions and recommendations
#' - Key charts embedded
#'
#' @export
PredictiveReporter <- R6::R6Class("PredictiveReporter",
  public = list(
    #' @field theme_obj IqrTheme object
    theme_obj = NULL,

    #' @field excel_exporter ExcelExporter instance
    excel_exporter = NULL,

    #' @description Initialize reporter
    #' @param theme_obj IqrTheme object
    initialize = function(theme_obj) {
      if (!inherits(theme_obj, "IqrTheme")) {
        stop("[PredictiveReporter] theme_obj must be an IqrTheme instance", call. = FALSE)
      }
      self$theme_obj <- theme_obj
      self$excel_exporter <- iQualityR.core::ExcelExporter$new(theme_obj$config)
    },

    #' @description Unified export entry point
    #' @param results Analysis results
    #' @param plan PredictivePlan object
    #' @param format Output format ("excel", "html")
    #' @param path Output path
    #' @param ... Other parameters
    #' @return  invisble(self)
    export = function(results, plan, format = "excel", path = NULL, ...) {
      if (is.null(path)) {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        ext <- switch(format,
                      excel = "xlsx",
                      html = "html",
                      "xlsx"
        )
        path <- paste0("Quality_Prediction_Report_", plan$task_tag, "_", timestamp, ".", ext)
      }

      message("[iQualityR] Generating report: ", format, " -> ", path)

      switch(format,
        excel = private$.export_excel_report(results, plan, path, ...),
        html = private$.export_html_report(results, plan, path, ...),
        stop("[PredictiveReporter] Unsupported format: ", format, call. = FALSE)
      )

      message("[iQualityR] Report saved: ", path)
      invisible(self)
    }
  ),

  private = list(
    .export_excel_report = function(results, plan, path, ...) {
      # Build multi-sheet Excel report

      sheets <- list()

      # 1. Model overview sheet
      sheets[["Model Overview"]] <- private$.build_model_summary_sheet(results)

      # 2. Performance metrics sheet
      sheets[["Performance Metrics"]] <- private$.build_metrics_sheet(results)

      # 3. Factor influence sheet
      if (!is.null(results$explanation$feature_importance$importance)) {
        sheets[["Factor Influence"]] <- private$.build_feature_importance_sheet(results)
      }

      # 4. Diagnostic conclusions sheet
      sheets[["Diagnostic Conclusions"]] <- private$.build_diagnostics_sheet(results)

      # 5. Coefficients sheet (linear models)
      if (!is.null(results$coefficients)) {
        sheets[["Model Coefficients"]] <- results$coefficients
      }

      # Export
      self$excel_exporter$export_excel(data = sheets, path = path, ...)
    },

    .export_html_report = function(results, plan, path, ...) {
      # Use IqrReporter's Rmd export capability
      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        stop("[PredictiveReporter] Generating HTML report requires 'rmarkdown' package", call. = FALSE)
      }

      # Find template
      template <- system.file(
        "templates", "predictive_template.Rmd",
        package = "iQualityR"
      )

      if (template == "") {
        message("[PredictiveReporter] Predictive modeling template not found, generating simple HTML report")
        private$.export_simple_html(results, plan, path)
        return(invisible(self))
      }

      # Render
      rmarkdown::render(
        input = template,
        output_file = basename(path),
        output_dir = dirname(path),
        output_format = "html_document",
        params = list(
          results = results,
          plan = plan,
          theme_obj = self$theme_obj,
          timestamp = Sys.time()
        ),
        quiet = TRUE
      )
    },

    .export_simple_html = function(results, plan, path) {
      # Generate simple HTML report (without template dependency)
      html_content <- private$.generate_simple_html(results, plan)
      writeLines(html_content, path)
    },

    .generate_simple_html = function(results, plan) {
      paste0(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>",
        "<title>Quality Prediction Report</title></head><body>",
        "<h1>Quality Prediction Model Report</h1>",
        "<h2>Model Overview</h2>",
        "<p>Task Type: ", plan$task_tag, "</p>",
        "<p>Target Variable: ", plan$target_var, "</p>",
        "<p>Model Rating: ", results$model_rating$level, "</p>",
        "<h2>Performance Metrics</h2>",
        "<ul>",
        "<li>R2: ", results$metrics$r_squared, "</li>",
        "<li>RMSE: ", results$metrics$rmse, "</li>",
        "<li>MAE: ", results$metrics$mae, "</li>",
        "</ul>",
        "<h2>Diagnostic Recommendations</h2>",
        "<ul>",
        paste0("<li>", results$diagnostics$recommendations, "</li>", collapse = ""),
        "</ul>",
        "</body></html>"
      )
    },

    .build_model_summary_sheet = function(results) {
      data.frame(
        Item = c("Task Type", "Target Variable", "Model Type", "Running Mode", "Sample Size", "Model Rating"),
        Value = c(
          results$metadata$task_tag,
          results$metadata$target_var,
          results$metadata$model_type,
          ifelse(results$metadata$is_expert_mode, "Expert", "Business"),
          results$metadata$n_observations,
          paste0(results$model_rating$level, " (",
                 paste(rep("*", results$model_rating$stars), collapse = ""), ")")
        ),
        stringsAsFactors = FALSE
      )
    },

    .build_metrics_sheet = function(results) {
      metrics <- results$metrics
      data.frame(
        Metric = c("R2", "RMSE", "MAE", "MAPE(%)"),
        Value = c(
          metrics$r_squared,
          metrics$rmse,
          metrics$mae,
          metrics$mape
        ),
        stringsAsFactors = FALSE
      )
    },

    .build_feature_importance_sheet = function(results) {
      imp <- results$explanation$feature_importance
      data.frame(
        Factor = names(imp$importance),
        Influence = as.numeric(imp$importance),
        Percentage = imp$percentage,
        stringsAsFactors = FALSE
      )
    },

    .build_diagnostics_sheet = function(results) {
      diags <- results$diagnostics
      rows <- list()

      # Normality
      if (!is.null(diags$normality$shapiro_wilk)) {
        rows[["Normality Test"]] <- diags$normality$shapiro_wilk$interpretation
      }

      # Homoscedasticity
      if (!is.null(diags$homoscedasticity$breusch_pagan)) {
        rows[["Homoscedasticity"]] <- diags$homoscedasticity$breusch_pagan$interpretation
      }

      # Multicollinearity
      if (!is.null(diags$multicollinearity$interpretation)) {
        rows[["Multicollinearity"]] <- diags$multicollinearity$interpretation
      }

      # Recommendations
      if (length(diags$recommendations) > 0) {
        rows[["Improvement Recommendations"]] <- paste(diags$recommendations, collapse = "; ")
      }

      data.frame(
        Item = names(rows),
        Conclusion = unlist(rows),
        stringsAsFactors = FALSE
      )
    }
  )
)
