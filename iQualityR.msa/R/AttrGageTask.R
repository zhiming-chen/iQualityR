# =============================================================================
# File: R/AttrGageTask.R
# Description: Attribute agreement task coordinator.
# =============================================================================

#' @title AttrGageTask
#' @description Task coordinator for attribute agreement analysis.
#'
#' @field plan Optional AttrGagePlan object with study parameters.
#' @field kappa_results List holding kappa statistics and raw output after `compute_kappa()`.
#' @field detection_results List holding detection statistics and raw output after `compute_detection()`.
#' @field mode Character scalar: `"kappa"`, `"detection"`, or `"all"`.
#'
#' @param data Data frame of attribute ratings.
#' @param plan Optional [AttrGagePlan] object.
#' @param theme Theme name or object accepted by the base task.
#' @param mode Character scalar: `"kappa"`, `"detection"`, or `"all"`.
#' @param kappa_method Character scalar kappa method or `"auto"` for inference.
#' @param type Plot type: `"summary"` or `"list"`.
#' @param format Report format: `"excel"`, `"html"`, `"pdf"`, `"docx"`, or `"word"`.
#' @param path Output file path. If `NULL`, a default path is generated.
#' @param ... Additional arguments passed to downstream methods.
#'
#' @export
AttrGageTask <- R6::R6Class(
  "AttrGageTask",
  inherit = IqrTaskBase,
  public = list(
    plan = NULL,
    kappa_results = NULL,
    detection_results = NULL,
    mode = NULL,

    initialize = function(data = NULL, plan = NULL, theme = "academic",
                          mode = c("kappa", "detection", "all"),
                          kappa_method = "auto") {
      self$mode <- match.arg(mode)
      super$initialize(data = data, theme = theme)
      self$plan <- plan
      self$executor$kappa_analyzer <- KappaAnalyzer$new()
      self$executor$detection_analyzer <- DetectionAnalyzer$new()
      self$executor$plotter <- AttrGagePlotter$new()

      if (!identical(kappa_method, "auto")) {
        self$executor$kappa_analyzer$params$kappa_method <- kappa_method
      }

      invisible(self)
    },

    compute = function() {
      if (is.null(self$data)) {
        stop("No data available for attribute agreement analysis.", call. = FALSE)
      }

      switch(self$mode,
        kappa = self$compute_kappa(),
        detection = self$compute_detection(),
        all = {
          self$compute_kappa()
          self$compute_detection()
        }
      )

      self$results <- self$build_results()
      invisible(self)
    },

    compute_kappa = function() {
      plan_params <- if (!is.null(self$plan)) self$plan$to_list() else list()

      if (is.null(plan_params$comparison_mode)) {
        plan_params$comparison_mode <- "one_way"
      }
      if (is.null(plan_params$kappa_method)) {
        # Infer kappa method from data structure, not just column count.
        # Cohen: exactly 2 raters in wide format (eval1, eval2 columns)
        # Fleiss: long format with multiple raters (sample, rater, rating columns)
        col_names <- tolower(names(self$data))
        has_eval_cols <- any(grepl("eval", col_names)) && sum(grepl("eval", col_names)) >= 2
        has_long_cols <- any(grepl("rater", col_names)) || any(grepl("appraiser", col_names))
        plan_params$kappa_method <- if (has_eval_cols && !has_long_cols) "cohen" else "fleiss"
      }

      self$executor$kappa_analyzer$run(self$data, plan_params)
      self$kappa_results <- self$executor$kappa_analyzer$results
      message("[AttrGage] Kappa analysis completed")
      invisible(self)
    },

    compute_detection = function() {
      plan_params <- if (!is.null(self$plan)) self$plan$to_list() else list()
      plan_params$conf_level <- plan_params$conf_level %||% 0.95

      self$executor$detection_analyzer$run(self$data, plan_params)
      self$detection_results <- self$executor$detection_analyzer$results
      message("[AttrGage] Detection analysis completed")
      invisible(self)
    },

    summary = function() {
      cat("\n========== Attribute Agreement Summary ==========\n")

      if (!is.null(self$kappa_results$raw_output)) {
        r <- self$kappa_results$raw_output
        ci <- r$ci %||% c(NA_real_, NA_real_)
        cat("\n--- Kappa Statistics ---\n")
        .msa_print_table(data.frame(
          Statistic = c("Method", "Kappa", "SE Kappa", "Z", "P(vs > 0)", "95% CI", "Observed agreement", "Expected agreement", "Interpretation"),
          Value = c(
            r$method %||% "N/A",
            .msa_fmt_num(r$kappa %||% r$V %||% NA_real_, 4),
            .msa_fmt_num(r$se %||% NA_real_, 4),
            .msa_fmt_num(r$z %||% NA_real_, 4),
            .msa_fmt_p(r$p_value %||% NA_real_, 4),
            paste0("[", .msa_fmt_num(ci[1], 4), ", ", .msa_fmt_num(ci[2], 4), "]"),
            .msa_fmt_pct(r$Po %||% NA_real_, 2, scale = TRUE),
            .msa_fmt_pct(r$Pe %||% NA_real_, 2, scale = TRUE),
            r$interpretation %||% r$grade %||% "N/A"
          )
        ))
        if (!is.null(r$pairwise_appraisers) && nrow(r$pairwise_appraisers) > 0) {
          cat("\n--- Between Appraisers ---\n")
          .msa_print_table(.msa_round_numeric_columns(r$pairwise_appraisers, 4))
        }
        if (!is.null(r$appraiser_vs_standard) && nrow(r$appraiser_vs_standard) > 0) {
          cat("\n--- Each Appraiser vs Standard ---\n")
          .msa_print_table(.msa_round_numeric_columns(r$appraiser_vs_standard, 4))
        }
        if (!is.null(r$within_appraiser) && nrow(r$within_appraiser) > 0) {
          cat("\n--- Within Appraiser Repeatability ---\n")
          .msa_print_table(.msa_round_numeric_columns(r$within_appraiser, 4))
        }
        if (!is.null(r$ordinal)) {
          cat("\n--- Ordinal Kendall Statistics ---\n")
          if (!is.null(r$ordinal$between_appraisers) && nrow(r$ordinal$between_appraisers) > 0) {
            cat("\nKendall's Coefficient of Concordance\n")
            .msa_print_table(.msa_round_numeric_columns(r$ordinal$between_appraisers, 4))
          }
          if (!is.null(r$ordinal$appraiser_vs_standard) && nrow(r$ordinal$appraiser_vs_standard) > 0) {
            cat("\nKendall's Correlation Coefficient vs Standard\n")
            .msa_print_table(.msa_round_numeric_columns(r$ordinal$appraiser_vs_standard, 4))
          }
        }
        if (!is.null(r$response_table) && nrow(r$response_table) > 0) {
          cat("\n--- Response-Level Agreement ---\n")
          .msa_print_table(.msa_round_numeric_columns(r$response_table, 4))
        }
      }

      if (!is.null(self$detection_results$raw_output)) {
        r <- self$detection_results$raw_output
        cat("\n--- Detection Performance ---\n")
        .msa_print_table(data.frame(
          Metric = c("Sensitivity", "False negative rate", "Specificity", "False positive rate", "Youden index", "LR+", "LR-", "Prevalence"),
          Estimate = c(
            .msa_fmt_pct(r$detection_rate, 2, scale = TRUE),
            .msa_fmt_pct(r$false_negative_rate, 2, scale = TRUE),
            .msa_fmt_pct(r$specificity, 2, scale = TRUE),
            .msa_fmt_pct(r$false_positive_rate, 2, scale = TRUE),
            .msa_fmt_num(r$youden_index, 4),
            .msa_fmt_num(r$LR_positive, 4),
            .msa_fmt_num(r$LR_negative, 4),
            .msa_fmt_pct(r$prevalence, 2, scale = TRUE)
          ),
          CI = c(
            paste0("[", .msa_fmt_pct(r$detection_ci$lower, 2, scale = TRUE), ", ", .msa_fmt_pct(r$detection_ci$upper, 2, scale = TRUE), "]"),
            "",
            paste0("[", .msa_fmt_pct(r$specificity_ci$lower, 2, scale = TRUE), ", ", .msa_fmt_pct(r$specificity_ci$upper, 2, scale = TRUE), "]"),
            "",
            "",
            "",
            "",
            ""
          )
        ))
      }

      cat("=================================================\n")
      invisible(self)
    },

    plot = function(type = "summary", theme = NULL, ...) {
      if (is.null(self$kappa_results) && is.null(self$detection_results)) {
        stop("No results available. Please call compute() first.", call. = FALSE)
      }

      theme_obj <- theme %||% self$theme_obj
      self$executor$plotter$render(self$build_results(), theme_obj = theme_obj, type = type, ...)
    },

    build_results = function() {
      statistics <- list()
      diagnostics <- list()

      if (!is.null(self$kappa_results)) {
        statistics$kappa <- self$kappa_results$statistics
        diagnostics$kappa_interpretation <- self$kappa_results$raw_output$interpretation %||%
          self$kappa_results$raw_output$grade %||% NULL
      }
      if (!is.null(self$detection_results)) {
        statistics$detection <- self$detection_results$statistics
      }

      data_tables <- list()
      if (!is.null(self$kappa_results$raw_output)) {
        kr <- self$kappa_results$raw_output
        data_tables$pairwise_appraisers <- kr$pairwise_appraisers %||% data.frame()
        data_tables$appraiser_vs_standard <- kr$appraiser_vs_standard %||% data.frame()
        data_tables$within_appraiser <- kr$within_appraiser %||% data.frame()
        data_tables$response_agreement <- kr$response_table %||% data.frame()
        data_tables$response_kappa <- kr$response_kappa %||% data.frame()
        data_tables$response_kappa_vs_standard <- kr$response_kappa_vs_standard %||% data.frame()
        data_tables$sample_disagreement <- kr$sample_disagreement %||% data.frame()
        data_tables$rating_matrix <- if (!is.null(kr$rating_matrix)) as.data.frame.matrix(kr$rating_matrix) else data.frame()
        data_tables$kendall_within_appraiser <- kr$ordinal$within_appraiser %||% data.frame()
        data_tables$kendall_between_appraisers <- kr$ordinal$between_appraisers %||% data.frame()
        data_tables$kendall_appraiser_vs_standard <- kr$ordinal$appraiser_vs_standard %||% data.frame()
        data_tables$kendall_all_vs_standard <- kr$ordinal$all_appraisers_vs_standard %||% data.frame()
      }
      if (!is.null(self$detection_results$raw_output)) {
        dr <- self$detection_results$raw_output
        data_tables$detection_confusion <- dr$confusion_matrix_table %||% data.frame()
        data_tables$detection_risk <- dr$risk_table %||% data.frame()
      }

      list(
        study_type = "attr_gage",
        mode = self$mode,
        statistics = statistics,
        diagnostics = diagnostics,
        data_tables = data_tables,
        raw_output = list(kappa = self$kappa_results, detection = self$detection_results),
        kappa_results = self$kappa_results,
        detection_results = self$detection_results,
        to_excel = function(plan = self$plan) self$build_excel_sheets()
      )
    },

    build_excel_sheets = function() {
      sheets <- list()

      if (!is.null(self$kappa_results$raw_output)) {
        r <- self$kappa_results$raw_output
        ci <- r$ci %||% c(NA_real_, NA_real_)
        sheets$Kappa_Results <- data.frame(
          Metric = c(
            "Method",
            "Kappa",
            "SE Kappa",
            "Z",
            "P(vs > 0)",
            "Observed Agreement",
            "Expected Agreement",
            "95% CI Lower",
            "95% CI Upper",
            "Interpretation"
          ),
          Value = c(
            r$method %||% "N/A",
            sprintf("%.4f", r$kappa %||% r$V %||% NA_real_),
            sprintf("%.4f", r$se %||% NA_real_),
            sprintf("%.4f", r$z %||% NA_real_),
            sprintf("%.4g", r$p_value %||% NA_real_),
            sprintf("%.4f", r$Po %||% NA_real_),
            sprintf("%.4f", r$Pe %||% NA_real_),
            sprintf("%.4f", ci[1]),
            sprintf("%.4f", ci[2]),
            r$interpretation %||% r$grade %||% "N/A"
          )
        )
        if (!is.null(r$pairwise_appraisers) && nrow(r$pairwise_appraisers) > 0) {
          sheets$Pairwise_Appraisers <- .msa_round_numeric_columns(r$pairwise_appraisers, 4)
        }
        if (!is.null(r$appraiser_vs_standard) && nrow(r$appraiser_vs_standard) > 0) {
          sheets$Appraiser_vs_Standard <- .msa_round_numeric_columns(r$appraiser_vs_standard, 4)
        }
        if (!is.null(r$within_appraiser) && nrow(r$within_appraiser) > 0) {
          sheets$Within_Appraiser <- .msa_round_numeric_columns(r$within_appraiser, 4)
        }
        if (!is.null(r$response_table) && nrow(r$response_table) > 0) {
          sheets$Response_Agreement <- .msa_round_numeric_columns(r$response_table, 4)
        }
        if (!is.null(r$response_kappa) && nrow(r$response_kappa) > 0) {
          sheets$Response_Kappa <- .msa_round_numeric_columns(r$response_kappa, 4)
        }
        if (!is.null(r$response_kappa_vs_standard) && nrow(r$response_kappa_vs_standard) > 0) {
          sheets$Response_Kappa_vs_Standard <- .msa_round_numeric_columns(r$response_kappa_vs_standard, 4)
        }
        if (!is.null(r$sample_disagreement) && nrow(r$sample_disagreement) > 0) {
          sheets$Sample_Disagreement <- .msa_round_numeric_columns(r$sample_disagreement, 4)
        }
        if (!is.null(r$ordinal)) {
          if (!is.null(r$ordinal$within_appraiser) && nrow(r$ordinal$within_appraiser) > 0) {
            sheets$Kendall_Within <- .msa_round_numeric_columns(r$ordinal$within_appraiser, 4)
          }
          if (!is.null(r$ordinal$between_appraisers) && nrow(r$ordinal$between_appraisers) > 0) {
            sheets$Kendall_Between <- .msa_round_numeric_columns(r$ordinal$between_appraisers, 4)
          }
          if (!is.null(r$ordinal$appraiser_vs_standard) && nrow(r$ordinal$appraiser_vs_standard) > 0) {
            sheets$Kendall_vs_Standard <- .msa_round_numeric_columns(r$ordinal$appraiser_vs_standard, 4)
          }
          if (!is.null(r$ordinal$all_appraisers_vs_standard) && nrow(r$ordinal$all_appraisers_vs_standard) > 0) {
            sheets$Kendall_All_vs_Standard <- .msa_round_numeric_columns(r$ordinal$all_appraisers_vs_standard, 4)
          }
        }
      }

      if (!is.null(self$detection_results$raw_output)) {
        r <- self$detection_results$raw_output
        sheets$Detection_Results <- data.frame(
          Metric = c(
            "Sensitivity",
            "False Negative Rate",
            "Specificity",
            "False Positive Rate",
            "Youden Index",
            "LR+",
            "LR-",
            "Prevalence"
          ),
          Estimate = c(
            sprintf("%.2f%%", r$detection_rate * 100),
            sprintf("%.2f%%", r$false_negative_rate * 100),
            sprintf("%.2f%%", r$specificity * 100),
            sprintf("%.2f%%", r$false_positive_rate * 100),
            sprintf("%.4f", r$youden_index),
            sprintf("%.4f", r$LR_positive),
            sprintf("%.4f", r$LR_negative),
            sprintf("%.2f%%", r$prevalence * 100)
          ),
          CI = c(
            sprintf("(%.2f%%, %.2f%%)", r$detection_ci$lower * 100, r$detection_ci$upper * 100),
            "-",
            sprintf("(%.2f%%, %.2f%%)", r$specificity_ci$lower * 100, r$specificity_ci$upper * 100),
            "-",
            "-",
            "-",
            "-",
            "-"
          )
        )
        if (!is.null(r$confusion_matrix_table)) sheets$Detection_Confusion <- r$confusion_matrix_table
        if (!is.null(r$risk_table)) sheets$Detection_Risk <- .msa_round_numeric_columns(r$risk_table, 4)
      }

      sheets$Raw_Data <- as.data.frame(self$data)
      sheets
    },

    build_report_plots = function() {
      if (is.null(self$kappa_results) && is.null(self$detection_results)) {
        return(list())
      }
      self$executor$plotter$render(
        self$build_results(),
        theme_obj = self$theme_obj,
        type = "list"
      )
    },

    report = function(format = c("excel", "html", "pdf", "docx", "word"), path = NULL, ...) {
      format <- .msa_format_report(format, allowed = c("excel", "html", "pdf", "docx", "word"))
      if (is.null(self$kappa_results) && is.null(self$detection_results)) {
        stop("No results available. Please call compute() first.", call. = FALSE)
      }

      results <- self$build_results()
      path <- path %||% .msa_default_report_path("attr_gage", format, prefix = "attr_gage_report")

      .msa_export_report(
        results = results,
        plan = self$plan,
        task_tag = "attr_gage",
        format = format,
        path = path,
        theme_obj = self$theme_obj,
        plan_name = .msa_plan_name(self$plan, "Attr_Gage_Study"),
        study_type = self$mode,
        kappa_results = self$kappa_results,
        detection_results = self$detection_results,
        plots = if (format == "excel") NULL else self$build_report_plots(),
        report_date = Sys.Date(),
        ...
      )

      invisible(self)
    }
  )
)
