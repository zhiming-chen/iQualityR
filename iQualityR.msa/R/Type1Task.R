# =============================================================================
# File: R/Type1Task.R
# Description: Type1 Gage Study task coordinator.
# =============================================================================

#' @title Type1 Gage Study Task
#' @description
#' Task coordinator for Type1 Gage Study and Gage Linearity & Bias Study.
#' The task owns workflow orchestration only; report rendering is delegated to
#' iQualityR.core::IqrReporter.
#'
#' @field plan Optional Type1Plan object with study parameters.
#' @field study_type Character scalar: `"bias"` or `"linearity"`.
#' @field results List holding statistics, diagnostics, data_tables, and raw_output after `compute()`.
#'
#' @param data Data frame or numeric vector of measurements.
#' @param plan Optional [Type1Plan] object.
#' @param theme Theme name or object accepted by the base task.
#' @param study_type Character scalar: `"bias"` or `"linearity"`.
#' @param reference_value Numeric scalar reference value for bias study.
#' @param reference_values Numeric vector of reference values for linearity study.
#' @param spec_limits Named list with elements `lsl` and `usl`.
#' @param tolerance Numeric, tolerance directly (T = USL - LSL). If given,
#'   overrides `spec_limits` for tolerance-based calculations.
#' @param natural_zero Logical, if TRUE and `usl` is given in `spec_limits`,
#'   tolerance = USL - 0 (for one-sided specs with a natural zero).
#' @param process_variation Numeric, process variation (PV) for linearity
#'   study. Typically 6*sigma from a capability study. Can also be the
#'   string `"from_study"` to use 6*sd(measurements).
#' @param k_factor Multiplier for Cg/Cgk (default 0.2).
#' @param study_multiplier Study multiplier for SV (default 6; use 5.15 for
#'   AIAG MSA 3rd edition convention).
#' @param alternative Alternative hypothesis for the bias t-test: `"two.sided"`
#'   (default), `"greater"`, or `"less"`.
#' @param historical_sd Historical sigma for known-sigma mode. When supplied,
#'   capability indices use this sigma and the bias test switches from t-test
#'   to z-test.
#' @param linearity_corrected Logical, whether to apply linearity correction
#'   (default FALSE).
#' @param resolution Measurement resolution for VDA5.
#' @param u_cal Calibration uncertainty for VDA5.
#' @param u_lin Linearity uncertainty for VDA5 (default 0 for Type1; a
#'   separate Linearity & Bias Study can supply a non-zero value).
#' @param u_rest List of other uncertainties for VDA5.
#' @param conf_level Confidence level (default 0.95).
#' @param type Plot type: `"summary"`, `"list"`, `"run_chart"`, `"capability"`, `"vda5"`, `"linearity"`, `"ref_biases"`, or `"repeatability"`.
#' @param format Report format: `"excel"`, `"html"`, `"pdf"`, `"docx"`, or `"word"`.
#' @param path Output file path. If `NULL`, a default path is generated.
#' @param ... Additional arguments passed to downstream methods.
#'
#' @export
Type1Task <- R6::R6Class(
  "Type1Task",
  inherit = IqrTaskBase,
  public = list(
    plan = NULL,
    study_type = NULL,
    results = NULL,

    initialize = function(data, plan = NULL, theme = "academic",
                          study_type = c("bias", "linearity"), ...) {
      super$initialize(data = data, theme = theme, ...)
      self$study_type <- match.arg(study_type)
      self$plan <- plan
      self$executor$type1_analyzer <- Type1Analyzer$new()
      self$executor$linearity_analyzer <- Type1LinearityAnalyzer$new()
      self$executor$plotter <- Type1Plotter$new()
      invisible(self)
    },

    compute = function(reference_value = NULL, reference_values = NULL,
                       spec_limits = list(lsl = NULL, usl = NULL),
                       tolerance = NULL, natural_zero = FALSE,
                       process_variation = NULL,
                       k_factor = 0.2, study_multiplier = 6,
                       alternative = "two.sided", historical_sd = NULL,
                       resolution = NULL,
                       u_cal = 0, u_lin = 0, u_rest = list(),
                       linearity_corrected = FALSE,
                       conf_level = 0.95, ...) {
      params <- if (!is.null(self$plan)) {
        plan_params <- self$plan$to_list()
        plan_params$reference_value <- reference_value %||% plan_params$reference_value
        plan_params$reference_values <- reference_values %||% plan_params$reference_values
        if (!is.null(spec_limits$lsl) && !is.null(spec_limits$usl)) {
          plan_params$spec_limits <- spec_limits
        }
        # Allow compute() to override plan-level tolerance / process_variation
        if (!is.null(tolerance)) plan_params$tolerance <- tolerance
        if (!is.null(process_variation)) plan_params$process_variation <- process_variation
        plan_params$study_multiplier <- study_multiplier %||% plan_params$study_multiplier
        plan_params$alternative <- alternative %||% plan_params$alternative
        plan_params$historical_sd <- historical_sd %||% plan_params$historical_sd
        plan_params$resolution <- resolution %||% plan_params$resolution
        plan_params$u_cal <- u_cal
        plan_params$u_lin <- u_lin
        plan_params$u_rest <- u_rest
        plan_params$linearity_corrected <- linearity_corrected
        plan_params
      } else {
        # Resolve tolerance from multiple input modes (mirrors Type1Plan)
        tol_resolved <- tolerance
        if (is.null(tol_resolved) && !is.null(spec_limits$usl) &&
            !is.null(spec_limits$lsl)) {
          tol_resolved <- spec_limits$usl - spec_limits$lsl
        } else if (is.null(tol_resolved) && !is.null(spec_limits$usl) &&
                   natural_zero) {
          tol_resolved <- spec_limits$usl
        }
        list(
          task_tag = "type1",
          conf_level = conf_level,
          reference_value = reference_value,
          reference_values = reference_values,
          spec_limits = spec_limits,
          tolerance = tol_resolved,
          natural_zero = natural_zero,
          process_variation = process_variation,
          k_factor = k_factor,
          study_multiplier = study_multiplier,
          alternative = alternative,
          historical_sd = historical_sd,
          resolution = resolution,
          u_cal = u_cal,
          u_lin = u_lin,
          u_rest = u_rest,
          linearity_corrected = linearity_corrected,
          criteria = list(
            Cg_min = 1.33,
            Cgk_min = 1.33,
            percent_bias_max = 10,
            percent_repeatability_max = 10,
            linearity_slope_tolerance = 0.1,
            linearity_r2_min = 0.95,
            vda5_qms_max = 15
          )
        )
      }

      params$study_type <- self$study_type
      if (self$study_type == "bias" && is.null(params$reference_value)) {
        stop("[Type1Task] reference_value required for bias study.", call. = FALSE)
      }
      if (self$study_type == "linearity" && is.null(params$reference_values)) {
        stop("[Type1Task] reference_values required for linearity study.", call. = FALSE)
      }

      # Validate tolerance (degradation strategy E3: informative error message
      # guiding the user to the supported input modes, instead of letting NaN
      # propagate silently into capability calculations).
      tol <- params$tolerance
      if (is.null(tol) || !is.finite(tol) || tol <= 0) {
        stop("[Type1Task] Tolerance must be provided. Supported modes:\n",
             "  (1) tolerance = <numeric>             (direct, covers all one-sided cases)\n",
             "  (2) spec_limits = list(lsl=, usl=)    (T = usl - lsl)\n",
             "  (3) spec_limits = list(usl=) + natural_zero=TRUE  (T = usl - 0)\n",
             "For one-sided lower specs (LSL only), use mode (1) with the\n",
             "tolerance band width directly. Tolerance must be positive.",
             call. = FALSE)
      }

      dt <- data.table::as.data.table(self$data)
      if (self$study_type == "bias") {
        analyzer <- self$executor$type1_analyzer
        analyzer$setup(params)
        analyzer$run(dt)
        self$results <- analyzer$results
        self$results$raw_output$measurements <- dt[[1]]
      } else {
        analyzer <- self$executor$linearity_analyzer
        analyzer$setup(params)
        analyzer$run(dt)
        self$results <- analyzer$results
      }
      self$results$raw_output$study_type <- self$study_type
      invisible(self)
    },

    summary = function() {
      if (is.null(self$results)) {
        cat("No results available. Please call compute() first.\n")
        return(invisible(self))
      }

      if (self$study_type == "bias") {
        s <- self$results$statistics
        d <- self$results$diagnostics
        cat("\n========== Type 1 Gage Study ==========\n")
        .msa_print_table(data.frame(
          Statistic = c("N", "Reference", "Mean", "StDev", "6 * StDev (SV)", "Tolerance", "Bias", "T", "P-value"),
          Value = c(
            s$n,
            .msa_fmt_num(s$reference_value, 6),
            .msa_fmt_num(s$mean_meas, 6),
            .msa_fmt_num(s$sd_meas, 6),
            .msa_fmt_num(6 * s$sd_meas, 6),
            .msa_fmt_num(s$tolerance, 6),
            .msa_fmt_num(s$bias, 6),
            .msa_fmt_num(s$t_stat, 4),
            .msa_fmt_p(s$p_value, 4)
          )
        ))
        cat("\n--- Gage Capability ---\n")
        .msa_print_table(data.frame(
          Metric = c("Cg", "Cgk", "%Var (Repeatability)", "%Var (Repeatability and Bias)", "%Bias of Tolerance"),
          Value = c(
            .msa_fmt_num(s$Cg, 3),
            .msa_fmt_num(s$Cgk, 3),
            .msa_fmt_pct(s$percent_repeatability, 2),
            .msa_fmt_pct(s$percent_repeatability_bias, 2),
            .msa_fmt_pct(s$percent_bias, 2)
          ),
          Decision = c(
            ifelse(d$Cg_ok, "PASS", "FAIL"),
            ifelse(d$Cgk_ok, "PASS", "FAIL"),
            "",
            "",
            ""
          )
        ))
        cat("\n--- VDA 5 Uncertainty ---\n")
        .msa_print_table(data.frame(
          Component = c("uEVR", "uRE", "uBI", "uLIN", "uCAL", "uREST",
                        "uMS", "Expanded U (k=2)", "%QMS", "Capability"),
          Value = c(
            .msa_fmt_num(s$vda5_u_evr, 6),
            .msa_fmt_num(s$vda5_u_re, 6),
            .msa_fmt_num(s$vda5_u_bi, 6),
            .msa_fmt_num(s$vda5_u_lin, 6),
            .msa_fmt_num(s$vda5_u_cal, 6),
            .msa_fmt_num(s$vda5_u_rest, 6),
            .msa_fmt_num(s$vda5_u_ms, 6),
            .msa_fmt_num(s$vda5_expanded_u, 6),
            .msa_fmt_pct(s$vda5_qms_percent, 2),
            s$vda5_qms_capability
          ),
          Decision = c("", "", "", "", "", "", "", "", "",
                       ifelse(d$vda5_qms_ok, "PASS", "FAIL"))
        ))
        # Stability diagnostics (MSA 4th ed. requires in-control process)
        if (!is.null(s$stability_ok)) {
          cat("\n--- Run-Chart Stability (MSA 4th ed.) ---\n")
          stab_decision <- if (is.na(s$stability_ok)) {
            "N/A"
          } else if (s$stability_ok) {
            "PASS"
          } else {
            "FAIL"
          }
          .msa_print_table(data.frame(
            Rule = c("Beyond +-3 sigma", "9-pt same side (shift)",
                     "6-pt trend", "2 of 3 beyond +-2 sigma", "Overall"),
            Triggered = c(
              ifelse(isTRUE(s$stability_beyond_3s), "YES", "no"),
              ifelse(isTRUE(s$stability_shift_9), "YES", "no"),
              ifelse(isTRUE(s$stability_trend_6), "YES", "no"),
              ifelse(isTRUE(s$stability_2of3_beyond_2s), "YES", "no"),
              ifelse(is.na(s$stability_ok), "N/A",
                     ifelse(s$stability_ok, "no", "YES"))
            ),
            Decision = c("", "", "", "", stab_decision)
          ))
        }
      } else {
        s <- self$results$statistics
        d <- self$results$diagnostics
        cat("\n========== Gage Linearity and Bias Study ==========\n")
        .msa_print_table(data.frame(
          Statistic = c("Total measurements", "Reference points",
                        "Tolerance", "Process Variation",
                        "Average bias", "Max absolute bias",
                        "Linearity", "Average StDev"),
          Value = c(
            s$n_total,
            s$n_ref_points,
            .msa_fmt_num(s$tolerance, 6),
            .msa_fmt_num(s$process_variation, 6),
            .msa_fmt_num(s$avg_bias, 6),
            .msa_fmt_num(s$max_bias, 6),
            .msa_fmt_num(s$linearity, 6),
            .msa_fmt_num(s$avg_sd, 6)
          ),
          Percent_of_PV = c(
            "", "", "", "",
            .msa_fmt_pct(s$percent_avg_bias, 2),
            .msa_fmt_pct(s$percent_max_bias, 2),
            .msa_fmt_pct(s$percent_linearity, 2),
            ""
          )
        ))
        cat("\n--- Per-Reference Summary (bias test & stability) ---\n")
        ref_sum <- self$results$data_tables$ref_summary
        .msa_print_table(data.frame(
          Reference = ref_sum$reference,
          N = ref_sum$n,
          Mean = sprintf("%.6f", ref_sum$mean),
          StDev = sprintf("%.6f", ref_sum$sd),
          Bias = sprintf("%.6f", ref_sum$bias),
          T = sprintf("%.4f", ref_sum$t_stat),
          df = ref_sum$df,
          p_value = sprintf("%.4f", ref_sum$p_value),
          Bias_95pct_CI = sprintf("[%.4f, %.4f]",
                                  ref_sum$ci_lower, ref_sum$ci_upper),
          Stable = ifelse(ref_sum$stability_ok, "yes", "NO")
        ))
        cat("\n--- Regression of Bias on Reference ---\n")
        .msa_print_table(data.frame(
          Term = c("Intercept", "Slope", "Slope 95% CI", "R-squared",
                   "Slope p-value"),
          Value = c(
            .msa_fmt_num(s$intercept, 6),
            .msa_fmt_num(s$slope, 6),
            paste0("[", .msa_fmt_num(s$ci_slope[1], 6), ", ", .msa_fmt_num(s$ci_slope[2], 6), "]"),
            .msa_fmt_num(s$r_squared, 4),
            formatC(s$p_slope, format = "f", digits = 6)
          ),
          Decision = c("", ifelse(d$slope_ok, "PASS", "FAIL"), "",
                       "(diagnostic only)",
                       ifelse(d$linearity_sig_ok, "PASS", "FAIL"))
        ))
        cat("\n--- Linearity & Bias Acceptance ---\n")
        .msa_print_table(data.frame(
          Criterion = c("%Linearity of PV", "%Avg bias of PV",
                        "%Max bias of PV", "Per-ref bias CI contains 0",
                        "Linearity not significant", "Per-ref stability"),
          Observed = c(
            .msa_fmt_pct(s$percent_linearity, 2),
            .msa_fmt_pct(s$percent_avg_bias, 2),
            .msa_fmt_pct(s$percent_max_bias, 2),
            ifelse(d$per_ref_bias_ok, "all PASS", "some FAIL"),
            ifelse(d$linearity_sig_ok, "yes", "no"),
            ifelse(d$per_ref_stability_ok, "all PASS", "some FAIL")
          ),
          Decision = c(
            ifelse(d$percent_linearity_ok, "PASS", "FAIL"),
            ifelse(d$percent_avg_bias_ok, "PASS", "FAIL"),
            ifelse(d$percent_max_bias_ok, "PASS", "FAIL"),
            ifelse(d$per_ref_bias_ok, "PASS", "FAIL"),
            ifelse(d$linearity_sig_ok, "PASS", "FAIL"),
            ifelse(d$per_ref_stability_ok, "PASS", "FAIL")
          )
        ))
        cat("\n--- VDA 5 Uncertainty (Linearity Study) ---\n")
        .msa_print_table(data.frame(
          Component = c("uEVR (pooled)", "uRE", "uBI (avg)", "uLIN",
                        "uCAL", "uREST", "uMS", "Expanded U (k=2)",
                        "%QMS", "Capability"),
          Value = c(
            .msa_fmt_num(s$vda5_u_evr, 6),
            .msa_fmt_num(s$vda5_u_re, 6),
            .msa_fmt_num(s$vda5_u_bi, 6),
            .msa_fmt_num(s$vda5_u_lin, 6),
            .msa_fmt_num(s$vda5_u_cal, 6),
            .msa_fmt_num(s$vda5_u_rest, 6),
            .msa_fmt_num(s$vda5_u_ms, 6),
            .msa_fmt_num(s$vda5_expanded_u, 6),
            .msa_fmt_pct(s$vda5_qms_percent, 2),
            s$vda5_qms_capability
          ),
          Decision = c("", "", "", "", "", "", "", "", "",
                       ifelse(d$vda5_qms_ok, "PASS", "FAIL"))
        ))
      }
      invisible(self)
    },

    plot = function(type = "summary", theme = NULL, ...) {
      if (is.null(self$results)) {
        stop("No results available. Please call compute() first.", call. = FALSE)
      }

      theme <- theme %||% self$theme_obj
      self$executor$plotter$render(self$results, theme, type = type, ...)
    },

    build_report_results = function() {
      list(
        study_type = self$study_type,
        statistics = self$results$statistics,
        diagnostics = self$results$diagnostics,
        data_tables = self$results$data_tables,
        raw_output = self$results$raw_output,
        to_excel = function(plan = self$plan) self$build_excel_sheets()
      )
    },

    build_excel_sheets = function() {
      if (self$study_type == "bias") {
        stats <- data.frame(
          Metric = c("n", "Reference Value", "Mean Measurement", "Std Deviation",
                    "Bias", "Percent Bias", "t-stat", "p-value", "DF",
                    "Tolerance", "k_factor", "Cg", "Cgk",
                    "Percent Repeatability", "Percent Repeatability+Bias",
                    "VDA5 u_evr", "VDA5 u_re", "VDA5 u_bi", "VDA5 u_lin",
                    "VDA5 u_cal", "VDA5 u_rest", "VDA5 u_ms",
                    "VDA5 Expanded U (k=2)", "VDA5 %QMS", "VDA5 Capability"),
          Value = c(self$results$statistics$n, self$results$statistics$reference_value,
                   self$results$statistics$mean_meas, self$results$statistics$sd_meas,
                   self$results$statistics$bias, self$results$statistics$percent_bias,
                   self$results$statistics$t_stat, self$results$statistics$p_value,
                   self$results$statistics$df,
                   self$results$statistics$tolerance, self$results$statistics$k_factor,
                   self$results$statistics$Cg, self$results$statistics$Cgk,
                   self$results$statistics$percent_repeatability,
                   self$results$statistics$percent_repeatability_bias,
                   self$results$statistics$vda5_u_evr,
                   self$results$statistics$vda5_u_re, self$results$statistics$vda5_u_bi,
                   self$results$statistics$vda5_u_lin,
                   self$results$statistics$vda5_u_cal, self$results$statistics$vda5_u_rest,
                   self$results$statistics$vda5_u_ms,
                   self$results$statistics$vda5_expanded_u,
                   self$results$statistics$vda5_qms_percent,
                   self$results$statistics$vda5_qms_capability)
        )
        return(list(Statistics = stats, VDA5_Uncertainty = self$results$data_tables$vda5_uncertainty))
      }

      stats <- data.frame(
        Metric = c("n_total", "n_ref_points", "Tolerance", "Process Variation",
                  "Average Bias", "Percent Average Bias", "Max Bias",
                  "Percent Max Bias", "Linearity", "Percent Linearity",
                  "Linearity CI Lower", "Linearity CI Upper",
                  "Intercept", "Slope", "Slope SE", "Slope t-stat", "Slope p-value",
                  "R-squared", "Adjusted R-squared", "S (regression)",
                  "Average Std Dev",
                  "VDA5 u_evr (pooled)", "VDA5 u_re", "VDA5 u_bi (avg)",
                  "VDA5 u_lin", "VDA5 u_cal", "VDA5 u_rest", "VDA5 u_ms",
                  "VDA5 Expanded U (k=2)", "VDA5 %QMS", "VDA5 Capability",
                  "VDA5 s_pooled"),
        Value = c(self$results$statistics$n_total, self$results$statistics$n_ref_points,
                 self$results$statistics$tolerance,
                 self$results$statistics$process_variation,
                 self$results$statistics$avg_bias,
                 self$results$statistics$percent_avg_bias,
                 self$results$statistics$max_bias,
                 self$results$statistics$percent_max_bias,
                 self$results$statistics$linearity,
                 self$results$statistics$percent_linearity,
                 self$results$statistics$ci_linearity[1],
                 self$results$statistics$ci_linearity[2],
                 self$results$statistics$intercept,
                 self$results$statistics$slope,
                 self$results$statistics$se_slope,
                 self$results$statistics$t_slope,
                 self$results$statistics$p_slope,
                 self$results$statistics$r_squared,
                 self$results$statistics$adj_r_squared,
                 self$results$statistics$s_regression,
                 self$results$statistics$avg_sd,
                 self$results$statistics$vda5_u_evr,
                 self$results$statistics$vda5_u_re,
                 self$results$statistics$vda5_u_bi,
                 self$results$statistics$vda5_u_lin,
                 self$results$statistics$vda5_u_cal,
                 self$results$statistics$vda5_u_rest,
                 self$results$statistics$vda5_u_ms,
                 self$results$statistics$vda5_expanded_u,
                 self$results$statistics$vda5_qms_percent,
                 self$results$statistics$vda5_qms_capability,
                 self$results$statistics$vda5_s_pooled)
      )
      list(Statistics = stats,
           Reference_Summary = self$results$data_tables$ref_summary,
           VDA5_Uncertainty = self$results$data_tables$vda5_uncertainty)
    },

    report = function(format = "excel", path = NULL, ...) {
      if (is.null(self$results)) {
        stop("No results available. Please call compute() first.", call. = FALSE)
      }

      format <- .msa_format_report(format, allowed = c("excel", "html", "pdf", "docx", "word"))
      path <- path %||% .msa_default_report_path("type1", format, prefix = paste0("type1_", self$study_type, "_report"))
      .msa_export_report(
        results = self$build_report_results(),
        plan = self$plan,
        task_tag = "type1",
        format = format,
        path = path,
        theme_obj = self$theme_obj,
        plots = if (format == "excel") NULL else self$executor$plotter$render(self$results, self$theme_obj, type = "list"),
        ...
      )
      invisible(self)
    }
  )
)
