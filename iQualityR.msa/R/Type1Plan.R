# =============================================================================
# File: R/Type1Plan.R
# Description: iQualityR MSA Type1 Gage Study Plan Configuration Class
# =============================================================================

#' @title Type1 Gage Study Plan
#' @description
#' Plan configuration for Type1 Gage Study (single reference value, bias and repeatability)
#' and Linearity & Bias Study (multiple reference values, linearity evaluation)
#' Follows iQualityR framework specification v2.0.
#'
#' @export
Type1Plan <- R6::R6Class("Type1Plan",
  inherit = IqrPlanBase,

  public = list(
    #' @description Initialize Type1 Gage Plan
    #' @param study_type Type of study: "bias" (single ref) or "linearity" (multi-ref)
    #' @param reference_value Numeric, single reference value for bias study
    #' @param reference_values Numeric vector, multiple reference values for linearity study
    #' @param spec_limits List with lsl and usl for tolerance calculation. One
    #'   of `spec_limits`, `tolerance`, or `usl`+`natural_zero` must be given.
    #' @param tolerance Numeric, tolerance directly (T = USL - LSL). If given,
    #'   overrides `spec_limits` for tolerance-based calculations.
    #' @param natural_zero Logical, if TRUE and `usl` is given in `spec_limits`,
    #'   tolerance = USL - 0 (for one-sided specs with a natural zero).
    #' @param process_variation Numeric, process variation (PV) for linearity
    #'   study. Typically 6*sigma from a capability study or historical data.
    #'   Can also be the string `"from_study"` to use 6*sd(measurements).
    #'   Required by Minitab's Gage Linearity and Bias Study for
    #'   Linearity = |slope|*PV and %Linearity/%Bias calculations.
    #' @param k_factor Numeric, multiplier for Cg/Cgk calculation (default 0.2)
    #' @param study_multiplier Numeric, study variation multiplier (default 6).
    #'   Minitab historically supports 6 (modern default) and 5.15 (AIAG MSA
    #'   3rd edition convention). SV = study_multiplier * sigma. Affects Cg,
    #'   Cgk, and %Var(Repeatability).
    #' @param alternative Character, direction of the bias t-test alternative
    #'   hypothesis: `"two.sided"` (default), `"greater"`, or `"less"`.
    #'   - `"two.sided"`: H1: bias != 0
    #'   - `"greater"` : H1: bias > 0  (one-sided, measurements run high)
    #'   - `"less"`    : H1: bias < 0  (one-sided, measurements run low)
    #' @param historical_sd Numeric, historical/known sigma to use instead of
    #'   the sample standard deviation. When supplied, SD-based statistics
    #'   (StDev, SV, Cg, Cgk, u_EVR) use this value; the bias t-test still
    #'   uses the sample standard error of the mean. Minitab "Use known
    #'   standard deviation" option.
    #' @param resolution Numeric, measurement system resolution for VDA5 uncertainty
    #' @param u_cal Numeric, calibration uncertainty for VDA5 (optional)
    #' @param u_lin Numeric, linearity uncertainty for VDA5 (optional, default 0
    #'   for Type1 since linearity is assessed separately)
    #' @param u_rest List, additional uncertainty factors for VDA5 (optional)
    #' @param conf_level Numeric, confidence level (default 0.95)
    initialize = function(study_type = c("bias", "linearity"),
                          reference_value = NULL,
                          reference_values = NULL,
                          spec_limits = list(lsl = NULL, usl = NULL),
                          tolerance = NULL,
                          natural_zero = FALSE,
                          process_variation = NULL,
                          k_factor = 0.2,
                          study_multiplier = 6,
                          alternative = c("two.sided", "greater", "less"),
                          historical_sd = NULL,
                          resolution = NULL,
                          u_cal = 0,
                          u_lin = 0,
                          u_rest = list(),
                          conf_level = 0.95) {
      # Validate study type
      study_type <- match.arg(study_type)
      alternative <- match.arg(alternative)

      # Call super class
      super$initialize(task_tag = "type1", conf_level = conf_level)

      # Store study parameters
      self$stats_params$study_type <- study_type
      self$stats_params$reference_value <- reference_value
      self$stats_params$reference_values <- reference_values
      self$stats_params$spec_limits <- spec_limits
      self$stats_params$k_factor <- k_factor
      self$stats_params$study_multiplier <- study_multiplier
      self$stats_params$alternative <- alternative
      self$stats_params$historical_sd <- historical_sd
      self$stats_params$resolution <- resolution
      self$stats_params$u_cal <- u_cal
      self$stats_params$u_lin <- u_lin
      self$stats_params$u_rest <- u_rest
      self$stats_params$process_variation <- process_variation
      self$stats_params$natural_zero <- natural_zero

      # Resolve tolerance from multiple input modes:
      #   (1) explicit tolerance parameter -> use directly
      #   (2) spec_limits with lsl+usl     -> T = usl - lsl
      #   (3) spec_limits with usl + natural_zero=TRUE -> T = usl - 0
      if (!is.null(tolerance)) {
        self$stats_params$tolerance <- as.numeric(tolerance)
      } else if (!is.null(spec_limits$usl) && !is.null(spec_limits$lsl)) {
        self$stats_params$tolerance <- spec_limits$usl - spec_limits$lsl
      } else if (!is.null(spec_limits$usl) && natural_zero) {
        self$stats_params$tolerance <- spec_limits$usl
      } else {
        self$stats_params$tolerance <- NULL  # validated later
      }

      # Set default criteria
      self$set_criteria(
        Cg_min = 1.33,
        Cgk_min = 1.33,
        percent_bias_max = 10,
        percent_repeatability_max = 10,
        linearity_slope_tolerance = 0.1,
        linearity_r2_min = 0.95,
        vda5_qms_max = 15
      )

      invisible(self)
    },

    #' @description Validate plan configuration
    validate = function() {
      params <- self$stats_params

      if (params$study_type == "bias" && is.null(params$reference_value)) {
        stop("[Type1Plan] reference_value required for bias study.", call. = FALSE)
      }
      if (params$study_type == "linearity" && is.null(params$reference_values)) {
        stop("[Type1Plan] reference_values required for linearity study.", call. = FALSE)
      }

      # Tolerance is required for capability indices (Cg, Cgk) and VDA5 %QMS.
      # Accept any of: explicit tolerance, spec_limits(lsl+usl), or
      # spec_limits(usl) + natural_zero.
      #
      # Degradation strategy (E3): for one-sided lower specs (LSL only, no USL,
      # no natural zero), do NOT hard-stop. Instead, guide the user to supply
      # the tolerance directly. Only stop if tolerance truly cannot be resolved.
      if (is.null(params$tolerance) || !is.finite(params$tolerance) ||
          params$tolerance <= 0) {
        stop("[Type1Plan] Tolerance must be provided. Supported modes:\n",
             "  (1) tolerance = <numeric>             (direct, covers all one-sided cases)\n",
             "  (2) spec_limits = list(lsl=, usl=)    (T = usl - lsl)\n",
             "  (3) spec_limits = list(usl=) + natural_zero=TRUE  (T = usl - 0)\n",
             "For one-sided lower specs (LSL only), use mode (1) with the\n",
             "tolerance band width directly. Tolerance must be positive.",
             call. = FALSE)
      }
      if (!is.null(params$spec_limits$usl) && !is.null(params$spec_limits$lsl) &&
          params$spec_limits$usl <= params$spec_limits$lsl) {
        stop("[Type1Plan] usl must be greater than lsl.", call. = FALSE)
      }

      # Validate study_multiplier
      sm <- params$study_multiplier
      if (!is.numeric(sm) || length(sm) != 1 || !is.finite(sm) || sm <= 0) {
        stop("[Type1Plan] study_multiplier must be a positive numeric scalar ",
             "(e.g. 6 or 5.15).", call. = FALSE)
      }

      # Validate historical_sd
      hs <- params$historical_sd
      if (!is.null(hs)) {
        if (!is.numeric(hs) || length(hs) != 1 || !is.finite(hs) || hs <= 0) {
          stop("[Type1Plan] historical_sd must be a positive numeric scalar or NULL.",
               call. = FALSE)
        }
      }

      invisible(self)
    },

    #' @description Generate measurement protocol
    #' @param ... Additional arguments (ignored).
    generate_protocol = function(...) {
      params <- self$stats_params
      if (params$study_type == "bias") {
        cat("=== Type1 Gage Study Protocol ===\n")
        cat("Study Type: Single Reference Value Bias Study\n")
        cat(sprintf("Reference Value: %.4f\n", params$reference_value))
        cat(sprintf("Specification Limits: LSL=%.4f, USL=%.4f\n",
                    params$spec_limits$lsl, params$spec_limits$usl))
        cat(sprintf("Tolerance: %.4f\n", diff(unlist(params$spec_limits))))
        if (!is.null(params$resolution)) {
          cat(sprintf("Measurement Resolution: %.6f\n", params$resolution))
        }
      } else {
        cat("=== Gage Linearity & Bias Study Protocol ===\n")
        cat("Study Type: Multiple Reference Values Linearity Study\n")
        cat(sprintf("Reference Values: %s\n", paste(sprintf("%.4f", params$reference_values), collapse = ", ")))
        cat(sprintf("Specification Limits: LSL=%.4f, USL=%.4f\n",
                    params$spec_limits$lsl, params$spec_limits$usl))
        cat(sprintf("Tolerance: %.4f\n", diff(unlist(params$spec_limits))))
      }
      invisible(self)
    },

    #' @description Export configuration as a plain list for the Analyzer.
    #'
    #' Flatten `stats_params` to the top level so that downstream analyzers
    #' (e.g. [Type1Analyzer]) can read `reference_value`, `spec_limits`,
    #' `k_factor`, `resolution`, `u_cal`, and `u_rest` directly from
    #' `params`. This mirrors the layout produced by [AttrGagePlan$to_list()].
    to_list = function() {
      sp <- self$stats_params
      c(super$to_list(), list(
        study_type        = sp$study_type,
        reference_value   = sp$reference_value,
        reference_values  = sp$reference_values,
        spec_limits       = sp$spec_limits,
        tolerance         = sp$tolerance,
        natural_zero      = sp$natural_zero,
        process_variation = sp$process_variation,
        k_factor          = sp$k_factor,
        study_multiplier  = sp$study_multiplier,
        alternative       = sp$alternative,
        historical_sd     = sp$historical_sd,
        resolution        = sp$resolution,
        u_cal             = sp$u_cal,
        u_lin             = sp$u_lin,
        u_rest            = sp$u_rest
      ))
    }
  )
)
