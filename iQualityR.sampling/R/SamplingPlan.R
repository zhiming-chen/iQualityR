# =============================================================================
# File: R/SamplingPlan.R
# Description: Sampling plan configurator (inherits IqrPlanBase)
# =============================================================================

#' @title SamplingPlan: Acceptance Sampling Plan Configurator
#'
#' @description
#' Inherits from [IqrPlanBase] and stores all configuration parameters for an
#' acceptance sampling task. Supports single, double, and multiple sampling
#' plans, plus power and risk analysis.
#'
#' **Supported sampling types**:
#' - **Single**: one sample of size n, accept if d <= c
#' - **Double**: two-stage plan with intermediate decision
#' - **Multiple**: multi-stage extension of double sampling
#' - **Sequential**: reserved for future use (Wald sequential plan)
#'
#' @export
SamplingPlan <- R6::R6Class("SamplingPlan",
  inherit = IqrPlanBase,

  public = list(
    #' @field sample_size Integer. Sample size for single sampling.
    sample_size = NULL,

    #' @field acceptance_number Integer. Acceptance number c.
    acceptance_number = NULL,

    #' @field rejection_number Integer. Rejection number r (for double/multiple).
    rejection_number = NULL,

    #' @field inspection_level Character. Inspection level ("I", "II", "III").
    inspection_level = "II",

    #' @field sampling_type Character. Sampling type
    #'   (`"single"`, `"double"`, `"multiple"`, `"sequential"`).
    sampling_type = "single",

    #' @field aql Numeric. Acceptable Quality Level (0 < aql < 1).
    aql = 0.01,

    #' @field rql Numeric. Rejectable Quality Level (0 < rql < 1, rql > aql).
    rql = 0.10,

    #' @field alpha Numeric. Producer's risk (default 0.05).
    alpha = 0.05,

    #' @field beta Numeric. Consumer's risk (default 0.10).
    beta = 0.10,

    #' @field batch_size Integer. Lot size (optional).
    batch_size = NULL,

    #' @field stage_plans List. Per-stage plans for double/multiple sampling.
    #'   Each element is a list with `n`, `c`, and optionally `r`.
    stage_plans = NULL,

    #' @description Initialize a sampling plan.
    #'
    #' @param task_tag Character. Task identifier (e.g., `"sampling_single"`).
    #' @param sample_size Integer. Sample size (single sampling).
    #' @param acceptance_number Integer. Acceptance number c.
    #' @param sampling_type Character. Sampling type
    #'   (`"single"`, `"double"`, `"multiple"`, `"sequential"`).
    #' @param aql Numeric. AQL (default 0.01).
    #' @param rql Numeric. RQL (default 0.10).
    #' @param alpha Numeric. Producer's risk (default 0.05).
    #' @param beta Numeric. Consumer's risk (default 0.10).
    #' @param inspection_level Character. Inspection level
    #'   (`"I"`, `"II"`, `"III"`; default `"II"`).
    #' @param batch_size Integer. Lot size (optional).
    #' @param stage_plans List. Per-stage plans for double/multiple sampling.
    #' @param ... Additional arguments passed to [IqrPlanBase].
    initialize = function(task_tag = "sampling_single",
                          sample_size = NULL,
                          acceptance_number = NULL,
                          sampling_type = c("single", "double",
                                            "multiple", "sequential"),
                          aql = 0.01,
                          rql = 0.10,
                          alpha = 0.05,
                          beta = 0.10,
                          inspection_level = c("I", "II", "III"),
                          batch_size = NULL,
                          stage_plans = NULL,
                          ...) {
      super$initialize(task_tag = task_tag, ...)

      self$sample_size <- sample_size
      self$acceptance_number <- acceptance_number
      self$sampling_type <- private$.validate_sampling_type(sampling_type)
      self$aql <- private$.validate_probability(aql, "aql")
      self$rql <- private$.validate_probability(rql, "rql")
      self$alpha <- private$.validate_probability(alpha, "alpha")
      self$beta <- private$.validate_probability(beta, "beta")
      self$inspection_level <- private$.validate_inspection_level(inspection_level)
      self$batch_size <- batch_size

      if (!is.null(stage_plans)) {
        self$stage_plans <- private$.validate_stage_plans(stage_plans)
      }

      if (self$aql >= self$rql) {
        stop("[SamplingPlan] AQL (", self$aql,
             ") must be strictly less than RQL (", self$rql, ").",
             call. = FALSE)
      }
    },

    #' @description Validate data and configuration compatibility.
    #' @param data Data frame (optional). If provided, validated for format.
    validate = function(data = NULL) {
      # Re-check enum values to catch externally modified fields.
      self$sampling_type <- private$.validate_sampling_type(self$sampling_type)
      self$inspection_level <- private$.validate_inspection_level(self$inspection_level)
      super$validate()

      if (self$sampling_type == "single") {
        if (is.null(self$sample_size) || self$sample_size <= 0) {
          stop("[SamplingPlan] Single sampling requires a positive sample_size.",
               call. = FALSE)
        }
        if (is.null(self$acceptance_number) || self$acceptance_number < 0) {
          stop("[SamplingPlan] Single sampling requires a non-negative ",
               "acceptance_number.", call. = FALSE)
        }
        if (self$acceptance_number >= self$sample_size) {
          warning("[SamplingPlan] acceptance_number (c=", self$acceptance_number,
                  ") is close to or exceeds sample_size (n=", self$sample_size,
                  "); the plan may be impractical.", call. = FALSE)
        }
      }

      if (self$sampling_type %in% c("double", "multiple", "sequential")) {
        if (is.null(self$stage_plans) || length(self$stage_plans) < 2) {
          stop("[SamplingPlan] ", self$sampling_type,
               " sampling requires at least 2 stage_plans.", call. = FALSE)
        }
      }

      if (!is.null(self$batch_size) && self$batch_size <= 0) {
        stop("[SamplingPlan] batch_size must be a positive integer.",
             call. = FALSE)
      }

      if (!is.null(data)) {
        private$.validate_data(data)
      }

      invisible(self)
    },

    #' @description Return all plan parameters as a list (for Analyzer use).
    get_all_params = function() {
      list(
        task_tag = self$task_tag,
        sample_size = self$sample_size,
        acceptance_number = self$acceptance_number,
        rejection_number = self$rejection_number,
        inspection_level = self$inspection_level,
        sampling_type = self$sampling_type,
        aql = self$aql,
        rql = self$rql,
        alpha = self$alpha,
        beta = self$beta,
        batch_size = self$batch_size,
        stage_plans = self$stage_plans,
        conf_level = self$conf_level
      )
    },

    #' @description Whether this is a multi-stage plan.
    is_multistage = function() {
      self$sampling_type %in% c("double", "multiple", "sequential")
    }
  ),

  private = list(
    .validate_sampling_type = function(st) {
      valid <- c("single", "double", "multiple", "sequential")
      if (length(st) > 1) st <- st[1]
      if (!st %in% valid) {
        stop("[SamplingPlan] Unsupported sampling_type: ", st,
             ". Valid: ", paste(valid, collapse = ", "), call. = FALSE)
      }
      st
    },

    .validate_probability = function(val, name) {
      if (!is.numeric(val) || length(val) != 1 || val <= 0 || val >= 1) {
        stop("[SamplingPlan] ", name, " must be a single number in (0, 1).",
             call. = FALSE)
      }
      val
    },

    .validate_inspection_level = function(il) {
      valid <- c("I", "II", "III")
      if (length(il) > 1) il <- il[1]
      if (!il %in% valid) {
        stop("[SamplingPlan] Unsupported inspection_level: ", il,
             ". Valid: ", paste(valid, collapse = ", "), call. = FALSE)
      }
      il
    },

    .validate_stage_plans = function(plans) {
      if (!is.list(plans)) {
        stop("[SamplingPlan] stage_plans must be a list.", call. = FALSE)
      }
      for (i in seq_along(plans)) {
        stage <- plans[[i]]
        required <- c("n", "c")
        missing <- setdiff(required, names(stage))
        if (length(missing) > 0) {
          stop("[SamplingPlan] Stage ", i, " is missing required fields: ",
               paste(missing, collapse = ", "),
               ". Each stage needs: n (sample size), c (acceptance number).",
               call. = FALSE)
        }
        if (!is.numeric(stage$n) || stage$n <= 0) {
          stop("[SamplingPlan] Stage ", i, " sample size must be positive.",
               call. = FALSE)
        }
        if (!is.numeric(stage$c) || stage$c < 0) {
          stop("[SamplingPlan] Stage ", i,
               " acceptance number must be non-negative.", call. = FALSE)
        }
        if (!is.null(stage$r)) {
          if (!is.numeric(stage$r) || stage$r <= stage$c) {
            stop("[SamplingPlan] Stage ", i,
                 " rejection number r must be greater than c.", call. = FALSE)
          }
        }
      }
      plans
    },

    .validate_data = function(data) {
      if (!is.data.frame(data)) {
        stop("[SamplingPlan] data must be a data frame.", call. = FALSE)
      }
      if (nrow(data) == 0) {
        stop("[SamplingPlan] data must not be empty.", call. = FALSE)
      }
      if ("quality_status" %in% names(data)) {
        vals <- unique(data$quality_status)
        ok <- all(vals %in% c("defective", "good", "pass", "fail", 0, 1))
        if (!ok) {
          warning("[SamplingPlan] quality_status has unusual values: ",
                  paste(vals, collapse = ", "),
                  ". Recommended: 'defective'/'good' or 0/1.", call. = FALSE)
        }
      }
      invisible(NULL)
    }
  )
)
