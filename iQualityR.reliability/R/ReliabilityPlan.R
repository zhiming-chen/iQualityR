# =============================================================================
# File: R/ReliabilityPlan.R
# Description: Reliability and survival analysis plan configurator
# =============================================================================

#' @title ReliabilityPlan: Reliability and Survival Analysis Plan Configurator
#'
#' @description
#' Inherits from [IqrPlanBase] and stores all configuration parameters for a
#' reliability or survival analysis task.
#'
#' **Supported analysis types**:
#' - **Parametric reliability**: Weibull, exponential, lognormal, logistic
#' - **Nonparametric survival**: Kaplan-Meier estimation
#' - **Semiparametric survival**: Cox proportional hazards model
#'
#' @export
ReliabilityPlan <- R6::R6Class("ReliabilityPlan",
  inherit = IqrPlanBase,

  public = list(
    #' @field time_var Character. Name of the time/lifetime variable.
    time_var = NULL,

    #' @field status_var Character. Name of the status variable.
    status_var = NULL,

    #' @field censoring_type Character. Censoring type.
    censoring_type = "right",

    #' @field distribution Character. Fitted distribution.
    distribution = "weibull",

    #' @field method Character. Analysis method.
    method = "parametric",

    #' @field factors Character vector. Covariates for the Cox model.
    factors = NULL,

    #' @field stress_vars Character vector. Stress variables for ALT.
    stress_vars = NULL,

    #' @field acceleration_model Character. Acceleration model.
    acceleration_model = "arrhenius",

    #' @description Initialize the reliability analysis plan.
    #' @param time_var Character. Name of the time/lifetime variable.
    #' @param status_var Character. Name of the status variable
    #'   (optional; defaults to no censoring).
    #' @param censoring_type Character. Censoring type.
    #' @param distribution Character. Fitted distribution.
    #' @param method Character. Analysis method.
    #' @param factors Character vector. Covariates for the Cox model.
    #' @param stress_vars Character vector. Stress variables for ALT.
    #' @param acceleration_model Character. Acceleration model.
    #' @param conf_level Numeric. Confidence level, between 0 and 1.
    #' @param ... Additional arguments passed to [IqrPlanBase].
    initialize = function(time_var,
                          status_var = NULL,
                          censoring_type = "right",
                          distribution = "weibull",
                          method = "parametric",
                          factors = NULL,
                          stress_vars = NULL,
                          acceleration_model = "arrhenius",
                          conf_level = 0.95,
                          ...) {
      task_tag <- switch(method,
        "kaplan_meier" = "survival_km",
        "cox"          = "survival_cox",
        "parametric"   = "reliability",
        "reliability"
      )
      super$initialize(task_tag = task_tag, conf_level = conf_level, ...)

      self$time_var           <- time_var
      self$status_var         <- status_var
      self$censoring_type     <- censoring_type
      self$distribution       <- distribution
      self$method             <- method
      self$factors            <- factors
      self$stress_vars        <- stress_vars
      self$acceleration_model <- acceleration_model

      # Validate enum values immediately
      self$validate_type()
    },

    #' @description Validate enum values (called by `initialize()`).
    validate_type = function() {
      valid_distributions <- c("weibull", "exponential", "lognormal", "logistic")
      if (!self$distribution %in% valid_distributions) {
        stop("[ReliabilityPlan] Invalid distribution: ", self$distribution,
             ". Valid values: ", paste(valid_distributions, collapse = ", "),
             call. = FALSE)
      }

      valid_censoring <- c("right", "left", "interval", "none")
      if (!self$censoring_type %in% valid_censoring) {
        stop("[ReliabilityPlan] Invalid censoring type: ", self$censoring_type,
             ". Valid values: ", paste(valid_censoring, collapse = ", "),
             call. = FALSE)
      }

      valid_methods <- c("parametric", "kaplan_meier", "cox")
      if (!self$method %in% valid_methods) {
        stop("[ReliabilityPlan] Invalid analysis method: ", self$method,
             ". Valid values: ", paste(valid_methods, collapse = ", "),
             call. = FALSE)
      }
    },

    #' @description Validate data and configuration compatibility.
    #' @param data Data frame.
    validate = function(data) {
      # Re-check enum values so externally modified fields are caught.
      self$validate_type()
      super$validate()

      # Check time variable
      if (!self$time_var %in% names(data)) {
        stop("[ReliabilityPlan] Time variable '", self$time_var,
             "' not found in data.", call. = FALSE)
      }
      if (!is.numeric(data[[self$time_var]])) {
        stop("[ReliabilityPlan] Time variable '", self$time_var,
             "' must be numeric.", call. = FALSE)
      }
      if (any(data[[self$time_var]] <= 0, na.rm = TRUE)) {
        stop("[ReliabilityPlan] Time variable '", self$time_var,
             "' must be strictly positive.", call. = FALSE)
      }

      # Check status variable
      if (!is.null(self$status_var)) {
        if (!self$status_var %in% names(data)) {
          stop("[ReliabilityPlan] Status variable '", self$status_var,
               "' not found in data.", call. = FALSE)
        }
        status_col <- data[[self$status_var]]
        if (!is.factor(status_col) && !is.numeric(status_col)) {
          warning("[ReliabilityPlan] Status variable is recommended to be a ",
                  "factor or 0/1 numeric; current class: ",
                  paste(class(status_col), collapse = "/"), call. = FALSE)
        }
      }

      # Cox model requires covariates
      if (self$method == "cox" && (is.null(self$factors) || length(self$factors) == 0)) {
        stop("[ReliabilityPlan] Cox model requires 'factors' (covariates).",
             call. = FALSE)
      }

      # Check covariates exist in data
      if (!is.null(self$factors)) {
        missing <- setdiff(self$factors, names(data))
        if (length(missing) > 0) {
          stop("[ReliabilityPlan] The following covariates are not in data: ",
               paste(missing, collapse = ", "), call. = FALSE)
        }
      }

      # Check sample size
      n_obs <- nrow(data)
      if (n_obs < 5) {
        stop("[ReliabilityPlan] Insufficient data (n = ", n_obs,
             "); need at least 5 observations.", call. = FALSE)
      }

      invisible(self)
    },

    #' @description Whether the data is censored.
    is_censored = function() {
      self$censoring_type != "none" && !is.null(self$status_var)
    },

    #' @description Whether this is an accelerated life test plan.
    is_alt = function() {
      !is.null(self$stress_vars) && length(self$stress_vars) > 0
    }
  )
)
