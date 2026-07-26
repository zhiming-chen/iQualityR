# =============================================================================
# File: R/CapabilityPlan.R
# Description: Capability analysis plan configuration (inherits IqrPlanBase)
# =============================================================================

#' @title CapabilityPlan
#' @description
#' Plan configuration for process capability analysis. Inherits `IqrPlanBase`
#' and adds capability-specific parameters such as specification limits,
#' target, subgroup column, and analysis type.
#'
#' @field lsl Lower specification limit.
#' @field usl Upper specification limit.
#' @field target Optional target value (used for Cpm calculation).
#' @field subgroup Optional subgroup column name for within-group sigma estimation.
#' @field sixpack Logical; whether to generate Sixpack diagnostic plots.
#' @field use_bootstrap Logical; whether to compute bootstrap confidence intervals.
#' @field bootstrap_samples Number of bootstrap replications.
#' @field analysis_type Analysis type: `"normal"`, `"nonnormal"`, or `"nonparametric"`.
#' @field distribution Distribution name for non-normal analysis, or `"auto"`.
#' @field nonparametric_method Non-parametric method: `"kernel"` or `"empirical"`.
#'
#' @param lsl Lower specification limit.
#' @param usl Upper specification limit.
#' @param target Optional target value.
#' @param subgroup Optional subgroup column name.
#' @param conf_level Confidence level (default 0.95).
#' @param sixpack Logical; whether to generate Sixpack.
#' @param use_bootstrap Logical; whether to use Bootstrap.
#' @param bootstrap_samples Number of Bootstrap replications.
#' @param analysis_type Analysis type string.
#' @param distribution Distribution name or `"auto"`.
#' @param nonparametric_method Non-parametric method string.
#' @param task_tag Task tag (default `"capability"`).
#' @param ... Additional arguments passed to `IqrPlanBase$initialize()`.
#'
#' @export
CapabilityPlan <- R6::R6Class("CapabilityPlan",
  inherit = IqrPlanBase,
  public = list(
    lsl = NULL,
    usl = NULL,
    target = NULL,
    subgroup = NULL,
    sixpack = FALSE,
    use_bootstrap = FALSE,
    bootstrap_samples = 1000,
    analysis_type = "normal",
    distribution = NULL,
    nonparametric_method = "kernel",

    #' @description Create a new CapabilityPlan object
    #' @param lsl Lower specification limit.
    #' @param usl Upper specification limit.
    #' @param target Optional target value.
    #' @param subgroup Optional subgroup column name.
    #' @param conf_level Confidence level.
    #' @param sixpack Logical; whether to generate Sixpack.
    #' @param use_bootstrap Logical; whether to use Bootstrap.
    #' @param bootstrap_samples Number of Bootstrap replications.
    #' @param analysis_type Analysis type string.
    #' @param distribution Distribution name or "auto".
    #' @param nonparametric_method Non-parametric method string.
    #' @param task_tag Task tag.
    #' @param ... Additional arguments.
    initialize = function(lsl, usl, target = NULL, subgroup = NULL,
                          conf_level = 0.95, sixpack = FALSE,
                          use_bootstrap = FALSE, bootstrap_samples = 1000,
                          analysis_type = "normal", distribution = NULL,
                          nonparametric_method = "kernel",
                          task_tag = "capability", ...) {
      super$initialize(task_tag = task_tag, conf_level = conf_level, ...)
      if (lsl >= usl) stop("lsl must be less than usl", call. = FALSE)
      self$lsl <- lsl
      self$usl <- usl
      self$target <- target
      self$subgroup <- subgroup
      self$sixpack <- sixpack
      self$use_bootstrap <- use_bootstrap
      self$bootstrap_samples <- bootstrap_samples
      self$analysis_type <- analysis_type
      self$distribution <- distribution
      self$nonparametric_method <- nonparametric_method
      # Set default judgment criteria (can be overridden)
      self$set_criteria(cpk = 1.33, ppk = 1.33)
      invisible(self)
    },

    #' @description Get specification tolerance
    tolerance = function() self$usl - self$lsl,

    #' @description Export configuration as a list (overrides base method to include extra fields)
    to_list = function() {
      base_list <- super$to_list()
      base_list$lsl <- self$lsl
      base_list$usl <- self$usl
      base_list$target <- self$target
      base_list$subgroup <- self$subgroup
      base_list$sixpack <- self$sixpack
      base_list$use_bootstrap <- self$use_bootstrap
      base_list$bootstrap_samples <- self$bootstrap_samples
      base_list$analysis_type <- self$analysis_type
      base_list$distribution <- self$distribution
      base_list$nonparametric_method <- self$nonparametric_method
      base_list
    }
  )
)
