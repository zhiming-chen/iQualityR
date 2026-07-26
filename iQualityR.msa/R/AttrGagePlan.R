# =============================================================================
# File: iQualityR.msa/R/AttrGagePlan.R
# Description: Attribute agreement analysis plan class
# Depends: iQualityR.core (IqrPlanBase)
# =============================================================================

#' @title AttrGagePlan: Attribute Agreement Analysis Plan
#' @description Manage parameter configuration for attribute agreement analysis.
#'
#' @field appraisers Vector of appraiser identifiers.
#' @field samples Vector of sample identifiers.
#' @field standards Vector of known reference standards for samples.
#' @field categories Vector of evaluation categories.
#' @field category_order Ordered vector of categories.
#' @field scale_type Character scalar: `"nominal"` or `"ordinal"`.
#' @field trials Integer scalar number of trials per appraiser/sample.
#' @field comparison_mode Character scalar: `"one_way"` or `"two_way"`.
#' @field kappa_method Character scalar: `"cohen"`, `"fleiss"`, or `"wagner"`.
#'
#' @param plan_name Character scalar project name.
#' @param objectives Character scalar study objectives.
#' @param appraisers Vector of appraiser identifiers.
#' @param samples Vector of sample identifiers.
#' @param standards Vector of known reference standards for samples.
#' @param categories Vector of evaluation categories.
#' @param category_order Ordered vector of categories.
#' @param scale_type Character scalar: `"nominal"` or `"ordinal"`.
#' @param trials Integer scalar number of trials per appraiser/sample.
#' @param comparison_mode Character scalar: `"one_way"` or `"two_way"`.
#' @param kappa_method Character scalar: `"cohen"`, `"fleiss"`, or `"wagner"`.
#' @param conf_level Confidence level (default 0.95).
#'
#' @export
AttrGagePlan <- R6::R6Class("AttrGagePlan",
  inherit = IqrPlanBase,
  public = list(
    appraisers = NULL,
    samples = NULL,
    standards = NULL,
    categories = NULL,
    category_order = NULL,
    scale_type = NULL,
    trials = NULL,
    comparison_mode = NULL,
    kappa_method = NULL,

    initialize = function(plan_name = "Attr_Gage_Study",
                          objectives = "Attribute agreement analysis",
                          appraisers = NULL,
                          samples = NULL,
                          standards = NULL,
                          categories = NULL,
                          category_order = NULL,
                          scale_type = c("nominal", "ordinal"),
                          trials = 2,
                          comparison_mode = c("one_way", "two_way"),
                          kappa_method = c("cohen", "fleiss", "wagner"),
                          conf_level = 0.95) {

      comparison_mode <- match.arg(comparison_mode)
      kappa_method <- match.arg(kappa_method)
      scale_type <- match.arg(scale_type)

      super$initialize(task_tag = "attr_gage", conf_level = conf_level)
      self$set_meta("project", plan_name = plan_name, objectives = objectives)

      self$appraisers <- appraisers
      self$samples <- samples
      self$standards <- standards
      self$categories <- categories
      self$category_order <- category_order %||% categories
      self$scale_type <- scale_type
      self$trials <- trials
      self$comparison_mode <- comparison_mode
      self$kappa_method <- kappa_method

      invisible(self)
    },

    set_appraisers = function(appraisers) {
      self$appraisers <- appraisers
      invisible(self)
    },

    set_samples = function(samples) {
      self$samples <- samples
      invisible(self)
    },

    set_standards = function(standards) {
      self$standards <- standards
      invisible(self)
    },

    set_categories = function(categories) {
      self$categories <- categories
      if (is.null(self$category_order)) self$category_order <- categories
      invisible(self)
    },

    set_category_order = function(category_order) {
      self$category_order <- category_order
      self$categories <- self$categories %||% category_order
      invisible(self)
    },

    set_scale_type = function(scale_type = c("nominal", "ordinal")) {
      self$scale_type <- match.arg(scale_type)
      invisible(self)
    },

    validate = function() {
      super$validate()
      if (is.null(self$categories) || length(self$categories) < 2) {
        stop("At least 2 evaluation categories are required", call. = FALSE)
      }
      invisible(self)
    },

    generate_protocol = function() {
      list(
        task = "attr_gage",
        mode = self$comparison_mode,
        kappa_method = self$kappa_method,
        n_appraisers = length(self$appraisers),
        n_samples = length(self$samples),
        n_categories = length(self$categories),
        scale_type = self$scale_type,
        category_order = self$category_order,
        n_trials = self$trials,
        has_standard = !is.null(self$standards),
        conf_level = self$conf_level
      )
    },

    to_list = function() {
      c(super$to_list(), list(
        appraisers = self$appraisers,
        samples = self$samples,
        standards = self$standards,
        categories = self$categories,
        category_order = self$category_order,
        scale_type = self$scale_type,
        trials = self$trials,
        comparison_mode = self$comparison_mode,
        kappa_method = self$kappa_method
      ))
    }
  )
)
