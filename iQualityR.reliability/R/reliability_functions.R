# =============================================================================
# File: R/reliability_functions.R
# Description: Convenience function entries for reliability analysis
# =============================================================================

#' @title Parametric Reliability Analysis
#'
#' @description Convenience wrapper that builds a [ReliabilityPlan], runs the
#' [ReliabilityAnalyzer], and returns the results. For full task-level
#' orchestration (plots, reports) use [IqrReliabilityTask] instead.
#'
#' @param data Data frame containing the time column.
#' @param time_var Character. Name of the time/lifetime column.
#' @param status_var Character. Name of the status column (optional).
#' @param distribution Character. Distribution to fit
#'   (`"weibull"`, `"exponential"`, `"lognormal"`, `"logistic"`).
#' @param conf_level Numeric. Confidence level.
#' @param ... Additional arguments passed to [ReliabilityPlan]$initialize().
#' @return List of analysis results (see [ReliabilityAnalyzer]$analyze()).
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' times <- rweibull(50, shape = 2, scale = 100)
#' data <- data.frame(time = times)
#' result <- reliability_analysis(data, time_var = "time",
#'                                distribution = "weibull")
#' result$reliability_metrics$mttf
#' }
reliability_analysis <- function(data, time_var,
                                 status_var = NULL,
                                 distribution = "weibull",
                                 conf_level = 0.95,
                                 ...) {
  plan <- ReliabilityPlan$new(
    time_var     = time_var,
    status_var   = status_var,
    distribution = distribution,
    method       = "parametric",
    conf_level   = conf_level,
    ...
  )
  analyzer <- ReliabilityAnalyzer$new()
  analyzer$analyze(data, plan)
}

#' @title Kaplan-Meier Survival Estimation
#'
#' @description Convenience wrapper for Kaplan-Meier nonparametric survival
#' estimation. Requires the `survival` package.
#'
#' @param data Data frame.
#' @param time_var Character. Name of the time column.
#' @param status_var Character. Name of the status column (1 = event, 0 = censored).
#' @param conf_level Numeric. Confidence level.
#' @param ... Additional arguments passed to [ReliabilityPlan]$initialize().
#' @return List of analysis results.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   set.seed(42)
#'   data <- data.frame(
#'     time   = rexp(50, rate = 0.01),
#'     status = rbinom(50, 1, 0.8)
#'   )
#'   result <- kaplan_meier_estimate(data, "time", "status")
#'   head(result$survival_curve)
#' }
#' }
kaplan_meier_estimate <- function(data, time_var, status_var,
                                   conf_level = 0.95, ...) {
  plan <- ReliabilityPlan$new(
    time_var       = time_var,
    status_var     = status_var,
    method         = "kaplan_meier",
    censoring_type = "right",
    conf_level     = conf_level,
    ...
  )
  analyzer <- ReliabilityAnalyzer$new()
  analyzer$analyze(data, plan)
}

#' @title Cox Proportional Hazards Model
#'
#' @description Convenience wrapper for fitting a Cox proportional hazards
#' model. Requires the `survival` package.
#'
#' @param data Data frame.
#' @param time_var Character. Name of the time column.
#' @param status_var Character. Name of the status column.
#' @param factors Character vector. Covariate names for the model.
#' @param conf_level Numeric. Confidence level.
#' @param ... Additional arguments passed to [ReliabilityPlan]$initialize().
#' @return List of analysis results.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   set.seed(42)
#'   data <- data.frame(
#'     time   = rexp(100, rate = 0.01),
#'     status = rbinom(100, 1, 0.7),
#'     age    = rnorm(100, 50, 10),
#'     treat  = rbinom(100, 1, 0.5)
#'   )
#'   result <- cox_model(data, "time", "status", factors = c("age", "treat"))
#'   result$cox_model$coefficients
#' }
#' }
cox_model <- function(data, time_var, status_var, factors,
                      conf_level = 0.95, ...) {
  plan <- ReliabilityPlan$new(
    time_var       = time_var,
    status_var     = status_var,
    method         = "cox",
    censoring_type = "right",
    factors        = factors,
    conf_level     = conf_level,
    ...
  )
  analyzer <- ReliabilityAnalyzer$new()
  analyzer$analyze(data, plan)
}
