# =============================================================================
# File: R/Capability_nonnormal.R
# Description: User entry function - Non-normal capability analysis
# =============================================================================

#' @title Non-Normal Process Capability Analysis
#'
#' @description
#' Performs process capability analysis under a non-normal distribution
#' assumption. The distribution can be auto-selected or specified explicitly.
#' Supported distributions: Weibull, Lognormal, Gamma, Exponential, Logistic,
#' and Beta.
#'
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name (character).
#' @param lsl Lower specification limit (numeric).
#' @param usl Upper specification limit (numeric).
#' @param distribution Distribution name or `"auto"` for automatic selection
#'   (default `"auto"`). Available distributions: `"weibull"`, `"lognormal"`,
#'   `"gamma"`, `"exponential"`, `"logistic"`, `"beta"`.
#' @param target Optional target value used for Cpm calculation (numeric).
#' @param conf_level Confidence level (default 0.95).
#' @param use_bootstrap Logical; whether to use Bootstrap for confidence intervals.
#' @param bootstrap_samples Number of Bootstrap replications (default 1000).
#' @param theme Theme name or [IqrTheme] object (default "academic").
#' @param ... Additional arguments passed to [IqrCapabilityTask].
#'
#' @return An [IqrCapabilityTask] object with computed results.
#' @export
#'
#' @examples
#' set.seed(123)
#' \donttest{
#' data <- data.frame(
#'   measurement = rweibull(100, shape = 2, scale = 50)
#' )
#' result <- capability_nonnormal(
#'   data = data, measurement = "measurement",
#'   lsl = 10, usl = 100,
#'   distribution = "auto"
#' )
#' result$summary()
#' }
capability_nonnormal <- function(data, measurement, lsl, usl,
                                 distribution = "auto", target = NULL,
                                 conf_level = 0.95,
                                 use_bootstrap = FALSE, bootstrap_samples = 1000,
                                 theme = "academic", ...) {
  # Input validation
  if (!is.data.frame(data)) stop("data must be a data.frame", call. = FALSE)
  if (!measurement %in% names(data)) stop("Column '", measurement, "' not found in data", call. = FALSE)
  if (!is.numeric(data[[measurement]])) stop("measurement column must be numeric", call. = FALSE)
  if (!is.numeric(lsl) || !is.numeric(usl)) stop("lsl and usl must be numeric", call. = FALSE)
  if (lsl >= usl) stop("lsl must be less than usl", call. = FALSE)
  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be between 0 and 1", call. = FALSE)
  }
  if (!is.null(target) && !is.numeric(target)) stop("target must be numeric", call. = FALSE)
  if (!is.logical(use_bootstrap)) stop("use_bootstrap must be logical", call. = FALSE)
  if (!is.numeric(bootstrap_samples) || bootstrap_samples < 1) {
    stop("bootstrap_samples must be a positive integer", call. = FALSE)
  }

  # Validate distribution
  valid_dists <- c("auto", "weibull", "lognormal", "gamma",
                   "exponential", "logistic", "beta")
  if (!distribution %in% valid_dists) {
    stop("Invalid distribution. Must be one of: ",
         paste(valid_dists, collapse = ", "), call. = FALSE)
  }

  plan <- CapabilityPlan$new(
    lsl = lsl, usl = usl, target = target,
    conf_level = conf_level,
    use_bootstrap = use_bootstrap, bootstrap_samples = bootstrap_samples,
    analysis_type = "nonnormal", distribution = distribution
  )
  task <- IqrCapabilityTask$new(data, measurement, plan, theme = theme, ...)
  task$compute()
  invisible(task)
}
