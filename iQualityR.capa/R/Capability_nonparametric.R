# =============================================================================
# File: R/Capability_nonparametric.R
# Description: User entry function - Non-parametric capability analysis
# =============================================================================

#' @title Non-Parametric Process Capability Analysis
#'
#' @description
#' Performs non-parametric process capability analysis based on the empirical
#' distribution. Suitable when the underlying distribution is unknown or does
#' not fit common parametric families.
#'
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name (character).
#' @param lsl Lower specification limit (numeric).
#' @param usl Upper specification limit (numeric).
#' @param target Optional target value (numeric).
#' @param conf_level Confidence level (default 0.95).
#' @param use_bootstrap Logical; whether to use Bootstrap for confidence intervals
#'   (default FALSE).
#' @param bootstrap_samples Number of Bootstrap replications (default 1000).
#' @param nonparametric_method Non-parametric method: `"kernel"` (kernel density
#'   estimation) or `"empirical"` (empirical distribution).
#' @param theme Theme name or [IqrTheme] object (default "academic").
#' @param ... Additional arguments passed to [IqrCapabilityTask].
#'
#' @return An [IqrCapabilityTask] object with computed results.
#' @export
#'
#' @examples
#' set.seed(123)
#' \donttest{
#' data <- data.frame(measurement = rweibull(100, shape = 2, scale = 50))
#'
#' # Basic non-parametric capability analysis
#' result <- capability_nonparametric(
#'   data = data, measurement = "measurement",
#'   lsl = 10, usl = 100
#' )
#' result$summary()
#'
#' # Using Bootstrap confidence intervals
#' result <- capability_nonparametric(
#'   data = data, measurement = "measurement",
#'   lsl = 10, usl = 100,
#'   use_bootstrap = TRUE, bootstrap_samples = 500
#' )
#' result$summary()
#'
#' # Specify the non-parametric method
#' result <- capability_nonparametric(
#'   data = data, measurement = "measurement",
#'   lsl = 10, usl = 100,
#'   nonparametric_method = "empirical"
#' )
#' result$summary()
#' }
capability_nonparametric <- function(data, measurement, lsl, usl,
                                    target = NULL,
                                    conf_level = 0.95,
                                    use_bootstrap = FALSE, bootstrap_samples = 1000,
                                    nonparametric_method = "kernel",
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
  if (!nonparametric_method %in% c("kernel", "empirical")) {
    stop("nonparametric_method must be either 'kernel' or 'empirical'", call. = FALSE)
  }

  plan <- CapabilityPlan$new(
    lsl = lsl, usl = usl, target = target,
    conf_level = conf_level,
    use_bootstrap = use_bootstrap, bootstrap_samples = bootstrap_samples,
    analysis_type = "nonparametric",
    nonparametric_method = nonparametric_method
  )

  task <- IqrCapabilityTask$new(data, measurement, plan, theme = theme, ...)
  task$compute()
  invisible(task)
}
