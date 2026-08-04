# =============================================================================
# File: R/Capability_between_within.R
# Description: User entry function - Between/Within capability analysis
# =============================================================================

#' @title Between/Within Process Capability Analysis
#'
#' @description
#' Computes process capability indices under the Minitab "Between/Within"
#' (B/W) convention. Cp/Cpk (B/W) use the combined between-within sigma
#' (`sigma_between_within`) and Pp/Ppk use the overall sigma (`sigma_total`).
#'
#' Requires subgroup data to decompose sigma into within, between, and
#' between-within components via [iQualityR.stat::sigma_decomposition]. This
#' path is appropriate when both within-subgroup and between-subgroup variation
#' should be pooled for the short-term (potential) capability estimate, which
#' gives a more honest Cp/Cpk than the pure within-sigma approach when the
#' process drifts between subgroups.
#'
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name (character).
#' @param lsl Lower specification limit (numeric).
#' @param usl Upper specification limit (numeric).
#' @param target Optional target value used for Cpm calculation (numeric).
#' @param subgroup Subgroup column name for within/between sigma decomposition
#'   (required).
#' @param conf_level Confidence level (default 0.95).
#' @param use_bootstrap Logical; whether to compute bootstrap confidence intervals.
#' @param bootstrap_samples Number of bootstrap replications (default 1000).
#' @param theme Theme name or [IqrTheme] object (default "academic").
#' @param ... Additional arguments passed to [IqrCapabilityTask].
#'
#' @return An [IqrCapabilityTask] object with computed results. In addition to
#'   the standard `sd_within` and `sd_overall` statistics, the result container
#'   also exposes `sd_between` and `sd_between_within`.
#' @export
#'
#' @examples
#' set.seed(123)
#' \donttest{
#' data <- data.frame(
#'   measurement = rnorm(100, mean = 50, sd = 2),
#'   subgroup = rep(1:20, each = 5)
#' )
#' result <- capability_between_within(
#'   data = data, measurement = "measurement",
#'   lsl = 44, usl = 56, target = 50,
#'   subgroup = "subgroup"
#' )
#' result$summary()
#' }
capability_between_within <- function(data, measurement, lsl, usl, target = NULL,
                                     subgroup, conf_level = 0.95,
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
  if (missing(subgroup) || is.null(subgroup)) {
    stop("subgroup is required for Between/Within capability analysis", call. = FALSE)
  }
  if (!subgroup %in% names(data)) {
    stop("subgroup column '", subgroup, "' not found in data", call. = FALSE)
  }
  if (!is.logical(use_bootstrap)) stop("use_bootstrap must be logical", call. = FALSE)
  if (!is.numeric(bootstrap_samples) || bootstrap_samples < 1) {
    stop("bootstrap_samples must be a positive integer", call. = FALSE)
  }

  plan <- CapabilityPlan$new(
    lsl = lsl, usl = usl, target = target, subgroup = subgroup,
    conf_level = conf_level,
    use_bootstrap = use_bootstrap, bootstrap_samples = bootstrap_samples,
    analysis_type = "between_within"
  )
  task <- IqrCapabilityTask$new(data, measurement, plan, theme = theme, ...)
  task$compute()
  invisible(task)
}
