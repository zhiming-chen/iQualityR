# =============================================================================
# File: R/Capability_normal.R
# Description: User entry function - Normal capability analysis
# =============================================================================

#' @title Normal Process Capability Analysis
#'
#' @description
#' Computes process capability indices (Cp, Cpk, Pp, Ppk, and optionally Cpm)
#' under the normal distribution assumption, and optionally generates a Sixpack
#' diagnostic plot set.
#'
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name (character).
#' @param lsl Lower specification limit (numeric).
#' @param usl Upper specification limit (numeric).
#' @param target Optional target value used for Cpm calculation (numeric).
#' @param subgroup Optional subgroup column name for within-group sigma estimation.
#' @param sixpack Logical; whether to generate the Sixpack diagnostic plots.
#' @param conf_level Confidence level (default 0.95).
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
#'   measurement = rnorm(100, mean = 50, sd = 2),
#'   subgroup = rep(1:20, each = 5)
#' )
#' result <- capability_normal(
#'   data = data, measurement = "measurement",
#'   lsl = 44, usl = 56, target = 50,
#'   subgroup = "subgroup"
#' )
#' result$summary()
#' }
capability_normal <- function(data, measurement, lsl, usl, target = NULL,
                              subgroup = NULL, sixpack = FALSE, conf_level = 0.95,
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
  if (!is.null(subgroup) && !subgroup %in% names(data)) {
    stop("subgroup column '", subgroup, "' not found in data", call. = FALSE)
  }

  plan <- CapabilityPlan$new(
    lsl = lsl, usl = usl, target = target, subgroup = subgroup,
    conf_level = conf_level, sixpack = sixpack,
    analysis_type = "normal"
  )
  task <- IqrCapabilityTask$new(data, measurement, plan, theme = theme, ...)
  task$compute()
  invisible(task)
}
