# =============================================================================
# File: R/spc_run_changepoint.R
# Description: User entry function - Change-point detection chart (v0.2)
# =============================================================================

#' @title Change-Point Detection Control Chart
#' @description Detects change-points in the process mean and/or variance
#'   using a CUSUM-based binary segmentation algorithm. Change-points are
#'   displayed as vertical lines on the time-series plot; segment means
#'   before and after each change are reported.
#'
#'   If the optional `changepoint` package is installed, it will be used
#'   (via `requireNamespace`) to apply the PELT algorithm with the
#'   configured `cp_penalty`. Otherwise, an in-house binary segmentation
#'   routine based on cumulative sums is used.
#'
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name.
#' @param cp_method Method string. Default `"pelt"`.
#' @param cp_penalty Penalty string or numeric. Default `"MBIC"`.
#' @param sigma_method Sigma estimation method (default `"total"`).
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param phase_boundaries Optional integer vector of phase boundary indices.
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results. The `data_tables`
#'   field additionally contains a `change_points` data frame with columns
#'   `index`, `mean_before`, `mean_after`, `mean_diff`.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(measurement = c(rnorm(30, 100, 1),
#'                                  rnorm(30, 103, 1)))
#' \donttest{
#' task <- run_spc_changepoint(df, "measurement")
#' task$summary()
#' }
run_spc_changepoint <- function(data, measurement,
                                cp_method = "pelt", cp_penalty = "MBIC",
                                sigma_method = NULL, nelson_rules = 1:8,
                                phase_boundaries = NULL, conf_level = 0.95,
                                theme = "academic", ...) {
  if (!is.data.frame(data)) stop("data must be a data.frame", call. = FALSE)
  if (is.null(measurement) || !measurement %in% names(data)) {
    stop("measurement column not found in data", call. = FALSE)
  }
  if (!is.numeric(data[[measurement]])) {
    stop("measurement column must be numeric", call. = FALSE)
  }

  plan <- SpcPlan$new(
    chart_type = "changepoint",
    sigma_method = sigma_method %||% "total",
    subgroup_size = 1L,
    nelson_rules = nelson_rules,
    cp_method = cp_method,
    cp_penalty = cp_penalty,
    phase = "phase1",
    phase_boundaries = phase_boundaries,
    conf_level = conf_level,
    task_tag = "spc",
    ...
  )
  task <- IqrSpcTask$new(data = data, measurement = measurement,
                         plan = plan, theme = theme)
  task$compute()
  invisible(task)
}
