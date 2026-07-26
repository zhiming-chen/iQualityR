# =============================================================================
# File: R/spc_run_adaptive.R
# Description: User entry functions - Adaptive control charts (v0.2)
#             - run_spc_adaptive: rolling-window adaptive Shewhart chart
#             - run_spc_aewma:    adaptive EWMA (variable lambda)
# =============================================================================

# ---------------------------------------------------------------------------
# Internal: build task for adaptive / aewma charts
# ---------------------------------------------------------------------------
.spc_adaptive_task <- function(data, measurement, chart_type,
                               sigma_method = NULL, nelson_rules = 1:8,
                               window_size = 20L,
                               aewma_lambda = 0.2, aewma_k = 1.5,
                               phase_boundaries = NULL,
                               conf_level = 0.95,
                               theme = "academic", ...) {
  if (!is.data.frame(data)) stop("data must be a data.frame", call. = FALSE)
  if (is.null(measurement) || !measurement %in% names(data)) {
    stop("measurement column not found in data", call. = FALSE)
  }
  if (!is.numeric(data[[measurement]])) {
    stop("measurement column must be numeric", call. = FALSE)
  }

  plan <- SpcPlan$new(
    chart_type = chart_type,
    sigma_method = sigma_method %||% "mr_bar",
    subgroup_size = 1L,
    nelson_rules = nelson_rules,
    window_size = window_size,
    aewma_lambda = aewma_lambda,
    aewma_k = aewma_k,
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

#' @title Adaptive Rolling-Window Control Chart
#' @description Creates an adaptive control chart whose center line and
#'   sigma are recomputed over a rolling window of the most recent
#'   `window_size` observations. Useful for processes with naturally
#'   drifting mean where fixed limits would produce false alarms.
#'
#'   For each point `i`, the center is the mean of the previous
#'   `window_size` observations (including point `i`) and sigma is
#'   estimated via `iQualityR.stat::sigma_estimate` on the same window.
#'
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name.
#' @param sigma_method Sigma estimation method (default `"mr_bar"`).
#' @param window_size Rolling window size (>=5). Default 20.
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param phase_boundaries Optional integer vector of phase boundary indices.
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(measurement = c(rnorm(30, 100, 1),
#'                                  rnorm(20, 102, 1)))
#' \donttest{
#' task <- run_spc_adaptive(df, "measurement", window_size = 15)
#' task$summary()
#' }
run_spc_adaptive <- function(data, measurement, sigma_method = NULL,
                             window_size = 20L, nelson_rules = 1:8,
                             phase_boundaries = NULL, conf_level = 0.95,
                             theme = "academic", ...) {
  .spc_adaptive_task(data, measurement, "adaptive",
    sigma_method = sigma_method, nelson_rules = nelson_rules,
    window_size = window_size, phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}

#' @title Adaptive EWMA Control Chart
#' @description Creates an Adaptive EWMA (AEWMA) control chart where the
#'   smoothing parameter `lambda` varies over time based on the magnitude
#'   of the recent forecast error. Large deviations increase `lambda`,
#'   making the chart more responsive to sudden shifts while retaining
#'   the smoothing benefit of standard EWMA in steady state.
#'
#'   The adaptive update is:
#'   \eqn{\lambda_i = \lambda_0 + (1 - \lambda_0) \cdot \min(1, k \cdot |e_i| / \sigma)}
#'   where \eqn{e_i = x_i - z_{i-1}} is the one-step forecast error.
#'
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name.
#' @param sigma_method Sigma estimation method (default `"mr_bar"`).
#' @param aewma_lambda Base EWMA smoothing in (0, 1]. Default 0.2.
#' @param aewma_k Sensitivity parameter (>0). Larger = faster adaptation. Default 1.5.
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param phase_boundaries Optional integer vector of phase boundary indices.
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(measurement = c(rnorm(30, 100, 1), rnorm(15, 103, 1)))
#' \donttest{
#' task <- run_spc_aewma(df, "measurement", aewma_lambda = 0.2, aewma_k = 1.5)
#' task$summary()
#' }
run_spc_aewma <- function(data, measurement, sigma_method = NULL,
                          aewma_lambda = 0.2, aewma_k = 1.5,
                          nelson_rules = 1:8,
                          phase_boundaries = NULL, conf_level = 0.95,
                          theme = "academic", ...) {
  .spc_adaptive_task(data, measurement, "aewma",
    sigma_method = sigma_method, nelson_rules = nelson_rules,
    aewma_lambda = aewma_lambda, aewma_k = aewma_k,
    phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}
