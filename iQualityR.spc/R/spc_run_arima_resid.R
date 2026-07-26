# =============================================================================
# File: R/spc_run_arima_resid.R
# Description: User entry function - ARIMA residual control chart (v0.2)
# =============================================================================

#' @title ARIMA Residual Control Chart
#' @description Fits an ARIMA(p,d,q) model to the time series, then applies
#'   an I-MR control chart to the residuals. Useful for autocorrelated
#'   data where standard Shewhart charts produce misleading limits.
#'
#'   The model is fit via `stats::arima(x, order = c(p, d, q))`.
#'   Residuals are extracted and monitored via `iQualityR.stat`'s
#'   moving-range sigma estimation and Nelson rules detection.
#'
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name.
#' @param arima_order Integer vector `c(p, d, q)`. Default `c(1, 0, 1)`.
#' @param sigma_method Sigma estimation method for residuals (default `"mr_bar"`).
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param phase_boundaries Optional integer vector of phase boundary indices.
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments passed to `stats::arima`.
#' @return An `IqrSpcTask` object with computed results. The `diagnostics`
#'   field additionally stores the ARIMA model coefficients, sigma2, and
#'   AIC for traceability.
#' @export
#' @examples
#' set.seed(123)
#' # Autocorrelated process
#' ar1 <- stats::arima.sim(list(ar = 0.6), n = 60, sd = 1) + 100
#' df <- data.frame(measurement = as.numeric(ar1))
#' \donttest{
#' task <- run_spc_arima_resid(df, "measurement", arima_order = c(1, 0, 0))
#' task$summary()
#' }
run_spc_arima_resid <- function(data, measurement, arima_order = c(1L, 0L, 1L),
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
    chart_type = "arima_resid",
    sigma_method = sigma_method %||% "mr_bar",
    subgroup_size = 1L,
    nelson_rules = nelson_rules,
    arima_order = arima_order,
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
