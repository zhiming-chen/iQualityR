# =============================================================================
# File: R/spc_run_time_weighted.R
# Description: User entry functions - Time-weighted control charts
#             (EWMA, CUSUM, MA)
# =============================================================================

# ---------------------------------------------------------------------------
# Internal: build task for time-weighted charts
# ---------------------------------------------------------------------------
.spc_time_weighted_task <- function(data, measurement, chart_type,
                                    subgroup = NULL, sigma_method = NULL,
                                    nelson_rules = 1:8,
                                    lambda = 0.2, k = 0.5, h = 4.77,
                                    ma_window = 3L,
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
    subgroup = subgroup,
    nelson_rules = nelson_rules,
    lambda = lambda,
    k = k,
    h = h,
    ma_window = ma_window,
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

#' @title EWMA Control Chart
#' @description Creates an Exponentially Weighted Moving Average (EWMA)
#'   control chart. The EWMA statistic \eqn{Z_i = \lambda X_i + (1 - \lambda) Z_{i-1}}
#'   smooths short-term fluctuations, making it sensitive to small sustained
#'   shifts in the process mean.
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name.
#' @param sigma_method Sigma estimation method (default `"mr_bar"`).
#' @param lambda EWMA smoothing parameter in (0, 1]. Default 0.2.
#'   Smaller values detect smaller shifts but react slower.
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param phase_boundaries Optional integer vector of phase boundary indices.
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(measurement = c(rnorm(20, 100, 1), rnorm(10, 102, 1)))
#' \donttest{
#' task <- run_spc_ewma(df, "measurement", lambda = 0.2)
#' task$summary()
#' }
run_spc_ewma <- function(data, measurement, sigma_method = NULL,
                         lambda = 0.2, nelson_rules = 1:8,
                         phase_boundaries = NULL, conf_level = 0.95,
                         theme = "academic", ...) {
  .spc_time_weighted_task(data, measurement, "ewma",
    sigma_method = sigma_method, nelson_rules = nelson_rules,
    lambda = lambda, phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}

#' @title CUSUM Control Chart (Tabular)
#' @description Creates a tabular (two-sided) Cumulative Sum control chart.
#'   The CUSUM statistic accumulates deviations from the target, making it
#'   sensitive to small persistent shifts. Reference value `k` (in sigma units)
#'   and decision interval `h` (in sigma units) control sensitivity.
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name.
#' @param sigma_method Sigma estimation method (default `"mr_bar"`).
#' @param k Reference value (in sigma units). Default 0.5 (detects 0.5 sigma shifts).
#' @param h Decision interval (in sigma units). Default 4.77 (ARL0 ~ 370).
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param phase_boundaries Optional integer vector of phase boundary indices.
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(measurement = c(rnorm(20, 50, 1), rnorm(10, 51.5, 1)))
#' \donttest{
#' task <- run_spc_cusum(df, "measurement", k = 0.5, h = 4.77)
#' }
run_spc_cusum <- function(data, measurement, sigma_method = NULL,
                         k = 0.5, h = 4.77, nelson_rules = 1:8,
                         phase_boundaries = NULL, conf_level = 0.95,
                         theme = "academic", ...) {
  .spc_time_weighted_task(data, measurement, "cusum",
    sigma_method = sigma_method, nelson_rules = nelson_rules,
    k = k, h = h, phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}

#' @title Moving Average Control Chart
#' @description Creates a Moving Average (MA) control chart. The MA statistic
#'   averages the most recent `ma_window` observations, smoothing short-term
#'   noise. Control limits use sigma / sqrt(window).
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name.
#' @param sigma_method Sigma estimation method (default `"mr_bar"`).
#' @param ma_window Moving-average window size. Default 3.
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param phase_boundaries Optional integer vector of phase boundary indices.
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(measurement = rnorm(30, 100, 1))
#' \donttest{
#' task <- run_spc_ma(df, "measurement", ma_window = 3)
#' }
run_spc_ma <- function(data, measurement, sigma_method = NULL,
                       ma_window = 3L, nelson_rules = 1:8,
                       phase_boundaries = NULL, conf_level = 0.95,
                       theme = "academic", ...) {
  .spc_time_weighted_task(data, measurement, "ma",
    sigma_method = sigma_method, nelson_rules = nelson_rules,
    ma_window = ma_window, phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}
