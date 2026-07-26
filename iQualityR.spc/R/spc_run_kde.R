# =============================================================================
# File: R/spc_run_kde.R
# Description: User entry function - KDE nonparametric control chart (v0.2)
# =============================================================================

#' @title KDE Nonparametric Control Chart
#' @description Creates a control chart whose limits are derived from the
#'   empirical distribution of the data via Kernel Density Estimation
#'   (KDE). Useful when the underlying process is non-normal and the
#'   standard 3-sigma limits of Shewhart charts are not appropriate.
#'
#'   The density is estimated via `stats::density(x, bw, kernel = "gaussian")`.
#'   Control limits are the empirical 0.135th and 99.865th percentiles
#'   (equivalent to +-3 sigma under normality) computed from the KDE
#'   via quantile integration.
#'
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name.
#' @param kde_bandwidth Optional numeric bandwidth. If NULL (default),
#'   Sheather-Jones (`bw.SJ`) is used to select the bandwidth automatically.
#' @param sigma_method Sigma estimation method (default `"total"`).
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param phase_boundaries Optional integer vector of phase boundary indices.
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results. The `diagnostics`
#'   field stores the bandwidth used.
#' @export
#' @examples
#' set.seed(123)
#' # Non-normal data (exponential)
#' df <- data.frame(measurement = rexp(50, rate = 0.1))
#' \donttest{
#' task <- run_spc_kde(df, "measurement")
#' task$summary()
#' }
run_spc_kde <- function(data, measurement, kde_bandwidth = NULL,
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
    chart_type = "kde",
    sigma_method = sigma_method %||% "total",
    subgroup_size = 1L,
    nelson_rules = nelson_rules,
    kde_bandwidth = kde_bandwidth,
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
