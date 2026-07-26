# =============================================================================
# File: R/spc_run_variables.R
# Description: User entry functions - Variables control charts
#             (Xbar-R, Xbar-S, I-MR, I-MR-R/S)
# =============================================================================

# Internal null-coalescing operator (not exported, no Rd generation)
`%||%` <- function(x, y) if (is.null(x)) y else x  # nolint

# ---------------------------------------------------------------------------
# Internal: build task for variables charts
# ---------------------------------------------------------------------------
.spc_variables_task <- function(data, measurement, chart_type,
                                subgroup = NULL, subgroup_size = NULL,
                                sigma_method = NULL,
                                nelson_rules = 1:8,
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

  if (is.null(subgroup_size)) {
    subgroup_size <- if (chart_type %in% c("imr", "imr_rs")) 1L else 5L
  }

  plan <- SpcPlan$new(
    chart_type = chart_type,
    sigma_method = sigma_method,
    subgroup_size = subgroup_size,
    subgroup = subgroup,
    nelson_rules = nelson_rules,
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

#' @title Xbar-R Control Chart
#' @description Creates an Xbar-R (mean and range) control chart for subgrouped
#'   continuous data. Control limits use the A2 and D4 constants from
#'   `iQualityR.stat`.
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name.
#' @param subgroup Optional subgroup column name. If NULL, observations are
#'   chunked into groups of `subgroup_size`.
#' @param subgroup_size Subgroup size (default 5). Must be >= 2.
#' @param sigma_method Sigma estimation method. Default uses `"r_bar"`.
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param phase_boundaries Optional integer vector of phase boundary indices.
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   measurement = rnorm(50, mean = 100, sd = 2),
#'   subgroup = rep(1:10, each = 5)
#' )
#' \donttest{
#' task <- run_spc_xbar_r(df, "measurement", subgroup = "subgroup",
#'                       subgroup_size = 5)
#' task$summary()
#' }
run_spc_xbar_r <- function(data, measurement, subgroup = NULL,
                           subgroup_size = 5L, sigma_method = NULL,
                           nelson_rules = 1:8, phase_boundaries = NULL,
                           conf_level = 0.95, theme = "academic", ...) {
  .spc_variables_task(data, measurement, "xbar_r",
    subgroup = subgroup, subgroup_size = subgroup_size,
    sigma_method = sigma_method %||% "r_bar",
    nelson_rules = nelson_rules, phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}

#' @title Xbar-S Control Chart
#' @description Creates an Xbar-S (mean and standard deviation) control chart.
#'   Uses A3, B3, B4 constants from `iQualityR.stat`.
#' @inheritParams run_spc_xbar_r
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   measurement = rnorm(60, mean = 50, sd = 1.5),
#'   subgroup = rep(1:10, each = 6)
#' )
#' \donttest{
#' task <- run_spc_xbar_s(df, "measurement", subgroup = "subgroup",
#'                       subgroup_size = 6)
#' }
run_spc_xbar_s <- function(data, measurement, subgroup = NULL,
                          subgroup_size = 5L, sigma_method = NULL,
                          nelson_rules = 1:8, phase_boundaries = NULL,
                          conf_level = 0.95, theme = "academic", ...) {
  .spc_variables_task(data, measurement, "xbar_s",
    subgroup = subgroup, subgroup_size = subgroup_size,
    sigma_method = sigma_method %||% "s_bar",
    nelson_rules = nelson_rules, phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}

#' @title I-MR Control Chart
#' @description Creates an Individuals and Moving-Range control chart for
#'   ungrouped continuous data. Sigma is estimated via MR-bar / d2(2).
#' @inheritParams run_spc_xbar_r
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(measurement = rnorm(30, mean = 100, sd = 1))
#' \donttest{
#' task <- run_spc_imr(df, "measurement")
#' task$summary()
#' }
run_spc_imr <- function(data, measurement, sigma_method = NULL,
                        nelson_rules = 1:8, phase_boundaries = NULL,
                        conf_level = 0.95, theme = "academic", ...) {
  .spc_variables_task(data, measurement, "imr",
    subgroup = NULL, subgroup_size = 1L,
    sigma_method = sigma_method %||% "mr_bar",
    nelson_rules = nelson_rules, phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}

#' @title I-MR-R/S Control Chart
#' @description Creates an I-MR-R/S (Individual-Moving Range / Range or Std Dev)
#'   chart. The between-subgroup dispersion is estimated separately from
#'   within-subgroup variation. Sigma uses pooled SD by default.
#' @inheritParams run_spc_xbar_r
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   measurement = rnorm(40, mean = 50, sd = 1.2),
#'   subgroup = rep(1:8, each = 5)
#' )
#' \donttest{
#' task <- run_spc_imr_rs(df, "measurement", subgroup = "subgroup",
#'                       subgroup_size = 5)
#' }
run_spc_imr_rs <- function(data, measurement, subgroup = NULL,
                           subgroup_size = 5L, sigma_method = NULL,
                           nelson_rules = 1:8, phase_boundaries = NULL,
                           conf_level = 0.95, theme = "academic", ...) {
  .spc_variables_task(data, measurement, "imr_rs",
    subgroup = subgroup, subgroup_size = subgroup_size,
    sigma_method = sigma_method %||% "pooled_s",
    nelson_rules = nelson_rules, phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}
