# =============================================================================
# File: R/spc_run_multivariate.R
# Description: User entry functions - Multivariate control charts
#             (Hotelling T2, MEWMA)
# =============================================================================

# ---------------------------------------------------------------------------
# Internal: build task for multivariate charts
# ---------------------------------------------------------------------------
.spc_multivariate_task <- function(data, chart_type,
                                    nelson_rules = 1:8, lambda = 0.2,
                                    phase_boundaries = NULL,
                                    conf_level = 0.95,
                                    theme = "academic", ...) {
  if (!is.data.frame(data)) stop("data must be a data.frame", call. = FALSE)
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_cols) < 2) {
    stop("Need at least 2 numeric columns for multivariate charts.", call. = FALSE)
  }

  plan <- SpcPlan$new(
    chart_type = chart_type,
    sigma_method = "total",
    subgroup_size = 1L,
    nelson_rules = nelson_rules,
    lambda = lambda,
    phase = "phase1",
    phase_boundaries = phase_boundaries,
    conf_level = conf_level,
    task_tag = "spc",
    ...
  )
  task <- IqrSpcTask$new(data = data, plan = plan, theme = theme)
  task$compute()
  invisible(task)
}

#' @title Hotelling T2 Control Chart
#' @description Creates a Hotelling T2 control chart for monitoring multiple
#'   correlated variables simultaneously. The T2 statistic is the multivariate
#'   generalization of the univariate t-statistic. Control limits use the
#'   Phase I F-distribution approximation.
#' @param data Data frame with at least 2 numeric columns.
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
#'   x1 = rnorm(30, 50, 1),
#'   x2 = rnorm(30, 30, 0.8),
#'   x3 = rnorm(30, 75, 1.2)
#' )
#' \donttest{
#' task <- run_spc_t2(df)
#' task$summary()
#' }
run_spc_t2 <- function(data, nelson_rules = 1:8, phase_boundaries = NULL,
                       conf_level = 0.95, theme = "academic", ...) {
  .spc_multivariate_task(data, "t2",
    nelson_rules = nelson_rules, phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}

#' @title MEWMA Control Chart
#' @description Creates a Multivariate Exponentially Weighted Moving Average
#'   (MEWMA) control chart (Lowry et al., 1992). MEWMA is the multivariate
#'   extension of EWMA and is sensitive to small sustained shifts in the
#'   process mean vector.
#' @param data Data frame with at least 2 numeric columns.
#' @param lambda MEWMA smoothing parameter in (0, 1]. Default 0.2.
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
#'   x1 = c(rnorm(20, 50, 1), rnorm(10, 51.5, 1)),
#'   x2 = c(rnorm(20, 30, 0.8), rnorm(10, 31.2, 0.8))
#' )
#' \donttest{
#' task <- run_spc_mewma(df, lambda = 0.2)
#' }
run_spc_mewma <- function(data, lambda = 0.2, nelson_rules = 1:8,
                          phase_boundaries = NULL, conf_level = 0.95,
                          theme = "academic", ...) {
  .spc_multivariate_task(data, "mewma",
    nelson_rules = nelson_rules, lambda = lambda,
    phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}
