# =============================================================================
# File: R/spc_run_t2_mewma.R
# Description: User entry function - T2 + MEWMA hybrid multivariate chart (v0.2)
# =============================================================================

#' @title T2 + MEWMA Hybrid Multivariate Control Chart
#' @description Combines Hotelling T2 (sensitive to large mean shifts) with
#'   MEWMA (sensitive to small sustained shifts) into a single chart.
#'   Both statistics are computed on the same data; a violation is flagged
#'   when EITHER statistic exceeds its respective upper control limit.
#'
#'   The combined statistic displayed on the chart is the maximum of
#'   `T2 / UCL_T2` and `MEWMA / UCL_MEWMA` (a value > 1 indicates a
#'   violation). Both raw statistics are stored in the `points` data table.
#'
#' @param data Data frame with at least 2 numeric columns.
#' @param lambda MEWMA smoothing parameter in (0, 1]. Default 0.2.
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   x1 = c(rnorm(25, 50, 1), rnorm(10, 52, 1)),
#'   x2 = c(rnorm(25, 30, 0.8), rnorm(10, 31, 0.8))
#' )
#' \donttest{
#' task <- run_spc_t2_mewma(df, lambda = 0.2)
#' task$summary()
#' }
run_spc_t2_mewma <- function(data, lambda = 0.2, nelson_rules = 1:8,
                              conf_level = 0.95, theme = "academic", ...) {
  if (!is.data.frame(data)) stop("data must be a data.frame", call. = FALSE)

  plan <- SpcPlan$new(
    chart_type = "t2_mewma",
    sigma_method = "total",
    subgroup_size = 1L,
    nelson_rules = nelson_rules,
    lambda = lambda,
    phase = "phase1",
    conf_level = conf_level,
    task_tag = "spc",
    ...
  )
  task <- IqrSpcTask$new(data = data, plan = plan, theme = theme)
  task$compute()
  invisible(task)
}
