# =============================================================================
# File: R/spc_run_rare_events.R
# Description: User entry functions - Rare-event control charts
#             (G chart, T chart)
# =============================================================================

# ---------------------------------------------------------------------------
# Internal: build task for rare-event charts
# ---------------------------------------------------------------------------
.spc_rare_event_task <- function(data, measurement, chart_type,
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

  plan <- SpcPlan$new(
    chart_type = chart_type,
    sigma_method = "total",
    subgroup_size = 1L,
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

#' @title G Chart (Geometric)
#' @description Creates a G chart for monitoring the number of opportunities
#'   between consecutive rare events (defects). The G chart is appropriate when
#'   defects are infrequent and traditional P / NP charts are insensitive.
#'   Based on the geometric distribution.
#' @param data Data frame containing the measurement column.
#' @param measurement Column of opportunity counts between events.
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param phase_boundaries Optional integer vector of phase boundary indices.
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(opportunities = rgeom(30, prob = 0.05) + 1)
#' \donttest{
#' task <- run_spc_g(df, "opportunities")
#' task$summary()
#' }
run_spc_g <- function(data, measurement, nelson_rules = 1:8,
                     phase_boundaries = NULL, conf_level = 0.95,
                     theme = "academic", ...) {
  .spc_rare_event_task(data, measurement, "g",
    nelson_rules = nelson_rules, phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}

#' @title T Chart (Time Between Events)
#' @description Creates a T chart for monitoring the time between consecutive
#'   rare events. The T chart is appropriate when defects are infrequent and
#'   the time interval is exponentially distributed. A log transform is applied
#'   internally so that Nelson rules can be applied on the normal scale.
#' @param data Data frame containing the measurement column.
#' @param measurement Column of time intervals between events.
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param phase_boundaries Optional integer vector of phase boundary indices.
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(time_between = rexp(30, rate = 0.1))
#' \donttest{
#' task <- run_spc_t(df, "time_between")
#' }
run_spc_t <- function(data, measurement, nelson_rules = 1:8,
                     phase_boundaries = NULL, conf_level = 0.95,
                     theme = "academic", ...) {
  .spc_rare_event_task(data, measurement, "t",
    nelson_rules = nelson_rules, phase_boundaries = phase_boundaries,
    conf_level = conf_level, theme = theme, ...)
}
