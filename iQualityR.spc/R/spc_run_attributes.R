# =============================================================================
# File: R/spc_run_attributes.R
# Description: User entry functions - Attributes control charts
#             (P, NP, U, C, Laney P', Laney U')
# =============================================================================

# ---------------------------------------------------------------------------
# Internal: build task for attributes charts
# ---------------------------------------------------------------------------
.spc_attributes_task <- function(data, count, sample_size = NULL,
                                  chart_type, nelson_rules = 1:8,
                                  conf_level = 0.95,
                                  theme = "academic", ...) {
  if (!is.data.frame(data)) stop("data must be a data.frame", call. = FALSE)
  if (is.null(count)) stop("count is required for attributes charts.", call. = FALSE)
  if (is.null(sample_size) && chart_type != "c") {
    stop("sample_size is required for P/NP/U/Laney charts.", call. = FALSE)
  }

  plan <- SpcPlan$new(
    chart_type = chart_type,
    sigma_method = "total",
    subgroup_size = 1L,
    nelson_rules = nelson_rules,
    phase = "phase1",
    conf_level = conf_level,
    task_tag = "spc",
    ...
  )
  task <- IqrSpcTask$new(data = data, count = count,
                         sample_size = sample_size,
                         plan = plan, theme = theme)
  task$compute()
  invisible(task)
}

#' @title P Chart (Proportion Defective)
#' @description Creates a P chart for monitoring the proportion of defective
#'   items. Sample sizes may vary across subgroups.
#' @param data Data frame containing count and sample-size columns.
#' @param count Count of defective items (column name or numeric vector).
#' @param sample_size Sample size per subgroup (column name or numeric vector).
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   defectives = rbinom(20, 100, 0.05),
#'   sample_size = rep(100, 20)
#' )
#' \donttest{
#' task <- run_spc_p(df, "defectives", "sample_size")
#' task$summary()
#' }
run_spc_p <- function(data, count, sample_size,
                     nelson_rules = 1:8, conf_level = 0.95,
                     theme = "academic", ...) {
  .spc_attributes_task(data, count, sample_size, "p",
    nelson_rules = nelson_rules, conf_level = conf_level,
    theme = theme, ...)
}

#' @title NP Chart (Number Defective)
#' @description Creates an NP chart for monitoring the count of defective items
#'   when sample sizes are equal (or approximately equal).
#' @inheritParams run_spc_p
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   defectives = rbinom(20, 50, 0.04),
#'   sample_size = rep(50, 20)
#' )
#' \donttest{
#' task <- run_spc_np(df, "defectives", "sample_size")
#' }
run_spc_np <- function(data, count, sample_size,
                     nelson_rules = 1:8, conf_level = 0.95,
                     theme = "academic", ...) {
  .spc_attributes_task(data, count, sample_size, "np",
    nelson_rules = nelson_rules, conf_level = conf_level,
    theme = theme, ...)
}

#' @title U Chart (Defects per Unit)
#' @description Creates a U chart for monitoring the rate of nonconformities
#'   per unit. Sample sizes may vary.
#' @inheritParams run_spc_p
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   defects = rpois(20, lambda = 4),
#'   sample_size = rep(1, 20)
#' )
#' \donttest{
#' task <- run_spc_u(df, "defects", "sample_size")
#' }
run_spc_u <- function(data, count, sample_size,
                     nelson_rules = 1:8, conf_level = 0.95,
                     theme = "academic", ...) {
  .spc_attributes_task(data, count, sample_size, "u",
    nelson_rules = nelson_rules, conf_level = conf_level,
    theme = theme, ...)
}

#' @title C Chart (Count of Defects)
#' @description Creates a C chart for monitoring the total count of
#'   nonconformities when sample sizes are equal.
#' @param data Data frame containing the count column.
#' @param count Count of defects (column name or numeric vector).
#' @param nelson_rules Nelson rule numbers enabled (default 1:8).
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrSpcTask` object with computed results.
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(defects = rpois(20, lambda = 5))
#' \donttest{
#' task <- run_spc_c(df, "defects")
#' }
run_spc_c <- function(data, count, nelson_rules = 1:8,
                     conf_level = 0.95, theme = "academic", ...) {
  .spc_attributes_task(data, count, sample_size = NULL, "c",
    nelson_rules = nelson_rules, conf_level = conf_level,
    theme = theme, ...)
}

#' @title Laney P' Chart
#' @description Creates a Laney P' chart that adjusts for over-dispersion in
#'   attribute data. Recommended by AIAG-VDA SPC (2026) when subgroup sizes are
#'   large and traditional P charts produce too many false alarms.
#' @inheritParams run_spc_p
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   defectives = rbinom(25, 500, 0.05),
#'   sample_size = rep(500, 25)
#' )
#' \donttest{
#' task <- run_spc_p_laney(df, "defectives", "sample_size")
#' }
run_spc_p_laney <- function(data, count, sample_size,
                            nelson_rules = 1:8, conf_level = 0.95,
                            theme = "academic", ...) {
  .spc_attributes_task(data, count, sample_size, "p_laney",
    nelson_rules = nelson_rules, conf_level = conf_level,
    theme = theme, ...)
}

#' @title Laney U' Chart
#' @description Creates a Laney U' chart that adjusts for over-dispersion in
#'   defect-rate data. Recommended when traditional U charts produce too many
#'   false alarms due to large sample sizes.
#' @inheritParams run_spc_p
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   defects = rpois(25, lambda = 30),
#'   sample_size = rep(100, 25)
#' )
#' \donttest{
#' task <- run_spc_u_laney(df, "defects", "sample_size")
#' }
run_spc_u_laney <- function(data, count, sample_size,
                            nelson_rules = 1:8, conf_level = 0.95,
                            theme = "academic", ...) {
  .spc_attributes_task(data, count, sample_size, "u_laney",
    nelson_rules = nelson_rules, conf_level = conf_level,
    theme = theme, ...)
}
