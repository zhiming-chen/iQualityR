# =============================================================================
# File: R/spc_phase.R
# Description: Phase / stage analysis helper functions
# =============================================================================

#' @title SPC Phase Analysis
#' @description
#' Performs phase (stage) analysis on a control chart. The data is split at
#' specified boundaries and control limits are computed separately for each
#' phase. Useful for assessing whether a process improvement has shifted the
#' process mean or reduced variability.
#'
#' @param data Data frame containing the measurement column.
#' @param measurement Measurement column name.
#' @param chart_type Chart type string (default `"imr"`).
#' @param phase_boundaries Integer vector of indices where phases start.
#'   For example, `c(15, 30)` splits data into phases `1-14`, `15-29`, `30-n`.
#' @param sigma_method Sigma method (default NULL uses chart-specific default).
#' @param subgroup_size Subgroup size (default NULL uses chart-specific default).
#' @param nelson_rules Nelson rules enabled (default 1:8).
#' @param conf_level Confidence level (default 0.95).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments passed to `SpcPlan`.
#'
#' @return A list of `IqrSpcTask` objects, one per phase.
#' @export
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(measurement = c(rnorm(20, 100, 1), rnorm(15, 102, 1)))
#' \donttest{
#' phases <- run_spc_phase(df, "measurement", "imr",
#'   phase_boundaries = c(21))
#' length(phases)  # 2 phases
#' }
run_spc_phase <- function(data, measurement, chart_type = "imr",
                          phase_boundaries, sigma_method = NULL,
                          subgroup_size = NULL, nelson_rules = 1:8,
                          conf_level = 0.95, theme = "academic", ...) {
  if (!is.data.frame(data)) stop("data must be a data.frame", call. = FALSE)
  if (is.null(measurement) || !measurement %in% names(data)) {
    stop("measurement column not found in data", call. = FALSE)
  }
  if (!is.numeric(data[[measurement]])) {
    stop("measurement column must be numeric", call. = FALSE)
  }
  if (missing(phase_boundaries) || !is.numeric(phase_boundaries)) {
    stop("phase_boundaries must be an integer vector.", call. = FALSE)
  }
  phase_boundaries <- as.integer(phase_boundaries)
  if (any(phase_boundaries < 1) || any(phase_boundaries > nrow(data))) {
    stop("phase_boundaries must be in [1, nrow(data)].", call. = FALSE)
  }

  # Construct phase index ranges
  n <- nrow(data)
  starts <- c(1L, phase_boundaries)
  ends <- c(phase_boundaries - 1L, n)
  keep <- ends >= starts
  starts <- starts[keep]; ends <- ends[keep]

  # Build a plan per phase and compute
  tasks <- lapply(seq_along(starts), function(i) {
    s <- starts[i]; e <- ends[i]
    phase_data <- data[s:e, , drop = FALSE]
    plan <- SpcPlan$new(
      chart_type = chart_type,
      sigma_method = sigma_method,
      subgroup_size = subgroup_size,
      nelson_rules = nelson_rules,
      phase = if (i == 1) "phase1" else "phase2",
      phase_boundaries = NULL,
      conf_level = conf_level,
      task_tag = "spc",
      ...
    )
    task <- IqrSpcTask$new(data = phase_data, measurement = measurement,
                           plan = plan, theme = theme)
    task$compute()
    task
  })

  names(tasks) <- paste0("phase_", seq_along(tasks))
  structure(tasks,
    class = c("spc_phase_list", "list"),
    phase_boundaries = phase_boundaries,
    n_phases = length(tasks))
}

#' @title Print SPC Phase Analysis Result
#' @description Prints a compact summary of an SPC phase analysis returned by
#'   `run_spc_phase`.
#' @param x An object of class `spc_phase_list`.
#' @param ... Additional arguments (ignored).
#' @export
print.spc_phase_list <- function(x, ...) {
  cat("\n========== SPC Phase Analysis Summary ==========\n")
  cat(sprintf("Number of phases: %d\n", length(x)))
  cat(sprintf("Phase boundaries:  %s\n",
    paste(attr(x, "phase_boundaries"), collapse = ", ")))
  for (i in seq_along(x)) {
    task <- x[[i]]
    r <- task$results
    if (is.null(r)) next
    cat(sprintf("\n--- Phase %d ---\n", i))
    cat(sprintf("  Chart type:  %s\n", r$statistics$chart_type))
    cat(sprintf("  Sample size: %d\n", r$statistics$n_points))
    cat(sprintf("  Center:      %.4f\n", r$statistics$center))
    cat(sprintf("  Sigma:       %.4f\n", r$statistics$sigma))
    cat(sprintf("  UCL:         %.4f\n", r$statistics$ucl))
    cat(sprintf("  LCL:         %.4f\n", r$statistics$lcl))
    cat(sprintf("  Violations:  %d (%s)\n",
      r$statistics$n_violations,
      if (isTRUE(r$statistics$is_in_control)) "in control" else "out of control"))
  }
  cat("===============================================\n")
  invisible(x)
}
