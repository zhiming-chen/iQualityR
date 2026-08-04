# =============================================================================
# File: R/plot_capability_last_observations.R
# Description: Last 25 Subgroups scatter plot — shows ALL observations from the
#   last 25 subgroups (e.g. 25 groups × 5 obs = 125 points), with points
#   jittered horizontally within each group. Overlays LSL/USL (red dashed) and
#   Target (green dotted) as horizontal reference lines. This is the Minitab-
#   standard "Last 25 Subgroups" panel of the Capability Sixpack.
# =============================================================================

#' Last 25 Subgroups — All Observations Scatter Plot
#'
#' Draws a scatter plot of all observations from the last 25 subgroups. Points
#' within each subgroup are jittered horizontally around the subgroup index to
#' avoid overlap. Overlays LSL/USL (red dashed) and Target (green dotted) as
#' horizontal reference lines. When fewer than 25 subgroups exist, all are
#' shown. For individual data (subgroup size 1), shows the last 25 observations.
#'
#' @param data A data.frame with columns:
#'   \itemize{
#'     \item \code{subgroup} — subgroup index (integer, 1..N)
#'     \item \code{value} — measurement (numeric)
#'   }
#'   Or, for individual data, a data.frame with columns \code{index} and
#'   \code{value}.
#' @param lsl Optional numeric. Lower spec limit.
#' @param usl Optional numeric. Upper spec limit.
#' @param target Optional numeric. Target value.
#' @param max_subgroups Integer. Maximum number of subgroups to show (default 25).
#' @param jitter_width Numeric. Width of horizontal jitter within subgroup (default 0.15).
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_last_observations <- function(data, lsl = NULL, usl = NULL,
                                              target = NULL, max_subgroups = 25,
                                              jitter_width = 0.15, theme = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }
  c <- .iqr_aes(theme)

  # Detect data shape: subgrouped (subgroup + value) vs individual (index + value)
  if ("subgroup" %in% names(data)) {
    subg_col <- "subgroup"
  } else if ("index" %in% names(data)) {
    subg_col <- "index"
  } else {
    stop("data must have 'subgroup' or 'index' column.")
  }
  if (!"value" %in% names(data)) {
    stop("data must have a 'value' column.")
  }

  # Keep only the last max_subgroups subgroups
  subgs <- sort(unique(data[[subg_col]]))
  if (length(subgs) > max_subgroups) {
    keep <- subgs[(length(subgs) - max_subgroups + 1):length(subgs)]
    data <- data[data[[subg_col]] %in% keep, , drop = FALSE]
    # Re-index subgroup numbers to 1..K for clean x-axis
    subgs <- sort(unique(data[[subg_col]]))
    remap <- setNames(seq_along(subgs), subgs)
    data[[subg_col]] <- remap[as.character(data[[subg_col]])]
  }

  n_groups <- length(unique(data[[subg_col]]))
  n_obs <- nrow(data)

  p <- base_plot(data, ggplot2::aes(x = .data[[subg_col]], y = .data$value),
                 theme = theme) +
    ggplot2::geom_jitter(width = jitter_width, height = 0,
                         color = c$primary, size = 1.2, alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = min(n_groups, 10)))

  # Spec limits (horizontal)
  if (!is.null(usl)) {
    p <- p + ggplot2::geom_hline(yintercept = usl, color = c$danger,
                                 linetype = "dashed", linewidth = 0.8)
  }
  if (!is.null(lsl)) {
    p <- p + ggplot2::geom_hline(yintercept = lsl, color = c$danger,
                                 linetype = "dashed", linewidth = 0.8)
  }
  if (!is.null(target)) {
    p <- p + ggplot2::geom_hline(yintercept = target, color = c$success,
                                 linetype = "dotted", linewidth = 0.8)
  }

  p + ggplot2::labs(
    title = "Last 25 Subgroups",
    subtitle = sprintf("%d subgroups | %d observations | Specs: LSL=%s, USL=%s, T=%s",
                       n_groups, n_obs,
                       .fmt_spec(lsl), .fmt_spec(usl), .fmt_spec(target)),
    x = "Subgroup", y = "Measurement"
  )
}
