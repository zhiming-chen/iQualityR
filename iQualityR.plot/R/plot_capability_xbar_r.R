# =============================================================================
# File: R/plot_capability_xbar_r.R
# Description: Xbar and R control charts for subgrouped capability analysis.
#   These are used by the Capability Sixpack when subgroup information is
#   available (subgroup_size > 1). When subgroup_size == 1, the sixpack falls
#   back to individual/moving-range charts (plot_capability_individual_chart /
#   plot_capability_moving_range).
#
#   Both functions use layers_control_chart() for CL/UCL/LCL lines + OOC
#   highlighting, with every colour resolved through .iqr_aes().
# =============================================================================

#' Capability Xbar Chart
#'
#' Draws the Panel-1 Xbar chart of the Capability Sixpack (subgrouped data):
#' subgroup means plotted over time, the grand-mean center line, and upper /
#' lower control limits computed from R-bar (UCL = X-bar-bar + A2 * R-bar,
#' LCL = X-bar-bar - A2 * R-bar). Out-of-control subgroups are highlighted.
#'
#' @param data A data.frame with columns:
#'   \itemize{
#'     \item \code{index} — subgroup index (numeric)
#'     \item \code{value} — subgroup mean (numeric)
#'     \item \code{cl} — center line / grand mean (numeric)
#'     \item \code{ucl} — upper control limit (numeric)
#'     \item \code{lcl} — lower control limit (numeric)
#'     \item \code{ooc} — logical, out-of-control flag
#'   }
#' @param specs Optional list with \code{lsl} and/or \code{usl}. When supplied,
#'   horizontal spec lines are drawn via \code{layers_spec_limits(orientation="h")}.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_xbar_chart <- function(data, specs = NULL, theme = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }
  c <- .iqr_aes(theme)

  # layers_control_chart expects columns: x, y, cl, ucl, lcl
  ctrl_df <- data.frame(
    x   = data$index,
    y   = data$value,
    cl  = data$cl,
    ucl = data$ucl,
    lcl = if (is.null(data$lcl) || all(is.na(data$lcl))) {
      rep(NA_real_, nrow(data))
    } else {
      data$lcl
    }
  )

  p <- base_plot(data, ggplot2::aes(x = .data$index, y = .data$value),
                 theme = theme) +
    layers_control_chart(data = ctrl_df, theme = theme) +
    # Override data points with OOC highlighting
    ggplot2::geom_point(
      ggplot2::aes(color = ifelse(data$ooc, "out", "in")),
      size = 1.8, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = c("in" = c$neutral, "out" = c$fail))

  if (!is.null(specs)) {
    p <- p + layers_spec_limits(lsl = specs$lsl, usl = specs$usl,
                                theme = theme, orientation = "h")
  }

  p + ggplot2::labs(
    title = "Xbar Chart",
    subtitle = sprintf("CL = %s | UCL = %s | LCL = %s",
                       .fmt_spec(data$cl[1]),
                       .fmt_spec(data$ucl[1]),
                       .fmt_spec(data$lcl[1])),
    x = "Subgroup", y = "Subgroup Mean"
  )
}

#' Capability R Chart
#'
#' Draws the Panel-2 R chart of the Capability Sixpack (subgrouped data):
#' subgroup ranges plotted over time, the R-bar center line, and upper /
#' lower control limits (UCL = D4 * R-bar, LCL = D3 * R-bar). Out-of-control
#' subgroups are highlighted.
#'
#' @param data A data.frame with columns:
#'   \itemize{
#'     \item \code{index} — subgroup index
#'     \item \code{value} — subgroup range (numeric)
#'     \item \code{cl} — center line / R-bar (numeric)
#'     \item \code{ucl} — upper control limit (numeric)
#'     \item \code{lcl} — lower control limit (numeric, may be NA for n<=6)
#'     \item \code{ooc} — logical, out-of-control flag
#'   }
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_r_chart <- function(data, theme = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }
  c <- .iqr_aes(theme)

  ctrl_df <- data.frame(
    x   = data$index,
    y   = data$value,
    cl  = data$cl,
    ucl = data$ucl,
    lcl = if (is.null(data$lcl) || all(is.na(data$lcl))) {
      rep(NA_real_, nrow(data))
    } else {
      data$lcl
    }
  )

  base_plot(data, ggplot2::aes(x = .data$index, y = .data$value),
            theme = theme) +
    layers_control_chart(data = ctrl_df, theme = theme) +
    ggplot2::geom_point(
      ggplot2::aes(color = ifelse(data$ooc, "out", "in")),
      size = 1.8, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = c("in" = c$neutral, "out" = c$fail)) +
    ggplot2::labs(
      title = "R Chart",
      subtitle = sprintf("R-bar = %s | UCL = %s | LCL = %s",
                         .fmt_spec(data$cl[1]),
                         .fmt_spec(data$ucl[1]),
                         .fmt_spec(data$lcl[1])),
      x = "Subgroup", y = "Subgroup Range"
    )
}
