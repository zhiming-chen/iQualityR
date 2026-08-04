# =============================================================================
# File: R/plot_capability_goalplot.R
# Description: Multi-process capability comparison functions for the iQualityR
#   ecosystem. Implements the JMP-standard Goal Plot (mean shift vs process
#   spread scatter with goal box) and the Process Capability Snapshot (faceted
#   mini-histograms with spec limits and Cpk annotation). These are the two
#   JMP "highlights" for simultaneously assessing many CTQs on a single view.
#   All functions are pure data-in / ggplot-out and resolve every color
#   through the IqrTheme toolbox (.iqr_aes).
# =============================================================================

#' Goal Plot (JMP-style)
#'
#' Draws the JMP-standard Goal Plot for comparing many processes on a single
#' view. Each point is one process (CTQ). The X-axis is the mean shift from
#' target as a fraction of the half-tolerance (0 = on target, 1 = mean at a
#' spec limit). The Y-axis is the process spread (3*sigma) as a fraction of
#' the half-tolerance (0 = no variation, 1 = 3sigma reaches the spec limit,
#' i.e. Cp = 1). The lower-left "goal box" (shift <= `shift_limit` AND
#' spread <= `spread_limit`) marks capable processes.
#'
#' This is the academic / JMP industry standard for multi-CTQ capability
#' triage: it separates "off-target" problems (high X) from "too much
#' variation" problems (high Y) at a glance, which the usual Cpk bar cannot.
#'
#' @param processes data.frame with one row per process and columns:
#'   `name` (character), `mean` (numeric), `sd` (numeric, overall sigma),
#'   `target` (numeric), `lsl` (numeric), `usl` (numeric).
#' @param shift_limit Numeric scalar, goal-box threshold on the shift axis
#'   (fraction of half-tolerance). Default 0.5 (mean within a quarter of the
#'   spec width from target, i.e. |mean - target| <= 0.25 * (USL - LSL)).
#' @param spread_limit Numeric scalar, goal-box threshold on the spread axis
#'   (fraction of half-tolerance). Default 0.5 (3sigma <= 0.25 * (USL - LSL),
#'   i.e. Cp >= 2).
#' @param label_points Logical; if TRUE, label each point with its process
#'   name. Default TRUE.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_goal_plot <- function(processes, shift_limit = 0.5, spread_limit = 0.5,
                           label_points = TRUE, theme = NULL) {
  c <- .iqr_aes(theme)

  # Tolerance = half the spec width; normalize both axes to it so that
  #   shift = 1  <=> mean sits exactly on a spec limit
  #   spread = 1 <=> 3*sigma just reaches a spec limit (Cp = 1)
  half_tol <- (processes$usl - processes$lsl) / 2
  shift <- abs(processes$mean - processes$target) / half_tol
  spread <- (3 * processes$sd) / half_tol

  # Overall verdict: pass only when BOTH shift and spread are inside the box.
  verdict <- vapply(seq_len(nrow(processes)), function(i) {
    if (is.na(shift[i]) || is.na(spread[i]) ||
        !is.finite(shift[i]) || !is.finite(spread[i])) {
      "fail"
    } else if (shift[i] <= shift_limit && spread[i] <= spread_limit) {
      "pass"
    } else if (shift[i] <= 1 && spread[i] <= 1) {
      "watch"
    } else {
      "fail"
    }
  }, character(1))

  df <- data.frame(
    name = processes$name,
    shift = shift,
    spread = spread,
    verdict = factor(verdict, levels = c("pass", "watch", "fail"))
  )

  # Axis upper bound: leave headroom for labels and the goal box.
  xy_max <- max(1.0, max(c(shift, spread), na.rm = TRUE) * 1.15, na.rm = TRUE)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$shift, y = .data$spread)) +
    # Goal box (lower-left rectangle).
    ggplot2::annotate("rect", xmin = 0, xmax = shift_limit,
                      ymin = 0, ymax = spread_limit,
                      fill = c$success, alpha = 0.12,
                      color = c$success, linetype = "dotted", linewidth = 0.6) +
    # Cp = 1 reference line (spread = 1): the hard capability frontier.
    ggplot2::geom_vline(xintercept = 1, color = c$muted,
                        linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = 1, color = c$muted,
                        linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(color = .data$verdict),
                       size = 3.2, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c("pass" = c$success,
                                           "watch" = c$warning,
                                           "fail" = c$danger))

  if (label_points && nrow(df) > 0) {
    # Prefer ggrepel for non-overlapping labels when available; fall back to
    # a plain geom_text nudge so we do not add a hard dependency.
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(
        ggplot2::aes(label = .data$name), color = c$text, size = 3.0,
        max.overlaps = 30, segment.color = c$muted, segment.size = 0.3
      )
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = .data$name), color = c$text, size = 3.0,
        vjust = -1.2, hjust = 0.2
      )
    }
  }

  p + ggplot2::labs(
    title = "Goal Plot (multi-process)",
    subtitle = sprintf("Goal box: shift <= %s & spread <= %s | lines @1 = Cp 1.0 frontier",
                       .fmt_spec(shift_limit), .fmt_spec(spread_limit)),
    x = "Mean shift (fraction of half-tolerance)",
    y = "Process spread 3*sigma (fraction of half-tolerance)",
    caption = "Lower-left = capable | High X = off-target | High Y = too much variation"
  ) +
    ggplot2::coord_equal(xlim = c(0, xy_max), ylim = c(0, xy_max), expand = FALSE) +
    as_iqr_theme(theme)
}


#' Process Capability Snapshot (JMP-style)
#'
#' Draws a faceted small-multiples overview of many processes: one compact
#' panel per CTQ showing a density histogram with vertical spec-limit lines
#' and the target, plus the per-process Cpk annotated in the strip header.
#' This is the JMP "Process Capability Snapshot" — a Tufte-style dashboard for
#' spotting the worst CTQs at a glance without paging through individual
#' sixpacks.
#'
#' @param values_list List of numeric vectors, one per process (the raw
#'   measurements).
#' @param processes data.frame with one row per process and columns:
#'   `name` (character), `target` (numeric), `lsl` (numeric),
#'   `usl` (numeric), `cpk` (numeric, optional — when missing it is
#'   omitted from the strip header).
#' @param bins Integer, histogram bin count. Default 20.
#' @param ncol Integer, number of columns in the facet grid. Default 3.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_snapshot <- function(values_list, processes, bins = 20,
                                      ncol = 3, theme = NULL) {
  c <- .iqr_aes(theme)
  n <- length(values_list)
  if (n != nrow(processes)) {
    stop("length(values_list) must equal nrow(processes).", call. = FALSE)
  }

  # Build a long data.frame: each row is an observation tagged with its
  # process name (used as the faceting variable).
  rows <- vector("list", n)
  for (i in seq_len(n)) {
    v <- values_list[[i]]
    v <- v[is.finite(v)]
    cpk_str <- if (!is.null(processes$cpk) && !is.na(processes$cpk[i]) &&
                 is.finite(processes$cpk[i])) {
      sprintf(" | Cpk = %s", .fmt_spec(processes$cpk[i]))
    } else ""
    facet_label <- sprintf("%s%s", processes$name[i], cpk_str)
    rows[[i]] <- data.frame(value = v, process = facet_label,
                            stringsAsFactors = FALSE)
  }
  df <- do.call(rbind, rows)
  df$process <- factor(df$process, levels = vapply(rows, function(r) r$process[1],
                                                  character(1)))

  # Spec / target annotations as a separate data frame (one row per facet).
  ref_df <- data.frame(
    process = vapply(rows, function(r) r$process[1], character(1)),
    lsl = processes$lsl, usl = processes$usl, target = processes$target
  )

  # Cpk-based strip header coloring is not portable across facets with a
  # single fill scale; instead, we encode the verdict in the facet label text
  # and keep the spec lines colored by role.
  ggplot2::ggplot(df, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = bins, fill = c$surface_soft,
                            color = c$border, linewidth = 0.3) +
    ggplot2::geom_vline(data = ref_df,
                        ggplot2::aes(xintercept = .data$lsl),
                        color = c$danger, linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_vline(data = ref_df,
                        ggplot2::aes(xintercept = .data$usl),
                        color = c$danger, linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_vline(data = ref_df,
                        ggplot2::aes(xintercept = .data$target),
                        color = c$success, linetype = "dotted", linewidth = 0.6) +
    ggplot2::facet_wrap(~ .data$process, ncol = ncol, scales = "free") +
    ggplot2::labs(
      title = "Process Capability Snapshot",
      subtitle = "Per-CTQ density histogram with LSL/USL (dashed) and target (dotted)",
      x = "Measurement", y = "Density"
    ) +
    as_iqr_theme(theme)
}
