# =============================================================================
# File: R/plot_capability_multivariate.R
# Description: Multivariate process capability visualization functions for
#   the iQualityR ecosystem. Implements the bivariate spec ellipse, MCPV
#   three-component bar, marginal capability matrix, Hotelling T^2 control
#   chart, MCPV volume ratio diagram, joint PPM / HPCI summary bar, and the
#   6-panel composite. All functions are pure data-in / ggplot-out and resolve
#   every color through the IqrTheme toolbox (.iqr_aes).
#   NOTE: The HPCI radar chart was removed (industry consensus: non-standard,
#   low information density). HPCI is now visualized via plot_mcpv_bar (the
#   academic standard three-component horizontal bar, Taam 1993 / Shahriari
#   1995) plus the joint-PPM summary bar.
# =============================================================================

#' Bivariate Specification Ellipse Plot
#'
#' Draws a bivariate scatter plot with the specification rectangle, the 99.73%
#' prediction ellipse, the process mean point, and the target point. The core
#' visualization for bivariate multivariate capability analysis.
#'
#' @param X data.frame or matrix with 2 columns (the two CTQs).
#' @param specs data.frame with columns LSL, USL, Target (2 rows, one per CTQ).
#' @param ctq_names Character vector of length 2, CTQ names for axis labels.
#' @param mean_vec Numeric vector of length 2, process mean (optional; if NULL, colMeans(X) is used).
#' @param target_vec Optional numeric vector of length 2, target point.
#' @param level Numeric, confidence level for ellipse. Default 0.9973.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_spec_ellipse <- function(X, specs, ctq_names, mean_vec = NULL,
                              target_vec = NULL, level = 0.9973,
                              theme = NULL) {
  c <- .iqr_aes(theme)
  X <- as.data.frame(X)
  names(X) <- ctq_names
  if (is.null(mean_vec)) {
    mean_vec <- colMeans(X)
  }
  p <- base_plot(X, ggplot2::aes(x = .data[[ctq_names[1]]],
                                 y = .data[[ctq_names[2]]]),
                 theme = theme) +
    ggplot2::geom_point(color = c$muted, alpha = 0.5, size = 1.8) +
    ggplot2::stat_ellipse(level = level, type = "norm",
                         color = c$primary, linewidth = 1.0) +
    ggplot2::annotate("rect",
                      xmin = specs$LSL[1], xmax = specs$USL[1],
                      ymin = specs$LSL[2], ymax = specs$USL[2],
                      fill = NA, color = c$fail, linetype = "dashed",
                      linewidth = 0.8) +
    ggplot2::annotate("point", x = mean_vec[1], y = mean_vec[2],
                      color = c$primary, size = 4, shape = 21,
                      stroke = 1.5, fill = c$surface)
  if (!is.null(target_vec)) {
    p <- p + ggplot2::annotate("point", x = target_vec[1], y = target_vec[2],
                               color = c$success, size = 3.5, shape = 4,
                               stroke = 1.5)
  }
  p + ggplot2::labs(title = "Bivariate Specification Ellipse",
                    subtitle = sprintf("99.73%% prediction ellipse vs spec region"),
                    x = ctq_names[1], y = ctq_names[2]) +
    ggplot2::coord_fixed()
}

#' HPCI Three-Component Bar Chart (Shahriari 1995)
#'
#' Draws the academic-standard horizontal three-component bar chart for the
#' HPCI (Shahriari-Hubele-Lawrence 1995) vector: npc (volume ratio),
#' pv (Hotelling T^2 centering p-value), and lri (location ratio index).
#' Each component is drawn as a horizontal bar with a threshold marker and
#' semantic coloring (pass/watch/fail). This is the industry-standard
#' visualization for multivariate capability vectors (Taam 1993 /
#' Shahriari 1995), replacing the previously used low-information radar
#' chart.
#'
#' @param npc Numeric scalar, volume ratio component (>=0). Larger is better;
#'   pass when `npc >= npc_threshold`.
#' @param pv Numeric scalar, Hotelling T^2 p-value for centering (0-1). Larger
#'   is better (p large => fail to reject H0 => center on target); pass when
#'   `pv >= pv_threshold`.
#' @param lri Numeric scalar, location ratio index (0-1). Larger is better;
#'   pass when `lri >= lri_threshold`.
#' @param npc_threshold Numeric, pass threshold for npc. Default 1.0.
#' @param pv_threshold Numeric, pass threshold for pv. Default 0.05.
#' @param lri_threshold Numeric, pass threshold for lri. Default 0.9.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_mcpv_bar <- function(npc, pv, lri,
                          npc_threshold = 1.0, pv_threshold = 0.05,
                          lri_threshold = 0.9, theme = NULL) {
  c <- .iqr_aes(theme)

  # Per-component pass/fail (all three: larger = better)
  status <- c(
    if (is.na(npc) || !is.finite(npc) || npc < npc_threshold) "fail" else "pass",
    if (is.na(pv)  || !is.finite(pv)  || pv  < pv_threshold)  "fail" else "pass",
    if (is.na(lri) || !is.finite(lri) || lri < lri_threshold) "fail" else "pass"
  )

  # Display values: npc can exceed 1; cap at 2 for display ratio.
  disp_npc <- if (is.na(npc) || !is.finite(npc)) 0 else min(npc, 2)
  disp_pv  <- if (is.na(pv)  || !is.finite(pv))  0 else pv
  disp_lri <- if (is.na(lri) || !is.finite(lri)) 0 else lri

  labels <- c("NPC (Volume)", "PV (Center)", "LRI (Location)")
  vals <- c(disp_npc, disp_pv, disp_lri)
  thresholds <- c(npc_threshold, pv_threshold, lri_threshold)
  raw_labels <- c(.fmt_spec(npc),
                  if (is.na(pv) || !is.finite(pv)) "NA" else sprintf("%.3f", pv),
                  .fmt_spec(lri))

  df <- data.frame(
    Component = factor(labels, levels = rev(labels)),
    Value     = vals,
    Threshold = thresholds,
    Status    = factor(status, levels = c("pass", "watch", "fail")),
    RawLabel  = raw_labels,
    stringsAsFactors = FALSE
  )

  # Use ggplot() directly to avoid base_plot auto-injecting a discrete fill
  # scale that would conflict with the semantic palette below.
  y_max <- max(c(vals, thresholds), na.rm = TRUE) * 1.25
  ggplot2::ggplot(df, ggplot2::aes(x = .data$Component, y = .data$Value,
                                   fill = .data$Status)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.6) +
    # Threshold markers: use geom_errorbar with ymin = ymax to draw a single
    # horizontal reference line per component.
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$Threshold,
                                        ymax = .data$Threshold),
                           width = 0.7, color = c$text,
                           linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$RawLabel),
                       hjust = -0.25, color = c$text, size = 3.3) +
    ggplot2::scale_fill_manual(values = c("pass" = c$success,
                                          "watch" = c$warning,
                                          "fail" = c$danger)) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
    ggplot2::labs(
      title = "HPCI Three-Component (Shahriari 1995)",
      subtitle = sprintf("Thresholds: NPC >= %s | PV >= %.2f | LRI >= %s",
                        .fmt_spec(npc_threshold), pv_threshold,
                        .fmt_spec(lri_threshold)),
      x = NULL, y = "Value (larger = better)") +
    as_iqr_theme(theme)
}

#' Marginal Capability Matrix
#'
#' Draws a horizontal bar chart of per-CTQ Cpk values with capability status
#' coloring (pass >= 1.33, watch 1.0-1.33, fail < 1.0).
#'
#' @param per_ctq data.frame with columns: CTQ (character), Cpk (numeric),
#'   LSL (numeric), USL (numeric), Target (numeric), Mean (numeric).
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_marginal_capability_matrix <- function(per_ctq, theme = NULL) {
  c <- .iqr_aes(theme)

  # Compute capability status: pass >= 1.33, watch 1.0-1.33, fail < 1.0
  status <- vapply(per_ctq$Cpk, function(v) {
    if (is.na(v) || !is.finite(v)) "fail"
    else if (v >= 1.33) "pass"
    else if (v >= 1.0) "watch"
    else "fail"
  }, character(1))
  per_ctq$Status <- factor(status, levels = c("pass", "watch", "fail"))

  # Sort by Cpk ascending so highest appears at top after coord_flip
  per_ctq <- per_ctq[order(per_ctq$Cpk), ]
  per_ctq$CTQ <- factor(per_ctq$CTQ, levels = per_ctq$CTQ)

  # Use ggplot() directly to avoid base_plot auto-injecting a fill scale
  # that would conflict with the manual semantic palette below.
  ggplot2::ggplot(per_ctq, ggplot2::aes(x = .data$CTQ, y = .data$Cpk,
                                        fill = .data$Status)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.6) +
    ggplot2::geom_hline(yintercept = c(1.0, 1.33), linetype = "dashed",
                        color = c$muted, linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$Cpk)),
                       hjust = -0.2, color = c$text, size = 3.2) +
    ggplot2::scale_fill_manual(values = c("pass" = c$success,
                                          "watch" = c$warning,
                                          "fail" = c$danger)) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Marginal Capability (per CTQ)",
                  subtitle = "Reference: 1.00 / 1.33",
                  x = NULL, y = "Cpk") +
    as_iqr_theme(theme)
}

#' Hotelling T^2 Control Chart
#'
#' Draws the multivariate Hotelling T^2 control chart: per-observation T^2
#' statistics with the upper control limit. Out-of-control points are
#' highlighted in red.
#'
#' @param t2_values Numeric vector of T^2 statistics per observation (in observation order).
#' @param ucl Numeric scalar, upper control limit (chi-square quantile).
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_hotelling_t2 <- function(t2_values, ucl, theme = NULL) {
  c <- .iqr_aes(theme)
  df <- data.frame(index = seq_along(t2_values), t2 = t2_values,
                   ooc = t2_values > ucl)
  base_plot(df, ggplot2::aes(x = .data$index, y = .data$t2), theme = theme) +
    ggplot2::geom_line(color = c$muted, linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(color = ifelse(.data$ooc, "out", "in")),
                       size = 1.8, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c("in" = c$data, "out" = c$danger)) +
    ggplot2::geom_hline(yintercept = ucl, color = c$fail,
                        linetype = "dashed", linewidth = 0.8) +
    ggplot2::labs(title = "Hotelling T^2 Chart",
                  subtitle = sprintf("UCL = %s (chi-square)", .fmt_spec(ucl)),
                  x = "Observation", y = expression(T^2))
}

#' MCPV Volume Ratio Diagram
#'
#' Draws a bar comparison of the specification region volume vs the process
#' prediction ellipsoid volume, with the MCPV ratio annotated.
#'
#' @param v_spec Numeric, specification region volume.
#' @param v_process Numeric, process ellipsoid volume.
#' @param mcpv_p Numeric, MCPV Cp-equivalent ratio (V_spec / V_process).
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_mcpv_volume <- function(v_spec, v_process, mcpv_p, theme = NULL) {
  c <- .iqr_aes(theme)
  df <- data.frame(Category = c("Spec Region", "Process Ellipsoid"),
                   Volume = c(v_spec, v_process))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$Category, y = .data$Volume,
                                   fill = .data$Category)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.5) +
    ggplot2::scale_fill_manual(values = c("Spec Region" = c$success,
                                          "Process Ellipsoid" = c$danger)) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$Volume)),
                       vjust = -0.5, color = c$text, size = 3.5) +
    ggplot2::geom_hline(yintercept = v_spec, color = c$muted,
                        linetype = "dotted", linewidth = 0.5) +
    ggplot2::labs(title = "MCPV Volume Ratio",
                  subtitle = sprintf("MCPV (Cp) = %s", .fmt_spec(mcpv_p)),
                  x = NULL, y = "Volume") +
    as_iqr_theme(theme)
}

#' Multivariate Capability Sixpack
#'
#' Assembles the 6-panel multivariate capability composite (Taam 1993 /
#' Shahriari 1995): spec ellipse, HPCI three-component bar, marginal
#' capability, Hotelling T^2, MCPV volume, and joint yield/PPM summary.
#'
#' @param panels Named list with elements: ellipse, mcpv_bar, marginal, t2,
#'   mcpv, joint_ppm (each a ggplot object).
#' @param title Character.
#' @param subtitle Character.
#' @param theme Theme spec.
#' @return A patchwork object.
#' @export
plot_multivariate_sixpack <- function(panels, title, subtitle, theme = NULL) {
  layout <- (panels$ellipse | panels$mcpv_bar) /
            (panels$marginal | panels$t2) /
            (panels$mcpv | panels$joint_ppm)
  layout + patchwork::plot_annotation(title = title, subtitle = subtitle,
                                       theme = as_iqr_theme(theme))
}

#' Joint Yield & PPM Summary Bar
#'
#' Draws the multivariate capability summary panel: a horizontal yield bar
#' (joint probability P(X in spec region) as a percentage) with the 99.73%
#' reference line, the expected PPM annotated, and the overall verdict
#' controlling the bar color. This replaces the previous monospaced text
#' summary card with an information-dense visual (industry consensus: text
#' panels lack visual quality; a yield bar with verdict coloring communicates
#' the same information at a glance).
#'
#' @param yield_prob Numeric scalar, joint probability P(X in spec region),
#'   between 0 and 1.
#' @param ppm_expected Numeric scalar, expected joint PPM (defects per million).
#' @param verdict Character, one of "pass", "watch", "fail". Controls bar color.
#' @param target_yield Numeric scalar, target yield threshold. Default 0.9973
#'   (corresponds to a 3-sigma process).
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_joint_ppm_bar <- function(yield_prob, ppm_expected, verdict,
                               target_yield = 0.9973, theme = NULL) {
  c <- .iqr_aes(theme)
  verdict_col <- switch(verdict,
                        pass = c$success,
                        watch = c$warning,
                        fail = c$danger,
                        c$muted)
  df <- data.frame(Metric = "Joint Yield", Value = yield_prob * 100)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$Metric, y = .data$Value)) +
    ggplot2::geom_col(fill = verdict_col, alpha = 0.75, width = 0.4) +
    ggplot2::geom_hline(yintercept = target_yield * 100,
                        linetype = "dashed", color = c$text,
                        linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.4f%%",
                                                    yield_prob * 100)),
                       hjust = -0.15, color = c$text, size = 3.5) +
    ggplot2::scale_y_continuous(limits = c(0, 100.5), expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Joint Yield & PPM",
      subtitle = sprintf("PPM = %s | Target yield = %.2f%% | %s",
                        formatC(ppm_expected, format = "f", digits = 0),
                        target_yield * 100, toupper(verdict)),
      x = NULL, y = "Yield (%)") +
    as_iqr_theme(theme)
}
