# =============================================================================
# File: R/plot_capability_attribute.R
# Description: Attribute capability analysis (binomial/poisson) visualization
#   functions for the iQualityR ecosystem. Implements the attribute control
#   chart (P/U chart), rate histogram, cumulative rate with 95% CI band,
#   defects bar, binomial/poisson distribution fit plot, observed-vs-expected
#   PPM performance bar, and the 6-panel composite. All functions are pure
#   data-in / ggplot-out and resolve every color through the IqrTheme toolbox
#   (.iqr_aes).
#
#   NOTE: The previous sigma-level gauge (plot_sigma_gauge) and the monospaced
#   text summary card (plot_attribute_summary_table) were removed (industry
#   consensus: decorative gauge carries no information; text panel lacks
#   visual quality). The Z.Bench / sigma-level information is now surfaced
#   as numeric annotations on plot_attribute_performance_bar.
# =============================================================================

#' Attribute Control Chart (P-chart / U-chart)
#'
#' Draws the attribute control chart panel: per-subgroup rate with center line,
#' variable control limits, and out-of-control points highlighted.
#'
#' @param points data.frame with columns: index (integer), value (numeric, rate),
#'   cl (numeric, center line), ucl (numeric), lcl (numeric), ooc (logical).
#' @param rate_name Character, the y-axis label (e.g. "Proportion defective" or "Defects per unit").
#' @param target Optional numeric, target rate/proportion. Draws a dotted line.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_attribute_control_chart <- function(points, rate_name, target = NULL,
                                         theme = NULL) {
  c <- .iqr_aes(theme)
  p <- base_plot(points, ggplot2::aes(x = .data$index, y = .data$value),
                 theme = theme) +
    layers_control_chart(data = data.frame(x = points$index, y = points$value,
                                           cl = points$cl, ucl = points$ucl,
                                           lcl = points$lcl), theme = theme) +
    ggplot2::geom_point(ggplot2::aes(color = ifelse(points$ooc, "out", "in")),
                        size = 1.8, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c("in" = c$muted, "out" = c$fail))
  if (!is.null(target)) {
    p <- p + ggplot2::geom_hline(yintercept = target, color = c$warning,
                                 linetype = "dotted", linewidth = 0.8)
  }
  p + ggplot2::labs(title = "Attribute Control Chart",
                    subtitle = sprintf("CL = %s | UCL = %s",
                                       .fmt_spec(points$cl[1]),
                                       .fmt_spec(points$ucl[1])),
                    x = "Subgroup", y = rate_name)
}

#' Attribute Rate Distribution Histogram
#'
#' Draws the rate distribution histogram across subgroups with the estimated
#' rate (solid) and 95% CI bounds (dashed).
#'
#' @param values Numeric vector of per-subgroup rates.
#' @param rate Numeric scalar, the estimated rate (p-bar or u-bar).
#' @param rate_lower Numeric scalar, CI lower bound.
#' @param rate_upper Numeric scalar, CI upper bound.
#' @param rate_name Character, x-axis label.
#' @param target Optional numeric, target rate.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_attribute_rate_histogram <- function(values, rate, rate_lower, rate_upper,
                                          rate_name, target = NULL,
                                          theme = NULL) {
  c <- .iqr_aes(theme)
  p <- base_plot(data.frame(rate = values), ggplot2::aes(x = .data$rate),
                 theme = theme) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = min(15, max(5, floor(length(values) / 3))),
      fill = c$surface_soft, color = c$border
    ) +
    ggplot2::geom_vline(xintercept = rate, color = c$primary, linewidth = 1) +
    ggplot2::geom_vline(xintercept = c(rate_lower, rate_upper),
                        color = c$muted, linetype = "dashed", linewidth = 0.8)
  if (!is.null(target)) {
    p <- p + ggplot2::geom_vline(xintercept = target, color = c$warning,
                                 linetype = "dotted", linewidth = 0.8)
  }
  p + ggplot2::labs(title = "Rate Distribution",
                    subtitle = sprintf("Rate = %s (95%% CI: %s, %s)",
                                       .fmt_spec(rate),
                                       .fmt_spec(rate_lower),
                                       .fmt_spec(rate_upper)),
                    x = rate_name, y = "Density")
}

#' Attribute Defects Bar Chart
#'
#' Draws per-subgroup defect/defective counts as a bar chart, with out-of-control
#' subgroups highlighted in red.
#'
#' @param points data.frame with columns: index (integer), defects (integer), ooc (logical).
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_attribute_defects_bar <- function(points, theme = NULL) {
  c <- .iqr_aes(theme)
  df <- data.frame(index = points$index, defects = points$defects,
                   Status = ifelse(points$ooc, "out", "in"))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$index, y = .data$defects,
                                   fill = .data$Status)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.7) +
    ggplot2::scale_fill_manual(values = c("in" = c$primary, "out" = c$fail)) +
    ggplot2::labs(title = "Defects per Subgroup",
                  subtitle = "Red bars: out of control",
                  x = "Subgroup", y = "Count") +
    as_iqr_theme(theme)
}

#' Attribute Cumulative Rate Plot with 95% CI Band
#'
#' Draws the cumulative rate convergence plot: cumulative defect rate across
#' subgroups with the overall rate line and a 95% confidence-interval band
#' (shaded). Convergence of the cumulative rate into the CI band indicates
#' sufficient sample size — this is the Minitab-standard "Cumulative %defective"
#' diagnostic panel for attribute capability.
#'
#' @param points data.frame with columns: index, defects, n (sample size/exposure).
#' @param rate Numeric scalar, overall estimated rate.
#' @param rate_lower Numeric scalar, CI lower bound.
#' @param rate_upper Numeric scalar, CI upper bound.
#' @param rate_name Character, y-axis label.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_attribute_cumulative <- function(points, rate, rate_lower, rate_upper,
                                      rate_name, theme = NULL) {
  c <- .iqr_aes(theme)
  cum_defects <- cumsum(points$defects)
  cum_n <- cumsum(points$n)
  cum_rate <- cum_defects / cum_n
  df <- data.frame(index = points$index, cum_rate = cum_rate)
  base_plot(df, ggplot2::aes(x = .data$index, y = .data$cum_rate),
            theme = theme) +
    # 95% CI band (shaded region between rate_lower and rate_upper)
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                      ymin = rate_lower, ymax = rate_upper,
                      fill = c$muted, alpha = 0.15) +
    ggplot2::geom_line(color = c$muted, linewidth = 0.5) +
    ggplot2::geom_point(color = c$data, size = 1.5) +
    ggplot2::geom_hline(yintercept = rate, color = c$primary,
                        linetype = "solid", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = c(rate_lower, rate_upper),
                        color = c$warning, linetype = "dashed",
                        linewidth = 0.6) +
    ggplot2::labs(title = "Cumulative Rate",
                  subtitle = "Shaded band: 95% CI | Convergence indicates sufficient sample size",
                  x = "Subgroup", y = rate_name)
}

#' Binomial Distribution Fit Plot
#'
#' Draws the per-subgroup proportion-defective histogram with the fitted
#' binomial probability mass function overlaid (based on the average sample
#' size and the estimated p-hat). This is the Minitab-standard "Binomial Plot"
#' panel for binomial capability analysis, replacing the previous decorative
#' sigma-level gauge.
#'
#' @param defects Numeric vector of per-subgroup defective counts.
#' @param sample_sizes Numeric vector of per-subgroup sample sizes.
#' @param p_hat Numeric scalar, the estimated proportion defective (p-bar).
#' @param target Optional numeric, target proportion. Draws a dotted line.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_attribute_binomial_fit <- function(defects, sample_sizes, p_hat,
                                        target = NULL, theme = NULL) {
  c <- .iqr_aes(theme)
  rates <- defects / sample_sizes
  n_avg <- round(mean(sample_sizes, na.rm = TRUE))
  # Binomial PMF as rate density: dbinom(x, n_avg, p_hat) converted to
  # density per rate unit (multiply by n_avg since rate = x / n_avg).
  x_max <- max(defects, na.rm = TRUE)
  x_seq <- 0:x_max
  pmf <- stats::dbinom(x_seq, n_avg, p_hat)
  rate_seq <- x_seq / n_avg
  dens_seq <- pmf * n_avg
  fit_df <- data.frame(rate = rate_seq, density = dens_seq)
  df <- data.frame(rate = rates)
  p <- base_plot(df, ggplot2::aes(x = .data$rate), theme = theme) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = min(15, max(5, floor(length(rates) / 3))),
      fill = c$surface_soft, color = c$border
    ) +
    ggplot2::geom_line(data = fit_df,
                       ggplot2::aes(x = .data$rate, y = .data$density),
                       color = c$primary, linewidth = 1.0,
                       inherit.aes = FALSE) +
    ggplot2::geom_vline(xintercept = p_hat, color = c$primary,
                        linetype = "solid", linewidth = 0.8)
  if (!is.null(target)) {
    p <- p + ggplot2::geom_vline(xintercept = target, color = c$warning,
                                 linetype = "dotted", linewidth = 0.8)
  }
  p + ggplot2::labs(title = "Binomial Fit",
                    subtitle = sprintf("p-hat = %s | n_avg = %d",
                                       .fmt_spec(p_hat), n_avg),
                    x = "Proportion defective", y = "Density")
}

#' Poisson Distribution Fit Plot
#'
#' Draws the per-subgroup defect-count histogram with the fitted Poisson
#' probability mass function overlaid (based on the expected per-sample
#' defect count lambda). This is the Minitab-standard "Poisson Plot" panel
#' for Poisson capability analysis.
#'
#' @param defects Numeric vector of per-subgroup defect counts.
#' @param lambda Numeric scalar, expected defects per sample (= u-bar * mean
#'   sample size / exposure).
#' @param u_bar Numeric scalar, the estimated defects-per-unit rate (u-bar).
#'   Drawn as a reference line at the per-unit rate when supplied.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_attribute_poisson_fit <- function(defects, lambda, u_bar = NULL,
                                       theme = NULL) {
  c <- .iqr_aes(theme)
  x_max <- max(defects, na.rm = TRUE)
  x_seq <- 0:x_max
  pmf <- stats::dpois(x_seq, lambda)
  fit_df <- data.frame(x = x_seq, density = pmf)
  df <- data.frame(defects = defects)
  p <- base_plot(df, ggplot2::aes(x = .data$defects), theme = theme) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      binwidth = 1, fill = c$surface_soft, color = c$border
    ) +
    ggplot2::geom_line(data = fit_df,
                       ggplot2::aes(x = .data$x, y = .data$density),
                       color = c$primary, linewidth = 1.0,
                       inherit.aes = FALSE) +
    ggplot2::geom_vline(xintercept = lambda, color = c$primary,
                        linetype = "solid", linewidth = 0.8)
  if (!is.null(u_bar) && is.finite(u_bar)) {
    p <- p + ggplot2::geom_vline(xintercept = u_bar, color = c$warning,
                                 linetype = "dotted", linewidth = 0.8)
  }
  p + ggplot2::labs(title = "Poisson Fit",
                    subtitle = sprintf("lambda = %s | u-bar = %s",
                                       .fmt_spec(lambda), .fmt_spec(u_bar)),
                    x = "Defects per subgroup", y = "Density")
}

#' Attribute Performance Bar (Observed vs Expected PPM)
#'
#' Draws a two-bar comparison of Observed PPM (empirical defect rate scaled to
#' parts-per-million) versus Expected PPM (the model-based expected defect rate
#' under the fitted binomial/poisson distribution). Z.Bench and sigma-level
#' are annotated as subtitle text — replacing the previous decorative
#' sigma-level gauge with information-dense numeric reporting (industry
#' consensus: gauges carry no information; numeric values with context do).
#'
#' @param observed_ppm Numeric scalar, observed defect rate as PPM.
#' @param expected_ppm Numeric scalar, model-expected defect rate as PPM.
#' @param z_bench Optional numeric scalar, Z.Bench value for annotation.
#' @param sigma_level Optional numeric scalar, sigma level for annotation.
#' @param target_ppm Optional numeric scalar, target PPM threshold line.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_attribute_performance_bar <- function(observed_ppm, expected_ppm,
                                           z_bench = NULL, sigma_level = NULL,
                                           target_ppm = NULL, theme = NULL) {
  c <- .iqr_aes(theme)
  df <- data.frame(
    Category = factor(c("Observed", "Expected"),
                      levels = c("Observed", "Expected")),
    PPM = c(observed_ppm, expected_ppm)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Category, y = .data$PPM,
                                        fill = .data$Category)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = formatC(.data$PPM, format = "f",
                                                    digits = 0)),
                       vjust = -0.5, color = c$text, size = 3.3) +
    ggplot2::scale_fill_manual(values = c("Observed" = c$muted,
                                          "Expected" = c$primary))
  if (!is.null(target_ppm) && is.finite(target_ppm)) {
    p <- p + ggplot2::geom_hline(yintercept = target_ppm, color = c$warning,
                                 linetype = "dashed", linewidth = 0.8)
  }
  subtitle <- "Observed vs Expected PPM"
  if (!is.null(z_bench) && is.finite(z_bench)) {
    subtitle <- paste0(subtitle, " | Z.Bench = ", .fmt_spec(z_bench))
  }
  if (!is.null(sigma_level) && is.finite(sigma_level)) {
    subtitle <- paste0(subtitle, " | Sigma = ", sprintf("%.2f", sigma_level))
  }
  p + ggplot2::labs(title = "Performance (PPM)",
                    subtitle = subtitle,
                    x = NULL, y = "PPM (parts per million)") +
    as_iqr_theme(theme)
}

#' Attribute Capability Sixpack
#'
#' Assembles the 6-panel attribute capability composite following the
#' Minitab Binomial/Poisson Capability layout:
#' Row 1: control chart | rate distribution histogram
#' Row 2: cumulative rate (with 95% CI band) | defects per subgroup bar
#' Row 3: distribution fit plot (binomial/poisson) | performance bar (PPM)
#'
#' @param panels Named list with elements: control, rate, cumulative,
#'   defects, fit, performance (each a ggplot object).
#' @param title Character.
#' @param subtitle Character.
#' @param theme Theme spec.
#' @return A patchwork object.
#' @export
plot_attribute_sixpack <- function(panels, title = NULL, subtitle = NULL,
                                   theme = NULL) {
  layout <- (panels$control | panels$rate) /
            (panels$cumulative | panels$defects) /
            (panels$fit | panels$performance)
  layout + patchwork::plot_annotation(
    title = title, subtitle = subtitle,
    theme = patchwork_annotation_theme(theme)
  ) + patchwork::plot_layout(guides = "collect") +
    patchwork::plot_spacing(grid::unit(0.3, "cm")) &
    patchwork_panel_theme(theme) &
    ggplot2::theme(legend.position = "bottom")
}
