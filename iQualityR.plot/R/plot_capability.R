# =============================================================================
# File: R/plot_capability.R
# Description: Process-capability visualization functions for the iQualityR
#   ecosystem. Implements the Minitab-style Capability Sixpack layout, the
#   dual-normal-curve capability histogram, the capability-index bar with
#   reference thresholds and bootstrap CI, plus non-normal / non-parametric
#   panels (Box-Cox lambda, arbitrary-distribution P-P, empirical CDF,
#   bootstrap CI distribution). All functions are pure data-in / ggplot-out
#   and resolve every color through the IqrTheme toolbox (.iqr_aes). L2
#   Plotters in iQualityR.capa delegate to these functions and only extract
#   parameters from the analysis result.
# =============================================================================

#' Capability Individual-Values (I) Chart
#'
#' Draws the Panel-1 individual-values chart of the Capability Sixpack:
#' a connected line of observations, the mean center line, and horizontal
#' specification limits. Used to verify process-mean stability.
#'
#' @param data A data.frame with columns `index` (numeric) and `value` (numeric).
#' @param mean_val Numeric scalar, the process mean (center line).
#' @param specs Optional list with `lsl` and/or `usl` numeric scalars. When
#'   supplied, horizontal specification lines are drawn via [layers_spec_limits()].
#' @param theme Theme spec (NULL / string / function / IqrTheme).
#' @return A ggplot object.
#' @export
plot_capability_individual_chart <- function(data, mean_val, specs = NULL,
                                             theme = NULL) {
  c <- .iqr_aes(theme)
  p <- base_plot(data, ggplot2::aes(x = .data$index, y = .data$value),
                 theme = theme) +
    ggplot2::geom_line(color = c$muted, linewidth = 0.4) +
    ggplot2::geom_point(color = c$data, size = 1.5) +
    ggplot2::geom_hline(yintercept = mean_val, color = c$primary,
                       linetype = "solid", linewidth = 0.8) +
    ggplot2::labs(title = "Individual Values",
                  subtitle = sprintf("Mean = %s", .fmt_spec(mean_val)),
                  x = "Observation", y = "Measurement")
  if (!is.null(specs)) {
    p <- p + layers_spec_limits(lsl = specs$lsl, usl = specs$usl,
                                theme = theme, orientation = "h")
  }
  p
}

#' Capability Moving-Range (MR) Chart
#'
#' Draws the Panel-2 moving-range chart of the Capability Sixpack: a connected
#' line of moving ranges, the mean-R center line and the upper control limit
#' (UCL). Used to verify short-term dispersion stability.
#'
#' @param data A data.frame with columns `index`, `mr`, `mean_mr`, `ucl_mr`.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_moving_range <- function(data, theme = NULL) {
  c <- .iqr_aes(theme)
  base_plot(data, ggplot2::aes(x = .data$index, y = .data$mr), theme = theme) +
    ggplot2::geom_line(color = c$muted, linewidth = 0.4) +
    ggplot2::geom_point(color = c$data, size = 1.5) +
    ggplot2::geom_hline(yintercept = data$mean_mr[1], color = c$primary,
                        linetype = "solid", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = data$ucl_mr[1], color = c$fail,
                        linetype = "dashed", linewidth = 0.8) +
    ggplot2::labs(title = "Moving Range",
                  subtitle = sprintf("MR = %s, UCL = %s",
                                    .fmt_spec(data$mean_mr[1]),
                                    .fmt_spec(data$ucl_mr[1])),
                  x = "Observation", y = "Moving Range")
}

#' Capability Histogram with Dual Normal Curves
#'
#' Draws the Panel-4 capability histogram: a density histogram, the within-σ
#' normal curve (solid) and the overall-σ normal curve (dashed), plus vertical
#' specification limits and the target line. For non-normal analyses an
#' optional fitted-distribution curve (Weibull / Lognormal / Gamma / Johnson)
#' can be overlaid via `fitted_curve`.
#'
#' @param values Numeric vector of measurements.
#' @param mean_val Numeric scalar, process mean.
#' @param sd_within Numeric scalar, within-subgroup sigma (drives the within curve).
#' @param sd_overall Numeric scalar, overall sigma (drives the overall curve).
#' @param sd_between Optional numeric scalar, between-subgroup sigma. When
#'   supplied (Between/Within analysis), a third normal curve is drawn in the
#'   `danger` semantic color to visualize the between-subgroup variation.
#' @param specs Optional list with `lsl` and/or `usl`.
#' @param target Optional numeric scalar, target value.
#' @param fitted_curve Optional data.frame with columns `x` and `density` for a
#'   non-normal fitted-distribution curve overlay.
#' @param bins Number of histogram bins. Defaults to 30.
#' @param subtitle_text Optional subtitle string override.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_histogram <- function(values, mean_val, sd_within, sd_overall,
                                      sd_between = NULL,
                                      specs = NULL, target = NULL,
                                      fitted_curve = NULL, bins = 30,
                                      subtitle_text = NULL, theme = NULL) {
  c <- .iqr_aes(theme)
  df <- data.frame(measurement = values)
  # Explicitly inject fill/color into layers_histogram_density via ... so that
  # ggplot2 does NOT fall back to its default grey35 fill (the "黑乎乎一片" bug).
  # density_args overrides the density curve's color separately (primary, not border).
  p <- base_plot(df, ggplot2::aes(x = .data$measurement), theme = theme) +
    layers_histogram_density(
      bins = bins, theme = theme,
      fill = c$surface_soft,        # bar fill: light surface
      color = c$border,             # bar outline: subtle border
      linewidth = 0.4,              # thin outlines so bars don't merge into a black mass
      density_args = list(color = c$primary, linewidth = 1, fill = NA)
    )

  # Within-σ normal curve (solid, primary/data color).
  if (!is.null(sd_within) && is.finite(sd_within) && sd_within > 0) {
    p <- p + ggplot2::stat_function(
      fun = stats::dnorm, args = list(mean = mean_val, sd = sd_within),
      color = c$primary, linewidth = 1.0
    )
  }
  # Overall-σ normal curve (dashed, warning/amber color).
  if (!is.null(sd_overall) && is.finite(sd_overall) && sd_overall > 0) {
    p <- p + ggplot2::stat_function(
      fun = stats::dnorm, args = list(mean = mean_val, sd = sd_overall),
      color = c$warning, linetype = "dashed", linewidth = 1.0
    )
  }
  # Between-σ normal curve (dotted, danger color) — Between/Within analysis.
  if (!is.null(sd_between) && is.finite(sd_between) && sd_between > 0) {
    p <- p + ggplot2::stat_function(
      fun = stats::dnorm, args = list(mean = mean_val, sd = sd_between),
      color = c$danger, linetype = "dotted", linewidth = 0.9
    )
  }
  # Optional non-normal fitted-distribution curve overlay (distinct color).
  if (!is.null(fitted_curve)) {
    p <- p + ggplot2::geom_line(data = fitted_curve,
                                ggplot2::aes(x = .data$x, y = .data$density),
                                color = c$danger, linewidth = 1.1,
                                inherit.aes = FALSE)
  }
  # Specification limits (vertical, fail-semantic red).
  if (!is.null(specs)) {
    p <- p + layers_spec_limits(lsl = specs$lsl, usl = specs$usl,
                                theme = theme, orientation = "v")
  }
  # Target line (success-semantic, dotted).
  if (!is.null(target) && is.finite(target)) {
    p <- p + ggplot2::geom_vline(xintercept = target, color = c$success,
                                 linetype = "dotted", linewidth = 0.8)
  }
  if (is.null(subtitle_text)) {
    subtitle_text <- sprintf("Within SD = %s | Overall SD = %s",
                             .fmt_spec(sd_within), .fmt_spec(sd_overall))
  }
  p + ggplot2::labs(title = "Capability Histogram",
                    subtitle = subtitle_text,
                    x = "Measurement", y = "Density")
}

#' Capability Normal Q-Q Plot
#'
#' Draws the Panel-5 normal probability plot of the Capability Sixpack.
#'
#' @param values Numeric vector.
#' @param subtitle_text Optional subtitle.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_qq <- function(values, subtitle_text = NULL, theme = NULL) {
  df <- data.frame(measurement = values)
  p <- base_plot(df, ggplot2::aes(sample = .data$measurement), theme = theme) +
    layers_qq(distribution = "norm", theme = theme) +
    ggplot2::labs(title = "Normal Q-Q Plot", subtitle = subtitle_text,
                  x = "Theoretical Quantiles", y = "Sample Quantiles")
  p
}

#' Capability Fitted-Distribution Q-Q Plot
#'
#' Draws a Q-Q plot against an arbitrary fitted distribution by computing
#' theoretical quantiles from a fitter object. Used in non-normal capability
#' analysis to diagnose the chosen distribution's goodness of fit.
#'
#' @param values Numeric vector of measurements.
#' @param dist_name Distribution name understood by `fitter$eval_quantile`.
#' @param params Parameter list for the distribution.
#' @param fitter Object exposing `eval_quantile(p, dist_name, params)`.
#' @param subtitle_text Optional subtitle.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_qq_fitted <- function(values, dist_name, params, fitter,
                                       subtitle_text = NULL, theme = NULL) {
  c <- .iqr_aes(theme)
  x <- sort(values[!is.na(values)])
  n <- length(x)
  probs <- (seq_len(n) - 0.5) / n
  theoretical <- vapply(probs, function(p) {
    fitter$eval_quantile(p, dist_name, params)
  }, numeric(1))
  df <- data.frame(theoretical = theoretical, sample = x)
  base_plot(df, ggplot2::aes(x = .data$theoretical, y = .data$sample),
            theme = theme) +
    ggplot2::geom_point(color = c$data, alpha = 0.6) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                         color = c$fail) +
    ggplot2::labs(title = sprintf("Q-Q Plot (%s)", dist_name),
                  subtitle = subtitle_text,
                  x = sprintf("Theoretical Quantiles (%s)", dist_name),
                  y = "Sample Quantiles")
}

#' Capability Empirical Q-Q (Rankit) Plot
#'
#' Draws an empirical Q-Q plot against expected normal order statistics
#' (rankit). Used in non-parametric capability analysis to inspect distribution
#' shape without assuming a parametric family.
#'
#' @param values Numeric vector.
#' @param subtitle_text Optional subtitle.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_qq_empirical <- function(values, subtitle_text = NULL,
                                          theme = NULL) {
  c <- .iqr_aes(theme)
  x <- sort(values[!is.na(values)])
  n <- length(x)
  theoretical <- stats::qnorm((seq_len(n) - 0.5) / n)
  df <- data.frame(theoretical = theoretical, sample = x)
  base_plot(df, ggplot2::aes(x = .data$theoretical, y = .data$sample),
            theme = theme) +
    ggplot2::geom_point(color = c$data, alpha = 0.6) +
    ggplot2::geom_abline(intercept = mean(x), slope = stats::sd(x),
                         linetype = "dashed", color = c$fail) +
    ggplot2::labs(title = "Empirical Q-Q Plot", subtitle = subtitle_text,
                  x = "Expected Normal Quantiles", y = "Sample Quantiles")
}

#' Capability Index Bar with Thresholds and CI
#'
#' Draws the Panel-6 capability-index bar chart of the Capability Sixpack:
#' grouped bars for Cp / Cpk / Pp / Ppk colored by capability status
#' (pass >= 1.33, watch 1.0-1.33, fail < 1.0), with reference threshold lines
#' at 1.00 / 1.33 / 1.67 / 2.00 and optional bootstrap confidence-interval
#' error bars.
#'
#' @param indices_df data.frame with columns `Index` (character) and `Value`
#'   (numeric).
#' @param ci_df Optional data.frame with columns `Index`, `Lower`, `Upper` for
#'   95% CI error bars. When NULL, no error bars are drawn.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_index_bar <- function(indices_df, ci_df = NULL, theme = NULL) {
  c <- .iqr_aes(theme)

  status <- vapply(indices_df$Value, function(v) {
    if (is.na(v) || !is.finite(v)) "fail"
    else if (v >= 1.33) "pass"
    else if (v >= 1.0)  "watch"
    else "fail"
  }, character(1))
  indices_df$Status <- factor(status, levels = c("pass", "watch", "fail"))

  # Use ggplot() directly to avoid base_plot auto-injecting a discrete fill
  # scale that would conflict with the semantic palette below.
  p <- ggplot2::ggplot(indices_df,
                      ggplot2::aes(x = .data$Index, y = .data$Value,
                                   fill = .data$Status)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.6) +
    ggplot2::geom_hline(yintercept = c(1.0, 1.33, 1.67, 2.0),
                        linetype = "dashed", color = c$muted,
                        linewidth = 0.5, alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$Value)),
                       vjust = -0.5, color = c$text, size = 3.2) +
    ggplot2::labs(title = "Capability Indices",
                  subtitle = "Reference: 1.00 / 1.33 / 1.67 / 2.00",
                  x = NULL, y = "Index Value") +
    as_iqr_theme(theme)

  if (!is.null(ci_df)) {
    ci_df <- merge(ci_df, indices_df[, c("Index", "Status")], by = "Index")
    p <- p + ggplot2::geom_errorbar(data = ci_df,
                    ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper),
                    width = 0.2, color = c$text, linewidth = 0.5)
  }
  p + c$theme_obj$plot$scale_fill_semantic(labels = c("pass", "watch", "fail"))
}

#' Capability Trend / Last-Subgroups Chart
#'
#' Draws the Panel-3 trend panel of the Capability Sixpack. Supports two
#' layouts chosen by the columns present in `trend_data`:
#'   * windowed Cpk trend: columns `window` (numeric) and `cpk` (numeric);
#'     draws a Cpk line with a 1.33 reference line.
#'   * subgroup-mean stability chart: column `mean_val` (numeric); draws the
#'     last subgroups' means with horizontal specification limits.
#'
#' @param trend_data data.frame produced by the Analyzer.
#' @param specs Optional list with `lsl`/`usl` for the subgroup-mean layout.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_trend <- function(trend_data, specs = NULL, theme = NULL) {
  c <- .iqr_aes(theme)
  if (is.null(trend_data)) {
    return(.empty_capability_plot(theme, "Trend", "No trend data available."))
  }
  # Windowed Cpk trend.
  if ("cpk" %in% names(trend_data) && "window" %in% names(trend_data)) {
    df <- data.frame(x = trend_data$window, y = trend_data$cpk)
    if (nrow(df) < 2) {
      df <- rbind(df, df); df$x[2] <- df$x[1] + 1
    }
    return(base_plot(df, ggplot2::aes(x = .data$x, y = .data$y), theme = theme) +
      ggplot2::geom_line(color = c$muted, linewidth = 0.5) +
      ggplot2::geom_point(color = c$data, size = 2) +
      ggplot2::geom_hline(yintercept = 1.33, linetype = "dashed",
                          color = c$fail, linewidth = 0.8) +
      ggplot2::labs(title = "Cpk Trend (Windowed)",
                    subtitle = "Dashed line: 1.33 reference threshold",
                    x = "Window", y = "Cpk"))
  }
  # Subgroup-mean stability chart.
  if ("mean_val" %in% names(trend_data)) {
    df <- data.frame(x = seq_len(nrow(trend_data)), y = trend_data$mean_val)
    p <- base_plot(df, ggplot2::aes(x = .data$x, y = .data$y), theme = theme) +
      ggplot2::geom_line(color = c$muted, linewidth = 0.5) +
      ggplot2::geom_point(color = c$data, size = 2) +
      ggplot2::labs(title = "Last Subgroups (Mean)",
                    x = "Subgroup", y = "Mean")
    if (!is.null(specs)) {
      p <- p + layers_spec_limits(lsl = specs$lsl, usl = specs$usl,
                                  theme = theme, orientation = "h")
    }
    return(p)
  }
  .empty_capability_plot(theme, "Trend", "Trend data not available.")
}

#' Capability Sixpack Composite
#'
#' Assembles the Minitab-style 3x2 Capability Sixpack from six pre-built ggplot
#' panels. Layout follows the industry-standard arrangement:
#'
#' \preformatted{
#'   Row 1 (stability):  I chart        |  MR chart
#'   Row 2 (capability): histogram      |  Q-Q plot
#'   Row 3 (indices):    index bar      |  trend / last subgroups
#' }
#'
#' @param panels A named list with elements `individual`, `moving_range`,
#'   `histogram`, `qq`, `index_bar`, `trend` (each a ggplot object), and
#'   optionally `process_table` (a tableGrob) for the 6+1 layout.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param theme Theme spec.
#' @return A patchwork object.
#' @export
plot_capability_sixpack <- function(panels, title = "Process Capability Sixpack",
                                    subtitle = NULL, theme = NULL) {
  # When process_table is provided, use 6+1 layout (4 rows, table spans full width)
  if (!is.null(panels$process_table)) {
    # Use design syntax: A-F = 6 plot panels, G = table (spans 2 cols)
    # Row 1: A B | Row 2: C D | Row 3: E F | Row 4: G G
    design <- "
AABB
CCDD
EEFF
GGGG
"
    layout <- (panels$individual + panels$moving_range +
              panels$histogram + panels$qq +
              panels$index_bar + panels$trend +
              patchwork::wrap_elements(panels$process_table)) +
      patchwork::plot_layout(design = design, heights = c(1, 1, 1, 0.6))
  } else {
    # 6-panel base layout (3 rows x 2 cols)
    layout <- (panels$individual | panels$moving_range) /
              (panels$histogram    | panels$qq) /
              (panels$index_bar    | panels$trend)
  }
  layout + patchwork::plot_annotation(
    title = title, subtitle = subtitle,
    theme = as_iqr_theme(theme)
  )
}

#' Box-Cox Lambda Optimization Plot
#'
#' Draws the Box-Cox transformation parameter optimization plot: log-likelihood
#' (or equivalently the residual sum of squares) as a function of lambda, with
#' the optimal lambda* marked and the 95% confidence interval band shaded.
#'
#' @param lambda_data data.frame with columns `lambda` (numeric) and `ll`
#'   (numeric, log-likelihood).
#' @param lambda_opt Numeric scalar, the optimal lambda.
#' @param ci_low Numeric scalar, lower bound of the 95% CI for lambda.
#' @param ci_high Numeric scalar, upper bound of the 95% CI for lambda.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_box_cox_lambda <- function(lambda_data, lambda_opt, ci_low, ci_high,
                                theme = NULL) {
  c <- .iqr_aes(theme)
  base_plot(lambda_data, ggplot2::aes(x = .data$lambda, y = .data$ll),
            theme = theme) +
    ggplot2::geom_line(color = c$primary, linewidth = 1.0) +
    ggplot2::annotate("rect", xmin = ci_low, xmax = ci_high,
                      ymin = -Inf, ymax = Inf,
                      fill = c$primary, alpha = 0.08) +
    ggplot2::geom_vline(xintercept = lambda_opt, color = c$success,
                        linetype = "dashed", linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = c(0, 1), color = c$muted,
                        linetype = "dotted", linewidth = 0.5) +
    ggplot2::labs(title = "Box-Cox Transformation",
                  subtitle = sprintf("Optimal lambda = %s (95%% CI: %s, %s)",
                                    .fmt_spec(lambda_opt),
                                    .fmt_spec(ci_low), .fmt_spec(ci_high)),
                  x = "Lambda", y = "Log-Likelihood")
}

#' Arbitrary-Distribution P-P Plot
#'
#' Draws a probability-probability plot for an arbitrary fitted distribution:
#' empirical CDF versus fitted CDF, with a 45-degree reference line. Used in
#' non-normal capability analysis to assess goodness of fit.
#'
#' @param values Numeric vector of measurements.
#' @param dist_name Distribution name understood by `fitter$eval_cdf`.
#' @param params Parameter list for the distribution.
#' @param fitter Object exposing `eval_cdf(x, dist_name, params)`.
#' @param subtitle_text Optional subtitle.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_pp_distribution <- function(values, dist_name, params, fitter,
                                 subtitle_text = NULL, theme = NULL) {
  c <- .iqr_aes(theme)
  x <- sort(values[!is.na(values)])
  n <- length(x)
  emp <- (seq_len(n) - 0.5) / n
  fitted <- vapply(x, function(xi) fitter$eval_cdf(xi, dist_name, params),
                   numeric(1))
  df <- data.frame(empirical = emp, fitted = fitted)
  base_plot(df, ggplot2::aes(x = .data$fitted, y = .data$empirical),
            theme = theme) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                         color = c$muted) +
    ggplot2::geom_point(color = c$data, alpha = 0.6) +
    ggplot2::labs(title = sprintf("P-P Plot (%s)", dist_name),
                  subtitle = subtitle_text,
                  x = sprintf("Fitted CDF (%s)", dist_name),
                  y = "Empirical CDF")
}

#' Empirical CDF Capability Plot
#'
#' Draws the empirical cumulative distribution function with vertical
#' specification limits and annotated tail probabilities P(X < LSL) and
#' P(X > USL). Used in non-parametric capability analysis.
#'
#' @param values Numeric vector.
#' @param specs Optional list with `lsl` and/or `usl`.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_ecdf_capability <- function(values, specs = NULL, theme = NULL) {
  c <- .iqr_aes(theme)
  x <- sort(values[!is.na(values)])
  n <- length(x)
  df <- data.frame(x = x, y = (seq_len(n)) / n)
  p <- base_plot(df, ggplot2::aes(x = .data$x, y = .data$y), theme = theme) +
    ggplot2::geom_step(color = c$primary, linewidth = 0.9) +
    ggplot2::labs(title = "Empirical CDF",
                  x = "Measurement", y = "Cumulative Probability")
  if (!is.null(specs)) {
    p <- p + layers_spec_limits(lsl = specs$lsl, usl = specs$usl,
                                theme = theme, orientation = "v")
  }
  p
}

#' Bootstrap Capability-Index CI Distribution
#'
#' Draws the bootstrap sampling distribution of a capability index (Cpk or
#' Ppk) with the 95% confidence-interval band shaded. Used to communicate the
#' uncertainty of non-parametric / small-sample capability estimates.
#'
#' @param boot_values Numeric vector of bootstrap replicates of the index.
#' @param ci_low Numeric scalar, 2.5% quantile.
#' @param ci_high Numeric scalar, 97.5% quantile.
#' @param index_name Character, name of the index (e.g. "Cpk").
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_bootstrap_ci <- function(boot_values, ci_low, ci_high,
                              index_name = "Cpk", theme = NULL) {
  c <- .iqr_aes(theme)
  df <- data.frame(value = boot_values)
  base_plot(df, ggplot2::aes(x = .data$value), theme = theme) +
    ggplot2::annotate("rect", xmin = ci_low, xmax = ci_high,
                      ymin = -Inf, ymax = Inf,
                      fill = c$muted, alpha = 0.15) +
    layers_histogram_density(bins = 40, theme = theme) +
    ggplot2::geom_vline(xintercept = c(ci_low, ci_high), color = c$fail,
                        linetype = "dashed", linewidth = 0.8) +
    ggplot2::labs(title = sprintf("Bootstrap %s Distribution", index_name),
                  subtitle = sprintf("95%% CI: %s, %s",
                                    .fmt_spec(ci_low), .fmt_spec(ci_high)),
                  x = index_name, y = "Density")
}

# Internal empty-panel helper for capability charts.
.empty_capability_plot <- function(theme, title, message) {
  c <- .iqr_aes(theme)
  df <- data.frame(x = 0, y = 0, label = message)
  base_plot(df, ggplot2::aes(x = .data$x, y = .data$y), theme = theme) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label),
                       color = c$muted, size = 4) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
}

#' Capability PPM Probability Plot
#'
#' Draws the probability density curve with specification-limit tail regions
#' shaded to visualize Parts-Per-Million (PPM) defect rates. Used in normal
#' and non-normal capability analysis.
#'
#' @param mean_val Numeric scalar, process mean.
#' @param sd Numeric scalar, process standard deviation.
#' @param specs List with lsl and/or usl numeric scalars.
#' @param target Optional numeric scalar.
#' @param dist_func Optional distribution density function (e.g. dweibull) for
#'   non-normal curves. When NULL, the normal density is used.
#' @param dist_params Optional list of parameters for dist_func.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_ppm <- function(mean_val, sd, specs = NULL, target = NULL,
                                dist_func = NULL, dist_params = NULL,
                                theme = NULL) {
  c <- .iqr_aes(theme)
  if (is.null(dist_func)) {
    dist_func <- stats::dnorm
    dist_params <- list(mean = mean_val, sd = sd)
  }
  xlim <- range(c(mean_val - 4 * sd, mean_val + 4 * sd,
                  specs$lsl, specs$usl, target), na.rm = TRUE)
  xlim <- xlim + c(-0.5, 0.5) * diff(xlim)
  x_grid <- seq(xlim[1], xlim[2], length.out = 300)
  dens <- vapply(x_grid, function(xi) do.call(dist_func, c(list(xi), dist_params)), numeric(1))

  df <- data.frame(x = x_grid, density = dens)
  p <- base_plot(df, ggplot2::aes(x = .data$x, y = .data$density), theme = theme) +
    ggplot2::geom_line(color = c$primary, linewidth = 1.0)

  # Shade left tail (below LSL) in danger color
  if (!is.null(specs$lsl) && is.finite(specs$lsl)) {
    left <- x_grid[x_grid <= specs$lsl]
    if (length(left) > 1) {
      left_df <- data.frame(x = c(left[1], left, specs$lsl),
                            y = c(0, dens[x_grid <= specs$lsl], 0))
      p <- p + ggplot2::geom_polygon(data = left_df,
                    ggplot2::aes(x = .data$x, y = .data$y),
                    fill = c$danger, alpha = 0.35, inherit.aes = FALSE)
    }
  }
  # Shade right tail (above USL) in danger color
  if (!is.null(specs$usl) && is.finite(specs$usl)) {
    right <- x_grid[x_grid >= specs$usl]
    if (length(right) > 1) {
      right_df <- data.frame(x = c(specs$usl, right, right[length(right)]),
                             y = c(0, dens[x_grid >= specs$usl], 0))
      p <- p + ggplot2::geom_polygon(data = right_df,
                    ggplot2::aes(x = .data$x, y = .data$y),
                    fill = c$danger, alpha = 0.35, inherit.aes = FALSE)
    }
  }
  # Spec limits (vertical)
  if (!is.null(specs)) {
    p <- p + layers_spec_limits(lsl = specs$lsl, usl = specs$usl,
                                theme = theme, orientation = "v")
  }
  # Target line
  if (!is.null(target) && is.finite(target)) {
    p <- p + ggplot2::geom_vline(xintercept = target, color = c$success,
                                 linetype = "dotted", linewidth = 0.8)
  }
  p + ggplot2::labs(title = "PPM Probability Plot",
                    subtitle = "Shaded tails = expected defective proportion",
                    x = "Measurement", y = "Density")
}

#' Capability Performance Bar (Observed vs Expected PPM)
#'
#' Draws the Minitab-standard performance panel for normal capability
#' analysis: a three-bar comparison of Observed PPM (empirical), Expected
#' Within PPM (model-based, using within-subgroup sigma), and Expected
#' Overall PPM (model-based, using overall sigma). This communicates the
#' process defect rate from both empirical and model-based perspectives.
#'
#' @param observed_ppm Numeric scalar, observed defect rate as PPM.
#' @param expected_within_ppm Numeric scalar, expected within-subgroup PPM.
#' @param expected_overall_ppm Numeric scalar, expected overall PPM.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_performance_bar <- function(observed_ppm, expected_within_ppm,
                                            expected_overall_ppm, theme = NULL) {
  c <- .iqr_aes(theme)
  df <- data.frame(
    Category = factor(c("Observed", "Expected (Within)", "Expected (Overall)"),
                      levels = c("Observed", "Expected (Within)",
                                 "Expected (Overall)")),
    PPM = c(observed_ppm, expected_within_ppm, expected_overall_ppm)
  )
  ggplot2::ggplot(df, ggplot2::aes(x = .data$Category, y = .data$PPM,
                                   fill = .data$Category)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = formatC(.data$PPM, format = "f",
                                                    digits = 0)),
                       vjust = -0.5, color = c$text, size = 3.2) +
    ggplot2::scale_fill_manual(values = c("Observed" = c$muted,
                                          "Expected (Within)" = c$primary,
                                          "Expected (Overall)" = c$warning)) +
    ggplot2::labs(title = "Performance (Observed vs Expected PPM)",
                  subtitle = "Observed = empirical | Expected = model-based",
                  x = NULL, y = "PPM (parts per million)") +
    as_iqr_theme(theme)
}

#' Histogram with Kernel Density Estimate Overlay
#'
#' Draws a density histogram with a kernel density estimate (KDE) curve
#' overlaid, plus specification limits and target line. Used in non-parametric
#' capability analysis to inspect the distribution shape without assuming a
#' parametric family.
#'
#' @param values Numeric vector of measurements.
#' @param specs Optional list with `lsl` and/or `usl`.
#' @param target Optional numeric scalar, target value.
#' @param bins Number of histogram bins. Default 30.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_kde_overlay <- function(values, specs = NULL, target = NULL,
                             bins = 30, theme = NULL) {
  c <- .iqr_aes(theme)
  df <- data.frame(measurement = values)
  dens <- stats::density(values, na.rm = TRUE)
  kde_df <- data.frame(x = dens$x, density = dens$y)
  p <- base_plot(df, ggplot2::aes(x = .data$measurement), theme = theme) +
    layers_histogram_density(bins = bins, theme = theme) +
    ggplot2::geom_line(data = kde_df,
                      ggplot2::aes(x = .data$x, y = .data$density),
                      color = c$danger, linewidth = 1.0,
                      inherit.aes = FALSE)
  if (!is.null(specs)) {
    p <- p + layers_spec_limits(lsl = specs$lsl, usl = specs$usl,
                                theme = theme, orientation = "v")
  }
  if (!is.null(target) && is.finite(target)) {
    p <- p + ggplot2::geom_vline(xintercept = target, color = c$success,
                                 linetype = "dotted", linewidth = 0.8)
  }
  p + ggplot2::labs(title = "Histogram + KDE",
                    subtitle = "Kernel density estimate overlay",
                    x = "Measurement", y = "Density")
}

#' Distribution Identification Matrix
#'
#' Draws a small-multiples matrix of P-P plots for multiple candidate
#' distributions, each annotated with its Anderson-Darling statistic and
#' p-value. This is the Minitab-standard "Individual Distribution
#' Identification" panel — the mandatory step before selecting a
#' distribution for non-normal capability analysis.
#'
#' @param values Numeric vector of measurements.
#' @param candidates data.frame with columns: `dist_name` (character),
#'   `params` (list column, each element a parameter list for that
#'   distribution), `ad_stat` (numeric, Anderson-Darling statistic),
#'   `p_value` (numeric, p-value).
#' @param fitter Object exposing `eval_cdf(x, dist_name, params)`.
#' @param theme Theme spec.
#' @return A patchwork object.
#' @export
plot_capability_distid_matrix <- function(values, candidates, fitter,
                                           theme = NULL) {
  plots <- lapply(seq_len(nrow(candidates)), function(i) {
    dist_name <- candidates$dist_name[i]
    params <- candidates$params[[i]]
    ad <- candidates$ad_stat[i]
    pv <- candidates$p_value[i]
    sub <- sprintf("AD = %s | p = %s", .fmt_spec(ad),
                   if (is.na(pv) || !is.finite(pv)) "NA"
                   else sprintf("%.3f", pv))
    plot_pp_distribution(values = values, dist_name = dist_name,
                         params = params, fitter = fitter,
                         subtitle_text = sub, theme = theme)
  })
  patchwork::wrap_plots(plots, ncol = 3) +
    patchwork::plot_annotation(
      title = "Distribution Identification Matrix",
      subtitle = "P-P plots for candidate distributions (lower AD = better fit)",
      theme = as_iqr_theme(theme))
}

#' Johnson Transformation Diagnostic Plot
#'
#' Draws a P-P plot assessing the goodness of fit of a Johnson transformation
#' (Su / Sb / Sl), with the selected transformation type annotated. Used in
#' the Johnson transformation path of non-normal capability analysis.
#'
#' @param values Numeric vector of measurements (after Johnson transformation).
#' @param johnson_type Character, one of "Su", "Sb", "Sl".
#' @param params Parameter list for the Johnson distribution.
#' @param fitter Object exposing `eval_cdf(x, dist_name, params)` with
#'   Johnson support.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_johnson_diagnostic <- function(values, johnson_type, params, fitter,
                                    theme = NULL) {
  sub <- sprintf("Johnson %s | Goodness-of-fit P-P", johnson_type)
  plot_pp_distribution(values = values, dist_name = "johnson",
                       params = params, fitter = fitter,
                       subtitle_text = sub, theme = theme) +
    ggplot2::labs(title = sprintf("Johnson %s Diagnostic", johnson_type))
}

#' Non-normal Capability Sixpack Composite
#'
#' Assembles the 6-panel non-normal capability composite following the
#' Minitab Capability Sixpack (Nonnormal) layout:
#' Row 1: distribution ID matrix | Box-Cox lambda plot
#' Row 2: capability histogram   | fitted-distribution P-P plot
#' Row 3: capability index bar   | trend / last subgroups
#'
#' @param panels Named list with elements: distid, boxcox, histogram, pp,
#'   capbar, trend (each a ggplot object).
#' @param title Character.
#' @param subtitle Character.
#' @param theme Theme spec.
#' @return A patchwork object.
#' @export
plot_capability_sixpack_nonnormal <- function(panels,
                                               title = "Non-normal Capability Sixpack",
                                               subtitle = NULL, theme = NULL) {
  layout <- (panels$distid | panels$boxcox) /
            (panels$histogram | panels$pp) /
            (panels$capbar | panels$trend)
  layout + patchwork::plot_annotation(title = title, subtitle = subtitle,
                                      theme = as_iqr_theme(theme))
}
