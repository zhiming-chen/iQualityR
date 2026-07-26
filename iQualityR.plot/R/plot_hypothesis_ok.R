# ============================================================================
# File: R/plot_hypothesis.R
# Description: Hypothesis testing visualization functions (rejection region plot + boxplot + statistics table)
# ============================================================================

#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_vline
#' @importFrom ggplot2 geom_boxplot geom_jitter annotate labs scale_x_continuous
#' @importFrom ggplot2 annotation_custom theme
#' @importFrom stats na.omit dnorm dt pnorm pt qnorm qt
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom grid unit
#' @importFrom gridExtra tableGrob ttheme_default
#' @name iQualityR.plot-hypothesis
#' @title Hypothesis testing visualization functions
#' @description Hypothesis testing visualization series including rejection region plots and boxplots with statistics tables.
#' @keywords internal
NULL

#' @title Hypothesis testing rejection region plot
#' @description
#' Draw a normal distribution or t distribution curve, annotating the rejection region (red shading) and the statistic position (blue vertical line).
#' Blue line falls in red region -> reject H0; falls in white region -> fail to reject H0.
#'
#' @param dist Distribution type ("norm" or "t")
#' @param stat_value Calculated statistic (z-value or t-value)
#' @param critical_value Critical value (positive)
#' @param alternative Test type ("two.sided", "less", "greater")
#' @param alpha Significance level (default 0.05)
#' @param df Degrees of freedom (used for t distribution)
#' @param stat_label Statistic label (default "Z" or "t")
#' @param p_value p-value (optional, displayed on the plot)
#' @param theme Theme (IqrTheme object or theme name)
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' # Two-sided test: z = 1.53, critical value = 1.96
#' plot_hypothesis_curve(dist = "norm", stat_value = 1.53,
#'                       critical_value = 1.96, alternative = "two.sided")
#'
#' # Right-sided test: t = 2.46, critical value = 1.645, df = 399
#' plot_hypothesis_curve(dist = "t", stat_value = 2.46,
#'                       critical_value = 1.645, alternative = "greater", df = 399)
#' }
plot_hypothesis_curve <- function(dist = c("norm", "t"),
                                   stat_value,
                                   critical_value,
                                   alternative = c("two.sided", "less", "greater"),
                                   alpha = 0.05,
                                   df = NULL,
                                   stat_label = NULL,
                                   p_value = NULL,
                                   theme = NULL) {
  dist <- match.arg(dist)
  alternative <- match.arg(alternative)

  if (is.null(stat_label)) {
    stat_label <- if (dist == "norm") "Z" else "t"
  }

  theme_obj <- as_iqr_theme_object(theme)

  # Determine x-axis range
  x_max <- max(abs(stat_value), critical_value) * 1.3
  x_seq <- seq(-x_max, x_max, length.out = 1000)

  # Density function
  if (dist == "norm") {
    y_seq <- dnorm(x_seq)
    d_func <- function(x) dnorm(x)
    p_func <- function(x) pnorm(x)
  } else {
    if (is.null(df)) stop("df is required for t distribution.")
    y_seq <- dt(x_seq, df = df)
    d_func <- function(x) dt(x, df = df)
    p_func <- function(x) pt(x, df = df)
  }

  df_curve <- data.frame(x = x_seq, y = y_seq)

  # Build rejection region data
  if (alternative == "two.sided") {
    df_reject_left <- data.frame(
      x = x_seq[x_seq <= -critical_value],
      y = d_func(x_seq[x_seq <= -critical_value])
    )
    df_reject_right <- data.frame(
      x = x_seq[x_seq >= critical_value],
      y = d_func(x_seq[x_seq >= critical_value])
    )
  } else if (alternative == "less") {
    df_reject_left <- data.frame(
      x = x_seq[x_seq <= -critical_value],
      y = d_func(x_seq[x_seq <= -critical_value])
    )
    df_reject_right <- NULL
  } else {
    df_reject_left <- NULL
    df_reject_right <- data.frame(
      x = x_seq[x_seq >= critical_value],
      y = d_func(x_seq[x_seq >= critical_value])
    )
  }

  # Build plot
  p <- ggplot2::ggplot(df_curve, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(color = .iqr_plotter$.pal_discrete(theme_obj)[1], linewidth = 1.2)

  # Add rejection region shading
  if (!is.null(df_reject_left) && nrow(df_reject_left) > 0) {
    p <- p + ggplot2::geom_ribbon(
      data = df_reject_left,
      ggplot2::aes(x = x, ymin = 0, ymax = y),
      fill = .iqr_plotter$.pal_semantic(theme_obj, "fail"), alpha = 0.25
    )
  }
  if (!is.null(df_reject_right) && nrow(df_reject_right) > 0) {
    p <- p + ggplot2::geom_ribbon(
      data = df_reject_right,
      ggplot2::aes(x = x, ymin = 0, ymax = y),
      fill = .iqr_plotter$.pal_semantic(theme_obj, "fail"), alpha = 0.25
    )
  }

  # Statistic vertical line (blue)
  p <- p + ggplot2::geom_vline(
    xintercept = stat_value, color = .iqr_plotter$.pal_ui(theme_obj, "primary"), linewidth = 1.2
  )

  # Critical value dashed lines
  if (alternative == "two.sided") {
    p <- p +
      ggplot2::geom_vline(xintercept = critical_value, color = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666"), linetype = "dashed") +
      ggplot2::geom_vline(xintercept = -critical_value, color = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666"), linetype = "dashed")
  } else if (alternative == "less") {
    p <- p + ggplot2::geom_vline(xintercept = -critical_value, color = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666"), linetype = "dashed")
  } else {
    p <- p + ggplot2::geom_vline(xintercept = critical_value, color = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666"), linetype = "dashed")
  }

  # Annotate statistic value
  y_at_stat <- d_func(stat_value)
  p <- p + ggplot2::annotate(
    "text", x = stat_value, y = y_at_stat * 1.15,
    label = sprintf("%s = %.2f", stat_label, stat_value),
    vjust = 0, hjust = 0.5, size = 4, color = .iqr_plotter$.pal_ui(theme_obj, "primary"), fontface = "bold"
  )

  # Annotate p-value
  if (!is.null(p_value)) {
    p_label <- if (p_value < 0.001) "p < 0.001" else sprintf("p = %.4f", p_value)
    p <- p + ggplot2::annotate(
      "text", x = 0, y = max(y_seq) * 0.9,
      label = p_label,
      vjust = 0, hjust = 0.5, size = 4, color = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666"), fontface = "bold"
    )
  }

  # Annotate rejection region
  if (alternative == "two.sided") {
    p <- p +
      ggplot2::annotate("text", x = -x_max * 0.7, y = max(y_seq) * 0.15,
                        label = "Rejection region", size = 3, color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
      ggplot2::annotate("text", x = x_max * 0.7, y = max(y_seq) * 0.15,
                        label = "Rejection region", size = 3, color = .iqr_plotter$.pal_semantic(theme_obj, "fail"))
  } else if (alternative == "less") {
    p <- p + ggplot2::annotate("text", x = -x_max * 0.7, y = max(y_seq) * 0.15,
                               label = "Rejection region", size = 3, color = .iqr_plotter$.pal_semantic(theme_obj, "fail"))
  } else {
    p <- p + ggplot2::annotate("text", x = x_max * 0.7, y = max(y_seq) * 0.15,
                               label = "Rejection region", size = 3, color = .iqr_plotter$.pal_semantic(theme_obj, "fail"))
  }

  # Title
  alt_symbol <- switch(alternative,
    two.sided = "!=",
    less = "<",
    greater = ">"
  )

  title <- bquote("Hypothesis Test: " * H[0] ~ "vs" ~ H[a] * ": " * mu ~ .(alt_symbol) ~ mu[0])

  p <- p + ggplot2::labs(
    x = stat_label,
    y = "Density",
    # title = sprintf("Hypothesis Test: H0 vs Ha: mu %s mu0", alt_symbol),
    title = title,
    subtitle = sprintf("alpha = %.2f, %s = %.2f, Critical = +/-%.3f",
                       alpha, stat_label, stat_value, critical_value)
  )

  # Theme
  p <- p + as_iqr_theme(theme)

  p
}


#' @title Hypothesis testing boxplot (with confidence interval and statistics table)
#' @description
#' Draw a sample data boxplot, annotating the H0 value (red cross), sample mean (blue point),
#' confidence interval (blue error bars), and embed a key statistics table on the right.
#'
#' @param x Numeric vector (sample data)
#' @param mu H0 hypothesized population mean
#' @param sigma Known population standard deviation (Z-test); when NULL, use sample standard deviation (t-test)
#' @param alternative Test type ("two.sided", "less", "greater")
#' @param conf_level Confidence level (default 0.95)
#' @param show_table Whether to show the statistics table (default TRUE)
#' @param table_position Table position ("right", "bottom")
#' @param theme Theme (IqrTheme object or theme name)
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' # Right-sided test example
#' set.seed(123)
#' x <- rnorm(400, mean = 178, sd = 68)
#' plot_hypothesis_box(x, mu = 170, sigma = 65, alternative = "greater")
#' }
plot_hypothesis_box <- function(x, mu, sigma = NULL,
                                 alternative = c("two.sided", "less", "greater"),
                                 conf_level = 0.95,
                                 show_table = TRUE,
                                 table_position = "right",
                                 theme = NULL) {
  alternative <- match.arg(alternative)

  x <- stats::na.omit(x)
  n <- length(x)
  if (n < 2) stop("Need at least 2 non-missing observations.")

  theme_obj <- as_iqr_theme_object(theme)

  x_bar <- mean(x)
  s <- sd(x)

  # Determine whether to use Z or t
  use_z <- !is.null(sigma)
  se <- if (use_z) sigma / sqrt(n) else s / sqrt(n)
  stat_value <- (x_bar - mu) / se

  # Calculate p-value
  if (use_z) {
    p_func <- pnorm
    q_func <- qnorm
    stat_label <- "Z"
  } else {
    p_func <- function(q) pt(q, df = n - 1)
    q_func <- function(p) qt(p, df = n - 1)
    stat_label <- "t"
  }

  p_value <- switch(alternative,
    two.sided = 2 * (1 - p_func(abs(stat_value))),
    less = p_func(stat_value),
    greater = 1 - p_func(stat_value)
  )

  # Calculate confidence interval
  if (alternative == "two.sided") {
    ci_low <- x_bar - q_func(1 - (1 - conf_level) / 2) * se
    ci_upp <- x_bar + q_func(1 - (1 - conf_level) / 2) * se
    ci_text <- sprintf("[%.2f, %.2f]", ci_low, ci_upp)
  } else if (alternative == "greater") {
    ci_low <- x_bar - q_func(1 - (1 - conf_level)) * se
    ci_text <- sprintf("[%.2f, Inf]", ci_low)
  } else {
    ci_upp <- x_bar + q_func(1 - (1 - conf_level)) * se
    ci_text <- sprintf("[-Inf, %.2f]", ci_upp)
  }

  # Critical value
  if (alternative == "two.sided") {
    crit <- q_func(1 - (1 - conf_level) / 2)
  } else {
    crit <- q_func(conf_level)
  }

  # Build statistics table
  res_df <- data.frame(
    Statistic = c("N", "Mean", "sd", "SE", "mu_0", stat_label, "P", "CI", "Alternative"),
    Value = c(
      n,
      sprintf("%.2f", x_bar),
      sprintf("%.2f", s),
      sprintf("%.4f", se),
      mu,
      sprintf("%.4f", stat_value),
      if (p_value < 0.001) "<0.001" else sprintf("%.4f", p_value),
      ci_text,
      alternative
    ),
    stringsAsFactors = FALSE
  )

  # Base boxplot
  p <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = 1, y = x)) +
    ggplot2::geom_boxplot(width = 0.2, fill = .iqr_plotter$.pal_discrete(theme_obj)[1], alpha = 0.3) +
    ggplot2::geom_jitter(width = 0.08, alpha = 0.3, size = 0.8, color = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666")) +
    # H0 marker (red cross)
    ggplot2::annotate("point", x = 0.78, y = mu, size = 3, color = .iqr_plotter$.pal_semantic(theme_obj, "fail"), shape = 13) +
    ggplot2::annotate("text", x = 0.72, y = mu, label = "H[0]", parse = TRUE,
                      size = 3.5, color = .iqr_plotter$.pal_semantic(theme_obj, "fail"), fontface = "bold") +
    # Sample mean (blue point)
    ggplot2::annotate("point", x = 0.85, y = x_bar, size = 2.5, color = .iqr_plotter$.pal_ui(theme_obj, "primary")) +
    ggplot2::annotate("text", x = 0.90, y = x_bar, label = sprintf("x_bar = %.2f", x_bar),
                      size = 3, color = .iqr_plotter$.pal_ui(theme_obj, "primary"), hjust = 0)

  # Confidence interval error bars
  if (alternative == "two.sided") {
    p <- p + ggplot2::annotate("segment",
      x = 0.83, xend = 0.87, y = ci_low, yend = ci_low,
      color = .iqr_plotter$.pal_ui(theme_obj, "primary"), linewidth = 1
    ) + ggplot2::annotate("segment",
      x = 0.83, xend = 0.87, y = ci_upp, yend = ci_upp,
      color = .iqr_plotter$.pal_ui(theme_obj, "primary"), linewidth = 1
    ) + ggplot2::annotate("segment",
      x = 0.85, xend = 0.85, y = ci_low, yend = ci_upp,
      color = .iqr_plotter$.pal_ui(theme_obj, "primary"), linewidth = 0.8
    )
  } else if (alternative == "greater") {
    p <- p + ggplot2::annotate("segment",
      x = 0.83, xend = 0.87, y = ci_low, yend = ci_low,
      color = .iqr_plotter$.pal_ui(theme_obj, "primary"), linewidth = 1
    ) + ggplot2::annotate("segment",
      x = 0.85, xend = 0.85, y = ci_low, yend = max(x),
      color = .iqr_plotter$.pal_ui(theme_obj, "primary"), linewidth = 0.8,
      arrow = ggplot2::arrow(type = "closed", length = grid::unit(0.15, "cm"))
    )
  } else {
    p <- p + ggplot2::annotate("segment",
      x = 0.83, xend = 0.87, y = ci_upp, yend = ci_upp,
      color = .iqr_plotter$.pal_ui(theme_obj, "primary"), linewidth = 1
    ) + ggplot2::annotate("segment",
      x = 0.85, xend = 0.85, y = ci_upp, yend = min(x),
      color = .iqr_plotter$.pal_ui(theme_obj, "primary"), linewidth = 0.8,
      arrow = ggplot2::arrow(type = "closed", length = grid::unit(0.15, "cm"), angle = 180)
    )
  }

  # Title
  alt_symbol <- switch(alternative,
    two.sided = "!=",
    less = "<",
    greater = ">"
  )
  title = bquote("Hypothesis Test: " * H[0] * ": " * mu ~ "=" ~ .(mu) ~ " vs " * H[a] * ": " * mu ~ .(alt_symbol) ~ .(mu))
  p_display <- if (p_value < 0.001) "<0.001" else sprintf("%.4f", p_value)
  decision <- if (p_value <= (1 - conf_level)) "Reject" else "Fail to reject"

  subtitle = bquote(.(stat_label) ~ "=" ~ .(round(stat_value, 2)) * "," ~ P ~ "=" ~ .(p_display) ~ " -> " ~ .(decision) ~ " " * H[0])


  p <- p + ggplot2::labs(
    x = NULL,
    y = "Value",
    # title = sprintf("Hypothesis Test: H0: mu = %s vs Ha: mu %s %s", mu, alt_symbol, mu),
    title = title,
    subtitle = subtitle
  ) +
    ggplot2::scale_x_continuous(limits = c(0.5, if (show_table && table_position == "right") 1.8 else 1.2),
                                breaks = NULL)

  # Embed statistics table
  if (show_table) {
    table_grob <- gridExtra::tableGrob(res_df, rows = NULL,
      theme = gridExtra::ttheme_default(
        base_size = 8,
        base_colour = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666"),
        padding = grid::unit(c(4, 6), "mm")
      )
    )

    if (table_position == "right") {
      y_range <- range(x, na.rm = TRUE)
      y_span <- y_range[2] - y_range[1]
      p <- p + ggplot2::annotation_custom(
        grob = table_grob,
        xmin = 1.25, xmax = 1.75,
        ymin = y_range[1] + y_span * 0.25,
        ymax = y_range[2] - y_span * 0.05
      )
    }
  }

  # Theme
  p <- p + as_iqr_theme(theme)

  p
}


#' @title Hypothesis testing combined plot (rejection region + boxplot)
#' @description
#' Combine the rejection region plot and boxplot into a single plot, suitable for report presentation.
#'
#' @param x Sample data vector
#' @param mu H0 hypothesized population mean
#' @param sigma Known population standard deviation (when NULL, use sample standard deviation)
#' @param alternative Test type
#' @param conf_level Confidence level
#' @param theme Theme
#'
#' @return patchwork combined plot object
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' x <- rnorm(50, mean = 102, sd = 5)
#' plot_hypothesis_combined(x, mu = 100, sigma = 5, alternative = "two.sided")
#' }
plot_hypothesis_combined <- function(x, mu, sigma = NULL,
                                      alternative = c("two.sided", "less", "greater"),
                                      conf_level = 0.95,
                                      theme = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("'patchwork' package is required. Install with: install.packages('patchwork')")
  }

  alternative <- match.arg(alternative)

  x <- stats::na.omit(x)
  n <- length(x)
  x_bar <- mean(x)
  s <- sd(x)

  use_z <- !is.null(sigma)
  se <- if (use_z) sigma / sqrt(n) else s / sqrt(n)
  stat_value <- (x_bar - mu) / se

  # Critical value
  if (use_z) {
    q_func <- qnorm
  } else {
    q_func <- function(p) qt(p, df = n - 1)
  }

  if (alternative == "two.sided") {
    crit <- q_func(1 - (1 - conf_level) / 2)
  } else {
    crit <- q_func(conf_level)
  }

  # p-value
  if (use_z) {
    p_func <- pnorm
  } else {
    p_func <- function(q) pt(q, df = n - 1)
  }
  p_value <- switch(alternative,
    two.sided = 2 * (1 - p_func(abs(stat_value))),
    less = p_func(stat_value),
    greater = 1 - p_func(stat_value)
  )

  dist_type <- if (use_z) "norm" else "t"
  df_val <- if (use_z) NULL else n - 1
  stat_label <- if (use_z) "Z" else "t"

  # Rejection region plot
  p_curve <- plot_hypothesis_curve(
    dist = dist_type,
    stat_value = stat_value,
    critical_value = crit,
    alternative = alternative,
    alpha = 1 - conf_level,
    df = df_val,
    stat_label = stat_label,
    p_value = p_value,
    theme = theme
  ) + ggplot2::labs(title = "Rejection Region")

  # Boxplot
  p_box <- plot_hypothesis_box(
    x = x, mu = mu, sigma = sigma,
    alternative = alternative,
    conf_level = conf_level,
    show_table = FALSE,
    theme = theme
  ) + ggplot2::labs(title = "Sample Data")

  # Combine
  p_display <- if (p_value < 0.001) "<0.001" else sprintf("%.4f", p_value)

  subtitle = bquote(H[0] * ": " * mu ~ "=" ~ .(mu) ~ " | " ~ .(stat_label) ~ " = " ~ .(round(stat_value, 2)) ~ " | " ~ P ~ " = " ~ .(p_display))

  p_combined <- p_curve + p_box +
    patchwork::plot_layout(ncol = 2, widths = c(1, 1)) +
    patchwork::plot_annotation(
      title = "Hypothesis Test Results",
      # subtitle = sprintf("H0: mu = %s | %s = %.2f | P = %s",
      #                    mu, stat_label, stat_value,
      #                    if (p_value < 0.001) "<0.001" else sprintf("%.4f", p_value)),
      subtitle = subtitle,
      theme = as_iqr_theme(theme)
    )

  p_combined
}

