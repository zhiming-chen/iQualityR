# =============================================================================
# File: R/plot_capability_qq_ci.R
# Description: Normal Q-Q plot with 95% confidence-interval band (filled).
#   Extends plot_capability_qq() by adding a semi-transparent CI ribbon around
#   the reference line, computed from the standard error of order statistics
#   under normality. AD statistic and p-value are shown in the subtitle.
# =============================================================================

#' Normal Q-Q Plot with 95% CI Band
#'
#' Draws a Normal Q-Q plot with a 95% confidence-interval band (semi-transparent
#' filled ribbon) around the reference line. Uses the standard error of order
#' statistics under normality: SE(z_i) = sqrt(p_i*(1-p_i)/n) / phi(z_i), where
#' p_i = (i - 0.5)/n and phi is the standard normal density.
#'
#' @param values Numeric vector of measurements.
#' @param ad_stat Optional Anderson-Darling statistic. If provided, shown in subtitle.
#' @param ad_p Optional Anderson-Darling p-value. If provided, shown in subtitle.
#' @param subtitle_text Optional subtitle text. When provided, overrides the AD annotation.
#' @param ci_level Numeric. Confidence level (default 0.95).
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_qq_ci <- function(values, ad_stat = NULL, ad_p = NULL,
                                   subtitle_text = NULL, ci_level = 0.95,
                                   theme = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }
  c <- .iqr_aes(theme)

  x <- sort(values[!is.na(values)])
  n <- length(x)
  if (n < 3) stop("Need at least 3 non-NA values for Q-Q plot.")

  # Probability points and theoretical quantiles
  p_i <- (seq_len(n) - 0.5) / n
  z_i <- stats::qnorm(p_i)

  # Standard error of order statistics under normality
  # SE(z_i) = sqrt(p_i * (1 - p_i) / n) / phi(z_i)
  phi_z <- stats::dnorm(z_i)
  se_z <- sqrt(p_i * (1 - p_i) / n) / pmax(phi_z, .Machine$double.eps)

  # CI bounds (theoretical quantile ± z_{alpha/2} * SE)
  z_alpha <- stats::qnorm(1 - (1 - ci_level) / 2)
  ci_lower <- z_i - z_alpha * se_z
  ci_upper <- z_i + z_alpha * se_z

  # Sample mean and SD for reference line (y = mean + sd * z)
  x_mean <- mean(x)
  x_sd <- stats::sd(x)
  ref_line <- x_mean + x_sd * z_i
  ci_lower_y <- x_mean + x_sd * ci_lower
  ci_upper_y <- x_mean + x_sd * ci_upper

  df <- data.frame(
    theoretical = z_i,
    sample = x,
    ci_lower = ci_lower_y,
    ci_upper = ci_upper_y
  )

  # Build subtitle with AD if available
  if (is.null(subtitle_text)) {
    if (!is.null(ad_stat) && !is.null(ad_p)) {
      subtitle_text <- sprintf("AD = %s | p = %s", .fmt_spec(ad_stat), .fmt_spec(ad_p))
    } else {
      subtitle_text <- sprintf("95%% CI band (n = %d)", n)
    }
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$theoretical, y = .data$sample)) +
    # CI ribbon (filled)
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
      fill = c$primary, alpha = 0.15, color = NA
    ) +
    # CI boundary lines (dashed)
    ggplot2::geom_line(ggplot2::aes(y = .data$ci_lower),
                       color = c$primary, linetype = "dashed",
                       linewidth = 0.5, alpha = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = .data$ci_upper),
                       color = c$primary, linetype = "dashed",
                       linewidth = 0.5, alpha = 0.6) +
    # Reference line (y = mean + sd * x)
    ggplot2::geom_abline(intercept = x_mean, slope = x_sd,
                         color = c$muted, linetype = "dashed", linewidth = 0.8) +
    # Data points
    ggplot2::geom_point(color = c$primary, size = 2, alpha = 0.7) +
    ggplot2::labs(
      title = "Normal Q-Q Plot",
      subtitle = subtitle_text,
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    as_iqr_theme(theme)

  p
}
