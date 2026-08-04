# =============================================================================
# File: R/plot_capability_forest.R
# Description: Forest-plot visualisations for capability analysis — replaces
#   the previous bar-chart presentations (plot_capability_index_bar /
#   plot_capability_ppm_bar) with a Cleveland dot-plot / forest-plot style:
#   point estimate + horizontal CI whiskers + vertical threshold reference
#   lines + semantic colouring (pass/watch/fail). All colours resolved via
#   .iqr_aes(), no hard-coding.
# =============================================================================

#' Capability Indices Forest Plot
#'
#' Draws a horizontal forest plot (Cleveland dot-plot style) of capability
#' indices (Cp, Cpk, Pp, Ppk, ...). Each index is shown as a point with a
#' horizontal bootstrap-CI whisker; vertical reference lines at 1.00, 1.33
#' and 1.67 allow instant capability verdict at a glance. Points are coloured
#' by semantic role (pass / watch / fail) based on the index value.
#'
#' @param indices data.frame with columns:
#'   \itemize{
#'     \item \code{name} — index label (e.g. "Cp", "Cpk")
#'     \item \code{value} — point estimate (numeric)
#'     \item \code{lower} — CI lower bound (numeric, may be NA)
#'     \item \code{upper} — CI upper bound (numeric, may be NA)
#'   }
#' @param thresholds Named numeric vector of vertical reference lines.
#'   Default: \code{c("1.00" = 1.00, "1.33" = 1.33, "1.67" = 1.67)}.
#' @param pass_threshold Numeric. Values >= this are "pass" (default 1.33).
#' @param watch_threshold Numeric. Values >= this but < pass_threshold are
#'   "watch" (default 1.00). Below this is "fail".
#' @param show_values Logical. If TRUE (default), annotate each point with
#'   the numeric value and CI.
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   name  = c("Cp", "Cpk", "Pp", "Ppk"),
#'   value = c(1.45, 1.20, 1.50, 1.25),
#'   lower = c(1.30, 1.05, 1.35, 1.10),
#'   upper = c(1.60, 1.35, 1.65, 1.40)
#' )
#' plot_capability_index_forest(df)
#' }
plot_capability_index_forest <- function(indices, thresholds = NULL,
                                         pass_threshold = 1.33,
                                         watch_threshold = 1.00,
                                         show_values = TRUE,
                                         theme = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }
  required <- c("name", "value", "lower", "upper")
  missing <- required[!required %in% names(indices)]
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  c <- .iqr_aes(theme)

  # Semantic classification
  verdict <- vapply(indices$value, function(v) {
    if (is.na(v)) "neutral"
    else if (v >= pass_threshold) "pass"
    else if (v >= watch_threshold) "watch"
    else "fail"
  }, character(1))
  indices$verdict <- factor(verdict,
                            levels = c("pass", "watch", "fail", "neutral"))
  # Preserve caller order (don't sort — Cp/Cpk/Pp/Ppk has a natural order)
  indices$name <- factor(indices$name, levels = indices$name)

  if (is.null(thresholds)) {
    thresholds <- c("1.00" = 1.00, "1.33" = 1.33, "1.67" = 1.67)
  }
  thresh_df <- data.frame(
    x = unname(thresholds),
    label = names(thresholds)
  )

  # x-axis range: cover all CI whiskers + reference lines
  all_vals <- c(indices$value, indices$lower, indices$upper, unname(thresholds))
  all_vals <- all_vals[is.finite(all_vals)]
  x_min <- min(all_vals, na.rm = TRUE) * 0.9
  x_max <- max(all_vals, na.rm = TRUE) * 1.1

  p <- ggplot2::ggplot(indices, ggplot2::aes(x = .data$value, y = .data$name)) +
    # Threshold reference lines (vertical, since we coord_flip)
    ggplot2::geom_vline(data = thresh_df,
                        ggplot2::aes(xintercept = .data$x),
                        color = c$muted, linetype = "dashed", linewidth = 0.5) +
    # Threshold labels (top of plot)
    ggplot2::geom_text(data = thresh_df,
                       ggplot2::aes(x = .data$x, y = Inf, label = .data$label),
                       vjust = 1.5, hjust = -0.1, color = c$muted, size = 3,
                       inherit.aes = FALSE) +
    # CI whiskers
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$lower, xmax = .data$upper, color = .data$verdict),
      height = 0.2, linewidth = 1.2, na.rm = TRUE
    ) +
    # Point estimates
    ggplot2::geom_point(
      ggplot2::aes(color = .data$verdict),
      size = 3.5, shape = 18
    ) +
    ggplot2::scale_color_manual(
      values = c("pass" = c$success, "watch" = c$warning,
                 "fail" = c$fail, "neutral" = c$muted),
      guide = "none"
    )

  # Annotate values next to each point
  if (show_values && nrow(indices) > 0) {
    label_txt <- vapply(seq_len(nrow(indices)), function(i) {
      v <- indices$value[i]; lo <- indices$lower[i]; hi <- indices$upper[i]
      if (is.na(v)) "NA"
      else if (is.na(lo) || is.na(hi)) sprintf("%.3f", v)
      else sprintf("%.3f  [%.3f, %.3f]", v, lo, hi)
    }, character(1))
    annot_df <- data.frame(
      name = indices$name,
      label = label_txt,
      x = x_max
    )
    p <- p + ggplot2::geom_text(
      data = annot_df,
      ggplot2::aes(x = .data$x, y = .data$name, label = .data$label),
      hjust = 1.05, color = c$text, size = 3, inherit.aes = FALSE
    )
  }

  p + ggplot2::labs(
    title = "Capability Indices",
    subtitle = sprintf("Thresholds: %s (pass) | %s (watch) | < %s (fail)",
                       pass_threshold, watch_threshold, watch_threshold),
    x = "Index value", y = ""
  ) +
    ggplot2::coord_cartesian(xlim = c(x_min, x_max)) +
    as_iqr_theme(theme)
}

#' PPM Performance Forest Plot
#'
#' Draws a horizontal forest plot of PPM (parts-per-million) performance
#' metrics: observed PPM, expected (within) PPM, expected (overall) PPM, etc.
#' Each metric is a point with an optional CI whisker; a vertical target-PPM
#' reference line is drawn when supplied.
#'
#' @param ppm data.frame with columns:
#'   \itemize{
#'     \item \code{name} — metric label (e.g. "Observed", "Expected (Overall)")
#'     \item \code{value} — PPM point estimate
#'     \item \code{lower} — CI lower (may be NA)
#'     \item \code{upper} — CI upper (may be NA)
#'     \item \code{role} — optional semantic role: "observed" / "expected" /
#'       "target". When present, drives the point colour.
#'   }
#' @param target_ppm Optional numeric. Draws a vertical reference line at this
#'   PPM value.
#' @param show_values Logical. Annotate numeric values (default TRUE).
#' @param theme Theme spec.
#' @return A ggplot object.
#' @export
plot_capability_ppm_forest <- function(ppm, target_ppm = NULL,
                                        show_values = TRUE, theme = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }
  required <- c("name", "value", "lower", "upper")
  missing <- required[!required %in% names(ppm)]
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  c <- .iqr_aes(theme)

  # Default role if not provided
  if (!"role" %in% names(ppm)) {
    ppm$role <- "observed"
  }
  ppm$name <- factor(ppm$name, levels = ppm$name)

  # Role -> colour mapping (not pass/fail — role-based: observed=neutral,
  # expected=warning, target=success; keeps the "information role" semantics)
  role_colors <- c(
    "observed" = c$neutral,
    "expected" = c$warning,
    "expected_within" = c$primary,
    "expected_overall" = c$warning,
    "target" = c$success
  )

  # x-axis range
  all_vals <- c(ppm$value, ppm$lower, ppm$upper, target_ppm)
  all_vals <- all_vals[is.finite(all_vals)]
  x_max <- max(all_vals, na.rm = TRUE) * 1.25
  x_min <- 0

  p <- ggplot2::ggplot(ppm, ggplot2::aes(x = .data$value, y = .data$name)) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$lower, xmax = .data$upper, color = .data$role),
      height = 0.2, linewidth = 1.2, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$role),
      size = 3.5, shape = 18
    ) +
    ggplot2::scale_color_manual(values = role_colors, guide = "none")

  # Target reference line
  if (!is.null(target_ppm) && is.finite(target_ppm)) {
    p <- p + ggplot2::geom_vline(xintercept = target_ppm,
                                 color = c$success, linetype = "dotted",
                                 linewidth = 0.8) +
      ggplot2::annotate("text", x = target_ppm, y = Inf,
                        label = sprintf("Target = %s", .fmt_spec(target_ppm)),
                        vjust = 1.5, hjust = -0.1, color = c$success, size = 3)
  }

  # Annotate values
  if (show_values && nrow(ppm) > 0) {
    label_txt <- vapply(seq_len(nrow(ppm)), function(i) {
      v <- ppm$value[i]; lo <- ppm$lower[i]; hi <- ppm$upper[i]
      if (is.na(v)) "NA"
      else if (is.na(lo) || is.na(hi)) formatC(v, format = "f", digits = 0)
      else sprintf("%s [%s, %s]",
                   formatC(v, format = "f", digits = 0),
                   formatC(lo, format = "f", digits = 0),
                   formatC(hi, format = "f", digits = 0))
    }, character(1))
    annot_df <- data.frame(
      name = ppm$name,
      label = label_txt,
      x = x_max
    )
    p <- p + ggplot2::geom_text(
      data = annot_df,
      ggplot2::aes(x = .data$x, y = .data$name, label = .data$label),
      hjust = 1.05, color = c$text, size = 3, inherit.aes = FALSE
    )
  }

  p + ggplot2::labs(
    title = "PPM Performance",
    subtitle = "Points = estimate | Whiskers = CI | Dotted = target",
    x = "PPM", y = ""
  ) +
    ggplot2::coord_cartesian(xlim = c(x_min, x_max)) +
    as_iqr_theme(theme)
}
