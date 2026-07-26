# =============================================================================
# File: R/SamplingPlotter.R
# Description: Sampling plan visualization engine (OC curve / power / ASN)
# =============================================================================

#' @title SamplingPlotter: Acceptance Sampling Visualization Engine
#'
#' @description
#' Inherits from [IqrPlotterBase] and generates visualizations for an
#' acceptance sampling plan:
#'
#' - Operating Characteristic (OC) curve
#' - Power curve (single sampling)
#' - Average Sample Number (ASN) curve (double / multiple sampling)
#' - Risk profile (bar chart)
#' - Full panel (combined)
#'
#' @export
SamplingPlotter <- R6::R6Class("SamplingPlotter",
  inherit = IqrPlotterBase,

  public = list(
    #' @description Render sampling plan plots.
    #'
    #' @param results Analysis results list (from [SamplingAnalyzer]).
    #' @param theme_obj IqrTheme object.
    #' @param type Character. Plot type: `"full"`, `"oc"`, `"power"`,
    #'   `"asn"`, `"risk"`.
    #' @return A `ggplot` or `patchwork` object.
    render = function(results, theme_obj, type = "full") {
      if (is.null(results) || length(results) == 0) {
        return(.empty_plot("No results available for plotting."))
      }

      switch(type,
        "full"  = private$.plot_full_panel(results, theme_obj),
        "oc"    = private$.plot_oc_curve(results, theme_obj),
        "power" = private$.plot_power_curve(results, theme_obj),
        "asn"   = private$.plot_asn_curve(results, theme_obj),
        "risk"  = private$.plot_risk_profile(results, theme_obj),
        .empty_plot(paste("Unknown plot type:", type))
      )
    }
  ),

  private = list(

    # -------------------------------------------------------------------------
    # OC curve
    # -------------------------------------------------------------------------

    .plot_oc_curve = function(results, theme_obj) {
      oc <- results$oc_curve
      if (is.null(oc)) return(.empty_plot("OC curve data not available."))

      df <- data.frame(
        p = oc$p_values,
        pa = oc$acceptance_probabilities
      )

      primary <- .safe_primary(theme_obj)
      danger  <- .safe_danger(theme_obj)
      success <- .safe_success(theme_obj)

      p <- ggplot2::ggplot(df, ggplot2::aes(.data[["p"]], .data[["pa"]])) +
        ggplot2::geom_line(color = primary, linewidth = 1.2) +
        ggplot2::geom_hline(yintercept = 0.95, linetype = "dashed",
                            color = success, alpha = 0.7) +
        ggplot2::geom_hline(yintercept = 0.10, linetype = "dashed",
                            color = danger, alpha = 0.7) +
        ggplot2::labs(
          title = "Operating Characteristic (OC) Curve",
          subtitle = "Probability of acceptance vs. lot fraction defective",
          x = "Fraction defective (p)",
          y = "Probability of acceptance"
        )

      kp <- oc$key_points
      if (!is.null(kp)) {
        if (!is.null(kp$aql_point) && !is.na(kp$aql_point$prob)) {
          p <- p + ggplot2::annotate(
            "point", x = kp$aql_point$p, y = kp$aql_point$prob,
            color = success, size = 3
          )
        }
        if (!is.null(kp$rql_point) && !is.na(kp$rql_point$prob)) {
          p <- p + ggplot2::annotate(
            "point", x = kp$rql_point$p, y = kp$rql_point$prob,
            color = danger, size = 3
          )
        }
      }

      p
    },

    # -------------------------------------------------------------------------
    # Power curve
    # -------------------------------------------------------------------------

    .plot_power_curve = function(results, theme_obj) {
      pw <- results$power_analysis
      if (is.null(pw)) return(.empty_plot("Power analysis not available (multi-stage plan)."))

      df <- data.frame(
        p = pw$p_values,
        power = pw$powers
      )

      primary <- .safe_primary(theme_obj)
      danger  <- .safe_danger(theme_obj)

      ggplot2::ggplot(df, ggplot2::aes(.data[["p"]], .data[["power"]])) +
        ggplot2::geom_line(color = primary, linewidth = 1.2) +
        ggplot2::geom_hline(yintercept = pw$target_power,
                            linetype = "dashed", color = danger, alpha = 0.6) +
        ggplot2::labs(
          title = "Power Curve",
          subtitle = paste("Power vs. fraction defective at RQL",
                           "(target power =", pw$target_power, ")"),
          x = "Fraction defective (p)",
          y = "Power (1 - P(accept))"
        )
    },

    # -------------------------------------------------------------------------
    # ASN curve
    # -------------------------------------------------------------------------

    .plot_asn_curve = function(results, theme_obj) {
      asn <- results$asn_curve
      if (is.null(asn)) return(.empty_plot("ASN curve not available (single-stage plan)."))

      df <- data.frame(
        p = asn$p_values,
        asn = asn$asn_values
      )

      primary <- .safe_primary(theme_obj)

      ggplot2::ggplot(df, ggplot2::aes(.data[["p"]], .data[["asn"]])) +
        ggplot2::geom_line(color = primary, linewidth = 1.2) +
        ggplot2::labs(
          title = "Average Sample Number (ASN) Curve",
          subtitle = "Expected sample size vs. fraction defective",
          x = "Fraction defective (p)",
          y = "Average sample number"
        )
    },

    # -------------------------------------------------------------------------
    # Risk profile
    # -------------------------------------------------------------------------

    .plot_risk_profile = function(results, theme_obj) {
      rk <- results$risk_analysis
      if (is.null(rk)) return(.empty_plot("Risk analysis not available."))

      df <- data.frame(
        risk = c("Producer's risk", "Consumer's risk"),
        value = c(rk$producer_risk, rk$consumer_risk),
        target = c(rk$risk_profile$target_alpha,
                   rk$risk_profile$target_beta)
      )

      primary <- .safe_primary(theme_obj)
      danger  <- .safe_danger(theme_obj)

      ggplot2::ggplot(df, ggplot2::aes(.data[["risk"]], .data[["value"]])) +
        ggplot2::geom_col(fill = primary, alpha = 0.8, width = 0.6) +
        ggplot2::geom_point(ggplot2::aes(y = .data[["target"]]),
                            color = danger, size = 4, shape = 18) +
        ggplot2::scale_y_continuous(
          labels = function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")
        ) +
        ggplot2::labs(
          title = "Risk Profile",
          subtitle = "Bar: actual risk; diamond: target",
          x = NULL,
          y = "Risk probability"
        )
    },

    # -------------------------------------------------------------------------
    # Full panel
    # -------------------------------------------------------------------------

    .plot_full_panel = function(results, theme_obj) {
      plots <- list()

      plots$oc <- private$.plot_oc_curve(results, theme_obj)
      plots$risk <- private$.plot_risk_profile(results, theme_obj)
      if (!is.null(results$power_analysis)) {
        plots$power <- private$.plot_power_curve(results, theme_obj)
      }
      if (!is.null(results$asn_curve)) {
        plots$asn <- private$.plot_asn_curve(results, theme_obj)
      }

      patchwork::wrap_plots(plots, ncol = 2) +
        patchwork::plot_annotation(
          title = "Sampling Plan Analysis",
          theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
        )
    }
  )
)
