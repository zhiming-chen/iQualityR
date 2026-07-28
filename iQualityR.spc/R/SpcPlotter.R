# =============================================================================
# File: R/SpcPlotter.R
# Description: SPC plot generation (inherits IqrPlotterBase)
# =============================================================================

#' @title SpcPlotter
#' @description
#' Plotter for statistical process control charts. Inherits `IqrPlotterBase`
#' and extends `iQualityR.plot::layers_control_chart` with:
#' - Violation point highlighting (red filled circles)
#' - Nelson rule zone bands (1 sigma / 2 sigma / 3 sigma shading)
#' - Phase boundary vertical lines
#' - Center line / UCL / LCL labels
#'
#' @param results Analysis results list returned by `SpcAnalyzer`.
#' @param theme_obj An `IqrTheme` object (may be NULL).
#' @param type Plot type: `"single"` (main chart only), `"full"` (main + dispersion
#'   for variables charts), or `"summary"` (main + rules table).
#' @param plan A `SpcPlan` object providing phase boundaries.
#' @param ... Additional arguments (currently ignored).
#'
#' @export
SpcPlotter <- R6::R6Class("SpcPlotter",
  inherit = IqrPlotterBase,
  public = list(
    #' @description Render SPC control chart(s).
    #' @param results Analysis results list.
    #' @param theme_obj Theme object.
    #' @param type Plot type.
    #' @param plan Plan object.
    #' @param ... Additional arguments.
    render = function(results, theme_obj, type = "full", plan = NULL, ...) {
      chart_type <- results$statistics$chart_type
      points_df <- results$data_tables$points
      if (is.null(points_df) || nrow(points_df) == 0) {
        stop("No point data in results for plotting.", call. = FALSE)
      }

      # Resolve ggplot2 theme and IqrTheme object (for toolbox color access)
      gg_theme <- self$.resolve_theme(theme_obj)
      iqr_theme <- self$.resolve_iqr_theme(theme_obj)

      # Main chart
      p_main <- self$.plot_main(points_df, results, gg_theme, iqr_theme, plan)

      if (type == "single") {
        return(p_main)
      }

      if (type == "summary") {
        p_rules <- self$.plot_rules_table(results, gg_theme, iqr_theme)
        return(p_main / p_rules +
          patchwork::plot_annotation(title = self$.chart_title(chart_type)))
      }

      # type == "full": add dispersion chart for variables charts
      if (chart_type %in% c("xbar_r", "xbar_s", "imr", "imr_rs") &&
          !is.null(results$data_tables$dispersion)) {
        p_disp <- self$.plot_dispersion(
          results$data_tables$dispersion, results, gg_theme, iqr_theme,
          chart_type)
        return((p_main / p_disp) +
          patchwork::plot_annotation(title = self$.chart_title(chart_type)))
      }

      p_rules <- self$.plot_rules_table(results, gg_theme, iqr_theme)
      (p_main / p_rules) +
        patchwork::plot_annotation(title = self$.chart_title(chart_type))
    },

    # ---------------------------------------------------------------
    #' @description Resolve ggplot2 theme from theme_obj.
    #' @param theme_obj Theme object.
    #' @return A ggplot2 theme.
    .resolve_theme = function(theme_obj) {
      if (is.null(theme_obj)) {
        return(ggplot2::theme_minimal(base_size = 12))
      }
      if (inherits(theme_obj, "IqrTheme")) {
        return(theme_obj$plot$theme_iqr())
      }
      ggplot2::theme_minimal(base_size = 12)
    },

    #' @description Resolve a valid IqrTheme object for toolbox color access.
    #'   Falls back to the default theme when theme_obj is NULL or not an
    #'   IqrTheme, so the .pal_* / .scale_* helpers always receive a usable
    #'   theme.
    #' @param theme_obj Theme object (may be NULL).
    #' @return An IqrTheme object.
    .resolve_iqr_theme = function(theme_obj) {
      if (inherits(theme_obj, "IqrTheme")) {
        return(theme_obj)
      }
      iQualityR.core::IqrTheme$new(
        theme_style = getOption("iqr.default_theme", "academic"))
    },

    #' @description Lookup chart title by chart type.
    #' @param chart_type Chart type string.
    #' @return Character chart title.
    .chart_title = function(chart_type) {
      titles <- c(
        xbar_r = "Xbar-R Control Chart",
        xbar_s = "Xbar-S Control Chart",
        imr = "I-MR Control Chart",
        imr_rs = "I-MR-R/S Control Chart",
        p = "P Chart (Proportion Defective)",
        np = "NP Chart (Number Defective)",
        u = "U Chart (Defects per Unit)",
        c = "C Chart (Count of Defects)",
        p_laney = "Laney P' Chart",
        u_laney = "Laney U' Chart",
        ewma = "EWMA Control Chart",
        cusum = "CUSUM Control Chart (Tabular)",
        ma = "Moving Average Control Chart",
        t2 = "Hotelling T2 Control Chart",
        mewma = "MEWMA Control Chart",
        g = "G Chart (Geometric)",
        t = "T Chart (Time Between Events)",
        adaptive = "Adaptive Rolling-Window Control Chart",
        arima_resid = "ARIMA Residual Control Chart",
        aewma = "Adaptive EWMA Control Chart",
        changepoint = "Change-Point Detection Chart",
        kde = "KDE Nonparametric Control Chart",
        t2_mewma = "T2 + MEWMA Hybrid Chart",
        lstm = "LSTM Anomaly Detection Chart",
        autoencoder = "Autoencoder Anomaly Detection Chart",
        iforest = "Isolation Forest Anomaly Detection Chart",
        bocpd = "Bayesian Online Change Point Detection Chart"
      )
      titles[chart_type] %||% "SPC Control Chart"
    },

    #' @description Build main control chart.
    #' @param points_df Points data frame.
    #' @param results Results list.
    #' @param gg_theme ggplot2 theme.
    #' @param iqr_theme IqrTheme object (for toolbox color access).
    #' @param plan Plan object.
    #' @return A ggplot object.
    .plot_main = function(points_df, results, gg_theme, iqr_theme, plan) {
      center <- results$statistics$center
      ucl <- results$statistics$ucl
      lcl <- results$statistics$lcl
      sigma <- results$statistics$sigma
      chart_type <- results$statistics$chart_type

      # Resolve SPC colors from the IqrPlotterBase toolbox singleton.
      # Mapping: UCL/LCL & violations -> fail (red), CL -> muted (neutral),
      # data line/points -> discrete[1] (primary), 2-sigma band -> watch
      # (yellow), 1-sigma band -> good (green).
      col_fail   <- .iqr_plotter$.pal_semantic(iqr_theme, "fail")
      col_cl     <- .iqr_plotter$.pal_ui(iqr_theme, "muted")
      col_data   <- .iqr_plotter$.pal_discrete(iqr_theme)[1]
      col_watch  <- .iqr_plotter$.pal_semantic(iqr_theme, "watch")
      col_good   <- .iqr_plotter$.pal_semantic(iqr_theme, "good")

      # Identify violation indices for highlighting
      viol_df <- results$data_tables$violations
      viol_idx <- if (!is.null(viol_df) && nrow(viol_df) > 0) {
        unique(unlist(lapply(strsplit(viol_df$indices, ","), function(s) {
          as.integer(s[s != ""])
        })))
      } else integer(0)
      points_df$is_violation <- seq_len(nrow(points_df)) %in% viol_idx

      # Build base plot data with required columns for layers_control_chart
      plot_data <- data.frame(
        x = points_df$index,
        y = points_df$value,
        cl = points_df$cl,
        ucl = points_df$ucl,
        lcl = points_df$lcl,
        stringsAsFactors = FALSE
      )

      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
        # Zone bands (1 sigma / 2 sigma)
        ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf,
          ymin = center - 2 * sigma, ymax = center + 2 * sigma),
          fill = col_watch, alpha = 0.3, inherit.aes = FALSE) +
        ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf,
          ymin = center - sigma, ymax = center + sigma),
          fill = col_good, alpha = 0.3, inherit.aes = FALSE) +
        # Control limit and center lines
        ggplot2::geom_hline(yintercept = ucl, linetype = "dashed",
          color = col_fail, linewidth = 0.8) +
        ggplot2::geom_hline(yintercept = lcl, linetype = "dashed",
          color = col_fail, linewidth = 0.8) +
        ggplot2::geom_hline(yintercept = center, color = col_cl,
          linewidth = 0.8) +
        # Data line and points
        ggplot2::geom_line(color = col_data, linewidth = 0.6) +
        ggplot2::geom_point(color = col_data, size = 2) +
        # Highlight violations
        ggplot2::geom_point(data = subset(points_df, is_violation),
          ggplot2::aes(x = index, y = value),
          color = col_fail, size = 3.5, shape = 19) +
        # Phase boundaries
        self$.phase_geom_vline(plan, iqr_theme) +
        # Labels for UCL / CL / LCL
        ggplot2::annotate("text", x = max(plot_data$x), y = ucl,
          label = sprintf("UCL = %.3f", ucl), hjust = 1.05, vjust = -0.5,
          size = 3, color = col_fail) +
        ggplot2::annotate("text", x = max(plot_data$x), y = lcl,
          label = sprintf("LCL = %.3f", lcl), hjust = 1.05, vjust = 1.5,
          size = 3, color = col_fail) +
        ggplot2::annotate("text", x = max(plot_data$x), y = center,
          label = sprintf("CL = %.3f", center), hjust = 1.05, vjust = -0.5,
          size = 3, color = col_cl) +
        ggplot2::labs(
          title = self$.chart_title(chart_type),
          subtitle = sprintf("Sigma = %.4f (%s), Violations = %d",
            sigma, results$statistics$sigma_method,
            results$statistics$n_violations),
          x = "Subgroup / Observation Index",
          y = self$.y_label(chart_type)) +
        gg_theme

      p
    },

    #' @description Build dispersion (R/S/MR) chart for variables charts.
    #' @param disp_df Dispersion data frame.
    #' @param results Results list.
    #' @param gg_theme ggplot2 theme.
    #' @param iqr_theme IqrTheme object (for toolbox color access).
    #' @param chart_type Chart type string.
    #' @return A ggplot object.
    .plot_dispersion = function(disp_df, results, gg_theme, iqr_theme,
                                chart_type) {
      plot_data <- data.frame(
        x = disp_df$index, y = disp_df$value,
        cl = disp_df$cl, ucl = disp_df$ucl, lcl = disp_df$lcl,
        stringsAsFactors = FALSE)
      disp_title <- if (chart_type == "xbar_r") "R Chart (Range)"
                    else if (chart_type == "xbar_s") "S Chart (Std Dev)"
                    else "Moving Range Chart"
      # Resolve SPC colors from the IqrPlotterBase toolbox singleton.
      col_fail <- .iqr_plotter$.pal_semantic(iqr_theme, "fail")
      col_cl   <- .iqr_plotter$.pal_ui(iqr_theme, "muted")
      col_data <- .iqr_plotter$.pal_discrete(iqr_theme)[1]
      ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_hline(yintercept = unique(plot_data$ucl)[1],
          linetype = "dashed", color = col_fail, linewidth = 0.8) +
        ggplot2::geom_hline(yintercept = unique(plot_data$lcl)[1],
          linetype = "dashed", color = col_fail, linewidth = 0.8) +
        ggplot2::geom_hline(yintercept = unique(plot_data$cl)[1],
          color = col_cl, linewidth = 0.8) +
        ggplot2::geom_line(color = col_data, linewidth = 0.6) +
        ggplot2::geom_point(color = col_data, size = 2) +
        ggplot2::labs(title = disp_title,
          x = "Subgroup Index", y = disp_title) +
        gg_theme
    },

    #' @description Build Nelson rules violation summary bar chart.
    #' @param results Results list.
    #' @param gg_theme ggplot2 theme.
    #' @param iqr_theme IqrTheme object (for toolbox color access).
    #' @return A ggplot object.
    .plot_rules_table = function(results, gg_theme, iqr_theme) {
      rules_df <- results$data_tables$rules_summary
      if (is.null(rules_df) || nrow(rules_df) == 0) {
        return(ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
            label = "No Nelson rules triggered.\nProcess appears in control.",
            hjust = 0.5, vjust = 0.5, size = 5) +
          ggplot2::theme_void() +
          ggplot2::labs(title = "Nelson Rules Summary"))
      }
      rules_df$rule <- factor(rules_df$rule, levels = rules_df$rule)
      # Warning-color bars: violations are a caution indicator.
      col_warning <- .iqr_plotter$.pal_ui(iqr_theme, "warning")
      ggplot2::ggplot(rules_df, ggplot2::aes(x = rule, y = n_violations)) +
        ggplot2::geom_col(fill = col_warning, alpha = 0.85) +
        ggplot2::geom_text(ggplot2::aes(label = n_violations),
          vjust = -0.5, size = 3.5) +
        ggplot2::labs(title = "Nelson Rules Violations",
          x = "Rule", y = "Number of Violations") +
        gg_theme
    },

    #' @description Build phase boundary vertical line geom.
    #' @param plan Plan object with phase_boundaries.
    #' @param iqr_theme IqrTheme object (for toolbox color access).
    #' @return A ggplot2 layer.
    .phase_geom_vline = function(plan, iqr_theme) {
      if (is.null(plan) || is.null(plan$phase_boundaries)) {
        return(ggplot2::geom_vline(xintercept = c(), alpha = 0))
      }
      boundaries <- plan$phase_boundaries
      # Phase boundaries are structural reference lines — use muted UI color.
      col_phase <- .iqr_plotter$.pal_ui(iqr_theme, "muted")
      ggplot2::geom_vline(xintercept = boundaries, linetype = "dotdash",
        color = col_phase, linewidth = 0.6, alpha = 0.6)
    },

    #' @description Lookup y-axis label by chart type.
    #' @param chart_type Chart type string.
    #' @return Character y-axis label.
    .y_label = function(chart_type) {
      labels <- c(
        xbar_r = "Subgroup Mean", xbar_s = "Subgroup Mean",
        imr = "Individual Value", imr_rs = "Individual Value",
        p = "Proportion", np = "Count",
        u = "Defects per Unit", c = "Count of Defects",
        p_laney = "Proportion (Laney)", u_laney = "Defects/Unit (Laney)",
        ewma = "EWMA Statistic", cusum = "CUSUM (positive)",
        ma = "Moving Average",
        t2 = "Hotelling T2", mewma = "MEWMA Statistic",
        g = "Opportunities Between Events",
        t = "Time Between Events",
        adaptive = "Individual Value (rolling)",
        arima_resid = "ARIMA Residual",
        aewma = "Adaptive EWMA",
        changepoint = "Statistic with change-points",
        kde = "Individual Value",
        t2_mewma = "T2 / MEWMA statistic"
      )
      labels[chart_type] %||% "Value"
    }
  )
)
