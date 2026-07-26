# =============================================================================
# File: R/capability/CapabilityPlotter.R
# Description: Capability analysis plot generation (inherits IqrPlotterBase,
#              uses standalone functions from iQualityR.plot)
# =============================================================================

#' CapabilityPlotter
#'
#' @title CapabilityPlotter
#'
#' @description Capability analysis plot executor. Inherits from `IqrPlotterBase`
#'   and uses standalone functions from the `iQualityR.plot` package to build
#'   histograms, Q-Q plots, capability index bar charts, and combined
#'   "Sixpack" views for process capability analysis.
#'
#' @param results Analysis results list returned by `CapabilityAnalyzer`.
#'   Expected to contain `data_tables$raw_data`, `statistics`, and
#'   `diagnostics` elements.
#' @param theme_obj An `IqrTheme` object used to style the generated plots.
#'   May be `NULL`, in which case a default ggplot2 theme is used.
#' @param type Plot type to render. One of:
#'   * `"basic"` - histogram with density and specification limits
#'   * `"qq"` - normal Q-Q plot
#'   * `"full"` - combined "Sixpack" view (histogram, Q-Q, capability bar)
#' @param plan A `CapabilityPlan` object providing specification limits
#'   (`lsl`, `usl`) used by the histogram layers. May be `NULL`.
#' @param ... Additional arguments passed to downstream helpers (currently
#'   unused).
#'
#' @export
CapabilityPlotter <- R6::R6Class("CapabilityPlotter",
  inherit = IqrPlotterBase,
  public = list(
    render = function(results, theme_obj, type = "full", plan = NULL, ...) {
      if (type == "basic") {
        p <- self$.plot_histogram(results, theme_obj, plan)
      } else if (type == "qq") {
        p <- self$.plot_qq(results, theme_obj)
      } else if (type == "full") {
        p_hist <- self$.plot_histogram(results, theme_obj, plan)
        p_qq <- self$.plot_qq(results, theme_obj)
        p_capbar <- self$.plot_capability_bar(results, theme_obj)
        p <- (p_hist | p_qq) / p_capbar +
             patchwork::plot_annotation(title = "Process Capability Sixpack")
      } else {
        stop("Unknown plot type: ", type, call. = FALSE)
      }
      # Each sub-plot already has its theme applied via base_plot() or
      # theme_obj$plot$theme_iqr(); return the assembled plot as-is.
      p
    },

    .plot_histogram = function(results, theme_obj, plan) {
      df <- results$data_tables$raw_data %||% results$raw_data
      p <- iQualityR.plot::base_plot(
             df,
             ggplot2::aes(x = measurement),
             theme = theme_obj
           ) +
           iQualityR.plot::layers_histogram_density(bins = 30) +
           iQualityR.plot::layers_spec_limits(
             lsl = plan$lsl,
             usl = plan$usl
           ) +
           ggplot2::labs(
             title = "Histogram with Density Curve",
             subtitle = sprintf("Cp = %.2f, Cpk = %.2f",
                                results$statistics$cp,
                                results$statistics$cpk),
             x = "Measurement",
             y = "Density"
           )
      p
    },

    .plot_qq = function(results, theme_obj) {
      df <- results$data_tables$raw_data %||% results$raw_data
      p <- iQualityR.plot::base_plot(
             df,
             ggplot2::aes(sample = measurement),
             theme = theme_obj
           ) +
           iQualityR.plot::layers_qq(distribution = "norm") +
           ggplot2::labs(
             title = "Normal Q-Q Plot",
             subtitle = sprintf("Shapiro-Wilk p-value = %.4f",
                                results$diagnostics$normality_p_value),
             x = "Theoretical Quantiles",
             y = "Sample Quantiles"
           )
      p
    },

    .plot_capability_bar = function(results, theme_obj) {
      df_indices <- data.frame(
        Index = c("Cp", "Cpk", "Pp", "Ppk"),
        Value = c(results$statistics$cp, results$statistics$cpk,
                  results$statistics$pp, results$statistics$ppk)
      )
      # Resolve the ggplot2 theme from theme_obj, falling back to
      # theme_minimal() when no IqrTheme is provided.
      gg_theme <- if (is.null(theme_obj)) {
        ggplot2::theme_minimal()
      } else {
        theme_obj$plot$theme_iqr()
      }
      ggplot2::ggplot(df_indices, ggplot2::aes(x = Index, y = Value, fill = Index)) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::geom_hline(yintercept = 1.33, linetype = "dashed",
                            color = "darkgreen", linewidth = 1) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Value)),
                           vjust = -0.5) +
        ggplot2::labs(title = "Capability Indices", y = "Index Value") +
        gg_theme
    }
  )
)
