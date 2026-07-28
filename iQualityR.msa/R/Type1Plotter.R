# =============================================================================
# File: R/Type1Plotter.R
# Description: Type1 gage study and linearity plotter.
# =============================================================================

#' @title Type1 Gage Plotter
#' @description Plotter for Type1 gage study and linearity/bias visualizations.
#'
#' @param study_type Character scalar: `"bias"` or `"linearity"`.
#' @param results Results list produced by [Type1Analyzer] or [Type1LinearityAnalyzer].
#' @param theme_obj ggplot2 theme object.
#' @param type Plot type string.
#' @param colors Character vector of colors.
#' @param ... Additional arguments (ignored).
#'
#' @export
Type1Plotter <- R6::R6Class(
  "Type1Plotter",
  inherit = IqrPlotterBase,
  public = list(
    available_plots = function(study_type = c("bias", "linearity")) {
      study_type <- match.arg(study_type)
      if (study_type == "bias") {
        return(c("summary", "list", "run_chart", "capability", "vda5"))
      }
      c("summary", "list", "linearity", "ref_biases", "repeatability")
    },

    render = function(results, theme_obj, type = "summary", ...) {
      iqr_theme <- if (inherits(theme_obj, "IqrTheme")) theme_obj else NULL
      gg_theme <- if (is.null(iqr_theme)) ggplot2::theme_bw() else iqr_theme$theme_iqr()
      colors <- self$.pal_discrete(iqr_theme %||% IqrTheme$new(), 8)

      study_type <- results$raw_output$study_type
      if (is.null(study_type)) {
        study_type <- if (!is.null(results$data_tables$ref_summary)) "linearity" else "bias"
      }
      type <- self$normalize_type(type, study_type)

      plots <- if (study_type == "bias") {
        list(
          run_chart = self$plot_bias_runchart(results, gg_theme, colors),
          capability = self$plot_capability(results, gg_theme, colors),
          vda5 = self$plot_vda5_uncertainty(results, gg_theme, colors)
        )
      } else {
        list(
          linearity = self$plot_linearity(results, gg_theme, colors),
          ref_biases = self$plot_ref_biases(results, gg_theme, colors),
          repeatability = self$plot_repeatability(results, gg_theme, colors)
        )
      }

      if (type == "list") return(plots)
      if (type == "summary") return(patchwork::wrap_plots(plots, ncol = 1))
      plots[[type]] %||% stop("Unknown Type1 plot type: ", type, call. = FALSE)
    },

    normalize_type = function(type, study_type) {
      aliases <- c(all = "summary", full = "summary", bias = "run_chart", runchart = "run_chart")
      if (type %in% names(aliases)) type <- aliases[[type]]
      valid <- self$available_plots(study_type)
      if (!type %in% valid) {
        stop("Unknown Type1 plot type: ", type, ". Valid types: ", paste(valid, collapse = ", "), call. = FALSE)
      }
      type
    },

    plot_bias_runchart = function(results, theme_obj, colors = NULL) {
      if (is.null(colors)) colors <- .iqr_plotter$.pal_discrete(IqrTheme$new(), 3)
      dt <- data.table::data.table(
        index = seq_along(results$raw_output$measurements),
        measurement = results$raw_output$measurements
      )
      ref_val <- results$statistics$reference_value
      mean_meas <- results$statistics$mean_meas
      bias <- results$statistics$bias
      ci_bias <- results$statistics$ci_bias

      ggplot2::ggplot(dt, ggplot2::aes(x = .data$index, y = .data$measurement)) +
        ggplot2::geom_point(color = colors[[1]], alpha = 0.75, size = 2) +
        ggplot2::geom_line(color = colors[[1]], alpha = 0.55, linewidth = 0.5) +
        ggplot2::geom_hline(yintercept = ref_val, color = colors[[2]], linetype = "dashed", linewidth = 0.8) +
        ggplot2::geom_hline(yintercept = mean_meas, color = colors[[3]], linetype = "solid", linewidth = 0.8) +
        ggplot2::labs(
          title = "Measurement Run Chart",
          subtitle = sprintf("Bias = %.4f (95%% CI: %.4f to %.4f)", bias, ci_bias[1], ci_bias[2]),
          x = "Run order",
          y = "Measurement"
        ) +
        theme_obj
    },

    plot_capability = function(results, theme_obj, colors = NULL) {
      if (is.null(colors)) colors <- .iqr_plotter$.pal_discrete(IqrTheme$new(), 3)
      cap_df <- data.table::data.table(
        metric = c("Cg", "Cgk"),
        value = c(results$statistics$Cg, results$statistics$Cgk)
      )
      k_factor <- results$statistics$k_factor
      tol <- results$statistics$tolerance

      ggplot2::ggplot(cap_df, ggplot2::aes(x = .data$metric, y = .data$value, fill = .data$metric)) +
        ggplot2::geom_col(width = 0.5, alpha = 0.9) +
        ggplot2::geom_hline(yintercept = 1.33, color = colors[[3]], linetype = "dashed", linewidth = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", .data$value)), vjust = -0.3) +
        ggplot2::scale_fill_manual(values = c(Cg = colors[[1]], Cgk = colors[[2]])) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
        ggplot2::labs(
          title = "Gage Capability Indices",
          subtitle = sprintf("k = %.2f, T = %.4f  |  Cg = k*T/(6s), Cgk = (k*T/2 - |Bias|)/(3s)  |  threshold 1.33",
                             k_factor, tol),
          x = NULL,
          y = "Index"
        ) +
        theme_obj +
        ggplot2::theme(legend.position = "none")
    },

    plot_vda5_uncertainty = function(results, theme_obj, colors = NULL) {
      if (is.null(colors)) colors <- .iqr_plotter$.pal_discrete(IqrTheme$new(), 3)
      uc <- results$data_tables$vda5_uncertainty
      uc_filter <- uc[uc$component != "Total", ]

      ggplot2::ggplot(uc_filter, ggplot2::aes(x = stats::reorder(.data$component, -.data$uncertainty), y = .data$percent, fill = .data$component)) +
        ggplot2::geom_col(width = 0.6, alpha = 0.9) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$percent)), vjust = -0.3) +
        ggplot2::scale_fill_manual(values = rep(colors, length.out = nrow(uc_filter))) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
        ggplot2::labs(
          title = "VDA5 Uncertainty Components",
          subtitle = sprintf("Total QMS = %.1f%%", results$statistics$vda5_qms_percent),
          x = "Component",
          y = "Contribution (%)"
        ) +
        theme_obj +
        ggplot2::theme(legend.position = "none")
    },

    plot_linearity = function(results, theme_obj, colors = NULL) {
      if (is.null(colors)) colors <- .iqr_plotter$.pal_discrete(IqrTheme$new(), 3)
      ref_sum <- results$data_tables$ref_summary
      lm_mod <- results$raw_output$lm_model
      pred_data <- data.table::data.table(
        reference = seq(min(ref_sum$reference), max(ref_sum$reference), length.out = 100)
      )
      pred_data$pred_bias <- stats::predict(lm_mod, newdata = pred_data)

      # Individual measurement biases (n_total points). The regression is fit
      # on these, so plotting only the per-reference means would hide the
      # actual scatter that drives R^2 and the slope SE. Overlay them as
      # low-alpha jittered points behind the mean markers.
      bias_long <- results$data_tables$bias_long
      has_individual <- !is.null(bias_long) && nrow(bias_long) > 0

      p <- ggplot2::ggplot(ref_sum, ggplot2::aes(x = .data$reference, y = .data$bias))
      if (has_individual) {
        p <- p +
          ggplot2::geom_point(data = bias_long,
                              ggplot2::aes(x = .data$reference, y = .data$bias),
                              color = colors[[1]], alpha = 0.25, size = 1.4,
                              position = ggplot2::position_jitter(width = 0.05, height = 0, seed = 1))
      }
      p <- p +
        ggplot2::geom_point(size = 3.2, color = colors[[1]], alpha = 0.95,
                            shape = 19) +
        ggplot2::geom_line(data = pred_data,
                           ggplot2::aes(x = .data$reference, y = .data$pred_bias),
                           color = colors[[2]], linewidth = 1.1) +
        ggplot2::geom_hline(yintercept = 0, color = colors[[3]],
                            linetype = "dashed", linewidth = 0.7) +
        ggplot2::labs(
          title = "Gage Linearity Plot",
          subtitle = sprintf("Slope = %.4f, R-squared = %.3f, n = %d (individual biases)",
                             results$statistics$slope,
                             results$statistics$r_squared,
                             results$statistics$n_total),
          x = "Reference value",
          y = "Bias"
        ) +
        theme_obj
      p
    },

    plot_ref_biases = function(results, theme_obj, colors = NULL) {
      if (is.null(colors)) colors <- .iqr_plotter$.pal_discrete(IqrTheme$new(), 3)
      ref_sum <- results$data_tables$ref_summary

      ggplot2::ggplot(ref_sum, ggplot2::aes(x = factor(.data$reference), y = .data$bias, fill = factor(.data$reference))) +
        ggplot2::geom_col(width = 0.6, alpha = 0.9) +
        ggplot2::geom_hline(yintercept = 0, color = colors[[3]], linetype = "dashed", linewidth = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.4f", .data$bias)), vjust = ifelse(ref_sum$bias > 0, -0.3, 1.3)) +
        ggplot2::scale_fill_manual(values = rep(colors, length.out = length(unique(ref_sum$reference)))) +
        ggplot2::labs(
          title = "Bias at Each Reference",
          x = "Reference value",
          y = "Bias"
        ) +
        theme_obj +
        ggplot2::theme(legend.position = "none")
    },

    plot_repeatability = function(results, theme_obj, colors = NULL) {
      if (is.null(colors)) colors <- .iqr_plotter$.pal_discrete(IqrTheme$new(), 3)
      ref_sum <- results$data_tables$ref_summary

      ggplot2::ggplot(ref_sum, ggplot2::aes(x = factor(.data$reference), y = .data$sd, fill = factor(.data$reference))) +
        ggplot2::geom_col(width = 0.6, alpha = 0.9) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.4f", .data$sd)), vjust = -0.3) +
        ggplot2::scale_fill_manual(values = rep(colors, length.out = length(unique(ref_sum$reference)))) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
        ggplot2::labs(
          title = "Repeatability at Each Reference",
          x = "Reference value",
          y = "Standard deviation"
        ) +
        theme_obj +
        ggplot2::theme(legend.position = "none")
    }
  )
)
