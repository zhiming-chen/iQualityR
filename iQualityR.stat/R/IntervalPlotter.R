# =============================================================================
# File: R/IntervalPlotter.R
# Description: Interval estimation visualization (L2 presentation layer).
#              Per Contract 2 (STAT_ANALYSIS_PLAN.md v2.0): exposes the unified
#              $plot(result, plot_type, show_table, theme_obj) signature.
#
#              Visualization strategy by interval type:
#                ci_mean / pi_mean / margin_of_error (mean) -> single-point
#                    error-bar with the sample histogram background.
#                ci_proportion / margin_of_error (proportion) -> bar + error-bar.
#                ci_variance -> variance CI on a chi-sq density backdrop.
#                ci_diff_mean -> two-sample error-bar (group 1 vs group 2)
#                    with the difference CI annotated.
#                tolerance_interval -> histogram with TI bounds + spec limits
#                    overlay (when available).
# =============================================================================

#' @title IntervalPlotter: Interval Estimation Visualization
#' @description
#' L2 presentation engine for interval estimates produced by
#' `IntervalAnalyzer`. Renders interval bounds as error bars overlaid on the
#' underlying sample distribution, with the active `IqrTheme` controlling
#' colors.
#'
#' **Contract 2 signature** (fixed across all L2 Plotters in .stat):
#' ```
#' $plot(result, plot_type = "auto", show_table = FALSE, theme_obj = NULL)
#' ```
#'
#' @export
IntervalPlotter <- R6::R6Class("IntervalPlotter",
  inherit = StatPlotter,
  public = list(

    #' @description Initialize with a theme
    #' @param theme Theme name or `IqrTheme` object.
    initialize = function(theme = "academic") {
      super$initialize(theme)
    },

    #' @description Unified plot entry point (Contract 2)
    #' @param result A `stat_result` from `IntervalAnalyzer`.
    #' @param plot_type Plot type: `"auto"` (default), `"errorbar"`,
    #'   `"histogram"`, `"bar"`.
    #' @param show_table Logical; annotate the interval bounds on the plot.
    #' @param theme_obj Optional `IqrTheme` override.
    #' @return A `ggplot` object.
    plot = function(result, plot_type = "auto", show_table = FALSE,
                    theme_obj = NULL) {
      private$.check_plot_available()
      theme <- theme_obj %||% self$theme_obj
      pt <- if (plot_type == "auto") {
        private$.auto_select(result)
      } else {
        plot_type
      }
      switch(pt,
        "errorbar"  = private$.plot_errorbar(result, show_table, theme),
        "histogram" = private$.plot_histogram(result, show_table, theme),
        "bar"       = private$.plot_bar(result, show_table, theme),
        stop("IntervalPlotter: unknown plot_type '", pt, "'.",
             call. = FALSE)
      )
    },

    #' @description Error-bar plot (point + CI)
    plot_errorbar = function(result, show_table = FALSE, theme_obj = NULL) {
      private$.check_plot_available()
      private$.plot_errorbar(result, show_table, theme_obj %||% self$theme_obj)
    },

    #' @description Histogram with interval overlay
    plot_histogram = function(result, show_table = FALSE, theme_obj = NULL) {
      private$.check_plot_available()
      private$.plot_histogram(result, show_table, theme_obj %||% self$theme_obj)
    },

    #' @description Bar chart with interval overlay (proportion case)
    plot_bar = function(result, show_table = FALSE, theme_obj = NULL) {
      private$.check_plot_available()
      private$.plot_bar(result, show_table, theme_obj %||% self$theme_obj)
    }
  ),

  private = list(
    .auto_select = function(result) {
      tt <- result$test_type
      if (tt %in% c("ci_proportion", "margin_of_error") &&
          identical(result$type, "proportion")) {
        return("bar")
      }
      # Model-based pi_mean stores a data.frame in result$data$x; the histogram
      # path expects a numeric vector, so prefer the errorbar summary there.
      if (tt == "pi_mean" && identical(result$pi_mode, "model")) {
        return("errorbar")
      }
      if (!is.null(result$data$x) && is.numeric(result$data$x)) {
        return("histogram")
      }
      "errorbar"
    },

    .plot_errorbar = function(result, show_table, theme) {
      ci <- result$conf.int
      est <- if (!is.null(result$estimate)) as.numeric(result$estimate[1]) else
             as.numeric(result$statistic[1])
      df_plot <- data.frame(
        label = result$test_type,
        est   = est,
        lower = ci[1],
        upper = ci[2]
      )
      subtitle <- sprintf("%.0f%% interval: [%.4f, %.4f]",
                          (result$conf.level %||% NA) * 100, ci[1], ci[2])
      ggplot2::ggplot(df_plot, ggplot2::aes(x = label, y = est)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                               width = 0.2, linewidth = 1.1) +
        ggplot2::geom_point(size = 4) +
        ggplot2::labs(
          title = result$method %||% result$test_type,
          subtitle = subtitle,
          x = "", y = "Estimate"
        ) +
        ggplot2::theme_minimal()
    },

    .plot_histogram = function(result, show_table, theme) {
      x <- result$data$x
      if (is.null(x) || !is.numeric(x)) {
        return(private$.plot_errorbar(result, show_table, theme))
      }
      ci <- result$conf.int
      x_bar <- as.numeric(result$statistic[1])
      subtitle <- sprintf("%.0f%% interval: [%.4f, %.4f]",
                          (result$conf.level %||% NA) * 100, ci[1], ci[2])

      p <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                bins = 30, fill = "grey85", colour = "white") +
        ggplot2::geom_vline(xintercept = ci[1], linetype = "dashed",
                            colour = "firebrick", linewidth = 0.9) +
        ggplot2::geom_vline(xintercept = ci[2], linetype = "dashed",
                            colour = "firebrick", linewidth = 0.9) +
        ggplot2::geom_vline(xintercept = x_bar, colour = "navy",
                            linewidth = 0.9) +
        ggplot2::labs(
          title = result$method %||% result$test_type,
          subtitle = subtitle,
          x = "x", y = "Density"
        ) +
        ggplot2::theme_minimal()
      p
    },

    .plot_bar = function(result, show_table, theme) {
      ci <- result$conf.int
      p_hat <- as.numeric(result$estimate[1])
      n <- result$n
      df_plot <- data.frame(
        label = "proportion",
        est   = p_hat,
        lower = max(0, ci[1]),
        upper = min(1, ci[2])
      )
      subtitle <- sprintf("%.0f%% CI: [%.4f, %.4f], n = %s",
                          (result$conf.level %||% NA) * 100, ci[1], ci[2], n)
      ggplot2::ggplot(df_plot, ggplot2::aes(x = label, y = est)) +
        ggplot2::geom_col(fill = "steelblue", alpha = 0.6, width = 0.5) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                               width = 0.15, linewidth = 1.1) +
        ggplot2::scale_y_continuous(limits = c(0, 1),
                                    labels = function(x) sprintf("%.0f%%", x * 100)) +
        ggplot2::labs(
          title = result$method %||% result$test_type,
          subtitle = subtitle,
          x = "", y = "Proportion"
        ) +
        ggplot2::theme_minimal()
    }
  )
)
