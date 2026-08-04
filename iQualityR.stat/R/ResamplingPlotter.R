# =============================================================================
# File: R/ResamplingPlotter.R
# Description: Resampling visualization (L2 presentation layer).
#              Per Contract 2 (STAT_ANALYSIS_PLAN.md v2.0): exposes the unified
#              $plot(result, plot_type, show_table, theme_obj) signature.
#
#              Visualization strategy by resampling type:
#                bootstrap_ci     -> histogram / density / QQ of the bootstrap
#                                    replicate distribution with the observed
#                                    statistic and CI bounds overlaid.
#                permutation_test -> histogram / density / QQ of the null
#                                    distribution with the observed statistic
#                                    and the rejection tail(s) shaded.
# =============================================================================

#' @title ResamplingPlotter: Resampling Visualization
#' @description
#' L2 presentation engine for resampling results produced by
#' `ResamplingAnalyzer`. Renders the bootstrap / permutation replicate
#' distribution with the observed statistic and (for bootstrap) CI bounds
#' overlaid. The active `IqrTheme` controls colors.
#'
#' **Contract 2 signature** (fixed across all L2 Plotters in .stat):
#' ```
#' $plot(result, plot_type = "auto", show_table = FALSE, theme_obj = NULL)
#' ```
#'
#' @export
ResamplingPlotter <- R6::R6Class("ResamplingPlotter",
  inherit = StatPlotter,
  public = list(

    #' @description Initialize with a theme
    #' @param theme Theme name or `IqrTheme` object.
    initialize = function(theme = "academic") {
      super$initialize(theme)
    },

    #' @description Unified plot entry point (Contract 2)
    #' @param result A `stat_result` from `ResamplingAnalyzer`.
    #' @param plot_type Plot type: `"auto"` (default, `"hist"`), `"hist"`,
    #'   `"density"`, `"qq"`.
    #' @param show_table Logical; annotate observed statistic / CI / p-value
    #'   on the plot.
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
        "hist"    = private$.plot_hist(result, show_table, theme),
        "density" = private$.plot_density(result, show_table, theme),
        "qq"      = private$.plot_qq(result, show_table, theme),
        stop("ResamplingPlotter: unknown plot_type '", pt, "'.",
             call. = FALSE)
      )
    },

    #' @description Histogram of the replicate distribution
    #' @param result A `stat_result` from `ResamplingAnalyzer`.
    #' @param show_table Logical; annotate observed statistic / CI / p-value.
    #' @param theme_obj Optional `IqrTheme` override.
    #' @return A `ggplot` object.
    plot_hist = function(result, show_table = FALSE, theme_obj = NULL) {
      private$.check_plot_available()
      private$.plot_hist(result, show_table, theme_obj %||% self$theme_obj)
    },

    #' @description Density of the replicate distribution
    #' @param result A `stat_result` from `ResamplingAnalyzer`.
    #' @param show_table Logical; annotate observed statistic / CI / p-value.
    #' @param theme_obj Optional `IqrTheme` override.
    #' @return A `ggplot` object.
    plot_density = function(result, show_table = FALSE, theme_obj = NULL) {
      private$.check_plot_available()
      private$.plot_density(result, show_table, theme_obj %||% self$theme_obj)
    },

    #' @description Normal QQ plot of the replicate distribution
    #' @param result A `stat_result` from `ResamplingAnalyzer`.
    #' @param show_table Logical; annotate observed statistic / CI / p-value.
    #' @param theme_obj Optional `IqrTheme` override.
    #' @return A `ggplot` object.
    plot_qq = function(result, show_table = FALSE, theme_obj = NULL) {
      private$.check_plot_available()
      private$.plot_qq(result, show_table, theme_obj %||% self$theme_obj)
    }
  ),

  private = list(
    .auto_select = function(result) {
      # Histogram is the canonical resampling diagnostic: shows shape,
      # bias and (for bootstrap) the CI bounds in one frame.
      "hist"
    },

    .plot_hist = function(result, show_table, theme) {
      reps  <- result$replicates
      theta <- as.numeric(result$statistic[1])
      tt    <- result$test_type
      df_plot <- data.frame(rep = reps)

      p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$rep)) +
        ggplot2::geom_histogram(
          ggplot2::aes(y = ggplot2::after_stat(density)),
          bins = 30, fill = "steelblue", alpha = 0.55, colour = "white"
        ) +
        ggplot2::geom_vline(xintercept = theta, colour = "firebrick",
                            linewidth = 1.0)

      subtitle <- private$.build_subtitle(result)

      if (tt == "bootstrap_ci" && !is.null(result$conf.int)) {
        ci <- result$conf.int
        if (is.finite(ci[1]))
          p <- p + ggplot2::geom_vline(xintercept = ci[1], linetype = "dashed",
                                       colour = "grey25", linewidth = 0.8)
        if (is.finite(ci[2]))
          p <- p + ggplot2::geom_vline(xintercept = ci[2], linetype = "dashed",
                                       colour = "grey25", linewidth = 0.8)
      }

      p <- p +
        ggplot2::labs(
          title   = result$method %||% result$test_type,
          subtitle = subtitle,
          x = "Replicate statistic", y = "Density"
        ) +
        ggplot2::theme_minimal()

      if (isTRUE(show_table)) p <- private$.annotate_table(p, result)
      p
    },

    .plot_density = function(result, show_table, theme) {
      reps  <- result$replicates
      theta <- as.numeric(result$statistic[1])
      tt    <- result$test_type
      df_plot <- data.frame(rep = reps)

      p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$rep)) +
        ggplot2::geom_density(fill = "steelblue", alpha = 0.35,
                              colour = "steelblue", linewidth = 0.9) +
        ggplot2::geom_vline(xintercept = theta, colour = "firebrick",
                            linewidth = 1.0)

      subtitle <- private$.build_subtitle(result)

      if (tt == "bootstrap_ci" && !is.null(result$conf.int)) {
        ci <- result$conf.int
        if (is.finite(ci[1]))
          p <- p + ggplot2::geom_vline(xintercept = ci[1], linetype = "dashed",
                                       colour = "grey25", linewidth = 0.8)
        if (is.finite(ci[2]))
          p <- p + ggplot2::geom_vline(xintercept = ci[2], linetype = "dashed",
                                       colour = "grey25", linewidth = 0.8)
      }

      p <- p +
        ggplot2::labs(
          title   = result$method %||% result$test_type,
          subtitle = subtitle,
          x = "Replicate statistic", y = "Density"
        ) +
        ggplot2::theme_minimal()

      if (isTRUE(show_table)) p <- private$.annotate_table(p, result)
      p
    },

    .plot_qq = function(result, show_table, theme) {
      reps  <- result$replicates
      theta <- as.numeric(result$statistic[1])
      qq <- stats::qqnorm(reps, plot.it = FALSE)
      df_plot <- data.frame(theoretical = qq$x, sample = qq$y)

      p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$theoretical,
                                                 y = .data$sample)) +
        ggplot2::geom_point(colour = "steelblue", alpha = 0.6, size = 1.8) +
        ggplot2::geom_abline(slope = stats::sd(reps),
                             intercept = mean(reps),
                             colour = "firebrick", linewidth = 0.9) +
        ggplot2::labs(
          title    = result$method %||% result$test_type,
          subtitle = private$.build_subtitle(result),
          x = "Theoretical Normal Quantiles",
          y = "Replicate Statistic Quantiles"
        ) +
        ggplot2::theme_minimal()

      p
    },

    .build_subtitle = function(result) {
      theta <- as.numeric(result$statistic[1])
      tt <- result$test_type
      if (tt == "bootstrap_ci") {
        ci <- result$conf.int
        cl <- (result$conf.level %||% NA) * 100
        lo <- if (is.infinite(ci[1])) "-Inf" else sprintf("%.4f", ci[1])
        hi <- if (is.infinite(ci[2]))  "Inf" else sprintf("%.4f", ci[2])
        sprintf("observed = %.4f | %.0f%% CI [%s, %s] | bias = %.4f, se = %.4f",
                theta, cl, lo, hi, result$boot_bias %||% NA,
                result$boot_se %||% NA)
      } else {
        sprintf("observed = %.4f | p = %s | R = %d",
                theta, private$.fmt_p(result$p.value), result$R)
      }
    },

    .fmt_p = function(p) {
      if (is.na(p)) "NA"
      else if (p < 1e-4) "<1e-04"
      else sprintf("%.4f", p)
    },

    .annotate_table = function(p, result) {
      # Lightweight text annotation in the upper-right corner summarizing the
      # key numbers. Kept as geom_label so it renders without extra deps.
      theta <- as.numeric(result$statistic[1])
      tt <- result$test_type
      if (tt == "bootstrap_ci") {
        ci <- result$conf.int
        cl <- (result$conf.level %||% NA) * 100
        lab <- sprintf("statistic = %.4f\n%.0f%% CI = [%.4f, %.4f]\nbias = %.4f, se = %.4f",
                       theta, cl, ci[1], ci[2],
                       result$boot_bias %||% NA, result$boot_se %||% NA)
      } else {
        lab <- sprintf("statistic = %.4f\np-value = %s\nR = %d",
                       theta, private$.fmt_p(result$p.value), result$R)
      }
      p + ggplot2::annotate("label", x = Inf, y = Inf, label = lab,
                            hjust = 1.1, vjust = 1.1, size = 3,
                            colour = "grey20", fill = "white")
    }
  )
)
