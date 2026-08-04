# =============================================================================
# File: R/HTestPlotter.R
# Description: Hypothesis test plotting coordinator (L2 presentation layer).
#              Thin delegation layer -- ALL visualization logic lives in
#              iQualityR.plot. This class only:
#                1. Dispatches by result$test_type to the correct .plot fn
#                2. Extracts parameters from the stat_result object
#                3. Passes them to the appropriate .plot function
#
# Per Architecture Decision 2: soft-depends on iQualityR.plot via
#   requireNamespace() + qualified :: calls.
# Per Contract 2: plot() signature is fixed at
#   plot(result, plot_type = "auto", show_table = FALSE, theme_obj = NULL)
# =============================================================================

#' @title HTestPlotter: Hypothesis Test Plotting Coordinator
#' @description
#' Test-type-aware visualization for hypothesis test results. This is a thin
#' delegation layer -- ALL ggplot2 visualization code lives in
#' `iQualityR.plot`. HTestPlotter only dispatches by `result$test_type` and
#' extracts parameters from the `stat_result` object.
#'
#' **Contract 2 signature** (fixed across all L2 Plotters in .stat):
#' ```
#' $plot(result, plot_type = "auto", show_table = FALSE, theme_obj = NULL)
#' ```
#'
#' @export
HTestPlotter <- R6::R6Class("HTestPlotter",
  inherit = StatPlotter,
  public = list(
    #' @description Unified plot entry point (Contract 2 signature).
    #'
    #' @param result A `stat_result` returned by `HTestAnalyzer`.
    #' @param plot_type One of `"auto"`, `"curve"`, `"box"`, `"combined"`.
    #' @param show_table Logical; overlay a stats table on box plots.
    #' @param theme_obj Optional `IqrTheme` overriding `self$theme_obj`.
    #' @return A `ggplot` or `patchwork` object.
    plot = function(result, plot_type = "auto", show_table = FALSE, theme_obj = NULL) {
      plot_type <- match.arg(plot_type, c("auto", "curve", "box", "combined"))
      effective_theme <- if (!is.null(theme_obj)) theme_obj else self$theme_obj

      .check_plot_available()

      if (plot_type == "auto") {
        plot_type <- private$.auto_select(result)
      }
      plot_type <- private$.resolve_plot_type(result, plot_type)

      switch(plot_type,
        "curve"    = self$plot_curve(result, theme_obj = effective_theme),
        "box"      = self$plot_box(result, show_table = show_table, theme_obj = effective_theme),
        "combined" = self$plot_combined(result, theme_obj = effective_theme),
        stop("Invalid plot_type: ", plot_type)
      )
    },

    #' @description Plot the rejection-region / distribution curve
    #' @param result A `stat_result` from `HTestAnalyzer`
    #' @param theme_obj Optional `IqrTheme` override
    #' @return ggplot object
    plot_curve = function(result, theme_obj = NULL) {
      effective_theme <- if (!is.null(theme_obj)) theme_obj else self$theme_obj
      .check_plot_available()
      private$.dispatch_curve(result, effective_theme)
    },

    #' @description Plot a boxplot of the raw sample data
    #' @param result A `stat_result` from `HTestAnalyzer`
    #' @param show_table Logical; overlay a stats table
    #' @param theme_obj Optional `IqrTheme` override
    #' @return ggplot object
    plot_box = function(result, show_table = FALSE, theme_obj = NULL) {
      effective_theme <- if (!is.null(theme_obj)) theme_obj else self$theme_obj
      .check_plot_available()
      private$.dispatch_box(result, show_table, effective_theme)
    },

    #' @description Plot a combined figure (distribution curve + boxplot)
    #' @param result A `stat_result` from `HTestAnalyzer`
    #' @param theme_obj Optional `IqrTheme` override
    #' @return patchwork object
    plot_combined = function(result, theme_obj = NULL) {
      effective_theme <- if (!is.null(theme_obj)) theme_obj else self$theme_obj
      .check_plot_available()
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("[HTestPlotter] patchwork package is required for combined plots.", call. = FALSE)
      }

      p_curve <- self$plot_curve(result, theme_obj = effective_theme)
      p_box <- tryCatch(
        self$plot_box(result, show_table = FALSE, theme_obj = effective_theme),
        error = function(e) NULL
      )
      if (is.null(p_box)) return(p_curve)

      p_curve + p_box +
        patchwork::plot_layout(ncol = 2, widths = c(3, 2)) +
        patchwork::plot_annotation(
          title = result$method %||% result$test_type,
          subtitle = sprintf("statistic = %.4f, p-value = %.4f",
                             as.numeric(result$statistic[1]), result$p.value)
        )
    }
  ),

  private = list(

    # =====================================================================
    # Auto-select plot type
    # =====================================================================
    .auto_select = function(result) {
      tt <- result$test_type
      x <- result$data$x
      # Summary-only tests (no raw x) -> curve
      if (tt %in% c("prop_test_1s", "prop_test_2s", "chisq_test") || is.null(x)) {
        return("curve")
      }
      # Non-parametric rank tests shine on box plots of the raw data
      if (tt %in% c("wilcoxon_signed_rank", "wilcoxon_rank_sum",
                    "kruskal_wallis", "friedman")) {
        return("box")
      }
      # Equivalence / non-inferiority / superiority: dedicated text-panel curve
      if (tt %in% c("tost_mean", "tost_proportion",
                    "non_inferiority", "superiority")) {
        return("curve")
      }
      # Correlation tests: scatter plot is the natural visualization
      if (tt %in% c("cor_test_pearson", "cor_test_spearman", "cor_test_kendall")) {
        return("box")
      }
      # Variance-equality tests: grouped boxplot of the raw data
      if (tt %in% c("levene_test", "bartlett_test")) {
        return("box")
      }
      "combined"
    },

    # Resolve requested plot_type to what the test type can actually produce
    .resolve_plot_type = function(result, plot_type) {
      tt <- result$test_type
      x <- result$data$x
      if (plot_type == "curve") return("curve")
      if (plot_type %in% c("box", "combined")) {
        if (tt %in% c("prop_test_1s", "prop_test_2s", "chisq_test") || is.null(x)) {
          warning("[HTestPlotter] plot_type = '", plot_type,
                  "' not available for '", tt, "'. Falling back to 'curve'.",
                  call. = FALSE)
          return("curve")
        }
        # Proportion-based equivalence tests have no raw x either
        if (tt %in% c("tost_proportion") ||
            (tt %in% c("non_inferiority", "superiority") &&
             identical(result$type, "proportion"))) {
          warning("[HTestPlotter] plot_type = '", plot_type,
                  "' not available for '", tt, "' (proportion). Falling back to 'curve'.",
                  call. = FALSE)
          return("curve")
        }
        # Poisson tests are summary-only; no raw data for box / combined
        if (tt %in% c("poisson_test_1s", "poisson_test_2s")) {
          warning("[HTestPlotter] plot_type = '", plot_type,
                  "' not available for '", tt, "'. Falling back to 'curve'.",
                  call. = FALSE)
          return("curve")
        }
      }
      plot_type
    },

    # =====================================================================
    # Curve dispatch -- delegates to iQualityR.plot functions
    # =====================================================================
    .dispatch_curve = function(result, theme) {
      tt <- result$test_type
      stat_value <- as.numeric(result$statistic[1])
      alpha <- 1 - (result$conf.level %||% 0.95)
      alt <- result$alternative
      p_val <- result$p.value

      # --- One-sample t/Z & proportion tests: use plot_hypothesis_curve ---
      if (tt %in% c("t_test_1s", "z_test_1s", "prop_test_1s", "prop_test_2s")) {
        dist_type <- result$dist_type %||% "t"
        df_val <- if (!is.null(result$parameter) && "df" %in% names(result$parameter))
          result$parameter["df"] else NULL
        crit <- result$critical_value %||% .compute_critical(dist_type, alt, alpha, df_val)
        stat_label <- names(result$statistic)[1] %||%
          if (dist_type == "norm") "Z" else "t"
        return(iQualityR.plot::plot_hypothesis_curve(
          dist = dist_type, stat_value = stat_value,
          critical_value = crit, alternative = alt, alpha = alpha,
          df = df_val, stat_label = stat_label, p_value = p_val, theme = theme
        ))
      }

      # --- Two-sample & paired t-test: t-curve of the difference ---
      if (tt %in% c("t_test_2s", "t_test_paired")) {
        df_val <- result$parameter["df"]
        crit <- result$critical_value %||% .compute_critical("t", alt, alpha, df_val)
        return(iQualityR.plot::plot_hypothesis_curve(
          dist = "t", stat_value = stat_value,
          critical_value = crit, alternative = alt, alpha = alpha,
          df = df_val, stat_label = "t", p_value = p_val, theme = theme
        ))
      }

      # --- F-test: F-distribution curve ---
      if (tt == "f_test") {
        df1 <- result$parameter["num_df"]
        df2 <- result$parameter["den_df"]
        return(iQualityR.plot::plot_hypothesis_curve_f(
          stat_value = stat_value, df1 = df1, df2 = df2,
          alternative = alt, alpha = alpha, p_value = p_val, theme = theme
        ))
      }

      # --- Chi-square: observed vs expected bar chart ---
      if (tt == "chisq_test") {
        return(private$.chisq_curve(result, theme))
      }

      # --- Non-parametric rank tests: no natural distribution curve ---
      # Fall back to a text panel summarizing the rank-based statistic.
      if (tt %in% c("wilcoxon_signed_rank", "wilcoxon_rank_sum",
                    "kruskal_wallis", "friedman")) {
        return(private$.rank_text_panel(result, theme))
      }

      # --- Equivalence / non-inferiority / superiority tests ---
      if (tt %in% c("tost_mean", "tost_proportion",
                    "non_inferiority", "superiority")) {
        return(private$.equivalence_text_panel(result, theme))
      }

      # --- Poisson rate tests: summary-only, text panel ---
      if (tt %in% c("poisson_test_1s", "poisson_test_2s")) {
        return(private$.poisson_text_panel(result, theme))
      }

      # --- Correlation tests: text panel summary (scatter handled by box) ---
      if (tt %in% c("cor_test_pearson", "cor_test_spearman", "cor_test_kendall")) {
        return(private$.cor_text_panel(result, theme))
      }

      stop("[HTestPlotter] Unsupported test_type: ", tt, call. = FALSE)
    },

    # =====================================================================
    # Box dispatch -- delegates to iQualityR.plot functions
    # =====================================================================
    .dispatch_box = function(result, show_table, theme) {
      tt <- result$test_type
      x <- result$data$x
      y <- result$data$y

      # --- One-sample: use plot_hypothesis_box ---
      if (tt %in% c("t_test_1s", "z_test_1s") && !is.null(x)) {
        mu <- if (!is.null(result$null.value)) result$null.value[1] else 0
        sigma <- if (!is.null(result$sigma)) result$sigma else NULL
        return(iQualityR.plot::plot_hypothesis_box(
          x = x, mu = mu, sigma = sigma,
          alternative = result$alternative,
          conf_level = result$conf.level %||% 0.95,
          show_table = show_table, theme = theme
        ))
      }

      # --- Paired t-test: use plot_paired_before_after ---
      if (tt == "t_test_paired") {
        .require_data(x, y, tt)
        subtitle <- sprintf("mean diff = %.4f, p-value = %.4f",
                            result$mean_diff, result$p.value)
        return(iQualityR.plot::plot_paired_before_after(
          x = x, y = y, subtitle = subtitle, theme = theme
        ))
      }

      # --- Two-sample t-test: use plot_hypothesis_box_two_group ---
      if (tt == "t_test_2s") {
        .require_data(x, y, tt)
        subtitle <- sprintf("mean A = %.4f, mean B = %.4f, p-value = %.4f",
                            result$mean1, result$mean2, result$p.value)
        return(iQualityR.plot::plot_hypothesis_box_two_group(
          x = x, y = y, subtitle = subtitle,
          title = "Two-Sample t-test", theme = theme
        ))
      }

      # --- F-test: use plot_hypothesis_box_two_group ---
      if (tt == "f_test") {
        .require_data(x, y, tt)
        subtitle <- sprintf("var A = %.4f, var B = %.4f, p-value = %.4f",
                            result$var1, result$var2, result$p.value)
        return(iQualityR.plot::plot_hypothesis_box_two_group(
          x = x, y = y, subtitle = subtitle,
          title = "F test: Variance Comparison", theme = theme
        ))
      }

      # --- Wilcoxon signed rank (one-sample): box plot vs mu ---
      if (tt == "wilcoxon_signed_rank" && is.null(y)) {
        mu <- if (!is.null(result$null.value)) result$null.value[1] else 0
        return(iQualityR.plot::plot_hypothesis_box(
          x = x, mu = mu, sigma = NULL,
          alternative = result$alternative,
          conf_level = result$conf.level %||% 0.95,
          show_table = show_table, theme = theme
        ))
      }

      # --- Wilcoxon signed rank (paired): before/after paired plot ---
      if (tt == "wilcoxon_signed_rank" && !is.null(y)) {
        .require_data(x, y, tt)
        subtitle <- sprintf("pseudo-median diff = %.4f, p-value = %.4f",
                            as.numeric(result$estimate[1]), result$p.value)
        return(iQualityR.plot::plot_paired_before_after(
          x = x, y = y, subtitle = subtitle, theme = theme
        ))
      }

      # --- Wilcoxon rank sum (two-sample): two-group box plot ---
      if (tt == "wilcoxon_rank_sum") {
        .require_data(x, y, tt)
        subtitle <- sprintf("W = %.4f, p-value = %.4f",
                            as.numeric(result$statistic[1]), result$p.value)
        return(iQualityR.plot::plot_hypothesis_box_two_group(
          x = x, y = y, subtitle = subtitle,
          title = "Wilcoxon Rank Sum (Mann-Whitney U)", theme = theme
        ))
      }

      # --- Kruskal-Wallis: k-group box plot (built inline) ---
      if (tt == "kruskal_wallis") {
        return(private$.kruskal_box(result, show_table, theme))
      }

      # --- Friedman: per-block treatment profile (inline) ---
      if (tt == "friedman") {
        return(private$.friedman_profile(result, theme))
      }

      # --- Correlation tests: scatter plot with smooth fit ---
      if (tt %in% c("cor_test_pearson", "cor_test_spearman", "cor_test_kendall")) {
        return(private$.cor_scatter(result, theme))
      }

      # --- Variance-equality tests: grouped boxplot across k groups ---
      if (tt %in% c("levene_test", "bartlett_test")) {
        return(private$.variance_box(result, show_table, theme))
      }

      # --- TOST mean (one-sample with raw x): box plot vs reference mu ---
      if (tt == "tost_mean" && is.null(y)) {
        mu <- if (!is.null(result$null.value)) {
          # null.value is the equivalence margin; use estimate + margin to back
          # out the reference. For one-sample TOST, estimate = mean(x) - mu,
          # so reference mu = mean(x) - estimate.
          mean(x) - as.numeric(result$estimate[1])
        } else 0
        subtitle <- sprintf("delta = %.4f, %s, p-value = %.4f",
                            result$delta,
                            result$equivalence %||% "",
                            result$p.value)
        return(iQualityR.plot::plot_hypothesis_box(
          x = x, mu = mu, sigma = NULL,
          alternative = "two.sided",
          conf_level = result$conf.level %||% 0.95,
          show_table = show_table, theme = theme
        ))
      }

      stop("[HTestPlotter] Box plot not available for '", tt,
           "'. Use plot_type = 'curve' instead.", call. = FALSE)
    },

    # =====================================================================
    # Chi-square visualization -- delegates to .plot
    # =====================================================================
    .chisq_curve = function(result, theme) {
      obs <- result$observed
      exp <- result$expected
      stat_value <- as.numeric(result$statistic[1])
      df_val <- result$parameter
      p_val <- result$p.value

      if (is.null(obs) || is.null(exp)) {
        # Contingency table without extractable counts -- fallback text panel
        label <- sprintf("%s\nX-squared = %.4f, df = %s, p-value = %.4f",
                         result$method %||% result$test_type,
                         stat_value,
                         if (is.null(df_val)) "NA" else format(df_val),
                         p_val)
        return(ggplot2::ggplot(data.frame(x = 0, y = 0, label = label)) +
                 ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = label),
                                    size = 4, hjust = 0.5) +
                 ggplot2::theme_void() +
                 ggplot2::labs(title = result$method %||% result$test_type))
      }

      iQualityR.plot::plot_chi_square_observed_expected(
        observed = obs, expected = exp,
        stat_value = stat_value, df = df_val, p_value = p_val,
        theme = theme
      )
    },

    # =====================================================================
    # Non-parametric helpers
    # =====================================================================

    # Text-panel fallback for non-parametric tests when a curve is requested
    # but the rank-based statistic has no smooth reference distribution to draw.
    .rank_text_panel = function(result, theme) {
      stat_value <- as.numeric(result$statistic[1])
      stat_name <- names(result$statistic)[1] %||% "statistic"
      df_val <- result$parameter
      p_val <- result$p.value
      label <- sprintf("%s\n%s = %.4f, %s, p-value = %.4f",
                       result$method %||% result$test_type,
                       stat_name, stat_value,
                       if (is.null(df_val) || length(df_val) == 0L)
                         "rank-based" else
                         paste(names(df_val), df_val, sep = " = ", collapse = ", "),
                       p_val)
      ggplot2::ggplot(data.frame(x = 0, y = 0, label = label)) +
        ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = label),
                           size = 4, hjust = 0.5) +
        ggplot2::theme_void() +
        ggplot2::labs(title = result$method %||% result$test_type,
                      caption = "Rank-based test: no smooth reference curve.")
    },

    # Equivalence / non-inferiority / superiority text panel.
    # These tests carry a margin (delta) and a binary conclusion flag; there is
    # no single smooth reference distribution to draw, so a text panel is the
    # clearest visualization of the result and the margin.
    .equivalence_text_panel = function(result, theme) {
      tt <- result$test_type
      p_val <- result$p.value
      delta <- result$delta %||% NA
      ci <- result$conf.int
      stat_vals <- result$statistic
      stat_str <- paste(names(stat_vals), sprintf("%.4f", as.numeric(stat_vals)),
                        sep = " = ", collapse = ", ")

      # Pick the binary conclusion label carried on the result
      conclusion <- switch(tt,
        "tost_mean"       = ,
        "tost_proportion" = result$equivalence %||% "",
        "non_inferiority" = if (isTRUE(result$non_inferior))
                              "non-inferior" else "NOT non-inferior",
        "superiority"     = if (isTRUE(result$superior))
                              "superior" else "NOT superior",
        ""
      )

      ci_str <- if (!is.null(ci) && length(ci) == 2L)
        sprintf("\n%.1f%% CI: [%.4f, %.4f]",
                100 * (result$conf.level %||% 0.95), ci[1], ci[2]) else ""

      label <- sprintf("%s\n%s\np-value = %.4f\ndelta = %.4f\nConclusion: %s%s",
                       result$method %||% tt, stat_str, p_val, delta,
                       conclusion, ci_str)

      ggplot2::ggplot(data.frame(x = 0, y = 0, label = label)) +
        ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = label),
                           size = 4, hjust = 0.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = result$method %||% tt,
          caption = "Equivalence / margin test: conclusion shown with margin and CI."
        )
    },

    # Poisson rate test text panel.  These are summary-only tests (no raw
    # data vector) so a text panel is the natural visualization.
    .poisson_text_panel = function(result, theme) {
      p_val <- result$p.value
      ci <- result$conf.int
      stat_vals <- result$statistic
      stat_str <- paste(names(stat_vals), sprintf("%.0f", as.numeric(stat_vals)),
                        sep = " = ", collapse = ", ")

      ci_str <- if (!is.null(ci) && length(ci) == 2L)
        sprintf("\n%.1f%% CI: [%.4f, %.4f]",
                100 * (result$conf.level %||% 0.95), ci[1], ci[2]) else ""

      if (result$test_type == "poisson_test_1s") {
        est_str <- sprintf("\nestimated rate = %.4f (null = %.4f)",
                           result$rate, result$r0 %||% 1)
      } else {
        est_str <- sprintf("\nrate1 = %.4f, rate2 = %.4f, ratio = %.4f",
                           result$rate1, result$rate2, result$rate_ratio)
      }

      label <- sprintf("%s\n%s\np-value = %.4f%s%s",
                       result$method %||% result$test_type,
                       stat_str, p_val, est_str, ci_str)

      ggplot2::ggplot(data.frame(x = 0, y = 0, label = label)) +
        ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = label),
                           size = 4, hjust = 0.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = result$method %||% result$test_type,
          caption = "Exact Poisson test: count, rate, and CI shown."
        )
    },

    # Correlation test text-panel fallback (used when plot_type = "curve").
    # Shows the estimate, statistic, p-value, and CI (if any).
    .cor_text_panel = function(result, theme) {
      p_val <- result$p.value
      ci <- result$conf.int
      stat_vals <- result$statistic
      stat_name <- names(stat_vals)[1] %||% "statistic"
      stat_str <- sprintf("%s = %.4f", stat_name, as.numeric(stat_vals[1]))

      est_name <- names(result$estimate)[1] %||% "estimate"
      est_str <- sprintf("%s = %.4f", est_name, as.numeric(result$estimate[1]))

      ci_str <- if (!is.null(ci) && length(ci) == 2L)
        sprintf("\n%.1f%% CI: [%.4f, %.4f]",
                100 * (result$conf.level %||% 0.95), ci[1], ci[2]) else ""

      df_str <- if (!is.null(result$parameter) && length(result$parameter) > 0L)
        sprintf("\n%s = %s",
                names(result$parameter)[1],
                format(result$parameter[1])) else ""

      label <- sprintf("%s\n%s\n%s\np-value = %.4f%s%s",
                       result$method %||% result$test_type,
                       stat_str, est_str, p_val, df_str, ci_str)

      ggplot2::ggplot(data.frame(x = 0, y = 0, label = label)) +
        ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = label),
                           size = 4, hjust = 0.5) +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = result$method %||% result$test_type,
          caption = "Correlation test summary."
        )
    },

    # Correlation scatter plot with optional linear fit.  Used for both
    # box / auto / combined dispatch for all three correlation methods.
    .cor_scatter = function(result, theme) {
      x <- result$data$x
      y <- result$data$y
      if (is.null(x) || is.null(y)) {
        stop("[HTestPlotter] correlation scatter requires data$x and data$y.",
             call. = FALSE)
      }
      df_plot <- data.frame(x = as.numeric(x), y = as.numeric(y))

      est_name <- names(result$estimate)[1] %||% "estimate"
      est_val <- as.numeric(result$estimate[1])
      p_val <- result$p.value
      stat_val <- as.numeric(result$statistic[1])
      stat_name <- names(result$statistic)[1] %||% "statistic"

      subtitle <- sprintf("%s = %.4f, %s = %.4f, p-value = %.4f",
                          est_name, est_val, stat_name, stat_val, p_val)

      p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.6, size = 2.0) +
        ggplot2::labs(
          title = result$method %||% result$test_type,
          subtitle = subtitle,
          x = "x", y = "y"
        ) +
        ggplot2::theme_minimal()

      # Add a smooth fit only for Pearson (linear). For Spearman/Kendall a
      # linear fit can be misleading on monotonic-but-nonlinear relations.
      if (result$test_type == "cor_test_pearson") {
        p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE,
                                      colour = "steelblue", fill = "lightblue",
                                      alpha = 0.2)
      } else {
        p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE,
                                      colour = "darkorange")
      }
      p
    },

    # Kruskal-Wallis: grouped box plot across k groups.
    .kruskal_box = function(result, show_table, theme) {
      x <- result$data$x
      g <- result$data$y
      if (is.null(x) || is.null(g)) {
        stop("[HTestPlotter] kruskal_wallis requires data$x and data$y (grouping).",
             call. = FALSE)
      }
      g <- as.factor(g)
      df_plot <- data.frame(value = x, group = g)
      subtitle <- sprintf("chi-squared = %.4f, df = %d, p-value = %.4f",
                          as.numeric(result$statistic[1]),
                          as.integer(result$parameter),
                          result$p.value)
      p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = group, y = value, fill = group)) +
        ggplot2::geom_boxplot(alpha = 0.7) +
        ggplot2::geom_jitter(width = 0.15, alpha = 0.5, size = 1.2) +
        ggplot2::labs(
          title = "Kruskal-Wallis Rank Sum Test",
          subtitle = subtitle,
          x = "Group", y = "Value"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
      p
    },

    # Friedman: per-block treatment profile plot (wide matrix -> line plot).
    .friedman_profile = function(result, theme) {
      mat <- result$wide_matrix
      if (is.null(mat)) {
        # Long-form input without cached matrix -- fall back to text panel.
        return(private$.rank_text_panel(result, theme))
      }
      df_plot <- data.frame(
        block   = factor(rep(seq_len(nrow(mat)), ncol(mat))),
        treatment = factor(rep(seq_len(ncol(mat)), each = nrow(mat)),
                           labels = paste0("T", seq_len(ncol(mat)))),
        value = as.numeric(mat)
      )
      subtitle <- sprintf("chi-squared = %.4f, df = %d, p-value = %.4f",
                          as.numeric(result$statistic[1]),
                          as.integer(result$parameter),
                          result$p.value)
      ggplot2::ggplot(df_plot, ggplot2::aes(x = treatment, y = value,
                                            group = block, colour = block)) +
        ggplot2::geom_line(alpha = 0.6) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::labs(
          title = "Friedman Rank Sum Test",
          subtitle = subtitle,
          x = "Treatment", y = "Response", colour = "Block"
        ) +
        ggplot2::theme_minimal()
    },

    # Levene / Bartlett: grouped boxplot across k groups with per-group
    # variances annotated. Visualizes the spread difference that the test
    # quantifies.
    .variance_box = function(result, show_table, theme) {
      x <- result$data$x
      g <- result$data$y
      if (is.null(x) || is.null(g)) {
        stop("[HTestPlotter] variance-equality test requires data$x and data$y (grouping).",
             call. = FALSE)
      }
      g <- as.factor(g)
      df_plot <- data.frame(value = as.numeric(x), group = g)

      stat_name <- names(result$statistic)[1] %||% "statistic"
      stat_val  <- as.numeric(result$statistic[1])
      p_val     <- result$p.value
      group_var <- result$group_var

      # Subtitle carries the test statistic and p-value
      param_str <- if (!is.null(result$parameter) && length(result$parameter) > 0L) {
        paste(names(result$parameter), as.integer(result$parameter),
              sep = " = ", collapse = ", ")
      } else {
        ""
      }
      subtitle <- sprintf("%s = %.4f%s, p-value = %.4f",
                          stat_name, stat_val,
                          if (nzchar(param_str)) paste0(", ", param_str) else "",
                          p_val)

      p <- ggplot2::ggplot(df_plot,
                           ggplot2::aes(x = group, y = value, fill = group)) +
        ggplot2::geom_boxplot(alpha = 0.7) +
        ggplot2::geom_jitter(width = 0.15, alpha = 0.5, size = 1.2) +
        ggplot2::labs(
          title = result$method %||% result$test_type,
          subtitle = subtitle,
          x = "Group", y = "Value"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")

      # Annotate per-group variance below each group label
      if (!is.null(group_var) && length(group_var) > 0L) {
        var_labels <- sprintf("var = %.3f", as.numeric(group_var))
        df_anno <- data.frame(
          group = factor(names(group_var), levels = levels(g)),
          label = var_labels
        )
        p <- p + ggplot2::annotate(
          "text",
          x = seq_along(levels(g)),
          y = -Inf, vjust = -1.5,
          label = var_labels,
          size = 3.2
        )
      }

      p
    }
  )
)

# =============================================================================
# Module-scope helpers (not methods)
# =============================================================================
# Package-level helpers (.check_plot_available / .resolve_theme) are defined
# in package.R so they are available to all files regardless of Collate order.
# =============================================================================

.require_data <- function(x, y, tt) {
  if (is.null(x) || is.null(y)) {
    stop("[HTestPlotter] test_type '", tt, "' requires both data$x and data$y.",
         call. = FALSE)
  }
}

# Compute critical value for a given distribution / alternative / alpha
.compute_critical <- function(dist_type, alternative, alpha, df = NULL) {
  if (dist_type == "norm") {
    if (alternative == "two.sided") stats::qnorm(1 - alpha / 2)
    else stats::qnorm(1 - alpha)
  } else if (dist_type == "t") {
    if (is.null(df)) stop("[HTestPlotter] df required for t distribution.", call. = FALSE)
    if (alternative == "two.sided") stats::qt(1 - alpha / 2, df = df)
    else stats::qt(1 - alpha, df = df)
  } else {
    stop("[HTestPlotter] Unsupported dist_type for critical value: ", dist_type,
         call. = FALSE)
  }
}
