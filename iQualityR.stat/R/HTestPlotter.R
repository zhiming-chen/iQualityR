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
  public = list(
    #' @field theme_obj Active `IqrTheme` object (NULL until a theme resolves).
    theme_obj = NULL,

    #' @description Initialize with a theme
    #' @param theme Theme name (e.g. `"academic"`) or an `IqrTheme` object.
    initialize = function(theme = "academic") {
      self$theme_obj <- .resolve_theme(theme)
    },

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
    },

    #' @description Set / replace the active theme
    #' @param theme Theme name or `IqrTheme` object
    #' @return Invisible self (for chaining)
    set_theme = function(theme) {
      self$theme_obj <- .resolve_theme(theme)
      invisible(self)
    }
  ),

  private = list(

    # =====================================================================
    # Auto-select plot type
    # =====================================================================
    .auto_select = function(result) {
      tt <- result$test_type
      x <- result$data$x
      if (tt %in% c("prop_test_1s", "prop_test_2s", "chisq_test") || is.null(x)) {
        return("curve")
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
