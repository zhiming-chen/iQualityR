# =============================================================================
# File: R/NormalityPlotter.R
# Description: Normality test plotting coordinator
# =============================================================================

#' @title NormalityPlotter: Normality test plotting coordinator
#' @description
#' Automatically select appropriate visualization based on test results.
#' All plotting functions reuse iQualityR.plot package functions.
#'
#' @export
NormalityPlotter <- R6::R6Class("NormalityPlotter",
  inherit = StatPlotter,
  public = list(
    #' @description Unified plot entry point (Contract 2 signature).
    #'
    #' Accepts the fixed Contract 2 signature `(result, plot_type, show_table,
    #' theme_obj)` and dispatches to the appropriate sub-plot. The raw sample
    #' vector `x` and the `add_confidence` flag are absorbed through `...` for
    #' backward compatibility with the legacy `plot(x, result, ...)` call:
    #' when the first argument is a numeric vector it is treated as the old
    #' `x` sample (so `plot(x, plot_type = "hist")` still works).
    #'
    #' @param result A normality test result list (from `NormalityAnalyzer`),
    #'   or a numeric sample vector for backward compatibility (legacy `x`).
    #' @param plot_type Plot type ("auto", "hist", "qq", "pp", "combined").
    #' @param show_table Logical; reserved for Contract 2 signature uniformity
    #'   (normality plots do not overlay a stats table).
    #' @param theme_obj Optional `IqrTheme` overriding `self$theme_obj`.
    #' @param ... Backward-compat channel: `x` (raw sample) and `add_confidence`
    #'   (logical) are extracted from here when not derivable from `result`.
    #' @return A `ggplot` or `patchwork` object.
    plot = function(result, plot_type = "auto", show_table = FALSE,
                    theme_obj = NULL, ...) {
      .check_plot_available()
      if (!is.null(theme_obj)) self$theme_obj <- theme_obj
      plot_type <- match.arg(plot_type, c("auto", "hist", "qq", "pp", "combined"))

      dots <- list(...)
      add_confidence <- dots$add_confidence %||% FALSE

      # Backward-compat: legacy signature was plot(x, result = NULL, ...). If
      # the first argument is a numeric vector, treat it as the old `x` sample.
      if (is.numeric(result)) {
        x <- result
        result_obj <- NULL
      } else {
        result_obj <- result
        x <- dots$x
      }

      if (is.null(x)) {
        stop("[NormalityPlotter] Sample data `x` is required (pass via ... ).",
             call. = FALSE)
      }

      if (plot_type == "auto") {
        plot_type <- private$.auto_select(result_obj)
      }

      switch(plot_type,
        "hist" = self$plot_hist(x),
        "qq" = self$plot_qq(x, add_confidence = add_confidence),
        "pp" = self$plot_pp(x),
        "combined" = self$plot_combined(x, add_confidence = add_confidence),
        stop("Invalid plot_type")
      )
    },

    #' @description Plot histogram with normal curve
    #' @param x Sample data
    #' @param bins Number of histogram bins
    #' @return ggplot2 object
    plot_hist = function(x, bins = NULL) {
      .check_plot_available()
      x <- x[!is.na(x)]
      n <- length(x)
      if (n < 3) stop("Need at least 3 non-missing values.")

      if (is.null(bins)) {
        bins <- ceiling(log2(n) + 1)
      }

      mu <- mean(x)
      sigma <- sd(x)

      df <- data.frame(x = x)

      layers <- iQualityR.plot::layers_histogram_density(
        bins = bins,
        fill = .iqr_plotter$.pal_discrete(self$theme_obj)[1],
        alpha = 0.6,
        color = "white"
      )

      p <- iQualityR.plot::base_plot(df, ggplot2::aes(x = x), theme = self$theme_obj) + layers

      x_seq <- seq(min(x), max(x), length.out = 200)
      df_norm <- data.frame(x = x_seq, y = stats::dnorm(x_seq, mean = mu, sd = sigma))
      p <- p + ggplot2::geom_line(
        data = df_norm, ggplot2::aes(x = x, y = y),
        color = .iqr_plotter$.pal_semantic(self$theme_obj, "fail"), linewidth = 1.2
      )

      p + ggplot2::labs(
        x = "Value",
        y = "Density",
        title = "Histogram with Normal Curve",
        subtitle = sprintf("n = %d, Mean = %.3f, SD = %.3f", n, mu, sigma)
      )
    },

    #' @description Plot QQ diagram
    #' @param x Sample data
    #' @param add_confidence Whether to add confidence band
    #' @return ggplot2 object
    plot_qq = function(x, add_confidence = FALSE) {
      .check_plot_available()
      df <- data.frame(x = x)
      iQualityR.plot::plot_qq(
        df, "x",
        dist_family = "norm",
        add_confidence = add_confidence,
        theme = self$theme_obj
      )
    },

    #' @description Plot PP diagram
    #' @param x Sample data
    #' @return ggplot2 object
    plot_pp = function(x) {
      .check_plot_available()
      df <- data.frame(x = x)
      iQualityR.plot::plot_pp(
        df, "x",
        dist_family = "norm",
        theme = self$theme_obj
      )
    },

    #' @description Plot combined figure
    #' @param x Sample data
    #' @param add_confidence Whether to add confidence band for QQ plot
    #' @param layout Layout
    #' @return patchwork object
    plot_combined = function(x, add_confidence = FALSE, layout = "1x3") {
      p1 <- self$plot_hist(x)
      p2 <- self$plot_qq(x, add_confidence = add_confidence)
      p3 <- self$plot_pp(x)

      if (layout == "1x3") {
        p1 + p2 + p3 + patchwork::plot_layout(ncol = 3)
      } else if (layout == "2x2") {
        (p1 / p2) + p3 + patchwork::plot_layout(ncol = 2)
      } else {
        p1 + p2 + p3 + patchwork::plot_layout(ncol = 3)
      }
    }
  ),

  private = list(
    .auto_select = function(result) {
      if (!is.null(result)) {
        return("combined")
      }
      "hist"
    }
  )
)
