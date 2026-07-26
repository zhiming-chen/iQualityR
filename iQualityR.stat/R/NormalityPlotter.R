# =============================================================================
# File: R/normality/NormalityPlotter.R
# Description: Normality test plotting coordinator
# =============================================================================

#' @title NormalityPlotter: Normality test plotting coordinator
#' @description
#' Automatically select appropriate visualization based on test results.
#' All plotting functions reuse iQualityR.plot package functions.
#'
#' @export
NormalityPlotter <- R6::R6Class("NormalityPlotter",
  public = list(
    #' @field theme_obj Theme object
    theme_obj = NULL,

    #' @description Initialize
    #' @param theme Theme name or IqrTheme object
    initialize = function(theme = "academic") {
      if (inherits(theme, "IqrTheme")) {
        self$theme_obj <- theme
      } else {
        tryCatch({
          self$theme_obj <- IqrTheme$new(theme)
        }, error = function(e) {
          self$theme_obj <<- NULL
        })
      }
    },

    #' @description Automatically plot based on test results
    #' @param x Sample data
    #' @param result Test result list (NormalityAnalyzer output)
    #' @param plot_type Plot type ("auto", "hist", "qq", "pp", "combined")
    #' @param add_confidence Whether to add confidence band for QQ plot
    #' @return ggplot2 or patchwork object
    plot = function(x, result = NULL, plot_type = "auto",
                    add_confidence = FALSE) {
      plot_type <- match.arg(plot_type, c("auto", "hist", "qq", "pp", "combined"))

      if (plot_type == "auto") {
        plot_type <- private$.auto_select(result)
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
      x <- x[!is.na(x)]
      n <- length(x)
      if (n < 3) stop("Need at least 3 non-missing values.")

      if (is.null(bins)) {
        bins <- ceiling(log2(n) + 1)
      }

      mu <- mean(x)
      sigma <- sd(x)

      df <- data.frame(x = x)

      layers <- layers_histogram_density(
        bins = bins,
        fill = .iqr_plotter$.pal_discrete(self$theme_obj)[1],
        alpha = 0.6,
        color = "white"
      )

      p <- base_plot(df, ggplot2::aes(x = x), theme = self$theme_obj) + layers

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
