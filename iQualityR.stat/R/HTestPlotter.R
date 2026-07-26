# =============================================================================
# File: R/htest/HTestPlotter.R
# Description: Hypothesis test plotting coordinator (calls plot package graphics functions)
# =============================================================================

#' @title HTestPlotter: Hypothesis Test Plotting Coordinator
#' @description
#' Automatically select appropriate visualization based on test results.
#' All plotting functions reuse iQualityR.plot package functions.
#'
#' @export
HTestPlotter <- R6::R6Class("HTestPlotter",
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
    #' @param result Test result list (HTestAnalyzer output)
    #' @param x Sample data (required for box plot)
    #' @param y Second sample (required for two-sample tests)
    #' @param plot_type Plot type ("auto", "curve", "box", "combined")
    #' @param show_table Whether to display statistics table
    #' @return ggplot2 or patchwork object
    plot = function(result, x = NULL, y = NULL,
                    plot_type = "auto", show_table = TRUE) {
      plot_type <- match.arg(plot_type, c("auto", "curve", "box", "combined"))

      if (plot_type == "auto") {
        plot_type <- private$.auto_select(result, x)
      }

      switch(plot_type,
        "curve" = self$plot_curve(result),
        "box" = self$plot_box(result, x, y, show_table = show_table),
        "combined" = self$plot_combined(result, x, y),
        stop("Invalid plot_type")
      )
    },

    #' @description Plot rejection region
    #' @param result Test result
    #' @return ggplot2 object
    plot_curve = function(result) {
      stat_value <- as.numeric(result$statistic[1])
      dist_type <- result$dist_type %||% "norm"

      # Critical value
      if (!is.null(result$critical_value)) {
        crit <- result$critical_value
      } else {
        crit <- 1.96
      }

      # Degrees of freedom
      df <- if (!is.null(result$parameter) && "df" %in% names(result$parameter)) {
        result$parameter["df"]
      } else {
        NULL
      }

      # Statistic label
      stat_label <- names(result$statistic)[1] %||%
        if (dist_type == "norm") "Z" else "t"

    plot_hypothesis_curve(
        dist = dist_type,
        stat_value = stat_value,
        critical_value = crit,
        alternative = result$alternative,
        alpha = 1 - (result$conf.level %||% 0.95),
        df = df,
        stat_label = stat_label,
        p_value = result$p.value,
        theme = self$theme_obj
      )
    },

    #' @description Plot boxplot
    #' @param result Test result
    #' @param x Sample data
    #' @param y Second sample
    #' @param show_table Whether to display statistics table
    #' @return ggplot2 object
    plot_box = function(result, x = NULL, y = NULL, show_table = TRUE) {
      mu <- if (!is.null(result$null.value)) {
        result$null.value[1]
      } else {
        0
      }

      # Determine sigma (for Z-test)
      sigma <- if (!is.null(result$sigma)) result$sigma else NULL

      # For two-sample test, use x data
      if (is.null(x) && !is.null(result$n1)) {
        stop("x data is required for box plot.")
      }

      if (is.null(x)) {
        stop("x data is required for box plot.")
      }

    plot_hypothesis_box(
        x = x,
        mu = mu,
        sigma = sigma,
        alternative = result$alternative,
        conf_level = result$conf.level %||% 0.95,
        show_table = show_table,
        theme = self$theme_obj
      )
    },

    #' @description Plot combined figure
    #' @param result Test result
    #' @param x Sample data
    #' @param y Second sample
    #' @return patchwork object
    plot_combined = function(result, x = NULL, y = NULL) {
      if (is.null(x)) {
        stop("x data is required for combined plot.")
      }

      mu <- if (!is.null(result$null.value)) result$null.value[1] else 0
      sigma <- if (!is.null(result$sigma)) result$sigma else NULL

    plot_hypothesis_combined(
        x = x,
        mu = mu,
        sigma = sigma,
        alternative = result$alternative,
        conf_level = result$conf.level %||% 0.95,
        theme = self$theme_obj
      )
    },

    #' @description Set theme
    #' @param theme Theme name or IqrTheme object
    set_theme = function(theme) {
      if (inherits(theme, "IqrTheme")) {
        self$theme_obj <- theme
      } else {
        tryCatch({
          self$theme_obj <- IqrTheme$new(theme)
        }, error = function(e) {
          self$theme_obj <<- NULL
        })
      }
      invisible(self)
    }
  ),

  private = list(
    .auto_select = function(result, x) {
      # Has raw data and moderate sample size -> combined
      if (!is.null(x) && length(x) <= 500) {
        return("combined")
      }
      # Only summary data -> curve
      if (is.null(x)) {
        return("curve")
      }
      # Large sample -> curve (boxplot not informative)
      if (length(x) > 500) {
        return("curve")
      }
      # Default
      "box"
    }
  )
)
