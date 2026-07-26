# =============================================================================
# File: R/ReliabilityPlotter.R
# Description: Reliability and survival analysis visualization engine
# =============================================================================

#' @title ReliabilityPlotter: Reliability and Survival Visualization Engine
#'
#' @description
#' Generates reliability analysis plots:
#' - Weibull probability plot
#' - Survival curves (with confidence intervals)
#' - Hazard curves
#' - Bathtub curve (Weibull only)
#' - Cox forest plot
#'
#' Inherits from [IqrPlotterBase].
#'
#' @export
ReliabilityPlotter <- R6::R6Class("ReliabilityPlotter",
  inherit = IqrPlotterBase,

  public = list(
    #' @description Render reliability plots.
    #' @param results Analysis results list (from [ReliabilityAnalyzer]).
    #' @param theme_obj [IqrTheme] object.
    #' @param type Character. Plot type: `"full"`, `"probability"`,
    #'   `"survival"`, `"hazard"`, `"bathtub"`, `"kaplan_meier"`, `"forest"`.
    #' @return A `ggplot` or `patchwork` object.
    render = function(results, theme_obj, type = "full") {
      if (is.null(results) || length(results) == 0) {
        stop("[ReliabilityPlotter] Results are NULL or empty.", call. = FALSE)
      }

      if (results$method == "parametric") {
        switch(type,
          "full"        = private$.plot_full_panel(results, theme_obj),
          "probability" = private$.plot_probability_plot(results, theme_obj),
          "survival"    = private$.plot_survival_curve(results, theme_obj),
          "hazard"      = private$.plot_hazard_curve(results, theme_obj),
          "bathtub"     = private$.plot_bathtub_curve(results, theme_obj),
          stop("[ReliabilityPlotter] Invalid plot type: ", type, call. = FALSE)
        )
      } else if (results$method == "kaplan_meier") {
        switch(type,
          "full"          = private$.plot_km_full(results, theme_obj),
          "kaplan_meier" = private$.plot_km_curve(results, theme_obj),
          stop("[ReliabilityPlotter] Invalid plot type: ", type, call. = FALSE)
        )
      } else if (results$method == "cox") {
        switch(type,
          "full"   = private$.plot_cox_full(results, theme_obj),
          "forest" = private$.plot_cox_forest(results, theme_obj),
          stop("[ReliabilityPlotter] Invalid plot type: ", type, call. = FALSE)
        )
      } else {
        stop("[ReliabilityPlotter] Unsupported analysis method: ",
             results$method, call. = FALSE)
      }
    }
  ),

  private = list(
    # ========================================================================
    # Parametric reliability plots
    # ========================================================================

    .plot_full_panel = function(results, theme_obj) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        message("[ReliabilityPlotter] 'patchwork' not available; returning single plot.")
        return(private$.plot_probability_plot(results, theme_obj))
      }

      p1 <- private$.plot_probability_plot(results, theme_obj)
      p2 <- private$.plot_survival_curve(results, theme_obj)
      p3 <- private$.plot_hazard_curve(results, theme_obj)

      # Key metrics text panel
      metrics <- results$reliability_metrics
      metrics_text <- paste0(
        "MTTF: ", round(.safe_metric(metrics$mttf), 1), "\n",
        "B10:  ", round(.safe_metric(metrics$b10_life), 1), "\n",
        "B50:  ", round(.safe_metric(metrics$b50_life), 1)
      )
      p4 <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.6, label = metrics_text,
                          size = 6, hjust = 0.5) +
        ggplot2::labs(title = "Key Reliability Metrics") +
        ggplot2::theme_void() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14))

      (p1 + p2) / (p3 + p4) +
        patchwork::plot_layout(heights = c(3, 2)) +
        patchwork::plot_annotation(
          title = "Reliability Analysis Report",
          theme = theme_obj$plot$theme_iqr()$theme
        )
    },

    .plot_probability_plot = function(results, theme_obj) {
      dist <- results$distribution
      if (dist == "weibull") {
        private$.plot_weibull_probability(results, theme_obj)
      } else {
        # Generic CDF plot for non-Weibull distributions
        surv_func <- results$survival_function
        if (is.null(surv_func) || nrow(surv_func) == 0) {
          return(.empty_plot("No survival data available."))
        }
        ggplot2::ggplot(surv_func, ggplot2::aes(x = .data$time, y = 1 - .data$survival_prob)) +
          ggplot2::geom_line(color = .safe_primary(theme_obj), linewidth = 1.2) +
          ggplot2::labs(
            x = "Time",
            y = "Cumulative failure probability F(t)",
            title = paste0(toupper(dist), " Cumulative Distribution Function")
          ) +
          theme_obj$plot$theme_iqr()
      }
    },

    .plot_weibull_probability = function(results, theme_obj) {
      params <- results$distribution_fit$parameters
      shape  <- params$shape
      scale  <- params$scale
      surv_func <- results$survival_function
      if (is.null(surv_func) || nrow(surv_func) == 0) {
        return(.empty_plot("No survival data available."))
      }

      # Weibull probability paper: ln(t) vs ln(-ln(R(t)))
      t_seq <- surv_func$time
      theoretical <- shape * log(t_seq / scale)
      actual <- log(-log(surv_func$survival_prob))

      plot_df <- data.frame(
        ln_time     = log(t_seq),
        theoretical = theoretical,
        actual      = actual
      )

      ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$ln_time)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$theoretical),
                           color = "grey50", linetype = "dashed", linewidth = 1) +
        ggplot2::geom_line(ggplot2::aes(y = .data$actual),
                           color = .safe_primary(theme_obj), linewidth = 1.2) +
        ggplot2::labs(
          x = "ln(Time)",
          y = "ln(-ln(R(t)))",
          title = "Weibull Probability Plot",
          subtitle = paste0("Shape beta = ", round(shape, 2),
                            ", Scale eta = ", round(scale, 1))
        ) +
        theme_obj$plot$theme_iqr()
    },

    .plot_survival_curve = function(results, theme_obj) {
      surv_func <- results$survival_function
      if (is.null(surv_func) || nrow(surv_func) == 0) {
        return(.empty_plot("No survival data available."))
      }

      ggplot2::ggplot(surv_func, ggplot2::aes(x = .data$time, y = .data$survival_prob)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_ci, ymax = .data$upper_ci),
                             fill = .safe_primary(theme_obj), alpha = 0.2) +
        ggplot2::geom_line(color = .safe_primary(theme_obj), linewidth = 1.2) +
        ggplot2::labs(
          x = "Time",
          y = "Reliability R(t)",
          title = "Reliability Curve"
        ) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        theme_obj$plot$theme_iqr()
    },

    .plot_hazard_curve = function(results, theme_obj) {
      hazard_func <- results$hazard_function
      if (is.null(hazard_func) || nrow(hazard_func) == 0) {
        return(.empty_plot("No hazard data available."))
      }

      ggplot2::ggplot(hazard_func, ggplot2::aes(x = .data$time, y = .data$hazard_rate)) +
        ggplot2::geom_line(color = .safe_danger(theme_obj), linewidth = 1.2) +
        ggplot2::labs(
          x = "Time",
          y = "Hazard rate h(t)",
          title = "Hazard Rate Curve"
        ) +
        theme_obj$plot$theme_iqr()
    },

    .plot_bathtub_curve = function(results, theme_obj) {
      dist <- results$distribution
      if (dist != "weibull") {
        message("[ReliabilityPlotter] Bathtub curve is only available for Weibull.")
        return(private$.plot_hazard_curve(results, theme_obj))
      }

      params <- results$distribution_fit$parameters
      shape  <- params$shape
      scale  <- params$scale
      if (is.null(results$hazard_function) || nrow(results$hazard_function) == 0) {
        return(.empty_plot("No hazard data available."))
      }

      t_seq <- seq(0.01, max(results$hazard_function$time, na.rm = TRUE) * 1.1,
                   length.out = 300)
      hazard <- (shape / scale) * (t_seq / scale)^(shape - 1)

      # Illustrative three-phase decomposition
      early  <- if (shape < 1) hazard * 0.3 else rep(0, length(t_seq))
      random <- rep(min(hazard), length(t_seq))
      wear   <- pmax(0, hazard - early - random)

      plot_df <- data.frame(
        time   = t_seq,
        total  = hazard,
        early  = early,
        random = random,
        wear   = wear
      )

      ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$time)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$early,  color = "Early failures")) +
        ggplot2::geom_line(ggplot2::aes(y = .data$random, color = "Random failures")) +
        ggplot2::geom_line(ggplot2::aes(y = .data$wear,   color = "Wear-out failures")) +
        ggplot2::geom_line(ggplot2::aes(y = .data$total,  color = "Total hazard"),
                           linewidth = 1.2, linetype = "dashed") +
        ggplot2::scale_color_manual(
          name = "Failure phase",
          values = c("Early failures"   = "orange",
                     "Random failures"  = "blue",
                     "Wear-out failures" = "red",
                     "Total hazard"     = "black")
        ) +
        ggplot2::labs(
          x = "Time",
          y = "Hazard rate h(t)",
          title = "Bathtub Curve"
        ) +
        theme_obj$plot$theme_iqr()
    },

    # ========================================================================
    # Kaplan-Meier plots
    # ========================================================================

    .plot_km_full = function(results, theme_obj) {
      p1 <- private$.plot_km_curve(results, theme_obj)

      if (!requireNamespace("patchwork", quietly = TRUE)) {
        return(p1)
      }

      p2 <- private$.plot_km_risk_table(results, theme_obj)
      p1 / p2 + patchwork::plot_layout(heights = c(3, 1))
    },

    .plot_km_curve = function(results, theme_obj) {
      km_data <- results$survival_curve
      if (is.null(km_data) || nrow(km_data) == 0) {
        return(.empty_plot("No Kaplan-Meier data available."))
      }

      ggplot2::ggplot(km_data, ggplot2::aes(x = .data$time, y = .data$survival_prob)) +
        ggplot2::geom_step(color = .safe_primary(theme_obj), linewidth = 1.2) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_ci, ymax = .data$upper_ci),
                             fill = .safe_primary(theme_obj), alpha = 0.2) +
        ggplot2::labs(
          x = "Time",
          y = "Survival probability",
          title = "Kaplan-Meier Survival Curve"
        ) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        theme_obj$plot$theme_iqr()
    },

    .plot_km_risk_table = function(results, theme_obj) {
      km_data <- results$survival_curve
      if (is.null(km_data) || nrow(km_data) == 0) {
        return(.empty_plot("No risk table data."))
      }

      # Sample key time points for the at-risk table
      n_rows <- nrow(km_data)
      step <- max(1, n_rows %/% 5)
      key_times <- km_data[seq(1, n_rows, by = step), ]

      if (!requireNamespace("gridExtra", quietly = TRUE)) {
        return(.empty_plot("gridExtra required for risk table."))
      }

      table_grob <- gridExtra::tableGrob(
        key_times[, c("time", "n_risk", "n_event")],
        theme = gridExtra::ttheme_minimal()
      )

      ggplot2::ggplot() +
        ggplot2::annotation_custom(table_grob) +
        ggplot2::labs(title = "At-risk Table") +
        ggplot2::theme_void()
    },

    # ========================================================================
    # Cox model plots
    # ========================================================================

    .plot_cox_full = function(results, theme_obj) {
      p1 <- private$.plot_cox_forest(results, theme_obj)

      if (requireNamespace("patchwork", quietly = TRUE)) {
        p1 + patchwork::plot_layout()
      } else {
        p1
      }
    },

    .plot_cox_forest = function(results, theme_obj) {
      coef_df <- results$cox_model$coefficients
      if (is.null(coef_df) || nrow(coef_df) == 0) {
        return(.empty_plot("No Cox coefficients available."))
      }

      ggplot2::ggplot(coef_df, ggplot2::aes(x = .data$factor, y = .data$hazard_ratio)) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data$hr_lower, ymax = .data$hr_upper),
          width = 0.2, color = .safe_primary(theme_obj)
        ) +
        ggplot2::geom_point(color = .safe_primary(theme_obj), size = 3) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "Factor",
          y = "Hazard ratio (HR)",
          title = "Cox Model Forest Plot"
        ) +
        ggplot2::scale_y_log10() +
        theme_obj$plot$theme_iqr()
    }
  )
)

# ----------------------------------------------------------------------------
# Theme/color safe accessors (project memory: use safe helper functions)
# ----------------------------------------------------------------------------

.safe_primary <- function(theme_obj) {
  color <- tryCatch(
    theme_obj$config$config$ui$primary,
    error = function(e) NULL
  )
  if (is.null(color) || !nzchar(color)) "#2563EB" else color
}

.safe_danger <- function(theme_obj) {
  color <- tryCatch(
    theme_obj$config$config$ui$danger,
    error = function(e) NULL
  )
  if (is.null(color) || !nzchar(color)) "#B42318" else color
}

.safe_metric <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) NA_real_ else as.numeric(x)
}

.empty_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message,
                      hjust = 0.5, size = 5) +
    ggplot2::theme_void()
}
