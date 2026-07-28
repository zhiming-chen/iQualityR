# =============================================================================
# File: R/predict/PredictivePlotter.R
# Description: Quality Prediction Modeling Plotting Module
# =============================================================================

.safe_theme_fn <- function(theme_obj, fn_name = "theme_iqr", default_fn = ggplot2::theme_minimal) {
  tryCatch({
    theme_obj$plot[[fn_name]]
  }, error = function(e) {
    default_fn
  })
}

#' @title PredictivePlotter: Quality Prediction Modeling Plot Executor
#' @description
#' Inherits from IqrPlotterBase, responsible for generating all prediction modeling
#' related visualization charts.
#
#' **Chart Types**:
#' - `basic`: Basic 4-panel (actual vs predicted, residuals, factor influence, model rating)
#' - `diagnosis`: Complete diagnostics (residual analysis, normality, multicollinearity, influence points)
#' - `prediction`: Prediction plot (predicted values, confidence intervals, error distribution)
#' - `explanation`: Explanation plots (factor influence, SHAP values, partial dependence)
#' - `full`: All charts combined
#'
#' @export
PredictivePlotter <- R6::R6Class("PredictivePlotter",
  # Remove inherit = IqrPlotterBase to make it an independent class

  public = list(
    #' @description Render plots
    #' @param results Analysis results list
    #' @param theme_obj IqrTheme object
    #' @param type Plot type ("basic", "diagnosis", "prediction", "explanation", "full")
    #' @param ... Other parameters
    #' @return patchwork or ggplot object
    render = function(results, theme_obj, type = "basic", ...) {
      if (is.null(results) || length(results) == 0) {
        stop("[PredictivePlotter] Results are empty, please execute compute() first", call. = FALSE)
      }

      message("[iQualityR] Generating plot type: ", type)

      # Determine task type from results metadata
      task_type <- if (!is.null(results$metadata$task_tag)) {
        results$metadata$task_tag
      } else {
        "regression"  # Default to regression for backward compatibility
      }

      switch(type,
        "basic"       = if (task_type == "classification") {
                          private$.plot_classification_dashboard(results, theme_obj)
                        } else {
                          private$.plot_basic_dashboard(results, theme_obj)
                        },
        "diagnosis"   = if (task_type == "classification") {
                          private$.plot_classification_diagnostics(results, theme_obj)
                        } else {
                          private$.plot_diagnostics(results, theme_obj)
                        },
        "prediction"  = private$.plot_predictions(results, theme_obj),
        "explanation" = private$.plot_explanations(results, theme_obj),
        "full"        = if (task_type == "classification") {
                          private$.plot_classification_full(results, theme_obj)
                        } else {
                          private$.plot_full(results, theme_obj)
                        },
        stop("[PredictivePlotter] Unsupported plot type: ", type, call. = FALSE)
      )
    }
  ),

  private = list(
    # ========== Basic 4-Panel Dashboard ==========

    .plot_basic_dashboard = function(results, theme_obj) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("[PredictivePlotter] Requires 'patchwork' package to be installed", call. = FALSE)
      }

      # Calculate actual values (reverse from fitted and residuals)
      actual <- NULL
      if (!is.null(results$diagnostics$residuals$fitted_values) &&
          !is.null(results$diagnostics$residuals$raw)) {
        actual <- results$diagnostics$residuals$fitted_values +
                  results$diagnostics$residuals$raw
      }

      # 1. Actual vs Predicted
      p1 <- private$.plot_fitted_vs_actual(
        actual = actual,
        predicted = results$fitted_values,
        theme_obj = theme_obj
      )

      # 2. Residuals vs Fitted
      p2 <- private$.plot_residual_vs_fitted(
        residuals = results$diagnostics$residuals$raw,
        fitted = results$diagnostics$residuals$fitted_values,
        theme_obj = theme_obj
      )

      # 3. Factor influence
      p3 <- private$.plot_feature_importance(
        importance = results$explanation$feature_importance$importance,
        theme_obj = theme_obj
      )

      # 4. Model rating card
      p4 <- private$.plot_model_rating_card(
        rating = results$model_rating,
        metrics = results$metrics,
        theme_obj = theme_obj
      )

      # Combine into 4-panel
      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      layout <- (p1 + p2) / (p3 + p4) +
        patchwork::plot_annotation(
          title = "Quality Prediction Model Overview",
          subtitle = paste("Target Variable:", results$metadata$target_var,
                           "| Model Type:", results$metadata$model_type),
          theme = theme_fn()
        )

      layout
    },

    # ========== Complete Diagnostics Plot ==========

    .plot_diagnostics = function(results, theme_obj) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("[PredictivePlotter] Requires 'patchwork' package to be installed", call. = FALSE)
      }

      plots <- list()

      # 1. Residuals vs Fitted
      if (!is.null(results$diagnostics$residuals)) {
        plots[[1]] <- private$.plot_residual_vs_fitted(
          residuals = results$diagnostics$residuals$raw,
          fitted = results$diagnostics$residuals$fitted_values,
          theme_obj = theme_obj
        )
      }

      # 2. Q-Q plot
      if (!is.null(results$diagnostics$normality$qq_data)) {
        plots[[2]] <- private$.plot_qq(
          qq_data = results$diagnostics$normality$qq_data,
          theme_obj = theme_obj
        )
      }

      # 3. Residual histogram
      if (!is.null(results$diagnostics$residuals$raw)) {
        plots[[3]] <- private$.plot_residual_histogram(
          residuals = results$diagnostics$residuals$raw,
          theme_obj = theme_obj
        )
      }

      # 4. VIF plot
      if (!is.null(results$diagnostics$multicollinearity$vif)) {
        plots[[4]] <- private$.plot_vif(
          vif_df = results$diagnostics$multicollinearity$vif,
          theme_obj = theme_obj
        )
      }

      # 5. Leverage plot
      if (!is.null(results$diagnostics$influence$leverage)) {
        plots[[5]] <- private$.plot_leverage(
          leverage_df = results$diagnostics$influence$leverage,
          theme_obj = theme_obj
        )
      }

      # 6. Cook's distance plot
      if (!is.null(results$diagnostics$influence$cooks_distance)) {
        plots[[6]] <- private$.plot_cooks_distance(
          cooks_df = results$diagnostics$influence$cooks_distance,
          theme_obj = theme_obj
        )
      }

      # Combine
      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      if (length(plots) > 0) {
        patchwork::wrap_plots(plots, ncol = 2) +
          patchwork::plot_annotation(
            title = "Model Diagnostics Plot",
            theme = theme_fn()
          )
      } else {
        message("[PredictivePlotter] No available diagnostics plots")
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                           label = "No diagnostics data available", size = 5) +
          ggplot2::theme_void()
      }
    },

    # ========== Prediction Plot ==========

    .plot_predictions = function(results, theme_obj) {
      # Simplified implementation: actual vs predicted scatter plot
      private$.plot_fitted_vs_actual(
        actual = NULL,  # Need to get from raw data
        predicted = results$fitted_values,
        theme_obj = theme_obj
      )
    },

    # ========== Explanation Plot ==========

    .plot_explanations = function(results, theme_obj) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("[PredictivePlotter] Requires 'patchwork' package to be installed", call. = FALSE)
      }

      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      plots <- list()

      # 1. Factor influence bar chart
      if (!is.null(results$explanation$feature_importance$importance) &&
          length(results$explanation$feature_importance$importance) > 0) {
        plots[[1]] <- private$.plot_feature_importance(
          importance = results$explanation$feature_importance$importance,
          theme_obj = theme_obj
        )
      }

      # 2. SHAP summary plot (if available)
      if (!is.null(results$explanation$shap$available) &&
          results$explanation$shap$available &&
          !is.null(results$explanation$shap$values)) {
        plots[[2]] <- private$.plot_shap_summary(
          shap_result = results$explanation$shap,
          theme_obj = theme_obj
        )
      }

      # 3. Partial dependence plot (if available)
      if (!is.null(results$explanation$partial_dependence) &&
          length(results$explanation$partial_dependence) > 0) {
        plots[[3]] <- private$.plot_partial_dependence(
          pdp_results = results$explanation$partial_dependence,
          theme_obj = theme_obj
        )
      }

      if (length(plots) > 0) {
        patchwork::wrap_plots(plots, ncol = 2) +
          patchwork::plot_annotation(
            title = "Model Explanation Plot",
            theme = theme_fn()
          )
      } else {
        message("[PredictivePlotter] No available explanation plots")
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                           label = "No explanation data available", size = 5) +
          ggplot2::theme_void()
      }
    },

    # ========== Full Plot ==========

    .plot_full = function(results, theme_obj) {
      # Combine all chart types
      p_basic <- private$.plot_basic_dashboard(results, theme_obj)
      p_diag <- private$.plot_diagnostics(results, theme_obj)
      p_exp <- private$.plot_explanations(results, theme_obj)

      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      p_basic / p_diag / p_exp +
        patchwork::plot_annotation(
          title = "Quality Prediction Model Complete Report",
          theme = theme_fn()
        )
    },

    # ========== Plot Helper Functions ==========

    .plot_fitted_vs_actual = function(actual, predicted, theme_obj) {
      if (is.null(actual) || is.null(predicted)) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "Data not available", size = 6) +
                 ggplot2::theme_void())
      }

      df <- data.frame(
        actual = actual,
        predicted = predicted
      )

      primary_color <- .iqr_plotter$.pal_discrete(theme_obj)[1]
      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      ggplot2::ggplot(df, ggplot2::aes(x = actual, y = predicted)) +
        ggplot2::geom_point(alpha = 0.6, color = primary_color) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                             color = .iqr_plotter$.pal_semantic(theme_obj, "fail"), linewidth = 1) +
        theme_fn() +
        ggplot2::labs(
          x = "Actual",
          y = "Predicted",
          title = "Actual vs Predicted"
        )
    },

    .plot_residual_vs_fitted = function(residuals, fitted, theme_obj) {
      if (is.null(residuals) || is.null(fitted)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      df <- data.frame(
        fitted = fitted,
        residual = residuals
      )

      primary_color <- .iqr_plotter$.pal_discrete(theme_obj)[1]
      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = residual)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
        ggplot2::geom_point(alpha = 0.6, color = primary_color) +
        ggplot2::geom_smooth(method = "loess", color = .iqr_plotter$.pal_semantic(theme_obj, "fail"), linewidth = 1) +
        theme_fn() +
        ggplot2::labs(
          x = "Fitted",
          y = "Residual",
          title = "Residuals vs Fitted"
        )
    },

    .plot_qq = function(qq_data, theme_obj) {
      if (is.null(qq_data)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      primary_color <- .iqr_plotter$.pal_discrete(theme_obj)[1]
      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      ggplot2::ggplot(qq_data, ggplot2::aes(x = theoretical, y = sample)) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                             color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
        ggplot2::geom_point(color = primary_color) +
        theme_fn() +
        ggplot2::labs(
          x = "Theoretical Quantiles",
          y = "Sample Quantiles",
          title = "Normal Q-Q Plot"
        )
    },

    .plot_residual_histogram = function(residuals, theme_obj) {
      if (is.null(residuals)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      df <- data.frame(residual = residuals)
      mean_res <- mean(residuals)
      sd_res <- sd(residuals)

      primary_color <- .iqr_plotter$.pal_discrete(theme_obj)[1]
      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      ggplot2::ggplot(df, ggplot2::aes(x = residual)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                bins = 20, fill = primary_color,
                                alpha = 0.7) +
        ggplot2::geom_density(color = .iqr_plotter$.pal_semantic(theme_obj, "fail"), linewidth = 1) +
        ggplot2::geom_vline(xintercept = mean_res, linetype = "dashed", color = .iqr_plotter$.pal_discrete(theme_obj)[2]) +
        theme_fn() +
        ggplot2::labs(
          x = "Residual",
          y = "Density",
          title = "Residual Distribution"
        )
    },

    .plot_vif = function(vif_df, theme_obj) {
      if (is.null(vif_df)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      # Sort by VIF value
      vif_df <- vif_df[order(-vif_df$vif), ]
      vif_df$term <- factor(vif_df$term, levels = vif_df$term)

      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      ggplot2::ggplot(vif_df, ggplot2::aes(x = term, y = vif, fill = severity)) +
        ggplot2::geom_col() +
        ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = .iqr_plotter$.pal_ui(theme_obj, "warning")) +
        ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
        theme_fn() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(
          x = "Factor",
          y = "VIF",
          title = "Variance Inflation Factor (VIF)",
          fill = "Severity"
        )
    },

    .plot_leverage = function(leverage_df, theme_obj) {
      if (is.null(leverage_df)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      df <- leverage_df
      df$is_highlight <- df$is_high

      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      ggplot2::ggplot(df, ggplot2::aes(x = observation, y = hat_value)) +
        ggplot2::geom_hline(yintercept = df$threshold[1], linetype = "dashed",
                            color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
        ggplot2::geom_point(color = ifelse(df$is_highlight, .iqr_plotter$.pal_semantic(theme_obj, "fail"), "grey"),
                            alpha = 0.7) +
        theme_fn() +
        ggplot2::labs(
          x = "Observation Index",
          y = "Leverage",
          title = "Leverage Plot"
        )
    },

    .plot_cooks_distance = function(cooks_df, theme_obj) {
      if (is.null(cooks_df)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      ggplot2::ggplot(cooks_df, ggplot2::aes(x = observation, y = cooks_d)) +
        ggplot2::geom_hline(yintercept = cooks_df$threshold[1], linetype = "dashed",
                            color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
        ggplot2::geom_point(color = ifelse(cooks_df$is_influential, .iqr_plotter$.pal_semantic(theme_obj, "fail"), "grey"),
                            alpha = 0.7) +
        theme_fn() +
        ggplot2::labs(
          x = "Observation Index",
          y = "Cook's Distance",
          title = "Cook's Distance Plot"
        )
    },

    .plot_feature_importance = function(importance, theme_obj) {
      if (is.null(importance) || length(importance) == 0) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "Feature importance not available", size = 5) +
                 ggplot2::theme_void())
      }

      # Convert to data frame and sort
      df <- data.frame(
        factor = names(importance),
        importance = as.numeric(importance),
        stringsAsFactors = FALSE
      )

      # Check for all-zero or invalid importance
      if (all(df$importance == 0, na.rm = TRUE) || all(is.na(df$importance))) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "All feature importance values are zero\n(Model may have no significant predictors)", size = 4) +
                 ggplot2::theme_void())
      }

      df <- df[order(-df$importance), ]
      df$factor <- factor(df$factor, levels = df$factor)

      # Calculate percentage
      total_imp <- sum(abs(df$importance), na.rm = TRUE)
      if (total_imp > 0) {
        df$percentage <- abs(df$importance) / total_imp * 100
      } else {
        df$percentage <- rep(0, nrow(df))
      }

      # Determine bar fill color using unified palette
      bar_color <- .iqr_plotter$.pal_discrete(theme_obj)[1]

      ggplot2::ggplot(df, ggplot2::aes(x = factor, y = importance)) +
        ggplot2::geom_col(fill = bar_color) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", percentage)),
                           vjust = -0.5) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(
          x = "Influencing Factor",
          y = "Importance",
          title = "Factor Influence Ranking"
        )
    },

    .plot_model_rating_card = function(rating, metrics, theme_obj) {
      # Create a simple text card showing model rating
      # Use gridExtra to draw in table format

      if (!requireNamespace("gridExtra", quietly = TRUE)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      # Helper function for null coalescing
      `%||%` <- function(a, b) {
        if (is.null(a)) b else a
      }

      # Build rating text
      stars_text <- paste(rep("*", rating$stars), collapse = "")
      stars_text <- paste0(stars_text, paste(rep("o", 5 - rating$stars), collapse = ""))

      # Safely extract metrics with null coalescing
      r2_val <- ifelse(is.null(metrics$r_squared), NA, metrics$r_squared)
      rmse_val <- ifelse(is.null(metrics$rmse), NA, metrics$rmse)
      mae_val <- ifelse(is.null(metrics$mae), NA, metrics$mae)

      # Create data frame for display
      display_df <- data.frame(
        Metric = c("Model Rating", "R2", "RMSE", "MAE"),
        Value = c(
          paste0(rating$level, " ", stars_text),
          sprintf("%.3f", r2_val),
          sprintf("%.3f", rmse_val),
          sprintf("%.3f", mae_val)
        ),
        stringsAsFactors = FALSE
      )

      # Use gridExtra to draw table
      tbl <- gridExtra::tableGrob(display_df, rows = NULL)

      # Return as a wrap_elements full plot
      patchwork::wrap_elements(full = tbl) +
        ggplot2::theme(plot.margin = ggplot2::margin(5, 5, 5, 5))
    },

    .plot_shap_summary = function(shap_result, theme_obj) {
      if (is.null(shap_result) || !shap_result$available) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "SHAP data not available", size = 5) +
                 ggplot2::theme_void())
      }

      # SHAP summary plot: show direction and magnitude of each factor's influence on prediction
      shap_values <- shap_result$values
      
      # Check if SHAP values are empty
      if (is.null(shap_values)) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "SHAP values are empty", size = 5) +
                 ggplot2::theme_void())
      }
      
      # Ensure shap_values is numeric matrix
      if (!is.matrix(shap_values)) {
        shap_values <- as.matrix(shap_values)
      }
      
      # Keep only numeric columns
      numeric_cols <- sapply(seq_len(ncol(shap_values)), function(i) {
        is.numeric(shap_values[, i])
      })
      if (!all(numeric_cols)) {
        warning("[PredictivePlotter] SHAP values contain non-numeric columns, removed")
        shap_values <- shap_values[, numeric_cols, drop = FALSE]
      }
      
      factor_names <- colnames(shap_values)

      # Calculate mean absolute SHAP value as importance
      mean_abs_shap <- colMeans(abs(shap_values))
      importance_df <- data.frame(
        factor = factor_names,
        importance = mean_abs_shap,
        stringsAsFactors = FALSE
      )
      importance_df <- importance_df[order(-importance_df$importance), ]
      importance_df$factor <- factor(importance_df$factor, levels = importance_df$factor)

      bar_color <- .iqr_plotter$.pal_discrete(theme_obj)[1]
      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      ggplot2::ggplot(importance_df, ggplot2::aes(x = factor, y = importance)) +
        ggplot2::geom_col(fill = bar_color) +
        ggplot2::coord_flip() +
        theme_fn() +
        ggplot2::labs(
          x = "Influencing Factor",
          y = "Mean |SHAP Value|",
          title = "SHAP Factor Influence"
        )
    },

    .plot_partial_dependence = function(pdp_results, theme_obj) {
      if (length(pdp_results) == 0) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      bar_color <- .iqr_plotter$.pal_discrete(theme_obj)[1]
      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      # Create partial dependence plot for each factor, combine into multi-panel
      plots <- list()
      for (i in seq_along(pdp_results)) {
        pdp <- pdp_results[[i]]
        if (is.null(pdp) || is.null(pdp$x_values)) next

        df <- data.frame(
          x = pdp$x_values,
          pdp = pdp$pdp_values
        )

        p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = pdp)) +
          ggplot2::geom_line(color = bar_color, linewidth = 1) +
          ggplot2::geom_point(color = bar_color, size = 2) +
          ggplot2::geom_hline(yintercept = mean(pdp$pdp_values), linetype = "dashed",
                              color = "grey") +
          theme_fn() +
          ggplot2::labs(
            x = pdp$factor,
            y = "Predicted Value",
            title = paste("Factor-Response Relationship:", pdp$factor)
          )

        plots[[i]] <- p
      }

      if (length(plots) > 0) {
        patchwork::wrap_plots(plots, ncol = 2)
      } else {
        ggplot2::ggplot() + ggplot2::theme_void()
      }
    },

    # ========== Classification Task Exclusive Charts ==========

    .plot_confusion_matrix = function(actual, predicted, theme_obj) {
      if (is.null(actual) || is.null(predicted)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      # Build confusion matrix
      cm <- table(Actual = actual, Predicted = predicted)
      cm_df <- as.data.frame(cm)

      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      ggplot2::ggplot(cm_df, ggplot2::aes(x = Predicted, y = Actual, fill = Freq)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(ggplot2::aes(label = Freq), size = 5) +
        theme_fn() +
        .iqr_plotter$.scale_fill_sequential(theme_obj) +
        ggplot2::labs(title = "Confusion Matrix")
    },

    .plot_roc_curve = function(true_labels, predicted_probs, theme_obj) {
      if (is.null(true_labels) || is.null(predicted_probs)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      if (!requireNamespace("pROC", quietly = TRUE)) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "Requires pROC package", size = 5) +
                 ggplot2::theme_void())
      }

      tryCatch({
        roc_obj <- pROC::roc(true_labels, predicted_probs, quiet = TRUE)
        auc_val <- pROC::auc(roc_obj)

        # Extract ROC curve data
        roc_df <- data.frame(
          fpr = 1 - roc_obj$specificities,
          tpr = roc_obj$sensitivities
        )

        bar_color <- .iqr_plotter$.pal_discrete(theme_obj)[1]
        theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

        ggplot2::ggplot(roc_df, ggplot2::aes(x = fpr, y = tpr)) +
          ggplot2::geom_line(color = bar_color, linewidth = 1.5) +
          ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
          ggplot2::annotate("text", x = 0.75, y = 0.25,
                            label = sprintf("AUC = %.3f", auc_val), size = 6) +
          theme_fn() +
          ggplot2::labs(
            x = "1 - Specificity (FPR)",
            y = "Sensitivity (TPR)",
            title = "ROC Curve"
          ) +
          ggplot2::coord_fixed()
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "ROC curve generation failed", size = 5) +
          ggplot2::theme_void()
      })
    },

    # ========== Time Series Forecasting Exclusive Charts ==========

    .plot_time_series_forecast = function(history, forecast, ci_lower, ci_upper, theme_obj) {
      if (is.null(history) || is.null(forecast)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      n_history <- length(history)
      n_forecast <- length(forecast)

      # Build time index
      time_idx <- c(seq_len(n_history), (n_history + 1):(n_history + n_forecast))

      df <- data.frame(
        time = time_idx,
        value = c(history, forecast),
        type = c(rep("Historical", n_history), rep("Forecast", n_forecast)),
        ci_lower = c(rep(NA, n_history), ci_lower),
        ci_upper = c(rep(NA, n_history), ci_upper)
      )

      ggplot2::ggplot(df, ggplot2::aes(x = time, y = value, color = type)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2) +
        {if (!all(is.na(df$ci_lower))) ggplot2::geom_ribbon(
          ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
          fill = .iqr_plotter$.pal_ui(theme_obj, "muted"), alpha = 0.3, inherit.aes = FALSE,
          data = df[!is.na(df$ci_lower), ]
        )} +
        .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)() +
        ggplot2::labs(
          x = "Time",
          y = "Value",
          title = "Time Series Forecast",
          color = "Type"
        )
    },

    .plot_time_series_decompose = function(ts_object, theme_obj) {
      # Simplified implementation: show message
      message("[PredictivePlotter] Time series decomposition plot to be implemented")
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "Time series decomposition plot to be implemented", size = 5) +
        ggplot2::theme_void()
    },

    # ========== Model Comparison Charts ==========

    .plot_model_comparison_radar = function(metrics_by_model, model_names, metric_names, theme_obj) {
      if (is.null(metrics_by_model)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      # Normalize metrics to 0-1 range
      normalized <- metrics_by_model
      for (m in metric_names) {
        range_m <- range(normalized[, m])
        if (range_m[2] > range_m[1]) {
          normalized[, m] <- (normalized[, m] - range_m[1]) / (range_m[2] - range_m[1])
        }
      }

      # Convert to long format
      plot_df <- reshape2::melt(normalized, id.vars = NULL, measure.vars = metric_names)
      plot_df$model <- rep(model_names, each = length(metric_names))

      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      ggplot2::ggplot(plot_df, ggplot2::aes(x = variable, y = value, color = model, group = model)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 3) +
        theme_fn() +
        ggplot2::labs(
          x = "Metric",
          y = "Normalized Value",
          title = "Model Performance Radar Comparison"
        )
    },

    .plot_learning_curve = function(train_sizes, train_scores, val_scores, theme_obj) {
      if (is.null(train_sizes) || is.null(train_scores) || is.null(val_scores)) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }

      df <- data.frame(
        size = train_sizes,
        train = train_scores,
        validation = val_scores
      )

      df_long <- reshape2::melt(df, id.vars = "size",
                                measure.vars = c("train", "validation"),
                                variable.name = "Dataset",
                                value.name = "Score")

      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)

      ggplot2::ggplot(df_long, ggplot2::aes(x = size, y = score, color = dataset)) +
        ggplot2::geom_line(linewidth = 1.2) +
        ggplot2::geom_point(size = 3) +
        theme_fn() +
        ggplot2::labs(
          x = "Training Sample Size",
          y = "Performance Score",
          title = "Learning Curve"
        )
    },

    # ========== Classification Task Dashboard ==========

    .plot_classification_dashboard = function(results, theme_obj) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("[PredictivePlotter] Requires 'patchwork' package to be installed", call. = FALSE)
      }

      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)
      bar_color <- .iqr_plotter$.pal_discrete(theme_obj)[1]

      plots <- list()

      # 1. Confusion Matrix (top-left)
      if (!is.null(results$metrics$confusion_matrix)) {
        cm <- results$metrics$confusion_matrix
        cm_df <- as.data.frame(cm)
        if (nrow(cm_df) > 0 && ncol(cm_df) > 0) {
          p1 <- ggplot2::ggplot(cm_df, ggplot2::aes(x = Predicted, y = Actual, fill = Freq)) +
            ggplot2::geom_tile() +
            ggplot2::geom_text(ggplot2::aes(label = Freq), size = 6, fontface = "bold") +
            theme_fn() +
            .iqr_plotter$.scale_fill_sequential(theme_obj) +
            ggplot2::labs(title = "Confusion Matrix")
          plots[[1]] <- p1
        }
      }

      # 2. ROC Curve (top-right)
      if (!is.null(results$metrics$auc) && !is.na(results$metrics$auc)) {
        # Get predicted probabilities and true labels
        pred_probs <- results$predicted_probs
        true_labels <- results$true_labels  # True labels for ROC curve

        if (!is.null(pred_probs) && length(pred_probs) > 0 && !is.null(true_labels)) {
          p2 <- private$.plot_roc_curve(true_labels, pred_probs, theme_obj)
          plots[[2]] <- p2
        } else {
          # Create a placeholder for ROC
          p2 <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("AUC =", sprintf("%.3f", results$metrics$auc)), size = 8) +
            theme_fn() +
            ggplot2::labs(title = "ROC Curve (AUC Available)") +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
          plots[[2]] <- p2
        }
      }

      # 3. Classification Metrics Summary (bottom-left) - use bar chart for visual consistency
      metrics <- results$metrics
      accuracy <- ifelse(is.null(metrics$accuracy), NA, metrics$accuracy)
      precision <- ifelse(is.null(metrics$precision), NA, metrics$precision)
      recall <- ifelse(is.null(metrics$recall), NA, metrics$recall)
      f1 <- ifelse(is.null(metrics$f1_score), NA, metrics$f1_score)
      auc_val <- ifelse(is.null(metrics$auc), NA, metrics$auc)

      metrics_df <- data.frame(
        Metric = c("Accuracy", "Precision", "Recall", "F1-Score", "AUC"),
        Value = as.numeric(c(accuracy, precision, recall, f1, auc_val)),
        stringsAsFactors = FALSE
      )

      # Remove rows with NA values for plotting
      metrics_df <- metrics_df[!is.na(metrics_df$Value), ]

      if (nrow(metrics_df) > 0) {
        p3 <- ggplot2::ggplot(metrics_df, ggplot2::aes(x = Metric, y = Value, fill = Metric)) +
          ggplot2::geom_col(width = 0.6) +
          ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", Value)), vjust = -0.5, size = 4) +
          ggplot2::scale_y_continuous(limits = c(0, 1.1), expand = ggplot2::expansion(mult = c(0, 0.1))) +
          theme_fn() +
          .iqr_plotter$.scale_fill_discrete(theme_obj) +
          ggplot2::labs(title = "Classification Metrics", y = "Value", x = "") +
          ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      } else {
        p3 <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Metrics not available", size = 5) +
          ggplot2::theme_void()
      }

      plots[[3]] <- p3

      # 4. Class Distribution (bottom-right)
      if (!is.null(results$metrics$confusion_matrix)) {
        cm <- results$metrics$confusion_matrix
        if (length(dim(cm)) == 2) {
          class_counts <- rowSums(cm)
          class_df <- data.frame(
            Class = names(class_counts),
            Count = as.numeric(class_counts)
          )

          p4 <- ggplot2::ggplot(class_df, ggplot2::aes(x = Class, y = Count, fill = Class)) +
            ggplot2::geom_col(width = 0.6) +
            ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.5, size = 4) +
            theme_fn() +
            .iqr_plotter$.scale_fill_discrete(theme_obj) +
            ggplot2::labs(title = "Class Distribution") +
            ggplot2::theme(legend.position = "none")
          plots[[4]] <- p4
        }
      }

      # Handle case where we don't have enough plots
      n_plots <- length(plots)
      if (n_plots == 0) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "Classification results not available", size = 5) +
                 ggplot2::theme_void())
      }

      # Create 2x2 layout
      if (n_plots == 1) {
        return(plots[[1]])
      } else if (n_plots == 2) {
        return(plots[[1]] / plots[[2]])
      } else if (n_plots == 3) {
        return((plots[[1]] | plots[[2]]) / plots[[3]])
      } else {
        return((plots[[1]] | plots[[2]]) / (plots[[3]] | plots[[4]]))
      }
    },

    .plot_classification_diagnostics = function(results, theme_obj) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("[PredictivePlotter] Requires 'patchwork' package to be installed", call. = FALSE)
      }

      theme_fn <- .safe_theme_fn(theme_obj, "theme_iqr", ggplot2::theme_minimal)
      bar_color <- .safe_theme_color(theme_obj, "primary", "#4477AA")

      plots <- list()

      # 1. Confusion Matrix (full)
      if (!is.null(results$metrics$confusion_matrix)) {
        cm <- results$metrics$confusion_matrix
        cm_df <- as.data.frame(cm)
        if (nrow(cm_df) > 0 && ncol(cm_df) > 0) {
          p1 <- ggplot2::ggplot(cm_df, ggplot2::aes(x = Predicted, y = Actual, fill = Freq)) +
            ggplot2::geom_tile() +
            ggplot2::geom_text(ggplot2::aes(label = Freq), size = 8, fontface = "bold") +
            theme_fn() +
            ggplot2::scale_fill_gradient(low = "white", high = bar_color) +
            ggplot2::labs(title = "Confusion Matrix")
          plots[[1]] <- p1
        }
      }

      # 2. ROC Curve
      if (!is.null(results$metrics$auc) && !is.na(results$metrics$auc)) {
        pred_probs <- results$predicted_probs
        true_labels <- results$true_labels  # True labels for ROC curve

        if (!is.null(pred_probs) && length(pred_probs) > 0 && !is.null(true_labels)) {
          plots[[2]] <- private$.plot_roc_curve(true_labels, pred_probs, theme_obj)
        } else {
          plots[[2]] <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("AUC =", sprintf("%.3f", results$metrics$auc)), size = 8) +
            theme_fn() +
            ggplot2::labs(title = "ROC Curve") +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
        }
      }

      # 3. Class Distribution
      if (!is.null(results$metrics$confusion_matrix)) {
        cm <- results$metrics$confusion_matrix
        if (length(dim(cm)) == 2) {
          class_counts <- rowSums(cm)
          class_df <- data.frame(
            Class = names(class_counts),
            Count = as.numeric(class_counts)
          )

          p3 <- ggplot2::ggplot(class_df, ggplot2::aes(x = Class, y = Count, fill = Class)) +
            ggplot2::geom_col(width = 0.6) +
            ggplot2::geom_text(ggplot2::aes(label = paste0(Count, " (", round(Count/sum(Count)*100,1), "%)")), vjust = -0.5, size = 4) +
            theme_fn() +
            ggplot2::scale_fill_brewer(palette = "Set2") +
            ggplot2::labs(title = "Class Distribution") +
            ggplot2::theme(legend.position = "none")
          plots[[3]] <- p3
        }
      }

      # 4. Metrics Bar Chart
      metrics <- results$metrics
      metrics_df <- data.frame(
        Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
        Value = c(
          ifelse(is.null(metrics$accuracy), 0, metrics$accuracy),
          ifelse(is.null(metrics$precision), 0, metrics$precision),
          ifelse(is.null(metrics$recall), 0, metrics$recall),
          ifelse(is.null(metrics$f1_score), 0, metrics$f1_score)
        )
      )

      p4 <- ggplot2::ggplot(metrics_df, ggplot2::aes(x = Metric, y = Value, fill = Metric)) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", Value)), vjust = -0.5, size = 4) +
        ggplot2::ylim(0, 1.1) +
        theme_fn() +
        ggplot2::scale_fill_brewer(palette = "Set3") +
        ggplot2::labs(title = "Classification Metrics") +
        ggplot2::theme(legend.position = "none")
      plots[[4]] <- p4

      n_plots <- length(plots)
      if (n_plots == 0) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "Classification diagnostics not available", size = 5) +
                 ggplot2::theme_void())
      }

      patchwork::wrap_plots(plots, ncol = 2) +
        patchwork::plot_annotation(
          title = "Classification Model Diagnostics",
          theme = theme_fn()
        )
    },

    .plot_classification_full = function(results, theme_obj) {
      p_basic <- private$.plot_classification_dashboard(results, theme_obj)
      p_diag <- private$.plot_classification_diagnostics(results, theme_obj)

      p_basic / p_diag +
        patchwork::plot_annotation(
          title = "Classification Model Complete Report",
          theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16))
        )
    }
  )
)
