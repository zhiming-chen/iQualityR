# =============================================================================
# File: R/predict/IqrPredictiveTask.R
# Description: Quality Prediction Modeling Task Coordinator
# =============================================================================

#' @title IqrPredictiveTask: Quality Prediction Modeling Task Coordinator
#' @description
#' Main entry point for prediction modeling tasks.
#' Coordinates Plan, Analyzer, Plotter, and Reporter four major components,
#' providing unified `compute()`, `summary()`, `plot()`, `report()` interfaces.
#'
#' @field plan PredictivePlan instance
#' @field results Computation results
#' @field data Original data
#' @field theme_obj Theme object
#' @field executor Executor list
#'
#' @export
IqrPredictiveTask <- R6::R6Class("IqrPredictiveTask",
  # Remove inherit = IqrTaskBase to make it an independent class

  public = list(
    #' @field plan Modeling plan
    plan = NULL,

    #' @field data Original data
    data = NULL,

    #' @field theme_obj Theme object
    theme_obj = NULL,

    #' @field results Computation results
    results = NULL,

    #' @field executor Executor list
    executor = list(),

    #' @description Initialize prediction modeling task
    #' @param data Data frame
    #' @param plan PredictivePlan object
    #' @param theme Theme name or IqrTheme object
    #' @param ... Other parameters
    initialize = function(data, plan, theme = "academic", ...) {
      # Directly initialize fields (replacing super$initialize)
      self$data <- data
      self$plan <- plan
      self$results <- NULL

      # Theme object handling
      if (is.character(theme)) {
        # Try to use iQualityR.core's IqrTheme
        if (requireNamespace("iQualityR.core", quietly = TRUE)) {
          self$theme_obj <- iQualityR.core::IqrTheme$new(theme_style = theme)
        } else {
          # If not available, use simplified version
          self$theme_obj <- list(style = theme, config = list())
        }
      } else {
        self$theme_obj <- theme
      }

      # Initialize executors
      self$executor <- list(
        analyzer = PredictiveAnalyzer$new(),
        plotter = PredictivePlotter$new(),
        reporter = PredictiveReporter$new(self$theme_obj)
      )

      invisible(self)
    },

    #' @description Execute modeling computation
    #' @return Self reference
    compute = function() {
      message("[iQualityR] Starting prediction modeling task...")

      # Call analyzer to execute
      self$executor$analyzer$run(
        data = self$data,
        plan = self$plan
      )

      # Store results
      self$results <- self$executor$analyzer$get_results()

      message("[iQualityR] Modeling computation complete")
      invisible(self)
    },

    #' @description Print model summary (quality-friendly format)
    #' @return Self reference
    summary = function() {
      if (is.null(self$results)) {
        cat("Computation not yet executed, please call compute() first\n")
        return(invisible(self))
      }

      cat("\n")
      cat("========================================================\n")
      cat("  Quality Prediction Model Summary\n")
      cat("========================================================\n\n")

      # 1. Model rating
      rating <- self$results$model_rating
      cat("Model Rating: ", paste(rep("*", rating$stars), collapse = ""),
          " (", rating$level, ")\n")
      cat("   -> ", rating$interpretation, "\n\n")

      # 2. Prediction capability metrics
      metrics <- self$results$metrics
      if (!is.null(metrics)) {
        cat("Prediction Capability Metrics:\n")
        cat(sprintf("   - R2 = %.3f  (Model explains %.1f%% of quality variation)\n",
                    metrics$r_squared, metrics$r_squared * 100))
        cat(sprintf("   - RMSE = %.3f (Root Mean Square Error)\n", metrics$rmse))
        cat(sprintf("   - MAE = %.3f  (Mean Absolute Error)\n", metrics$mae))
        if (!is.null(metrics$mape)) {
          cat(sprintf("   - MAPE = %.2f%% (Mean Absolute Percentage Error)\n", metrics$mape))
        }
        cat("\n")
      }

      # 3. Key influencing factors
      if (!is.null(self$results$explanation$feature_importance$importance)) {
        cat("Key Influencing Factors:\n")
        imp <- self$results$explanation$feature_importance
        top_n <- min(5, length(imp$percentage))
        for (i in seq_len(top_n)) {
          factor_name <- imp$ranking[i]
          pct <- imp$percentage[factor_name]
          bar <- paste(rep("#", round(pct / 5)), collapse = "")
          bar <- paste0(bar, paste(rep("-", 20 - round(pct / 5)), collapse = ""))
          cat(sprintf("   %d. %-15s (Influence: %5.1f%%) %s\n",
                      i, factor_name, pct, bar))
        }
        cat("\n")
      }

      # 4. Diagnostic conclusions
      if (!is.null(self$results$diagnostics$warnings) &&
          length(self$results$diagnostics$warnings) > 0) {
        cat("Diagnostic Conclusions:\n")
        for (w in self$results$diagnostics$warnings) {
          cat("   ! ", w, "\n")
        }
        cat("\n")
      } else {
        cat("Diagnostic Conclusions: No significant issues found\n\n")
      }

      # 5. Improvement recommendations
      if (!is.null(self$results$diagnostics$recommendations) &&
          length(self$results$diagnostics$recommendations) > 0) {
        cat("Improvement Recommendations:\n")
        for (i in seq_along(self$results$diagnostics$recommendations)) {
          cat(sprintf("   %d. %s\n", i, self$results$diagnostics$recommendations[i]))
        }
        cat("\n")
      }

      cat("========================================================\n")
      cat("\n")

      invisible(self)
    },

    #' @description Generate plots
    #' @param type Plot type ("basic", "diagnosis", "prediction", "explanation", "full")
    #' @param ... Other parameters passed to plotter
    #' @return patchwork or ggplot object
    plot = function(type = "basic", ...) {
      if (is.null(self$results)) {
        stop("[IqrPredictiveTask] Computation not yet executed, please call compute() first", call. = FALSE)
      }

      self$executor$plotter$render(
        results = self$results,
        theme_obj = self$theme_obj,
        type = type,
        ...
      )
    },

    #' @description Generate report
    #' @param format Output format ("excel", "html")
    #' @param path Output path (optional, auto-generate default name)
    #' @param ... Other parameters
    #' @return  invisble(self)
    report = function(format = "excel", path = NULL, ...) {
      if (is.null(self$results)) {
        stop("[IqrPredictiveTask] Computation not yet executed, please call compute() first", call. = FALSE)
      }

      self$executor$reporter$export(
        results = self$results,
        plan = self$plan,
        format = format,
        path = path,
        ...
      )

      invisible(self)
    },

    #' @description Predict new data
    #' @param new_data New data frame
    #' @param ... Other parameters
    #' @return Predicted values
    predict = function(new_data, ...) {
      if (is.null(self$results)) {
        stop("[IqrPredictiveTask] Model not yet trained, please call compute() first", call. = FALSE)
      }

      self$executor$analyzer$predict_new(new_data, ...)
    }
  )
)
