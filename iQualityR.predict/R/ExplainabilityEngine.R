# =============================================================================
# File: R/predict/ExplainabilityEngine.R
# Description: Quality Prediction Model Explainability Analysis Engine
# =============================================================================

#' @title ExplainabilityEngine: Quality Prediction Model Explainability Analysis Engine
#' @description
#' Provides interpretability analysis for quality prediction models, helping to understand
#' the influence and direction of each factor on quality indicators.
#
#' Translates machine learning terminology to quality domain language:
#' - "Feature Importance" -> "Factor Influence"
#' - "SHAP Values" -> "Factor Contribution Decomposition"
#' - "Partial Dependence" -> "Factor-Response Relationship"
#'
#' **Analysis Items**:
#' - Factor influence ranking (global interpretation)
#' - SHAP value decomposition (local + global interpretation)
#' - Partial Dependence Plot PDP (factor-response relationship)
#'
#' @export
ExplainabilityEngine <- R6::R6Class("ExplainabilityEngine",
  public = list(
    #' @field explanation Explanation results
    explanation = list(),

    #' @description Execute explainability analysis
    #' @param model_result Model training result
    #' @param data Original training data
    #' @param plan PredictivePlan object
    #' @return Self reference
    explain = function(model_result, data, plan) {
      message("[iQualityR] === Model Explainability Analysis ===")

      # Clear results
      self$explanation <- list()

      model <- model_result$raw_model
      target <- plan$target_var
      factors <- plan$factor_vars
      exp_config <- plan$explanation

      # 1. Factor influence analysis (enabled by default)
      if (exp_config$feature_importance) {
        message("[iQualityR] Computing factor influence...")
        self$explanation$feature_importance <- private$.compute_feature_importance(
          model, data, factors
        )
      }

      # 2. SHAP value decomposition (disabled by default, high computational cost)
      if (exp_config$shap_values) {
        message("[iQualityR] Computing SHAP value decomposition...")
        self$explanation$shap <- private$.compute_shap_values(
          model, data, factors, target
        )
      }

      # 3. Partial dependence plot (disabled by default)
      if (exp_config$partial_dependence) {
        message("[iQualityR] Computing partial dependence...")
        self$explanation$partial_dependence <- private$.compute_partial_dependence(
          model, data, factors, target
        )
      }

      message("[iQualityR] Explainability analysis complete")
      invisible(self)
    },

    #' @description Get explanation summary
    get_summary = function() {
      list(
        feature_importance = self$explanation$feature_importance,
        shap = self$explanation$shap,
        partial_dependence = self$explanation$partial_dependence
      )
    }
  ),

  private = list(
    .compute_feature_importance = function(model, data, factors) {
      result <- list()

      tryCatch({
        # Select importance calculation method based on model type
        if (inherits(model, "lm")) {
          # Linear model: use standardized coefficients or t-statistics
          result <- private$.importance_from_lm(model, factors)
        } else if (inherits(model, "randomForest")) {
          # Random forest: use built-in importance
          result <- private$.importance_from_rf(model, factors)
        } else if (inherits(model, "xgb.Booster")) {
          # XGBoost: use built-in importance
          result <- private$.importance_from_xgb(model, factors)
        } else if (requireNamespace("vip", quietly = TRUE)) {
          # Other models: use vip package
          result <- private$.importance_from_vip(model, data, factors)
        } else {
          # Fallback: use permutation importance
          result <- private$.importance_from_permutation(model, data, factors)
        }

        # Calculate percentage contribution
        if (!is.null(result$importance) && length(result$importance) > 0) {
          abs_imp <- abs(result$importance)
          result$percentage <- abs_imp / sum(abs_imp) * 100
          result$ranking <- names(sort(result$percentage, decreasing = TRUE))
        }
      }, error = function(e) {
        message("[iQualityR] Factor influence computation failed: ", e$message)
        result$error <- e$message
      })

      result
    },

    .importance_from_lm = function(model, factors) {
      # Use standardized coefficients (beta coefficients) as importance
      coef_values <- stats::coef(model)

      # Exclude intercept
      if (names(coef_values)[1] == "(Intercept)") {
        coef_values <- coef_values[-1]
      }

      # Keep only items in factors
      # Simplified handling: assume factor names correspond directly
      importance <- numeric()
      for (f in factors) {
        # Find matching coefficients
        matching <- grep(f, names(coef_values), value = TRUE)
        if (length(matching) > 0) {
          importance[f] <- sum(abs(coef_values[matching]))
        } else {
          importance[f] <- 0
        }
      }

      list(
        method = "Standardized coefficients",
        importance = importance,
        coefficients = coef_values
      )
    },

    .importance_from_rf = function(model, factors) {
      importance <- model$importance

      # Handle randomForest importance structure
      # randomForest returns a matrix with columns: %IncMSE, IncNodePurity
      # We need to extract as a named vector
      if (is.matrix(importance)) {
        # Use IncNodePurity column (or %IncMSE if preferred)
        if ("IncNodePurity" %in% colnames(importance)) {
          importance <- setNames(importance[, "IncNodePurity"], rownames(importance))
        } else if ("%IncMSE" %in% colnames(importance)) {
          importance <- setNames(importance[, "%IncMSE"], rownames(importance))
        } else {
          # Fallback: use first column
          importance <- setNames(importance[, 1], rownames(importance))
        }
      }

      # Keep only target factors that exist in the importance names
      valid_factors <- factors[factors %in% names(importance)]
      if (length(valid_factors) > 0) {
        importance <- importance[valid_factors]
      }

      list(
        method = "Random Forest built-in importance",
        importance = importance
      )
    },

    .importance_from_xgb = function(model, factors) {
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        return(list(method = "xgboost", importance = numeric()))
      }

      imp <- xgboost::xgb.importance(model = model)
      importance <- setNames(imp$Gain, imp$Feature)

      list(
        method = "XGBoost gain importance",
        importance = importance
      )
    },

    .importance_from_vip = function(model, data, factors) {
      if (!requireNamespace("vip", quietly = TRUE)) {
        return(list(method = "vip", importance = numeric()))
      }

      tryCatch({
        vip_result <- vip::vi(model, train = data, method = "firm")
        importance <- setNames(vip_result$Importance, vip_result$Variable)

        list(
          method = "VIP (Variable Importance Projection)",
          importance = importance
        )
      }, error = function(e) {
        list(method = "vip", importance = numeric(), error = e$message)
      })
    },

    .importance_from_permutation = function(model, data, factors) {
      # Permutation importance: shuffle each factor and observe performance degradation
      original_pred <- stats::predict(model, data)
      actual <- data[[names(original_pred)[1]]]
      if (is.null(actual)) actual <- data[[1]]  # fallback

      importance <- numeric()

      for (f in factors) {
        if (!f %in% names(data)) next

        # Shuffle current factor
        data_shuffled <- data
        data_shuffled[[f]] <- sample(data_shuffled[[f]])

        # Predict again
        pred_shuffled <- stats::predict(model, data_shuffled)
        pred_shuffled_val <- pred_shuffled[[1]]

        # Calculate performance degradation
        original_mse <- mean((actual - original_pred[[1]])^2)
        shuffled_mse <- mean((actual - pred_shuffled_val)^2)

        importance[f] <- shuffled_mse - original_mse
      }

      list(
        method = "Permutation importance",
        importance = importance
      )
    },

    .compute_shap_values = function(model, data, factors, target) {
      result <- list()

      tryCatch({
        if (requireNamespace("shapviz", quietly = TRUE)) {
          # Use shapviz package to compute SHAP values
          # Requires model to support predict returning matrix
          result <- private$.shap_via_shapviz(model, data, factors)
        } else if (inherits(model, "lm")) {
          # Simplified SHAP values for linear models
          result <- private$.shap_for_linear(model, data, factors)
        } else {
          message("[iQualityR] SHAP value computation requires 'shapviz' package, skipping")
          result$available <- FALSE
          result$message <- "Requires 'shapviz' package to be installed"
          return(result)
        }

        result$available <- TRUE
      }, error = function(e) {
        message("[iQualityR] SHAP value computation failed: ", e$message)
        result$available <- FALSE
        result$error <- e$message
      })

      result
    },

    .shap_via_shapviz = function(model, data, factors) {
      # Implementation using shapviz package
      # Framework provided here, actual use requires customization based on model type
      list(
        available = FALSE,
        message = "SHAP value computation requires customization based on specific model type"
      )
    },

    .shap_for_linear = function(model, data, factors) {
      # Simplified SHAP values for linear models
      # SHAP_i = sum(beta_j * (x_ij - mean(x_j)))

      coefs <- stats::coef(model)
      intercept <- coefs[1]
      coefs <- coefs[-1]

      n <- nrow(data)
      shap_values <- matrix(0, nrow = n, ncol = length(factors))
      colnames(shap_values) <- factors

      for (i in seq_along(factors)) {
        f <- factors[i]
        if (f %in% names(coefs) && f %in% names(data)) {
          shap_values[, i] <- coefs[f] * (data[[f]] - mean(data[[f]], na.rm = TRUE))
        }
      }

      list(
        available = TRUE,
        method = "Linear model simplified SHAP",
        values = shap_values,
        base_value = as.numeric(intercept)
      )
    },

    .compute_partial_dependence = function(model, data, factors, target) {
      result <- list()

      # Compute partial dependence for each factor
      for (f in factors) {
        if (!f %in% names(data)) next

        tryCatch({
          pdp <- private$.compute_single_pdp(model, data, f)
          result[[f]] <- pdp
        }, error = function(e) {
          message("[iQualityR] Partial dependence computation for factor '", f, "' failed: ", e$message)
        })
      }

      result
    },

    .compute_single_pdp = function(model, data, factor_name) {
      # Simplified PDP computation
      x_values <- sort(unique(data[[factor_name]]))

      # Generate grid data
      grid_data <- data[rep(1, length(x_values)), ]
      grid_data[[factor_name]] <- x_values

      # Predict
      predictions <- stats::predict(model, grid_data)
      pred_values <- if (is.data.frame(predictions)) predictions[[1]] else predictions

      list(
        factor = factor_name,
        x_values = x_values,
        pdp_values = pred_values
      )
    }
  )
)
