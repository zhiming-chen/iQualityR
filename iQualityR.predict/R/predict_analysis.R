# =============================================================================
# File: R/predict/predict_analysis.R
# Description: Entry point functions for quality prediction modeling
# =============================================================================

#' @title Quality Prediction Analysis Entry Point
#' @description
#' Creates a quality prediction task for regression, classification, or time series.
#' This is the main user-facing function for the iQualityR.predict package.
#'
#' @param data A data frame containing the training data.
#' @param target Character string specifying the target variable (response).
#' @param factors Character vector of factor (feature) variable names.
#'   If NULL, all columns except target will be used.
#' @param model_type Model type: "auto", "linear", "ridge", "elasticnet",
#'   "random_forest", "xgboost", "svm", "pls" for regression;
#'   "auto", "logistic", "random_forest", "xgboost", "svm" for classification.
#' @param validation Validation settings list with method, folds, etc.
#' @param diagnostics Diagnostics options list.
#' @param explanation Explanation options list (feature_importance, shap_values, etc.).
#' @param expert_config Expert configuration list for professional mode.
#' @param time_column Time column name (required for time series).
#' @param horizon Forecast horizon (required for time series).
#' @param ... Additional arguments passed to IqrPredictiveTask.
#'
#' @return An IqrPredictiveTask object.
#'
#' @examples
#' \dontrun{
#' # Regression
#' task <- regression_analysis(data, target = "strength",
#'                            factors = c("temp", "pressure"))
#' task$compute()
#' task$summary()
#'
#' # Classification
#' task <- classification_analysis(data, target = "status",
#'                                 factors = c("temp", "pressure"))
#' task$compute()
#'
#' # Time series
#' task <- time_series_forecast(data, target = "defect_rate",
#'                              time_column = "date", horizon = 6)
#' task$compute()
#' }
#'
#' @name regression_analysis
NULL

#' @rdname regression_analysis
#' @export
regression_analysis <- function(data,
                                target,
                                factors = NULL,
                                model_type = "auto",
                                validation = list(),
                                diagnostics = list(),
                                explanation = list(),
                                expert_config = NULL,
                                ...) {
  # Create predictive plan
  plan <- PredictivePlan$new(
    task_tag = "regression",
    target = target,
    factors = factors,
    model_type = model_type,
    validation = validation,
    diagnostics = diagnostics,
    explanation = explanation,
    ...
  )

  # Apply expert config if provided
  if (!is.null(expert_config)) {
    plan$set_expert_config(expert_config)
  }

  # Create and return task
  IqrPredictiveTask$new(
    data = data,
    plan = plan,
    ...
  )
}

#' @rdname regression_analysis
#' @export
classification_analysis <- function(data,
                                   target,
                                   factors = NULL,
                                   model_type = "auto",
                                   validation = list(),
                                   diagnostics = list(),
                                   explanation = list(),
                                   expert_config = NULL,
                                   ...) {
  # Create predictive plan for classification
  plan <- PredictivePlan$new(
    task_tag = "classification",
    target = target,
    factors = factors,
    model_type = model_type,
    validation = validation,
    diagnostics = diagnostics,
    explanation = explanation,
    ...
  )

  # Apply expert config if provided
  if (!is.null(expert_config)) {
    plan$set_expert_config(expert_config)
  }

  # Create and return task
  IqrPredictiveTask$new(
    data = data,
    plan = plan,
    ...
  )
}

#' @rdname regression_analysis
#' @export
time_series_forecast <- function(data,
                                target,
                                factors = NULL,
                                time_column = NULL,
                                horizon = 12,
                                model_type = "auto",
                                validation = list(),
                                diagnostics = list(),
                                explanation = list(),
                                expert_config = NULL,
                                ...) {
  # Validate time column
  if (is.null(time_column)) {
    stop("[time_series_forecast] time_column is required for time series forecasting")
  }

  if (!time_column %in% names(data)) {
    stop("[time_series_forecast] time_column '", time_column, "' not found in data")
  }

  # Create predictive plan for time series
  plan <- PredictivePlan$new(
    task_tag = "time_series",
    target = target,
    factors = factors,
    model_type = model_type,
    validation = validation,
    diagnostics = diagnostics,
    explanation = explanation,
    meta_data = list(
      method = list(
        time_column = time_column,
        forecast_horizon = horizon
      )
    ),
    ...
  )

  # Apply expert config if provided
  if (!is.null(expert_config)) {
    plan$set_expert_config(expert_config)
  }

  # Create and return task
  IqrPredictiveTask$new(
    data = data,
    plan = plan,
    ...
  )
}
