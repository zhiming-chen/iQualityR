# =============================================================================
# File: R/predict/predict_utils.R
# Description: Internal Utility Functions for Quality Prediction Modeling
# =============================================================================

#' @title iQualityR.predict package
#' @description
#' Predictive quality modeling tools for the iQualityR ecosystem, including
#' regression, classification, model training, and diagnostics.
#'
#' @importFrom R6 R6Class
#' @importFrom stats sd predict lm glm formula model.matrix coef residuals
#' @importFrom stats hatvalues cooks.distance dffits fitted
#' @importFrom rlang .data
#' @importFrom utils packageVersion
#' @importFrom iQualityR.core IqrTaskBase IqrPlanBase IqrPlotterBase
#' @keywords internal
"_PACKAGE"

# =============================================================================
# Global Configuration Constants
# =============================================================================

#' @title Global Configuration Constants
#' @description Thresholds and configuration for predictive modeling
#' @keywords internal
.MODEL_CONFIG <- list(
  # R-squared thresholds for model rating
  thresholds = list(
    r2_excellent = 0.8,
    r2_good = 0.7,
    r2_acceptable = 0.5,
    r2_weak = 0.3,
    min_sample_for_rf = 100,
    min_sample_for_ridge = 50,
    max_factor_for_linear = 20,
    max_categorical_for_linear = 5,
    vif_severe = 10,
    vif_moderate = 5,
    cooks_threshold = 1,
    leverage_factor = 2
  ),
  # Default validation settings
  validation = list(
    method = "cross_validation",
    folds = 5,
    repeats = 1
  )
)

# =============================================================================
# Model Rating Function (Unified Implementation)
# =============================================================================

#' @title Quality-oriented Model Rating
#' @description
#' Converts statistical metrics to quality-engineer-readable rating and interpretation
#' @param r_squared R-squared value
#' @param rmse RMSE value (optional)
#' @return List containing rating, stars, and interpretation
#' @keywords internal
.rate_model <- function(r_squared, rmse = NULL) {
  if (is.null(r_squared) || is.na(r_squared)) {
    return(list(
      stars = 0,
      level = "Unable to Evaluate",
      interpretation = "Model metrics not computed or invalid"
    ))
  }

  thresh <- .MODEL_CONFIG$thresholds

  if (r_squared >= thresh$r2_excellent) {
    list(
      stars = 5,
      level = "Excellent",
      interpretation = "Model effectively explains quality variation, can be used for production forecasting"
    )
  } else if (r_squared >= thresh$r2_good) {
    list(
      stars = 4,
      level = "Good",
      interpretation = "Model has good prediction capability, recommend continuous monitoring"
    )
  } else if (r_squared >= thresh$r2_acceptable) {
    list(
      stars = 3,
      level = "Acceptable",
      interpretation = "Model has some explanatory power, recommend checking if important factors are included"
    )
  } else if (r_squared >= thresh$r2_weak) {
    list(
      stars = 2,
      level = "Weak",
      interpretation = "Model explanatory power is insufficient, recommend reselecting factors or collecting more data"
    )
  } else {
    list(
      stars = 1,
      level = "Not Recommended",
      interpretation = "Current factor combination cannot effectively explain quality variation, need to redesign experiment"
    )
  }
}

# =============================================================================
# Algorithm Auto-Selection Function (Unified Implementation)
# =============================================================================

#' @title Intelligent Algorithm Selection
#' @description
#' Automatically selects the most appropriate algorithm based on data characteristics
#' @param data Training data
#' @param target Target variable name
#' @param factors Factor variable names
#' @param task_type Task type ("regression", "classification", "time_series")
#' @return Recommended algorithm name
#' @keywords internal
.select_algorithm <- function(data, target, factors, task_type = "regression") {
  n_obs <- nrow(data)
  n_factors <- length(factors)
  thresh <- .MODEL_CONFIG$thresholds

  # Check factor types
  numeric_factors <- sum(sapply(data[factors], is.numeric))
  categorical_factors <- n_factors - numeric_factors

  if (task_type == "regression") {
    # Regression task selection strategy
    if (n_obs < thresh$min_sample_for_ridge) {
      message("[iQualityR] Small data volume (n < ", thresh$min_sample_for_ridge, "), selecting linear model to prevent overfitting")
      return("linear")
    }

    if (n_factors > thresh$max_factor_for_linear) {
      message("[iQualityR] Large number of factors (p > ", thresh$max_factor_for_linear, "), selecting ridge regression to handle multicollinearity")
      return("ridge")
    }

    if (categorical_factors > thresh$max_categorical_for_linear) {
      message("[iQualityR] Large number of categorical factors, selecting random forest to handle nonlinear relationships")
      return("random_forest")
    }

    # Default: random forest (robust, no fine-tuning needed)
    message("[iQualityR] Using default regression algorithm: random_forest")
    return("random_forest")

  } else if (task_type == "classification") {
    # Classification task selection strategy
    if (n_obs < thresh$min_sample_for_ridge) {
      message("[iQualityR] Small data volume, selecting logistic regression")
      return("logistic")
    }

    message("[iQualityR] Using default classification algorithm: random_forest")
    return("random_forest")

  } else if (task_type == "time_series") {
    # Time series: use linear trend by default
    message("[iQualityR] Time series forecasting using linear trend model")
    return("linear")
  }

  # Default fallback
  "linear"
}

# =============================================================================
# Data Quality Check Utility (Preserved for Use)
# =============================================================================

#' @title Data Quality Check
#' @description
#' Checks if data is suitable for modeling, returns check report
#' @param data Data frame
#' @param target Target variable name
#' @param factors Factor variable names
#' @return Data quality check report list
#' @keywords internal
.check_data_quality <- function(data, target, factors) {
  report <- list()

  # 1. Basic statistics
  report$n_obs <- nrow(data)
  report$n_factors <- length(factors)

  # 2. Missing value check
  missing <- colSums(is.na(data[c(target, factors)]))
  report$missing_values <- missing[missing > 0]

  # 3. Target variable check
  if (is.numeric(data[[target]])) {
    report$target_stats <- list(
      mean = mean(data[[target]], na.rm = TRUE),
      sd = sd(data[[target]], na.rm = TRUE),
      min = min(data[[target]], na.rm = TRUE),
      max = max(data[[target]], na.rm = TRUE)
    )
  }

  # 4. Factor variable type check
  factor_types <- sapply(data[factors], class)
  report$factor_types <- factor_types

  # 5. Sample-to-factor ratio warning
  if (report$n_obs < 5 * report$n_factors) {
    report$warning <- "Sample size to factor count ratio is low, overfitting risk may exist"
  }

  report
}

# =============================================================================
# Progress and Information Message Utility
# =============================================================================

#' @title User-friendly Progress Message
#' @description
#' Outputs formatted progress information
#' @param stage Stage name
#' @param details Details information
#' @keywords internal
.progress_message <- function(stage, details = NULL) {
  msg <- paste0("[iQualityR] ", stage)
  if (!is.null(details)) {
    msg <- paste0(msg, " - ", details)
  }
  message(msg)
}

# =============================================================================
# Helper Functions
# =============================================================================

#' @title NULL-safe Operator
#' @description Returns b if a is NULL, otherwise returns a
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
