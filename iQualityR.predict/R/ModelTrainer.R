# =============================================================================
# File: R/predict/ModelTrainer.R
# Description: Quality Prediction Model Training Engine (aggregating tidymodels/mlr3 algorithms)
# =============================================================================

#' @title ModelTrainer: Quality Prediction Model Training Engine
#' @description
#
#' Algorithm aggregator, supporting training of various machine learning algorithms.
#' Encapsulates tidymodels and mlr3 complexity while providing expert-level access.
#'
#' **Supported Algorithms**:
#' - linear: Linear regression (ordinary least squares)
#' - ridge: Ridge regression (L2 regularization, handles multicollinearity)
#' - elasticnet: Elastic net (L1+L2 regularization)
#' - random_forest: Random forest (nonlinear, robust)
#' - xgboost: Gradient boosting tree (high accuracy, requires tuning)
#' - svm: Support vector machine (complex boundaries)
#' - pls: Partial least squares (high-dimensional collinear data)
#'
#' @field raw_model Trained raw model object
#' @field workflow tidymodels workflow object (accessible to experts)
#' @field tuning_results Tuning results (expert mode)
#' @field model_metrics Model evaluation metrics
#'
#' @export
ModelTrainer <- R6::R6Class("ModelTrainer",
  public = list(
    #' @field raw_model Trained raw model
    raw_model = NULL,

    #' @field workflow tidymodels workflow object
    workflow = NULL,

    #' @field tuning_results Tuning results
    tuning_results = NULL,

    #' @field model_metrics Model evaluation metrics
    model_metrics = NULL,

    #' @field fitted_values Fitted values
    fitted_values = NULL,

    #' @field predicted_probs Predicted probabilities (for classification)
    predicted_probs = NULL,

    #' @field true_labels True labels (for classification ROC curve)
    true_labels = NULL,

    #' @field coefficients Model coefficients (linear models)
    coefficients = NULL,

    #' @description Train model (business group mode - automated)
    #' @param data Training data
    #' @param plan PredictivePlan object
    #' @return Self reference
    train_auto = function(data, plan) {
      message("[iQualityR] === Model Training (Business Group Mode) ===")

      # 0. Select training strategy based on task type
      task_type <- plan$task_tag

      # 1. Intelligent algorithm selection
      model_type <- plan$model_type
      if (model_type == "auto") {
        model_type <- private$.select_algorithm(data, plan)
        message("[iQualityR] Auto-selected algorithm: ", model_type)
      }

      # 2. Prepare training data
      formula_str <- private$.build_formula(plan)
      message("[iQualityR] Modeling formula: ", formula_str)

      # 3. Set validation strategy
      resampling <- private$.setup_resampling(data, plan)
      message("[iQualityR] Validation method: ", plan$validation$method,
              " (folds = ", plan$validation$folds, ")")

      # 4. Train model based on task type
      if (task_type == "regression") {
        self$raw_model <- switch(model_type,
          "linear"       = private$.train_linear(data, formula_str),
          "ridge"        = private$.train_ridge(data, formula_str, plan),
          "elasticnet"   = private$.train_elasticnet(data, formula_str, plan),
          "random_forest" = private$.train_random_forest(data, formula_str, plan),
          "xgboost"      = private$.train_xgboost(data, formula_str, plan),
          "svm"          = private$.train_svm(data, formula_str, plan),
          "pls"          = private$.train_pls(data, formula_str, plan),
          stop("[ModelTrainer] Unsupported regression model type: ", model_type, call. = FALSE)
        )

        # Compute regression metrics
        self$fitted_values <- tryCatch({
          # Special handling for glmnet models
          if (inherits(self$raw_model, "list") && "model" %in% names(self$raw_model) && 
              inherits(self$raw_model$model, "glmnet")) {
            x_mat <- model.matrix(as.formula(self$raw_model$formula), data)[, -1, drop = FALSE]
            pred <- predict(self$raw_model$model, newx = x_mat, s = "lambda.min")
            as.vector(pred)
          } else {
            pred <- stats::predict(self$raw_model, data)
            if (is.data.frame(pred) && ".pred" %in% names(pred)) {
              pred$.pred
            } else if (is.data.frame(pred)) {
              pred[[1]]
            } else {
              pred
            }
          }
        }, error = function(e) {
          message("[ModelTrainer] Failed to extract predicted values: ", e$message)
          rep(NA_real_, nrow(data))
        })

        self$model_metrics <- private$.compute_regression_metrics(
          actual = data[[plan$target_var]],
          predicted = self$fitted_values
        )

        # Extract coefficients (linear models)
        if (model_type %in% c("linear", "ridge", "elasticnet")) {
          self$coefficients <- private$.extract_coefficients(self$raw_model)
        }

      } else if (task_type == "classification") {
        self$raw_model <- switch(model_type,
          "logistic"      = private$.train_logistic(data, formula_str),
          "random_forest" = private$.train_random_forest(data, formula_str, plan),
          "xgboost"      = private$.train_xgboost(data, formula_str, plan),
          "svm"          = private$.train_svm(data, formula_str, plan),
          stop("[ModelTrainer] Unsupported classification model type: ", model_type, call. = FALSE)
        )

        # Compute classification metrics
        pred_probs <- private$.predict_probs(self$raw_model, data)
        pred_class <- ifelse(pred_probs > 0.5,
                             levels(data[[plan$target_var]])[2],
                             levels(data[[plan$target_var]])[1])

        self$fitted_values <- pred_class
        self$predicted_probs <- pred_probs  # Store probabilities for ROC curve
        self$true_labels <- data[[plan$target_var]]  # Store true labels for ROC curve
        self$model_metrics <- private$.compute_classification_metrics(
          actual = data[[plan$target_var]],
          predicted = pred_class,
          predicted_probs = pred_probs
        )

      } else if (task_type == "time_series") {
        # Time series forecasting
        self$raw_model <- private$.train_time_series(data, plan)

        # Compute time series metrics
        self$fitted_values <- self$raw_model$fitted
        self$model_metrics <- private$.compute_regression_metrics(
          actual = data[[plan$target_var]],
          predicted = self$fitted_values
        )
      }

      message("[iQualityR] Model training complete")
      invisible(self)
    },

    #' @description Train model (expert mode - full control)
    #' @param data Training data
    #' @param plan PredictivePlan object
    #' @return Self reference
    train_expert = function(data, plan) {
      message("[iQualityR] === Model Training (Expert Mode) ===")
      message("[iQualityR] Compute backend: ", plan$backend)

      if (plan$backend == "mlr3") {
        private$.train_with_mlr3(data, plan)
      } else {
        private$.train_with_tidymodels_expert(data, plan)
      }

      message("[iQualityR] Expert mode model training complete")
      invisible(self)
    },

    #' @description Predict new data
    #' @param new_data New data frame
    #' @param type Prediction type ("response", "prob", "class")
    #' @return Predicted values
    predict_new = function(new_data, type = "response") {
      if (is.null(self$raw_model)) {
        stop("[ModelTrainer] Model not yet trained, please call train_auto() or train_expert() first",
             call. = FALSE)
      }

      # Select prediction method based on backend
      if (inherits(self$raw_model, "_lvr")) {
        # mlr3 model
        private$.predict_mlr3(self$raw_model, new_data, type)
      } else {
        # tidymodels or base model
        private$.predict_tidymodels(self$raw_model, new_data, type)
      }
    },

    #' @description Get model information summary
    get_model_info = function() {
      if (is.null(self$raw_model)) {
        return(list(status = "Not trained"))
      }

      info <- list(
        status = "Trained",
        class = class(self$raw_model),
        metrics = self$model_metrics,
        n_coefficients = if (!is.null(self$coefficients)) nrow(self$coefficients) else 0
      )
      info
    },

    #' @description Get model rating (public interface)
    #' @param metrics Model metrics list
    #' @return Rating result list
    rate_model = function(metrics) {
      private$.rate_model(metrics)
    },

    #' @description Intelligent algorithm selection (public interface)
    #' @param data Training data
    #' @param plan PredictivePlan object
    #' @return Recommended algorithm name
    select_algorithm = function(data, plan) {
      private$.select_algorithm(data, plan)
    }
  ),

  private = list(
    # ========== Business Group Mode Algorithm Implementation ==========

    .train_linear = function(data, formula_str) {
      stats::lm(as.formula(formula_str), data = data)
    },

    .train_logistic = function(data, formula_str) {
      stats::glm(as.formula(formula_str), data = data, family = binomial)
    },

    .train_ridge = function(data, formula_str, plan) {
      if (!requireNamespace("glmnet", quietly = TRUE)) {
        message("[iQualityR] Using stats::lm as ridge regression substitute (glmnet not installed)")
        return(self$.train_linear(data, formula_str))
      }

      # Use glmnet for ridge regression
      x <- model.matrix(as.formula(formula_str), data)[, -1]
      y <- data[[plan$target_var]]

      fit <- glmnet::glmnet(x, y, alpha = 0, lambda = "lambda.min")
      list(model = fit, formula = formula_str, type = "ridge")
    },

    .train_elasticnet = function(data, formula_str, plan) {
      if (!requireNamespace("glmnet", quietly = TRUE)) {
        message("[iQualityR] Using stats::lm as elastic net substitute (glmnet not installed)")
        return(self$.train_linear(data, formula_str))
      }

      x <- model.matrix(as.formula(formula_str), data)[, -1]
      y <- data[[plan$target_var]]

      fit <- glmnet::glmnet(x, y, alpha = 0.5, lambda = "lambda.min")
      list(model = fit, formula = formula_str, type = "elasticnet")
    },

    .train_random_forest = function(data, formula_str, plan) {
      if (!requireNamespace("randomForest", quietly = TRUE)) {
        message("[iQualityR] Using stats::lm as random forest substitute (randomForest not installed)")
        return(self$.train_linear(data, formula_str))
      }

      randomForest::randomForest(
        as.formula(formula_str),
        data = data,
        importance = TRUE,
        ntree = 500
      )
    },

    .train_xgboost = function(data, formula_str, plan) {
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        message("[iQualityR] Using randomForest as XGBoost substitute (xgboost not installed)")
        return(self$.train_random_forest(data, formula_str, plan))
      }

      # Prepare xgboost data
      x <- model.matrix(as.formula(formula_str), data)[, -1]
      y <- data[[plan$target_var]]

      dtrain <- xgboost::xgb.DMatrix(data = x, label = y)

      params <- list(
        objective = "reg:squarederror",
        max_depth = 6,
        eta = 0.1,
        nrounds = 100
      )

      xgboost::xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)
    },

    .train_svm = function(data, formula_str, plan) {
      if (!requireNamespace("e1071", quietly = TRUE)) {
        message("[iQualityR] Using stats::lm as SVM substitute (e1071 not installed)")
        return(self$.train_linear(data, formula_str))
      }

      e1071::svm(as.formula(formula_str), data = data, kernel = "radial")
    },

    .train_pls = function(data, formula_str, plan) {
      if (!requireNamespace("pls", quietly = TRUE)) {
        message("[iQualityR] Using stats::lm as PLS substitute (pls not installed)")
        return(self$.train_linear(data, formula_str))
      }

      pls::plsr(as.formula(formula_str), data = data, validation = "CV")
    },

    # ========== Expert Mode Algorithm Implementation ==========

    .train_with_tidymodels_expert = function(data, plan) {
      if (!requireNamespace("parsnip", quietly = TRUE) ||
          !requireNamespace("workflows", quietly = TRUE)) {
        stop("[ModelTrainer] Expert mode requires tidymodels packages: ",
             "parsnip, workflows, recipes, tune, rsample, yardstick",
             call. = FALSE)
      }

      cfg <- plan$expert_config

      # 1. Build model specification
      model_spec <- if (!is.null(cfg$model_spec)) {
        cfg$model_spec
      } else {
        parsnip::linear_reg() %>% parsnip::set_engine("lm")
      }

      # 2. Build preprocessing recipe
      recipe_obj <- if (!is.null(cfg$recipe)) {
        cfg$recipe
      } else {
        recipes::recipe(as.formula(private$.build_formula(plan)), data = data)
      }

      # 3. Build workflow
      wf <- workflows::workflow() %>%
        workflows::add_model(model_spec) %>%
        workflows::add_recipe(recipe_obj)

      self$workflow <- wf

      # 4. Train or tune
      if (!is.null(cfg$tune_grid) && !is.null(cfg$resampling)) {
        # Parameter tuning
        message("[iQualityR] Executing parameter tuning...")

        metric_set <- cfg$metric %||% yardstick::metric_set(yardstick::rmse, yardstick::rsq)

        tune_results <- tune::tune_grid(
          wf,
          resamples = cfg$resampling,
          grid = cfg$tune_grid,
          metrics = metric_set,
          control = cfg$control %||% tune::control_grid(verbose = FALSE)
        )

        self$tuning_results <- tune_results

        # Select best parameters
        best_params <- tune::select_best(tune_results, metric = "rmse")

        # Finalize workflow
        wf_final <- workflows::finalize_workflow(wf, best_params)

        # Train final model
        self$raw_model <- parsnip::fit(wf_final, data)
      } else {
        # Direct training
        self$raw_model <- parsnip::fit(wf, data)
      }

      # 5. Compute metrics
      predictions <- predict(self$raw_model, data)$.pred
      self$fitted_values <- predictions
      self$model_metrics <- private$.compute_basic_metrics(
        actual = data[[plan$target_var]],
        predicted = predictions
      )
    },

    .train_with_mlr3 = function(data, plan) {
      if (!requireNamespace("mlr3", quietly = TRUE)) {
        stop("[ModelTrainer] Expert mode (mlr3) requires mlr3 package", call. = FALSE)
      }

      cfg <- plan$expert_config

      # 1. Create mlr3 task
      task <- mlr3::TaskRegr$new("predict", data, plan$target_var)

      # 2. Get learner
      learner <- if (!is.null(cfg$learner)) {
        cfg$learner
      } else {
        mlr3::lrn("regr.lm")
      }

      # 3. Resampling strategy
      resampling <- if (!is.null(cfg$resampling)) {
        cfg$resampling
      } else {
        mlr3::rsmp("cv", folds = plan$validation$folds)
      }

      # 4. Train
      learner$train(task)
      self$raw_model <- learner

      # 5. Compute metrics
      predictions <- learner$predict(task)
      self$fitted_values <- predictions$response
      self$model_metrics <- private$.compute_basic_metrics(
        actual = task$truth(),
        predicted = predictions$response
      )
    },

    .predict_mlr3 = function(model, new_data, type) {
      task <- model$predict(new_data)
      if (type == "response") {
        task$response
      } else {
        task
      }
    },

    .predict_tidymodels = function(model, new_data, type) {
      if (inherits(model, "workflows")) {
        pred <- predict(model, new_data)
      } else if (inherits(model, "lm")) {
        pred <- predict(model, new_data)
      } else if (inherits(model, "randomForest")) {
        pred <- predict(model, new_data)
      } else {
        pred <- stats::predict(model, new_data)
      }

      if (type == "response") {
        if (is.data.frame(pred)) {
          if (".pred" %in% names(pred)) pred$.pred
          else pred[[1]]
        } else {
          pred
        }
      } else {
        pred
      }
    },

    # ========== Helper Functions ==========

    .select_algorithm = function(data, plan) {
      # Use unified utility function
      .select_algorithm(data, plan$target_var, plan$factor_vars, plan$task_tag)
    },

    .build_formula = function(plan) {
      # Use reformulate() instead of paste() to avoid injection risks
      reformulate(plan$factor_vars, response = plan$target_var)
    },

    .setup_resampling = function(data, plan) {
      val <- plan$validation

      if (val$method == "bootstrap") {
        return("bootstrap")
      }

      if (val$method == "validation_split") {
        return("validation_split")
      }

      # Default cross-validation
      "cross_validation"
    },

    .compute_basic_metrics = function(actual, predicted) {
      # Remove missing values
      valid <- !is.na(actual) & !is.na(predicted)
      actual <- actual[valid]
      predicted <- predicted[valid]

      residuals <- actual - predicted

      list(
        r_squared = 1 - sum(residuals^2) / sum((actual - mean(actual))^2),
        adj_r_squared = NA,  # Computed in full analysis
        rmse = sqrt(mean(residuals^2)),
        mae = mean(abs(residuals)),
        mape = mean(abs(residuals / actual)) * 100,
        n_obs = length(actual)
      )
    },

    .compute_regression_metrics = function(actual, predicted) {
      # Remove missing values
      valid <- !is.na(actual) & !is.na(predicted)
      actual <- actual[valid]
      predicted <- predicted[valid]

      residuals <- actual - predicted
      n <- length(actual)
      p <- 1  # Simplified handling

      r_squared <- 1 - sum(residuals^2) / sum((actual - mean(actual))^2)
      adj_r_squared <- 1 - (1 - r_squared) * (n - 1) / (n - p - 1)

      list(
        r_squared = r_squared,
        adj_r_squared = ifelse(is.finite(adj_r_squared), adj_r_squared, NA),
        rmse = sqrt(mean(residuals^2)),
        mae = mean(abs(residuals)),
        mape = ifelse(all(actual != 0), mean(abs(residuals / actual)) * 100, NA),
        n_obs = n
      )
    },

    .compute_classification_metrics = function(actual, predicted, predicted_probs = NULL) {
      # Confusion matrix
      cm <- table(Actual = actual, Predicted = predicted)

      # Binary classification metrics
      if (length(levels(actual)) == 2) {
        positive_class <- levels(actual)[2]
        tp <- sum(actual == positive_class & predicted == positive_class)
        tn <- sum(actual != positive_class & predicted != positive_class)
        fp <- sum(actual != positive_class & predicted == positive_class)
        fn <- sum(actual == positive_class & predicted != positive_class)

        accuracy <- (tp + tn) / (tp + tn + fp + fn)
        precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
        recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
        f1 <- ifelse((precision + recall) > 0,
                     2 * precision * recall / (precision + recall), 0)

        # AUC (if probabilities available)
        auc_val <- NA
        if (!is.null(predicted_probs) && requireNamespace("pROC", quietly = TRUE)) {
          tryCatch({
            roc_obj <- pROC::roc(actual, predicted_probs, quiet = TRUE)
            auc_val <- pROC::auc(roc_obj)
          }, error = function(e) NULL)
        }

        list(
          accuracy = accuracy,
          precision = precision,
          recall = recall,
          f1_score = f1,
          auc = auc_val,
          confusion_matrix = cm,
          n_obs = length(actual)
        )
      } else {
        # Multi-class
        accuracy <- sum(actual == predicted) / length(actual)
        list(
          accuracy = accuracy,
          confusion_matrix = cm,
          n_obs = length(actual)
        )
      }
    },

    .predict_probs = function(model, data) {
      # Predict probabilities based on model type
      if (inherits(model, "glm")) {
        stats::predict(model, data, type = "response")
      } else if (inherits(model, "randomForest")) {
        pred <- stats::predict(model, data, type = "prob")
        if (is.matrix(pred)) pred[, 2]  # Return positive class probability
        else pred
      } else {
        # Default return 0.5 (no probability output)
        rep(0.5, nrow(data))
      }
    },

    .train_time_series = function(data, plan) {
      # Simplified implementation: use linear regression to fit time trend
      target <- plan$target_var

      # If data has time column, use it; otherwise use index
      time_col <- plan$meta_data$method$time_column
      if (!is.null(time_col) && time_col %in% names(data)) {
        time_var <- data[[time_col]]
        if (is.numeric(time_var)) {
          # Use reformulate() instead of paste() to build formula (more safe)
          formula_ts <- reformulate(time_col, response = target)
        } else {
          # Convert time to numeric
          data$time_numeric <- as.numeric(as.factor(time_var))
          formula_ts <- reformulate("time_numeric", response = target)
        }
      } else {
        data$time_index <- seq_len(nrow(data))
        formula_ts <- reformulate("time_index", response = target)
      }

      model <- stats::lm(formula_ts, data = data)

      list(
        model = model,
        fitted = stats::predict(model, data),
        formula = formula_ts
      )
    },

    #' @description Time series forecasting - predict future values
    #' @param horizon Forecast periods
    #' @return Forecast result list
    forecast = function(horizon = 12) {
      if (is.null(self$raw_model)) {
        stop("[ModelTrainer] Model not yet trained, cannot forecast", call. = FALSE)
      }

      if (inherits(self$raw_model, "list") && !is.null(self$raw_model$model)) {
        # Time series model
        model <- self$raw_model$model
        last_time <- nrow(model$model)

        # Generate future time index
        future_data <- data.frame(
          time_index = (last_time + 1):(last_time + horizon)
        )

        pred <- stats::predict(model, future_data)

        # Simplified confidence interval
        se <- summary(model)$sigma
        list(
          forecast = pred,
          ci_lower = pred - 1.96 * se,
          ci_upper = pred + 1.96 * se,
          horizon = horizon
        )
      } else {
        stop("[ModelTrainer] Current model does not support time series forecasting", call. = FALSE)
      }
    },

    #' @description Predict new data (with confidence interval)
    #' @param new_data New data
    #' @param level Confidence level
    #' @return Prediction result
    predict_with_interval = function(new_data, level = 0.95) {
      if (is.null(self$raw_model)) {
        stop("[ModelTrainer] Model not yet trained", call. = FALSE)
      }

      if (inherits(self$raw_model, "lm") || inherits(self$raw_model, "glm")) {
        pred <- stats::predict(self$raw_model, new_data, interval = "prediction",
                               level = level)
        list(
          fit = pred[, "fit"],
          lwr = pred[, "lwr"],
          upr = pred[, "upr"]
        )
      } else {
        # Other models: point prediction only
        point_pred <- self$predict_new(new_data)
        list(
          fit = point_pred,
          lwr = point_pred - sd(point_pred),  # Simplified
          upr = point_pred + sd(point_pred)
        )
      }
    },

    .extract_coefficients = function(model) {
      if (inherits(model, "lm")) {
        coef_summary <- summary(model)$coefficients
        data.frame(
          term = rownames(coef_summary),
          estimate = coef_summary[, "Estimate"],
          std_error = coef_summary[, "Std. Error"],
          statistic = coef_summary[, "t value"],
          p_value = coef_summary[, "Pr(>|t|)"],
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    },

    .rate_model = function(metrics) {
      # Use unified utility function
      if (is.null(metrics) || is.null(metrics$r_squared)) {
        .rate_model(NULL)
      } else {
        .rate_model(metrics$r_squared)
      }
    }
  )
)
