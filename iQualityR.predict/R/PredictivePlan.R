# =============================================================================
# File: R/predict/PredictivePlan.R
# Description: Quality Prediction Modeling Plan Configurator
# =============================================================================

#' @title PredictivePlan: Quality Prediction Modeling Plan Configurator
#' @description
#' Inherits from IqrPlanBase, responsible for storing and managing all configuration
#' parameters for prediction modeling tasks.
#
#' **Dual Mode Support**:
#' - **Business Group Mode (auto_mode = TRUE)**: Intelligent default parameters, one-click modeling
#' - **Expert Mode (with expert_config)**: Full control over tidymodels/mlr3 configuration
#'
#' @field target_var Character, target variable (prediction indicator) name
#' @field factor_vars Character vector, influencing factor (feature) variable names
#' @field model_type Character, model type ("auto", "linear", "ridge", "elasticnet",
#'   "random_forest", "xgboost", "svm", "pls")
#' @field backend Character, compute backend ("tidymodels", "mlr3")
#' @field validation List, validation settings
#' @field diagnostics List, diagnostic options
#' @field explanation List, explainability analysis options
#' @field expert_config List, expert-level configuration (expert mode)
#'
#' @export
PredictivePlan <- R6::R6Class("PredictivePlan",
  # Remove inherit = IqrPlanBase to make it an independent class

  public = list(
    #' @field target_var Character, target variable name
    target_var = NULL,

    #' @field factor_vars Character vector, influencing factor variable names
    factor_vars = NULL,

    #' @field model_type Character, model type
    model_type = "auto",

    #' @field backend Character, compute backend
    backend = "tidymodels",

    #' @field validation List, validation settings
    validation = list(),

    #' @field diagnostics List, diagnostic options
    diagnostics = list(),

    #' @field explanation List, explainability analysis options
    explanation = list(),

    #' @field expert_config List, expert-level configuration
    expert_config = list(),

    #' @field task_tag Task tag
    task_tag = NULL,

    #' @field conf_level Confidence level
    conf_level = 0.95,

    #' @field meta_data Metadata list
    meta_data = NULL,

    #' @description Initialize prediction modeling plan
    #' @param task_tag Task tag ("regression", "classification", "time_series")
    #' @param target Target variable name
    #' @param factors Influencing factor variable names (optional, defaults to all columns except target)
    #' @param model_type Model type ("auto"=auto select)
    #' @param backend Compute backend ("tidymodels" or "mlr3")
    #' @param validation Validation settings list
    #' @param diagnostics Diagnostic options list
    #' @param explanation Explainability analysis options list
    #' @param conf_level Confidence level
    #' @param meta_data Metadata list
    #' @param ... Other parameters (passed to expert config)
    initialize = function(task_tag,
                          target,
                          factors = NULL,
                          model_type = "auto",
                          backend = "tidymodels",
                          validation = list(),
                          diagnostics = list(),
                          explanation = list(),
                          conf_level = 0.95,
                          meta_data = NULL,
                          ...) {
      # Initialize base class fields (replacing super$initialize)
      self$task_tag <- task_tag
      self$conf_level <- private$.validate_conf_level(conf_level)

      # Initialize meta_data (4M1E structure)
      if (is.null(meta_data)) {
        self$meta_data <- list(
          man = list(),
          machine = list(),
          material = list(),
          method = list(),
          environment = list(),
          project = list()
        )
      } else {
        self$meta_data <- meta_data
      }

      # Basic configuration
      self$target_var <- target
      self$factor_vars <- factors  # Allow NULL, auto-infer in validate

      # Model and backend
      self$model_type <- private$.validate_model_type(model_type)
      self$backend <- private$.validate_backend(backend)

      # Intelligent fill default validation settings
      self$validation <- private$.fill_validation_defaults(validation)

      # Intelligent fill default diagnostic options
      self$diagnostics <- private$.fill_diagnostics_defaults(diagnostics)

      # Intelligent fill default explainability options
      self$explanation <- private$.fill_explanation_defaults(explanation)

      # Detect if expert config is provided (auto-detect mode)
      dots <- list(...)
      if (!is.null(dots$expert_config)) {
        self$expert_config <- dots$expert_config
        message("[iQualityR] Expert config detected, switching to Expert Mode")
      }
    },

    #' @description Set expert-level configuration (Expert Mode)
    #' @param config Expert config list (can contain any tidymodels/mlr3 configuration)
    #' @examples
    #' \dontrun{
    #' plan$set_expert_config(list(
    #'   tune_grid = expand.grid(trees = c(100, 500), min_n = c(2, 10)),
    #'   metric = yardstick::metric_set(yardstick::rmse, yardstick::rsq),
    #'   resampling = rsample::vfold_cv(data, v = 5)
    #' ))
    #' }
    set_expert_config = function(config) {
      self$expert_config <- modifyList(self$expert_config, config)
      invisible(self)
    },

    #' @description Validate data and configuration compatibility
    #' @param data Data frame
    validate = function(data) {
      # Base class validation (replacing super$validate)
      private$.validate_conf_level(self$conf_level)

      # Check target variable
      if (!self$target_var %in% names(data)) {
        stop("[PredictivePlan] Target variable '", self$target_var, "' not in data", call. = FALSE)
      }

      # Auto-infer factor variables
      if (is.null(self$factor_vars)) {
        self$factor_vars <- setdiff(names(data), self$target_var)
        message("[iQualityR] Factor variables not specified, automatically using all columns except '",
                self$target_var, "' (", length(self$factor_vars), " factors)")
      }

      # Check factor variables
      missing_factors <- setdiff(self$factor_vars, names(data))
      if (length(missing_factors) > 0) {
        stop("[PredictivePlan] The following factor variables are not in data: ",
             paste(missing_factors, collapse = ", "), call. = FALSE)
      }

      # Check data volume
      n_obs <- nrow(data)
      if (n_obs < 10) {
        stop("[PredictivePlan] Insufficient data (n = ", n_obs, "), cannot perform reliable modeling",
             call. = FALSE)
      }

      # Check sample-to-factor ratio
      n_factors <- length(self$factor_vars)
      if (n_obs < 5 * n_factors) {
        warning("[PredictivePlan] Sample size (n=", n_obs, ") to factor count (p=", n_factors,
                ") ratio is low, overfitting risk may exist", call. = FALSE)
      }

      # Task-specific validation
      if (self$task_tag == "classification") {
        private$.validate_classification(data)
      } else if (self$task_tag == "time_series") {
        private$.validate_time_series(data)
      }

      # Validate expert config (if exists)
      if (length(self$expert_config) > 0) {
        private$.validate_expert_config()
      }

      invisible(self)
    },

    #' @description Check if in Expert Mode
    is_expert_mode = function() {
      length(self$expert_config) > 0
    },

    #' @description Get all parameters (unified for Analyzer use)
    #' @return List containing all configurations
    get_all_params = function() {
      list(
        task_tag = self$task_tag,
        target_var = self$target_var,
        factor_vars = self$factor_vars,
        model_type = self$model_type,
        backend = self$backend,
        validation = self$validation,
        diagnostics = self$diagnostics,
        explanation = self$explanation,
        expert_config = self$expert_config,
        is_expert = self$is_expert_mode(),
        conf_level = self$conf_level
      )
    }
  ),

  private = list(
    .validate_conf_level = function(cl) {
      if (!is.numeric(cl) || cl <= 0 || cl >= 1) {
        stop("[Planner] conf_level must be between 0 and 1.", call. = FALSE)
      }
      return(cl)
    },

    .validate_model_type = function(mt) {
      valid_types <- c("auto", "linear", "ridge", "elasticnet",
                       "logistic",
                       "random_forest", "xgboost", "svm", "pls")
      if (!mt %in% valid_types) {
        stop("[PredictivePlan] Unsupported model type: ", mt,
             "\n  Options: ", paste(valid_types, collapse = ", "),
             call. = FALSE)
      }
      mt
    },

    .validate_backend = function(be) {
      valid_backends <- c("tidymodels", "mlr3")
      if (!be %in% valid_backends) {
        stop("[PredictivePlan] Unsupported compute backend: ", be,
             "\n  Options: ", paste(valid_backends, collapse = ", "),
             call. = FALSE)
      }
      be
    },

    .fill_validation_defaults = function(validation) {
      defaults <- list(
        method = "auto",           # "auto", "cross_validation", "validation_split", "bootstrap"
        folds = 5,                 # Cross-validation folds
        repeats = 1,               # Number of repeats
        strata = NULL              # Stratification variable (classification task)
      )
      modifyList(defaults, validation)
    },

    .fill_diagnostics_defaults = function(diagnostics) {
      defaults <- list(
        residuals = TRUE,          # Residual analysis
        qq_plot = TRUE,            # Normal Q-Q plot
        influence = TRUE,          # Influence point diagnostics
        multicollinearity = TRUE,  # Multicollinearity test (VIF)
        normality_test = TRUE,     # Normality test (Shapiro-Wilk)
        homoscedasticity = TRUE    # Homoscedasticity test
      )
      modifyList(defaults, diagnostics)
    },

    .fill_explanation_defaults = function(explanation) {
      defaults <- list(
        feature_importance = TRUE, # Factor influence ranking
        shap_values = FALSE,       # SHAP value decomposition (high computational cost, off by default)
        partial_dependence = FALSE # Partial dependence plot
      )
      modifyList(defaults, explanation)
    },

    .validate_expert_config = function() {
      # Check basic structure of expert config
      cfg <- self$expert_config

      # If using mlr3 backend, requires mlr3 related config
      if (self$backend == "mlr3") {
        required_mlr3 <- c("learner", "resampling")
        missing <- setdiff(required_mlr3, names(cfg))
        if (length(missing) > 0) {
          stop("[PredictivePlan] Expert Mode (mlr3) missing required config: ",
               paste(missing, collapse = ", "),
               "\n  Please provide: ", paste(required_mlr3, collapse = ", "),
               call. = FALSE)
        }
      }
      # tidymodels backend is very flexible, no mandatory checks (expert responsible)
    },

    .validate_classification = function(data) {
      # Check if target variable is factor or character
      target_col <- data[[self$target_var]]
      if (!is.factor(target_col) && !is.character(target_col)) {
        warning("[PredictivePlan] Classification task target variable should be factor or character type, current is ",
                class(target_col), ". Auto-converting to factor",
                call. = FALSE)
        target_col <- as.factor(target_col)
        data[[self$target_var]] <- target_col
      } else if (is.character(target_col)) {
        # Convert character to factor for nlevels check
        target_col <- as.factor(target_col)
        data[[self$target_var]] <- target_col
      }

      # Check number of classes (after ensuring factor type)
      n_classes <- nlevels(target_col)
      if (n_classes < 2) {
        stop("[PredictivePlan] Classification task target variable requires at least 2 classes, current is ",
             n_classes, " class(es)", call. = FALSE)
      }
      if (n_classes > 2) {
        message("[iQualityR] Detected multi-class classification task (", n_classes, " classes)")
      }

      # Check class balance
      class_counts <- table(target_col)
      min_class_pct <- min(class_counts) / sum(class_counts)
      if (min_class_pct < 0.1) {
        warning("[PredictivePlan] Class distribution is severely imbalanced (minimum class proportion ",
                round(min_class_pct * 100, 1), "%), may affect model performance",
                call. = FALSE)
      }
    },

    .validate_time_series = function(data) {
      # Check if time column is specified
      time_col <- self$meta_data$method$time_column
      if (is.null(time_col) || !(time_col %in% names(data))) {
        stop("[PredictivePlan] Time series forecasting task must specify time column", call. = FALSE)
      }

      # Check if time column is ordered
      time_var <- data[[time_col]]
      if (is.numeric(time_var)) {
        if (!all(diff(time_var) > 0)) {
          warning("[PredictivePlan] Time column appears not strictly sorted, recommend sorting data by time first",
                  call. = FALSE)
        }
      } else if (inherits(time_var, c("Date", "POSIXt"))) {
        if (!all(diff(as.numeric(time_var)) > 0)) {
          warning("[PredictivePlan] Time column appears not strictly sorted, recommend sorting data by time first",
                  call. = FALSE)
        }
      }

      # Check forecast horizon
      horizon <- self$meta_data$method$forecast_horizon
      if (!is.null(horizon) && horizon <= 0) {
        stop("[PredictivePlan] Forecast horizon must be a positive integer, current is: ", horizon,
             call. = FALSE)
      }

      # Check data volume
      n_obs <- nrow(data)
      if (n_obs < 30) {
        warning("[PredictivePlan] Time series data volume is small (n = ", n_obs,
                "), time series forecasting may be unreliable", call. = FALSE)
      }
    }
  )
)
