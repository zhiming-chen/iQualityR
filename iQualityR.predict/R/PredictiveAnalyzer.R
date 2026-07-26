# =============================================================================
# File: R/predict/PredictiveAnalyzer.R
# Description: Quality Prediction Modeling Analysis Executor
# =============================================================================

#' @title PredictiveAnalyzer: Quality Prediction Modeling Analysis Executor
#' @description
#' Core executor for prediction modeling tasks.
#
#' Coordinates the complete workflow of model training, diagnostic analysis,
#' and explainability analysis. Adopts "Business Group / Expert" dual mode:
#' - Business Group: Intelligent defaults, one-click results
#' - Expert: Full control, supports expert customization
#'
#' @field model_trainer ModelTrainer instance
#' @field diagnostic_analyzer DiagnosticAnalyzer instance
#' @field explainability_engine ExplainabilityEngine instance
#' @field results Standardized results container
#' @field params Parameter list
#'
#' @export
PredictiveAnalyzer <- R6::R6Class("PredictiveAnalyzer",
  # Remove inherit = IqrAnalyzerBase to make it an independent class

  public = list(
    #' @field model_trainer Model trainer
    model_trainer = NULL,

    #' @field diagnostic_analyzer Diagnostic analyzer
    diagnostic_analyzer = NULL,

    #' @field explainability_engine Explainability engine
    explainability_engine = NULL,

    #' @field results Standardized results container
    results = NULL,

    #' @field params Parameter list
    params = list(),

    #' @description Initialize analyzer
    initialize = function() {
      # Directly initialize results container (replacing super$initialize)
      self$reset()
      self$model_trainer <- ModelTrainer$new()
      self$diagnostic_analyzer <- DiagnosticAnalyzer$new()
      self$explainability_engine <- ExplainabilityEngine$new()
    },

    #' @description Reset results container
    reset = function() {
      self$results <- list(
        statistics  = list(),
        diagnostics = list(),
        data_tables = list(),
        raw_output  = NULL
      )
      invisible(self)
    },

    #' @description Store single statistic
    #' @param key Statistic name
    #' @param value Statistic value
    set_statistic = function(key, value) {
      self$results$statistics[[key]] <- value
      invisible(self)
    },

    #' @description Store single diagnostic entry
    #' @param key Diagnostic name
    #' @param value Diagnostic value
    set_diagnostic = function(key, value) {
      self$results$diagnostics[[key]] <- value
      invisible(self)
    },

    #' @description Store single data table
    #' @param key Table name
    #' @param value Data frame
    set_datatable = function(key, value) {
      self$results$data_tables[[key]] <- value
      invisible(self)
    },

    #' @description Store raw output
    #' @param value Raw object
    set_raw_output = function(value) {
      self$results$raw_output <- value
      invisible(self)
    },

    #' @description Get standardized results
    get_results = function() {
      return(self$results)
    },

    #' @description Execute prediction modeling analysis
    #' @param data Training data
    #' @param plan PredictivePlan object
    #' @return Self reference
    run = function(data, plan) {
      # Data standardization
      if (missing(data) || is.null(data)) stop("Data required.", call. = FALSE)
      if (is.data.frame(data)) {
        dt <- data
      } else {
        dt <- as.data.frame(data)
      }

      # Reset results container
      self$reset()

      message("[iQualityR] ========================================")
      message("[iQualityR] Starting quality prediction modeling analysis")
      message("[iQualityR] Task type: ", plan$task_tag)
      message("[iQualityR] Target variable: ", plan$target_var)
      message("[iQualityR] Number of factors: ", length(plan$factor_vars))
      message("[iQualityR] Running mode: ", if (plan$is_expert_mode()) "Expert" else "Business")
      message("[iQualityR] ========================================")

      # 1. Validate configuration
      plan$validate(dt)

      # 2. Data preprocessing
      dt <- private$.preprocess_data(dt, plan)

      # 3. Model training
      if (plan$is_expert_mode()) {
        self$model_trainer$train_expert(dt, plan)
      } else {
        self$model_trainer$train_auto(dt, plan)
      }

      # 4. Model diagnostics (only regression tasks execute residual analysis)
      if (plan$task_tag != "classification") {
        self$diagnostic_analyzer$analyze(
          model_result = self$model_trainer,
          data = dt,
          plan = plan
        )
      } else {
        message("[iQualityR] Classification task, skipping residual analysis")
      }

      # 5. Explainability analysis
      self$explainability_engine$explain(
        model_result = self$model_trainer,
        data = dt,
        plan = plan
      )

      # 6. Assemble results
      private$.assemble_results(plan)

      message("[iQualityR] ========================================")
      message("[iQualityR] Prediction modeling analysis complete")
      message("[iQualityR] ========================================")

      invisible(self)
    },

    #' @description Predict new data
    #' @param new_data New data
    #' @param ... Other parameters
    predict_new = function(new_data, ...) {
      if (is.null(self$model_trainer$raw_model)) {
        stop("[PredictiveAnalyzer] Model not yet trained, cannot predict", call. = FALSE)
      }

      self$model_trainer$predict_new(new_data, ...)
    }
  ),

  private = list(
    .run_logic = function(dt) {
      # This method is manually handled in run(), base class call will not enter here
      invisible(self)
    },

    .preprocess_data = function(data, plan) {
      # Data quality check
      n_obs <- nrow(data)
      n_factors <- length(plan$factor_vars)

      message("[iQualityR] Data preprocessing: ", n_obs, " observations x ", n_factors, " factors")

      # Check missing values
      missing_counts <- colSums(is.na(data))
      if (any(missing_counts > 0)) {
        message("[iQualityR] Found missing values:")
        for (col in names(missing_counts[missing_counts > 0])) {
          message("  - ", col, ": ", missing_counts[col], " missing values")
        }

        # Simple handling: delete rows with missing values (expert mode can customize via expert_config)
        if (!plan$is_expert_mode()) {
          n_removed <- sum(!complete.cases(data))
          data <- data[complete.cases(data), ]
          message("[iQualityR] Removed ", n_removed, " rows with missing values")
        }
      }

      # Check factor variable types
      for (f in plan$factor_vars) {
        if (!is.numeric(data[[f]]) && !is.factor(data[[f]])) {
          message("[iQualityR] Factor '", f, "' is non-numeric, attempting conversion")
          data[[f]] <- as.numeric(as.factor(data[[f]]))
        }
      }

      data
    },

    .assemble_results = function(plan) {
      # Assemble standardized results container
      self$results <- list(
        # Model results
        model = self$model_trainer$get_model_info(),

        # Fitted values and coefficients
        fitted_values = self$model_trainer$fitted_values,
        predicted_probs = self$model_trainer$predicted_probs,  # For classification ROC curve
        true_labels = self$model_trainer$true_labels,  # For classification ROC curve
        coefficients = self$model_trainer$coefficients,

        # Model metrics
        metrics = self$model_trainer$model_metrics,

        # Diagnostic results
        diagnostics = self$diagnostic_analyzer$get_summary(),

        # Explanation results
        explanation = self$explainability_engine$get_summary(),

        # Model rating (quality domain language)
        model_rating = private$.rate_model(self$model_trainer$model_metrics),

        # Metadata
        metadata = list(
          task_tag = plan$task_tag,
          target_var = plan$target_var,
          factor_vars = plan$factor_vars,
          model_type = plan$model_type,
          is_expert_mode = plan$is_expert_mode(),
          timestamp = Sys.time(),
          n_observations = if (!is.null(self$model_trainer$model_metrics))
            self$model_trainer$model_metrics$n_obs else NA
        ),

        # Raw object references (accessible to experts)
        raw_model = self$model_trainer$raw_model,
        workflow = self$model_trainer$workflow,
        tuning_results = self$model_trainer$tuning_results
      )

      invisible(self)
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
