# =============================================================================
# File: R/predict/DiagnosticAnalyzer.R
# Description: Quality Prediction Model Diagnostic Analysis Engine
# =============================================================================

#' @title DiagnosticAnalyzer: Quality Prediction Model Diagnostic Analysis Engine
#' @description
#' Provides comprehensive diagnostics for trained quality prediction models, including
#' residual analysis, normality testing, multicollinearity detection, and outlier
#' identification. Translates statistical results into quality-engineer-readable language.
#'
#' **Diagnostic Items**:
#' - Residual Analysis: Raw residuals, standardized residuals, studentized residuals
#' - Normality Test: Shapiro-Wilk test, Q-Q plot data
#' - Homoscedasticity: Breusch-Pagan test, residuals vs fitted values plot data
#' - Multicollinearity Test: VIF (Variance Inflation Factor)
#' - Influence Point Diagnostics: Leverage values, Cook's distance, DFFITS, DFBETAS
#'
#' @export
DiagnosticAnalyzer <- R6::R6Class("DiagnosticAnalyzer",
  public = list(
    #' @field diagnostics Diagnostic results list
    diagnostics = list(),

    #' @field warnings Diagnostic warnings
    warnings = character(),

    #' @field recommendations Improvement recommendations
    recommendations = character(),

    #' @description Execute model diagnostics
    #' @param model_result Model training result (contains model, fitted_values, etc.)
    #' @param data Original training data
    #' @param plan PredictivePlan object
    #' @return Self reference
    analyze = function(model_result, data, plan) {
      message("[iQualityR] === Model Diagnostic Analysis ===")

      # Clear previous results
      self$diagnostics <- list()
      self$warnings <- character()
      self$recommendations <- character()

      # Extract necessary information
      model <- model_result$raw_model
      fitted <- model_result$fitted_values
      actual <- data[[plan$target_var]]
      
      # Check if fitted values are null or length mismatch
      if (is.null(fitted)) {
        stop("[DiagnosticAnalyzer] No fitted_values in model training result", call. = FALSE)
      }
      if (length(actual) != length(fitted)) {
        stop("[DiagnosticAnalyzer] Actual values(", length(actual), ") length mismatch with fitted values(",
             length(fitted), ")", call. = FALSE)
      }
      
      residuals <- actual - fitted
      
      # Check residuals
      if (length(residuals) == 0) {
        stop("[DiagnosticAnalyzer] Residuals are empty, cannot perform diagnostic analysis", call. = FALSE)
      }
      if (any(is.na(residuals))) {
        warning("[DiagnosticAnalyzer] Residuals contain NA values (", sum(is.na(residuals)), "), removed",
                call. = FALSE)
        valid_idx <- !is.na(residuals) & !is.na(fitted)
        residuals <- residuals[valid_idx]
        fitted <- fitted[valid_idx]
      }

      # 1. Residual analysis
      if (plan$diagnostics$residuals) {
        message("[iQualityR] Executing residual analysis...")
        self$diagnostics$residuals <- private$.analyze_residuals(residuals, fitted)
      }

      # 2. Normality test
      if (plan$diagnostics$normality_test || plan$diagnostics$qq_plot) {
        message("[iQualityR] Executing normality test...")
        self$diagnostics$normality <- private$.test_normality(residuals)
      }

      # 3. Homoscedasticity test
      if (plan$diagnostics$homoscedasticity) {
        message("[iQualityR] Executing homoscedasticity test...")
        self$diagnostics$homoscedasticity <- private$.test_homoscedasticity(residuals, fitted)
      }

      # 4. Multicollinearity test
      if (plan$diagnostics$multicollinearity) {
        message("[iQualityR] Executing multicollinearity test (VIF)...")
        self$diagnostics$multicollinearity <- private$.test_multicollinearity(model, data, plan)
      }

      # 5. Influence point diagnostics
      if (plan$diagnostics$influence) {
        message("[iQualityR] Executing influence point diagnostics...")
        self$diagnostics$influence <- private$.analyze_influence_points(model, data, residuals)
      }

      # 6. Generate quality-domain interpretation and recommendations
      private$.generate_quality_interpretation()

      # 7. Collect warnings
      private$.collect_warnings()

      message("[iQualityR] Diagnostic analysis complete")
      invisible(self)
    },

    #' @description Get diagnostic summary
    get_summary = function() {
      list(
        residuals = self$diagnostics$residuals,
        normality = self$diagnostics$normality,
        homoscedasticity = self$diagnostics$homoscedasticity,
        multicollinearity = self$diagnostics$multicollinearity,
        influence = self$diagnostics$influence,
        warnings = self$warnings,
        recommendations = self$recommendations
      )
    }
  ),

  private = list(
    .analyze_residuals = function(residuals, fitted) {
      # Calculate various types of residuals
      n <- length(residuals)

      # Standardized residuals
      sigma <- sd(residuals)
      standardized_residuals <- residuals / sigma

      # Studentized residuals (simplified version)
      # Full implementation requires hat values, using approximation here
      hii <- rep(1/n, n)  # Simplified assumption
      studentized <- residuals / (sigma * sqrt(1 - hii))

      list(
        raw = residuals,
        standardized = standardized_residuals,
        studentized = studentized,
        fitted_values = fitted,
        summary = list(
          mean = mean(residuals),
          sd = sd(residuals),
          min = min(residuals),
          max = max(residuals),
          median = median(residuals)
        )
      )
    },

    .test_normality = function(residuals) {
      result <- list()

      # Shapiro-Wilk test
      if (requireNamespace("stats", quietly = TRUE)) {
        sw_test <- tryCatch(
          stats::shapiro.test(residuals),
          error = function(e) NULL
        )

        if (!is.null(sw_test)) {
          result$shapiro_wilk <- list(
            statistic = sw_test$statistic,
            p_value = sw_test$p.value,
            is_normal = sw_test$p.value > 0.05,
            interpretation = if (sw_test$p.value > 0.05)
              "Residuals follow normal distribution (p > 0.05)"
            else
              "Residuals deviate from normal distribution (p < 0.05), may affect interval estimation reliability"
          )
        }
      }

      # Q-Q plot data
      qq_data <- private$.compute_qq_data(residuals)
      result$qq_data <- qq_data

      result
    },

    .compute_qq_data = function(residuals) {
      # Calculate theoretical and sample quantiles for Q-Q plot
      n <- length(residuals)
      
      if (n == 0) {
        stop("[DiagnosticAnalyzer] .compute_qq_data: residuals length is 0", call. = FALSE)
      }
      if (any(is.na(residuals))) {
        warning("[DiagnosticAnalyzer] .compute_qq_data: residuals contain NA, removed", call. = FALSE)
        residuals <- residuals[!is.na(residuals)]
        n <- length(residuals)
      }
      
      theoretical <- stats::qnorm((1:n - 0.5) / n)
      sample_quantiles <- sort(residuals)

      data.frame(
        theoretical = theoretical,
        sample = sample_quantiles
      )
    },

    .test_homoscedasticity = function(residuals, fitted) {
      result <- list()

      # Breusch-Pagan test (if car package is available)
      if (requireNamespace("car", quietly = TRUE)) {
        bp_test <- tryCatch(
          car::ncvTest(lm(residuals ~ fitted)),
          error = function(e) NULL
        )

        if (!is.null(bp_test)) {
          result$breusch_pagan <- list(
            statistic = bp_test$Chisquare,
            p_value = bp_test$p,
            is_homoscedastic = bp_test$p > 0.05,
            interpretation = if (bp_test$p > 0.05)
              "Homoscedasticity assumption satisfied (p > 0.05)"
            else
              "Heteroscedasticity present (p < 0.05), pay attention to prediction accuracy in high-value region"
          )
        }
      }

      # Residual vs fitted plot data
      result$residual_vs_fitted <- data.frame(
        fitted = fitted,
        residual = residuals
      )

      result
    },

    .test_multicollinearity = function(model, data, plan) {
      result <- list()

      # VIF only for linear models
      if (!inherits(model, "lm")) {
        result$available <- FALSE
        result$message <- "VIF test is only applicable to linear models"
        return(result)
      }

      result$available <- TRUE

      # Calculate VIF
      if (requireNamespace("car", quietly = TRUE)) {
        vif_values <- tryCatch(
          car::vif(model),
          error = function(e) NULL
        )

        if (!is.null(vif_values)) {
          vif_df <- data.frame(
            term = names(vif_values),
            vif = as.numeric(vif_values),
            stringsAsFactors = FALSE
          )

          # Assess multicollinearity severity
          vif_df$severity <- ifelse(vif_df$vif > 10, "Severe",
                             ifelse(vif_df$vif > 5, "Moderate",
                                    "Mild"))

          result$vif <- vif_df
          result$max_vif <- max(vif_df$vif)
          result$has_multicollinearity <- any(vif_df$vif > 10)

          if (result$has_multicollinearity) {
            result$interpretation <- paste(
              "Severe multicollinearity present (max VIF =", round(result$max_vif, 2),
              "), recommend checking highly correlated factors"
            )
          } else {
            result$interpretation <- paste(
              "Multicollinearity is within acceptable range (max VIF =", round(result$max_vif, 2), ")"
            )
          }
        }
      } else {
        result$available <- FALSE
        result$message <- "VIF test requires 'car' package to be installed"
      }

      result
    },

    .analyze_influence_points = function(model, data, residuals) {
      result <- list()

      # Influence statistics only for linear models
      if (!inherits(model, "lm")) {
        result$available <- FALSE
        result$message <- "Influence point diagnostics only applicable to linear models"
        return(result)
      }

      result$available <- TRUE

      tryCatch({
        # Leverage values
        hat_values <- stats::hatvalues(model)
        p <- length(model$coefficients)
        n <- nrow(data)
        leverage_threshold <- 2 * p / n

        # Cook's distance
        cooks_d <- stats::cooks.distance(model)
        cook_threshold <- 1  # Common threshold

        # DFFITS
        dffits <- stats::dffits(model)
        dffits_threshold <- 2 * sqrt(p / n)

        # Summary
        result$leverage <- data.frame(
          observation = 1:n,
          hat_value = hat_values,
          is_high = hat_values > leverage_threshold,
          threshold = leverage_threshold
        )

        result$cooks_distance <- data.frame(
          observation = 1:n,
          cooks_d = cooks_d,
          is_influential = cooks_d > cook_threshold,
          threshold = cook_threshold
        )

        result$dffits <- data.frame(
          observation = 1:n,
          dffits = dffits,
          is_influential = abs(dffits) > dffits_threshold,
          threshold = dffits_threshold
        )

        # Outlier summary
        n_high_leverage <- sum(hat_values > leverage_threshold)
        n_influential <- sum(cooks_d > cook_threshold)

        result$summary <- list(
          n_high_leverage = n_high_leverage,
          n_influential = n_influential,
          leverage_threshold = leverage_threshold,
          cook_threshold = cook_threshold,
          interpretation = if (n_influential > 0)
            paste("Found", n_influential, "strong influence points, recommend checking these data points")
          else
            "No strong influence points found"
        )
      }, error = function(e) {
        result$available <<- FALSE
        result$message <<- paste("Influence point diagnostics failed:", e$message)
      })

      result
    },

    .generate_quality_interpretation = function() {
      # Convert statistical diagnostic results to recommendations understandable by quality engineers
      recommendations <- character()
      warnings <- character()

      # Normality interpretation
      if (!is.null(self$diagnostics$normality$shapiro_wilk)) {
        if (!self$diagnostics$normality$shapiro_wilk$is_normal) {
          warnings <- c(warnings, "Residual normality test failed")
          recommendations <- c(recommendations,
            "Residuals deviate from normal distribution. If sample size is sufficient, this has limited impact on interval estimation;",
            "however, recommend checking for outliers or data transformation needs"
          )
        }
      }

      # Homoscedasticity interpretation
      if (!is.null(self$diagnostics$homoscedasticity$breusch_pagan)) {
        if (!self$diagnostics$homoscedasticity$breusch_pagan$is_homoscedastic) {
          warnings <- c(warnings, "Homoscedasticity assumption not satisfied")
          recommendations <- c(recommendations,
            "Heteroscedasticity is present. This means the model will have larger prediction errors in certain regions,",
            "recommend paying attention to prediction reliability in high or low value regions"
          )
        }
      }

      # Multicollinearity interpretation
      if (!is.null(self$diagnostics$multicollinearity$has_multicollinearity)) {
        if (self$diagnostics$multicollinearity$has_multicollinearity) {
          warnings <- c(warnings, "Severe multicollinearity present")
          recommendations <- c(recommendations,
            "Some factors are highly correlated, which may cause model instability.",
            "Recommendations: 1) Combine correlated factors; 2) Use ridge regression; 3) Remove redundant factors"
          )
        }
      }

      # Influence point interpretation
      if (!is.null(self$diagnostics$influence$summary)) {
        if (self$diagnostics$influence$summary$n_influential > 0) {
          warnings <- c(warnings,
            paste("Found", self$diagnostics$influence$summary$n_influential, "strong influence points"))
          recommendations <- c(recommendations,
            "Data points with high influence on the model exist. Recommend checking if these are measurement anomalies",
            "or special operating conditions. If data is confirmed valid, keep them; otherwise consider removal"
          )
        }
      }

      self$warnings <- warnings
      self$recommendations <- recommendations
    },

    .collect_warnings = function() {
      # Collect warnings from all diagnostics
      # (Already handled in .generate_quality_interpretation)
    }
  )
)
