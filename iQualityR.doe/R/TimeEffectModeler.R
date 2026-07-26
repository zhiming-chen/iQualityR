# =============================================================================
# File: R/TimeEffectModeler.R
# Description: Time-effect modeling - product storage / aging / decay / stability
# Theory: Arrhenius equation, Weibull decay model, first/zero-order kinetics,
#         ICH Q1E guideline
# Applications: Food shelf life, pharmaceutical stability, material aging,
#               battery degradation, process parameter drift
# =============================================================================

#' @title TimeEffectModeler: Time-Effect Modeler
#' @description
#' Models the change of a product response over time. Supported capabilities:
#' 1. **Decay models**: zero-order / first-order kinetics, Weibull decay,
#'    exponential decay to a plateau.
#' 2. **Shelf-life prediction**: based on the ICH Q1E guideline, predicts the
#'    shelf life from the lower confidence bound of the response.
#' 3. **Multi-temperature acceleration**: uses the Arrhenius equation to
#'    extrapolate the time effect at a target (e.g. room) temperature.
#'
#' @details
#' **Theoretical references**:
#' - ICH Q1E (2003). *Evaluation for Stability Data*.
#' - Arrhenius, S. (1889). *Uber die Reaktionsgeschwindigkeit bei der Inversion
#'   von Rohrzucker*.
#' - Weibull, W. (1951). *A statistical distribution function of wide
#'   applicability*.
#'
#' @export
TimeEffectModeler <- R6::R6Class("TimeEffectModeler",
  public = list(

    #' @description Create a new TimeEffectModeler instance.
    #' @return A `TimeEffectModeler` instance (invisibly).
    initialize = function() {
      invisible(self)
    },

    # =========================================================================
    # Method 1: Time decay model fitting
    # =========================================================================

    #' @description Fit a time decay model to observed (time, response) data.
    #' @param time Numeric vector of time points (days / months / years).
    #' @param response Numeric vector of response values at the given time points.
    #' @param model_type Character scalar giving the decay model to fit:
    #'   `"zero_order"` (zero-order kinetics), `"first_order"` (first-order
    #'   kinetics, the default), `"weibull"` (Weibull decay) or
    #'   `"exponential"` (exponential decay to a plateau).
    #' @return A list with elements `model_type`, `parameters`, `r_squared`,
    #'   `n_observations` and `model_obj` (the underlying `lm` object).
    fit_decay_model = function(time, response, model_type = "first_order") {
      n <- length(time)
      if (n < 3) stop("[TEM] At least 3 time points are required for fitting.")
      model_type <- match.arg(model_type,
                              c("zero_order", "first_order", "weibull", "exponential"))

      if (model_type == "zero_order") {
        # Zero-order kinetics: R(t) = R0 - k*t
        model <- lm(response ~ time)
        R0 <- coef(model)[1]
        k <- -coef(model)[2]
        r_squared <- summary(model)$r.squared

      } else if (model_type == "first_order") {
        # First-order kinetics: R(t) = R0 * exp(-k*t)
        # Linearization: ln(R) = ln(R0) - k*t
        if (any(response <= 0)) {
          stop("[TEM] First-order kinetics requires all response values to be > 0.")
        }
        log_response <- log(response)
        model <- lm(log_response ~ time)
        R0 <- exp(coef(model)[1])
        k <- -coef(model)[2]
        r_squared <- cor(response, R0 * exp(-k * time))^2

      } else if (model_type == "weibull") {
        # Weibull decay: R(t) = R0 * exp(-(t/lambda)^beta)
        # Linearization: ln(-ln(R/R0)) = beta*ln(t) - beta*ln(lambda)
        if (any(response <= 0)) {
          stop("[TEM] The Weibull model requires all response values to be > 0.")
        }
        if (any(time <= 0)) {
          stop("[TEM] The Weibull model requires all time values to be > 0.")
        }
        R0 <- max(response)

        ratio <- response / R0
        ratio <- pmax(pmin(ratio, 0.999), 0.001)  # Boundary protection
        y_trans <- log(-log(ratio))
        x_trans <- log(time)

        model <- lm(y_trans ~ x_trans)
        beta <- coef(model)[2]
        lambda <- exp(-coef(model)[1] / beta)
        k <- NA_real_  # Weibull has no single rate constant; use lambda/beta
        r_squared <- cor(y_trans, fitted(model))^2

      } else {
        # Exponential decay to a plateau: R(t) = R_inf + (R0 - R_inf)*exp(-k*t)
        R_inf <- min(response)
        R0 <- max(response)
        y_adj <- (response - R_inf) / (R0 - R_inf + .Machine$double.eps)
        y_adj <- pmax(y_adj, 0.001)

        model <- lm(log(y_adj) ~ time)
        k <- -coef(model)[2]
        r_squared <- cor(response, R_inf + (R0 - R_inf) * exp(-k * time))^2
      }

      list(
        model_type = model_type,
        parameters = list(
          R0 = R0,
          k = k,
          lambda = if (model_type == "weibull") lambda else NA_real_,
          beta = if (model_type == "weibull") beta else NA_real_,
          R_inf = if (model_type == "exponential") R_inf else NA_real_
        ),
        r_squared = r_squared,
        n_observations = n,
        model_obj = model
      )
    },

    #' @description Predict the response at new time points from a fitted
    #'   decay model.
    #' @param fit_result Return value of `fit_decay_model()`.
    #' @param new_time Numeric vector of time points at which to predict.
    #' @param se_fit Logical scalar; if `TRUE`, also return the prediction
    #'   standard error and a 95% interval.
    #' @return A numeric vector of predictions, or (when `se_fit = TRUE`) a
    #'   list with elements `predict`, `se.fit`, `lower` and `upper`.
    predict_response = function(fit_result, new_time, se_fit = FALSE) {
      model_type <- fit_result$model_type
      params <- fit_result$parameters

      if (model_type == "zero_order") {
        pred <- params$R0 - params$k * new_time
      } else if (model_type == "first_order") {
        pred <- params$R0 * exp(-params$k * new_time)
      } else if (model_type == "weibull") {
        pred <- params$R0 * exp(-(new_time / params$lambda)^params$beta)
      } else if (model_type == "exponential") {
        pred <- params$R_inf + (params$R0 - params$R_inf) * exp(-params$k * new_time)
      }

      pred <- pmax(pred, 0)  # Boundary protection

      if (se_fit) {
        # Simplified SE estimate based on the residual standard deviation.
        model <- fit_result$model_obj
        sigma <- summary(model)$sigma
        se <- rep(sigma, length(new_time))
        list(
          predict = pred,
          se.fit = se,
          lower = pred - 1.96 * se,
          upper = pred + 1.96 * se
        )
      } else {
        pred
      }
    },

    # =========================================================================
    # Method 2: Arrhenius accelerated aging model
    # =========================================================================

    #' @description Fit an Arrhenius accelerated-aging model from multi-
    #'   temperature stability data.
    #' @param data Data frame with columns `time`, `temperature` (in degrees
    #'   Celsius) and the response column named by `response_col`.
    #' @param response_col Character scalar giving the name of the response
    #'   column.
    #' @param Ea_activation Optional numeric scalar, the activation energy
    #'   (J/mol). When `NULL` (the default) it is estimated from the data.
    #' @return A list with the Arrhenius parameters and the per-temperature
    #'   first-order fits.
    fit_arrhenius_model = function(data, response_col, Ea_activation = NULL) {
      required_cols <- c("time", "temperature", response_col)
      missing_cols <- setdiff(required_cols, names(data))
      if (length(missing_cols) > 0) {
        stop("[TEM] Data is missing columns: ",
             paste(missing_cols, collapse = ", "))
      }

      R_gas <- 8.314  # Gas constant J/(mol*K)

      # Fit first-order kinetics separately at each temperature.
      temps <- unique(data$temperature)
      k_values <- numeric(length(temps))
      r_squared_values <- numeric(length(temps))

      for (i in seq_along(temps)) {
        subset_data <- data[data$temperature == temps[i], ]
        if (nrow(subset_data) >= 3) {
          fit <- self$fit_decay_model(
            time = subset_data$time,
            response = subset_data[[response_col]],
            model_type = "first_order"
          )
          k_values[i] <- fit$parameters$k
          r_squared_values[i] <- fit$r_squared
        } else {
          k_values[i] <- NA
          r_squared_values[i] <- NA
        }
      }

      # Arrhenius equation: ln(k) = ln(A) - Ea/(R*T)
      valid_idx <- which(!is.na(k_values) & k_values > 0)
      if (length(valid_idx) < 2) {
        stop("[TEM] At least 2 valid temperatures are required to fit the Arrhenius equation.")
      }

      ln_k <- log(k_values[valid_idx])
      T_valid <- temps[valid_idx] + 273.15  # Use the Kelvin temperature of each point
      inv_T <- 1 / T_valid

      arrhenius_model <- lm(ln_k ~ inv_T)
      ln_A <- coef(arrhenius_model)[1]
      Ea <- -coef(arrhenius_model)[2] * R_gas

      if (!is.null(Ea_activation)) {
        Ea <- Ea_activation
      }

      list(
        Ea_activation = Ea,
        pre_exponential_factor = exp(ln_A),
        k_values = setNames(k_values, temps),
        r_squared_individual = setNames(r_squared_values, temps),
        r_squared_arrhenius = summary(arrhenius_model)$r.squared,
        arrhenius_model_obj = arrhenius_model
      )
    },

    #' @description Predict the response at a target temperature by Arrhenius
    #'   extrapolation.
    #' @param arrhenius_result Return value of `fit_arrhenius_model()`.
    #' @param target_temp Numeric scalar, the target temperature in degrees
    #'   Celsius (default 25).
    #' @param new_time Numeric vector of time points at which to predict.
    #' @param R0 Numeric scalar, the assumed initial (normalized) response at
    #'   time zero. The default `R0 = 1` returns the fraction of the initial
    #'   response remaining. Supply the actual initial response to obtain
    #'   absolute predictions.
    #' @return Numeric vector of predicted responses at the target temperature.
    predict_at_target_temp = function(arrhenius_result, target_temp = 25, new_time,
                                      R0 = 1) {
      Ea <- arrhenius_result$Ea_activation
      A <- arrhenius_result$pre_exponential_factor
      R_gas <- 8.314
      T_K <- target_temp + 273.15

      # Rate constant at the target temperature.
      k_target <- A * exp(-Ea / (R_gas * T_K))

      # First-order prediction. With the default R0 = 1 the result is the
      # fraction of the initial response remaining at each time point.
      R0 * exp(-k_target * new_time)
    },

    # =========================================================================
    # Method 3: Shelf-life prediction (ICH Q1E)
    # =========================================================================

    #' @description Estimate the shelf life from a fitted decay model.
    #' @param fit_result Return value of `fit_decay_model()`.
    #' @param specification Numeric scalar, the response specification limit
    #'   below which the product is considered out of compliance.
    #' @param confidence_level Numeric scalar, the confidence level for the
    #'   shelf-life interval (default 0.95).
    #' @return A list with the shelf-life estimate, the lower/upper bounds and
    #'   a human-readable note.
    estimate_shelf_life = function(fit_result, specification, confidence_level = 0.95) {
      model_type <- fit_result$model_type
      params <- fit_result$parameters
      k_abs <- abs(params$k)

      # Closed-form shelf-life.
      if (model_type == "zero_order") {
        t_shelf <- (params$R0 - specification) / params$k
      } else if (model_type == "first_order") {
        t_shelf <- log(params$R0 / specification) / params$k
      } else if (model_type == "weibull") {
        t_shelf <- params$lambda * (-log(specification / params$R0))^(1 / params$beta)
      } else if (model_type == "exponential") {
        if (specification <= params$R_inf) {
          warning("[TEM] Specification limit is below the plateau value; shelf life is theoretically infinite.")
          t_shelf <- Inf
        } else {
          t_shelf <- -log((specification - params$R_inf) / (params$R0 - params$R_inf)) / params$k
        }
      }

      # Delta-method confidence interval.
      # For first-order kinetics t_shelf = log(R0/spec) / k, so
      # dt/dk = -t_shelf / k and SE(t_shelf) ~= (t_shelf / |k|) * SE(k).
      # Here the residual standard deviation `sigma` is used as a conservative
      # proxy for SE(k).
      model <- fit_result$model_obj
      sigma <- summary(model)$sigma
      if (is.finite(t_shelf) && k_abs > 0) {
        se_t <- sigma * abs(t_shelf) / k_abs
      } else {
        se_t <- NA_real_
      }
      # ICH Q1E specifies a one-sided confidence bound on shelf life:
      # report the (1 - alpha) lower bound using the one-sided z quantile,
      # i.e. z = qnorm(confidence_level). For a default 95% level, z = 1.645
      # (not the two-sided qnorm(0.975) = 1.96). An upper bound is also
      # reported for reference using the same z value.
      z_val <- qnorm(confidence_level)
      t_lower <- t_shelf - z_val * se_t
      t_upper <- t_shelf + z_val * se_t

      list(
        shelf_life = t_shelf,
        lower_95 = max(t_lower, 0),
        upper_95 = t_upper,
        se_shelf_life = se_t,
        confidence_level = confidence_level,
        specification = specification,
        model_type = model_type,
        note = sprintf("Estimated shelf life: %.1f time units [%.1f, %.1f] (%.0f%% CI)",
                       t_shelf, max(t_lower, 0), t_upper, confidence_level * 100)
      )
    },

    # =========================================================================
    # Method 4: Multi-batch time-effect analysis (ICH Q1E ANOVA)
    # =========================================================================

    #' @description Analyze the consistency of the time effect across batches.
    #' @param data Data frame with columns `batch`, `time` and the response
    #'   column named by `response_col`.
    #' @param response_col Character scalar giving the name of the response
    #'   column.
    #' @return A list with the ANOVA table, the batch-by-time interaction
    #'   p-value, a `poolable` flag and a textual interpretation.
    analyze_multi_batch = function(data, response_col) {
      required_cols <- c("batch", "time", response_col)
      missing_cols <- setdiff(required_cols, names(data))
      if (length(missing_cols) > 0) {
        stop("[TEM] Data is missing columns: ",
             paste(missing_cols, collapse = ", "))
      }

      # ANOVA: response ~ batch * time
      anova_model <- lm(reformulate(c("batch", "time", "batch:time"),
                                     response = response_col),
                        data = data)
      anova_table <- anova(anova_model)

      # Extract the interaction p-value (guard against missing term).
      interaction_p <- anova_table$`Pr(>F)`["batch:time"]
      if (is.na(interaction_p)) {
        interaction_p <- NA_real_
      }

      poolable <- !is.na(interaction_p) && interaction_p > 0.25
      interpretation <- if (is.na(interaction_p)) {
        "Interaction p-value is not available."
      } else if (poolable) {
        "No significant difference in time effect across batches; batches can be pooled."
      } else {
        "Significant difference in time effect across batches; analyze each batch separately."
      }

      list(
        anova_table = anova_table,
        batch_time_interaction_p = interaction_p,
        poolable = poolable,
        interpretation = interpretation
      )
    },

    # =========================================================================
    # Method 5: Visualization
    # =========================================================================

    #' @description Plot the time-effect model with observed points, fitted
    #'   curve and prediction interval.
    #' @param fit_result Return value of `fit_decay_model()`.
    #' @param time Numeric vector of the original observed time points.
    #' @param response Numeric vector of the original observed responses.
    #' @param prediction_time Numeric vector of time points at which to draw
    #'   the prediction curve. When `NULL` a regular grid covering 0 to 120%
    #'   of the observed range is used.
    #' @param theme_obj Optional ggplot2 theme object to apply to the plot.
    #' @return A `ggplot` object (invisibly).
    plot_time_effect = function(fit_result, time, response,
                                prediction_time = NULL, theme_obj = NULL) {
      if (is.null(prediction_time)) {
        prediction_time <- seq(0, max(time) * 1.2, length.out = 100)
      }

      pred_result <- self$predict_response(fit_result, prediction_time, se_fit = TRUE)

      # Observed data.
      df_obs <- data.frame(
        time = time,
        response = response,
        type = "observed"
      )

      # Prediction curve.
      df_pred <- data.frame(
        time = prediction_time,
        predict = pred_result$predict,
        lower = pred_result$lower,
        upper = pred_result$upper,
        type = "predicted"
      )

      p <- ggplot() +
        geom_ribbon(data = df_pred,
                    aes(x = time, ymin = lower, ymax = upper),
                    fill = "#2C7BB6", alpha = 0.2) +
        geom_line(data = df_pred,
                  aes(x = time, y = predict),
                  color = "#2C7BB6", linewidth = 1.2) +
        geom_point(data = df_obs,
                   aes(x = time, y = response),
                   size = 3, color = "#D9534F") +
        labs(
          title = sprintf("Time Effect Model (%s)", fit_result$model_type),
          subtitle = sprintf("R^2 = %.4f, n = %d",
                             fit_result$r_squared, fit_result$n_observations),
          x = "Time",
          y = "Response"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 14),
          panel.grid.minor = element_blank()
        )

      if (!is.null(theme_obj)) {
        p <- tryCatch(p + theme_obj, error = function(e) p)
      }

      invisible(p)
    },

    # =========================================================================
    # Method 6: Report generation (consistency with IqrDoeTask)
    # =========================================================================

    #' @description Generate a textual or CSV summary report from a fitted
    #'   `TimeEffectModeler` result. This method parallels the
    #'   `IqrDoeTask$report()` API so that all modelers in the DOE ecosystem
    #'   expose a consistent reporting interface. The method auto-detects the
    #'   type of the supplied `fit_result` (decay model fit, Arrhenius model
    #'   fit, or multi-batch analysis) and dispatches to the appropriate
    #'   summary layout.
    #' @param fit_result Return value of `fit_decay_model()`,
    #'   `fit_arrhenius_model()`, or `analyze_multi_batch()`.
    #' @param path Character scalar; output file path. When `NULL` (the
    #'   default) the report is printed to the console. Otherwise the report
    #'   is written to the supplied file in the format selected by `format`.
    #' @param format Character scalar selecting the output format when
    #'   `path` is supplied: `"text"` (default, plain text) or `"csv"` (CSV
    #'   table of the parameter estimates / ANOVA rows).
    #' @return The textual report as a character scalar (invisibly).
    report = function(fit_result = NULL, path = NULL,
                      format = c("text", "csv")) {
      format <- match.arg(format)

      if (is.null(fit_result)) {
        stop("[TEM] fit_result is required. Pass the return value of ",
             "fit_decay_model(), fit_arrhenius_model(), or analyze_multi_batch().",
             call. = FALSE)
      }

      lines <- character()
      lines <- c(lines,
                 "================ TimeEffectModeler Report ================")
      lines <- c(lines,
                 sprintf("Generated: %s",
                         format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

      # Dispatch on the structure of fit_result so a single report() method
      # covers all three analysis entry points.
      if (!is.null(fit_result$model_type)) {
        # ----- fit_decay_model result -----
        lines <- c(lines, "")
        lines <- c(lines, "--- Decay Model Fit ---")
        lines <- c(lines, sprintf("Model type:    %s", fit_result$model_type))
        lines <- c(lines, sprintf("N observations:%d", fit_result$n_observations))
        lines <- c(lines, sprintf("R-squared:     %.4f", fit_result$r_squared))
        lines <- c(lines, "")
        lines <- c(lines, "Parameters:")
        params <- fit_result$parameters
        for (nm in names(params)) {
          val <- params[[nm]]
          if (!is.na(val)) {
            lines <- c(lines, sprintf("  %-10s = %.6g", nm, val))
          }
        }
        # Optional: attach shelf-life estimate when available alongside the
        # fit result. The caller may pass a list(fit = ..., shelf = ...).
        if (!is.null(fit_result$shelf_life)) {
          lines <- c(lines, "")
          lines <- c(lines, "Shelf-life (ICH Q1E):")
          lines <- c(lines,
                     sprintf("  Estimate:    %.3f", fit_result$shelf_life))
          if (!is.null(fit_result$lower_95)) {
            lines <- c(lines,
                       sprintf("  Lower 95%%:   %.3f", fit_result$lower_95))
          }
          if (!is.null(fit_result$upper_95)) {
            lines <- c(lines,
                       sprintf("  Upper 95%%:   %.3f", fit_result$upper_95))
          }
        }
      } else if (!is.null(fit_result$Ea_activation)) {
        # ----- fit_arrhenius_model result -----
        lines <- c(lines, "")
        lines <- c(lines, "--- Arrhenius Model ---")
        lines <- c(lines,
                   sprintf("Activation energy (Ea):        %.2f J/mol",
                           fit_result$Ea_activation))
        lines <- c(lines,
                   sprintf("Pre-exponential factor (A):    %.6g",
                           fit_result$pre_exponential_factor))
        if (!is.null(fit_result$r_squared_arrhenius)) {
          lines <- c(lines,
                     sprintf("Arrhenius R-squared:           %.4f",
                             fit_result$r_squared_arrhenius))
        }
        lines <- c(lines, "")
        lines <- c(lines, "Per-temperature first-order rate constants:")
        for (nm in names(fit_result$k_values)) {
          r2_val <- fit_result$r_squared_individual[[nm]]
          r2_str  <- if (is.na(r2_val)) "NA" else sprintf("%.4f", r2_val)
          lines <- c(lines,
                     sprintf("  T = %s C: k = %.6g (R^2 = %s)",
                             nm, fit_result$k_values[[nm]], r2_str))
        }
      } else if (!is.null(fit_result$anova_table)) {
        # ----- analyze_multi_batch result -----
        lines <- c(lines, "")
        lines <- c(lines, "--- Multi-Batch Analysis (ICH Q1E) ---")
        ip <- fit_result$batch_time_interaction_p
        ip_str <- if (is.na(ip)) "NA" else sprintf("%.4f", ip)
        lines <- c(lines,
                   sprintf("Batch x Time interaction p-value: %s", ip_str))
        lines <- c(lines,
                   sprintf("Poolable:                          %s",
                           fit_result$poolable))
        lines <- c(lines,
                   sprintf("Interpretation:                    %s",
                           fit_result$interpretation))
        lines <- c(lines, "")
        lines <- c(lines, "ANOVA Table:")
        lines <- c(lines,
                   paste(capture.output(print(fit_result$anova_table)),
                         collapse = "\n"))
      } else {
        # Unknown structure
        lines <- c(lines, "")
        lines <- c(lines,
                   "(Unrecognized fit_result structure; cannot summarize.)")
        lines <- c(lines, "Available top-level fields:")
        for (nm in names(fit_result)) {
          lines <- c(lines, sprintf("  - %s", nm))
        }
      }

      lines <- c(lines, "")
      lines <- c(lines,
                 "=========================================================")

      report_text <- paste(lines, collapse = "\n")

      if (is.null(path)) {
        cat(report_text, "\n")
      } else {
        if (format == "csv") {
          # Write the most useful tabular structure as CSV.
          if (!is.null(fit_result$parameters)) {
            params_df <- data.frame(
              Parameter = names(fit_result$parameters),
              Value = as.numeric(unlist(fit_result$parameters)),
              stringsAsFactors = FALSE
            )
            params_df <- params_df[!is.na(params_df$Value), , drop = FALSE]
            utils::write.csv(params_df, path, row.names = FALSE)
          } else if (!is.null(fit_result$k_values)) {
            temp_df <- data.frame(
              Temperature_C = as.numeric(names(fit_result$k_values)),
              k = as.numeric(unlist(fit_result$k_values)),
              R_squared = as.numeric(unlist(fit_result$r_squared_individual)),
              stringsAsFactors = FALSE
            )
            utils::write.csv(temp_df, path, row.names = FALSE)
          } else if (!is.null(fit_result$anova_table)) {
            anova_df <- as.data.frame(fit_result$anova_table)
            utils::write.csv(anova_df, path, row.names = FALSE)
          } else {
            # Fallback: write the text report with .csv extension.
            writeLines(report_text, path)
          }
        } else {
          writeLines(report_text, path)
        }
        message("[TEM] Report written to: ", path)
      }

      invisible(report_text)
    }
  ),

  private = list()
)
