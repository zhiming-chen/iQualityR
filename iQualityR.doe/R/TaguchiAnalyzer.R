# =============================================================================
# File: R/TaguchiAnalyzer.R
# Description: Taguchi robust design - S/N ratio computation and contribution analysis
# =============================================================================

#' @title TaguchiAnalyzer: S/N Ratio Computation and Contribution Analysis for Taguchi Robust Design
#' @description
#' Handles the core computational logic of the Taguchi Method:
#' - Signal-to-Noise Ratio (S/N) computation
#' - Factor contribution analysis
#' - Full robustness analysis
#'
#' **Core features**:
#' - Larger-the-better, nominal-the-best, and smaller-the-better S/N ratio computation
#' - ANOVA-based factor contribution percentage computation
#' - Robustness metric summarization across control and noise factor combinations
#'
#' @export
TaguchiAnalyzer <- R6::R6Class("TaguchiAnalyzer",
  public = list(

    #' @description Compute the Signal-to-Noise Ratio (S/N Ratio)
    #' @param y Numeric vector of response values from repeated experiments
    #' @param type Character scalar giving the S/N ratio type: `"larger"` (larger-the-better), `"nominal"` (nominal-the-best), or `"smaller"` (smaller-the-better)
    #' @return Numeric scalar giving the S/N ratio value (unit: dB)
    compute_sn_ratio = function(y, type = "nominal") {
      # Argument validation
      if (!is.numeric(y) || length(y) == 0) {
        stop("[TaguchiAnalyzer] y must be a non-empty numeric vector", call. = FALSE)
      }

      # Validate type before length checks so an invalid type is caught
      # regardless of the length of y.
      type <- match.arg(type, choices = c("larger", "nominal", "smaller"))

      if (length(y) == 1 && type != "nominal") {
        stop("[TaguchiAnalyzer] larger/smaller types require multiple observations", call. = FALSE)
      }

      if (type == "larger") {
        # Larger-the-better: S/N = -10 * log10(mean(1/y^2))
        if (any(y == 0, na.rm = TRUE)) {
          stop("[TaguchiAnalyzer] larger type does not allow zero values in y", call. = FALSE)
        }
        sn <- -10 * log10(mean(1 / y^2, na.rm = TRUE))
      } else if (type == "smaller") {
        # Smaller-the-better: S/N = -10 * log10(mean(y^2))
        sn <- -10 * log10(mean(y^2, na.rm = TRUE))
      } else {
        # Nominal-the-best: S/N = 10 * log10(mean^2 / var)
        y_clean <- y[!is.na(y)]
        if (length(y_clean) < 2) {
          stop("[TaguchiAnalyzer] nominal type requires at least 2 non-NA observations to compute variance", call. = FALSE)
        }
        y_bar <- mean(y_clean)
        s2 <- var(y_clean)
        if (s2 == 0) {
          # Return a large finite value instead of Inf to avoid breaking
          # downstream aggregations such as mean().
          warning("[TaguchiAnalyzer] variance of y is zero; returning a large finite S/N value (1e10)", call. = FALSE)
          sn <- 1e10
        } else {
          # Use the ratio of mean square to variance, subtracting 1/n to
          # remove the degrees-of-freedom bias.
          sn <- 10 * log10((y_bar^2 - s2 / length(y_clean)) / s2)
        }
      }

      sn
    },

    #' @description Compute factor contributions
    #' @param anova_table Data frame giving the ANOVA table. Both the legacy
    #'   iQualityR format (columns `Term`, `Df`, `Sum_Sq`, `Mean_Sq`) and the
    #'   standard R `anova()` output (columns `Df`, `Sum Sq`, `Mean Sq` with
    #'   rownames as term labels) are accepted; column names are normalised
    #'   internally so the caller does not need to pre-process the table.
    #'   When `Term` is not present, the rownames of `anova_table` are used.
    #' @return Data frame containing each factor's contribution percentage and cumulative contribution
    compute_contribution = function(anova_table) {
      if (!is.data.frame(anova_table)) {
        stop("[TaguchiAnalyzer] anova_table must be a data.frame", call. = FALSE)
      }

      # Normalize column names so both the legacy iQualityR format
      # (Sum_Sq / Mean_Sq) and the standard R anova() output (Sum Sq / Mean Sq)
      # work transparently. We accept case-insensitive variants and rewrite
      # them to the canonical snake_case names used downstream.
      col_map <- c(
        "Sum_Sq" = "Sum_Sq", "Sum Sq"  = "Sum_Sq", "sum_sq"   = "Sum_Sq",
        "Mean_Sq"= "Mean_Sq","Mean Sq" = "Mean_Sq","mean_sq"  = "Mean_Sq",
        "Df"     = "Df",     "df"      = "Df",
        "F value"= "F_value","F_Value" = "F_value","f_value"  = "F_value",
        "Pr(>F)" = "Pr_F",   "Pr_F"    = "Pr_F",   "pr_f"     = "Pr_F"
      )
      matched <- names(anova_table) %in% names(col_map)
      names(anova_table)[matched] <- col_map[names(anova_table)[matched]]

      # If there is no Term column, use the rownames of the anova table as
      # the term labels. This is what users get from R's base anova() and
      # car::Anova().
      if (!"Term" %in% names(anova_table)) {
        term_labels <- rownames(anova_table)
        if (is.null(term_labels) || length(term_labels) != nrow(anova_table)) {
          term_labels <- paste0("Term", seq_len(nrow(anova_table)))
        }
        anova_table$Term <- term_labels
      }

      required_cols <- c("Term", "Df", "Sum_Sq", "Mean_Sq")
      missing_cols <- setdiff(required_cols, names(anova_table))
      if (length(missing_cols) > 0) {
        stop("[TaguchiAnalyzer] anova_table is missing required columns: ",
             paste(missing_cols, collapse = ", "),
             ". Accepted formats: (1) Term/Df/Sum_Sq/Mean_Sq or ",
             "(2) standard R anova() output (Df/Sum Sq/Mean Sq with rownames).",
             call. = FALSE)
      }

      total_ss <- sum(anova_table$Sum_Sq, na.rm = TRUE)
      if (total_ss == 0) {
        stop("[TaguchiAnalyzer] total sum of squares is zero; cannot compute contribution", call. = FALSE)
      }

      # Compute each factor's contribution percentage. We compute against
      # the total SS (including the residual row, if any) which matches the
      # standard Taguchi contribution convention; the caller may filter out
      # the residual before passing if they want contributions relative only
      # to the model SS.
      contribution_pct <- (anova_table$Sum_Sq / total_ss) * 100

      result <- data.frame(
        Term = anova_table$Term,
        Df = anova_table$Df,
        Sum_Sq = anova_table$Sum_Sq,
        Mean_Sq = anova_table$Mean_Sq,
        Contribution_Pct = round(contribution_pct, 2),
        stringsAsFactors = FALSE
      )

      # Preserve optional F and p columns when present so the output table
      # is a strict superset of the input.
      if ("F_value" %in% names(anova_table)) {
        result$F_Value <- anova_table$F_value
      }
      if ("Pr_F" %in% names(anova_table)) {
        result$Pr_F <- anova_table$Pr_F
      }

      # Add cumulative contribution.
      result$Cumulative_Pct <- round(cumsum(result$Contribution_Pct), 2)

      # Sort by contribution in descending order.
      result <- result[order(-result$Contribution_Pct), ]
      rownames(result) <- NULL

      result
    },

    #' @description Full robustness analysis: compute S/N ratio, mean, and standard deviation for each experimental combination
    #' @param data Data frame containing control factors, noise factors, and the response variable
    #' @param control_factors Character vector of control factor column names
    #' @param noise_factors Character vector of noise factor column names
    #' @param response Character scalar giving the response variable column name
    #' @param sn_type Character scalar giving the S/N ratio type: `"larger"`, `"nominal"`, or `"smaller"`
    #' @return Data frame with one row per control factor level combination, containing the S/N ratio, mean, standard deviation, and number of replicates
    analyze_robustness = function(data, control_factors, noise_factors, response,
                                  sn_type = "nominal") {
      # Argument validation
      if (!is.data.frame(data)) {
        stop("[TaguchiAnalyzer] data must be a data.frame", call. = FALSE)
      }
      if (!is.character(control_factors) || length(control_factors) == 0) {
        stop("[TaguchiAnalyzer] control_factors must be a non-empty character vector", call. = FALSE)
      }
      if (!is.character(noise_factors) || length(noise_factors) == 0) {
        stop("[TaguchiAnalyzer] noise_factors must be a non-empty character vector", call. = FALSE)
      }
      if (!is.character(response) || length(response) != 1) {
        stop("[TaguchiAnalyzer] response must be a character scalar of length 1", call. = FALSE)
      }

      all_factors <- c(control_factors, noise_factors, response)
      missing_cols <- setdiff(all_factors, names(data))
      if (length(missing_cols) > 0) {
        stop("[TaguchiAnalyzer] data is missing columns: ", paste(missing_cols, collapse = ", "),
             call. = FALSE)
      }

      sn_type <- match.arg(sn_type, choices = c("larger", "nominal", "smaller"))

      # Group by control factor combination
      control_combo <- data[, control_factors, drop = FALSE]

      # Compute S/N ratio, mean, and standard deviation for each unique control factor combination
      groups <- split(data, interaction(control_combo, drop = TRUE, sep = "_"))

      results_list <- lapply(groups, function(group_data) {
        y <- group_data[[response]]
        n_reps <- length(y)

        sn_ratio <- self$compute_sn_ratio(y, type = sn_type)
        y_mean <- mean(y, na.rm = TRUE)
        y_sd <- if (n_reps >= 2) sd(y, na.rm = TRUE) else NA_real_

        # Extract control factor levels (take the first row; levels are identical within a group)
        control_levels <- as.list(group_data[1, control_factors, drop = FALSE])

        c(control_levels, list(
          SN_Ratio = sn_ratio,
          Mean = y_mean,
          SD = y_sd,
          N_Reps = n_reps
        ))
      })

      # Combine into a data frame
      result_df <- do.call(rbind, lapply(results_list, function(x) {
        as.data.frame(x, stringsAsFactors = FALSE)
      }))
      rownames(result_df) <- NULL

      # Ensure numeric columns have the correct type
      result_df$SN_Ratio <- as.numeric(result_df$SN_Ratio)
      result_df$Mean <- as.numeric(result_df$Mean)
      result_df$SD <- as.numeric(result_df$SD)
      result_df$N_Reps <- as.integer(result_df$N_Reps)

      result_df
    },

    #' @description Predict the optimal control-factor levels and the
    #'   predicted Signal-to-Noise ratio at those levels, with an
    #'   approximate confidence interval. This implements the Taguchi
    #'   additivity (superposition) model: the predicted SN ratio at the
    #'   optimal setting equals the overall mean SN plus the sum of each
    #'   factor's level effect (level mean minus overall mean) at its
    #'   optimal level.
    #' @param robustness_results Data frame produced by
    #'   `$analyze_robustness()` (one row per control-factor combination,
    #'   with SN_Ratio and the control-factor columns).
    #' @param control_factors Character vector of control-factor column
    #'   names present in `robustness_results`.
    #' @param sn_type Character scalar giving the S/N ratio type used when
    #'   `robustness_results` was computed: `"larger"`, `"nominal"`, or
    #'   `"smaller"`. Determines whether the optimal level maximises
    #'   (larger/nominal) or minimises (smaller) the S/N ratio.
    #' @param conf_level Numeric. Confidence level for the interval on the
    #'   predicted SN ratio (default 0.95).
    #' @return List with `optimal_levels` (named character vector),
    #'   `predicted_sn` (numeric), `se` (standard error), `ci` (numeric
    #'   vector of length 2), and `level_effects` (data frame of each
    #'   factor's per-level mean S/N ratio and the chosen optimal level).
    #' @references Taguchi, G. (1987). System of Experimental Design,
    #'   vol. 1-2. ASI Press. Phadke, M. S. (1989). Quality Engineering
    #'   Using Robust Design. Prentice-Hall, ch. 6.
    predict_optimal = function(robustness_results, control_factors,
                              sn_type = "nominal", conf_level = 0.95) {
      if (!is.data.frame(robustness_results)) {
        stop("[TaguchiAnalyzer] robustness_results must be a data.frame",
             call. = FALSE)
      }
      if (!is.character(control_factors) || length(control_factors) == 0) {
        stop("[TaguchiAnalyzer] control_factors must be a non-empty ",
             "character vector", call. = FALSE)
      }
      missing_cols <- setdiff(c(control_factors, "SN_Ratio"),
                              names(robustness_results))
      if (length(missing_cols) > 0) {
        stop("[TaguchiAnalyzer] robustness_results is missing columns: ",
             paste(missing_cols, collapse = ", "), call. = FALSE)
      }
      sn_type <- match.arg(sn_type, choices = c("larger", "nominal", "smaller"))
      if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
        stop("[TaguchiAnalyzer] conf_level must be in (0, 1)", call. = FALSE)
      }

      overall_mean <- mean(robustness_results$SN_Ratio, na.rm = TRUE)
      # For larger/nominal, pick the level with the highest mean SN ratio;
      # for smaller-the-better, pick the lowest mean SN ratio.
      maximise <- sn_type %in% c("larger", "nominal")

      optimal_levels <- character(length(control_factors))
      names(optimal_levels) <- control_factors
      level_effects_list <- list()

      for (cf in control_factors) {
        level_means <- tapply(robustness_results$SN_Ratio,
                              robustness_results[[cf]], mean, na.rm = TRUE)
        chosen <- if (maximise) {
          names(which.max(level_means))
        } else {
          names(which.min(level_means))
        }
        optimal_levels[cf] <- chosen
        level_effects_list[[cf]] <- data.frame(
          Factor = cf,
          Level = names(level_means),
          Mean_SN = as.numeric(level_means),
          Optimal = names(level_means) == chosen,
          stringsAsFactors = FALSE
        )
      }
      level_effects <- do.call(rbind, level_effects_list)
      rownames(level_effects) <- NULL

      # Additive (superposition) prediction: overall mean plus the sum of the
      # chosen level effects (level mean - overall mean) for each control
      # factor. This is the standard Taguchi prediction formula.
      level_effect_sum <- 0
      for (cf in control_factors) {
        lvl <- optimal_levels[cf]
        lvl_mean <- level_effects$Mean_SN[level_effects$Factor == cf &
                                          level_effects$Level == lvl]
        level_effect_sum <- level_effect_sum + (lvl_mean - overall_mean)
      }
      predicted_sn <- overall_mean + level_effect_sum

      # Standard error of the prediction: estimated from the residual
      # variance of the additive model fitted to the observed SN ratios.
      # The residual variance uses (n - 1 - sum(df_factor)) df; with the
      # additive model the prediction SE is approximated by the residual
      # SD of the level-effect decomposition.
      fitted_sn <- overall_mean + rowSums(sapply(control_factors, function(cf) {
        lvl_means <- tapply(robustness_results$SN_Ratio,
                            robustness_results[[cf]], mean, na.rm = TRUE)
        lvl_means[as.character(robustness_results[[cf]])] - overall_mean
      }))
      residuals <- robustness_results$SN_Ratio - fitted_sn
      n_obs <- sum(!is.na(robustness_results$SN_Ratio))
      res_df <- n_obs - 1L - sum(vapply(control_factors, function(cf) {
        length(unique(robustness_results[[cf]])) - 1L
      }, integer(1)))
      res_df <- max(1L, res_df)
      res_var <- sum(residuals^2, na.rm = TRUE) / res_df
      se <- sqrt(res_var / n_obs)
      z_crit <- stats::qnorm((1 + conf_level) / 2)
      ci <- c(predicted_sn - z_crit * se, predicted_sn + z_crit * se)

      list(
        optimal_levels = optimal_levels,
        predicted_sn   = predicted_sn,
        overall_mean   = overall_mean,
        se             = se,
        ci             = ci,
        conf_level     = conf_level,
        level_effects  = level_effects
      )
    },

    #' @description Compute the dynamic Signal-to-Noise ratio for a Taguchi
    #'   experiment with a signal (input) factor. The dynamic SN ratio
    #'   evaluates the input-output (linear) relationship between the signal
    #'   factor `M` and the response `y`. Two reference models are
    #'   supported: `"zero_point"` (zero-point proportional,
    #'   y = beta * M, the most common dynamic Taguchi model) and
    #'   `"linear"` (reference-point proportional, y = beta0 + beta1 * M).
    #' @param y Numeric vector of response values.
    #' @param M Numeric vector of signal-factor values (same length as y).
    #' @param type Character scalar giving the dynamic model:
    #'   `"zero_point"` or `"linear"`.
    #' @return Numeric scalar giving the dynamic S/N ratio (dB).
    #' @references Taguchi, G., Elsayed, E. A., & Hsiang, T. (1989). Quality
    #'   Engineering in Production Systems. McGraw-Hill, ch. 5.
    compute_dynamic_sn_ratio = function(y, M, type = "zero_point") {
      if (!is.numeric(y) || !is.numeric(M) || length(y) != length(M)) {
        stop("[TaguchiAnalyzer] y and M must be numeric vectors of equal ",
             "length", call. = FALSE)
      }
      if (length(y) < 2) {
        stop("[TaguchiAnalyzer] dynamic SN ratio requires at least 2 ",
             "observations", call. = FALSE)
      }
      type <- match.arg(type, choices = c("zero_point", "linear"))

      ok <- is.finite(y) & is.finite(M)
      y <- y[ok]; M <- M[ok]
      n <- length(y)
      if (n < 2) {
        stop("[TaguchiAnalyzer] fewer than 2 complete (y, M) pairs",
             call. = FALSE)
      }

      if (type == "zero_point") {
        # Zero-point proportional: beta_hat = sum(y*M) / sum(M^2).
        S_M2 <- sum(M^2)
        if (S_M2 <= 0) {
          stop("[TaguchiAnalyzer] zero_point dynamic SN requires nonzero ",
               "signal values", call. = FALSE)
        }
        beta_hat <- sum(y * M) / S_M2
        # Total sum of squares of y.
        S_T <- sum(y^2)
        # Variance of residuals around the proportional model.
        S_e <- S_T - beta_hat^2 * S_M2
        # Effective variance (per degree of freedom): n-1 because the slope
        # consumes one df.
        Ve <- S_e / (n - 1)
        if (Ve <= 0) {
          # Perfect linear fit through origin: return a large finite value.
          return(1e10)
        }
        # Dynamic SN ratio = 10 * log10(beta^2 / Ve).
        10 * log10(beta_hat^2 / Ve)
      } else {
        # Linear (reference-point proportional): y = beta0 + beta1 * M.
        fit <- stats::lm(y ~ M)
        beta1 <- stats::coef(fit)["M"]
        # ANOVA-style decomposition.
        S_T <- sum((y - mean(y))^2)
        S_beta1 <- sum((stats::fitted(fit) - mean(y))^2)
        S_e <- S_T - S_beta1
        Ve <- S_e / (n - 2)
        if (Ve <= 0) return(1e10)
        10 * log10(beta1^2 / Ve)
      }
    },

    #' @description Full dynamic robustness analysis: compute the dynamic
    #'   S/N ratio for each control-factor combination when a signal
    #'   factor is present. This is the dynamic counterpart of
    #'   `$analyze_robustness()`: instead of grouping replicates of a
    #'   static response, each control-factor combination contributes a
    #'   set of (y, M) pairs across the signal-factor levels.
    #' @param data Data frame containing control factors, the signal factor
    #'   and the response.
    #' @param control_factors Character vector of control-factor column names.
    #' @param signal_factor Character scalar giving the signal-factor column.
    #' @param response Character scalar giving the response column.
    #' @param type Character scalar giving the dynamic model:
    #'   `"zero_point"` or `"linear"`.
    #' @return Data frame with one row per control-factor combination,
    #'   containing the dynamic S/N ratio, slope (beta), and number of
    #'   (y, M) pairs.
    analyze_dynamic_robustness = function(data, control_factors, signal_factor,
                                          response, type = "zero_point") {
      if (!is.data.frame(data)) {
        stop("[TaguchiAnalyzer] data must be a data.frame", call. = FALSE)
      }
      if (!is.character(control_factors) || length(control_factors) == 0) {
        stop("[TaguchiAnalyzer] control_factors must be a non-empty ",
             "character vector", call. = FALSE)
      }
      if (!is.character(signal_factor) || length(signal_factor) != 1) {
        stop("[TaguchiAnalyzer] signal_factor must be a character scalar",
             call. = FALSE)
      }
      if (!is.character(response) || length(response) != 1) {
        stop("[TaguchiAnalyzer] response must be a character scalar",
             call. = FALSE)
      }
      type <- match.arg(type, choices = c("zero_point", "linear"))
      needed <- c(control_factors, signal_factor, response)
      missing_cols <- setdiff(needed, names(data))
      if (length(missing_cols) > 0) {
        stop("[TaguchiAnalyzer] data is missing columns: ",
             paste(missing_cols, collapse = ", "), call. = FALSE)
      }

      # Group by control-factor combination.
      control_combo <- data[, control_factors, drop = FALSE]
      groups <- split(data, interaction(control_combo, drop = TRUE, sep = "_"))

      results_list <- lapply(groups, function(group_data) {
        y <- group_data[[response]]
        M <- group_data[[signal_factor]]
        n_pts <- length(y)

        sn_ratio <- self$compute_dynamic_sn_ratio(y, M, type = type)
        # Slope (beta) of the input-output relationship.
        beta <- if (type == "zero_point") {
          if (sum(M^2) > 0) sum(y * M) / sum(M^2) else NA_real_
        } else {
          stats::coef(stats::lm(y ~ M))["M"]
        }

        control_levels <- as.list(group_data[1, control_factors, drop = FALSE])
        c(control_levels, list(
          SN_Ratio = sn_ratio,
          Slope = as.numeric(beta),
          N_Points = n_pts
        ))
      })

      result_df <- do.call(rbind, lapply(results_list, function(x) {
        as.data.frame(x, stringsAsFactors = FALSE)
      }))
      rownames(result_df) <- NULL
      result_df$SN_Ratio <- as.numeric(result_df$SN_Ratio)
      result_df$Slope <- as.numeric(result_df$Slope)
      result_df$N_Points <- as.integer(result_df$N_Points)
      result_df
    }
  )
)
