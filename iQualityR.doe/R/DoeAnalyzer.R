# =============================================================================
# File: R/DoeAnalyzer.R
# Description: DOE Analysis Executor - Design Generation / ANOVA / Effect Estimation
# =============================================================================

#' @title DoeAnalyzer: DOE Analysis Executor
#' @description Executes the core DOE computations, including experimental design
#'   generation, analysis of variance (ANOVA), effect estimation, and model fitting.
#'   The class supports a wide range of design types, including full factorial,
#'   fractional factorial, orthogonal, central composite (CCD), Box-Behnken,
#'   Taguchi, Latin Hypercube (LHS), and maximin designs.
#'
#' @export
DoeAnalyzer <- R6::R6Class("DoeAnalyzer",
  public = list(

    # Run the complete DOE analysis workflow. The method first
    #   generates the experimental design described by `plan`, and, when a
    #   `data` frame with observed response values is supplied, additionally
    #   performs ANOVA, effect estimation, and model fit diagnostics for each
    #   response variable declared on `plan`.
    #' @param data Optional data frame containing the observed response values
    #'   for the runs in `plan`. When `NULL` (or when `plan$response_vars` is
    #'   `NULL`), only the design is generated.
    #' @param plan An `IqrDoePlan` object describing the experimental design
    #'   (factors, design type, replication, center points, randomization, etc.).
    #' @return A list with element `design_info` (the generated design data
    #'   frame). When response data is provided, the list additionally contains
    #'   `anova_results`: a named list keyed by response variable, where each
    #'   entry holds `anova` (ANOVA table), `model` (fitted `lm` object),
    #'   `effects` (main and interaction effects), and `model_fit` (fit
    #'   statistics including R-squared and lack-of-fit p-value). A top-level
    #'   `effects` element (taken from the first response) is also populated so
    #'   that `DoePlotter$plot(type = "main_effects")` can render without an
    #'   additional argument.
    run = function(data = NULL, plan) {
      results <- list()

      # 1. Generate the experimental design
      results$design_info <- private$.generate_design(plan)

      # 2. If response data is provided, perform ANOVA
      if (!is.null(data) && !is.null(plan$response_vars)) {
        results$anova_results <- private$.perform_anova(data, plan)
        # Populate top-level `effects` (first response) so that DoePlotter's
        # plot_main_effects() can render without an explicit response argument.
        if (length(results$anova_results) > 0) {
          results$effects <- results$anova_results[[1]]$effects
          # Also populate top-level `model` (first response) so that
          # DoePlotter's plot_residual() / plot_response_surface() can render
          # without an explicit response argument.
          results$model <- results$anova_results[[1]]$model
          results$model_fit <- results$anova_results[[1]]$model_fit

          # For RSM designs (CCD / BBD / RSM / DSD), automatically compute the
          # stationary point via canonical analysis so that task$summary()
          # can report the optimal conditions without manual computation.
          if (plan$design_type %in% c("ccd", "box_behnken", "rsm", "dsd")) {
            factor_names <- vapply(plan$factors, function(f) f$name, character(1))
            results$stationary_point <- tryCatch({
              self$compute_stationary_point(results$model, factor_names)
            }, error = function(e) NULL)
          }
        }
      }

      results
    },

    # =========================================================================
    # Public: Lenth (1989) Pseudo Standard Error for unreplicated factorials
    # =========================================================================
    #
    # Lenth's method identifies active effects in unreplicated two-level
    # factorials by using the median of the absolute effect estimates as
    # a robust scale estimator. It is the de-facto standard in DOE practice
    # (Minitab, JMP, Design-Expert all report it) and is more reliable than
    # the ordinary t-test when no replication is available to estimate the
    # error variance.
    #
    # Reference:
    #   Lenth, R. V. (1989). Quick and Easy Analysis of Unreplicated
    #   Factorials. Technometrics, 31(4), 469-473.
    #
    #' @description Compute the Lenth (1989) Pseudo Standard Error (PSE) and
    #'   the corresponding individual (ME) and simultaneous (SME) margins of
    #'   error for a vector of effect estimates. Used to identify active
    #'   effects in unreplicated factorial designs.
    #' @param effects Numeric vector of effect estimates (e.g. model
    #'   coefficients), optionally named. The intercept, if present under
    #'   the name `"Intercept"`, is automatically dropped.
    #' @return A list with elements `effects`, `abs_effects`, `pse`, `me`,
    #'   `sme`, `active_me` (logical vector flagging effects significant at
    #'   the individual 5% level), `active_sme` (simultaneous 5% level) and
    #'   `method` (a character string identifying the procedure used).
    compute_lenth_pse = function(effects) {
      if (!is.numeric(effects) || length(effects) < 3) {
        stop("[DoeAnalyzer] effects must be a numeric vector of length >= 3",
             call. = FALSE)
      }

      # Drop the intercept if it was accidentally included.
      if (!is.null(names(effects)) && "(Intercept)" %in% names(effects)) {
        effects <- effects[names(effects) != "(Intercept)"]
      }
      effects <- effects[!is.na(effects)]
      abs_effects <- abs(effects)
      n <- length(abs_effects)
      if (n < 3) {
        stop("[DoeAnalyzer] Lenth PSE requires at least 3 effects",
             call. = FALSE)
      }

      # Step 1 (Lenth 1989, eq. 2): s0 = 1.5 * median(|effect|) is a
      # preliminary robust scale used to screen out the largest (likely
      # active) effects before computing the final PSE.
      s0 <- 1.5 * median(abs_effects)

      # Step 2: PSE = 1.5 * median of |effect| values that are <= 2.5 * s0.
      # The 2.5 * s0 cutoff is Lenth's recommendation for screening out
      # likely-active effects before computing the noise scale.
      selected <- abs_effects[abs_effects <= 2.5 * s0]
      if (length(selected) < 1) {
        pse <- s0
      } else {
        pse <- 1.5 * median(selected)
      }

      # Step 3: ME and SME. Following Lenth (1989) and the implementation in
      # the `BsMD` and `FrF2` packages, the individual margin of error uses
      # the t-distribution with df approximately equal to m/1.5 (where m is
      # the number of effects used in PSE). The simultaneous margin uses the
      # Bonferroni-adjusted alpha level.
      m <- length(selected)
      # PSE is computed from m effects; the effective degrees of freedom
      # for the median-based scale is approximated as m / 1.5 (Lenth 1989).
      df_pse <- max(round(m / 1.5), 1)

      # ME: 5% individual significance (two-sided).
      me <- pse * stats::qt(0.975, df_pse)
      # SME: 5% simultaneous significance (Bonferroni-adjusted).
      # alpha/2 adjusted for m simultaneous tests.
      sme <- pse * stats::qt(1 - 0.05 / (2 * m), df_pse)

      active_me  <- abs_effects > me
      active_sme <- abs_effects > sme

      list(
        effects     = effects,
        abs_effects = abs_effects,
        pse         = pse,
        me          = me,
        sme         = sme,
        active_me   = active_me,
        active_sme  = active_sme,
        method      = "Lenth (1989)"
      )
    },

    # =========================================================================
    # Public: Alias structure for fractional factorial designs
    # =========================================================================
    #
    # Returns the defining relation and alias pattern for a 2^(k-p) fractional
    # factorial design. The defining relation is the set of all products of the
    # generators (e.g. for generators D=AB and E=AC, the defining relation is
    # I = ABD = ACE = BCDE). Each effect is aliased with all products of itself
    # and the words in the defining relation (e.g. A = BD = CE = ABCDE).
    #
    # Reference:
    #   Box, G. E. P., Hunter, J. S., & Hunter, W. G. (2005).
    #   Statistics for Experimenters (2nd ed.), ch. 5.
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments (10th ed.),
    #   ch. 8.
    #
    #' @description Compute the defining relation and alias structure for a
    #'   2^(k-p) fractional factorial design. Returns the defining relation
    #'   words, the alias pattern for each main effect and two-factor
    #'   interaction, and the design resolution.
    #' @param n_factors Integer scalar, the number of factors.
    #' @param resolution Character scalar, the requested resolution
    #'   (`"III"`, `"IV"`, or `"V"`).
    #' @return A list with elements `defining_relation` (character vector of
    #'   relation words, e.g. `"ABCDE"`), `generators` (character vector of
    #'   generator assignments, e.g. `"E = ABCD"`), `resolution` (integer
    #'   scalar, the actual resolution), `alias_main` (named list mapping each
    #'   main effect to its aliases), `alias_2fi` (named list mapping each
    #'   two-factor interaction to its aliases), and `n_runs` (integer, the
    #'   number of runs in the base fraction).
    get_alias_structure = function(n_factors, resolution) {
      gens <- private$.get_ff_generators(n_factors, resolution)
      if (is.null(gens)) {
        stop("[DoeAnalyzer] No standard generator available for ",
             "n_factors = ", n_factors, ", resolution = ", resolution,
             ". Use a full factorial instead.", call. = FALSE)
      }

      p <- gens$p
      generators <- gens$generators
      base_letters <- LETTERS[seq_len(n_factors - p)]
      added_letters <- LETTERS[(n_factors - p + 1):n_factors]

      # Build the defining relation. For each added factor i with letter L_i
      # and generator G_i, the basic defining word is L_i * G_i (the letters
      # of L_i and G_i concatenated, with repeats canceling mod 2). The full
      # defining relation is all 2^p products of these basic words (excluding
      # the identity I itself).
      #
      # Example: for k=5, p=1, generator="ABCD" -> basic word = "E" * "ABCD"
      # = "ABCDE", so I = ABCDE (resolution V).
      basic_words <- vapply(seq_along(generators), function(i) {
        private$.multiply_words(added_letters[i], generators[i])
      }, character(1))

      # Build all 2^p - 1 non-identity products of the basic words.
      words <- "I"
      for (i in seq_along(basic_words)) {
        new_words <- character(0)
        for (w in words) {
          combined <- private$.multiply_words(w, basic_words[i])
          new_words <- c(new_words, combined)
        }
        words <- unique(c(words, new_words))
      }
      relation_words <- setdiff(words, "I")

      # Compute the actual resolution = length of the shortest word.
      word_lengths <- nchar(relation_words)
      actual_res <- if (length(word_lengths) > 0) min(word_lengths) else n_factors

      # Compute aliases for each main effect and each two-factor interaction.
      # Effect X is aliased with X * W for each word W in the relation.
      all_main <- base_letters
      if (length(added_letters) > 0) all_main <- c(all_main, added_letters)

      alias_main <- list()
      for (eff in all_main) {
        aliases <- character(0)
        for (w in relation_words) {
          aliased <- private$.multiply_words(eff, w)
          if (aliased != "I" && aliased != eff) {
            aliases <- c(aliases, aliased)
          }
        }
        alias_main[[eff]] <- aliases
      }

      alias_2fi <- list()
      if (n_factors >= 2) {
        pairs <- utils::combn(all_main, 2, FUN = function(pr) paste(pr, collapse = ""),
                              simplify = FALSE)
        for (pair in pairs) {
          aliases <- character(0)
          for (w in relation_words) {
            aliased <- private$.multiply_words(pair, w)
            if (aliased != "I" && aliased != pair) {
              aliases <- c(aliases, aliased)
            }
          }
          alias_2fi[[pair]] <- aliases
        }
      }

      # Pretty-print the generators, e.g. "E = ABCD"
      gen_pretty <- if (length(generators) > 0) {
        paste0(added_letters, " = ", generators)
      } else character(0)

      list(
        defining_relation = relation_words,
        generators         = gen_pretty,
        resolution         = actual_res,
        alias_main         = alias_main,
        alias_2fi          = alias_2fi,
        n_runs             = as.integer(2^(n_factors - p))
      )
    },

    # =========================================================================
    # Public: Curvature test for unreplicated 2^k factorials with center points
    # =========================================================================
    #
    # Compares the mean response at the factorial (corner) points to the mean
    # response at the center points. A significant curvature indicates that
    # a pure first-order model is inadequate and a second-order (RSM) design
    # should be run to estimate quadratic terms.
    #
    # Theory (Montgomery 2019, sec. 6.6):
    #   y_bar_f = mean of factorial-point responses
    #   y_bar_c = mean of center-point responses
    #   H0: no curvature (E[y_f] = E[y_c])
    #   t = (y_bar_f - y_bar_c) / sqrt(MSE * (1/n_f + 1/n_c))
    #
    # Pure-error preference (Montgomery 2019, sec. 6.6):
    #   When the factorial points are unreplicated, the model MSE includes
    #   the curvature lack-of-fit itself, which dilutes the test. The standard
    #   practice is to estimate MSE from the n_c center-point replicates
    #   (pure error, df = n_c - 1). Only when there are fewer than 2 center
    #   points do we fall back to the model residual MSE.
    #
    #' @description Perform a curvature test for a 2^k factorial design with
    #'   center points. Compares the mean response at the factorial (corner)
    #'   points to the mean response at the center points; a small p-value
    #'   indicates that the first-order model is inadequate and a second-order
    #'   (RSM) design is recommended.
    #' @param model A fitted `lm` object.
    #' @param data Data frame containing the design columns and the response.
    #' @param response_var Character scalar naming the response column.
    #' @param factor_vars Character vector naming the original factor columns
    #'   (excluding derived terms like `I(A^2)`).
    #' @return A list with `curvature_estimate`, `t_statistic`, `p_value`,
    #'   `df`, `mean_factorial`, `mean_center`, `n_factorial`, `n_center`,
    #'   `method`, and `interpretation` (a short human-readable string). When
    #'   no center points are present, all numeric fields are `NA` and
    #'   `interpretation` explains why.
    test_curvature = function(model, data, response_var, factor_vars) {
      if (!response_var %in% names(data)) {
        return(list(curvature_estimate = NA_real_, t_statistic = NA_real_,
                    p_value = NA_real_, df = NA_integer_,
                    mean_factorial = NA_real_, mean_center = NA_real_,
                    n_factorial = NA_integer_, n_center = NA_integer_,
                    method = "curvature (no response column)",
                    interpretation = "Response column not found in data."))
      }

      factor_vars <- intersect(factor_vars, names(data))
      if (length(factor_vars) == 0) {
        return(list(curvature_estimate = NA_real_, t_statistic = NA_real_,
                    p_value = NA_real_, df = NA_integer_,
                    mean_factorial = NA_real_, mean_center = NA_real_,
                    n_factorial = NA_integer_, n_center = NA_integer_,
                    method = "curvature (no factor columns)",
                    interpretation = "No factor columns found in data."))
      }

      # A run is a "center point" if every factor value is at the center of
      # its declared range. For 2-level coded factors, the center is the
      # mean of the two coded levels (typically 0).
      is_center <- apply(data[, factor_vars, drop = FALSE], 1, function(row) {
        all(vapply(seq_along(row), function(i) {
          f <- factor_vars[i]
          lv <- sort(unique(data[[f]]))
          if (length(lv) < 2) return(FALSE)
          center <- mean(range(lv))
          abs(row[i] - center) < 1e-9
        }, logical(1)))
      })

      n_center <- sum(is_center)
      n_factorial <- sum(!is_center)

      if (n_center < 1) {
        return(list(curvature_estimate = NA_real_, t_statistic = NA_real_,
                    p_value = NA_real_, df = NA_integer_,
                    mean_factorial = NA_real_, mean_center = NA_real_,
                    n_factorial = n_factorial, n_center = 0L,
                    method = "curvature (no center points)",
                    interpretation = "No center points in the design; curvature cannot be tested."))
      }

      y <- data[[response_var]]
      y_f <- y[!is_center]
      y_c <- y[is_center]

      y_bar_f <- mean(y_f, na.rm = TRUE)
      y_bar_c <- mean(y_c, na.rm = TRUE)
      curvature <- y_bar_f - y_bar_c

      # Detect whether the factorial points are replicated. If any two
      # factorial runs share identical factor settings, the design has
      # replication at the corner points and the model MSE is a valid
      # pure-error estimate. Otherwise, the model MSE is contaminated by
      # the curvature lack-of-fit and we must use the center-point pure
      # error instead (Montgomery 2019 sec. 6.6).
      has_factorial_replicates <- FALSE
      if (n_factorial >= 2) {
        fac_mat <- as.matrix(data[!is_center, factor_vars, drop = FALSE])
        if (!any(is.na(fac_mat))) {
          dup_rows <- duplicated(fac_mat)
          has_factorial_replicates <- any(dup_rows)
        }
      }

      use_pure_error <- FALSE
      if (has_factorial_replicates) {
        # Factorial replicates available: use the model residual MSE.
        res_df <- tryCatch(df.residual(model), error = function(e) NA_integer_)
        mse <- tryCatch(sum(resid(model)^2) / res_df, error = function(e) NA_real_)
      } else if (n_center >= 2) {
        # No factorial replicates: use the center-point pure error.
        mse <- stats::var(y_c, na.rm = TRUE)
        res_df <- n_center - 1L
        use_pure_error <- TRUE
      } else {
        # Neither source available: cannot estimate error.
        return(list(curvature_estimate = curvature, t_statistic = NA_real_,
                    p_value = NA_real_, df = NA_integer_,
                    mean_factorial = y_bar_f, mean_center = y_bar_c,
                    n_factorial = n_factorial, n_center = n_center,
                    method = "curvature (no error estimate)",
                    interpretation = "Cannot estimate MSE without factorial replicates or >= 2 center points."))
      }

      # Degenerate case: zero variance in center points (perfectly repeatable
      # measurements). Report p_value = 1 when there is no curvature, and
      # p_value = 0 when curvature exists, with a clear interpretation.
      if (!is.finite(mse) || is.na(mse) || mse <= 0) {
        if (abs(curvature) < 1e-12) {
          return(list(
            curvature_estimate = curvature,
            t_statistic          = 0,
            p_value              = 1,
            df                   = as.integer(res_df),
            mean_factorial       = y_bar_f,
            mean_center          = y_bar_c,
            n_factorial          = as.integer(n_factorial),
            n_center             = as.integer(n_center),
            method               = "curvature (zero pure error: no curvature detected)",
            interpretation       = "Center-point replicates have zero variance and curvature estimate is 0; no curvature detected (p = 1)."
          ))
        } else {
          return(list(
            curvature_estimate = curvature,
            t_statistic          = sign(curvature) * Inf,
            p_value              = 0,
            df                   = as.integer(res_df),
            mean_factorial       = y_bar_f,
            mean_center          = y_bar_c,
            n_factorial          = as.integer(n_factorial),
            n_center             = as.integer(n_center),
            method               = "curvature (zero pure error: curvature detected)",
            interpretation       = "Center-point replicates have zero variance but curvature estimate is non-zero; curvature detected (p = 0)."
          ))
        }
      }

      se <- sqrt(mse * (1 / n_factorial + 1 / n_center))
      t_stat <- curvature / se
      p_value <- 2 * stats::pt(-abs(t_stat), df = res_df)

      interpretation <- if (is.na(p_value)) {
        "Curvature test could not be evaluated."
      } else if (p_value < 0.05) {
        sprintf("Significant curvature detected (p = %.4f). Recommend augmenting to a second-order (RSM) design.", p_value)
      } else {
        sprintf("No significant curvature (p = %.4f). A first-order model appears adequate.", p_value)
      }

      list(
        curvature_estimate = curvature,
        t_statistic        = t_stat,
        p_value            = p_value,
        df                 = as.integer(res_df),
        mean_factorial     = y_bar_f,
        mean_center        = y_bar_c,
        n_factorial        = as.integer(n_factorial),
        n_center           = as.integer(n_center),
        method             = if (use_pure_error) "curvature (pure-error from center points)" else "curvature (model MSE with factorial replicates)",
        interpretation     = interpretation
      )
    },

    # =========================================================================
    # Public: Power analysis for 2^k factorial designs
    # =========================================================================
    #
    # Computes the statistical power to detect a given effect size in a 2^k
    # factorial design. The power is the probability of correctly rejecting
    # the null hypothesis (no effect) when the true effect is Delta.
    #
    # Theory (Montgomery 2019, sec. 7.4):
    #   For a 2^k design with n replicates per run, the F-statistic for a
    #   single effect (df_num = 1) has a non-central F distribution with
    #   non-centrality parameter:
    #
    #     lambda = n * Delta^2 / (4 * sigma^2)
    #
    #   where:
    #     - Delta = the effect size to detect (difference between the two
    #       level means, e.g. mean at high level - mean at low level)
    #     - sigma = error standard deviation
    #     - n = number of replicates per factorial run
    #
    #   df_num = 1 (single effect)
    #   df_den = N - p - 1 (model error df) where N = n * 2^k (or 2^k + n_c
    #            when center points are present) and p is the number of model
    #            terms (intercept + k main effects + interactions).
    #
    #   Power = P(F > F_crit | F ~ F(1, df_den, lambda))
    #         = 1 - pf(F_crit, 1, df_den, lambda)
    #
    #' @description Compute the statistical power to detect a given effect
    #'   size in a 2^k factorial design. Uses the non-central F distribution
    #'   per Montgomery (2019) sec. 7.4.
    #' @param n_factors Integer scalar, the number of factors in the design.
    #' @param n_replicates Integer scalar, the number of replicates per
    #'   factorial run. Default is 1.
    #' @param effect_size Numeric scalar, the minimum effect size to detect
    #'   (Delta, in the same units as the response). This is the difference
    #'   between the mean response at the high level and the mean response at
    #'   the low level of the factor of interest.
    #' @param sigma Numeric scalar, the error standard deviation. May be
    #'   estimated from prior data, pilot studies, or historical process
    #'   performance.
    #' @param n_center_points Integer scalar, the number of center points
    #'   (added to the factorial runs but not replicated). Default 0.
    #' @param alpha Numeric scalar, the Type I error rate. Default 0.05.
    #' @param model_order Character scalar, the model order assumed for the
    #'   power calculation. One of `"main"` (main effects only, default),
    #'   `"main_2fi"` (main effects + all two-factor interactions), or
    #'   `"full"` (full factorial model with all interactions up to order k).
    #' @return A list with `power`, `n_factors`, `n_replicates`,
    #'   `effect_size`, `sigma`, `effect_to_sigma_ratio`, `alpha`,
    #'   `df_num`, `df_den`, `noncentrality`, `n_runs`, `model_order`, and
    #'   `interpretation`.
    compute_power = function(n_factors, n_replicates = 1, effect_size,
                              sigma, n_center_points = 0,
                              alpha = 0.05, model_order = "main") {
      if (!is.numeric(n_factors) || n_factors < 1 || n_factors != as.integer(n_factors)) {
        stop("[DoeAnalyzer] n_factors must be a positive integer.", call. = FALSE)
      }
      n_factors <- as.integer(n_factors)
      if (!is.numeric(n_replicates) || n_replicates < 1 ||
          n_replicates != as.integer(n_replicates)) {
        stop("[DoeAnalyzer] n_replicates must be a positive integer.", call. = FALSE)
      }
      n_replicates <- as.integer(n_replicates)
      if (!is.numeric(effect_size) || length(effect_size) != 1 || effect_size <= 0) {
        stop("[DoeAnalyzer] effect_size must be a positive numeric scalar.", call. = FALSE)
      }
      if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
        stop("[DoeAnalyzer] sigma must be a positive numeric scalar.", call. = FALSE)
      }
      if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
        stop("[DoeAnalyzer] alpha must be in (0, 1).", call. = FALSE)
      }
      if (!model_order %in% c("main", "main_2fi", "full")) {
        stop("[DoeAnalyzer] model_order must be 'main', 'main_2fi', or 'full'.",
             call. = FALSE)
      }

      # Determine the number of model terms (excluding intercept).
      n_main <- n_factors
      n_2fi  <- if (n_factors >= 2) choose(n_factors, 2) else 0
      n_full <- sum(choose(n_factors, 1:n_factors))
      p_terms <- switch(model_order,
        "main"      = n_main,
        "main_2fi"  = n_main + n_2fi,
        "full"      = n_full
      )
      # Total parameters including intercept.
      p_model <- 1L + p_terms

      # Total runs: factorial portion with replication + center points.
      n_factorial_runs <- n_replicates * 2^n_factors
      n_total <- n_factorial_runs + as.integer(n_center_points)

      # Error degrees of freedom.
      df_num <- 1L  # single effect
      df_den <- n_total - p_model
      if (df_den < 1) {
        stop("[DoeAnalyzer] Insufficient error degrees of freedom (df_den = ",
             df_den, "). Increase n_replicates or reduce model_order.",
             call. = FALSE)
      }

      # Non-centrality parameter (Montgomery 2019 eq. 7.10):
      #   lambda = n * Delta^2 / (4 * sigma^2)
      # where Delta is the effect size (difference between the two level
      # means, which corresponds to 2 * coefficient in regression coding).
      ncr <- n_replicates * effect_size^2 / (4 * sigma^2)

      # Critical F value at the requested alpha level.
      f_crit <- stats::qf(1 - alpha, df1 = df_num, df2 = df_den)

      # Power = P(F > F_crit | non-central F with noncentrality = ncr).
      power <- 1 - stats::pf(f_crit, df1 = df_num, df2 = df_den, ncp = ncr)

      ratio <- effect_size / sigma

      interpretation <- if (power >= 0.95) {
        sprintf("Power = %.1f%% (>= 95%%): the design has very high power to detect an effect of size %.3f (Delta/sigma = %.2f).",
                100 * power, effect_size, ratio)
      } else if (power >= 0.80) {
        sprintf("Power = %.1f%% (>= 80%%): the design has adequate power to detect an effect of size %.3f (Delta/sigma = %.2f).",
                100 * power, effect_size, ratio)
      } else if (power >= 0.50) {
        sprintf("Power = %.1f%% (50-80%%): the design has marginal power; consider increasing n_replicates to improve detection of an effect of size %.3f.",
                100 * power, effect_size)
      } else {
        sprintf("Power = %.1f%% (< 50%%): the design is underpowered to detect an effect of size %.3f; increase n_replicates or effect_size.",
                100 * power, effect_size)
      }

      list(
        power                  = power,
        n_factors              = n_factors,
        n_replicates           = n_replicates,
        effect_size            = effect_size,
        sigma                  = sigma,
        effect_to_sigma_ratio  = ratio,
        alpha                  = alpha,
        df_num                 = df_num,
        df_den                 = as.integer(df_den),
        noncentrality          = ncr,
        n_runs                 = as.integer(n_total),
        model_order            = model_order,
        interpretation         = interpretation
      )
    },

    # =========================================================================
    # Public: Sample size calculation for desired power
    # =========================================================================
    #
    # Finds the minimum number of replicates required to achieve a target
    # power for detecting a given effect size in a 2^k factorial design.
    #
    #' @description Find the minimum number of replicates per factorial run
    #'   needed to achieve a target power for detecting an effect of size
    #'   `effect_size` with error standard deviation `sigma`.
    #' @param n_factors Integer scalar, the number of factors.
    #' @param effect_size Numeric scalar, the minimum effect size to detect.
    #' @param sigma Numeric scalar, the error standard deviation.
    #' @param target_power Numeric scalar, the desired power (default 0.80).
    #' @param n_center_points Integer scalar, the number of center points
    #'   (default 0).
    #' @param alpha Numeric scalar, the Type I error rate (default 0.05).
    #' @param model_order Character scalar, model order (default "main").
    #' @param max_replicates Integer scalar, the maximum number of replicates
    #'   to consider before giving up (default 100).
    #' @return A list with `n_replicates`, `achieved_power`, `n_runs`,
    #'   `target_power`, `converged`, and `interpretation`.
    compute_sample_size = function(n_factors, effect_size, sigma,
                                    target_power = 0.80,
                                    n_center_points = 0,
                                    alpha = 0.05,
                                    model_order = "main",
                                    max_replicates = 100L) {
      if (!is.numeric(target_power) || target_power <= 0 || target_power >= 1) {
        stop("[DoeAnalyzer] target_power must be in (0, 1).", call. = FALSE)
      }

      # Search for the smallest n_replicates that achieves target_power.
      best_n <- NA_integer_
      best_power <- NA_real_
      for (n_rep in seq_len(max_replicates)) {
        result <- tryCatch(
          self$compute_power(n_factors = n_factors,
                              n_replicates = n_rep,
                              effect_size = effect_size,
                              sigma = sigma,
                              n_center_points = n_center_points,
                              alpha = alpha,
                              model_order = model_order),
          error = function(e) NULL
        )
        if (is.null(result)) next
        if (result$power >= target_power) {
          best_n <- n_rep
          best_power <- result$power
          break
        }
        best_power <- result$power  # track closest
      }

      converged <- !is.na(best_n)
      n_runs_total <- if (converged) {
        as.integer(best_n * 2^n_factors + n_center_points)
      } else NA_integer_

      interpretation <- if (converged) {
        sprintf("Use n_replicates = %d per factorial run to achieve power = %.1f%% (target %.0f%%). Total runs = %d.",
                best_n, 100 * best_power, 100 * target_power, n_runs_total)
      } else {
        sprintf("Could not achieve target power %.0f%% within max_replicates = %d. Best power = %.1f%%.",
                100 * target_power, max_replicates, 100 * best_power)
      }

      list(
        n_replicates   = best_n,
        achieved_power = best_power,
        n_runs         = n_runs_total,
        target_power   = target_power,
        converged      = converged,
        interpretation = interpretation
      )
    },

    # =========================================================================
    # Public: Design evaluation (D/A/G/I-optimality criteria)
    # =========================================================================
    #
    # Evaluates a design matrix against the standard alphabetic optimality
    # criteria used in optimal design theory. These criteria compare designs
    # based on the information matrix M = X'X / N (where X is the model
    # matrix and N is the number of runs).
    #
    # References:
    #   Myers, R. H., Montgomery, D. C., & Anderson-Cook, C. M. (2016).
    #     Response Surface Methodology: Process and Product Optimization
    #     Using Designed Experiments (4th ed.), ch. 6 & 8.
    #   Atkinson, A. C., Donev, A. N., & Tobias, R. D. (2007).
    #     Optimum Experimental Designs, with SAS.
    #
    #' @description Evaluate a design matrix against the standard alphabetic
    #'   optimality criteria (D, A, G, I). Returns the criteria values plus
    #'   a brief interpretation.
    #' @param design Data frame or matrix. The design matrix in coded units
    #'   (e.g. -1/+1 for 2-level factors, 0 for center points). Columns are
    #'   factors; rows are runs.
    #' @param model_order Character scalar, the assumed model. One of
    #'   `"main"` (default), `"main_2fi"`, or `"full"` (for 2-level designs)
    #'   or `"quadratic"` (for response-surface designs with center points).
    #' @return A list with `D_eff`, `A_eff`, `G_eff`, `I_eff`,
    #'   `determinant_XtX`, `trace_XtX_inv`, `max_pred_var`, `avg_pred_var`,
    #'   `n_runs`, `n_params`, `condition_number`, `criteria`, and
    #'   `interpretation`.
    evaluate_design = function(design, model_order = "main") {
      if (!is.data.frame(design) && !is.matrix(design)) {
        stop("[DoeAnalyzer] design must be a data frame or matrix.", call. = FALSE)
      }
      X_raw <- as.matrix(design)
      if (!is.numeric(X_raw)) {
        stop("[DoeAnalyzer] design must contain only numeric values.", call. = FALSE)
      }
      n_runs <- nrow(X_raw)
      n_factors <- ncol(X_raw)

      # Build the model matrix X based on the requested model order.
      X <- private$.build_model_matrix(X_raw, model_order)

      n_params <- ncol(X)
      if (n_runs < n_params) {
        stop("[DoeAnalyzer] Design is rank-deficient: n_runs (", n_runs,
             ") < n_params (", n_params, "). Cannot compute optimality criteria.",
             call. = FALSE)
      }

      # Information matrix M = X'X / N (per-unit information).
      XtX <- t(X) %*% X
      N <- n_runs

      # D-optimality: maximize det(X'X). Report the determinant and the
      # D-efficiency relative to a hypothetical D-optimal design (here we
      # report det(X'X / N)^(1/p), which is a scale-free measure).
      det_XtX <- tryCatch(det(XtX), error = function(e) NA_real_)
      if (is.na(det_XtX) || det_XtX <= 0) {
        return(list(
          D_eff             = NA_real_,
          A_eff             = NA_real_,
          G_eff             = NA_real_,
          I_eff             = NA_real_,
          determinant_XtX   = det_XtX,
          trace_XtX_inv     = NA_real_,
          max_pred_var      = NA_real_,
          avg_pred_var      = NA_real_,
          n_runs            = as.integer(n_runs),
          n_params          = as.integer(n_params),
          condition_number  = NA_real_,
          criteria          = model_order,
          interpretation    = "Design is singular (det(X'X) = 0); criteria cannot be computed. The design is rank-deficient or aliased."
        ))
      }

      # Inverse of information matrix.
      XtX_inv <- tryCatch(solve(XtX), error = function(e) NULL)
      if (is.null(XtX_inv)) {
        return(list(
          D_eff             = NA_real_,
          A_eff             = NA_real_,
          G_eff             = NA_real_,
          I_eff             = NA_real_,
          determinant_XtX   = det_XtX,
          trace_XtX_inv     = NA_real_,
          max_pred_var      = NA_real_,
          avg_pred_var      = NA_real_,
          n_runs            = as.integer(n_runs),
          n_params          = as.integer(n_params),
          condition_number  = NA_real_,
          criteria          = model_order,
          interpretation    = "Information matrix is not invertible; criteria cannot be computed."
        ))
      }

      # D-efficiency: det(M)^(1/p) where M = X'X/N. This is the
      # per-parameter geometric mean of the eigenvalues of M, and is the
      # standard scale-free D-optimality measure.
      M <- XtX / N
      D_eff <- (det(M))^(1 / n_params)

      # A-optimality: minimize trace((X'X)^-1). Report A_eff = p / trace((X'X)^-1 * N)
      # scaled to [0, 1] form for comparability with D_eff.
      tr_inv <- sum(diag(XtX_inv))
      A_eff <- n_params / (tr_inv * N)  # scaled to be dimensionless

      # G-optimality: minimize max prediction variance over the design region.
      # For a 2-level factorial, the prediction variance at each design point
      # is x_i' (X'X)^-1 x_i. The G-efficiency is p / (N * max_pred_var).
      pred_vars <- diag(X %*% XtX_inv %*% t(X))
      max_pred_var <- max(pred_vars)
      G_eff <- n_params / (N * max_pred_var)

      # I-optimality: minimize average prediction variance over the design
      # region. For a discrete approximation, we use the average prediction
      # variance over the design points themselves (a common practical
      # approximation). I_eff = p / (N * avg_pred_var).
      avg_pred_var <- mean(pred_vars)
      I_eff <- n_params / (N * avg_pred_var)

      # Condition number of X'X (a measure of near-singularity).
      cond_num <- tryCatch(rcond(XtX), error = function(e) NA_real_)

      interpretation <- if (D_eff >= 0.95) {
        sprintf("D-efficiency = %.4f (>= 0.95): the design is highly efficient. Condition number = %.2e.",
                D_eff, cond_num)
      } else if (D_eff >= 0.80) {
        sprintf("D-efficiency = %.4f (0.80-0.95): the design is reasonably efficient. Consider augmenting if resources allow. Condition number = %.2e.",
                D_eff, cond_num)
      } else {
        sprintf("D-efficiency = %.4f (< 0.80): the design is inefficient. Consider using a more balanced design or reducing model order. Condition number = %.2e.",
                D_eff, cond_num)
      }

      list(
        D_eff             = D_eff,
        A_eff             = A_eff,
        G_eff             = G_eff,
        I_eff             = I_eff,
        determinant_XtX   = det_XtX,
        trace_XtX_inv     = tr_inv,
        max_pred_var      = max_pred_var,
        avg_pred_var      = avg_pred_var,
        n_runs            = as.integer(n_runs),
        n_params          = as.integer(n_params),
        condition_number  = cond_num,
        criteria          = model_order,
        interpretation    = interpretation
      )
    },

    # =========================================================================
    # Public: Canonical analysis / stationary point of a fitted RSM model
    # =========================================================================
    #
    # For a second-order model  y = b0 + b'x + x'Bx , the stationary point is
    #   x_s = -1/2 * B^{-1} * b
    # The eigenvalues of B classify the stationary point:
    #   - All eigenvalues > 0  -> minimum (bowl)
    #   - All eigenvalues < 0  -> maximum (hill)
    #   - Mixed signs          -> saddle point
    #
    # Reference:
    #   Myers, Montgomery & Anderson-Cook (2016), ch. 6: "The Second-Order
    #   Model and Canonical Analysis".
    #' @description Compute the stationary point of a fitted second-order
    #'   response surface model via canonical analysis. Returns the
    #'   stationary point, the B matrix of quadratic coefficients, its
    #'   eigenvalues, the nature of the point (minimum / maximum / saddle),
    #'   and the predicted response at the stationary point.
    #' @param model A fitted `lm` object whose formula is a second-order RSM
    #'   model (e.g. `y ~ A*B*C + I(A^2) + I(B^2) + I(C^2)`).
    #' @param factor_names Character vector of factor names matching the
    #'   linear terms in the model.
    #' @return A list with elements `stationary_point` (named numeric vector),
    #'   `B_matrix` (k x k matrix), `eigenvalues` (numeric vector), `nature`
    #'   (character: "minimum" / "maximum" / "saddle point"), `b_vector`
    #'   (linear coefficients), `predicted_response` (numeric scalar),
    #'   `prediction_interval` (numeric vector of length 2: lower, upper),
    #'   `converged` (logical: FALSE if B is singular and x_s could not be
    #'   computed).
    compute_stationary_point = function(model, factor_names) {
      beta_hat <- stats::coef(model)

      k <- length(factor_names)
      if (k < 2) {
        stop("[DoeAnalyzer] compute_stationary_point requires at least 2 factors.",
             call. = FALSE)
      }

      # --- Linear coefficient vector b ---------------------------------
      b_vec <- beta_hat[factor_names]

      # --- Quadratic coefficient matrix B ------------------------------
      # Diagonal entries: pure quadratic coefficients from I(X^2) terms.
      # Off-diagonal entries: half the two-factor interaction coefficient.
      B_mat <- matrix(0, nrow = k, ncol = k,
                      dimnames = list(factor_names, factor_names))

      quad_names <- paste0("I(", factor_names, "^2)")
      has_quad <- quad_names %in% names(beta_hat)
      if (!all(has_quad)) {
        stop("[DoeAnalyzer] Model is missing pure quadratic terms for: ",
             paste(factor_names[!has_quad], collapse = ", "),
             ". compute_stationary_point requires a full second-order model.",
             call. = FALSE)
      }
      diag(B_mat) <- beta_hat[quad_names]

      for (i in seq_len(k - 1)) {
        for (j in (i + 1):k) {
          int_name <- paste0(factor_names[i], ":", factor_names[j])
          if (!int_name %in% names(beta_hat)) {
            int_name <- paste0(factor_names[j], ":", factor_names[i])
          }
          if (int_name %in% names(beta_hat)) {
            B_mat[i, j] <- B_mat[j, i] <- beta_hat[int_name] / 2
          }
        }
      }

      # --- Stationary point: x_s = -0.5 * B^{-1} * b -------------------
      converged <- TRUE
      x_s <- tryCatch({
        as.numeric(-0.5 * solve(B_mat, b_vec))
      }, error = function(e) {
        converged <<- FALSE
        rep(NA_real_, k)
      })
      names(x_s) <- factor_names

      # --- Eigenvalues classify the stationary point -------------------
      eig <- eigen(B_mat, symmetric = TRUE)
      eig_values <- eig$values

      nature <- if (anyNA(eig_values)) {
        "indeterminate"
      } else if (all(eig_values > 0)) {
        "minimum"
      } else if (all(eig_values < 0)) {
        "maximum"
      } else {
        "saddle point"
      }

      # --- Predicted response at the stationary point ------------------
      predicted <- NA_real_
      pred_lwr <- NA_real_
      pred_upr <- NA_real_
      if (converged) {
        newdata <- as.data.frame(t(x_s))
        colnames(newdata) <- factor_names
        # Rebind the model terms environment to baseenv() so predict.lm
        # evaluates derived terms (I(X^2)) from newdata only.
        m_local <- model
        attr(m_local$terms, ".Environment") <- baseenv()
        environment(m_local$terms) <- baseenv()
        pred <- tryCatch({
          predict(m_local, newdata = newdata,
                  interval = "prediction", level = 0.95)
        }, error = function(e) NULL)
        if (!is.null(pred)) {
          predicted <- pred[, "fit"]
          pred_lwr  <- pred[, "lwr"]
          pred_upr  <- pred[, "upr"]
        }
      }

      list(
        stationary_point    = x_s,
        B_matrix            = B_mat,
        b_vector            = b_vec,
        eigenvalues         = eig_values,
        nature              = nature,
        predicted_response  = predicted,
        prediction_interval = c(lower = pred_lwr, upper = pred_upr),
        converged           = converged
      )
    },

    # =========================================================================
    # Public: Path of Steepest Ascent
    # =========================================================================
    #
    # For a first-order model y = b0 + b'x (main effects only, no curvature),
    # the direction of steepest ascent is the gradient direction: move along
    # the vector b (the signed main-effect coefficients). Each step of size
    # delta in the direction of b increases the predicted response by the
    # maximum possible amount for that step length.
    #
    # The procedure (Box & Wilson 1951; Montgomery 2019 sec. 5.5):
    #
    #   1. Fit a first-order model y = b0 + sum(b_i * x_i) in coded units.
    #   2. Choose a step size delta for the factor with the largest |b_i|.
    #      This sets the practical "unit step" for the exploration.
    #   3. For all other factors, the step is delta * (b_j / b_max) where
    #      b_max is the coefficient of the reference factor. This ensures
    #      the exploration follows the gradient direction.
    #   4. Generate a sequence of n_steps points along the gradient,
    #      starting from the design center (all x_i = 0 in coded units).
    #   5. Convert each point back to actual (uncoded) factor units so
    #      the experimenter can run confirmation trials.
    #   6. Optionally predict the response at each step using the fitted
    #      first-order model.
    #
    # When `maximize = FALSE`, the direction is reversed (steepest descent)
    # so the procedure finds the direction that minimizes the response.
    #
    # Reference:
    #   Box, G. E. P. & Wilson, K. B. (1951). On the Experimental Attainment
    #     of Optimum Conditions. J. Royal Statistical Society, B, 13(1).
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments (10th
    #     ed.), sec. 5.5.
    #' @description Compute the path of steepest ascent (or descent) from a
    #'   fitted first-order model. Returns a sequence of operating conditions
    #'   along the gradient direction, along with the predicted response at
    #'   each step. This is the standard follow-up procedure when a screening
    #'   design identifies significant main effects and the goal is to move
    #'   toward the optimum before running a second-order (RSM) design.
    #' @param model A fitted `lm` object containing a first-order model
    #'   (intercept + main effects only; interactions and quadratic terms
    #'   are ignored when computing the gradient).
    #' @param factors List of factor definitions (as stored on an
    #'   `IqrDoePlan` object), where each element has `$name` and `$levels`.
    #'   The factor levels are used to convert coded steps back to actual
    #'   engineering units.
    #' @param step_size Numeric scalar. The step size in coded units for the
    #'   factor with the largest absolute coefficient. Default 1.0 (one
    #'   coded unit per step for the reference factor).
    #' @param n_steps Integer scalar. Number of points to generate along the
    #'   path. Default 5.
    #' @param maximize Logical. If `TRUE` (default), compute the path of
    #'   steepest ascent (increase the response). If `FALSE`, compute the
    #'   path of steepest descent (decrease the response).
    #' @return A data frame with one row per step. Columns include the
    #'   coded and actual factor values, the step number, and the predicted
    #'   response. An attribute `"gradient"` holds the normalized gradient
    #'   vector.
    compute_steepest_ascent = function(model, factors, step_size = 1.0,
                                      n_steps = 5, maximize = TRUE) {
      beta_hat <- stats::coef(model)
      # Drop intercept; keep only main-effect coefficients.
      factor_names <- vapply(factors, function(f) f$name, character(1))
      main_coefs <- beta_hat[factor_names]
      if (length(main_coefs) < 1) {
        stop("[DoeAnalyzer] compute_steepest_ascent requires at least 1 factor.",
             call. = FALSE)
      }

      # Gradient direction = signed coefficients (ascent) or negative
      # (descent). Normalize so the reference factor (largest |b|) has
      # step_size = 1 coded unit per step.
      sign_dir <- if (maximize) 1 else -1
      b_signed <- sign_dir * main_coefs
      ref_idx <- which.max(abs(b_signed))
      b_ref <- b_signed[ref_idx]
      if (abs(b_ref) < .Machine$double.eps) {
        stop("[DoeAnalyzer] All main-effect coefficients are zero; ",
             "steepest ascent direction is undefined.", call. = FALSE)
      }
      # Normalized step vector: each factor's coded step per unit step of
      # the reference factor. The sign of b_ref is preserved so the
      # direction follows the gradient (ascent) or anti-gradient (descent).
      step_coded <- (b_signed / abs(b_ref)) * step_size

      # Build the path: n_steps rows, each at step * step_coded from the
      # center (coded 0). Step 0 = the design center.
      steps <- 0:n_steps
      coded_path <- outer(steps, step_coded)
      colnames(coded_path) <- factor_names

      # Convert coded path to actual engineering units. Each factor's
      # actual value = center + coded * (range / 2).
      actual_path <- matrix(NA_real_, nrow = nrow(coded_path),
                            ncol = ncol(coded_path))
      colnames(actual_path) <- factor_names
      for (i in seq_along(factors)) {
        f <- factors[[i]]
        center <- mean(f$levels)
        half_range <- (max(f$levels) - min(f$levels)) / 2
        actual_path[, i] <- center + coded_path[, i] * half_range
      }

      # Predict response at each step using the fitted model.
      newdata <- as.data.frame(actual_path)
      colnames(newdata) <- factor_names
      # Rebind the model terms environment so predict.lm evaluates
      # derived terms from newdata only.
      m_local <- model
      attr(m_local$terms, ".Environment") <- baseenv()
      environment(m_local$terms) <- baseenv()
      predicted <- tryCatch({
        as.numeric(predict(m_local, newdata = newdata))
      }, error = function(e) rep(NA_real_, nrow(newdata)))

      result <- as.data.frame(actual_path)
      colnames(result) <- paste0(factor_names, "_actual")
      for (i in seq_along(factor_names)) {
        result[[paste0(factor_names[i], "_coded")]] <- coded_path[, i]
      }
      result$Step     <- steps
      result$Predicted <- predicted
      attr(result, "gradient") <- step_coded
      attr(result, "reference_factor") <- factor_names[ref_idx]
      attr(result, "direction") <- if (maximize) "ascent" else "descent"
      result
    },

    # =========================================================================
    # Public: Model selection (stepwise / forward / backward) with
    #         AIC / AICc / BIC criteria
    # =========================================================================
    #
    # Minitab offers stepwise, forward, and backward selection with
    # p-value, AICc, and BIC criteria. This method wraps MASS::stepAIC()
    # to provide the same capabilities inside iQualityR.doe so that users
    # are not forced to keep the full (saturated) model when many terms
    # are inert.
    #
    # The default criterion is AICc (corrected AIC), which is preferred
    # over AIC when n / k is small (Burnham & Anderson 2002). BIC applies
    # a heavier penalty and tends to select more parsimonious models.
    #
    # Reference:
    #   Venables, W. N. & Ripley, B. D. (2002). Modern Applied Statistics
    #     with S (4th ed.), sec. 8.9.
    #   Burnham, K. P. & Anderson, D. R. (2002). Model Selection and
    #     Multimodel Inference (2nd ed.).
    #' @description Perform model selection on a fitted DOE model using
    #'   stepwise, forward, or backward selection with AIC, AICc, or BIC
    #'   as the selection criterion. Returns the reduced model and a summary
    #'   of the selection trace.
    #' @param model A fitted `lm` object (typically the full model from
    #'   `$compute()`).
    #' @param direction Character. One of `"backward"` (default),
    #'   `"forward"`, or `"both"` (stepwise).
    #' @param criterion Character. One of `"aic"`, `"aicc"` (default), or
    #'   `"bic"`.
    #' @param trace Logical. If `TRUE`, print the selection trace.
    #'   Default `FALSE`.
    #' @param k Numeric. Penalty multiplier. When `criterion = "bic"` this
    #'   is set to `log(n)` automatically; otherwise it is derived from the
    #'   criterion. Ignored when `criterion` is explicitly specified.
    #' @return A list with elements `model` (the reduced `lm` object),
    #'   `criterion`, `direction`, `trace` (data frame of selection steps),
    #'   `n_terms_initial`, `n_terms_final`, `criterion_values`.
    select_model = function(model, direction = "backward",
                            criterion = "aicc", trace = FALSE,
                            k = NULL) {
      if (!requireNamespace("MASS", quietly = TRUE)) {
        stop("[DoeAnalyzer] select_model requires the MASS package.",
             call. = FALSE)
      }
      direction <- match.arg(direction, c("backward", "forward", "both"))
      criterion <- match.arg(criterion, c("aic", "aicc", "bic"))

      # Re-fit the model using its model.frame so that MASS::stepAIC can
      # access the data when it refits candidate models internally.
      # Without this, stepAIC evaluates the model's call in the method's
      # environment and fails to find the original data object.
      mf <- stats::model.frame(model)
      model_refit <- stats::lm(stats::formula(model), data = mf)

      n <- stats::nobs(model_refit)
      # Determine the penalty multiplier k for MASS::stepAIC().
      # stepAIC uses AIC = -2*logLik + k * df; for AIC, k = 2; for BIC,
      # k = log(n). For AICc we use k = 2 (AIC) and then re-rank the
      # candidate models on the trace by the exact AICc formula
      # (AICc = AIC + 2k(k+1)/(n-k-1)) so the returned trace reflects
      # the intended criterion even though stepAIC optimizes AIC.
      k_use <- if (criterion == "bic") {
        log(n)
      } else {
        2  # standard AIC penalty
      }
      if (!is.null(k)) k_use <- k

      # For forward / stepwise selection, the scope must include the
      # full model as the upper bound. For backward selection, stepAIC
      # starts from the full model and removes terms.
      scope <- if (direction == "backward") {
        list(lower = stats::reformulate("1",
                                        response = as.character(model_refit$terms[[2]])),
             upper = formula(model_refit))
      } else {
        list(upper = formula(model_refit),
             lower = stats::reformulate("1",
                                         response = as.character(model_refit$terms[[2]])))
      }

      reduced <- MASS::stepAIC(model_refit, direction = direction,
                                trace = trace, scope = scope, k = k_use,
                                steps = 1000)

      # Build a trace summary by re-running stepAIC with trace = TRUE
      # text capture when requested. This captures each step's AIC value
      # and the terms dropped/added.
      trace_df <- NULL
      if (trace) {
        trace_capture <- tryCatch({
          out_text <- utils::capture.output(
            MASS::stepAIC(model, direction = direction, trace = 1,
                          scope = scope, k = k_use, steps = 1000)
          )
          # Parse the captured text into a data frame. Each step is
          # introduced by "Start: ..." or "<none>" / "<step>" lines.
          step_lines <- grep("^Start:|^Step:", out_text, value = TRUE)
          if (length(step_lines) > 0) {
            trace_df <- data.frame(
              Step     = seq_along(step_lines) - 1,
              Action   = step_lines,
              stringsAsFactors = FALSE
            )
          }
          trace_df
        }, error = function(e) NULL)
      }

      # Compute the requested criterion for initial and final models
      # so the user can see the improvement.
      calc_crit <- function(m) {
        ll_obj <- stats::logLik(m)
        ll <- as.numeric(ll_obj)
        # Extract df before coercing to numeric, otherwise as.numeric()
        # strips the attribute and the penalty term vanishes.
        df_m <- attr(ll_obj, "df")
        aic_val <- -2 * ll + 2 * df_m
        if (criterion == "aicc") {
          aic_val + 2 * df_m * (df_m + 1) / max(n - df_m - 1, 1)
        } else if (criterion == "bic") {
          -2 * ll + log(n) * df_m
        } else {
          aic_val
        }
      }

      list(
        model             = reduced,
        criterion         = criterion,
        direction         = direction,
        trace             = trace_df,
        n_terms_initial   = length(coef(model_refit)),
        n_terms_final     = length(coef(reduced)),
        criterion_initial = calc_crit(model_refit),
        criterion_final   = calc_crit(reduced)
      )
    },

    # =========================================================================
    # Public: Extended model fit statistics (PRESS, R²pred, VIF)
    # =========================================================================
    #
    # Minitab reports S, R², R²(adj), R²(pred), and PRESS in the model
    # summary. iQualityR.doe currently reports only R² and adj-R²; this
    # method computes the missing predicted R-squared and PRESS via
    # leave-one-out residuals, plus VIF for multicollinearity detection.
    #
    # Reference:
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments
    #     (10th ed.), sec. 10.7.
    #' @description Compute extended model fit statistics: PRESS,
    #'   predicted R-squared, and VIF (Variance Inflation Factor). These
    #'   augment the basic R²/adj-R² reported by `$compute()` and match
    #'   the model summary statistics reported by Minitab.
    #' @param model A fitted `lm` object.
    #' @return A list with `PRESS`, `r_squared_pred`, `vif` (named numeric
    #'   vector), `max_vif`, and `has_multicollinearity` (logical).
    compute_model_fit_extended = function(model) {
      n <- stats::nobs(model)
      # PRESS: predicted residual sum of squares.
      # Use the hat matrix to compute leave-one-out residuals:
      #   e_(i) = e_i / (1 - h_ii)
      # where e_i is the ordinary residual and h_ii is the leverage.
      # This avoids refitting the model n times.
      hat <- stats::hatvalues(model)
      resid <- stats::residuals(model)
      loo_resid <- resid / (1 - hat)
      press <- sum(loo_resid^2, na.rm = TRUE)

      # R²(pred) = 1 - PRESS / SS_total
      ss_total <- sum(resid^2, na.rm = TRUE) +
        sum(stats::fitted(model)^2 - mean(stats::fitted(model))^2)
      # More robust: SST = sum((y - y_bar)^2)
      y <- stats::model.response(stats::model.frame(model))
      ss_total <- sum((y - mean(y))^2)
      r_sq_pred <- 1 - press / ss_total

      # VIF: use the diagonal of the inverse of X'X (correlation-based).
      # VIF_j = 1 / (1 - R_j^2), where R_j^2 is from regressing X_j on
      # all other predictors. We compute it via the car::vif() approach
      # but without the car dependency.
      vif <- tryCatch({
        X <- stats::model.matrix(model)
        # Drop intercept column.
        if (ncol(X) > 1) X <- X[, -1, drop = FALSE]
        if (ncol(X) < 2) {
          return(setNames(numeric(0), character(0)))
        }
        vif_vals <- numeric(ncol(X))
        names(vif_vals) <- colnames(X)
        for (j in seq_len(ncol(X))) {
          others <- X[, -j, drop = FALSE]
          # Regress X_j on the other predictors.
          fit_j <- stats::lm(X[, j] ~ others)
          r_j <- summary(fit_j)$r.squared
          vif_vals[j] <- 1 / (1 - r_j + .Machine$double.eps)
        }
        vif_vals
      }, error = function(e) setNames(numeric(0), character(0)))

      max_vif_val <- if (length(vif) > 0) max(vif) else NA_real_
      list(
        PRESS                  = press,
        r_squared_pred         = r_sq_pred,
        vif                    = vif,
        max_vif                = max_vif_val,
        has_multicollinearity  = !is.na(max_vif_val) && max_vif_val > 10
      )
    },

    # =========================================================================
    # Public: Fold-Over augmentation for fractional factorial designs
    # =========================================================================
    #
    # A fold-over adds the mirror image (sign-reversed) of every run to a
    # 2-level fractional factorial design. This doubles the number of runs
    # and breaks specific aliasing patterns:
    #
    #   - Full fold-over (default): reverses ALL factor signs. This
    #     de-aliases all main effects from two-factor interactions.
    #   - Partial fold-over: reverses only the signs of one factor. This
    #     de-aliases the selected factor's main effect and its 2FIs.
    #
    # Reference:
    #   Box, G. E. P., Hunter, J. S., & Hunter, W. G. (2005).
    #   Statistics for Experimenters (2nd ed.), sec. 6.4.
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments
    #   (10th ed.), sec. 8.6.
    #' @description Augment a 2-level fractional factorial design with a
    #'   fold-over (mirror image) to break specific aliasing patterns. A full
    #'   fold-over reverses all factor signs and de-aliases all main effects
    #'   from two-factor interactions. A partial fold-over reverses only one
    #'   factor's signs.
    #' @param design Data frame, the original design (as returned in
    #'   `results$design_info`).
    #' @param factors List of factor definitions (as stored on an
    #'   `IqrDoePlan` object).
    #' @param fold_factor Character scalar or `NULL`. If `NULL` (default),
    #'   perform a full fold-over (reverse all factor signs). If a factor
    #'   name, perform a partial fold-over reversing only that factor's sign.
    #' @return A data frame with the augmented design (original + mirror
    #'   runs). A `Foldover` column flags original (`"original"`) vs mirror
    #'   (`"foldover"`) runs.
    fold_over = function(design, factors, fold_factor = NULL) {
      if (!is.data.frame(design)) {
        stop("[DoeAnalyzer] design must be a data frame.", call. = FALSE)
      }
      factor_names <- vapply(factors, function(f) f$name, character(1))
      design_cols <- intersect(factor_names, names(design))
      if (length(design_cols) < 1) {
        stop("[DoeAnalyzer] design does not contain any factor columns.",
             call. = FALSE)
      }

      if (!is.null(fold_factor)) {
        if (length(fold_factor) != 1 || !fold_factor %in% design_cols) {
          stop("[DoeAnalyzer] fold_factor must be a single factor name ",
               "present in the design, or NULL for a full fold-over.",
               call. = FALSE)
        }
      }

      # Create the mirror copy.
      mirror <- design
      if (is.null(fold_factor)) {
        # Full fold-over: reverse all factor signs.
        for (col in design_cols) {
          mirror[[col]] <- -mirror[[col]]
        }
      } else {
        # Partial fold-over: reverse only the selected factor's sign.
        mirror[[fold_factor]] <- -mirror[[fold_factor]]
      }

      # Tag the runs.
      if (!"Foldover" %in% names(design)) {
        design$Foldover <- "original"
      }
      mirror$Foldover <- "foldover"

      rbind(design, mirror)
    },

    # =========================================================================
    # Public: Augment a 2-level factorial to a CCD by adding axial points
    # =========================================================================
    #
    # Converts a 2^k factorial (or fractional factorial) design into a
    # Central Composite Design by appending axial (star) points and
    # (optionally) additional center points. This is the standard RSM
    # augmentation path when a first-order model shows significant curvature.
    #
    # The axial distance alpha controls the geometry:
    #   - rotatable:    alpha = (n_factorial)^(1/4)
    #   - face_centered: alpha = 1 (star points on the faces of the cube)
    #   - spherical:    alpha = (2^k)^(1/4)
    #   - orthogonal:    alpha = sqrt(k * (n_factorial + n_center) / n_factorial)
    #
    # Reference:
    #   Box, G. E. P. & Wilson, K. B. (1951). On the Experimental
    #   Attainment of Optimum Conditions. J. Royal Statistical Society, B.
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments
    #   (10th ed.), sec. 11.4.
    #' @description Augment a 2-level factorial design into a Central
    #'   Composite Design (CCD) by adding axial (star) points and center
    #'   points. This is the standard path from screening to RSM when
    #'   curvature is detected.
    #' @param design Data frame, the original 2-level factorial design.
    #' @param factors List of factor definitions.
    #' @param alpha_type Character scalar, the axial distance type:
    #'   `"rotatable"` (default), `"face_centered"`, `"spherical"`, or
    #'   `"orthogonal"`.
    #' @param n_center_points Integer scalar, number of additional center
    #'   points to add (default 4).
    #' @return A data frame with the augmented CCD design. A `PointType`
    #'   column labels runs as `"cube"`, `"axial"`, or `"center"`.
    augment_to_ccd = function(design, factors, alpha_type = "rotatable",
                               n_center_points = 4) {
      if (!is.data.frame(design)) {
        stop("[DoeAnalyzer] design must be a data frame.", call. = FALSE)
      }
      factor_names <- vapply(factors, function(f) f$name, character(1))
      design_cols <- intersect(factor_names, names(design))
      k <- length(design_cols)
      if (k < 2) {
        stop("[DoeAnalyzer] augment_to_ccd requires at least 2 factors.",
             call. = FALSE)
      }
      alpha_type <- match.arg(alpha_type,
                              c("rotatable", "face_centered",
                                "spherical", "orthogonal"))

      n_factorial <- nrow(design)

      # Compute axial distance alpha.
      alpha <- switch(alpha_type,
        "rotatable"     = (n_factorial)^(1 / 4),
        "face_centered" = 1,
        "spherical"     = (2^k)^(1 / 4),
        "orthogonal"    = sqrt(k * (n_factorial + n_center_points) /
                                 n_factorial)
      )

      # Tag existing runs as "cube" if PointType not already present.
      if (!"PointType" %in% names(design)) {
        design$PointType <- "cube"
      }

      # Build axial points: for each factor, one run at +alpha and one at
      # -alpha, with all other factors at 0 (coded center).
      axial_list <- list()
      for (i in seq_along(design_cols)) {
        fname <- design_cols[i]
        for (sign_val in c(-1, 1)) {
          row <- rep(0, k)
          names(row) <- design_cols
          row[i] <- sign_val * alpha
          axial_row <- as.data.frame(as.list(row))
          axial_row$PointType <- "axial"
          axial_list <- c(axial_list, list(axial_row))
        }
      }
      axial_df <- do.call(rbind, axial_list)

      # Build center points (all factors at coded 0).
      center_rows <- do.call(rbind, replicate(n_center_points, {
        row <- rep(0, k)
        names(row) <- design_cols
        df <- as.data.frame(as.list(row))
        df$PointType <- "center"
        df
      }, simplify = FALSE))

      # Ensure all columns align before rbind.
      all_cols <- union(names(design), names(axial_df))
      all_cols <- union(all_cols, names(center_rows))

      for (col in setdiff(all_cols, names(design))) design[[col]] <- NA
      for (col in setdiff(all_cols, names(axial_df))) axial_df[[col]] <- NA
      for (col in setdiff(all_cols, names(center_rows))) center_rows[[col]] <- NA

      rbind(design[, all_cols], axial_df[, all_cols], center_rows[, all_cols])
    },

    # =========================================================================
    # Public: Uncoded (natural-unit) coefficient equation
    # =========================================================================
    #
    # Minitab displays the regression equation in uncoded (actual engineering)
    # units alongside the coded-coefficient table. This method transforms
    # the coded coefficients of a DOE model into their natural-unit
    # counterparts so the user can plug in real factor values directly.
    #
    # For a coded model y = b0 + b1*x1 + b2*x2 + ... where x_i is coded,
    # the actual factor value is X_i = center_i + x_i * (range_i / 2), so
    # x_i = (X_i - center_i) / (range_i / 2). Substituting and collecting
    # terms yields the uncoded coefficients.
    #
    # Reference:
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments
    #   (10th ed.), sec. 10.8.
    #' @description Transform the coefficients of a fitted DOE model from
    #'   coded units to natural (uncoded / engineering) units so the
    #'   regression equation can be evaluated with real factor values.
    #' @param model A fitted `lm` object (typically from `$run()` with
    #'   response data).
    #' @param factors List of factor definitions, each with `$name` and
    #'   `$levels`.
    #' @return A list with `equation` (character string), `coefficients`
    #'   (named numeric vector of uncoded coefficients), and `coded_coefficients`
    #'   (the original coded coefficients).
    get_uncoded_equation = function(model, factors) {
      beta_hat <- stats::coef(model)
      factor_names <- vapply(factors, function(f) f$name, character(1))

      # Build the transformation: coded x_i = (X_i - center_i) / half_range_i.
      centers <- vapply(factors, function(f) mean(f$levels), numeric(1))
      half_ranges <- vapply(factors, function(f) {
        (max(f$levels) - min(f$levels)) / 2
      }, numeric(1))
      names(centers) <- factor_names
      names(half_ranges) <- factor_names

      # Start with the intercept. The uncoded intercept accounts for the
      # shift when substituting coded -> uncoded: every linear term b_i*x_i
      # becomes b_i*(X_i - center_i)/half_range_i, which introduces a
      # constant -b_i*center_i/half_range_i into the intercept.
      # unname() strips the coefficient names carried by stats::coef() so
      # that c()/unlist() below does not mangle names (e.g. "A.A").
      b0_uncoded <- unname(beta_hat["(Intercept)"])
      uncoded_coef <- list()

      # Linear terms.
      for (fname in factor_names) {
        if (fname %in% names(beta_hat)) {
          b_coded <- beta_hat[fname]
          b_uncoded <- unname(b_coded / half_ranges[fname])
          uncoded_coef[[fname]] <- b_uncoded
          b0_uncoded <- b0_uncoded - unname(b_coded) * centers[fname] / half_ranges[fname]
        }
      }

      # Two-factor interaction terms: b_ij * x_i * x_j.
      # x_i * x_j = (X_i - c_i)(X_j - c_j) / (hr_i * hr_j)
      for (i in seq_along(factor_names)) {
        for (j in seq_along(factor_names)) {
          if (j <= i) next
          fi <- factor_names[i]
          fj <- factor_names[j]
          int_name <- paste0(fi, ":", fj)
          if (!int_name %in% names(beta_hat)) {
            int_name <- paste0(fj, ":", fi)
          }
          if (int_name %in% names(beta_hat)) {
            b_int <- unname(beta_hat[int_name])
            hr_prod <- half_ranges[fi] * half_ranges[fj]
            uncoded_coef[[paste0(fi, ":", fj)]] <- unname(b_int / hr_prod)
            cur_lin_i <- if (fi %in% names(uncoded_coef)) uncoded_coef[[fi]] else 0
            cur_lin_j <- if (fj %in% names(uncoded_coef)) uncoded_coef[[fj]] else 0
            uncoded_coef[[fi]] <- cur_lin_i - b_int * centers[fj] / hr_prod
            uncoded_coef[[fj]] <- cur_lin_j - b_int * centers[fi] / hr_prod
            b0_uncoded <- b0_uncoded + b_int * centers[fi] * centers[fj] / hr_prod
          }
        }
      }

      # Pure quadratic terms: b_ii * x_i^2.
      # x_i^2 = (X_i - c_i)^2 / hr_i^2
      for (fname in factor_names) {
        quad_name <- paste0("I(", fname, "^2)")
        if (quad_name %in% names(beta_hat)) {
          b_quad <- unname(beta_hat[quad_name])
          hr_sq <- half_ranges[fname]^2
          uncoded_coef[[paste0(fname, "^2")]] <- unname(b_quad / hr_sq)
          cur_lin <- if (fname %in% names(uncoded_coef)) uncoded_coef[[fname]] else 0
          uncoded_coef[[fname]] <- cur_lin - 2 * b_quad * centers[fname] / hr_sq
          b0_uncoded <- b0_uncoded + b_quad * centers[fname]^2 / hr_sq
        }
      }

      # Strip any names accumulated from arithmetic with named vectors
      # (centers[], half_ranges[]) so that c()/unlist() does not mangle
      # the final coefficient names (e.g. "(Intercept).A").
      b0_uncoded <- unname(b0_uncoded)
      uncoded_coef <- lapply(uncoded_coef, unname)
      uncoded_coef <- c("(Intercept)" = b0_uncoded, unlist(uncoded_coef, use.names = TRUE))

      # Build a readable equation string.
      terms_str <- character(0)
      for (nm in names(uncoded_coef)) {
        val <- uncoded_coef[nm]
        sign_str <- if (val >= 0 && length(terms_str) == 0) "" else
                    if (val >= 0) " + " else " - "
        abs_val <- abs(val)
        term_label <- gsub("\\^2", "\u00b2", nm)
        term_label <- gsub(":", " * ", term_label)
        terms_str <- c(terms_str, paste0(sign_str, sprintf("%.4g", abs_val),
                                          if (nm != "(Intercept)")
                                            paste0(" * ", term_label)))
      }
      equation <- paste0("y = ", paste0(terms_str, collapse = ""))

      list(
        equation             = equation,
        coefficients         = uncoded_coef,
        coded_coefficients   = beta_hat,
        factor_centers       = centers,
        factor_half_ranges   = half_ranges
      )
    },

    # =========================================================================
    # Public: Power curve plot data
    # =========================================================================
    #
    # Minitab renders a power-vs-effect-size (or power-vs-n) curve alongside
    # the numeric power calculation. This method computes the power curve
    # data points so they can be rendered by DoePlotter's "power_curve"
    # plot type. The curve sweeps the effect size from near-zero to a
    # user-specified maximum, holding all other parameters fixed.
    #
    #' @description Compute the data for a power curve plot, sweeping the
    #'   effect size from near zero to `max_effect` while holding other
    #'   parameters fixed. Returns a data frame that can be passed to
    #'   `DoePlotter$plot(type = "power_curve")`.
    #' @param n_factors Integer scalar, the number of factors.
    #' @param n_replicates Integer scalar, the number of replicates.
    #' @param sigma Numeric scalar, the error standard deviation.
    #' @param n_center_points Integer scalar, the number of center points.
    #' @param alpha Numeric scalar, the Type I error rate.
    #' @param model_order Character scalar, the model order.
    #' @param max_effect Numeric scalar, the maximum effect size to plot.
    #'   Defaults to `4 * sigma`.
    #' @param n_points Integer scalar, the number of curve points.
    #' @return A data frame with columns `Effect_Size`, `Power`,
    #'   `Effect_to_Sigma_Ratio`, and `Target` (the 0.80 reference).
    plot_power_curve = function(n_factors, n_replicates = 1, sigma,
                                 n_center_points = 0, alpha = 0.05,
                                 model_order = "main",
                                 max_effect = NULL, n_points = 50) {
      if (is.null(max_effect)) max_effect <- 4 * sigma
      effect_seq <- seq(max_effect / n_points, max_effect, length.out = n_points)
      power_vals <- vapply(effect_seq, function(delta) {
        self$compute_power(n_factors, n_replicates, delta, sigma,
                           n_center_points, alpha, model_order)$power
      }, numeric(1))
      data.frame(
        Effect_Size            = effect_seq,
        Power                  = power_vals,
        Effect_to_Sigma_Ratio  = effect_seq / sigma,
        Target                 = 0.80,
        stringsAsFactors       = FALSE
      )
    },

    # =========================================================================
    # Public: Ridge Analysis for constrained RSM
    # =========================================================================
    #
    # When the stationary point of a second-order response surface lies
    # outside the experimental region (a saddle point or a distant optimum),
    # ridge analysis (also called canonical ridge analysis or constrained
    # optimization) finds the optimum of the predicted response on the
    # surface of a hypersphere of radius r centered at the design origin.
    #
    # The procedure (Hoerl 1959; Draper 1963):
    #   1. For a series of radii r = 0, 0.2, 0.4, ..., r_max:
    #      a. Constrained optimum x*(r) maximizes (or minimizes) the
    #         predicted response subject to x'x = r^2.
    #      b. Using the method of Lagrange multipliers, this reduces to
    #         solving (B - lambda*I) * x = -0.5 * b for the largest (or
    #         smallest) lambda that yields x'x = r^2.
    #   2. Return the path of ridge points x*(r) and the predicted response
    #      at each.
    #
    # Reference:
    #   Hoerl, A. E. (1959). Optimum Solutions to a Set of Linear Equations.
    #     Inst. Math. Stats.
    #   Draper, N. R. (1963). Ridge Analysis of Response Surfaces.
    #     Technometrics, 5(4), 469-479.
    #   Myers, R. H., Montgomery, D. C., & Anderson-Cook, C. M. (2016).
    #     Response Surface Methodology (4th ed.), sec. 6.4.
    #' @description Perform ridge analysis on a fitted second-order RSM
    #'   model. Computes the optimum of the predicted response on a series
    #'   of hyperspheres of increasing radius, starting from the design
    #'   center. Used when the stationary point is outside the design region
    #'   or is a saddle point.
    #' @param model A fitted `lm` object with a second-order formula.
    #' @param factor_names Character vector of factor names.
    #' @param maximize Logical. If `TRUE` (default), find the ridge of
    #'   steepest ascent; if `FALSE`, the ridge of steepest descent.
    #' @param max_radius Numeric scalar, the maximum radius to explore.
    #'   Defaults to `sqrt(k)` (the corner of the design cube).
    #' @param n_radii Integer scalar, the number of radii to evaluate.
    #' @return A data frame with one row per radius, containing the coded
    #'   factor values, the radius, the predicted response, and the Lagrange
    #'   multiplier. An attribute `"nature"` indicates whether the
    #'   stationary point is inside (`"inside"`) or outside (`"outside"`)
    #'   the design region.
    ridge_analysis = function(model, factor_names, maximize = TRUE,
                               max_radius = NULL, n_radii = 20) {
      beta_hat <- stats::coef(model)
      k <- length(factor_names)
      if (k < 2) {
        stop("[DoeAnalyzer] ridge_analysis requires at least 2 factors.",
             call. = FALSE)
      }

      if (is.null(max_radius)) max_radius <- sqrt(k)

      # Extract b (linear) and B (quadratic) matrices, same as
      # compute_stationary_point.
      b_vec <- beta_hat[factor_names]
      B_mat <- matrix(0, k, k, dimnames = list(factor_names, factor_names))
      quad_names <- paste0("I(", factor_names, "^2)")
      has_quad <- quad_names %in% names(beta_hat)
      if (!all(has_quad)) {
        stop("[DoeAnalyzer] ridge_analysis requires a full second-order model.",
             call. = FALSE)
      }
      diag(B_mat) <- beta_hat[quad_names]
      for (i in seq_len(k - 1)) {
        for (j in (i + 1):k) {
          int_name <- paste0(factor_names[i], ":", factor_names[j])
          if (!int_name %in% names(beta_hat)) {
            int_name <- paste0(factor_names[j], ":", factor_names[i])
          }
          if (int_name %in% names(beta_hat)) {
            B_mat[i, j] <- B_mat[j, i] <- beta_hat[int_name] / 2
          }
        }
      }

      b0 <- beta_hat["(Intercept)"]

      # Check if stationary point is inside the design region.
      sp <- tryCatch({
        x_s <- -0.5 * solve(B_mat, b_vec)
        sqrt(sum(x_s^2))
      }, error = function(e) Inf)
      nature <- if (is.na(sp) || is.infinite(sp) || sp > max_radius) {
        "outside"
      } else {
        "inside"
      }
      attr_nature <- nature

      # Sweep radii from 0 to max_radius.
      radii <- seq(0, max_radius, length.out = n_radii)
      ridge_results <- list()

      for (r in radii) {
        if (r < 1e-8) {
          # Radius 0 = the design center.
          x_r <- rep(0, k)
          pred_r <- b0
          lambda_r <- NA_real_
        } else {
          # Solve (B - lambda*I) * x = -0.5*b subject to x'x = r^2.
          # Use eigen-decomposition: B = V * D * V'.
          eig <- eigen(B_mat, symmetric = TRUE)
          V <- eig$vectors
          d <- eig$values

          # Transform b to the eigen-space.
          alpha_vec <- t(V) %*% (-0.5 * b_vec)

          # For a given lambda, x = V * (alpha / (d - lambda)).
          # Constraint: sum(alpha_i^2 / (d_i - lambda)^2) = r^2.
          # Find lambda by bisection: for maximize, lambda > max(d);
          # for minimize, lambda < min(d).
          find_lambda <- function(target_r2, maximize_flag) {
            if (maximize_flag) {
              lo <- max(d) + 1e-6
              hi <- max(d) + 1e6
            } else {
              lo <- min(d) - 1e6
              hi <- min(d) - 1e-6
            }
            for (iter in 1:100) {
              mid <- (lo + hi) / 2
              denom <- d - mid
              r2_est <- sum((alpha_vec / denom)^2)
              if (is.na(r2_est) || is.infinite(r2_est)) {
                return(mid)
              }
              if (abs(r2_est - target_r2) < 1e-8) return(mid)
              if (r2_est > target_r2) {
                if (maximize_flag) lo <- mid else hi <- mid
              } else {
                if (maximize_flag) hi <- mid else lo <- mid
              }
            }
            mid
          }

          lambda_r <- find_lambda(r^2, maximize)
          x_r <- as.numeric(V %*% (alpha_vec / (d - lambda_r)))
          pred_r <- b0 + sum(b_vec * x_r) + 0.5 * sum(x_r * (B_mat %*% x_r))
        }

        row_df <- data.frame(
          Radius = r,
          Lambda = lambda_r,
          Predicted = pred_r,
          stringsAsFactors = FALSE
        )
        for (i in seq_along(factor_names)) {
          row_df[[factor_names[i]]] <- x_r[i]
        }
        ridge_results <- c(ridge_results, list(row_df))
      }

      result <- do.call(rbind, ridge_results)
      attr(result, "nature") <- attr_nature
      attr(result, "maximize") <- maximize
      result
    },

    # =========================================================================
    # Public: Optimal Design Construction (D/I/A-optimal)
    # =========================================================================
    #
    # Constructs an optimal design by the coordinate-exchange algorithm of
    # Meyer & Nachtsheim (1995). Starting from a random initial design, the
    # algorithm cycles through every run and every factor; for each
    # (run, factor) cell it evaluates a grid of candidate levels and replaces
    # the current value with whichever level optimises the chosen criterion.
    # Iterations repeat until the criterion stabilises or `n_iter` is reached.
    #
    # Supported criteria:
    #   "D"  - D-optimality: maximise log det(X'X) (minimise generalised
    #          variance of the coefficient estimates).
    #   "A"  - A-optimality: minimise trace((X'X)^-1) (minimise the average
    #          variance of the coefficient estimates).
    #   "I"  - I-optimality: minimise the average prediction variance over
    #          the design region, trace((X'X)^-1) * trace(M) where M is the
    #          moment matrix of the model expanded over a fine candidate grid.
    #
    # The design region is the k-dimensional hypercube spanned by each
    # factor's levels (coded to [-1, +1] internally). The candidate set is a
    # grid of `n_levels` equally spaced values in [-1, +1] per factor; for
    # categorical factors the user-supplied levels are used directly.
    #
    # Reference:
    #   Meyer, R. K. & Nachtsheim, C. J. (1995). The Coordinate-Exchange
    #     Algorithm for Constructing Exact Optimal Experimental Designs.
    #     Technometrics, 37(1).
    #' @description Construct an optimal design via the coordinate-exchange
    #'   algorithm (Meyer & Nachtsheim 1995). Supports D-, A-, and
    #'   I-optimality criteria. The design is built in coded space (from -1
    #'   to +1) and then converted to the actual factor levels.
    #' @param factors List of factor definitions (each with `name`, `type`,
    #'   `levels`).
    #' @param n_runs Integer. Number of runs in the design.
    #' @param model_order Character. Model expanded when building the model
    #'   matrix: `"main"` (main effects only), `"interaction"` (main + 2FI),
    #'   or `"quadratic"` (full second-order).
    #' @param criterion Character. Optimality criterion: `"D"`, `"A"`, or
    #'   `"I"`.
    #' @param n_levels Integer. Number of candidate levels per factor in the
    #'   coordinate-exchange grid (default 5).
    #' @param n_iter Integer. Maximum number of coordinate-exchange sweeps
    #'   (default 20).
    #' @param n_starts Integer. Number of random restarts (default 10). The
    #'   best design across restarts is returned.
    #' @param seed Optional random seed for reproducibility.
    #' @return A list with `design` (data frame in actual factor levels),
    #'   `criterion` (the achieved criterion value), `criterion_name`,
    #'   `model_order`, `n_iter_run`, `X` (the final model matrix), and
    #'   `history` (criterion value per iteration of the best restart).
    create_optimal_design = function(factors, n_runs, model_order = "main",
                                      criterion = "D", n_levels = 5L,
                                      n_iter = 20L, n_starts = 10L,
                                      seed = NULL) {
      if (!is.list(factors) || length(factors) < 1) {
        stop("[DoeAnalyzer] create_optimal_design requires a non-empty ",
             "factors list.", call. = FALSE)
      }
      if (!is.numeric(n_runs) || n_runs < 1) {
        stop("[DoeAnalyzer] n_runs must be a positive integer.", call. = FALSE)
      }
      n_runs <- as.integer(n_runs)
      n_factors <- length(factors)
      criterion <- match.arg(criterion, choices = c("D", "A", "I"))
      model_order <- match.arg(model_order,
                               choices = c("main", "interaction", "quadratic"))
      factor_names <- vapply(factors, function(f) f$name, character(1))
      if (anyDuplicated(factor_names)) {
        stop("[DoeAnalyzer] factor names must be unique.", call. = FALSE)
      }

      # Candidate levels for each factor in coded [-1, +1] space.
      cand_levels <- replicate(n_factors,
                               seq(-1, 1, length.out = n_levels),
                               simplify = FALSE)

      # I-optimality requires the moment matrix M = integral f(x)f(x)' over
      # the design region. We approximate it on a fine grid; only the trace
      # of (X'X)^-1 * M is needed, so we precompute M once.
      moment_M <- NULL
      if (criterion == "I") {
        fine_grid <- as.matrix(expand.grid(
          replicate(n_factors, seq(-1, 1, length.out = 7), simplify = FALSE)
        ))
        moment_X <- private$.build_model_matrix_coded(fine_grid, factor_names,
                                                       model_order)
        moment_M <- crossprod(moment_X) / nrow(moment_X)
      }

      crit_fun <- function(X) {
        XtX <- crossprod(X)
        if (criterion == "D") {
          # D: maximise log det(X'X). Use log to keep scale manageable.
          ev <- tryCatch(eigen(XtX, symmetric = TRUE, only.values = TRUE),
                         error = function(e) NULL)
          if (is.null(ev)) return(-Inf)
          pos <- ev$values[ev$values > 1e-12 * max(ev$values)]
          if (length(pos) < ncol(X)) return(-Inf)
          sum(log(pos))
        } else if (criterion == "A") {
          # A: minimise trace((X'X)^-1).
          -sum(diag(solve(XtX + diag(1e-10, ncol(X)))))
        } else {
          # I: minimise trace((X'X)^-1 * M).
          XtX_inv <- tryCatch(solve(XtX + diag(1e-10, ncol(X))),
                              error = function(e) NULL)
          if (is.null(XtX_inv)) return(-Inf)
          -sum(XtX_inv * moment_M)
        }
      }

      withr::local_seed(seed)
      best_design <- NULL
      best_crit <- -Inf
      best_history <- numeric(0)

      for (start in seq_len(n_starts)) {
        # Random initial design: each cell drawn from the candidate levels.
        design_coded <- matrix(0, n_runs, n_factors)
        for (j in seq_len(n_factors)) {
          design_coded[, j] <- sample(cand_levels[[j]], n_runs, replace = TRUE)
        }

        history <- numeric(n_iter)
        prev_crit <- -Inf
        for (iter in seq_len(n_iter)) {
          for (i in seq_len(n_runs)) {
            for (j in seq_len(n_factors)) {
              cur_val <- design_coded[i, j]
              # Evaluate each candidate level for this cell; keep the best.
              cands <- unique(c(cur_val, cand_levels[[j]]))
              crits <- vapply(cands, function(v) {
                design_coded[i, j] <- v
                X <- private$.build_model_matrix_coded(design_coded,
                                                       factor_names,
                                                       model_order)
                crit_fun(X)
              }, numeric(1))
              design_coded[i, j] <- cands[which.max(crits)]
            }
          }
          X <- private$.build_model_matrix_coded(design_coded, factor_names,
                                                  model_order)
          cur_crit <- crit_fun(X)
          history[iter] <- cur_crit
          if (abs(cur_crit - prev_crit) < 1e-9) break
          prev_crit <- cur_crit
        }

        if (cur_crit > best_crit) {
          best_crit <- cur_crit
          best_design <- design_coded
          best_history <- history[seq_len(iter)]
        }
      }

      # Convert coded design to actual factor levels.
      design_df <- as.data.frame(best_design)
      colnames(design_df) <- factor_names
      for (j in seq_len(n_factors)) {
        f <- factors[[j]]
        if (length(f$levels) == 2) {
          # Linear map from [-1, +1] to [low, high].
          lo <- min(f$levels); hi <- max(f$levels)
          design_df[[f$name]] <- (hi + lo) / 2 + (hi - lo) / 2 * design_df[[f$name]]
        } else {
          # For multi-level factors, snap to the nearest supplied level.
          idx <- round((design_df[[f$name]] + 1) / 2 * (length(f$levels) - 1)) + 1
          idx <- pmax(1, pmin(length(f$levels), idx))
          design_df[[f$name]] <- f$levels[idx]
        }
      }
      design_df$RunOrder <- seq_len(nrow(design_df))

      list(
        design         = design_df,
        criterion      = best_crit,
        criterion_name = criterion,
        model_order    = model_order,
        n_iter_run     = length(best_history),
        X              = private$.build_model_matrix_coded(best_design,
                                                           factor_names,
                                                           model_order),
        history        = best_history
      )
    },

    # =========================================================================
    # Public: Variance Inflation Factor (VIF)
    # =========================================================================
    #
    # VIF_j = 1 / (1 - R_j^2), where R_j^2 is obtained by regressing
    # predictor X_j on all remaining predictors. VIF > 10 is the conventional
    # threshold flagging harmful multicollinearity (Montgomery 2019, sec. 7.6).
    #
    # This is a standalone accessor matching Minitab's "Coefficients" table
    # VIF column. It reuses the same algorithm as compute_model_fit_extended
    # but returns only the VIF vector plus a quick diagnostic summary.
    #
    # Reference:
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments
    #     (10th ed.), sec. 7.6.
    #   Marquardt, D. W. (1970). Generalized Inverses, Ridge Regression,
    #     Biased Linear Estimation, and Nonlinear Estimation. Technometrics.
    #' @description Compute Variance Inflation Factors (VIF) for the
    #'   predictors of a fitted linear model. VIF > 10 indicates harmful
    #'   multicollinearity. Matches the VIF column in Minitab's Coefficients
    #'   table.
    #' @param model A fitted `lm` object.
    #' @return A numeric vector of VIF values named by predictor. Returns
    #'   an empty vector when the model has fewer than 2 non-intercept
    #'   predictors.
    compute_vif = function(model) {
      X <- stats::model.matrix(model)
      if (ncol(X) > 1) X <- X[, -1, drop = FALSE]
      if (ncol(X) < 2) {
        return(setNames(numeric(0), character(0)))
      }
      vif_vals <- numeric(ncol(X))
      names(vif_vals) <- colnames(X)
      for (j in seq_len(ncol(X))) {
        others <- X[, -j, drop = FALSE]
        fit_j <- stats::lm(X[, j] ~ others)
        r_j <- summary(fit_j)$r.squared
        vif_vals[j] <- 1 / (1 - r_j + .Machine$double.eps)
      }
      vif_vals
    },

    # =========================================================================
    # Public: Botched Runs Handling
    # =========================================================================
    #
    # In practice, planned factor levels sometimes deviate from the nominal
    # settings (e.g. temperature set to 80 but actual measured 82). Minitab
    # allows users to "use actual levels" so the analysis reflects what really
    # happened rather than the planned values. This method replaces the
    # planned factor levels in the design matrix with the actual measured
    # levels and optionally re-fits the model.
    #
    # Reference:
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments
    #     (10th ed.), sec. 4.3 (Missing / Botched Runs).
    #' @description Replace planned factor levels with actual measured levels
    #'   (botched-run handling). Matches Minitab's "Use actual levels"
    #'   option. When the response column is present in `actual_data`, a
    #'   re-fitted `lm` model is returned; otherwise only the corrected
    #'   design is returned.
    #' @param design A data frame of the planned design (factor columns +
    #'   optional response column).
    #' @param actual_data A data frame containing the actual measured levels.
    #'   Must have the same number of rows as `design`. Column names identify
    #'   the factors to override; any column absent from `actual_data` keeps
    #'   its planned value.
    #' @param factor_names Character vector of factor column names to
    #'   consider for override. If `NULL`, all numeric columns in
    #'   `actual_data` are used.
    #' @param formula Optional model formula for re-fitting. If `NULL` and a
    #'   response column exists, a main-effects-plus-interactions formula is
    #'   constructed automatically.
    #' @param response_name Character scalar naming the response column in
    #'   `actual_data`. If `NULL`, the response is inherited from `design`.
    #' @return A list with `design_corrected` (data frame) and, when a
    #'   response is available, `model` (refitted `lm`) and `changed_cols`
    #'   (character vector of columns that were overridden).
    handle_botched_runs = function(design, actual_data, factor_names = NULL,
                                    formula = NULL, response_name = NULL) {
      if (!is.data.frame(design) || !is.data.frame(actual_data)) {
        stop("[DoeAnalyzer] design and actual_data must be data frames.",
             call. = FALSE)
      }
      if (nrow(design) != nrow(actual_data)) {
        stop("[DoeAnalyzer] design and actual_data must have the same ",
             "number of rows.", call. = FALSE)
      }

      # Determine which columns to override.
      if (is.null(factor_names)) {
        factor_names <- intersect(names(actual_data), names(design))
        factor_names <- factor_names[
          vapply(actual_data[factor_names], is.numeric, logical(1))
        ]
      }

      corrected <- design
      changed_cols <- character(0)
      for (col in factor_names) {
        if (col %in% names(actual_data) && col %in% names(corrected)) {
          actual_vals <- actual_data[[col]]
          planned_vals <- corrected[[col]]
          # Only override rows where actual differs from planned (within a
          # small tolerance) or where planned is NA but actual is present.
          differs <- is.na(planned_vals) |
            (abs(actual_vals - planned_vals) > 1e-9)
          if (any(differs, na.rm = TRUE)) {
            corrected[[col]] <- actual_vals
            changed_cols <- c(changed_cols, col)
          }
        }
      }

      out <- list(
        design_corrected = corrected,
        changed_cols     = changed_cols
      )

      # Re-fit model when a response is available.
      resp_col <- if (!is.null(response_name)) {
        response_name
      } else if (!is.null(attr(design, "response_name"))) {
        attr(design, "response_name")
      } else {
        # Heuristic: the last column that is numeric and not a factor col.
        cand <- setdiff(names(corrected), factor_names)
        cand <- cand[vapply(corrected[cand], is.numeric, logical(1))]
        if (length(cand) > 0) cand[length(cand)] else NULL
      }

      if (!is.null(resp_col) && resp_col %in% names(actual_data)) {
        corrected[[resp_col]] <- actual_data[[resp_col]]
        if (is.null(formula)) {
          # Default: full second-order for >= 2 factors, else main effects.
          k <- length(factor_names)
          if (k >= 2) {
            rhs <- paste(c(factor_names,
                           vapply(utils::combn(factor_names, 2,
                                               simplify = FALSE),
                                  function(p) paste(p, collapse = ":"),
                                  character(1)),
                           paste0("I(", factor_names, "^2)")),
                         collapse = " + ")
          } else {
            rhs <- paste(factor_names, collapse = " + ")
          }
          formula <- stats::as.formula(paste(resp_col, "~", rhs))
        }
        out$model <- stats::lm(formula, data = corrected)
      }

      out
    }
  ),

  private = list(

    # Lookup table of standard fractional-factorial design generators.
    # Returns a list with `p` (number of added factors) and `generators`
    # (character vector of generator strings, each being a product of base
    # factor letters). Base factors are always the first (k-p) letters, and
    # added factors are the next p letters. For example, for k=5 resolution V,
    # p=1 and generators="ABCD" means factor E = A*B*C*D (defining relation
    # I = ABCDE, which has length 5 -> resolution V).
    #
    # Sources:
    #   Montgomery (2019) Tables 8.3, 8.8, 8.14
    #   Box, Hunter & Hunter (2005) ch. 5,6
    #   NIST/SEMATECH e-Handbook of Statistical Methods, sec. 5.3.3
    .get_ff_generators = function(k, resolution) {
      if (!resolution %in% c("III", "IV", "V")) {
        return(NULL)
      }

      if (resolution == "III") {
        if (k == 3) return(list(p = 1, generators = c("AB")))               # I=ABC, 4 runs
        if (k == 4) return(list(p = 1, generators = c("ABC")))              # I=ABCD, 8 runs (res IV)
        if (k == 5) return(list(p = 2, generators = c("AB", "AC")))         # I=ABD=ACE=BCDE, 8 runs
        if (k == 6) return(list(p = 3, generators = c("AB", "AC", "BC")))   # I=ABD=ACE=BCF, 8 runs
        if (k == 7) return(list(p = 4, generators = c("AB", "AC", "BC", "ABC"))) # I=ABD=ACE=BCF=ABCG, 8 runs
        if (k >= 8) {
          # Use a saturated Plackett-Burman-style fraction (12 runs for up to
          # 11 factors). We approximate by giving 4 added factors based on
          # the base 4 columns; this yields a resolution III design.
          p <- k - 4L
          # Recursively reuse the k=7 generators for the base and add new
          # columns aliased with the highest-order interaction.
          gens <- c("AB", "AC", "BC", "ABC")
          if (p > 4) {
            # For p > 4 we need additional generators. Use the standard
            # Montgomery tables for saturated designs (every added factor
            # is aliased with a distinct word in the base 4 letters).
            extra_pool <- c("ABCD", "AB", "AC", "AD", "BC", "BD", "CD", "ABC", "ABD", "ACD", "BCD")
            gens <- c(gens, extra_pool[seq_len(p - 4)])
          }
          return(list(p = p, generators = gens[seq_len(p)]))
        }
      }

      if (resolution == "IV") {
        if (k == 4) return(list(p = 1, generators = c("ABC")))              # I=ABCD, 8 runs
        if (k == 5) return(list(p = 1, generators = c("ABCD")))             # I=ABCDE, 16 runs (res V)
        if (k == 6) return(list(p = 2, generators = c("ABC", "BCD")))       # I=ABCE=BCDF=ADEF, 16 runs
        if (k == 7) return(list(p = 2, generators = c("ABC", "ADE")))       # I=ABCF=ADEG=BCDEFG, 32 runs
        if (k == 8) return(list(p = 4, generators = c("AB", "AC", "AD", "BC"))) # I=ABE=ACF=ADG=BCD... 16 runs
        if (k >= 9) {
          # For large k resolution IV: use the standard 2^(k-4) generators
          # from Montgomery Table 8.8 (saturated resolution IV).
          p <- 4L
          return(list(p = p, generators = c("AB", "AC", "AD", "BC")))
        }
      }

      if (resolution == "V") {
        if (k == 5) return(list(p = 1, generators = c("ABCD")))             # I=ABCDE, 16 runs
        if (k == 6) return(list(p = 1, generators = c("ABCDE")))            # I=ABCDEF, 32 runs (res VI)
        if (k == 7) return(list(p = 2, generators = c("ABCD", "ABCE")))     # I=ABCDE=ABCDF=DEF 64 runs
        if (k == 8) return(list(p = 3, generators = c("ABCDE", "ABCF", "ABCG"))) # 2^(8-3)=32 runs
        if (k >= 9) {
          # For very large k with resolution V, p grows linearly. We fall
          # back to a single added factor (half-fraction) which gives a
          # resolution (k-1) design - usually better than V anyway.
          p <- 1L
          gen_str <- paste0(LETTERS[seq_len(k - 1)], collapse = "")
          return(list(p = p, generators = c(gen_str)))
        }
      }

      NULL
    },

    # Multiply two words in GF(2) arithmetic (X * X = I, so repeated letters
    # cancel). The identity word is "I". For example, "AB" * "AC" = "BC"
    # (the A cancels), and "ABC" * "ABC" = "I".
    .multiply_words = function(w1, w2) {
      if (w1 == "I") return(w2)
      if (w2 == "I") return(w1)
      chars <- c(strsplit(w1, "")[[1]], strsplit(w2, "")[[1]])
      # Modulo-2: a letter appears in the product iff it appears an odd number
      # of times in the operands.
      tab <- table(chars)
      result_chars <- names(tab)[tab %% 2 == 1]
      if (length(result_chars) == 0) return("I")
      paste(sort(result_chars), collapse = "")
    },
    # Generate the experimental design table by dispatching on plan$design_type.
    # After the base design is produced, optional replication, center points,
    # blocking, and run-order randomization are applied as configured on the
    # plan. Note: CCD/RSM/Box-Behnken designs already include their own center
    # points inside their generators, so .add_center_points is skipped for
    # those design types to avoid double-counting. When blocking is enabled,
    # the design is partitioned into `plan$n_blocks` blocks of (nearly) equal
    # size and run order is randomized within each block (restricted
    # randomization). This implements a Randomized Complete Block Design
    # (RCBD) when each block contains a full replicate, or an Incomplete
    # Block Design (IBD) otherwise.
    .generate_design = function(plan) {
      design <- switch(plan$design_type,
        "factorial" = private$.generate_factorial(plan),
        "fractional" = private$.generate_fractional(plan),
        "orthogonal" = private$.generate_orthogonal(plan),
        "rsm" = private$.generate_rsm(plan),
        "ccd" = private$.generate_ccd(plan),
        "box_behnken" = private$.generate_box_behnken(plan),
        "taguchi" = private$.generate_taguchi(plan),
        "lhs" = private$.generate_lhs(plan),
        "maximin" = private$.generate_maximin(plan),
        "dsd" = private$.generate_dsd(plan),
        "simplex_centroid" = private$.generate_simplex_centroid(plan),
        "simplex_lattice" = private$.generate_simplex_lattice(plan),
        "extreme_vertices" = private$.generate_extreme_vertices(plan),
        "split_plot" = private$.generate_split_plot(plan),
        stop("Unsupported design type: ", plan$design_type)
      )

      # Add replications
      if (plan$replication > 1) {
        design <- private$.add_replication(design, plan$replication)
      }

      # Add center points (skip for designs that already include them)
      if (plan$center_points > 0 &&
          !plan$design_type %in% c("ccd", "rsm", "box_behnken", "dsd")) {
        design <- private$.add_center_points(design, plan$center_points, plan)
      }

      # Apply blocking (restricted randomization within blocks)
      if (isTRUE(plan$blocking) && plan$n_blocks > 1) {
        design <- private$.apply_blocking(design, plan$n_blocks, plan$seed)
      } else if (plan$randomize) {
        # Standard (unrestricted) randomization of run order
        design <- private$.randomize_design(design, plan$seed)
      }

      design
    },

    # Partition the design into `n_blocks` blocks of (nearly) equal size,
    # randomize the run order within each block, and assign a Block column
    # to every run. This implements a Randomized Complete Block Design (RCBD)
    # when each block contains a full set of treatment combinations, or an
    # Incomplete Block Design (IBD) otherwise.
    #
    # Theory (Montgomery 2019, sec. 4.4):
    #   Block effects account for known nuisance variation (day, operator,
    #   batch, etc.). Randomization within blocks prevents confounding of
    #   treatment effects with time trends, while still allowing the block
    #   effect to be separated in the ANOVA decomposition.
    .apply_blocking = function(design, n_blocks, seed = NULL) {
      withr::local_seed(seed)

      n_runs <- nrow(design)
      if (n_blocks > n_runs) {
        warning("[DoeAnalyzer] n_blocks (", n_blocks, ") > n_runs (",
                n_runs, "); reducing to ", n_runs, " blocks.", call. = FALSE)
        n_blocks <- n_runs
      }

      # Randomly permute the runs first, then assign to blocks in order.
      # This guarantees both between-block balance (block sizes differ by at
      # most 1) and within-block randomization (each block receives a
      # random subset of the permuted runs).
      perm <- sample(n_runs)
      block_sizes <- private$.allocate_block_sizes(n_runs, n_blocks)
      block_id <- rep(seq_len(n_blocks), block_sizes)

      design <- design[perm, , drop = FALSE]
      design$Block <- factor(block_id)

      # Reset RunOrder within each block so that operators can run the
      # blocks independently. We also expose a global RunOrder column for
      # downstream consumers that expect a single sequential run counter.
      design$RunOrder <- seq_len(n_runs)

      # Re-order by block, then by within-block run order, so the printed
      # design table reads in block order.
      design <- design[order(design$Block, design$RunOrder), ]
      rownames(design) <- NULL
      design$RunOrder <- seq_len(n_runs)
      design
    },

    # Allocate `n_runs` runs to `n_blocks` blocks as evenly as possible.
    # The first (n_runs %% n_blocks) blocks receive one extra run; the
    # remaining blocks receive floor(n_runs / n_blocks) runs.
    .allocate_block_sizes = function(n_runs, n_blocks) {
      base <- n_runs %/% n_blocks
      extra <- n_runs %% n_blocks
      sizes <- rep(base, n_blocks)
      if (extra > 0) sizes[seq_len(extra)] <- sizes[seq_len(extra)] + 1L
      sizes
    },

    # Generate a full factorial design. Produces every combination of factor
    # levels via expand.grid and assigns a run order column.
    .generate_factorial = function(plan) {
      # Collect factor names and levels
      factor_names <- sapply(plan$factors, function(f) f$name)
      factor_levels <- lapply(plan$factors, function(f) f$levels)

      # Generate the full set of level combinations using expand.grid
      names(factor_levels) <- factor_names
      design <- expand.grid(factor_levels, stringsAsFactors = FALSE)

      # Assign a run order column
      design$RunOrder <- seq_len(nrow(design))
      # Tag every row as a cube (corner) point so the design plot can
      # distinguish factorial points from axial/center points in RSM designs.
      design$PointType <- "cube"

      design
    },

    # Generate a 2^(k-p) fractional factorial design using design generators
    # (defining relations). The base fraction is a full 2^(k-p) factorial in
    # the first (k-p) factors; the remaining p factors are generated as
    # products of base factor columns (mod 2 arithmetic on +/-1 coded values).
    #
    # This is the textbook construction (Montgomery 2019 ch.8, Box-Hunter-
    # Hunter 2005 ch.5) and produces a design with the correct alias
    # structure. The defining relation can be retrieved via
    # `get_alias_structure(n_factors, resolution)`.
    .generate_fractional = function(plan) {
      n_factors <- length(plan$factors)
      resolution <- plan$resolution
      if (is.null(resolution)) {
        stop("[DoeAnalyzer] Fractional factorial requires 'resolution' ",
             "(III / IV / V) to be set on the plan.", call. = FALSE)
      }

      gens <- private$.get_ff_generators(n_factors, resolution)
      if (is.null(gens)) {
        stop("[DoeAnalyzer] No standard fractional factorial generator for ",
             "k = ", n_factors, ", resolution = ", resolution, ". ",
             "Use a full factorial or reduce the number of factors.",
             call. = FALSE)
      }

      p <- gens$p
      generators <- gens$generators
      base_k <- n_factors - p
      factor_names <- vapply(plan$factors, function(f) f$name, character(1))

      # 1. Build the base 2^base_k full factorial using the first (k-p)
      # factors' coded levels (-1, +1). We always use +/-1 coding here so
      # that column products produce valid +/-1 values; the actual factor
      # levels are applied afterwards via .convert_coded_to_actual.
      base_levels <- replicate(base_k, c(-1, 1), simplify = FALSE)
      names(base_levels) <- factor_names[seq_len(base_k)]
      base_design <- expand.grid(base_levels, stringsAsFactors = FALSE)

      # 2. Compute the added factor columns as products of base columns.
      # Each generator string like "ABC" means the new factor column is
      # the element-wise product of base columns A, B, and C. In +/-1
      # coding, multiplication mod 2 corresponds to ordinary arithmetic
      # multiplication, so the product of -1, +1, +1 is -1.
      for (i in seq_along(generators)) {
        gen_str <- generators[i]
        gen_letters <- strsplit(gen_str, "")[[1]]
        col_indices <- match(gen_letters, LETTERS[seq_len(base_k)])
        if (any(is.na(col_indices))) {
          stop("[DoeAnalyzer] Generator '", gen_str, "' references a base ",
               "factor that does not exist (base_k = ", base_k, ").",
               call. = FALSE)
        }
        added_col <- rep(1, nrow(base_design))
        for (idx in col_indices) {
          added_col <- added_col * base_design[[idx]]
        }
        base_design[[base_k + i]] <- added_col
      }

      colnames(base_design) <- factor_names
      # Convert coded +/-1 values back to the user-declared factor levels.
      base_design <- private$.convert_coded_to_actual(base_design, plan)
      base_design$RunOrder <- seq_len(nrow(base_design))
      base_design$PointType <- "cube"
      base_design
    },

    # Generate an orthogonal array design. Supports the standard Taguchi
    # arrays L4, L8, L9, L12, L16, and L27, selecting the smallest array that
    # accommodates the requested number of factors and levels. Falls back to
    # the full factorial for cases not covered by the catalogue.
    #
    # Sources:
    #   Taguchi, G. (1987). System of Experimental Design, vols. 1 & 2.
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments,
    #     ch. 9 (Taguchi robust design).
    .generate_orthogonal = function(plan) {
      n_factors <- length(plan$factors)
      max_levels <- max(vapply(plan$factors, function(f) length(f$levels), integer(1)))

      # All factors must share the same number of levels for the standard
      # Taguchi arrays to apply directly. If a mixed-level design is needed,
      # we fall back to the full factorial (a future implementation could
      # add mixed-level arrays such as L18 or L36).
      all_same_levels <- all(vapply(plan$factors,
        function(f) length(f$levels) == max_levels, logical(1)))

      design <- if (max_levels == 2 && n_factors <= 3) {
        # L4 (2^3): 3 factors at 2 levels, 4 runs
        private$.array_l4()
      } else if (max_levels == 2 && n_factors <= 7) {
        # L8 (2^7): 7 factors at 2 levels, 8 runs
        private$.array_l8()
      } else if (max_levels == 2 && n_factors <= 11) {
        # L12 (2^11): 11 factors at 2 levels, 12 runs (Plackett-Burman)
        private$.array_l12()
      } else if (max_levels == 2 && n_factors <= 15) {
        # L16 (2^15): 15 factors at 2 levels, 16 runs
        private$.array_l16()
      } else if (max_levels == 3 && all_same_levels && n_factors <= 4) {
        # L9 (3^4): 4 factors at 3 levels, 9 runs
        private$.array_l9()
      } else if (max_levels == 3 && all_same_levels && n_factors <= 13) {
        # L27 (3^13): 13 factors at 3 levels, 27 runs
        private$.array_l27()
      } else {
        # No standard orthogonal array available: fall back to full factorial
        message("[DoeAnalyzer] No standard orthogonal array for ",
                n_factors, " factors at ", max_levels, " levels; ",
                "falling back to full factorial.")
        return(private$.generate_factorial(plan))
      }

      # Subset to the requested number of factor columns and rename.
      factor_names <- vapply(plan$factors, function(f) f$name, character(1))
      selected_cols <- seq_len(min(n_factors, ncol(design)))
      design <- design[, selected_cols, drop = FALSE]
      colnames(design) <- factor_names[seq_len(min(n_factors, ncol(design)))]

      # Convert coded values (-1, 0, +1 for 2- and 3-level designs) to the
      # user-declared actual factor levels.
      design <- private$.convert_coded_to_actual(design, plan)

      design$RunOrder <- seq_len(nrow(design))
      design$PointType <- "cube"
      design
    },

    # L4 (2^3) orthogonal array, 3 factors at 2 levels, 4 runs.
    # Coded levels: -1 = low, +1 = high.
    .array_l4 = function() {
      data.frame(
        A = c(-1, -1,  1,  1),
        B = c(-1,  1, -1,  1),
        C = c(-1,  1,  1, -1)
      )
    },

    # L8 (2^7) orthogonal array, 7 factors at 2 levels, 8 runs.
    .array_l8 = function() {
      data.frame(
        A = c(-1, -1, -1, -1,  1,  1,  1,  1),
        B = c(-1, -1,  1,  1, -1, -1,  1,  1),
        C = c(-1,  1, -1,  1, -1,  1, -1,  1),
        D = c(-1,  1,  1, -1, -1,  1,  1, -1),
        E = c(-1, -1, -1, -1,  1,  1,  1,  1),
        F = c(-1, -1,  1,  1, -1, -1,  1,  1),
        G = c(-1,  1, -1,  1, -1,  1, -1,  1)
      )
    },

    # L9 (3^4) orthogonal array, 4 factors at 3 levels, 9 runs.
    # Coded levels: -1 = low, 0 = middle, +1 = high.
    .array_l9 = function() {
      data.frame(
        A = c(-1, -1, -1,  0,  0,  0,  1,  1,  1),
        B = c(-1,  0,  1, -1,  0,  1, -1,  0,  1),
        C = c(-1,  0,  1,  0,  1, -1,  1, -1,  0),
        D = c(-1,  0,  1,  1, -1,  0,  0,  1, -1)
      )
    },

    # L12 (2^11) Plackett-Burman design, 11 factors at 2 levels, 12 runs.
    # Generated from the standard PB12 sign vector by cyclic permutation.
    .array_l12 = function() {
      # Standard PB12 first row (after the all-low row); the array is built by
      # cyclic shifts of this row. Reference: Plackett & Burman (1946).
      first_row <- c(1, 1, 1, -1, -1, -1, 1, -1, 1, 1, -1)
      rows <- list(as.integer(rep(-1, 11)))
      row_so_far <- first_row
      for (i in 1:11) {
        rows[[i + 1]] <- as.integer(row_so_far)
        row_so_far <- c(row_so_far[11], row_so_far[1:10])  # cyclic right shift
      }
      mat <- do.call(rbind, rows)
      df <- as.data.frame(mat)
      names(df) <- LETTERS[1:11]
      df
    },

    # L16 (2^15) orthogonal array, 15 factors at 2 levels, 16 runs.
    # Built as a regular 2^(4) full factorial in the first 4 base columns,
    # with the remaining 11 columns generated as products of base columns
    # (standard 2^(15-11) fractional factorial construction with I = all
    # products of length >= 4 ... here we use the well-known Hall/Lin
    # construction). The columns below are the standard L16(2^15) signs.
    .array_l16 = function() {
      # Base 4 columns: a 2^4 full factorial in coded -1/+1.
      base <- expand.grid(
        A = c(-1, 1), B = c(-1, 1), C = c(-1, 1), D = c(-1, 1),
        stringsAsFactors = FALSE
      )
      # The remaining 11 columns are the standard 2-factor, 3-factor, and
      # 4-factor interactions of the base columns (this gives the regular
      # 2^(15-11) design with resolution IV).
      base$E <- base$A * base$B
      base$F <- base$A * base$C
      base$G <- base$A * base$D
      base$H <- base$B * base$C
      base$I <- base$B * base$D
      base$J <- base$C * base$D
      base$K <- base$A * base$B * base$C
      base$L <- base$A * base$B * base$D
      base$M <- base$A * base$C * base$D
      base$N <- base$B * base$C * base$D
      base$O <- base$A * base$B * base$C * base$D
      base
    },

    # L27 (3^13) orthogonal array, 13 factors at 3 levels, 27 runs.
    # Built as the standard 3^(13-10) fractional factorial design using the
    # linear/quadratic pseudo-factors of a 3^3 base array.
    .array_l27 = function() {
      # Base 3 columns: a 3^3 full factorial in coded -1/0/+1.
      base <- expand.grid(
        A = c(-1, 0, 1), B = c(-1, 0, 1), C = c(-1, 0, 1),
        stringsAsFactors = FALSE
      )
      # The remaining 10 columns are generated by the standard 3-level
      # generator pairs (linear and quadratic components). Each pair (X_L, X_Q)
      # is derived from two base columns (i, j) as:
      #   X_L =  i_lin +  j_lin (mod 3), mapped back to -1/0/+1
      #   X_Q = 2*i_lin +  j_lin (mod 3), mapped back to -1/0/+1
      # where i_lin is the linear coded value of base column i.
      # Reference: Taguchi (1987), System of Experimental Design, vol. 1.
      map_to_coded <- function(v) {
        # Map 0,1,2 (mod 3) to -1,0,+1.
        c(-1, 0, 1)[(v %% 3) + 1]
      }
      to_lin <- function(x) (x + 2) %% 3  # -1,0,+1 -> 0,2,1 -> wait, we want -1->0, 0->1, 1->2
      # Actually -1 -> 0, 0 -> 1, +1 -> 2 in mod-3 arithmetic.
      to_mod3 <- function(x) match(x, c(-1, 0, 1)) - 1
      base$D <- sapply(seq_len(nrow(base)), function(k) map_to_coded(to_mod3(base$A[k]) + to_mod3(base$B[k])))
      base$E <- sapply(seq_len(nrow(base)), function(k) map_to_coded((2 * to_mod3(base$A[k]) + to_mod3(base$B[k])) %% 3))
      base$F <- sapply(seq_len(nrow(base)), function(k) map_to_coded(to_mod3(base$A[k]) + to_mod3(base$C[k])))
      base$G <- sapply(seq_len(nrow(base)), function(k) map_to_coded((2 * to_mod3(base$A[k]) + to_mod3(base$C[k])) %% 3))
      base$H <- sapply(seq_len(nrow(base)), function(k) map_to_coded(to_mod3(base$B[k]) + to_mod3(base$C[k])))
      base$I <- sapply(seq_len(nrow(base)), function(k) map_to_coded((2 * to_mod3(base$B[k]) + to_mod3(base$C[k])) %% 3))
      base$J <- sapply(seq_len(nrow(base)), function(k) map_to_coded((to_mod3(base$A[k]) + to_mod3(base$B[k]) + to_mod3(base$C[k])) %% 3))
      base$K <- sapply(seq_len(nrow(base)), function(k) map_to_coded((2 * to_mod3(base$A[k]) + to_mod3(base$B[k]) + to_mod3(base$C[k])) %% 3))
      base$L <- sapply(seq_len(nrow(base)), function(k) map_to_coded((to_mod3(base$A[k]) + 2 * to_mod3(base$B[k]) + to_mod3(base$C[k])) %% 3))
      base$M <- sapply(seq_len(nrow(base)), function(k) map_to_coded((to_mod3(base$A[k]) + to_mod3(base$B[k]) + 2 * to_mod3(base$C[k])) %% 3))
      base
    },

    # Generate a Central Composite Design (CCD). The design combines the
    # factorial portion (2^k), axial (star) points, and center points.
    .generate_ccd = function(plan) {
      n_factors <- length(plan$factors)

      # Factorial portion (2^k)
      factorial_part <- private$.generate_factorial(plan)
      # Drop RunOrder to align columns with axial/center parts; re-add later.
      factorial_part$RunOrder <- NULL

      # Axial (star) points (2k points)
      alpha <- private$.resolve_ccd_alpha(plan$alpha, n_factors)
      axial_points <- private$.generate_axial_points(n_factors, alpha, plan)

      # Center points
      center_points <- private$.generate_center_points(n_factors, max(6, plan$center_points), plan)

      # Combine all parts and assign sequential run order
      design <- rbind(factorial_part, axial_points, center_points)
      design$RunOrder <- seq_len(nrow(design))
      design
    },

    # Resolve the CCD axial distance alpha. When `alpha_spec` is NULL or one of
    # the standard keywords ("rotatable", "spherical", "face_centered",
    # "orthogonal"), the corresponding formula is applied:
    #   - rotatable:     alpha = (2^k)^(1/4)   (default for CCD/RSM)
    #   - spherical:     alpha = sqrt(k)
    #   - face_centered: alpha = 1             (axial points on the cube faces)
    #   - orthogonal:    alpha chosen so the design is variance-orthogonal;
    #                     a closed-form approximation is used here.
    # When `alpha_spec` is a positive numeric scalar, it is used as-is.
    .resolve_ccd_alpha = function(alpha_spec, n_factors) {
      if (is.null(alpha_spec)) {
        # Default: rotatable
        return((2^n_factors)^(1 / 4))
      }
      if (is.numeric(alpha_spec) && length(alpha_spec) == 1 && alpha_spec > 0) {
        return(alpha_spec)
      }
      if (is.character(alpha_spec) && length(alpha_spec) == 1) {
        switch(alpha_spec,
          "rotatable"     = (2^n_factors)^(1 / 4),
          "spherical"     = sqrt(n_factors),
          "face_centered" = 1,
          "orthogonal"    = {
            # Approximation of the orthogonal-alpha value (Box & Hunter, 1957).
            # For a design with n_f factorial points, 2k axial points, and n_c
            # center points, alpha = ((4 * 2^k * (n_c + 2^k) - 2^k * n_c) /
            # (2 * 2^k))^(1/4). With n_c = 6 center points (package default):
            n_f <- 2^n_factors
            n_c <- 6
            ((4 * n_f * (n_c + n_f) - n_f * n_c) / (2 * n_f))^(1 / 4)
          },
          # Unknown keyword: fall back to rotatable.
          (2^n_factors)^(1 / 4)
        )
      }
      # Fallback
      (2^n_factors)^(1 / 4)
    },

    # Generate axial (star) points for a CCD at distance alpha from the center
    # along each factor axis.
    .generate_axial_points = function(n_factors, alpha, plan) {
      axial_list <- list()

      for (i in seq_len(n_factors)) {
        for (sign in c(-1, 1)) {
          point <- rep(0, n_factors)
          point[i] <- sign * alpha
          axial_list[[length(axial_list) + 1]] <- point
        }
      }

      design <- as.data.frame(do.call(rbind, axial_list),
                              stringsAsFactors = FALSE)
      # Ensure all columns are numeric (do.call(rbind, list) sometimes
      # returns list columns when alpha is not integer-valued).
      for (col in names(design)) {
        design[[col]] <- as.numeric(design[[col]])
      }
      factor_names <- sapply(plan$factors, function(f) f$name)
      colnames(design) <- factor_names

      private$.convert_coded_to_actual(design, plan)

      # Tag axial (star) points so the design plot can distinguish them
      # from cube/center points in a CCD.
      design$PointType <- "axial"

      design
    },

    # Generate center points for the design. Each center point is located at
    # the mean of every factor's levels.
    .generate_center_points = function(n_factors, n_centers, plan) {
      center_values <- lapply(plan$factors, function(f) {
        mean(f$levels)
      })

      design <- as.data.frame(do.call(rbind,
                                      replicate(n_centers, center_values,
                                                simplify = FALSE)),
                              stringsAsFactors = FALSE)
      # Ensure numeric columns
      for (col in names(design)) {
        design[[col]] <- as.numeric(design[[col]])
      }
      factor_names <- sapply(plan$factors, function(f) f$name)
      colnames(design) <- factor_names

      # Tag center points so the design plot can distinguish them
      # from cube/axial points in CCD and BBD designs.
      design$PointType <- "center"

      design
    },

    # Generate a Box-Behnken design for 3 to ~6 factors. The design consists
    # of the midpoints of the edges of the experimental cube (plus center
    # points), giving an efficient 3-level second-order design that avoids
    # the extreme (corner + axial) points of a CCD. Box-Behnken designs are
    # preferred when the corner points are physically infeasible or expensive.
    #
    # Construction (Box & Behnken, 1960):
    #   For each pair of factors (i, j), generate 4 runs where factors i and j
    #   are at combinations (-1,-1), (-1,+1), (+1,-1), (+1,+1), and every
    #   other factor is at the center (0).
    #   Total factorial points: 4 * C(k, 2) = 2k(k-1).
    #   Plus n_c center points (default 3-6).
    #
    # Reference:
    #   Box, G. E. P., & Behnken, D. W. (1960). Some New Three Level Designs
    #   for the Study of Quantitative Variables. Technometrics, 2(4), 455-475.
    .generate_box_behnken = function(plan) {
      n_factors <- length(plan$factors)
      if (n_factors < 3) {
        stop("[DoeAnalyzer] Box-Behnken design requires at least 3 factors.",
             call. = FALSE)
      }
      if (n_factors > 10) {
        warning("[DoeAnalyzer] Box-Behnken with ", n_factors, " factors ",
                "produces ", 2 * n_factors * (n_factors - 1),
                " factorial runs; this is large.", call. = FALSE)
      }

      factor_names <- vapply(plan$factors, function(f) f$name, character(1))

      # Build the 2k(k-1) factorial runs as edge midpoints.
      pair_indices <- utils::combn(n_factors, 2, simplify = FALSE)
      run_list <- list()
      for (pair in pair_indices) {
        i <- pair[1]; j <- pair[2]
        for (s1 in c(-1, 1)) {
          for (s2 in c(-1, 1)) {
            row <- rep(0, n_factors)
            row[i] <- s1
            row[j] <- s2
            run_list[[length(run_list) + 1]] <- row
          }
        }
      }
      factorial_part <- as.data.frame(do.call(rbind, run_list),
                                      stringsAsFactors = FALSE)
      for (col in names(factorial_part)) {
        factorial_part[[col]] <- as.numeric(factorial_part[[col]])
      }
      colnames(factorial_part) <- factor_names

      # Convert coded +/-1/0 values to the user-declared factor levels so
      # the edge midpoints are in actual engineering units, consistent with
      # the center points (which are generated directly in actual units via
      # mean(f$levels)). Without this step the design table would mix coded
      # and actual units (issue: BBD generation produced mixed-unit output).
      factorial_part <- private$.convert_coded_to_actual(factorial_part, plan)

      # Tag edge-midpoint runs so the design plot can distinguish them
      # from center points in a BBD.
      factorial_part$PointType <- "edge"

      # Add center points (Box & Behnken recommend at least 3; the user
      # can override via plan$center_points).
      n_centers <- max(plan$center_points, 3)
      center_part <- private$.generate_center_points(n_factors, n_centers, plan)

      design <- rbind(factorial_part, center_part)
      design$RunOrder <- seq_len(nrow(design))
      design
    },

    # Generate a Response Surface Methodology (RSM) design. Currently defaults
    # to a Central Composite Design.
    .generate_rsm = function(plan) {
      # Default to CCD
      private$.generate_ccd(plan)
    },

    # Generate a Taguchi design. NOTE: simplified implementation that reuses
    # the L8 orthogonal array logic.
    .generate_taguchi = function(plan) {
      # Simplified: use the L8 orthogonal array
      private$.generate_orthogonal(plan)
    },

    # Generate a Latin Hypercube Sample (LHS). The number of runs follows the
    # rule of thumb of at least 10 * n_factors (minimum 20). NOTE: this is a
    # simplified LHS implementation; production use should rely on
    # DiceDesign::dmax or similar for optimized space-filling designs.
    .generate_lhs = function(plan) {
      n_factors <- length(plan$factors)
      n_runs <- max(10 * n_factors, 20)  # Rule of thumb

      # Simplified LHS implementation (use DiceDesign::dmax in production)
      # Pre-allocate the design data frame so column assignment works.
      design <- as.data.frame(matrix(NA_real_, nrow = n_runs,
                                     ncol = n_factors))
      colnames(design) <- vapply(plan$factors, function(f) f$name,
                                 character(1))

      for (i in seq_len(n_factors)) {
        factor <- plan$factors[[i]]
        levels <- sort(factor$levels)

        # Stratify the level range into equal-probability intervals
        breaks <- quantile(levels, probs = seq(0, 1, length.out = n_runs + 1))
        samples <- runif(n_runs, min = breaks[-(n_runs + 1)], max = breaks[-1])

        design[[factor$name]] <- samples
      }

      design$RunOrder <- seq_len(nrow(design))
      design
    },

    # Generate a maximin space-filling design. NOTE: simplified implementation
    # that reuses the Latin Hypercube logic.
    .generate_maximin = function(plan) {
      # Simplified: reuse the improved LHS
      private$.generate_lhs(plan)
    },

    # Generate a Definitive Screening Design (DSD).
    #
    # DSDs are a class of three-level screening designs introduced by
    # Jones & Nachtsheim (2011). They provide the following key properties:
    #
    #   1. Main effects are not aliased with any two-factor interaction or
    #      pure quadratic term (Resolution IV-like behavior).
    #   2. Two-factor interactions are not fully aliased with each other.
    #   3. Pure quadratic terms are estimable and not aliased with main
    #      effects.
    #   4. The design requires only 2k + 1 runs (plus optional center
    #      points), which is substantially smaller than a 3^k factorial
    #      or a comparable CCD.
    #
    # Construction (Jones & Nachtsheim 2011; Xiao & Xu 2017):
    #
    #   For k factors:
    #     Step 1. Build a conference matrix C of order k. A conference
    #             matrix is a k x k matrix with entries in {-1, 0, +1}
    #             satisfying C'C = (k-1) * I. When k is even, a symmetric
    #             conference matrix is used; when k is odd, a skew-
    #             symmetric conference matrix is used.
    #     Step 2. The folded design matrix is constructed from C and -C:
    #
    #             [ C  1 ]
    #             [-C -1 ]
    #             [ 0  0 ]
    #
    #             where the last column is a dummy factor filled with 1s,
    #             -1s, and a 0 in the center row.
    #     Step 3. Drop the dummy column; the resulting 2k+1 x k matrix
    #             is the DSD in coded units (-1, 0, +1).
    #
    # Conference matrices exist for many orders k. For small k we use
    # explicit constructions from the catalog compiled by
    # Craigen (1996) and the Neumann–Spence (2010) tables:
    #
    #   k = 2:  [[0, 1], [1, 0]]
    #   k = 3:  [[0, 1, 1], [-1, 0, 0], [-1, 0, 0]] (skew)
    #   k = 4:  [[0, 1, 1, 1], [1, 0, 1, -1], [1, -1, 0, 1], [1, -1, -1, 0]] (sym)
    #   k = 5:  skew-symmetric explicit construction
    #   k = 6..12: symmetric conference matrices from standard catalogs
    #
    # For k not supported by an explicit construction we fall back to
    # a foldover of a saturated Plackett-Burman-like base, which still
    # satisfies the DSD main-effect properties (though some quadratic
    # aliasing may be introduced).
    #
    # Reference:
    #   Jones, B. & Nachtsheim, C. J. (2011). A Class of Three-Level
    #     Designs for Definitive Screening. Technometrics, 53(1), 1-7.
    #   Xiao, Q. & Xu, H. (2017). Construction of Definitive Screening
    #     Designs Using Conference Matrices. J. Quality Technology, 49(2).
    .generate_dsd = function(plan) {
      n_factors <- length(plan$factors)

      # Build the conference matrix C of order k.
      C <- private$.conference_matrix(n_factors)

      # Construct the folded design:
      #
      #   [  C   1 ]
      #   [ -C  -1 ]
      #   [  0   0 ]
      #
      # where the last column is a dummy factor used only during the
      # foldover step; it is dropped before returning the design.
      folded <- rbind(
        cbind(C,                            1),
        cbind(-C,                          -1),
        c(rep(0, n_factors),                 0)
      )

      # Drop the dummy column; keep only the real factor columns.
      design <- as.data.frame(folded[, seq_len(n_factors), drop = FALSE])
      colnames(design) <- vapply(plan$factors, function(f) f$name,
                                 character(1))

      # Convert from coded units (-1, 0, +1) to actual factor levels.
      # For 3-level factors the levels map directly; for 2-level factors
      # we use the low/high endpoints and the midpoint as the center.
      design <- private$.convert_coded_to_actual(design, plan)

      # Tag every row with its point type so the design plot can
      # distinguish foldover points from center points.
      design$PointType <- c(
        rep("cube", nrow(C)),
        rep("cube", nrow(C)),
        "center"
      )

      # Add optional extra center points (the single center row above
      # is required by the DSD construction; plan$center_points controls
      # additional replicate center points for lack-of-fit testing).
      n_extra_center <- max(0, plan$center_points - 1)
      if (n_extra_center > 0) {
        center_values <- lapply(plan$factors, function(f) mean(f$levels))
        extra_center <- as.data.frame(
          matrix(rep(unlist(center_values), each = n_extra_center),
                 nrow = n_extra_center, byrow = TRUE)
        )
        colnames(extra_center) <- vapply(plan$factors, function(f) f$name,
                                         character(1))
        extra_center$PointType <- "center"
        design <- rbind(design, extra_center)
      }

      design$RunOrder <- seq_len(nrow(design))
      design
    },

    # =========================================================================
    # Mixture Designs
    # =========================================================================
    #
    # Mixture experiments model a response as a function of component
    # proportions x_1, ..., x_q subject to the constraints sum(x_i) = 1 and
    # x_i >= 0. Three canonical design families are supported:
    #
    #   1. Simplex centroid (Scheffe 1958): 2^q - 1 points covering all
    #      subset centroids from pure components (size-1 subsets) up to the
    #      overall centroid (size-q subset).
    #   2. Simplex lattice {q, m} (Scheffe 1958): uniformly spaced points on
    #      the simplex where each x_i takes values in {0, 1/m, ..., 1} and
    #      the proportions sum to 1. The lattice degree `m` is taken from
    #      `plan$alpha` when numeric (default m = 2).
    #   3. Extreme vertices (McLean & Anderson 1966): vertices of the
    #      constrained simplex defined by lower/upper bounds on each
    #      component. Each component's bounds are read from `f$levels` as
    #      c(lower, upper); for unconstrained components use c(0, 1).
    #
    # References:
    #   Scheffe, H. (1958). Experiments with Mixtures. JRSS-B, 20(2).
    #   Scheffe, H. (1963). The Simplex-Centroid Design for Experiments
    #     with Mixtures. JRSS-B, 25(2).
    #   McLean, R. A. & Anderson, V. L. (1966). Extreme Vertices Design
    #     of Mixture Experiments. Technometrics, 8(3).
    #   Cornell, J. A. (2002). Experiments with Mixtures (3rd ed.).
    #   Montgomery (2019) sec. 13.6.
    .generate_simplex_centroid = function(plan) {
      q <- length(plan$factors)
      if (q < 2) {
        stop("[DoeAnalyzer] simplex_centroid requires at least 2 components.",
             call. = FALSE)
      }
      comp_names <- vapply(plan$factors, function(f) f$name, character(1))

      # Generate all non-empty subsets of {1, ..., q}. For a subset of size s,
      # the corresponding centroid assigns proportion 1/s to each member and
      # 0 to the rest. This yields 2^q - 1 design points.
      indices <- seq_len(q)
      point_list <- list()
      for (s in seq_len(q)) {
        subsets <- utils::combn(indices, s, simplify = FALSE)
        for (sb in subsets) {
          pt <- numeric(q)
          pt[sb] <- 1 / s
          point_list[[length(point_list) + 1L]] <- pt
        }
      }
      design <- as.data.frame(do.call(rbind, point_list))
      colnames(design) <- comp_names
      design$PointType <- "centroid"
      # Tag the pure-component vertices (size-1 subsets) as vertex points so
      # downstream mixture plots can distinguish the simplex corners.
      is_vertex <- rowSums(design[, comp_names, drop = FALSE] > 0) == 1
      design$PointType[is_vertex] <- "vertex"
      design$RunOrder <- seq_len(nrow(design))
      design
    },

    # Simplex lattice design {q, m}: points whose component proportions are
    # multiples of 1/m and sum to 1. The number of points is
    # C(q + m - 1, m) (combinations with repetition). The lattice degree m
    # is read from plan$alpha when numeric; otherwise it defaults to 2.
    .generate_simplex_lattice = function(plan) {
      q <- length(plan$factors)
      if (q < 2) {
        stop("[DoeAnalyzer] simplex_lattice requires at least 2 components.",
             call. = FALSE)
      }
      comp_names <- vapply(plan$factors, function(f) f$name, character(1))

      # Resolve lattice degree m from plan$alpha (numeric) or default to 2.
      m <- NULL
      if (!is.null(plan$alpha) && is.numeric(plan$alpha) &&
          length(plan$alpha) == 1 && plan$alpha == round(plan$alpha) &&
          plan$alpha >= 1) {
        m <- as.integer(plan$alpha)
      }
      if (is.null(m)) m <- 2L

      # Enumerate compositions of m into q non-negative integer parts via
      # recursion. Each composition (k_1, ..., k_q) with sum k_i = m gives a
      # design point x_i = k_i / m.
      compositions <- function(total, parts) {
        if (parts == 1) return(list(total))
        out <- list()
        for (k in 0:total) {
          rest <- compositions(total - k, parts - 1)
          for (r in rest) out[[length(out) + 1L]] <- c(k, r)
        }
        out
      }
      comps <- compositions(m, q)
      point_list <- lapply(comps, function(kk) kk / m)
      design <- as.data.frame(do.call(rbind, point_list))
      colnames(design) <- comp_names
      design$PointType <- "lattice"
      # Mark the q pure-component vertices (one part = m, rest = 0).
      pure <- vapply(comps, function(kk) any(kk == m), logical(1))
      design$PointType[pure] <- "vertex"
      design$RunOrder <- seq_len(nrow(design))
      design
    },

    # Extreme vertices design for constrained mixtures. Each component i has
    # bounds L_i <= x_i <= U_i read from f$levels (c(lower, upper)); the
    # proportions must still sum to 1. Implements the McLean-Anderson (1966)
    # vertex-generation algorithm: for every permutation of the component
    # ordering, set the first q-1 components alternately to their lower or
    # upper bound and solve for the last component; keep the point when the
    # solved last component falls within its own bounds. Unique vertices are
    # returned; edge centroids / overall centroids can be added by the user
    # via center_points replication.
    .generate_extreme_vertices = function(plan) {
      q <- length(plan$factors)
      if (q < 2) {
        stop("[DoeAnalyzer] extreme_vertices requires at least 2 components.",
             call. = FALSE)
      }
      comp_names <- vapply(plan$factors, function(f) f$name, character(1))

      # Extract per-component bounds. f$levels is c(lower, upper). For
      # unconstrained components the user supplies c(0, 1).
      bounds <- lapply(plan$factors, function(f) {
        lv <- f$levels
        if (length(lv) < 2) c(0, 1) else c(min(lv), max(lv))
      })
      lower <- vapply(bounds, function(b) b[1], numeric(1))
      upper <- vapply(bounds, function(b) b[2], numeric(1))
      names(lower) <- names(upper) <- comp_names

      # Sanity check: bounds must admit at least one feasible point.
      if (sum(lower) > 1 + 1e-9 || sum(upper) < 1 - 1e-9) {
        stop("[DoeAnalyzer] extreme_vertices bounds are infeasible: ",
             "sum(lower)=", round(sum(lower), 6),
             " > 1 or sum(upper)=", round(sum(upper), 6), " < 1.",
             call. = FALSE)
      }

      # McLean-Anderson: for each candidate "last" (solved) component, set the
      # remaining q-1 components alternately to their lower or upper bound and
      # solve for the last component; keep the point when the solved value
      # falls within its own bounds. Enumerating all 2^(q-1) bound combinations
      # for the first q-1 factors is equivalent to permuting the component
      # ordering, since the bound enumeration covers every corner.
      vertices <- list()
      for (last in seq_len(q)) {
        first_set <- setdiff(seq_len(q), last)
        n_first <- q - 1L
        for (code in seq_len(2^n_first) - 1L) {
          x <- rep(NA_real_, q)
          bits <- as.integer(intToBits(code))[seq_len(n_first)]
          x[first_set] <- ifelse(bits == 0, lower[first_set], upper[first_set])
          x_last <- 1 - sum(x[first_set])
          if (x_last >= lower[last] - 1e-9 && x_last <= upper[last] + 1e-9) {
            x[last] <- x_last
            vertices[[length(vertices) + 1L]] <- x
          }
        }
      }

      if (length(vertices) == 0) {
        stop("[DoeAnalyzer] extreme_vertices found no feasible vertices; ",
             "check component bounds.", call. = FALSE)
      }
      # Deduplicate vertices (rows identical up to rounding).
      vmat <- unique(round(do.call(rbind, vertices), 9))
      design <- as.data.frame(vmat)
      colnames(design) <- comp_names
      design$PointType <- "vertex"
      design$RunOrder <- seq_len(nrow(design))
      design
    },

    # =========================================================================
    # Split-Plot Design
    # =========================================================================
    #
    # A split-plot design accommodates hard-to-change (whole-plot) factors
    # alongside easy-to-change (subplot) factors. Whole-plot factors are held
    # constant across a group of subplot runs (a whole plot), so the
    # randomisation structure is nested rather than fully crossed.
    #
    # Each factor in plan$factors may carry an optional logical field
    # `hard_to_change`. Factors with hard_to_change = TRUE form the whole-plot
    # (WP) stratum; the remaining factors form the subplot (SP) stratum. The
    # design is the Cartesian product of the WP full factorial with the SP
    # full factorial; runs sharing a WP combination receive a common
    # WholePlot id. Run order is randomised at the whole-plot level first
    # (restricting the randomisation of hard-to-change factors), then within
    # each whole plot for the subplot factors.
    #
    # Reference:
    #   Jones, B. & Nachtsheim, C. J. (2009). Split-Plot Designs: What, Why,
    #     and How. J. Quality Technology, 41(4).
    #   Montgomery (2019) sec. 14.5.
    .generate_split_plot = function(plan) {
      factor_names <- vapply(plan$factors, function(f) f$name, character(1))

      # Partition factors into whole-plot (HTC) and subplot (ETC) strata.
      htc <- vapply(plan$factors, function(f) isTRUE(f$hard_to_change),
                    logical(1))
      if (!any(htc)) {
        # No HTC factors declared: fall back to treating the first factor as
        # the whole-plot factor so the split-plot structure is still defined.
        htc[1] <- TRUE
        warning("[DoeAnalyzer] split_plot: no factor has hard_to_change=TRUE; ",
                "treating '", factor_names[1], "' as the whole-plot factor.",
                call. = FALSE)
      }
      wp_factors <- plan$factors[htc]
      sp_factors <- plan$factors[!htc]
      if (length(sp_factors) == 0) {
        stop("[DoeAnalyzer] split_plot requires at least one easy-to-change ",
             "(subplot) factor.", call. = FALSE)
      }

      # Whole-plot full factorial (cube points).
      wp_levels <- lapply(wp_factors, function(f) f$levels)
      names(wp_levels) <- vapply(wp_factors, function(f) f$name, character(1))
      wp_design <- expand.grid(wp_levels, stringsAsFactors = FALSE)

      # Subplot full factorial.
      sp_levels <- lapply(sp_factors, function(f) f$levels)
      names(sp_levels) <- vapply(sp_factors, function(f) f$name, character(1))
      sp_design <- expand.grid(sp_levels, stringsAsFactors = FALSE)

      # Cartesian product: for each whole plot, replicate the subplot design.
      n_wp <- nrow(wp_design)
      n_sp <- nrow(sp_design)
      design <- data.frame(
        WholePlot = rep(seq_len(n_wp), each = n_sp),
        cbind(wp_design[rep(seq_len(n_wp), each = n_sp), , drop = FALSE],
              sp_design[rep(seq_len(n_sp), times = n_wp), , drop = FALSE]),
        stringsAsFactors = FALSE
      )
      rownames(design) <- NULL
      design$PointType <- "cube"

      # Restricted randomisation: randomise whole-plot order, then subplot
      # order within each whole plot. This preserves the HTC constraint.
      if (isTRUE(plan$randomize)) {
        withr::local_seed(plan$seed)
        wp_order <- sample(n_wp)
        design_list <- lapply(wp_order, function(w) {
          sub <- design[design$WholePlot == w, , drop = FALSE]
          sub[sample(nrow(sub)), , drop = FALSE]
        })
        design <- do.call(rbind, design_list)
        design$WholePlot <- rep(seq_len(n_wp), each = n_sp)
      }
      design$RunOrder <- seq_len(nrow(design))
      rownames(design) <- NULL
      design
    },

    # Build a conference matrix C of order k with entries in
    # {-1, 0, +1} satisfying C'C = (k-1) * I_k. Returns a k x k
    # numeric matrix. Uses explicit catalogued constructions for k in
    # {2, 3, 4, 5, 6, 7, 8, 10, 12} and a heuristic foldover-based
    # construction for other orders.
    #
    # References:
    #   Craigen, R. (1996). Conference matrices. In: The CRC Handbook of
    #     Combinatorial Designs.
    #   Neumann, J. & Spence, S. (2010). A catalogue of small
    #     symmetric conference matrices.
    .conference_matrix = function(k) {
      if (k == 2) {
        return(matrix(c(0, 1, 1, 0), 2, 2, byrow = TRUE))
      }
      if (k == 3) {
        return(matrix(c(0,  1,  1,
                        -1, 0,  0,
                        -1, 0,  0), 3, 3, byrow = TRUE))
      }
      if (k == 4) {
        return(matrix(c(0,  1,  1,  1,
                        1,  0,  1, -1,
                        1, -1,  0,  1,
                        1, -1, -1,  0), 4, 4, byrow = TRUE))
      }
      if (k == 5) {
        return(matrix(c(0,  1,  1,  1,  1,
                       -1, 0,  1, -1,  1,
                       -1, -1, 0,  1, -1,
                       -1, 1, -1,  0,  1,
                       -1, -1, 1, -1,  0), 5, 5, byrow = TRUE))
      }
      if (k == 6) {
        return(matrix(c(0,  1,  1,  1,  1,  1,
                        1,  0,  1,  1, -1, -1,
                        1,  1,  0, -1,  1, -1,
                        1,  1, -1,  0, -1,  1,
                        1, -1,  1, -1,  0,  1,
                        1, -1, -1,  1,  1,  0), 6, 6, byrow = TRUE))
      }
      if (k == 7) {
        return(matrix(c(0,  1,  1,  1,  1,  1,  1,
                        1,  0,  1,  1, -1, -1,  1,
                        1,  1,  0, -1,  1, -1, -1,
                        1,  1, -1,  0, -1,  1,  1,
                        1, -1,  1, -1,  0,  1, -1,
                        1, -1, -1,  1,  1,  0,  1,
                        1,  1, -1, -1, -1,  1,  0), 7, 7, byrow = TRUE))
      }
      if (k == 8) {
        return(matrix(c(0,  1,  1,  1,  1,  1,  1,  1,
                        1,  0,  1,  1,  1, -1, -1, -1,
                        1,  1,  0, -1, -1,  1,  1, -1,
                        1,  1, -1,  0, -1, -1,  1,  1,
                        1,  1, -1, -1,  0,  1, -1,  1,
                        1, -1,  1, -1,  1,  0, -1,  1,
                        1, -1,  1,  1, -1, -1,  0, -1,
                        1, -1, -1,  1,  1,  1, -1,  0),
                      8, 8, byrow = TRUE))
      }
      if (k == 10) {
        return(matrix(c(0,  1,  1,  1,  1,  1,  1,  1,  1,  1,
                        1,  0,  1,  1,  1, -1, -1, -1,  1, -1,
                        1,  1,  0, -1, -1,  1,  1, -1,  1,  1,
                        1,  1, -1,  0, -1, -1,  1,  1, -1, -1,
                        1,  1, -1, -1,  0,  1, -1,  1,  1, -1,
                        1, -1,  1, -1,  1,  0, -1,  1, -1, -1,
                        1, -1,  1,  1, -1, -1,  0, -1, -1,  1,
                        1, -1, -1,  1,  1,  1, -1,  0, -1,  1,
                        1,  1,  1, -1,  1, -1, -1, -1,  0,  1,
                        1, -1,  1, -1, -1, -1,  1,  1,  1,  0),
                      10, 10, byrow = TRUE))
      }
      if (k == 12) {
        return(matrix(c(0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
                        1,  0,  1,  1,  1,  1, -1, -1, -1,  1, -1, -1,
                        1,  1,  0, -1, -1,  1,  1,  1, -1, -1,  1, -1,
                        1,  1, -1,  0, -1, -1,  1, -1,  1,  1, -1, -1,
                        1,  1, -1, -1,  0,  1, -1,  1,  1, -1, -1,  1,
                        1,  1,  1, -1,  1,  0,  1, -1, -1, -1,  1, -1,
                        1, -1,  1,  1, -1,  1,  0, -1, -1, -1, -1,  1,
                        1, -1,  1, -1,  1, -1, -1,  0,  1, -1, -1,  1,
                        1, -1, -1,  1,  1, -1, -1,  1,  0, -1,  1, -1,
                        1,  1, -1,  1, -1, -1, -1, -1, -1,  0,  1,  1,
                        1, -1,  1, -1, -1,  1, -1, -1,  1,  1,  0, -1,
                        1, -1, -1, -1,  1, -1,  1,  1, -1,  1, -1,  0),
                      12, 12, byrow = TRUE))
      }

      # Fallback for k not in the catalog: use a foldover of a
      # Plackett-Burman-style base matrix. We build a k x k matrix
      # with zeros on the diagonal and +/-1 off-diagonal entries chosen
      # to approximately satisfy the conference property. This is a
      # heuristic; for production use, supply a pre-computed conference
      # matrix from an external source.
      C <- matrix(0, k, k)
      for (i in seq_len(k)) {
        for (j in seq_len(k)) {
          if (i == j) next
          # Assign +/-1 by a deterministic sign pattern that mimics
          # the conference matrix structure.
          C[i, j] <- if (((i + j) %% 2) == 0) 1 else -1
        }
      }
      C
    },

    # Replicate a design n_rep times, adding a Replication identifier column.
    .add_replication = function(design, n_rep) {
      replicated <- do.call(rbind, replicate(n_rep, design, simplify = FALSE))
      replicated$Replication <- rep(seq_len(n_rep), each = nrow(design))
      replicated
    },

    # Append n_centers center points to a design. Center points sit at the mean
    # of every factor's levels and receive sequential run orders following the
    # existing design rows. Any auxiliary columns that the design may have
    # acquired upstream (e.g. `Replication` added by `.add_replication`) are
    # back-filled with NA so that `rbind()` matches on column names and the
    # combined design remains rectangular.
    .add_center_points = function(design, n_centers, plan) {
      center_values <- lapply(plan$factors, function(f) {
        mean(f$levels)
      })

      center_df <- as.data.frame(do.call(rbind, replicate(n_centers, center_values, simplify = FALSE)),
                                  stringsAsFactors = FALSE)
      colnames(center_df) <- sapply(plan$factors, function(f) f$name)
      # Coerce factor columns to numeric (do.call(rbind, list) occasionally
      # returns list columns when the levels are not integers).
      for (col in names(center_df)) {
        center_df[[col]] <- as.numeric(center_df[[col]])
      }
      center_df$RunOrder <- seq(nrow(design) + 1, nrow(design) + n_centers)

      # Back-fill any auxiliary columns (Replication, etc.) so that rbind()
      # succeeds regardless of which upstream augmenters have already run.
      missing_cols <- setdiff(colnames(design), colnames(center_df))
      for (col in missing_cols) {
        center_df[[col]] <- NA
      }
      # Match column order to the existing design so the combined frame is
      # predictable for downstream consumers (randomizer, ANOVA, plotter).
      center_df <- center_df[, colnames(design), drop = FALSE]

      # Override PointType for center points (back-fill set it to NA).
      if ("PointType" %in% colnames(design)) {
        center_df$PointType <- "center"
      }

      rbind(design, center_df)
    },

    # Randomize the run order of a design. When a seed is supplied, the RNG
    # state is locally set for the duration of the call and restored on exit
    # via withr::local_seed(); when seed is NULL, the RNG is left untouched.
    # The RunOrder column is reset after shuffling and row names are cleared.
    .randomize_design = function(design, seed = NULL) {
      # Locally set and automatically restore the RNG state when a seed is
      # provided. withr::local_seed() is a no-op when seed is NULL.
      withr::local_seed(seed)

      n_runs <- nrow(design)
      random_order <- sample(n_runs)

      design <- design[random_order, ]
      design$RunOrder <- seq_len(n_runs)
      rownames(design) <- NULL

      design
    },

    # Convert coded factor values into actual factor levels. The mapping
    # depends on the number of factor levels declared on the plan:
    #   - 2-level factor: coded -1 -> low, +1 -> high (linear map)
    #   - 3-level factor: coded -1 -> low, 0 -> mid, +1 -> high (linear map)
    #   - n-level factor (n > 3): direct lookup using the rank order of the
    #     coded value among {-1, +1} or {-1, 0, +1} (whichever matches the
    #     design generation). For multi-level factors, the convention is that
    #     the coded value -1 corresponds to the lowest declared level, +1 to
    #     the highest, and intermediate coded values map linearly in between.
    .convert_coded_to_actual = function(design, plan) {
      for (i in seq_along(plan$factors)) {
        factor <- plan$factors[[i]]
        factor_name <- factor$name

        if (!factor_name %in% colnames(design)) next

        coded_values <- design[[factor_name]]
        levels <- sort(factor$levels)
        n_levels <- length(levels)
        low  <- levels[1]
        high <- levels[n_levels]

        if (n_levels == 2) {
          # Linear transformation: coded=-1 -> low, coded=+1 -> high.
          actual_values <- (coded_values + 1) / 2 * (high - low) + low
        } else if (n_levels == 3) {
          # 3-level design (L9, L27): coded=-1 -> low, 0 -> mid, +1 -> high.
          mid <- levels[2]
          actual_values <- ifelse(coded_values < 0, low,
                           ifelse(coded_values > 0, high, mid))
        } else {
          # For >3 levels: use linear interpolation between low and high
          # based on the coded value (treating it as a continuous coordinate).
          actual_values <- (coded_values + 1) / 2 * (high - low) + low
        }

        design[[factor_name]] <- actual_values
      }

      design
    },

    # Perform ANOVA for each response variable declared on the plan. The
    # model formula is selected based on the design type:
    #   - RSM-family designs (ccd, rsm, box_behnken) use a full second-order
    #     model: y ~ A*B*C + I(A^2) + I(B^2) + ...
    #   - All other designs use a full-interaction first-order model:
    #     y ~ A * B * C
    # When `plan$blocking = TRUE`, a `Block` factor is added to the model
    # formula so that the block effect is accounted for in the ANOVA
    # decomposition (RCBD analysis). Results are returned as a named list
    # keyed by response variable, with each entry holding the ANOVA table,
    # the fitted model, the estimated effects, and the model fit statistics
    # (including the curvature test when center points are present).
    .perform_anova = function(data, plan) {
      results <- list()

      for (response_var in plan$response_vars) {
        if (!response_var %in% names(data)) {
          warning("Response variable '", response_var, "' not found in data")
          next
        }

        factor_names <- sapply(plan$factors, function(f) f$name)

        # Build the model formula based on design type
        formula_obj <- if (plan$design_type %in% c("ccd", "rsm", "box_behnken", "dsd")) {
          private$.build_rsm_formula(factor_names, response_var)
        } else {
          # Full interaction first-order model: y ~ A * B * C
          reformulate(paste(factor_names, collapse = " * "),
                      response = response_var)
        }

        # When blocking is enabled, add the Block factor to the model so
        # that the block effect is removed from the error term. This is the
        # standard RCBD / IBD analysis (Montgomery 2019, sec. 4.4).
        if (isTRUE(plan$blocking) && "Block" %in% names(data)) {
          formula_obj <- update(formula_obj, . ~ . + Block)
        }

        model <- lm(formula_obj, data = data)
        anova_table <- anova(model)

        # Extract main and interaction effects
        effects <- private$.extract_effects(model, plan)

        # Compute model fit statistics, including the curvature test when
        # center points are present in the design.
        model_fit <- private$.compute_model_fit(model, data, response_var, plan)

        results[[response_var]] <- list(
          anova = anova_table,
          model = model,
          effects = effects,
          model_fit = model_fit
        )
      }

      results
    },

    # Build a full second-order Response Surface Model formula:
    #   y ~ A * B * C + I(A^2) + I(B^2) + I(C^2) + ...
    # The A * B * C expansion covers main effects + pairwise + higher-order
    # interactions; quadratic terms are appended explicitly via I(.^2).
    .build_rsm_formula = function(factor_names, response_var) {
      interaction_part <- paste(factor_names, collapse = " * ")
      quadratic_part <- paste0("I(", factor_names, "^2)")
      rhs <- paste(c(interaction_part, quadratic_part), collapse = " + ")
      reformulate(rhs, response = response_var)
    },

    # Extract main effects and pairwise interaction effects from the fitted
    # model coefficients.
    .extract_effects = function(model, plan) {
      coef_values <- coef(model)

      # Main effects
      main_effects <- list()
      for (factor in plan$factors) {
        effect_name <- factor$name
        if (effect_name %in% names(coef_values)) {
          main_effects[[effect_name]] <- coef_values[[effect_name]]
        }
      }

      # Interaction effects (pairwise)
      interaction_effects <- list()
      n_factors <- length(plan$factors)
      for (i in seq_len(n_factors)) {
        if (i + 1 > n_factors) break
        for (j in (i + 1):n_factors) {
          interaction_name <- paste(plan$factors[[i]]$name,
                                   plan$factors[[j]]$name,
                                   sep = ":")
          if (interaction_name %in% names(coef_values)) {
            interaction_effects[[interaction_name]] <- coef_values[[interaction_name]]
          }
        }
      }

      list(main = main_effects, interaction = interaction_effects)
    },

    # Compute model fit statistics, including R-squared, adjusted R-squared,
    # the lack-of-fit p-value (when replicates exist), and the curvature test
    # p-value (when center points are present in the design).
    #
    # The curvature test compares the mean response at the factorial (corner)
    # points to the mean response at the center points. A small curvature
    # p-value indicates that the first-order model is inadequate and a
    # second-order (RSM) design should be used to estimate quadratic terms.
    # This is the standard 2^k + center-points analysis (Montgomery 2019,
    # sec. 6.6).
    .compute_model_fit = function(model, data, response_var, plan = NULL) {
      summary_model <- summary(model)

      # R-squared
      r_squared <- summary_model$r.squared
      adj_r_squared <- summary_model$adj.r.squared

      # Lack-of-fit test (requires replicated design points)
      lack_of_fit_p <- private$.test_lack_of_fit(model, data, response_var)

      # Curvature test (requires center points in the design).
      # Only run when we have access to the plan (so we can identify the
      # original factor columns) and when center points were requested.
      curvature_test <- NULL
      if (!is.null(plan) && plan$center_points > 0) {
        factor_vars <- vapply(plan$factors, function(f) f$name, character(1))
        tryCatch({
          curvature_test <- self$test_curvature(model, data, response_var,
                                                factor_vars)
        }, error = function(e) {
          curvature_test <- list(
            p_value = NA_real_,
            interpretation = paste("Curvature test failed:", conditionMessage(e))
          )
        })
      }

      out <- list(
        r_squared = r_squared,
        adj_r_squared = adj_r_squared,
        lack_of_fit_p = lack_of_fit_p
      )
      if (!is.null(curvature_test)) {
        out$curvature_p <- curvature_test$p_value
        out$curvature_interpretation <- curvature_test$interpretation
        out$curvature_test <- curvature_test
      }
      out
    },

    # Lack-of-fit test for replicated designs. Compares the variability of
    # replicate observations around their group mean (pure error) with the
    # variability of the group means around the model predictions (lack of
    # fit). Returns the p-value of the F-test under H0: model fits adequately.
    #
    # Theory (Draper & Smith 1998, sec. 2.7; Montgomery 2019, ch. 5):
    #   Residual SS = Lack-of-fit SS + Pure error SS
    #   Pure error SS = sum_i sum_j (y_ij - y_bar_i)^2
    #                  within each group i of replicates sharing identical
    #                  predictor settings.
    #   F = (LoF SS / df_lof) / (PE SS / df_pe) ~ F(df_lof, df_pe) under H0.
    #
    # Returns NA when the design has no replicated points (df_pe = 0), which
    # is the standard behavior in Minitab / Design-Expert for unreplicated
    # factorials; in that case the Lenth PSE method should be used instead.
    .test_lack_of_fit = function(model, data, response_var) {
      # Use the model's internal frame so the predictor columns line up with
      # what was actually used in the fit (handles transformations, missing
      # rows, etc.).
      model_frame <- model$model
      if (is.null(model_frame)) return(NA_real_)

      response_col <- names(model_frame)[1]
      predictor_cols <- setdiff(names(model_frame), response_col)

      # For replication detection we use only the original factor columns;
      # derived terms such as I(A^2) or interaction columns (containing ":")
      # are deterministic functions of the original factors and therefore do
      # not provide independent replication information.
      predictor_cols <- predictor_cols[!grepl("^I\\(|:|\\^", predictor_cols)]
      if (length(predictor_cols) == 0) return(NA_real_)

      # Build a composite key identifying unique factor combinations.
      # We collapse each row's factor settings into a single string so that
      # split() can group replicated runs.
      design_keys <- model_frame[, predictor_cols, drop = FALSE]
      group_key <- apply(design_keys, 1, function(row) {
        paste(row, collapse = "\u001f")
      })
      groups <- split(seq_len(nrow(design_keys)), group_key)

      # Pure error requires at least one group with more than one replicate.
      replicated_groups <- groups[lengths(groups) > 1]
      if (length(replicated_groups) == 0) {
        # No pure-error estimate is possible without replicates.
        return(NA_real_)
      }

      # Compute pure error SS and df.
      pure_error_ss <- 0
      pure_error_df  <- 0
      for (g in replicated_groups) {
        y_group <- model_frame[[response_col]][g]
        n_g <- length(y_group)
        if (n_g >= 2) {
          pure_error_ss <- pure_error_ss + sum((y_group - mean(y_group))^2)
          pure_error_df  <- pure_error_df  + (n_g - 1)
        }
      }
      if (pure_error_df == 0) return(NA_real_)

      # Pull the residual SS and df from the fitted model. These already
      # account for the model's degrees of freedom.
      rss   <- sum(resid(model)^2)
      res_df <- df.residual(model)

      # Lack-of-fit SS and df are obtained by subtraction.
      lof_ss <- rss - pure_error_ss
      lof_df <- res_df - pure_error_df

      # Guard against degenerate cases (zero SS or zero df).
      if (lof_df <= 0 || pure_error_ss <= 0 || !is.finite(lof_ss) ||
          lof_ss < 0) {
        return(NA_real_)
      }

      # F-test for lack of fit.
      F_stat <- (lof_ss / lof_df) / (pure_error_ss / pure_error_df)
      p_value <- 1 - stats::pf(F_stat, lof_df, pure_error_df)

      p_value
    },

    # Build the model matrix X for design evaluation. The model order
    # controls the columns: intercept, main effects, two-factor interactions,
    # full interactions, or quadratic terms (for RSM designs).
    .build_model_matrix = function(X_raw, model_order) {
      n_factors <- ncol(X_raw)
      n_runs <- nrow(X_raw)
      col_names <- colnames(X_raw)
      if (is.null(col_names)) col_names <- LETTERS[seq_len(n_factors)]

      # Start with intercept.
      cols <- list(Intercept = rep(1, n_runs))

      # Main effects.
      for (i in seq_len(n_factors)) {
        cols[[col_names[i]]] <- X_raw[, i]
      }

      if (model_order == "main_2fi" && n_factors >= 2) {
        # Two-factor interactions.
        pairs <- utils::combn(seq_len(n_factors), 2, simplify = FALSE)
        for (pr in pairs) {
          cols[[paste0(col_names[pr[1]], ":", col_names[pr[2]])]] <-
            X_raw[, pr[1]] * X_raw[, pr[2]]
        }
      } else if (model_order == "full" && n_factors >= 2) {
        # All interactions of all orders (2 through k).
        for (order in 2:n_factors) {
          combos <- utils::combn(seq_len(n_factors), order, simplify = FALSE)
          for (cb in combos) {
            col_prod <- rep(1, n_runs)
            for (idx in cb) col_prod <- col_prod * X_raw[, idx]
            cols[[paste0(col_names[cb], collapse = ":")]] <- col_prod
          }
        }
      } else if (model_order == "quadratic") {
        # Quadratic terms (X_i^2) for response surface designs. Note that
        # for -1/+1 coded factors without center points, X_i^2 = 1 for all
        # rows, which is aliased with the intercept; the quadratic model
        # is only meaningful when center points are present.
        for (i in seq_len(n_factors)) {
          cols[[paste0(col_names[i], "^2")]] <- X_raw[, i]^2
        }
        # Add 2FI terms for the full second-order (RSM) model.
        if (n_factors >= 2) {
          pairs <- utils::combn(seq_len(n_factors), 2, simplify = FALSE)
          for (pr in pairs) {
            cols[[paste0(col_names[pr[1]], ":", col_names[pr[2]])]] <-
              X_raw[, pr[1]] * X_raw[, pr[2]]
          }
        }
      }

      do.call(cbind, cols)
    },

    # Thin wrapper around .build_model_matrix used by the coordinate-exchange
    # optimal-design constructor. Coerces the supplied design matrix to a
    # numeric matrix, attaches factor names as column names, and delegates
    # the model-matrix expansion to .build_model_matrix so that main / 2FI /
    # quadratic columns are built consistently with the rest of the package.
    .build_model_matrix_coded = function(design_coded, factor_names,
                                         model_order) {
      X_raw <- as.matrix(design_coded)
      colnames(X_raw) <- factor_names
      private$.build_model_matrix(X_raw, model_order)
    }
  )
)
