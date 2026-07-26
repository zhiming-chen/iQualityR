# =============================================================================
# File: R/Type1LinearityAnalyzer.R
# Description: Gage Linearity & Bias Study Analyzer (Multi Reference Values)
#
# Formulas follow Minitab "Gage Linearity and Bias Study" methods and
# formulas (AIAG MSA Reference Manual, 4th edition, Chapter III, Section B):
#   * Regression is fit on ALL individual biases y_ij (not per-reference means)
#   * Linearity = |slope| * Process Variation
#   * %Linearity = Linearity / Process Variation * 100 = |slope| * 100
#   * %Bias = 100 * |average bias| / Process Variation
#   * Per-reference bias t-test (sample standard deviation method, df = n_r - 1)
# =============================================================================

#' @title Gage Linearity & Bias Study Analyzer
#' @description
#' Analyzer for multiple reference values Linearity and Bias Study.
#'
#' The analysis pipeline is exposed as discrete public methods:
#'   1. [compute_per_reference_summary()] - per-reference statistics & bias tests
#'   2. [fit_linearity_regression()]      - lm(bias ~ reference) on individual biases
#'   3. [evaluate_linearity_criteria()]   - pass/fail diagnostics
#'
#' Formulas follow the Minitab *Gage Linearity and Bias Study* methods and
#' formulas documentation (AIAG MSA Reference Manual, 4th edition, Chapter
#' III, Section B). Key points:
#'   - The linearity regression is fit on **all individual measurement
#'     biases** y_ij (not on the per-reference mean biases). This preserves
#'     the within-reference variation in the regression's residual, R-squared,
#'     and slope standard error.
#'   - `Linearity = |slope| * Process Variation`.
#'   - `%Linearity = Linearity / Process Variation * 100 = |slope| * 100`.
#'   - `%Bias = 100 * |average bias| / Process Variation`.
#'   - `Process Variation` must be supplied by the user (typically 6*sigma
#'     from a capability study or historical data). If absent, the analyzer
#'     falls back to `6 * sd(all_measurements)` with a warning.
#'
#' The inherited [IqrAnalyzerBase$run()] method orchestrates the full pipeline
#' via `private$.run_logic()`.
#'
#' @export
Type1LinearityAnalyzer <- R6::R6Class("Type1LinearityAnalyzer",
  inherit = IqrAnalyzerBase,

  public = list(

    #' @description
    #' Compute per-reference summary statistics and per-reference bias tests.
    #'
    #' For each distinct reference value, calculates count, mean, standard
    #' deviation, bias (mean - reference), and a two-sided t-test of
    #' H0: bias_r = 0 (sample standard deviation method, df = n_r - 1).
    #' Also derives aggregate metrics (average bias, max absolute bias,
    #' percent-of-process-variation values, average standard deviation and
    #' CV). The per-reference table is stored in
    #' `self$results$data_tables$ref_summary`; the individual-bias long table
    #' is stored in `self$results$data_tables$bias_long` for the regression
    #' step and for plotting.
    #'
    #' @param dt Data frame or data.table with `reference` and `measurement`
    #'   columns.
    #' @return A named list of per-reference and aggregate statistics
    #'   (invisibly).
    compute_per_reference_summary = function(dt) {
      params     <- self$params
      ref_vals   <- params$reference_values
      spec       <- params$spec_limits
      # Tolerance resolved by Type1Plan from multiple input modes
      tolerance  <- params$tolerance
      if (is.null(tolerance)) {
        tolerance <- spec$usl - spec$lsl
      }
      conf_level <- params$conf_level

      if (!"reference" %in% names(dt) || !"measurement" %in% names(dt)) {
        stop("[Type1LinearityAnalyzer] Data must have 'reference' and 'measurement' columns.",
             call. = FALSE)
      }

      # --- Resolve Process Variation ---------------------------------------
      # MSA 4th ed. requires Process Variation (PV) to be supplied by the
      # user (typically 6*sigma from a capability study or historical data).
      # We support four input modes:
      #   params$process_variation = numeric scalar          -> use as-is
      #   params$process_variation = "from_study"            -> 6 * s_pooled(within-ref)
      #   params$process_variation = "from_historical_sigma" -> 6 * historical_sd
      #   params$process_variation = NULL                    -> degrade to "from_study"
      # E3 degradation strategy: NULL degrades to "from_study" with a warning
      # rather than hard-stopping, so the analysis still produces results.
      pv_input <- params$process_variation
      historical_sd <- params$historical_sd
      if (is.null(pv_input)) {
        warning("[Type1LinearityAnalyzer] process_variation is NULL; degrading ",
                "to 'from_study' (6*s_pooled within-reference). For publishable ",
                "results, supply an explicit Process Variation from a capability ",
                "study or historical data.", call. = FALSE)
        pv_input <- "from_study"
      }
      if (is.character(pv_input) && pv_input == "from_study") {
        # Estimate PV from within-reference variation only (pooled sd),
        # NOT sd(all measurements) which would include process variation.
        # This is a fallback for when no historical capability data exists.
        n_r <- dt[, .N, by = reference]$N
        s_r <- dt[, stats::sd(measurement), by = reference]$V1
        df_within <- sum(n_r - 1)
        s_pooled <- sqrt(sum((n_r - 1) * s_r^2) / df_within)
        process_variation <- 6 * s_pooled
        warning("[Type1LinearityAnalyzer] process_variation = 'from_study': ",
                "using 6 * s_pooled(within-reference) = ",
                round(process_variation, 6),
                " as a fallback. For publishable results, supply an explicit ",
                "Process Variation from a capability study or historical data.",
                call. = FALSE)
      } else if (is.character(pv_input) && pv_input == "from_historical_sigma") {
        if (is.null(historical_sd) || !is.numeric(historical_sd) ||
            length(historical_sd) != 1 || historical_sd <= 0) {
          stop("[Type1LinearityAnalyzer] process_variation = 'from_historical_sigma' ",
               "requires a positive numeric historical_sd.", call. = FALSE)
        }
        process_variation <- 6 * historical_sd
        message("[Type1LinearityAnalyzer] process_variation = 6 * historical_sd = ",
                round(process_variation, 6), ".")
      } else {
        process_variation <- as.numeric(pv_input)
        if (length(process_variation) != 1 || !is.finite(process_variation) ||
            process_variation <= 0) {
          stop("[Type1LinearityAnalyzer] process_variation must be a positive ",
               "numeric scalar, 'from_study', or 'from_historical_sigma'.",
               call. = FALSE)
        }
      }

      # --- Per-reference summary with bias t-test --------------------------
      # Sample standard deviation method (Minitab default for the bias test
      # when sample range is not requested): t = mean_bias / (sd/sqrt(n)),
      # df = n_r - 1.
      # Also evaluates per-reference run-chart stability (MSA 4th ed.
      # requires each reference's measurement process to be in control).
      ref_summary <- dt[, {
        n_r    <- .N
        m_r    <- mean(measurement)
        s_r    <- stats::sd(measurement)
        b_r    <- m_r - reference[1]
        se_r   <- s_r / sqrt(n_r)
        t_r    <- b_r / se_r
        df_r   <- n_r - 1
        p_r    <- 2 * stats::pt(abs(t_r), df = df_r, lower.tail = FALSE)
        t_crit <- stats::qt(1 - (1 - conf_level) / 2, df = df_r)
        ci_r   <- b_r + c(-1, 1) * t_crit * se_r
        # Per-reference stability (Nelson R1/R3 simplifications)
        z_r    <- (measurement - m_r) / s_r
        beyond_3s <- any(abs(z_r) > 3, na.rm = TRUE)
        trend_6   <- .msa_has_trend(measurement, 6)
        stab_ok   <- !beyond_3s && !trend_6
        list(n = n_r, mean = m_r, sd = s_r, bias = b_r,
             se_bias = se_r, t_stat = t_r, df = df_r, p_value = p_r,
             ci_lower = ci_r[1], ci_upper = ci_r[2],
             stability_beyond_3s = beyond_3s,
             stability_trend_6 = trend_6,
             stability_ok = stab_ok)
      }, by = reference]

      # --- Individual-bias long table (for regression & plotting) ----------
      bias_long <- data.table::copy(dt)
      bias_long[, bias := measurement - reference]

      # --- Aggregate metrics (denominator = Process Variation) -------------
      avg_bias          <- mean(bias_long$bias)
      max_bias          <- max(abs(ref_summary$bias))
      percent_avg_bias  <- (abs(avg_bias) / process_variation) * 100
      percent_max_bias  <- (max_bias / process_variation) * 100
      avg_sd            <- mean(ref_summary$sd)
      avg_cv            <- mean(ref_summary$sd / ref_summary$mean)

      # --- Average bias t-test (across all individual biases) --------------
      n_total     <- nrow(bias_long)
      se_avg      <- stats::sd(bias_long$bias) / sqrt(n_total)
      t_avg       <- avg_bias / se_avg
      df_avg      <- n_total - 1
      p_avg       <- 2 * stats::pt(abs(t_avg), df = df_avg, lower.tail = FALSE)
      t_crit_avg  <- stats::qt(1 - (1 - conf_level) / 2, df = df_avg)
      ci_avg      <- avg_bias + c(-1, 1) * t_crit_avg * se_avg

      self$set_statistic("n_total", n_total)
      self$set_statistic("n_ref_points", length(ref_vals))
      self$set_statistic("spec_limits", spec)
      self$set_statistic("tolerance", tolerance)
      self$set_statistic("process_variation", process_variation)
      self$set_statistic("avg_bias", avg_bias)
      self$set_statistic("max_bias", max_bias)
      self$set_statistic("percent_avg_bias", percent_avg_bias)
      self$set_statistic("percent_max_bias", percent_max_bias)
      self$set_statistic("avg_sd", avg_sd)
      self$set_statistic("avg_cv", avg_cv)
      # Average bias test (single overall test, reported by Minitab)
      self$set_statistic("avg_bias_se", se_avg)
      self$set_statistic("avg_bias_t", t_avg)
      self$set_statistic("avg_bias_df", df_avg)
      self$set_statistic("avg_bias_p", p_avg)
      self$set_statistic("avg_bias_ci", ci_avg)
      self$set_datatable("ref_summary", ref_summary)
      self$set_datatable("bias_long", bias_long)

      invisible(list(
        ref_summary        = ref_summary,
        bias_long          = bias_long,
        n_total            = n_total,
        n_ref_points       = length(ref_vals),
        tolerance          = tolerance,
        process_variation  = process_variation,
        avg_bias           = avg_bias,
        max_bias           = max_bias,
        percent_avg_bias   = percent_avg_bias,
        percent_max_bias   = percent_max_bias,
        avg_sd             = avg_sd,
        avg_cv             = avg_cv,
        avg_bias_se        = se_avg,
        avg_bias_t         = t_avg,
        avg_bias_df        = df_avg,
        avg_bias_p         = p_avg,
        avg_bias_ci        = ci_avg
      ))
    },

    #' @description
    #' Fit the linearity regression of bias on reference value.
    #'
    #' Fits `lm(bias ~ reference)` on **all individual measurement biases**
    #' (not on per-reference means), per Minitab's methods and formulas.
    #' Then derives:
    #'   - Linearity = |slope| * Process Variation
    #'   - %Linearity = Linearity / Process Variation * 100 = |slope| * 100
    #'   - Linearity 95% CI from the slope CI: |slope_CI| * PV
    #'   - Slope / Intercept t-tests and p-values
    #'
    #' Requires [compute_per_reference_summary()] to have been called first.
    #'
    #' @return A named list of regression results (invisibly).
    fit_linearity_regression = function() {
      s          <- self$results$statistics
      conf_level <- self$params$conf_level
      bias_long  <- self$results$data_tables$bias_long
      pv         <- s$process_variation

      # --- Regression on individual biases (Minitab spec) ------------------
      lm_model    <- stats::lm(bias ~ reference, data = bias_long)
      lm_summary  <- summary(lm_model)

      intercept     <- unname(stats::coef(lm_model)[1])
      slope         <- unname(stats::coef(lm_model)[2])
      r_squared     <- lm_summary$r.squared
      adj_r_squared <- lm_summary$adj.r.squared
      se_slope      <- unname(lm_summary$coefficients[2, 2])
      t_slope       <- unname(lm_summary$coefficients[2, 3])
      p_slope       <- unname(lm_summary$coefficients[2, 4])
      se_intercept  <- unname(lm_summary$coefficients[1, 2])
      t_intercept   <- unname(lm_summary$coefficients[1, 3])
      p_intercept   <- unname(lm_summary$coefficients[1, 4])
      s_regression  <- lm_summary$sigma  # S: residual std dev of the regression

      df_slope      <- lm_model$df.residual  # n_total - 2
      t_crit        <- stats::qt(1 - (1 - conf_level) / 2, df = df_slope)
      ci_slope      <- slope + c(-1, 1) * t_crit * se_slope

      # --- Linearity and %Linearity (Minitab spec) -------------------------
      # Linearity = |slope| * Process Variation
      # %Linearity = Linearity / Process Variation * 100 = |slope| * 100
      linearity         <- abs(slope) * pv
      percent_linearity <- (linearity / pv) * 100  # equivalent to |slope| * 100

      # Linearity 95% CI: derived from the slope CI.
      # Minitab reports the linearity as a single number; the CI on
      # linearity follows from the CI on slope via Linearity = |slope|*PV.
      # Sort to ensure lower <= upper (abs() can flip the order when the
      # CI straddles zero).
      ci_linearity_raw <- abs(ci_slope) * pv
      ci_linearity <- c(min(ci_linearity_raw), max(ci_linearity_raw))

      self$set_statistic("intercept", intercept)
      self$set_statistic("slope", slope)
      self$set_statistic("se_slope", se_slope)
      self$set_statistic("t_slope", t_slope)
      self$set_statistic("p_slope", p_slope)
      self$set_statistic("df_slope", df_slope)
      self$set_statistic("se_intercept", se_intercept)
      self$set_statistic("t_intercept", t_intercept)
      self$set_statistic("p_intercept", p_intercept)
      self$set_statistic("ci_slope", ci_slope)
      self$set_statistic("r_squared", r_squared)
      self$set_statistic("adj_r_squared", adj_r_squared)
      self$set_statistic("s_regression", s_regression)
      self$set_statistic("linearity", linearity)
      self$set_statistic("percent_linearity", percent_linearity)
      self$set_statistic("ci_linearity", ci_linearity)

      invisible(list(
        lm_model          = lm_model,
        lm_summary        = lm_summary,
        intercept         = intercept,
        slope             = slope,
        se_slope          = se_slope,
        t_slope           = t_slope,
        p_slope           = p_slope,
        df_slope          = df_slope,
        se_intercept      = se_intercept,
        t_intercept       = t_intercept,
        p_intercept       = p_intercept,
        ci_slope          = ci_slope,
        r_squared         = r_squared,
        adj_r_squared     = adj_r_squared,
        s_regression      = s_regression,
        linearity         = linearity,
        percent_linearity = percent_linearity,
        ci_linearity      = ci_linearity
      ))
    },

    #' @description
    #' Compute VDA 5 measurement-system uncertainty budget for the linearity
    #' study (VDA 5, 3rd edition, section 5.3).
    #'
    #' The linearity study is the canonical source of u_LIN (VDA 5 §5.3.4):
    #' the uncertainty contribution due to magnitude-dependent bias. When
    #' the gage's linearity is **not** corrected in production, the full
    #' linearity magnitude contributes as a systematic error modelled by a
    #' rectangular distribution over the linearity range:
    #'
    #'   u_LIN = |Linearity| / sqrt(3) = |slope| * PV / sqrt(3)
    #'
    #' (rectangular, VDA 5 §5.3.4 / GUM 4.3.7). The repeatability component
    #' u_EVR is the **pooled within-reference standard deviation** (root mean
    #' square of the per-reference s_r), representing single-measurement
    #' repeatability across the operating range - NOT s_regression (which
    #' conflates within- and between-reference variation).
    #'
    #' The budget also includes u_RE (resolution), u_BI (overall average
    #' bias), u_CAL, and u_REST, assembled into u_MS and reported as %QMS
    #' against tolerance.
    #'
    #' Requires [compute_per_reference_summary()] and [fit_linearity_regression()]
    #' to have been called first.
    #'
    #' @return A named list of VDA 5 uncertainty results (invisibly).
    compute_vda5 = function() {
      params     <- self$params
      s          <- self$results$statistics
      ref_summary <- self$results$data_tables$ref_summary
      tolerance  <- s$tolerance
      resolution <- params$resolution
      u_cal      <- params$u_cal
      u_rest     <- params$u_rest

      # u_EVR: pooled within-reference repeatability (single-measurement level).
      # s_pooled = sqrt( sum((n_r-1)*s_r^2) / sum(n_r-1) )
      # This isolates pure repeatability, unlike s_regression which also
      # contains between-reference (linearity) variation.
      n_r       <- ref_summary$n
      s_r       <- ref_summary$sd
      df_within <- sum(n_r - 1)
      s_pooled  <- sqrt(sum((n_r - 1) * s_r^2) / df_within)
      u_evr     <- s_pooled

      # u_RE: resolution (rectangular distribution, VDA 5 §5.3.3)
      u_re <- if (is.null(resolution)) 0 else resolution / (2 * sqrt(3))

      # u_BI: uncorrected systematic bias = overall average bias magnitude.
      # (Per-reference biases are evaluated separately in ref_summary.)
      u_bi <- abs(s$avg_bias)

      # u_LIN: linearity uncertainty (VDA 5 §5.3.4). Resolution order:
      #   1. Explicit params$u_lin (numeric) -> use as-is
      #   2. params$linearity_corrected = TRUE -> software compensation applied
      #      in production, residual linearity uncertainty = s_regression
      #   3. Default: linearity NOT corrected, full linearity magnitude behaves
      #      as a rectangular distribution over |Linearity|, giving
      #      u_LIN = |Linearity| / sqrt(3) (VDA 5 §5.3.4 / GUM 4.3.7)
      if (!is.null(params$u_lin) && is.numeric(params$u_lin)) {
        u_lin <- params$u_lin
      } else if (isTRUE(params$linearity_corrected)) {
        u_lin <- s$s_regression
      } else {
        u_lin <- s$linearity / sqrt(3)
      }

      u_cal_val   <- if (is.null(u_cal)) 0 else u_cal
      u_rest_total <- 0
      if (length(u_rest) > 0) {
        u_rest_total <- sqrt(sum(unlist(u_rest)^2))
      }

      u_ms <- sqrt(u_evr^2 + u_re^2 + u_bi^2 + u_lin^2 +
                   u_cal_val^2 + u_rest_total^2)
      qms_percent <- (2 * u_ms / tolerance) * 100
      expanded_u  <- 2 * u_ms

      # VDA 5 capability decision (same thresholds as Type 1)
      if (qms_percent < 15) {
        qms_capability <- "capable"
      } else if (qms_percent <= 30) {
        qms_capability <- "conditional"
      } else {
        qms_capability <- "not_capable"
      }

      u_components <- data.table::data.table(
        component = c("Repeatability (pooled)", "Resolution", "Bias (avg)",
                      "Linearity", "Calibration", "Other", "Total"),
        # GUM/VDA 5 uncertainty type classification
        type = c("A", "B", "A", "A", "B", "B", ""),
        source = c(
          "VDA 5 5.3.1 - pooled within-reference repeatability (s_pooled)",
          "VDA 5 5.3.3 - resolution (rectangular, res/2/sqrt(3))",
          "VDA 5 5.3.2 - uncorrected average bias (|avg_bias|)",
          "VDA 5 5.3.4 - linearity (|Linearity|/sqrt(3), rectangular)",
          "VDA 5 5.3.5 - calibration (from certificate)",
          "VDA 5 5.3.6 - other contributors",
          "RSS combined standard uncertainty u_MS"
        ),
        uncertainty = c(u_evr, u_re, u_bi, u_lin, u_cal_val,
                        u_rest_total, u_ms),
        percent = c(
          (u_evr^2 / u_ms^2 * 100),
          (u_re^2 / u_ms^2 * 100),
          (u_bi^2 / u_ms^2 * 100),
          (u_lin^2 / u_ms^2 * 100),
          (u_cal_val^2 / u_ms^2 * 100),
          (u_rest_total^2 / u_ms^2 * 100),
          100
        )
      )

      self$set_statistic("vda5_u_evr", u_evr)
      self$set_statistic("vda5_u_re", u_re)
      self$set_statistic("vda5_u_bi", u_bi)
      self$set_statistic("vda5_u_lin", u_lin)
      self$set_statistic("vda5_u_cal", u_cal_val)
      self$set_statistic("vda5_u_rest", u_rest_total)
      self$set_statistic("vda5_u_ms", u_ms)
      self$set_statistic("vda5_expanded_u", expanded_u)
      self$set_statistic("vda5_qms_percent", qms_percent)
      self$set_statistic("vda5_qms_capability", qms_capability)
      self$set_statistic("vda5_s_pooled", s_pooled)
      self$set_datatable("vda5_uncertainty", u_components)

      invisible(list(
        u_evr = u_evr, u_re = u_re, u_bi = u_bi, u_lin = u_lin,
        u_cal = u_cal_val, u_rest = u_rest_total, u_ms = u_ms,
        expanded_u = expanded_u, qms_percent = qms_percent,
        qms_capability = qms_capability, s_pooled = s_pooled,
        components = u_components
      ))
    },

    #' @description
    #' Evaluate pass/fail diagnostics for the linearity study.
    #'
    #' Checks the regression slope magnitude, R-squared, %Linearity, %Bias,
    #' linearity significance, per-reference bias, and VDA 5 %QMS against
    #' the thresholds in `params$criteria`. Requires
    #' [compute_per_reference_summary()], [fit_linearity_regression()], and
    #' [compute_vda5()] to have been called first.
    #'
    #' @return A named logical list of diagnostic results (invisibly).
    evaluate_linearity_criteria = function() {
      s         <- self$results$statistics
      ref_summary <- self$results$data_tables$ref_summary
      criteria  <- self$params$criteria
      conf_level <- self$params$conf_level

      # --- MSA 4th ed. acceptance criteria for Linearity Study -------------
      # Per AIAG MSA 4th edition Chapter III Section B, the acceptance
      # decision is based on:
      #   1. %Linearity < threshold (default 10%): linearity magnitude acceptable
      #   2. %Avg bias < threshold (default 10%): average bias acceptable
      #   3. %Max bias < threshold (default 10%): worst-case bias acceptable
      #   4. Slope not significant (p >= alpha): no statistically detectable
      #      magnitude-dependent bias
      #   5. Per-reference bias CI contains 0: no significant bias at any
      #      individual reference value
      # R-squared is NOT an acceptance criterion (high R-squared actually
      # indicates a strong linearity problem, so using it as a minimum
      # threshold is backwards). R-squared is reported as a diagnostic only.
      #
      # Thresholds fall back to MSA 4th ed. defaults when criteria is not
      # populated (mirrors Type1Plan$set_criteria defaults).
      slope_tol       <- criteria$linearity_slope_tolerance %||% 0.1
      pct_bias_max    <- criteria$percent_bias_max %||% 10
      pct_linearity_max <- criteria$percent_linearity_max %||% 10
      qms_threshold   <- criteria$vda5_qms_max %||% 15
      alpha           <- 1 - conf_level

      slope_ok            <- abs(s$slope) < slope_tol
      percent_max_bias_ok <- s$percent_max_bias < pct_bias_max
      # Linearity significance: slope p-value > alpha => fail to reject H0
      # (no significant linearity effect).
      linearity_sig_ok    <- s$p_slope >= alpha

      # %Linearity acceptance (AIAG MSA 4th ed. convention: <10% acceptable)
      percent_linearity_ok <- s$percent_linearity < pct_linearity_max

      # %Average bias acceptance (AIAG: <10% of PV)
      percent_avg_bias_ok  <- s$percent_avg_bias < pct_linearity_max

      # Per-reference bias: every reference's 95% CI must contain 0
      per_ref_bias_ok <- all(ref_summary$ci_lower <= 0 &
                             ref_summary$ci_upper >= 0)

      # VDA 5 %QMS acceptance (< 15% capable, VDA 5 3rd ed.)
      qms_ok <- !is.null(s$vda5_qms_percent) &&
        s$vda5_qms_percent < qms_threshold

      # Per-reference stability: every reference must be in statistical control
      per_ref_stability_ok <- all(ref_summary$stability_ok)

      self$set_diagnostic("slope_ok", slope_ok)
      self$set_diagnostic("percent_max_bias_ok", percent_max_bias_ok)
      self$set_diagnostic("linearity_sig_ok", linearity_sig_ok)
      self$set_diagnostic("percent_linearity_ok", percent_linearity_ok)
      self$set_diagnostic("percent_avg_bias_ok", percent_avg_bias_ok)
      self$set_diagnostic("per_ref_bias_ok", per_ref_bias_ok)
      self$set_diagnostic("vda5_qms_ok", qms_ok)
      self$set_diagnostic("per_ref_stability_ok", per_ref_stability_ok)

      invisible(list(
        slope_ok              = slope_ok,
        percent_max_bias_ok   = percent_max_bias_ok,
        linearity_sig_ok      = linearity_sig_ok,
        percent_linearity_ok  = percent_linearity_ok,
        percent_avg_bias_ok   = percent_avg_bias_ok,
        per_ref_bias_ok       = per_ref_bias_ok,
        vda5_qms_ok           = qms_ok,
        per_ref_stability_ok  = per_ref_stability_ok
      ))
    }
  ),

  private = list(
    # Core execution logic - orchestrates the public step methods in order.
    .run_logic = function(dt) {
      self$compute_per_reference_summary(dt)
      reg <- self$fit_linearity_regression()
      vda5 <- self$compute_vda5()
      self$evaluate_linearity_criteria()

      # Assemble raw_output bundle for downstream consumers (plotter reads
      # lm_model, bias_long, and ref_summary from raw_output).
      s <- self$results$statistics
      self$set_raw_output(list(
        ref_summary         = self$results$data_tables$ref_summary,
        bias_long           = self$results$data_tables$bias_long,
        vda5_uncertainty    = self$results$data_tables$vda5_uncertainty,
        lm_model            = reg$lm_model,
        lm_summary          = reg$lm_summary,
        n_total             = s$n_total,
        n_ref_points        = s$n_ref_points,
        tolerance           = s$tolerance,
        process_variation   = s$process_variation,
        avg_bias            = s$avg_bias,
        max_bias            = s$max_bias,
        percent_avg_bias    = s$percent_avg_bias,
        percent_max_bias    = s$percent_max_bias,
        linearity           = s$linearity,
        percent_linearity   = s$percent_linearity,
        ci_linearity        = s$ci_linearity,
        intercept           = s$intercept,
        slope               = s$slope,
        se_slope            = s$se_slope,
        t_slope             = s$t_slope,
        p_slope             = s$p_slope,
        ci_slope            = s$ci_slope,
        r_squared           = s$r_squared,
        adj_r_squared       = s$adj_r_squared,
        s_regression        = s$s_regression,
        avg_sd              = s$avg_sd,
        avg_cv              = s$avg_cv,
        vda5 = list(
          u_evr       = s$vda5_u_evr,
          u_re        = s$vda5_u_re,
          u_bi        = s$vda5_u_bi,
          u_lin       = s$vda5_u_lin,
          u_cal       = s$vda5_u_cal,
          u_rest      = s$vda5_u_rest,
          u_ms        = s$vda5_u_ms,
          expanded_u  = s$vda5_expanded_u,
          qms_percent = s$vda5_qms_percent,
          qms_capability = s$vda5_qms_capability,
          s_pooled    = s$vda5_s_pooled,
          components  = self$results$data_tables$vda5_uncertainty
        )
      ))
    }
  )
)
