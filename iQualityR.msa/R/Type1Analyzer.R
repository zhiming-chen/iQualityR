# =============================================================================
# File: R/Type1Analyzer.R
# Description: Type1 Gage Study Analyzer (Single Reference Value Bias & Repeatability)
# =============================================================================

#' @title Type1 Gage Study Analyzer
#' @description
#' Analyzer for single reference value Type1 Gage Study,
#' including Cg/Cgk capability, VDA5 uncertainty, and bias analysis.
#'
#' The analyzer exposes the analysis pipeline as discrete public methods so
#' that each step can be inspected, unit-tested, or re-run independently:
#'   1. [compute_bias()]         - basic statistics and bias t-test
#'   2. [compute_capability()]   - Cg/Cgk and percent repeatability
#'   3. [compute_vda5()]         - VDA5 uncertainty components and %QMS
#'   4. [evaluate_criteria()]    - pass/fail diagnostics against plan criteria
#'
#' The inherited [IqrAnalyzerBase$run()] method orchestrates the full pipeline
#' via `private$.run_logic()`, which simply calls the four steps in order.
#'
#' @export
Type1Analyzer <- R6::R6Class("Type1Analyzer",
  inherit = IqrAnalyzerBase,

  public = list(

    #' @description
    #' Compute basic statistics and bias hypothesis test.
    #'
    #' Calculates sample mean, standard deviation, bias versus the reference
    #' value, and a two-sided t-test of H0: bias = 0 with confidence interval.
    #' Results are stored via `set_statistic()`.
    #'
    #' @param dt Data frame or data.table with a `measurement` column
    #'   (the first column is used if `measurement` is absent).
    #' @return A named list of computed bias statistics (invisibly). The same
    #'   values are also stored in `self$results$statistics`.
    compute_bias = function(dt) {
      params <- self$params
      ref_val   <- params$reference_value
      conf_level <- params$conf_level
      alternative <- params$alternative %||% "two.sided"
      historical_sd <- params$historical_sd

      if (!"measurement" %in% names(dt)) {
        meas_col <- names(dt)[1]
        names(dt)[1] <- "measurement"
      }
      meas <- dt$measurement

      n         <- length(meas)
      mean_meas <- mean(meas)
      sd_sample <- stats::sd(meas)
      # Minitab "Use known standard deviation": when historical_sd is supplied,
      # it replaces the sample SD for capability / SV / u_EVR computations.
      # The bias t-test still uses the sample standard error of the mean
      # (s/sqrt(n)) per Minitab methods-and-formulas, EXCEPT when historical_sd
      # is given Minitab switches to a z-test with sigma/sqrt(n).
      sd_meas   <- if (!is.null(historical_sd) && is.finite(historical_sd)) {
        historical_sd
      } else {
        sd_sample
      }
      bias      <- mean_meas - ref_val

      # Bias hypothesis test. With historical_sd the test statistic is z
      # (known sigma); otherwise t with df = n-1.
      if (!is.null(historical_sd) && is.finite(historical_sd)) {
        se_bias <- historical_sd / sqrt(n)
        t_stat  <- bias / se_bias
        df      <- Inf
        # z-test p-value
        p_value <- switch(alternative,
          two.sided = 2 * stats::pnorm(-abs(t_stat)),
          greater   = stats::pnorm(-t_stat, lower.tail = FALSE),
          less      = stats::pnorm(t_stat, lower.tail = TRUE),
          2 * stats::pnorm(-abs(t_stat))
        )
        # CI uses z critical value
        t_crit <- stats::qnorm(1 - (1 - conf_level) /
                                 ifelse(alternative == "two.sided", 2, 1))
        # For one-sided, CI is half-infinite; report two-sided-style CI for
        # display continuity but flag the alternative.
        if (alternative == "two.sided") {
          ci_bias <- bias + c(-1, 1) * t_crit * se_bias
        } else if (alternative == "greater") {
          ci_bias <- c(bias - t_crit * se_bias, Inf)
        } else {
          ci_bias <- c(-Inf, bias + t_crit * se_bias)
        }
      } else {
        se_bias <- sd_sample / sqrt(n)
        t_stat  <- bias / se_bias
        df      <- n - 1
        p_value <- switch(alternative,
          two.sided = 2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE),
          greater   = stats::pt(t_stat, df = df, lower.tail = FALSE),
          less      = stats::pt(t_stat, df = df, lower.tail = TRUE),
          2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE)
        )
        t_crit <- stats::qt(1 - (1 - conf_level) /
                              ifelse(alternative == "two.sided", 2, 1), df = df)
        if (alternative == "two.sided") {
          ci_bias <- bias + c(-1, 1) * t_crit * se_bias
        } else if (alternative == "greater") {
          ci_bias <- c(bias - t_crit * se_bias, Inf)
        } else {
          ci_bias <- c(-Inf, bias + t_crit * se_bias)
        }
      }

      # Percent bias relative to tolerance (computed here for convenience;
      # tolerance is resolved by Type1Plan from multiple input modes)
      spec      <- params$spec_limits
      tolerance <- params$tolerance
      if (is.null(tolerance)) {
        tolerance <- spec$usl - spec$lsl
      }
      percent_bias <- (abs(bias) / tolerance) * 100

      self$set_statistic("n", n)
      self$set_statistic("reference_value", ref_val)
      self$set_statistic("mean_meas", mean_meas)
      self$set_statistic("sd_meas", sd_meas)
      self$set_statistic("sd_sample", sd_sample)
      self$set_statistic("bias", bias)
      self$set_statistic("percent_bias", percent_bias)
      self$set_statistic("t_stat", t_stat)
      self$set_statistic("p_value", p_value)
      self$set_statistic("df", df)
      self$set_statistic("ci_bias", ci_bias)
      self$set_statistic("alternative", alternative)
      self$set_statistic("tolerance", tolerance)
      self$set_statistic("spec_limits", spec)

      # Resolution vs 5% tolerance check (Minitab Type 1 Gage Study guideline)
      resolution <- params$resolution
      if (!is.null(resolution) && is.finite(resolution) && tolerance > 0) {
        resolution_ratio <- (resolution / tolerance) * 100
        resolution_ok    <- resolution_ratio <= 5
        self$set_statistic("resolution", resolution)
        self$set_statistic("resolution_ratio", resolution_ratio)
        self$set_statistic("resolution_ok", resolution_ok)
      }

      # Run-chart stability diagnostics (MSA 4th ed. Chapter III requires the
      # measurement process to be in statistical control before bias/Cg/Cgk
      # are meaningful). Apply simplified Nelson rules on the run of
      # measurements vs the mean line:
      #   R1: any point beyond +-3 sigma  (out-of-control)
      #   R2: 9 consecutive points on same side of mean (shift)
      #   R3: 6 consecutive points steadily increasing or decreasing (trend)
      #   R5: 2 of 3 consecutive points beyond +-2 sigma (same side)
      if (n >= 6 && sd_meas > 0) {
        z <- (meas - mean_meas) / sd_meas
        beyond_3s <- any(abs(z) > 3, na.rm = TRUE)
        # R2: 9 consecutive same side (use 7 as a conservative proxy for n=25..50)
        run_side <- sign(z)
        max_run  <- .msa_max_consecutive_run(run_side)
        shift_9  <- max_run >= 9
        # R3: 6 consecutive trending
        trend_6  <- .msa_has_trend(meas, 6)
        # R5: 2 of 3 beyond +-2 sigma (same side)
        beyond_2s_pairs <- .msa_two_of_three_beyond_2s(z)
        unstable <- beyond_3s || shift_9 || trend_6 || beyond_2s_pairs
        self$set_statistic("stability_beyond_3s",  beyond_3s)
        self$set_statistic("stability_shift_9",    shift_9)
        self$set_statistic("stability_trend_6",    trend_6)
        self$set_statistic("stability_2of3_beyond_2s", beyond_2s_pairs)
        self$set_statistic("stability_ok", !unstable)
      }

      invisible(list(
        n = n, reference_value = ref_val, mean_meas = mean_meas,
        sd_meas = sd_meas, bias = bias, percent_bias = percent_bias,
        t_stat = t_stat, p_value = p_value, ci_bias = ci_bias,
        tolerance = tolerance, se_bias = se_bias
      ))
    },

    #' @description
    #' Compute Type 1 gage capability indices Cg and Cgk.
    #'
    #' Cg measures repeatability alone; Cgk penalises for bias. Both are
    #' referenced against the tolerance window scaled by `k_factor`.
    #' Also computes study variation (6*sigma and 5.15*sigma) and percent
    #' repeatability metrics.
    #'
    #' @return A named list of capability metrics (invisibly). Requires
    #'   [compute_bias()] to have been called first so that `sd_meas` and
    #'   `bias` are available in `self$results$statistics`.
    compute_capability = function() {
      params     <- self$params
      s          <- self$results$statistics
      tolerance  <- s$tolerance
      sd_meas    <- s$sd_meas
      bias       <- s$bias
      k_factor   <- params$k_factor
      # Study multiplier (Minitab historically supports 6 and 5.15).
      # SV = study_multiplier * sigma; Cg/Cgk denominators derived from SV.
      study_mult <- params$study_multiplier %||% 6

      sv         <- study_mult * sd_meas
      sv_6sigma  <- 6 * sd_meas        # always reported for reference
      sv_515sigma <- 5.15 * sd_meas    # always reported for reference

      Cg   <- (k_factor * tolerance) / sv
      # CgK = (K*T/2 - |Bias|) / (SV/2)
      # (Minitab Type 1 Gage Study methods and formulas - CgK)
      Cgk  <- (k_factor * tolerance / 2 - abs(bias)) / (sv / 2)
      # %Var(Repeatability) = SV/T * 100 = K*100/Cg (uses active study_mult)
      percent_repeatability <- (sv / tolerance) * 100
      # %Var(Repeatability and Bias) = K*100/CgK
      if (is.finite(Cgk) && Cgk > 0) {
        percent_repeatability_bias <- (k_factor * 100) / Cgk
      } else {
        # Cgk <= 0 means bias exceeds half-tolerance; %Var is undefined
        # (effectively infinite). Report as NA.
        percent_repeatability_bias <- NA_real_
      }

      self$set_statistic("sv", sv)
      self$set_statistic("sv_6sigma", sv_6sigma)
      self$set_statistic("sv_515sigma", sv_515sigma)
      self$set_statistic("study_multiplier", study_mult)
      self$set_statistic("k_factor", k_factor)
      self$set_statistic("Cg", Cg)
      self$set_statistic("Cgk", Cgk)
      self$set_statistic("percent_repeatability", percent_repeatability)
      self$set_statistic("percent_repeatability_bias", percent_repeatability_bias)

      invisible(list(
        Cg = Cg, Cgk = Cgk,
        sv = sv, sv_6sigma = sv_6sigma, sv_515sigma = sv_515sigma,
        percent_repeatability = percent_repeatability,
        percent_repeatability_bias = percent_repeatability_bias
      ))
    },

    #' @description
    #' Compute VDA5 measurement-system uncertainty components (VDA 5, 3rd
    #' edition, section 5.3).
    #'
    #' Aggregates repeatability (uEVR), resolution (uRE), bias (uBI),
    #' linearity (uLIN), calibration (uCAL), and other (uREST) uncertainties
    #' into the combined standard uncertainty uMS, and expresses it as %QMS
    #' of the tolerance. A breakdown table is stored in
    #' `self$results$data_tables$vda5_uncertainty`.
    #'
    #' Per VDA 5 section 5.3, a Type 1 Gage Study characterises the
    #' measurement system (a single reading in production), so uEVR is the
    #' single-measurement repeatability s (not s/sqrt(n)), and uBI is the
    #' uncorrected systematic bias magnitude |Bias| (not s/sqrt(n), which
    #' would double-count repeatability already captured in uEVR). See the
    #' inline comments in the method body for the full formula derivation.
    #'
    #' @return A named list of VDA5 uncertainty results (invisibly). Requires
    #'   [compute_bias()] to have been called first.
    compute_vda5 = function() {
      params     <- self$params
      s          <- self$results$statistics
      sd_meas    <- s$sd_meas
      bias       <- s$bias
      tolerance  <- s$tolerance
      resolution <- params$resolution
      u_cal      <- params$u_cal
      u_rest     <- params$u_rest

      # VDA 5 §5.3.1 - repeatability of a SINGLE measurement (the gage makes
      # one reading in production, not the mean of n readings).
      u_evr <- sd_meas
      # VDA 5 §5.3.3 - resolution contribution (rectangular distribution).
      u_re  <- if (is.null(resolution)) 0 else resolution / (2 * sqrt(3))
      # VDA 5 §5.3.2 - uncorrected systematic bias. The bias is being
      # evaluated, not corrected, so its full magnitude contributes. The
      # statistical uncertainty of the bias estimate (s/sqrt(n)) is already
      # in u_EVR; using s/sqrt(n) again here would double-count repeatability
      # and force u_EVR == u_BI (a 50/50 split regardless of the data).
      u_bi  <- abs(bias)
      # u_lin: uncertainty due to linearity. Type 1 Gage Study evaluates a
      # single reference value, so linearity is not assessed here (u_lin = 0).
      # Included explicitly for VDA 5 budget completeness; a separate
      # Linearity & Bias Study (Type1LinearityAnalyzer) can supply a non-zero
      # value via params$u_lin.
      u_lin <- if (is.null(params$u_lin)) 0 else params$u_lin
      u_cal_val <- u_cal
      u_rest_total <- 0
      if (length(u_rest) > 0) {
        u_rest_total <- sqrt(sum(unlist(u_rest)^2))
      }

      u_ms <- sqrt(u_evr^2 + u_re^2 + u_bi^2 + u_lin^2 + u_cal_val^2 + u_rest_total^2)
      qms_percent <- (2 * u_ms / tolerance) * 100

      # Expanded uncertainty U = k * uMS, k = 2 (VDA 5, 95% coverage)
      expanded_u <- 2 * u_ms

      # VDA 5 capability decision for the measurement system (QMS):
      #   QMS < 15%        -> capable (accepted)
      #   15% <= QMS <= 30% -> conditionally capable (review required)
      #   QMS > 30%        -> not capable (rejected)
      # (VDA 5, 3rd edition, 2021)
      if (qms_percent < 15) {
        qms_capability <- "capable"
      } else if (qms_percent <= 30) {
        qms_capability <- "conditional"
      } else {
        qms_capability <- "not_capable"
      }

      u_components <- data.table::data.table(
        component = c("Repeatability", "Resolution", "Bias", "Linearity",
                      "Calibration", "Other", "Total"),
        # GUM/VDA 5 uncertainty type classification:
        #   Type A = evaluated by statistical analysis of repeated observations
        #   Type B = evaluated by other means (assumed distribution, certificate)
        type = c("A", "B", "A", "A", "B", "B", ""),
        # VDA 5 source reference for each component
        source = c(
          "VDA 5 5.3.1 - repeatability (single measurement, s)",
          "VDA 5 5.3.3 - resolution (rectangular, res/2/sqrt(3))",
          "VDA 5 5.3.2 - uncorrected systematic bias (|Bias|)",
          "VDA 5 5.3.4 - linearity (0 in Type 1; use Linearity Study)",
          "VDA 5 5.3.5 - calibration (from certificate)",
          "VDA 5 5.3.6 - other contributors",
          "RSS combined standard uncertainty u_MS"
        ),
        uncertainty = c(u_evr, u_re, u_bi, u_lin, u_cal_val, u_rest_total, u_ms),
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
      self$set_datatable("vda5_uncertainty", u_components)

      invisible(list(
        u_evr = u_evr, u_re = u_re, u_bi = u_bi, u_lin = u_lin,
        u_cal = u_cal_val, u_rest = u_rest_total, u_ms = u_ms,
        expanded_u = expanded_u, qms_percent = qms_percent,
        qms_capability = qms_capability,
        components = u_components
      ))
    },

    #' @description
    #' Evaluate pass/fail diagnostics against the plan criteria.
    #'
    #' Checks Cg, Cgk, bias (CI contains 0), percent bias, and VDA5 %QMS
    #' against the thresholds stored in `params$criteria`. Diagnostics are
    #' stored via `set_diagnostic()`.
    #'
    #' @return A named logical list of diagnostic results (invisibly). Requires
    #'   [compute_bias()], [compute_capability()], and [compute_vda5()] to have
    #'   been called first.
    evaluate_criteria = function() {
      s         <- self$results$statistics
      d         <- self$results$diagnostics
      criteria  <- self$params$criteria

      Cg_ok      <- s$Cg >= criteria$Cg_min
      Cgk_ok     <- s$Cgk >= criteria$Cgk_min
      bias_ok    <- 0 >= s$ci_bias[1] && 0 <= s$ci_bias[2]
      percent_bias_ok <- s$percent_bias < criteria$percent_bias_max
      qms_ok     <- s$vda5_qms_percent < criteria$vda5_qms_max
      # MSA 4th ed. requires statistical stability before capability indices
      # are meaningful. stability_ok is FALSE only when the run-chart rules
      # fired; if n is too small to evaluate, it is NA and treated as PASS
      # (with a diagnostic flag the caller can inspect).
      stability_ok <- if (is.null(s$stability_ok)) NA else s$stability_ok

      self$set_diagnostic("Cg_ok", Cg_ok)
      self$set_diagnostic("Cgk_ok", Cgk_ok)
      self$set_diagnostic("bias_ok", bias_ok)
      self$set_diagnostic("percent_bias_ok", percent_bias_ok)
      self$set_diagnostic("vda5_qms_ok", qms_ok)
      self$set_diagnostic("stability_ok", stability_ok)

      invisible(list(
        Cg_ok = Cg_ok, Cgk_ok = Cgk_ok, bias_ok = bias_ok,
        percent_bias_ok = percent_bias_ok, vda5_qms_ok = qms_ok,
        stability_ok = stability_ok
      ))
    }
  ),

  private = list(
    # Core execution logic - orchestrates the public step methods in order.
    # Kept thin so that each step remains independently callable and testable.
    .run_logic = function(dt) {
      self$compute_bias(dt)
      self$compute_capability()
      self$compute_vda5()
      self$evaluate_criteria()

      # Assemble a raw_output bundle for downstream consumers (reporting,
      # plotting) that expect a single consolidated object.
      s <- self$results$statistics
      self$set_raw_output(list(
        n               = s$n,
        reference_value = s$reference_value,
        measurements    = dt$measurement,
        mean_meas       = s$mean_meas,
        sd_meas         = s$sd_meas,
        bias            = s$bias,
        percent_bias    = s$percent_bias,
        t_stat          = s$t_stat,
        p_value         = s$p_value,
        ci_bias         = s$ci_bias,
        tolerance       = s$tolerance,
        spec_limits     = s$spec_limits,
        Cg              = s$Cg,
        Cgk             = s$Cgk,
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
          components  = self$results$data_tables$vda5_uncertainty
        )
      ))
    }
  )
)
