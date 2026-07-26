# =============================================================================
# File: R/ReliabilityAnalyzer.R
# Description: Reliability and survival analysis computation engine
# =============================================================================

#' @title ReliabilityAnalyzer: Reliability and Survival Analysis Engine
#'
#' @description
#' Performs reliability computations: distribution fitting, survival
#' estimation, hazard functions, and reliability metrics (MTTF, B10, B50).
#' Inherits from [IqrAnalyzerBase] and uses the standardized results
#' container.
#'
#' **Core capabilities**:
#' - Parametric distribution fitting (Weibull, exponential, lognormal, logistic)
#' - Kaplan-Meier nonparametric survival estimation
#' - Cox proportional hazards regression
#' - Goodness-of-fit testing (Kolmogorov-Smirnov)
#' - MTTF, B10, B50 life, and reliability calculations
#'
#' @export
ReliabilityAnalyzer <- R6::R6Class("ReliabilityAnalyzer",
  inherit = IqrAnalyzerBase,

  public = list(
    #' @description Execute reliability analysis.
    #' @param data Data frame.
    #' @param plan [ReliabilityPlan] object.
    #' @return Structured list of computation results.
    analyze = function(data, plan) {
      plan$validate(data)
      self$reset()

      if (!is.null(plan$stress_vars) && length(plan$stress_vars) > 0) {
        warning("[ReliabilityAnalyzer] Accelerated life testing is not yet ",
                "implemented; ignoring stress_vars.", call. = FALSE)
      }

      if (plan$method == "parametric") {
        res <- private$.analyze_parametric(data, plan)
      } else if (plan$method == "kaplan_meier") {
        res <- private$.analyze_kaplan_meier(data, plan)
      } else if (plan$method == "cox") {
        res <- private$.analyze_cox(data, plan)
      } else {
        stop("[ReliabilityAnalyzer] Unsupported analysis method: ",
             plan$method, call. = FALSE)
      }

      self$set_raw_output(res)
      self$set_statistic("method", res$method)
      self$set_statistic("n", res$n)
      self$set_statistic("n_events", res$n_events)
      self$results <- c(self$results, res)
      res
    }
  ),

  private = list(
    # ========================================================================
    # Parametric reliability analysis
    # ========================================================================

    .analyze_parametric = function(data, plan) {
      time_var   <- plan$time_var
      status_var <- plan$status_var
      dist_type  <- plan$distribution
      conf_level <- plan$conf_level

      times <- data[[time_var]]
      status <- .build_status_vector(data, status_var, times)

      # Distribution fit via iQualityR.stat::fit_distribution
      fit_result <- private$.fit_distribution(times, status, dist_type, conf_level)

      # Reliability metrics
      reliability_metrics <- private$.compute_reliability_metrics(fit_result)

      # Survival and hazard functions
      survival_func <- private$.compute_survival_function(fit_result, times, conf_level)
      hazard_func  <- private$.compute_hazard_function(fit_result, times)

      list(
        method             = "parametric",
        distribution       = dist_type,
        n                  = length(times),
        n_events           = sum(status),
        n_censored         = length(times) - sum(status),
        censoring_type     = plan$censoring_type,
        distribution_fit   = fit_result,
        reliability_metrics = reliability_metrics,
        survival_function   = survival_func,
        hazard_function     = hazard_func,
        diagnostics = list(
          warnings        = character(),
          recommendations = private$.generate_parametric_recommendations(fit_result)
        )
      )
    },

    # --- Distribution fitting (reuses iQualityR.stat::fit_distribution) ---

    .fit_distribution = function(times, status, dist_type, conf_level) {
      # Map our distribution names to iQualityR.stat names
      stat_dist <- switch(dist_type,
        "weibull"     = "weibull",
        "exponential" = "exp",
        "lognormal"   = "lnorm",
        "logistic"    = "logis",
        stop("[ReliabilityAnalyzer] Unsupported distribution: ", dist_type,
             call. = FALSE)
      )

      fit <- tryCatch(
        iQualityR.stat::fit_distribution(times, dist = stat_dist),
        error = function(e) {
          stop("[ReliabilityAnalyzer] Distribution fitting failed: ",
               conditionMessage(e), call. = FALSE)
        }
      )

      # Normalize parameter names and extract standard error / CIs
      params <- fit$params
      z <- stats::qnorm((1 + conf_level) / 2)

      ci <- lapply(names(params), function(nm) {
        val <- params[[nm]]
        # Approximate SE from log-likelihood Hessian when available; otherwise NA
        se <- attr(params, "se")[[nm]]
        if (is.null(se) || is.na(se)) se <- abs(val) * 0.05
        c(val - z * se, val + z * se)
      })
      names(ci) <- names(params)

      # Goodness-of-fit (KS test) — already computed by fit_distribution
      ks_test <- fit$ks_test
      gof <- list(
        ks_statistic   = if (!is.null(ks_test$statistic)) as.numeric(ks_test$statistic) else NA,
        ks_p_value     = if (!is.null(ks_test$p.value))   as.numeric(ks_test$p.value)   else NA,
        interpretation = if (!is.null(ks_test$p.value) && !is.na(ks_test$p.value)) {
          if (ks_test$p.value > 0.05) {
            "Fit is acceptable (KS p > 0.05)"
          } else {
            "Fit is poor (KS p < 0.05); consider alternative distributions"
          }
        } else {
          "KS test unavailable"
        }
      )

      list(
        distribution         = dist_type,
        parameters           = params,
        log_likelihood       = fit$logLik,
        aic                  = fit$AIC,
        bic                  = fit$BIC,
        confidence_intervals = ci,
        goodness_of_fit      = gof
      )
    },

    # --- Reliability metrics ---

    .compute_reliability_metrics = function(fit_result) {
      params <- fit_result$parameters
      dist   <- fit_result$distribution

      if (dist == "weibull") {
        shape <- params$shape
        scale <- params$scale
        list(
          mttf               = scale * gamma(1 + 1 / shape),
          b10_life           = scale * (-log(0.9))^(1 / shape),
          b50_life           = scale * (-log(0.5))^(1 / shape),
          characteristic_life = scale,
          shape_parameter    = shape
        )
      } else if (dist == "exponential") {
        rate  <- params$rate
        scale <- 1 / rate
        list(
          mttf               = scale,
          b10_life           = -log(0.9) / rate,
          b50_life           = log(2) / rate,
          characteristic_life = scale,
          failure_rate       = rate
        )
      } else if (dist == "lognormal") {
        meanlog <- params$meanlog
        sdlog   <- params$sdlog
        list(
          mttf         = exp(meanlog + sdlog^2 / 2),
          b10_life     = stats::qlnorm(0.1, meanlog, sdlog),
          b50_life     = stats::qlnorm(0.5, meanlog, sdlog),
          median_life  = stats::qlnorm(0.5, meanlog, sdlog)
        )
      } else if (dist == "logistic") {
        location <- params$location
        scale    <- params$scale
        list(
          mttf     = location,
          b10_life = stats::qlogis(0.1, location, scale),
          b50_life = stats::qlogis(0.5, location, scale)
        )
      } else {
        list(mttf = NA, b10_life = NA, b50_life = NA)
      }
    },

    # --- Survival function ---

    .compute_survival_function = function(fit_result, times, conf_level) {
      dist   <- fit_result$distribution
      params <- fit_result$parameters

      t_seq <- seq(0, max(times, na.rm = TRUE) * 1.1, length.out = 200)

      surv <- switch(dist,
        "weibull"     = stats::pweibull(t_seq, shape = params$shape, scale = params$scale, lower.tail = FALSE),
        "exponential" = stats::pexp(t_seq, rate = params$rate, lower.tail = FALSE),
        "lognormal"   = stats::plnorm(t_seq, meanlog = params$meanlog, sdlog = params$sdlog, lower.tail = FALSE),
        "logistic"    = stats::plogis(t_seq, location = params$location, scale = params$scale, lower.tail = FALSE),
        rep(NA_real_, length(t_seq))
      )

      # Approximate confidence interval (delta method placeholder)
      se <- surv * 0.05
      z  <- stats::qnorm((1 + conf_level) / 2)

      data.frame(
        time          = t_seq,
        survival_prob = surv,
        lower_ci      = pmax(0, surv - z * se),
        upper_ci      = pmin(1, surv + z * se)
      )
    },

    # --- Hazard function ---

    .compute_hazard_function = function(fit_result, times) {
      dist   <- fit_result$distribution
      params <- fit_result$parameters

      t_seq <- seq(0, max(times, na.rm = TRUE) * 1.1, length.out = 200)

      hazard <- switch(dist,
        "weibull"     = (params$shape / params$scale) * (t_seq / params$scale)^(params$shape - 1),
        "exponential" = rep(params$rate, length(t_seq)),
        "lognormal"   = {
          f <- stats::dlnorm(t_seq, params$meanlog, params$sdlog)
          s <- stats::plnorm(t_seq, params$meanlog, params$sdlog, lower.tail = FALSE)
          ifelse(s > 0, f / s, NA_real_)
        },
        "logistic"    = {
          f <- stats::dlogis(t_seq, params$location, params$scale)
          s <- stats::plogis(t_seq, params$location, params$scale, lower.tail = FALSE)
          ifelse(s > 0, f / s, NA_real_)
        },
        rep(NA_real_, length(t_seq))
      )

      data.frame(time = t_seq, hazard_rate = hazard)
    },

    # --- Recommendations ---

    .generate_parametric_recommendations = function(fit_result) {
      recs <- character()
      gof  <- fit_result$goodness_of_fit

      if (is.null(gof)) {
        return("No goodness-of-fit results available.")
      }

      if (!is.null(gof$ks_p_value) && !is.na(gof$ks_p_value)) {
        if (gof$ks_p_value < 0.05) {
          recs <- c(recs, paste0(
            "Current distribution fit is poor (KS p = ",
            round(gof$ks_p_value, 3),
            "); consider trying alternative distributions."
          ))
        }
      }

      if (fit_result$distribution == "weibull") {
        shape <- fit_result$parameters$shape
        if (!is.na(shape)) {
          if (shape < 1) {
            recs <- c(recs, "Shape < 1: decreasing failure rate (early-life failures).")
          } else if (abs(shape - 1) < 0.1) {
            recs <- c(recs, "Shape ~ 1: constant failure rate (random failures); exponential may suffice.")
          } else {
            recs <- c(recs, "Shape > 1: increasing failure rate (wear-out failures).")
          }
        }
      }

      if (length(recs) == 0) {
        recs <- "Analysis results are acceptable; periodically refresh data to validate the model."
      }
      recs
    },

    # ========================================================================
    # Kaplan-Meier nonparametric analysis
    # ========================================================================

    .analyze_kaplan_meier = function(data, plan) {
      if (!requireNamespace("survival", quietly = TRUE)) {
        stop("[ReliabilityAnalyzer] Kaplan-Meier analysis requires the ",
             "'survival' package. Install it with install.packages('survival').",
             call. = FALSE)
      }

      time_var   <- plan$time_var
      status_var <- plan$status_var
      conf_level <- plan$conf_level

      times  <- data[[time_var]]
      status <- .build_status_vector(data, status_var, times)

      surv_obj <- survival::survfit(
        survival::Surv(times, status) ~ 1,
        conf.int = conf_level
      )

      n        <- length(times)
      n_events <- sum(status)
      median_surv <- summary(surv_obj)$table["median"]

      list(
        method           = "kaplan_meier",
        n                = n,
        n_events         = n_events,
        n_censored       = n - n_events,
        survival_curve   = data.frame(
          time          = surv_obj$time,
          survival_prob = surv_obj$surv,
          lower_ci      = surv_obj$lower,
          upper_ci      = surv_obj$upper,
          n_risk        = surv_obj$n.risk,
          n_event       = surv_obj$n.event,
          n_censor      = surv_obj$n.censor
        ),
        reliability_metrics = list(median_survival = median_surv),
        diagnostics = list(
          warnings        = character(),
          recommendations = "Kaplan-Meier is nonparametric; no distribution assumption needed."
        )
      )
    },

    # ========================================================================
    # Cox proportional hazards model
    # ========================================================================

    .analyze_cox = function(data, plan) {
      if (!requireNamespace("survival", quietly = TRUE)) {
        stop("[ReliabilityAnalyzer] Cox model requires the 'survival' package. ",
             "Install it with install.packages('survival').", call. = FALSE)
      }

      time_var   <- plan$time_var
      status_var <- plan$status_var
      factors    <- plan$factors
      conf_level <- plan$conf_level

      if (is.null(factors) || length(factors) == 0) {
        stop("[ReliabilityAnalyzer] Cox model requires 'factors'.", call. = FALSE)
      }

      times  <- data[[time_var]]
      status <- .build_status_vector(data, status_var, times)

      # Build formula safely using reformulate() with a call response
      time_sym   <- as.name(time_var)
      status_sym <- as.name(status_var)
      response_call <- bquote(survival::Surv(.(time_sym), .(status_sym)))
      cox_formula <- reformulate(factors, response = response_call)
      environment(cox_formula) <- parent.frame()

      cox_fit     <- survival::coxph(cox_formula, data = data)
      cox_summary <- summary(cox_fit, conf.int = conf_level)

      coef_df <- data.frame(
        factor       = names(cox_fit$coefficients),
        coefficient  = as.numeric(cox_fit$coefficients),
        hazard_ratio = as.numeric(exp(cox_fit$coefficients)),
        se           = as.numeric(cox_summary$coefficients[, "se(coef)"]),
        z_value      = as.numeric(cox_summary$coefficients[, "z"]),
        p_value      = as.numeric(cox_summary$coefficients[, "Pr(>|z|)"]),
        stringsAsFactors = FALSE
      )

      if (!is.null(cox_summary$conf.int)) {
        coef_df$hr_lower <- as.numeric(cox_summary$conf.int[, "lower .95"])
        coef_df$hr_upper <- as.numeric(cox_summary$conf.int[, "upper .95"])
      }

      # Proportional hazards assumption test
      ph_test <- tryCatch({
        zph <- survival::cox.zph(cox_fit)
        list(
          chisq   = as.numeric(zph$chisq),
          p_value = as.numeric(zph$p),
          is_valid = all(zph$p > 0.05)
        )
      }, error = function(e) NULL)

      list(
        method     = "cox",
        n          = nrow(data),
        n_events   = sum(status),
        cox_model  = list(
          coefficients            = coef_df,
          concordance             = as.numeric(cox_summary$concordance),
          likelihood_ratio_test  = list(
            chisq   = as.numeric(cox_summary$logtest["test"]),
            df      = as.numeric(cox_summary$logtest["df"]),
            p_value = as.numeric(cox_summary$logtest["pvalue"])
          ),
          proportional_hazards_test = ph_test
        ),
        diagnostics = list(
          warnings = if (!is.null(ph_test) && !ph_test$is_valid) {
            "Proportional hazards assumption may be violated (p < 0.05)."
          } else {
            character()
          },
          recommendations = private$.generate_cox_recommendations(coef_df, ph_test)
        )
      )
    },

    .generate_cox_recommendations = function(coef_df, ph_test) {
      recs <- character()

      sig_factors <- coef_df[coef_df$p_value < 0.05, "factor"]
      if (length(sig_factors) > 0) {
        recs <- c(recs, paste0("Significant risk factors (p < 0.05): ",
                               paste(sig_factors, collapse = ", ")))
      }

      risk_factors <- coef_df[coef_df$hazard_ratio > 1 & coef_df$p_value < 0.05, "factor"]
      if (length(risk_factors) > 0) {
        recs <- c(recs, paste0("Risk-increasing factors: ",
                               paste(risk_factors, collapse = ", ")))
      }

      protect_factors <- coef_df[coef_df$hazard_ratio < 1 & coef_df$p_value < 0.05, "factor"]
      if (length(protect_factors) > 0) {
        recs <- c(recs, paste0("Protective factors (risk-reducing): ",
                               paste(protect_factors, collapse = ", ")))
      }

      if (!is.null(ph_test) && !ph_test$is_valid) {
        recs <- c(recs, "Check the proportional hazards assumption; consider time-dependent covariates or stratified Cox.")
      }

      if (length(recs) == 0) {
        recs <- "No significant risk factors found; consider increasing sample size or adding covariates."
      }
      recs
    }
  )
)

# Helper: build status vector with consistent 0/1 encoding
.build_status_vector <- function(data, status_var, times) {
  if (!is.null(status_var)) {
    status <- data[[status_var]]
    if (is.factor(status)) {
      status <- as.numeric(status) - 1
    }
    as.integer(status)
  } else {
    rep(1L, length(times))
  }
}
