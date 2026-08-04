# =============================================================================
# File: R/RegressionAnalyzer.R
# Description: Regression analysis engine (L1).
#              R3-B1: lm/logit/poisson.
#              R3-B2: cox/pls/stepwise/best_subset.
#              R3-D1: mars (earth) / spline (bs / ns).
#              Returns stat_result S3 (class c("stat_result", "regression_result")).
# =============================================================================

#' @title RegressionAnalyzer: Regression Analysis Engine
#' @description
#' Pure computation engine for regression models.
#' **Supported model types**:
#' - R3-B1: lm_fit, logit_fit, poisson_fit
#' - R3-B2: cox_fit, pls_fit, stepwise_fit, best_subset_fit
#' - R3-D1: mars_fit (requires \pkg{earth}), spline_fit (B-spline / natural spline via \pkg{splines})
#' @export
RegressionAnalyzer <- R6::R6Class("RegressionAnalyzer",
  public = list(
    #' @description Fit a model by type code
    #' @param model_type One of: "lm_fit", "logit_fit", "poisson_fit",
    #'   "cox_fit", "pls_fit", "stepwise_fit", "best_subset_fit",
    #'   "mars_fit", "spline_fit".
    #' @param ... Parameters forwarded to the matching private method.
    #' @return A stat_result S3 object.
    analyze = function(model_type, ...) {
      args <- list(...)
      switch(model_type,
        "lm_fit"          = private$.lm_fit(args),
        "logit_fit"       = private$.logit_fit(args),
        "poisson_fit"     = private$.poisson_fit(args),
        "cox_fit"         = private$.cox_fit(args),
        "pls_fit"         = private$.pls_fit(args),
        "stepwise_fit"    = private$.stepwise_fit(args),
        "best_subset_fit" = private$.best_subset_fit(args),
        "mars_fit"        = private$.mars_fit(args),
        "spline_fit"      = private$.spline_fit(args),
        stop(sprintf("Unknown model type: %s", model_type))
      )
    },
    #' @description Fit a linear regression model (OLS)
    #' @param formula A model formula.
    #' @param data A data frame.
    #' @param subset Optional subset expression.
    #' @param weights Optional prior weights.
    #' @param na.action NA handling.
    #' @return A stat_result S3 object.
    lm_fit = function(formula, data, subset = NULL, weights = NULL, na.action = stats::na.omit) {
      private$.lm_fit(list(formula = formula, data = data, subset = subset, weights = weights, na.action = na.action))
    },
    #' @description Fit a logistic regression model
    #' @param formula A model formula.
    #' @param data A data frame.
    #' @param subset Optional subset expression.
    #' @param weights Optional prior weights.
    #' @param na.action NA handling.
    #' @return A stat_result S3 object.
    logit_fit = function(formula, data, subset = NULL, weights = NULL, na.action = stats::na.omit) {
      private$.logit_fit(list(formula = formula, data = data, subset = subset, weights = weights, na.action = na.action))
    },
    #' @description Fit a Poisson regression model
    #' @param formula A model formula.
    #' @param data A data frame.
    #' @param subset Optional subset expression.
    #' @param weights Optional prior weights.
    #' @param na.action NA handling.
    #' @return A stat_result S3 object.
    poisson_fit = function(formula, data, subset = NULL, weights = NULL, na.action = stats::na.omit) {
      private$.poisson_fit(list(formula = formula, data = data, subset = subset, weights = weights, na.action = na.action))
    },
    #' @description Fit a Cox proportional hazards model (requires \pkg{survival})
    #' @param formula A survival formula, e.g. \code{Surv(time, status) ~ x}.
    #' @param data A data frame.
    #' @param subset Optional subset expression.
    #' @param na.action NA handling.
    #' @return A stat_result S3 object.
    cox_fit = function(formula, data, subset = NULL, na.action = stats::na.omit) {
      private$.cox_fit(list(formula = formula, data = data, subset = subset, na.action = na.action))
    },
    #' @description Fit a Partial Least Squares (PLS) regression model (requires \pkg{pls})
    #' @param formula A model formula.
    #' @param data A data frame.
    #' @param ncomp Number of components to fit. Default auto-selects up to the rank.
    #' @param subset Optional subset expression.
    #' @param na.action NA handling.
    #' @return A stat_result S3 object.
    pls_fit = function(formula, data, ncomp = NULL, subset = NULL, na.action = stats::na.omit) {
      private$.pls_fit(list(formula = formula, data = data, ncomp = ncomp, subset = subset, na.action = na.action))
    },
    #' @description Stepwise variable selection via AIC (requires \pkg{MASS})
    #' @param formula A full model formula.
    #' @param data A data frame.
    #' @param direction One of "both", "forward", "backward".
    #' @param family Optional glm family; if NULL, an lm is fitted.
    #' @param k Penalty per parameter (default 2, AIC; log(n) gives BIC).
    #' @param subset Optional subset expression.
    #' @param na.action NA handling.
    #' @return A stat_result S3 object.
    stepwise_fit = function(formula, data, direction = "both", family = NULL, k = 2,
                            subset = NULL, na.action = stats::na.omit) {
      private$.stepwise_fit(list(formula = formula, data = data, direction = direction,
                                 family = family, k = k, subset = subset, na.action = na.action))
    },
    #' @description Best subset variable selection (requires \pkg{leaps})
    #' @param formula A model formula (response on left, predictors on right).
    #' @param data A data frame.
    #' @param nvmax Maximum number of variables to consider. Default uses all.
    #' @param subset Optional subset expression.
    #' @param na.action NA handling.
    #' @return A stat_result S3 object.
    best_subset_fit = function(formula, data, nvmax = NULL, subset = NULL, na.action = stats::na.omit) {
      private$.best_subset_fit(list(formula = formula, data = data, nvmax = nvmax,
                                    subset = subset, na.action = na.action))
    },
    #' @description Fit a MARS regression model (requires \pkg{earth})
    #'
    #' Multivariate Adaptive Regression Splines (MARS) build a piecewise
    #' regression by searching hinge functions and their products. This
    #' method wraps [earth::earth()] and surfaces the selected terms, the
    #' generalized R-squared, and the GCV statistic.
    #'
    #' @param formula A model formula (response on left, predictors on right).
    #' @param data A data frame.
    #' @param degree Maximum interaction degree (default 1, additive MARS).
    #' @param nk Maximum number of model terms. NULL lets earth auto-select.
    #' @param thresh Forward step threshold. Default uses earth's default.
    #' @param pmethod Pruning method. Default "backward".
    #' @param trace Logical; print earth progress.
    #' @param subset Optional subset expression.
    #' @param na.action NA handling.
    #' @return A stat_result S3 object.
    mars_fit = function(formula, data, degree = 1, nk = NULL, thresh = 0.001,
                        pmethod = "backward", trace = FALSE,
                        subset = NULL, na.action = stats::na.omit) {
      private$.mars_fit(list(formula = formula, data = data, degree = degree,
                             nk = nk, thresh = thresh, pmethod = pmethod,
                             trace = trace, subset = subset, na.action = na.action))
    },
    #' @description Fit a spline regression model (B-spline / natural spline)
    #'
    #' Wraps [stats::lm()] with a spline basis on the primary predictor.
    #' The response is modelled as `y ~ <basis>(x, df)` where `<basis>` is
    #' `splines::bs` (B-spline, default) or `splines::ns` (natural spline).
    #' Additional covariates may be added via the formula's right-hand side.
    #'
    #' @param formula A model formula. The first predictor on the RHS is
    #'   expanded with the spline basis (e.g. `y ~ x + z` becomes
    #'   `y ~ bs(x, df) + z`).
    #' @param data A data frame.
    #' @param df Degrees of freedom (number of basis parameters). Default 4.
    #' @param basis `"bs"` (B-spline, default) or `"ns"` (natural spline).
    #' @param knots Optional explicit knot positions. Overrides `df` when given.
    #' @param degree B-spline degree (only used for `basis = "bs"`). Default 3.
    #' @param subset Optional subset expression.
    #' @param weights Optional prior weights.
    #' @param na.action NA handling.
    #' @return A stat_result S3 object.
    spline_fit = function(formula, data, df = 4, basis = c("bs", "ns"),
                          knots = NULL, degree = 3,
                          subset = NULL, weights = NULL,
                          na.action = stats::na.omit) {
      private$.spline_fit(list(formula = formula, data = data, df = df,
                               basis = basis, knots = knots, degree = degree,
                               subset = subset, weights = weights,
                               na.action = na.action))
    }
  ),
  private = list(
    .coef_table = function(sm) {
      ct <- sm$coefficients
      if (NCOL(ct) < 4L) stop("RegressionAnalyzer: unexpected coefficient table shape.", call. = FALSE)
      df <- as.data.frame(ct)
      names(df) <- c("Estimate", "Std_Error", "Statistic", "p_value")
      df$Term <- rownames(ct)
      df <- df[, c("Term", "Estimate", "Std_Error", "Statistic", "p_value")]
      rownames(df) <- NULL
      df
    },
    .model_stats = function(model, sm, family_label) {
      n <- length(stats::residuals(model))
      if (inherits(model, "lm") && !inherits(model, "glm")) {
        f_stat <- sm$fstatistic
        f_p <- stats::pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
        list(n = n, r_squared = sm$r.squared, adj_r_squared = sm$adj.r.squared,
             sigma = sm$sigma, df_residual = sm$df[2],
             f_statistic = c("F" = f_stat[1]), f_df = c(num_df = f_stat[2], den_df = f_stat[3]),
             f_p_value = f_p, aic = stats::AIC(model), bic = stats::BIC(model),
             deviance = stats::deviance(model), null_deviance = NA,
             family = "gaussian", link = "identity")
      } else {
        list(n = n, r_squared = NA, adj_r_squared = NA, sigma = NA,
             df_residual = sm$df.residual, f_statistic = NULL, f_df = NULL, f_p_value = NA,
             aic = sm$aic, bic = stats::BIC(model), deviance = sm$deviance,
             null_deviance = sm$null.deviance, family = family_label, link = sm$family$link %||% NA)
      }
    },
    .lm_fit = function(args) {
      formula <- args$formula; data <- args$data
      if (!is.data.frame(data)) stop("lm_fit: 'data' must be a data frame.", call. = FALSE)
      call_args <- list(formula = formula, data = data, na.action = args$na.action)
      if (!is.null(args$subset)) call_args$subset <- args$subset
      if (!is.null(args$weights)) call_args$weights <- args$weights
      model <- do.call(stats::lm, call_args)
      sm <- summary(model)
      res <- list(test_type = "lm_fit", method = "Linear regression (OLS)", data_name = deparse(formula),
                  model = model, coefficients = private$.coef_table(sm), model_stats = private$.model_stats(model, sm, "gaussian"),
                  residuals = as.numeric(stats::residuals(model)), fitted = as.numeric(stats::fitted(model)),
                  formula = formula, dist_type = "t", data = list(x = NULL, y = NULL))
      class(res) <- c("stat_result", "regression_result"); res$domain <- "regression"; res
    },
    .logit_fit = function(args) {
      formula <- args$formula; data <- args$data
      if (!is.data.frame(data)) stop("logit_fit: 'data' must be a data frame.", call. = FALSE)
      call_args <- list(formula = formula, data = data, family = stats::binomial(link = "logit"), na.action = args$na.action)
      if (!is.null(args$subset)) call_args$subset <- args$subset
      if (!is.null(args$weights)) call_args$weights <- args$weights
      model <- do.call(stats::glm, call_args)
      sm <- summary(model)
      coef_vals <- stats::coef(model); or <- exp(coef_vals)
      se <- sqrt(diag(stats::vcov(model)))
      or_ci <- exp(cbind(coef_vals - stats::qnorm(0.975) * se, coef_vals + stats::qnorm(0.975) * se))
      res <- list(test_type = "logit_fit", method = "Logistic regression (binomial, logit link)", data_name = deparse(formula),
                  model = model, coefficients = private$.coef_table(sm), odds_ratios = or, odds_ratio_ci = or_ci,
                  model_stats = private$.model_stats(model, sm, "binomial"),
                  residuals = as.numeric(stats::residuals(model)), fitted = as.numeric(stats::fitted(model)),
                  formula = formula, dist_type = "asymptotic", data = list(x = NULL, y = NULL))
      class(res) <- c("stat_result", "regression_result"); res$domain <- "regression"; res
    },
    .poisson_fit = function(args) {
      formula <- args$formula; data <- args$data
      if (!is.data.frame(data)) stop("poisson_fit: 'data' must be a data frame.", call. = FALSE)
      call_args <- list(formula = formula, data = data, family = stats::poisson(link = "log"), na.action = args$na.action)
      if (!is.null(args$subset)) call_args$subset <- args$subset
      if (!is.null(args$weights)) call_args$weights <- args$weights
      model <- do.call(stats::glm, call_args)
      sm <- summary(model)
      coef_vals <- stats::coef(model); irr <- exp(coef_vals)
      se <- sqrt(diag(stats::vcov(model)))
      irr_ci <- exp(cbind(coef_vals - stats::qnorm(0.975) * se, coef_vals + stats::qnorm(0.975) * se))
      res <- list(test_type = "poisson_fit", method = "Poisson regression (log link)", data_name = deparse(formula),
                  model = model, coefficients = private$.coef_table(sm), rate_ratios = irr, rate_ratio_ci = irr_ci,
                  model_stats = private$.model_stats(model, sm, "poisson"),
                  residuals = as.numeric(stats::residuals(model)), fitted = as.numeric(stats::fitted(model)),
                  formula = formula, dist_type = "asymptotic", data = list(x = NULL, y = NULL))
      class(res) <- c("stat_result", "regression_result"); res$domain <- "regression"; res
    },
    # ----------------------------------------------------------------------
    # R3-B2: cox_fit / pls_fit / stepwise_fit / best_subset_fit
    # ----------------------------------------------------------------------
    .cox_fit = function(args) {
      formula <- args$formula; data <- args$data
      if (!is.data.frame(data)) stop("cox_fit: 'data' must be a data frame.", call. = FALSE)
      if (!requireNamespace("survival", quietly = TRUE))
        stop("cox_fit requires the 'survival' package.", call. = FALSE)
      call_args <- list(formula = formula, data = data, na.action = args$na.action)
      if (!is.null(args$subset)) call_args$subset <- args$subset
      model <- do.call(survival::coxph, call_args)
      sm <- summary(model)
      # Coefficient table: coef / se(coef) / z / Pr(>|z|)
      ct <- sm$coefficients
      df <- data.frame(
        Term      = rownames(ct),
        Estimate  = ct[, 1],
        Std_Error = ct[, 3],
        Statistic = ct[, 4],
        p_value   = ct[, 5],
        stringsAsFactors = FALSE
      )
      rownames(df) <- NULL
      # Hazard ratios and CI
      hr <- sm$conf.int[, "exp(coef)"]
      hr_lower <- sm$conf.int[, "lower .95"]
      hr_upper <- sm$conf.int[, "upper .95"]
      hr_ci_mat <- cbind(lower = hr_lower, upper = hr_upper)
      n <- model$n
      n_events <- model$nevent
      concordance <- sm$concordance["C"]
      # Nagelkerke-ish R-squared via concordance / loglik
      ll_null <- model$loglik[1]; ll_full <- model$loglik[2]
      cox_r2 <- 1 - exp(-2 / n * (ll_full - ll_null))
      ms <- list(n = n, n_events = as.numeric(n_events),
                 concordance = as.numeric(concordance), r_squared = cox_r2,
                 adj_r_squared = NA, sigma = NA, df_residual = model$df.residual,
                 f_statistic = NULL, f_df = NULL, f_p_value = NA,
                 aic = stats::AIC(model), bic = NA,
                 deviance = -2 * ll_full, null_deviance = -2 * ll_null,
                 family = "cox", link = "log-hazard")
      res <- list(test_type = "cox_fit", method = "Cox proportional hazards regression",
                  data_name = deparse(formula),
                  model = model, coefficients = df,
                  hazard_ratios = hr, hazard_ratio_ci = hr_ci_mat,
                  model_stats = ms,
                  residuals = as.numeric(stats::residuals(model)),
                  fitted = as.numeric(stats::fitted(model)),
                  formula = formula, dist_type = "z", data = list(x = NULL, y = NULL))
      class(res) <- c("stat_result", "regression_result"); res$domain <- "regression"; res
    },
    .pls_fit = function(args) {
      formula <- args$formula; data <- args$data
      if (!is.data.frame(data)) stop("pls_fit: 'data' must be a data frame.", call. = FALSE)
      if (!requireNamespace("pls", quietly = TRUE))
        stop("pls_fit requires the 'pls' package.", call. = FALSE)
      call_args <- list(formula = formula, data = data, na.action = args$na.action,
                        validation = "CV")
      if (!is.null(args$subset)) call_args$subset <- args$subset
      model <- do.call(pls::plsr, call_args)
      max_comp <- model$ncomp
      ncomp <- if (is.null(args$ncomp)) max_comp else min(args$ncomp, max_comp)
      # Coefficients at chosen ncomp
      coefs <- stats::coef(model, ncomp = ncomp)
      df <- data.frame(
        Term      = rownames(coefs),
        Estimate  = as.numeric(coefs[, 1, 1]),
        Std_Error = NA_real_,
        Statistic = NA_real_,
        p_value   = NA_real_,
        stringsAsFactors = FALSE
      )
      rownames(df) <- NULL
      # Explained variance in X / Y per component
      expl <- pls::explvar(model)
      yvar <- drop(pls::R2(model, estimate = "train")$val)
      rmse <- drop(pls::RMSEP(model, estimate = "CV")$val)
      ms <- list(n = model$nobs, ncomp = ncomp,
                 r_squared = as.numeric(yvar[ncomp + 1]), adj_r_squared = NA,
                 sigma = as.numeric(rmse[ncomp + 1]), df_residual = NA,
                 f_statistic = NULL, f_df = NULL, f_p_value = NA,
                 aic = NA, bic = NA, deviance = NA, null_deviance = NA,
                 family = "pls", link = "identity",
                 x_explained_var = expl, rmsep_cv = rmse)
      res <- list(test_type = "pls_fit", method = sprintf("Partial Least Squares regression (%d components)", ncomp),
                  data_name = deparse(formula),
                  model = model, coefficients = df,
                  model_stats = ms,
                  residuals = as.numeric(stats::residuals(model)[, 1, ncomp]),
                  fitted = as.numeric(stats::fitted(model)[, 1, ncomp]),
                  formula = formula, dist_type = "asymptotic", data = list(x = NULL, y = NULL))
      class(res) <- c("stat_result", "regression_result"); res$domain <- "regression"; res
    },
    .stepwise_fit = function(args) {
      formula <- args$formula; data <- args$data
      if (!is.data.frame(data)) stop("stepwise_fit: 'data' must be a data frame.", call. = FALSE)
      if (!requireNamespace("MASS", quietly = TRUE))
        stop("stepwise_fit requires the 'MASS' package.", call. = FALSE)
      direction <- args$direction %||% "both"
      k <- args$k %||% 2
      if (is.null(args$family)) {
        call_args <- list(formula = formula, data = data, na.action = args$na.action)
        if (!is.null(args$subset)) call_args$subset <- args$subset
        base_model <- do.call(stats::lm, call_args)
      } else {
        call_args <- list(formula = formula, data = data, family = args$family, na.action = args$na.action)
        if (!is.null(args$subset)) call_args$subset <- args$subset
        base_model <- do.call(stats::glm, call_args)
      }
      sel <- MASS::stepAIC(base_model, direction = direction, k = k, trace = 0)
      sm <- summary(sel)
      coef_terms <- names(stats::coef(sel))
      is_glm <- inherits(sel, "glm")
      ms <- if (is_glm) {
        private$.model_stats(sel, sm, sm$family$family)
      } else {
        private$.model_stats(sel, sm, "gaussian")
      }
      res <- list(test_type = "stepwise_fit",
                  method = sprintf("Stepwise selection (direction=%s, k=%.2f)", direction, k),
                  data_name = deparse(formula),
                  model = sel, coefficients = private$.coef_table(sm),
                  model_stats = ms,
                  selected_terms = coef_terms,
                  direction = direction, penalty_k = k,
                  residuals = as.numeric(stats::residuals(sel)),
                  fitted = as.numeric(stats::fitted(sel)),
                  formula = stats::formula(sel),
                  dist_type = if (is_glm) "asymptotic" else "t",
                  data = list(x = NULL, y = NULL))
      class(res) <- c("stat_result", "regression_result"); res$domain <- "regression"; res
    },
    .best_subset_fit = function(args) {
      formula <- args$formula; data <- args$data
      if (!is.data.frame(data)) stop("best_subset_fit: 'data' must be a data frame.", call. = FALSE)
      if (!requireNamespace("leaps", quietly = TRUE))
        stop("best_subset_fit requires the 'leaps' package.", call. = FALSE)
      mf <- stats::model.frame(formula, data = data, na.action = args$na.action)
      y <- stats::model.response(mf)
      X <- stats::model.matrix(formula, mf)[, -1, drop = FALSE]  # drop intercept
      p <- ncol(X)
      nvmax <- if (is.null(args$nvmax)) p else min(args$nvmax, p)
      rs <- leaps::regsubsets(x = X, y = y, nvmax = nvmax, method = "exhaustive",
                              really.big = (p > 30))
      rs_sm <- summary(rs)
      # Build a summary data.frame: one row per subset size
      summ <- data.frame(
        n_vars   = seq_len(nvmax),
        r_squared      = rs_sm$rsq,
        adj_r_squared  = rs_sm$adjr2,
        Cp       = rs_sm$cp,
        BIC      = rs_sm$bic,
        stringsAsFactors = FALSE
      )
      # Selected variables for each size
      sel_mat <- rs_sm$outmat  # character matrix, "*" marks selected
      selected_list <- lapply(seq_len(nrow(sel_mat)), function(i) {
        colnames(sel_mat)[sel_mat[i, ] == "*"]
      })
      summ$selected_vars <- vapply(selected_list, function(v) paste(v, collapse = ", "), character(1))
      n <- length(y)
      ms <- list(n = n, ncomp = NULL, r_squared = NA, adj_r_squared = NA,
                 sigma = NA, df_residual = NA, f_statistic = NULL, f_df = NULL,
                 f_p_value = NA, aic = NA, bic = NA, deviance = NA, null_deviance = NA,
                 family = "gaussian", link = "identity",
                 nvmax = nvmax, n_predictors = p)
      # Pick best by BIC as the "model"
      best_idx <- which.min(rs_sm$bic)
      res <- list(test_type = "best_subset_fit",
                  method = sprintf("Best subset selection (nvmax=%d, best by BIC: %d vars)", nvmax, best_idx),
                  data_name = deparse(formula),
                  model = rs, coefficients = NULL,
                  subset_summary = summ,
                  best_by_bic = list(n_vars = best_idx,
                                     variables = selected_list[[best_idx]],
                                     r_squared = rs_sm$rsq[best_idx],
                                     adj_r_squared = rs_sm$adjr2[best_idx],
                                     bic = rs_sm$bic[best_idx]),
                  model_stats = ms,
                  residuals = NULL, fitted = NULL,
                  formula = formula, dist_type = "asymptotic",
                  data = list(x = NULL, y = NULL))
      class(res) <- c("stat_result", "regression_result"); res$domain <- "regression"; res
    },

    # ----------------------------------------------------------------------
    # R3-D1: mars_fit / spline_fit
    # ----------------------------------------------------------------------
    .mars_fit = function(args) {
      formula <- args$formula; data <- args$data
      if (!is.data.frame(data)) stop("mars_fit: 'data' must be a data frame.", call. = FALSE)
      if (!requireNamespace("earth", quietly = TRUE))
        stop("mars_fit requires the 'earth' package.", call. = FALSE)
      degree <- args$degree %||% 1
      pmethod <- args$pmethod %||% "backward"
      thresh <- args$thresh %||% 0.001
      trace <- isTRUE(args$trace)
      # earth sets na.action internally to na.fail and rejects an explicit
      # na.action argument. Pre-filter NA rows so callers using na.omit get
      # equivalent behaviour without triggering earth's error.
      mf <- stats::model.frame(formula, data = data, na.action = args$na.action)
      data_clean <- as.data.frame(mf)
      call_args <- list(
        formula = formula, data = data_clean,
        degree = degree, pmethod = pmethod, thresh = thresh, trace = trace
      )
      if (!is.null(args$nk)) call_args$nk <- args$nk
      model <- do.call(earth::earth, call_args)
      sm <- summary(model)
      # earth's coefficient table: columns include Estimate, StdErr, tValue, pValue
      # (names vary slightly across versions; index defensively)
      raw_ct <- sm$coefficients
      # First row is "(Intercept)" in earth; some versions use "y" as intercept label.
      ct <- as.data.frame(raw_ct)
      # Normalize column names to canonical Term / Estimate / Std_Error / Statistic / p_value
      est_col  <- grep("Estimate",  names(ct), value = TRUE)[1]
      se_col   <- grep("Std|StdErr", names(ct), value = TRUE, ignore.case = TRUE)[1]
      stat_col <- grep("tValue|t$",  names(ct), value = TRUE, ignore.case = TRUE)[1]
      p_col    <- grep("pValue|p$",  names(ct), value = TRUE, ignore.case = TRUE)[1]
      coef_df <- data.frame(
        Term      = rownames(raw_ct),
        Estimate  = if (!is.na(est_col))  ct[[est_col]]  else NA_real_,
        Std_Error = if (!is.na(se_col))   ct[[se_col]]   else NA_real_,
        Statistic = if (!is.na(stat_col)) ct[[stat_col]] else NA_real_,
        p_value   = if (!is.na(p_col))    ct[[p_col]]    else NA_real_,
        stringsAsFactors = FALSE
      )
      rownames(coef_df) <- NULL
      # earth selected terms (hinge functions) -- coefficient path
      selected_terms <- tryCatch(
        colnames(model$dirs[model$selected.terms, , drop = FALSE]),
        error = function(e) names(stats::coef(model))
      )
      # Fit statistics: earth stores rsq, gcv, grsq in model$rss etc.
      rsq   <- model$rsq[length(model$rsq)]
      grsq  <- model$grsq[length(model$grsq)]
      gcv   <- model$gcv[length(model$gcv)]
      n     <- model$n
      n_terms <- length(model$selected.terms)
      ms <- list(
        n = n, n_terms = n_terms, degree = degree,
        r_squared = grsq, adj_r_squared = NA,
        sigma = sqrt(model$rss[length(model$rss)] / max(1L, n - n_terms)),
        df_residual = n - n_terms,
        f_statistic = NULL, f_df = NULL, f_p_value = NA,
        aic = NA, bic = NA,
        deviance = model$rss[length(model$rss)],
        null_deviance = NA,
        family = "gaussian", link = "identity",
        # MARS-specific
        generalized_rsq = grsq, rsq = rsq, gcv = gcv,
        pmethod = pmethod, nk = if (!is.null(args$nk)) args$nk else NA
      )
      res <- list(
        test_type = "mars_fit",
        method = sprintf("MARS (degree=%d, pmethod=%s, %d terms)", degree, pmethod, n_terms),
        data_name = deparse(formula),
        model = model,
        coefficients = coef_df,
        selected_terms = selected_terms,
        model_stats = ms,
        residuals = as.numeric(stats::residuals(model)),
        fitted = as.numeric(stats::fitted(model)),
        formula = formula,
        dist_type = "asymptotic",
        data = list(x = NULL, y = NULL)
      )
      class(res) <- c("stat_result", "regression_result"); res$domain <- "regression"; res
    },

    .spline_fit = function(args) {
      formula <- args$formula; data <- args$data
      if (!is.data.frame(data)) stop("spline_fit: 'data' must be a data frame.", call. = FALSE)
      # splines is a base recommended package; require it explicitly to be safe
      if (!requireNamespace("splines", quietly = TRUE))
        stop("spline_fit requires the 'splines' package (base recommended).", call. = FALSE)
      basis <- match.arg(args$basis %||% "bs", c("bs", "ns"))
      df <- args$df %||% 4
      degree <- args$degree %||% 3
      knots <- args$knots

      # Identify the first predictor on the RHS and build a new formula
      # with the spline basis applied to it. Other terms are preserved.
      trms <- stats::terms(formula, data = data)
      term_labels <- attr(trms, "term.labels")
      if (length(term_labels) < 1L) {
        stop("spline_fit: formula needs at least one predictor on the RHS.",
             call. = FALSE)
      }
      primary <- term_labels[1]
      others  <- term_labels[-1]

      # Build the basis call expression
      if (basis == "bs") {
        if (is.null(knots)) {
          basis_call <- sprintf("splines::bs(%s, df = %d, degree = %d)",
                                primary, df, degree)
        } else {
          knots_str <- paste(format(knots, trim = TRUE), collapse = ", ")
          basis_call <- sprintf("splines::bs(%s, knots = c(%s), degree = %d)",
                                primary, knots_str, degree)
        }
      } else {
        # natural spline: degree ignored, linear beyond boundary knots
        if (is.null(knots)) {
          basis_call <- sprintf("splines::ns(%s, df = %d)", primary, df)
        } else {
          knots_str <- paste(format(knots, trim = TRUE), collapse = ", ")
          basis_call <- sprintf("splines::ns(%s, knots = c(%s))",
                                primary, knots_str)
        }
      }
      rhs <- paste(c(basis_call, others), collapse = " + ")
      new_formula <- stats::as.formula(sprintf("%s ~ %s",
                                               deparse(formula[[2]]), rhs),
                                       env = environment(formula))

      call_args <- list(formula = new_formula, data = data,
                        na.action = args$na.action)
      if (!is.null(args$subset))  call_args$subset  <- args$subset
      if (!is.null(args$weights)) call_args$weights <- args$weights
      model <- do.call(stats::lm, call_args)
      sm <- summary(model)

      res <- list(
        test_type = "spline_fit",
        method = sprintf("%s spline regression (df=%d, predictor=%s)",
                         toupper(basis), df, primary),
        data_name = deparse(formula),
        model = model,
        coefficients = private$.coef_table(sm),
        model_stats = private$.model_stats(model, sm, "gaussian"),
        residuals = as.numeric(stats::residuals(model)),
        fitted = as.numeric(stats::fitted(model)),
        spline_basis = basis,
        spline_df = df,
        spline_degree = if (basis == "bs") degree else NA_integer_,
        spline_knots = knots,
        spline_predictor = primary,
        fitted_formula = new_formula,
        formula = formula,
        dist_type = "t",
        data = list(x = NULL, y = NULL)
      )
      class(res) <- c("stat_result", "regression_result"); res$domain <- "regression"; res
    }
  )
)
