# =============================================================================
# File: R/model_diag.R
# Description: Model diagnostics module - regression diagnostics
# =============================================================================

#' @title Linear regression model diagnostics
#' @description
#' Performs comprehensive diagnostics on linear regression models, including residual analysis,
#' normality tests, heteroscedasticity tests, multicollinearity, and influence point detection.
#'
#' @param model lm object
#' @param alpha Significance level (default 0.05)
#'
#' @return List containing residuals (residual statistics), normality (normality test),
#'   heteroscedasticity (heteroscedasticity test), multicollinearity (multicollinearity),
#'   influence (influence points), assumptions (hypothesis test summary)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' y <- 2 * x + rnorm(100, sd = 0.5)
#' model <- lm(y ~ x)
#' diagnose_lm(model)
diagnose_lm <- function(model, alpha = 0.05) {
  if (!inherits(model, "lm")) {
    stop("model must be an lm object.")
  }

  res <- residuals(model)
  fitted <- fitted.values(model)
  n <- length(res)

  list(
    residuals = private_residual_stats(res, fitted),
    normality = private_normality_test(res, alpha),
    heteroscedasticity = private_heteroscedasticity_test(model, res, fitted, alpha),
    multicollinearity = private_multicollinearity_test(model),
    influence = private_influence_analysis(model, res),
    assumptions = private_assumption_summary(res, fitted, alpha),
    model_summary = private_model_summary(model),
    n = n
  )
}

#' @title Residual normality test
#' @description
#' Performs multiple normality tests on model residuals.
#'
#' @param model lm object
#' @param method Test method ("auto", "sw", "ad", "lillie", "cvm", "sf")
#' @param alpha Significance level (default 0.05)
#'
#' @return List of normality test results
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' y <- 2 * x + rnorm(100, sd = 0.5)
#' model <- lm(y ~ x)
#' test_residual_normality(model)
test_residual_normality <- function(model, method = "auto", alpha = 0.05) {
  if (!inherits(model, "lm")) {
    stop("model must be an lm object.")
  }

  res <- residuals(model)
  n <- length(res)

  if (method == "auto") {
    method <- if (n <= 5000) "sw" else "ad"
  }

  result <- switch(method,
    "sw" = list(
      method = "Shapiro-Wilk",
      statistic = stats::shapiro.test(res)$statistic,
      p.value = stats::shapiro.test(res)$p.value,
      is_normal = stats::shapiro.test(res)$p.value >= alpha
    ),
    "ad" = {
      if (!requireNamespace("nortest", quietly = TRUE)) {
        stop("nortest package required for AD test.")
      }
      ad <- nortest::ad.test(res)
      list(
        method = "Anderson-Darling",
        statistic = ad$statistic,
        p.value = ad$p.value,
        is_normal = ad$p.value >= alpha
      )
    },
    "lillie" = {
      if (!requireNamespace("nortest", quietly = TRUE)) {
        stop("nortest package required for Lilliefors test.")
      }
      lf <- nortest::lillie.test(res)
      list(
        method = "Lilliefors",
        statistic = lf$statistic,
        p.value = lf$p.value,
        is_normal = lf$p.value >= alpha
      )
    },
    "cvm" = {
      if (!requireNamespace("nortest", quietly = TRUE)) {
        stop("nortest package required for Cramer-von Mises test.")
      }
      cvm <- nortest::cvm.test(res)
      list(
        method = "Cramer-von Mises",
        statistic = cvm$statistic,
        p.value = cvm$p.value,
        is_normal = cvm$p.value >= alpha
      )
    },
    "sf" = {
      if (!requireNamespace("nortest", quietly = TRUE)) {
        stop("nortest package required for Shapiro-Francia test.")
      }
      sf <- nortest::sf.test(res)
      list(
        method = "Shapiro-Francia",
        statistic = sf$statistic,
        p.value = sf$p.value,
        is_normal = sf$p.value >= alpha
      )
    },
    stop(sprintf("Unknown method: %s", method))
  )

  result$n <- n
  result$skewness = moments::skewness(res)
  result$excess_kurtosis = moments::kurtosis(res) - 3

  result
}

#' @title Heteroscedasticity test
#' @description
#' Tests whether model residuals exhibit heteroscedasticity (non-constant variance).
#'
#' @param model lm object
#' @param test Test method ("bp" Breusch-Pagan, "ncv" Non-constant Variance, "white" White test)
#' @param alpha Significance level (default 0.05)
#'
#' @return List containing statistic (test statistic), p.value, is_heteroscedastic
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' y <- 2 * x + rnorm(100, sd = 0.5 + 0.1 * x)
#' model <- lm(y ~ x)
#' test_heteroscedasticity(model)
test_heteroscedasticity <- function(model, test = c("bp", "ncv", "white"), alpha = 0.05) {
  if (!inherits(model, "lm")) {
    stop("model must be an lm object.")
  }

  test <- match.arg(test)

  switch(test,
    "bp" = private_bp_test(model, alpha),
    "ncv" = private_ncv_test(model, alpha),
    "white" = private_white_test(model, alpha)
  )
}

#' @title Multicollinearity diagnostics
#' @description
#' Calculates Variance Inflation Factor (VIF) and condition index to diagnose multicollinearity among predictor variables.
#'
#' @param model lm object
#'
#' @return List containing vif (VIF values for each variable), condition_index (condition index),
#'   has_multicollinearity (whether severe multicollinearity exists)
#' @export
#'
#' @examples
#' set.seed(123)
#' x1 <- rnorm(100)
#' x2 <- x1 + rnorm(100, sd = 0.1)
#' y <- 2 * x1 + 3 * x2 + rnorm(100)
#' model <- lm(y ~ x1 + x2)
#' diagnose_multicollinearity(model)
diagnose_multicollinearity <- function(model) {
  if (!inherits(model, "lm")) {
    stop("model must be an lm object.")
  }

  private_multicollinearity_test(model)
}

#' @title Influence point diagnostics
#' @description
#' Identifies observations with abnormal influence on model fit (high leverage points, strongly influential points, outliers).
#'
#' @param model lm object
#' @param cook_threshold Cook's distance threshold (default 4/n)
#'
#' @return List containing high_leverage (high leverage points), outliers (outliers),
#'   influential (strongly influential points), cook_distances (Cook's distances)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' y <- 2 * x + rnorm(100, sd = 0.5)
#' y[50] <- y[50] + 10
#' model <- lm(y ~ x)
#' diagnose_influential_points(model)
diagnose_influential_points <- function(model, cook_threshold = NULL) {
  if (!inherits(model, "lm")) {
    stop("model must be an lm object.")
  }

  n <- length(residuals(model))
  p <- length(coef(model))

  if (is.null(cook_threshold)) {
    cook_threshold <- 4 / n
  }

  private_influence_analysis(model, residuals(model), cook_threshold)
}

#' @title Model assumption test summary
#' @description
#' Summarizes hypothesis test results for linear model assumptions.
#'
#' @param model lm object
#' @param alpha Significance level
#'
#' @return Data frame containing assumption (assumption name), test (test method),
#'   statistic (test statistic), p.value, passed (whether passed)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' y <- 2 * x + rnorm(100, sd = 0.5)
#' model <- lm(y ~ x)
#' summarize_assumptions(model)
summarize_assumptions <- function(model, alpha = 0.05) {
  if (!inherits(model, "lm")) {
    stop("model must be an lm object.")
  }

  diag_result <- diagnose_lm(model, alpha)

  data.frame(
    assumption = c("Linearity", "Normality", "Homoscedasticity", "Independence", "No severe multicollinearity"),
    test = c("Visual inspection", diag_result$normality$method, "Breusch-Pagan", "Durbin-Watson", "VIF"),
    statistic = c(
      NA,
      diag_result$normality$statistic,
      diag_result$heteroscedasticity$statistic,
      diag_result$assumptions$dw_stat,
      max(diag_result$multicollinearity$vif)
    ),
    p.value = c(
      NA,
      diag_result$normality$p.value,
      diag_result$heteroscedasticity$p.value,
      diag_result$assumptions$dw_p,
      NA
    ),
    passed = c(
      TRUE,
      diag_result$normality$is_normal,
      !diag_result$heteroscedasticity$is_heteroscedastic,
      diag_result$assumptions$dw_passed,
      !diag_result$multicollinearity$has_multicollinearity
    ),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Internal helper functions
# =============================================================================

private_residual_stats <- function(res, fitted) {
  list(
    mean = mean(res),
    sd = sd(res),
    min = min(res),
    max = max(res),
    median = median(res),
    skewness = moments::skewness(res),
    excess_kurtosis = moments::kurtosis(res) - 3
  )
}

private_normality_test <- function(res, alpha) {
  n <- length(res)
  method <- if (n <= 5000) "sw" else "ad"

  if (method == "sw") {
    sw <- stats::shapiro.test(res)
    list(
      method = "Shapiro-Wilk",
      statistic = sw$statistic,
      p.value = sw$p.value,
      is_normal = sw$p.value >= alpha,
      n = n,
      skewness = moments::skewness(res),
      excess_kurtosis = moments::kurtosis(res) - 3
    )
  } else {
    if (requireNamespace("nortest", quietly = TRUE)) {
      ad <- nortest::ad.test(res)
      list(
        method = "Anderson-Darling",
        statistic = ad$statistic,
        p.value = ad$p.value,
        is_normal = ad$p.value >= alpha,
        n = n,
        skewness = moments::skewness(res),
        excess_kurtosis = moments::kurtosis(res) - 3
      )
    } else {
      sw <- stats::shapiro.test(res)
      list(
        method = "Shapiro-Wilk",
        statistic = sw$statistic,
        p.value = sw$p.value,
        is_normal = sw$p.value >= alpha,
        n = n,
        skewness = moments::skewness(res),
        excess_kurtosis = moments::kurtosis(res) - 3
      )
    }
  }
}

private_heteroscedasticity_test <- function(model, res, fitted, alpha) {
  private_bp_test(model, alpha)
}

private_bp_test <- function(model, alpha = 0.05) {
  res <- residuals(model)
  fitted <- fitted.values(model)
  n <- length(res)

  aux_model <- stats::lm(res^2 ~ fitted)
  aux_summary <- summary(aux_model)

  bp_stat <- n * aux_summary$r.squared
  bp_p <- 1 - stats::pchisq(bp_stat, df = 1)

  list(
    method = "Breusch-Pagan",
    statistic = bp_stat,
    p.value = bp_p,
    is_heteroscedastic = bp_p < alpha
  )
}

private_ncv_test <- function(model, alpha = 0.05) {
  res <- residuals(model)
  fitted <- fitted.values(model)
  n <- length(res)

  z <- fitted
  g <- (res^2 / mean(res^2) - 1)
  reg <- stats::lm(g ~ z)
  reg_sum <- summary(reg)

  ncv_stat <- n * reg_sum$r.squared
  ncv_p <- 1 - stats::pchisq(ncv_stat, df = 1)

  list(
    method = "Non-constant Variance",
    statistic = ncv_stat,
    p.value = ncv_p,
    is_heteroscedastic = ncv_p < alpha
  )
}

private_white_test <- function(model, alpha = 0.05) {
  res <- residuals(model)
  fitted <- fitted.values(model)
  n <- length(res)

  aux_model <- stats::lm(res^2 ~ fitted + I(fitted^2))
  aux_summary <- summary(aux_model)

  white_stat <- n * aux_summary$r.squared
  white_p <- 1 - stats::pchisq(white_stat, df = 2)

  list(
    method = "White",
    statistic = white_stat,
    p.value = white_p,
    is_heteroscedastic = white_p < alpha
  )
}

private_multicollinearity_test <- function(model) {
  X <- model.matrix(model)
  X <- X[, -1, drop = FALSE]

  if (ncol(X) < 2) {
    return(list(
      vif = c(Intercept = 1),
      condition_index = 1,
      has_multicollinearity = FALSE,
      note = "Only one predictor; VIF not applicable."
    ))
  }

  vif_values <- sapply(1:ncol(X), function(i) {
    y <- X[, i]
    X_other <- X[, -i, drop = FALSE]
    aux <- stats::lm(y ~ X_other)
    1 / (1 - summary(aux)$r.squared)
  })
  names(vif_values) <- colnames(X)

  eigen_values <- eigen(crossprod(X))$values
  condition_index <- sqrt(max(eigen_values) / eigen_values)

  list(
    vif = vif_values,
    condition_index = condition_index,
    has_multicollinearity = any(vif_values > 10),
    max_vif = max(vif_values)
  )
}

private_influence_analysis <- function(model, res, cook_threshold = NULL) {
  n <- length(res)
  p <- length(coef(model))

  if (is.null(cook_threshold)) {
    cook_threshold <- 4 / n
  }

  hat_values <- hatvalues(model)
  cook_d <- cooks.distance(model)
  dffits <- dffits(model)
  dfbetas <- dfbetas(model)

  hat_threshold <- 2 * p / n
  dffits_threshold <- 2 * sqrt(p / n)

  high_leverage <- which(hat_values > hat_threshold)
  outliers <- which(abs(res) > 2 * sd(res))
  influential <- which(cook_d > cook_threshold)

  list(
    high_leverage = high_leverage,
    outliers = outliers,
    influential = influential,
    cook_distances = cook_d,
    hat_values = hat_values,
    dffits = dffits,
    dfbetas = dfbetas,
    thresholds = list(
      hat = hat_threshold,
      cook = cook_threshold,
      dffits = dffits_threshold
    )
  )
}

private_assumption_summary <- function(res, fitted, alpha) {
  dw_stat <- private_dw_test(res)

  list(
    dw_stat = dw_stat$statistic,
    dw_p = dw_stat$p.value,
    dw_passed = dw_stat$p.value >= alpha,
    dw_note = dw_stat$note
  )
}

private_dw_test <- function(res) {
  n <- length(res)
  d <- sum(diff(res)^2) / sum(res^2)

  dw_stat <- d

  # Under H0 (no autocorrelation): E[d] ~ 2, Var[d] ~ 4/n (Theil 1971, Savin & White 1978)
  # z-score = (d - 2) / sqrt(4/n) = sqrt(n) * (d - 2) / 2
  # Two-sided p-value via normal approximation
  z <- sqrt(n) * (dw_stat - 2) / 2
  p_value <- 2 * stats::pnorm(-abs(z))

  list(
    statistic = dw_stat,
    p.value = p_value,
    note = ifelse(dw_stat < 1.5, "Possible positive autocorrelation",
                  ifelse(dw_stat > 2.5, "Possible negative autocorrelation", "No significant autocorrelation"))
  )
}

private_model_summary <- function(model) {
  s <- summary(model)

  list(
    r_squared = s$r.squared,
    adj_r_squared = s$adj.r.squared,
    f_statistic = s$fstatistic[1],
    f_p_value = s$fstatistic[3],
    sigma = s$sigma,
    df = s$df[1],
    residual_df = s$df[2]
  )
}
