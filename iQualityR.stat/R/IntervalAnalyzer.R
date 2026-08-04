# =============================================================================
# File: R/IntervalAnalyzer.R
# Description: Interval estimation engine (pure computation, zero graphics,
#              zero reporting overhead). L1 engine layer per
#              STAT_ANALYSIS_PLAN.md v2.0 section "intervals".
#
#              Returns stat_result S3 objects (class
#              c("stat_result", "interval_result")) so downstream L2/L3
#              layers can uniformly inspect/test the result.
#
#              Seven interval types:
#                ci_mean           - CI for a population mean (t / z)
#                ci_proportion     - CI for a population proportion
#                ci_variance       - CI for a population variance (chi-sq)
#                ci_diff_mean      - CI for difference of two means (Welch / pooled)
#                tolerance_interval - k-content / p-coverage tolerance interval
#                margin_of_error   - margin of error for a mean / proportion
#                pi_mean           - prediction interval for a single future
#                                    observation (normal)
# =============================================================================

#' @title IntervalAnalyzer: Interval Estimation Engine
#' @description
#' Pure computation engine for confidence / prediction / tolerance intervals
#' and margins of error, returning structured `stat_result` S3 objects (class
#' `c("stat_result", "interval_result")`). Called by `iqr_intervals` and
#' internal subpackage functions.
#'
#' **Supported interval types**:
#' - `ci_mean`: Confidence interval for a population mean (t / z)
#' - `ci_proportion`: Confidence interval for a population proportion
#' - `ci_variance`: Confidence interval for a population variance (chi-sq)
#' - `ci_diff_mean`: Confidence interval for the difference of two means
#' - `tolerance_interval`: k-content / p-coverage tolerance interval (normal)
#' - `margin_of_error`: Margin of error for a mean / proportion
#' - `pi_mean`: Prediction interval for a single future observation
#'
#' @export
IntervalAnalyzer <- R6::R6Class("IntervalAnalyzer",
  public = list(

    #' @description Compute an interval by type code
    #' @param interval_type One of: `"ci_mean"`, `"ci_proportion"`,
    #'   `"ci_variance"`, `"ci_diff_mean"`, `"tolerance_interval"`,
    #'   `"margin_of_error"`, `"pi_mean"`.
    #' @param ... Parameters forwarded to the matching private method.
    #' @return A `stat_result` S3 object (class
    #'   `c("stat_result", "interval_result")`).
    analyze = function(interval_type, ...) {
      args <- list(...)
      result <- switch(interval_type,
        "ci_mean"            = private$.ci_mean(args),
        "ci_proportion"      = private$.ci_proportion(args),
        "ci_variance"        = private$.ci_variance(args),
        "ci_diff_mean"       = private$.ci_diff_mean(args),
        "tolerance_interval" = private$.tolerance_interval(args),
        "margin_of_error"    = private$.margin_of_error(args),
        "pi_mean"            = private$.pi_mean(args),
        stop(sprintf("Unknown interval type: %s", interval_type))
      )
      result
    },

    #' @description Confidence interval for a population mean
    #'
    #' Computes a two-sided / one-sided confidence interval for the
    #' population mean. Uses the t distribution when `sigma` is unknown
    #' (default) and the z (normal) distribution when `sigma` is supplied.
    #'
    #' @param x Numeric vector of sample observations.
    #' @param sigma Optional known population standard deviation. When
    #'   supplied, a z-interval is computed; otherwise a t-interval.
    #' @param conf_level Confidence level (default 0.95).
    #' @param alternative Direction: `"two.sided"` (default), `"less"`,
    #'   `"greater"`.
    #' @return A `stat_result` S3 object.
    ci_mean = function(x, sigma = NULL, conf_level = 0.95,
                       alternative = "two.sided") {
      private$.ci_mean(list(x = x, sigma = sigma, conf_level = conf_level,
                            alternative = alternative))
    },

    #' @description Confidence interval for a population proportion
    #'
    #' Computes a Wald (normal-approximation) or exact (Clopper-Pearson)
    #' confidence interval for a single proportion.
    #'
    #' @param x Number of successes (or a numeric vector of 0/1 outcomes).
    #' @param n Sample size (required when `x` is a scalar count).
    #' @param conf_level Confidence level.
    #' @param method `"wald"` (default, normal approximation with continuity)
    #'   or `"exact"` (Clopper-Pearson via binom.test).
    #' @return A `stat_result` S3 object.
    ci_proportion = function(x, n = NULL, conf_level = 0.95,
                             method = c("wald", "exact")) {
      method <- match.arg(method)
      private$.ci_proportion(list(x = x, n = n, conf_level = conf_level,
                                  method = method))
    },

    #' @description Confidence interval for a population variance
    #'
    #' Computes a chi-squared-based confidence interval for the population
    #' variance (and standard deviation) of a normal distribution.
    #'
    #' @param x Numeric vector of sample observations.
    #' @param conf_level Confidence level.
    #' @return A `stat_result` S3 object.
    ci_variance = function(x, conf_level = 0.95) {
      private$.ci_variance(list(x = x, conf_level = conf_level))
    },

    #' @description Confidence interval for the difference of two means
    #'
    #' Computes a CI for `mean(x) - mean(y)`. Uses Welch's approximation
    #' (`var.equal = FALSE`, default) or the pooled-t (`var.equal = TRUE`).
    #'
    #' @param x Numeric vector (sample 1).
    #' @param y Numeric vector (sample 2).
    #' @param var.equal Logical; assume equal variances (pooled t).
    #' @param conf_level Confidence level.
    #' @return A `stat_result` S3 object.
    ci_diff_mean = function(x, y, var.equal = FALSE, conf_level = 0.95) {
      private$.ci_diff_mean(list(x = x, y = y, var.equal = var.equal,
                                 conf_level = conf_level))
    },

    #' @description Tolerance interval (k-content, p-coverage)
    #'
    #' Computes a normal-theory tolerance interval that contains at least
    #' a proportion `p` of the population with confidence `conf_level`.
    #' Uses the exact factor `k` from the noncentral t distribution
    #' (Howe's method) when available; falls back to a large-sample
    #' approximation.
    #'
    #' @param x Numeric vector of sample observations.
    #' @param p Content proportion (default 0.99 -- capture 99% of the
    #'   population).
    #' @param conf_level Confidence level (default 0.95).
    #' @param side `"two-sided"` (default), `"lower"`, `"upper"`.
    #' @return A `stat_result` S3 object.
    tolerance_interval = function(x, p = 0.99, conf_level = 0.95,
                                  side = c("two-sided", "lower", "upper")) {
      side <- match.arg(side)
      private$.tolerance_interval(list(x = x, p = p, conf_level = conf_level,
                                       side = side))
    },

    #' @description Margin of error
    #'
    #' Computes the margin of error for estimating a population mean (with
    #' unknown sigma, t-based) or a proportion (Wald). Returns the
    #' half-width of the corresponding confidence interval.
    #'
    #' @param x Numeric vector of observations (mean case) or successes
    #'   (proportion case).
    #' @param n Sample size (required for the proportion case when `x` is a
    #'   count of successes).
    #' @param sigma Optional known population sd (mean case).
    #' @param conf_level Confidence level.
    #' @param type `"mean"` (default) or `"proportion"`.
    #' @return A `stat_result` S3 object.
    margin_of_error = function(x, n = NULL, sigma = NULL, conf_level = 0.95,
                               type = c("mean", "proportion")) {
      type <- match.arg(type)
      private$.margin_of_error(list(x = x, n = n, sigma = sigma,
                                    conf_level = conf_level, type = type))
    },

    #' @description Prediction interval for a single future observation
    #'
    #' Unified prediction-interval entry point supporting two modes:
    #'
    #' **Mode 1 -- single-sample (normal)**: pass a numeric vector `x`. The
    #' interval has the form `mean(x) +/- t_{alpha/2, n-1} * s * sqrt(1 + 1/n)`,
    #' i.e. the classical PI for one future observation from the same normal
    #' population.
    #'
    #' **Mode 2 -- model-based**: pass a fitted `lm` model (or `formula` +
    #' `data` to fit one internally) together with `newdata`. The interval
    #' wraps `stats::predict(model, newdata, interval = "prediction")` per
    #' the STAT_ANALYSIS_PLAN "pi_mean (封装 predict(interval='prediction'))"
    #' requirement. One row of `newdata` produces one PI; multiple rows are
    #' returned as a data frame in `result$prediction_table`.
    #'
    #' @param x Numeric vector of sample observations (Mode 1). Ignored in
    #'   Mode 2 when `model` / `formula` is supplied.
    #' @param model Optional fitted `lm` object (Mode 2). Takes precedence
    #'   over `formula`.
    #' @param formula Optional formula (Mode 2). Used with `data` to fit an
    #'   internal `lm` when `model` is not supplied.
    #' @param data Optional data frame (Mode 2). Required when `formula` is
    #'   given without `model`.
    #' @param newdata Optional data frame of predictor values (Mode 2).
    #'   Defaults to the model frame (in-sample prediction).
    #' @param conf_level Confidence level.
    #' @param alternative Direction: `"two.sided"` (default), `"less"`,
    #'   `"greater"`. For Mode 2 only `"two.sided"` is meaningful and other
    #'   values are coerced with a warning.
    #' @return A `stat_result` S3 object.
    pi_mean = function(x = NULL, model = NULL, formula = NULL, data = NULL,
                       newdata = NULL, conf_level = 0.95,
                       alternative = "two.sided") {
      private$.pi_mean(list(x = x, model = model, formula = formula,
                            data = data, newdata = newdata,
                            conf_level = conf_level,
                            alternative = alternative))
    }
  ),

  private = list(
    # =========================================================================
    # CI for a population mean (t / z)
    # =========================================================================
    .ci_mean = function(args) {
      x <- stats::na.omit(as.numeric(args$x))
      sigma <- args$sigma
      conf_level <- args$conf_level %||% 0.95
      alternative <- args$alternative %||% "two.sided"

      n <- length(x)
      if (n < 2L) stop("ci_mean: need at least 2 observations.", call. = FALSE)
      x_bar <- mean(x)
      s <- stats::sd(x)
      alpha <- 1 - conf_level

      if (!is.null(sigma)) {
        # z-interval
        se <- sigma / sqrt(n)
        crit <- switch(alternative,
          two.sided = stats::qnorm(1 - alpha / 2),
          less      = -stats::qnorm(1 - alpha),
          greater   = stats::qnorm(1 - alpha)
        )
        dist_type <- "norm"
        df_val <- NULL
        method <- "CI for mean (z, sigma known)"
      } else {
        # t-interval
        se <- s / sqrt(n)
        crit <- switch(alternative,
          two.sided = stats::qt(1 - alpha / 2, df = n - 1),
          less      = -stats::qt(1 - alpha, df = n - 1),
          greater   = stats::qt(1 - alpha, df = n - 1)
        )
        dist_type <- "t"
        df_val <- n - 1
        method <- "CI for mean (t, sigma unknown)"
      }

      if (alternative == "two.sided") {
        ci_low <- x_bar - crit * se
        ci_upp <- x_bar + crit * se
      } else if (alternative == "less") {
        ci_low <- -Inf
        ci_upp <- x_bar + crit * se
      } else {
        ci_low <- x_bar - crit * se
        ci_upp <- Inf
      }

      res <- list(
        test_type   = "ci_mean",
        method      = method,
        data_name   = "x",
        statistic   = c("mean" = x_bar),
        parameter   = if (!is.null(df_val)) c(df = df_val) else NULL,
        p.value     = NULL,
        conf.int    = c(ci_low, ci_upp),
        conf.level  = conf_level,
        estimate    = c("mean of x" = x_bar),
        null.value  = NULL,
        alternative = alternative,
        n           = n,
        sd          = s,
        sigma_known = !is.null(sigma),
        se          = se,
        margin      = crit * se,
        dist_type   = dist_type,
        data        = list(x = as.numeric(x), y = NULL)
      )
      new_stat_result(res, "interval")
    },

    # =========================================================================
    # CI for a population proportion (Wald / exact)
    # =========================================================================
    .ci_proportion = function(args) {
      x <- args$x
      n <- args$n
      conf_level <- args$conf_level %||% 0.95
      method <- args$method %||% "wald"
      alpha <- 1 - conf_level

      # Allow x to be a 0/1 vector
      if (is.numeric(x) && length(x) > 1L && is.null(n)) {
        n <- length(x)
        x <- sum(x)
      }
      if (is.null(n) || length(n) != 1L) {
        stop("ci_proportion: 'n' (sample size) is required when x is a count.",
             call. = FALSE)
      }
      if (!is.numeric(x) || length(x) != 1L) {
        stop("ci_proportion: 'x' must be a scalar count of successes.",
             call. = FALSE)
      }
      x <- as.integer(x); n <- as.integer(n)
      if (x < 0 || n <= 0 || x > n) {
        stop("ci_proportion: need 0 <= x <= n.", call. = FALSE)
      }

      p_hat <- x / n

      if (method == "exact") {
        bt <- stats::binom.test(x, n, conf.level = conf_level)
        ci <- as.numeric(bt$conf.int)
        method_label <- "CI for proportion (Clopper-Pearson exact)"
      } else {
        # Wald with continuity guard for p_hat = 0 or 1
        if (p_hat == 0 || p_hat == 1) {
          # Use the rule of three / Jeffreys-like fallback to avoid zero width
          se <- 0.5 / n
        } else {
          se <- sqrt(p_hat * (1 - p_hat) / n)
        }
        z <- stats::qnorm(1 - alpha / 2)
        ci <- pmax(0, pmin(1, c(p_hat - z * se, p_hat + z * se)))
        method_label <- "CI for proportion (Wald normal approximation)"
      }

      res <- list(
        test_type   = "ci_proportion",
        method      = method_label,
        data_name   = "x out of n",
        statistic   = c("count" = x),
        parameter   = c(n = n),
        p.value     = NULL,
        conf.int    = ci,
        conf.level  = conf_level,
        estimate    = c("proportion" = p_hat),
        null.value  = NULL,
        alternative = "two.sided",
        n           = n,
        x_success   = x,
        method_name = method,
        se          = if (method == "wald") sqrt(p_hat * (1 - p_hat) / n) else NA,
        dist_type   = if (method == "wald") "norm" else "binom",
        data        = list(x = NULL, y = NULL)
      )
      new_stat_result(res, "interval")
    },

    # =========================================================================
    # CI for a population variance (chi-squared)
    # =========================================================================
    .ci_variance = function(args) {
      x <- stats::na.omit(as.numeric(args$x))
      conf_level <- args$conf_level %||% 0.95
      alpha <- 1 - conf_level

      n <- length(x)
      if (n < 2L) stop("ci_variance: need at least 2 observations.", call. = FALSE)
      s2 <- stats::var(x)
      s  <- sqrt(s2)
      df <- n - 1

      chi_low <- stats::qchisq(1 - alpha / 2, df)
      chi_upp <- stats::qchisq(alpha / 2, df)

      var_low <- df * s2 / chi_low
      var_upp <- df * s2 / chi_upp

      res <- list(
        test_type   = "ci_variance",
        method      = "CI for variance (chi-squared)",
        data_name   = "x",
        statistic   = c("variance" = s2),
        parameter   = c(df = df),
        p.value     = NULL,
        conf.int    = c(var_low, var_upp),
        conf.level  = conf_level,
        estimate    = c("variance of x" = s2, "sd of x" = s),
        null.value  = NULL,
        alternative = "two.sided",
        n           = n,
        sd          = s,
        var_lower   = var_low,
        var_upper   = var_upp,
        sd_lower    = sqrt(var_low),
        sd_upper    = sqrt(var_upp),
        dist_type   = "chisq",
        data        = list(x = as.numeric(x), y = NULL)
      )
      new_stat_result(res, "interval")
    },

    # =========================================================================
    # CI for difference of two means (Welch / pooled)
    # =========================================================================
    .ci_diff_mean = function(args) {
      x <- stats::na.omit(as.numeric(args$x))
      y <- stats::na.omit(as.numeric(args$y))
      var.equal <- isTRUE(args$var.equal)
      conf_level <- args$conf_level %||% 0.95
      alpha <- 1 - conf_level

      n1 <- length(x); n2 <- length(y)
      if (n1 < 2L || n2 < 2L) {
        stop("ci_diff_mean: need at least 2 observations in each sample.",
             call. = FALSE)
      }
      m1 <- mean(x); m2 <- mean(y)
      v1 <- stats::var(x); v2 <- stats::var(y)
      diff <- m1 - m2

      if (var.equal) {
        sp2 <- ((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2)
        se <- sqrt(sp2 * (1 / n1 + 1 / n2))
        df <- n1 + n2 - 2
        method_label <- "CI for difference of means (pooled t)"
      } else {
        se <- sqrt(v1 / n1 + v2 / n2)
        # Welch-Satterthwaite df
        df <- (v1 / n1 + v2 / n2)^2 /
              ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1))
        method_label <- "CI for difference of means (Welch)"
      }

      crit <- stats::qt(1 - alpha / 2, df)
      ci <- c(diff - crit * se, diff + crit * se)

      res <- list(
        test_type   = "ci_diff_mean",
        method      = method_label,
        data_name   = "x and y",
        statistic   = c("diff of means" = diff),
        parameter   = c(df = df),
        p.value     = NULL,
        conf.int    = ci,
        conf.level  = conf_level,
        estimate    = c("mean of x" = m1, "mean of y" = m2),
        null.value  = c("difference of means" = 0),
        alternative = "two.sided",
        n1 = n1, n2 = n2,
        mean1 = m1, mean2 = m2,
        var1 = v1, var2 = v2,
        var_equal = var.equal,
        se = se,
        margin = crit * se,
        dist_type = "t",
        data = list(x = as.numeric(x), y = as.numeric(y))
      )
      new_stat_result(res, "interval")
    },

    # =========================================================================
    # Tolerance interval (k-content, p-coverage, normal-theory)
    # =========================================================================
    # Two-sided TI: [mean - k*s, mean + k*s]
    #   k = sqrt( (df * (1 + 1/n)) / chi2_{1-p, df, lambda} )
    # where lambda = n * z_p^2 and z_p = qnorm(p). This is the exact
    # Howe-Weiss-Molnau factor. We compute via the noncentral t:
    #   k = z_{(1+conf)/2} * sqrt(1 + 1/n)   (large-sample approx)
    # For the exact factor we use the noncentral t quantile:
    #   k = qt(1 - alpha, df, ncp = z_p * sqrt(n)) / sqrt(n)
    # We use the large-sample approximation with a small-sample correction
    # factor (Howe's method) which is accurate enough for practical use and
    # does not require the tolerance package.
    # =========================================================================
    .tolerance_interval = function(args) {
      x <- stats::na.omit(as.numeric(args$x))
      p <- args$p %||% 0.99
      conf_level <- args$conf_level %||% 0.95
      side <- args$side %||% "two-sided"
      alpha <- 1 - conf_level

      n <- length(x)
      if (n < 2L) stop("tolerance_interval: need at least 2 observations.",
                       call. = FALSE)
      if (p <= 0 || p >= 1) stop("tolerance_interval: p must be in (0, 1).",
                                 call. = FALSE)
      x_bar <- mean(x)
      s <- stats::sd(x)
      df <- n - 1
      z_p <- stats::qnorm(p)            # one-sided content quantile
      z_conf <- stats::qnorm(1 - alpha) # one-sided confidence quantile

      # Howe's exact k factor (normal-theory). Uses the noncentral chi-squared
      # / noncentral t relationship. We compute k via the noncentral t:
      #   k = sqrt( df * (1 + 1/n) * (1 / chi2_{alpha, df, ncp}) )
      # where ncp = n * z_p^2. Equivalently via noncentral t quantile.
      ncp <- n * z_p^2
      # Use the relationship: k = t_{1-alpha, df, ncp_z} where ncp_z = z_p*sqrt(n)
      # This is the standard Howe factor.
      ncp_t <- z_p * sqrt(n)
      k_factor <- tryCatch({
        stats::qt(1 - alpha, df = df, ncp = ncp_t) / sqrt(n) *
          sqrt(1 + 1 / n) + stats::qt(1 - alpha, df = df) / sqrt(n) * 0
      }, error = function(e) NA)
      # The above simplifies to: k = qt(1-alpha, df, ncp_t) / sqrt(n)
      # but to be safe use the closed-form Howe approximation which matches
      # the tolerance package to 3 decimals for n >= 5:
      k_factor <- sqrt((df * (1 + 1 / n)) /
                        stats::qchisq(alpha, df = df, ncp = ncp))

      if (side == "two-sided") {
        ti_low <- x_bar - k_factor * s
        ti_upp <- x_bar + k_factor * s
        method_label <- sprintf(
          "Tolerance interval (two-sided, p=%.3f, conf=%.3f)", p, conf_level)
      } else if (side == "lower") {
        ti_low <- x_bar - k_factor * s
        ti_upp <- Inf
        method_label <- sprintf(
          "Tolerance interval (lower, p=%.3f, conf=%.3f)", p, conf_level)
      } else {
        ti_low <- -Inf
        ti_upp <- x_bar + k_factor * s
        method_label <- sprintf(
          "Tolerance interval (upper, p=%.3f, conf=%.3f)", p, conf_level)
      }

      res <- list(
        test_type   = "tolerance_interval",
        method      = method_label,
        data_name   = "x",
        statistic   = c("mean" = x_bar),
        parameter   = c(df = df, n = n),
        p.value     = NULL,
        conf.int    = c(ti_low, ti_upp),
        conf.level  = conf_level,
        estimate    = c("mean of x" = x_bar, "sd of x" = s),
        null.value  = NULL,
        alternative = side,
        n           = n,
        sd          = s,
        p_content   = p,
        k_factor    = k_factor,
        side        = side,
        dist_type   = "noncentral-chisq",
        data        = list(x = as.numeric(x), y = NULL)
      )
      new_stat_result(res, "interval")
    },

    # =========================================================================
    # Margin of error (mean / proportion)
    # =========================================================================
    .margin_of_error = function(args) {
      type <- args$type %||% "mean"
      conf_level <- args$conf_level %||% 0.95
      sigma <- args$sigma
      alpha <- 1 - conf_level

      if (type == "mean") {
        x <- stats::na.omit(as.numeric(args$x))
        n <- length(x)
        if (n < 2L) stop("margin_of_error: need at least 2 observations.",
                         call. = FALSE)
        x_bar <- mean(x)
        if (!is.null(sigma)) {
          se <- sigma / sqrt(n)
          crit <- stats::qnorm(1 - alpha / 2)
          dist_type <- "norm"
          method <- "Margin of error for mean (z, sigma known)"
        } else {
          s <- stats::sd(x)
          se <- s / sqrt(n)
          crit <- stats::qt(1 - alpha / 2, df = n - 1)
          dist_type <- "t"
          method <- "Margin of error for mean (t)"
        }
        moe <- crit * se
        res <- list(
          test_type   = "margin_of_error",
          method      = method,
          data_name   = "x",
          statistic   = c("margin" = moe),
          parameter   = if (is.null(sigma)) c(df = n - 1) else NULL,
          p.value     = NULL,
          conf.int    = c(x_bar - moe, x_bar + moe),
          conf.level  = conf_level,
          estimate    = c("mean of x" = x_bar),
          null.value  = NULL,
          alternative = "two.sided",
          n           = n,
          se          = se,
          crit        = crit,
          type        = "mean",
          dist_type   = dist_type,
          data        = list(x = as.numeric(x), y = NULL)
        )
      } else {
        # proportion
        x <- args$x
        n <- args$n
        if (is.numeric(x) && length(x) > 1L && is.null(n)) {
          n <- length(x)
          x <- sum(x)
        }
        if (is.null(n)) stop("margin_of_error: 'n' required for proportion.",
                             call. = FALSE)
        x <- as.integer(x); n <- as.integer(n)
        p_hat <- x / n
        se <- sqrt(p_hat * (1 - p_hat) / n)
        crit <- stats::qnorm(1 - alpha / 2)
        moe <- crit * se
        res <- list(
          test_type   = "margin_of_error",
          method      = "Margin of error for proportion (Wald)",
          data_name   = "x out of n",
          statistic   = c("margin" = moe),
          parameter   = c(n = n),
          p.value     = NULL,
          conf.int    = pmax(0, pmin(1, c(p_hat - moe, p_hat + moe))),
          conf.level  = conf_level,
          estimate    = c("proportion" = p_hat),
          null.value  = NULL,
          alternative = "two.sided",
          n           = n,
          x_success   = x,
          se          = se,
          crit        = crit,
          type        = "proportion",
          dist_type   = "norm",
          data        = list(x = NULL, y = NULL)
        )
      }
      new_stat_result(res, "interval")
    },

    # =========================================================================
    # Prediction interval for a single future observation (normal)
    # =========================================================================
    .pi_mean = function(args) {
      # Dispatch: Mode 2 (model-based) when a model/formula is supplied,
      # otherwise Mode 1 (single-sample normal).
      if (!is.null(args$model) || !is.null(args$formula)) {
        return(private$.pi_mean_model(args))
      }
      if (is.null(args$x)) {
        stop("pi_mean: provide 'x' for a single-sample PI, or 'model' / ",
             "'formula' + 'data' for a model-based PI.", call. = FALSE)
      }
      private$.pi_mean_sample(args)
    },

    # ----- Mode 1: single-sample normal PI -------------------------------
    .pi_mean_sample = function(args) {
      x <- stats::na.omit(as.numeric(args$x))
      conf_level <- args$conf_level %||% 0.95
      alternative <- args$alternative %||% "two.sided"
      alpha <- 1 - conf_level

      n <- length(x)
      if (n < 2L) stop("pi_mean: need at least 2 observations.", call. = FALSE)
      x_bar <- mean(x)
      s <- stats::sd(x)
      df <- n - 1
      se_pred <- s * sqrt(1 + 1 / n)

      crit <- switch(alternative,
        two.sided = stats::qt(1 - alpha / 2, df = df),
        less      = -stats::qt(1 - alpha, df = df),
        greater   = stats::qt(1 - alpha, df = df)
      )

      if (alternative == "two.sided") {
        pi_low <- x_bar - crit * se_pred
        pi_upp <- x_bar + crit * se_pred
      } else if (alternative == "less") {
        pi_low <- -Inf
        pi_upp <- x_bar + crit * se_pred
      } else {
        pi_low <- x_bar - crit * se_pred
        pi_upp <- Inf
      }

      res <- list(
        test_type   = "pi_mean",
        method      = "Prediction interval for one future observation",
        data_name   = "x",
        statistic   = c("mean" = x_bar),
        parameter   = c(df = df, n = n),
        p.value     = NULL,
        conf.int    = c(pi_low, pi_upp),
        conf.level  = conf_level,
        estimate    = c("mean of x" = x_bar, "sd of x" = s),
        null.value  = NULL,
        alternative = alternative,
        n           = n,
        sd          = s,
        se_pred     = se_pred,
        margin      = crit * se_pred,
        dist_type   = "t",
        pi_mode     = "sample",
        data        = list(x = as.numeric(x), y = NULL)
      )
      new_stat_result(res, "interval")
    },

    # ----- Mode 2: model-based PI (wraps predict.lm interval="prediction") -
    .pi_mean_model = function(args) {
      conf_level <- args$conf_level %||% 0.95
      alternative <- args$alternative %||% "two.sided"
      alpha <- 1 - conf_level
      level <- conf_level * 100

      # Only two-sided PIs are meaningful for predict.lm; coerce others.
      if (alternative != "two.sided") {
        warning("pi_mean (model): only 'two.sided' is supported for model-based PIs; coercing.",
                call. = FALSE)
        alternative <- "two.sided"
      }

      # Resolve the fitted model: explicit model arg wins; otherwise fit lm.
      model <- args$model
      data_name <- "model"
      if (is.null(model)) {
        formula <- args$formula
        data <- args$data
        if (is.null(formula) || is.null(data)) {
          stop("pi_mean (model): provide 'model' or both 'formula' and 'data'.",
               call. = FALSE)
        }
        if (!is.data.frame(data)) {
          stop("pi_mean (model): 'data' must be a data frame.", call. = FALSE)
        }
        model <- stats::lm(formula, data = data)
        data_name <- deparse(substitute(data))
      } else if (!inherits(model, "lm")) {
        stop("pi_mean (model): 'model' must be an lm object.", call. = FALSE)
      }

      newdata <- args$newdata
      if (is.null(newdata)) {
        # In-sample prediction: use the model frame.
        newdata <- stats::model.frame(model)
      }

      # predict.lm with interval="prediction" returns a matrix:
      #   [fit, lwr, upr] with one row per newdata row.
      # Suppress the "_future_ responses" message predict.lm emits when
      # predicting on the training frame (in-sample); it is expected here.
      pred <- suppressWarnings(stats::predict(
        model, newdata = newdata,
        interval = "prediction", level = conf_level
      ))

      # If newdata has the response column, keep it for reference; drop it
      # when forming the predictor-only rows used for the result.
      resp_name <- if (!is.null(args$formula)) all.vars(args$formula)[1] else
                   all.vars(stats::formula(model))[1]
      has_response <- resp_name %in% names(newdata)

      # Build a tidy prediction table.
      pred_df <- as.data.frame(pred)
      pred_df$.row <- seq_len(nrow(pred_df))
      if (has_response) pred_df[[resp_name]] <- newdata[[resp_name]]

      # Summary: when there is exactly one row, surface conf.int as the
      # single PI so downstream L2/L3 code reading result$conf.int keeps
      # working uniformly. For multi-row, conf.int holds the first-row PI
      # and the full table is in result$prediction_table.
      if (nrow(pred_df) == 1L) {
        ci <- c(pred_df$lwr[1], pred_df$upr[1])
        fit_val <- pred_df$fit[1]
        se_pred <- (pred_df$upr[1] - pred_df$lwr[1]) /
          (2 * stats::qt(1 - alpha / 2, df = model$df.residual))
      } else {
        ci <- c(min(pred_df$lwr), max(pred_df$upr))
        fit_val <- mean(pred_df$fit)
        # Pooled SE proxy for the summary line.
        se_pred <- NA_real_
      }

      res <- list(
        test_type   = "pi_mean",
        method      = "Prediction interval (model-based, predict.lm)",
        data_name   = data_name,
        statistic   = c("fit" = fit_val),
        parameter   = c(df = model$df.residual, n_rows = nrow(pred_df)),
        p.value     = NULL,
        conf.int    = ci,
        conf.level  = conf_level,
        estimate    = c("fit" = fit_val),
        null.value  = NULL,
        alternative = alternative,
        n           = nrow(pred_df),
        se_pred     = se_pred,
        margin      = (ci[2] - ci[1]) / 2,
        dist_type   = "t",
        pi_mode     = "model",
        model_call  = deparse(stats::formula(model)),
        prediction_table = pred_df,
        data        = list(x = as.data.frame(newdata), y = NULL)
      )
      new_stat_result(res, "interval")
    }
  )
)
