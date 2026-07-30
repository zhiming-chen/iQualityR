# =============================================================================
# File: R/HTestAnalyzer.R
# Description: Hypothesis test calculation engine (pure computation, zero
#              graphics, zero reporting overhead). L1 engine layer per
#              STAT_ANALYSIS_PLAN.md v2.0 -- returns stat_result S3 objects so
#              downstream L2/L3 layers can uniformly inspect/test the result.
# =============================================================================

#' @title HTestAnalyzer: Hypothesis Test Calculation Engine
#' @description
#' A pure computation engine for performing various hypothesis tests, returning
#' structured `stat_result` S3 objects (class `c("stat_result", "htest_result")`).
#' Called by `iqr_htest` and internal subpackage functions.
#'
#' **Supported test types**:
#' - One-sample Z-test (population standard deviation known)
#' - One-sample t-test (population standard deviation unknown)
#' - Two-sample t-test (independent samples)
#' - Paired t-test
#' - One-sample proportion test
#' - Two-sample proportion test
#' - Variance test (F-test)
#' - Chi-square test
#'
#' @export
HTestAnalyzer <- R6::R6Class("HTestAnalyzer",
  public = list(
    #' @description Perform hypothesis test by test_type code
    #' @param test_type Test type code, one of:
    #'   `"z_test_1s"`, `"t_test_1s"`, `"t_test_2s"`, `"t_test_paired"`,
    #'   `"prop_test_1s"`, `"prop_test_2s"`, `"f_test"`, `"chisq_test"`
    #' @param ... Test parameters (forwarded to the matching private method)
    #' @return A `stat_result` S3 object (class `c("stat_result", "htest_result")`)
    analyze = function(test_type, ...) {
      args <- list(...)

      result <- switch(test_type,
        "z_test_1s"      = private$.z_test_1s(args),
        "t_test_1s"      = private$.t_test_1s(args),
        "t_test_2s"      = private$.t_test_2s(args),
        "t_test_paired"  = private$.t_test_paired(args),
        "prop_test_1s"   = private$.prop_test_1s(args),
        "prop_test_2s"   = private$.prop_test_2s(args),
        "f_test"         = private$.f_test(args),
        "chisq_test"     = private$.chisq_test(args),
        stop(sprintf("Unknown test type: %s", test_type))
      )

      result
    },

    #' @description One-sample Z-test
    #' @param x Sample data vector, or list containing `mean`/`n`/`sigma`
    #' @param mu Hypothesized population mean
    #' @param sigma Known population standard deviation
    #' @param alternative Test direction (`"two.sided"`, `"less"`, `"greater"`)
    #' @param conf_level Confidence level
    #' @return A `stat_result` S3 object
    z_test_1s = function(x, mu = 0, sigma, alternative = "two.sided", conf_level = 0.95) {
      private$.z_test_1s(list(
        x = x, mu = mu, sigma = sigma,
        alternative = alternative, conf_level = conf_level
      ))
    },

    #' @description One-sample t-test
    #' @param x Sample data vector
    #' @param mu Hypothesized population mean
    #' @param alternative Test direction
    #' @param conf_level Confidence level
    #' @return A `stat_result` S3 object
    t_test_1s = function(x, mu = 0, alternative = "two.sided", conf_level = 0.95) {
      private$.t_test_1s(list(
        x = x, mu = mu, alternative = alternative, conf_level = conf_level
      ))
    },

    #' @description Two-sample t-test
    #' @param x First sample
    #' @param y Second sample
    #' @param alternative Test direction
    #' @param conf_level Confidence level
    #' @param var.equal Whether variances are equal
    #' @return A `stat_result` S3 object
    t_test_2s = function(x, y, alternative = "two.sided", conf_level = 0.95, var.equal = FALSE) {
      private$.t_test_2s(list(
        x = x, y = y, alternative = alternative,
        conf_level = conf_level, var.equal = var.equal
      ))
    },

    #' @description Paired t-test
    #' @param x First sample
    #' @param y Second sample
    #' @param alternative Test direction
    #' @param conf_level Confidence level
    #' @return A `stat_result` S3 object
    t_test_paired = function(x, y, alternative = "two.sided", conf_level = 0.95) {
      private$.t_test_paired(list(
        x = x, y = y, alternative = alternative, conf_level = conf_level
      ))
    },

    #' @description One-sample proportion test
    #' @param x Number of successes
    #' @param n Total sample size
    #' @param p0 Hypothesized proportion
    #' @param alternative Test direction
    #' @param conf_level Confidence level
    #' @return A `stat_result` S3 object
    prop_test_1s = function(x, n, p0 = 0.5, alternative = "two.sided", conf_level = 0.95) {
      private$.prop_test_1s(list(
        x = x, n = n, p0 = p0, alternative = alternative, conf_level = conf_level
      ))
    },

    #' @description Two-sample proportion test
    #' @param x1 Number of successes in first group
    #' @param n1 Sample size of first group
    #' @param x2 Number of successes in second group
    #' @param n2 Sample size of second group
    #' @param alternative Test direction
    #' @param conf_level Confidence level
    #' @return A `stat_result` S3 object
    prop_test_2s = function(x1, n1, x2, n2, alternative = "two.sided", conf_level = 0.95) {
      private$.prop_test_2s(list(
        x1 = x1, n1 = n1, x2 = x2, n2 = n2,
        alternative = alternative, conf_level = conf_level
      ))
    },

    #' @description Variance equality test (F-test)
    #' @param x First sample
    #' @param y Second sample
    #' @param alternative Test direction
    #' @param conf_level Confidence level
    #' @return A `stat_result` S3 object
    f_test = function(x, y, alternative = "two.sided", conf_level = 0.95) {
      private$.f_test(list(
        x = x, y = y, alternative = alternative, conf_level = conf_level
      ))
    },

    #' @description Chi-square test
    #' @param x Vector of observed frequencies or contingency table
    #' @param p Vector of expected probabilities
    #' @return A `stat_result` S3 object
    chisq_test = function(x, p = NULL) {
      private$.chisq_test(list(x = x, p = p))
    }
  ),

  private = list(
    # =========================================================================
    # One-sample Z-test
    # =========================================================================
    .z_test_1s = function(args) {
      x <- args$x
      mu <- args$mu %||% 0
      sigma <- args$sigma
      alternative <- args$alternative %||% "two.sided"
      conf_level <- args$conf_level %||% 0.95

      if (is.null(sigma)) stop("sigma is required for Z test.")

      # Capture raw data for plotting (NULL when only summary stats provided)
      raw_x <- if (is.numeric(x) && length(x) > 1L) x else NULL

      if (is.numeric(x) && length(x) > 1L) {
        n <- length(x)
        x_bar <- mean(x)
        s <- sd(x)
      } else if (is.list(x) && !is.null(x$mean) && !is.null(x$n)) {
        n <- x$n
        x_bar <- x$mean
        s <- x$sd %||% sigma
      } else {
        stop("x must be a numeric vector or a list with mean/n/sigma.")
      }

      se <- sigma / sqrt(n)
      z <- (x_bar - mu) / se

      p_value <- switch(alternative,
        two.sided = 2 * (1 - pnorm(abs(z))),
        less = pnorm(z),
        greater = 1 - pnorm(z)
      )

      if (alternative == "two.sided") {
        ci_low <- x_bar - qnorm(1 - (1 - conf_level) / 2) * se
        ci_upp <- x_bar + qnorm(1 - (1 - conf_level) / 2) * se
      } else if (alternative == "greater") {
        ci_low <- x_bar - qnorm(1 - (1 - conf_level)) * se
        ci_upp <- Inf
      } else {
        ci_low <- -Inf
        ci_upp <- x_bar + qnorm(1 - (1 - conf_level)) * se
      }

      crit <- if (alternative == "two.sided") {
        qnorm(1 - (1 - conf_level) / 2)
      } else {
        qnorm(conf_level)
      }

      res <- list(
        test_type   = "z_test_1s",
        method      = "One Sample Z-test",
        data_name   = if (!is.null(raw_x)) "x" else "summary data",
        statistic   = c(Z = z),
        parameter   = NULL,
        p.value     = p_value,
        conf.int    = c(ci_low, ci_upp),
        conf.level  = conf_level,
        estimate    = c("mean of x" = x_bar),
        null.value  = c("mean" = mu),
        alternative = alternative,
        n           = n,
        mean        = x_bar,
        sd          = s,
        sigma       = sigma,
        se          = se,
        critical_value = crit,
        dist_type   = "norm",
        data        = list(x = raw_x, y = NULL)
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # One-sample t-test
    # =========================================================================
    .t_test_1s = function(args) {
      x <- args$x
      mu <- args$mu %||% 0
      alternative <- args$alternative %||% "two.sided"
      conf_level <- args$conf_level %||% 0.95

      x <- stats::na.omit(x)
      n <- length(x)
      if (n < 2) stop("Need at least 2 observations for t test.")

      x_bar <- mean(x)
      s <- sd(x)
      se <- s / sqrt(n)
      df <- n - 1
      t_stat <- (x_bar - mu) / se

      p_value <- switch(alternative,
        two.sided = 2 * (1 - pt(abs(t_stat), df = df)),
        less = pt(t_stat, df = df),
        greater = 1 - pt(t_stat, df = df)
      )

      if (alternative == "two.sided") {
        ci_low <- x_bar - qt(1 - (1 - conf_level) / 2, df = df) * se
        ci_upp <- x_bar + qt(1 - (1 - conf_level) / 2, df = df) * se
      } else if (alternative == "greater") {
        ci_low <- x_bar - qt(1 - (1 - conf_level), df = df) * se
        ci_upp <- Inf
      } else {
        ci_low <- -Inf
        ci_upp <- x_bar + qt(1 - (1 - conf_level), df = df) * se
      }

      crit <- if (alternative == "two.sided") {
        qt(1 - (1 - conf_level) / 2, df = df)
      } else {
        qt(conf_level, df = df)
      }

      res <- list(
        test_type   = "t_test_1s",
        method      = "One Sample t-test",
        data_name   = "x",
        statistic   = c("t" = t_stat),
        parameter   = c(df = df),
        p.value     = p_value,
        conf.int    = c(ci_low, ci_upp),
        conf.level  = conf_level,
        estimate    = c("mean of x" = x_bar),
        null.value  = c("mean" = mu),
        alternative = alternative,
        n           = n,
        mean        = x_bar,
        sd          = s,
        se          = se,
        critical_value = crit,
        dist_type   = "t",
        data        = list(x = as.numeric(x), y = NULL)
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # Two-sample t-test
    # =========================================================================
    .t_test_2s = function(args) {
      x <- stats::na.omit(args$x)
      y <- stats::na.omit(args$y)
      alternative <- args$alternative %||% "two.sided"
      conf_level <- args$conf_level %||% 0.95
      var_equal <- args$var.equal %||% FALSE

      n1 <- length(x); n2 <- length(y)
      m1 <- mean(x); m2 <- mean(y)
      s1 <- sd(x); s2 <- sd(y)

      if (var_equal) {
        se <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)) * sqrt(1/n1 + 1/n2)
        df <- n1 + n2 - 2
      } else {
        se <- sqrt(s1^2 / n1 + s2^2 / n2)
        v1 <- s1^2 / n1; v2 <- s2^2 / n2
        df <- (v1 + v2)^2 / (v1^2 / (n1 - 1) + v2^2 / (n2 - 1))
      }

      t_stat <- (m1 - m2) / se

      p_value <- switch(alternative,
        two.sided = 2 * (1 - pt(abs(t_stat), df = df)),
        less = pt(t_stat, df = df),
        greater = 1 - pt(t_stat, df = df)
      )

      diff_mean <- m1 - m2
      if (alternative == "two.sided") {
        ci_low <- diff_mean - qt(1 - (1 - conf_level) / 2, df = df) * se
        ci_upp <- diff_mean + qt(1 - (1 - conf_level) / 2, df = df) * se
      } else if (alternative == "greater") {
        ci_low <- diff_mean - qt(1 - (1 - conf_level), df = df) * se
        ci_upp <- Inf
      } else {
        ci_low <- -Inf
        ci_upp <- diff_mean + qt(1 - (1 - conf_level), df = df) * se
      }

      crit <- if (alternative == "two.sided") {
        qt(1 - (1 - conf_level) / 2, df = df)
      } else {
        qt(1 - (1 - conf_level), df = df)
      }

      res <- list(
        test_type   = "t_test_2s",
        method      = if (var_equal) "Two Sample t-test (equal var)" else "Welch Two Sample t-test",
        data_name   = "x and y",
        statistic   = c("t" = t_stat),
        parameter   = c(df = df),
        p.value     = p_value,
        conf.int    = c(ci_low, ci_upp),
        conf.level  = conf_level,
        estimate    = c("mean of x" = m1, "mean of y" = m2),
        null.value  = c("difference in means" = 0),
        alternative = alternative,
        n1 = n1, n2 = n2,
        mean1 = m1, mean2 = m2,
        sd1 = s1, sd2 = s2,
        diff_mean = diff_mean,
        se = se,
        critical_value = crit,
        dist_type = "t",
        data = list(x = as.numeric(x), y = as.numeric(y))
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # Paired t-test
    # =========================================================================
    .t_test_paired = function(args) {
      x <- stats::na.omit(args$x)
      y <- stats::na.omit(args$y)
      alternative <- args$alternative %||% "two.sided"
      conf_level <- args$conf_level %||% 0.95

      if (length(x) != length(y)) stop("x and y must have the same length for paired test.")

      d <- x - y
      n <- length(d)
      d_bar <- mean(d)
      s_d <- sd(d)
      se <- s_d / sqrt(n)
      df <- n - 1
      t_stat <- d_bar / se

      p_value <- switch(alternative,
        two.sided = 2 * (1 - pt(abs(t_stat), df = df)),
        less = pt(t_stat, df = df),
        greater = 1 - pt(t_stat, df = df)
      )

      if (alternative == "two.sided") {
        ci_low <- d_bar - qt(1 - (1 - conf_level) / 2, df = df) * se
        ci_upp <- d_bar + qt(1 - (1 - conf_level) / 2, df = df) * se
      } else if (alternative == "greater") {
        ci_low <- d_bar - qt(1 - (1 - conf_level), df = df) * se
        ci_upp <- Inf
      } else {
        ci_low <- -Inf
        ci_upp <- d_bar + qt(1 - (1 - conf_level), df = df) * se
      }

      crit <- if (alternative == "two.sided") {
        qt(1 - (1 - conf_level) / 2, df = df)
      } else {
        qt(1 - (1 - conf_level), df = df)
      }

      res <- list(
        test_type   = "t_test_paired",
        method      = "Paired t-test",
        data_name   = "x and y",
        statistic   = c("t" = t_stat),
        parameter   = c(df = df),
        p.value     = p_value,
        conf.int    = c(ci_low, ci_upp),
        conf.level  = conf_level,
        estimate    = c("mean of differences" = d_bar),
        null.value  = c("mean" = 0),
        alternative = alternative,
        n           = n,
        mean_diff   = d_bar,
        sd_diff     = s_d,
        se          = se,
        critical_value = crit,
        dist_type   = "t",
        data        = list(x = as.numeric(x), y = as.numeric(y))
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # One-sample proportion test
    # =========================================================================
    .prop_test_1s = function(args) {
      x <- args$x
      n <- args$n
      p0 <- args$p0 %||% 0.5
      alternative <- args$alternative %||% "two.sided"
      conf_level <- args$conf_level %||% 0.95

      p_hat <- x / n
      se <- sqrt(p0 * (1 - p0) / n)
      z <- (p_hat - p0) / se

      p_value <- switch(alternative,
        two.sided = 2 * (1 - pnorm(abs(z))),
        less = pnorm(z),
        greater = 1 - pnorm(z)
      )

      if (alternative == "two.sided") {
        ci_low <- p_hat - qnorm(1 - (1 - conf_level) / 2) * sqrt(p_hat * (1 - p_hat) / n)
        ci_upp <- p_hat + qnorm(1 - (1 - conf_level) / 2) * sqrt(p_hat * (1 - p_hat) / n)
      } else if (alternative == "greater") {
        ci_low <- p_hat - qnorm(1 - (1 - conf_level)) * sqrt(p_hat * (1 - p_hat) / n)
        ci_upp <- 1
      } else {
        ci_low <- 0
        ci_upp <- p_hat + qnorm(1 - (1 - conf_level)) * sqrt(p_hat * (1 - p_hat) / n)
      }

      crit <- if (alternative == "two.sided") {
        qnorm(1 - (1 - conf_level) / 2)
      } else {
        qnorm(1 - (1 - conf_level))
      }

      res <- list(
        test_type   = "prop_test_1s",
        method      = "One Sample Proportion Z-test",
        data_name   = sprintf("x = %d out of n = %d", x, n),
        statistic   = c("Z" = z),
        parameter   = NULL,
        p.value     = p_value,
        conf.int    = c(ci_low, ci_upp),
        conf.level  = conf_level,
        estimate    = c("p" = p_hat),
        null.value  = c("p" = p0),
        alternative = alternative,
        n           = n,
        x_success   = x,
        p_hat       = p_hat,
        p0          = p0,
        se          = se,
        critical_value = crit,
        dist_type   = "norm",
        data        = list(x = NULL, y = NULL)  # summary-only test, no raw vector
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # Two-sample proportion test
    # =========================================================================
    .prop_test_2s = function(args) {
      x1 <- args$x1; n1 <- args$n1
      x2 <- args$x2; n2 <- args$n2
      alternative <- args$alternative %||% "two.sided"
      conf_level <- args$conf_level %||% 0.95

      p1 <- x1 / n1; p2 <- x2 / n2
      p_pool <- (x1 + x2) / (n1 + n2)
      se <- sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))
      z <- (p1 - p2) / se

      p_value <- switch(alternative,
        two.sided = 2 * (1 - pnorm(abs(z))),
        less = pnorm(z),
        greater = 1 - pnorm(z)
      )

      diff <- p1 - p2
      se_diff <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
      if (alternative == "two.sided") {
        ci_low <- diff - qnorm(1 - (1 - conf_level) / 2) * se_diff
        ci_upp <- diff + qnorm(1 - (1 - conf_level) / 2) * se_diff
      } else if (alternative == "greater") {
        ci_low <- diff - qnorm(1 - (1 - conf_level)) * se_diff
        ci_upp <- 1
      } else {
        ci_low <- -1
        ci_upp <- diff + qnorm(1 - (1 - conf_level)) * se_diff
      }

      crit <- if (alternative == "two.sided") {
        qnorm(1 - (1 - conf_level) / 2)
      } else {
        qnorm(1 - (1 - conf_level))
      }

      res <- list(
        test_type   = "prop_test_2s",
        method      = "Two Sample Proportion Z-test",
        data_name   = sprintf("p1 = %d/%d, p2 = %d/%d", x1, n1, x2, n2),
        statistic   = c("Z" = z),
        parameter   = NULL,
        p.value     = p_value,
        conf.int    = c(ci_low, ci_upp),
        conf.level  = conf_level,
        estimate    = c("p1" = p1, "p2" = p2),
        null.value  = c("difference" = 0),
        alternative = alternative,
        n1 = n1, n2 = n2,
        x1 = x1, x2 = x2,
        p1 = p1, p2 = p2,
        p_pool = p_pool,
        diff = diff,
        se = se,
        critical_value = crit,
        dist_type = "norm",
        data = list(x = NULL, y = NULL)  # summary-only test
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # F-test (variance equality)
    # =========================================================================
    .f_test = function(args) {
      x <- stats::na.omit(args$x)
      y <- stats::na.omit(args$y)
      alternative <- args$alternative %||% "two.sided"
      conf_level <- args$conf_level %||% 0.95

      n1 <- length(x); n2 <- length(y)
      var1 <- var(x); var2 <- var(y)
      f_stat <- var1 / var2
      df1 <- n1 - 1; df2 <- n2 - 1

      p_value <- switch(alternative,
        two.sided = 2 * min(pf(f_stat, df1, df2), 1 - pf(f_stat, df1, df2)),
        less = pf(f_stat, df1, df2),
        greater = 1 - pf(f_stat, df1, df2)
      )

      crit_low <- if (alternative == "two.sided") {
        stats::qf((1 - conf_level) / 2, df1, df2)
      } else if (alternative == "less") {
        stats::qf(1 - conf_level, df1, df2)
      } else {
        NULL
      }
      crit_upp <- if (alternative == "two.sided") {
        stats::qf(1 - (1 - conf_level) / 2, df1, df2)
      } else if (alternative == "greater") {
        stats::qf(1 - (1 - conf_level), df1, df2)
      } else {
        NULL
      }

      res <- list(
        test_type   = "f_test",
        method      = "F test to compare two variances",
        data_name   = "x and y",
        statistic   = c("F" = f_stat),
        parameter   = c(num_df = df1, den_df = df2),
        p.value     = p_value,
        conf.int    = NULL,
        conf.level  = conf_level,
        estimate    = c("variance of x" = var1, "variance of y" = var2),
        null.value  = c("ratio of variances" = 1),
        alternative = alternative,
        n1 = n1, n2 = n2,
        var1 = var1, var2 = var2,
        critical_value_low = crit_low,
        critical_value_upp = crit_upp,
        dist_type = "f",
        data = list(x = as.numeric(x), y = as.numeric(y))
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # Chi-square test
    # =========================================================================
    .chisq_test = function(args) {
      x <- args$x
      p <- args$p

      if (is.matrix(x) || is.table(x)) {
        result <- stats::chisq.test(x)
      } else if (is.null(p)) {
        result <- stats::chisq.test(x)
      } else {
        result <- stats::chisq.test(x, p = p)
      }

      res <- list(
        test_type   = "chisq_test",
        method      = result$method,
        data_name   = "x",
        statistic   = c("X-squared" = result$statistic),
        parameter   = c(df = result$parameter),
        p.value     = result$p.value,
        conf.int    = NULL,
        conf.level  = NULL,
        estimate    = NULL,
        null.value  = NULL,
        alternative = "two.sided",
        observed    = result$observed,
        expected    = result$expected,
        residuals   = result$residuals,
        dist_type   = "chisq",
        data        = list(x = x, y = NULL)
      )

      new_stat_result(res, "htest")
    }
  )
)
