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
#' - Wilcoxon signed rank test (one-sample / paired)
#' - Wilcoxon rank sum test (two-sample, Mann-Whitney U)
#' - Kruskal-Wallis rank sum test (k-sample)
#' - Friedman rank sum test (randomized complete block)
#' - TOST for mean equivalence (one-sample / two-sample)
#' - TOST for proportion equivalence (two-sample)
#' - Non-inferiority test (mean or proportion, one-sided)
#' - Superiority test (mean or proportion, one-sided)
#' - One-sample Poisson rate test
#' - Two-sample Poisson rate test (rate ratio)
#'
#' @export
HTestAnalyzer <- R6::R6Class("HTestAnalyzer",
  public = list(
    #' @description Perform hypothesis test by test_type code
    #' @param test_type Test type code, one of:
    #'   `"z_test_1s"`, `"t_test_1s"`, `"t_test_2s"`, `"t_test_paired"`,
    #'   `"prop_test_1s"`, `"prop_test_2s"`, `"f_test"`, `"chisq_test"`,
    #'   `"wilcoxon_signed_rank"`, `"wilcoxon_rank_sum"`,
    #'   `"kruskal_wallis"`, `"friedman"`,
    #'   `"tost_mean"`, `"tost_proportion"`,
    #'   `"non_inferiority"`, `"superiority"`,
    #'   `"poisson_test_1s"`, `"poisson_test_2s"`
    #' @param ... Test parameters (forwarded to the matching private method)
    #' @return A `stat_result` S3 object (class `c("stat_result", "htest_result")`)
    analyze = function(test_type, ...) {
      args <- list(...)

      result <- switch(test_type,
        "z_test_1s"          = private$.z_test_1s(args),
        "t_test_1s"          = private$.t_test_1s(args),
        "t_test_2s"          = private$.t_test_2s(args),
        "t_test_paired"      = private$.t_test_paired(args),
        "prop_test_1s"       = private$.prop_test_1s(args),
        "prop_test_2s"       = private$.prop_test_2s(args),
        "f_test"             = private$.f_test(args),
        "chisq_test"         = private$.chisq_test(args),
        "wilcoxon_signed_rank" = private$.wilcoxon_signed_rank(args),
        "wilcoxon_rank_sum"    = private$.wilcoxon_rank_sum(args),
        "kruskal_wallis"       = private$.kruskal_wallis(args),
        "friedman"             = private$.friedman(args),
        "tost_mean"            = private$.tost_mean(args),
        "tost_proportion"      = private$.tost_proportion(args),
        "non_inferiority"      = private$.non_inferiority(args),
        "superiority"          = private$.superiority(args),
        "poisson_test_1s"      = private$.poisson_test_1s(args),
        "poisson_test_2s"      = private$.poisson_test_2s(args),
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
    },

    #' @description Wilcoxon signed rank test (one-sample or paired)
    #'
    #' Wraps [stats::wilcox.test] for the one-sample / paired case. When `y` is
    #' supplied the test is performed on the paired differences `x - y`.
    #'
    #' @param x First (or only) sample.
    #' @param y Optional second sample for paired comparison.
    #' @param mu Hypothesized location of `x` (or of `x - y` when `y` given).
    #' @param alternative Test direction (`"two.sided"`, `"less"`, `"greater"`).
    #' @param conf_level Confidence level.
    #' @param paired Logical; if `TRUE` and `y` is supplied, runs paired test.
    #'   Defaults to `TRUE` when `y` is non-NULL to honour the non-parametric
    #'   paired-test contract.
    #' @return A `stat_result` S3 object.
    wilcoxon_signed_rank = function(x, y = NULL, mu = 0,
                                    alternative = "two.sided",
                                    conf_level = 0.95, paired = TRUE) {
      private$.wilcoxon_signed_rank(list(
        x = x, y = y, mu = mu, alternative = alternative,
        conf_level = conf_level, paired = paired
      ))
    },

    #' @description Wilcoxon rank sum test (Mann-Whitney U, two-sample)
    #'
    #' Wraps [stats::wilcox.test] for two independent samples.
    #'
    #' @param x First sample.
    #' @param y Second sample.
    #' @param mu Hypothesized shift parameter.
    #' @param alternative Test direction.
    #' @param conf_level Confidence level.
    #' @return A `stat_result` S3 object.
    wilcoxon_rank_sum = function(x, y, mu = 0,
                                 alternative = "two.sided",
                                 conf_level = 0.95) {
      private$.wilcoxon_rank_sum(list(
        x = x, y = y, mu = mu, alternative = alternative,
        conf_level = conf_level
      ))
    },

    #' @description Kruskal-Wallis rank sum test
    #'
    #' Wraps [stats::kruskal.test] to compare `k` independent groups.
    #'
    #' @param x Numeric vector of observations, or a list of numeric vectors.
    #' @param g Grouping vector / factor (ignored when `x` is a list).
    #' @return A `stat_result` S3 object.
    kruskal_wallis = function(x, g = NULL) {
      private$.kruskal_wallis(list(x = x, g = g))
    },

    #' @description Friedman rank sum test (randomized complete block)
    #'
    #' Wraps [stats::friedman.test]. Accepts either a wide matrix / data frame
    #' (rows = blocks, columns = treatments) or the long form `x`/`g`/`b`.
    #'
    #' @param x Numeric vector, matrix, or data frame.
    #' @param g Treatment grouping vector (ignored when `x` is a matrix).
    #' @param b Blocking vector (ignored when `x` is a matrix).
    #' @return A `stat_result` S3 object.
    friedman = function(x, g = NULL, b = NULL) {
      private$.friedman(list(x = x, g = g, b = b))
    },

    #' @description TOST for mean equivalence (Two One-Sided Tests)
    #'
    #' Tests whether the mean of `x` (or the difference `x - y` when `y` is
    #' supplied) is practically equivalent to a reference value, i.e. within
    #' the equivalence margin `[-delta, +delta]`. The TOST procedure runs two
    #' one-sided t-tests at level `conf_level` and declares equivalence when
    #' both reject.
    #'
    #' @param x First (or only) sample.
    #' @param y Optional second sample for two-sample comparison.
    #' @param mu Reference value for one-sample; ignored when `y` is supplied.
    #' @param delta Equivalence margin (positive scalar). Equivalence is
    #'   declared when `|mean(x) - mu| < delta` (one-sample) or
    #'   `|mean(x) - mean(y)| < delta` (two-sample).
    #' @param conf_level Confidence level (also the significance level of each
    #'   one-sided test).
    #' @param var.equal Whether to assume equal variances for two-sample test.
    #' @return A `stat_result` S3 object with `extra$equivalence` field
    #'   indicating `"equivalent"` or `"not equivalent"`.
    tost_mean = function(x, y = NULL, mu = 0, delta, conf_level = 0.95,
                         var.equal = FALSE) {
      private$.tost_mean(list(
        x = x, y = y, mu = mu, delta = delta,
        conf_level = conf_level, var.equal = var.equal
      ))
    },

    #' @description TOST for two-sample proportion equivalence
    #'
    #' Tests whether the difference between two proportions is within the
    #' equivalence margin `[-delta, +delta]` using two one-sided z-tests.
    #'
    #' @param x1 Successes in group 1.
    #' @param n1 Sample size of group 1.
    #' @param x2 Successes in group 2.
    #' @param n2 Sample size of group 2.
    #' @param delta Equivalence margin on the proportion-difference scale
    #'   (positive scalar, typically 0.1-0.2).
    #' @param conf_level Confidence level.
    #' @return A `stat_result` S3 object.
    tost_proportion = function(x1, n1, x2, n2, delta, conf_level = 0.95) {
      private$.tost_proportion(list(
        x1 = x1, n1 = n1, x2 = x2, n2 = n2,
        delta = delta, conf_level = conf_level
      ))
    },

    #' @description Non-inferiority test (one-sided)
    #'
    #' Tests whether `x` (or `x - y` for two-sample) is non-inferior to a
    #' reference, i.e. the treatment is not worse than the reference by more
    #' than `delta`. For proportions, supply `x1/n1/x2/n2` instead of `x/y`.
    #'
    #' @param type `"mean"` (default) or `"proportion"`.
    #' @param x First sample (for `type = "mean"`) -- ignored for proportions.
    #' @param y Second sample (for `type = "mean"`) -- one-sample if NULL.
    #' @param mu Reference value for one-sample mean test.
    #' @param x1,n1,x2,n2 Counts / sizes for `type = "proportion"`.
    #' @param delta Non-inferiority margin. Rejecting H0: `x - ref <= -delta`
    #'   concludes non-inferiority.
    #' @param conf_level Confidence level.
    #' @param var.equal Equal-variance assumption for two-sample mean test.
    #' @return A `stat_result` S3 object.
    non_inferiority = function(type = c("mean", "proportion"),
                               x = NULL, y = NULL, mu = 0,
                               x1 = NULL, n1 = NULL, x2 = NULL, n2 = NULL,
                               delta, conf_level = 0.95, var.equal = FALSE) {
      private$.non_inferiority(list(
        type = match.arg(type), x = x, y = y, mu = mu,
        x1 = x1, n1 = n1, x2 = x2, n2 = n2,
        delta = delta, conf_level = conf_level, var.equal = var.equal
      ))
    },

    #' @description Superiority test (one-sided)
    #'
    #' Tests whether `x` (or `x - y` for two-sample) is superior to a
    #' reference by more than `delta`. Rejecting H0: `x - ref <= delta`
    #' concludes superiority.
    #'
    #' @param type `"mean"` (default) or `"proportion"`.
    #' @param x First sample (for `type = "mean"`) -- ignored for proportions.
    #' @param y Second sample (for `type = "mean"`) -- one-sample if NULL.
    #' @param mu Reference value for one-sample mean test.
    #' @param x1,n1,x2,n2 Counts / sizes for `type = "proportion"`.
    #' @param delta Superiority margin.
    #' @param conf_level Confidence level.
    #' @param var.equal Equal-variance assumption for two-sample mean test.
    #' @return A `stat_result` S3 object.
    superiority = function(type = c("mean", "proportion"),
                           x = NULL, y = NULL, mu = 0,
                           x1 = NULL, n1 = NULL, x2 = NULL, n2 = NULL,
                           delta, conf_level = 0.95, var.equal = FALSE) {
      private$.superiority(list(
        type = match.arg(type), x = x, y = y, mu = mu,
        x1 = x1, n1 = n1, x2 = x2, n2 = n2,
        delta = delta, conf_level = conf_level, var.equal = var.equal
      ))
    },

    #' @description One-sample Poisson rate test
    #'
    #' Tests whether the observed count `x` over exposure time `T` is
    #' consistent with a hypothesized rate `r`. Wraps [stats::poisson.test]
    #' (exact test based on the Poisson distribution).
    #'
    #' @param x Observed count (non-negative integer).
    #' @param T_exposure Exposure time / sample size base (default 1).
    #' @param r Hypothesized event rate (default 1).
    #' @param alternative Test direction (`"two.sided"`, `"less"`, `"greater"`).
    #' @param conf_level Confidence level.
    #' @return A `stat_result` S3 object.
    poisson_test_1s = function(x, T_exposure = 1, r = 1,
                               alternative = "two.sided", conf_level = 0.95) {
      private$.poisson_test_1s(list(
        x = x, T_exposure = T_exposure, r = r,
        alternative = alternative, conf_level = conf_level
      ))
    },

    #' @description Two-sample Poisson rate test (rate ratio)
    #'
    #' Compares the event rates of two independent Poisson counts.
    #' Wraps [stats::poisson.test] for the two-sample case. The null
    #' hypothesis is that the rate ratio `r1 / r2` equals 1.
    #'
    #' @param x1 Observed count in group 1.
    #' @param T1 Exposure time for group 1 (default 1).
    #' @param x2 Observed count in group 2.
    #' @param T2 Exposure time for group 2 (default 1).
    #' @param alternative Test direction.
    #' @param conf_level Confidence level.
    #' @return A `stat_result` S3 object.
    poisson_test_2s = function(x1, T1 = 1, x2 = NULL, T2 = 1,
                               alternative = "two.sided", conf_level = 0.95) {
      private$.poisson_test_2s(list(
        x1 = x1, T1 = T1, x2 = x2, T2 = T2,
        alternative = alternative, conf_level = conf_level
      ))
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
    },

    # =========================================================================
    # Wilcoxon signed rank test (one-sample / paired)
    # =========================================================================
    .wilcoxon_signed_rank = function(args) {
      x <- stats::na.omit(args$x)
      y <- args$y
      mu <- args$mu %||% 0
      alternative <- args$alternative %||% "two.sided"
      conf_level <- args$conf_level %||% 0.95
      paired <- isTRUE(args$paired) && !is.null(y)

      if (is.null(y)) {
        # One-sample Wilcoxon signed rank test
        ht <- stats::wilcox.test(x, mu = mu, alternative = alternative,
                                 conf.level = conf_level, conf.int = TRUE)
        method <- "Wilcoxon signed rank test (one-sample)"
        data_name <- "x"
        raw_x <- as.numeric(x); raw_y <- NULL
      } else {
        # Paired Wilcoxon signed rank test on x - y
        y <- stats::na.omit(y)
        if (length(x) != length(y)) {
          stop("x and y must have the same length for paired Wilcoxon test.",
               call. = FALSE)
        }
        ht <- stats::wilcox.test(x, y, paired = TRUE, mu = mu,
                                 alternative = alternative,
                                 conf.level = conf_level, conf.int = TRUE)
        method <- "Wilcoxon signed rank test (paired)"
        data_name <- "x and y"
        raw_x <- as.numeric(x); raw_y <- as.numeric(y)
      }

      # Parameter for Wilcoxon is the number of observations used by wilcox.test
      # (after dropping zeros / NAs). wilcox.test does not expose it directly,
      # so compute n as length of non-zero differences.
      diff_vec <- if (is.null(y)) x - mu else x - y
      n_used <- sum(diff_vec != 0, na.rm = TRUE)

      res <- list(
        test_type   = "wilcoxon_signed_rank",
        method      = method,
        data_name   = data_name,
        statistic   = ht$statistic,            # V (named)
        parameter   = c(n = n_used),
        p.value     = ht$p.value,
        conf.int    = if (!is.null(ht$conf.int)) as.numeric(ht$conf.int) else NULL,
        conf.level  = conf_level,
        estimate    = ht$estimate,             # (pseudo)median
        null.value  = c("location" = mu),
        alternative = alternative,
        n           = length(raw_x),
        n_used      = n_used,
        mu          = mu,
        paired      = paired,
        dist_type   = "wilcox",
        data        = list(x = raw_x, y = raw_y)
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # Wilcoxon rank sum test (Mann-Whitney U, two-sample)
    # =========================================================================
    .wilcoxon_rank_sum = function(args) {
      x <- stats::na.omit(args$x)
      y <- stats::na.omit(args$y)
      mu <- args$mu %||% 0
      alternative <- args$alternative %||% "two.sided"
      conf_level <- args$conf_level %||% 0.95

      ht <- stats::wilcox.test(x, y, paired = FALSE, mu = mu,
                               alternative = alternative,
                               conf.level = conf_level, conf.int = TRUE)

      n1 <- length(x); n2 <- length(y)

      res <- list(
        test_type   = "wilcoxon_rank_sum",
        method      = "Wilcoxon rank sum test (Mann-Whitney U)",
        data_name   = "x and y",
        statistic   = ht$statistic,            # W (named)
        parameter   = c(n1 = n1, n2 = n2),
        p.value     = ht$p.value,
        conf.int    = if (!is.null(ht$conf.int)) as.numeric(ht$conf.int) else NULL,
        conf.level  = conf_level,
        estimate    = ht$estimate,             # difference in location
        null.value  = c("location shift" = mu),
        alternative = alternative,
        n1 = n1, n2 = n2,
        mu  = mu,
        dist_type = "wilcox",
        data = list(x = as.numeric(x), y = as.numeric(y))
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # Kruskal-Wallis rank sum test
    # =========================================================================
    .kruskal_wallis = function(args) {
      x <- args$x
      g <- args$g

      if (is.list(x) && !is.data.frame(x)) {
        # List of numeric vectors -> reshape to long form
        g <- rep(seq_along(x), lengths(x))
        x <- unlist(x, use.names = FALSE)
      }

      if (is.null(g)) {
        stop("kruskal_wallis: grouping vector 'g' is required when 'x' is a vector.",
             call. = FALSE)
      }

      g <- as.factor(g)
      if (length(x) != length(g)) {
        stop("kruskal_wallis: 'x' and 'g' must have the same length.",
             call. = FALSE)
      }
      k <- nlevels(g)
      if (k < 2L) {
        stop("kruskal_wallis: need at least 2 groups.", call. = FALSE)
      }

      ht <- stats::kruskal.test(x ~ g)

      # Per-group sample sizes & mean ranks for the report
      group_n <- as.integer(table(g))
      names(group_n) <- levels(g)

      ranks <- rank(x)
      group_mean_rank <- tapply(ranks, g, mean)

      res <- list(
        test_type   = "kruskal_wallis",
        method      = "Kruskal-Wallis rank sum test",
        data_name   = "x by g",
        statistic   = ht$statistic,            # Kruskal-Wallis chi-squared
        parameter   = ht$parameter,            # df
        p.value     = ht$p.value,
        conf.int    = NULL,
        conf.level  = NULL,
        estimate    = NULL,
        null.value  = NULL,
        alternative = "two.sided",
        k           = k,
        group_n     = group_n,
        group_mean_rank = group_mean_rank,
        dist_type   = "chisq",
        data        = list(x = as.numeric(x), y = g)
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # Friedman rank sum test (randomized complete block)
    # =========================================================================
    .friedman = function(args) {
      x <- args$x
      g <- args$g
      b <- args$b

      # Coerce data frame input to matrix (rows = blocks, cols = treatments)
      if (is.data.frame(x)) {
        x <- as.matrix(x)
      }

      if (is.matrix(x)) {
        if (ncol(x) < 2L || nrow(x) < 2L) {
          stop("friedman: matrix needs at least 2 rows (blocks) and 2 columns (treatments).",
               call. = FALSE)
        }
        ht <- stats::friedman.test(x)
        n_blocks <- nrow(x)
        n_treat <- ncol(x)
        wide_mat <- x
      } else {
        if (is.null(g) || is.null(b)) {
          stop("friedman: when 'x' is a vector, both 'g' (treatment) and 'b' (block) are required.",
               call. = FALSE)
        }
        if (length(x) != length(g) || length(x) != length(b)) {
          stop("friedman: 'x', 'g', 'b' must have the same length.",
               call. = FALSE)
        }
        ht <- stats::friedman.test(x, g, b)
        g <- as.factor(g); b <- as.factor(b)
        n_blocks <- nlevels(b)
        n_treat <- nlevels(g)
        wide_mat <- NULL
      }

      res <- list(
        test_type   = "friedman",
        method      = "Friedman rank sum test",
        data_name   = if (is.matrix(x)) "x (matrix)" else "x, g, b",
        statistic   = ht$statistic,            # Friedman chi-squared
        parameter   = ht$parameter,            # df
        p.value     = ht$p.value,
        conf.int    = NULL,
        conf.level  = NULL,
        estimate    = NULL,
        null.value  = NULL,
        alternative = "two.sided",
        n_blocks    = n_blocks,
        n_treatments = n_treat,
        wide_matrix = wide_mat,
        dist_type   = "chisq",
        data        = list(x = if (is.matrix(x)) x else as.numeric(x), y = NULL)
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # TOST for mean equivalence (one-sample / two-sample)
    # =========================================================================
    .tost_mean = function(args) {
      x <- stats::na.omit(args$x)
      y <- args$y
      mu <- args$mu %||% 0
      delta <- args$delta
      conf_level <- args$conf_level %||% 0.95
      var_equal <- args$var.equal %||% FALSE

      if (is.null(delta) || length(delta) != 1L || delta <= 0) {
        stop("tost_mean: 'delta' must be a positive scalar (equivalence margin).",
             call. = FALSE)
      }

      alpha <- 1 - conf_level
      two_sided <- TRUE

      if (is.null(y)) {
        # One-sample TOST: H0: |mu_x - mu| >= delta
        n <- length(x)
        if (n < 2L) stop("tost_mean: need at least 2 observations.", call. = FALSE)
        x_bar <- mean(x); s <- sd(x); se <- s / sqrt(n)
        df <- n - 1
        method <- "TOST for mean equivalence (one-sample)"
        data_name <- "x"
        diff_est <- x_bar - mu
      } else {
        # Two-sample TOST: H0: |mu1 - mu2| >= delta
        y <- stats::na.omit(y)
        n1 <- length(x); n2 <- length(y)
        m1 <- mean(x); m2 <- mean(y)
        s1 <- sd(x); s2 <- sd(y)
        if (var_equal) {
          se <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)) *
                sqrt(1/n1 + 1/n2)
          df <- n1 + n2 - 2
        } else {
          se <- sqrt(s1^2/n1 + s2^2/n2)
          v1 <- s1^2/n1; v2 <- s2^2/n2
          df <- (v1 + v2)^2 / (v1^2/(n1 - 1) + v2^2/(n2 - 1))
        }
        n <- n1 + n2
        method <- "TOST for mean equivalence (two-sample)"
        data_name <- "x and y"
        diff_est <- m1 - m2
      }

      # Two one-sided t-tests
      # t1: H0: diff <= -delta  vs  H1: diff > -delta  (lower bound)
      # t2: H0: diff >=  delta  vs  H1: diff <  delta  (upper bound)
      t1 <- (diff_est - (-delta)) / se
      t2 <- (diff_est -   delta)  / se
      p1 <- stats::pt(t1, df = df, lower.tail = FALSE)   # P(T > t1)
      p2 <- stats::pt(t2, df = df, lower.tail = TRUE)    # P(T < t2)
      p_max <- max(p1, p2)
      equiv <- p_max < alpha

      # 100*(1-2*alpha)% CI is the conventional TOST CI
      ci_alpha <- 1 - 2 * alpha
      tcrit <- stats::qt(1 - alpha, df = df)
      ci_low <- diff_est - tcrit * se
      ci_upp <- diff_est + tcrit * se

      res <- list(
        test_type   = "tost_mean",
        method      = method,
        data_name   = data_name,
        statistic   = c(t1 = t1, t2 = t2),
        parameter   = c(df = df),
        p.value     = p_max,
        conf.int    = c(ci_low, ci_upp),
        conf.level  = ci_alpha,
        estimate    = c("difference" = diff_est),
        null.value  = c("equivalence margin" = delta),
        alternative = "equivalence",
        n           = n,
        delta       = delta,
        se          = se,
        t1 = t1, t2 = t2,
        p1 = p1, p2 = p2,
        equivalence = if (equiv) "equivalent" else "not equivalent",
        dist_type   = "t",
        data        = list(x = as.numeric(x), y = if (is.null(y)) NULL else as.numeric(y))
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # TOST for two-sample proportion equivalence
    # =========================================================================
    .tost_proportion = function(args) {
      x1 <- args$x1; n1 <- args$n1
      x2 <- args$x2; n2 <- args$n2
      delta <- args$delta
      conf_level <- args$conf_level %||% 0.95

      if (is.null(delta) || length(delta) != 1L || delta <= 0) {
        stop("tost_proportion: 'delta' must be a positive scalar.",
             call. = FALSE)
      }

      alpha <- 1 - conf_level
      p1 <- x1 / n1; p2 <- x2 / n2
      diff <- p1 - p2
      # Unpooled SE for the CI / TOST (common practice for proportion TOST)
      se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)

      z1 <- (diff - (-delta)) / se
      z2 <- (diff -   delta)  / se
      p1_test <- stats::pnorm(z1, lower.tail = FALSE)
      p2_test <- stats::pnorm(z2, lower.tail = TRUE)
      p_max <- max(p1_test, p2_test)
      equiv <- p_max < alpha

      ci_alpha <- 1 - 2 * alpha
      zcrit <- stats::qnorm(1 - alpha)
      ci_low <- diff - zcrit * se
      ci_upp <- diff + zcrit * se

      res <- list(
        test_type   = "tost_proportion",
        method      = "TOST for proportion equivalence (two-sample)",
        data_name   = sprintf("p1 = %d/%d, p2 = %d/%d", x1, n1, x2, n2),
        statistic   = c(z1 = z1, z2 = z2),
        parameter   = NULL,
        p.value     = p_max,
        conf.int    = c(ci_low, ci_upp),
        conf.level  = ci_alpha,
        estimate    = c("p1" = p1, "p2" = p2, "difference" = diff),
        null.value  = c("equivalence margin" = delta),
        alternative = "equivalence",
        n1 = n1, n2 = n2,
        x1 = x1, x2 = x2,
        delta = delta,
        se = se,
        z1 = z1, z2 = z2,
        p1_test = p1_test, p2_test = p2_test,
        equivalence = if (equiv) "equivalent" else "not equivalent",
        dist_type = "norm",
        data = list(x = NULL, y = NULL)
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # Non-inferiority test (one-sided)
    # =========================================================================
    .non_inferiority = function(args) {
      type <- args$type %||% "mean"
      delta <- args$delta
      conf_level <- args$conf_level %||% 0.95

      if (is.null(delta) || length(delta) != 1L) {
        stop("non_inferiority: 'delta' must be a scalar margin.", call. = FALSE)
      }

      alpha <- 1 - conf_level

      if (type == "mean") {
        x <- stats::na.omit(args$x)
        y <- args$y
        mu <- args$mu %||% 0
        var_equal <- args$var.equal %||% FALSE

        if (is.null(y)) {
          n <- length(x)
          if (n < 2L) stop("non_inferiority: need at least 2 observations.",
                           call. = FALSE)
          x_bar <- mean(x); s <- sd(x); se <- s / sqrt(n); df <- n - 1
          diff_est <- x_bar - mu
          data_name <- "x"
          raw_y <- NULL
        } else {
          y <- stats::na.omit(y)
          n1 <- length(x); n2 <- length(y)
          m1 <- mean(x); m2 <- mean(y)
          s1 <- sd(x); s2 <- sd(y)
          if (var_equal) {
            se <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2)) * sqrt(1/n1 + 1/n2)
            df <- n1 + n2 - 2
          } else {
            se <- sqrt(s1^2/n1 + s2^2/n2)
            v1 <- s1^2/n1; v2 <- s2^2/n2
            df <- (v1 + v2)^2 / (v1^2/(n1-1) + v2^2/(n2-1))
          }
          n <- n1 + n2
          diff_est <- m1 - m2
          data_name <- "x and y"
          raw_y <- as.numeric(y)
        }
        # H0: diff <= -delta  vs  H1: diff > -delta
        t_stat <- (diff_est - (-delta)) / se
        p_val <- stats::pt(t_stat, df = df, lower.tail = FALSE)
        ci_low <- diff_est - stats::qt(1 - alpha, df = df) * se
        res <- list(
          test_type   = "non_inferiority",
          method      = sprintf("Non-inferiority test (mean, delta = %.4f)", delta),
          data_name   = data_name,
          statistic   = c(t = t_stat),
          parameter   = c(df = df),
          p.value     = p_val,
          conf.int    = c(ci_low, Inf),
          conf.level  = conf_level,
          estimate    = c("difference" = diff_est),
          null.value  = c("non-inferiority margin" = -delta),
          alternative = "greater",
          n = n,
          delta = delta,
          se = se,
          type = "mean",
          non_inferior = p_val < alpha,
          dist_type = "t",
          data = list(x = as.numeric(x), y = raw_y)
        )
      } else if (type == "proportion") {
        x1 <- args$x1; n1 <- args$n1
        x2 <- args$x2; n2 <- args$n2
        if (is.null(x1) || is.null(n1) || is.null(x2) || is.null(n2)) {
          stop("non_inferiority (proportion): x1/n1/x2/n2 are required.",
               call. = FALSE)
        }
        p1 <- x1/n1; p2 <- x2/n2
        diff <- p1 - p2
        # Pooled SE under the null for the non-inferiority margin
        se <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
        z_stat <- (diff - (-delta)) / se
        p_val <- stats::pnorm(z_stat, lower.tail = FALSE)
        ci_low <- diff - stats::qnorm(1 - alpha) * se
        res <- list(
          test_type   = "non_inferiority",
          method      = sprintf("Non-inferiority test (proportion, delta = %.4f)", delta),
          data_name   = sprintf("p1 = %d/%d, p2 = %d/%d", x1, n1, x2, n2),
          statistic   = c(z = z_stat),
          parameter   = NULL,
          p.value     = p_val,
          conf.int    = c(ci_low, Inf),
          conf.level  = conf_level,
          estimate    = c("p1" = p1, "p2" = p2, "difference" = diff),
          null.value  = c("non-inferiority margin" = -delta),
          alternative = "greater",
          n1 = n1, n2 = n2,
          x1 = x1, x2 = x2,
          delta = delta,
          se = se,
          type = "proportion",
          non_inferior = p_val < alpha,
          dist_type = "norm",
          data = list(x = NULL, y = NULL)
        )
      } else {
        stop("non_inferiority: type must be 'mean' or 'proportion'.", call. = FALSE)
      }

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # Superiority test (one-sided)
    # =========================================================================
    .superiority = function(args) {
      type <- args$type %||% "mean"
      delta <- args$delta
      conf_level <- args$conf_level %||% 0.95

      if (is.null(delta) || length(delta) != 1L) {
        stop("superiority: 'delta' must be a scalar margin.", call. = FALSE)
      }

      alpha <- 1 - conf_level

      if (type == "mean") {
        x <- stats::na.omit(args$x)
        y <- args$y
        mu <- args$mu %||% 0
        var_equal <- args$var.equal %||% FALSE

        if (is.null(y)) {
          n <- length(x)
          if (n < 2L) stop("superiority: need at least 2 observations.",
                           call. = FALSE)
          x_bar <- mean(x); s <- sd(x); se <- s / sqrt(n); df <- n - 1
          diff_est <- x_bar - mu
          data_name <- "x"
          raw_y <- NULL
        } else {
          y <- stats::na.omit(y)
          n1 <- length(x); n2 <- length(y)
          m1 <- mean(x); m2 <- mean(y)
          s1 <- sd(x); s2 <- sd(y)
          if (var_equal) {
            se <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2)) * sqrt(1/n1 + 1/n2)
            df <- n1 + n2 - 2
          } else {
            se <- sqrt(s1^2/n1 + s2^2/n2)
            v1 <- s1^2/n1; v2 <- s2^2/n2
            df <- (v1 + v2)^2 / (v1^2/(n1-1) + v2^2/(n2-1))
          }
          n <- n1 + n2
          diff_est <- m1 - m2
          data_name <- "x and y"
          raw_y <- as.numeric(y)
        }
        # H0: diff <= delta  vs  H1: diff > delta
        t_stat <- (diff_est - delta) / se
        p_val <- stats::pt(t_stat, df = df, lower.tail = FALSE)
        ci_low <- diff_est - stats::qt(1 - alpha, df = df) * se
        res <- list(
          test_type   = "superiority",
          method      = sprintf("Superiority test (mean, delta = %.4f)", delta),
          data_name   = data_name,
          statistic   = c(t = t_stat),
          parameter   = c(df = df),
          p.value     = p_val,
          conf.int    = c(ci_low, Inf),
          conf.level  = conf_level,
          estimate    = c("difference" = diff_est),
          null.value  = c("superiority margin" = delta),
          alternative = "greater",
          n = n,
          delta = delta,
          se = se,
          type = "mean",
          superior = p_val < alpha,
          dist_type = "t",
          data = list(x = as.numeric(x), y = raw_y)
        )
      } else if (type == "proportion") {
        x1 <- args$x1; n1 <- args$n1
        x2 <- args$x2; n2 <- args$n2
        if (is.null(x1) || is.null(n1) || is.null(x2) || is.null(n2)) {
          stop("superiority (proportion): x1/n1/x2/n2 are required.",
               call. = FALSE)
        }
        p1 <- x1/n1; p2 <- x2/n2
        diff <- p1 - p2
        se <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
        z_stat <- (diff - delta) / se
        p_val <- stats::pnorm(z_stat, lower.tail = FALSE)
        ci_low <- diff - stats::qnorm(1 - alpha) * se
        res <- list(
          test_type   = "superiority",
          method      = sprintf("Superiority test (proportion, delta = %.4f)", delta),
          data_name   = sprintf("p1 = %d/%d, p2 = %d/%d", x1, n1, x2, n2),
          statistic   = c(z = z_stat),
          parameter   = NULL,
          p.value     = p_val,
          conf.int    = c(ci_low, Inf),
          conf.level  = conf_level,
          estimate    = c("p1" = p1, "p2" = p2, "difference" = diff),
          null.value  = c("superiority margin" = delta),
          alternative = "greater",
          n1 = n1, n2 = n2,
          x1 = x1, x2 = x2,
          delta = delta,
          se = se,
          type = "proportion",
          superior = p_val < alpha,
          dist_type = "norm",
          data = list(x = NULL, y = NULL)
        )
      } else {
        stop("superiority: type must be 'mean' or 'proportion'.", call. = FALSE)
      }

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # One-sample Poisson rate test
    # =========================================================================
    .poisson_test_1s = function(args) {
      x <- args$x
      T_exp <- args$T_exposure %||% 1
      r <- args$r %||% 1
      alternative <- args$alternative %||% "two.sided"
      conf_level <- args$conf_level %||% 0.95

      if (is.null(x) || length(x) != 1L || !is.numeric(x) || x < 0) {
        stop("poisson_test_1s: 'x' must be a non-negative numeric scalar (observed count).",
             call. = FALSE)
      }
      if (length(T_exp) != 1L || !is.numeric(T_exp) || T_exp <= 0) {
        stop("poisson_test_1s: 'T_exposure' must be a positive scalar.",
             call. = FALSE)
      }
      if (length(r) != 1L || !is.numeric(r) || r <= 0) {
        stop("poisson_test_1s: 'r' must be a positive scalar (hypothesized rate).",
             call. = FALSE)
      }

      ht <- stats::poisson.test(x = as.integer(round(x)), T = T_exp, r = r,
                                alternative = alternative,
                                conf.level = conf_level)

      res <- list(
        test_type   = "poisson_test_1s",
        method      = "Exact one-sample Poisson rate test",
        data_name   = sprintf("count = %d, exposure = %g", as.integer(round(x)), T_exp),
        statistic   = c("count" = as.numeric(ht$statistic)),
        parameter   = c("exposure" = T_exp),
        p.value     = ht$p.value,
        conf.int    = ht$conf.int,
        conf.level  = conf_level,
        estimate    = ht$estimate,
        null.value  = c("rate" = r),
        alternative = alternative,
        n           = as.integer(round(x)),
        T_exposure  = T_exp,
        rate        = as.numeric(ht$estimate),
        r0          = r,
        dist_type   = "poisson",
        data        = list(x = NULL, y = NULL)
      )

      new_stat_result(res, "htest")
    },

    # =========================================================================
    # Two-sample Poisson rate test (rate ratio)
    # =========================================================================
    .poisson_test_2s = function(args) {
      x1 <- args$x1; T1 <- args$T1 %||% 1
      x2 <- args$x2; T2 <- args$T2 %||% 1
      alternative <- args$alternative %||% "two.sided"
      conf_level <- args$conf_level %||% 0.95

      if (is.null(x1) || is.null(x2) || length(x1) != 1L || length(x2) != 1L ||
          !is.numeric(x1) || !is.numeric(x2) || x1 < 0 || x2 < 0) {
        stop("poisson_test_2s: 'x1' and 'x2' must be non-negative numeric scalars.",
             call. = FALSE)
      }
      if (length(T1) != 1L || length(T2) != 1L ||
          !is.numeric(T1) || !is.numeric(T2) || T1 <= 0 || T2 <= 0) {
        stop("poisson_test_2s: 'T1' and 'T2' must be positive scalars.",
             call. = FALSE)
      }

      ht <- stats::poisson.test(x = c(as.integer(round(x1)), as.integer(round(x2))),
                                T = c(T1, T2),
                                alternative = alternative,
                                conf.level = conf_level)

      rate1 <- as.numeric(x1) / T1
      rate2 <- as.numeric(x2) / T2

      res <- list(
        test_type   = "poisson_test_2s",
        method      = "Exact two-sample Poisson rate test (rate ratio)",
        data_name   = sprintf("count1 = %d (T=%g), count2 = %d (T=%g)",
                              as.integer(round(x1)), T1,
                              as.integer(round(x2)), T2),
        statistic   = c("count1" = as.numeric(x1), "count2" = as.numeric(x2)),
        parameter   = c("T1" = T1, "T2" = T2),
        p.value     = ht$p.value,
        conf.int    = ht$conf.int,
        conf.level  = conf_level,
        estimate    = ht$estimate,
        null.value  = c("rate ratio" = 1),
        alternative = alternative,
        x1 = as.integer(round(x1)), x2 = as.integer(round(x2)),
        T1 = T1, T2 = T2,
        rate1 = rate1, rate2 = rate2,
        rate_ratio = rate1 / rate2,
        dist_type   = "poisson",
        data        = list(x = NULL, y = NULL)
      )

      new_stat_result(res, "htest")
    }
  )
)
