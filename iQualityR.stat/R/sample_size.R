# =============================================================================
# File: R/sample_size.R
# Description: Sample size and power calculation module
# =============================================================================

#' @title Sample size for one-sample mean test
#' @description
#' Calculates sample size required for one-sample t-test or Z-test.
#'
#' @param mu0 Hypothesized population mean
#' @param mu1 Alternative hypothesis mean (actual mean)
#' @param sigma Population standard deviation (for Z-test) or estimated standard deviation (for t-test)
#' @param alpha Significance level (default 0.05)
#' @param power Power (default 0.80)
#' @param alternative Alternative hypothesis direction ("two.sided", "less", "greater")
#' @param test_type Test type ("t" or "z")
#'
#' @return List containing n (required sample size), power (actual power), parameters (input parameters)
#' @export
#'
#' @examples
#' sample_size_mean(mu0 = 10, mu1 = 10.5, sigma = 1, power = 0.80)
sample_size_mean <- function(mu0, mu1, sigma, alpha = 0.05, power = 0.80,
                              alternative = c("two.sided", "less", "greater"),
                              test_type = c("t", "z")) {
  alternative <- match.arg(alternative)
  test_type <- match.arg(test_type)

  delta <- abs(mu1 - mu0)
  if (delta == 0) stop("mu1 must differ from mu0.")
  if (sigma <= 0) stop("sigma must be positive.")

  effect_size <- delta / sigma

  z_alpha <- switch(alternative,
    "two.sided" = stats::qnorm(1 - alpha / 2),
    stats::qnorm(1 - alpha)
  )
  z_beta <- stats::qnorm(power)

  n <- ((z_alpha + z_beta) / effect_size)^2

  if (test_type == "t") {
    n <- private_adjust_t_sample_size(n, alpha, power, effect_size, alternative)
  }

  n <- ceiling(n)
  actual_power <- private_calc_power(n, effect_size, alpha, alternative, test_type)

  list(
    n = n,
    actual_power = actual_power,
    effect_size = effect_size,
    parameters = list(
      mu0 = mu0, mu1 = mu1, sigma = sigma,
      alpha = alpha, power = power,
      alternative = alternative, test_type = test_type
    )
  )
}

#' @title Two-sample mean test sample size calculation
#' @description
#' Calculates per-group sample size required for two-sample t-test.
#'
#' @param mu1 First group mean
#' @param mu2 Second group mean
#' @param sigma Common standard deviation (assuming equal variances)
#' @param alpha Significance level (default 0.05)
#' @param power Power (default 0.80)
#' @param alternative Alternative hypothesis direction
#' @param ratio Ratio of second group to first group sample size (default 1, i.e., equal sample size)
#'
#' @return List containing n1, n2 (required sample sizes for two groups), power, effect_size
#' @export
#'
#' @examples
#' sample_size_two_means(mu1 = 10, mu2 = 11, sigma = 1.5, power = 0.90)
sample_size_two_means <- function(mu1, mu2, sigma, alpha = 0.05, power = 0.80,
                                   alternative = c("two.sided", "less", "greater"),
                                   ratio = 1) {
  alternative <- match.arg(alternative)

  delta <- abs(mu1 - mu2)
  if (delta == 0) stop("mu1 must differ from mu2.")
  if (sigma <= 0) stop("sigma must be positive.")
  if (ratio <= 0) stop("ratio must be positive.")

  effect_size <- delta / sigma

  z_alpha <- switch(alternative,
    "two.sided" = stats::qnorm(1 - alpha / 2),
    stats::qnorm(1 - alpha)
  )
  z_beta <- stats::qnorm(power)

  n1 <- ((z_alpha + z_beta)^2 * (1 + 1/ratio)) / effect_size^2
  n2 <- n1 * ratio

  actual_power <- private_calc_power_two_sample(ceiling(n1), ceiling(n2), effect_size, alpha, alternative)

  list(
    n1 = ceiling(n1),
    n2 = ceiling(n2),
    total_n = ceiling(n1) + ceiling(n2),
    actual_power = actual_power,
    effect_size = effect_size,
    parameters = list(
      mu1 = mu1, mu2 = mu2, sigma = sigma,
      alpha = alpha, power = power,
      alternative = alternative, ratio = ratio
    )
  )
}

#' @title One-sample proportion test sample size calculation
#' @description
#' Calculates sample size required for one-sample proportion test.
#'
#' @param p0 Hypothesized proportion
#' @param p1 Alternative hypothesis proportion
#' @param alpha Significance level (default 0.05)
#' @param power Power (default 0.80)
#' @param alternative Alternative hypothesis direction
#'
#' @return List containing n (required sample size), power, effect_size
#' @export
#'
#' @examples
#' sample_size_proportion(p0 = 0.05, p1 = 0.08, power = 0.80)
sample_size_proportion <- function(p0, p1, alpha = 0.05, power = 0.80,
                                    alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  if (p0 <= 0 || p0 >= 1) stop("p0 must be between 0 and 1.")
  if (p1 <= 0 || p1 >= 1) stop("p1 must be between 0 and 1.")

  delta <- abs(p1 - p0)
  if (delta == 0) stop("p1 must differ from p0.")

  effect_size <- private_es_h(p0, p1)

  z_alpha <- switch(alternative,
    "two.sided" = stats::qnorm(1 - alpha / 2),
    stats::qnorm(1 - alpha)
  )
  z_beta <- stats::qnorm(power)

  n <- ((z_alpha * sqrt(p0 * (1 - p0)) + z_beta * sqrt(p1 * (1 - p1))) / delta)^2

  actual_power <- private_calc_power_proportion(ceiling(n), p0, p1, alpha, alternative)

  list(
    n = ceiling(n),
    actual_power = actual_power,
    effect_size = effect_size,
    parameters = list(
      p0 = p0, p1 = p1,
      alpha = alpha, power = power,
      alternative = alternative
    )
  )
}

#' @title Two-sample proportion test sample size calculation
#' @description
#' Calculates per-group sample size required for two-sample proportion test.
#'
#' @param p1 First group proportion
#' @param p2 Second group proportion
#' @param alpha Significance level (default 0.05)
#' @param power Power (default 0.80)
#' @param alternative Alternative hypothesis direction
#' @param ratio Ratio of second group to first group sample size
#'
#' @return List containing n1, n2, power, effect_size
#' @export
#'
#' @examples
#' sample_size_two_proportions(p1 = 0.10, p2 = 0.15, power = 0.80)
sample_size_two_proportions <- function(p1, p2, alpha = 0.05, power = 0.80,
                                         alternative = c("two.sided", "less", "greater"),
                                         ratio = 1) {
  alternative <- match.arg(alternative)

  if (p1 <= 0 || p1 >= 1) stop("p1 must be between 0 and 1.")
  if (p2 <= 0 || p2 >= 1) stop("p2 must be between 0 and 1.")

  delta <- abs(p1 - p2)
  if (delta == 0) stop("p2 must differ from p1.")

  effect_size <- private_es_h(p1, p2)

  z_alpha <- switch(alternative,
    "two.sided" = stats::qnorm(1 - alpha / 2),
    stats::qnorm(1 - alpha)
  )
  z_beta <- stats::qnorm(power)

  p_bar <- (p1 + p2 * ratio) / (1 + ratio)

  n1 <- (z_alpha * sqrt(p_bar * (1 - p_bar) * (1 + 1/ratio)) +
         z_beta * sqrt(p1 * (1 - p1) + p2 * (1 - p2) / ratio))^2 / delta^2
  n2 <- n1 * ratio

  actual_power <- private_calc_power_two_proportion(ceiling(n1), ceiling(n2), p1, p2, alpha, alternative)

  list(
    n1 = ceiling(n1),
    n2 = ceiling(n2),
    total_n = ceiling(n1) + ceiling(n2),
    actual_power = actual_power,
    effect_size = effect_size,
    parameters = list(
      p1 = p1, p2 = p2,
      alpha = alpha, power = power,
      alternative = alternative, ratio = ratio
    )
  )
}

#' @title ANOVA sample size calculation
#' @description
#' Calculates per-group sample size required for one-way ANOVA.
#'
#' @param k Number of groups
#' @param means Vector of group means
#' @param sigma Common standard deviation
#' @param alpha Significance level (default 0.05)
#' @param power Power (default 0.80)
#'
#' @return List containing n_per_group (sample size per group), total_n, power, effect_size_f
#' @export
#'
#' @examples
#' sample_size_anova(k = 3, means = c(10, 11, 12), sigma = 1.5, power = 0.80)
sample_size_anova <- function(k, means, sigma, alpha = 0.05, power = 0.80) {
  if (k < 2) stop("k must be at least 2.")
  if (length(means) != k) stop("Length of means must equal k.")
  if (sigma <= 0) stop("sigma must be positive.")

  grand_mean <- mean(means)
  effect_size_f <- sqrt(sum((means - grand_mean)^2) / k) / sigma

  if (effect_size_f == 0) stop("All means are equal; no sample size can detect a difference.")

  z_alpha <- stats::qf(1 - alpha, df1 = k - 1, df2 = Inf)
  z_alpha <- sqrt(z_alpha)

  z_beta <- stats::qnorm(power)

  # Noncentrality parameter: lambda = n * k * f^2, so n = lambda / (k * f^2).
  # Using the normal approximation lambda ~ (z_alpha + z_beta)^2 gives:
  #   n_per_group = (z_alpha + z_beta)^2 / (k * effect_size_f^2)
  n_per_group <- max(2L, ceiling(((z_alpha + z_beta) / effect_size_f)^2 / k))

  # Refine via the noncentral F distribution: the normal approximation for the
  # F-test is crude with small denominator df, so search for the minimum n that
  # actually achieves the target power.
  refine_power <- function(n) {
    lambda <- n * sum((means - grand_mean)^2) / sigma^2
    df2 <- k * (n - 1)
    f_crit <- stats::qf(1 - alpha, k - 1, df2)
    1 - stats::pf(f_crit, k - 1, df2, ncp = lambda)
  }

  actual_power <- refine_power(n_per_group)
  # Decrease if the approximation over-estimated.
  while (n_per_group > 2) {
    lower_power <- refine_power(n_per_group - 1)
    if (lower_power >= power) {
      n_per_group <- n_per_group - 1
      actual_power <- lower_power
    } else {
      break
    }
  }
  # Increase if the target power is not yet reached.
  while (actual_power < power && n_per_group < 100000L) {
    n_per_group <- n_per_group + 1
    actual_power <- refine_power(n_per_group)
  }

  list(
    n_per_group = n_per_group,
    total_n = n_per_group * k,
    actual_power = actual_power,
    effect_size_f = effect_size_f,
    parameters = list(
      k = k, means = means, sigma = sigma,
      alpha = alpha, power = power
    )
  )
}

#' @title Power calculation
#' @description
#' Given sample size, effect size, and significance level, calculates power.
#'
#' @param n Sample size
#' @param effect_size Cohen's d effect size
#' @param alpha Significance level (default 0.05)
#' @param alternative Alternative hypothesis direction
#' @param test_type Test type ("t" or "z")
#'
#' @return Power (value between 0 and 1)
#' @export
#'
#' @examples
#' calc_power(n = 50, effect_size = 0.5, alpha = 0.05)
calc_power <- function(n, effect_size, alpha = 0.05,
                        alternative = c("two.sided", "less", "greater"),
                        test_type = c("t", "z")) {
  alternative <- match.arg(alternative)
  test_type <- match.arg(test_type)

  if (n < 2) stop("n must be at least 2.")

  ncp <- effect_size * sqrt(n)

  if (test_type == "t") {
    crit_val <- switch(alternative,
      "two.sided" = stats::qt(1 - alpha / 2, df = n - 1),
      "less" = stats::qt(alpha, df = n - 1),
      "greater" = stats::qt(1 - alpha, df = n - 1)
    )

    power <- switch(alternative,
      "two.sided" = stats::pt(-crit_val, df = n - 1, ncp = ncp) +
                    (1 - stats::pt(crit_val, df = n - 1, ncp = ncp)),
      "less" = stats::pt(crit_val, df = n - 1, ncp = -ncp),
      "greater" = 1 - stats::pt(crit_val, df = n - 1, ncp = ncp)
    )
  } else {
    z_alpha <- switch(alternative,
      "two.sided" = stats::qnorm(1 - alpha / 2),
      stats::qnorm(1 - alpha)
    )

    power <- switch(alternative,
      "two.sided" = stats::pnorm(-z_alpha + ncp) + (1 - stats::pnorm(z_alpha + ncp)),
      "less" = stats::pnorm(-z_alpha - ncp),
      "greater" = 1 - stats::pnorm(z_alpha - ncp)
    )
  }

  power
}

#' @title Effect size calculation
#' @description
#' Calculates common effect size metrics (Cohen's d, Hedges' g, eta squared,
#' omega squared, Cohen's h, r).
#'
#' @param type Effect size type (`"cohens_d"`, `"hedges_g"`, `"eta_squared"`,
#'   `"omega_squared"`, `"cohens_h"`, `"r"`)
#' @param ... Different parameters based on type
#'
#' @return Effect size value (scalar numeric)
#' @export
#'
#' @examples
#' effect_size("cohens_d", mean1 = 10, mean2 = 11, sd_pooled = 1.5)
#' effect_size("omega_squared", F = 10, df_between = 2, df_within = 60)
effect_size <- function(type = c("cohens_d", "hedges_g", "eta_squared",
                                  "omega_squared", "cohens_h", "r"), ...) {
  type <- match.arg(type)
  args <- list(...)

  switch(type,
    "cohens_d" = private_cohens_d(args$mean1, args$mean2, args$sd_pooled),
    "hedges_g" = private_hedges_g(args$mean1, args$mean2, args$sd_pooled, args$n1, args$n2),
    "eta_squared" = private_eta_squared(args$F, args$df_between, args$df_within,
                                         args$ss_between, args$ss_total),
    "omega_squared" = private_omega_squared(args$F, args$df_between, args$df_within,
                                             args$ss_between, args$ss_within,
                                             args$ss_total, args$ms_within),
    "cohens_h" = private_cohens_h(args$p1, args$p2),
    "r" = private_effect_size_r(args$t, args$df, args$z, args$n)
  )
}

#' @title Odds ratio for a 2x2 table
#' @description
#' Computes the odds ratio (OR) for a 2x2 contingency table, with a Wald-type
#' confidence interval on the log scale. A Haldane-Anscombe correction
#' (add 0.5 to every cell) is applied automatically when any cell is zero so
#' the estimate remains finite.
#'
#' The 2x2 table layout is:
#' ```
#'              Outcome+  Outcome-
#' Exposed+       a         b
#' Exposed-       c         d
#' ```
#' OR = (a/b) / (c/d) = (a*d) / (b*c).
#'
#' @param a Count of exposed+ / outcome+ cells.
#' @param b Count of exposed+ / outcome- cells.
#' @param c Count of exposed- / outcome+ cells.
#' @param d Count of exposed- / outcome- cells.
#' @param conf_level Confidence level (default 0.95).
#' @return A list with elements `odds_ratio`, `conf.int`, `conf.level`,
#'   `log_or`, `se_log_or`, `method`, and `corrected` (logical; whether the
#'   zero-cell correction was applied).
#' @export
#'
#' @examples
#' # Case-control: 45 exposed cases, 25 exposed controls,
#' # 20 unexposed cases, 60 unexposed controls
#' odds_ratio(a = 45, b = 25, c = 20, d = 60)
odds_ratio <- function(a, b, c, d, conf_level = 0.95) {
  if (a < 0 || b < 0 || c < 0 || d < 0)
    stop("odds_ratio: all cell counts must be non-negative.", call. = FALSE)
  if (a + b == 0 || c + d == 0)
    stop("odds_ratio: each exposure group must have at least one observation.",
         call. = FALSE)
  if (a + c == 0 || b + d == 0)
    stop("odds_ratio: each outcome group must have at least one observation.",
         call. = FALSE)

  # Haldane-Anscombe correction when any cell is zero
  corrected <- any(c(a, b, c, d) == 0)
  if (corrected) {
    a <- a + 0.5; b <- b + 0.5; c <- c + 0.5; d <- d + 0.5
  }

  or <- (a / b) / (c / d)
  log_or <- log(or)
  se_log_or <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - alpha / 2)
  ci <- exp(log_or + c(-1, 1) * z * se_log_or)

  list(
    odds_ratio  = as.numeric(or),
    conf.int    = as.numeric(ci),
    conf.level  = conf_level,
    log_or      = as.numeric(log_or),
    se_log_or   = as.numeric(se_log_or),
    method      = sprintf("Odds ratio (Wald log CI, %.0f%%)", conf_level * 100),
    corrected   = corrected
  )
}

#' @title Relative risk for a 2x2 cohort table
#' @description
#' Computes the relative risk (RR, risk ratio) for a 2x2 cohort table, with a
#' Wald-type confidence interval on the log scale.
#'
#' The 2x2 table layout is:
#' ```
#'              Outcome+  Outcome-   Total
#' Exposed+       a         b        n1=a+b
#' Exposed-       c         d        n2=c+d
#' ```
#' RR = (a / n1) / (c / n2).
#'
#' @param a Count of exposed+ / outcome+ cells.
#' @param b Count of exposed+ / outcome- cells.
#' @param c Count of exposed- / outcome+ cells.
#' @param d Count of exposed- / outcome- cells.
#' @param conf_level Confidence level (default 0.95).
#' @return A list with elements `relative_risk`, `conf.int`, `conf.level`,
#'   `log_rr`, `se_log_rr`, `method`, `risk_exposed`, `risk_unexposed`.
#' @export
#'
#' @examples
#' # Cohort: 30 of 100 exposed subjects had the event,
#' # 10 of 100 unexposed subjects had the event
#' relative_risk(a = 30, b = 70, c = 10, d = 90)
relative_risk <- function(a, b, c, d, conf_level = 0.95) {
  if (a < 0 || b < 0 || c < 0 || d < 0)
    stop("relative_risk: all cell counts must be non-negative.", call. = FALSE)
  n1 <- a + b
  n2 <- c + d
  if (n1 == 0 || n2 == 0)
    stop("relative_risk: each exposure group must have at least one observation.",
         call. = FALSE)

  risk_exp <- a / n1
  risk_unexp <- c / n2
  if (risk_exp == 0 || risk_unexp == 0)
    warning("relative_risk: a zero event count in one group yields RR = 0 or Inf; ",
            "interpret the CI with care.", call. = FALSE)

  rr <- risk_exp / risk_unexp
  log_rr <- log(rr)
  # Delta-method SE on the log scale: sqrt(1/a - 1/n1 + 1/c - 1/n2)
  # Guard against zero cells by adding 0.5 (Haldane-Anscombe style) when needed.
  a_c <- if (a == 0) 0.5 else a
  c_c <- if (c == 0) 0.5 else c
  se_log_rr <- sqrt(1 / a_c - 1 / n1 + 1 / c_c - 1 / n2)
  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - alpha / 2)
  ci <- exp(log_rr + c(-1, 1) * z * se_log_rr)

  list(
    relative_risk   = as.numeric(rr),
    conf.int        = as.numeric(ci),
    conf.level      = conf_level,
    log_rr          = as.numeric(log_rr),
    se_log_rr       = as.numeric(se_log_rr),
    method          = sprintf("Relative risk (Wald log CI, %.0f%%)", conf_level * 100),
    risk_exposed    = as.numeric(risk_exp),
    risk_unexposed  = as.numeric(risk_unexp)
  )
}

#' @title Sample size and power relationship table
#' @description
#' Generates comparison table of sample size vs power, useful for experimental design planning.
#'
#' @param effect_size Effect size
#' @param alpha Significance level (default 0.05)
#' @param n_range Sample size range (default seq(10, 200, by = 10))
#' @param alternative Alternative hypothesis direction
#'
#' @return Data frame with n and power columns
#' @export
#'
#' @examples
#' power_table(effect_size = 0.5, alpha = 0.05)
power_table <- function(effect_size, alpha = 0.05,
                         n_range = seq(10, 200, by = 10),
                         alternative = "two.sided") {
  powers <- sapply(n_range, function(n) {
    calc_power(n, effect_size, alpha, alternative)
  })

  data.frame(
    n = n_range,
    power = round(powers, 4),
    meets_80 = powers >= 0.80,
    meets_90 = powers >= 0.90
  )
}

# =============================================================================
# R3-B4: Sample size extensions
#   - Paired t-test
#   - Regression (global F-test / R²)
#   - Confidence-interval-width driven (mean / proportion)
#   - Tolerance-interval driven (normal, two-sided)
#   - Reliability study (binomial / success-run)
# =============================================================================

#' @title Sample size for paired t-test
#' @description
#' Calculates the required number of paired observations for a paired t-test.
#' The paired test reduces to a one-sample t-test on the within-pair
#' differences `d = y2 - y1`, so the sample size is driven by the mean
#' difference `delta = mu1 - mu0` relative to the standard deviation of the
#' differences `sigma_d`.
#'
#' @param mu0 Hypothesized mean difference (usually 0).
#' @param mu1 Alternative mean difference to detect.
#' @param sigma_d Standard deviation of the paired differences.
#' @param alpha Significance level (default 0.05).
#' @param power Desired power (default 0.80).
#' @param alternative Alternative hypothesis direction
#'   (`"two.sided"`, `"less"`, `"greater"`).
#' @return A list with `n` (number of pairs), `actual_power`, `effect_size`,
#'   and `parameters`.
#' @export
#' @examples
#' # Detect a mean difference of 0.5 with SD of differences = 1
#' sample_size_paired(mu0 = 0, mu1 = 0.5, sigma_d = 1, power = 0.80)
sample_size_paired <- function(mu0, mu1, sigma_d, alpha = 0.05, power = 0.80,
                                alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  delta <- abs(mu1 - mu0)
  if (delta == 0) stop("mu1 must differ from mu0.", call. = FALSE)
  if (sigma_d <= 0) stop("sigma_d must be positive.", call. = FALSE)

  effect_size <- delta / sigma_d

  z_alpha <- switch(alternative,
    "two.sided" = stats::qnorm(1 - alpha / 2),
    stats::qnorm(1 - alpha)
  )
  z_beta <- stats::qnorm(power)

  n <- ((z_alpha + z_beta) / effect_size)^2
  # Iterative t-correction (paired test uses df = n - 1)
  n <- private_adjust_t_sample_size(n, alpha, power, effect_size, alternative)
  n <- ceiling(n)
  actual_power <- private_calc_power(n, effect_size, alpha, alternative, "t")

  list(
    n = n,
    actual_power = actual_power,
    effect_size = effect_size,
    parameters = list(
      mu0 = mu0, mu1 = mu1, sigma_d = sigma_d,
      alpha = alpha, power = power, alternative = alternative
    )
  )
}

#' @title Sample size for linear regression (global F-test / R²)
#' @description
#' Calculates the required sample size to detect a given population R² in a
#' linear regression with `p` predictors, using the global F-test. The effect
#' size is Cohen's f² = R² / (1 - R²). The calculation uses an iterative
#' search over the noncentral F distribution so that the achieved power
#' matches the target.
#'
#' @param p Number of predictor variables (excluding the intercept).
#' @param r_squared Anticipated population R² (0 < r_squared < 1).
#' @param alpha Significance level (default 0.05).
#' @param power Desired power (default 0.80).
#' @return A list with `n` (total sample size, >= p + 2), `actual_power`,
#'   `f_squared`, `r_squared`, and `parameters`.
#' @export
#' @examples
#' # 3 predictors, R² = 0.30
#' sample_size_regression(p = 3, r_squared = 0.30, power = 0.80)
sample_size_regression <- function(p, r_squared, alpha = 0.05, power = 0.80) {
  if (p < 1) stop("p must be at least 1.", call. = FALSE)
  if (r_squared <= 0 || r_squared >= 1)
    stop("r_squared must be in (0, 1).", call. = FALSE)

  f2 <- r_squared / (1 - r_squared)

  # Normal-approximation starting n:  n ~ p + 1 + (z_alpha + z_beta)^2 / f^2
  z_alpha <- stats::qnorm(1 - alpha / 2)
  z_beta <- stats::qnorm(power)
  n_start <- ceiling(p + 1 + ((z_alpha + z_beta)^2) / f2)
  n <- max(n_start, p + 2L)

  # Refine using noncentral F:  F ~ F(df1=p, df2=n-p-1, ncp=n*f2)
  power_at <- function(n) {
    df1 <- p
    df2 <- n - p - 1L
    if (df2 < 1L) return(0)
    ncp <- n * f2
    f_crit <- stats::qf(1 - alpha, df1, df2)
    1 - stats::pf(f_crit, df1, df2, ncp = ncp)
  }

  # Decrease if over-estimated
  actual_power <- power_at(n)
  while (n > p + 2L) {
    lower <- power_at(n - 1L)
    if (lower >= power) {
      n <- n - 1L
      actual_power <- lower
    } else break
  }
  # Increase until target reached
  while (actual_power < power && n < 1000000L) {
    n <- n + 1L
    actual_power <- power_at(n)
  }

  list(
    n = n,
    actual_power = actual_power,
    f_squared = f2,
    r_squared = r_squared,
    parameters = list(
      p = p, r_squared = r_squared,
      alpha = alpha, power = power
    )
  )
}

#' @title Sample size driven by confidence-interval width
#' @description
#' Calculates the sample size required so that a two-sided confidence interval
#' has at most a specified half-width (precision). Supports the mean (normal,
#' known or estimated sigma) and a proportion.
#'
#' For the mean:  `n = (z * sigma / h)^2` where `h` is the desired half-width.
#' For the proportion:  `n = z^2 * p*(1-p) / h^2`.
#'
#' @param type `"mean"` or `"proportion"`.
#' @param h Desired half-width of the two-sided CI (the full width is `2*h`).
#' @param sigma Population SD (required when `type = "mean"`).
#' @param p Anticipated proportion in `(0, 1)` (required when `type = "proportion"`).
#'   Defaults to 0.5 (maximises required n) when omitted.
#' @param conf_level Confidence level (default 0.95).
#' @return A list with `n`, `half_width`, `full_width`, `conf_level`, and
#'   `parameters`.
#' @export
#' @examples
#' sample_size_ci(type = "mean", h = 0.3, sigma = 1, conf_level = 0.95)
#' sample_size_ci(type = "proportion", h = 0.05, p = 0.5, conf_level = 0.95)
sample_size_ci <- function(type = c("mean", "proportion"), h,
                            sigma = NULL, p = 0.5, conf_level = 0.95) {
  type <- match.arg(type)
  if (h <= 0) stop("h (half-width) must be positive.", call. = FALSE)
  if (conf_level <= 0 || conf_level >= 1)
    stop("conf_level must be in (0, 1).", call. = FALSE)

  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  if (type == "mean") {
    if (is.null(sigma) || sigma <= 0)
      stop("sigma must be a positive number for type = 'mean'.", call. = FALSE)
    n <- (z * sigma / h)^2
    params <- list(type = "mean", sigma = sigma, h = h, conf_level = conf_level)
  } else {
    if (p < 0 || p > 1) stop("p must be in [0, 1].", call. = FALSE)
    if (p == 0 || p == 1) p <- 0.5  # degenerate; fall back to worst case
    n <- (z^2) * p * (1 - p) / (h^2)
    params <- list(type = "proportion", p = p, h = h, conf_level = conf_level)
  }

  list(
    n = ceiling(n),
    half_width = h,
    full_width = 2 * h,
    conf_level = conf_level,
    parameters = params
  )
}

#' @title Sample size driven by a normal tolerance interval
#' @description
#' Calculates the minimum sample size so that a two-sided normal-theory
#' tolerance interval `x_bar +/- k * s` has the required content `p` and
#' confidence `conf_level`, while keeping the interval half-width no larger
#' than `max_half_width * sigma`.
#'
#' The k-factor for a two-sided normal tolerance interval is approximated
#' using the Howe (1969) / exact chi-square approach:
#' `k = sqrt( (nu * (1 + 1/n) * z_p^2) / chi2_{1-gamma, nu} )`
#' where `nu = n - 1`, `z_p = z_{(1+p)/2}`, and `chi2_{1-gamma, nu}` is the
#' `1-gamma` quantile of chi-square with `nu` df.
#'
#' @param p Content (population coverage) of the tolerance interval, in `(0,1)`.
#' @param conf_level Confidence level (probability that the interval covers at
#'   least proportion `p` of the population), in `(0,1)`.
#' @param max_half_width Multiple of `sigma` that the interval half-width
#'   (`k * s`) must not exceed. For example 3.0 means `k * s <= 3 * sigma`.
#' @param sigma Anticipated population SD (used only to express the width
#'   constraint; the returned `n` is scale-free).
#' @return A list with `n`, `k_factor`, `half_width` (in units of `sigma`),
#'   `p`, `conf_level`, and `parameters`.
#' @export
#' @examples
#' sample_size_tolerance(p = 0.95, conf_level = 0.95, max_half_width = 3.0)
sample_size_tolerance <- function(p, conf_level, max_half_width,
                                   sigma = 1) {
  if (p <= 0 || p >= 1) stop("p must be in (0, 1).", call. = FALSE)
  if (conf_level <= 0 || conf_level >= 1)
    stop("conf_level must be in (0, 1).", call. = FALSE)
  if (max_half_width <= 0) stop("max_half_width must be positive.", call. = FALSE)
  if (sigma <= 0) stop("sigma must be positive.", call. = FALSE)

  z_p <- stats::qnorm((1 + p) / 2)
  gamma <- 1 - conf_level

  # k-factor for two-sided normal tolerance interval (Howe / exact chi-square).
  k_factor <- function(n) {
    nu <- n - 1
    chi2_crit <- stats::qchisq(gamma, df = nu)
    sqrt(nu * (1 + 1 / n) * z_p^2 / chi2_crit)
  }

  # Expected s = c4(n) * sigma; use the k*s <= max_half_width * sigma
  # constraint with E[s] for a stable, deterministic search.
  c4 <- function(n) sqrt(2 / (n - 1)) * exp(lgamma(n / 2) - lgamma((n - 1) / 2))

  # Increase n until k * c4(n) <= max_half_width (the half-width in sigma units).
  n <- 3L
  while (n < 1000000L) {
    k <- k_factor(n)
    half_w <- k * c4(n)  # in sigma units
    if (half_w <= max_half_width) break
    n <- n + 1L
  }

  list(
    n = n,
    k_factor = k_factor(n),
    half_width = k_factor(n) * c4(n) * sigma,
    half_width_in_sigma = k_factor(n) * c4(n),
    p = p,
    conf_level = conf_level,
    parameters = list(
      p = p, conf_level = conf_level,
      max_half_width = max_half_width, sigma = sigma
    )
  )
}

#' @title Sample size for a reliability / success-run study
#' @description
#' Calculates the minimum sample size for a reliability demonstration test
#' under a binomial (success-run) model. With zero allowed failures the
#' required sample size for demonstrating reliability `R` at confidence level
#' `conf_level` is `n = log(1 - conf_level) / log(R)`. When `n_failures > 0`
#' failures are allowed, the sample size is found by solving the binomial
#' cumulative probability `P(X <= n_failures | n, 1 - R) <= 1 - conf_level`.
#'
#' @param reliability Target reliability (probability of a unit surviving),
#'   in `(0, 1)`.
#' @param conf_level Confidence level (probability of passing the test when
#'   true reliability is below target), in `(0, 1)`.
#' @param n_failures Number of failures allowed in the test (default 0).
#' @return A list with `n`, `reliability`, `conf_level`, `n_failures`, and
#'   `parameters`.
#' @export
#' @examples
#' # Demonstrate 95% reliability with 90% confidence, 0 failures
#' sample_size_reliability(reliability = 0.95, conf_level = 0.90)
#' # Allow 1 failure
#' sample_size_reliability(reliability = 0.95, conf_level = 0.90, n_failures = 1)
sample_size_reliability <- function(reliability, conf_level, n_failures = 0L) {
  if (reliability <= 0 || reliability >= 1)
    stop("reliability must be in (0, 1).", call. = FALSE)
  if (conf_level <= 0 || conf_level >= 1)
    stop("conf_level must be in (0, 1).", call. = FALSE)
  if (n_failures < 0)
    stop("n_failures must be non-negative.", call. = FALSE)
  n_failures <- as.integer(n_failures)

  if (n_failures == 0L) {
    # Success-run formula:  R >= (1 - conf_level)^(1/n)
    # => n >= log(1 - conf_level) / log(R)
    n <- log(1 - conf_level) / log(reliability)
    n <- ceiling(n)
    # Verify the achieved confidence
    achieved <- 1 - reliability^n
  } else {
    # Find smallest n such that  P(X <= r | n, 1 - R) <= 1 - conf_level,
    # i.e. consumer's risk is controlled. Search upward.
    n <- n_failures + 1L
    while (n < 1000000L) {
      prob <- stats::pbinom(n_failures, size = n, prob = 1 - reliability)
      if (prob <= 1 - conf_level) break
      n <- n + 1L
    }
    achieved <- 1 - stats::pbinom(n_failures, size = n, prob = 1 - reliability)
  }

  list(
    n = n,
    reliability = reliability,
    conf_level = conf_level,
    n_failures = n_failures,
    achieved_conf_level = achieved,
    parameters = list(
      reliability = reliability, conf_level = conf_level,
      n_failures = n_failures
    )
  )
}

# =============================================================================
# Internal helper functions
# =============================================================================

private_adjust_t_sample_size <- function(n_init, alpha, power, effect_size, alternative) {
  n <- n_init
  for (i in 1:20) {
    df <- n - 1
    t_alpha <- switch(alternative,
      "two.sided" = stats::qt(1 - alpha / 2, df),
      stats::qt(1 - alpha, df)
    )
    t_beta <- stats::qt(power, df)
    n_new <- ((t_alpha + t_beta) / effect_size)^2
    if (abs(n_new - n) < 0.1) break
    n <- n_new
  }
  n
}

private_calc_power <- function(n, effect_size, alpha, alternative, test_type) {
  if (n < 2) return(0)

  ncp <- effect_size * sqrt(n)

  if (test_type == "t") {
    crit_val <- switch(alternative,
      "two.sided" = stats::qt(1 - alpha / 2, df = n - 1),
      "less" = stats::qt(alpha, df = n - 1),
      "greater" = stats::qt(1 - alpha, df = n - 1)
    )

    power <- switch(alternative,
      "two.sided" = stats::pt(-crit_val, df = n - 1, ncp = ncp) +
                    (1 - stats::pt(crit_val, df = n - 1, ncp = ncp)),
      "less" = stats::pt(crit_val, df = n - 1, ncp = -ncp),
      "greater" = 1 - stats::pt(crit_val, df = n - 1, ncp = ncp)
    )
  } else {
    z_alpha <- switch(alternative,
      "two.sided" = stats::qnorm(1 - alpha / 2),
      stats::qnorm(1 - alpha)
    )

    power <- switch(alternative,
      "two.sided" = stats::pnorm(-z_alpha + ncp) + (1 - stats::pnorm(z_alpha + ncp)),
      "less" = stats::pnorm(-z_alpha - ncp),
      "greater" = 1 - stats::pnorm(z_alpha - ncp)
    )
  }

  power
}

private_calc_power_two_sample <- function(n1, n2, effect_size, alpha, alternative) {
  n_eff <- 1 / (1/n1 + 1/n2)
  ncp <- effect_size * sqrt(n_eff)

  df <- n1 + n2 - 2
  crit_val <- switch(alternative,
    "two.sided" = stats::qt(1 - alpha / 2, df),
    "less" = stats::qt(alpha, df),
    "greater" = stats::qt(1 - alpha, df)
  )

  power <- switch(alternative,
    "two.sided" = stats::pt(-crit_val, df, ncp) + (1 - stats::pt(crit_val, df, ncp)),
    "less" = stats::pt(crit_val, df, -ncp),
    "greater" = 1 - stats::pt(crit_val, df, ncp)
  )

  power
}

private_calc_power_proportion <- function(n, p0, p1, alpha, alternative) {
  se0 <- sqrt(p0 * (1 - p0) / n)
  se1 <- sqrt(p1 * (1 - p1) / n)

  z_alpha <- switch(alternative,
    "two.sided" = stats::qnorm(1 - alpha / 2),
    stats::qnorm(1 - alpha)
  )

  crit <- p0 + z_alpha * se0

  power <- switch(alternative,
    "two.sided" = stats::pnorm((p0 - z_alpha * se0 - p1) / se1) +
                  (1 - stats::pnorm((p0 + z_alpha * se0 - p1) / se1)),
    "less" = stats::pnorm((p0 - z_alpha * se0 - p1) / se1),
    "greater" = 1 - stats::pnorm((crit - p1) / se1)
  )

  power
}

private_calc_power_two_proportion <- function(n1, n2, p1, p2, alpha, alternative) {
  p_bar <- (p1 * n1 + p2 * n2) / (n1 + n2)
  se <- sqrt(p_bar * (1 - p_bar) * (1/n1 + 1/n2))

  z_alpha <- switch(alternative,
    "two.sided" = stats::qnorm(1 - alpha / 2),
    stats::qnorm(1 - alpha)
  )

  crit <- z_alpha * se

  se_diff <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  delta <- p1 - p2

  power <- switch(alternative,
    "two.sided" = stats::pnorm((-crit - delta) / se_diff) +
                  (1 - stats::pnorm((crit - delta) / se_diff)),
    "less" = stats::pnorm((-crit - delta) / se_diff),
    "greater" = 1 - stats::pnorm((crit - delta) / se_diff)
  )

  power
}

private_es_h <- function(p1, p2) {
  2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
}

private_cohens_d <- function(mean1, mean2, sd_pooled) {
  (mean1 - mean2) / sd_pooled
}

private_hedges_g <- function(mean1, mean2, sd_pooled, n1, n2) {
  d <- private_cohens_d(mean1, mean2, sd_pooled)
  n_total <- n1 + n2
  j <- 1 - 3 / (4 * (n_total - 2) - 1)
  d * j
}

private_eta_squared <- function(F = NULL, df_between = NULL, df_within = NULL,
                                 ss_between = NULL, ss_total = NULL) {
  if (!is.null(F) && !is.null(df_between) && !is.null(df_within)) {
    F * df_between / (F * df_between + df_within)
  } else if (!is.null(ss_between) && !is.null(ss_total)) {
    ss_between / ss_total
  } else {
    stop("eta_squared requires F/df or SS values.")
  }
}

# Omega squared (ω²): a less-biased ANOVA effect size than η².
#   From F statistic:  ω² = (F*df_b - df_b) / (F*df_b + df_w + 1)
#   From sums of squares:  ω² = (SS_b - df_b * MS_w) / (SS_total + MS_w)
# When SS_within is supplied instead of MS_within, MS_within = SS_within / df_within.
private_omega_squared <- function(F = NULL, df_between = NULL, df_within = NULL,
                                   ss_between = NULL, ss_within = NULL,
                                   ss_total = NULL, ms_within = NULL) {
  if (!is.null(F) && !is.null(df_between) && !is.null(df_within)) {
    num <- F * df_between - df_between
    den <- F * df_between + df_within + 1
    return(num / den)
  }
  # Resolve MS_within
  if (is.null(ms_within) && !is.null(ss_within) && !is.null(df_within)) {
    ms_within <- ss_within / df_within
  }
  if (!is.null(ss_between) && !is.null(ss_total) && !is.null(ms_within) &&
      !is.null(df_between)) {
    num <- ss_between - df_between * ms_within
    den <- ss_total + ms_within
    return(num / den)
  }
  stop("omega_squared requires F/df_between/df_within or ss_between/ss_total/ms_within (or ss_within/df_within).")
}

private_cohens_h <- function(p1, p2) {
  2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
}

private_effect_size_r <- function(t = NULL, df = NULL, z = NULL, n = NULL) {
  if (!is.null(t) && !is.null(df)) {
    sqrt(t^2 / (t^2 + df))
  } else if (!is.null(z) && !is.null(n)) {
    z / sqrt(n)
  } else {
    stop("effect_size_r requires t/df or z/n.")
  }
}
