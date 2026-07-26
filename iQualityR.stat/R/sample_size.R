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

  actual_power <- private_calc_power(n, effect_size, alpha, alternative, test_type)

  list(
    n = ceiling(n),
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
    total_n = ceiling(n1 + n2),
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
    total_n = ceiling(n1 + n2),
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

  n_per_group <- ((z_alpha + z_beta) / effect_size_f)^2 * k

  lambda <- n_per_group * sum((means - grand_mean)^2) / sigma^2
  actual_power <- 1 - stats::pf(stats::qf(1 - alpha, k - 1, Inf), k - 1, k * (n_per_group - 1), ncp = lambda)

  list(
    n_per_group = ceiling(n_per_group),
    total_n = ceiling(n_per_group * k),
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
#' Calculates common effect size metrics (Cohen's d, Hedges' g, eta squared, etc.).
#'
#' @param type Effect size type ("cohens_d", "hedges_g", "eta_squared", "cohens_h", "r")
#' @param ... Different parameters based on type
#'
#' @return Effect size value
#' @export
#'
#' @examples
#' effect_size("cohens_d", mean1 = 10, mean2 = 11, sd_pooled = 1.5)
effect_size <- function(type = c("cohens_d", "hedges_g", "eta_squared", "cohens_h", "r"), ...) {
  type <- match.arg(type)
  args <- list(...)

  switch(type,
    "cohens_d" = private_cohens_d(args$mean1, args$mean2, args$sd_pooled),
    "hedges_g" = private_hedges_g(args$mean1, args$mean2, args$sd_pooled, args$n1, args$n2),
    "eta_squared" = private_eta_squared(args$F, args$df_between, args$df_within,
                                         args$ss_between, args$ss_total),
    "cohens_h" = private_cohens_h(args$p1, args$p2),
    "r" = private_effect_size_r(args$t, args$df, args$z, args$n)
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
    "less" = stats::pnorm((crit - p1) / se1),
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
    "less" = stats::pnorm((crit - delta) / se_diff),
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
