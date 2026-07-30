# =============================================================================
# File: R/dist_fit.R
# Description: Distribution fitting module - automatic best-fit selection
# =============================================================================

#' @title Single distribution fitting
#' @description
#' Fit specified distribution to given data, estimate parameters and evaluate goodness of fit.
#'
#' @param x Numeric vector
#' @param dist Distribution name (supports all R built-in distributions)
#' @param start Initial parameter values (optional, required for some distributions)
#' @param method Optimization method (default "mle" = maximum likelihood estimation)
#'
#' @return List containing dist (distribution name), params (estimated parameters),
#'   logLik (log-likelihood), AIC, BIC, ks_test (KS test result)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' fit_distribution(x, "norm")
fit_distribution <- function(x, dist = c("norm", "exp", "gamma", "weibull", "lnorm",
                                          "beta", "logis", "cauchy", "t", "f", "chisq"),
                                start = NULL, method = c("mle", "mme")) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) stop("Need at least 3 non-missing values.")

  dist <- match.arg(dist)
  method <- match.arg(method)

  result <- switch(dist,
    "norm" = private_fit_norm(x, n),
    "exp" = private_fit_exp(x, n),
    "gamma" = private_fit_gamma(x, n, start),
    "weibull" = private_fit_weibull(x, n),
    "lnorm" = private_fit_lnorm(x, n),
    "beta" = private_fit_beta(x, n, start),
    "logis" = private_fit_logis(x, n),
    "cauchy" = private_fit_cauchy(x, n, start),
    "t" = private_fit_t(x, n, start),
    "f" = private_fit_f(x, n, start),
    "chisq" = private_fit_chisq(x, n),
    stop(sprintf("Unsupported distribution: %s", dist))
  )

  result
}

#' @title Automatic Distribution Fitting
#' @description
#' Try multiple candidate distributions and automatically select the best fitting one.
#'
#' @param x Numeric vector
#' @param candidates List of candidate distributions (default includes common continuous distributions)
#' @param criterion Selection criterion ("aic", "bic", "ks")
#' @param positive_only Whether to fit only positive distributions (when all data is positive)
#'
#' @return List containing best_dist (best distribution), best_result (best fit result),
#'   all_results (all candidate fit results), ranking (ranking table)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rexp(100, rate = 0.5)
#' result <- auto_fit_distribution(x)
#' result$best_dist
auto_fit_distribution <- function(x, candidates = c("norm", "exp", "gamma", "weibull", "lnorm", "logis"),
                                   criterion = c("aic", "bic", "ks"),
                                   positive_only = NULL) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) stop("Need at least 3 non-missing values.")

  criterion <- match.arg(criterion)

  if (is.null(positive_only)) {
    positive_only <- all(x > 0)
  }

  if (positive_only) {
    candidates <- intersect(candidates, c("exp", "gamma", "weibull", "lnorm", "logis"))
  } else {
    candidates <- intersect(candidates, c("norm", "logis", "cauchy", "t"))
  }

  if (length(candidates) == 0) {
    stop("No valid candidate distributions for the given data.")
  }

  results <- list()

  for (dist in candidates) {
    result <- tryCatch({
      fit_distribution(x, dist)
    }, error = function(e) NULL)

    if (!is.null(result)) {
      results[[dist]] <- result
    }
  }

  if (length(results) == 0) {
    stop("All distribution fits failed.")
  }

  scores <- sapply(results, function(r) {
    switch(criterion,
      "aic" = r$AIC,
      "bic" = r$BIC,
      "ks" = r$ks_test$statistic
    )
  })

  ranking <- data.frame(
    dist = names(results),
    score = round(scores, 4),
    AIC = sapply(results, function(r) round(r$AIC, 4)),
    BIC = sapply(results, function(r) round(r$BIC, 4)),
    ks_stat = sapply(results, function(r) round(r$ks_test$statistic, 4)),
    ks_p = sapply(results, function(r) round(r$ks_test$p.value, 4)),
    stringsAsFactors = FALSE
  )
  ranking <- ranking[order(ranking$score), ]

  best_dist <- ranking$dist[1]
  best_result <- results[[best_dist]]

  list(
    best_dist = best_dist,
    best_result = best_result,
    all_results = results,
    ranking = ranking,
    criterion = criterion,
    n = n
  )
}

#' @title Distribution Fit Comparison
#' @description
#' Statistically compare fit results of multiple distributions.
#'
#' @param fit_results List of multiple distribution fit results
#' @param alpha Significance level (default 0.05)
#'
#' @return List containing test_results (goodness of fit tests for each distribution),
#'   best_by_ks (best by KS test), best_by_ad (best by AD test)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' f1 <- fit_distribution(x, "norm")
#' f2 <- fit_distribution(x, "logis")
#' compare_fits(list(norm = f1, logis = f2))
compare_fits <- function(fit_results, alpha = 0.05) {
  test_results <- list()

  for (dist_name in names(fit_results)) {
    fit <- fit_results[[dist_name]]
    x <- fit$data
    params <- fit$params
    dist <- fit$dist

    ks_result <- tryCatch({
      if (dist == "t") {
        # Scaled t-distribution: pt() does not accept location/scale, so build
        # a closure that accounts for them (mirrors private_fit_t()).
        location <- params$location
        scale <- params$scale
        df <- params$df
        fitted_cdf <- function(q) pt((q - location) / scale, df = df)
        stats::ks.test(x, fitted_cdf)
      } else {
        # Unpack named params as individual arguments via do.call so that
        # e.g. list(mean=0, sd=1) becomes pnorm(q, mean=0, sd=1) rather than
        # pnorm(q, c(mean=0, sd=1)) which would misinterpret the vector.
        cdf_func <- get(sprintf("p%s", dist), mode = "function",
                        envir = getNamespace("stats"))
        do.call(stats::ks.test, c(list(x = x, y = cdf_func), params))
      }
    }, error = function(e) list(statistic = NA, p.value = NA))

    ad_result <- tryCatch({
      if (dist == "norm" && requireNamespace("nortest", quietly = TRUE)) {
        nortest::ad.test(x)
      } else {
        NULL
      }
    }, error = function(e) NULL)

    test_results[[dist_name]] <- list(
      dist = dist,
      ks_test = ks_result,
      ad_test = ad_result,
      AIC = fit$AIC,
      BIC = fit$BIC
    )
  }

  ks_stats <- sapply(test_results, function(r) {
    if (!is.null(r$ks_test)) unname(r$ks_test$statistic) else Inf
  })

  best_by_ks <- names(which.min(ks_stats))

  # Track whether any AD test was actually performed (non-NULL). Without this,
  # when nortest is absent all ad_stats are Inf and which.min() would still
  # return the first name, falsely reporting a "best" by AD.
  ad_performed <- sapply(test_results, function(r) !is.null(r$ad_test))
  ad_stats <- sapply(test_results, function(r) {
    if (!is.null(r$ad_test)) unname(r$ad_test$statistic) else Inf
  })

  if (any(ad_performed)) {
    # Consider only distributions where AD was actually computed
    ad_valid <- ad_stats[ad_performed]
    best_by_ad <- names(which.min(ad_valid))
  } else {
    best_by_ad <- NA
  }

  list(
    test_results = test_results,
    best_by_ks = best_by_ks,
    best_by_ad = best_by_ad,
    n_fits = length(test_results)
  )
}

#' @title Empirical Distribution Function
#' @description
#' Calculate and return the empirical distribution function of the data.
#'
#' @param x Numeric vector
#'
#' @return List containing ecdf (empirical distribution function object),
#'   points (points for plotting), n (sample size)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' edf <- empirical_distribution(x)
#' edf$ecdf(0)
empirical_distribution <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 2) stop("Need at least 2 non-missing values.")

  ecdf_func <- stats::ecdf(x)
  sorted_x <- sort(x)
  probs <- (1:n) / n

  list(
    ecdf = ecdf_func,
    points = data.frame(x = sorted_x, prob = probs),
    n = n
  )
}

#' @title QQ Data Calculation
#' @description
#' Calculate theoretical quantiles and sample quantiles required for QQ plot.
#'
#' @param x Numeric vector
#' @param dist Distribution name
#' @param params Distribution parameter list
#'
#' @return Data frame containing theoretical (theoretical quantiles), sample (sample quantiles)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' qq_data <- calc_qq_data(x, "norm", list(mean = 0, sd = 1))
calc_qq_data <- function(x, dist = "norm", params = list()) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 2) stop("Need at least 2 non-missing values.")

  probs <- (1:n - 0.5) / n

  # Scaled t-distribution: qt() does not accept location/scale, so compute
  # theoretical quantiles manually (location + scale * qt(p, df)).
  if (dist == "t" && all(c("location", "scale", "df") %in% names(params))) {
    theoretical <- params$location + params$scale * qt(probs, df = params$df)
  } else {
    qfunc <- get(sprintf("q%s", dist), mode = "function",
                 envir = getNamespace("stats"))
    theoretical <- do.call(qfunc, c(list(p = probs), params))
  }
  sample_quantiles <- sort(x)

  data.frame(
    theoretical = theoretical,
    sample = sample_quantiles
  )
}

# =============================================================================
# Internal helper functions - Distribution fitting implementations
# =============================================================================

private_fit_norm <- function(x, n) {
  mu <- mean(x)
  sigma <- sd(x)

  loglik <- sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))
  k <- 2

  ks_result <- tryCatch(stats::ks.test(x, "pnorm", mean = mu, sd = sigma),
                        error = function(e) list(statistic = NA, p.value = NA))

  list(
    dist = "norm",
    params = list(mean = mu, sd = sigma),
    logLik = loglik,
    AIC = -2 * loglik + 2 * k,
    BIC = -2 * loglik + k * log(n),
    ks_test = ks_result,
    n = n,
    data = x
  )
}

private_fit_exp <- function(x, n) {
  if (any(x <= 0)) stop("Exponential distribution requires positive data.")

  rate <- 1 / mean(x)
  loglik <- sum(dexp(x, rate = rate, log = TRUE))
  k <- 1

  ks_result <- tryCatch(stats::ks.test(x, "pexp", rate = rate),
                        error = function(e) list(statistic = NA, p.value = NA))

  list(
    dist = "exp",
    params = list(rate = rate),
    logLik = loglik,
    AIC = -2 * loglik + 2 * k,
    BIC = -2 * loglik + k * log(n),
    ks_test = ks_result,
    n = n,
    data = x
  )
}

private_fit_gamma <- function(x, n, start = NULL) {
  if (any(x <= 0)) stop("Gamma distribution requires positive data.")

  shape <- (mean(x) / sd(x))^2
  rate <- shape / mean(x)

  if (requireNamespace("MASS", quietly = TRUE)) {
    fit <- tryCatch(
      suppressWarnings(MASS::fitdistr(x, "gamma", start = start)),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      shape <- fit$estimate["shape"]
      rate <- fit$estimate["rate"]
    }
  }

  loglik <- sum(dgamma(x, shape = shape, rate = rate, log = TRUE))
  k <- 2

  ks_result <- tryCatch(stats::ks.test(x, "pgamma", shape = shape, rate = rate),
                        error = function(e) list(statistic = NA, p.value = NA))

  list(
    dist = "gamma",
    params = list(shape = shape, rate = rate),
    logLik = loglik,
    AIC = -2 * loglik + 2 * k,
    BIC = -2 * loglik + k * log(n),
    ks_test = ks_result,
    n = n,
    data = x
  )
}

private_fit_weibull <- function(x, n) {
  if (any(x <= 0)) stop("Weibull distribution requires positive data.")

  shape <- 1.5
  scale <- mean(x)

  if (requireNamespace("MASS", quietly = TRUE)) {
    fit <- tryCatch(
      suppressWarnings(MASS::fitdistr(x, "weibull")),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      shape <- fit$estimate["shape"]
      scale <- fit$estimate["scale"]
    }
  }

  loglik <- sum(dweibull(x, shape = shape, scale = scale, log = TRUE))
  k <- 2
  ks_result <- tryCatch(stats::ks.test(x, "pweibull", shape = shape, scale = scale),
                        error = function(e) list(statistic = NA, p.value = NA))

  list(
    dist = "weibull",
    params = list(shape = shape, scale = scale),
    logLik = loglik,
    AIC = -2 * loglik + 2 * k,
    BIC = -2 * loglik + k * log(n),
    ks_test = ks_result,
    n = n,
    data = x
  )
}

private_fit_lnorm <- function(x, n) {
  if (any(x <= 0)) stop("Log-normal distribution requires positive data.")

  logx <- log(x)
  meanlog <- mean(logx)
  sdlog <- sd(logx)

  loglik <- sum(dlnorm(x, meanlog = meanlog, sdlog = sdlog, log = TRUE))
  k <- 2

  ks_result <- tryCatch(stats::ks.test(x, "plnorm", meanlog = meanlog, sdlog = sdlog),
                        error = function(e) list(statistic = NA, p.value = NA))

  list(
    dist = "lnorm",
    params = list(meanlog = meanlog, sdlog = sdlog),
    logLik = loglik,
    AIC = -2 * loglik + 2 * k,
    BIC = -2 * loglik + k * log(n),
    ks_test = ks_result,
    n = n,
    data = x
  )
}

private_fit_beta <- function(x, n, start = NULL) {
  if (any(x <= 0 | x >= 1)) stop("Beta distribution requires data in (0, 1).")

  mx <- mean(x)
  vx <- var(x)
  shape1 <- mx * (mx * (1 - mx) / vx - 1)
  shape2 <- (1 - mx) * (mx * (1 - mx) / vx - 1)

  if (shape1 <= 0 || shape2 <= 0) {
    shape1 <- 1
    shape2 <- 1
  }

  if (requireNamespace("MASS", quietly = TRUE)) {
    fit <- tryCatch(
      suppressWarnings(MASS::fitdistr(x, "beta", start = start)),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      shape1 <- fit$estimate["shape1"]
      shape2 <- fit$estimate["shape2"]
    }
  }

  loglik <- sum(dbeta(x, shape1 = shape1, shape2 = shape2, log = TRUE))
  k <- 2
  ks_result <- tryCatch(stats::ks.test(x, "pbeta", shape1 = shape1, shape2 = shape2),
                        error = function(e) list(statistic = NA, p.value = NA))

  list(
    dist = "beta",
    params = list(shape1 = shape1, shape2 = shape2),
    logLik = loglik,
    AIC = -2 * loglik + 2 * k,
    BIC = -2 * loglik + k * log(n),
    ks_test = ks_result,
    n = n,
    data = x
  )
}

private_fit_logis <- function(x, n) {
  location <- mean(x)
  scale <- sd(x) * sqrt(3) / pi

  loglik <- sum(dlogis(x, location = location, scale = scale, log = TRUE))
  k <- 2

  ks_result <- tryCatch(stats::ks.test(x, "plogis", location = location, scale = scale),
                        error = function(e) list(statistic = NA, p.value = NA))

  list(
    dist = "logis",
    params = list(location = location, scale = scale),
    logLik = loglik,
    AIC = -2 * loglik + 2 * k,
    BIC = -2 * loglik + k * log(n),
    ks_test = ks_result,
    n = n,
    data = x
  )
}

private_fit_cauchy <- function(x, n, start = NULL) {
  location <- median(x)
  scale <- mad(x)
  if (scale == 0) scale <- IQR(x) / 1.349

  if (requireNamespace("MASS", quietly = TRUE)) {
    fit <- tryCatch(
      suppressWarnings(MASS::fitdistr(x, "cauchy", start = start)),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      location <- fit$estimate["location"]
      scale <- fit$estimate["scale"]
    }
  }

  loglik <- sum(dcauchy(x, location = location, scale = scale, log = TRUE))
  k <- 2
  ks_result <- tryCatch(stats::ks.test(x, "pcauchy", location = location, scale = scale),
                        error = function(e) list(statistic = NA, p.value = NA))

  list(
    dist = "cauchy",
    params = list(location = location, scale = scale),
    logLik = loglik,
    AIC = -2 * loglik + 2 * k,
    BIC = -2 * loglik + k * log(n),
    ks_test = ks_result,
    n = n,
    data = x
  )
}

private_fit_t <- function(x, n, start = NULL) {
  df <- 5
  location <- median(x)
  scale <- mad(x)
  if (scale == 0) scale <- IQR(x) / 1.349

  if (requireNamespace("MASS", quietly = TRUE)) {
    fit <- tryCatch(
      suppressWarnings(MASS::fitdistr(x, "t", start = start)),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      df <- unname(fit$estimate["df"])
      location <- unname(fit$estimate["m"])
      scale <- unname(fit$estimate["s"])
    }
  }

  loglik <- sum(dt((x - location) / scale, df = df, log = TRUE) - log(scale))
  k <- 3

  fitted_cdf <- function(q) pt((q - location) / scale, df = df)
  ks_result <- tryCatch(stats::ks.test(x, fitted_cdf),
                        error = function(e) list(statistic = NA, p.value = NA))

  list(
    dist = "t",
    params = list(df = df, location = location, scale = scale),
    logLik = loglik,
    AIC = -2 * loglik + 2 * k,
    BIC = -2 * loglik + k * log(n),
    ks_test = ks_result,
    n = n,
    data = x
  )
}

private_fit_f <- function(x, n, start = NULL) {
  if (any(x <= 0)) stop("F distribution requires positive data.")

  df1 <- 5
  df2 <- 10

  if (requireNamespace("MASS", quietly = TRUE)) {
    fit <- tryCatch(
      suppressWarnings(MASS::fitdistr(x, "f", start = start)),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      df1 <- fit$estimate["df1"]
      df2 <- fit$estimate["df2"]
    }
  }

  loglik <- sum(df(x, df1 = df1, df2 = df2, log = TRUE))
  k <- 2
  ks_result <- tryCatch(stats::ks.test(x, "pf", df1 = df1, df2 = df2),
                        error = function(e) list(statistic = NA, p.value = NA))

  list(
    dist = "f",
    params = list(df1 = df1, df2 = df2),
    logLik = loglik,
    AIC = -2 * loglik + 2 * k,
    BIC = -2 * loglik + k * log(n),
    ks_test = ks_result,
    n = n,
    data = x
  )
}

private_fit_chisq <- function(x, n) {
  if (any(x <= 0)) stop("Chi-squared distribution requires positive data.")

  df <- mean(x)

  loglik <- sum(dchisq(x, df = df, log = TRUE))
  k <- 1

  ks_result <- tryCatch(stats::ks.test(x, "pchisq", df = df),
                        error = function(e) list(statistic = NA, p.value = NA))

  list(
    dist = "chisq",
    params = list(df = df),
    logLik = loglik,
    AIC = -2 * loglik + 2 * k,
    BIC = -2 * loglik + k * log(n),
    ks_test = ks_result,
    n = n,
    data = x
  )
}
