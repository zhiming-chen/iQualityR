# =============================================================================
# File: R/ml_bocpd.R
# Description: Bayesian Online Change Point Detection (BOCPD) - pure R impl.
#             Adams & MacKay (2007) algorithm with Gaussian conjugate model.
#             No external dependencies; suitable for CRAN.
# =============================================================================

# ---------------------------------------------------------------------------
# Internal: Gaussian conjugate sufficient statistics update
# ---------------------------------------------------------------------------
# Model: x_t | mu, sigma^2 ~ N(mu, sigma^2)
# Prior: mu | sigma^2 ~ N(mu0, sigma^2 / kappa0)
#        sigma^2 ~ Inv-Gamma(alpha0, beta0)
# Conjugate update after observing n data points with mean m and sum of squares S:
#   kappa_n = kappa0 + n
#   mu_n    = (kappa0 * mu0 + n * m) / kappa_n
#   alpha_n = alpha0 + n / 2
#   beta_n  = beta0 + 0.5 * S + 0.5 * kappa0 * n * (m - mu0)^2 / kappa_n
# ---------------------------------------------------------------------------

.bocpd_gaussian_init <- function(mu0, kappa0, alpha0, beta0) {
  list(mu = mu0, kappa = kappa0, alpha = alpha0, beta = beta0, n = 0L)
}

# Update prior with a batch of observations x
.bocpd_gaussian_update_batch <- function(prior, x) {
  n <- length(x)
  if (n == 0) return(prior)
  m <- mean(x)
  S <- sum((x - m)^2)
  kappa_new <- prior$kappa + n
  mu_new <- (prior$kappa * prior$mu + n * m) / kappa_new
  alpha_new <- prior$alpha + n / 2
  beta_new <- prior$beta + 0.5 * S +
    0.5 * prior$kappa * n * (m - prior$mu)^2 / kappa_new
  list(mu = mu_new, kappa = kappa_new,
       alpha = alpha_new, beta = beta_new,
       n = prior$n + n)
}

# Predictive log-likelihood of x under Student-t posterior predictive
# t_{2*alpha}(mu, beta*(kappa+1)/(alpha*kappa))
.bocpd_gaussian_loglik <- function(prior, x) {
  # Posterior predictive: Student-t with df = 2*alpha, loc = mu, scale^2 = ...
  df <- 2 * prior$alpha
  loc <- prior$mu
  scale2 <- prior$beta * (prior$kappa + 1) / (prior$alpha * prior$kappa)
  scale <- sqrt(scale2)
  z <- (x - loc) / scale
  # log density of Student-t
  log_const <- lgamma((df + 1) / 2) - lgamma(df / 2) -
    0.5 * log(df * pi) - log(scale)
  log_den <- log_const - (df + 1) / 2 * log1p(z^2 / df)
  log_den
}

# ---------------------------------------------------------------------------
# Main BOCPD: Adams-MacKay algorithm
# ---------------------------------------------------------------------------
# Inputs:
#   x: numeric vector of observations
#   hazard: numeric in (0, 1), 1/expected run length
#   prior: list(mu, kappa, alpha, beta) for Gaussian conjugate prior
# Returns:
#   list with:
#     R: matrix (T+1) x T, run-length posterior (rows = time, cols = run length)
#     changepoint_prob: numeric vector length T, P(changepoint at time t)
#     run_length_mean: numeric vector length T, expected run length
#     most_likely_changepoints: integer vector of detected changepoints
#     max_prob: maximum a posteriori changepoints
# ---------------------------------------------------------------------------
.bocpd_run <- function(x, hazard, prior_params) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  T <- length(x)
  if (T < 2) {
    return(list(
      R = matrix(NA_real_, 0, 0),
      changepoint_prob = numeric(0),
      run_length_mean = numeric(0),
      most_likely_changepoints = integer(0),
      max_prob = numeric(0)))
  }

  # R[t+1, r+1] = P(run_length = r at time t)
  # Use log space for numerical stability when summing.
  R <- matrix(0, nrow = T + 1, ncol = T + 1)
  R[1, 1] <- 1  # Initial: run length 0 at time 0 with prob 1

  # One prior per possible run length (renewed at changepoints)
  # For efficiency, store list of priors indexed by run length.
  priors <- list(.bocpd_gaussian_init(
    prior_params$mu, prior_params$kappa,
    prior_params$alpha, prior_params$beta))

  changepoint_prob <- numeric(T)
  run_length_mean <- numeric(T)
  max_prob <- numeric(T)

  for (t in seq_len(T)) {
    # Predictive probabilities for each existing run length
    # New observation: x[t]
    # Compute log-likelihood under each prior in priors
    logliks <- sapply(priors, function(p) .bocpd_gaussian_loglik(p, x[t]))
    # Convert to likelihoods (numerically safe)
    max_ll <- max(logliks)
    liks <- exp(logliks - max_ll)
    # Growth: P(r_t = r_{t-1} + 1, x_t) = P(r_{t-1}) * (1 - H) * predictive
    prev_probs <- R[t, seq_along(priors)]
    growth <- prev_probs * (1 - hazard) * liks
    # Changepoint: P(r_t = 0) = sum over r_{t-1} P(r_{t-1}) * H * predictive
    cp_prob <- sum(prev_probs * hazard * liks)
    # Normalize (sum of growth + cp_prob = total evidence)
    evidence <- sum(growth) + cp_prob
    if (evidence <= 0 || !is.finite(evidence)) evidence <- 1e-300
    # Shift growth by 1 (run length increased), set R[0] = cp_prob
    new_probs <- c(cp_prob, growth)
    # Pad if necessary (truncate to T+1)
    np <- length(new_probs)
    if (np > T + 1) new_probs <- new_probs[1:(T + 1)]
    R[t + 1, seq_along(new_probs)] <- new_probs / evidence

    # Update priors: existing ones get updated with x[t], plus a new one for r=0
    priors <- c(
      list(.bocpd_gaussian_init(
        prior_params$mu, prior_params$kappa,
        prior_params$alpha, prior_params$beta)),
      lapply(priors, function(p) .bocpd_gaussian_update_batch(p, x[t]))
    )
    # Truncate priors to match R matrix size
    if (length(priors) > T + 1) priors <- priors[1:(T + 1)]

    # Diagnostics
    norm_probs <- R[t + 1, ]
    # P(changepoint at t) = R[t+1, 1]
    changepoint_prob[t] <- norm_probs[1]
    # Expected run length: sum_{r=0..T} r * P(r_t = r)
    rl_vals <- seq.int(0, length(norm_probs) - 1)
    run_length_mean[t] <- sum(rl_vals * norm_probs)
    # Max probability location
    max_prob[t] <- which.max(norm_probs) - 1
  }

  # Detect changepoints: local peaks where P(cp) > threshold
  # Use a simple heuristic: P(cp) > 0.5 OR local maximum above 0.2
  cp_threshold <- 0.5
  detected <- which(changepoint_prob > cp_threshold)
  # Also detect local peaks
  if (length(changepoint_prob) >= 3) {
    for (i in 2:(T - 1)) {
      if (changepoint_prob[i] > 0.2 &&
          changepoint_prob[i] >= changepoint_prob[i - 1] &&
          changepoint_prob[i] >= changepoint_prob[i + 1]) {
        detected <- c(detected, i)
      }
    }
  }
  most_likely_changepoints <- sort(unique(detected))

  list(
    R = R,
    changepoint_prob = changepoint_prob,
    run_length_mean = run_length_mean,
    most_likely_changepoints = most_likely_changepoints,
    max_prob = max_prob
  )
}
