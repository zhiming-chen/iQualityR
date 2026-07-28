# =============================================================================
# File: R/prob/dist_registry.R
# Description: Probability distribution registry
# =============================================================================

#' @title Probability distribution registry
#' @description
#' Stores metadata and calculation functions for all available distributions.
#' Uses registry pattern, adding a new distribution only requires adding a record in this file,
#' without modifying ProbNode's d/p/q methods.
#'
#' @format A named list, each element contains:
#' \describe{
#'   \item{d}{Probability density/mass function}
#'   \item{p}{Cumulative distribution function}
#'   \item{q}{Quantile function}
#'   \item{defaults}{Default parameter values}
#'   \item{validate}{Parameter validation function}
#'   \item{is_discrete}{Whether it is a discrete distribution}
#'   \item{support}{Support description}
#'   \item{description}{Distribution description (English)}
#' }
#'
#' @keywords internal
DIST_REGISTRY <- list(

  # ============================================================================
  # Continuous distributions
  # ============================================================================

  norm = list(
    d = function(x, p) dnorm(x, p$mean, p$sd),
    p = function(q, p, lower.tail = TRUE) pnorm(q, p$mean, p$sd, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qnorm(p_val, p$mean, p$sd, lower.tail = lower.tail),
    defaults = list(mean = 0, sd = 1),
    is_discrete = FALSE,
    support = "(-Inf, +Inf)",
    description = "Normal distribution (Gaussian) - most common continuous distribution, suitable for measurement errors, product quality characteristics, etc."
  ),

  weibull = list(
    d = function(x, p) ifelse(x < 0, 0, dweibull(x, p$shape, p$scale)),
    p = function(q, p, lower.tail = TRUE) pweibull(pmax(0, q), p$shape, p$scale, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qweibull(p_val, p$shape, p$scale, lower.tail = lower.tail),
    defaults = list(shape = 1, scale = 1),
    is_discrete = FALSE,
    support = "[0, +Inf)",
    description = "Weibull distribution - widely used in reliability engineering and life analysis"
  ),

  lnorm = list(
    d = function(x, p) ifelse(x <= 0, 0, dlnorm(x, p$meanlog, p$sdlog)),
    p = function(q, p, lower.tail = TRUE) plnorm(pmax(0, q), p$meanlog, p$sdlog, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qlnorm(p_val, p$meanlog, p$sdlog, lower.tail = lower.tail),
    defaults = list(meanlog = 0, sdlog = 1),
    is_discrete = FALSE,
    support = "(0, +Inf)",
    description = "Log-normal distribution - suitable for particle size, reaction time, and other right-skewed data"
  ),

  gamma = list(
    d = function(x, p) ifelse(x < 0, 0, dgamma(x, p$shape, p$scale)),
    p = function(q, p, lower.tail = TRUE) pgamma(pmax(0, q), p$shape, p$scale, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qgamma(p_val, p$shape, p$scale, lower.tail = lower.tail),
    defaults = list(shape = 1, scale = 1),
    is_discrete = FALSE,
    support = "[0, +Inf)",
    description = "Gamma distribution - suitable for waiting time, cumulative quantity modeling"
  ),

  exp = list(
    d = function(x, p) ifelse(x < 0, 0, dexp(x, p$rate)),
    p = function(q, p, lower.tail = TRUE) pexp(pmax(0, q), p$rate, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qexp(p_val, p$rate, lower.tail = lower.tail),
    defaults = list(rate = 1),
    is_discrete = FALSE,
    support = "[0, +Inf)",
    description = "Exponential distribution - suitable for modeling failure intervals, event intervals"
  ),

  t = list(
    d = function(x, p) dt(x, p$df),
    p = function(q, p, lower.tail = TRUE) pt(q, p$df, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qt(p_val, p$df, lower.tail = lower.tail),
    defaults = list(df = 1),
    is_discrete = FALSE,
    support = "(-Inf, +Inf)",
    description = "t distribution (Student's t) - suitable for small sample inference, tails heavier than normal"
  ),

  f = list(
    d = function(x, p) ifelse(x < 0, 0, df(x, p$df1, p$df2)),
    p = function(q, p, lower.tail = TRUE) pf(pmax(0, q), p$df1, p$df2, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qf(p_val, p$df1, p$df2, lower.tail = lower.tail),
    defaults = list(df1 = 1, df2 = 1),
    is_discrete = FALSE,
    support = "[0, +Inf)",
    description = "F distribution - used for variance comparison and ANOVA"
  ),

  chisq = list(
    d = function(x, p) ifelse(x < 0, 0, dchisq(x, p$df)),
    p = function(q, p, lower.tail = TRUE) pchisq(pmax(0, q), p$df, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qchisq(p_val, p$df, lower.tail = lower.tail),
    defaults = list(df = 1),
    is_discrete = FALSE,
    support = "[0, +Inf)",
    description = "Chi-square distribution - used for goodness of fit tests and confidence intervals for variance"
  ),

  # --- New continuous distributions ---

  beta = list(
    d = function(x, p) ifelse(x < 0 | x > 1, 0, dbeta(x, p$shape1, p$shape2)),
    p = function(q, p, lower.tail = TRUE) pbeta(pmin(pmax(0, q), 1), p$shape1, p$shape2, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qbeta(p_val, p$shape1, p$shape2, lower.tail = lower.tail),
    defaults = list(shape1 = 1, shape2 = 1),
    is_discrete = FALSE,
    support = "[0, 1]",
    description = "Beta distribution - suitable for proportion/rate modeling (e.g., pass rate, defect rate), defined on [0,1] interval"
  ),

  unif = list(
    d = function(x, p) ifelse(x < p$min | x > p$max, 0, dunif(x, p$min, p$max)),
    p = function(q, p, lower.tail = TRUE) punif(pmin(pmax(q, p$min), p$max), p$min, p$max, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qunif(p_val, p$min, p$max, lower.tail = lower.tail),
    defaults = list(min = 0, max = 1),
    is_discrete = FALSE,
    support = "[min, max]",
    description = "Uniform distribution - suitable for measurement system analysis (MSA) and equal probability scenarios"
  ),

  logis = list(
    d = function(x, p) dlogis(x, p$location, p$scale),
    p = function(q, p, lower.tail = TRUE) plogis(q, p$location, p$scale, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qlogis(p_val, p$location, p$scale, lower.tail = lower.tail),
    defaults = list(location = 0, scale = 1),
    is_discrete = FALSE,
    support = "(-Inf, +Inf)",
    description = "Logistic distribution - similar to normal but with heavier tails, suitable for robust analysis"
  ),

  cauchy = list(
    d = function(x, p) dcauchy(x, p$location, p$scale),
    p = function(q, p, lower.tail = TRUE) pcauchy(q, p$location, p$scale, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qcauchy(p_val, p$location, p$scale, lower.tail = lower.tail),
    defaults = list(location = 0, scale = 1),
    is_discrete = FALSE,
    support = "(-Inf, +Inf)",
    description = "Cauchy distribution - heavy-tailed distribution, mean and variance do not exist, suitable for extreme value scenarios"
  ),

  # ============================================================================
  # Discrete distributions
  # ============================================================================

  binom = list(
    d = function(x, p) dbinom(x, p$size, p$prob),
    p = function(q, p, lower.tail = TRUE) pbinom(q, p$size, p$prob, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qbinom(p_val, p$size, p$prob, lower.tail = lower.tail),
    defaults = list(size = 10, prob = 0.5),
    is_discrete = TRUE,
    support = "0, 1, 2, ..., size",
    description = "Binomial distribution - suitable for modeling number of successes in n independent trials (e.g., number of nonconforming items)"
  ),

  pois = list(
    d = function(x, p) dpois(x, p$lambda),
    p = function(q, p, lower.tail = TRUE) ppois(q, p$lambda, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qpois(p_val, p$lambda, lower.tail = lower.tail),
    defaults = list(lambda = 1),
    is_discrete = TRUE,
    support = "0, 1, 2, ...",
    description = "Poisson distribution - suitable for modeling number of events in unit time/space (e.g., defect count)"
  ),

  nbinom = list(
    d = function(x, p) dnbinom(x, p$size, p$prob),
    p = function(q, p, lower.tail = TRUE) pnbinom(q, p$size, p$prob, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qnbinom(p_val, p$size, p$prob, lower.tail = lower.tail),
    defaults = list(size = 10, prob = 0.5),
    is_discrete = TRUE,
    support = "0, 1, 2, ...",
    description = "Negative binomial distribution - suitable for modeling number of failures before reaching specified number of successes"
  ),

  hyper = list(
    d = function(x, p) dhyper(x, p$m, p$n, p$k),
    p = function(q, p, lower.tail = TRUE) phyper(q, p$m, p$n, p$k, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qhyper(p_val, p$m, p$n, p$k, lower.tail = lower.tail),
    defaults = list(m = 10, n = 10, k = 5),
    is_discrete = TRUE,
    support = "max(0, k-n), ..., min(k, m)",
    description = "Hypergeometric distribution - suitable for sampling without replacement from a finite population"
  ),

  geom = list(
    d = function(x, p) dgeom(x, p$prob),
    p = function(q, p, lower.tail = TRUE) pgeom(q, p$prob, lower.tail = lower.tail),
    q = function(p_val, p, lower.tail = TRUE) qgeom(p_val, p$prob, lower.tail = lower.tail),
    defaults = list(prob = 0.5),
    is_discrete = TRUE,
    support = "0, 1, 2, ...",
    description = "Geometric distribution - suitable for modeling number of successful trials before first failure"
  )
)

# =============================================================================
# Registry operation functions
# =============================================================================

#' @title List all available distributions
#' @description Returns names and descriptions of all registered distributions
#' @return Data frame containing type, description, is_discrete, support columns
#' @export
#' @examples
#' list_available_dists()
list_available_dists <- function() {
  data.frame(
    type = names(DIST_REGISTRY),
    description = sapply(DIST_REGISTRY, function(d) d$description),
    is_discrete = sapply(DIST_REGISTRY, function(d) d$is_discrete),
    support = sapply(DIST_REGISTRY, function(d) d$support),
    stringsAsFactors = FALSE
  )
}

#' @title Get detailed distribution information
#' @description Returns complete metadata for specified distribution
#' @param dist_type Distribution type name
#' @return Distribution information list
#' @export
#' @examples
#' get_dist_info("norm")
get_dist_info <- function(dist_type) {
  if (!dist_type %in% names(DIST_REGISTRY)) {
    stop("[dist_registry] Unknown distribution type: ", dist_type,
         "\n  Available distributions: ", paste(names(DIST_REGISTRY), collapse = ", "),
         call. = FALSE)
  }
  DIST_REGISTRY[[dist_type]]
}

#' @title Register New Distribution
#' @description Add a new distribution to the registry
#' @param dist_type Distribution type name
#' @param d_func Probability density/mass function
#' @param p_func Cumulative distribution function
#' @param q_func Quantile function
#' @param defaults Default parameter values list
#' @param validate Parameter validation function
#' @param is_discrete Whether it is a discrete distribution
#' @param support Support description
#' @param description Distribution description
#' @return Invisible NULL
#' @export
#' @examples
#' \dontrun{
#' register_dist("my_dist", d = function(x, p) ..., p = ..., q = ...,
#'               defaults = list(...), is_discrete = FALSE,
#'               description = "My custom distribution")
#' }
register_dist <- function(dist_type, d_func, p_func, q_func,
                          defaults, validate = NULL,
                          is_discrete = FALSE,
                          support = "Custom",
                          description = "User-defined distribution") {
  if (dist_type %in% names(DIST_REGISTRY)) {
    warning("[dist_registry] Distribution '", dist_type, "' already exists, will be overwritten")
  }

  DIST_REGISTRY[[dist_type]] <<- list(
    d = d_func,
    p = p_func,
    q = q_func,
    defaults = defaults,
    validate = validate,
    is_discrete = is_discrete,
    support = support,
    description = description
  )

  invisible(NULL)
}

#' @title Unregister Distribution
#' @description Remove a specified distribution from the registry
#' @param dist_type Distribution type name
#' @return Invisible NULL
#' @export
unregister_dist <- function(dist_type) {
  if (!dist_type %in% names(DIST_REGISTRY)) {
    warning("[dist_registry] Distribution '", dist_type, "' does not exist")
    return(invisible(NULL))
  }
  DIST_REGISTRY[[dist_type]] <<- NULL
  invisible(NULL)
}
