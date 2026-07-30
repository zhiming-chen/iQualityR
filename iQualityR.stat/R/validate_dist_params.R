# Validate probability distribution parameters.
#
# This helper is intentionally not exported. Public callers should use ProbNode
# or the probability convenience APIs so the app layer receives stable results.

.prob_error <- function(code, message, metadata = list()) {
  structure(
    list(
      message = message,
      call = NULL,
      code = code,
      metadata = metadata
    ),
    class = c("iqr_prob_error", "iqr_error", "error", "condition")
  )
}

.or_else <- function(x, y) {
  if (is.null(x)) y else x
}

.stop_prob_param <- function(code, dist, param = NULL, rule = NULL, available = NULL) {
  key <- switch(code,
    prob_param_missing = "prob.param.missing",
    prob_param_type = "prob.param.type",
    prob_param_range = "prob.param.range",
    prob_unknown_distribution = "prob.dist.unknown",
    "common.invalid_input"
  )

  message <- iQualityR.core::iqr_t(
    key,
    dist = dist,
    param = .or_else(param, ""),
    rule = .or_else(rule, ""),
    available = paste(.or_else(available, character()), collapse = ", "),
    default = sprintf("Invalid parameter '%s' for distribution '%s'.", param, dist)
  )

  stop(.prob_error(
    code = code,
    message = message,
    metadata = list(
      dist = dist,
      param = param,
      rule = rule,
      available = available
    )
  ))
}

.require_numeric_param <- function(params,
                                   dist,
                                   param,
                                   rule = "numeric",
                                   positive = FALSE,
                                   non_negative = FALSE,
                                   integer = FALSE,
                                   lower = NULL,
                                   lower_open = FALSE,
                                   upper = NULL,
                                   upper_open = FALSE) {
  value <- params[[param]]

  if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
    .stop_prob_param("prob_param_type", dist, param, "numeric scalar")
  }

  if (isTRUE(integer) && value != as.integer(value)) {
    .stop_prob_param("prob_param_type", dist, param, "integer")
  }

  if (isTRUE(positive) && value <= 0) {
    .stop_prob_param("prob_param_range", dist, param, "> 0")
  }

  if (isTRUE(non_negative) && value < 0) {
    .stop_prob_param("prob_param_range", dist, param, ">= 0")
  }

  if (!is.null(lower)) {
    invalid <- if (isTRUE(lower_open)) value <= lower else value < lower
    if (invalid) {
      .stop_prob_param(
        "prob_param_range",
        dist,
        param,
        paste0(if (isTRUE(lower_open)) "> " else ">= ", lower)
      )
    }
  }

  if (!is.null(upper)) {
    invalid <- if (isTRUE(upper_open)) value >= upper else value > upper
    if (invalid) {
      .stop_prob_param(
        "prob_param_range",
        dist,
        param,
        paste0(if (isTRUE(upper_open)) "< " else "<= ", upper)
      )
    }
  }

  invisible(value)
}

.require_params <- function(params, dist, required) {
  missing <- required[vapply(required, function(name) is.null(params[[name]]), logical(1))]
  if (length(missing)) {
    .stop_prob_param("prob_param_missing", dist, paste(missing, collapse = ", "), "required")
  }
}

#' Validate Distribution Parameters
#'
#' Validate and complete probability distribution parameters. This is an
#' internal helper invoked by [ProbNode] during construction and by the
#' probability convenience APIs. End users should never need to call it
#' directly — public entry points already route through this validator.
#'
#' The validator performs two jobs:
#'
#' 1. **Fill defaults** — when a required parameter is `NULL`, it is populated
#'    with the canonical default for the distribution (e.g. `mean=0, sd=1`
#'    for `norm`, `shape=1, scale=1` for `weibull`).
#' 2. **Enforce contracts** — each parameter is checked for type (numeric
#'    scalar / integer), positivity, and bounded ranges. Violations raise a
#'    structured `iqr_prob_error` condition carrying `code`, `dist`,
#'    `param`, `rule`, and `available` metadata for downstream handlers.
#'
#' Supported distributions and their parameter contracts:
#'
#' | `dist`    | Parameters                              | Constraints                              |
#' |-----------|-----------------------------------------|------------------------------------------|
#' | `norm`    | `mean`, `sd`                            | `sd > 0`                                 |
#' | `lnorm`   | `meanlog`, `sdlog`                      | `sdlog > 0`                              |
#' | `weibull` | `shape`, `scale`                        | `shape > 0`, `scale > 0`                 |
#' | `gamma`   | `shape`, `scale`                        | `shape > 0`, `scale > 0`                 |
#' | `exp`     | `rate`                                  | `rate > 0`                               |
#' | `t`       | `df`                                    | `df > 0`                                 |
#' | `f`       | `df1`, `df2`                            | `df1 > 0`, `df2 > 0`                     |
#' | `chisq`   | `df`                                    | `df > 0`                                 |
#' | `beta`    | `shape1`, `shape2`                      | `shape1 > 0`, `shape2 > 0`               |
#' | `unif`    | `min`, `max`                            | `min < max`                              |
#' | `logis`   | `location`, `scale`                     | `scale > 0`                              |
#' | `cauchy`  | `location`, `scale`                     | `scale > 0`                              |
#' | `binom`   | `size`, `prob`                          | `size >= 1` integer, `0 <= prob <= 1`    |
#' | `pois`    | `lambda`                                | `lambda > 0`                             |
#' | `nbinom`  | `size`, `prob`                          | `size > 0`, `0 < prob < 1`               |
#' | `hyper`   | `m`, `n`, `k`                           | integer `>= 0`, `k <= m + n`             |
#' | `geom`    | `prob`                                  | `0 < prob < 1`                           |
#'
#' Unknown distributions raise `prob_unknown_distribution` with the list of
#' registered distribution keys attached as `available` metadata.
#'
#' @param dist Distribution key, such as `"norm"`, `"weibull"`, or `"binom"`.
#' @param params Parameter list. `NULL` entries are replaced with defaults.
#' @return A named list of validated parameters with defaults filled.
#' @keywords internal
#' @noRd
validate_dist_params <- function(dist, params) {
  if (!is.list(params)) {
    params <- list()
  }

  dist <- as.character(dist)[[1]]

  switch(dist,
    norm = {
      params$mean <- .or_else(params$mean, 0)
      params$sd <- .or_else(params$sd, 1)
      .require_numeric_param(params, dist, "mean")
      .require_numeric_param(params, dist, "sd", positive = TRUE)
    },
    weibull = {
      params$shape <- .or_else(params$shape, 1)
      params$scale <- .or_else(params$scale, 1)
      .require_numeric_param(params, dist, "shape", positive = TRUE)
      .require_numeric_param(params, dist, "scale", positive = TRUE)
    },
    lnorm = {
      params$meanlog <- .or_else(params$meanlog, 0)
      params$sdlog <- .or_else(params$sdlog, 1)
      .require_numeric_param(params, dist, "meanlog")
      .require_numeric_param(params, dist, "sdlog", positive = TRUE)
    },
    gamma = {
      params$shape <- .or_else(params$shape, 1)
      params$scale <- .or_else(params$scale, 1)
      .require_numeric_param(params, dist, "shape", positive = TRUE)
      .require_numeric_param(params, dist, "scale", positive = TRUE)
    },
    exp = {
      params$rate <- .or_else(params$rate, 1)
      .require_numeric_param(params, dist, "rate", positive = TRUE)
    },
    t = {
      params$df <- .or_else(params$df, 1)
      .require_numeric_param(params, dist, "df", positive = TRUE)
    },
    f = {
      params$df1 <- .or_else(params$df1, 1)
      params$df2 <- .or_else(params$df2, 1)
      .require_numeric_param(params, dist, "df1", positive = TRUE)
      .require_numeric_param(params, dist, "df2", positive = TRUE)
    },
    chisq = {
      params$df <- .or_else(params$df, 1)
      .require_numeric_param(params, dist, "df", positive = TRUE)
    },
    beta = {
      params$shape1 <- .or_else(params$shape1, 1)
      params$shape2 <- .or_else(params$shape2, 1)
      .require_numeric_param(params, dist, "shape1", positive = TRUE)
      .require_numeric_param(params, dist, "shape2", positive = TRUE)
    },
    unif = {
      params$min <- .or_else(params$min, 0)
      params$max <- .or_else(params$max, 1)
      .require_numeric_param(params, dist, "min")
      .require_numeric_param(params, dist, "max")
      if (params$min >= params$max) {
        .stop_prob_param("prob_param_range", dist, "min", "< max")
      }
    },
    logis = {
      params$location <- .or_else(params$location, 0)
      params$scale <- .or_else(params$scale, 1)
      .require_numeric_param(params, dist, "location")
      .require_numeric_param(params, dist, "scale", positive = TRUE)
    },
    cauchy = {
      params$location <- .or_else(params$location, 0)
      params$scale <- .or_else(params$scale, 1)
      .require_numeric_param(params, dist, "location")
      .require_numeric_param(params, dist, "scale", positive = TRUE)
    },
    binom = {
      params$size <- .or_else(params$size, 10)
      params$prob <- .or_else(params$prob, 0.5)
      .require_numeric_param(params, dist, "size", integer = TRUE, lower = 1)
      .require_numeric_param(params, dist, "prob", lower = 0, upper = 1)
    },
    pois = {
      params$lambda <- .or_else(params$lambda, 1)
      .require_numeric_param(params, dist, "lambda", positive = TRUE)
    },
    nbinom = {
      params$size <- .or_else(params$size, 10)
      params$prob <- .or_else(params$prob, 0.5)
      .require_numeric_param(params, dist, "size", positive = TRUE)
      .require_numeric_param(params, dist, "prob", lower = 0, lower_open = TRUE, upper = 1)
    },
    hyper = {
      .require_params(params, dist, c("m", "n", "k"))
      .require_numeric_param(params, dist, "m", integer = TRUE, non_negative = TRUE)
      .require_numeric_param(params, dist, "n", integer = TRUE, non_negative = TRUE)
      .require_numeric_param(params, dist, "k", integer = TRUE, non_negative = TRUE)
      if (params$k > (params$m + params$n)) {
        .stop_prob_param("prob_param_range", dist, "k", "<= m + n")
      }
    },
    geom = {
      params$prob <- .or_else(params$prob, 0.5)
      .require_numeric_param(params, dist, "prob", lower = 0, lower_open = TRUE, upper = 1)
    },
    {
      available <- if (exists("DIST_REGISTRY", inherits = TRUE)) {
        names(get("DIST_REGISTRY", inherits = TRUE))
      } else {
        character()
      }
      .stop_prob_param("prob_unknown_distribution", dist, available = available)
    }
  )

  params
}
