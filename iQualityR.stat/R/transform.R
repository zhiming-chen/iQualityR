# =============================================================================
# File: R/transform.R
# Description: Data transformation module - convert non-normal to normal
# =============================================================================

#' @title Box-Cox transformation
#' @description
#' Applies Box-Cox transformation to positive data, finding optimal lambda to make data most normal.
#'
#' @param x Numeric vector (must be all positive)
#' @param lambda Transformation parameter (default auto-optimized)
#' @param optimize Whether to automatically optimize lambda (default TRUE)
#' @param lambda_range Lambda search range (default c(-2, 2))
#'
#' @return List containing transformed (transformed data), lambda (lambda value used),
#'   original (original data), method (transformation method name)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rexp(100, rate = 0.5)
#' result <- box_cox_transform(x)
#' result$lambda
box_cox_transform <- function(x, lambda = NULL, optimize = TRUE,
                               lambda_range = c(-2, 2)) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) stop("Need at least 3 non-missing values.")
  if (any(x <= 0)) stop("Box-Cox transform requires all positive values. Use Yeo-Johnson for non-positive data.")

  if (optimize && is.null(lambda)) {
    lambda <- private_optimize_lambda(x, method = "box_cox", range = lambda_range)
  } else if (is.null(lambda)) {
    lambda <- 0
  }

  transformed <- private_apply_box_cox(x, lambda)

  list(
    transformed = transformed,
    lambda = lambda,
    original = x,
    method = "Box-Cox",
    n = n,
    normality_before = stats::shapiro.test(x)$p.value,
    normality_after = stats::shapiro.test(transformed)$p.value
  )
}

#' @title Yeo-Johnson transformation
#' @description
#' Applies Yeo-Johnson transformation to any real-valued data, supporting positive and negative values.
#'
#' @param x Numeric vector
#' @param lambda Transformation parameter (default auto-optimized)
#' @param optimize Whether to automatically optimize lambda (default TRUE)
#' @param lambda_range Lambda search range (default c(-2, 2))
#'
#' @return List containing transformed (transformed data), lambda (lambda value used),
#'   original (original data), method (transformation method name)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100, mean = -1, sd = 2)
#' result <- yeo_johnson_transform(x)
#' result$lambda
yeo_johnson_transform <- function(x, lambda = NULL, optimize = TRUE,
                                   lambda_range = c(-2, 2)) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) stop("Need at least 3 non-missing values.")

  if (optimize && is.null(lambda)) {
    lambda <- private_optimize_lambda(x, method = "yeo_johnson", range = lambda_range)
  } else if (is.null(lambda)) {
    lambda <- 0
  }

  transformed <- private_apply_yeo_johnson(x, lambda)

  list(
    transformed = transformed,
    lambda = lambda,
    original = x,
    method = "Yeo-Johnson",
    n = n,
    normality_before = stats::shapiro.test(x)$p.value,
    normality_after = stats::shapiro.test(transformed)$p.value
  )
}

#' @title Johnson transformation
#' @description
#' Fits Johnson distribution family to data, finding best transformation to normalize.
#' Supports SU (unbounded), SB (bounded), SL (lognormal) three types.
#'
#' @param x Numeric vector
#' @param type Johnson type ("auto", "SU", "SB", "SL")
#'
#' @return List containing transformed (transformed data), type (Johnson type),
#'   parameters (transformation parameters), original (original data), method (transformation method name)
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' x <- rexp(100, rate = 0.5)
#' result <- johnson_transform(x)
#' result$type
#' }
johnson_transform <- function(x, type = c("auto", "SU", "SB", "SL")) {
  if (!requireNamespace("SuppDists", quietly = TRUE)) {
    stop("Johnson transform requires 'SuppDists' package. Install it first.")
  }

  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) stop("Need at least 3 non-missing values.")

  type <- match.arg(type)

  params <- SuppDists::JohnsonFit(x, moment = "quant")

  if (type == "auto") {
    type <- params$type
  }

  transformed <- private_apply_johnson(x, params)

  list(
    transformed = transformed,
    type = type,
    parameters = params,
    original = x,
    method = "Johnson",
    n = n,
    normality_before = stats::shapiro.test(x)$p.value,
    normality_after = stats::shapiro.test(transformed)$p.value
  )
}

#' @title Logarithmic transformation
#' @description
#' Applies logarithmic transformation to positive data (natural log, common log, base-2 log).
#'
#' @param x Numeric vector (must be all positive unless add_constant > 0)
#' @param base Logarithm base ("natural", "10", "2")
#' @param add_constant Constant added (for handling zero or negative data, default 0)
#'
#' @return List containing transformed (transformed data), base (base used),
#'   original (original data), method (transformation method name)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rexp(100, rate = 0.5)
#' result <- log_transform(x, base = "natural")
log_transform <- function(x, base = c("natural", "10", "2"), add_constant = 0) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) stop("Need at least 3 non-missing values.")

  base <- match.arg(base)

  x_adj <- x + add_constant
  if (any(x_adj <= 0)) {
    stop(sprintf("Data contains non-positive values. Set add_constant >= %.2f.", abs(min(x)) + 0.001))
  }

  transformed <- switch(base,
    "natural" = log(x_adj),
    "10" = log10(x_adj),
    "2" = log2(x_adj)
  )

  list(
    transformed = transformed,
    base = base,
    add_constant = add_constant,
    original = x,
    method = sprintf("Log (%s)", base),
    n = n,
    normality_before = stats::shapiro.test(x)$p.value,
    normality_after = stats::shapiro.test(transformed)$p.value
  )
}

#' @title Square root transformation
#' @description
#' Applies square root transformation to non-negative data, suitable for count data or Poisson-distributed data.
#'
#' @param x Numeric vector (must be all non-negative unless add_constant > 0)
#' @param add_constant Constant added (default 0)
#'
#' @return List containing transformed (transformed data), original (original data),
#'   method (transformation method name)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rpois(100, lambda = 5)
#' result <- sqrt_transform(x)
sqrt_transform <- function(x, add_constant = 0) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) stop("Need at least 3 non-missing values.")

  x_adj <- x + add_constant
  if (any(x_adj < 0)) {
    stop(sprintf("Data contains negative values. Set add_constant >= %.2f.", abs(min(x))))
  }

  transformed <- sqrt(x_adj)

  list(
    transformed = transformed,
    add_constant = add_constant,
    original = x,
    method = "Square Root",
    n = n,
    normality_before = stats::shapiro.test(x)$p.value,
    normality_after = stats::shapiro.test(transformed)$p.value
  )
}

#' @title Reciprocal transformation
#' @description
#' Applies reciprocal transformation (1/x) to non-zero data.
#'
#' @param x Numeric vector (cannot contain zero)
#'
#' @return List containing transformed (transformed data), original (original data),
#'   method (transformation method name)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100, mean = 5, sd = 1)
#' result <- reciprocal_transform(x)
reciprocal_transform <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) stop("Need at least 3 non-missing values.")
  if (any(x == 0)) stop("Reciprocal transform cannot handle zero values.")

  transformed <- 1 / x

  list(
    transformed = transformed,
    original = x,
    method = "Reciprocal",
    n = n,
    normality_before = stats::shapiro.test(x)$p.value,
    normality_after = stats::shapiro.test(transformed)$p.value
  )
}

#' @title Auto-select best transformation
#' @description
#' Tries multiple transformation methods, automatically selecting the one that makes data most normal.
#'
#' @param x Numeric vector
#' @param methods Candidate transformation methods (default includes common transformations)
#' @param criterion Selection criterion ("shapiro" uses Shapiro-Wilk P-value, "ad" uses Anderson-Darling)
#'
#' @return List containing best_method (best transformation name), best_result (best transformation result),
#'   all_results (comparison of all transformation results)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rexp(100, rate = 0.5)
#' result <- auto_transform(x)
#' result$best_method
auto_transform <- function(x, methods = c("box_cox", "yeo_johnson", "log", "sqrt", "reciprocal"),
                            criterion = c("shapiro", "ad")) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) stop("Need at least 3 non-missing values.")

  criterion <- match.arg(criterion)

  results <- list()
  valid_methods <- c()

  for (method in methods) {
    result <- tryCatch({
      switch(method,
        "box_cox" = {
          if (all(x > 0)) {
            box_cox_transform(x, optimize = TRUE)
          } else {
            NULL
          }
        },
        "yeo_johnson" = {
          yeo_johnson_transform(x, optimize = TRUE)
        },
        "log" = {
          if (all(x > 0)) {
            log_transform(x, base = "natural")
          } else {
            NULL
          }
        },
        "sqrt" = {
          if (all(x >= 0)) {
            sqrt_transform(x)
          } else {
            NULL
          }
        },
        "reciprocal" = {
          if (all(x != 0)) {
            reciprocal_transform(x)
          } else {
            NULL
          }
        }
      )
    }, error = function(e) NULL)

    if (!is.null(result)) {
      results[[method]] <- result
      valid_methods <- c(valid_methods, method)
    }
  }

  if (length(valid_methods) == 0) {
    stop("No valid transform method found for the given data.")
  }

  scores <- sapply(results, function(r) r$normality_after)

  best_method <- valid_methods[which.max(scores)]
  best_result <- results[[best_method]]

  list(
    best_method = best_result$method,
    best_result = best_result,
    all_results = results,
    scores = scores,
    n = n,
    criterion = criterion
  )
}

#' @title Inverse transformation
#' @description
#' Restores transformed data to original scale.
#'
#' @param transformed Transformed data
#' @param method Transformation method name
#' @param params Transformation parameters (e.g., lambda, base, etc.)
#'
#' @return Restored numeric vector
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rexp(100, rate = 0.5)
#' result <- box_cox_transform(x)
#' original <- inverse_transform(result$transformed, result$method, list(lambda = result$lambda))
inverse_transform <- function(transformed, method, params = list()) {
  method_lower <- tolower(method)

  if (grepl("box.cox", method_lower)) {
    lambda <- params$lambda %||% 0
    private_inverse_box_cox(transformed, lambda)
  } else if (grepl("yeo.johnson", method_lower)) {
    lambda <- params$lambda %||% 0
    private_inverse_yeo_johnson(transformed, lambda)
  } else if (grepl("log", method_lower)) {
    base <- params$base %||% "natural"
    add_constant <- params$add_constant %||% 0
    switch(base,
      "natural" = exp(transformed) - add_constant,
      "10" = 10^transformed - add_constant,
      "2" = 2^transformed - add_constant
    )
  } else if (grepl("sqrt", method_lower)) {
    add_constant <- params$add_constant %||% 0
    transformed^2 - add_constant
  } else if (grepl("reciprocal", method_lower)) {
    1 / transformed
  } else if (grepl("johnson", method_lower)) {
    private_inverse_johnson(transformed, params)
  } else {
    stop(sprintf("Unknown transform method: %s", method))
  }
}

# =============================================================================
# Internal helper functions
# =============================================================================

private_optimize_lambda <- function(x, method = "box_cox", range = c(-2, 2)) {
  objective <- function(lambda) {
    transformed <- switch(method,
      "box_cox" = private_apply_box_cox(x, lambda),
      "yeo_johnson" = private_apply_yeo_johnson(x, lambda)
    )

    if (length(transformed) < 3) return(Inf)

    -stats::shapiro.test(transformed)$p.value
  }

  opt <- stats::optimize(objective, interval = range, maximum = FALSE)
  opt$minimum
}

private_apply_box_cox <- function(x, lambda) {
  if (abs(lambda) < 1e-10) {
    log(x)
  } else {
    (x^lambda - 1) / lambda
  }
}

private_inverse_box_cox <- function(y, lambda) {
  if (abs(lambda) < 1e-10) {
    exp(y)
  } else {
    (y * lambda + 1)^(1 / lambda)
  }
}

private_apply_yeo_johnson <- function(x, lambda) {
  n <- length(x)
  y <- numeric(n)

  for (i in seq_len(n)) {
    if (x[i] >= 0) {
      if (abs(lambda) < 1e-10) {
        y[i] <- log(x[i] + 1)
      } else {
        y[i] <- ((x[i] + 1)^lambda - 1) / lambda
      }
    } else {
      if (abs(lambda - 2) < 1e-10) {
        y[i] <- -log(-x[i] + 1)
      } else {
        y[i] <- -((-x[i] + 1)^(2 - lambda) - 1) / (2 - lambda)
      }
    }
  }

  y
}

private_inverse_yeo_johnson <- function(y, lambda) {
  n <- length(y)
  x <- numeric(n)

  for (i in seq_len(n)) {
    if (y[i] >= 0) {
      if (abs(lambda) < 1e-10) {
        x[i] <- exp(y[i]) - 1
      } else {
        x[i] <- (y[i] * lambda + 1)^(1 / lambda) - 1
      }
    } else {
      if (abs(lambda - 2) < 1e-10) {
        x[i] <- 1 - exp(-y[i])
      } else {
        x[i] <- 1 - (-(y[i] * (2 - lambda)) + 1)^(1 / (2 - lambda))
      }
    }
  }

  x
}

private_apply_johnson <- function(x, params) {
  gamma <- params$gamma
  delta <- params$delta
  xi <- params$xi
  lambda <- params$lambda
  type <- params$type

  z <- (x - xi) / lambda

  switch(type,
    "SU" = {
      sinh_inv <- function(v) log(v + sqrt(v^2 + 1))
      sinh_inv((z - gamma) / delta)
    },
    "SB" = {
      if (any(z <= 0 | z >= 1)) {
        warning("Some values outside [0,1] range for SB type. Results may be unreliable.")
      }
      z_clamped <- pmin(pmax(z, 1e-10), 1 - 1e-10)
      gamma + delta * log(z_clamped / (1 - z_clamped))
    },
    "SL" = {
      if (any(z <= 0)) {
        warning("Some values non-positive for SL type. Results may be unreliable.")
      }
      gamma + delta * log(pmax(z, 1e-10))
    },
    stop(sprintf("Unknown Johnson type: %s", type))
  )
}

private_inverse_johnson <- function(y, params) {
  gamma <- params$gamma
  delta <- params$delta
  xi <- params$xi
  lambda <- params$lambda
  type <- params$type

  switch(type,
    "SU" = {
      z <- gamma + delta * sinh(y)
      xi + lambda * z
    },
    "SB" = {
      w <- exp((y - gamma) / delta)
      z <- w / (1 + w)
      xi + lambda * z
    },
    "SL" = {
      z <- exp((y - gamma) / delta)
      xi + lambda * z
    },
    stop(sprintf("Unknown Johnson type: %s", type))
  )
}
