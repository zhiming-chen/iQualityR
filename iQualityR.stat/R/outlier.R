# =============================================================================
# File: R/outlier.R
# Description: Outlier detection module
# =============================================================================

#' @title IQR-based outlier detection
#' @description
#' Detect outliers using the Interquartile Range (IQR) method.
#' Outlier definition: less than Q1 - k*IQR or greater than Q3 + k*IQR.
#'
#' @param x Numeric vector
#' @param k IQR multiplier (default 1.5, use 3 for extreme values)
#' @param na.rm Whether to remove missing values (default TRUE)
#'
#' @return List containing outliers (outlier vector), indices (outlier indices),
#'   lower_bound, upper_bound, n_outliers (number of outliers)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- c(rnorm(100), 10, -10)
#' detect_outliers_iqr(x)
detect_outliers_iqr <- function(x, k = 1.5, na.rm = TRUE) {
  x <- as.numeric(x)
  if (na.rm) x <- x[!is.na(x)]

  q1 <- quantile(x, 0.25, na.rm = na.rm)
  q3 <- quantile(x, 0.75, na.rm = na.rm)
  iqr <- q3 - q1

  lower <- q1 - k * iqr
  upper <- q3 + k * iqr

  outlier_mask <- x < lower | x > upper
  outliers <- x[outlier_mask]

  list(
    outliers = outliers,
    indices = which(outlier_mask),
    lower_bound = lower,
    upper_bound = upper,
    n_outliers = length(outliers),
    n_total = length(x),
    proportion = length(outliers) / length(x),
    method = "IQR",
    k = k
  )
}

#' @title Z-Score outlier detection
#' @description
#' Detect outliers using standardized Z-scores.
#' Outlier definition: |Z| > threshold.
#'
#' @param x Numeric vector
#' @param threshold Z-score threshold (default 3, corresponding to 99.7% confidence interval)
#' @param robust Whether to use robust estimation (median + MAD, default FALSE)
#'
#' @return List containing outliers, indices, z_scores, threshold, n_outliers
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- c(rnorm(100), 10, -10)
#' detect_outliers_zscore(x)
detect_outliers_zscore <- function(x, threshold = 3, robust = FALSE) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]

  if (robust) {
    center <- median(x)
    scale <- mad(x, constant = 1)
    if (scale == 0) scale <- sd(x)
  } else {
    center <- mean(x)
    scale <- sd(x)
  }

  if (scale == 0) {
    return(list(
      outliers = numeric(0),
      indices = integer(0),
      z_scores = rep(0, length(x)),
      threshold = threshold,
      n_outliers = 0,
      n_total = length(x),
      proportion = 0,
      method = if (robust) "Z-Score (Robust)" else "Z-Score"
    ))
  }

  z_scores <- (x - center) / scale
  outlier_mask <- abs(z_scores) > threshold
  outliers <- x[outlier_mask]

  list(
    outliers = outliers,
    indices = which(outlier_mask),
    z_scores = z_scores,
    threshold = threshold,
    lower_bound = center - threshold * scale,
    upper_bound = center + threshold * scale,
    n_outliers = length(outliers),
    n_total = length(x),
    proportion = length(outliers) / length(x),
    method = if (robust) "Z-Score (Robust)" else "Z-Score"
  )
}

#' @title Grubbs test outlier detection
#' @description
#' Detect single extreme outliers in data using Grubbs test.
#' Assumes data are approximately normally distributed.
#'
#' @param x Numeric vector
#' @param alpha Significance level (default 0.05)
#' @param type Test type ("extreme" detects most extreme value,
#'   "min" detects minimum, "max" detects maximum)
#'
#' @return List containing outlier, G (Grubbs statistic),
#'   critical_value, p.value, is_outlier
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- c(rnorm(100), 10)
#' detect_outliers_grubbs(x)
detect_outliers_grubbs <- function(x, alpha = 0.05, type = c("extreme", "min", "max")) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) stop("Grubbs test requires at least 3 observations.")

  type <- match.arg(type)

  mean_x <- mean(x)
  sd_x <- sd(x)

  if (sd_x == 0) {
    return(list(
      outlier = NULL,
      G = 0,
      critical_value = NA,
      p.value = 1,
      is_outlier = FALSE,
      method = "Grubbs",
      n = n
    ))
  }

  if (type == "extreme") {
    deviations <- abs(x - mean_x)
    idx <- which.max(deviations)
    G <- deviations[idx] / sd_x
  } else if (type == "min") {
    idx <- which.min(x)
    G <- (mean_x - x[idx]) / sd_x
  } else {
    idx <- which.max(x)
    G <- (x[idx] - mean_x) / sd_x
  }

  t_crit <- stats::qt(1 - alpha / (2 * n), df = n - 2)
  critical_value <- ((n - 1) * t_crit) / sqrt(n * (n - 2 + t_crit^2))

  # private_grubbs_pvalue returns 2*(1 - pt(t, df)) which is the two-sided
  # p-value for a SINGLE tested value. Since Grubbs tests the maximum of n
  # deviations, the overall p-value must account for all n comparisons:
  #   p = 1 - (1 - p_single)^n
  # This makes the p-value consistent with the critical_value-based decision.
  p_single <- private_grubbs_pvalue(G, n)
  p_value <- 1 - (1 - p_single)^n

  list(
    outlier = x[idx],
    index = idx,
    G = G,
    critical_value = critical_value,
    p.value = p_value,
    is_outlier = G > critical_value,
    method = "Grubbs",
    alpha = alpha,
    n = n
  )
}

#' @title Dixon test outlier detection
#' @description
#' Detect outliers in small samples (3 <= n <= 30) using Dixon Q test.
#'
#' @param x Numeric vector
#' @param alpha Significance level (default 0.05)
#' @param type Test type ("min" detects minimum, "max" detects maximum, "both" detects both ends)
#'
#' @return List containing outlier, Q (Dixon statistic), critical_value, p.value, is_outlier
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- c(rnorm(20), 10)
#' detect_outliers_dixon(x)
detect_outliers_dixon <- function(x, alpha = 0.05, type = c("both", "min", "max")) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3 || n > 30) stop("Dixon test requires 3 <= n <= 30.")

  type <- match.arg(type)

  sorted_x <- sort(x)
  range_x <- sorted_x[n] - sorted_x[1]

  if (range_x == 0) {
    return(list(
      outlier = NULL,
      Q = 0,
      critical_value = NA,
      p.value = 1,
      is_outlier = FALSE,
      method = "Dixon",
      n = n
    ))
  }

  results <- list()

  if (type %in% c("min", "both")) {
    gap_min <- sorted_x[2] - sorted_x[1]
    Q_min <- gap_min / range_x
    crit_min <- private_dixon_critical(n, alpha)
    results$min <- list(
      outlier = sorted_x[1],
      index = which(x == sorted_x[1])[1],
      Q = Q_min,
      critical_value = crit_min,
      is_outlier = Q_min > crit_min
    )
  }

  if (type %in% c("max", "both")) {
    gap_max <- sorted_x[n] - sorted_x[n - 1]
    Q_max <- gap_max / range_x
    crit_max <- private_dixon_critical(n, alpha)
    results$max <- list(
      outlier = sorted_x[n],
      index = which(x == sorted_x[n])[1],
      Q = Q_max,
      critical_value = crit_max,
      is_outlier = Q_max > crit_max
    )
  }

  if (type == "both") {
    is_outlier <- results$min$is_outlier || results$max$is_outlier
    outlier <- c(
      if (results$min$is_outlier) results$min$outlier,
      if (results$max$is_outlier) results$max$outlier
    )
  } else {
    res <- results[[type]]
    is_outlier <- res$is_outlier
    outlier <- if (is_outlier) res$outlier else NULL
  }

  list(
    outlier = outlier,
    results = results,
    is_outlier = is_outlier,
    method = "Dixon",
    alpha = alpha,
    n = n
  )
}

#' @title Mahalanobis distance outlier detection (multivariate)
#' @description
#' Detect outliers in multivariate data using Mahalanobis distance.
#' Accounts for correlations between variables.
#'
#' @param data Data frame or matrix (one variable per column)
#' @param alpha Significance level (default 0.05)
#'
#' @return List containing outliers (outlier row numbers), mahalanobis_dist (Mahalanobis distance vector),
#'   critical_value, n_outliers
#' @export
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(x = rnorm(100), y = rnorm(100))
#' df <- rbind(df, c(10, 10))
#' detect_outliers_mahalanobis(df)
detect_outliers_mahalanobis <- function(data, alpha = 0.05) {
  data <- as.data.frame(data)
  data <- data[stats::complete.cases(data), ]
  n <- nrow(data)
  p <- ncol(data)

  if (n <= p) stop("Need more observations than variables.")

  center <- colMeans(data)
  cov_mat <- stats::cov(data)

  if (det(cov_mat) == 0) {
    stop("Covariance matrix is singular. Check for collinear variables.")
  }

  cov_inv <- solve(cov_mat)
  md <- stats::mahalanobis(data, center = center, cov = cov_mat)

  critical_value <- stats::qchisq(1 - alpha, df = p)

  outlier_mask <- md > critical_value
  outlier_indices <- which(outlier_mask)

  list(
    outliers = outlier_indices,
    mahalanobis_dist = md,
    critical_value = critical_value,
    n_outliers = length(outlier_indices),
    n_total = n,
    proportion = length(outlier_indices) / n,
    method = "Mahalanobis",
    alpha = alpha,
    p = p
  )
}

#' @title Comprehensive outlier detection
#' @description
#' Detect outliers using multiple methods simultaneously and summarize results.
#'
#' @param x Numeric vector
#' @param methods Detection methods (default includes IQR, Z-Score, Grubbs)
#' @param alpha Significance level (for Grubbs/Dixon)
#'
#' @return List containing consensus (outliers identified by all methods),
#'   all_results (results from each method), summary (summary statistics)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- c(rnorm(100), 10, -10)
#' detect_outliers_all(x)
detect_outliers_all <- function(x, methods = c("iqr", "zscore", "grubbs"),
                                 alpha = 0.05) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]

  results <- list()

  for (method in methods) {
    result <- tryCatch({
      switch(method,
        "iqr" = detect_outliers_iqr(x),
        "zscore" = detect_outliers_zscore(x),
        "grubbs" = detect_outliers_grubbs(x, alpha = alpha),
        "dixon" = if (length(x) <= 30) detect_outliers_dixon(x, alpha = alpha) else NULL,
        NULL
      )
    }, error = function(e) NULL)

    if (!is.null(result)) {
      results[[method]] <- result
    }
  }

  outlier_sets <- lapply(results, function(r) {
    if (!is.null(r$outliers)) r$outliers else if (!is.null(r$outlier)) r$outlier else numeric(0)
  })

  if (length(outlier_sets) > 0) {
    all_outliers <- unique(unlist(outlier_sets))
    consensus <- private_find_consensus(outlier_sets)
  } else {
    all_outliers <- numeric(0)
    consensus <- numeric(0)
  }

  list(
    consensus = consensus,
    n_consensus = length(consensus),
    all_outliers = all_outliers,
    n_total_outliers = length(all_outliers),
    n_total = length(x),
    all_results = results,
    summary = data.frame(
      method = names(results),
      n_outliers = sapply(results, function(r) r$n_outliers %||% ifelse(r$is_outlier, 1, 0)),
      proportion = sapply(results, function(r) r$proportion %||% NA)
    )
  )
}

# =============================================================================
# Internal helper functions
# =============================================================================

private_grubbs_pvalue <- function(G, n) {
  t_val <- sqrt((n - 2) * G^2 / (n - 1 - G^2))
  2 * (1 - stats::pt(t_val, df = n - 2))
}

private_dixon_critical <- function(n, alpha) {
  critical_values <- list(
    "0.05" = c(NA, NA, 0.941, 0.765, 0.642, 0.560, 0.507, 0.468,
               0.437, 0.412, 0.392, 0.376, 0.361, 0.349, 0.338,
               0.329, 0.321, 0.313, 0.306, 0.300, 0.295, 0.290,
               0.285, 0.281, 0.277, 0.274, 0.270, 0.267, 0.264),
    "0.01" = c(NA, NA, 0.988, 0.889, 0.780, 0.698, 0.637, 0.590,
               0.555, 0.526, 0.502, 0.482, 0.464, 0.448, 0.434,
               0.422, 0.411, 0.401, 0.392, 0.384, 0.377, 0.370,
               0.364, 0.358, 0.353, 0.348, 0.344, 0.340, 0.336)
  )

  alpha_key <- as.character(alpha)
  if (!alpha_key %in% names(critical_values)) {
    alpha_key <- "0.05"
  }

  if (n < 3 || n > 30) return(NA)

  critical_values[[alpha_key]][n]
}

private_find_consensus <- function(outlier_sets) {
  all_values <- unique(unlist(outlier_sets))
  consensus <- all_values[sapply(all_values, function(v) {
    sum(sapply(outlier_sets, function(s) v %in% s)) >= ceiling(length(outlier_sets) / 2)
  })]
  consensus
}
