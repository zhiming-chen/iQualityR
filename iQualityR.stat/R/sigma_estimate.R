#' @title Standard Deviation Estimation and Decomposition Functions
#' @description Provides methods for estimating standard deviation and decomposing total standard deviation into within-group and between-group components
#' @param x Numeric vector or data frame containing measurements
#' @param subgroup Optional vector specifying subgroup membership
#' @param n_size Optional integer for sequential subgroup size
#' @param m_span Span for moving range methods, default is 2
#' @param method Estimation method, options include "r_bar", "s_bar", "pooled_s", "mr_bar", "mr_median", "mssd", "total"
#' @param use_unbiased Whether to use unbiased estimation, default is TRUE
#' @param x_col Column name for measurements when x is a data frame
#' @param subgroup_col Column name for subgroup identifiers when x is a data frame
#' @details
#' This set of functions is used for standard deviation estimation in quality control and process capability analysis. **Standard deviation estimation** is a core component of Statistical Process Control (SPC), used to measure process variation. The functions support multiple estimation methods, each designed for different scenarios:
#'
#' 1. **Range method ("r_bar")**: Estimates standard deviation based on subgroup ranges, suitable for small sample subgroups (n<=10), with formula: sigma = R_bar / d2, where d2 is the unbiased coefficient for range.
#'
#' 2. **Standard deviation method ("s_bar")**: Based on weighted average of subgroup standard deviations, suitable for large sample subgroups, with formula: sigma = (sum(s_i * c4(n_i))) / (sum(c4(n_i))), where c4 is the unbiased coefficient for standard deviation.
#'
#' 3. **Pooled standard deviation ("pooled_s")**: Combines all subgroups to calculate overall standard deviation, suitable when subgroup sizes are similar, with formula: s_p = sqrt(sum((n_i-1)*s_i^2) / sum(n_i-1)).
#'
#' 4. **Moving range method ("mr_bar")**: Estimates based on the **average** of moving ranges of subgroup means, suitable when between-subgroup variation is large, with formula: sigma = MR_bar / d2(m), where m is the moving span.
#'
#' 5. **Moving range method ("mr_median")**: Estimates based on the **median** of moving ranges of subgroup means, suitable when between-subgroup variation is large, with formula: sigma = MR_median / d4(m), where m is the moving span.
#'
#' 6. **Mean Squared Successive Difference ("mssd")**: Based on squared differences between consecutive subgroup means, with formula: sigma = sqrt(sum((x_i - x_i-1)^2) / (2*(n-1))). If unbiased estimation is used, the result is divided by c4'(n).
#'
#' 7. **Total standard deviation ("total")**: Directly calculates the overall standard deviation of all data, reflecting long-term process variation.
#'
#' **Standard deviation decomposition** is based on variance decomposition principle: Total variance = Within-group variance + Between-group variance. The specific formula is:
#' sigma_total^2 = sigma_within^2 + (sigma_between^2 / n)
#' where n is the subgroup size. The function estimates within-group standard deviation (sigma_within) and between-group standard deviation (sigma_between), then calculates the total standard deviation (sigma_total).
#'
#' Unbiased estimation is achieved through coefficients c4(n), d2(n), d4(m), etc., which are related to sample size, ensuring the unbiasedness of the estimators.
#'
#' @references
#' 1. Montgomery, D. C. (2013). *Introduction to Statistical Quality Control* (7th ed.). Wiley.
#' 2. Wheeler, D. J., & Chambers, D. S. (1992). *Understanding Statistical Process Control* (2nd ed.). SPC Press.
#' 3. ASTM E2587-12: *Standard Practice for Use of Control Charts in Statistical Process Control*.
#'
#' @examples
#' # Example 1: Standard deviation estimation
#' set.seed(123)
#' data <- rnorm(50, mean = 10, sd = 2)
#' subgroup <- rep(1:10, each = 5)
#'
#' # Estimate standard deviation using range method
#' sigma_rbar <- sigma_estimate(data, subgroup = subgroup, method = "r_bar")
#' print(sigma_rbar)
#'
#' # Estimate standard deviation using standard deviation method
#' sigma_sbar <- sigma_estimate(data, subgroup = subgroup, method = "s_bar")
#' print(sigma_sbar)
#'
#' # Example 2: Standard deviation decomposition
#' # Generate data with within-group and between-group variation
#' set.seed(123)
#' between_var <- rnorm(10, mean = 0, sd = 1.5)
#' data <- unlist(lapply(between_var, function(x) rnorm(5, mean = x, sd = 1)))
#' subgroup <- rep(1:10, each = 5)
#'
#' # Decompose standard deviation
#' sigma_decomp <- sigma_decomposition(data, subgroup = subgroup)
#' print(sigma_decomp)
#'
#' # Example 3: Data frame input
#' df <- data.frame(value = data, group = subgroup)
#' sigma_df <- sigma_estimate(df, x_col = "value", subgroup_col = "group", method = "pooled_s")
#' print(sigma_df)
#'
#' @seealso
#' \code{\link[stats]{sd}}
#'
#' @name sigma_functions
#' @aliases sigma_estimate
#' @rdname sigma_functions
#' @export

sigma_estimate <- function(x,
                           subgroup = NULL,
                           n_size = NULL,
                           m_span = 2,
                           method = c("r_bar", "s_bar", "pooled_s", "mr_bar", "mr_median", "mssd", "total"),
                           use_unbiased = TRUE,
                           x_col = NULL,
                           subgroup_col = NULL) {
  method <- match.arg(method)


  if (method == "total") {
    data_clean <- .prepare_sigma_data(x, subgroup, n_size, x_col, subgroup_col)
    if (nrow(data_clean) < 2) {
      return(NA_real_)
    }
    s_total <- stats::sd(data_clean$x, na.rm = TRUE)
    if (use_unbiased) s_total <- s_total / get_c4(nrow(data_clean))
    return(s_total)
  }


  data_clean <- .prepare_sigma_data(x, subgroup, n_size, x_col, subgroup_col)
  if (nrow(data_clean) < 2) {
    return(NA_real_)
  }


  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::as.data.table(data_clean)
    stats <- dt[, .(
      n = .N,
      sub_mean = mean(x, na.rm = TRUE),
      R = if (.N > 1) diff(range(x, na.rm = TRUE)) else NA_real_,
      S = if (.N > 1) stats::sd(x, na.rm = TRUE) else NA_real_
    ), by = g]
  } else {
    g <- data_clean$g
    x <- data_clean$x
    ug <- unique(g)
    stats <- data.frame(
      g = ug,
      n = as.numeric(tapply(x, g, length)),
      stringsAsFactors = FALSE
    )
    stats$sub_mean <- tapply(x, g, mean, na.rm = TRUE)
    stats$R <- tapply(x, g, function(y) if (length(y) > 1) diff(range(y, na.rm = TRUE)) else NA)
    stats$S <- tapply(x, g, function(y) if (length(y) > 1) stats::sd(y, na.rm = TRUE) else NA)
  }


  sigma <- .compute_sigma_core(stats, method, use_unbiased, m_span)

  return(sigma)
}

#' @title Prepare Data for Standard Deviation Estimation
#' @description Internal helper function to organize input data into subgroups and clean missing values
#' @param x Numeric vector or data frame containing measurements
#' @param subgroup Optional vector specifying subgroup membership
#' @param n_size Optional integer for sequential subgroup size
#' @param x_col Column name for measurements when x is a data frame
#' @param subgroup_col Column name for subgroup identifiers when x is a data frame
#' @return A data frame with columns 'g' (subgroup) and 'x' (measurements)
#' @noRd

.prepare_sigma_data <- function(x, subgroup, n_size, x_col, subgroup_col) {
  if (is.data.frame(x)) {
    if (is.null(x_col) || is.null(subgroup_col)) {
      stop("Must specify `x_col` and `subgroup_col` for data frames.")
    }
    df <- data.frame(g = x[[subgroup_col]], x = x[[x_col]])
  } else {
    if (!is.numeric(x)) stop("`x` must be numeric.")

    if (!is.null(subgroup)) {
      if (length(subgroup) != length(x)) stop("`subgroup` length mismatch.")
      g <- subgroup
    } else if (!is.null(n_size)) {
      g <- rep(seq_len(ceiling(length(x) / n_size)), each = n_size, length.out = length(x))
    } else {
      g <- seq_along(x)
    }
    df <- data.frame(g = g, x = x)
  }

  # Remove rows with missing values
  df[stats::complete.cases(df), ]
}


#' @title Standard Deviation Estimation
#' @description Estimates process standard deviation using various statistical methods
#' @param x Numeric vector or data frame containing measurements
#' @param subgroup Optional vector specifying subgroup membership
#' @param n_size Optional integer for sequential subgroup size
#' @param m_span Span for moving range methods, default is 2
#' @param method Estimation method, options include "r_bar", "s_bar", "pooled_s", "mr_bar", "mr_median", "mssd", "total"
#' @param use_unbiased Whether to use unbiased estimation, default is TRUE
#' @param x_col Column name for measurements when x is a data frame
#' @param subgroup_col Column name for subgroup identifiers when x is a data frame
#' @return Estimated standard deviation value
#' @examples
#' # Generate example data
#' set.seed(123)
#' data <- rnorm(50, mean = 10, sd = 2)
#' subgroup <- rep(1:10, each = 5)
#'
#' # Estimate standard deviation using range method
#' sigma_rbar <- sigma_estimate(data, subgroup = subgroup, method = "r_bar")
#' print(sigma_rbar)
#'
#' # Estimate total standard deviation
#' sigma_total <- sigma_estimate(data, method = "total")
#' print(sigma_total)
#'
#' @noRd
#'
.compute_sigma_core <- function(stats, method, use_unbiased, m_span) {
  switch(method,
    "r_bar" = {
      d2 <- sapply(stats$n, get_d2)
      d3 <- sapply(stats$n, get_d3)

      r_aux <- d2^2 / d3^2
      sum(stats$R * d2 / d3^2, na.rm = TRUE) / sum(r_aux, na.rm = TRUE)
    },
    "s_bar" = {
      if (!use_unbiased) {
        return(mean(stats$S, na.rm = TRUE))
      }
      c4 <- sapply(stats$n, get_c4)
      s_aux <- c4^2 / (1 - c4^2)
      sum(stats$S * s_aux / c4, na.rm = TRUE) / sum(s_aux, na.rm = TRUE)
    },
    "pooled_s" = {
      df_total <- sum(stats$n - 1)
      s_pooled <- sqrt(sum(stats$S^2 * (stats$n - 1)) / df_total)
      if (use_unbiased) s_pooled / get_c4(df_total + 1) else s_pooled
    },
    "mr_bar" = {
      means <- stats$sub_mean
      if (length(means) <= m_span) {
        return(NA_real_)
      }
      mr <- abs(diff(means, lag = (m_span - 1)))
      mean(mr, na.rm = TRUE) / get_d2(m_span)
    },
    "mr_median" = {
      means <- stats$sub_mean
      if (length(means) <= m_span) {
        return(NA_real_)
      }

      mr <- abs(diff(means, lag = (m_span - 1)))
      stats::median(mr, na.rm = TRUE) / get_d4(m_span)
    },
    "mssd" = {
      mssd_val <- sqrt(sum(diff(stats$sub_mean)^2) / (2 * (nrow(stats) - 1)))
      if (use_unbiased) mssd_val / get_c4_prime(nrow(stats)) else mssd_val
    }
  )
}


#' @title Standard Deviation Decomposition
#' @description Decomposes total standard deviation into within-group and between-group components
#' @param x Numeric vector or data frame containing measurements
#' @param subgroup Optional vector specifying subgroup membership
#' @param n_size Optional integer for sequential subgroup size
#' @param within_method Method for within-group standard deviation estimation, default is "r_bar"
#' @param between_method Method for between-group standard deviation estimation, default is "mssd"
#' @param total_unbiased Whether to use unbiased estimation for total standard deviation, default is FALSE
#' @param within_unbiased Whether to use unbiased estimation for within-group standard deviation, default is TRUE
#' @param between_unbiased Whether to use unbiased estimation for between-group standard deviation, default is TRUE
#' @param ... Additional parameters passed to sigma_estimate
#' @return A data frame containing within-group, between-group, combined, and total standard deviations
#' @examples
#' # Generate data with within-group and between-group variation
#' set.seed(123)
#' between_var <- rnorm(10, mean = 0, sd = 1.5)
#' data <- unlist(lapply(between_var, function(x) rnorm(5, mean = x, sd = 1)))
#' subgroup <- rep(1:10, each = 5)
#'
#' # Decompose standard deviation
#' sigma_decomp <- sigma_decomposition(data, subgroup = subgroup)
#' print(sigma_decomp)
#' @export


sigma_decomposition <- function(x,
                                subgroup = NULL,
                                n_size = NULL,
                                within_method = "r_bar",
                                between_method = "mssd",
                                total_unbiased = FALSE,
                                within_unbiased = TRUE,
                                between_unbiased = TRUE,
                                ...) {
  # Calculate total standard deviation (long-term variation)
  s_total <- sigma_estimate(
    x = x,
    subgroup = subgroup,
    n_size = n_size,
    method = "total",
    use_unbiased = total_unbiased,
    ...
  )

  # Calculate within-subgroup standard deviation (short-term variation)
  s_w <- sigma_estimate(
    x = x,
    subgroup = subgroup,
    n_size = n_size,
    method = within_method,
    use_unbiased = within_unbiased,
    ...
  )

  # Calculate between-subgroup standard deviation component
  s_b_raw <- sigma_estimate(
    x = x,
    subgroup = subgroup,
    n_size = n_size,
    method = between_method,
    use_unbiased = between_unbiased,
    ...
  )

  # Determine subgroup size for adjustment
  if (!is.null(n_size)) {
    n <- n_size
  } else if (!is.null(subgroup)) {
    n <- length(x) / length(unique(subgroup))
  } else {
    n <- 1
  }

  # Calculate adjusted between-subgroup standard deviation
  # Using the relationship: total^2 = within^2 + between^2/n
  # Therefore: between^2 = total^2 - within^2/n -> but we're using between_method directly
  # So: s_b^2 = s_b_raw^2 - (s_w^2 / n) to avoid double counting within variation
  s_b <- sqrt(max(0, s_b_raw^2 - (s_w^2 / n)))

  # Calculate combined within and between variation
  s_b_w <- sqrt(s_w^2 + s_b^2)

  # Return results as a data frame
  return(data.frame(
    sigma_within = s_w,
    sigma_between = s_b,
    sigma_between_within = s_b_w,
    sigma_total = s_total,
    n_subgroup = n,
    stringsAsFactors = FALSE
  ))
}
