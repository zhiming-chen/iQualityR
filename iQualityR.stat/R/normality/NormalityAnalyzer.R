# =============================================================================
# File: R/normality/NormalityAnalyzer.R
# Description: Normality test calculation engine (pure computation, no graphics)
# =============================================================================

#' @title NormalityAnalyzer: Normality test computation engine
#' @description
#' A pure computation engine for performing various normality tests, returning structured results.
#' Called by iqr_normality and internal subpackage functions.
#'
#' **Supported test methods**:
#' - Shapiro-Wilk (n <= 5000, highest power for small samples)
#' - Anderson-Darling (available for large samples, sensitive to tails)
#' - Lilliefors (modified KS test)
#' - Cramer-von Mises
#' - Shapiro-Francia (sensitive to skewness)
#' - Auto selection (based on sample size)
#'
#' @export
NormalityAnalyzer <- R6::R6Class("NormalityAnalyzer",
  public = list(
    #' @description Perform normality test
    #' @param x Numeric vector
    #' @param method Test method ("auto", "sw", "ad", "lillie", "cvm", "sf")
    #' @param alpha Significance level (default 0.05)
    #' @return Structured test result list
    test = function(x, method = c("auto", "sw", "ad", "lillie", "cvm", "sf"),
                    alpha = 0.05) {
      method <- match.arg(method)

      x <- x[!is.na(x)]
      n <- length(x)

      if (n < 3) {
        stop("Need at least 3 non-missing values for normality test.")
      }

      # Auto-select
      if (method == "auto") {
        method <- private$.select_method(n)
      }

      result <- switch(method,
        "sw"     = private$.test_sw(x, n, alpha),
        "ad"     = private$.test_ad(x, n, alpha),
        "lillie" = private$.test_lillie(x, n, alpha),
        "cvm"    = private$.test_cvm(x, n, alpha),
        "sf"     = private$.test_sf(x, n, alpha),
        stop(sprintf("Unknown method: %s", method))
      )

      result
    },

    #' @description Batch normality tests for multiple columns in a data frame
    #' @param data Data frame
    #' @param vars Vector of column names (default all numeric columns)
    #' @param method Test method
    #' @param alpha Significance level
    #' @return List of test results (one element per variable)
    test_multiple = function(data, vars = NULL, method = "auto", alpha = 0.05) {
      if (!is.data.frame(data)) {
        stop("data must be a data frame.")
      }

      if (is.null(vars)) {
        vars <- names(data)[sapply(data, is.numeric)]
      }

      results <- list()
      for (v in vars) {
        if (v %in% names(data)) {
          x <- data[[v]]
          if (is.numeric(x)) {
            tryCatch({
              results[[v]] <- self$test(x, method = method, alpha = alpha)
              results[[v]]$variable <- v
            }, error = function(e) {
              results[[v]] <<- list(
                variable = v,
                method = "error",
                error = e$message
              )
            })
          }
        }
      }

      results
    },

    #' @description Normality diagnosis (skewness, kurtosis, descriptive statistics)
    #' @param x Numeric vector
    #' @return Diagnostic result list
    diagnose = function(x) {
      x <- x[!is.na(x)]
      n <- length(x)

      if (n < 4) stop("Need at least 4 non-missing values for diagnosis.")

      skew_val <- moments::skewness(x)
      kurt_val <- moments::kurtosis(x)

      # Skewness direction
      if (abs(skew_val) < 0.5) {
        skew_dir <- "Approximately symmetric"
      } else if (skew_val > 0) {
        skew_dir <- "Right-skewed (positive)"
      } else {
        skew_dir <- "Left-skewed (negative)"
      }

      # Kurtosis type
      excess_kurt <- kurt_val - 3
      if (abs(excess_kurt) < 0.5) {
        kurt_type <- "Mesokurtic (normal-like)"
      } else if (excess_kurt > 0) {
        kurt_type <- "Leptokurtic (heavy-tailed)"
      } else {
        kurt_type <- "Platykurtic (light-tailed)"
      }

      list(
        n = n,
        mean = mean(x),
        sd = sd(x),
        median = median(x),
        skewness = skew_val,
        skewness_direction = skew_dir,
        kurtosis = kurt_val,
        excess_kurtosis = excess_kurt,
        kurtosis_type = kurt_type,
        min = min(x),
        max = max(x),
        range = max(x) - min(x),
        q1 = quantile(x, 0.25),
        q3 = quantile(x, 0.75),
        iqr = IQR(x)
      )
    }
  ),

  private = list(
    # Auto-select test method based on sample size
    .select_method = function(n) {
      if (n <= 5000) {
        "sw"       # Shapiro-Wilk (highest power for small samples)
      } else {
        "ad"       # Anderson-Darling (available for large samples, sensitive to tails)
      }
    },

    # Shapiro-Wilk test
    .test_sw = function(x, n, alpha) {
      if (n > 5000) {
        warning("Shapiro-Wilk test is not recommended for n > 5000. Consider Anderson-Darling.")
      }

      result <- shapiro.test(x)

      list(
        test_type = "Normality Test",
        method = "Shapiro-Wilk",
        data_name = deparse(substitute(x)),
        statistic = c(W = result$statistic),
        p.value = result$p.value,
        alpha = alpha,
        is_normal = result$p.value > alpha,
        n = n,
        sample_mean = mean(x),
        sample_sd = sd(x),
        skewness = moments::skewness(x),
        excess_kurtosis = moments::kurtosis(x) - 3
      )
    },

    # Anderson-Darling test
    .test_ad = function(x, n, alpha) {
      result <- nortest::ad.test(x)

      list(
        test_type = "Normality Test",
        method = "Anderson-Darling",
        data_name = deparse(substitute(x)),
        statistic = c(A = result$statistic),
        p.value = result$p.value,
        alpha = alpha,
        is_normal = result$p.value > alpha,
        n = n,
        sample_mean = mean(x),
        sample_sd = sd(x),
        skewness = moments::skewness(x),
        excess_kurtosis = moments::kurtosis(x) - 3
      )
    },

    # Lilliefors test (modified KS)
    .test_lillie = function(x, n, alpha) {
      result <- nortest::lillie.test(x)

      list(
        test_type = "Normality Test",
        method = "Lilliefors (Kolmogorov-Smirnov)",
        data_name = deparse(substitute(x)),
        statistic = c(D = result$statistic),
        p.value = result$p.value,
        alpha = alpha,
        is_normal = result$p.value > alpha,
        n = n,
        sample_mean = mean(x),
        sample_sd = sd(x),
        skewness = moments::skewness(x),
        excess_kurtosis = moments::kurtosis(x) - 3
      )
    },

    # Cramer-von Mises test
    .test_cvm = function(x, n, alpha) {
      result <- nortest::cvm.test(x)

      list(
        test_type = "Normality Test",
        method = "Cramer-von Mises",
        data_name = deparse(substitute(x)),
        statistic = c(W = result$statistic),
        p.value = result$p.value,
        alpha = alpha,
        is_normal = result$p.value > alpha,
        n = n,
        sample_mean = mean(x),
        sample_sd = sd(x),
        skewness = moments::skewness(x),
        excess_kurtosis = moments::kurtosis(x) - 3
      )
    },

    # Shapiro-Francia test
    .test_sf = function(x, n, alpha) {
      result <- nortest::sf.test(x)

      list(
        test_type = "Normality Test",
        method = "Shapiro-Francia",
        data_name = deparse(substitute(x)),
        statistic = c(W = result$statistic),
        p.value = result$p.value,
        alpha = alpha,
        is_normal = result$p.value > alpha,
        n = n,
        sample_mean = mean(x),
        sample_sd = sd(x),
        skewness = moments::skewness(x),
        excess_kurtosis = moments::kurtosis(x) - 3
      )
    }
  )
)
