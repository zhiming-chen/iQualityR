#' @useDynLib iQualityR.stat, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL
#' @title Comprehensive SPC Control Chart Constants Library
#' @description A vectorized, high-precision implementation of Shewhart constants for Statistical Process Control.
#' @details
#' This library calculates constants used to estimate process variation (sigma)
#' and define control limits for X-bar, R, S, and I-MR charts.
#'
#' **Reference Legend:**
#' * **Basic Factors:** Lower-case (d2, d3, c4) - Used for unbiased estimation of sigma.
#' * **Control Factors:** Upper-case (A2, A3, D3, D4, B3, B4, E2) - Used for UCL/LCL calculation.
#'
#' @name SPC_Constants
NULL

# --- Internal Core Calculation ---

#' @noRd
# Optimized internal core calculation function
.get_range_pair <- function(n) {
    calc_single <- function(ni) {
        if (ni < 2) stop("Sample size n must be at least 2.")

        up <- tryCatch({
            suppressWarnings(qtukey(1 - 1e-10, ni, Inf))
        }, error = function(e) {
            return(5 + 2 * sqrt(ni))
        })

        if (is.null(up) || is.na(up) || !is.finite(up)) {
            up <- 5 + 2 * sqrt(ni)
        }

        d2 <- integrate(function(x) 1 - ptukey(x, ni, Inf), 0, up, rel.tol=1e-12)$value
        e_w2 <- integrate(function(x) 2 * x * (1 - ptukey(x, ni, Inf)), 0, up, rel.tol=1e-12)$value
        d3 <- sqrt(pmax(0, e_w2 - d2^2))

        return(c(d2 = d2, d3 = d3))
    }
    if (length(n) == 1) return(calc_single(n))
    vapply(n, calc_single, numeric(2))
}

# --- 1. Basic Statistical Constants (Lower-case) ---

#' @title Calculate d2 Constant
#' @param n Sample size(s).
#' @return The value of d2 constant.
#' @details
#' **1. Source and Definition:** The d2 constant originates from the study of the "Studentized Range Distribution." It is defined as the expected value `E[W]` of the relative range W = R/sigma for a sample of size n from a normal distribution.
#'
#' **2. Theoretical Logic:** In Statistical Process Control (SPC), d2 serves as the bridge between the sample average range (R-bar) and the population standard deviation (sigma). The logic is: **Estimate of sigma = R-bar / d2**. It corrects the inherent bias where the range systematically increases as the sample size n grows.
#'
#' **3. Mathematical Formula:** This function implements the numerical integration method proposed by Hartley (1942):
#' **d2 = integral from 0 to infinity of 1 - Fr(w) dw**
#' where Fr(w) is the Cumulative Distribution Function (CDF) of the range for sample size n.
#'
#' **4. References:** #' * ASTM E2587 - Standard Practice for Use of Control Charts in Statistical Process Control.
#' * Hartley, H. O. (1942). "The Range in Normal Samples". Biometrika.
#'
#' @examples
#' get_d2(5) # Returns approximately 2.326
#' @export
get_d2 <- function(n) {
    res <- .get_range_pair(n)
    if (is.matrix(res)) {
        return(unname(res["d2", , drop = TRUE]))
    } else {
        return(unname(res["d2"]))
    }
}

#' @title Calculate d3 Constant
#' @param n Sample size(s).
#' @return The value of d3 constant.
#' @details
#' **1. Source and Definition:** d3 represents the standard deviation of the relative range SD(R/sigma) for a sample of size n from a normal distribution.
#'
#' **2. Theoretical Logic:** It is used to calculate the control limits for R-charts. Since control limits are typically set at +/- 3 * sigma_R, and **sigma_R = d3 * sigma**, substituting **sigma = R-bar/d2** allows us to derive the D3 and D4 factors.
#'
#' **3. Mathematical Formula:**
#' `d3 = sqrt( E[W^2] - (E[W])^2 )`
#'
#' **4. References:** #' * Montgomery, D. C. (2019). "Introduction to Statistical Quality Control". Wiley.
#'
#' @examples
#' get_d3(5) # Returns approximately 0.864
#' @export
get_d3 <- function(n) {
    res <- .get_range_pair(n)
    if (is.matrix(res)) {
        return(unname(res["d3", , drop = TRUE]))
    } else {
        return(unname(res["d3"]))
    }
}

#' @title Calculate c4 Constant
#' @param n Sample size(s).
#' @return The value of c4 constant.
#' @details
#' **1. Source and Theoretical Logic:** The sample standard deviation s is the square root of an unbiased estimator of variance sigma^2. According to Jensen's Inequality, E(s) < sigma. The c4 factor corrects this bias so that **E(s/c4) = sigma**.
#'
#' **2. Mathematical Formula:** Derived using the properties of the Chi-distribution:
#' **c4(n) = sqrt(2/(n-1)) * Gamma(n/2) / Gamma((n-1)/2)**
#'
#' **3. References:** #' * Shewhart, W. A. (1931). "Economic Control of Quality of Manufactured Product".
#' @examples
#' get_c4(5) # Returns approximately 0.9400
#' @export
get_c4 <- function(n) {
    if (any(n < 2)) stop("n must be >= 2")
    exp(log(sqrt(2/(n-1))) + lgamma(n/2) - lgamma((n-1)/2))
}

#' @title Calculate c4_prime Constant
#' @param n Sample size(s).
#' @param B Iterations for simulation (default 1e6).
#' @return The value of c4_prime constant.
#' @details
#' c4_prime is used for bias correction when sigma is estimated via
#' Mean Successive Squared Differences (MSSD).
#' @export
get_c4_prime <- function(n, B = 1000000) {
    res <- vapply(n, function(ni) {
        if (ni < 2) return(NA_real_)

        get_c4_prime_cpp(as.integer(ni), as.integer(B))
    }, numeric(1))
    return(res)
}


# --- 2. Combined Factors for X-bar Charts (Upper-case) ---

#' @title Calculate A2 Constant
#' @param n Sample size(s).
#' @param k Sigma level (default 3).
#' @description **A2 = k / (d2 * sqrt(n))**
#' @details
#' **1. Purpose:** Used in X-bar charts when estimating variation via the average range (R-bar):
#' **UCL/LCL = X-double-bar +/- A2 * R-bar**.
#'
#' **2. Theoretical Logic:** The standard control limit formula is **mu +/- k * sigma / sqrt(n)**. Substituting **sigma = R-bar / d2** yields **k * R-bar / (d2 * sqrt(n))**.
#' @export
get_A2 <- function(n,k = 3) k / (get_d2(n) * sqrt(n))

#' @title Calculate A3 Constant
#' @param n Sample size(s).
#' @param k Sigma level (default 3).
#' @description **A3 = k / (c4 * sqrt(n))**
#' @details Used in X-bar charts when variation is estimated via average standard deviation (s-bar).
#' @export
get_A3 <- function(n,k = 3) k / (get_c4(n) * sqrt(n))

# --- 3. Combined Factors for Range Charts (Upper-case) ---

#' @title Calculate D4 Constant
#' @rdname get_d4
#' @param n Sample size(s).
#' @param k Sigma level (default 3).
#' @description D4 = 1 + k*(d3/d2)
#' @details
#' Used to determine the upper control limit for R-charts: **UCL = D4 * R-bar**.
#' @examples
#' get_D4(5) # Returns approximately 2.114
#' get_D4(10) # Returns approximately 1.777
#' @export
get_D4 <- function(n, k = 3) {
    1 + k * (get_d3(n) / get_d2(n))
}

#' @title Calculate D3 Constant
#' @rdname get_d3
#' @param n Sample size(s).
#' @param k Sigma level (default 3).
#' @description D3 = max(0, 1 - k*(d3/d2))
#' @details
#' Used to determine the lower control limit for R-charts: **LCL = D3 * R-bar**.
#' @examples
#' get_D3(5) # Returns approximately 0
#' get_D3(10) # Returns approximately 0.223
#' @export
get_D3 <- function(n, k = 3) {
    val <- 1 - k * (get_d3(n) / get_d2(n))
    pmax(0, val)
}

# --- 4. Combined Factors for Standard Deviation Charts (Upper-case) ---

#' @title Calculate B3 and B4 Constants
#' @param n Sample size(s).
#' @param k Sigma level (default 3).
#' @description **B4 = 1 + (k/c4) * sqrt(1 - c4^2)**, **B3 = max(0, 1 - (k/c4) * sqrt(1 - c4^2))**
#' @details
#' **1. Purpose:** Used for S-charts: **UCL = B4 * s-bar** and **LCL = B3 * s-bar**.
#' @export
get_B4 <- function(n, k = 3) {
    c4 <- get_c4(n)
    1 + k * (sqrt(1 - c4^2) / c4)
}

#' @title Calculate B3 Constant
#' @param n Sample size(s).
#' @param k Sigma level (default 3).
#' @description **B3 = max(0, 1 - (k/c4) * sqrt(1 - c4^2))**
#' @details
#' Used to determine the lower control limit for S-charts: **LCL = B3 * s-bar**.
#' @export
get_B3 <- function(n, k = 3) {
    c4 <- get_c4(n)
    pmax(0, 1 - k * (sqrt(1 - c4^2) / c4))
}

# --- 5. Combined Factors for Individuals Charts (Upper-case) ---

#' @title Calculate E2 Constant
#' @param n Sample size(s).
#' @param k Sigma level (default 3).
#' @description **E2 = k / d2**. Used for Individual (X) charts.
#' @return A numeric vector of E2 values.
#' @details
#' **1. Source:** Based on the relationship between the Mean Moving Range and sigma in an Individuals Control Chart (I-MR).
#'
#' **2. Purpose:** Used to calculate control limits for an Individuals (X) chart:
#' **UCL/LCL = X-bar +/- E2 * MR-bar**
#' where MR-bar is the average moving range.
#'
#' **3. Theoretical Logic:** The limits are defined as mu +/- k*sigma. Since **sigma = MR-bar / d2**, the formula becomes X-bar +/- k*(MR-bar / d2).
#' @export
get_E2 <- function(n, k = 3) k / get_d2(n)


#' @title Calculate d4 Constant
#' @param n Sample size(s).
#' @return The value of the d4 constant (median of the relative range).
#' @details
#' **1. Source and Theoretical Logic:** While d2(n) is the mean of the relative range distribution (E(R)/sigma), d4(n) is the **median** of the relative range distribution. It is used to estimate sigma based on the median moving range or median subgroup range, providing a robust estimate that is less sensitive to outliers than the mean-based d2.
#'
#' **2. Mathematical Formula:** d4(n) is the value for which the Cumulative Distribution Function (CDF) of the relative range equals 0.5. The CDF of the range (R) for a sample of size n from a standard normal distribution is:
#' **F(r; n) = n * integral from -inf to inf of (Phi(x+r) - Phi(x))^(n-1) * phi(x) dx**
#' d4(n) is found by solving: **F(d4; n) = 0.5** via numerical root-finding.
#'
#' **3. References:** #' * Minitab Technical Support. "Methods and formulas for Z-MR Chart".
#' * Wheeler, D. J. (1995). "Advanced Topics in Statistical Process Control".
#' @examples
#' get_d4(2) # Returns approximately 0.9539
#' get_d4(5) # Returns approximately 2.2566
#' @export
get_d4 <- function(n) {
    # Standardize input for vectorized processing if necessary
    # d4 is only defined for n >= 2
    res <- sapply(n, function(ni) {
        if (is.na(ni) || ni < 2) return(NA)

        # Internal function: Cumulative Distribution Function of the Relative Range
        range_cdf <- function(r, n_size) {
            if (r <= 0) return(0)
            integrand <- function(x) {
                (pnorm(x + r) - pnorm(x))^(n_size - 1) * dnorm(x)
            }
            # Integrate across the effective range of the standard normal distribution
            n_size * integrate(integrand, lower = -8, upper = 8)$value
        }

        # Root-finding to solve F(r; n) - 0.5 = 0
        target_fn <- function(r) {
            range_cdf(r, ni) - 0.5
        }

        # Numerical search for the median (d4)
        # The search interval [0.01, 10] safely covers common sample sizes
        tryCatch({
            uniroot(target_fn, interval = c(0.01, 10), tol = 1e-8)$root
        }, error = function(e) NA)
    })

    return(res)
}

