# =============================================================================
# File: R/quality_metrics.R
# Description: Cross-industry basic quality metric calculation functions (regular functions, not R6)
#
# Inclusion principles:
#   - Require statistical distribution knowledge (e.g., Cpk->PPM needs normal distribution integral)
#   - Require look-up tables or iteration (e.g., Sigma Level <-> PPM conversion)
#   - Require multi-step calculation (e.g., DPMO needs to consider opportunity count)
#   - Require industry benchmark comparison
#
# Not included: Simple division (e.g., PPM = defects/total * 10^6), users can do this in one line
# =============================================================================

# =============================================================================
# 1. Process capability -> Expected non-conforming rate
# =============================================================================

#' @title Convert process capability index to expected non-conforming rate (PPM)
#' @description
#' Calculates expected non-conforming rate (PPM) based on Cpk/Ppk and specification limits.
#' Based on normal distribution assumption, uses Phi function to calculate tail probability.
#'
#' @param cpk Process capability index Cpk
#' @param ppk Process performance index Ppk (optional, default equals Cpk)
#' @param usl Upper specification limit
#' @param lsl Lower specification limit
#' @param mean Process mean (optional, default centered)
#' @param sigma_within Within-group standard deviation (optional, derived from Cpk)
#' @param sigma_overall Overall standard deviation (optional, derived from Ppk)
#'
#' @return List containing within and overall two sub-lists, each with below_lsl, above_usl, total three PPM values
#' @export
#'
#' @examples
#' # Cpk = 1.33, USL = 10, LSL = 0
#' capability_to_ppm(cpk = 1.33, usl = 10, lsl = 0)
#'
#' # Cpk = 1.67, Ppk = 1.50
#' capability_to_ppm(cpk = 1.67, ppk = 1.50, usl = 10, lsl = 0)
capability_to_ppm <- function(cpk, ppk = NULL, usl, lsl,
                               mean = NULL, sigma_within = NULL, sigma_overall = NULL) {
  if (is.null(ppk)) ppk <- cpk

  # Specification tolerance
  tolerance <- usl - lsl
  if (tolerance <= 0) stop("USL must be greater than LSL.")

  # If mean not provided, assume centered
  if (is.null(mean)) mean <- (usl + lsl) / 2

  # Back-calculate sigma
  if (is.null(sigma_within)) {
    # Cpk = min[(USL-mu)/(3*sigma), (mu-LSL)/(3*sigma)]
    # When centered: Cpk = (USL-LSL)/(6*sigma) -> sigma = (USL-LSL)/(6*Cpk)
    sigma_within <- tolerance / (6 * cpk)
  }
  if (is.null(sigma_overall)) {
    sigma_overall <- tolerance / (6 * ppk)
  }

  # Within (subgroup)
  p_below_within <- pnorm(lsl, mean = mean, sd = sigma_within)
  p_above_within <- 1 - pnorm(usl, mean = mean, sd = sigma_within)

  # Overall
  p_below_overall <- pnorm(lsl, mean = mean, sd = sigma_overall)
  p_above_overall <- 1 - pnorm(usl, mean = mean, sd = sigma_overall)

  list(
    within = list(
      below_lsl = p_below_within * 1e6,
      above_usl = p_above_within * 1e6,
      total = (p_below_within + p_above_within) * 1e6
    ),
    overall = list(
      below_lsl = p_below_overall * 1e6,
      above_usl = p_above_overall * 1e6,
      total = (p_below_overall + p_above_overall) * 1e6
    ),
    cpk = cpk,
    ppk = ppk,
    mean = mean,
    sigma_within = sigma_within,
    sigma_overall = sigma_overall
  )
}


# =============================================================================
# 2. Sigma Level <-> PPM <-> Yield conversion
# =============================================================================

#' @title Convert PPM to Sigma Level
#' @description
#' Converts non-conforming rate (PPM) to corresponding Sigma level.
#' Considers 1.5 sigma shift (six sigma convention), can be turned off.
#'
#' @param ppm Non-conforming parts per million
#' @param shift Long-term shift (default 1.5, set to 0 to disable)
#'
#' @return Numeric, Sigma Level
#' @export
#'
#' @examples
#' ppm_to_sigma(3.4)        # ~ 6.0 (with 1.5 sigma shift)
#' ppm_to_sigma(3.4, 0)     # ~ 4.5 (no shift)
#' ppm_to_sigma(66807)       # ~ 3.0
ppm_to_sigma <- function(ppm, shift = 1.5) {
  if (ppm < 0) stop("PPM must be non-negative.")
  if (ppm == 0) return(Inf)
  if (ppm >= 1e6) return(0)

  p_defect <- ppm / 1e6
  # One-sided tail probability -> Z value
  z <- qnorm(1 - p_defect)
  # Add shift
  z + shift
}

#' @title Convert Sigma Level to PPM
#' @description
#' Converts Sigma level to expected non-conforming rate (PPM).
#'
#' @param sigma Sigma Level
#' @param shift Long-term shift (default 1.5)
#'
#' @return Numeric, PPM
#' @export
#'
#' @examples
#' sigma_to_ppm(6)          # ~ 3.4 (with 1.5 sigma shift)
#' sigma_to_ppm(3)          # ~ 66807
#' sigma_to_ppm(4.5, 0)     # ~ 3.4 (no shift)
sigma_to_ppm <- function(sigma, shift = 1.5) {
  if (sigma < 0) stop("Sigma must be non-negative.")
  if (is.infinite(sigma)) return(0)

  # Subtract shift -> short-term Z value
  z_short <- sigma - shift
  # One-sided tail probability -> PPM
  pnorm(-z_short) * 1e6
}

#' @title Convert Yield to DPMO
#' @description
#' Converts first-pass yield to DPMO.
#'
#' @param yield First-pass yield (decimal between 0~1, or percentage)
#' @param opportunities Opportunity count per unit (default 1)
#'
#' @return Numeric, DPMO
#' @export
#'
#' @examples
#' yield_to_dpmo(0.95)              # 50000
#' yield_to_dpmo(99, opportunities = 10)  # Input percentage
yield_to_dpmo <- function(yield, opportunities = 1) {
  # Auto-detect percentage
  if (yield > 1) yield <- yield / 100
  if (yield < 0 || yield > 1) stop("Yield must be between 0 and 1 (or 0~100).")
  if (opportunities < 1) stop("Opportunities must be >= 1.")

  dpu <- -log(yield)  # via exponential distribution relationship
  dpmo <- (dpu / opportunities) * 1e6
  dpmo
}

#' @title Convert DPMO to Yield
#' @description
#' Converts DPMO to first-pass yield.
#'
#' @param dpmo Defects per million opportunities
#' @param opportunities Opportunity count per unit (default 1)
#'
#' @return Numeric, Yield (between 0~1)
#' @export
#'
#' @examples
#' dpmo_to_yield(50000)             # ~ 0.95
#' dpmo_to_yield(3.4, opportunities = 10)  # ~ 0.99999966
dpmo_to_yield <- function(dpmo, opportunities = 1) {
  if (dpmo < 0) stop("DPMO must be non-negative.")
  if (opportunities < 1) stop("Opportunities must be >= 1.")

  dpu <- (dpmo / 1e6) * opportunities
  exp(-dpu)
}


# =============================================================================
# 3. Z.Bench calculation
# =============================================================================

#' @title Z.Bench calculation (comprehensive Sigma level)
#' @description
#' Calculates Z.Bench value (comprehensive process Sigma level) from actual non-conforming rate.
#' Z.Bench is a commonly used comprehensive process capability metric in six sigma.
#'
#' @param p_total Total non-conforming rate (decimal between 0~1)
#' @param shift Long-term shift (default 1.5)
#'
#' @return Numeric, Z.Bench
#' @export
#'
#' @examples
#' z_bench(0.001)       # Non-conforming rate 0.1%
#' z_bench(0.066807)    # Non-conforming rate 6.68%
z_bench <- function(p_total, shift = 1.5) {
  if (p_total < 0 || p_total > 1) stop("p_total must be between 0 and 1.")
  if (p_total == 0) return(Inf)
  if (p_total == 1) return(0)

  z <- qnorm(1 - p_total)
  z + shift
}


# =============================================================================
# 4. Throughput Yield (Throughput Yield / Rolled Throughput Yield)
# =============================================================================

#' @title Throughput yield calculation
#' @description
#' Calculates throughput yield (Rolled Throughput Yield, RTY) for multi-process.
#' RTY = e^(-DPU_1) * e^(-DPU_2) * ... * e^(-DPU_n)
#'
#' @param dpu DPU values for each process (numeric vector)
#' @param yield First-pass yield for each process (choose one with dpu)
#'
#' @return Numeric, RTY (between 0~1)
#' @export
#'
#' @examples
#' # Three processes, DPU = 0.01, 0.02, 0.015
#' throughput_yield(dpu = c(0.01, 0.02, 0.015))
#'
#' # Or use process yields
#' throughput_yield(yield = c(0.99, 0.98, 0.985))
throughput_yield <- function(dpu = NULL, yield = NULL) {
  if (!is.null(dpu) && !is.null(yield)) {
    stop("Provide either dpu or yield, not both.")
  }

  if (!is.null(dpu)) {
    if (any(dpu < 0)) stop("DPU must be non-negative.")
    prod(exp(-dpu))
  } else if (!is.null(yield)) {
    if (any(yield < 0 | yield > 1)) stop("Yield must be between 0 and 1.")
    prod(yield)
  } else {
    stop("Provide either dpu or yield.")
  }
}


# =============================================================================
# 5. Reliability metrics
# =============================================================================

#' @title Reliability calculation
#' @description
#' Calculates reliability at specified time from failure rate or MTBF.
#' Default exponential distribution assumption: R(t) = e^(-lambda*t) = e^(-t/MTBF)
#'
#' @param t Mission time
#' @param lambda Failure rate (choose one with mtbf)
#' @param mtbf Mean time between failures (choose one with lambda)
#' @param dist Distribution type ("exp" exponential, "weibull" Weibull)
#' @param shape Weibull shape parameter (required when dist = "weibull")
#' @param scale Weibull scale parameter (required when dist = "weibull")
#'
#' @return Numeric, reliability R(t) (between 0~1)
#' @export
#'
#' @examples
#' # Exponential distribution: MTBF = 1000 hours, find R(t) at t = 100 hours
#' reliability(t = 100, mtbf = 1000)
#'
#' # Weibull distribution
#' reliability(t = 100, shape = 2, scale = 1000, dist = "weibull")
reliability <- function(t, lambda = NULL, mtbf = NULL,
                         dist = c("exp", "weibull"),
                         shape = NULL, scale = NULL) {
  dist <- match.arg(dist)

  if (dist == "exp") {
    if (!is.null(lambda)) {
      exp(-lambda * t)
    } else if (!is.null(mtbf)) {
      exp(-t / mtbf)
    } else {
      stop("Provide either lambda or mtbf for exponential distribution.")
    }
  } else {
    # Weibull
    if (is.null(shape) || is.null(scale)) {
      stop("shape and scale are required for Weibull distribution.")
    }
    exp(-(t / scale)^shape)
  }
}

#' @title Availability calculation
#' @description
#' Calculates system availability: A = MTBF / (MTBF + MTTR)
#'
#' @param mtbf Mean time between failures
#' @param mttr Mean time to repair
#'
#' @return Numeric, availability (between 0~1)
#' @export
#'
#' @examples
#' availability(mtbf = 1000, mttr = 10)   # ~ 0.99
availability <- function(mtbf, mttr) {
  if (mtbf < 0 || mttr < 0) stop("MTBF and MTTR must be non-negative.")
  if (mtbf + mttr == 0) return(1)
  mtbf / (mtbf + mttr)
}


# =============================================================================
# 6. Process capability interpretation
# =============================================================================

#' @title Process capability index level interpretation
#' @description
#' Returns capability level and plain language interpretation based on Cpk value.
#'
#' @param cpk Process capability index
#' @param standard Evaluation standard ("auto" automatic, "six_sigma" six sigma, "automotive" automotive, "custom" custom)
#' @param custom_thresholds Custom threshold list (only used when standard = "custom")
#'
#' @return List containing level (level), color (color), interpretation (interpretation)
#' @export
#'
#' @examples
#' capability_interpret(1.67)   # Excellent
#' capability_interpret(1.33)   # Good
#' capability_interpret(1.0)    # Marginal
#' capability_interpret(0.67)   # Unacceptable
capability_interpret <- function(cpk, standard = c("auto", "six_sigma", "automotive", "custom"),
                                  custom_thresholds = NULL) {
  standard <- match.arg(standard)

  thresholds <- switch(standard,
    "six_sigma" = list(
      excellent = 2.0,
      good = 1.67,
      acceptable = 1.33,
      marginal = 1.0
    ),
    "automotive" = list(
      excellent = 1.67,
      good = 1.33,
      acceptable = 1.0,
      marginal = 0.67
    ),
    "custom" = {
      if (is.null(custom_thresholds)) {
        stop("custom_thresholds is required when standard = 'custom'.")
      }
      custom_thresholds
    },
    # auto: automatically select based on cpk range
    {
      if (cpk >= 1.67) {
        list(excellent = 2.0, good = 1.67, acceptable = 1.33, marginal = 1.0)
      } else {
        list(excellent = 1.67, good = 1.33, acceptable = 1.0, marginal = 0.67)
      }
    }
  )

  if (cpk >= thresholds$excellent) {
    level <- "Excellent"
    color <- "green"
    interpretation <- "Process capability is excellent, far exceeding requirements."
  } else if (cpk >= thresholds$good) {
    level <- "Good"
    color <- "blue"
    interpretation <- "Process capability is good, meeting most industry standards."
  } else if (cpk >= thresholds$acceptable) {
    level <- "Acceptable"
    color <- "yellow"
    interpretation <- "Process capability is acceptable, but continuous monitoring is recommended."
  } else if (cpk >= thresholds$marginal) {
    level <- "Marginal"
    color <- "orange"
    interpretation <- "Process capability is marginal, improvement needed."
  } else {
    level <- "Unacceptable"
    color <- "red"
    interpretation <- "Process capability is unacceptable, corrective action must be taken."
  }

  list(
    cpk = cpk,
    level = level,
    color = color,
    interpretation = interpretation,
    standard = standard
  )
}


# =============================================================================
# 7. Industry benchmark comparison
# =============================================================================

#' @title Quality metric industry benchmark comparison
#' @description
#' Compares calculated quality metrics with industry benchmarks to evaluate position.
#'
#' @param metric Metric name ("cpk", "ppm", "yield", "availability", "defect_rate")
#' @param value Metric value
#' @param industry Industry ("manufacturing", "electronics", "automotive", "aerospace",
#'   "software", "healthcare", "service", "semiconductor")
#'
#' @return List containing benchmark (benchmark value), rating (rating), percentile (percentile)
#' @export
#'
#' @examples
#' benchmark_compare("cpk", 1.33, "automotive")
#' benchmark_compare("ppm", 500, "electronics")
benchmark_compare <- function(metric, value, industry = c("manufacturing", "electronics",
                                                            "automotive", "aerospace",
                                                            "software", "healthcare",
                                                            "service", "semiconductor")) {
  industry <- match.arg(industry)

  # Industry benchmark database
  benchmarks <- list(
    cpk = list(
      manufacturing = list(world_class = 2.0, industry_avg = 1.33, minimum = 1.0),
      electronics = list(world_class = 2.0, industry_avg = 1.5, minimum = 1.33),
      automotive = list(world_class = 1.67, industry_avg = 1.33, minimum = 1.0),
      aerospace = list(world_class = 2.0, industry_avg = 1.67, minimum = 1.33),
      semiconductor = list(world_class = 2.0, industry_avg = 1.67, minimum = 1.33)
    ),
    ppm = list(
      manufacturing = list(world_class = 10, industry_avg = 1000, minimum = 5000),
      electronics = list(world_class = 50, industry_avg = 500, minimum = 2000),
      automotive = list(world_class = 10, industry_avg = 100, minimum = 500),
      aerospace = list(world_class = 1, industry_avg = 10, minimum = 100),
      semiconductor = list(world_class = 1, industry_avg = 50, minimum = 200)
    ),
    yield = list(
      manufacturing = list(world_class = 0.999, industry_avg = 0.95, minimum = 0.90),
      electronics = list(world_class = 0.995, industry_avg = 0.97, minimum = 0.93),
      automotive = list(world_class = 0.999, industry_avg = 0.98, minimum = 0.95),
      semiconductor = list(world_class = 0.95, industry_avg = 0.85, minimum = 0.75)
    ),
    availability = list(
      manufacturing = list(world_class = 0.99, industry_avg = 0.95, minimum = 0.90),
      service = list(world_class = 0.999, industry_avg = 0.99, minimum = 0.95),
      healthcare = list(world_class = 0.999, industry_avg = 0.995, minimum = 0.99)
    ),
    defect_rate = list(
      software = list(world_class = 0.001, industry_avg = 0.01, minimum = 0.05),
      manufacturing = list(world_class = 0.001, industry_avg = 0.01, minimum = 0.05),
      electronics = list(world_class = 0.0005, industry_avg = 0.005, minimum = 0.02)
    )
  )

  # Find benchmark
  if (!metric %in% names(benchmarks)) {
    stop(sprintf("Unknown metric: %s. Available: %s",
                 metric, paste(names(benchmarks), collapse = ", ")))
  }

  if (!industry %in% names(benchmarks[[metric]])) {
    stop(sprintf("No benchmark for metric '%s' in industry '%s'.", metric, industry))
  }

  bench <- benchmarks[[metric]][[industry]]

  # Determine rating (PPM and defect_rate: lower is better, others: higher is better)
  lower_is_better <- metric %in% c("ppm", "defect_rate")

  if (lower_is_better) {
    if (value <= bench$world_class) {
      rating <- "World Class"
      percentile <- 95
    } else if (value <= bench$industry_avg) {
      rating <- "Above Average"
      percentile <- 75
    } else if (value <= bench$minimum) {
      rating <- "Acceptable"
      percentile <- 50
    } else {
      rating <- "Below Minimum"
      percentile <- 25
    }
  } else {
    if (value >= bench$world_class) {
      rating <- "World Class"
      percentile <- 95
    } else if (value >= bench$industry_avg) {
      rating <- "Above Average"
      percentile <- 75
    } else if (value >= bench$minimum) {
      rating <- "Acceptable"
      percentile <- 50
    } else {
      rating <- "Below Minimum"
      percentile <- 25
    }
  }

  list(
    metric = metric,
    value = value,
    industry = industry,
    benchmark = bench,
    rating = rating,
    percentile = percentile
  )
}


# =============================================================================
# 8. Comprehensive quality dashboard
# =============================================================================

#' @title Comprehensive quality metric dashboard
#' @description
#' One-click output of multiple key quality metric calculation results and interpretations.
#'
#' @param cpk Process capability index (optional)
#' @param ppm Non-conforming rate PPM (optional)
#' @param yield_val First-pass yield (optional)
#' @param dpmo DPMO value (optional)
#' @param availability Availability (optional)
#' @param industry Industry (for benchmark comparison)
#'
#' @return List containing calculation results, interpretations and benchmark comparisons for each metric
#' @export
#'
#' @examples
#' quality_dashboard(cpk = 1.33, ppm = 500, industry = "automotive")
quality_dashboard <- function(cpk = NULL, ppm = NULL, yield_val = NULL,
                               dpmo = NULL, availability = NULL,
                               industry = "manufacturing") {
  result <- list()

  if (!is.null(cpk)) {
    result$cpk <- list(
      value = cpk,
      interpretation = capability_interpret(cpk),
      benchmark = tryCatch(benchmark_compare("cpk", cpk, industry), error = function(e) NULL)
    )
    # Auto-calculate expected PPM
    result$cpm_from_cpk <- capability_to_ppm(cpk = cpk, usl = 1, lsl = 0)
  }

  if (!is.null(ppm)) {
    result$ppm <- list(
      value = ppm,
      sigma_level = ppm_to_sigma(ppm),
      benchmark = tryCatch(benchmark_compare("ppm", ppm, industry), error = function(e) NULL)
    )
  }

  if (!is.null(yield_val)) {
    result$yield <- list(
      value = yield_val,
      dpmo = yield_to_dpmo(yield_val),
      benchmark = tryCatch(benchmark_compare("yield", yield_val, industry), error = function(e) NULL)
    )
  }

  if (!is.null(dpmo)) {
    result$dpmo <- list(
      value = dpmo,
      yield = dpmo_to_yield(dpmo),
      sigma_level = ppm_to_sigma(dpmo)
    )
  }

  if (!is.null(availability)) {
    result$availability <- list(
      value = availability,
      benchmark = tryCatch(benchmark_compare("availability", availability, industry), error = function(e) NULL)
    )
  }

  # Print summary
  cat("\n")
  cat("============================================\n")
  cat("         Comprehensive Quality Dashboard\n")
  cat("============================================\n")
  cat(sprintf("  Industry: %s\n", industry))
  cat("--------------------------------------------\n")

  if (!is.null(result$cpk)) {
    cat(sprintf("  Cpk: %.2f -> %s (%s)\n",
                cpk, result$cpk$interpretation$level,
                result$cpk$interpretation$interpretation))
  }
  if (!is.null(result$ppm)) {
    cat(sprintf("  PPM: %.0f -> Sigma Level = %.2f\n", ppm, result$ppm$sigma_level))
  }
  if (!is.null(result$yield)) {
    cat(sprintf("  Yield: %.2f%% -> DPMO = %.0f\n", yield_val * 100, result$yield$dpmo))
  }
  if (!is.null(result$dpmo)) {
    cat(sprintf("  DPMO: %.0f -> Yield = %.2f%%, Sigma = %.2f\n",
                dpmo, result$dpmo$yield * 100, result$dpmo$sigma_level))
  }
  if (!is.null(result$availability)) {
    cat(sprintf("  Availability: %.2f%%\n", availability * 100))
  }

  cat("============================================\n\n")

  invisible(result)
}
