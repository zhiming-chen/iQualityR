# =============================================================================
# File: R/Capability_attribute.R
# Description: Attribute (Binomial / Poisson) process capability analysis.
#              Implements Minitab-style attribute capability with p/np/u/c
#              charts, PPM/DPMO, Z.Bench / Sigma level, and Wilson /
#              Clopper-Pearson confidence intervals. Reuses iQualityR.stat
#              for z_bench / capability_interpret / capability_to_ppm-style
#              verdicts and iQualityR.plot for control-chart layers.
# =============================================================================

# ---- helpers ---------------------------------------------------------------

# Wilson score interval for a binomial proportion. Closed form, good coverage
# even for small n / extreme p. Used as the default CI for p-bar.
.wilson_ci <- function(x, n, conf_level = 0.95) {
  z <- stats::qnorm((1 + conf_level) / 2)
  p_hat <- x / n
  denom <- 1 + z^2 / n
  center <- (p_hat + z^2 / (2 * n)) / denom
  half   <- (z * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))) / denom
  list(estimate = p_hat,
       lower = max(0, center - half),
       upper = min(1, center + half))
}

# Clopper-Pearson "exact" interval. More conservative than Wilson; uses the
# F distribution. Computed via base stats::binom.test for robustness.
.clopper_pearson_ci <- function(x, n, conf_level = 0.95) {
  bt <- stats::binom.test(x, n, conf.level = conf_level)
  list(estimate = unname(bt$estimate),
       lower = bt$conf.int[1],
       upper = bt$conf.int[2])
}

# Exact (Garwood) interval for a Poisson rate. Conservative, based on the
# chi-square distribution. Wrapped via stats::poisson.test.
.poisson_ci <- function(x, t = 1, conf_level = 0.95) {
  # x = total count, t = total exposure (sum of opportunities / units).
  pt <- stats::poisson.test(x, T = t, conf.level = conf_level)
  list(estimate = unname(pt$estimate),
       lower = pt$conf.int[1],
       upper = pt$conf.int[2])
}

# Poisson dispersion test (variance/mean ratio). Under H0 (Poisson),
# (n-1) * sample_var / mean ~ chi-square with n-1 df. Used to detect
# over-dispersion, which would make the binomial/Poisson model invalid.
.poisson_dispersion_test <- function(counts) {
  n <- length(counts)
  if (n < 2) return(list(statistic = NA_real_, p_value = NA_real_,
                         method = "Poisson dispersion (n<2)"))
  m <- mean(counts)
  v <- stats::var(counts)
  if (m <= 0) return(list(statistic = NA_real_, p_value = NA_real_,
                          method = "Poisson dispersion"))
  D <- (n - 1) * v / m
  p <- 1 - stats::pchisq(D, df = n - 1)
  list(statistic = D, p_value = p, method = "Poisson dispersion (chi-square)")
}

# Binomial dispersion test (variance/mean ratio on per-subgroup proportions).
# Uses the binomial-overdispersion statistic B = sum((x_i - n_i*p)^2 / (n_i*p*(1-p)))
# ~ chi-square with k-1 df under H0.
.binomial_dispersion_test <- function(counts, sample_sizes, p_bar) {
  k <- length(counts)
  if (k < 2 || p_bar <= 0 || p_bar >= 1) {
    return(list(statistic = NA_real_, p_value = NA_real_,
                method = "Binomial dispersion (degenerate)"))
  }
  expected <- sample_sizes * p_bar
  var_binom <- sample_sizes * p_bar * (1 - p_bar)
  var_binom[var_binom <= 0] <- NA_real_
  B <- sum((counts - expected)^2 / var_binom, na.rm = TRUE)
  p <- 1 - stats::pchisq(B, df = k - 1)
  list(statistic = B, p_value = p, method = "Binomial dispersion (chi-square)")
}

# Z.Bench for attribute data: the sigma quality level implied by a total
# defect probability p_total. Reuses iQualityR.stat::z_bench with shift=0
# (short-term convention; expose shift via plan if needed).
.attr_z_bench <- function(p_total, shift = 0) {
  if (is.na(p_total) || p_total <= 0) return(Inf)
  if (p_total >= 1) return(-Inf)
  iQualityR.stat::z_bench(p_total, shift = shift)
}

# =============================================================================
# AttributeCapabilityAnalyzer
# =============================================================================

#' AttributeCapabilityAnalyzer
#'
#' @title AttributeCapabilityAnalyzer
#'
#' @description Attribute (Binomial / Poisson) process capability analyzer.
#'   Inherits `IqrAnalyzerBase` and computes the standard Minitab-style
#'   attribute capability outputs: rate estimate, CI, PPM/DPMO, Z.Bench /
#'   sigma level, control chart for stability, and dispersion GOF test.
#'
#'   Two distributions are supported via `plan$analysis_type`:
#'   * `"binomial"` — for defectives / pass-fail data. Rate `p` = proportion
#'     defective; PPM = p * 1e6; uses p-chart limits.
#'   * `"poisson"` — for defect counts. Rate `lambda` = defects per unit;
#'     DPMO = lambda * 1e6; uses u-chart limits.
#'
#' @field results Standardized result container (inherited).
#'
#' @param counts Integer vector of defectives (binomial) or defects (Poisson).
#' @param sample_sizes Integer vector aligned with `counts`. For binomial
#'   this is the subgroup size `n_i`; for Poisson it is the exposure
#'   (opportunities / units inspected) per subgroup.
#' @param plan An `AttributeCapabilityPlan` object.
#'
#' @export
AttributeCapabilityAnalyzer <- R6::R6Class("AttributeCapabilityAnalyzer",
  inherit = IqrAnalyzerBase,
  public = list(

    #' @description Run attribute capability analysis
    #' @param counts Integer vector of defectives (binomial) or defects (Poisson).
    #' @param sample_sizes Integer vector of subgroup sizes / exposures.
    #' @param plan [AttributeCapabilityPlan] object.
    run = function(counts, sample_sizes, plan) {
      if (length(counts) != length(sample_sizes)) {
        stop("counts and sample_sizes must have the same length.", call. = FALSE)
      }
      if (length(counts) < 1) stop("Need at least one subgroup.", call. = FALSE)
      if (any(counts < 0, na.rm = TRUE)) stop("counts must be non-negative.", call. = FALSE)
      if (any(sample_sizes <= 0, na.rm = TRUE)) {
        stop("sample_sizes must be positive.", call. = FALSE)
      }
      if (plan$analysis_type == "binomial" &&
          any(counts > sample_sizes, na.rm = TRUE)) {
        stop("For binomial analysis, counts must not exceed sample_sizes.",
             call. = FALSE)
      }

      # NA-masking
      keep <- !is.na(counts) & !is.na(sample_sizes)
      counts <- as.integer(counts[keep])
      sample_sizes <- as.integer(sample_sizes[keep])

      if (plan$analysis_type == "poisson") {
        self$.run_poisson(counts, sample_sizes, plan)
      } else {
        self$.run_binomial(counts, sample_sizes, plan)
      }
      invisible(self)
    },

    #' @description Binomial capability path
    #' @param counts Integer vector of defectives per subgroup.
    #' @param sample_sizes Integer vector of subgroup sizes.
    #' @param plan [AttributeCapabilityPlan] object.
    .run_binomial = function(counts, sample_sizes, plan) {
      k <- length(counts)
      total_defectives <- sum(counts)
      total_inspected <- sum(sample_sizes)
      p_bar <- total_defectives / total_inspected

      # CI for p (Wilson by default; Clopper-Pearson if requested)
      ci_method <- match.arg(plan$ci_method %||% "wilson",
                             c("wilson", "clopper_pearson"))
      if (ci_method == "clopper_pearson") {
        ci <- .clopper_pearson_ci(total_defectives, total_inspected,
                                  conf_level = plan$conf_level)
      } else {
        ci <- .wilson_ci(total_defectives, total_inspected,
                         conf_level = plan$conf_level)
      }

      # Per-subgroup p-chart limits (variable-width: 3-sigma around p_bar)
      p_i <- counts / sample_sizes
      sigma_i <- sqrt(p_bar * (1 - p_bar) / sample_sizes)
      ucl_i <- pmin(1, p_bar + 3 * sigma_i)
      lcl_i <- pmax(0, p_bar - 3 * sigma_i)
      ooc_i <- p_i > ucl_i | p_i < lcl_i

      # Dispersion / over-dispersion test (binomial assumption check)
      disp <- .binomial_dispersion_test(counts, sample_sizes, p_bar)

      # PPM = p * 1e6 (defective parts per million)
      ppm_expected <- p_bar * 1e6
      ppm_observed <- (sum(counts > 0) / k) * 1e6  # subgroups with any defect

      # Yield and sigma level. Convention matches capability_normal:
      #   z_bench (short-term) = qnorm(1 - p_total)  [no shift]
      #   sigma_level (long-term) = z_bench + shift
      yield <- 1 - p_bar
      z_bench <- .attr_z_bench(p_bar, shift = 0)
      sigma_level <- z_bench + plan$z_shift

      # Target / spec check (optional: plan$target_proportion)
      verdict <- self$.judge_attribute(
        p_bar = p_bar, z_bench = z_bench,
        target_proportion = plan$target_proportion
      )

      # Assemble standardized result container
      self$reset()
      self$set_statistic("distribution", "binomial")
      self$set_statistic("rate_name", "p (proportion defective)")
      self$set_statistic("rate", p_bar)
      self$set_statistic("rate_lower", ci$lower)
      self$set_statistic("rate_upper", ci$upper)
      self$set_statistic("ci_method", ci_method)
      self$set_statistic("total_defectives", total_defectives)
      self$set_statistic("total_inspected", total_inspected)
      self$set_statistic("n_subgroups", k)
      self$set_statistic("yield", yield)
      self$set_statistic("ppm_expected", ppm_expected)
      self$set_statistic("ppm_observed", ppm_observed)
      self$set_statistic("z_bench", z_bench)
      self$set_statistic("sigma_level", sigma_level)
      self$set_statistic("z_shift", plan$z_shift)
      if (!is.null(plan$target_proportion)) {
        self$set_statistic("target_proportion", plan$target_proportion)
      }
      self$set_diagnostic("dispersion_test", disp)
      self$set_diagnostic("capability_judgment", verdict)

      warnings <- character()
      if (any(ooc_i)) {
        warnings <- c(warnings,
          sprintf("p-chart: %d of %d subgroups out of control limits.",
                  sum(ooc_i), k))
      }
      if (!is.na(disp$p_value) && disp$p_value < 0.05) {
        warnings <- c(warnings,
          sprintf("Binomial dispersion test p = %.4f < 0.05: data is over- or under-dispersed; the binomial model may be invalid. Consider Laney P' or a beta-binomial model.",
                  disp$p_value))
      }
      if (k < 25) {
        warnings <- c(warnings,
          sprintf("Only %d subgroups: control limits and rate estimate are unstable; aim for k >= 25.", k))
      }
      if (length(warnings) > 0) self$set_diagnostic("warnings", warnings)

      # Data tables (consumed by plotter + reporter)
      points_df <- data.frame(
        index = seq_len(k),
        value = p_i,
        cl = p_bar,
        ucl = ucl_i,
        lcl = lcl_i,
        n = sample_sizes,
        defects = counts,
        ooc = ooc_i,
        stringsAsFactors = FALSE
      )
      self$set_datatable("points", points_df)
      self$set_datatable("ppm_summary", data.frame(
        Metric = c("Defectives (observed)", "Inspected (total)",
                   "Proportion defective (p-bar)",
                   sprintf("PPM expected (%.0f%% CI)", 100 * plan$conf_level),
                   "PPM observed (subgroups with defects)",
                   "Yield", "Z.Bench (short-term)",
                   sprintf("Sigma level (+ %.1f shift)", plan$z_shift)),
        Value = c(total_defectives, total_inspected,
                  formatC(p_bar, format = "f", digits = 6),
                  sprintf("%.2f (%.2f, %.2f)", ppm_expected,
                          ci$lower * 1e6, ci$upper * 1e6),
                  formatC(ppm_observed, format = "f", digits = 2),
                  formatC(yield, format = "f", digits = 6),
                  formatC(z_bench, format = "f", digits = 4),
                  formatC(sigma_level, format = "f", digits = 4)),
        stringsAsFactors = FALSE
      ))
      self$set_datatable("raw_data", data.frame(
        subgroup = seq_len(k), defects = counts,
        sample_size = sample_sizes, proportion = p_i,
        stringsAsFactors = FALSE
      ))

      invisible(self)
    },

    #' @description Poisson capability path
    #' @param counts Integer vector of defects per subgroup.
    #' @param sample_sizes Integer vector of exposures (units/opportunities).
    #' @param plan [AttributeCapabilityPlan] object.
    .run_poisson = function(counts, sample_sizes, plan) {
      k <- length(counts)
      total_defects <- sum(counts)
      total_exposure <- sum(sample_sizes)
      u_bar <- total_defects / total_exposure  # defects per unit

      # Exact (Garwood) CI for the Poisson rate lambda = u_bar
      ci <- .poisson_ci(total_defects, t = total_exposure,
                        conf_level = plan$conf_level)

      # Per-subgroup u-chart limits (variable-width: 3-sigma around u_bar)
      u_i <- counts / sample_sizes
      sigma_i <- sqrt(u_bar / sample_sizes)
      ucl_i <- u_bar + 3 * sigma_i
      lcl_i <- pmax(0, u_bar - 3 * sigma_i)
      ooc_i <- u_i > ucl_i | u_i < lcl_i

      # Dispersion / over-dispersion test
      disp <- .poisson_dispersion_test(counts)

      # DPMO = u_bar * 1e6 (defects per million opportunities)
      dpmo_expected <- u_bar * 1e6
      dpmo_observed <- (sum(counts > 0) / k) * 1e6

      # Yield and sigma level
      # For Poisson the "defect probability" is 1 - P(X=0) = 1 - exp(-lambda)
      # for a single unit; use this as the per-unit defect probability.
      p_total <- 1 - exp(-u_bar)
      yield <- 1 - p_total
      # Convention: z_bench (short-term) without shift; sigma_level with shift
      z_bench <- .attr_z_bench(p_total, shift = 0)
      sigma_level <- z_bench + plan$z_shift

      verdict <- self$.judge_attribute(
        p_bar = u_bar, z_bench = z_bench,
        target_proportion = plan$target_proportion
      )

      # Assemble standardized result container
      self$reset()
      self$set_statistic("distribution", "poisson")
      self$set_statistic("rate_name", "u (defects per unit)")
      self$set_statistic("rate", u_bar)
      self$set_statistic("rate_lower", ci$lower)
      self$set_statistic("rate_upper", ci$upper)
      self$set_statistic("ci_method", "garwood_exact")
      self$set_statistic("total_defects", total_defects)
      self$set_statistic("total_exposure", total_exposure)
      self$set_statistic("n_subgroups", k)
      self$set_statistic("yield", yield)
      self$set_statistic("dpmo_expected", dpmo_expected)
      self$set_statistic("dpmo_observed", dpmo_observed)
      self$set_statistic("ppm_expected", p_total * 1e6)  # per-unit defect PPM
      self$set_statistic("z_bench", z_bench)
      self$set_statistic("sigma_level", sigma_level)
      self$set_statistic("z_shift", plan$z_shift)
      if (!is.null(plan$target_proportion)) {
        self$set_statistic("target_rate", plan$target_proportion)
      }
      self$set_diagnostic("dispersion_test", disp)
      self$set_diagnostic("capability_judgment", verdict)

      warnings <- character()
      if (any(ooc_i)) {
        warnings <- c(warnings,
          sprintf("u-chart: %d of %d subgroups out of control limits.",
                  sum(ooc_i), k))
      }
      if (!is.na(disp$p_value) && disp$p_value < 0.05) {
        warnings <- c(warnings,
          sprintf("Poisson dispersion test p = %.4f < 0.05: data is over-dispersed (variance > mean); the Poisson model is invalid. Consider Laney U' or a negative-binomial model.",
                  disp$p_value))
      }
      if (k < 25) {
        warnings <- c(warnings,
          sprintf("Only %d subgroups: control limits and rate estimate are unstable; aim for k >= 25.", k))
      }
      if (length(warnings) > 0) self$set_diagnostic("warnings", warnings)

      points_df <- data.frame(
        index = seq_len(k),
        value = u_i,
        cl = u_bar,
        ucl = ucl_i,
        lcl = lcl_i,
        n = sample_sizes,
        defects = counts,
        ooc = ooc_i,
        stringsAsFactors = FALSE
      )
      self$set_datatable("points", points_df)
      self$set_datatable("ppm_summary", data.frame(
        Metric = c("Defects (observed)", "Exposure (total units)",
                   "Defects per unit (u-bar)",
                   sprintf("DPMO expected (%.0f%% CI)", 100 * plan$conf_level),
                   "DPMO observed (subgroups with defects)",
                   "Yield (1 - P(at least 1 defect on 1 unit))",
                   "Z.Bench (short-term)",
                   sprintf("Sigma level (+ %.1f shift)", plan$z_shift)),
        Value = c(total_defects, total_exposure,
                  formatC(u_bar, format = "f", digits = 6),
                  sprintf("%.2f (%.2f, %.2f)", dpmo_expected,
                          ci$lower * 1e6, ci$upper * 1e6),
                  formatC(dpmo_observed, format = "f", digits = 2),
                  formatC(yield, format = "f", digits = 6),
                  formatC(z_bench, format = "f", digits = 4),
                  formatC(sigma_level, format = "f", digits = 4)),
        stringsAsFactors = FALSE
      ))
      self$set_datatable("raw_data", data.frame(
        subgroup = seq_len(k), defects = counts,
        exposure = sample_sizes, rate = u_i,
        stringsAsFactors = FALSE
      ))

      invisible(self)
    },

    #' @description Verdict logic for attribute capability
    #' @param p_bar Estimated rate (proportion or rate per unit)
    #' @param z_bench Z.Bench value
    #' @param target_proportion Optional target rate
    #' @return List with status strings
    .judge_attribute = function(p_bar, z_bench, target_proportion = NULL) {
      # Sigma-level thresholds (Six Sigma convention with shift)
      # >= 6 sigma: world-class; 4-6: acceptable; 3-4: marginal; <3: unacceptable
      sigma_status <- if (z_bench >= 4) "pass" else if (z_bench >= 3) "watch" else "fail"
      sigma_label <- if (z_bench >= 4) "acceptable (>= 4 sigma)"
                     else if (z_bench >= 3) "marginal (3-4 sigma)"
                     else "unacceptable (< 3 sigma)"

      target_status <- "n/a"
      target_label  <- "no target specified"
      if (!is.null(target_proportion)) {
        if (p_bar <= target_proportion) {
          target_status <- "pass"
          target_label <- sprintf("rate %.4f <= target %.4f",
                                   p_bar, target_proportion)
        } else {
          target_status <- "fail"
          target_label <- sprintf("rate %.4f > target %.4f",
                                   p_bar, target_proportion)
        }
      }

      overall <- if (sigma_status == "fail" || target_status == "fail") "fail"
                 else if (sigma_status == "watch" || target_status == "watch") "watch"
                 else "pass"

      list(
        sigma_status = sigma_status,
        sigma_label  = sigma_label,
        target_status = target_status,
        target_label  = target_label,
        overall_verdict = overall
      )
    }
  )
)

# null-coalesce helper (R has no built-in)
`%||%` <- function(a, b) if (is.null(a)) b else a
