# =============================================================================
# File: R/CapabilityAnalyzer.R
# Description: Capability analysis core engine (inherits IqrAnalyzerBase)
# =============================================================================

#' @title CapabilityAnalyzer
#' @description
#' Analyzer for process capability under normal, non-normal, and non-parametric
#' assumptions. Inherits `IqrAnalyzerBase` and implements `private$.run_logic()`
#' per the framework contract. Reuses `iQualityR.stat` for sigma estimation,
#' normality testing, capability interpretation, and PPM conversion.
#'
#' @field results Standardized result container (inherited).
#' @field params Parameter list (inherited).
#'
#' @param x Numeric vector of measurements.
#' @param subgroup Optional subgroup label vector aligned with `x`.
#' @param plan [CapabilityPlan] object.
#' @param data Data passed by the base class `run()`.
#' @param ... Additional arguments (ignored).
#'
#' @export
CapabilityAnalyzer <- R6::R6Class("CapabilityAnalyzer",
  inherit = IqrAnalyzerBase,
  public = list(

    #' @description Run capability analysis. Overrides base `run()` to accept the
    #' capability-specific `(x, subgroup, plan)` signature while preserving the
    #' reset/dispatch contract.
    #' @param x Numeric vector of measurements.
    #' @param subgroup Optional subgroup label vector.
    #' @param plan [CapabilityPlan] object.
    run = function(x, subgroup = NULL, plan) {
      if (length(x) < 2) stop("Need at least 2 observations.", call. = FALSE)
      if (length(x) < 30) {
        self$set_diagnostic("warning_small_sample",
          sprintf("Sample size = %d (< 30). Capability indices may be unstable.", length(x)))
      }

      # Data cleaning: compute keep mask first, then filter both x and subgroup
      keep <- !is.na(x)
      x <- x[keep]
      if (!is.null(subgroup)) subgroup <- subgroup[keep]

      # Dispatch to the appropriate analysis method
      if (plan$analysis_type == "nonnormal") {
        self$.run_nonnormal(x, plan)
      } else if (plan$analysis_type == "nonparametric") {
        self$.run_nonparametric(x, plan)
      } else {
        self$.run_normal(x, subgroup, plan)
      }

      invisible(self)
    },

    #' @description Normal capability analysis
    #' @param x Numeric vector of measurements.
    #' @param subgroup Optional subgroup label vector.
    #' @param plan [CapabilityPlan] object.
    .run_normal = function(x, subgroup, plan) {
      # Sigma decomposition (reuse iQualityR.stat)
      sigma <- self$.estimate_sigma(x, subgroup)

      # Capability indices
      indices <- self$.compute_indices(x, sigma, plan)

      # PPM (reuse iQualityR.stat::capability_to_ppm)
      ppm <- self$.compute_ppm(x, sigma, plan, indices)

      # Normality diagnostics (reuse iQualityR.stat::normality_test)
      diagnostics <- self$.diagnose(x)

      # Sixpack data
      sixpack_data <- self$.prepare_sixpack_data(x, subgroup, sigma, indices, plan)

      # Bootstrap CI (optional)
      bootstrap_results <- NULL
      if (plan$use_bootstrap && plan$bootstrap_samples > 0) {
        bootstrap_results <- self$.bootstrap_ci(x, subgroup, plan, n_boot = plan$bootstrap_samples)
      }

      # Populate result container
      self$reset()
      self$set_statistic("cp", indices$cp)
      self$set_statistic("cpk", indices$cpk)
      self$set_statistic("cpl", indices$cpl)
      self$set_statistic("cpu", indices$cpu)
      self$set_statistic("pp", indices$pp)
      self$set_statistic("ppk", indices$ppk)
      self$set_statistic("ppl", indices$ppl)
      self$set_statistic("ppu", indices$ppu)
      if (!is.null(indices$cpm)) self$set_statistic("cpm", indices$cpm)
      self$set_statistic("mean", mean(x))
      self$set_statistic("sd_within", sigma$within)
      self$set_statistic("sd_overall", sigma$overall)
      self$set_statistic("n", length(x))
      if (!is.null(subgroup)) {
        self$set_statistic("n_subgroups", length(unique(subgroup)))
        self$set_statistic("subgroup_sizes", table(subgroup))
      }

      self$set_diagnostic("normality_p_value", diagnostics$normality_p_value)
      self$set_diagnostic("normality_method", diagnostics$method)
      if (length(diagnostics$warnings) > 0) {
        self$set_diagnostic("warnings", diagnostics$warnings)
      }

      self$set_datatable("ppm_within", data.frame(
        Direction = c("Below LSL", "Above USL", "Total"),
        PPM = c(ppm$within$below_lsl, ppm$within$above_usl, ppm$within$total)
      ))
      self$set_datatable("ppm_overall", data.frame(
        Direction = c("Below LSL", "Above USL", "Total"),
        PPM = c(ppm$overall$below_lsl, ppm$overall$above_usl, ppm$overall$total)
      ))
      raw_df <- data.frame(measurement = x)
      if (!is.null(subgroup)) {
        raw_df$subgroup <- subgroup
      }
      self$set_datatable("raw_data", raw_df)

      if (!is.null(bootstrap_results)) {
        self$set_datatable("bootstrap_ci", bootstrap_results$ci_table)
      }

      self$set_datatable("sixpack", sixpack_data)

      # Intelligent judgment (reuse iQualityR.stat::capability_interpret)
      self$set_diagnostic("capability_judgment",
        self$.judge_capability(indices$cpk, indices$ppk, indices$cp, indices$pp, plan))
      self$set_diagnostic("process_advice",
        self$.generate_advice(indices, sigma, plan, diagnostics))

      invisible(self)
    },

    #' @description Non-parametric capability analysis
    #' @param x Numeric vector of measurements.
    #' @param plan [CapabilityPlan] object.
    .run_nonparametric = function(x, plan) {
      # Validate non-parametric method
      valid_methods <- c("kernel", "empirical")
      method <- plan$nonparametric_method
      if (!method %in% valid_methods) {
        warning(sprintf("Invalid nonparametric method: %s. Using 'kernel' instead.", method), call. = FALSE)
        method <- "kernel"
      }

      # Capability indices based on empirical distribution
      indices <- self$.compute_nonparametric_indices(x, method, plan)

      # PPM based on empirical distribution
      ppm <- self$.compute_nonparametric_ppm(x, method, plan)

      # Diagnostics
      diagnostics <- list(
        method = method,
        sample_size = length(x),
        warnings = character(0)
      )
      if (length(x) < 50) {
        diagnostics$warnings <- c(diagnostics$warnings, "Small sample size for nonparametric analysis. Results may be unstable.")
      }

      # Sixpack data
      sixpack_data <- self$.prepare_nonparametric_sixpack_data(x, method, plan)

      # Bootstrap CI
      bootstrap_results <- NULL
      if (plan$use_bootstrap && plan$bootstrap_samples > 0) {
        bootstrap_results <- self$.bootstrap_nonparametric_ci(x, method, plan, n_boot = plan$bootstrap_samples)
      }

      # Populate result container
      self$reset()
      self$set_statistic("cp", indices$cp)
      self$set_statistic("cpk", indices$cpk)
      self$set_statistic("cpl", indices$cpl)
      self$set_statistic("cpu", indices$cpu)
      self$set_statistic("pp", indices$pp)
      self$set_statistic("ppk", indices$ppk)
      self$set_statistic("ppl", indices$ppl)
      self$set_statistic("ppu", indices$ppu)
      if (!is.null(indices$cpm)) self$set_statistic("cpm", indices$cpm)
      self$set_statistic("mean", mean(x))
      self$set_statistic("sd_overall", stats::sd(x))
      self$set_statistic("n", length(x))

      self$set_diagnostic("method", method)
      self$set_diagnostic("sample_size", length(x))
      if (length(diagnostics$warnings) > 0) {
        self$set_diagnostic("warnings", diagnostics$warnings)
      }

      self$set_datatable("ppm_overall", data.frame(
        Direction = c("Below LSL", "Above USL", "Total"),
        PPM = c(ppm$below_lsl, ppm$above_usl, ppm$total)
      ))
      self$set_datatable("raw_data", data.frame(measurement = x))

      if (!is.null(bootstrap_results)) {
        self$set_datatable("bootstrap_ci", bootstrap_results$ci_table)
      }

      self$set_datatable("sixpack", sixpack_data)

      self$set_diagnostic("capability_judgment",
        self$.judge_capability(indices$cpk, indices$ppk, indices$cp, indices$pp, plan))
      self$set_diagnostic("process_advice",
        self$.generate_nonparametric_advice(indices, method, plan))

      invisible(self)
    },

    #' @description Non-normal capability analysis
    #' @param x Numeric vector of measurements.
    #' @param plan [CapabilityPlan] object.
    .run_nonnormal = function(x, plan) {
      # Initialize DistributionFitter
      fitter <- DistributionFitter$new()

      # Distribution fitting (plan$distribution may be NULL; default to "auto")
      dist_choice <- plan$distribution %||% "auto"
      if (dist_choice == "auto") {
        best_fit <- fitter$fit_auto(x)
        dist_name <- fitter$best_distribution
      } else {
        best_fit <- fitter$fit_one(x, dist_choice)
        dist_name <- dist_choice
        if (!best_fit$converged) {
          warning(sprintf("Failed to fit %s distribution. Falling back to auto.", dist_choice), call. = FALSE)
          best_fit <- fitter$fit_auto(x)
          dist_name <- fitter$best_distribution
        }
      }

      # Non-normal capability indices (quantile method)
      indices <- self$.compute_nonnormal_indices(x, dist_name, best_fit$params, plan, fitter)

      # PPM based on CDF
      ppm <- self$.compute_nonnormal_ppm(x, dist_name, best_fit$params, plan, fitter)

      # Goodness-of-fit diagnostics
      diagnostics <- list(
        distribution = dist_name,
        aic = best_fit$aic,
        bic = best_fit$bic,
        ks_statistic = best_fit$ks_statistic,
        ks_p_value = best_fit$ks_p_value,
        warnings = character(0)
      )
      if (!is.null(best_fit$ks_p_value) && best_fit$ks_p_value < 0.05) {
        diagnostics$warnings <- c(diagnostics$warnings,
          sprintf("Kolmogorov-Smirnov test suggests poor fit (p = %.4f).", best_fit$ks_p_value))
      }

      # Sixpack data
      sixpack_data <- self$.prepare_nonnormal_sixpack_data(x, dist_name, best_fit$params, plan, fitter, indices)

      # Bootstrap CI
      bootstrap_results <- NULL
      if (plan$use_bootstrap && plan$bootstrap_samples > 0) {
        bootstrap_results <- self$.bootstrap_nonnormal_ci(x, dist_name, best_fit$params, plan, fitter, n_boot = plan$bootstrap_samples)
      }

      # Populate result container
      self$reset()
      self$set_statistic("cp", indices$cp)
      self$set_statistic("cpk", indices$cpk)
      self$set_statistic("cpl", indices$cpl)
      self$set_statistic("cpu", indices$cpu)
      self$set_statistic("pp", indices$pp)
      self$set_statistic("ppk", indices$ppk)
      self$set_statistic("ppl", indices$ppl)
      self$set_statistic("ppu", indices$ppu)
      if (!is.null(indices$cpm)) self$set_statistic("cpm", indices$cpm)
      self$set_statistic("mean", mean(x))
      self$set_statistic("sd_overall", stats::sd(x))
      self$set_statistic("n", length(x))

      self$set_diagnostic("distribution", dist_name)
      self$set_diagnostic("aic", best_fit$aic)
      self$set_diagnostic("bic", best_fit$bic)
      self$set_diagnostic("ks_statistic", best_fit$ks_statistic)
      self$set_diagnostic("ks_p_value", best_fit$ks_p_value)
      if (length(diagnostics$warnings) > 0) {
        self$set_diagnostic("warnings", diagnostics$warnings)
      }

      self$set_datatable("ppm_overall", data.frame(
        Direction = c("Below LSL", "Above USL", "Total"),
        PPM = c(ppm$below_lsl, ppm$above_usl, ppm$total)
      ))
      self$set_datatable("raw_data", data.frame(measurement = x))

      if (!is.null(bootstrap_results)) {
        self$set_datatable("bootstrap_ci", bootstrap_results$ci_table)
      }

      self$set_datatable("sixpack", sixpack_data)

      self$set_diagnostic("capability_judgment",
        self$.judge_capability(indices$cpk, indices$ppk, indices$cp, indices$pp, plan))
      self$set_diagnostic("process_advice",
        self$.generate_nonnormal_advice(indices, dist_name, best_fit, plan))

      invisible(self)
    },

    # ---- Sigma estimation (reuse iQualityR.stat::sigma_decomposition) ----

    .estimate_sigma = function(x, subgroup) {
      if (!is.null(subgroup) && length(unique(subgroup)) > 1) {
        # Within sigma via subgroup decomposition; overall sigma via total
        decomp <- iQualityR.stat::sigma_decomposition(
          x = x,
          subgroup = subgroup,
          within_method = "r_bar",
          between_method = "mssd",
          within_unbiased = TRUE,
          total_unbiased = FALSE
        )
        sigma_within <- decomp$sigma_within
        sigma_overall <- decomp$sigma_total
      } else {
        # No subgroups: use moving range (d2 for n=2)
        mr <- abs(diff(x))
        mr_bar <- mean(mr, na.rm = TRUE)
        d2_n2 <- iQualityR.stat::get_d2(2)
        sigma_within <- mr_bar / d2_n2
        sigma_overall <- stats::sd(x)
      }
      # Guard against zero sigma
      if (!is.finite(sigma_within) || sigma_within <= 0) sigma_within <- stats::sd(x)
      if (!is.finite(sigma_overall) || sigma_overall <= 0) sigma_overall <- stats::sd(x)
      list(within = sigma_within, overall = sigma_overall)
    },

    # ---- Capability indices ----

    .compute_indices = function(x, sigma, plan) {
      tol <- plan$tolerance()
      mu <- mean(x)

      # Within
      cp <- tol / (6 * sigma$within)
      cpu <- (plan$usl - mu) / (3 * sigma$within)
      cpl <- (mu - plan$lsl) / (3 * sigma$within)
      cpk <- min(cpu, cpl)

      # Overall
      pp <- tol / (6 * sigma$overall)
      ppu <- (plan$usl - mu) / (3 * sigma$overall)
      ppl <- (mu - plan$lsl) / (3 * sigma$overall)
      ppk <- min(ppu, ppl)

      # Cpm (Taguchi)
      cpm <- NULL
      if (!is.null(plan$target)) {
        tau <- sqrt(mean((x - plan$target)^2))
        if (tau > 0) cpm <- tol / (6 * tau)
      }

      list(cp = cp, cpk = cpk, cpl = cpl, cpu = cpu,
           pp = pp, ppk = ppk, ppl = ppl, ppu = ppu, cpm = cpm)
    },

    # ---- PPM (reuse iQualityR.stat::capability_to_ppm) ----

    .compute_ppm = function(x, sigma, plan, indices) {
      mu <- mean(x)
      ppm <- iQualityR.stat::capability_to_ppm(
        cpk = indices$cpk,
        ppk = indices$ppk,
        usl = plan$usl,
        lsl = plan$lsl,
        mean = mu,
        sigma_within = sigma$within,
        sigma_overall = sigma$overall
      )
      list(
        within = ppm$within,
        overall = ppm$overall
      )
    },

    # ---- Normality diagnostics (reuse iQualityR.stat::normality_test) ----

    .diagnose = function(x) {
      warnings <- character(0)
      p_val <- NULL
      method <- "Not computed"
      n <- length(x)

      if (n < 3) {
        method <- "Insufficient data"
        warnings <- c(warnings, "Sample size too small for normality test.")
      } else if (n >= 5000) {
        # For large samples, use skewness/kurtosis heuristic (avoid SW warning spam)
        sk <- mean((x - mean(x))^3) / (stats::sd(x)^3)
        ku <- mean((x - mean(x))^4) / (stats::sd(x)^4) - 3
        method <- "Skewness/Kurtosis (large n)"
        p_val <- NULL
        if (abs(sk) > 1 || abs(ku) > 2) {
          warnings <- c(warnings, "Data shows significant skewness or kurtosis.")
        }
      } else {
        # Use iQualityR.stat::normality_test for n in [3, 5000)
        result <- tryCatch(
          iQualityR.stat::normality_test(x, method = "auto", alpha = 0.05),
          error = function(e) NULL
        )
        if (!is.null(result)) {
          p_val <- result$p.value
          method <- result$method
          if (!is.null(p_val) && p_val < 0.05) {
            warnings <- c(warnings, "Data deviates significantly from normality (p < 0.05). Consider non-normal analysis.")
          }
        } else {
          method <- "Normality test failed"
        }
      }

      list(
        normality_p_value = p_val,
        method = method,
        warnings = warnings
      )
    },

    # ---- Intelligent capability judgment (reuse iQualityR.stat::capability_interpret) ----

    .judge_capability = function(cpk, ppk, cp, pp, plan) {
      criteria <- plan$criteria
      cpk_thr <- if (!is.null(criteria$cpk)) criteria$cpk else 1.33
      ppk_thr <- if (!is.null(criteria$ppk)) criteria$ppk else cpk_thr
      cp_thr <- if (!is.null(criteria$cp)) criteria$cp else cpk_thr
      pp_thr <- if (!is.null(criteria$pp)) criteria$pp else ppk_thr

      # Use iQualityR.stat::capability_interpret for level/interpretation
      cpk_interp <- iQualityR.stat::capability_interpret(cpk)
      ppk_interp <- iQualityR.stat::capability_interpret(ppk)

      judgment <- list(
        cpk_status = cpk_interp$level,
        ppk_status = ppk_interp$level,
        cp_status = if (cp >= cp_thr) "Acceptable" else if (cp >= 1.0) "Marginal" else "Unacceptable",
        pp_status = if (pp >= pp_thr) "Acceptable" else if (pp >= 1.0) "Marginal" else "Unacceptable",
        cpk_interpretation = cpk_interp$interpretation,
        ppk_interpretation = ppk_interp$interpretation,
        overall_verdict = if (cpk >= cpk_thr && ppk >= ppk_thr) {
          "PASS - Process capability is adequate."
        } else if (cpk >= 1.0 && ppk >= 1.0) {
          "CONDITIONAL - Process capability is marginal. Improvement recommended."
        } else {
          "FAIL - Process capability is inadequate."
        }
      )
      judgment
    },

    # ---- Improvement advice ----

    .generate_advice = function(indices, sigma, plan, diagnostics) {
      advice <- character(0)

      # Check centering (Cp/Cpk ratio), guard against cpk <= 0
      if (is.finite(indices$cpk) && indices$cpk > 0) {
        centering_ratio <- indices$cp / indices$cpk
        if (centering_ratio > 1.1) {
          advice <- c(advice, sprintf(
            "Process mean is offset from target. Cp/Cpk ratio = %.2f. Consider centering the process.",
            centering_ratio))
        }
      }

      # Check variation
      if (indices$cp < 1.33) {
        advice <- c(advice, sprintf(
          "Process variation is too high. Cp = %.2f < 1.33. Consider reducing process variability.",
          indices$cp))
      }

      # Normality warning (NULL-safe: large-sample path sets p_val to NULL)
      p_val <- diagnostics$normality_p_value
      if (!is.null(p_val) && is.finite(p_val) && p_val < 0.05) {
        advice <- c(advice, "Data is non-normal. Consider using capability_nonnormal() for better accuracy.")
      }

      # Within vs Overall discrepancy
      if (is.finite(sigma$within) && sigma$within > 0) {
        if (abs(sigma$within - sigma$overall) / sigma$within > 0.2) {
          advice <- c(advice, "Significant difference between within and overall SD. Check for process shifts or special causes.")
        }
      }

      if (length(advice) == 0) advice <- "Process is performing well. Continue monitoring."
      advice
    },

    # ---- Sixpack data ----

    .prepare_sixpack_data = function(x, subgroup, sigma, indices, plan) {
      n <- length(x)
      d4_n2 <- iQualityR.stat::get_D4(2)

      list(
        # Chart 1: Individual values
        individual = data.frame(
          index = seq_len(n),
          value = x,
          mean = mean(x),
          lsl = plan$lsl,
          usl = plan$usl
        ),
        # Chart 2: Moving Range
        moving_range = if (is.null(subgroup)) {
          mr <- abs(diff(x))
          data.frame(
            index = seq_along(mr),
            mr = mr,
            mean_mr = mean(mr),
            ucl_mr = d4_n2 * mean(mr)
          )
        } else {
          dt <- data.table::data.table(x = x, g = as.factor(subgroup))
          ranges <- dt[, .(R = diff(range(x, na.rm = TRUE))), by = g]
          R_bar <- mean(ranges$R)
          # Use D4 for typical subgroup size (median n)
          n_bar <- dt[, .N, by = g][, median(N)]
          d4_n <- iQualityR.stat::get_D4(max(2, n_bar))
          data.frame(
            index = seq_len(nrow(ranges)),
            mr = ranges$R,
            mean_mr = R_bar,
            ucl_mr = d4_n * R_bar
          )
        },
        # Chart 3: Histogram data
        histogram = list(
          values = x,
          mean = mean(x),
          sd_within = sigma$within,
          lsl = plan$lsl,
          usl = plan$usl,
          target = plan$target
        ),
        # Chart 4: QQ plot data
        qq = list(
          values = x,
          distribution = "normal"
        ),
        # Chart 5: Capability indices
        indices = list(
          cp = indices$cp, cpk = indices$cpk,
          pp = indices$pp, ppk = indices$ppk,
          cpl = indices$cpl, cpu = indices$cpu,
          ppl = indices$ppl, ppu = indices$ppu,
          cpm = indices$cpm
        ),
        # Chart 6: Last 25 subgroups or Cpk trend
        last_subgroups = if (!is.null(subgroup)) {
          dt <- data.table::data.table(x = x, g = as.factor(subgroup))
          groups <- unique(subgroup)
          last_n <- tail(groups, 25)
          dt[g %in% last_n, .(mean_val = mean(x), sd_val = stats::sd(x), n = .N), by = g]
        } else {
          # Disjoint window Cpk trend
          window_size <- min(25, floor(n / 3))
          if (window_size < 5) window_size <- 5
          n_windows <- floor(n / window_size)
          if (n_windows < 2) {
            data.frame(window = 1, cpk = NA_real_)
          } else {
            cpk_trend <- sapply(seq_len(n_windows), function(i) {
              start_idx <- (i - 1) * window_size + 1
              end_idx <- min(i * window_size, n)
              window_x <- x[start_idx:end_idx]
              w_mean <- mean(window_x)
              w_sd <- stats::sd(window_x)
              if (w_sd > 0) {
                min((plan$usl - w_mean) / (3 * w_sd), (w_mean - plan$lsl) / (3 * w_sd))
              } else NA_real_
            })
            data.frame(window = seq_len(n_windows), cpk = cpk_trend)
          }
        }
      )
    },

    # ---- Bootstrap CI (use withr::local_seed for reproducibility) ----

    .bootstrap_ci = function(x, subgroup, plan, n_boot = 1000) {
      withr::local_seed(42)
      n <- length(x)

      boot_cpk <- numeric(n_boot)
      boot_cp <- numeric(n_boot)

      for (b in seq_len(n_boot)) {
        idx <- sample(seq_len(n), n, replace = TRUE)
        xb <- x[idx]
        mu_b <- mean(xb)
        sd_b <- stats::sd(xb)
        if (sd_b > 0) {
          boot_cp[b] <- (plan$usl - plan$lsl) / (6 * sd_b)
          boot_cpk[b] <- min(
            (plan$usl - mu_b) / (3 * sd_b),
            (mu_b - plan$lsl) / (3 * sd_b)
          )
        } else {
          boot_cp[b] <- NA_real_
          boot_cpk[b] <- NA_real_
        }
      }

      ci_table <- data.frame(
        Statistic = c("Cp", "Cpk"),
        Estimate = c(mean(boot_cp, na.rm = TRUE), mean(boot_cpk, na.rm = TRUE)),
        Lower_CI = c(stats::quantile(boot_cp, 0.025, na.rm = TRUE), stats::quantile(boot_cpk, 0.025, na.rm = TRUE)),
        Upper_CI = c(stats::quantile(boot_cp, 0.975, na.rm = TRUE), stats::quantile(boot_cpk, 0.975, na.rm = TRUE)),
        conf_level = plan$conf_level
      )

      list(boot_cpk = boot_cpk, boot_cp = boot_cp, ci_table = ci_table)
    },

    # ---- Non-normal analysis private methods ----

    .compute_nonnormal_indices = function(x, dist_name, params, plan, fitter) {
      tol <- plan$tolerance()

      # Quantile method: 0.135% and 99.865% quantiles correspond to +/-3 sigma
      q_00135 <- fitter$eval_quantile(0.00135, dist_name, params)
      q_99865 <- fitter$eval_quantile(0.99865, dist_name, params)
      q_50 <- fitter$eval_quantile(0.5, dist_name, params)

      sigma_eq <- (q_99865 - q_00135) / 6

      pp <- tol / (6 * sigma_eq)
      ppu <- (plan$usl - q_50) / (3 * sigma_eq)
      ppl <- (q_50 - plan$lsl) / (3 * sigma_eq)
      ppk <- min(ppu, ppl)

      # For non-normal, Cp/Cpk equal Pp/Ppk (no within/overall distinction)
      cp <- pp; cpk <- ppk; cpu <- ppu; cpl <- ppl

      cpm <- NULL
      if (!is.null(plan$target)) {
        tau <- sqrt(mean((x - plan$target)^2))
        if (tau > 0) cpm <- tol / (6 * tau)
      }

      list(cp = cp, cpk = cpk, cpl = cpl, cpu = cpu,
           pp = pp, ppk = ppk, ppl = ppl, ppu = ppu, cpm = cpm)
    },

    .compute_nonnormal_ppm = function(x, dist_name, params, plan, fitter) {
      p_below <- fitter$eval_cdf(plan$lsl, dist_name, params)
      p_above <- 1 - fitter$eval_cdf(plan$usl, dist_name, params)

      list(
        below_lsl = p_below * 1e6,
        above_usl = p_above * 1e6,
        total = (p_below + p_above) * 1e6
      )
    },

    .prepare_nonnormal_sixpack_data = function(x, dist_name, params, plan, fitter, indices) {
      n <- length(x)
      d4_n2 <- iQualityR.stat::get_D4(2)

      list(
        individual = data.frame(
          index = seq_len(n),
          value = x,
          mean = mean(x),
          lsl = plan$lsl,
          usl = plan$usl
        ),
        moving_range = {
          mr <- abs(diff(x))
          data.frame(
            index = seq_along(mr),
            mr = mr,
            mean_mr = mean(mr),
            ucl_mr = d4_n2 * mean(mr)
          )
        },
        histogram = list(
          values = x,
          mean = mean(x),
          lsl = plan$lsl,
          usl = plan$usl,
          target = plan$target,
          distribution = dist_name,
          params = params,
          fitter = fitter
        ),
        pp_plot = list(
          values = x,
          distribution = dist_name,
          params = params,
          fitter = fitter
        ),
        indices = list(
          cp = indices$cp, cpk = indices$cpk,
          pp = indices$pp, ppk = indices$ppk,
          cpl = indices$cpl, cpu = indices$cpu,
          ppl = indices$ppl, ppu = indices$ppu,
          cpm = indices$cpm
        ),
        fit_comparison = list(
          values = x,
          distribution = dist_name,
          params = params,
          fitter = fitter
        )
      )
    },

    .bootstrap_nonnormal_ci = function(x, dist_name, params, plan, fitter, n_boot = 1000) {
      withr::local_seed(42)
      n <- length(x)

      boot_cpk <- numeric(n_boot)
      boot_pp <- numeric(n_boot)

      for (b in seq_len(n_boot)) {
        idx <- sample(seq_len(n), n, replace = TRUE)
        xb <- x[idx]

        tryCatch({
          fit_b <- fitter$fit_one(xb, dist_name)
          if (fit_b$converged) {
            indices_b <- self$.compute_nonnormal_indices(xb, dist_name, fit_b$params, plan, fitter)
            boot_cpk[b] <- indices_b$cpk
            boot_pp[b] <- indices_b$pp
          } else {
            boot_cpk[b] <- NA_real_
            boot_pp[b] <- NA_real_
          }
        }, error = function(e) {
          boot_cpk[b] <<- NA_real_
          boot_pp[b] <<- NA_real_
        })
      }

      ci_table <- data.frame(
        Statistic = c("Pp", "Ppk"),
        Estimate = c(mean(boot_pp, na.rm = TRUE), mean(boot_cpk, na.rm = TRUE)),
        Lower_CI = c(stats::quantile(boot_pp, 0.025, na.rm = TRUE), stats::quantile(boot_cpk, 0.025, na.rm = TRUE)),
        Upper_CI = c(stats::quantile(boot_pp, 0.975, na.rm = TRUE), stats::quantile(boot_cpk, 0.975, na.rm = TRUE)),
        conf_level = plan$conf_level
      )

      list(boot_cpk = boot_cpk, boot_pp = boot_pp, ci_table = ci_table)
    },

    .generate_nonnormal_advice = function(indices, dist_name, best_fit, plan) {
      advice <- character(0)

      if (indices$cpk < 1.33) {
        advice <- c(advice, sprintf(
          "Process capability is %s. Cpk = %.2f < 1.33.",
          if (indices$cpk < 1.0) "inadequate" else "marginal",
          indices$cpk))
      }

      if (!is.null(best_fit$ks_p_value) && best_fit$ks_p_value < 0.05) {
        advice <- c(advice, sprintf(
          "%s distribution fit may not be optimal (KS p = %.4f). Consider other distributions.",
          dist_name, best_fit$ks_p_value))
      }

      if (is.finite(indices$cpk) && indices$cpk > 0 && indices$cp > 1.2 * indices$cpk) {
        advice <- c(advice, "Significant difference between Cp and Cpk suggests process centering issues.")
      }

      if (length(advice) == 0) advice <- "Process is performing well. Continue monitoring."
      advice
    },

    # ---- Non-parametric analysis private methods ----

    .compute_nonparametric_indices = function(x, method, plan) {
      tol <- plan$tolerance()

      # Empirical quantiles (0.135% and 99.865% correspond to +/-3 sigma)
      q_00135 <- stats::quantile(x, 0.00135)
      q_99865 <- stats::quantile(x, 0.99865)
      q_50 <- stats::quantile(x, 0.5)

      sigma_eq <- (q_99865 - q_00135) / 6

      pp <- tol / (6 * sigma_eq)
      ppu <- (plan$usl - q_50) / (3 * sigma_eq)
      ppl <- (q_50 - plan$lsl) / (3 * sigma_eq)
      ppk <- min(ppu, ppl)

      cp <- pp; cpk <- ppk; cpu <- ppu; cpl <- ppl

      cpm <- NULL
      if (!is.null(plan$target)) {
        tau <- sqrt(mean((x - plan$target)^2))
        if (tau > 0) cpm <- tol / (6 * tau)
      }

      list(cp = cp, cpk = cpk, cpl = cpl, cpu = cpu,
           pp = pp, ppk = ppk, ppl = ppl, ppu = ppu, cpm = cpm)
    },

    .compute_nonparametric_ppm = function(x, method, plan) {
      p_below <- sum(x < plan$lsl) / length(x)
      p_above <- sum(x > plan$usl) / length(x)

      list(
        below_lsl = p_below * 1e6,
        above_usl = p_above * 1e6,
        total = (p_below + p_above) * 1e6
      )
    },

    .prepare_nonparametric_sixpack_data = function(x, method, plan) {
      n <- length(x)
      d4_n2 <- iQualityR.stat::get_D4(2)

      list(
        individual = data.frame(
          index = seq_len(n),
          value = x,
          mean = mean(x),
          lsl = plan$lsl,
          usl = plan$usl
        ),
        moving_range = {
          mr <- abs(diff(x))
          data.frame(
            index = seq_along(mr),
            mr = mr,
            mean_mr = mean(mr),
            ucl_mr = d4_n2 * mean(mr)
          )
        },
        histogram = list(
          values = x,
          mean = mean(x),
          lsl = plan$lsl,
          usl = plan$usl,
          target = plan$target
        ),
        edf_plot = list(values = x, lsl = plan$lsl, usl = plan$usl),
        indices = list(
          cp = self$results$statistics$cp,
          cpk = self$results$statistics$cpk,
          pp = self$results$statistics$pp,
          ppk = self$results$statistics$ppk,
          cpl = self$results$statistics$cpl,
          cpu = self$results$statistics$cpu,
          ppl = self$results$statistics$ppl,
          ppu = self$results$statistics$ppu,
          cpm = self$results$statistics$cpm
        ),
        qq_plot = list(values = x)
      )
    },

    .bootstrap_nonparametric_ci = function(x, method, plan, n_boot = 1000) {
      withr::local_seed(42)
      n <- length(x)

      boot_cpk <- numeric(n_boot)
      boot_pp <- numeric(n_boot)

      for (b in seq_len(n_boot)) {
        idx <- sample(seq_len(n), n, replace = TRUE)
        xb <- x[idx]

        tryCatch({
          indices_b <- self$.compute_nonparametric_indices(xb, method, plan)
          boot_cpk[b] <- indices_b$cpk
          boot_pp[b] <- indices_b$pp
        }, error = function(e) {
          boot_cpk[b] <<- NA_real_
          boot_pp[b] <<- NA_real_
        })
      }

      ci_table <- data.frame(
        Statistic = c("Pp", "Ppk"),
        Estimate = c(mean(boot_pp, na.rm = TRUE), mean(boot_cpk, na.rm = TRUE)),
        Lower_CI = c(stats::quantile(boot_pp, 0.025, na.rm = TRUE), stats::quantile(boot_cpk, 0.025, na.rm = TRUE)),
        Upper_CI = c(stats::quantile(boot_pp, 0.975, na.rm = TRUE), stats::quantile(boot_cpk, 0.975, na.rm = TRUE)),
        conf_level = plan$conf_level
      )

      list(boot_cpk = boot_cpk, boot_pp = boot_pp, ci_table = ci_table)
    },

    .generate_nonparametric_advice = function(indices, method, plan) {
      advice <- character(0)

      if (indices$cpk < 1.33) {
        advice <- c(advice, sprintf(
          "Process capability is %s. Cpk = %.2f < 1.33.",
          if (indices$cpk < 1.0) "inadequate" else "marginal",
          indices$cpk))
      }

      if (is.finite(indices$cpk) && indices$cpk > 0 && indices$cp > 1.2 * indices$cpk) {
        advice <- c(advice, "Significant difference between Cp and Cpk suggests process centering issues.")
      }

      if (length(advice) == 0) advice <- "Process is performing well. Continue monitoring."
      advice
    }
  )
)
