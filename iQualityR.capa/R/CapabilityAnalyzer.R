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

      # Data cleaning: compute keep mask first, then filter both x and subgroup
      keep <- !is.na(x)
      x <- x[keep]
      if (!is.null(subgroup)) subgroup <- subgroup[keep]

      # Dispatch to the appropriate analysis method
      if (plan$analysis_type == "nonnormal") {
        self$.run_nonnormal(x, plan)
      } else if (plan$analysis_type == "nonparametric") {
        self$.run_nonparametric(x, plan)
      } else if (plan$analysis_type == "between_within") {
        self$.run_between_within(x, subgroup, plan)
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
      # Optional transformation (Box-Cox / Johnson / auto) per Minitab
      # convention: transform both data AND spec limits with the same
      # parameters, then run normal capability on the transformed scale.
      # The original (untransformed) plan is left untouched; a cloned plan
      # carries the transformed LSL/USL/target through the analysis.
      transform_info <- NULL
      plan_used <- plan
      if (!is.null(plan$transform)) {
        transform_info <- self$.apply_transformation(x, plan)
        if (!is.null(transform_info)) {
          x <- transform_info$transformed_x
          # Deep-clone the plan so the user's original plan is not mutated
          # but every downstream consumer (indices, PPM, sixpack, bootstrap)
          # sees the transformed spec limits.
          plan_used <- plan$clone(deep = TRUE)
          plan_used$lsl <- transform_info$transformed_lsl
          plan_used$usl <- transform_info$transformed_usl
          if (!is.null(plan_used$target)) {
            plan_used$target <- transform_info$transformed_target
          }
        } else {
          # Transform failed; warn and fall back to untransformed analysis.
          warning(sprintf("Transform '%s' could not be applied; falling back to untransformed normal analysis.",
                          plan$transform), call. = FALSE)
        }
      }

      # Sigma decomposition (reuse iQualityR.stat)
      sigma <- self$.estimate_sigma(x, subgroup)

      # Capability indices
      indices <- self$.compute_indices(x, sigma, plan_used)

      # PPM (reuse iQualityR.stat::capability_to_ppm)
      ppm <- self$.compute_ppm(x, sigma, plan_used, indices)

      # Normality diagnostics (reuse iQualityR.stat::normality_test)
      diagnostics <- self$.diagnose(x)

      # Sixpack data
      sixpack_data <- self$.prepare_sixpack_data(x, subgroup, sigma, indices, plan_used)

      # Bootstrap CI (optional)
      bootstrap_results <- NULL
      if (plan_used$use_bootstrap && plan_used$bootstrap_samples > 0) {
        bootstrap_results <- self$.bootstrap_ci(x, subgroup, plan_used, n_boot = plan_used$bootstrap_samples)
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
      if (!is.null(indices$cpmk)) self$set_statistic("cpmk", indices$cpmk)
      self$set_statistic("z_lsl", indices$z_lsl)
      self$set_statistic("z_usl", indices$z_usl)
      self$set_statistic("z_bench_within", indices$z_bench_within)
      self$set_statistic("z_bench_overall", indices$z_bench_overall)
      self$set_statistic("mean", mean(x))
      self$set_statistic("sd_within", sigma$within)
      self$set_statistic("sd_overall", sigma$overall)
      self$set_statistic("n", length(x))
      if (!is.null(subgroup)) {
        self$set_statistic("n_subgroups", length(unique(subgroup)))
        self$set_statistic("subgroup_sizes", table(subgroup))
      }

      # Merge size-based warnings (small-sample) so they survive reset() and are
      # surfaced through the standard diagnostics$warnings channel, consistent
      # with the other iQualityR subpackages' diagnostic protocol.
      diagnostics$warnings <- c(self$.small_sample_warning(x), diagnostics$warnings)

      self$set_diagnostic("normality_p_value", diagnostics$normality_p_value)
      self$set_diagnostic("normality_method", diagnostics$method)

      # Transform diagnostics (only populated when a transform was requested
      # and successfully applied). The plotter and reporters use these to
      # annotate output on the transformed scale.
      if (!is.null(transform_info)) {
        self$set_diagnostic("transform_applied", transform_info$method)
        self$set_diagnostic("transform_choice", plan$transform)
        if (!is.null(transform_info$lambda)) {
          self$set_diagnostic("transform_lambda", transform_info$lambda)
        }
        if (!is.null(transform_info$type)) {
          self$set_diagnostic("transform_type", transform_info$type)
        }
        self$set_diagnostic("transform_normality_before", transform_info$normality_before)
        self$set_diagnostic("transform_normality_after", transform_info$normality_after)
        self$set_diagnostic("transformed_lsl", transform_info$transformed_lsl)
        self$set_diagnostic("transformed_usl", transform_info$transformed_usl)
        if (!is.null(transform_info$transformed_target)) {
          self$set_diagnostic("transformed_target", transform_info$transformed_target)
        }
        # Surface an informational note via the warnings channel so users see
        # that reported indices are on the transformed scale.
        diagnostics$warnings <- c(diagnostics$warnings,
          sprintf("Indices computed on %s-transformed scale (specs transformed with same parameters).",
                  transform_info$method))
      }

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
        self$.judge_capability(indices$cpk, indices$ppk, indices$cp, indices$pp, plan_used))
      self$set_diagnostic("process_advice",
        self$.generate_advice(indices, sigma, plan_used, diagnostics))

      invisible(self)
    },

    #' @description Between/Within capability analysis (Minitab B/W convention).
    #' Requires subgroup data. Uses `sigma_between_within` for Cp/Cpk (B/W) and
    #' `sigma_total` for Pp/Ppk. Reuses `iQualityR.stat::sigma_decomposition`.
    #' @param x Numeric vector of measurements.
    #' @param subgroup Subgroup label vector (required).
    #' @param plan [CapabilityPlan] object.
    .run_between_within = function(x, subgroup, plan) {
      if (is.null(subgroup)) {
        stop("Between/Within capability analysis requires subgroup data.", call. = FALSE)
      }

      # Sigma decomposition (reuse iQualityR.stat). Same call as .run_normal
      # so the within / between / between-within / total components are
      # estimated consistently across the normal and B/W paths.
      decomp <- iQualityR.stat::sigma_decomposition(
        x = x,
        subgroup = subgroup,
        within_method = "r_bar",
        between_method = "mssd",
        within_unbiased = TRUE,
        total_unbiased = FALSE
      )
      sigma_within         <- decomp$sigma_within
      sigma_between        <- decomp$sigma_between
      sigma_between_within <- decomp$sigma_between_within
      sigma_overall        <- decomp$sigma_total

      # Guard against zero sigma
      if (!is.finite(sigma_within) || sigma_within <= 0) sigma_within <- stats::sd(x)
      if (!is.finite(sigma_between) || sigma_between < 0) sigma_between <- 0
      if (!is.finite(sigma_between_within) || sigma_between_within <= 0) sigma_between_within <- stats::sd(x)
      if (!is.finite(sigma_overall) || sigma_overall <= 0) sigma_overall <- stats::sd(x)

      # B/W convention: Cp/Cpk (B/W) use sigma_between_within; Pp/Ppk use sigma_total.
      # Reuse the shared .compute_indices / .compute_ppm / .prepare_sixpack_data
      # helpers by packaging the B/W sigma as sigma$within.
      sigma <- list(within = sigma_between_within, overall = sigma_overall)

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
      if (!is.null(indices$cpmk)) self$set_statistic("cpmk", indices$cpmk)
      self$set_statistic("z_lsl", indices$z_lsl)
      self$set_statistic("z_usl", indices$z_usl)
      self$set_statistic("z_bench_within", indices$z_bench_within)
      self$set_statistic("z_bench_overall", indices$z_bench_overall)
      self$set_statistic("mean", mean(x))
      self$set_statistic("sd_within", sigma_within)
      self$set_statistic("sd_between", sigma_between)
      self$set_statistic("sd_between_within", sigma_between_within)
      self$set_statistic("sd_overall", sigma_overall)
      self$set_statistic("n", length(x))
      if (!is.null(subgroup)) {
        self$set_statistic("n_subgroups", length(unique(subgroup)))
        self$set_statistic("subgroup_sizes", table(subgroup))
      }

      # Merge size-based warnings (small-sample) so they survive reset() and are
      # surfaced through the standard diagnostics$warnings channel, consistent
      # with the other iQualityR subpackages' diagnostic protocol.
      diagnostics$warnings <- c(self$.small_sample_warning(x), diagnostics$warnings)

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
      if (!is.null(best_fit$ks_p_value) && !is.na(best_fit$ks_p_value) &&
          best_fit$ks_p_value < 0.05) {
        diagnostics$warnings <- c(diagnostics$warnings,
          sprintf("Kolmogorov-Smirnov test suggests poor fit (p = %.4f).", best_fit$ks_p_value))
      }
      # Surface small-sample instability alongside fit-quality warnings.
      diagnostics$warnings <- c(self$.small_sample_warning(x), diagnostics$warnings)

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

    # Returns a (possibly empty) character vector of size-based warnings.
    # Centralized so normal/non-normal paths surface the same message and the
    # warning survives reset() because it is appended to diagnostics$warnings
    # after the result container is rebuilt.
    .small_sample_warning = function(x, threshold = 30) {
      n <- length(x)
      if (n < threshold) {
        sprintf("Sample size = %d (< %d). Capability indices may be unstable.",
                n, threshold)
      } else {
        character(0)
      }
    },

    # ---- Transformation path (Box-Cox / Johnson / auto) ----
    #
    # Applies the transform requested in plan$transform to the measurement
    # vector x via iQualityR.stat (never reimplemented locally) and projects
    # the spec limits (and target, if any) onto the same transformed scale
    # using the fitted parameters, following the Minitab convention where
    # both data and specs are transformed together.
    #
    # Returns NULL (and emits a warning) when the transform cannot be applied
    # so the caller can fall back gracefully to untransformed normal analysis.

    .apply_transformation = function(x, plan) {
      transform_choice <- plan$transform
      if (!transform_choice %in% c("box_cox", "johnson", "auto")) {
        warning(sprintf("Unknown transform '%s'; skipping transformation.",
                        transform_choice), call. = FALSE)
        return(NULL)
      }

      # Box-Cox requires strictly positive data. Pre-screen here so we can
      # emit a friendly warning rather than letting iQualityR.stat throw.
      if (transform_choice == "box_cox" && any(x <= 0)) {
        warning("Box-Cox transform requires all positive values; skipping transformation. Consider transform = 'johnson' or 'auto'.",
                call. = FALSE)
        return(NULL)
      }

      result <- tryCatch(
        {
          if (transform_choice == "box_cox") {
            iQualityR.stat::box_cox_transform(x, optimize = TRUE)
          } else if (transform_choice == "johnson") {
            iQualityR.stat::johnson_transform(x, type = "auto")
          } else {
            # auto: iQualityR.stat::auto_transform returns a wrapper list with
            # best_method / best_result; normalize to the same shape returned
            # by box_cox_transform / johnson_transform so downstream code can
            # treat all three uniformly.
            auto_result <- iQualityR.stat::auto_transform(x)
            best <- auto_result$best_result
            list(
              transformed       = best$transformed,
              method            = best$method %||% auto_result$best_method,
              lambda            = best$lambda,
              type              = best$type,
              parameters        = best$parameters,
              original          = x,
              n                 = length(x),
              normality_before  = best$normality_before,
              normality_after   = best$normality_after
            )
          }
        },
        error = function(e) {
          warning(sprintf("Transform '%s' failed: %s",
                          transform_choice, conditionMessage(e)),
                  call. = FALSE)
          NULL
        }
      )

      if (is.null(result)) return(NULL)
      if (length(result$transformed) != length(x)) {
        warning("Transformed data length mismatch; skipping transformation.",
                call. = FALSE)
        return(NULL)
      }

      # Project spec limits (and target) onto the transformed scale using the
      # same fitted parameters. .transform_value dispatches on result$method
      # so box_cox / johnson / yeo_johnson / log / sqrt / reciprocal are all
      # covered (auto_transform may pick any of these).
      transformed_lsl <- self$.transform_value(plan$lsl, result)
      transformed_usl <- self$.transform_value(plan$usl, result)
      transformed_target <- if (!is.null(plan$target)) {
        self$.transform_value(plan$target, result)
      } else {
        NULL
      }

      # Sanity: transformed specs must remain ordered (lsl < usl). If the
      # transform is monotonic increasing this always holds; if not, we warn
      # but still proceed (the indices will be computed on the transformed
      # scale regardless).
      if (is.finite(transformed_lsl) && is.finite(transformed_usl) &&
          transformed_lsl >= transformed_usl) {
        warning("Transformed LSL >= USL; results may be unreliable.",
                call. = FALSE)
      }

      list(
        transformed_x       = result$transformed,
        method              = result$method,
        lambda              = result$lambda,
        type                = result$type,
        parameters          = result$parameters,
        normality_before    = result$normality_before,
        normality_after     = result$normality_after,
        transformed_lsl     = transformed_lsl,
        transformed_usl     = transformed_usl,
        transformed_target  = transformed_target
      )
    },

    # Apply a fitted transform to a single scalar value (used for spec limits
    # and target). Mirrors the forward-transform formulas used internally by
    # iQualityR.stat::box_cox_transform / johnson_transform / auto_transform
    # so the specs land on exactly the same scale as the transformed data.
    # The transform *functions themselves* are always delegated to
    # iQualityR.stat; this helper only re-applies the already-fitted
    # parameters to a new scalar.
    .transform_value = function(value, transform_result) {
      method <- tolower(transform_result$method %||% "")

      if (grepl("box.cox", method)) {
        lambda <- transform_result$lambda
        if (is.null(lambda) || abs(lambda) < 1e-10) {
          log(value)
        } else {
          (value^lambda - 1) / lambda
        }
      } else if (grepl("johnson", method)) {
        params <- transform_result$parameters
        if (is.null(params)) return(NA_real_)
        gamma <- params$gamma; delta <- params$delta
        xi <- params$xi; lambda_p <- params$lambda
        type <- params$type
        z <- (value - xi) / lambda_p
        switch(type,
          "SU" = {
            sinh_inv <- function(v) log(v + sqrt(v^2 + 1))
            sinh_inv((z - gamma) / delta)
          },
          "SB" = {
            z_clamped <- pmin(pmax(z, 1e-10), 1 - 1e-10)
            gamma + delta * log(z_clamped / (1 - z_clamped))
          },
          "SL" = {
            gamma + delta * log(pmax(z, 1e-10))
          },
          NA_real_
        )
      } else if (grepl("yeo", method)) {
        lambda <- transform_result$lambda
        if (is.null(lambda)) lambda <- 0
        if (value >= 0) {
          if (abs(lambda) < 1e-10) log(value + 1)
          else ((value + 1)^lambda - 1) / lambda
        } else {
          if (abs(lambda - 2) < 1e-10) -log(-value + 1)
          else -((-value + 1)^(2 - lambda) - 1) / (2 - lambda)
        }
      } else if (grepl("log", method)) {
        log(value)
      } else if (grepl("sqrt|square.root", method)) {
        sqrt(value)
      } else if (grepl("reciprocal", method)) {
        1 / value
      } else {
        NA_real_
      }
    },

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
        # Individuals data (no subgroups): within sigma via moving range
        # (MR-bar / d2) and overall sigma via total SD. Both delegated to
        # iQualityR.stat::sigma_estimate so estimation stays consistent with
        # the rest of the iQualityR ecosystem (same helpers used by .stat's
        # control-chart and capability tooling).
        sigma_within  <- iQualityR.stat::sigma_estimate(x, method = "mr_bar", m_span = 2)
        sigma_overall <- iQualityR.stat::sigma_estimate(x, method = "total", use_unbiased = FALSE)
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

      # Cpm (Taguchi) and Cpmk (third-generation)
      cpm <- NULL; cpmk <- NULL
      if (!is.null(plan$target)) {
        tau <- sqrt(mean((x - plan$target)^2))
        if (tau > 0) {
          cpm <- tol / (6 * tau)
          cpmk <- min((plan$usl - mu) / (3 * tau), (mu - plan$lsl) / (3 * tau))
        }
      }

      # Z.Bench / Z.LSL / Z.USL (reuse iQualityR.stat::z_bench for sigma level).
      # Z.LSL/Z.USL are the one-sided sigma distances; Z.Bench is the combined
      # sigma level derived from the total expected non-conforming rate.
      z_lsl <- (mu - plan$lsl) / sigma$within
      z_usl <- (plan$usl - mu) / sigma$within
      p_total_within <- stats::pnorm(-z_lsl) + (1 - stats::pnorm(z_usl))
      z_bench_within <- iQualityR.stat::z_bench(p_total_within, shift = 0)

      z_lsl_o <- (mu - plan$lsl) / sigma$overall
      z_usl_o <- (plan$usl - mu) / sigma$overall
      p_total_overall <- stats::pnorm(-z_lsl_o) + (1 - stats::pnorm(z_usl_o))
      z_bench_overall <- iQualityR.stat::z_bench(p_total_overall, shift = 0)

      list(cp = cp, cpk = cpk, cpl = cpl, cpu = cpu,
           pp = pp, ppk = ppk, ppl = ppl, ppu = ppu,
           cpm = cpm, cpmk = cpmk,
           z_lsl = z_lsl, z_usl = z_usl,
           z_bench_within = z_bench_within,
           z_bench_overall = z_bench_overall)
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

      # ---- Xbar-R data (rational subgroups, size >= 2) ----
      # Per the capa requirements v5.0 §5.3: when rational subgroups exist,
      # the Sixpack stability row MUST use an Xbar-R chart (NOT an I-MR).
      # Only fall back to I-MR for individuals data (no subgroup / size == 1).
      # Control limits use the standard Shewhart constants from iQualityR.stat
      # (get_A2 / get_D3 / get_D4), consistent with the ASTM E2587 standard.
      xbar_chart <- NULL
      r_chart    <- NULL
      if (!is.null(subgroup)) {
        dt  <- data.table::data.table(x = x, g = as.factor(subgroup))
        agg <- dt[, .(xbar = mean(x), R = diff(range(x, na.rm = TRUE)),
                      n = .N), by = g]
        n_bar <- stats::median(agg$n)
        if (!is.na(n_bar) && n_bar >= 2) {
          xbar_bar <- mean(agg$xbar)
          r_bar    <- mean(agg$R)
          A2 <- iQualityR.stat::get_A2(n_bar)
          D3 <- iQualityR.stat::get_D3(n_bar)
          D4 <- iQualityR.stat::get_D4(n_bar)
          ucl_x <- xbar_bar + A2 * r_bar
          lcl_x <- xbar_bar - A2 * r_bar
          ucl_r <- D4 * r_bar
          lcl_r <- D3 * r_bar
          xbar_chart <- data.frame(
            index = seq_len(nrow(agg)),
            value = agg$xbar,
            cl    = xbar_bar,
            ucl   = ucl_x,
            lcl   = lcl_x,
            ooc   = (agg$xbar > ucl_x) | (agg$xbar < lcl_x),
            stringsAsFactors = FALSE
          )
          r_chart <- data.frame(
            index = seq_len(nrow(agg)),
            value = agg$R,
            cl    = r_bar,
            ucl   = ucl_r,
            lcl   = lcl_r,
            ooc   = (agg$R > ucl_r) | (agg$R < lcl_r),
            stringsAsFactors = FALSE
          )
        }
      }

      list(
        # Chart 1 (subgrouped): Xbar chart of subgroup means
        xbar_chart = xbar_chart,
        # Chart 1 (individuals): Individual values chart (fallback when no
        # rational subgroups exist)
        individual = data.frame(
          index = seq_len(n),
          value = x,
          mean = mean(x),
          lsl = plan$lsl,
          usl = plan$usl
        ),
        # Chart 2 (subgrouped): R chart of subgroup ranges
        r_chart = r_chart,
        # Chart 2 (individuals): Moving Range chart (fallback)
        moving_range = if (is.null(subgroup)) {
          mr <- abs(diff(x))
          data.frame(
            index = seq_along(mr),
            mr = mr,
            mean_mr = mean(mr),
            ucl_mr = d4_n2 * mean(mr)
          )
        } else {
          # Subgrouped but n_bar == 1 (degenerate): keep legacy MR-style frame
          # so the I-MR fallback path still has data to plot.
          dt <- data.table::data.table(x = x, g = as.factor(subgroup))
          ranges <- dt[, .(R = diff(range(x, na.rm = TRUE))), by = g]
          R_bar <- mean(ranges$R)
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
        },
        # Chart 6 (alt): Last 25 subgroups — ALL observations (scatter)
        # 25 groups × n_obs per group, for the "Last 25 Subgroups" panel
        last_observations = if (!is.null(subgroup)) {
          groups <- unique(subgroup)
          last_n <- tail(groups, 25)
          keep_idx <- subgroup %in% last_n
          # Re-index subgroup to 1..K for clean x-axis
          subgs <- sort(unique(subgroup[keep_idx]))
          remap <- setNames(seq_along(subgs), subgs)
          data.frame(
            subgroup = remap[as.character(subgroup[keep_idx])],
            value = x[keep_idx],
            stringsAsFactors = FALSE
          )
        } else {
          # Individuals: last 25 observations
          keep_idx <- seq_len(n)
          if (n > 25) keep_idx <- (n - 24):n
          data.frame(
            index = seq_along(keep_idx),
            value = x[keep_idx],
            stringsAsFactors = FALSE
          )
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

      if (!is.null(best_fit$ks_p_value) && !is.na(best_fit$ks_p_value) &&
          best_fit$ks_p_value < 0.05) {
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
