# =============================================================================
# File: R/AttributeCapabilityPlan.R
# Description: Plan + Task + Plotter + wrappers for attribute (Binomial /
#              Poisson) capability analysis.
# =============================================================================

#' AttributeCapabilityPlan
#'
#' @title AttributeCapabilityPlan
#'
#' @description Configuration for attribute (Binomial / Poisson) capability
#'   analysis. Inherits `IqrPlanBase` and adds attribute-specific fields.
#'
#' @field analysis_type `"binomial"` or `"poisson"`.
#' @field target_proportion Optional target rate (defectives or defects per unit).
#' @field ci_method CI method for binomial: `"wilson"` (default) or
#'   `"clopper_pearson"`.
#' @field z_shift Sigma-level shift convention (default 1.5, Six Sigma).
#'
#' @export
AttributeCapabilityPlan <- R6::R6Class("AttributeCapabilityPlan",
  inherit = IqrPlanBase,
  public = list(
    analysis_type = "binomial",
    target_proportion = NULL,
    ci_method = "wilson",
    z_shift = 1.5,

    #' @description Create a new attribute capability plan
    #' @param analysis_type `"binomial"` or `"poisson"`.
    #' @param target_proportion Optional target rate.
    #' @param ci_method `"wilson"` or `"clopper_pearson"` (binomial only).
    #' @param z_shift Sigma shift (default 1.5).
    #' @param conf_level Confidence level.
    #' @param task_tag Task tag.
    #' @param ... Additional arguments.
    initialize = function(analysis_type = c("binomial", "poisson"),
                          target_proportion = NULL,
                          ci_method = NULL,
                          z_shift = 1.5,
                          conf_level = 0.95,
                          task_tag = "attribute_capability", ...) {
      analysis_type <- match.arg(analysis_type)
      # Default ci_method depends on analysis_type:
      #   binomial -> wilson (or clopper_pearson when explicitly requested)
      #   poisson  -> garwood_exact (always; no user override)
      if (is.null(ci_method)) {
        ci_method <- if (analysis_type == "binomial") "wilson" else "garwood_exact"
      } else if (analysis_type == "binomial") {
        ci_method <- match.arg(ci_method, c("wilson", "clopper_pearson"))
      } else {
        # Poisson: respect garwood_exact if passed; otherwise default
        if (!ci_method %in% c("garwood_exact", "wilson", "clopper_pearson")) {
          stop("Unknown ci_method for poisson: ", ci_method, call. = FALSE)
        }
        if (ci_method != "garwood_exact") {
          ci_method <- "garwood_exact"  # Poisson always uses exact CI
        }
      }
      super$initialize(task_tag = task_tag, conf_level = conf_level, ...)
      self$analysis_type <- analysis_type
      self$target_proportion <- target_proportion
      self$ci_method <- ci_method
      self$z_shift <- z_shift
      # Default acceptance criteria: 4 sigma short-term (>= 4 -> pass)
      self$set_criteria(z_bench = 4.0, sigma_level = 4.0)
      invisible(self)
    },

    #' @description Export configuration as a list (overrides base method)
    to_list = function() {
      base_list <- super$to_list()
      base_list$analysis_type <- self$analysis_type
      base_list$target_proportion <- self$target_proportion
      base_list$ci_method <- self$ci_method
      base_list$z_shift <- self$z_shift
      base_list
    },

    #' @description Validate plan (overrides base)
    validate = function() {
      super$validate()
      if (!self$analysis_type %in% c("binomial", "poisson")) {
        stop("analysis_type must be 'binomial' or 'poisson'", call. = FALSE)
      }
      if (!is.null(self$target_proportion) &&
          (self$target_proportion < 0 || self$target_proportion > 1)) {
        stop("target_proportion must be in [0, 1]", call. = FALSE)
      }
      invisible(self)
    }
  )
)

# =============================================================================
# AttributeCapabilityPlotter
# =============================================================================

# Internal: format Z.Bench value for display
.fmt_z <- function(x) {
  if (is.null(x) || is.na(x) || !is.finite(x)) return("NA")
  formatC(signif(x, 4), format = "fg")
}

#' AttributeCapabilityPlotter
#'
#' @title AttributeCapabilityPlotter
#'
#' @description Thin delegation plotter for attribute capability analysis
#'   (binomial / poisson). Dispatches every panel to the matching
#'   `iQualityR.plot::plot_attribute_*` function. No ggplot2 geom_* / stat_*
#'   logic lives here.
#'
#' @export
AttributeCapabilityPlotter <- R6::R6Class("AttributeCapabilityPlotter",
  inherit = IqrPlotterBase,
  public = list(

    #' @description Render a plot by type.
    #' @param results Analysis results list.
    #' @param theme_obj IqrTheme object.
    #' @param type Plot type: "full" (Sixpack), "control", "histogram"
    #'   (alias for "rate"), "rate", "cumulative", "defects", "fit",
    #'   "performance".
    #' @param plan AttributeCapabilityPlan object.
    #' @param ... Additional arguments (unused).
    render = function(results, theme_obj, type = "full", plan, ...) {
      if (is.null(results)) stop("No results.", call. = FALSE)
      switch(type,
        full        = self$.plot_sixpack(results, theme_obj, plan),
        control     = self$.plot_control(results, theme_obj, plan),
        histogram   = self$.plot_rate(results, theme_obj, plan),
        rate        = self$.plot_rate(results, theme_obj, plan),
        cumulative  = self$.plot_cumulative(results, theme_obj, plan),
        defects     = self$.plot_defects(results, theme_obj),
        fit         = self$.plot_fit(results, theme_obj, plan),
        performance = self$.plot_performance(results, theme_obj, plan),
        stop("Unknown plot type: ", type, call. = FALSE)
      )
    },

    # -----------------------------------------------------------------
    # Sixpack (full): 6 panels in 3x2 layout
    # Row 1: control | rate
    # Row 2: cumulative | defects
    # Row 3: fit | performance
    # -----------------------------------------------------------------
    .plot_sixpack = function(results, theme_obj, plan) {
      panels <- list(
        control     = self$.plot_control(results, theme_obj, plan),
        rate        = self$.plot_rate(results, theme_obj, plan),
        cumulative  = self$.plot_cumulative(results, theme_obj, plan),
        defects     = self$.plot_defects(results, theme_obj),
        fit         = self$.plot_fit(results, theme_obj, plan),
        performance = self$.plot_performance(results, theme_obj, plan)
      )
      dist <- results$statistics$distribution
      iQualityR.plot::plot_attribute_sixpack(
        panels   = panels,
        title    = sprintf("Attribute Capability Sixpack (%s)", dist),
        subtitle = self$.build_subtitle(results),
        theme    = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Panel 1: Control chart (P-chart or U-chart)
    # -----------------------------------------------------------------
    .plot_control = function(results, theme_obj, plan) {
      pts <- results$data_tables$points
      target <- self$.effective_target(results, plan)
      iQualityR.plot::plot_attribute_control_chart(
        points    = pts,
        rate_name = results$statistics$rate_name,
        target    = target,
        theme     = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Panel 2: Rate distribution histogram
    # -----------------------------------------------------------------
    .plot_rate = function(results, theme_obj, plan) {
      pts <- results$data_tables$points
      target <- self$.effective_target(results, plan)
      iQualityR.plot::plot_attribute_rate_histogram(
        values      = pts$value,
        rate        = results$statistics$rate,
        rate_lower  = results$statistics$rate_lower,
        rate_upper  = results$statistics$rate_upper,
        rate_name   = results$statistics$rate_name,
        target      = target,
        theme       = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Panel 3: Cumulative rate convergence
    # -----------------------------------------------------------------
    .plot_cumulative = function(results, theme_obj, plan) {
      pts <- results$data_tables$points
      iQualityR.plot::plot_attribute_cumulative(
        points     = pts,
        rate       = results$statistics$rate,
        rate_lower = results$statistics$rate_lower,
        rate_upper = results$statistics$rate_upper,
        rate_name  = results$statistics$rate_name,
        theme      = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Panel 4: Defects per subgroup bar chart
    # -----------------------------------------------------------------
    .plot_defects = function(results, theme_obj) {
      iQualityR.plot::plot_attribute_defects_bar(
        points = results$data_tables$points,
        theme  = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Panel 5: Distribution fit (binomial / poisson PMF vs observed)
    # -----------------------------------------------------------------
    .plot_fit = function(results, theme_obj, plan) {
      pts <- results$data_tables$points
      target <- self$.effective_target(results, plan)
      dist <- results$statistics$distribution
      if (dist == "binomial") {
        iQualityR.plot::plot_attribute_binomial_fit(
          defects      = pts$defects,
          sample_sizes = pts$n,
          p_hat        = results$statistics$rate,
          target       = target,
          theme        = theme_obj
        )
      } else {
        iQualityR.plot::plot_attribute_poisson_fit(
          defects = pts$defects,
          lambda  = results$statistics$rate,
          u_bar   = results$statistics$rate,
          theme   = theme_obj
        )
      }
    },

    # -----------------------------------------------------------------
    # Panel 6: Performance bar (Observed vs Expected PPM)
    # -----------------------------------------------------------------
    .plot_performance = function(results, theme_obj, plan) {
      s <- results$statistics
      # Binomial stores observed as ppm_observed; Poisson stores dpmo_observed.
      observed <- s$ppm_observed
      if (is.null(observed)) observed <- s$dpmo_observed
      expected <- s$ppm_expected
      target_ppm <- NULL
      if (!is.null(plan$target_proportion) &&
          is.finite(plan$target_proportion)) {
        target_ppm <- plan$target_proportion * 1e6
      }
      iQualityR.plot::plot_attribute_performance_bar(
        observed_ppm = observed,
        expected_ppm = expected,
        z_bench      = s$z_bench,
        sigma_level  = s$sigma_level,
        target_ppm   = target_ppm,
        theme        = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Helpers
    # -----------------------------------------------------------------
    .effective_target = function(results, plan) {
      dist <- results$statistics$distribution
      if (dist == "binomial") {
        t <- plan$target_proportion
      } else {
        t <- plan$target_rate
      }
      if (is.null(t) || !is.finite(t)) NULL else t
    },

    .build_subtitle = function(results) {
      s <- results$statistics
      sprintf("%s | Z.Bench = %s | Sigma = %.2f | Verdict: %s",
              s$distribution, .fmt_z(s$z_bench), s$sigma_level,
              toupper(results$diagnostics$capability_judgment$overall_verdict))
    }
  )
)

# =============================================================================
# IqrAttributeCapabilityTask
# =============================================================================

#' IqrAttributeCapabilityTask
#'
#' @title IqrAttributeCapabilityTask
#'
#' @description Task coordinator for attribute capability analysis. Inherits
#'   `IqrTaskBase` and dispatches to `AttributeCapabilityAnalyzer` /
#'   `AttributeCapabilityPlotter`.
#'
#' @param data Data frame containing defect counts and sample sizes.
#' @param defects Column name for defect/defective counts.
#' @param sample_sizes Column name for subgroup sizes / exposures.
#' @param plan [AttributeCapabilityPlan] object.
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#'
#' @export
IqrAttributeCapabilityTask <- R6::R6Class("IqrAttributeCapabilityTask",
  inherit = IqrTaskBase,
  public = list(
    plan = NULL,

    #' @description Create a task instance
    initialize = function(data, defects, sample_sizes, plan,
                          theme = "academic", ...) {
      super$initialize(data, theme, ...)
      self$plan <- plan
      private$defects <- defects
      private$sample_sizes <- sample_sizes
      self$executor$analyzer <- AttributeCapabilityAnalyzer$new()
      self$executor$plotter  <- AttributeCapabilityPlotter$new()
    },

    #' @description Execute attribute capability analysis
    compute = function() {
      counts <- self$data[[private$defects]]
      n_vec  <- self$data[[private$sample_sizes]]
      keep <- !is.na(counts) & !is.na(n_vec)
      if (any(!keep)) warning("Missing values removed.", call. = FALSE)
      self$executor$analyzer$run(counts = counts[keep],
                                 sample_sizes = n_vec[keep],
                                 plan = self$plan)
      self$results <- self$executor$analyzer$get_results()
      invisible(self)
    },

    #' @description Print summary
    summary = function() {
      if (is.null(self$results)) {
        cat("No results yet. Run $compute() first.\n")
        return(invisible(self))
      }
      stats <- self$results$statistics
      diag <- self$results$diagnostics
      verdict <- diag$capability_judgment

      cat(sprintf("\n===== Attribute Capability (%s) =====\n",
                  stats$distribution))
      cat(sprintf("k subgroups: %d\n", stats$n_subgroups))
      if (stats$distribution == "binomial") {
        cat(sprintf("Defectives / inspected: %d / %d\n",
                    stats$total_defectives, stats$total_inspected))
        cat(sprintf("p-bar (proportion defective): %.6f\n", stats$rate))
      } else {
        cat(sprintf("Defects / exposure: %d / %d\n",
                    stats$total_defects, stats$total_exposure))
        cat(sprintf("u-bar (defects per unit): %.6f\n", stats$rate))
      }
      cat(sprintf("%.0f%% CI: [%.6f, %.6f] (%s)\n",
                  100 * self$plan$conf_level,
                  stats$rate_lower, stats$rate_upper, stats$ci_method))

      cat("\n--- Yield & PPM ---\n")
      cat(sprintf("Yield: %.4f%%\n", 100 * stats$yield))
      if (stats$distribution == "binomial") {
        cat(sprintf("PPM (expected): %.2f\n", stats$ppm_expected))
      } else {
        cat(sprintf("DPMO (expected): %.2f\n", stats$dpmo_expected))
      }

      cat("\n--- Sigma level ---\n")
      cat(sprintf("Z.Bench (short-term, shift=0): %.4f\n", stats$z_bench))
      cat(sprintf("Sigma level (+ %.1f shift): %.4f\n",
                  stats$z_shift, stats$sigma_level))

      cat("\n--- Dispersion test ---\n")
      disp <- diag$dispersion_test
      if (!is.na(disp$p_value)) {
        cat(sprintf("%s: stat = %.4f, p = %.4f %s\n",
                    disp$method, disp$statistic, disp$p_value,
                    ifelse(disp$p_value < 0.05,
                           "(over-dispersed; model invalid)",
                           "(model acceptable)")))
      }

      cat("\n--- Verdict ---\n")
      cat(sprintf("Sigma status: %s\n", verdict$sigma_label))
      if (!is.null(self$plan$target_proportion)) {
        cat(sprintf("Target status: %s\n", verdict$target_label))
      }
      cat(sprintf("Overall: %s\n", toupper(verdict$overall_verdict)))

      if (length(diag$warnings) > 0) {
        cat("\n--- Warnings ---\n")
        for (w in diag$warnings) cat("  ", w, "\n")
      }
      cat("=================================\n")
      invisible(self)
    },

    #' @description Render plot
    plot = function(type = "full", theme = NULL, ...) {
      if (is.null(self$results)) stop("No results. Run $compute() first.", call. = FALSE)
      theme_obj_use <- if (!is.null(theme)) {
        if (inherits(theme, "IqrTheme")) theme else IqrTheme$new(theme)
      } else self$theme_obj
      self$executor$plotter$render(
        results = self$results, theme_obj = theme_obj_use,
        type = type, plan = self$plan, ...)
    },

    #' @description Generate Excel report
    report = function(format = "excel", path = NULL, ...) {
      if (is.null(self$results)) stop("No results. Run $compute() first.", call. = FALSE)
      reporter <- iQualityR.core::IqrReporter$new(self$theme_obj)
      reporter$register(
        task_tag = "attribute_capability",
        excel_generator = function(results, plan) {
          list(
            Overview = results$data_tables$ppm_summary,
            Control_Chart = results$data_tables$points,
            Raw_Data = results$data_tables$raw_data
          )
        }
      )
      reporter$export(
        results = self$results, plan = self$plan,
        task_tag = "attribute_capability",
        format = format, path = path, ...)
      invisible(self)
    }
  ),
  private = list(
    defects = NULL,
    sample_sizes = NULL
  )
)

# =============================================================================
# Convenience wrappers (Minitab-style one-shot API)
# =============================================================================

#' Binomial process capability analysis
#'
#' Performs attribute capability analysis for pass/fail (defective)
#' data under the binomial assumption. Estimates the proportion
#' defective `p`, computes a Wilson (or Clopper-Pearson) CI, PPM, Z.Bench /
#' sigma level, a p-chart for stability, and a dispersion test for the
#' binomial assumption. This is the Minitab "Binomial Capability Analysis"
#' equivalent.
#'
#' @param data Data frame.
#' @param defects Column name for the count of defectives per subgroup.
#' @param sample_sizes Column name for the subgroup sizes (n_i).
#' @param target_proportion Optional target maximum proportion defective.
#' @param ci_method `"wilson"` (default) or `"clopper_pearson"`.
#' @param z_shift Sigma shift convention (default 1.5, Six Sigma).
#' @param conf_level Confidence level.
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrAttributeCapabilityTask` object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(batch = 1:30, defects = rbinom(30, 100, 0.02),
#'                  n = rep(100, 30))
#' task <- capability_binomial(data = df, defects = "defects",
#'                             sample_sizes = "n",
#'                             target_proportion = 0.02)
#' task$summary()
#' task$plot(type = "full")
#' }
capability_binomial <- function(data, defects, sample_sizes,
                                target_proportion = NULL,
                                ci_method = c("wilson", "clopper_pearson"),
                                z_shift = 1.5,
                                conf_level = 0.95,
                                theme = "academic", ...) {
  ci_method <- if (missing(ci_method)) "wilson" else match.arg(ci_method)
  plan <- AttributeCapabilityPlan$new(
    analysis_type = "binomial",
    target_proportion = target_proportion,
    ci_method = ci_method,
    z_shift = z_shift,
    conf_level = conf_level
  )
  task <- IqrAttributeCapabilityTask$new(
    data = data, defects = defects,
    sample_sizes = sample_sizes, plan = plan, theme = theme, ...
  )
  task$compute()
  invisible(task)
}

#' Poisson process capability analysis
#'
#' Performs attribute capability analysis for defect count data under the
#' Poisson assumption. Estimates the rate `u` (defects per unit), computes
#' an exact (Garwood) CI, DPMO, Z.Bench / sigma level, a u-chart for
#' stability, and a dispersion test for the Poisson assumption. This is
#' the Minitab "Poisson Capability Analysis" equivalent.
#'
#' @param data Data frame.
#' @param defects Column name for the count of defects per subgroup.
#' @param sample_sizes Column name for the exposure (units / opportunities)
#'   per subgroup.
#' @param target_rate Optional target maximum defects per unit.
#' @param z_shift Sigma shift convention (default 1.5, Six Sigma).
#' @param conf_level Confidence level.
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrAttributeCapabilityTask` object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(roll = 1:30, defects = rpois(30, 2),
#'                  meters = rep(50, 30))
#' task <- capability_poisson(data = df, defects = "defects",
#'                            sample_sizes = "meters",
#'                            target_rate = 0.05)
#' task$summary()
#' task$plot(type = "full")
#' }
capability_poisson <- function(data, defects, sample_sizes,
                               target_rate = NULL,
                               z_shift = 1.5,
                               conf_level = 0.95,
                               theme = "academic", ...) {
  plan <- AttributeCapabilityPlan$new(
    analysis_type = "poisson",
    target_proportion = target_rate,
    z_shift = z_shift,
    conf_level = conf_level
  )
  task <- IqrAttributeCapabilityTask$new(
    data = data, defects = defects,
    sample_sizes = sample_sizes, plan = plan, theme = theme, ...
  )
  task$compute()
  invisible(task)
}
