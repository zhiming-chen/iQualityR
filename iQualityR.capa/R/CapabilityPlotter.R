# =============================================================================
# File: R/CapabilityPlotter.R
# Description: Capability analysis plot coordinator (inherits IqrPlotterBase).
#              Thin delegation layer: extracts parameters from the analysis
#              result and dispatches to iQualityR.plot::plot_capability_*.
#              No ggplot2 geom_* / stat_* logic lives here.
# =============================================================================

#' CapabilityPlotter
#'
#' @title CapabilityPlotter
#'
#' @description Thin delegation plotter for process capability analysis.
#'   Inherits `IqrPlotterBase` and dispatches every panel to the matching
#'   `iQualityR.plot::plot_capability_*` function. The Sixpack layout follows
#'   the Minitab 3x2 convention (stability row, capability row, indices row).
#'
#'   All visualization logic (geom composition, color resolution, reference
#'   lines) lives in `iQualityR.plot`; this class only extracts data from the
#'   `results` object and forwards it with a resolved theme.
#'
#' @param results Analysis results list returned by `CapabilityAnalyzer`.
#'   Must contain `data_tables$sixpack`, `statistics`, `diagnostics`.
#' @param theme_obj An `IqrTheme` object (or NULL for the global default).
#' @param type Plot type: `"full"` (Sixpack), `"basic"` (histogram),
#'   `"qq"`, `"capbar"`, `"individual"`, `"mr"`, `"trend"`.
#' @param plan A `CapabilityPlan` object providing spec limits and
#'   `analysis_type`. May be NULL (type is auto-detected from diagnostics).
#' @param ... Additional arguments (unused).
#'
#' @export
CapabilityPlotter <- R6::R6Class("CapabilityPlotter",
  inherit = IqrPlotterBase,
  public = list(

    #' @description Render a plot by type.
    #' @param results Analysis results list.
    #' @param theme_obj IqrTheme object.
    #' @param type Plot type (see class description).
    #' @param plan CapabilityPlan object.
    #' @param ... Additional arguments (unused).
    render = function(results, theme_obj, type = "full", plan = NULL, ...) {
      analysis_type <- self$.detect_analysis_type(results, plan)
      switch(type,
        basic      = self$.plot_histogram(results, theme_obj, plan, analysis_type),
        qq         = self$.plot_qq(results, theme_obj, analysis_type),
        capbar     = self$.plot_capability_forest(results, theme_obj),
        forest     = self$.plot_capability_forest(results, theme_obj),
        individual = self$.plot_individual(results, theme_obj, plan),
        xbar       = self$.plot_xbar(results, theme_obj, plan),
        mr         = self$.plot_moving_range(results, theme_obj),
        rchart     = self$.plot_r_chart(results, theme_obj),
        trend      = self$.plot_trend(results, theme_obj, plan),
        full       = self$.plot_sixpack(results, theme_obj, plan, analysis_type),
        stop("Unknown plot type: ", type, call. = FALSE)
      )
    },

    # -----------------------------------------------------------------------
    # Sixpack (full): 6 panels arranged as 3 rows x 2 columns.
    # Stability row (panels 1-2): Xbar-R when subgrouped, I-MR when individual.
    # -----------------------------------------------------------------------

    .plot_sixpack = function(results, theme_obj, plan, analysis_type) {
      sp <- results$data_tables$sixpack
      # Choose stability panels: Xbar-R when subgroup data exists, else I-MR.
      # panel names must match what plot_capability_sixpack() expects
      # (individual/moving_range/index_bar) — the content can be Xbar/R/forest.
      has_xbar <- !is.null(sp$xbar_chart) && !is.null(sp$r_chart)
      if (has_xbar) {
        stability_1 <- self$.plot_xbar(results, theme_obj, plan)
        stability_2 <- self$.plot_r_chart(results, theme_obj)
      } else {
        stability_1 <- self$.plot_individual(results, theme_obj, plan)
        stability_2 <- self$.plot_moving_range(results, theme_obj)
      }
      panels <- list(
        individual  = stability_1,
        moving_range = stability_2,
        histogram   = self$.plot_histogram(results, theme_obj, plan, analysis_type),
        qq          = self$.plot_qq(results, theme_obj, analysis_type),
        index_bar   = self$.plot_capability_forest(results, theme_obj),
        trend       = self$.plot_trend(results, theme_obj, plan),
        process_table = self$.plot_process_table(results, theme_obj)
      )
      iQualityR.plot::plot_capability_sixpack(
        panels   = panels,
        title    = "Process Capability Sixpack",
        subtitle = self$.build_sixpack_subtitle(results, analysis_type),
        theme    = theme_obj
      )
    },

    # -----------------------------------------------------------------------
    # Panel 1a: Individual values chart (I chart) — fallback for individuals
    # -----------------------------------------------------------------------

    .plot_individual = function(results, theme_obj, plan) {
      sp <- results$data_tables$sixpack
      specs <- self$.effective_specs(results, plan)
      iQualityR.plot::plot_capability_individual_chart(
        data = sp$individual, mean_val = sp$individual$mean[1],
        specs = specs, theme = theme_obj
      )
    },

    # -----------------------------------------------------------------------
    # Panel 1b: Xbar chart — used when rational subgroups exist
    # -----------------------------------------------------------------------

    .plot_xbar = function(results, theme_obj, plan) {
      sp <- results$data_tables$sixpack
      specs <- self$.effective_specs(results, plan)
      iQualityR.plot::plot_capability_xbar_chart(
        data = sp$xbar_chart, specs = specs, theme = theme_obj
      )
    },

    # -----------------------------------------------------------------------
    # Panel 2a: Moving range chart — fallback for individuals
    # -----------------------------------------------------------------------

    .plot_moving_range = function(results, theme_obj) {
      sp <- results$data_tables$sixpack
      iQualityR.plot::plot_capability_moving_range(data = sp$moving_range,
                                                   theme = theme_obj)
    },

    # -----------------------------------------------------------------------
    # Panel 2b: R chart — used when rational subgroups exist
    # -----------------------------------------------------------------------

    .plot_r_chart = function(results, theme_obj) {
      sp <- results$data_tables$sixpack
      iQualityR.plot::plot_capability_r_chart(data = sp$r_chart,
                                              theme = theme_obj)
    },

    # -----------------------------------------------------------------------
    # Panel 5: Capability indices forest plot (replaces bar chart)
    # -----------------------------------------------------------------------

    .plot_capability_forest = function(results, theme_obj) {
      stats <- results$statistics
      # Build forest-plot data.frame: name, value, lower, upper
      idx_names <- c("Cp", "Cpk", "Pp", "Ppk")
      idx_vals  <- c(stats$cp, stats$cpk, stats$pp, stats$ppk)
      # Bootstrap CI (if available)
      boot <- results$data_tables$bootstrap_ci
      lower <- rep(NA_real_, 4)
      upper <- rep(NA_real_, 4)
      if (!is.null(boot)) {
        for (i in seq_along(idx_names)) {
          row <- boot[boot$Statistic == idx_names[i], ]
          if (nrow(row) > 0) {
            lower[i] <- row$Lower_CI[1]
            upper[i] <- row$Upper_CI[1]
          }
        }
      }
      indices_df <- data.frame(
        name  = idx_names,
        value = idx_vals,
        lower = lower,
        upper = upper,
        stringsAsFactors = FALSE
      )
      iQualityR.plot::plot_capability_index_forest(indices = indices_df,
                                                    theme = theme_obj)
    },

    # -----------------------------------------------------------------------
    # Panel 3: Histogram (dual normal curves / fitted curve)
    # -----------------------------------------------------------------------

    .plot_histogram = function(results, theme_obj, plan, analysis_type = NULL) {
      if (is.null(analysis_type)) {
        analysis_type <- self$.detect_analysis_type(results, plan)
      }
      sp <- results$data_tables$sixpack
      hist_data <- sp$histogram
      specs <- self$.effective_specs(results, plan)

      # Build the fitted-distribution curve overlay for non-normal analyses.
      fitted_curve <- NULL
      if (analysis_type == "nonnormal" &&
          !is.null(hist_data$distribution) &&
          !is.null(hist_data$params) &&
          !is.null(hist_data$fitter)) {
        fitted_curve <- self$.build_fitted_curve(
          hist_data$values, hist_data$distribution,
          hist_data$params, hist_data$fitter
        )
      }

      # Between/Within analysis: surface the between-subgroup sigma.
      sd_between <- results$statistics$sd_between
      if (is.null(sd_between) || !is.finite(sd_between)) sd_between <- NULL

      subtitle <- self$.build_histogram_subtitle(results, analysis_type, hist_data)
      iQualityR.plot::plot_capability_histogram(
        values       = hist_data$values,
        mean_val     = hist_data$mean,
        sd_within    = hist_data$sd_within %||% results$statistics$sd_within,
        sd_overall   = results$statistics$sd_overall,
        sd_between   = sd_between,
        specs        = specs,
        target       = hist_data$target,
        fitted_curve = fitted_curve,
        subtitle_text = subtitle,
        theme        = theme_obj
      )
    },

    # -----------------------------------------------------------------------
    # Panel 4: Q-Q plot (normal / fitted / empirical)
    # -----------------------------------------------------------------------

    .plot_qq = function(results, theme_obj, analysis_type = NULL) {
      if (is.null(analysis_type)) {
        analysis_type <- self$.detect_analysis_type(results, plan = NULL)
      }
      sp <- results$data_tables$sixpack
      diag <- results$diagnostics
      subtitle <- self$.build_qq_subtitle(diag, analysis_type)

      if (analysis_type == "nonnormal") {
        pp <- sp$pp_plot %||% sp$fit_comparison
        if (!is.null(pp) && !is.null(pp$distribution) &&
            !is.null(pp$params) && !is.null(pp$fitter)) {
          return(iQualityR.plot::plot_capability_qq_fitted(
            values = pp$values, dist_name = pp$distribution,
            params = pp$params, fitter = pp$fitter,
            subtitle_text = subtitle, theme = theme_obj))
        }
      }
      if (analysis_type == "nonparametric") {
        qq_data <- sp$qq_plot %||% sp$edf_plot
        if (!is.null(qq_data) && !is.null(qq_data$values)) {
          return(iQualityR.plot::plot_capability_qq_empirical(
            values = qq_data$values, subtitle_text = subtitle, theme = theme_obj))
        }
      }
      # Default normal Q-Q with 95% CI band (replaces plain plot_capability_qq).
      raw <- results$data_tables$raw_data %||% results$raw_data
      ad <- diag$normality_test
      ad_stat <- if (!is.null(ad)) ad$statistic else NULL
      ad_p <- if (!is.null(ad)) ad$p.value else NULL
      iQualityR.plot::plot_capability_qq_ci(
        values = raw$measurement, ad_stat = ad_stat, ad_p = ad_p,
        subtitle_text = subtitle, theme = theme_obj
      )
    },

    # -----------------------------------------------------------------------
    # Panel 6: Last 25 Subgroups — all observations scatter plot
    # -----------------------------------------------------------------------

    .plot_trend = function(results, theme_obj, plan) {
      sp <- results$data_tables$sixpack
      obs_data <- sp$last_observations
      specs <- self$.effective_specs(results, plan)
      lsl <- if (!is.null(specs)) specs$lsl else plan$lsl
      usl <- if (!is.null(specs)) specs$usl else plan$usl
      target <- plan$target
      iQualityR.plot::plot_capability_last_observations(
        data = obs_data, lsl = lsl, usl = usl, target = target,
        theme = theme_obj
      )
    },

    # -----------------------------------------------------------------------
    # Panel 7: Process data & capability summary table (6+1 layout)
    # -----------------------------------------------------------------------

    .plot_process_table = function(results, theme_obj) {
      stats <- self$.build_process_table_stats(results)
      iQualityR.plot::plot_capability_process_table(stats = stats,
                                                    theme = theme_obj)
    },

    # Build the stats list expected by plot_capability_process_table
    .build_process_table_stats = function(results) {
      s <- results$statistics
      sp <- results$data_tables$sixpack
      # Try to recover within/overall SD
      sd_within <- s$sd_within %||% s$sigma_within %||% NULL
      sd_overall <- s$sd_overall %||% s$sigma_overall %||% s$sd %||% NULL
      # Bootstrap CI (if available)
      boot <- results$data_tables$bootstrap_ci
      get_ci <- function(name) {
        if (is.null(boot)) return(NULL)
        row <- boot[boot$Statistic == name, ]
        if (nrow(row) > 0) c(row$Lower_CI[1], row$Upper_CI[1]) else NULL
      }
      list(
        mean = s$mean,
        sd_within = sd_within,
        sd_overall = sd_overall,
        variance_within = if (!is.null(sd_within)) sd_within^2 else NULL,
        variance_overall = if (!is.null(sd_overall)) sd_overall^2 else NULL,
        n_subgroups = if (!is.null(sp$xbar_chart)) nrow(sp$xbar_chart) else NULL,
        subgroup_size = sp$subgroup_size %||% NULL,
        n_obs = length(results$data_tables$raw_data$measurement %||%
                       results$raw_data$measurement),
        cp = s$cp, cpk = s$cpk, pp = s$pp, ppk = s$ppk,
        ccpk = s$ccpk, k = s$k %||% (if (!is.null(s$cp) && !is.null(s$cpk) &&
                                        s$cp > 0) s$cpk / s$cp else NULL),
        cp_ci = get_ci("Cp"), cpk_ci = get_ci("Cpk"),
        pp_ci = get_ci("Pp"), ppk_ci = get_ci("Ppk"),
        ccpk_ci = get_ci("CCpk"),
        ppm_obs = s$ppm_observed,
        ppm_exp_within = s$ppm_expected_within %||% s$ppm_expected,
        ppm_exp_overall = s$ppm_expected_overall %||% s$ppm_expected,
        yield_within = s$yield_within %||%
          (if (!is.null(s$ppm_expected_within))
            1 - s$ppm_expected_within / 1e6 else NULL),
        yield_overall = s$yield_overall %||%
          (if (!is.null(s$ppm_expected_overall))
            1 - s$ppm_expected_overall / 1e6 else NULL),
        z_bench_within = s$z_bench_within %||% s$z_bench,
        z_bench_overall = s$z_bench_overall %||% s$z_bench,
        sigma_level = s$sigma_level,
        overall_verdict = results$diagnostics$capability_judgment$overall_verdict %||%
          s$overall_verdict
      )
    },

    # -----------------------------------------------------------------------
    # Helpers: parameter extraction only (no plotting)
    # -----------------------------------------------------------------------

    .detect_analysis_type = function(results, plan) {
      if (!is.null(plan) && !is.null(plan$analysis_type)) {
        return(plan$analysis_type)
      }
      stats <- results$statistics
      if (!is.null(stats$sd_between_within)) return("between_within")
      diag <- results$diagnostics
      if (!is.null(diag$distribution)) return("nonnormal")
      if (!is.null(diag$method) && diag$method %in% c("kernel", "empirical")) {
        return("nonparametric")
      }
      "normal"
    },

    # Resolve the spec limits that should be drawn. When a transform was
    # applied the data is on the transformed scale, so specs must be the
    # transformed specs stored in diagnostics.
    .effective_specs = function(results, plan) {
      diag <- results$diagnostics
      if (!is.null(diag$transform_applied) &&
          !is.null(diag$transformed_lsl) &&
          !is.null(diag$transformed_usl)) {
        return(list(lsl = diag$transformed_lsl, usl = diag$transformed_usl))
      }
      if (!is.null(plan) && !is.null(plan$lsl) && !is.null(plan$usl)) {
        return(list(lsl = plan$lsl, usl = plan$usl))
      }
      NULL
    },

    # Build a data.frame of (x, density) for the fitted distribution curve.
    .build_fitted_curve = function(values, dist_name, params, fitter) {
      x_range <- range(values, na.rm = TRUE)
      span <- diff(x_range)
      x_seq <- seq(x_range[1] - 0.05 * span, x_range[2] + 0.05 * span,
                   length.out = 200)
      y_fit <- vapply(x_seq, function(xi) {
        fitter$eval_density(xi, dist_name, params)
      }, numeric(1))
      data.frame(x = x_seq, density = y_fit)
    },

    # Convert the bootstrap CI table (if present) to the ci_df format expected
    # by plot_capability_index_bar. Only indices present in both are matched.
    .build_ci_df = function(results, indices_df) {
      boot <- results$data_tables$bootstrap_ci
      if (is.null(boot)) return(NULL)
      matched <- merge(
        data.frame(Index = indices_df$Index),
        data.frame(Index = boot$Statistic, Lower = boot$Lower_CI,
                   Upper = boot$Upper_CI),
        by = "Index", sort = FALSE
      )
      if (nrow(matched) == 0) return(NULL)
      matched
    },

    .build_qq_subtitle = function(diag, analysis_type) {
      if (analysis_type == "nonnormal") {
        dist <- diag$distribution %||% "fitted"
        ks_p <- diag$ks_p_value
        if (is.null(ks_p) || is.na(ks_p)) {
          return(sprintf("Fitted Q-Q | Dist: %s", dist))
        }
        return(sprintf("Fitted Q-Q | Dist: %s (KS p = %.4f)", dist, ks_p))
      }
      if (analysis_type == "nonparametric") {
        return("Empirical Q-Q (rankit)")
      }
      method <- diag$normality_method %||% "Normality test"
      p_val  <- diag$normality_p_value
      if (is.null(p_val) || is.na(p_val)) {
        return(sprintf("%s (p-value not computed)", method))
      }
      sprintf("%s p = %.4f", method, p_val)
    },

    .build_histogram_subtitle = function(results, analysis_type, hist_data) {
      stats <- results$statistics
      base <- sprintf("Cp = %.2f, Cpk = %.2f", stats$cp, stats$cpk)
      if (analysis_type == "nonnormal" && !is.null(hist_data$distribution)) {
        base <- sprintf("%s | Dist: %s", base, hist_data$distribution)
      }
      transform_note <- self$.build_transform_note(results)
      if (!is.null(transform_note)) {
        base <- sprintf("%s | %s", base, transform_note)
      }
      base
    },

    .build_sixpack_subtitle = function(results, analysis_type) {
      stats <- results$statistics
      base <- sprintf("Cp = %.2f | Cpk = %.2f | Pp = %.2f | Ppk = %.2f | n = %d",
                      stats$cp, stats$cpk, stats$pp, stats$ppk, stats$n)
      if (analysis_type == "nonnormal") {
        dist <- results$diagnostics$distribution %||% "auto"
        return(paste0(base, " | Non-normal: ", dist))
      }
      if (analysis_type == "nonparametric") {
        return(paste0(base, " | Non-parametric"))
      }
      if (analysis_type == "between_within") {
        return(paste0(base, " | Between/Within"))
      }
      transform_note <- self$.build_transform_note(results)
      if (!is.null(transform_note)) {
        return(paste0(base, " | Normal | ", transform_note))
      }
      paste0(base, " | Normal")
    },

    # Short human-readable annotation for an applied transform.
    .build_transform_note = function(results) {
      diag <- results$diagnostics
      method <- diag$transform_applied
      if (is.null(method)) return(NULL)
      lambda <- diag$transform_lambda
      type <- diag$transform_type
      if (!is.null(lambda) && is.finite(lambda)) {
        return(sprintf("%s (lambda = %.4f)", method, lambda))
      }
      if (!is.null(type)) {
        return(sprintf("%s (%s)", method, type))
      }
      method
    }
  )
)
