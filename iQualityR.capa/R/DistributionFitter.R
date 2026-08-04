# =============================================================================
# File: R/capability/DistributionFitter.R
# Description: Distribution fitting and diagnostics tool (capability analysis helper)
# Status: Refactored to delegate fitting to iQualityR.stat::fit_distribution
# =============================================================================

#' @title DistributionFitter
#'
#' @description Distribution fitting and diagnostics tool for non-normal
#' capability analysis. Used for distribution identification, parameter
#' estimation, and goodness-of-fit testing.
#'
#' Delegates all distribution fitting to [iQualityR.stat::fit_distribution] so
#' that parameter estimation, AIC/BIC, and Kolmogorov-Smirnov goodness-of-fit
#' are computed consistently with the rest of the iQualityR ecosystem. Adds
#' capa-specific conveniences on top:
#' - Automatic data shifting for gamma/weibull when the data contains
#'   non-positive values (preserves the `params$shift` convention).
#' - Data normalization for beta (preserves the `params$range` convention).
#' - `eval_density` / `eval_quantile` / `eval_cdf` helpers for capability
#'   index and PPM computation.
#'
#' Supported distributions (capa names): normal, weibull, lognormal, gamma,
#' exponential, logistic, beta.
#'
#' @field fit_results List holding all fit results after fit_auto()
#' @field best_distribution Name of the best-fitting distribution after fit_auto()
#'
#' @param x Numeric vector of data
#' @param dist_name Distribution name string
#' @param candidate_dists Character vector of candidate distribution names
#' @param plan CapabilityPlan object
#' @param p_val Probability value for quantile calculation
#' @param params Distribution parameters list
#' @param theme Optional ggplot theme function for future theme unification
#' @param ... Additional arguments
#'
#' @export
DistributionFitter <- R6::R6Class("DistributionFitter",
  public = list(
    fit_results = list(),
    best_distribution = NULL,

    #' @description Fit a single distribution and return parameter estimates and
    #' statistics. Delegates to [iQualityR.stat::fit_distribution] with
    #' capa-specific data preprocessing (shift for gamma/weibull, normalize for
    #' beta) and result-shape conversion.
    #' @param x Numeric vector of data
    #' @param dist_name Distribution name string (capa convention)
    fit_one = function(x, dist_name) {
      tryCatch({
        stat_name <- self$.capa_to_stat_dist(dist_name)
        prep <- self$.preprocess_data(x, dist_name)
        stat_result <- iQualityR.stat::fit_distribution(prep$x, dist = stat_name)
        self$.convert_stat_to_capa(stat_result, dist_name, prep)
      }, error = function(e) {
        list(
          distribution = dist_name,
          converged = FALSE,
          error = conditionMessage(e),
          params = list(),
          log_likelihood = -Inf,
          aic = Inf,
          bic = Inf,
          ks_statistic = NA_real_,
          ks_p_value = NA_real_
        )
      })
    },

    #' @description Automatically select the best distribution based on AIC.
    #' Iterates over candidates calling fit_one() (which delegates to
    #' iQualityR.stat::fit_distribution) and selects the lowest AIC.
    #' @param x Numeric vector of data
    #' @param candidate_dists Character vector of candidate distribution names
    #' @return List containing the best distribution and all fit results
    fit_auto = function(x, candidate_dists = NULL) {
      if (is.null(candidate_dists)) {
        candidate_dists <- c("weibull", "lognormal", "gamma", "exponential", "logistic")
      }

      all_fits <- list()
      for (dist_name in candidate_dists) {
        fit <- self$fit_one(x, dist_name)
        all_fits[[dist_name]] <- fit
      }

      # Select the distribution with the smallest AIC
      valid_fits <- Filter(function(f) f$converged, all_fits)
      if (length(valid_fits) == 0) {
        warning("All distribution fits failed. Falling back to normal.", call. = FALSE)
        normal_fit <- self$fit_one(x, "normal")
        self$best_distribution <- "normal"
        self$fit_results <- list(all_fits = all_fits, best = normal_fit)
        return(normal_fit)
      }

      aic_values <- sapply(valid_fits, function(f) f$aic)
      best_name <- names(which.min(aic_values))

      self$best_distribution <- best_name
      self$fit_results <- list(all_fits = all_fits, best = valid_fits[[best_name]])

      valid_fits[[best_name]]
    },

    #' @description Plot distribution fit comparison
    #' @param x Numeric vector of data
    #' @param plan CapabilityPlan object
    #' @param theme Optional theme spec (NULL / string / function / IqrTheme).
    #'   Resolved via `iQualityR.plot::as_iqr_theme_object()` so the resulting
    #'   plot follows the active `IqrTheme` consistently, matching the look of
    #'   other iQualityR subpackages. A bare ggplot2 theme function is also
    #'   accepted for backward compatibility and added on top of the IqrTheme.
    #' @return ggplot object
    plot_fit_comparison = function(x, plan, theme = NULL) {
      if (is.null(self$best_distribution)) {
        stop("No fit results available. Run fit_auto() first.", call. = FALSE)
      }

      best_fit <- self$fit_results$best
      dist_name <- self$best_distribution

      # Resolve theme to a full IqrTheme object (handles NULL/string/function/IqrTheme)
      theme_obj <- iQualityR.plot::as_iqr_theme_object(theme)
      c <- .iqr_aes_local(theme_obj)

      # Compute density of the fitted curve over the data range
      x_range <- range(x, na.rm = TRUE)
      span <- diff(x_range)
      x_seq <- seq(x_range[1] - 0.05 * span,
                   x_range[2] + 0.05 * span,
                   length.out = 200)
      y_fit <- vapply(x_seq, function(xi) {
        self$eval_density(xi, dist_name, best_fit$params)
      }, numeric(1))
      df_fit <- data.frame(x = x_seq, density = y_fit)

      df_hist <- data.frame(measurement = x)

      p <- iQualityR.plot::base_plot(
        df_hist,
        ggplot2::aes(x = measurement),
        theme = theme_obj
      ) +
        iQualityR.plot::layers_histogram_density(bins = 30, theme = theme_obj) +
        ggplot2::geom_line(
          data = df_fit,
          ggplot2::aes(x = x, y = density),
          color = c$fail, linewidth = 1.2,
          inherit.aes = FALSE
        ) +
        iQualityR.plot::layers_spec_limits(
          lsl = plan$lsl, usl = plan$usl, theme = theme_obj
        ) +
        ggplot2::annotate(
          "text", x = Inf, y = Inf,
          label = sprintf("Best: %s\nAIC = %.1f\nKS p = %.4f",
                          dist_name, best_fit$aic, best_fit$ks_p_value),
          vjust = 1.2, hjust = 1.1, size = 3.5,
          color = c$fail, fontface = "bold"
        ) +
        ggplot2::labs(
          title = "Distribution Fit Comparison",
          subtitle = sprintf("Fitted distribution: %s", dist_name),
          x = "Measurement", y = "Density"
        )

      p
    },

    #' @description Compute density value at specified points
    #' @param x Numeric vector of data
    #' @param dist_name Distribution name string
    #' @param params Distribution parameters list
    eval_density = function(x, dist_name, params) {
      switch(dist_name,
        "normal" = dnorm(x, params$mean, params$sd),
        "weibull" = {
          x_adj <- x - params$shift
          dweibull(x_adj, params$shape, params$scale)
        },
        "lognormal" = dlnorm(x, params$meanlog, params$sdlog),
        "gamma" = {
          x_adj <- x - params$shift
          dgamma(x_adj, params$shape, params$rate)
        },
        "exponential" = dexp(x, params$rate),
        "logistic" = dlogis(x, params$location, params$scale),
        "beta" = {
          range <- params$range
          scaled_x <- (x - range[1]) / (range[2] - range[1])
          if (scaled_x <= 0 || scaled_x >= 1) return(0)
          dbeta(scaled_x, params$shape1, params$shape2) / (range[2] - range[1])
        },
        0
      )
    },

    #' @description Compute quantile
    #' @param p_val Probability value for quantile calculation
    #' @param dist_name Distribution name string
    #' @param params Distribution parameters list
    eval_quantile = function(p_val, dist_name, params) {
      switch(dist_name,
        "normal" = qnorm(p_val, params$mean, params$sd),
        "weibull" = {
          qweibull(p_val, params$shape, params$scale) + params$shift
        },
        "lognormal" = qlnorm(p_val, params$meanlog, params$sdlog),
        "gamma" = {
          qgamma(p_val, params$shape, params$rate) + params$shift
        },
        "exponential" = qexp(p_val, params$rate),
        "logistic" = qlogis(p_val, params$location, params$scale),
        "beta" = {
          range <- params$range
          range[1] + (range[2] - range[1]) * qbeta(p_val, params$shape1, params$shape2)
        },
        NA_real_
      )
    },

    #' @description Compute cumulative probability
    #' @param x Numeric vector of data
    #' @param dist_name Distribution name string
    #' @param params Distribution parameters list
    eval_cdf = function(x, dist_name, params) {
      switch(dist_name,
        "normal" = pnorm(x, params$mean, params$sd),
        "weibull" = {
          x_adj <- x - params$shift
          pweibull(x_adj, params$shape, params$scale)
        },
        "lognormal" = plnorm(x, params$meanlog, params$sdlog),
        "gamma" = {
          x_adj <- x - params$shift
          pgamma(x_adj, params$shape, params$rate)
        },
        "exponential" = pexp(x, params$rate),
        "logistic" = plogis(x, params$location, params$scale),
        "beta" = {
          range <- params$range
          scaled_x <- (x - range[1]) / (range[2] - range[1])
          pbeta(scaled_x, params$shape1, params$shape2)
        },
        NA_real_
      )
    },

    # ---- Private helpers: distribution name mapping & data preprocessing ----

    # Map capa distribution names to the R/.stat short names expected by
    # iQualityR.stat::fit_distribution (which uses match.arg against R's
    # built-in distribution suffixes).
    .capa_to_stat_dist = function(name) {
      switch(name,
        "normal" = "norm",
        "exponential" = "exp",
        "lognormal" = "lnorm",
        "logistic" = "logis",
        name  # weibull, gamma, beta are the same in both naming conventions
      )
    },

    # Preprocess data for distributions that require a restricted domain.
    # - gamma / weibull require positive data: shift if needed (store shift,
    #   default 0 so eval_* helpers can always do `x - params$shift`).
    # - beta requires data in (0, 1): rescale and clamp (store original range).
    # Other distributions need no preprocessing.
    # Returns list(x = processed_data, shift = shift_value, range = range_or_NULL)
    .preprocess_data = function(x, dist_name) {
      shift <- 0
      range_val <- NULL
      x_proc <- x

      if (dist_name %in% c("gamma", "weibull")) {
        if (any(x <= 0)) {
          shift <- min(x) - 0.001
          x_proc <- x - shift
        }
      } else if (dist_name == "beta") {
        range_val <- range(x)
        if (diff(range_val) < 1e-10) {
          stop("Data range too small for Beta distribution", call. = FALSE)
        }
        x_proc <- (x - range_val[1]) / (range_val[2] - range_val[1])
        x_proc <- pmin(pmax(x_proc, 1e-6), 1 - 1e-6)
      }

      list(x = x_proc, shift = shift, range = range_val)
    },

    # Convert the result shape returned by iQualityR.stat::fit_distribution to
    # capa's contract (distribution/converged/params/log_likelihood/aic/bic/
    # ks_statistic/ks_p_value), re-attaching the shift/range convention so
    # eval_density/eval_quantile/eval_cdf work transparently.
    .convert_stat_to_capa = function(stat_result, capa_name, prep) {
      params <- stat_result$params

      # Preserve capa's shift convention for gamma/weibull (always set,
      # 0 = no shift needed)
      if (capa_name %in% c("gamma", "weibull")) {
        params$shift <- prep$shift
      }

      # Preserve capa's range convention for beta
      if (capa_name == "beta" && !is.null(prep$range)) {
        params$range <- prep$range
      }

      ks_test <- stat_result$ks_test
      ks_stat <- if (is.null(ks_test$statistic) || length(ks_test$statistic) == 0)
        NA_real_ else as.numeric(ks_test$statistic)
      ks_p <- if (is.null(ks_test$p.value) || length(ks_test$p.value) == 0)
        NA_real_ else as.numeric(ks_test$p.value)

      list(
        distribution = capa_name,
        converged = TRUE,
        params = params,
        log_likelihood = stat_result$logLik,
        aic = stat_result$AIC,
        bic = stat_result$BIC,
        ks_statistic = ks_stat,
        ks_p_value = ks_p
      )
    }
  )
)

# -----------------------------------------------------------------------------
# Local helper: resolve IqrTheme semantic colors for plot_fit_comparison().
#
# DistributionFitter does not inherit IqrPlotterBase (it is a standalone tool),
# so we resolve colors through a temporary IqrPlotterBase instance instead of
# calling self$.pal_*. This keeps the plot consistent with the active theme
# without coupling DistributionFitter to the plotter base class.
# -----------------------------------------------------------------------------
.iqr_aes_local <- function(theme_obj) {
  plotter <- iQualityR.core::IqrPlotterBase$new()
  list(
    data    = plotter$.pal_discrete(theme_obj)[1],
    muted   = plotter$.pal_ui(theme_obj, "muted",   default = "#666666"),
    text     = plotter$.pal_ui(theme_obj, "text",    default = "#000000"),
    primary = plotter$.pal_ui(theme_obj, "primary", default = "#2563EB"),
    fail    = plotter$.pal_semantic(theme_obj, "fail"),
    watch   = plotter$.pal_semantic(theme_obj, "watch"),
    pass    = plotter$.pal_semantic(theme_obj, "pass"),
    theme_obj = theme_obj
  )
}
