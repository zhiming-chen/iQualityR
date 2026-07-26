# =============================================================================
# File: R/capability/DistributionFitter.R
# Description: Distribution fitting and diagnostics tool (capability analysis helper)
# Status: Phase 2 - Supports 7 continuous distributions + auto-selection
# =============================================================================

#' @title DistributionFitter
#'
#' @description Distribution fitting and diagnostics tool for non-normal
#' capability analysis. Used for distribution identification, parameter
#' estimation, and goodness-of-fit testing.
#'
#' Supports distributions: Normal, Weibull, Lognormal, Gamma, Exponential,
#' Logistic, Beta.
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

    #' @description Fit a single distribution and return parameter estimates and statistics
    #' @param x Numeric vector of data
    #' @param dist_name Distribution name string
    fit_one = function(x, dist_name) {
      tryCatch({
        switch(dist_name,
          "weibull" = self$.fit_weibull(x),
          "lognormal" = self$.fit_lognormal(x),
          "gamma" = self$.fit_gamma(x),
          "exponential" = self$.fit_exponential(x),
          "logistic" = self$.fit_logistic(x),
          "beta" = self$.fit_beta(x),
          "normal" = self$.fit_normal(x),
          stop("Unknown distribution: ", dist_name, call. = FALSE)
        )
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

    #' @description Automatically select the best distribution based on AIC
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
    #' @param theme Optional ggplot theme function; defaults to theme_minimal()
    #' @return ggplot object
    plot_fit_comparison = function(x, plan, theme = NULL) {
      if (is.null(self$best_distribution)) {
        stop("No fit results available. Run fit_auto() first.", call. = FALSE)
      }

      best_fit <- self$fit_results$best
      dist_name <- self$best_distribution

      # Compute histogram
      n_bins <- min(30, max(10, floor(length(x) / 10)))
      h <- hist(x, breaks = n_bins, plot = FALSE)
      max_density <- max(h$density)

      df_hist <- data.frame(x = x)
      breaks <- h$breaks
      density <- h$density
      mids <- h$mids
      df_hist2 <- data.frame(mids = mids, density = density)

      # Compute density of the fitted curve
      x_seq <- seq(min(x), max(x), length.out = 200)
      if (dist_name %in% c("weibull", "lognormal", "gamma", "exponential")) {
        x_seq <- pmax(x_seq, min(x_seq[x_seq > 0]))
      }

      y_fit <- sapply(x_seq, function(xi) {
        self$eval_density(xi, dist_name, best_fit$params)
      })

      df_fit <- data.frame(x = x_seq, density = y_fit)

      p <- ggplot() +
        geom_histogram(data = df_hist, aes(x = x, y = after_stat(density)),
                       bins = n_bins, fill = "#3498DB", alpha = 0.4, color = "#3498DB") +
        geom_line(data = df_fit, aes(x = x, y = density),
                  color = "#E74C3C", linewidth = 1.5) +
        geom_vline(xintercept = plan$lsl, color = "#E74C3C", linewidth = 1.5, linetype = "dashed") +
        geom_vline(xintercept = plan$usl, color = "#E74C3C", linewidth = 1.5, linetype = "dashed") +
        annotate("text", x = Inf, y = Inf,
                 label = sprintf("Best: %s\nAIC = %.1f\nKS p = %.4f",
                                 dist_name, best_fit$aic, best_fit$ks_p_value),
                 vjust = 1.2, hjust = 1.1, size = 3.5,
                 color = "#E74C3C", fontface = "bold") +
        labs(title = "Distribution Fit Comparison",
             x = "Measurement", y = "Density") +
        theme_minimal()

      if (!is.null(theme)) {
        p <- p + theme
      }

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
          dgamma(x_adj, params$shape, params$scale)
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
          qgamma(p_val, params$shape, params$scale) + params$shift
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
          pgamma(x_adj, params$shape, params$scale)
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

    # ---- Private methods: fitting various distributions ----

    .fit_normal = function(x) {
      fit <- list(mean = mean(x), sd = sd(x))
      n <- length(x)
      log_lik <- sum(dnorm(x, fit$mean, fit$sd, log = TRUE))
      ks_test <- ks.test(x, "pnorm", fit$mean, fit$sd)

      list(
        distribution = "normal",
        converged = TRUE,
        params = fit,
        log_likelihood = log_lik,
        aic = 2 * 2 - 2 * log_lik,
        bic = 2 * log(n) - 2 * log_lik,
        ks_statistic = ks_test$statistic,
        ks_p_value = ks_test$p.value
      )
    },

    .fit_weibull = function(x) {
      params <- list()
      if (any(x <= 0)) {
        # Weibull requires positive data; shift to make positive
        shift_offset <- min(x) - 0.001
        x_shift <- x - shift_offset
        params$shift <- shift_offset
      } else {
        x_shift <- x
        params$shift <- 0
      }
      fit <- MASS::fitdistr(x_shift, "weibull")
      params$shape <- fit$estimate["shape"]
      params$scale <- fit$estimate["scale"]
      n <- length(x_shift)
      log_lik <- sum(dweibull(x_shift, params$shape, params$scale, log = TRUE))

      ks_test <- tryCatch(
        ks.test(x_shift, "pweibull", params$shape, params$scale),
        error = function(e) list(statistic = NA_real_, p.value = NA_real_)
      )

      list(
        distribution = "weibull",
        converged = TRUE,
        params = params,
        log_likelihood = log_lik,
        aic = 2 * 2 - 2 * log_lik,
        bic = 2 * log(n) - 2 * log_lik,
        ks_statistic = ks_test$statistic,
        ks_p_value = ks_test$p.value
      )
    },

    .fit_lognormal = function(x) {
      if (any(x <= 0)) {
        stop("Lognormal distribution requires positive data", call. = FALSE)
      }
      log_x <- log(x)
      fit <- list(meanlog = mean(log_x), sdlog = sd(log_x))
      n <- length(x)
      log_lik <- sum(dlnorm(x, fit$meanlog, fit$sdlog, log = TRUE))

      ks_test <- tryCatch(
        ks.test(x, "plnorm", fit$meanlog, fit$sdlog),
        error = function(e) list(statistic = NA_real_, p.value = NA_real_)
      )

      list(
        distribution = "lognormal",
        converged = TRUE,
        params = fit,
        log_likelihood = log_lik,
        aic = 2 * 2 - 2 * log_lik,
        bic = 2 * log(n) - 2 * log_lik,
        ks_statistic = ks_test$statistic,
        ks_p_value = ks_test$p.value
      )
    },

    .fit_gamma = function(x) {
      params <- list()
      if (any(x <= 0)) {
        # Gamma requires positive data; shift to make positive
        shift_offset <- min(x) - 0.001
        x_shift <- x - shift_offset
        params$shift <- shift_offset
      } else {
        x_shift <- x
        params$shift <- 0
      }
      fit <- MASS::fitdistr(x_shift, "gamma")
      params$shape <- fit$estimate["shape"]
      params$scale <- fit$estimate["scale"]
      n <- length(x_shift)
      log_lik <- sum(dgamma(x_shift, params$shape, params$scale, log = TRUE))

      ks_test <- tryCatch(
        ks.test(x_shift, "pgamma", params$shape, params$scale),
        error = function(e) list(statistic = NA_real_, p.value = NA_real_)
      )

      list(
        distribution = "gamma",
        converged = TRUE,
        params = params,
        log_likelihood = log_lik,
        aic = 2 * 2 - 2 * log_lik,
        bic = 2 * log(n) - 2 * log_lik,
        ks_statistic = ks_test$statistic,
        ks_p_value = ks_test$p.value
      )
    },

    .fit_exponential = function(x) {
      if (any(x <= 0)) {
        stop("Exponential distribution requires positive data", call. = FALSE)
      }
      fit <- list(rate = 1 / mean(x))
      n <- length(x)
      log_lik <- sum(dexp(x, fit$rate, log = TRUE))

      ks_test <- tryCatch(
        ks.test(x, "pexp", fit$rate),
        error = function(e) list(statistic = NA_real_, p.value = NA_real_)
      )

      list(
        distribution = "exponential",
        converged = TRUE,
        params = fit,
        log_likelihood = log_lik,
        aic = 2 * 1 - 2 * log_lik,
        bic = 1 * log(n) - 2 * log_lik,
        ks_statistic = ks_test$statistic,
        ks_p_value = ks_test$p.value
      )
    },

    .fit_logistic = function(x) {
      fit <- MASS::fitdistr(x, "logistic")
      fit <- list(location = fit$estimate["location"], scale = fit$estimate["scale"])
      n <- length(x)
      log_lik <- sum(dlogis(x, fit$location, fit$scale, log = TRUE))

      ks_test <- tryCatch(
        ks.test(x, "plogis", fit$location, fit$scale),
        error = function(e) list(statistic = NA_real_, p.value = NA_real_)
      )

      list(
        distribution = "logistic",
        converged = TRUE,
        params = fit,
        log_likelihood = log_lik,
        aic = 2 * 2 - 2 * log_lik,
        bic = 2 * log(n) - 2 * log_lik,
        ks_statistic = ks_test$statistic,
        ks_p_value = ks_test$p.value
      )
    },

    .fit_beta = function(x) {
      data_range <- range(x)
      if (data_range[2] - data_range[1] < 1e-10) {
        stop("Data range too small for Beta distribution", call. = FALSE)
      }
      # Normalize to [0, 1]
      scaled_x <- (x - data_range[1]) / (data_range[2] - data_range[1])
      scaled_x <- pmin(pmax(scaled_x, 1e-6), 1 - 1e-6)  # Avoid boundaries

      fit <- MASS::fitdistr(scaled_x, "beta", start = list(shape1 = 1, shape2 = 1))
      fit <- list(shape1 = fit$estimate["shape1"], shape2 = fit$estimate["shape2"], range = data_range)
      n <- length(scaled_x)
      log_lik <- sum(dbeta(scaled_x, fit$shape1, fit$shape2, log = TRUE))

      # KS test needs adjustment
      ks_test <- tryCatch(
        ks.test(scaled_x, "pbeta", fit$shape1, fit$shape2),
        error = function(e) list(statistic = NA_real_, p.value = NA_real_)
      )

      list(
        distribution = "beta",
        converged = TRUE,
        params = fit,
        log_likelihood = log_lik,
        aic = 2 * 2 - 2 * log_lik,
        bic = 2 * log(n) - 2 * log_lik,
        ks_statistic = ks_test$statistic,
        ks_p_value = ks_test$p.value
      )
    }
  )
)
