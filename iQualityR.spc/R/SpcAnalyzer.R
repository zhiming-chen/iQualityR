# =============================================================================
# File: R/SpcAnalyzer.R
# Description: SPC analysis engine (inherits IqrAnalyzerBase)
# =============================================================================

#' @title SpcAnalyzer
#' @description
#' Analyzer for statistical process control charts. Inherits `IqrAnalyzerBase`
#' and dispatches to chart-type specific computation routines. Reuses
#' `iQualityR.stat` for control-limit calculation, sigma estimation, and
#' Nelson rules detection where possible.
#'
#' @param x Numeric vector of measurements (for variables / time-weighted charts).
#' @param subgroup Optional subgroup vector aligned with `x`.
#' @param count Optional count of defectives / defects (for attributes charts).
#' @param sample_size Optional sample-size vector aligned with `count`.
#' @param plan `SpcPlan` object holding chart configuration.
#' @param data Optional data.frame for multivariate charts.
#' @param ... Additional arguments (ignored).
#'
#' @export
SpcAnalyzer <- R6::R6Class("SpcAnalyzer",
  inherit = IqrAnalyzerBase,
  public = list(

    #' @description Run SPC analysis. Overrides base `run()` to accept the
    #' SPC-specific signature.
    #' @param x Numeric vector of measurements (or subgroup means).
    #' @param subgroup Optional subgroup vector.
    #' @param count Optional defect/defective count vector (attributes charts).
    #' @param sample_size Optional sample-size vector (attributes charts).
    #' @param plan SpcPlan object.
    #' @param data Optional data.frame for multivariate charts.
    run = function(x = NULL, subgroup = NULL, count = NULL,
                   sample_size = NULL, plan, data = NULL) {
      self$reset()
      private$.plan <- plan
      chart <- plan$chart_type

      if (chart %in% c("xbar_r", "xbar_s", "imr", "imr_rs")) {
        private$.run_variables(x, subgroup, plan)
      } else if (chart %in% c("p", "np", "u", "c", "p_laney", "u_laney")) {
        private$.run_attributes(count, sample_size, plan)
      } else if (chart %in% c("ewma", "cusum", "ma")) {
        private$.run_time_weighted(x, subgroup, plan)
      } else if (chart %in% c("t2", "mewma", "t2_mewma")) {
        private$.run_multivariate(data, plan)
      } else if (chart %in% c("g", "t")) {
        private$.run_rare_events(x, plan)
      } else if (chart %in% c("adaptive", "aewma")) {
        private$.run_adaptive(x, subgroup, plan)
      } else if (chart == "arima_resid") {
        private$.run_arima_resid(x, plan)
      } else if (chart == "changepoint") {
        private$.run_changepoint(x, plan)
      } else if (chart == "kde") {
        private$.run_kde(x, plan)
      } else if (chart == "lstm") {
        private$.run_lstm(x, plan)
      } else if (chart == "autoencoder") {
        private$.run_autoencoder(data, plan)
      } else if (chart == "iforest") {
        private$.run_iforest(data, plan)
      } else if (chart == "bocpd") {
        private$.run_bocpd(x, plan)
      } else {
        stop("Unsupported chart_type: ", chart, call. = FALSE)
      }
      invisible(self)
    },

    #' @description Get a compact result list suitable for the run_*() contract.
    to_spc_result = function() {
      self$results
    }
  ),

  private = list(
    .plan = NULL,

    # -------------------------------------------------------------------
    # Variables charts: Xbar-R, Xbar-S, I-MR, I-MR-R/S
    # -------------------------------------------------------------------
    .run_variables = function(x, subgroup, plan) {
      if (is.null(x)) stop("x is required for variables charts.", call. = FALSE)
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      if (length(x) < 2) stop("Need at least 2 observations.", call. = FALSE)

      chart <- plan$chart_type
      n_size <- plan$subgroup_size

      # Use iQualityR.stat::calc_control_limits for xbar_r / xbar_s / imr
      stat_chart <- switch(chart,
        xbar_r = "xbar_r", xbar_s = "xbar_s", imr = "imr", imr_rs = "imr")
      cl <- iQualityR.stat::calc_control_limits(
        data = x, subgroup_size = n_size, chart_type = stat_chart)

      # Build point-by-point data frame
      if (chart == "imr" || chart == "imr_rs") {
        points_df <- data.frame(
          index = seq_along(x),
          value = x,
          cl = cl$center,
          ucl = cl$ucl_x,
          lcl = cl$lcl_x,
          stringsAsFactors = FALSE
        )
        mr <- abs(diff(x))
        mr_df <- data.frame(
          index = seq_along(mr),
          value = mr,
          cl = cl$mr_bar,
          ucl = cl$ucl_mr,
          lcl = cl$lcl_mr,
          stringsAsFactors = FALSE
        )
        n_points <- length(x)
        n_subgroups <- n_points
        sigma <- cl$sigma
        sigma_method <- "mr_bar"
      } else {
        # Xbar-R / Xbar-S: reconstruct subgroup statistics
        n_sub <- cl$n_subgroups
        n_subgroups <- n_sub
        xbar <- private$.subgroup_means(x, n_size)
        ranges_or_s <- if (chart == "xbar_r") {
          private$.subgroup_ranges(x, n_size)
        } else {
          private$.subgroup_sds(x, n_size)
        }
        points_df <- data.frame(
          index = seq_len(n_sub),
          value = xbar,
          cl = cl$center,
          ucl = cl$ucl_x,
          lcl = cl$lcl_x,
          stringsAsFactors = FALSE
        )
        if (chart == "xbar_r") {
          mr_df <- data.frame(
            index = seq_len(n_sub),
            value = ranges_or_s,
            cl = cl$r_bar,
            ucl = cl$ucl_r,
            lcl = cl$lcl_r,
            stringsAsFactors = FALSE
          )
        } else {
          mr_df <- data.frame(
            index = seq_len(n_sub),
            value = ranges_or_s,
            cl = cl$s_bar,
            ucl = cl$ucl_s,
            lcl = cl$lcl_s,
            stringsAsFactors = FALSE
          )
        }
        n_points <- n_sub
        sigma <- cl$sigma
        sigma_method <- if (chart == "xbar_r") "r_bar" else "s_bar"
      }

      # Nelson rules detection on the charted statistic
      violations <- iQualityR.stat::detect_spc_violations(
        points_df$value, center = cl$center, sigma = sigma,
        rules = plan$nelson_rules)

      self$set_statistic("center", cl$center)
      self$set_statistic("sigma", sigma)
      self$set_statistic("sigma_method", sigma_method)
      self$set_statistic("ucl", cl$ucl_x)
      self$set_statistic("lcl", cl$lcl_x)
      self$set_statistic("n_points", n_points)
      self$set_statistic("n_subgroups", n_subgroups)
      self$set_statistic("n_violations", violations$n_violations)
      self$set_statistic("is_in_control", violations$is_in_control)
      self$set_statistic("chart_type", chart)
      if (chart == "imr" || chart == "imr_rs") {
        self$set_statistic("mr_bar", cl$mr_bar)
        self$set_statistic("ucl_mr", cl$ucl_mr)
        self$set_statistic("lcl_mr", cl$lcl_mr)
      } else if (chart == "xbar_r") {
        self$set_statistic("r_bar", cl$r_bar)
        self$set_statistic("ucl_r", cl$ucl_r)
        self$set_statistic("lcl_r", cl$lcl_r)
      } else {
        self$set_statistic("s_bar", cl$s_bar)
        self$set_statistic("ucl_s", cl$ucl_s)
        self$set_statistic("lcl_s", cl$lcl_s)
      }

      self$set_diagnostic("chart_type", chart)
      self$set_diagnostic("subgroup_size", n_size)
      self$set_diagnostic("sigma_method", sigma_method)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("dispersion", mr_df)
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary",
        iQualityR.stat::summarize_spc_rules(violations, format = "data.frame"))
    },

    # -------------------------------------------------------------------
    # Attributes charts: P, NP, U, C, Laney P', Laney U'
    # -------------------------------------------------------------------
    .run_attributes = function(count, sample_size, plan) {
      chart <- plan$chart_type
      if (is.null(count)) stop("count is required for attributes charts.", call. = FALSE)
      count <- as.numeric(count)
      if (is.null(sample_size)) sample_size <- rep(1, length(count))
      sample_size <- as.numeric(sample_size)
      keep <- !is.na(count) & !is.na(sample_size) & sample_size > 0
      count <- count[keep]; sample_size <- sample_size[keep]
      n <- length(count)
      if (n < 2) stop("Need at least 2 subgroups for attributes charts.", call. = FALSE)

      proportion <- count / sample_size

      if (chart == "p" || chart == "np" || chart == "p_laney") {
        p_bar <- sum(count) / sum(sample_size)
        center <- if (chart == "np") p_bar * sample_size else p_bar
      } else {
        # u / c / u_laney: defect rate per unit
        u_bar <- sum(count) / sum(sample_size)
        center <- if (chart == "c") rep(mean(count), n) else rep(u_bar, n)
      }

      # Sigma and control limits
      if (chart == "p" || chart == "np") {
        sigma_i <- sqrt(p_bar * (1 - p_bar) / sample_size)
        if (chart == "np") sigma_i <- sqrt(p_bar * (1 - p_bar) * sample_size)
        ucl <- center + 3 * sigma_i
        lcl <- pmax(0, center - 3 * sigma_i)
        sigma_overall <- sqrt(p_bar * (1 - p_bar) / mean(sample_size))
      } else if (chart == "c") {
        c_bar <- mean(count)
        sigma_i <- sqrt(rep(c_bar, n))
        ucl <- center + 3 * sigma_i
        lcl <- pmax(0, center - 3 * sigma_i)
        sigma_overall <- sqrt(c_bar)
      } else if (chart == "u") {
        sigma_i <- sqrt(u_bar / sample_size)
        ucl <- center + 3 * sigma_i
        lcl <- pmax(0, center - 3 * sigma_i)
        sigma_overall <- sqrt(u_bar / mean(sample_size))
      } else if (chart == "p_laney") {
        # Laney P': combine binomial sigma with between-subgroup sigma
        z <- (proportion - p_bar) / sqrt(p_bar * (1 - p_bar) / sample_size)
        sigma_z <- sqrt(mean(z^2))
        sigma_i <- sigma_z * sqrt(p_bar * (1 - p_bar) / sample_size)
        ucl <- p_bar + 3 * sigma_i
        lcl <- pmax(0, p_bar - 3 * sigma_i)
        center <- rep(p_bar, n)
        sigma_overall <- sigma_z * sqrt(p_bar * (1 - p_bar) / mean(sample_size))
      } else if (chart == "u_laney") {
        z <- (proportion - u_bar) / sqrt(u_bar / sample_size)
        sigma_z <- sqrt(mean(z^2))
        sigma_i <- sigma_z * sqrt(u_bar / sample_size)
        ucl <- u_bar + 3 * sigma_i
        lcl <- pmax(0, u_bar - 3 * sigma_i)
        center <- rep(u_bar, n)
        sigma_overall <- sigma_z * sqrt(u_bar / mean(sample_size))
      }

      points_df <- data.frame(
        index = seq_len(n),
        value = if (chart == "np") count else proportion,
        cl = center,
        ucl = ucl,
        lcl = lcl,
        sample_size = sample_size,
        count = count,
        stringsAsFactors = FALSE
      )

      # Nelson rules on standardized z-scores
      z_center <- if (chart == "p" || chart == "p_laney") p_bar
                  else if (chart == "u" || chart == "u_laney") u_bar
                  else mean(count)
      z_sigma <- if (chart %in% c("p", "np")) sqrt(p_bar * (1 - p_bar) / mean(sample_size))
                 else if (chart == "c") sqrt(mean(count))
                 else if (chart == "u") sqrt(u_bar / mean(sample_size))
                 else if (chart == "p_laney") sigma_z * sqrt(p_bar * (1 - p_bar) / mean(sample_size))
                 else sigma_z * sqrt(u_bar / mean(sample_size))
      z_scores <- (points_df$value - z_center) / z_sigma
      violations <- iQualityR.stat::detect_spc_violations(
        z_scores, center = 0, sigma = 1, rules = plan$nelson_rules)

      self$set_statistic("center", z_center)
      self$set_statistic("sigma", sigma_overall)
      self$set_statistic("sigma_method", "total")
      self$set_statistic("ucl", mean(ucl))
      self$set_statistic("lcl", mean(lcl))
      self$set_statistic("n_points", n)
      self$set_statistic("n_violations", violations$n_violations)
      self$set_statistic("is_in_control", violations$is_in_control)
      self$set_statistic("chart_type", chart)
      if (chart == "p" || chart == "np" || chart == "p_laney") {
        self$set_statistic("p_bar", p_bar)
      } else if (chart == "u") {
        self$set_statistic("u_bar", u_bar)
      } else if (chart == "c") {
        self$set_statistic("c_bar", mean(count))
      }
      if (chart %in% c("p_laney", "u_laney")) {
        self$set_statistic("sigma_z", sigma_z)
      }

      self$set_diagnostic("chart_type", chart)
      self$set_diagnostic("sigma_method", "total")
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary",
        iQualityR.stat::summarize_spc_rules(violations, format = "data.frame"))
    },

    # -------------------------------------------------------------------
    # Time-weighted charts: EWMA, CUSUM, MA
    # -------------------------------------------------------------------
    .run_time_weighted = function(x, subgroup, plan) {
      if (is.null(x)) stop("x is required for time-weighted charts.", call. = FALSE)
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      n <- length(x)
      if (n < 3) stop("Need at least 3 observations for time-weighted charts.", call. = FALSE)

      chart <- plan$chart_type

      # Estimate sigma (default: moving range)
      sigma <- private$.estimate_sigma_for(x, subgroup, plan)

      if (chart == "ewma") {
        lambda <- plan$lambda
        z <- numeric(n)
        z[1] <- x[1]
        for (i in 2:n) z[i] <- lambda * x[i] + (1 - lambda) * z[i - 1]
        # Control limits widen at start, converge to steady state
        mr_bar <- mean(abs(diff(x)))
        sigma_z <- sigma * sqrt(lambda / (2 - lambda) * (1 - (1 - lambda)^(2 * seq_len(n))))
        center <- mean(x)
        ucl <- center + 3 * sigma_z
        lcl <- center - 3 * sigma_z
        points_df <- data.frame(
          index = seq_len(n), value = z, cl = center,
          ucl = ucl, lcl = lcl, raw = x, stringsAsFactors = FALSE)
      } else if (chart == "cusum") {
        # Tabular (two-sided) CUSUM
        k <- plan$k
        h <- plan$h
        center <- mean(x)
        dev <- x - center
        c_pos <- numeric(n); c_neg <- numeric(n)
        c_pos[1] <- max(0, dev[1] - k * sigma)
        c_neg[1] <- max(0, -dev[1] - k * sigma)
        for (i in 2:n) {
          c_pos[i] <- max(0, c_pos[i - 1] + dev[i] - k * sigma)
          c_neg[i] <- max(0, c_neg[i - 1] - dev[i] - k * sigma)
        }
        ucl <- rep(h * sigma, n)
        lcl <- rep(0, n)
        points_df <- data.frame(
          index = seq_len(n), value = c_pos, cl = 0,
          ucl = ucl, lcl = lcl,
          cusum_neg = c_neg, raw = x, stringsAsFactors = FALSE)
      } else if (chart == "ma") {
        w <- plan$ma_window
        if (n < w) stop("ma_window larger than data length.", call. = FALSE)
        ma <- stats::filter(x, rep(1 / w, w), sides = 1)
        center <- mean(x)
        sigma_ma <- sigma / sqrt(w)
        ucl <- rep(center + 3 * sigma_ma, n)
        lcl <- rep(center - 3 * sigma_ma, n)
        points_df <- data.frame(
          index = seq_len(n), value = as.numeric(ma), cl = center,
          ucl = ucl, lcl = lcl, raw = x, stringsAsFactors = FALSE)
      }

      # Nelson rules on standardized residuals (z-score)
      z_scores <- (points_df$value - points_df$cl[1]) /
        pmax(sigma, .Machine$double.eps)
      violations <- iQualityR.stat::detect_spc_violations(
        z_scores, center = 0, sigma = 1, rules = plan$nelson_rules)

      self$set_statistic("center", points_df$cl[1])
      self$set_statistic("sigma", sigma)
      self$set_statistic("sigma_method", plan$sigma_method)
      self$set_statistic("ucl", mean(points_df$ucl, na.rm = TRUE))
      self$set_statistic("lcl", mean(points_df$lcl, na.rm = TRUE))
      self$set_statistic("n_points", n)
      self$set_statistic("n_violations", violations$n_violations)
      self$set_statistic("is_in_control", violations$is_in_control)
      self$set_statistic("chart_type", chart)
      if (chart == "ewma") self$set_statistic("lambda", plan$lambda)
      if (chart == "cusum") {
        self$set_statistic("k", plan$k)
        self$set_statistic("h", plan$h)
      }
      if (chart == "ma") self$set_statistic("ma_window", plan$ma_window)

      self$set_diagnostic("chart_type", chart)
      self$set_diagnostic("sigma_method", plan$sigma_method)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary",
        iQualityR.stat::summarize_spc_rules(violations, format = "data.frame"))
    },

    # -------------------------------------------------------------------
    # Multivariate charts: Hotelling T2, MEWMA
    # -------------------------------------------------------------------
    .run_multivariate = function(data, plan) {
      if (is.null(data) || !is.data.frame(data)) {
        stop("data (data.frame) is required for multivariate charts.", call. = FALSE)
      }
      nums <- names(data)[sapply(data, is.numeric)]
      if (length(nums) < 2) stop("Need at least 2 numeric columns.", call. = FALSE)
      X <- as.matrix(data[, nums])
      n <- nrow(X); p <- ncol(X)
      if (n < p + 1) stop("Need at least p+1 observations for T2 chart.", call. = FALSE)

      center <- colMeans(X)
      cov_mat <- stats::cov(X)
      # Regularize covariance via SVD (guard against singularity)
      sv <- svd(cov_mat)
      d_reg <- pmax(sv$d, max(sv$d) * 1e-8)
      cov_inv <- sv$u %*% diag(1 / d_reg) %*% t(sv$v)

      t2 <- apply(X, 1, function(row) {
        diff <- row - center
        as.numeric(t(diff) %*% cov_inv %*% diff)
      })

      # Phase I limit: UCL = (p * (n - 1)) / (n - p) * qf(0.9973, p, n - p)
      ucl <- (p * (n - 1)) / (n - p) * stats::qf(1 - 0.0027, p, n - p)
      lcl <- 0
      center_line <- mean(t2)

      points_df <- data.frame(
        index = seq_len(n), value = t2, cl = center_line,
        ucl = rep(ucl, n), lcl = rep(lcl, n),
        stringsAsFactors = FALSE)

      # Custom rules_summary placeholder (used only for t2_mewma)
      rules_summary_df <- NULL

      # MEWMA
      if (plan$chart_type %in% c("mewma", "t2_mewma")) {
        lambda <- plan$lambda
        Z <- matrix(0, nrow = n, ncol = p)
        Z[1, ] <- (1 - lambda) * center
        for (i in 2:n) {
          Z[i, ] <- lambda * X[i, ] + (1 - lambda) * Z[i - 1, ]
        }
        # MEWMA statistic (Lowry et al. 1992)
        # Use sigma = sqrt(lambda / (2 - lambda)) approximation for limit
        mewma_stat <- apply(Z, 1, function(z) {
          diff <- z - center
          as.numeric(t(diff) %*% cov_inv %*% diff)
        })
        ucl_mewma <- p * lambda / (2 - lambda) *
          stats::qf(1 - 0.0027, p, n - p) * (n - 1) / (n - p)
        if (plan$chart_type == "mewma") {
          points_df <- data.frame(
            index = seq_len(n), value = mewma_stat, cl = 0,
            ucl = rep(ucl_mewma, n), lcl = rep(0, n),
            stringsAsFactors = FALSE)
          ucl <- ucl_mewma
        } else {
          # t2_mewma hybrid: combined statistic = max(T2/UCL_T2, MEWMA/UCL_MEWMA)
          ucl_t2_orig <- ucl  # capture before reassigning ucl to 1
          combined_stat <- pmax(t2 / ucl_t2_orig, mewma_stat / ucl_mewma)
          points_df <- data.frame(
            index = seq_len(n), value = combined_stat, cl = 1,
            ucl = rep(1, n), lcl = rep(0, n),
            t2 = t2, mewma = mewma_stat,
            ucl_t2 = rep(ucl_t2_orig, n), ucl_mewma = rep(ucl_mewma, n),
            stringsAsFactors = FALSE)
          ucl <- 1
          # Violations: combined_stat > 1 means T2 or MEWMA exceeded its UCL
          all_viol_idx <- which(combined_stat > 1)
          # Build violations list with key matching rules_triggered
          violations <- list(
            violations = if (length(all_viol_idx) > 0) {
              list("T2/MEWMA" = list(description = "T2 or MEWMA beyond UCL",
                                     indices = all_viol_idx))
            } else list(),
            n_violations = length(all_viol_idx),
            is_in_control = length(all_viol_idx) == 0,
            rules_triggered = if (length(all_viol_idx) > 0) "T2/MEWMA" else character(0),
            ucl_3 = 1, lcl_3 = 0, ucl_2 = 1, lcl_2 = 0, ucl_1 = 1, lcl_1 = 0,
            center = 1, sigma = 1, n = n
          )
          # Custom rules_summary (bypass summarize_spc_rules: rule name
          # not in Nelson set, would yield NA description / row names)
          rules_summary_df <- if (length(all_viol_idx) > 0) {
            data.frame(
              rule = "T2/MEWMA",
              description = "T2 or MEWMA beyond UCL",
              n_violations = length(all_viol_idx),
              indices = paste(all_viol_idx, collapse = ","),
              stringsAsFactors = FALSE)
          } else {
            data.frame(
              rule = character(0), description = character(0),
              n_violations = integer(0), indices = character(0),
              stringsAsFactors = FALSE)
          }
        }
      }

      if (plan$chart_type != "t2_mewma") {
        # Nelson rules on standardized T2 statistics (skip for t2_mewma, already done above)
        sigma_t2 <- if (sd(t2) > 0) sd(t2) else 1
        z_scores <- (t2 - mean(t2)) / sigma_t2
        violations <- iQualityR.stat::detect_spc_violations(
          z_scores, center = 0, sigma = 1, rules = plan$nelson_rules)
      } else {
        sigma_t2 <- if (sd(t2) > 0) sd(t2) else 1
      }

      self$set_statistic("center",
        if (plan$chart_type == "t2_mewma") 1 else mean(t2))
      self$set_statistic("sigma", sigma_t2)
      self$set_statistic("sigma_method",
        if (plan$chart_type == "mewma") "mewma_sigma"
        else if (plan$chart_type == "t2_mewma") "t2_mewma_hybrid"
        else "t2_sd")
      self$set_statistic("ucl", ucl)
      self$set_statistic("lcl", 0)
      self$set_statistic("n_points", n)
      self$set_statistic("n_variables", p)
      self$set_statistic("n_violations", violations$n_violations)
      self$set_statistic("is_in_control", violations$is_in_control)
      self$set_statistic("chart_type", plan$chart_type)
      if (plan$chart_type == "t2_mewma") {
        self$set_statistic("ucl_t2", mean(points_df$ucl_t2))
        self$set_statistic("ucl_mewma", mean(points_df$ucl_mewma))
      }

      self$set_diagnostic("chart_type", plan$chart_type)
      self$set_diagnostic("variables", nums)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("violations", private$.violations_to_df(violations))
      if (is.null(rules_summary_df)) {
        self$set_datatable("rules_summary",
          iQualityR.stat::summarize_spc_rules(violations, format = "data.frame"))
      } else {
        self$set_datatable("rules_summary", rules_summary_df)
      }
    },

    # -------------------------------------------------------------------
    # Rare-event charts: G chart, T chart
    # -------------------------------------------------------------------
    .run_rare_events = function(x, plan) {
      if (is.null(x)) stop("x is required for rare-event charts.", call. = FALSE)
      chart <- plan$chart_type

      if (chart == "g") {
        # G chart: number of opportunities between events (geometric distribution)
        # x = count of opportunities between consecutive defects
        x <- as.integer(x)
        x <- x[!is.na(x)]
        n <- length(x)
        if (n < 2) stop("Need at least 2 intervals for G chart.", call. = FALSE)
        center <- mean(x)
        # Geometric control limits (approximate via normal for large mean)
        sigma <- sqrt(center * (center + 1))
        ucl <- center + 3 * sigma
        lcl <- pmax(0, center - 3 * sigma)
        points_df <- data.frame(
          index = seq_len(n), value = x, cl = center,
          ucl = ucl, lcl = lcl, stringsAsFactors = FALSE)
      } else {
        # T chart: time between events (exponential distribution)
        x <- as.numeric(x)
        x <- x[!is.na(x)]
        n <- length(x)
        if (n < 2) stop("Need at least 2 intervals for T chart.", call. = FALSE)
        # Use log-transform: ln(x) ~ Normal(mu, sigma)
        log_x <- log(pmax(x, .Machine$double.eps))
        center <- mean(log_x)
        sigma <- sd(log_x)
        ucl_log <- center + 3 * sigma
        lcl_log <- center - 3 * sigma
        # Transform back to original scale
        points_df <- data.frame(
          index = seq_len(n), value = x,
          cl = exp(center), ucl = exp(ucl_log), lcl = exp(lcl_log),
          stringsAsFactors = FALSE)
        center <- exp(center)
        sigma <- exp(sigma)
      }

      # Nelson rules on standardized scale
      if (chart == "g") {
        z_scores <- (x - mean(x)) / pmax(sqrt(mean(x) * (mean(x) + 1)), 1)
      } else {
        z_scores <- (log_x - mean(log_x)) / pmax(sd(log_x), .Machine$double.eps)
      }
      violations <- iQualityR.stat::detect_spc_violations(
        z_scores, center = 0, sigma = 1, rules = plan$nelson_rules)

      self$set_statistic("center", center)
      self$set_statistic("sigma", sigma)
      self$set_statistic("sigma_method",
        if (chart == "g") "geometric_sd" else "log_exp_sd")
      self$set_statistic("ucl", mean(points_df$ucl))
      self$set_statistic("lcl", mean(points_df$lcl))
      self$set_statistic("n_points", nrow(points_df))
      self$set_statistic("n_violations", violations$n_violations)
      self$set_statistic("is_in_control", violations$is_in_control)
      self$set_statistic("chart_type", chart)

      self$set_diagnostic("chart_type", chart)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary",
        iQualityR.stat::summarize_spc_rules(violations, format = "data.frame"))
    },

    # -------------------------------------------------------------------
    # v0.2: Adaptive rolling-window and Adaptive EWMA charts
    # -------------------------------------------------------------------
    .run_adaptive = function(x, subgroup, plan) {
      if (is.null(x)) stop("x is required for adaptive charts.", call. = FALSE)
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      n <- length(x)
      if (n < 5) stop("Need at least 5 observations for adaptive charts.", call. = FALSE)

      chart <- plan$chart_type

      if (chart == "adaptive") {
        w <- plan$window_size
        if (n < w) {
          warning("window_size > n; using n (= ", n, ") as window.", call. = FALSE)
          w <- n
        }
        # Rolling center and sigma
        center_i <- numeric(n)
        sigma_i <- numeric(n)
        for (i in seq_len(n)) {
          lo <- max(1L, i - w + 1L)
          win <- x[lo:i]
          center_i[i] <- mean(win)
          sigma_i[i] <- tryCatch(
            iQualityR.stat::sigma_estimate(win, method = plan$sigma_method,
                                           use_unbiased = TRUE),
            error = function(e) sd(win))
          if (!is.finite(sigma_i[i]) || sigma_i[i] <= 0) sigma_i[i] <- sd(win)
        }
        ucl <- center_i + 3 * sigma_i
        lcl <- center_i - 3 * sigma_i
        points_df <- data.frame(
          index = seq_len(n), value = x, cl = center_i,
          ucl = ucl, lcl = lcl, sigma = sigma_i,
          stringsAsFactors = FALSE)
        # Use na.rm = TRUE: first window (size 1) yields NA sigma
        sigma <- mean(sigma_i, na.rm = TRUE)
      } else {
        # AEWMA: variable lambda based on forecast error magnitude
        lambda0 <- plan$aewma_lambda
        k_sens <- plan$aewma_k
        sigma <- private$.estimate_sigma_for(x, subgroup, plan)
        z <- numeric(n)
        lambda_used <- numeric(n)
        z[1] <- x[1]
        lambda_used[1] <- lambda0
        for (i in 2:n) {
          e <- x[i] - z[i - 1]
          # Increase lambda when |e| is large relative to sigma
          ratio <- min(1, k_sens * abs(e) / pmax(sigma, .Machine$double.eps))
          lambda_i <- lambda0 + (1 - lambda0) * ratio
          lambda_i <- min(lambda_i, 1)
          lambda_used[i] <- lambda_i
          z[i] <- lambda_i * x[i] + (1 - lambda_i) * z[i - 1]
        }
        center <- mean(x)
        # Steady-state sigma_z approximation (variable lambda -> use mean)
        sigma_z <- sigma * sqrt(mean(lambda_used) / (2 - mean(lambda_used)))
        ucl <- center + 3 * sigma_z
        lcl <- center - 3 * sigma_z
        points_df <- data.frame(
          index = seq_len(n), value = z, cl = center,
          ucl = rep(ucl, n), lcl = rep(lcl, n),
          lambda = lambda_used, raw = x,
          stringsAsFactors = FALSE)
        center_i <- rep(center, n)
      }

      # Nelson rules on standardized residuals
      z_scores <- (points_df$value - center_i) / pmax(sigma, .Machine$double.eps)
      violations <- iQualityR.stat::detect_spc_violations(
        z_scores, center = 0, sigma = 1, rules = plan$nelson_rules)

      self$set_statistic("center", if (chart == "adaptive") mean(center_i) else center)
      self$set_statistic("sigma", sigma)
      self$set_statistic("sigma_method", plan$sigma_method)
      self$set_statistic("ucl", mean(points_df$ucl, na.rm = TRUE))
      self$set_statistic("lcl", mean(points_df$lcl, na.rm = TRUE))
      self$set_statistic("n_points", n)
      self$set_statistic("n_violations", violations$n_violations)
      self$set_statistic("is_in_control", violations$is_in_control)
      self$set_statistic("chart_type", chart)
      if (chart == "adaptive") self$set_statistic("window_size", plan$window_size)
      if (chart == "aewma") {
        self$set_statistic("aewma_lambda", plan$aewma_lambda)
        self$set_statistic("aewma_k", plan$aewma_k)
      }

      self$set_diagnostic("chart_type", chart)
      self$set_diagnostic("sigma_method", plan$sigma_method)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary",
        iQualityR.stat::summarize_spc_rules(violations, format = "data.frame"))
    },

    # -------------------------------------------------------------------
    # v0.2: ARIMA residual control chart
    # -------------------------------------------------------------------
    .run_arima_resid = function(x, plan) {
      if (is.null(x)) stop("x is required for ARIMA residual chart.", call. = FALSE)
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      n <- length(x)
      if (n < 10) stop("Need at least 10 observations for ARIMA modeling.", call. = FALSE)

      order <- plan$arima_order
      fit <- tryCatch(
        stats::arima(x, order = order),
        error = function(e) {
          stop("Failed to fit ARIMA(", paste(order, collapse = ","),
               "): ", conditionMessage(e), call. = FALSE)
        })
      resid <- as.numeric(stats::residuals(fit))
      resid <- resid[!is.na(resid)]
      nr <- length(resid)

      # I-MR chart on residuals
      sigma <- tryCatch(
        iQualityR.stat::sigma_estimate(resid, method = plan$sigma_method,
                                       use_unbiased = TRUE),
        error = function(e) sd(resid))
      if (!is.finite(sigma) || sigma <= 0) sigma <- sd(resid)
      center <- mean(resid)
      ucl <- center + 3 * sigma
      lcl <- center - 3 * sigma
      mr_bar <- mean(abs(diff(resid)))
      ucl_mr <- 3.267 * mr_bar
      lcl_mr <- 0

      points_df <- data.frame(
        index = seq_len(nr), value = resid, cl = center,
        ucl = rep(ucl, nr), lcl = rep(lcl, nr),
        raw = x[seq_len(nr)],
        stringsAsFactors = FALSE)

      violations <- iQualityR.stat::detect_spc_violations(
        resid, center = center, sigma = sigma, rules = plan$nelson_rules)

      self$set_statistic("center", center)
      self$set_statistic("sigma", sigma)
      self$set_statistic("sigma_method", plan$sigma_method)
      self$set_statistic("ucl", ucl)
      self$set_statistic("lcl", lcl)
      self$set_statistic("mr_bar", mr_bar)
      self$set_statistic("ucl_mr", ucl_mr)
      self$set_statistic("lcl_mr", lcl_mr)
      self$set_statistic("n_points", nr)
      self$set_statistic("n_violations", violations$n_violations)
      self$set_statistic("is_in_control", violations$is_in_control)
      self$set_statistic("chart_type", "arima_resid")
      self$set_statistic("arima_order", paste(order, collapse = ","))

      self$set_diagnostic("chart_type", "arima_resid")
      self$set_diagnostic("arima_order", order)
      self$set_diagnostic("arima_sigma2", fit$sigma2)
      self$set_diagnostic("arima_aic", fit$aic)
      self$set_diagnostic("arima_coef", as.list(fit$coef))
      self$set_diagnostic("sigma_method", plan$sigma_method)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary",
        iQualityR.stat::summarize_spc_rules(violations, format = "data.frame"))
    },

    # -------------------------------------------------------------------
    # v0.2: Change-point detection chart
    # -------------------------------------------------------------------
    .run_changepoint = function(x, plan) {
      if (is.null(x)) stop("x is required for change-point detection.", call. = FALSE)
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      n <- length(x)
      if (n < 10) stop("Need at least 10 observations for change-point detection.", call. = FALSE)

      # Try changepoint package; fall back to in-house binary segmentation.
      # changepoint::cpt.mean expects uppercase method (AMOC/PELT/SegNeigh/BinSeg).
      cps <- if (requireNamespace("changepoint", quietly = TRUE)) {
        tryCatch({
          obj <- changepoint::cpt.mean(x, method = toupper(plan$cp_method),
                                        penalty = plan$cp_penalty)
          as.integer(changepoint::cpts(obj))
        }, error = function(e) {
          warning("changepoint::cpt.mean failed: ", conditionMessage(e),
                  "; falling back to in-house binary segmentation.",
                  call. = FALSE)
          private$.binary_segmentation_mean(x)
        })
      } else {
        private$.binary_segmentation_mean(x)
      }

      center <- mean(x)
      sigma <- private$.estimate_sigma_for(x, NULL, plan)
      ucl <- center + 3 * sigma
      lcl <- center - 3 * sigma

      points_df <- data.frame(
        index = seq_len(n), value = x, cl = center,
        ucl = rep(ucl, n), lcl = rep(lcl, n),
        stringsAsFactors = FALSE)

      # Change-point table
      if (length(cps) > 0) {
        cp_df <- data.frame(
          index = cps,
          mean_before = sapply(cps, function(cp) if (cp > 1) mean(x[1:cp]) else NA_real_),
          mean_after = sapply(cps, function(cp) if (cp < n) mean(x[(cp + 1):n]) else NA_real_),
          stringsAsFactors = FALSE)
        cp_df$mean_diff <- cp_df$mean_after - cp_df$mean_before
      } else {
        cp_df <- data.frame(index = integer(0), mean_before = numeric(0),
                            mean_after = numeric(0), mean_diff = numeric(0),
                            stringsAsFactors = FALSE)
      }

      # Nelson rules on standardized residuals
      z_scores <- (x - center) / pmax(sigma, .Machine$double.eps)
      violations <- iQualityR.stat::detect_spc_violations(
        z_scores, center = 0, sigma = 1, rules = plan$nelson_rules)

      self$set_statistic("center", center)
      self$set_statistic("sigma", sigma)
      self$set_statistic("sigma_method", plan$sigma_method)
      self$set_statistic("ucl", ucl)
      self$set_statistic("lcl", lcl)
      self$set_statistic("n_points", n)
      self$set_statistic("n_violations", violations$n_violations)
      self$set_statistic("is_in_control", violations$is_in_control)
      self$set_statistic("chart_type", "changepoint")
      self$set_statistic("n_change_points", length(cps))

      self$set_diagnostic("chart_type", "changepoint")
      self$set_diagnostic("change_points", cps)
      self$set_diagnostic("cp_method", plan$cp_method)
      self$set_diagnostic("cp_penalty", plan$cp_penalty)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("change_points", cp_df)
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary",
        iQualityR.stat::summarize_spc_rules(violations, format = "data.frame"))
    },

    # -------------------------------------------------------------------
    # v0.2: KDE nonparametric control chart
    # -------------------------------------------------------------------
    .run_kde = function(x, plan) {
      if (is.null(x)) stop("x is required for KDE chart.", call. = FALSE)
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      n <- length(x)
      if (n < 10) stop("Need at least 10 observations for KDE chart.", call. = FALSE)

      bw <- plan$kde_bandwidth
      if (is.null(bw)) {
        bw <- tryCatch(stats::bw.SJ(x), error = function(e) stats::bw.nrd0(x))
      }
      dens <- stats::density(x, bw = bw, kernel = "gaussian", n = 512)
      # Empirical percentiles (0.135% and 99.865% -> equiv to +-3 sigma under normality)
      q_lo <- stats::quantile(x, probs = 0.00135, names = FALSE, type = 7)
      q_hi <- stats::quantile(x, probs = 0.99865, names = FALSE, type = 7)
      center <- stats::median(x)
      sigma <- stats::sd(x)

      points_df <- data.frame(
        index = seq_len(n), value = x, cl = center,
        ucl = rep(q_hi, n), lcl = rep(q_lo, n),
        stringsAsFactors = FALSE)

      # Nelson rules on standardized scale (informational)
      z_scores <- (x - center) / pmax(sigma, .Machine$double.eps)
      violations <- iQualityR.stat::detect_spc_violations(
        z_scores, center = 0, sigma = 1, rules = plan$nelson_rules)

      self$set_statistic("center", center)
      self$set_statistic("sigma", sigma)
      self$set_statistic("sigma_method", plan$sigma_method)
      self$set_statistic("ucl", q_hi)
      self$set_statistic("lcl", q_lo)
      self$set_statistic("n_points", n)
      self$set_statistic("n_violations", violations$n_violations)
      self$set_statistic("is_in_control", violations$is_in_control)
      self$set_statistic("chart_type", "kde")
      self$set_statistic("bandwidth", bw)

      self$set_diagnostic("chart_type", "kde")
      self$set_diagnostic("bandwidth", bw)
      self$set_diagnostic("density_x", dens$x)
      self$set_diagnostic("density_y", dens$y)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("density", data.frame(x = dens$x, y = dens$y,
                                               stringsAsFactors = FALSE))
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary",
        iQualityR.stat::summarize_spc_rules(violations, format = "data.frame"))
    },

    # -------------------------------------------------------------------
    # v0.6: ML enhancement charts
    # -------------------------------------------------------------------
    .run_lstm = function(x, plan) {
      if (is.null(x)) stop("x is required for LSTM chart.", call. = FALSE)
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      n <- length(x)
      if (n < 10) stop("Need at least 10 observations for LSTM chart.", call. = FALSE)

      result <- .lstm_fit_predict(
        x = x,
        window = plan$lstm_window,
        units = plan$lstm_units,
        epochs = plan$lstm_epochs,
        batch_size = plan$lstm_batch_size,
        threshold_z = plan$lstm_threshold
      )

      # Build control limits using sigma of original data
      center <- mean(x)
      sigma <- sd(x)
      if (!is.finite(sigma) || sigma <= 0) sigma <- 1
      ucl <- center + 3 * sigma
      lcl <- center - 3 * sigma

      # Anomaly threshold on z-scale
      anomaly_z <- if (sd(result$anomaly_score) > 0) {
        (result$anomaly_score - mean(result$anomaly_score)) /
          sd(result$anomaly_score)
      } else {
        rep(0, n)
      }
      is_anomaly <- anomaly_z > plan$lstm_threshold
      viol_idx <- which(is_anomaly)

      points_df <- data.frame(
        index = seq_len(n), value = x, cl = center,
        ucl = rep(ucl, n), lcl = rep(lcl, n),
        fit = result$fit_values,
        reconstruction_error = result$reconstruction_error,
        anomaly_score = result$anomaly_score,
        is_anomaly = is_anomaly,
        stringsAsFactors = FALSE)

      # Build violations list in standard SPC format
      violations <- list(
        violations = if (length(viol_idx) > 0) {
          list("ML Anomaly" = list(
            description = "LSTM anomaly score exceeds threshold",
            indices = viol_idx))
        } else list(),
        n_violations = length(viol_idx),
        is_in_control = length(viol_idx) == 0,
        rules_triggered = if (length(viol_idx) > 0) "ML Anomaly" else character(0),
        center = center, sigma = sigma,
        ucl_3 = ucl, lcl_3 = lcl,
        ucl_2 = center + 2 * sigma, lcl_2 = center - 2 * sigma,
        ucl_1 = center + sigma, lcl_1 = center - sigma,
        n = n
      )

      # Build rules_summary df (custom since "ML Anomaly" is not a Nelson rule)
      rules_summary_df <- if (length(viol_idx) > 0) {
        data.frame(
          rule = "ML Anomaly",
          description = "LSTM anomaly score exceeds threshold",
          n_violations = length(viol_idx),
          indices = paste(viol_idx, collapse = ","),
          stringsAsFactors = FALSE)
      } else {
        data.frame(rule = character(0), description = character(0),
                  n_violations = integer(0), indices = character(0),
                  stringsAsFactors = FALSE)
      }

      # Confidence: based on backend (keras higher) and error distribution
      confidence <- if (result$backend == "keras") 0.9 else 0.6
      # Adjust by data size
      confidence <- confidence * min(1, n / 100)

      # Build AI diagnostic with SHAP-like attribution
      shap_proxy <- data.frame(
        feature = "reconstruction_error",
        contribution = result$reconstruction_error,
        stringsAsFactors = FALSE)

      ai_diag <- .build_ai_diagnostic(
        method = "lstm",
        anomaly_score = result$anomaly_score,
        shap_values = shap_proxy,
        rule_attribution = if (length(viol_idx) > 0)
          paste("Anomaly at indices:", paste(viol_idx, collapse = ",")) else
          "No anomalies detected",
        confidence = confidence,
        extras = list(backend = result$backend,
                       threshold_z = plan$lstm_threshold))

      self$set_statistic("center", center)
      self$set_statistic("sigma", sigma)
      self$set_statistic("sigma_method", plan$sigma_method)
      self$set_statistic("ucl", ucl)
      self$set_statistic("lcl", lcl)
      self$set_statistic("n_points", n)
      self$set_statistic("n_violations", length(viol_idx))
      self$set_statistic("is_in_control", length(viol_idx) == 0)
      self$set_statistic("chart_type", "lstm")
      self$set_statistic("backend", result$backend)

      self$set_diagnostic("chart_type", "lstm")
      self$set_diagnostic("backend", result$backend)
      self$set_diagnostic("ai_diagnostic", ai_diag)
      self$set_diagnostic("lstm_window", plan$lstm_window)
      self$set_diagnostic("lstm_units", plan$lstm_units)
      self$set_diagnostic("anomaly_threshold", plan$lstm_threshold)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary", rules_summary_df)
    },

    .run_autoencoder = function(data, plan) {
      if (is.null(data) || !is.data.frame(data)) {
        stop("data (data.frame) is required for autoencoder chart.", call. = FALSE)
      }
      num_cols <- vapply(data, is.numeric, logical(1))
      data <- data[, num_cols, drop = FALSE]
      n <- nrow(data)
      p <- ncol(data)
      if (n < 5) stop("Need at least 5 observations for autoencoder.", call. = FALSE)
      if (p < 1) stop("Need at least 1 numeric column for autoencoder.", call. = FALSE)

      result <- .ae_fit_predict(
        data = data,
        encoding_dim = plan$ae_encoding_dim,
        hidden_dim = plan$ae_hidden_dim,
        epochs = plan$ae_epochs,
        batch_size = plan$ae_batch_size,
        threshold_quantile = plan$ae_threshold_quantile
      )

      # Control limits on reconstruction error
      recon <- result$reconstruction_error
      center <- mean(recon)
      sigma <- sd(recon)
      if (!is.finite(sigma) || sigma <= 0) sigma <- 1
      ucl <- result$threshold
      lcl <- 0

      is_anomaly <- recon > ucl
      viol_idx <- which(is_anomaly)

      # Build index for points_df
      points_df <- data.frame(
        index = seq_len(n), value = recon, cl = center,
        ucl = rep(ucl, n), lcl = rep(lcl, n),
        anomaly_score = result$anomaly_score,
        is_anomaly = is_anomaly,
        stringsAsFactors = FALSE)

      violations <- list(
        violations = if (length(viol_idx) > 0) {
          list("ML Anomaly" = list(
            description = "Autoencoder reconstruction error exceeds threshold",
            indices = viol_idx))
        } else list(),
        n_violations = length(viol_idx),
        is_in_control = length(viol_idx) == 0,
        rules_triggered = if (length(viol_idx) > 0) "ML Anomaly" else character(0),
        center = center, sigma = sigma,
        ucl_3 = ucl, lcl_3 = lcl,
        ucl_2 = ucl, lcl_2 = lcl,
        ucl_1 = ucl, lcl_1 = lcl,
        n = n
      )

      rules_summary_df <- if (length(viol_idx) > 0) {
        data.frame(
          rule = "ML Anomaly",
          description = "AE reconstruction error exceeds threshold",
          n_violations = length(viol_idx),
          indices = paste(viol_idx, collapse = ","),
          stringsAsFactors = FALSE)
      } else {
        data.frame(rule = character(0), description = character(0),
                  n_violations = integer(0), indices = character(0),
                  stringsAsFactors = FALSE)
      }

      confidence <- if (result$backend == "keras") 0.85 else 0.6
      confidence <- confidence * min(1, n / 50)

      # SHAP-like attribution via feature contribution
      shap_df <- result$feature_contrib
      if (nrow(shap_df) == 0) shap_df <- data.frame(feature = character(0),
                                                     contribution = numeric(0))

      ai_diag <- .build_ai_diagnostic(
        method = "autoencoder",
        anomaly_score = result$anomaly_score,
        shap_values = shap_df,
        rule_attribution = if (length(viol_idx) > 0)
          paste("Anomaly at indices:", paste(viol_idx, collapse = ",")) else
          "No anomalies detected",
        confidence = confidence,
        extras = list(backend = result$backend,
                       threshold = ucl,
                       encoding_dim = plan$ae_encoding_dim))

      self$set_statistic("center", center)
      self$set_statistic("sigma", sigma)
      self$set_statistic("sigma_method", plan$sigma_method)
      self$set_statistic("ucl", ucl)
      self$set_statistic("lcl", lcl)
      self$set_statistic("n_points", n)
      self$set_statistic("n_violations", length(viol_idx))
      self$set_statistic("is_in_control", length(viol_idx) == 0)
      self$set_statistic("chart_type", "autoencoder")
      self$set_statistic("backend", result$backend)

      self$set_diagnostic("chart_type", "autoencoder")
      self$set_diagnostic("backend", result$backend)
      self$set_diagnostic("ai_diagnostic", ai_diag)
      self$set_diagnostic("encoding_dim", plan$ae_encoding_dim)
      self$set_diagnostic("threshold", ucl)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("feature_contributions", result$feature_contrib)
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary", rules_summary_df)
    },

    .run_iforest = function(data, plan) {
      if (is.null(data) || !is.data.frame(data)) {
        stop("data (data.frame) is required for iforest chart.", call. = FALSE)
      }
      num_cols <- vapply(data, is.numeric, logical(1))
      data <- data[, num_cols, drop = FALSE]
      n <- nrow(data)
      p <- ncol(data)
      if (n < 5) stop("Need at least 5 observations for iforest.", call. = FALSE)
      if (p < 1) stop("Need at least 1 numeric column for iforest.", call. = FALSE)

      result <- .iforest_fit_predict(
        data = data,
        ntree = plan$iforest_ntree,
        sample_size = plan$iforest_sample_size
      )

      scores <- result$anomaly_score
      threshold <- plan$iforest_threshold
      is_anomaly <- scores > threshold
      viol_idx <- which(is_anomaly)

      center <- mean(scores)
      sigma <- sd(scores)
      if (!is.finite(sigma) || sigma <= 0) sigma <- 1
      ucl <- threshold
      lcl <- 0

      points_df <- data.frame(
        index = seq_len(n), value = scores, cl = center,
        ucl = rep(ucl, n), lcl = rep(lcl, n),
        anomaly_score = scores,
        is_anomaly = is_anomaly,
        stringsAsFactors = FALSE)

      violations <- list(
        violations = if (length(viol_idx) > 0) {
          list("ML Anomaly" = list(
            description = "Isolation Forest anomaly score exceeds threshold",
            indices = viol_idx))
        } else list(),
        n_violations = length(viol_idx),
        is_in_control = length(viol_idx) == 0,
        rules_triggered = if (length(viol_idx) > 0) "ML Anomaly" else character(0),
        center = center, sigma = sigma,
        ucl_3 = ucl, lcl_3 = lcl,
        ucl_2 = ucl, lcl_2 = lcl,
        ucl_1 = ucl, lcl_1 = lcl,
        n = n
      )

      rules_summary_df <- if (length(viol_idx) > 0) {
        data.frame(
          rule = "ML Anomaly",
          description = "IForest anomaly score exceeds threshold",
          n_violations = length(viol_idx),
          indices = paste(viol_idx, collapse = ","),
          stringsAsFactors = FALSE)
      } else {
        data.frame(rule = character(0), description = character(0),
                  n_violations = integer(0), indices = character(0),
                  stringsAsFactors = FALSE)
      }

      confidence <- if (result$backend == "isotree") 0.85 else 0.6
      confidence <- confidence * min(1, n / 50)

      shap_df <- result$feature_contrib
      if (nrow(shap_df) == 0) shap_df <- data.frame(feature = character(0),
                                                     contribution = numeric(0))

      ai_diag <- .build_ai_diagnostic(
        method = "iforest",
        anomaly_score = scores,
        shap_values = shap_df,
        rule_attribution = if (length(viol_idx) > 0)
          paste("Anomaly at indices:", paste(viol_idx, collapse = ",")) else
          "No anomalies detected",
        confidence = confidence,
        extras = list(backend = result$backend,
                       threshold = threshold,
                       ntree = plan$iforest_ntree))

      self$set_statistic("center", center)
      self$set_statistic("sigma", sigma)
      self$set_statistic("sigma_method", plan$sigma_method)
      self$set_statistic("ucl", ucl)
      self$set_statistic("lcl", lcl)
      self$set_statistic("n_points", n)
      self$set_statistic("n_violations", length(viol_idx))
      self$set_statistic("is_in_control", length(viol_idx) == 0)
      self$set_statistic("chart_type", "iforest")
      self$set_statistic("backend", result$backend)

      self$set_diagnostic("chart_type", "iforest")
      self$set_diagnostic("backend", result$backend)
      self$set_diagnostic("ai_diagnostic", ai_diag)
      self$set_diagnostic("ntree", plan$iforest_ntree)
      self$set_diagnostic("threshold", threshold)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("feature_contributions", result$feature_contrib)
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary", rules_summary_df)
    },

    .run_bocpd = function(x, plan) {
      if (is.null(x)) stop("x is required for BOCPD chart.", call. = FALSE)
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      n <- length(x)
      if (n < 10) stop("Need at least 10 observations for BOCPD.", call. = FALSE)

      prior_params <- list(
        mu = plan$bocpd_prior_mu,
        kappa = plan$bocpd_prior_kappa,
        alpha = plan$bocpd_prior_alpha,
        beta = plan$bocpd_prior_beta
      )

      result <- .bocpd_run(
        x = x,
        hazard = plan$bocpd_hazard,
        prior_params = prior_params
      )

      cp_prob <- result$changepoint_prob
      detected_cps <- result$most_likely_changepoints

      center <- mean(x)
      sigma <- sd(x)
      if (!is.finite(sigma) || sigma <= 0) sigma <- 1
      ucl <- center + 3 * sigma
      lcl <- center - 3 * sigma

      # Anomaly score = changepoint probability
      is_anomaly <- cp_prob > 0.5
      viol_idx <- which(is_anomaly)

      points_df <- data.frame(
        index = seq_len(n), value = x, cl = center,
        ucl = rep(ucl, n), lcl = rep(lcl, n),
        changepoint_prob = cp_prob,
        run_length = result$run_length_mean,
        is_anomaly = is_anomaly,
        stringsAsFactors = FALSE)

      violations <- list(
        violations = if (length(detected_cps) > 0) {
          list("ML Changepoint" = list(
            description = "BOCPD detected structural change",
            indices = detected_cps))
        } else list(),
        n_violations = length(detected_cps),
        is_in_control = length(detected_cps) == 0,
        rules_triggered = if (length(detected_cps) > 0) "ML Changepoint" else character(0),
        center = center, sigma = sigma,
        ucl_3 = ucl, lcl_3 = lcl,
        ucl_2 = center + 2 * sigma, lcl_2 = center - 2 * sigma,
        ucl_1 = center + sigma, lcl_1 = center - sigma,
        n = n
      )

      rules_summary_df <- if (length(detected_cps) > 0) {
        data.frame(
          rule = "ML Changepoint",
          description = "BOCPD detected structural change",
          n_violations = length(detected_cps),
          indices = paste(detected_cps, collapse = ","),
          stringsAsFactors = FALSE)
      } else {
        data.frame(rule = character(0), description = character(0),
                  n_violations = integer(0), indices = character(0),
                  stringsAsFactors = FALSE)
      }

      # Confidence: based on max changepoint probability
      max_cp <- if (length(cp_prob) > 0) max(cp_prob) else 0
      confidence <- 0.5 + 0.4 * max_cp  # 0.5 to 0.9
      confidence <- confidence * min(1, n / 50)

      shap_proxy <- data.frame(
        feature = "changepoint_prob",
        contribution = cp_prob,
        stringsAsFactors = FALSE)

      ai_diag <- .build_ai_diagnostic(
        method = "bocpd",
        anomaly_score = cp_prob,
        shap_values = shap_proxy,
        rule_attribution = if (length(detected_cps) > 0)
          paste("Changepoints at indices:", paste(detected_cps, collapse = ",")) else
          "No changepoints detected",
        confidence = confidence,
        extras = list(hazard = plan$bocpd_hazard,
                       detected_changepoints = detected_cps))

      self$set_statistic("center", center)
      self$set_statistic("sigma", sigma)
      self$set_statistic("sigma_method", plan$sigma_method)
      self$set_statistic("ucl", ucl)
      self$set_statistic("lcl", lcl)
      self$set_statistic("n_points", n)
      self$set_statistic("n_violations", length(detected_cps))
      self$set_statistic("is_in_control", length(detected_cps) == 0)
      self$set_statistic("chart_type", "bocpd")
      self$set_statistic("n_change_points", length(detected_cps))
      self$set_statistic("max_changepoint_prob", max_cp)

      self$set_diagnostic("chart_type", "bocpd")
      self$set_diagnostic("ai_diagnostic", ai_diag)
      self$set_diagnostic("hazard", plan$bocpd_hazard)
      self$set_diagnostic("changepoints", detected_cps)
      self$set_diagnostic("run_length_mean", result$run_length_mean)
      self$set_diagnostic("rules_triggered", violations$rules_triggered)

      self$set_datatable("points", points_df)
      self$set_datatable("changepoints", data.frame(
        index = detected_cps,
        probability = cp_prob[detected_cps],
        stringsAsFactors = FALSE))
      self$set_datatable("violations", private$.violations_to_df(violations))
      self$set_datatable("rules_summary", rules_summary_df)
    },

    # -------------------------------------------------------------------
    # Helpers
    # -------------------------------------------------------------------
    .subgroup_means = function(x, n) {
      n_sub <- length(x) %/% n
      rowMeans(matrix(x[1:(n_sub * n)], ncol = n, byrow = TRUE))
    },
    .subgroup_ranges = function(x, n) {
      n_sub <- length(x) %/% n
      mat <- matrix(x[1:(n_sub * n)], ncol = n, byrow = TRUE)
      apply(mat, 1, function(r) max(r) - min(r))
    },
    .subgroup_sds = function(x, n) {
      n_sub <- length(x) %/% n
      mat <- matrix(x[1:(n_sub * n)], ncol = n, byrow = TRUE)
      apply(mat, 1, sd)
    },
    .estimate_sigma_for = function(x, subgroup, plan) {
      method <- plan$sigma_method
      sigma <- tryCatch(
        iQualityR.stat::sigma_estimate(
          x = x, subgroup = subgroup, n_size = plan$subgroup_size,
          method = method, use_unbiased = TRUE),
        error = function(e) NA_real_)
      if (!is.finite(sigma) || sigma <= 0) sigma <- sd(x)
      sigma
    },
    .binary_segmentation_mean = function(x, min_seg = 5, threshold = 6) {
      # In-house CUSUM-based binary segmentation for mean shifts.
      # Returns integer indices of detected change-points (1-based, inclusive).
      n <- length(x)
      if (n < 2 * min_seg) return(integer(0))
      cps <- integer(0)
      .seg_cusum <- function(start, end) {
        if (end - start + 1 < 2 * min_seg) return(NULL)
        seg <- x[start:end]
        m <- mean(seg)
        s <- stats::sd(seg)
        if (!is.finite(s) || s <= 0) return(NULL)
        cusum <- cumsum(seg - m) / s
        stat_max <- max(abs(cusum))
        if (stat_max < threshold) return(NULL)
        cp_local <- which.max(abs(cusum))
        cp_global <- start + cp_local - 1
        cps <<- c(cps, cp_global)
        .seg_cusum(start, cp_global)
        .seg_cusum(cp_global + 1, end)
      }
      .seg_cusum(1, n)
      sort(unique(cps))
    },
    .violations_to_df = function(violations) {
      if (length(violations$violations) == 0) {
        return(data.frame(
          rule = character(0), description = character(0),
          indices = character(0), stringsAsFactors = FALSE))
      }
      do.call(rbind, lapply(names(violations$violations), function(r) {
        v <- violations$violations[[r]]
        data.frame(
          rule = r,
          description = v$description,
          indices = paste(v$indices, collapse = ","),
          stringsAsFactors = FALSE)
      }))
    }
  )
)
