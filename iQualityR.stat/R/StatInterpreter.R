# =============================================================================
# File: R/StatInterpreter.R
# Description: Statistical result interpreter - translate stats to plain language
# =============================================================================

#' @title StatInterpreter: Statistical result interpreter
#' @description
#' Translate cryptic statistical jargon into plain language that quality
#' professionals can understand while maintaining technical accuracy.
#' Especially useful when reporting to management, colleagues, or customers.
#'
#' **Supported scenarios**:
#' - Probability distribution result interpretation
#' - Hypothesis test result interpretation (connecting to htest objects)
#' - ANOVA result interpretation (connecting to aov/anova objects)
#' - Normality test result interpretation (connecting to normality test result lists)
#'
#' **Audience levels**:
#' - `manager`: Management/executives - plain language, focus on business impact
#' - `technical`: Technical staff/quality engineers - includes statistical terms but explains meaning
#' - `client`: Customers/external auditors - professional, formal, focus on quality assurance
#'
#' @export
StatInterpreter <- R6::R6Class("StatInterpreter",
  public = list(
    #' @description Unified interpretation entry
    #' @param x Statistical result object (htest / aov / list)
    #' @param audience Audience level ("manager", "technical", "client")
    #' @param ... Other parameters
    #' @return Interpretation string
    interpret = function(x, audience = "manager", ...) {
      if (inherits(x, "htest") || inherits(x, "htest_result")) {
        private$.interpret_htest(x, audience)
      } else if (inherits(x, "interval_result")) {
        private$.interpret_interval(x, audience)
      } else if (inherits(x, "regression_result")) {
        private$.interpret_regression(x, audience)
      } else if (inherits(x, "resampling_result")) {
        private$.interpret_resampling(x, audience)
      } else if (inherits(x, "aov") || inherits(x, "anova")) {
        private$.interpret_anova(x, audience)
      } else if (is.list(x) && "test_type" %in% names(x) && x$test_type == "ANOM") {
        private$.interpret_anom(x, audience)
      } else if (inherits(x, "ProbNode") || (is.list(x) && "type" %in% names(x))) {
        private$.interpret_dist(x, audience)
      } else if (is.list(x) && "test_type" %in% names(x) && grepl("Normality", x$test_type %||% "")) {
        private$.interpret_normality(x, audience, list(...))
      } else {
        "Unable to recognize statistical result type. Please provide htest, interval_result, regression_result, resampling_result, aov, probability distribution, or normality test result."
      }
    },

    #' @description Interpret probability distribution result
    #' @param dist_result Distribution calculation result list or ProbNode object
    #' @param audience Audience level
    #' @return Interpretation string
    interpret_dist = function(dist_result, audience = "manager") {
      private$.interpret_dist(dist_result, audience)
    },

    #' @description Interpret hypothesis test result
    #' @param htest_result Hypothesis test result (object returned by t.test / chisq.test etc.)
    #' @param audience Audience level
    #' @return Interpretation string
    interpret_htest = function(htest_result, audience = "manager") {
      private$.interpret_htest(htest_result, audience)
    },

    #' @description Interpret ANOVA result
    #' @param anova_result ANOVA result
    #' @param audience Audience level
    #' @return Interpretation string
    interpret_anova = function(anova_result, audience = "manager") {
      private$.interpret_anova(anova_result, audience)
    },

    #' @description Interpret normality test result
    #' @param normality_result Normality test result list
    #' @param audience Audience level
    #' @param diagnose Diagnostic result (optional)
    #' @return Interpretation string
    interpret_normality = function(normality_result, audience = "manager", diagnose = NULL) {
      private$.interpret_normality(normality_result, audience, list(diagnose = diagnose))
    },

    #' @description Interpret resampling result (bootstrap CI / permutation test)
    #' @param resampling_result A `stat_result` from `ResamplingAnalyzer`.
    #' @param audience Audience level
    #' @return Interpretation string
    interpret_resampling = function(resampling_result, audience = "manager") {
      private$.interpret_resampling(resampling_result, audience)
    }
  ),

  private = list(
    # ============================================================================
    # Probability distribution interpretation
    # ============================================================================

    .interpret_dist = function(dist_result, audience) {
      # Extract information
      if (inherits(dist_result, "ProbNode")) {
        info <- dist_result$get_node_info()
        type <- info$type
        params <- info$params
        calc_result <- NULL  # Requires user to execute calc() first
      } else if (is.list(dist_result) && "type" %in% names(dist_result) && "calc_result" %in% names(dist_result)) {
        # Structure passed from iqr_prob's interpret()
        type <- dist_result$type
        params <- dist_result$params
        calc_result <- dist_result$calc_result
      } else if (is.list(dist_result) && "type" %in% names(dist_result)) {
        # Legacy compatibility: no calc_result field
        type <- dist_result$type
        params <- dist_result$params
        calc_result <- if (!is.null(dist_result$calc_result)) dist_result$calc_result else NULL
      } else {
        return("Unable to parse distribution information")
      }

      # Get distribution description
      dist_desc <- if (type %in% names(DIST_REGISTRY)) {
        DIST_REGISTRY[[type]]$description
      } else {
        ""
      }

      # Generate interpretation based on audience level
      switch(audience,
        "manager" = private$.dist_manager_explain(type, params, calc_result, dist_desc),
        "technical" = private$.dist_technical_explain(type, params, calc_result, dist_desc),
        "client" = private$.dist_client_explain(type, params, calc_result, dist_desc),
        private$.dist_manager_explain(type, params, calc_result, dist_desc)
      )
    },

    .dist_manager_explain = function(type, params, calc_result, dist_desc) {
      header <- paste0("Distribution Analysis Interpretation (Manager Version)")
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      # Build interpretation
      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Distribution Description]",
        dist_desc,
        ""
      )

      # If there are calculation results
      if (!is.null(calc_result) && !is.null(calc_result$all_res)) {
        for (item in calc_result$all_res) {
          if (item$is_prob_mode) {
            x_val <- round(unlist(item$target_x), 4)
            p_val <- round(item$result_p, 4)
            pct <- round(p_val * 100, 1)

            lines <- c(lines,
              "[Calculation Result]",
              private$.format_prob_result(type, params, x_val, p_val, pct),
              "",
              "[Business Recommendation]",
              private$.get_business_advice(type, params, p_val),
              ""
            )
          } else {
            p_in <- round(item$target_p, 4)
            x_out <- round(item$result_x, 4)

            lines <- c(lines,
              "[Calculation Result]",
              sprintf("  Cumulative probability %.4f corresponds to quantile = %.4f", p_in, x_out),
              sprintf("  This means: in %.1f%% of cases, the result will be below %.4f", p_in * 100, x_out),
              ""
            )
          }
        }
      }

      paste(lines, collapse = "\n")
    },

    .dist_technical_explain = function(type, params, calc_result, dist_desc) {
      header <- paste0("Distribution Analysis Interpretation (Technical Version)")
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Distribution Information]",
        sprintf("  Type: %s", type),
        sprintf("  Description: %s", dist_desc),
        sprintf("  Parameters: %s", paste(names(params), "=", params, collapse = ", ")),
        ""
      )

      if (!is.null(calc_result) && !is.null(calc_result$all_res)) {
        for (item in calc_result$all_res) {
          if (item$is_prob_mode) {
            x_val <- round(unlist(item$target_x), 4)
            p_val <- round(item$result_p, 6)

            lines <- c(lines,
              "[Calculation Result]",
              sprintf("  P(X <= %s) = %.6f", paste(x_val, collapse = ", "), p_val),
              "",
              "[Statistical Interpretation]",
              private$.get_technical_interpretation(type, params, x_val, p_val),
              ""
            )
          }
        }
      }

      paste(lines, collapse = "\n")
    },

    .dist_client_explain = function(type, params, calc_result, dist_desc) {
      header <- paste0("Quality Assurance Report")
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Statistical Method]",
        sprintf("  This analysis uses %s for statistical inference.", dist_desc),
        "  This method is widely recognized and applied in quality management and statistics.",
        ""
      )

      if (!is.null(calc_result) && !is.null(calc_result$all_res)) {
        for (item in calc_result$all_res) {
          if (item$is_prob_mode) {
            p_val <- round(item$result_p, 4)
            pct <- round(p_val * 100, 1)

            lines <- c(lines,
              "[Analysis Conclusion]",
              sprintf("  Based on statistical analysis, the probability of this event occurring is %.1f%%.", pct),
              "  This result indicates the process is under control.",
              ""
            )
          }
        }
      }

      paste(lines, collapse = "\n")
    },

    # ============================================================================
    # Hypothesis test interpretation
    # ============================================================================

    .interpret_htest = function(htest_result, audience) {
      method <- htest_result$method
      p_val <- htest_result$p.value
      statistic <- unlist(htest_result$statistic)
      conf_int <- htest_result$conf.int
      tt <- htest_result$test_type %||% ""

      # Equivalence / non-inferiority / superiority tests use a reversed
      # conclusion logic: a small p-value CONFIRMS the desired claim, which
      # is the opposite of standard difference-detecting tests. Branch off
      # into a dedicated interpreter for these test types.
      if (tt %in% c("tost_mean", "tost_proportion",
                    "non_inferiority", "superiority")) {
        return(private$.interpret_equivalence(htest_result, audience))
      }

      # Correlation tests: a small p-value indicates a significant association
      # (rather than a difference). Branch off to a dedicated interpreter.
      if (tt %in% c("cor_test_pearson", "cor_test_spearman", "cor_test_kendall")) {
        return(private$.interpret_correlation(htest_result, audience))
      }

      # Variance-equality tests (Levene / Bartlett): branch off to a dedicated
      # interpreter that reports per-group variances and the equal-variance
      # assumption used by downstream t-tests / ANOVA.
      if (tt %in% c("levene_test", "bartlett_test")) {
        return(private$.interpret_variance_equality(htest_result, audience))
      }

      # Determine significance
      sig_005 <- p_val < 0.05
      sig_001 <- p_val < 0.01

      # Wording: pick a comparison scope based on the test_type so the
      # conclusion text fits k-sample / block designs as well as 2-sample tests.
      scope <- switch(tt,
        "kruskal_wallis"    = "the groups",
        "friedman"          = "the treatments across blocks",
        "wilcoxon_rank_sum" = "the two groups",
        "wilcoxon_signed_rank" = if (isTRUE(htest_result$paired))
                                   "the paired observations" else "the sample and the hypothesized location",
        "chisq_test"        = "the observed and expected frequencies",
        "f_test"            = "the two variances",
        "prop_test_2s"      = "the two proportions",
        "poisson_test_1s"   = "the observed count and the hypothesized rate",
        "poisson_test_2s"   = "the two event rates",
        "the two groups/processes"
      )

      header <- switch(audience,
        "manager" = "Hypothesis Test Result Interpretation (Manager Version)",
        "technical" = "Hypothesis Test Result Interpretation (Technical Version)",
        "client" = "Quality Assurance Report",
        "Hypothesis Test Result Interpretation"
      )
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Test Method]",
        sprintf("  %s", method),
        "",
        "[Core Result]",
        sprintf("  P Value = %.4f", p_val),
        sprintf("  Test Statistic = %.4f", statistic),
        ""
      )

      # Add different interpretations based on audience level
      if (audience == "manager") {
        if (sig_001) {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  P Value = %.4f < 0.01, result is highly significant.", p_val),
            sprintf("  We can be confident that there is a substantial difference between %s,", scope),
            "  and this difference is very unlikely to be caused by random fluctuation.",
            "  Recommendation: The root cause of the difference should be investigated and appropriate measures taken."
          )
        } else if (sig_005) {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  P Value = %.4f < 0.05, result is statistically significant.", p_val),
            sprintf("  There is sufficient evidence that there is a difference between %s,", scope),
            "  and this difference is unlikely to be caused by random fluctuation.",
            "  Recommendation: The source of the difference should be investigated and the process evaluated for adjustment."
          )
        } else {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  P Value = %.4f > 0.05, result is not significant.", p_val),
            sprintf("  There is insufficient evidence of a substantial difference between %s.", scope),
            "  The observed difference is likely due to random fluctuation.",
            "  Recommendation: If the difference is considered important in business, consider increasing sample size and retesting."
          )
        }
      } else {
        # Technical/client version
        if (!is.null(conf_int)) {
          lines <- c(lines,
            "[Confidence Interval]",
            sprintf("  95%% CI: [%.4f, %.4f]", conf_int[1], conf_int[2]),
            ""
          )
        }

        lines <- c(lines,
          "[Statistical Interpretation]",
          sprintf("  At significance level alpha = 0.05, %sreject the null hypothesis.",
                  ifelse(sig_005, "", "do not ")),
          sprintf("  P Value = %.4f means: if the null hypothesis is true, the probability of observing the current or more extreme result is %.2f%%.",
                  p_val, p_val * 100)
        )
      }

      paste(c(lines, "", separator), collapse = "\n")
    },

    # ============================================================================
    # Equivalence / non-inferiority / superiority interpretation
    # ============================================================================
    .interpret_equivalence = function(htest_result, audience) {
      method <- htest_result$method
      p_val <- htest_result$p.value
      statistic <- unlist(htest_result$statistic)
      conf_int <- htest_result$conf.int
      tt <- htest_result$test_type
      delta <- htest_result$delta

      # Decide the claim label and the binary outcome flag carried on the result
      claim <- switch(tt,
        "tost_mean"       = "mean equivalence",
        "tost_proportion" = "proportion equivalence",
        "non_inferiority" = "non-inferiority",
        "superiority"     = "superiority",
        "the desired claim"
      )

      confirmed <- switch(tt,
        "tost_mean"       = identical(htest_result$equivalence, "equivalent"),
        "tost_proportion" = identical(htest_result$equivalence, "equivalent"),
        "non_inferiority" = isTRUE(htest_result$non_inferior),
        "superiority"     = isTRUE(htest_result$superior),
        FALSE
      )

      header <- switch(audience,
        "manager"   = sprintf("%s Test Result Interpretation (Manager Version)",
                              tools::toTitleCase(claim)),
        "technical" = sprintf("%s Test Result Interpretation (Technical Version)",
                              tools::toTitleCase(claim)),
        "client"    = sprintf("%s Quality Assurance Report",
                              tools::toTitleCase(claim)),
        sprintf("%s Test Result Interpretation", tools::toTitleCase(claim))
      )
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Test Method]",
        sprintf("  %s", method),
        "",
        "[Core Result]",
        sprintf("  P Value = %.4f", p_val),
        sprintf("  Test Statistic = %.4f", statistic[1]),
        sprintf("  Equivalence / Margin (delta) = %.4f", delta),
        sprintf("  Conclusion: %s is %sconfirmed.",
                claim, ifelse(confirmed, "", "NOT ")),
        ""
      )

      if (!is.null(conf_int)) {
        lines <- c(lines,
          "[Confidence Interval]",
          sprintf("  %.1f%% CI: [%.4f, %.4f]",
                  100 * (htest_result$conf.level %||% 0.95),
                  conf_int[1], conf_int[2]),
          ""
        )
      }

      if (audience == "manager") {
        if (confirmed) {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  P Value = %.4f confirms %s at the chosen significance level.", p_val, claim),
            "  The evidence supports the desired practical conclusion (the treatment is within the margin).",
            "  Recommendation: The process/product can be considered to meet the equivalence / non-inferiority / superiority criterion."
          )
        } else {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  P Value = %.4f does NOT confirm %s.", p_val, claim),
            "  The evidence is insufficient to declare the desired practical conclusion.",
            "  Recommendation: Increase sample size, reduce variability, or re-examine the equivalence margin."
          )
        }
      } else {
        lines <- c(lines,
          "[Statistical Interpretation]",
          sprintf("  At significance level alpha = 0.05, %sconfirm the %s claim.",
                  ifelse(confirmed, "", "do NOT "), claim),
          sprintf("  P Value = %.4f means: under the null (no %s), the probability of observing the current or more extreme result is %.2f%%.",
                  p_val, claim, p_val * 100)
        )
      }

      paste(c(lines, "", separator), collapse = "\n")
    },

    # ============================================================================
    # Correlation test interpretation
    # ============================================================================
    .interpret_correlation = function(htest_result, audience) {
      method <- htest_result$method
      p_val <- htest_result$p.value
      statistic <- unlist(htest_result$statistic)
      conf_int <- htest_result$conf.int
      tt <- htest_result$test_type
      est <- as.numeric(htest_result$estimate[1])
      est_name <- names(htest_result$estimate)[1] %||% "correlation"
      n <- htest_result$n %||% NA

      sig_005 <- p_val < 0.05
      sig_001 <- p_val < 0.01

      # Strength-of-association wording based on |estimate|
      abs_est <- abs(est)
      strength <- if (abs_est < 0.1) {
        "negligible"
      } else if (abs_est < 0.3) {
        "weak"
      } else if (abs_est < 0.5) {
        "moderate"
      } else if (abs_est < 0.7) {
        "moderately strong"
      } else if (abs_est < 0.9) {
        "strong"
      } else {
        "very strong"
      }
      direction <- if (est > 0) "positive" else if (est < 0) "negative" else "no"

      header <- switch(audience,
        "manager"   = "Correlation Test Result Interpretation (Manager Version)",
        "technical" = "Correlation Test Result Interpretation (Technical Version)",
        "client"    = "Correlation Quality Assurance Report",
        "Correlation Test Result Interpretation"
      )
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Test Method]",
        sprintf("  %s", method),
        "",
        "[Core Result]",
        sprintf("  %s = %.4f", est_name, est),
        sprintf("  P Value = %.4f", p_val),
        sprintf("  Test Statistic = %.4f", statistic[1]),
        sprintf("  Sample size = %s", if (is.na(n)) "NA" else format(n)),
        ""
      )

      if (!is.null(conf_int) && length(conf_int) == 2L) {
        lines <- c(lines,
          "[Confidence Interval]",
          sprintf("  %.1f%% CI: [%.4f, %.4f]",
                  100 * (htest_result$conf.level %||% 0.95),
                  conf_int[1], conf_int[2]),
          ""
        )
      }

      if (audience == "manager") {
        if (sig_001) {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  P Value = %.4f < 0.01, the correlation is highly significant.", p_val),
            sprintf("  There is a %s %s correlation between the two variables (%s = %.4f).",
                    strength, direction, est_name, est),
            "  This association is very unlikely to be caused by random fluctuation.",
            "  Recommendation: The relationship can be relied upon for decision-making, but correlation does not imply causation -- investigate the underlying mechanism."
          )
        } else if (sig_005) {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  P Value = %.4f < 0.05, the correlation is statistically significant.", p_val),
            sprintf("  There is a %s %s correlation between the two variables (%s = %.4f).",
                    strength, direction, est_name, est),
            "  This association is unlikely to be caused by random fluctuation.",
            "  Recommendation: The relationship is worth further investigation; confirm with domain knowledge or additional data."
          )
        } else {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  P Value = %.4f > 0.05, the correlation is not statistically significant.", p_val),
            sprintf("  The observed %s %s correlation (%s = %.4f) could plausibly be due to random fluctuation.",
                    strength, direction, est_name, est),
            "  Recommendation: There is insufficient evidence of a linear/monotonic association; consider larger samples or alternative models."
          )
        }
      } else {
        lines <- c(lines,
          "[Statistical Interpretation]",
          sprintf("  At significance level alpha = 0.05, %sreject the null hypothesis (no correlation).",
                  ifelse(sig_005, "", "do not ")),
          sprintf("  P Value = %.4f means: if the true correlation is zero, the probability of observing |%s| >= %.4f is %.2f%%.",
                  p_val, est_name, abs_est, p_val * 100),
          sprintf("  Effect size: %s %s association (%s = %.4f).",
                  strength, direction, est_name, est)
        )
      }

      paste(c(lines, "", separator), collapse = "\n")
    },

    # ============================================================================
    # Variance-equality tests (Levene / Bartlett) interpretation
    # ============================================================================

    .interpret_variance_equality = function(htest_result, audience) {
      method <- htest_result$method
      p_val  <- htest_result$p.value
      statistic <- unlist(htest_result$statistic)
      tt     <- htest_result$test_type
      k      <- htest_result$k %||% NA
      group_n   <- htest_result$group_n
      group_var <- htest_result$group_var
      center    <- htest_result$center

      sig_005 <- p_val < 0.05
      sig_001 <- p_val < 0.01

      header <- switch(audience,
        "manager"   = "Variance Equality Test Interpretation (Manager Version)",
        "technical" = "Variance Equality Test Interpretation (Technical Version)",
        "client"    = "Variance Homogeneity Quality Report",
        "Variance Equality Test Interpretation"
      )
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Test Method]",
        sprintf("  %s", method),
        "",
        "[Core Result]",
        sprintf("  P Value = %.4f", p_val),
        sprintf("  Test Statistic = %.4f", statistic[1]),
        sprintf("  Number of groups = %s", if (is.na(k)) "NA" else format(k)),
        ""
      )

      # Per-group variance table
      if (!is.null(group_var) && length(group_var) > 0L) {
        lines <- c(lines,
          "[Per-Group Variances]",
          sprintf("  %-12s %8s %12s", "group", "n", "variance")
        )
        for (nm in names(group_var)) {
          lines <- c(lines,
            sprintf("  %-12s %8s %12.4f",
                    nm,
                    if (is.null(group_n)) "-" else group_var[nm],
                    as.numeric(group_var[nm]))
          )
        }
        # Variance ratio (max / min) as a practical magnitude indicator
        v_vals <- as.numeric(group_var)
        v_ratio <- max(v_vals, na.rm = TRUE) / min(v_vals, na.rm = TRUE)
        lines <- c(lines,
          sprintf("  Variance ratio (max / min) = %.2f", v_ratio),
          ""
        )
      }

      if (audience == "manager") {
        if (sig_001) {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  P Value = %.4f < 0.01, variances differ highly significantly across groups.", p_val),
            "  The assumption of equal variances is NOT supported.",
            "  Practical implication: Use Welch's t-test (not the pooled t-test) or Welch's ANOVA for follow-up mean comparisons.",
            "  Recommendation: Investigate whether one group has a much larger spread -- this often signals a process instability or a subgroup effect."
          )
        } else if (sig_005) {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  P Value = %.4f < 0.05, variances differ significantly across groups.", p_val),
            "  The assumption of equal variances is questionable.",
            "  Recommendation: Prefer Welch's variants of t-test / ANOVA, or apply a variance-stabilizing transformation."
          )
        } else {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  P Value = %.4f > 0.05, no significant difference in variances across groups.", p_val),
            "  The assumption of equal variances is reasonable.",
            "  Practical implication: Standard (pooled) t-tests and classic ANOVA can be used for follow-up mean comparisons."
          )
        }
      } else {
        # technical / client
        stat_name <- names(statistic)[1] %||% "statistic"
        dist_label <- switch(tt,
          "levene_test"   = "F(df1, df2)",
          "bartlett_test" = "K-squared(df)",
          "distribution"
        )
        lines <- c(lines,
          "[Statistical Interpretation]",
          sprintf("  At alpha = 0.05, %sreject H0 (equal variances).",
                  ifelse(sig_005, "", "do not ")),
          sprintf("  %s = %.4f under %s.", stat_name, statistic[1], dist_label),
          sprintf("  P Value = %.4f means: if all groups truly have equal variances, the probability of observing an %s >= %.4f is %.2f%%.",
                  p_val, stat_name, statistic[1], p_val * 100)
        )
        if (tt == "levene_test" && !is.null(center)) {
          lines <- c(lines,
            sprintf("  Levene center: %s (%s).",
                    center,
                    if (center == "median") "Brown-Forsythe, robust to non-normality"
                    else "classic Levene, more powerful under normality")
          )
        }
        if (tt == "bartlett_test") {
          lines <- c(lines,
            "  Note: Bartlett's test assumes normality and is sensitive to non-normality.",
            "  If normality is in doubt, prefer Levene's test (center = median)."
          )
        }
      }

      paste(c(lines, "", separator), collapse = "\n")
    },

    # ============================================================================
    # Interval estimation interpretation (ci / pi / tolerance / moe)
    # ============================================================================

    .interpret_interval = function(result, audience) {
      tt <- result$test_type %||% "interval"
      method <- result$method %||% tt
      ci <- result$conf.int
      conf_level <- result$conf.level %||% NA
      n <- result$n %||% NA
      est <- if (!is.null(result$estimate)) as.numeric(result$estimate[1]) else
             as.numeric(result$statistic[1])
      est_name <- if (!is.null(result$estimate)) names(result$estimate)[1] else
                  names(result$statistic)[1] %||% "estimate"

      header <- switch(audience,
        "manager"   = "Interval Estimate Interpretation (Manager Version)",
        "technical" = "Interval Estimate Interpretation (Technical Version)",
        "client"    = "Interval Estimate Quality Report",
        "Interval Estimate Interpretation"
      )
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      # Build the interval-type-specific label
      interval_label <- switch(tt,
        "ci_mean"            = "Confidence interval for the population mean",
        "ci_proportion"      = "Confidence interval for the population proportion",
        "ci_variance"        = "Confidence interval for the population variance",
        "ci_diff_mean"       = "Confidence interval for the difference of two means",
        "tolerance_interval" = "Tolerance interval (k-content, p-coverage)",
        "margin_of_error"    = "Margin of error",
        "pi_mean"            = "Prediction interval for one future observation",
        "Interval estimate"
      )

      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Interval Type]",
        sprintf("  %s", interval_label),
        sprintf("  Method: %s", method),
        "",
        "[Core Result]",
        sprintf("  %s = %.4f", est_name, est),
        sprintf("  %.1f%% interval: [%.4f, %.4f]",
                (conf_level %||% NA) * 100, ci[1], ci[2]),
        sprintf("  Interval width = %.4f", ci[2] - ci[1]),
        sprintf("  Sample size n = %s", if (is.na(n)) "NA" else format(n)),
        ""
      )

      # Type-specific extras
      if (tt == "tolerance_interval") {
        p_content <- result$p_content %||% NA
        k_factor  <- result$k_factor %||% NA
        lines <- c(lines,
          "[Tolerance Interval Parameters]",
          sprintf("  Content p = %.3f (captures >= %.1f%% of the population)",
                  p_content, (p_content %||% NA) * 100),
          sprintf("  Confidence = %.3f", conf_level),
          sprintf("  k factor = %.4f", k_factor),
          sprintf("  Side: %s", result$side %||% "two-sided"),
          ""
        )
      } else if (tt == "ci_variance") {
        lines <- c(lines,
          "[Variance / SD Interval]",
          sprintf("  Variance CI: [%.4f, %.4f]", result$var_lower, result$var_upper),
          sprintf("  SD CI: [%.4f, %.4f]", result$sd_lower, result$sd_upper),
          ""
        )
      } else if (tt == "ci_diff_mean") {
        lines <- c(lines,
          "[Two-Sample Difference]",
          sprintf("  mean1 = %.4f (n1 = %d)", result$mean1, result$n1),
          sprintf("  mean2 = %.4f (n2 = %d)", result$mean2, result$n2),
          sprintf("  Difference = %.4f", result$mean1 - result$mean2),
          sprintf("  Variances assumed %s",
                  if (isTRUE(result$var_equal)) "equal (pooled)" else "unequal (Welch)"),
          ""
        )
      } else if (tt == "margin_of_error") {
        lines <- c(lines,
          "[Margin of Error]",
          sprintf("  MOE = %.4f", as.numeric(result$statistic[1])),
          sprintf("  Standard error = %.4f", result$se %||% NA),
          sprintf("  Type: %s", result$type %||% "mean"),
          ""
        )
      } else if (tt == "pi_mean") {
        pi_mode <- result$pi_mode %||% "sample"
        if (pi_mode == "model") {
          lines <- c(lines,
            "[Prediction Interval (model-based)]",
            sprintf("  Model: %s", result$model_call %||% "(lm)"),
            sprintf("  Predictions: %d row(s) in result$prediction_table",
                    result$n %||% NA),
            if (is.na(result$se_pred %||% NA)) NULL else
              sprintf("  SE (prediction, first row) = %.4f", result$se_pred),
            "  Each row of newdata gets its own prediction interval via",
            "  predict.lm(interval='prediction'). The interval covers ONE",
            "  future observation at those predictor values, not the mean",
            "  response (which would be narrower).",
            ""
          )
        } else {
          lines <- c(lines,
            "[Prediction Interval]",
            sprintf("  SE (prediction) = %.4f", result$se_pred %||% NA),
            "  This interval predicts the range of ONE future observation,",
            "  not the mean. It is wider than the CI for the mean because",
            "  it includes both sampling error and individual variation.",
            ""
          )
        }
      }

      # Audience-specific conclusion
      if (audience == "manager") {
        conclusion <- switch(tt,
          "ci_mean" = sprintf(
            "We are %.0f%% confident that the true process mean lies between %.4f and %.4f.",
            conf_level * 100, ci[1], ci[2]),
          "ci_proportion" = sprintf(
            "We are %.0f%% confident that the true defect rate lies between %.2f%% and %.2f%%.",
            conf_level * 100, ci[1] * 100, ci[2] * 100),
          "ci_variance" = sprintf(
            "We are %.0f%% confident that the true process variance lies between %.4f and %.4f.",
            conf_level * 100, result$var_lower, result$var_upper),
          "ci_diff_mean" = sprintf(
            "We are %.0f%% confident that the difference between the two process means lies between %.4f and %.4f. %s",
            conf_level * 100, ci[1], ci[2],
            if (ci[1] > 0 || ci[2] < 0)
              "The difference is statistically significant (interval excludes 0)."
            else
              "The difference is NOT statistically significant (interval includes 0)."),
          "tolerance_interval" = sprintf(
            "With %.0f%% confidence, at least %.1f%% of all future output will fall between %.4f and %.4f. Compare these bounds to your specification limits to assess process capability.",
            conf_level * 100, (result$p_content %||% NA) * 100, ci[1], ci[2]),
          "margin_of_error" = sprintf(
            "The estimate is accurate to within +/-%.4f at %.0f%% confidence. A larger sample would narrow this margin.",
            as.numeric(result$statistic[1]), conf_level * 100),
          "pi_mean" = sprintf(
            "We are %.0f%% confident that the NEXT single measurement will fall between %.4f and %.4f. Use this for individual-unit acceptance decisions, not for averaging.",
            conf_level * 100, ci[1], ci[2]),
          sprintf("Interval: [%.4f, %.4f]", ci[1], ci[2])
        )
        lines <- c(lines, "[Conclusion]", sprintf("  %s", conclusion))
      } else {
        # technical / client
        lines <- c(lines,
          "[Statistical Interpretation]",
          sprintf("  At confidence level %.1f%%, the interval [%s, %s] is reported.",
                  conf_level * 100,
                  sprintf("%.4f", ci[1]), sprintf("%.4f", ci[2])),
          sprintf("  Interval type: %s", tt),
          sprintf("  Distribution basis: %s", result$dist_type %||% "unknown")
        )
        if (tt == "tolerance_interval") {
          lines <- c(lines,
            "  A tolerance interval answers 'what range covers a proportion p of the population?',",
            "  distinct from a CI (which covers a parameter) and a PI (which covers one future obs)."
          )
        }
      }

      paste(c(lines, "", separator), collapse = "\n")
    },

    # ============================================================================
    # Resampling interpretation (bootstrap CI / permutation test)
    # ============================================================================

    .interpret_resampling = function(result, audience) {
      tt <- result$test_type %||% "resampling"
      method <- result$method %||% tt
      theta <- as.numeric(result$statistic[1])

      header <- switch(audience,
        "manager"   = "Resampling Analysis Interpretation (Manager Version)",
        "technical" = "Resampling Analysis Interpretation (Technical Version)",
        "client"    = "Resampling Analysis Quality Report",
        "Resampling Analysis Interpretation"
      )
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Procedure]",
        sprintf("  %s", method),
        sprintf("  Data: %s", result$data_name %||% ""),
        ""
      )

      if (tt == "bootstrap_ci") {
        ci <- result$conf.int
        cl <- (result$conf.level %||% NA) * 100
        bias <- result$boot_bias %||% NA
        se <- result$boot_se %||% NA
        lo_str <- if (is.infinite(ci[1])) "-Inf" else sprintf("%.4f", ci[1])
        hi_str <- if (is.infinite(ci[2]))  "Inf" else sprintf("%.4f", ci[2])

        lines <- c(lines,
          "[Core Result]",
          sprintf("  Observed statistic = %.4f", theta),
          sprintf("  %.0f%% confidence interval (%s): [%s, %s]",
                  cl, toupper(result$boot_method %||% "BCA"), lo_str, hi_str),
          sprintf("  Bootstrap bias = %.4f", bias),
          sprintf("  Bootstrap standard error = %.4f", se),
          sprintf("  Replicates R = %d, sample size n = %s",
                  result$R %||% NA,
                  if (is.na(result$n)) "NA" else format(result$n)),
          ""
        )

        # Method-specific technical notes
        if (audience == "technical") {
          method_note <- switch(result$boot_method %||% "bca",
            "bca"   = "BCa adjusts both for bias (z0) and for skewness in the sampling distribution (acceleration a via jackknife). It is second-order accurate and transformation-respecting -- the recommended default.",
            "perc"  = "Percentile interval takes empirical quantiles of the bootstrap distribution. Simple and transformation-respecting, but first-order accurate only; can be biased for skewed statistics.",
            "basic" = "Basic (pivotal) interval mirrors the percentile interval around the observed statistic. First-order accurate; not transformation-respecting.",
            "norm"  = "Normal approximation interval assumes the bootstrap distribution is approximately normal. Fastest but least reliable for skewed or biased statistics."
          )
          lines <- c(lines,
            "[Method Note]",
            sprintf("  %s", method_note),
            ""
          )
        }

        # Audience-specific conclusion
        if (audience == "manager") {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  We are %.0f%% confident that the true value of the statistic lies between %s and %s.",
                    cl, lo_str, hi_str),
            if (abs(bias) > 2 * se)
              "  Note: the bootstrap bias is large relative to the standard error, suggesting the statistic may be a biased estimator -- interpret with care."
            else
              "  The bootstrap bias is small relative to the standard error, so the estimate is reliable.",
            ""
          )
        } else if (audience == "technical") {
          lines <- c(lines,
            "[Conclusion]",
            sprintf("  %s%% %s interval for the population value: [%s, %s].",
                    format(cl), toupper(result$boot_method %||% "BCA"),
                    lo_str, hi_str),
            sprintf("  Bias/sqrt(MSE) check: |bias| = %.4f vs se = %.4f.",
                    abs(bias), se),
            ""
          )
        } else {
          lines <- c(lines,
            "[Quality Assurance Conclusion]",
            sprintf("  Based on %d bootstrap resamples, the estimated value is %.4f with a %.0f%% confidence interval of [%s, %s].",
                    result$R %||% NA, theta, cl, lo_str, hi_str),
            "  This non-parametric procedure makes no distributional assumption and is robust to non-normal data.",
            ""
          )
        }
      } else if (tt == "permutation_test") {
        p_val <- result$p.value
        alt <- result$alternative %||% "two.sided"
        design <- result$design %||% ""
        p_str <- if (is.na(p_val)) "NA"
                 else if (p_val < 1e-4) "<1e-04"
                 else sprintf("%.4f", p_val)
        sig <- !is.na(p_val) && p_val < 0.05

        lines <- c(lines,
          "[Core Result]",
          sprintf("  Observed statistic = %.4f", theta),
          sprintf("  p-value = %s (alternative: %s)", p_str, alt),
          sprintf("  Resamples R = %d, n = %s, design = %s",
                  result$R %||% NA,
                  if (is.na(result$n)) "NA" else format(result$n),
                  design),
          ""
        )

        if (audience == "technical") {
          design_note <- switch(design,
            "two-sample"  = "Two-sample design: the group labels on the pooled data are randomly shuffled to build the null distribution of the difference of means.",
            "paired"      = "Paired design: the signs of the paired differences are randomly flipped to build the null distribution of the mean difference.",
            "one-sample"  = "One-sample design: the signs of (x - mu) are randomly flipped to build the null distribution of the mean.",
            "Labels are reshuffled under the null hypothesis to build the empirical null distribution."
          )
          lines <- c(lines,
            "[Design Note]",
            sprintf("  %s", design_note),
            "  The p-value uses the (count + 1) / (R + 1) convention so it is strictly positive.",
            ""
          )
        }

        if (audience == "manager") {
          lines <- c(lines,
            "[Conclusion]",
            if (sig) {
              if (alt == "two.sided")
                sprintf("  p = %s < 0.05: there is a statistically significant difference between the groups.", p_str)
              else if (alt == "greater")
                sprintf("  p = %s < 0.05: the statistic is significantly greater than the null value.", p_str)
              else
                sprintf("  p = %s < 0.05: the statistic is significantly less than the null value.", p_str)
            } else {
              sprintf("  p = %s >= 0.05: no statistically significant evidence of a difference.", p_str)
            },
            "  This permutation test makes no distributional assumption -- it is valid even for non-normal data.",
            ""
          )
        } else if (audience == "technical") {
          lines <- c(lines,
            "[Conclusion]",
            if (sig)
              sprintf("  Reject H0 at alpha = 0.05 (p = %s).", p_str)
            else
              sprintf("  Fail to reject H0 at alpha = 0.05 (p = %s).", p_str),
            ""
          )
        } else {
          lines <- c(lines,
            "[Quality Assurance Conclusion]",
            if (sig)
              sprintf("  The observed difference is statistically significant (p = %s).", p_str)
            else
              sprintf("  No statistically significant difference was found (p = %s).", p_str),
            "  The non-parametric permutation procedure is robust and assumption-light.",
            ""
          )
        }
      }

      paste(c(lines, separator), collapse = "\n")
    },

    # ============================================================================
    # Regression interpretation
    # ============================================================================

    .interpret_regression = function(result, audience) {
      tt <- result$test_type
      method <- result$method
      cf <- result$coefficients
      ms <- result$model_stats
      header <- switch(audience,
        "manager"   = "Regression Model Interpretation (Manager Version)",
        "technical" = "Regression Model Interpretation (Technical Version)",
        "client"    = "Regression Model Quality Report",
        "Regression Model Interpretation"
      )
      separator <- paste0(rep("-", nchar(header)), collapse = "")
      lines <- c(separator, header, separator, "", "[Model Type]",
        sprintf("  %s", method), "", "[Model Fit]")
      if (tt == "lm_fit" || (tt == "stepwise_fit" && !is.na(ms$r_squared))) {
        lines <- c(lines,
          sprintf("  R-squared = %.4f (%.1f%% of variance explained)", ms$r_squared, ms$r_squared * 100),
          sprintf("  Adjusted R-squared = %.4f", ms$adj_r_squared))
        if (!is.null(ms$f_statistic))
          lines <- c(lines, sprintf("  F-statistic = %.4f, p-value = %.4f",
                                    as.numeric(ms$f_statistic[1]), ms$f_p_value))
        if (!is.na(ms$sigma))
          lines <- c(lines, sprintf("  Residual std error = %.4f on %d df", ms$sigma, ms$df_residual))
      } else if (tt == "cox_fit") {
        lines <- c(lines,
          sprintf("  Concordance (C-index) = %.4f", ms$concordance),
          sprintf("  R-squared (Nagelkerke-ish) = %.4f", ms$r_squared),
          sprintf("  Log-likelihood ratio test: deviance = %.4f, null deviance = %.4f", ms$deviance, ms$null_deviance),
          sprintf("  AIC = %.4f", ms$aic),
          sprintf("  n = %d, events = %d", ms$n, ms$n_events))
      } else if (tt == "pls_fit") {
        lines <- c(lines,
          sprintf("  Components used = %d", ms$ncomp),
          sprintf("  R-squared (Y) = %.4f (%.1f%% explained)", ms$r_squared, ms$r_squared * 100),
          sprintf("  RMSEP (CV) = %.4f", ms$sigma),
          sprintf("  n = %d", ms$n))
      } else if (tt == "best_subset_fit") {
        bb <- result$best_by_bic
        lines <- c(lines,
          sprintf("  Best model (by BIC) uses %d variables: %s",
                  bb$n_vars, paste(bb$variables, collapse = ", ")),
          sprintf("  BIC = %.4f, Adjusted R-squared = %.4f", bb$bic, bb$adj_r_squared),
          sprintf("  n = %d, predictors considered = %d", ms$n, ms$n_predictors))
      } else if (tt == "mars_fit") {
        lines <- c(lines,
          sprintf("  Generalized R-squared = %.4f (%.1f%% variance explained)",
                  ms$generalized_rsq, ms$generalized_rsq * 100),
          sprintf("  R-squared = %.4f", ms$rsq),
          sprintf("  GCV = %.4f (lower is better; penalizes model complexity)",
                  ms$gcv),
          sprintf("  Selected terms = %d, interaction degree = %d",
                  ms$n_terms, ms$degree),
          sprintf("  n = %d", ms$n))
      } else if (tt == "spline_fit") {
        lines <- c(lines,
          sprintf("  Basis = %s, df = %d, predictor = %s",
                  toupper(result$spline_basis), result$spline_df,
                  result$spline_predictor),
          sprintf("  R-squared = %.4f (%.1f%% variance explained)",
                  ms$r_squared, ms$r_squared * 100),
          sprintf("  Adjusted R-squared = %.4f", ms$adj_r_squared))
        if (!is.null(ms$f_statistic))
          lines <- c(lines, sprintf("  F-statistic = %.4f, p-value = %.4f",
                                    as.numeric(ms$f_statistic[1]), ms$f_p_value))
        if (!is.na(ms$sigma))
          lines <- c(lines, sprintf("  Residual std error = %.4f on %d df",
                                    ms$sigma, ms$df_residual))
        lines <- c(lines, sprintf("  n = %d", ms$n))
      } else {
        lines <- c(lines,
          sprintf("  Deviance = %.4f", ms$deviance),
          sprintf("  Null deviance = %.4f", ms$null_deviance),
          sprintf("  AIC = %.4f", ms$aic))
      }
      if (!(tt %in% c("best_subset_fit", "pls_fit", "mars_fit", "spline_fit"))) lines <- c(lines, sprintf("  n = %d", ms$n))
      lines <- c(lines, "")
      # Significant predictors (only for models with p-values)
      if (!is.null(cf) && "p_value" %in% names(cf)) {
        sig <- cf[!is.na(cf$p_value) & cf$p_value < 0.05, ]
        if (nrow(sig) > 0L) {
          lines <- c(lines, "[Significant Predictors (p < 0.05)]")
          for (i in seq_len(nrow(sig))) {
            lines <- c(lines, sprintf("  %s: estimate = %.4f, p = %.4f",
              sig$Term[i], sig$Estimate[i], sig$p_value[i]))
          }
        } else {
          lines <- c(lines, "[Significant Predictors]", "  None at p < 0.05")
        }
      } else if (tt == "best_subset_fit") {
        lines <- c(lines, "[Selected Variables]",
          sprintf("  Best by BIC: %s", paste(result$best_by_bic$variables, collapse = ", ")))
      } else if (tt == "pls_fit") {
        lines <- c(lines, "[Coefficients]",
          "  PLS coefficients lack traditional p-values; use cross-validation for component selection.")
      } else if (tt == "mars_fit") {
        lines <- c(lines, "[MARS Selected Terms]",
          sprintf("  Hinge functions: %s",
                  paste(result$selected_terms, collapse = ", ")))
      }
      lines <- c(lines, "")
      if (audience == "manager") {
        if (tt == "lm_fit" || (tt == "stepwise_fit" && !is.na(ms$r_squared))) {
          if (!is.null(ms$f_p_value) && !is.na(ms$f_p_value) && ms$f_p_value < 0.05) {
            lines <- c(lines, "[Conclusion]",
              sprintf("  The model is statistically significant (F-test p = %.4f) and explains %.1f%% of the variation in the response.", ms$f_p_value, ms$r_squared * 100))
          } else if (!is.null(ms$f_p_value) && !is.na(ms$f_p_value)) {
            lines <- c(lines, "[Conclusion]", "  The model is NOT statistically significant. Consider adding predictors or revisiting the analysis.")
          } else {
            lines <- c(lines, "[Conclusion]", sprintf("  R-squared = %.1f%%. Review variable selection results.", ms$r_squared * 100))
          }
        } else if (tt == "cox_fit") {
          lines <- c(lines, "[Conclusion]",
            sprintf("  Concordance = %.4f (>0.7 indicates good discrimination). %d events on %d subjects.",
                    ms$concordance, ms$n_events, ms$n))
        } else if (tt == "pls_fit") {
          lines <- c(lines, "[Conclusion]",
            sprintf("  PLS with %d components explains %.1f%% of Y variance. Lower RMSEP = better predictive accuracy.",
                    ms$ncomp, ms$r_squared * 100))
        } else if (tt == "best_subset_fit") {
          lines <- c(lines, "[Conclusion]",
            sprintf("  Best subset (by BIC) selected %d variables. Compare BIC across subset sizes for parsimony.",
                    result$best_by_bic$n_vars))
        } else if (tt == "mars_fit") {
          lines <- c(lines, "[Conclusion]",
            sprintf("  MARS explains %.1f%% of variance (Generalized R-squared). GCV = %.4f balances fit and complexity; prefer models with lower GCV.",
                    ms$generalized_rsq * 100, ms$gcv))
        } else if (tt == "spline_fit") {
          if (!is.null(ms$f_p_value) && !is.na(ms$f_p_value) && ms$f_p_value < 0.05) {
            lines <- c(lines, "[Conclusion]",
              sprintf("  The %s spline model is statistically significant (F-test p = %.4f) and explains %.1f%% of the variation in the response.",
                      toupper(result$spline_basis), ms$f_p_value, ms$r_squared * 100))
          } else {
            lines <- c(lines, "[Conclusion]",
              sprintf("  %s spline (df=%d) explains %.1f%% of variance. Increase df or try knots if the fit is poor.",
                      toupper(result$spline_basis), result$spline_df, ms$r_squared * 100))
          }
        } else {
          lines <- c(lines, "[Conclusion]", sprintf("  Model AIC = %.4f. Lower AIC indicates better fit. Compare with alternative models.", ms$aic))
        }
      } else {
        lines <- c(lines, "[Statistical Interpretation]",
          sprintf("  Distribution basis: %s", result$dist_type %||% "unknown"))
        if (tt == "logit_fit" && !is.null(result$odds_ratios)) {
          lines <- c(lines, "  Odds ratios (exp(coefficients)) are reported for logistic regression.")
        }
        if (tt == "poisson_fit" && !is.null(result$rate_ratios)) {
          lines <- c(lines, "  Rate ratios / IRR (exp(coefficients)) are reported for Poisson regression.")
        }
        if (tt == "cox_fit" && !is.null(result$hazard_ratios)) {
          lines <- c(lines, "  Hazard ratios (exp(coefficients)) are reported for Cox regression.")
        }
        if (tt == "stepwise_fit") {
          lines <- c(lines, sprintf("  Direction: %s, penalty k = %.2f (2=AIC, log(n)=BIC).",
                                    result$direction, result$penalty_k),
            sprintf("  Selected terms: %s", paste(result$selected_terms, collapse = ", ")))
        }
        if (tt == "mars_fit") {
          lines <- c(lines,
            sprintf("  Pruning method: %s, interaction degree: %d",
                    ms$pmethod, ms$degree),
            sprintf("  Selected terms: %s",
                    paste(result$selected_terms, collapse = ", ")))
        }
        if (tt == "spline_fit") {
          lines <- c(lines,
            sprintf("  Basis: %s, df = %d, degree = %s",
                    toupper(result$spline_basis), result$spline_df,
                    if (is.na(result$spline_degree)) "NA" else as.character(result$spline_degree)),
            sprintf("  Predictor expanded: %s", result$spline_predictor))
        }
      }
      paste(c(lines, "", separator), collapse = "\n")
    },

    # ============================================================================
    # ANOVA interpretation
    # ============================================================================

    .interpret_anova = function(anova_result, audience) {
      # Extract ANOVA table
      if (inherits(anova_result, "aov")) {
        anova_table <- summary(anova_result)[[1]]
      } else {
        anova_table <- anova_result
      }

      f_val <- anova_table[1, "F value"]
      p_val <- anova_table[1, "Pr(>F)"]

      sig_005 <- p_val < 0.05
      sig_001 <- p_val < 0.01

      header <- switch(audience,
        "manager" = "ANOVA Result Interpretation (Manager Version)",
        "technical" = "ANOVA Result Interpretation (Technical Version)",
        "client" = "ANOVA Quality Assurance Report",
        "ANOVA Result Interpretation"
      )
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Test Purpose]",
        "  Compare whether the means of three or more groups are significantly different.",
        "",
        "[Core Result]",
        sprintf("  F Value = %.2f", f_val),
        sprintf("  P Value = %.4f", p_val),
        ""
      )

      if (audience == "manager") {
        if (sig_005) {
          lines <- c(lines,
            "[Conclusion]",
            "  There is a significant difference in quality indicators between different groups.",
            "  This means at least one group is different from others,",
            "  and this difference is unlikely to be caused by random fluctuation.",
            "",
            "[Business Recommendation]",
            "  - It is recommended to further conduct pairwise comparisons (e.g., Tukey HSD) to identify which specific groups differ",
            "  - If groups represent different suppliers/processes/equipment, the impact of differences on quality should be evaluated",
            "  - If differences are unacceptable, investigate causes and take improvement measures"
          )
        } else {
          lines <- c(lines,
            "[Conclusion]",
            "  There is insufficient evidence that quality indicators differ significantly between groups.",
            "  The observed inter-group differences are likely due to random fluctuation.",
            "",
            "[Business Recommendation]",
            "  - From a statistical perspective, the quality performance of all groups is essentially consistent",
            "  - If differences are expected in business, consider increasing sample size and retesting"
          )
        }
      } else {
        lines <- c(lines,
          "[Statistical Interpretation]",
          sprintf("  At significance level alpha = 0.05, %sreject the null hypothesis (group means are equal).",
                  ifelse(sig_005, "", "do not ")),
          sprintf("  P Value = %.4f means: if the group means are truly equal,", p_val),
          sprintf("  the probability of observing the current or larger F value is %.2f%%.", p_val * 100)
        )
      }

      paste(c(lines, "", separator), collapse = "\n")
    },

    # ============================================================================
    # ANOM (Analysis of Means) interpretation
    # ============================================================================

    .interpret_anom = function(result, audience) {
      anom_table <- result$anom_table
      grand_mean <- result$grand_mean
      k <- result$k
      df_error <- result$df_error
      alpha <- result$alpha
      h_alpha <- result$h_alpha
      out_levels <- anom_table$level[anom_table$out_of_limits]
      n_out <- length(out_levels)

      header <- switch(audience,
        "manager" = "Analysis of Means (ANOM) Interpretation (Manager Version)",
        "technical" = "Analysis of Means (ANOM) Interpretation (Technical Version)",
        "client" = "ANOM Quality Assurance Report",
        "Analysis of Means (ANOM) Interpretation"
      )
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Test Purpose]",
        "  Identify which specific group levels deviate from the overall (grand) mean,",
        "  rather than only testing whether any difference exists (as ANOVA does).",
        "",
        "[Core Result]",
        sprintf("  Factor: %s (%d levels)", result$factor %||% "", k),
        sprintf("  Grand mean = %.4f", grand_mean),
        sprintf("  Decision limits at alpha = %.3f (h = %.3f, df_error = %d)",
                alpha, h_alpha, df_error),
        sprintf("  Levels outside decision limits: %d / %d", n_out, k)
      )

      if (n_out > 0) {
        lines <- c(lines,
          "",
          "[Out-of-Limits Levels]",
          paste0("  - ", out_levels, sprintf(" (mean = %.4f)",
                  anom_table$mean[anom_table$out_of_limits]))
        )
      }

      if (audience == "manager") {
        if (n_out > 0) {
          lines <- c(lines,
            "",
            "[Conclusion]",
            sprintf("  %d of %d levels differ significantly from the grand mean.", n_out, k),
            "  These levels represent unusual process/supplier conditions that warrant investigation.",
            "",
            "[Business Recommendation]",
            "  - Investigate root causes for the out-of-limits levels identified above",
            "  - For levels well above the grand mean, retain/replicate the favorable conditions",
            "  - For levels well below the grand mean, apply containment and corrective action"
          )
        } else {
          lines <- c(lines,
            "",
            "[Conclusion]",
            "  No individual level differs significantly from the grand mean.",
            "  The process is stable across all levels of the factor.",
            "",
            "[Business Recommendation]",
            "  - All levels perform consistently with the overall average",
            "  - No targeted intervention is required at this confidence level"
          )
        }
      } else {
        lines <- c(lines,
          "",
          "[Statistical Interpretation]",
          sprintf("  Using the Studentized-range approximation h = qtukey(%.3f, %d, %d)/sqrt(2) = %.4f,",
                  1 - alpha, k, df_error, h_alpha),
          sprintf("  each level mean is compared to the grand mean with simultaneous decision limits."),
          sprintf("  At alpha = %.3f, %s level(s) show a significant deviation from the grand mean.",
                  alpha, ifelse(n_out > 0, as.character(n_out), "no"))
        )
      }

      paste(c(lines, "", separator), collapse = "\n")
    },

    # ============================================================================
    # Normality test interpretation
    # ============================================================================

    .interpret_normality = function(result, audience, extra_args) {
      diagnose <- extra_args$diagnose
      p_val <- result$p.value
      stat_val <- as.numeric(result$statistic[1])
      stat_name <- names(result$statistic)[1]
      method <- result$method %||% "Normality Test"
      is_normal <- result$is_normal %||% FALSE
      alpha <- result$alpha %||% 0.05
      n <- result$n %||% NA
      skewness <- result$skewness %||% NA
      kurtosis <- result$excess_kurtosis %||% NA

      header <- switch(audience,
        "manager" = "Normality Test Result Interpretation (Manager Version)",
        "technical" = "Normality Test Result Interpretation (Technical Version)",
        "client" = "Normality Test Quality Assurance Report",
        "Normality Test Result Interpretation"
      )
      separator <- paste0(rep("-", nchar(header)), collapse = "")

      lines <- c(
        separator,
        header,
        separator,
        "",
        "[Test Purpose]",
        "  Determine whether the sample data follows a normal distribution.",
        "  Normality assumption is a prerequisite for many statistical methods (such as control charts, process capability analysis, t-tests).",
        "",
        "[Test Method]",
        sprintf("  %s", method),
        "",
        "[Core Result]",
        sprintf("  %s = %.4f", stat_name, stat_val),
        sprintf("  P Value = %s", if (p_val < 0.001) "<0.001" else sprintf("%.4f", p_val)),
        sprintf("  Alpha = %.2f", alpha),
        ""
      )

      if (audience == "manager") {
        if (is_normal) {
          lines <- c(lines,
            "[Conclusion] Data follows normal distribution",
            sprintf("  P Value = %s > %.2f, insufficient evidence to reject normality assumption.",
                    if (p_val < 0.001) "<0.001" else sprintf("%.4f", p_val), alpha),
            "  The data distribution is consistent with normal distribution,",
            "  and statistical tools based on normality assumption can be safely used.",
            ""
          )
        } else {
          lines <- c(lines,
            "[Conclusion] Data does not follow normal distribution",
            sprintf("  P Value = %s < %.2f, sufficient evidence to reject normality assumption.",
                    if (p_val < 0.001) "<0.001" else sprintf("%.4f", p_val), alpha),
            "  There is a significant difference between the data distribution and normal distribution.",
            ""
          )
        }

        lines <- c(lines,
          "[Business Recommendation]",
          if (is_normal) {
            c(
              "  - Data meets normality assumption, conventional statistical tools can be used directly",
              "  - Recommendation: Continue to maintain current data collection and analysis process"
            )
          } else {
            c(
              "  - Data does not meet normality assumption, consider the following options:",
              "    1. Transform the data (e.g., Box-Cox transformation) to make it closer to normal",
              "    2. Use non-parametric statistical methods (not dependent on normality assumption)",
              "    3. Increase sample size, use central limit theorem to approximate normal",
              "  - Recommendation: Choose the appropriate method based on the specific scenario"
            )
          },
          ""
        )
      } else if (audience == "technical") {
        lines <- c(lines,
          "[Statistical Interpretation]",
          sprintf("  At significance level alpha = %.2f, %sreject the null hypothesis (data follows normal distribution).",
                  alpha, ifelse(is_normal, "do not ", "")),
          sprintf("  P Value = %.6f means: if the data truly follows normal distribution,", p_val),
          sprintf("  the probability of observing the current or more extreme deviation from normal is %.4f%%.", p_val * 100),
          ""
        )

        if (!is.na(n)) {
          lines <- c(lines,
            "[Sample Information]",
            sprintf("  Sample size: %d", n),
            ""
          )
        }

        if (!is.na(skewness)) {
          lines <- c(lines,
            "[Distribution Shape]",
            sprintf("  Skewness: %.4f", skewness),
            if (abs(skewness) < 0.5) "  -> Approximately symmetric"
            else if (skewness > 0) "  -> Right-skewed (positive)"
            else "  -> Left-skewed (negative)",
            ""
          )
        }

        if (!is.na(kurtosis)) {
          lines <- c(lines,
            sprintf("  Excess kurtosis: %.4f", kurtosis),
            if (abs(kurtosis) < 0.5) "  -> Kurtosis close to normal"
            else if (kurtosis > 0) "  -> Leptokurtic (more concentrated than normal)"
            else "  -> Platykurtic (more dispersed than normal)",
            ""
          )
        }

        if (!is.null(diagnose)) {
          lines <- c(lines,
            "[Diagnostic Details]",
            sprintf("  Skewness direction: %s", diagnose$skewness_direction %||% ""),
            sprintf("  Kurtosis type: %s", diagnose$kurtosis_type %||% ""),
            ""
          )
        }
      } else {
        lines <- c(lines,
          "[Quality Assurance Statement]",
          sprintf("  This normality test uses the %s method at significance level alpha = %.2f.", method, alpha),
          if (is_normal) {
            "  The test result indicates that the data meets the normality assumption and statistical tools based on normal distribution can continue to be used."
          } else {
            "  The test result indicates that the data does not meet the normality assumption. It is recommended to use non-parametric methods or retest after data transformation."
          },
          "",
          "  This test is based on the provided sample data, and the conclusion only reflects the characteristics of the population represented by the sample.",
          ""
        )
      }

      paste(c(lines, "", separator), collapse = "\n")
    },

    # ============================================================================
    # Helper functions
    # ============================================================================

    .get_dist_en_name = function(type) {
      names_map <- c(
        norm = "Normal Distribution", weibull = "Weibull Distribution", lnorm = "Log-normal Distribution",
        gamma = "Gamma Distribution", exp = "Exponential Distribution", t = "t Distribution",
        f = "F Distribution", chisq = "Chi-square Distribution", binom = "Binomial Distribution",
        pois = "Poisson Distribution", nbinom = "Negative Binomial Distribution", hyper = "Hypergeometric Distribution",
        geom = "Geometric Distribution", beta = "Beta Distribution", unif = "Uniform Distribution",
        logis = "Logistic Distribution", cauchy = "Cauchy Distribution"
      )
      if (type %in% names(names_map)) names_map[type] else type
    },

    .format_prob_result = function(type, params, x_val, p_val, pct) {
      # Generate plain language interpretation based on distribution type
      if (type == "binom") {
        n <- params$size
        prob <- params$prob
        sprintf(
          "  In %d independent trials (each with success rate %.1f%%),\n  the probability of getting no more than %s successes = %.1f%%\n  This means: in every 100 samplings, approximately %.0f times the number of successes <= %s",
          n, prob * 100, paste(x_val, collapse = ", "), pct, pct, paste(x_val, collapse = ", ")
        )
      } else if (type == "pois") {
        lambda <- params$lambda
        sprintf(
          "  In unit time/space (average occurrence %.1f times),\n  the probability of occurrence not exceeding %s = %.1f%%\n  This means: in every 100 observations, approximately %.0f events have count <= %s",
          lambda, paste(x_val, collapse = ", "), pct, pct, paste(x_val, collapse = ", ")
        )
      } else if (type == "norm") {
        sprintf(
          "  The probability of this indicator falling within %s = %.1f%%\n  This means: in every 100 products, approximately %.0f have indicators within this range",
          paste(x_val, collapse = " ~ "), pct, pct
        )
      } else {
        sprintf(
          "  Calculation result: P = %.4f (%.1f%%)\n  This means the likelihood of this event occurring is %.1f%%",
          p_val, pct, pct
        )
      }
    },

    .get_business_advice = function(type, params, p_val) {
      # Give business advice based on probability value
      if (p_val >= 0.95) {
        "  - This probability level is high, the process is in good condition\n  - Recommendation: Continue to maintain current monitoring frequency"
      } else if (p_val >= 0.90) {
        "  - This probability level is relatively high, the process is basically under control\n  - Recommendation: Pay appropriate attention to trend changes"
      } else if (p_val >= 0.80) {
        "  - This probability level is acceptable but there is some risk\n  - Recommendation: Strengthen monitoring, pay attention to abnormal trends"
      } else if (p_val >= 0.50) {
        "  - This probability level is low, there are some quality risks\n  - Recommendation: Investigate causes, consider whether process adjustment is needed"
      } else {
        "  - This probability level is low, quality risk is high\n  - Recommendation: Immediately investigate root causes, take corrective measures"
      }
    },

    .get_technical_interpretation = function(type, params, x_val, p_val) {
      if (type == "binom") {
        sprintf(
          "  This is the cumulative probability (Cumulative Probability), not the simple event occurrence rate.\n  Binomial distribution B(n=%d, p=%.2f) describes the distribution of the number of successes in n independent Bernoulli trials.\n  Unlike directly calculating x/n, the binomial distribution takes into account sampling variability.",
          params$size, params$prob
        )
      } else if (type == "norm") {
        sprintf(
          "  Under normal distribution N(mu=%.2f, sigma=%.2f), the cumulative probability of X <= %s is %.6f.\n  This represents the theoretical proportion of the random variable falling within this interval.",
          params$mean, params$sd, paste(x_val, collapse = ", "), p_val
        )
      } else {
        "  This result is the value of the cumulative distribution function (CDF), representing the probability that the random variable does not exceed the given value."
      }
    }
  )
)
