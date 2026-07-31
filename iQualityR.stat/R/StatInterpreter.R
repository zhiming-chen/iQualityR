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
      } else if (inherits(x, "aov") || inherits(x, "anova")) {
        private$.interpret_anova(x, audience)
      } else if (inherits(x, "ProbNode") || (is.list(x) && "type" %in% names(x))) {
        private$.interpret_dist(x, audience)
      } else if (is.list(x) && "test_type" %in% names(x) && grepl("Normality", x$test_type %||% "")) {
        private$.interpret_normality(x, audience, list(...))
      } else {
        "Unable to recognize statistical result type. Please provide htest, aov, probability distribution, or normality test result."
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
