# =============================================================================
# File: iQualityR.msa/R/DetectionAnalyzer.R
# Description: Detection rate/False negative rate analyzer (Binomial distribution)
# Independent module, does not depend on KappaAnalyzer
# =============================================================================

#' @title DetectionAnalyzer: Detection Rate and False Negative Rate Analysis
#' @description Statistical analysis of detection rate and false negative rate using binomial distribution
#'
#' @field detection_rate Proportion of positive reference samples correctly detected.
#' @field false_negative_rate Proportion of positive reference samples missed.
#' @field binomial_result Clopper-Pearson confidence interval result for the detection rate.
#'
#' @param data Data frame with reference and test columns.
#' @param plan Optional plan object or list of parameters.
#' @param x Number of successes (e.g. true positives).
#' @param n Number of trials (e.g. total positive reference samples).
#' @param conf_level Confidence level (default 0.95).
#' @param null_rate Null hypothesis rate for the binomial test.
#' @param alternative Alternative hypothesis: `"greater"`, `"less"`, or `"two.sided"`.
#'
#' @export
DetectionAnalyzer <- R6::R6Class("DetectionAnalyzer",
  inherit = IqrAnalyzerBase,
  public = list(
    detection_rate = NULL,
    false_negative_rate = NULL,
    binomial_result = NULL,

    initialize = function() {
      super$initialize()
    },

    run = function(data, plan = NULL) {
      self$reset()
      if (!is.null(plan)) {
        self$setup(plan)
        if (is.list(plan) && !is.null(plan$meta_data$data)) {
          data_params <- plan$meta_data$data
          self$params <- modifyList(self$params, data_params)
        }
      }

      result <- self$analyze_detection(data)
      self$set_raw_output(result)
      return(invisible(self))
    },

    analyze_detection = function(data) {
      ref_col <- self$params$reference_col %||% "Reference"
      test_col <- self$params$test_col %||% "Test"

      if (!ref_col %in% colnames(data) || !test_col %in% colnames(data)) {
        stop("Data must contain columns: ", ref_col, ", ", test_col)
      }

      d <- data.frame(
        reference = data[[ref_col]],
        test = data[[test_col]]
      )
      d <- d[complete.cases(d), ]

      categories <- unique(c(d$reference, d$test))
      positive_category <- self$params$positive_category %||% categories[1]
      negative_category <- self$params$negative_category %||% categories[2]

      TP <- sum(d$reference == positive_category & d$test == positive_category)
      FN <- sum(d$reference == positive_category & d$test == negative_category)
      FP <- sum(d$reference == negative_category & d$test == positive_category)
      TN <- sum(d$reference == negative_category & d$test == negative_category)

      n_positive <- TP + FN
      n_negative <- FP + TN
      n_total <- n_positive + n_negative

      detection_rate <- if (n_positive > 0) TP / n_positive else NA
      false_negative_rate <- if (n_positive > 0) FN / n_positive else NA
      specificity <- if (n_negative > 0) TN / n_negative else NA
      false_positive_rate <- if (n_negative > 0) FP / n_negative else NA

      binom_detection <- self$binomial_ci(TP, n_positive, conf_level = self$params$conf_level %||% 0.95)
      binom_specificity <- self$binomial_ci(TN, n_negative, conf_level = self$params$conf_level %||% 0.95)

      youden_index <- detection_rate + specificity - 1

      LR_positive <- if (specificity < 1) detection_rate / (1 - specificity) else Inf
      LR_negative <- if (detection_rate > 0) (1 - detection_rate) / specificity else Inf

      prevalence <- n_positive / n_total
      confusion_matrix_table <- data.frame(
        Reference = c(
          paste0("Reference: ", positive_category),
          paste0("Reference: ", negative_category)
        ),
        Test_Positive = c(TP, FP),
        Test_Negative = c(FN, TN),
        Total = c(n_positive, n_negative),
        check.names = FALSE
      )
      risk_table <- data.frame(
        Group = c("Positive reference", "Negative reference", "All samples"),
        N = c(n_positive, n_negative, n_total),
        Correct = c(TP, TN, TP + TN),
        Incorrect = c(FN, FP, FN + FP),
        Percent_Correct = c(
          ifelse(n_positive > 0, 100 * TP / n_positive, NA_real_),
          ifelse(n_negative > 0, 100 * TN / n_negative, NA_real_),
          ifelse(n_total > 0, 100 * (TP + TN) / n_total, NA_real_)
        )
      )

      result <- list(
        method = "Binomial Detection Analysis",
        detection_rate = detection_rate,
        false_negative_rate = false_negative_rate,
        specificity = specificity,
        false_positive_rate = false_positive_rate,
        youden_index = youden_index,
        prevalence = prevalence,
        LR_positive = LR_positive,
        LR_negative = LR_negative,
        confusion_matrix = list(
          TP = TP, FN = FN,
          FP = FP, TN = TN
        ),
        confusion_matrix_table = confusion_matrix_table,
        risk_table = risk_table,
        n_positive = n_positive,
        n_negative = n_negative,
        n_total = n_total,
        detection_ci = binom_detection,
        specificity_ci = binom_specificity,
        positive_category = positive_category,
        negative_category = negative_category
      )

      self$detection_rate <- detection_rate
      self$false_negative_rate <- false_negative_rate
      self$binomial_result <- binom_detection
      self$params$n_positive <- n_positive
      self$params$n_negative <- n_negative

      self$set_statistic("detection_rate", detection_rate)
      self$set_statistic("false_negative_rate", false_negative_rate)
      self$set_statistic("specificity", specificity)
      self$set_statistic("youden_index", youden_index)

      return(result)
    },

    binomial_ci = function(x, n, conf_level = 0.95) {
      if (n == 0) return(list(point = NA, lower = NA, upper = NA))

      p_hat <- x / n
      alpha <- 1 - conf_level

      ci <- binom.test(x, n, conf.level = conf_level)$conf.int

      list(
        point = p_hat,
        lower = ci[1],
        upper = ci[2],
        method = "Clopper-Pearson"
      )
    },

    detection_test = function(null_rate = 0.80, alternative = "greater") {
      if (is.null(self$binomial_result)) {
        stop("Please run run() method first")
      }

      x <- self$binomial_result$point * self$params$n_positive
      n <- self$params$n_positive
      if (is.null(n) || n == 0) {
        stop("No positive samples available for detection test", call. = FALSE)
      }

      binom_test <- binom.test(x, n, p = null_rate,
                               alternative = alternative,
                               conf.level = self$params$conf_level %||% 0.95)

      list(
        null_hypothesis = null_rate,
        alternative = alternative,
        p_value = binom_test$p.value,
        point_estimate = x / n,
        test_method = "Binomial Test (Clopper-Pearson)"
      )
    },

    summary_table = function() {
      r <- self$results$raw_output
      if (is.null(r)) {
        stop("Please run run() method first")
      }

      data.frame(
        Metric = c(
          "Detection Rate",
          "False Negative Rate",
          "Specificity",
          "False Positive Rate",
          "Youden Index",
          "Likelihood Ratio Positive (LR+)",
          "Likelihood Ratio Negative (LR-)",
          "Prevalence"
        ),
        Estimate = c(
          sprintf("%.2f%%", r$detection_rate * 100),
          sprintf("%.2f%%", r$false_negative_rate * 100),
          sprintf("%.2f%%", r$specificity * 100),
          sprintf("%.2f%%", r$false_positive_rate * 100),
          sprintf("%.4f", r$youden_index),
          sprintf("%.4f", r$LR_positive),
          sprintf("%.4f", r$LR_negative),
          sprintf("%.2f%%", r$prevalence * 100)
        ),
        CI = c(
          sprintf("(%.2f%%, %.2f%%)", r$detection_ci$lower * 100, r$detection_ci$upper * 100),
          "-",
          sprintf("(%.2f%%, %.2f%%)", r$specificity_ci$lower * 100, r$specificity_ci$upper * 100),
          "-",
          "-",
          "-",
          "-",
          "-"
        )
      )
    }
  )
)
