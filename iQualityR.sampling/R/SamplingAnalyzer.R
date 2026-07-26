# =============================================================================
# File: R/SamplingAnalyzer.R
# Description: Sampling plan analysis engine (OC curve / power / risk / ASN)
# =============================================================================

#' @title SamplingAnalyzer: Acceptance Sampling Analysis Engine
#'
#' @description
#' Inherits from [IqrAnalyzerBase] and performs the core computations for an
#' acceptance sampling plan:
#'
#' - Operating Characteristic (OC) curve via binomial distribution
#' - Producer's and consumer's risk
#' - Power analysis (single sampling)
#' - Average Sample Number (ASN) curve (double / multiple sampling)
#' - Optional actual sampling on a real data frame
#'
#' @export
SamplingAnalyzer <- R6::R6Class("SamplingAnalyzer",
  inherit = IqrAnalyzerBase,

  public = list(

    #' @description Run the full sampling analysis.
    #'
    #' @param data Optional data frame containing a `quality_status` column
    #'   for actual sampling validation. If NULL, only theoretical analysis
    #'   is performed.
    #' @param plan A [SamplingPlan] object.
    #' @return A list with components: `oc_curve`, `risk_analysis`,
    #'   `power_analysis`, `asn_curve` (if applicable), `actual_sampling`
    #'   (if data provided), and `meta`.
    analyze = function(data = NULL, plan) {
      if (is.null(plan) || !inherits(plan, "SamplingPlan")) {
        stop("[SamplingAnalyzer] plan must be a SamplingPlan object.",
             call. = FALSE)
      }

      results <- list()
      results$oc_curve <- private$.calculate_oc_curve(plan)
      results$risk_analysis <- private$.calculate_risk(plan)
      results$power_analysis <- private$.calculate_power(plan)

      if (plan$sampling_type %in% c("double", "multiple")) {
        results$asn_curve <- private$.calculate_asn_curve(plan)
      }

      if (!is.null(data) && "quality_status" %in% names(data)) {
        results$actual_sampling <- private$.perform_actual_sampling(data, plan)
      }

      results$meta <- list(
        sampling_type = plan$sampling_type,
        sample_size = plan$sample_size,
        acceptance_number = plan$acceptance_number,
        aql = plan$aql,
        rql = plan$rql,
        alpha = plan$alpha,
        beta = plan$beta
      )

      results
    }
  ),

  private = list(

    # -------------------------------------------------------------------------
    # OC curve
    # -------------------------------------------------------------------------

    .calculate_oc_curve = function(plan) {
      p_max <- max(plan$rql * 2, 0.30)
      p_values <- seq(0, p_max, length.out = 200)

      prob_accept <- switch(plan$sampling_type,
        "single"   = private$.oc_single(p_values, plan),
        "double"   = private$.oc_double(p_values, plan$stage_plans),
        "multiple" = private$.oc_multiple(p_values, plan$stage_plans),
        "sequential" = private$.oc_single(p_values, plan),
        rep(NA_real_, length(p_values))
      )

      aql_prob <- private$.interp_prob(p_values, prob_accept, plan$aql)
      rql_prob <- private$.interp_prob(p_values, prob_accept, plan$rql)

      list(
        p_values = p_values,
        acceptance_probabilities = prob_accept,
        key_points = list(
          aql_point = list(p = plan$aql, prob = aql_prob),
          rql_point = list(p = plan$rql, prob = rql_prob)
        )
      )
    },

    .oc_single = function(p_values, plan) {
      # Vectorized binomial CDF: P(X <= c) for each p
      stats::pbinom(plan$acceptance_number,
                    size = plan$sample_size,
                    prob = p_values)
    },

    .oc_double = function(p_values, stages) {
      stage1 <- stages[[1]]
      stage2 <- stages[[2]]
      n1 <- stage1$n; c1 <- stage1$c
      r1 <- stage1$r %||% (c1 + 1)
      n2 <- stage2$n; c2 <- stage2$c

      # P(accept at stage 1) = P(X1 <= c1)
      p_accept_1 <- stats::pbinom(c1, n1, p_values)

      # P(continue) = P(c1 < X1 < r1) = P(X1 <= r1-1) - P(X1 <= c1)
      p_continue <- stats::pbinom(r1 - 1, n1, p_values) -
                    stats::pbinom(c1, n1, p_values)

      # Simplified: stage 2 acceptance on cumulative count d1 + d2 <= c2,
      # with d2 ~ Binomial(n2, p). Use marginal: P(D1+D2 <= c2)
      p_accept_2 <- p_continue * stats::pbinom(c2 - c1, n2, p_values)

      p_accept_1 + p_accept_2
    },

    .oc_multiple = function(p_values, stages) {
      # NOTE: simplified approximation. For each p, accumulate acceptance
      # probability across stages assuming independence.
      p_accept <- stats::pbinom(stages[[1]]$c, stages[[1]]$n, p_values)
      for (i in seq_along(stages)[-1]) {
        stage <- stages[[i]]
        p_stage <- stats::pbinom(stage$c, stage$n, p_values)
        p_accept <- p_accept + (1 - p_accept) * p_stage
      }
      p_accept
    },

    .interp_prob = function(p_values, prob_accept, target_p) {
      if (length(prob_accept) == 0 || all(is.na(prob_accept))) return(NA_real_)
      tryCatch(
        as.numeric(approx(p_values, prob_accept, xout = target_p)$y),
        error = function(e) NA_real_
      )
    },

    # -------------------------------------------------------------------------
    # Risk
    # -------------------------------------------------------------------------

    .calculate_risk = function(plan) {
      if (plan$sampling_type == "single") {
        n <- plan$sample_size
        c <- plan$acceptance_number
        prob_at_aql <- stats::pbinom(c, n, plan$aql)
        prob_at_rql <- stats::pbinom(c, n, plan$rql)
      } else {
        prob_at_aql <- private$.oc_double(plan$aql, plan$stage_plans)
        prob_at_rql <- private$.oc_double(plan$rql, plan$stage_plans)
      }
      producer_risk <- 1 - prob_at_aql
      consumer_risk <- prob_at_rql

      list(
        producer_risk = producer_risk,
        consumer_risk = consumer_risk,
        risk_profile = list(
          aql_alpha = producer_risk,
          rql_beta = consumer_risk,
          target_alpha = plan$alpha,
          target_beta = plan$beta
        )
      )
    },

    # -------------------------------------------------------------------------
    # Power analysis (single sampling only)
    # -------------------------------------------------------------------------

    .calculate_power = function(plan) {
      if (plan$sampling_type != "single") return(NULL)

      n <- plan$sample_size
      c <- plan$acceptance_number

      p_values <- seq(0.01, 0.50, by = 0.01)
      powers <- 1 - stats::pbinom(c, n, p_values)

      target_power <- 0.80
      required_n <- private$.find_required_n(c, plan$rql, target_power)
      achieved_power <- 1 - stats::pbinom(c, n, plan$rql)

      list(
        p_values = p_values,
        powers = powers,
        required_sample_size = required_n,
        achieved_power = achieved_power,
        target_power = target_power
      )
    },

    .find_required_n = function(c, rql, target_power) {
      required_n <- NA_integer_
      for (test_n in seq(10, 500, by = 5)) {
        power_at_rql <- 1 - stats::pbinom(c, test_n, rql)
        if (power_at_rql >= target_power) {
          required_n <- test_n
          break
        }
      }
      if (is.na(required_n)) NULL else required_n
    },

    # -------------------------------------------------------------------------
    # ASN curve (double / multiple)
    # -------------------------------------------------------------------------

    .calculate_asn_curve = function(plan) {
      p_values <- seq(0, 0.30, length.out = 100)
      asn_values <- if (plan$sampling_type == "double") {
        private$.asn_double(p_values, plan$stage_plans)
      } else {
        private$.asn_multiple(p_values, plan$stage_plans)
      }
      list(
        p_values = p_values,
        asn_values = asn_values,
        single_sample_n = plan$sample_size
      )
    },

    .asn_double = function(p_values, stages) {
      stage1 <- stages[[1]]
      stage2 <- stages[[2]]
      n1 <- stage1$n; c1 <- stage1$c
      r1 <- stage1$r %||% (c1 + 1)
      n2 <- stage2$n

      p_continue <- stats::pbinom(r1 - 1, n1, p_values) -
                    stats::pbinom(c1, n1, p_values)
      n1 + n2 * p_continue
    },

    .asn_multiple = function(p_values, stages) {
      # NOTE: simplified. Uses reach probability 0.5 as approximation.
      asn <- rep(stages[[1]]$n, length(p_values))
      for (i in 2:length(stages)) {
        # Reach probability is approximated; replace with proper calc later.
        p_reach <- 0.5
        asn <- asn + stages[[i]]$n * p_reach
      }
      asn
    },

    # -------------------------------------------------------------------------
    # Actual sampling on real data
    # -------------------------------------------------------------------------

    .perform_actual_sampling = function(data, plan) {
      quality_col <- data$quality_status

      # Normalize to 0/1 (1 = defective)
      if (is.character(quality_col) || is.factor(quality_col)) {
        defects <- as.integer(quality_col %in% c("defective", "fail", 1))
      } else {
        defects <- as.integer(quality_col)
      }

      if (length(defects) >= plan$sample_size) {
        # RNG isolation: local_seed restores RNG state on function exit.
        withr::local_seed(42)
        sample_indices <- sample(seq_along(defects), plan$sample_size)
        sample_defects <- defects[sample_indices]
        n_defective <- sum(sample_defects)
        accepted <- n_defective <= plan$acceptance_number

        list(
          n_sampled = plan$sample_size,
          n_defective = n_defective,
          acceptance_number = plan$acceptance_number,
          accepted = accepted,
          defect_rate = n_defective / plan$sample_size
        )
      } else {
        list(
          warning = "Insufficient data for actual sampling validation.",
          n_available = length(defects),
          n_required = plan$sample_size
        )
      }
    }
  )
)
