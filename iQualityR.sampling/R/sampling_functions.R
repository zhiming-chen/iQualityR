# =============================================================================
# File: R/sampling_functions.R
# Description: Convenience entry-point functions (single / double / power)
# =============================================================================

#' Single Sampling Plan Analysis
#'
#' Build a single-sampling acceptance plan and run OC curve, risk, and power
#' analysis in one call.
#'
#' @param sample_size Integer. Sample size n.
#' @param acceptance_number Integer. Acceptance number c.
#' @param aql Numeric. Acceptable Quality Level (default 0.01).
#' @param rql Numeric. Rejectable Quality Level (default 0.10).
#' @param alpha Numeric. Producer's risk target (default 0.05).
#' @param beta Numeric. Consumer's risk target (default 0.10).
#' @param data Optional data frame with a `quality_status` column for
#'   actual sampling validation.
#' @param theme Theme name (default `"academic"`).
#' @return An [IqrSamplingTask] object with results already computed.
#'
#' @examples
#' \donttest{
#' task <- sampling_single(sample_size = 50, acceptance_number = 1)
#' task$summary()
#' }
#' @export
sampling_single <- function(sample_size,
                            acceptance_number,
                            aql = 0.01,
                            rql = 0.10,
                            alpha = 0.05,
                            beta = 0.10,
                            data = NULL,
                            theme = "academic") {
  plan <- SamplingPlan$new(
    task_tag = "sampling_single",
    sample_size = sample_size,
    acceptance_number = acceptance_number,
    sampling_type = "single",
    aql = aql,
    rql = rql,
    alpha = alpha,
    beta = beta
  )
  task <- IqrSamplingTask$new(data = data, plan = plan, theme = theme)
  task$compute()
  task
}

#' Double Sampling Plan Analysis
#'
#' Build a two-stage sampling plan and run OC curve, risk, and ASN analysis.
#'
#' @param stage1 List with `n` (sample size) and `c` (acceptance number),
#'   and optionally `r` (rejection number). If `r` is missing, it defaults
#'   to `c + 1`.
#' @param stage2 List with `n` and `c`.
#' @param aql Numeric. AQL (default 0.01).
#' @param rql Numeric. RQL (default 0.10).
#' @param alpha Numeric. Producer's risk target (default 0.05).
#' @param beta Numeric. Consumer's risk target (default 0.10).
#' @param data Optional data frame for actual sampling validation.
#' @param theme Theme name (default `"academic"`).
#' @return An [IqrSamplingTask] object with results already computed.
#'
#' @examples
#' \donttest{
#' task <- sampling_double(
#'   stage1 = list(n = 32, c = 0, r = 2),
#'   stage2 = list(n = 32, c = 1)
#' )
#' task$summary()
#' }
#' @export
sampling_double <- function(stage1,
                            stage2,
                            aql = 0.01,
                            rql = 0.10,
                            alpha = 0.05,
                            beta = 0.10,
                            data = NULL,
                            theme = "academic") {
  plan <- SamplingPlan$new(
    task_tag = "sampling_double",
    sampling_type = "double",
    aql = aql,
    rql = rql,
    alpha = alpha,
    beta = beta,
    stage_plans = list(stage1, stage2),
    sample_size = stage1$n,
    acceptance_number = stage2$c
  )
  task <- IqrSamplingTask$new(data = data, plan = plan, theme = theme)
  task$compute()
  task
}

#' Multiple Sampling Plan Analysis
#'
#' Build a multi-stage plan and run OC curve, risk, and ASN analysis.
#'
#' @param stages List of stage plans; each element must have `n` and `c`.
#' @param aql Numeric. AQL (default 0.01).
#' @param rql Numeric. RQL (default 0.10).
#' @param alpha Numeric. Producer's risk target (default 0.05).
#' @param beta Numeric. Consumer's risk target (default 0.10).
#' @param data Optional data frame for actual sampling validation.
#' @param theme Theme name (default `"academic"`).
#' @return An [IqrSamplingTask] object with results already computed.
#'
#' @examples
#' \donttest{
#' task <- sampling_multiple(stages = list(
#'   list(n = 20, c = 0),
#'   list(n = 20, c = 1),
#'   list(n = 20, c = 2)
#' ))
#' task$summary()
#' }
#' @export
sampling_multiple <- function(stages,
                              aql = 0.01,
                              rql = 0.10,
                              alpha = 0.05,
                              beta = 0.10,
                              data = NULL,
                              theme = "academic") {
  plan <- SamplingPlan$new(
    task_tag = "sampling_multiple",
    sampling_type = "multiple",
    aql = aql,
    rql = rql,
    alpha = alpha,
    beta = beta,
    stage_plans = stages,
    sample_size = stages[[1]]$n,
    acceptance_number = stages[[length(stages)]]$c
  )
  task <- IqrSamplingTask$new(data = data, plan = plan, theme = theme)
  task$compute()
  task
}
