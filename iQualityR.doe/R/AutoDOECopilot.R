# =============================================================================
# File: R/AutoDOECopilot.R
# Description: Intelligent DOE strategy recommender (rule-based expert system)
# Goal: Lower the barrier for users, avoid "over-design" or
#       "under-experimentation", and provide professional guidance.
# =============================================================================

#' @title AutoDOECopilot: Intelligent Experiment Strategy Recommendation
#' @description
#' Automatically recommends the optimal design of experiments (DOE) strategy
#' based on the number of factors, the goal, and the budget provided by the
#' user. Integrates industry best practices (Minitab/JMP logic) and
#' statistical power analysis.
#'
#' @details
#' The recommender supports both manufacturing and service industry scenarios.
#' For manufacturing it covers screening, optimization, and robustness goals.
#' For the service industry it enforces simulation/traffic-pool based
#' experimentation and requires online A/B validation of the optimal solution.
#'
#' @export
AutoDOECopilot <- R6::R6Class("AutoDOECopilot",
  public = list(

    #' @description
    #' Create a new AutoDOECopilot instance.
    #' @return An `AutoDOECopilot` object (invisibly).
    initialize = function() {
      invisible(self)
    },

    #' @description
    #' Get the recommended experiment strategies for the given inputs.
    #' @param n_factors Number of factors (integer, >= 1).
    #' @param goal Objective of the experiment:
    #'   `"screening"` (identify important factors),
    #'   `"optimization"` (find optimal settings), or
    #'   `"robustness"` (find conditions insensitive to noise).
    #' @param budget Budget / cost sensitivity: `"low"`, `"medium"`, or `"high"`.
    #' @param industry Industry type: `"manufacturing"` (default) or `"service"`.
    #' @param has_historical_data Whether relevant historical data is available
    #'   (`TRUE`/`FALSE`).
    #' @return A list with elements:
    #'   `input` (the provided arguments),
    #'   `strategies` (a list of recommended strategy descriptions), and
    #'   `next_step` (a character string describing the suggested next step).
    recommend = function(n_factors, goal = "optimization", budget = "medium",
                         industry = "manufacturing", has_historical_data = FALSE) {
      strategies <- list()

      # --- Service-industry-specific logic ---
      if (industry == "service") {
        strategies <- private$.recommend_service(n_factors, goal, budget, has_historical_data)
      } else {
        # --- Manufacturing logic (default) ---

        # Scenario A: Screening phase (many factors)
        if (goal == "screening" || n_factors > 5) {
          if (budget == "low") {
            strategies[[1]] <- .strategy_plackett_burman(n_factors)
          } else {
            strategies[[1]] <- .strategy_res_iv(n_factors)
          }
        }

        # Scenario B: Optimization phase (few factors)
        else if (goal == "optimization") {
          if (n_factors <= 2) {
            strategies[[1]] <- .strategy_full_factorial(n_factors)
          } else if (n_factors <= 4) {
            strategies[[1]] <- .strategy_ccd(n_factors)
          } else {
            strategies[[1]] <- .strategy_screening_first(n_factors)
            strategies[[2]] <- .strategy_ccd(n_factors)
          }
        }

        # Scenario C: Robustness / Taguchi
        else if (goal == "robustness") {
          strategies[[1]] <- .strategy_taguchi(n_factors)
        }
      }

      # --- Multi-Fidelity enhancement suggestion (applies to all industries) ---
      if (has_historical_data) {
        mf_note <- list(
          name = "Multi-Fidelity Boost",
          description = sprintf("Historical data detected! Recommend using 'MultiFidelityOptimizer' to calibrate the model; estimated %d%% reduction in experiment runs.",
                                ifelse(budget == "low", 30, ifelse(budget == "medium", 40, 50))),
          priority = "High"
        )
        strategies[[length(strategies) + 1]] <- mf_note
      }

      list(
        input = list(n_factors = n_factors, goal = goal, budget = budget, industry = industry),
        strategies = strategies,
        next_step = .get_next_step(strategies[[1]]$name)
      )
    }
  ),

  private = list(
    # =========================================================================
    # Service-industry-specific recommendation logic
    # =========================================================================
    .recommend_service = function(n_factors, goal, budget, has_hist) {
      strategies <- list()

      # Service-industry golden rule: responses must be statistics,
      # experiments run in simulation / traffic pools.
      service_note <- list(
        name = "Service Industry DOE Prerequisites",
        description = "Service industry DOE must be run in digital simulations or online traffic pools; never conduct physical experiments on real customers. Responses must use statistics from multiple replicates (mean + 95% CI), and multi-objective optimization (efficiency <-> cost <-> experience) is required.",
        priority = "Critical"
      )
      strategies[[1]] <- service_note

      if (goal == "screening" || n_factors > 4) {
        # Service screening: fractional factorial or Plackett-Burman in simulation
        strategies[[2]] <- list(
          name = sprintf("Service Industry Factor Screening: Resolution IV Fractional Factorial (n=%d simulations x 3 replicates)",
                         ifelse(n_factors <= 8, 16, 32)),
          design_type = "fractional_service",
          runs = ifelse(n_factors <= 8, 48, 96),
          pros = "Quickly identify key operational parameters in a simulation environment without disturbing real business",
          cons = "Requires calibrating the simulation model with historical logs first",
          applicable_scenario = "Call center routing / IVR strategy screening, cloud configuration parameter screening"
        )
      } else if (goal == "optimization") {
        # Service optimization: CCD / RSM searching the simulation space
        strategies[[2]] <- list(
          name = sprintf("Service Industry Response Optimization: CCD Design (n=%d simulations x 5 replicates)",
                         ifelse(n_factors <= 3, 40, 75)),
          design_type = "rsm_service",
          runs = ifelse(n_factors <= 3, 200, 375),
          pros = "Fit a second-order response surface for the service process and find the optimal combination of operational parameters",
          cons = "Simulation runtime is long; parallel computing is needed to speed it up",
          applicable_scenario = "Emergency department staffing optimization, microservice performance tuning, e-commerce conversion strategy optimization"
        )
      } else if (goal == "robustness") {
        # Service robustness: Taguchi outer array = arrival variability,
        # inner array = staffing / routing
        strategies[[2]] <- list(
          name = sprintf("Service Industry Robust Design: Taguchi Inner Array L%d x Outer Array (arrival variability)",
                         ifelse(n_factors <= 3, 4, 8)),
          design_type = "taguchi_service",
          runs = "inner array x outer array x 5 replicates",
          pros = "Find robust strategies that are insensitive to arrival-rate variability / traffic spikes",
          cons = "Experiment scale is large; recommend screening first to reduce the number of factors",
          applicable_scenario = "Call center SLA robustness, cloud server elastic scaling strategy"
        )
      }

      # Service-specific: A/B test validation recommendation
      strategies[[length(strategies) + 1]] <- list(
        name = "Online A/B Validation (Required Step)",
        description = "The optimal solution from the simulation DOE must undergo 1-2 weeks of small-traffic (5-10%) A/B validation in the real business environment. Confirm metric improvements before full rollout. Never launch directly at full scale!",
        priority = "Critical"
      )

      strategies
    }
  )
)

# =============================================================================
# Helper strategy functions (internal)
# =============================================================================

.strategy_full_factorial <- function(k) {
  n_runs <- 2^k
  list(
    name = sprintf("%d-Factor Full Factorial Design", k),
    design_type = "factorial",
    runs = n_runs,
    pros = "Estimates all main effects and interactions, no aliasing",
    cons = "Number of runs grows exponentially with the number of factors",
    applicable_scenario = "Very few factors (<=3) where an exact model is needed"
  )
}

.strategy_res_iv <- function(k) {
  # Estimate the number of runs required for Resolution IV (2^(k-p)).
  # Simplified logic: pick the smallest power of two that accommodates k
  # factors at Resolution IV. In practice, 16 runs can accommodate 5-8
  # factors at Resolution IV.
  if (k <= 4) n_runs <- 8
  else if (k <= 8) n_runs <- 16
  else if (k <= 16) n_runs <- 32
  else n_runs <- 64

  list(
    name = sprintf("Resolution IV Fractional Factorial Design (n=%d)", n_runs),
    design_type = "fractional",
    runs = n_runs,
    pros = "Main effects are not aliased with two-factor interactions; high efficiency",
    cons = "Two-factor interactions are aliased with each other",
    applicable_scenario = "Screening key factors, 5-15 factors"
  )
}

.strategy_ccd <- function(k) {
  # 2^k (factorial) + 2k (axial) + 6 (center)
  n_factorial <- 2^k
  # For larger designs, a fractional factorial may be used for the cube part
  if (k > 5) n_factorial <- n_factorial / 2

  n_runs <- n_factorial + 2 * k + 6
  list(
    name = sprintf("Central Composite Design (CCD, n=%d)", n_runs),
    design_type = "rsm",
    runs = n_runs,
    pros = "Fits a second-order response surface to find optimal parameters",
    cons = "Relatively many points; about 50 runs for five factors",
    applicable_scenario = "Response surface optimization, 2-5 factors"
  )
}

.strategy_screening_first <- function(k) {
  list(
    name = "Two-Stage Strategy: Screening -> Optimization",
    design_type = "sequential",
    runs = "N/A",
    pros = "Avoids running expensive experiments on unimportant factors",
    cons = "Longer cycle; requires two rounds",
    applicable_scenario = "More than 5 factors with a limited budget"
  )
}

.strategy_plackett_burman <- function(k) {
  # Plackett-Burman design sizes: 12, 20, 24, 28, 36, 40, 44, 48, ...
  # Use the smallest multiple of 4 that exceeds k, with a minimum of 12.
  n_runs <- max(12, ceiling((k + 1) / 4) * 4)
  list(
    name = sprintf("Plackett-Burman Design (n=%d)", n_runs),
    design_type = "screening",
    runs = n_runs,
    pros = "Extremely sample-efficient; estimates only main effects",
    cons = "Resolution III; main effects are aliased with interactions",
    applicable_scenario = "Severe budget / time constraints; pure main-effect screening"
  )
}

.strategy_taguchi <- function(k) {
  # Simplified estimate
  if (k <= 3) n_inner <- 4
  else if (k <= 7) n_inner <- 8
  else n_inner <- 16

  list(
    name = sprintf("Taguchi Robust Design (Inner Array L%d)", n_inner),
    design_type = "taguchi",
    runs = sprintf("%d x (outer array)", n_inner),
    pros = "Focuses on signal-to-noise ratio; finds conditions insensitive to the environment",
    cons = "Data analysis is complex; an outer array must be defined",
    applicable_scenario = "Product quality consistency optimization"
  )
}

.get_next_step <- function(strategy_name) {
  if (grepl("fractional", strategy_name, ignore.case = TRUE)) return("Run experiment -> ANOVA analysis -> Identify significant factors")
  if (grepl("CCD", strategy_name)) return("Run experiment -> Fit second-order model -> Response optimization")
  if (grepl("Multi-Fidelity", strategy_name)) return("Use MultiFidelityOptimizer to import historical data")
  return("See Vignettes for detailed steps")
}
