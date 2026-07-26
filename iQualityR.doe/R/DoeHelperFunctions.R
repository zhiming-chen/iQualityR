# =============================================================================
# File: R/DoeHelperFunctions.R
# Description: User-facing entry functions for the DOE subpackage
# =============================================================================

# ---------------------------------------------------------------------------
# Internal helper: validate a factor specification list
# ---------------------------------------------------------------------------
.validate_factor_list <- function(factors, arg_name = "factors") {
  if (is.null(factors) || !is.list(factors) || length(factors) == 0) {
    stop(sprintf("[%s] '%s' must be a non-empty list of factor specifications.",
                 "DOE", arg_name))
  }
  for (i in seq_along(factors)) {
    f <- factors[[i]]
    if (!is.list(f)) {
      stop(sprintf("[DOE] Factor %d must be a list with name/type/levels.", i))
    }
    if (is.null(f$name) || !is.character(f$name) || length(f$name) != 1) {
      stop(sprintf("[DOE] Factor %d must have a single 'name' string.", i))
    }
    if (is.null(f$levels) || length(f$levels) < 2) {
      stop(sprintf("[DOE] Factor '%s' must have at least 2 levels.", f$name))
    }
  }
  factor_names <- vapply(factors, function(f) f$name, character(1))
  if (anyDuplicated(factor_names)) {
    stop("[DOE] Factor names must be unique; duplicates were found.")
  }
  invisible(TRUE)
}


#' @title Orthogonal Design
#' @description Creates and runs an orthogonal-array experimental design,
#'   automatically generating the design table.
#'
#' @param factors List of factor specifications; each element must contain
#'   `name`, `type` and `levels`.
#' @param response_vars Character vector of response variable names.
#' @param replication Integer scalar, number of replications (default 1).
#' @param randomize Logical scalar, whether to randomize run order (default
#'   `TRUE`).
#' @param seed Optional integer scalar, the random seed.
#' @param theme Character scalar, the theme name.
#' @param auto_compute Logical scalar, whether to compute the design
#'   immediately (default `TRUE`).
#'
#' @return An `IqrDoeTask` object.
#'
#' @examples
#' \dontrun{
#' # 3-factor 2-level orthogonal design
#' factors <- list(
#'   list(name = "Temperature", type = "continuous", levels = c(100, 150)),
#'   list(name = "Pressure", type = "continuous", levels = c(5, 10)),
#'   list(name = "Time", type = "continuous", levels = c(30, 60))
#' )
#'
#' task <- orthogonal_design(factors = factors)
#' task$summary()
#' task$plot()
#' }
#'
#' @export
orthogonal_design <- function(factors,
                              response_vars = NULL,
                              replication = 1,
                              randomize = TRUE,
                              seed = NULL,
                              theme = "academic",
                              auto_compute = TRUE) {
  .validate_factor_list(factors)

  plan <- IqrDoePlan$new(
    task_tag = "orthogonal",
    design_type = "orthogonal",
    factors = factors,
    response_vars = response_vars,
    replication = replication,
    randomize = randomize,
    seed = seed
  )

  task <- IqrDoeTask$new(plan = plan, theme = theme)

  if (auto_compute) {
    task$compute()
  }

  task
}


#' @title Factorial Design
#' @description Creates and runs a full or fractional factorial experimental
#'   design.
#'
#' @param factors List of factor specifications.
#' @param design_type Character scalar, `"factorial"` (full factorial) or
#'   `"fractional"` (fractional factorial).
#' @param response_vars Character vector of response variable names.
#' @param replication Integer scalar, number of replications.
#' @param center_points Integer scalar, number of center points.
#' @param resolution Character scalar, the resolution (`"III"`, `"IV"` or
#'   `"V"`), used only for fractional designs.
#' @param randomize Logical scalar, whether to randomize.
#' @param seed Optional integer scalar, the random seed.
#' @param theme Character scalar, the theme name.
#' @param auto_compute Logical scalar, whether to compute immediately.
#'
#' @return An `IqrDoeTask` object.
#'
#' @examples
#' \dontrun{
#' # Full factorial design
#' factors <- list(
#'   list(name = "A", type = "continuous", levels = c(-1, 1)),
#'   list(name = "B", type = "continuous", levels = c(-1, 1))
#' )
#'
#' task <- factorial_design(factors = factors, design_type = "factorial")
#' }
#'
#' @export
factorial_design <- function(factors,
                             design_type = "factorial",
                             response_vars = NULL,
                             replication = 1,
                             center_points = 0,
                             resolution = NULL,
                             randomize = TRUE,
                             seed = NULL,
                             theme = "academic",
                             auto_compute = TRUE) {
  .validate_factor_list(factors)
  design_type <- match.arg(design_type, c("factorial", "fractional"))

  plan <- IqrDoePlan$new(
    task_tag = design_type,
    design_type = design_type,
    factors = factors,
    response_vars = response_vars,
    replication = replication,
    center_points = center_points,
    resolution = resolution,
    randomize = randomize,
    seed = seed
  )

  task <- IqrDoeTask$new(plan = plan, theme = theme)

  if (auto_compute) {
    task$compute()
  }

  task
}


#' @title Response Surface Design
#' @description Creates and runs a response-surface-methodology (RSM)
#'   experimental design, supporting central composite design (CCD) and
#'   Box-Behnken design.
#'
#' @param factors List of factor specifications.
#' @param design_type Character scalar, RSM design type: `"ccd"` (central
#'   composite) or `"box_behnken"`.
#' @param response_vars Character vector of response variable names.
#' @param center_points Integer scalar, number of center points (default 6).
#' @param alpha Numeric scalar or `NULL`, the axial distance for CCD. When
#'   `NULL` the value is chosen automatically.
#' @param randomize Logical scalar, whether to randomize.
#' @param seed Optional integer scalar, the random seed.
#' @param theme Character scalar, the theme name.
#' @param auto_compute Logical scalar, whether to compute immediately.
#'
#' @return An `IqrDoeTask` object.
#'
#' @examples
#' \dontrun{
#' # Central composite design
#' factors <- list(
#'   list(name = "Temperature", type = "continuous", levels = c(100, 200)),
#'   list(name = "Pressure", type = "continuous", levels = c(5, 15))
#' )
#'
#' task <- rsm_design(factors = factors, design_type = "ccd")
#' }
#'
#' @export
rsm_design <- function(factors,
                       design_type = "ccd",
                       response_vars = NULL,
                       center_points = 6,
                       alpha = NULL,
                       randomize = TRUE,
                       seed = NULL,
                       theme = "academic",
                       auto_compute = TRUE) {
  .validate_factor_list(factors)
  design_type <- match.arg(design_type, c("ccd", "box_behnken"))

  plan <- IqrDoePlan$new(
    task_tag = "rsm",
    design_type = design_type,
    factors = factors,
    response_vars = response_vars,
    center_points = center_points,
    alpha = alpha,
    randomize = randomize,
    seed = seed
  )

  task <- IqrDoeTask$new(plan = plan, theme = theme)

  if (auto_compute) {
    task$compute()
  }

  task
}


#' @title Screening Design
#' @description Creates a screening design (high-resolution fractional
#'   factorial) to quickly identify the key factors among many candidates.
#'
#' @param factors List of factor specifications (typically 5-15 factors).
#' @param resolution Character scalar, the resolution (`"III"` or `"IV"`,
#'   default `"IV"`).
#' @param response_vars Character vector of response variable names.
#' @param replication Integer scalar, number of replications.
#' @param randomize Logical scalar, whether to randomize.
#' @param seed Optional integer scalar, the random seed.
#' @param theme Character scalar, the theme name.
#' @param auto_compute Logical scalar, whether to compute immediately.
#'
#' @return An `IqrDoeTask` object.
#'
#' @examples
#' \dontrun{
#' # Screening design (8 factors)
#' factors <- lapply(1:8, function(i) {
#'   list(name = paste0("Factor_", i), type = "continuous", levels = c(-1, 1))
#' })
#'
#' task <- screening_design(factors = factors, resolution = "IV")
#' }
#'
#' @export
screening_design <- function(factors,
                             resolution = "IV",
                             response_vars = NULL,
                             replication = 1,
                             randomize = TRUE,
                             seed = NULL,
                             theme = "academic",
                             auto_compute = TRUE) {
  .validate_factor_list(factors)

  plan <- IqrDoePlan$new(
    task_tag = "screening",
    design_type = "fractional",
    factors = factors,
    response_vars = response_vars,
    replication = replication,
    resolution = resolution,
    randomize = randomize,
    seed = seed
  )

  task <- IqrDoeTask$new(plan = plan, theme = theme)

  if (auto_compute) {
    task$compute()
  }

  task
}


#' @title Taguchi Robust Design
#' @description Creates and runs a Taguchi robust design. When response data
#'   is supplied via the `data` argument, a Taguchi S/N ratio analysis and
#'   ANOVA-based contribution analysis are also produced.
#'
#' @param control_factors List of control factor specifications.
#' @param noise_factors Optional list of noise factor specifications. When
#'   `data` is supplied and contains columns matching the noise factor names,
#'   the S/N ratio is computed per control-factor combination using the
#'   noise-factor rows as replicates (inner/outer-array style). When `data`
#'   is `NULL`, `noise_factors` is recorded on the returned object for
#'   downstream use but does not generate an outer array.
#' @param response_vars Character vector of response variable names.
#' @param data Optional data frame containing the observed response values.
#'   Must contain one row per run of the (replicated) design table, with
#'   columns for every control factor and response variable declared on
#'   `response_vars`. When supplied, ANOVA, S/N ratio, and contribution
#'   analyses are produced and returned in `taguchi_analysis`.
#' @param sn_type Character scalar, the S/N ratio type: `"larger"`,
#'   `"nominal"` or `"smaller"`.
#' @param replication Integer scalar, number of replications (default 2, used
#'   to estimate variation).
#' @param randomize Logical scalar, whether to randomize.
#' @param seed Optional integer scalar, the random seed.
#' @param theme Character scalar, the theme name.
#' @param auto_compute Logical scalar, whether to compute immediately.
#'
#' @return A list with elements `task` (the `IqrDoeTask`), `design` (the
#'   design table) and `taguchi_analysis` (the S/N ratio analysis; `NULL`
#'   when `data` is not supplied).
#'
#' @examples
#' \dontrun{
#' # Taguchi design (3 control factors)
#' control_factors <- list(
#'   list(name = "Temp", type = "continuous", levels = c(100, 150)),
#'   list(name = "Pressure", type = "continuous", levels = c(5, 10)),
#'   list(name = "Time", type = "continuous", levels = c(30, 60))
#' )
#'
#' result <- taguchi_design(
#'   control_factors = control_factors,
#'   sn_type = "larger"
#' )
#'
#' # View the S/N ratio analysis
#' print(result$taguchi_analysis)
#' }
#'
#' @export
taguchi_design <- function(control_factors,
                           noise_factors = NULL,
                           response_vars = NULL,
                           data = NULL,
                           sn_type = "nominal",
                           replication = 2,
                           randomize = TRUE,
                           seed = NULL,
                           theme = "academic",
                           auto_compute = TRUE) {
  .validate_factor_list(control_factors, arg_name = "control_factors")
  if (!is.null(noise_factors)) {
    .validate_factor_list(noise_factors, arg_name = "noise_factors")
  }
  sn_type <- match.arg(sn_type, c("larger", "nominal", "smaller"))

  # Build the inner array (control factors) design.
  plan <- IqrDoePlan$new(
    task_tag = "taguchi",
    design_type = "orthogonal",
    factors = control_factors,
    response_vars = response_vars,
    replication = replication,
    randomize = randomize,
    seed = seed
  )

  task <- IqrDoeTask$new(plan = plan, data = data, theme = theme)

  if (auto_compute) {
    task$compute()
  }

  # Taguchi S/N ratio analysis (when response data is available).
  taguchi_analysis <- NULL
  if (!is.null(data) && !is.null(response_vars) &&
      !is.null(task$results$anova_results)) {
    analyzer <- TaguchiAnalyzer$new()

    # Build an ANOVA-table data frame for contribution analysis.
    contribution <- list()
    for (resp_var in response_vars) {
      if (resp_var %in% names(task$results$anova_results)) {
        anova_tbl <- task$results$anova_results[[resp_var]]$anova
        if (!is.null(anova_tbl)) {
          anova_df <- data.frame(
            Term = rownames(anova_tbl),
            Df = anova_tbl$Df,
            Sum_Sq = anova_tbl$`Sum Sq`,
            Mean_Sq = anova_tbl$`Mean Sq`,
            stringsAsFactors = FALSE
          )
          contribution[[resp_var]] <- analyzer$compute_contribution(anova_df)
        }
      }
    }

    # Compute per-control-combination S/N ratio. This treats noise-factor
    # rows (when present) or replicated runs as the replicates used to
    # estimate variation. Falls back to a single S/N value over all
    # observations when grouping is not possible.
    control_names <- vapply(control_factors, function(f) f$name, character(1))
    noise_names <- if (!is.null(noise_factors)) {
      vapply(noise_factors, function(f) f$name, character(1))
    } else character(0)

    sn_ratios_by_response <- list()
    for (resp_var in response_vars) {
      if (!resp_var %in% names(data)) next

      if (length(control_names) > 0 && all(control_names %in% names(data))) {
        # Group observations by control-factor combination.
        robustness <- tryCatch(
          analyzer$analyze_robustness(
            data = data,
            control_factors = control_names,
            noise_factors = if (length(noise_names) > 0 &&
                               all(noise_names %in% names(data))) noise_names
                            else setdiff(names(data),
                                         c(control_names, resp_var)),
            response = resp_var,
            sn_type = sn_type
          ),
          error = function(e) NULL
        )

        if (!is.null(robustness)) {
          # Add a single overall S/N value computed from all observations.
          overall_sn <- analyzer$compute_sn_ratio(
            data[[resp_var]], type = sn_type
          )
          sn_ratios_by_response[[resp_var]] <- list(
            overall_sn_ratio = overall_sn,
            sn_type = sn_type,
            n_observations = nrow(data),
            by_control_combination = robustness
          )
        } else {
          # Fallback: single S/N value from all observations.
          sn_ratio <- analyzer$compute_sn_ratio(
            data[[resp_var]], type = sn_type
          )
          sn_ratios_by_response[[resp_var]] <- list(
            overall_sn_ratio = sn_ratio,
            sn_type = sn_type,
            n_observations = nrow(data)
          )
        }
      } else {
        sn_ratio <- analyzer$compute_sn_ratio(
          data[[resp_var]], type = sn_type
        )
        sn_ratios_by_response[[resp_var]] <- list(
          overall_sn_ratio = sn_ratio,
          sn_type = sn_type,
          n_observations = nrow(data)
        )
      }
    }

    taguchi_analysis <- list(
      sn_ratios = sn_ratios_by_response,
      contributions = contribution,
      sn_type = sn_type
    )
  }

  list(
    task = task,
    design = task$results$design_info,
    taguchi_analysis = taguchi_analysis
  )
}


#' @title Bayesian Optimization Design
#' @description Uses Bayesian optimization with a Gaussian-process surrogate
#'   model to perform sequential experimental design, automatically
#'   recommending the next optimal trial point.
#'
#' @param factors List of factor specifications.
#' @param response_function A black-box response function that takes a
#'   data.frame and returns a numeric vector.
#' @param n_initial Integer scalar, number of initial LHS design points
#'   (default `5 * n_factors`).
#' @param n_iterations Integer scalar, number of Bayesian-optimization
#'   iterations (default 20).
#' @param kernel Character scalar, the kernel type: currently only `"rbf"`
#'   is supported.
#' @param minimize Logical scalar, whether to minimize (default `TRUE`); set
#'   to `FALSE` to maximize.
#' @param seed Optional integer scalar, the random seed.
#'
#' @return A list with the optimizer, design, responses, best point/value
#'   and the optimization history.
#'
#' @examples
#' \dontrun{
#' # Define factors
#' factors <- list(
#'   list(name = "x1", type = "continuous", levels = c(0, 10)),
#'   list(name = "x2", type = "continuous", levels = c(0, 10))
#' )
#'
#' # Define the response function (simulated)
#' response_fn <- function(df) {
#'   -(df$x1 - 5)^2 - (df$x2 - 3)^2 + 50
#' }
#'
#' # Run Bayesian optimization
#' result <- bayesian_optimization(
#'   factors = factors,
#'   response_function = response_fn,
#'   n_iterations = 15
#' )
#'
#' # View the best solution
#' print(result$best_point)
#' print(result$best_value)
#' }
#'
#' @export
bayesian_optimization <- function(factors,
                                  response_function,
                                  n_initial = NULL,
                                  n_iterations = 20,
                                  kernel = "rbf",
                                  minimize = TRUE,
                                  seed = NULL) {
  .validate_factor_list(factors)
  if (!is.function(response_function)) {
    stop("[DOE] 'response_function' must be a function.")
  }
  kernel <- match.arg(kernel, c("rbf", "matern"))

  if (!is.null(seed)) {
    withr::local_seed(seed)
  }

  n_factors <- length(factors)
  factor_names <- vapply(factors, function(f) f$name, character(1))
  if (is.null(n_initial)) {
    n_initial <- max(5 * n_factors, 10)
  }

  # Generate the initial LHS-style design (uniform within bounds).
  lhs_design <- data.frame(matrix(NA_real_, nrow = n_initial, ncol = n_factors))
  colnames(lhs_design) <- factor_names
  for (i in seq_len(n_factors)) {
    factor <- factors[[i]]
    low <- min(factor$levels)
    high <- max(factor$levels)
    lhs_design[[factor$name]] <- runif(n_initial, min = low, max = high)
  }

  # Run the initial trials.
  initial_responses <- response_function(lhs_design)
  if (minimize) {
    y <- initial_responses
  } else {
    y <- -initial_responses  # Convert to a minimization problem
  }

  # Create the optimizer.
  optimizer <- BayesianOptimizer$new(kernel = kernel)
  optimizer$fit(lhs_design, y)

  # Define the search bounds.
  bounds <- list(
    lower = vapply(factors, function(f) min(f$levels), numeric(1)),
    upper = vapply(factors, function(f) max(f$levels), numeric(1))
  )
  names(bounds$lower) <- factor_names
  names(bounds$upper) <- factor_names

  # Sequential optimization loop.
  optimization_history <- list()
  for (iter in seq_len(n_iterations)) {
    # Recommend the next trial point.
    recommended <- optimizer$suggest_next(bounds, n_starts = 20,
                                          seed = if (!is.null(seed)) seed + iter else NULL)
    next_point <- as.data.frame(t(recommended$point))
    colnames(next_point) <- factor_names

    # Run the trial (defensively).
    response <- tryCatch(
      response_function(next_point),
      error = function(e) {
        warning(sprintf("[Bayesian DOE] response_function failed at iteration %d: %s",
                        iter, conditionMessage(e)))
        NA_real_
      }
    )
    if (length(response) != 1 || is.na(response)) {
      response <- mean(y, na.rm = TRUE)
    }
    y_new <- if (minimize) response else -response

    # Update the data set.
    lhs_design <- rbind(lhs_design, next_point)
    y <- c(y, y_new)

    # Retrain the model.
    optimizer$fit(lhs_design, y)

    # Record history.
    optimization_history[[iter]] <- list(
      iteration = iter,
      point = recommended$point,
      ei = recommended$ei,
      response = response,
      best_so_far = min(y)
    )

    message(sprintf("[Bayesian DOE] Iteration %d: f_best = %.4f, EI = %.4f",
                   iter, min(y), recommended$ei))
  }

  # Find the best solution.
  best_idx <- which.min(y)
  best_point <- lhs_design[best_idx, , drop = FALSE]
  best_value <- y[best_idx]

  list(
    optimizer = optimizer,
    design = lhs_design,
    responses = if (minimize) y else -y,
    best_point = best_point,
    best_value = if (minimize) best_value else -best_value,
    history = optimization_history,
    n_iterations = n_iterations
  )
}


#' @title Predictive DOE Data Flywheel
#' @description
#' Implements the closed loop: historical data -> surrogate model -> DOE
#' recommendation -> new trials -> model update. This is the core capability
#' that distinguishes iQualityR from Minitab/JMP: historical data guides DOE,
#' and DOE results feed back into the predictive model.
#'
#' @param historical_data Data frame of historical production / experiment
#'   data (low fidelity).
#' @param factor_cols Character vector of factor / feature column names.
#' @param response_col Character scalar, the response variable column name.
#' @param goal Character scalar, the optimization goal: `"screening"` or
#'   `"optimization"`.
#' @param budget Character scalar, the budget sensitivity: `"low"`,
#'   `"medium"` or `"high"`.
#' @param industry Character scalar, the industry type: `"manufacturing"` or
#'   `"service"`.
#' @param seed Optional integer scalar, the random seed.
#'
#' @return A list with the results of each flywheel stage.
#'
#' @examples
#' \dontrun{
#' # Scenario: you have 1000 historical production records and want to
#' # optimize the process parameters.
#' result <- predictive_doe_flywheel(
#'   historical_data = mes_history,
#'   factor_cols = c("Temp", "Pressure", "Time"),
#'   response_col = "Yield",
#'   goal = "optimization",
#'   budget = "medium"
#' )
#'
#' # View the recommended minimum number of new trials
#' print(result$recommendation)
#'
#' # Get the generated design table
#' print(result$doe_design)
#' }
#'
#' @export
predictive_doe_flywheel <- function(historical_data,
                                    factor_cols,
                                    response_col,
                                    goal = "optimization",
                                    budget = "medium",
                                    industry = "manufacturing",
                                    seed = NULL) {
  # Validate inputs.
  if (!is.data.frame(historical_data)) {
    stop("[Flywheel] 'historical_data' must be a data frame.")
  }
  missing_cols <- setdiff(c(factor_cols, response_col), names(historical_data))
  if (length(missing_cols) > 0) {
    stop("[Flywheel] Data is missing columns: ",
         paste(missing_cols, collapse = ", "))
  }
  goal <- match.arg(goal, c("screening", "optimization"))
  budget <- match.arg(budget, c("low", "medium", "high"))
  industry <- match.arg(industry, c("manufacturing", "service"))

  if (!is.null(seed)) {
    withr::local_seed(seed)
  }

  copilot <- AutoDOECopilot$new()
  mfo <- MultiFidelityOptimizer$new()

  # =========================================================================
  # Stage 1: assess the auxiliary value of the historical data
  # =========================================================================
  message("[Flywheel] Stage 1: assessing the auxiliary value of historical data for new trials...")

  aux_value <- mfo$evaluate_auxiliary_value(
    historical_data = historical_data,
    current_data = NULL,  # No new trial data yet
    response_col = response_col
  )

  recommended_runs <- aux_value$recommended_min_runs
  baseline_runs <- if (goal == "screening") 16 else 20
  corr_str <- if (is.null(aux_value$correlation)) "unknown" else aux_value$correlation
  message(sprintf("[Flywheel] Historical records: %d, correlation assessment: %s",
                  nrow(historical_data), corr_str))
  message(sprintf("[Flywheel] Recommended minimum new trials: %d (without historical data: %d)",
                  recommended_runs, baseline_runs))

  # =========================================================================
  # Stage 2: train a surrogate model on the historical data (low fidelity)
  # =========================================================================
  message("[Flywheel] Stage 2: training a surrogate model on historical data (low fidelity)...")

  # Build a second-order response-surface model with main effects,
  # quadratic terms and two-way interactions among the factors only.
  rhs_terms <- factor_cols
  rhs_terms <- c(rhs_terms, paste0("I(", factor_cols, "^2)"))
  if (length(factor_cols) >= 2) {
    interactions <- combn(factor_cols, 2,
                          FUN = function(pair) paste(pair, collapse = ":"))
    rhs_terms <- c(rhs_terms, interactions)
  }
  flywheel_formula <- reformulate(rhs_terms, response = response_col)
  lf_model <- lm(flywheel_formula, data = historical_data)

  lf_r2 <- summary(lf_model)$r.squared
  lf_rmse <- summary(lf_model)$sigma
  message(sprintf("[Flywheel] Surrogate model fit complete: R^2 = %.4f, RMSE = %.2f",
                  lf_r2, lf_rmse))

  # =========================================================================
  # Stage 3: AutoDOE strategy recommendation
  # =========================================================================
  message("[Flywheel] Stage 3: intelligently recommending a DOE strategy...")

  rec <- copilot$recommend(
    n_factors = length(factor_cols),
    goal = goal,
    budget = budget,
    industry = industry,
    has_historical_data = TRUE
  )

  message(sprintf("[Flywheel] Recommended strategy: %s", rec$strategies[[1]]$name))
  if (length(rec$strategies) > 1) {
    message(sprintf("[Flywheel] Additional advice: %s", rec$strategies[[2]]$name))
  }

  # =========================================================================
  # Stage 4: generate the new trial design table (high fidelity)
  # =========================================================================
  message("[Flywheel] Stage 4: generating the new trial design table (high fidelity)...")

  # Generate a space-filling design within the historical data range.
  n_runs <- max(recommended_runs, 5)  # At least 5 trials
  lhs_design <- data.frame(matrix(NA_real_, nrow = n_runs, ncol = length(factor_cols)))
  colnames(lhs_design) <- factor_cols

  for (col in factor_cols) {
    col_data <- historical_data[[col]]
    low <- min(col_data, na.rm = TRUE)
    high <- max(col_data, na.rm = TRUE)
    # Uniformly sample within the historical range.
    lhs_design[[col]] <- runif(n_runs, min = low, max = high)
  }

  # Predict the response at these points with the surrogate model.
  lf_pred <- predict(lf_model, newdata = lhs_design)
  lhs_design$Predicted_Response <- lf_pred

  # =========================================================================
  # Summarize the output
  # =========================================================================
  list(
    # Stage 1 results
    auxiliary_value = aux_value,

    # Stage 2 results
    surrogate_model = list(
      model = lf_model,
      r_squared = lf_r2,
      rmse = lf_rmse,
      formula = deparse(flywheel_formula)
    ),

    # Stage 3 results
    recommendation = rec,

    # Stage 4 results
    doe_design = lhs_design,

    # Metadata
    workflow = list(
      n_historical = nrow(historical_data),
      n_factors = length(factor_cols),
      factor_names = factor_cols,
      response_name = response_col,
      recommended_new_runs = n_runs,
      next_steps = c(
        "1. Execute physical trials / online A/B tests according to doe_design",
        "2. Collect the real response values",
        "3. Call MultiFidelityOptimizer$bias_corrected_prediction() to correct the surrogate model",
        "4. Update the historical data set with the new data and re-run this function to close the loop"
      )
    )
  )
}
