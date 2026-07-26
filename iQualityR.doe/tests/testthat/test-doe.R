# =============================================================================
# File: tests/testthat/test-doe.R
# Description: Unit tests for the DOE subpackage
# =============================================================================

library(testthat)
library(iQualityR.doe)

# -----------------------------------------------------------------------------
# IqrDoePlan configurator
# -----------------------------------------------------------------------------

test_that("IqrDoePlan basic initialization", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )

  plan <- IqrDoePlan$new(
    task_tag = "factorial",
    design_type = "factorial",
    factors = factors
  )

  expect_equal(plan$design_type, "factorial")
  expect_equal(length(plan$factors), 2)
  expect_equal(plan$replication, 1)
})

test_that("IqrDoePlan validates design type", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )

  expect_error(
    IqrDoePlan$new(
      task_tag = "test",
      design_type = "invalid_type",
      factors = factors
    ),
    "Unsupported design type"
  )
})

test_that("IqrDoePlan validates factor count", {
  # Fewer than 2 factors
  factors_one <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1))
  )

  plan <- IqrDoePlan$new(
    task_tag = "test",
    design_type = "factorial",
    factors = factors_one
  )

  expect_error(plan$validate(), "At least 2 factors")
})

test_that("IqrDoePlan validates resolution parameter", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )

  plan <- IqrDoePlan$new(
    task_tag = "test",
    design_type = "fractional",
    factors = factors,
    resolution = "VI"
  )

  # Validation error is triggered at validate() time.
  expect_error(plan$validate(), "Resolution must be")
})


# -----------------------------------------------------------------------------
# DoeAnalyzer
# -----------------------------------------------------------------------------

test_that("DoeAnalyzer generates full factorial design", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )

  plan <- IqrDoePlan$new(
    task_tag = "factorial",
    design_type = "factorial",
    factors = factors
  )

  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  expect_true(!is.null(results$design_info))
  expect_equal(nrow(results$design_info), 4)  # 2^2 = 4 runs
})

test_that("DoeAnalyzer generates orthogonal design", {
  factors <- lapply(1:3, function(i) {
    list(name = paste0("F", i), type = "continuous", levels = c(-1, 1))
  })

  plan <- IqrDoePlan$new(
    task_tag = "orthogonal",
    design_type = "orthogonal",
    factors = factors
  )

  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  expect_true(!is.null(results$design_info))
  expect_equal(nrow(results$design_info), 4)  # L4 orthogonal array
})


# -----------------------------------------------------------------------------
# User-facing entry functions
# -----------------------------------------------------------------------------

test_that("orthogonal_design entry function", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1)),
    list(name = "C", type = "continuous", levels = c(-1, 1))
  )

  task <- orthogonal_design(factors = factors, seed = 123)

  expect_s3_class(task, "IqrDoeTask")
  expect_true(!is.null(task$results))
})

test_that("factorial_design entry function", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )

  task <- factorial_design(factors = factors, design_type = "factorial")

  expect_s3_class(task, "IqrDoeTask")
  expect_equal(nrow(task$results$design_info), 4)
})

test_that("screening_design entry function", {
  factors <- lapply(1:5, function(i) {
    list(name = paste0("F", i), type = "continuous", levels = c(-1, 1))
  })

  task <- screening_design(factors = factors, resolution = "IV")

  expect_s3_class(task, "IqrDoeTask")
  expect_equal(task$plan$resolution, "IV")
})

test_that("entry functions reject invalid factor lists", {
  expect_error(orthogonal_design(factors = list()),
               "non-empty list")
  expect_error(factorial_design(factors = "not a list"),
               "non-empty list")
})


# -----------------------------------------------------------------------------
# TaguchiAnalyzer
# -----------------------------------------------------------------------------

test_that("TaguchiAnalyzer S/N larger-the-better", {
  analyzer <- TaguchiAnalyzer$new()
  sn <- analyzer$compute_sn_ratio(c(70, 75, 80, 85), type = "larger")

  expect_type(sn, "double")
  expect_true(sn > 0)  # S/N should be positive
})

test_that("TaguchiAnalyzer S/N smaller-the-better", {
  analyzer <- TaguchiAnalyzer$new()
  sn <- analyzer$compute_sn_ratio(c(1, 2, 3, 4), type = "smaller")

  expect_type(sn, "double")
  expect_true(sn < 0)  # Smaller-the-better S/N is usually negative
})

test_that("TaguchiAnalyzer S/N nominal-the-best", {
  analyzer <- TaguchiAnalyzer$new()
  sn <- analyzer$compute_sn_ratio(c(10, 11, 12, 13), type = "nominal")

  expect_type(sn, "double")
  expect_true(sn > 0)
})

test_that("TaguchiAnalyzer S/N invalid type errors", {
  analyzer <- TaguchiAnalyzer$new()
  expect_error(analyzer$compute_sn_ratio(c(1, 2, 3), type = "invalid"),
               "should be one of")
})

test_that("TaguchiAnalyzer contribution analysis", {
  analyzer <- TaguchiAnalyzer$new()

  anova_df <- data.frame(
    Term = c("A", "B", "AB", "Error"),
    Df = c(1, 1, 1, 4),
    Sum_Sq = c(100, 50, 20, 10),
    Mean_Sq = c(100, 50, 20, 2.5),
    stringsAsFactors = FALSE
  )

  contrib <- analyzer$compute_contribution(anova_df)

  expect_true(is.data.frame(contrib))
  expect_true("Contribution_Pct" %in% names(contrib))
  expect_equal(sum(contrib$Contribution_Pct), 100, tolerance = 0.1)
})

test_that("TaguchiAnalyzer robustness analysis", {
  analyzer <- TaguchiAnalyzer$new()

  test_data <- data.frame(
    Control_A = rep(c(-1, 1), each = 4),
    Control_B = rep(c(-1, 1), times = 4),
    Noise_X = rep(c(-1, 1), each = 2, times = 2),
    Noise_Y = rep(c(-1, 1), times = 4),
    Response = c(10, 12, 11, 13, 15, 16, 14, 17,
                 8, 9, 10, 11, 12, 14, 13, 15)
  )

  result <- analyzer$analyze_robustness(
    data = test_data,
    control_factors = c("Control_A", "Control_B"),
    noise_factors = c("Noise_X", "Noise_Y"),
    response = "Response",
    sn_type = "nominal"
  )

  expect_true(is.data.frame(result))
  expect_true("SN_Ratio" %in% names(result))
  expect_equal(nrow(result), 4)  # 2 x 2 = 4 control-factor combinations
})


# -----------------------------------------------------------------------------
# BayesianOptimizer
# -----------------------------------------------------------------------------

test_that("BayesianOptimizer initialization and training", {
  withr::local_seed(42)
  optimizer <- BayesianOptimizer$new(kernel = "rbf")

  X <- data.frame(x1 = c(1, 2, 3, 4, 5), x2 = c(2, 4, 6, 8, 10))
  y <- c(10, 15, 12, 18, 14)

  optimizer$fit(X, y)

  expect_true(!is.null(optimizer$gp_model))
  expect_equal(optimizer$y, y)
})

test_that("BayesianOptimizer prediction", {
  withr::local_seed(42)
  optimizer <- BayesianOptimizer$new(kernel = "rbf")

  X <- data.frame(x1 = 1:10, x2 = 11:20)
  y <- X$x1 + 2 * X$x2 + rnorm(10, sd = 0.1)

  optimizer$fit(X, y)

  X_new <- data.frame(x1 = c(3, 7), x2 = c(13, 17))
  pred <- optimizer$predict(X_new)

  expect_type(pred, "list")
  expect_true("predict" %in% names(pred))
  expect_true("se.fit" %in% names(pred))
  expect_equal(length(pred$predict), 2)
})

test_that("BayesianOptimizer expected improvement", {
  withr::local_seed(42)
  optimizer <- BayesianOptimizer$new(kernel = "rbf")

  X <- data.frame(x1 = 1:5)
  y <- c(10, 8, 6, 4, 2)
  optimizer$fit(X, y)

  X_new <- data.frame(x1 = c(2.5, 3.5))
  pred <- optimizer$predict(X_new)

  ei <- optimizer$expected_improvement(X_new, min(y))

  expect_type(ei, "double")
  expect_equal(length(ei), 2)
  expect_true(all(ei >= 0))
})

test_that("BayesianOptimizer suggest_next recommendation", {
  withr::local_seed(42)
  optimizer <- BayesianOptimizer$new(kernel = "rbf")

  X <- data.frame(x1 = 1:5)
  y <- c(10, 8, 6, 4, 2)
  optimizer$fit(X, y)

  bounds <- list(lower = c(0), upper = c(6))
  recommended <- optimizer$suggest_next(bounds, n_starts = 5, seed = 42)

  expect_type(recommended, "list")
  expect_true("point" %in% names(recommended))
  expect_true("ei" %in% names(recommended))
  expect_true(is.numeric(recommended$point))
  expect_true(is.numeric(recommended$ei))
})


# -----------------------------------------------------------------------------
# AutoDOECopilot (including the service-industry branch)
# -----------------------------------------------------------------------------

test_that("AutoDOECopilot manufacturing recommendation", {
  copilot <- AutoDOECopilot$new()
  rec <- copilot$recommend(6, "screening", "medium", industry = "manufacturing")

  expect_type(rec, "list")
  expect_true("strategies" %in% names(rec))
  expect_true("next_step" %in% names(rec))
  expect_equal(rec$input$industry, "manufacturing")
})

test_that("AutoDOECopilot service recommendation", {
  copilot <- AutoDOECopilot$new()
  rec <- copilot$recommend(4, "optimization", "medium", industry = "service")

  expect_type(rec, "list")
  expect_true("strategies" %in% names(rec))
  # The first service strategy must be the prerequisites reminder.
  expect_equal(rec$strategies[[1]]$name, "Service Industry DOE Prerequisites")
  # The last strategy must be the A/B validation recommendation.
  last_idx <- length(rec$strategies)
  expect_equal(rec$strategies[[last_idx]]$name, "Online A/B Validation (Required Step)")
})


# -----------------------------------------------------------------------------
# MultiFidelityOptimizer
# -----------------------------------------------------------------------------

test_that("MultiFidelityOptimizer evaluates auxiliary value", {
  withr::local_seed(123)
  mfo <- MultiFidelityOptimizer$new()
  hist <- data.frame(y = rnorm(500, 50, 5))
  curr <- data.frame(y = rnorm(5, 52, 5))

  # Use named arguments to avoid positional-argument ambiguity: the API
  # signature is evaluate_auxiliary_value(historical_data, response_col,
  # current_data = NULL).
  val <- mfo$evaluate_auxiliary_value(
    historical_data = hist,
    response_col = "y",
    current_data = curr
  )

  expect_type(val, "list")
  expect_true("historical_stats" %in% names(val))
  expect_true("recommended_min_runs" %in% names(val))
  expect_true(val$recommended_min_runs <= 20)  # Historical data should reduce runs
})


# -----------------------------------------------------------------------------
# TimeEffectModeler
# -----------------------------------------------------------------------------

test_that("TimeEffectModeler fits first-order decay", {
  modeler <- TimeEffectModeler$new()
  time <- c(0, 30, 60, 90, 120, 180)
  response <- 100 * exp(-0.01 * time) + rnorm(length(time), sd = 0.5)

  fit <- modeler$fit_decay_model(time, response, model_type = "first_order")

  expect_equal(fit$model_type, "first_order")
  expect_true(fit$r_squared > 0.95)
  expect_true(fit$parameters$R0 > 90)
  expect_true(fit$parameters$k > 0)
})

test_that("TimeEffectModeler shelf-life estimation", {
  modeler <- TimeEffectModeler$new()
  time <- c(0, 30, 60, 90, 120, 180)
  response <- 100 * exp(-0.01 * time) + rnorm(length(time), sd = 0.5)

  fit <- modeler$fit_decay_model(time, response, model_type = "first_order")
  sl <- modeler$estimate_shelf_life(fit, specification = 50)

  expect_true(sl$shelf_life > 0)
  expect_true(sl$lower_95 >= 0)
  expect_true(sl$upper_95 > sl$shelf_life)
  expect_true(!is.na(sl$se_shelf_life))
})

test_that("TimeEffectModeler multi-batch analysis", {
  modeler <- TimeEffectModeler$new()
  data <- data.frame(
    batch = rep(c("A", "B", "C"), each = 4),
    time = rep(c(0, 30, 60, 90), times = 3),
    response = c(100, 95, 90, 86,
                 100, 96, 92, 88,
                 100, 94, 89, 85)
  )

  result <- modeler$analyze_multi_batch(data, response_col = "response")

  expect_true("poolable" %in% names(result))
  expect_true("interpretation" %in% names(result))
  expect_true(is.character(result$interpretation))
})

test_that("TimeEffectModeler plot returns ggplot invisibly", {
  modeler <- TimeEffectModeler$new()
  time <- c(0, 30, 60, 90, 120)
  response <- 100 * exp(-0.01 * time)

  fit <- modeler$fit_decay_model(time, response, model_type = "first_order")
  p <- modeler$plot_time_effect(fit, time, response)

  expect_true(inherits(p, "ggplot"))
})


# -----------------------------------------------------------------------------
# predictive_doe_flywheel (end-to-end)
# -----------------------------------------------------------------------------

test_that("predictive_doe_flywheel complete workflow", {
  withr::local_seed(42)
  # Simulate historical data.
  temp_vals <- runif(200, 200, 300)
  pres_vals <- runif(200, 40, 60)
  hist <- data.frame(
    Temp = temp_vals,
    Pressure = pres_vals,
    Yield = 50 + 0.2 * temp_vals - 0.3 * pres_vals + rnorm(200, 0, 3)
  )

  result <- predictive_doe_flywheel(
    historical_data = hist,
    factor_cols = c("Temp", "Pressure"),
    response_col = "Yield",
    goal = "optimization",
    budget = "medium"
  )

  expect_type(result, "list")
  expect_true("auxiliary_value" %in% names(result))
  expect_true("surrogate_model" %in% names(result))
  expect_true("recommendation" %in% names(result))
  expect_true("doe_design" %in% names(result))
  expect_true("workflow" %in% names(result))

  # The DOE design table row count equals the recommended number of trials.
  expect_equal(nrow(result$doe_design),
               result$auxiliary_value$recommended_min_runs)
})


# -----------------------------------------------------------------------------
# DOE professional enhancements: proper fractional factorial, Box-Behnken,
# curvature test, extended orthogonal arrays, blocking, alias structure.
# -----------------------------------------------------------------------------

test_that("fractional factorial uses proper design generators (5 factors res V = 16 runs)", {
  factors <- lapply(LETTERS[1:5], function(nm) {
    list(name = nm, type = "continuous", levels = c(-1, 1))
  })

  plan <- IqrDoePlan$new(
    task_tag = "fractional",
    design_type = "fractional",
    factors = factors,
    resolution = "V"
  )

  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  expect_true(!is.null(results$design_info))
  # 2^(5-1) = 16 runs, defining relation I = ABCDE (resolution V).
  expect_equal(nrow(results$design_info), 16)

  # Verify the design is orthogonal: each factor column has 8 high and 8 low.
  for (nm in LETTERS[1:5]) {
    expect_equal(sum(results$design_info[[nm]] ==  1), 8)
    expect_equal(sum(results$design_info[[nm]] == -1), 8)
  }
})

test_that("fractional factorial 5 factors res III = 8 runs", {
  factors <- lapply(LETTERS[1:5], function(nm) {
    list(name = nm, type = "continuous", levels = c(-1, 1))
  })

  plan <- IqrDoePlan$new(
    task_tag = "fractional",
    design_type = "fractional",
    factors = factors,
    resolution = "III",
    randomize = FALSE
  )

  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  # 2^(5-2) = 8 runs.
  expect_equal(nrow(results$design_info), 8)

  # D = A*B and E = A*C, so check the generator relations hold.
  d_expected <- results$design_info$A * results$design_info$B
  e_expected <- results$design_info$A * results$design_info$C
  expect_equal(results$design_info$D, d_expected)
  expect_equal(results$design_info$E, e_expected)
})

test_that("get_alias_structure returns defining relation for 5-factor res V", {
  analyzer <- DoeAnalyzer$new()
  alias <- analyzer$get_alias_structure(n_factors = 5, resolution = "V")

  expect_true(!is.null(alias$defining_relation))
  expect_equal(length(alias$defining_relation), 1)  # Only one word: ABCDE
  expect_equal(alias$defining_relation, "ABCDE")
  expect_equal(alias$resolution, 5)
  expect_equal(alias$n_runs, 16)
  expect_true("ABCDE" %in% alias$defining_relation)
  # Main effect A is aliased with BCDE
  expect_true("BCDE" %in% alias$alias_main[["A"]])
})

test_that("Box-Behnken design for 3 factors produces 12 + 3 = 15 runs", {
  factors <- lapply(LETTERS[1:3], function(nm) {
    list(name = nm, type = "continuous", levels = c(-1, 1))
  })

  plan <- IqrDoePlan$new(
    task_tag = "rsm",
    design_type = "box_behnken",
    factors = factors,
    center_points = 3,
    randomize = FALSE
  )

  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  expect_true(!is.null(results$design_info))
  # 4 * C(3,2) + 3 center = 12 + 3 = 15 runs.
  expect_equal(nrow(results$design_info), 15)
})

test_that("Box-Behnken 4 factors produces 24 + 3 = 27 runs", {
  factors <- lapply(LETTERS[1:4], function(nm) {
    list(name = nm, type = "continuous", levels = c(-1, 1))
  })

  plan <- IqrDoePlan$new(
    task_tag = "rsm",
    design_type = "box_behnken",
    factors = factors,
    center_points = 3,
    randomize = FALSE
  )

  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  # 4 * C(4,2) + 3 center = 4 * 6 + 3 = 27 runs.
  expect_equal(nrow(results$design_info), 27)
})

test_that("L9 orthogonal array generates 9 runs for 3-level factors", {
  factors <- lapply(LETTERS[1:4], function(nm) {
    list(name = nm, type = "continuous", levels = c(10, 20, 30))
  })

  plan <- IqrDoePlan$new(
    task_tag = "orthogonal",
    design_type = "orthogonal",
    factors = factors,
    randomize = FALSE
  )

  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  expect_equal(nrow(results$design_info), 9)
  # Verify each factor column has 3 low, 3 mid, 3 high values.
  for (nm in LETTERS[1:4]) {
    expect_equal(sum(results$design_info[[nm]] == 10), 3)
    expect_equal(sum(results$design_info[[nm]] == 20), 3)
    expect_equal(sum(results$design_info[[nm]] == 30), 3)
  }
})

test_that("L12 Plackett-Burman supports up to 11 factors at 2 levels", {
  factors <- lapply(LETTERS[1:11], function(nm) {
    list(name = nm, type = "continuous", levels = c(-1, 1))
  })

  plan <- IqrDoePlan$new(
    task_tag = "screening",
    design_type = "orthogonal",
    factors = factors,
    randomize = FALSE
  )

  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  expect_equal(nrow(results$design_info), 12)
})

test_that("L16 orthogonal array supports 12 factors at 2 levels", {
  # L12 (PB12) accommodates up to 11 factors; 12 factors forces L16.
  factors <- lapply(LETTERS[1:12], function(nm) {
    list(name = nm, type = "continuous", levels = c(-1, 1))
  })

  plan <- IqrDoePlan$new(
    task_tag = "screening",
    design_type = "orthogonal",
    factors = factors,
    randomize = FALSE
  )

  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  expect_equal(nrow(results$design_info), 16)
})

test_that("L27 orthogonal array supports 5 factors at 3 levels", {
  # L9 (3^4) accommodates up to 4 factors; 5 factors forces L27.
  factors <- lapply(LETTERS[1:5], function(nm) {
    list(name = nm, type = "continuous", levels = c(10, 20, 30))
  })

  plan <- IqrDoePlan$new(
    task_tag = "rsm",
    design_type = "orthogonal",
    factors = factors,
    randomize = FALSE
  )

  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  expect_equal(nrow(results$design_info), 27)
})

test_that("blocking partitions design into n_blocks blocks", {
  factors <- lapply(LETTERS[1:3], function(nm) {
    list(name = nm, type = "continuous", levels = c(-1, 1))
  })

  plan <- IqrDoePlan$new(
    task_tag = "factorial",
    design_type = "factorial",
    factors = factors,
    blocking = TRUE,
    n_blocks = 2,
    seed = 42
  )

  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  expect_true("Block" %in% names(results$design_info))
  expect_equal(length(unique(results$design_info$Block)), 2)
  # 8 factorial runs partitioned into 2 blocks of 4 each.
  expect_equal(sum(results$design_info$Block == 1), 4)
  expect_equal(sum(results$design_info$Block == 2), 4)
})

test_that("curvature test detects lack of curvature (linear response)", {
  # Construct a 2^2 factorial design with 4 center points.
  # With a perfectly linear response (no curvature), the test should not
  # detect significant curvature.
  set.seed(42)
  design_df <- data.frame(
    A = c(-1, -1,  1,  1, 0, 0, 0, 0),
    B = c(-1,  1, -1,  1, 0, 0, 0, 0),
    Y = c(10, 14, 18, 22, 16, 16, 16, 16)
  )

  model <- lm(Y ~ A * B, data = design_df)
  analyzer <- DoeAnalyzer$new()
  result <- analyzer$test_curvature(model, design_df, "Y", c("A", "B"))

  expect_true(!is.na(result$p_value))
  expect_true(result$n_center == 4)
  expect_true(result$n_factorial == 4)
  # With identical center and factorial means, curvature estimate should be ~0.
  expect_true(abs(result$curvature_estimate) < 1e-6)
})

test_that("curvature test detects curvature when center differs from factorial", {
  # Construct a 2^2 factorial + 4 center points where the response has
  # strong quadratic curvature (center point mean >> factorial point mean).
  design_df <- data.frame(
    A = c(-1, -1,  1,  1, 0, 0, 0, 0),
    B = c(-1,  1, -1,  1, 0, 0, 0, 0),
    Y = c(10, 10, 10, 10, 50, 50, 50, 50)
  )

  model <- lm(Y ~ A * B, data = design_df)
  analyzer <- DoeAnalyzer$new()
  result <- analyzer$test_curvature(model, design_df, "Y", c("A", "B"))

  expect_true(!is.na(result$p_value))
  # Curvature estimate should be -40 (factorial mean 10 minus center mean 50).
  expect_equal(result$curvature_estimate, -40)
  # The p-value should be highly significant.
  expect_true(result$p_value < 0.001)
})

test_that("curvature test uses center-point pure error with realistic noise", {
  # Realistic DOE data: factorial points unreplicated, center points have
  # small measurement noise. The implementation must use the center-point
  # pure error (not the model MSE, which would include lack-of-fit).
  set.seed(2026)
  design_df <- data.frame(
    A = c(-1, -1,  1,  1, 0, 0, 0, 0),
    B = c(-1,  1, -1,  1, 0, 0, 0, 0),
    Y = c(10, 10, 10, 10, 50, 50.1, 49.9, 50.05)  # center mean = 50.0125
  )

  model <- lm(Y ~ A * B, data = design_df)
  analyzer <- DoeAnalyzer$new()
  result <- analyzer$test_curvature(model, design_df, "Y", c("A", "B"))

  # Method must report using center-point pure error (no factorial replicates).
  expect_true(grepl("pure-error", result$method))
  expect_equal(result$df, 3L)  # n_center - 1 = 3
  expect_true(!is.na(result$p_value))
  # Curvature estimate ~ -40 (factorial mean 10 minus center mean ~50).
  expect_true(abs(result$curvature_estimate - (-40.0125)) < 1e-6)
  # With realistic noise, p-value should still be highly significant.
  expect_true(result$p_value < 0.001)
})


# -----------------------------------------------------------------------------
# Power analysis & sample size calculation (Montgomery 2019 ch. 7)
# -----------------------------------------------------------------------------

test_that("compute_power returns valid power for 2^3 design", {
  analyzer <- DoeAnalyzer$new()
  # 2^3 design with 2 replicates, detecting Delta = 2*sigma (effect twice sigma).
  result <- analyzer$compute_power(
    n_factors = 3,
    n_replicates = 2,
    effect_size = 2,
    sigma = 1,
    model_order = "main"
  )

  expect_type(result$power, "double")
  expect_true(result$power > 0 && result$power < 1)
  expect_equal(result$df_num, 1L)
  expect_equal(result$df_den, 16L - 4L)  # N=16, p=4 (intercept + 3 main)
  expect_equal(result$n_runs, 16L)
  expect_equal(result$effect_to_sigma_ratio, 2)
})

test_that("compute_power increases with n_replicates", {
  analyzer <- DoeAnalyzer$new()
  p1 <- analyzer$compute_power(n_factors = 3, n_replicates = 1,
                                effect_size = 2, sigma = 1)$power
  p2 <- analyzer$compute_power(n_factors = 3, n_replicates = 2,
                                effect_size = 2, sigma = 1)$power
  p3 <- analyzer$compute_power(n_factors = 3, n_replicates = 4,
                                effect_size = 2, sigma = 1)$power

  # Power should monotonically increase with replicates.
  expect_true(p2 > p1)
  expect_true(p3 > p2)
})

test_that("compute_power increases with effect size", {
  analyzer <- DoeAnalyzer$new()
  p_small <- analyzer$compute_power(n_factors = 3, n_replicates = 2,
                                     effect_size = 1, sigma = 1)$power
  p_large <- analyzer$compute_power(n_factors = 3, n_replicates = 2,
                                     effect_size = 4, sigma = 1)$power

  expect_true(p_large > p_small)
})

test_that("compute_power validates inputs", {
  analyzer <- DoeAnalyzer$new()
  expect_error(analyzer$compute_power(n_factors = 0, effect_size = 1, sigma = 1),
               "positive integer")
  expect_error(analyzer$compute_power(n_factors = 3, effect_size = -1, sigma = 1),
               "positive numeric")
  expect_error(analyzer$compute_power(n_factors = 3, effect_size = 1, sigma = -1),
               "positive numeric")
  expect_error(analyzer$compute_power(n_factors = 3, effect_size = 1, sigma = 1,
                                       alpha = 1.5),
               "alpha must be in")
  expect_error(analyzer$compute_power(n_factors = 3, effect_size = 1, sigma = 1,
                                       model_order = "invalid"),
               "model_order")
})

test_that("compute_sample_size finds minimum replicates for target power", {
  analyzer <- DoeAnalyzer$new()
  result <- analyzer$compute_sample_size(
    n_factors = 3,
    effect_size = 2,
    sigma = 1,
    target_power = 0.80,
    model_order = "main"
  )

  expect_true(result$converged)
  expect_true(result$n_replicates >= 1)
  expect_true(result$achieved_power >= 0.80)
  expect_equal(result$n_runs, as.integer(result$n_replicates * 2^3))
})

test_that("compute_sample_size handles underpowered scenarios", {
  analyzer <- DoeAnalyzer$new()
  # Very small effect relative to sigma: hard to detect.
  result <- analyzer$compute_sample_size(
    n_factors = 3,
    effect_size = 0.05,
    sigma = 1,
    target_power = 0.95,
    max_replicates = 5
  )

  expect_false(result$converged)
  expect_true(is.na(result$n_replicates))
})


# -----------------------------------------------------------------------------
# Design evaluation (D/A/G/I-optimality)
# -----------------------------------------------------------------------------

test_that("evaluate_design returns optimality criteria for 2^2 factorial", {
  analyzer <- DoeAnalyzer$new()
  design <- data.frame(
    A = c(-1, -1,  1,  1),
    B = c(-1,  1, -1,  1)
  )

  result <- analyzer$evaluate_design(design, model_order = "main")

  expect_true(!is.na(result$D_eff))
  expect_true(!is.na(result$A_eff))
  expect_true(!is.na(result$G_eff))
  expect_true(!is.na(result$I_eff))
  expect_equal(result$n_runs, 4L)
  expect_equal(result$n_params, 3L)  # intercept + A + B
  # For a balanced 2^2 factorial, D-efficiency should be 1 (perfect).
  expect_equal(result$D_eff, 1, tolerance = 1e-6)
  # G-efficiency should also be 1 for a balanced design.
  expect_equal(result$G_eff, 1, tolerance = 1e-6)
})

test_that("evaluate_design with main_2fi model order", {
  analyzer <- DoeAnalyzer$new()
  design <- data.frame(
    A = c(-1, -1,  1,  1),
    B = c(-1,  1, -1,  1)
  )

  result <- analyzer$evaluate_design(design, model_order = "main_2fi")

  expect_equal(result$n_params, 4L)  # intercept + A + B + AB
  # 2^2 is exactly saturated for the full 2FI model (4 runs, 4 params).
  # D-efficiency = 1 for a saturated balanced design.
  expect_equal(result$D_eff, 1, tolerance = 1e-6)
})

test_that("evaluate_design detects rank deficiency", {
  analyzer <- DoeAnalyzer$new()
  # Aliased design: B = A (no information to separate B from A).
  design <- data.frame(
    A = c(-1, -1,  1,  1),
    B = c(-1, -1,  1,  1)
  )

  result <- analyzer$evaluate_design(design, model_order = "main")

  # D-efficiency should be NA due to singularity.
  expect_true(is.na(result$D_eff))
  expect_true(grepl("singular|invertible", result$interpretation))
})

test_that("evaluate_design with CCD (rotatable)", {
  analyzer <- DoeAnalyzer$new()
  # 2-factor CCD: 4 factorial + 4 axial + 5 center = 13 runs.
  # Rotatable alpha = (2^2)^(1/4) = sqrt(2).
  alpha <- sqrt(2)
  design <- data.frame(
    A = c(-1, -1,  1,  1, -alpha, alpha, 0, 0, 0, 0, 0, 0, 0),
    B = c(-1,  1, -1,  1, 0, 0, -alpha, alpha, 0, 0, 0, 0, 0)
  )

  result <- analyzer$evaluate_design(design, model_order = "quadratic")

  expect_equal(result$n_runs, 13L)
  expect_equal(result$n_params, 6L)  # intercept + A + B + A^2 + B^2 + AB
  # A rotatable CCD should have high D-efficiency for the quadratic model.
  expect_true(result$D_eff > 0.5)
  # G-efficiency: max prediction variance should be moderate.
  expect_true(result$G_eff > 0.3)
})

test_that("evaluate_design validates inputs", {
  analyzer <- DoeAnalyzer$new()
  expect_error(analyzer$evaluate_design("not a data frame"),
               "must be a data frame or matrix")
  expect_error(analyzer$evaluate_design(data.frame(A = c("a", "b"))),
               "must contain only numeric")
})

# -----------------------------------------------------------------------------
# DSD (Definitive Screening Design) generator
# -----------------------------------------------------------------------------

test_that("DSD generates 2k+1 runs for k factors", {
  for (k in 2:6) {
    factors <- lapply(LETTERS[1:k], function(nm) {
      list(name = nm, type = "continuous", levels = c(-1, 0, 1))
    })
    plan <- IqrDoePlan$new(
      task_tag = "dsd", design_type = "dsd",
      factors = factors, center_points = 1, randomize = FALSE
    )
    analyzer <- DoeAnalyzer$new()
    results <- analyzer$run(plan = plan)
    # 2k + 1 runs (plus any extra center points)
    expect_equal(nrow(results$design_info), 2 * k + 1,
                 info = paste("k =", k))
  }
})

test_that("DSD has a center point at the origin", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 0, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 0, 1)),
    list(name = "C", type = "continuous", levels = c(-1, 0, 1))
  )
  plan <- IqrDoePlan$new(
    task_tag = "dsd", design_type = "dsd",
    factors = factors, center_points = 1, randomize = FALSE
  )
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  # The last row should be the center point (all zeros in coded units).
  # Check that at least one row has PointType == "center".
  expect_true("center" %in% results$design_info$PointType)
})

test_that("DSD supports extra center points", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 0, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 0, 1))
  )
  plan <- IqrDoePlan$new(
    task_tag = "dsd", design_type = "dsd",
    factors = factors, center_points = 4, randomize = FALSE
  )
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  # 2*2 + 1 = 5 base runs + 3 extra center points = 8 total.
  expect_equal(nrow(results$design_info), 8)
  expect_equal(sum(results$design_info$PointType == "center"), 4)
})

# -----------------------------------------------------------------------------
# Path of Steepest Ascent
# -----------------------------------------------------------------------------

test_that("compute_steepest_ascent returns correct path", {
  # Fit a simple first-order model with known coefficients.
  # y = 10 + 2*A + 1*B (gradient direction = (2, 1))
  data <- data.frame(
    A = c(-1, -1, 1, 1, 0),
    B = c(-1, 1, -1, 1, 0),
    y = c(10 - 2 - 1, 10 - 2 + 1, 10 + 2 - 1, 10 + 2 + 1, 10)
  )
  model <- lm(y ~ A + B, data = data)
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )
  analyzer <- DoeAnalyzer$new()
  path <- analyzer$compute_steepest_ascent(model, factors, step_size = 1,
                                            n_steps = 3, maximize = TRUE)
  expect_s3_class(path, "data.frame")
  expect_equal(nrow(path), 4)  # 0, 1, 2, 3 steps
  expect_equal(path$Step, 0:3)
  # Reference factor is A (|2| > |1|), so A's coded step = 1.
  expect_equal(path$A_coded, 0:3)
  # B's coded step = 1/2 = 0.5 per unit step of A.
  expect_equal(path$B_coded, seq(0, 1.5, by = 0.5))
  # Predicted response should increase (steepest ascent).
  expect_true(path$Predicted[4] > path$Predicted[1])
})

test_that("compute_steepest_ascent descent reverses direction", {
  data <- data.frame(
    A = c(-1, -1, 1, 1, 0),
    B = c(-1, 1, -1, 1, 0),
    y = c(10 - 2 - 1, 10 - 2 + 1, 10 + 2 - 1, 10 + 2 + 1, 10)
  )
  model <- lm(y ~ A + B, data = data)
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )
  analyzer <- DoeAnalyzer$new()
  path <- analyzer$compute_steepest_ascent(model, factors, maximize = FALSE)
  # Predicted response should decrease (steepest descent).
  expect_true(path$Predicted[4] < path$Predicted[1])
  expect_equal(attr(path, "direction"), "descent")
})

# -----------------------------------------------------------------------------
# Model selection (stepwise / forward / backward)
# -----------------------------------------------------------------------------

test_that("select_model reduces model with backward selection", {
  # Fit a model where some terms are clearly inert. Use a replicated 2^3
  # factorial (16 runs) so the full y ~ A*B*C model (8 parameters) is not
  # saturated; otherwise MASS::stepAIC cannot compute a finite AIC.
  set.seed(42)
  base <- expand.grid(A = c(-1, 1), B = c(-1, 1), C = c(-1, 1))
  data <- rbind(base, base)
  # Only main effects A and B are active; interactions and C are inert.
  data$y <- 10 + 2 * data$A + 1 * data$B + rnorm(16, 0, 0.1)
  # Full model: y ~ A * B * C (7 terms + intercept)
  model <- lm(y ~ A * B * C, data = data)
  analyzer <- DoeAnalyzer$new()
  result <- analyzer$select_model(model, direction = "backward",
                                   criterion = "aic", trace = FALSE)
  expect_s3_class(result$model, "lm")
  expect_true(result$n_terms_final <= result$n_terms_initial)
  expect_true(result$criterion_final <= result$criterion_initial)
})

test_that("select_model supports AICc criterion", {
  set.seed(42)
  data <- data.frame(
    A = rep(c(-1, 1), each = 4),
    B = rep(c(-1, 1, -1, 1), 2),
    y = c(10, 12, 14, 16, 10, 12, 14, 16) + rnorm(8, 0, 0.1)
  )
  model <- lm(y ~ A * B, data = data)
  analyzer <- DoeAnalyzer$new()
  result <- analyzer$select_model(model, direction = "backward",
                                   criterion = "aicc", trace = FALSE)
  expect_equal(result$criterion, "aicc")
  expect_s3_class(result$model, "lm")
})

# -----------------------------------------------------------------------------
# Extended model fit statistics (PRESS, R²pred, VIF)
# -----------------------------------------------------------------------------

test_that("compute_model_fit_extended returns PRESS and R²pred", {
  data <- data.frame(
    A = c(-1, -1, 1, 1),
    B = c(-1, 1, -1, 1),
    y = c(10, 12, 14, 16)
  )
  model <- lm(y ~ A + B, data = data)
  analyzer <- DoeAnalyzer$new()
  fit <- analyzer$compute_model_fit_extended(model)
  expect_true(fit$PRESS > 0)
  expect_true(fit$r_squared_pred <= 1)
  expect_true(length(fit$vif) >= 2)
})

test_that("compute_model_fit_extended detects multicollinearity", {
  # Create collinear data: C = 2*A (perfect collinearity).
  data <- data.frame(
    A = c(1, 2, 3, 4, 5),
    C = c(2, 4, 6, 8, 10),
    y = c(2, 4, 6, 8, 10)
  )
  model <- lm(y ~ A + C, data = data)
  analyzer <- DoeAnalyzer$new()
  fit <- analyzer$compute_model_fit_extended(model)
  # VIF should be very high (perfect collinearity).
  expect_true(fit$max_vif > 10)
  expect_true(fit$has_multicollinearity)
})

# -----------------------------------------------------------------------------
# Overlaid contour plot
# -----------------------------------------------------------------------------

test_that("overlaid contour plot renders for two responses", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )
  plan <- IqrDoePlan$new(
    task_tag = "rsm", design_type = "ccd",
    factors = factors, center_points = 4, randomize = FALSE,
    response_vars = c("Yield", "Cost")
  )
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)

  # Simulate two response models.
  design_data <- results$design_info
  set.seed(42)
  design_data$Yield <- 50 + 5 * design_data$A + 3 * design_data$B +
    2 * design_data$A * design_data$B - 1 * design_data$A^2 +
    rnorm(nrow(design_data), 0, 0.5)
  design_data$Cost <- 10 - 2 * design_data$A + 1 * design_data$B +
    rnorm(nrow(design_data), 0, 0.3)

  task <- IqrDoeTask$new(plan = plan, data = design_data,
                          theme = "academic")
  task$compute()

  # Build response specs from the two fitted models.
  response_specs <- list(
    Yield = list(
      model = task$results$anova_results$Yield$model,
      lower = 52, upper = Inf, target = 56
    ),
    Cost = list(
      model = task$results$anova_results$Cost$model,
      lower = -Inf, upper = 12, target = 10
    )
  )

  p <- task$plot(type = "overlaid_contour",
                 response_specs = response_specs)
  expect_s3_class(p, c("ggplot", "gg"))
})

# -----------------------------------------------------------------------------
# P1-5: Fold-Over / Design Augmentation
# -----------------------------------------------------------------------------

test_that("fold_over doubles runs and reverses signs (full foldover)", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1)),
    list(name = "C", type = "continuous", levels = c(-1, 1))
  )
  design <- data.frame(
    A = c(-1,  1, -1, 1),
    B = c(-1, -1,  1, 1),
    C = c(-1,  1,  1, -1)
  )
  analyzer <- DoeAnalyzer$new()
  folded <- analyzer$fold_over(design, factors)

  expect_equal(nrow(folded), 8)            # doubled
  expect_equal(unique(folded$Foldover), c("original", "foldover"))
  # Mirror rows must have all signs reversed.
  mirror <- folded[folded$Foldover == "foldover", c("A", "B", "C")]
  expect_equal(unname(as.matrix(mirror)), unname(-as.matrix(design)))
})

test_that("fold_over partial reverses only one factor", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )
  design <- data.frame(A = c(-1, 1, -1, 1), B = c(-1, -1, 1, 1))
  analyzer <- DoeAnalyzer$new()
  folded <- analyzer$fold_over(design, factors, fold_factor = "A")

  mirror <- folded[folded$Foldover == "foldover", ]
  expect_equal(mirror$A, -design$A)
  expect_equal(mirror$B, design$B)        # B unchanged
})

test_that("augment_to_ccd adds axial and center points", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )
  design <- data.frame(
    A = c(-1, 1, -1, 1), B = c(-1, -1, 1, 1)
  )
  analyzer <- DoeAnalyzer$new()
  ccd <- analyzer$augment_to_ccd(design, factors, alpha_type = "face_centered",
                                 n_center_points = 3)

  expect_true("PointType" %in% names(ccd))
  expect_true(all(c("cube", "axial", "center") %in% ccd$PointType))
  # 4 factorial + 4 axial + 3 center = 11 runs
  expect_equal(nrow(ccd), 4 + 4 + 3)
  # Face-centered alpha = 1.
  axial_rows <- ccd[ccd$PointType == "axial", c("A", "B")]
  expect_true(all(abs(axial_rows$A) <= 1 + 1e-9))
})

# -----------------------------------------------------------------------------
# P1-9: Uncoded Coefficient Equation
# -----------------------------------------------------------------------------

test_that("get_uncoded_equation converts coded coefficients to real units", {
  # Factor A spans 80..120 (center 100, half-range 20); B spans 10..30.
  factors <- list(
    list(name = "A", type = "continuous", levels = c(80, 120)),
    list(name = "B", type = "continuous", levels = c(10, 30))
  )
  # Coded design + a purely linear response y = 5 + 2*x1_coded + 3*x2_coded.
  coded_df <- data.frame(
    A = c(-1, 1, -1, 1), B = c(-1, -1, 1, 1)
  )
  coded_df$Y <- 5 + 2 * coded_df$A + 3 * coded_df$B
  model <- lm(Y ~ A + B, data = coded_df)

  analyzer <- DoeAnalyzer$new()
  unc <- analyzer$get_uncoded_equation(model, factors)

  expect_type(unc, "list")
  expect_true(all(c("equation", "coefficients") %in% names(unc)))
  # In real units the slope for A is 2 / 20 = 0.1, for B is 3 / 10 = 0.3.
  expect_equal(unname(unc$coefficients["A"]), 0.1, tolerance = 1e-9)
  expect_equal(unname(unc$coefficients["B"]), 0.3, tolerance = 1e-9)
  # Intercept picks up the shift: 5 - 2*100/20 - 3*20/10 = 5 - 10 - 6 = -11.
  expect_equal(unname(unc$coefficients["(Intercept)"]), -11, tolerance = 1e-9)
  # Equation string must contain the response symbol.
  expect_true(is.character(unc$equation) && nchar(unc$equation) > 0)
})

# -----------------------------------------------------------------------------
# P1-10: Power Curve
# -----------------------------------------------------------------------------

test_that("plot_power_curve returns a monotone increasing curve", {
  analyzer <- DoeAnalyzer$new()
  curve <- analyzer$plot_power_curve(n_factors = 3, n_replicates = 1,
                                      sigma = 1, n_points = 20,
                                      max_effect = 8)
  expect_s3_class(curve, "data.frame")
  expect_true(all(c("Effect_Size", "Power") %in% names(curve)))
  expect_equal(nrow(curve), 20)
  # Power must be non-decreasing in effect size.
  expect_true(all(diff(curve$Power) >= -1e-9))
  # Final power near 1 for a large effect.
  expect_true(curve$Power[nrow(curve)] > 0.5)
})

test_that("power_curve plot renders a ggplot", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )
  plan <- IqrDoePlan$new(task_tag = "factorial", design_type = "factorial",
                          factors = factors, randomize = FALSE)
  plotter <- DoePlotter$new()
  results <- list(power_curve_data = DoeAnalyzer$new()$plot_power_curve(
    n_factors = 2, sigma = 1, n_points = 15))
  p <- plotter$render(results, theme_obj = NULL, type = "power_curve",
                       plan = plan)
  expect_s3_class(p, c("ggplot", "gg"))
})

# -----------------------------------------------------------------------------
# P1-7 & P1-8: Normal Effects & Cube plots
# -----------------------------------------------------------------------------

test_that("normal_effects plot renders for a 2^3 factorial", {
  factors <- lapply(LETTERS[1:3], function(nm) {
    list(name = nm, type = "continuous", levels = c(-1, 1))
  })
  plan <- IqrDoePlan$new(task_tag = "factorial", design_type = "factorial",
                          factors = factors, randomize = FALSE,
                          response_vars = "Y")
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  design_data <- results$design_info
  set.seed(1)
  design_data$Y <- 10 + 5 * design_data$A - 3 * design_data$B +
    rnorm(nrow(design_data), 0, 0.5)
  task <- IqrDoeTask$new(plan = plan, data = design_data, theme = "academic")
  task$compute()
  p <- task$plot(type = "normal_effects")
  expect_s3_class(p, c("ggplot", "gg"))
})

test_that("cube plot renders for a 2^3 factorial", {
  factors <- lapply(LETTERS[1:3], function(nm) {
    list(name = nm, type = "continuous", levels = c(-1, 1))
  })
  plan <- IqrDoePlan$new(task_tag = "factorial", design_type = "factorial",
                          factors = factors, randomize = FALSE,
                          response_vars = "Y")
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  design_data <- results$design_info
  set.seed(2)
  design_data$Y <- 10 + 4 * design_data$A + 2 * design_data$B +
    1 * design_data$C + rnorm(nrow(design_data), 0, 0.3)
  task <- IqrDoeTask$new(plan = plan, data = design_data, theme = "academic")
  task$compute()
  p <- task$plot(type = "cube")
  expect_s3_class(p, c("ggplot", "gg"))
})

# -----------------------------------------------------------------------------
# P2-14: Ridge Analysis
# -----------------------------------------------------------------------------

test_that("ridge_analysis returns a path of predicted optima", {
  # A second-order model with a saddle (stationary point outside [-1,1]^2).
  # Use a proper CCD (cube + axial + center) so A^2 and B^2 are estimable
  # without collinearity.
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )
  coded_df <- expand.grid(A = c(-1, 1), B = c(-1, 1))
  # Add face-centered axial points so the quadratic terms are identifiable.
  coded_df <- rbind(coded_df,
                    data.frame(A = c(1, -1, 0, 0), B = c(0, 0, 1, -1)),
                    data.frame(A = 0, B = 0))   # center point
  set.seed(7)
  coded_df$Y <- 10 + 3 * coded_df$A + 2 * coded_df$B -
    4 * coded_df$A^2 - 4 * coded_df$B^2 + 1 * coded_df$A * coded_df$B +
    rnorm(nrow(coded_df), 0, 0.1)
  model <- lm(Y ~ A + B + A:B + I(A^2) + I(B^2), data = coded_df)

  analyzer <- DoeAnalyzer$new()
  ridge <- analyzer$ridge_analysis(model, factor_names = c("A", "B"),
                                    maximize = TRUE, n_radii = 10)

  expect_s3_class(ridge, "data.frame")
  expect_true(all(c("Radius", "Lambda", "Predicted") %in% names(ridge)))
  expect_equal(nrow(ridge), 10)
  # First row is the design center (radius 0).
  expect_equal(ridge$Radius[1], 0)
  # All factor columns present.
  expect_true(all(c("A", "B") %in% names(ridge)))
})

# -----------------------------------------------------------------------------
# P2-11: Mixture Designs
# -----------------------------------------------------------------------------

test_that("simplex_centroid produces 2^q - 1 points summing to 1", {
  factors <- lapply(LETTERS[1:3], function(nm) {
    list(name = nm, type = "continuous", levels = c(0, 1))
  })
  plan <- IqrDoePlan$new(task_tag = "mixture",
                          design_type = "simplex_centroid",
                          factors = factors, randomize = FALSE)
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  design <- results$design_info

  expect_equal(nrow(design), 2^3 - 1)            # 7 points
  # Component proportions must sum to 1 for every run.
  expect_equal(rowSums(design[, LETTERS[1:3]]), rep(1, nrow(design)))
  # Three pure-component vertices exist.
  expect_equal(sum(design$PointType == "vertex"), 3)
})

test_that("simplex_lattice {3, 2} produces C(4, 2) = 6 points", {
  factors <- lapply(LETTERS[1:3], function(nm) {
    list(name = nm, type = "continuous", levels = c(0, 1))
  })
  plan <- IqrDoePlan$new(task_tag = "mixture",
                          design_type = "simplex_lattice",
                          factors = factors, alpha = 2, randomize = FALSE)
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  design <- results$design_info

  # {q=3, m=2}: number of points = C(q+m-1, m) = C(4, 2) = 6.
  expect_equal(nrow(design), 6)
  expect_equal(rowSums(design[, LETTERS[1:3]]), rep(1, nrow(design)))
})

test_that("extreme_vertices respects component bounds", {
  # 3 components with bounds: x1 in [0, 0.5], x2 in [0, 0.5], x3 in [0.3, 1].
  factors <- list(
    list(name = "A", type = "continuous", levels = c(0, 0.5)),
    list(name = "B", type = "continuous", levels = c(0, 0.5)),
    list(name = "C", type = "continuous", levels = c(0.3, 1))
  )
  plan <- IqrDoePlan$new(task_tag = "mixture",
                          design_type = "extreme_vertices",
                          factors = factors, randomize = FALSE)
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  design <- results$design_info

  expect_gte(nrow(design), 1)
  # Every vertex must satisfy the bounds and sum to 1.
  expect_true(all(design$A >= 0 - 1e-9 & design$A <= 0.5 + 1e-9))
  expect_true(all(design$B >= 0 - 1e-9 & design$B <= 0.5 + 1e-9))
  expect_true(all(design$C >= 0.3 - 1e-9 & design$C <= 1 + 1e-9))
  expect_equal(rowSums(design[, c("A", "B", "C")]), rep(1, nrow(design)),
              tolerance = 1e-8)
})

test_that("extreme_vertices rejects infeasible bounds", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(0.6, 1)),
    list(name = "B", type = "continuous", levels = c(0.6, 1))
    # sum(lower) = 1.2 > 1 -> infeasible.
  )
  plan <- IqrDoePlan$new(task_tag = "mixture",
                          design_type = "extreme_vertices",
                          factors = factors, randomize = FALSE)
  analyzer <- DoeAnalyzer$new()
  expect_error(analyzer$run(plan = plan), "infeasible")
})

# -----------------------------------------------------------------------------
# P2-12: Split-Plot Design
# -----------------------------------------------------------------------------

test_that("split_plot nests subplot runs within whole plots", {
  factors <- list(
    list(name = "Temp", type = "continuous", levels = c(-1, 1),
         hard_to_change = TRUE),
    list(name = "Time", type = "continuous", levels = c(-1, 1)),
    list(name = "Press", type = "continuous", levels = c(-1, 1))
  )
  plan <- IqrDoePlan$new(task_tag = "split_plot",
                          design_type = "split_plot",
                          factors = factors, randomize = FALSE)
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  design <- results$design_info

  # 2 whole plots x 2x2 subplots = 8 runs.
  expect_equal(nrow(design), 8)
  expect_true("WholePlot" %in% names(design))
  # Whole-plot factor (Temp) constant within each whole plot.
  for (w in unique(design$WholePlot)) {
    sub <- design[design$WholePlot == w, ]
    expect_equal(length(unique(sub$Temp)), 1)
  }
})

# -----------------------------------------------------------------------------
# P2-13: Optimal Design Construction
# -----------------------------------------------------------------------------

test_that("create_optimal_design builds a D-optimal design of target size", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1)),
    list(name = "C", type = "continuous", levels = c(-1, 1))
  )
  analyzer <- DoeAnalyzer$new()
  opt <- analyzer$create_optimal_design(factors = factors, n_runs = 8,
                                         model_order = "interaction",
                                         criterion = "D", n_iter = 5,
                                         n_starts = 3, seed = 123)

  expect_equal(nrow(opt$design), 8)
  expect_equal(opt$criterion_name, "D")
  expect_true(is.finite(opt$criterion))
  expect_true(length(opt$history) >= 1)
  # Model matrix must be full rank for an interaction model with 3 factors
  # (intercept + 3 mains + 3 two-way = 7 columns; 8 runs > 7).
  expect_equal(qr(opt$X)$rank, ncol(opt$X))
})

test_that("create_optimal_design supports A and I criteria", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )
  analyzer <- DoeAnalyzer$new()
  for (crit in c("A", "I")) {
    opt <- analyzer$create_optimal_design(factors = factors, n_runs = 6,
                                           model_order = "main",
                                           criterion = crit, n_iter = 3,
                                           n_starts = 2, seed = 42)
    expect_equal(opt$criterion_name, crit)
    expect_true(is.finite(opt$criterion))
  }
})

# -----------------------------------------------------------------------------
# P2-15: Taguchi Prediction + Dynamic Taguchi
# -----------------------------------------------------------------------------

test_that("predict_optimal picks the best level per factor", {
  # Two control factors (2 levels) x one noise factor (2 levels) giving 2
  # replicates per control combination, so larger-the-better S/N is defined.
  dat <- expand.grid(A = c(1, 2), B = c(1, 2), N = c(-1, 1))
  set.seed(5)
  dat$Y <- 10 + (dat$A - 1) * 4 + (dat$B - 1) * 3 +
    (dat$N) * 0.5 + rnorm(nrow(dat), 0, 0.5)
  ta <- TaguchiAnalyzer$new()
  robust <- ta$analyze_robustness(dat, c("A", "B"), "N", "Y",
                                  sn_type = "larger")
  pred <- ta$predict_optimal(robust, c("A", "B"), sn_type = "larger")

  expect_true(all(c("optimal_levels", "predicted_sn", "ci") %in%
                  names(pred)))
  # Larger-the-better: optimal level is the higher level for both factors.
  expect_equal(unname(pred$optimal_levels["A"]), "2")
  expect_equal(unname(pred$optimal_levels["B"]), "2")
  expect_true(pred$predicted_sn > mean(robust$SN_Ratio))
  expect_length(pred$ci, 2)
})

test_that("compute_dynamic_sn_ratio evaluates input-output linearity", {
  # Perfect proportional relationship y = 2*M should yield a large SN ratio.
  M <- c(1, 2, 3, 4, 5)
  y <- 2 * M
  ta <- TaguchiAnalyzer$new()
  sn <- ta$compute_dynamic_sn_ratio(y, M, type = "zero_point")
  expect_true(is.finite(sn))
  expect_gt(sn, 50)            # near-perfect linearity -> high SN
})

test_that("compute_dynamic_sn_ratio linear model with intercept", {
  M <- c(1, 2, 3, 4, 5)
  y <- 3 + 2 * M + rnorm(5, 0, 0.01)
  ta <- TaguchiAnalyzer$new()
  sn <- ta$compute_dynamic_sn_ratio(y, M, type = "linear")
  expect_true(is.finite(sn))
})

test_that("analyze_dynamic_robustness groups by control combination", {
  # 2 control factors (2 levels each) x 5 signal levels = 20 rows.
  # Use a pure proportional model (y = beta * M, no intercept offset) so the
  # zero-point proportional slope estimate is unbiased.
  dat <- expand.grid(A = c(1, 2), B = c(1, 2), M = c(1, 2, 3, 4, 5))
  dat$Y <- 2 * dat$M
  ta <- TaguchiAnalyzer$new()
  res <- ta$analyze_dynamic_robustness(dat, c("A", "B"), "M", "Y",
                                        type = "zero_point")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4)              # 2 x 2 control combinations
  expect_true(all(c("SN_Ratio", "Slope") %in% names(res)))
  # Slope should be near 2 (the true input-output gain).
  expect_equal(mean(res$Slope), 2, tolerance = 1e-6)
})


# -----------------------------------------------------------------------------
# P3-16: VIF (standalone accessor)
# -----------------------------------------------------------------------------

test_that("compute_vif returns named vector for multi-predictor model", {
  data <- data.frame(
    A = c(-1, 1, -1, 1),
    B = c(-1, -1, 1, 1),
    y = c(10, 12, 14, 16)
  )
  model <- lm(y ~ A + B, data = data)
  analyzer <- DoeAnalyzer$new()
  vif <- analyzer$compute_vif(model)

  expect_type(vif, "double")
  expect_equal(length(vif), 2)
  expect_true(all(c("A", "B") %in% names(vif)))
  # Orthogonal design: VIF should be 1.
  expect_equal(mean(vif), 1, tolerance = 1e-6)
})

test_that("compute_vif flags multicollinearity", {
  data <- data.frame(
    A = c(1, 2, 3, 4, 5),
    C = c(2, 4, 6, 8, 10),   # C = 2*A (perfect collinearity)
    y = c(2, 4, 6, 8, 10)
  )
  model <- lm(y ~ A + C, data = data)
  analyzer <- DoeAnalyzer$new()
  vif <- analyzer$compute_vif(model)
  expect_true(all(vif > 10))
})

test_that("compute_vif returns empty for single-predictor model", {
  data <- data.frame(A = c(-1, 1), y = c(10, 12))
  model <- lm(y ~ A, data = data)
  analyzer <- DoeAnalyzer$new()
  vif <- analyzer$compute_vif(model)
  expect_equal(length(vif), 0)
})


# -----------------------------------------------------------------------------
# P3-17: Wireframe Plot
# -----------------------------------------------------------------------------

test_that("wireframe plot renders for a 2-factor RSM", {
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )
  plan <- IqrDoePlan$new(task_tag = "ccd", design_type = "ccd",
                          factors = factors, center_points = 4,
                          randomize = FALSE, response_vars = "Y")
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  design_data <- results$design_info
  set.seed(11)
  design_data$Y <- 50 + 5 * design_data$A + 3 * design_data$A * design_data$B -
    2 * design_data$A^2 + rnorm(nrow(design_data), 0, 0.5)
  task <- IqrDoeTask$new(plan = plan, data = design_data, theme = "academic")
  task$compute()
  p <- task$plot(type = "wireframe")
  expect_s3_class(p, c("ggplot", "gg"))
})


# -----------------------------------------------------------------------------
# P3-18: Residuals vs Predictors
# -----------------------------------------------------------------------------

test_that("residuals_vs_predictors renders multi-panel plot", {
  factors <- lapply(LETTERS[1:3], function(nm) {
    list(name = nm, type = "continuous", levels = c(-1, 1))
  })
  plan <- IqrDoePlan$new(task_tag = "factorial", design_type = "factorial",
                          factors = factors, randomize = FALSE,
                          response_vars = "Y")
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  design_data <- results$design_info
  set.seed(22)
  design_data$Y <- 10 + 4 * design_data$A - 2 * design_data$B +
    1 * design_data$C + rnorm(nrow(design_data), 0, 0.3)
  task <- IqrDoeTask$new(plan = plan, data = design_data, theme = "academic")
  task$compute()
  p <- task$plot(type = "residuals_vs_predictors")
  expect_true(inherits(p, "patchwork") || inherits(p, c("ggplot", "gg")))
})


# -----------------------------------------------------------------------------
# P3-19: Word / PowerPoint Export
# -----------------------------------------------------------------------------

test_that("export_word produces a .docx file when officer is available", {
  skip_if_not_installed("officer")
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )
  plan <- IqrDoePlan$new(task_tag = "factorial", design_type = "factorial",
                          factors = factors, randomize = FALSE,
                          response_vars = "Y")
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  design_data <- results$design_info
  design_data$Y <- c(10, 12, 14, 16)
  task <- IqrDoeTask$new(plan = plan, data = design_data, theme = "academic")
  task$compute()

  tmp <- tempfile(fileext = ".docx")
  reporter <- DoeReporter$new()
  reporter$export_word(task$results, plan, tmp)
  expect_true(file.exists(tmp))
  expect_true(file.info(tmp)$size > 0)
  unlink(tmp)
})

test_that("export_powerpoint produces a .pptx file when officer is available", {
  skip_if_not_installed("officer")
  factors <- list(
    list(name = "A", type = "continuous", levels = c(-1, 1)),
    list(name = "B", type = "continuous", levels = c(-1, 1))
  )
  plan <- IqrDoePlan$new(task_tag = "factorial", design_type = "factorial",
                          factors = factors, randomize = FALSE,
                          response_vars = "Y")
  analyzer <- DoeAnalyzer$new()
  results <- analyzer$run(plan = plan)
  design_data <- results$design_info
  design_data$Y <- c(10, 12, 14, 16)
  task <- IqrDoeTask$new(plan = plan, data = design_data, theme = "academic")
  task$compute()

  tmp <- tempfile(fileext = ".pptx")
  reporter <- DoeReporter$new()
  reporter$export_powerpoint(task$results, plan, tmp)
  expect_true(file.exists(tmp))
  expect_true(file.info(tmp)$size > 0)
  unlink(tmp)
})


# -----------------------------------------------------------------------------
# P3-20: Botched Runs Handling
# -----------------------------------------------------------------------------

test_that("handle_botched_runs replaces planned levels with actual levels", {
  design <- data.frame(
    A = c(-1, 1, -1, 1),
    B = c(-1, -1, 1, 1),
    Y = c(10, 12, 14, 16)
  )
  # Simulate a botched run: actual A for run 2 is 0.8 instead of 1.
  actual <- data.frame(
    A = c(-1, 0.8, -1, 1),
    B = c(-1, -1, 1, 1),
    Y = c(10, 12, 14, 16)
  )
  analyzer <- DoeAnalyzer$new()
  result <- analyzer$handle_botched_runs(design, actual,
                                          factor_names = c("A", "B"),
                                          response_name = "Y")

  expect_true(is.data.frame(result$design_corrected))
  expect_true("A" %in% result$changed_cols)
  expect_equal(result$design_corrected$A[2], 0.8)
  # The refitted model should be an lm object.
  expect_true(inherits(result$model, "lm"))
})

test_that("handle_botched_runs errors on row count mismatch", {
  design <- data.frame(A = c(-1, 1), B = c(-1, 1))
  actual <- data.frame(A = c(-1, 1, 0))
  analyzer <- DoeAnalyzer$new()
  expect_error(analyzer$handle_botched_runs(design, actual),
               "same number of rows")
})

test_that("handle_botched_runs returns no changed_cols when no deviation", {
  design <- data.frame(A = c(-1, 1), B = c(-1, 1))
  actual <- data.frame(A = c(-1, 1), B = c(-1, 1))
  analyzer <- DoeAnalyzer$new()
  result <- analyzer$handle_botched_runs(design, actual,
                                          factor_names = c("A", "B"))
  expect_equal(length(result$changed_cols), 0)
})
