test_that("unified iqr_msa dispatches to Type1 bias", {
  set.seed(100)
  task <- iqr_msa(
    rnorm(25, mean = 10.1, sd = 0.2),
    study = "type1_bias",
    reference_value = 10,
    lsl = 7,
    usl = 13
  )

  expect_s3_class(task, "Type1Task")
  expect_false(is.null(task$results$statistics))
})

test_that("attribute gage report uses standard reporter path", {
  set.seed(101)
  data <- data.frame(
    eval1 = sample(c("Go", "NoGo"), 30, replace = TRUE),
    eval2 = sample(c("Go", "NoGo"), 30, replace = TRUE)
  )

  task <- iqr_cohen_kappa(data, eval1_col = "eval1", eval2_col = "eval2", theme = "academic")
  expect_false(is.null(task$results))

  xlsx_path <- tempfile(fileext = ".xlsx")
  html_path <- tempfile(fileext = ".html")
  expect_no_error(task$report("excel", path = xlsx_path))
  expect_no_error(suppressWarnings(task$report("html", path = html_path)))
  expect_true(file.exists(xlsx_path))
  expect_true(file.exists(html_path))
})

test_that("gage R&R report uses MSA template through reporter", {
  set.seed(102)
  data <- expand.grid(
    Operator = paste0("O", 1:3),
    Part = paste0("P", 1:6),
    Replication = 1:2
  )
  data$MeasurementValue <- 10 + as.numeric(factor(data$Part)) * 0.2 + rnorm(nrow(data), 0, 0.05)

  task <- gage_rr_study(data, theme = "academic")
  expect_false(is.null(task$statistics))

  html_path <- tempfile(fileext = ".html")
  expect_no_error(suppressWarnings(task$report("html", path = html_path)))
  expect_true(file.exists(html_path))
})

test_that("xbar-r charts facet by operator and annotate control limits", {
  set.seed(103)
  data <- expand.grid(
    Operator = c("A", "B", "C"),
    Part = paste0("P", 1:10),
    Replication = 1:3
  )
  data$MeasurementValue <- 10 +
    as.numeric(factor(data$Part)) * 0.25 +
    c(A = 0, B = 0.04, C = -0.03)[data$Operator] +
    rnorm(nrow(data), 0, 0.04)

  task <- gage_rr_xbar_r(data, theme = "academic")
  xbar_plot <- task$plot("xbar_chart")
  r_plot <- task$plot("r_chart")

  expect_s3_class(xbar_plot, "ggplot")
  expect_s3_class(r_plot, "ggplot")

  xbar_build <- ggplot2::ggplot_build(xbar_plot)
  r_build <- ggplot2::ggplot_build(r_plot)

  expect_equal(length(unique(xbar_build$layout$layout$PANEL)), 3)
  expect_equal(length(unique(r_build$layout$layout$PANEL)), 3)
  xbar_labels <- unlist(lapply(xbar_build$data, function(layer) layer$label %||% character()))
  r_labels <- unlist(lapply(r_build$data, function(layer) layer$label %||% character()))
  expect_true(any(grepl("^UCL=", xbar_labels), na.rm = TRUE))
  # R chart center line is labelled "R=" (R-bar) per Minitab convention,
  # not "CL=". Accept either label for robustness.
  expect_true(any(grepl("^(CL|R)=", r_labels), na.rm = TRUE))
  expect_true(any(grepl("^LCL=", r_labels), na.rm = TRUE))
})

test_that("nested gage R&R keeps nested design and omits interaction plot", {
  set.seed(104)
  plan <- create_msa_plan(
    plan_name = "Nested_GRR",
    objectives = "Nested fixture study",
    operators = c("A", "B", "C"),
    parts = 4,
    measurements = 2,
    method = "nested",
    randomize = FALSE
  )
  data <- plan$get_measurement_sheet()
  data$MeasurementValue <- 10 +
    as.numeric(factor(data$Part)) * 0.15 +
    as.numeric(factor(data$Operator)) * 0.05 +
    rnorm(nrow(data), 0, 0.03)

  task <- gage_rr_study(data, method = "nested", theme = "academic")
  report_results <- task$build_report_results()
  plots <- task$plot("list")

  expect_equal(report_results$design_method, "nested")
  expect_false("interaction" %in% names(plots))
  expect_true(is.null(task$anova_results$with_interaction))
  expect_false(is.null(task$anova_results$without_interaction))
})

test_that("attribute agreement supports five-level ratings with three repeated trials", {
  set.seed(105)
  levels5 <- c("严重", "轻微", "正常", "良好", "优秀")
  samples <- sprintf("S%02d", 1:20)
  appraisers <- c("A", "B", "C")
  trials <- 1:3
  standard <- factor(rep(levels5, length.out = length(samples)), levels = levels5, ordered = TRUE)
  latent <- as.integer(standard)
  app_shift <- c(A = 0, B = 0.18, C = -0.12)

  data <- do.call(rbind, lapply(appraisers, function(app) {
    do.call(rbind, lapply(trials, function(trial) {
      score <- latent + app_shift[[app]] + rnorm(length(samples), 0, 0.55)
      idx <- pmin(pmax(round(score), 1), length(levels5))
      data.frame(
        Sample = samples,
        Appraiser = app,
        Trial = trial,
        Standard = as.character(standard),
        Rating = levels5[idx],
        stringsAsFactors = FALSE
      )
    }))
  }))

  plan <- AttrGagePlan$new(
    plan_name = "Five_Level_Ordinal_Attribute_Study",
    objectives = "Evaluate 5-level sample grading agreement",
    appraisers = appraisers,
    samples = samples,
    standards = as.character(standard),
    categories = levels5,
    trials = 3,
    comparison_mode = "one_way",
    kappa_method = "fleiss",
    conf_level = 0.95
  )
  plan$set_meta(
    "data",
    sample_col = "Sample",
    rater_col = "Appraiser",
    rating_col = "Rating",
    standard_col = "Standard",
    trial_col = "Trial"
  )

  task <- AttrGageTask$new(data = data, plan = plan, theme = "academic", mode = "kappa")
  task$compute()
  r <- task$kappa_results$raw_output

  expect_equal(nrow(data), 180)
  expect_equal(r$n_categories, 5)
  expect_equal(r$n_appraisers, 3)
  expect_equal(r$n_raters, 9)
  expect_equal(nrow(r$pairwise_appraisers), 3)
  expect_equal(nrow(r$appraiser_vs_standard), 4)
  expect_equal(nrow(r$within_appraiser), 9)
  expect_equal(nrow(r$response_table), 5)
  expect_s3_class(task$plot("summary"), "patchwork")
})

test_that("ordinal attribute agreement reports Kendall statistics", {
  set.seed(106)
  levels5 <- c("Severe", "Minor", "Normal", "Good", "Excellent")
  samples <- sprintf("S%02d", 1:20)
  appraisers <- c("A", "B", "C")
  standard <- factor(rep(levels5, length.out = length(samples)), levels = levels5, ordered = TRUE)
  latent <- as.integer(standard)

  data <- do.call(rbind, lapply(appraisers, function(app) {
    do.call(rbind, lapply(1:3, function(trial) {
      score <- latent + c(A = 0, B = 0.2, C = -0.15)[[app]] + stats::rnorm(length(samples), 0, 0.45)
      idx <- pmin(pmax(round(score), 1), length(levels5))
      data.frame(
        Sample = samples,
        Appraiser = app,
        Trial = trial,
        Standard = as.character(standard),
        Rating = levels5[idx],
        stringsAsFactors = FALSE
      )
    }))
  }))

  task <- iqr_fleiss_kappa(
    data,
    sample_col = "Sample",
    rater_col = "Appraiser",
    rating_col = "Rating",
    standard_col = "Standard",
    trial_col = "Trial",
    scale_type = "ordinal",
    category_order = levels5,
    theme = "academic"
  )
  r <- task$kappa_results$raw_output

  expect_equal(r$scale_type, "ordinal")
  expect_equal(r$category_order, levels5)
  expect_false(is.null(r$ordinal))
  expect_equal(nrow(r$ordinal$within_appraiser), 3)
  expect_equal(nrow(r$ordinal$between_appraisers), 1)
  expect_equal(nrow(r$ordinal$appraiser_vs_standard), 3)
  expect_equal(nrow(r$ordinal$all_appraisers_vs_standard), 1)
  expect_true(all(r$ordinal$between_appraisers$Coef >= 0 & r$ordinal$between_appraisers$Coef <= 1, na.rm = TRUE))
  expect_s3_class(task$plot("kendall_ordinal"), "ggplot")

  sheets <- task$build_excel_sheets()
  expect_true("Kendall_Between" %in% names(sheets))
  expect_true("Kendall_vs_Standard" %in% names(sheets))
})

test_that("attribute agreement matches five-level reference kappa calculations", {
  fz_candidates <- c(
    file.path("data", "fz.csv"),
    file.path("..", "data", "fz.csv"),
    file.path("..", "..", "data", "fz.csv"),
    file.path("..", "..", "..", "data", "fz.csv")
  )
  fz_path <- fz_candidates[file.exists(fz_candidates)][1]
  skip_if_not(file.exists(fz_path))

  data <- read.csv(fz_path)
  task <- iqr_fleiss_kappa(
    data,
    sample_col = "Sample",
    rater_col = "Appraiser",
    rating_col = "Rating",
    standard_col = "Standard",
    trial_col = "Trial",
    scale_type = "ordinal",
    category_order = 1:5,
    theme = "academic"
  )
  r <- task$kappa_results$raw_output

  expect_equal(r$kappa, 0.8817048, tolerance = 1e-6)
  expect_equal(r$se, 0.01343615, tolerance = 1e-6)

  all_vs_standard <- r$appraiser_vs_standard[
    r$appraiser_vs_standard$Comparison == "All Appraisers vs Standard",
  ]
  expect_equal(all_vs_standard$N, 50)
  expect_equal(all_vs_standard$Kappa, 0.9120821, tolerance = 1e-6)
  expect_equal(all_vs_standard$SE_Kappa, 0.02517048, tolerance = 1e-6)

  expect_equal(nrow(r$response_kappa), 5)
  expect_equal(r$response_kappa$Kappa, c(0.9543917, 0.8276942, 0.7725406, 0.8911268, 0.9681478), tolerance = 1e-6)
  expect_equal(r$response_kappa_vs_standard$Kappa, c(0.9778966, 0.8490684, 0.8149923, 0.9445803, 0.9837557), tolerance = 1e-6)

  sheets <- task$build_excel_sheets()
  expect_true("Response_Kappa" %in% names(sheets))
  expect_true("Response_Kappa_vs_Standard" %in% names(sheets))
})
