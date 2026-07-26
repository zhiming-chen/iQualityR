# Tests for the iQualityR.sampling package

test_that("SamplingPlan initializes correctly for single sampling", {
  plan <- SamplingPlan$new(
    task_tag = "sampling_single",
    sample_size = 50,
    acceptance_number = 1,
    sampling_type = "single",
    aql = 0.01,
    rql = 0.05
  )
  expect_equal(plan$sample_size, 50)
  expect_equal(plan$acceptance_number, 1)
  expect_equal(plan$sampling_type, "single")
  expect_equal(plan$aql, 0.01)
  expect_equal(plan$rql, 0.05)
  expect_false(plan$is_multistage())
})

test_that("SamplingPlan validates AQL < RQL", {
  expect_error(
    SamplingPlan$new(
      sample_size = 50,
      acceptance_number = 1,
      aql = 0.10,
      rql = 0.05
    ),
    class = "simpleError"
  )
})

test_that("SamplingPlan rejects invalid probability values", {
  expect_error(
    SamplingPlan$new(
      sample_size = 50,
      acceptance_number = 1,
      aql = -0.1
    ),
    class = "simpleError"
  )
  expect_error(
    SamplingPlan$new(
      sample_size = 50,
      acceptance_number = 1,
      rql = 1.5
    ),
    class = "simpleError"
  )
})

test_that("SamplingPlan rejects invalid sampling_type", {
  expect_error(
    SamplingPlan$new(
      sample_size = 50,
      acceptance_number = 1,
      sampling_type = "bogus"
    ),
    class = "simpleError"
  )
})

test_that("SamplingPlan catches field tampering in validate()", {
  plan <- SamplingPlan$new(
    sample_size = 50,
    acceptance_number = 1,
    sampling_type = "single"
  )
  plan$sampling_type <- "bogus"
  expect_error(plan$validate(), class = "simpleError")
})

test_that("SamplingPlan accepts double-sampling stage plans", {
  plan <- SamplingPlan$new(
    sampling_type = "double",
    aql = 0.01,
    rql = 0.10,
    stage_plans = list(
      list(n = 32, c = 0, r = 2),
      list(n = 32, c = 1)
    ),
    sample_size = 32,
    acceptance_number = 1
  )
  expect_true(plan$is_multistage())
  expect_equal(length(plan$stage_plans), 2)
})

test_that("SamplingPlan rejects malformed stage plans", {
  expect_error(
    SamplingPlan$new(
      sampling_type = "double",
      stage_plans = list(
        list(n = 32),
        list(n = 32, c = 1)
      )
    ),
    class = "simpleError"
  )
})

test_that("SamplingAnalyzer inherits from IqrAnalyzerBase", {
  analyzer <- SamplingAnalyzer$new()
  expect_true(inherits(analyzer, "IqrAnalyzerBase"))
  expect_true(inherits(analyzer, "SamplingAnalyzer"))
})

test_that("SamplingAnalyzer computes OC curve for single sampling", {
  plan <- SamplingPlan$new(
    sample_size = 50,
    acceptance_number = 1,
    sampling_type = "single"
  )
  analyzer <- SamplingAnalyzer$new()
  results <- analyzer$analyze(data = NULL, plan = plan)

  expect_type(results$oc_curve, "list")
  expect_length(results$oc_curve$p_values, 200)
  expect_length(results$oc_curve$acceptance_probabilities, 200)
  # At p = 0, acceptance probability should be 1
  expect_equal(results$oc_curve$acceptance_probabilities[1], 1)
  # At p_max, acceptance probability should be small
  expect_lt(results$oc_curve$acceptance_probabilities[200], 0.5)
})

test_that("SamplingAnalyzer computes risk analysis", {
  plan <- SamplingPlan$new(
    sample_size = 50,
    acceptance_number = 1,
    aql = 0.01,
    rql = 0.10
  )
  analyzer <- SamplingAnalyzer$new()
  results <- analyzer$analyze(data = NULL, plan = plan)

  expect_true(results$risk_analysis$producer_risk >= 0)
  expect_true(results$risk_analysis$producer_risk <= 1)
  expect_true(results$risk_analysis$consumer_risk >= 0)
  expect_true(results$risk_analysis$consumer_risk <= 1)
})

test_that("SamplingAnalyzer computes power analysis for single sampling", {
  plan <- SamplingPlan$new(
    sample_size = 50,
    acceptance_number = 1,
    sampling_type = "single"
  )
  analyzer <- SamplingAnalyzer$new()
  results <- analyzer$analyze(data = NULL, plan = plan)

  expect_false(is.null(results$power_analysis))
  expect_true(results$power_analysis$achieved_power >= 0)
  expect_true(results$power_analysis$achieved_power <= 1)
})

test_that("SamplingAnalyzer returns NULL power for double sampling", {
  plan <- SamplingPlan$new(
    sampling_type = "double",
    aql = 0.01,
    rql = 0.10,
    stage_plans = list(
      list(n = 32, c = 0, r = 2),
      list(n = 32, c = 1)
    ),
    sample_size = 32,
    acceptance_number = 1
  )
  analyzer <- SamplingAnalyzer$new()
  results <- analyzer$analyze(data = NULL, plan = plan)

  expect_null(results$power_analysis)
  expect_false(is.null(results$asn_curve))
})

test_that("SamplingAnalyzer rejects non-SamplingPlan input", {
  analyzer <- SamplingAnalyzer$new()
  expect_error(analyzer$analyze(data = NULL, plan = list()),
               class = "simpleError")
})

test_that("SamplingAnalyzer performs actual sampling on real data", {
  withr::local_seed(42)
  fake_data <- data.frame(
    quality_status = sample(c("good", "defective"), 200,
                            replace = TRUE, prob = c(0.95, 0.05))
  )
  plan <- SamplingPlan$new(
    sample_size = 50,
    acceptance_number = 1,
    sampling_type = "single"
  )
  analyzer <- SamplingAnalyzer$new()
  results <- analyzer$analyze(data = fake_data, plan = plan)

  expect_false(is.null(results$actual_sampling))
  expect_equal(results$actual_sampling$n_sampled, 50)
})

test_that("SamplingPlotter generates a ggplot object", {
  plan <- SamplingPlan$new(
    sample_size = 50,
    acceptance_number = 1,
    sampling_type = "single"
  )
  analyzer <- SamplingAnalyzer$new()
  results <- analyzer$analyze(data = NULL, plan = plan)

  plotter <- SamplingPlotter$new()
  theme_obj <- iQualityR.core::IqrTheme$new("academic")

  p <- plotter$render(results, theme_obj, type = "oc")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("SamplingPlotter returns empty plot for NULL results", {
  plotter <- SamplingPlotter$new()
  theme_obj <- iQualityR.core::IqrTheme$new("academic")
  p <- plotter$render(NULL, theme_obj, type = "full")
  expect_true(inherits(p, "ggplot"))
})

test_that("SamplingReporter prints to console without error", {
  plan <- SamplingPlan$new(
    sample_size = 50,
    acceptance_number = 1,
    sampling_type = "single"
  )
  analyzer <- SamplingAnalyzer$new()
  results <- analyzer$analyze(data = NULL, plan = plan)

  reporter <- SamplingReporter$new()
  # print_console() deliberately prints to stdout; just verify no error.
  expect_no_error(reporter$print_console(results, plan))
})

test_that("SamplingReporter converts results to a data frame", {
  plan <- SamplingPlan$new(
    sample_size = 50,
    acceptance_number = 1,
    sampling_type = "single"
  )
  analyzer <- SamplingAnalyzer$new()
  results <- analyzer$analyze(data = NULL, plan = plan)

  reporter <- SamplingReporter$new()
  df <- reporter$to_dataframe(results)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
})

test_that("SamplingReporter Excel export skips without openxlsx", {
  skip_if_not_installed("openxlsx")
  plan <- SamplingPlan$new(
    sample_size = 50,
    acceptance_number = 1,
    sampling_type = "single"
  )
  analyzer <- SamplingAnalyzer$new()
  results <- analyzer$analyze(data = NULL, plan = plan)

  reporter <- SamplingReporter$new()
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(reporter$to_excel(results, tmp))
  expect_true(file.exists(tmp))
})

test_that("IqrSamplingTask runs end-to-end", {
  plan <- SamplingPlan$new(
    sample_size = 50,
    acceptance_number = 1,
    sampling_type = "single",
    aql = 0.01,
    rql = 0.05
  )
  task <- IqrSamplingTask$new(data = NULL, plan = plan, theme = "academic")
  expect_silent(task$compute())
  expect_false(is.null(task$results))

  # summary() deliberately prints to console; just verify no error.
  expect_no_error(task$summary())

  p <- task$plot(type = "oc")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("convenience functions work", {
  task <- sampling_single(
    sample_size = 50,
    acceptance_number = 1,
    aql = 0.01,
    rql = 0.05
  )
  expect_true(inherits(task, "IqrSamplingTask"))
  expect_false(is.null(task$results))
})

test_that("double sampling convenience function works", {
  task <- sampling_double(
    stage1 = list(n = 32, c = 0, r = 2),
    stage2 = list(n = 32, c = 1)
  )
  expect_true(inherits(task, "IqrSamplingTask"))
  expect_false(is.null(task$results))
  expect_false(is.null(task$results$asn_curve))
})

test_that("multiple sampling convenience function works", {
  task <- sampling_multiple(stages = list(
    list(n = 20, c = 0),
    list(n = 20, c = 1),
    list(n = 20, c = 2)
  ))
  expect_true(inherits(task, "IqrSamplingTask"))
  expect_false(is.null(task$results))
})
