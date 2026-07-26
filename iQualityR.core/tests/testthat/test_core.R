test_that("IqrPlanBase initialization and methods work", {
  plan <- IqrPlanBase$new(task_tag = "test", conf_level = 0.99)

  expect_equal(plan$task_tag, "test")
  expect_equal(plan$conf_level, 0.99)
  expect_type(plan$meta_data, "list")
  expect_type(plan$stats_params, "list")
  expect_type(plan$criteria, "list")

  plan$set_criteria(cpk = 1.33, ppk = 1.0)
  expect_equal(plan$criteria$cpk, 1.33)
  expect_equal(plan$criteria$ppk, 1.0)

  plan$set_meta("man", operator = "John", experience = 5)
  expect_equal(plan$meta_data$man$operator, "John")
  expect_equal(plan$meta_data$man$experience, 5)

  expect_silent(plan$validate())

  plan_list <- plan$to_list()
  expect_type(plan_list, "list")
  expect_equal(plan_list$task_tag, "test")
  expect_equal(plan_list$conf_level, 0.99)
})

test_that("IqrTaskBase initialization works", {
  data <- data.frame(x = 1:10, y = rnorm(10))
  task <- IqrTaskBase$new(data, theme = "academic")

  expect_s3_class(task$data, "data.frame")
  expect_null(task$results)
  expect_type(task$executor, "list")

  expect_error(task$compute())
  expect_error(task$plot())
  expect_error(task$report())
})

test_that("IqrAnalyzerBase initialization and methods work", {
  analyzer <- IqrAnalyzerBase$new()

  expect_type(analyzer$results, "list")
  expect_type(analyzer$params, "list")

  analyzer$set_statistic("mean", 10)
  analyzer$reset()
  expect_equal(length(analyzer$results$statistics), 0)

  analyzer$set_statistic("mean", 10)
  analyzer$set_diagnostic("normality", "normal")
  analyzer$set_datatable("data", data.frame(x = 1:5))
  analyzer$set_raw_output(list(a = 1, b = 2))

  expect_equal(analyzer$results$statistics$mean, 10)
  expect_equal(analyzer$results$diagnostics$normality, "normal")
  expect_s3_class(analyzer$results$data_tables$data, "data.frame")
  expect_type(analyzer$results$raw_output, "list")

  analyzer$setup(list(alpha = 0.05, method = "t-test"))
  expect_equal(analyzer$params$alpha, 0.05)
  expect_equal(analyzer$params$method, "t-test")

  results <- analyzer$get_results()
  expect_type(results, "list")
})

test_that("IqrAnalyzerBase$run validates inputs and dispatches to .run_logic", {
  analyzer <- IqrAnalyzerBase$new()

  # Base class has no .run_logic implementation — calling run() should surface that.
  expect_error(analyzer$run(data.frame(x = 1:3)), "Subclass")

  # Input validation: null/missing data must be rejected before reaching .run_logic.
  expect_error(analyzer$run(), "Data required")
  expect_error(analyzer$run(NULL), "Data required")
})

test_that("IqrTheme initialization and methods work", {
  theme <- IqrTheme$new(theme_style = "academic")

  expect_silent(theme$set_theme("workbench"))

  pal <- theme$get_pal("discrete")
  expect_type(pal, "character")

  expect_s3_class(theme$scale_fill_iqr(), "ScaleDiscrete")
  expect_s3_class(theme$scale_color_iqr(), "ScaleDiscrete")

  expect_s3_class(theme$theme_iqr(), "theme")
  expect_equal(theme$get_ui_colors()$primary, "#2563EB")
})

test_that("i18n helpers translate and interpolate messages", {
  expect_equal(iqr_locale("zh-CN"), "zh-CN")
  expect_equal(iqr_locale("bad-locale"), "en-US")

  expect_equal(
    iqr_t("common.missing_required_column", locale = "en-US", column = "MeasurementValue"),
    "Required column 'MeasurementValue' was not found."
  )
  expect_equal(
    iqr_t("common.missing_required_column", locale = "zh-CN", column = "MeasurementValue"),
    "未找到必需列 'MeasurementValue'。"
  )
})

test_that("app result helpers return stable structures", {
  result <- app_success(data = list(n = 10))

  expect_true(result$ok)
  expect_equal(result$code, "success")
  expect_equal(result$data$n, 10)

  error <- app_error("missing_required_column", "Missing column.")
  expect_false(error$ok)
  expect_equal(error$code, "missing_required_column")
})

test_that("validate_output_dir creates and validates directory paths", {
  tmp <- tempfile("iqr_test_dir_")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path <- validate_output_dir(tmp, create = TRUE)
  expect_true(dir.exists(path))

  expect_error(validate_output_dir(NULL), "output_dir")
  expect_error(validate_output_dir(""), "output_dir")

  missing <- tempfile("iqr_missing_")
  expect_error(validate_output_dir(missing, create = FALSE), "does not exist")
})

test_that("ExcelExporter initialization and theme methods work", {
  theme <- IqrTheme$new(theme_style = "academic")
  exporter <- ExcelExporter$new(theme$config)

  expect_silent(exporter$set_excel_theme(title = "#FF0000"))
  expect_silent(exporter$reset_excel_theme())

  tmp <- tempfile("iqr_excel_", fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  sheets <- list(Summary = data.frame(stat = "mean", value = 10))
  exporter$export_excel(sheets, path = tmp, title = "Test Report")
  expect_true(file.exists(tmp))
})

test_that("IqrReporter initialization works", {
  theme <- IqrTheme$new(theme_style = "academic")
  reporter <- IqrReporter$new(theme)

  expect_silent(reporter$register("test", rmd_template = "test.Rmd"))
  expect_true("test" %in% names(reporter$templates))
})

test_that("Utility functions work", {
  metadata <- validate_metadata(list(man = list(name = "John")))
  expect_type(metadata, "list")
  expect_true("machine" %in% names(metadata))

  data <- data.frame(x = 1:10, y = rnorm(10))
  expect_true(validate_inputs(data, c("x", "y")))
  expect_error(validate_inputs(data, c("x", "z")), "Missing")

  error_msg <- create_error_message("Test error", "input")
  expect_equal(error_msg, "[INPUT] Test error")

  expect_null(get_config("non_existent"))
  expect_equal(get_config("non_existent", "default"), "default")

  ids <- generate_anon_id(5)
  expect_type(ids, "character")
  expect_length(ids, 5)
  expect_true(length(unique(ids)) == 5)

  data_vec <- 1:10
  mr_stats <- moving_range_stats(data_vec, 2)
  expect_type(mr_stats, "list")
  expect_true(all(c("mr", "mr_bar", "mr_median") %in% names(mr_stats)))

  p_values <- c(0.0001, 0.005, 0.03, 0.07, 0.5)
  formatted <- format_p_value(p_values)
  expect_type(formatted, "character")
  expect_equal(formatted[[1]], "***")
  expect_equal(formatted[[2]], "**")
  expect_equal(formatted[[3]], "*")
  expect_equal(formatted[[4]], ".")
  expect_match(formatted[[5]], "0\\.500")

  numbers <- c(0.0001, 1, 1000)
  formatted2 <- format_scientific(numbers)
  expect_type(formatted2, "character")

  expect_equal(safe_tolerance(10, 5), 5)
  expect_null(safe_tolerance(10, NULL))

  expect_equal(null_to_na(NULL), NA)
  expect_equal(null_to_na(5), 5)

  tasks <- c("capability", "doe")
  registry <- create_task_registry(tasks)
  expect_type(registry, "list")
  expect_true("capability" %in% names(registry))
  expect_true("doe" %in% names(registry))
})
