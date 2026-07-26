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
  # After set_theme("workbench"), primary must be workbench's primary (#2563EB),
  # not academic's (#1F77B4).
  expect_equal(theme$get_ui_colors()$primary, "#2563EB")
})

test_that("Four palette types are exposed by every preset", {
  for (style in c("academic", "workbench", "tech", "economist", "wsj",
                  "gdocs", "tufte", "few", "solarized", "prism")) {
    theme <- IqrTheme$new(style)
    for (ptype in c("discrete", "sequential", "diverging", "semantic")) {
      pal <- theme$get_pal(ptype)
      expect_type(pal, "character")
      expect_true(length(pal) >= 1L,
                  label = paste(style, ptype, "has length >= 1"))
    }
  }
})

test_that("External theme presets carry distinct palettes (not academic)", {
  # Each external theme must ship its own discrete palette, distinct from
  # academic's Paul Tol palette -- the whole point of method B is that
  # picking "economist" recolors plots to match the source publication.
  academic_pal <- IqrTheme$new("academic")$get_pal("discrete")

  for (style in c("economist", "wsj", "gdocs", "tufte", "few",
                  "solarized", "prism")) {
    pal <- IqrTheme$new(style)$get_pal("discrete")
    # At least the first color must differ from academic's #4477AA
    expect_false(pal[1] == academic_pal[1],
                 label = paste(style, "first color is not academic #4477AA"))
    # Palette length is 10 (matches the standard preset contract)
    expect_length(pal, 10L)
  }
})

test_that("External theme presets expose UI primary distinct from academic", {
  academic_primary <- IqrTheme$new("academic")$get_ui_colors()$primary
  for (style in c("economist", "wsj", "gdocs", "tufte", "few",
                  "solarized", "prism")) {
    primary <- IqrTheme$new(style)$get_ui_colors()$primary
    expect_false(primary == academic_primary,
                 label = paste(style, "UI primary is not academic #1F77B4"))
  }
})

test_that("External theme presets carry semantic palettes with pass/fail/watch", {
  for (style in c("economist", "wsj", "gdocs", "tufte", "few",
                  "solarized", "prism")) {
    theme <- IqrTheme$new(style)
    full <- theme$get_pal("semantic")
    expect_true(all(c("pass", "fail", "watch") %in% names(full)),
                label = paste(style, "semantic has pass/fail/watch"))
    # pass and fail must be different colors
    expect_false(full[["pass"]] == full[["fail"]],
                 label = paste(style, "pass and fail differ"))
  }
})

test_that("External theme function is attached when package is installed", {
  # Skip silently when neither external package is available
  skip_if_not_installed("ggthemes")
  theme <- IqrTheme$new("economist")
  expect_false(is.null(theme$config$external_theme_fun))
  # theme_iqr() must still produce a valid ggplot2 theme
  expect_s3_class(theme$theme_iqr(), "theme")
})

test_that("External theme preset works even when package is absent", {
  # Simulate the package being unavailable by constructing the preset
  # directly through the style_presets dictionary. The preset must still
  # resolve colors; only the theme function is skipped.
  theme <- IqrTheme$new("academic")
  # Force-select the economist preset entry without going through the
  # external_map path
  preset <- theme$config$style_presets[["economist"]]
  expect_type(preset, "list")
  expect_false(is.null(preset$data$discrete))
  expect_false(is.null(preset$ui$primary))
})

test_that("Discrete palette auto-extends beyond base length", {
  theme <- IqrTheme$new("academic")
  base <- theme$get_pal("discrete")
  expect_length(base, 10L)

  extended <- theme$get_pal("discrete", n = 15L)
  expect_length(extended, 15L)
  # colorRampPalette interpolates, so the first color is preserved
  expect_equal(extended[1], base[1])
  expect_equal(extended[15], base[10])
})

test_that("Continuous is a legacy alias for sequential", {
  theme <- IqrTheme$new("academic")
  expect_equal(theme$get_pal("continuous"),
               theme$get_pal("sequential"))
  expect_equal(theme$get_data_colors("continuous"),
               theme$get_pal("sequential"))
})

test_that("Semantic palette returns named vector or single color by name", {
  theme <- IqrTheme$new("academic")
  full <- theme$get_pal("semantic")
  expect_type(full, "character")
  expect_true(all(c("pass", "fail", "watch") %in% names(full)))

  expect_equal(theme$get_pal("semantic", name = "pass"),
               full[["pass"]])
  expect_error(theme$get_pal("semantic", name = "nope"),
               "Unknown semantic color name")
})

test_that("Palette resolution priority: custom > option > preset", {
  theme <- IqrTheme$new("academic")
  preset <- theme$get_pal("discrete")

  # custom overrides preset
  expect_equal(theme$get_pal("discrete", custom = c("#000000")),
               c("#000000"))

  # option overrides preset (but custom wins over option)
  op <- options(iqr.custom_discrete = c("#111111", "#222222"))
  on.exit(options(op), add = TRUE)
  expect_equal(theme$get_pal("discrete"), c("#111111", "#222222"))
  expect_equal(theme$get_pal("discrete", custom = c("#333333")),
               c("#333333"))
})

test_that("style = 'paired' produces a working color scale", {
  theme <- IqrTheme$new("academic")
  fill_pal <- theme$get_pal("discrete")

  color_scale <- theme$plot$scale_color_iqr(discrete = TRUE, style = "paired")
  expect_s3_class(color_scale, "ScaleDiscrete")

  # Calling the palette function should return a color vector of the right length
  color_out <- color_scale$palette(length(fill_pal))
  expect_length(color_out, length(fill_pal))

  # With style = "same" (default), color matches fill
  same_scale <- theme$plot$scale_color_iqr(discrete = TRUE, style = "same")
  same_out <- same_scale$palette(length(fill_pal))
  expect_equal(same_out, fill_pal)
})

test_that("scale_fill_sequential / scale_color_sequential return ScaleContinuous", {
  theme <- IqrTheme$new("academic")
  expect_s3_class(theme$plot$scale_fill_sequential(), "ScaleContinuous")
  expect_s3_class(theme$plot$scale_color_sequential(), "ScaleContinuous")
})

test_that("scale_fill_diverging / scale_color_diverging return ScaleContinuous", {
  theme <- IqrTheme$new("academic")
  expect_s3_class(theme$plot$scale_fill_diverging(), "ScaleContinuous")
  expect_s3_class(theme$plot$scale_color_diverging(), "ScaleContinuous")
})

test_that("scale_fill_semantic / scale_color_semantic bind semantic colors", {
  theme <- IqrTheme$new("academic")
  fill_scale <- theme$plot$scale_fill_semantic(labels = c("pass", "fail"))
  expect_s3_class(fill_scale, "ScaleDiscrete")

  pal <- theme$get_pal("semantic")
  # The palette function should return the bound semantic colors
  out <- fill_scale$palette(2)
  expect_length(out, 2)

  expect_error(theme$plot$scale_fill_semantic(labels = "nope"),
               "Unknown semantic labels")
})

test_that("IqrTheme exposes new scale_* delegates", {
  theme <- IqrTheme$new("academic")
  expect_s3_class(theme$scale_fill_sequential(), "ScaleContinuous")
  expect_s3_class(theme$scale_color_sequential(), "ScaleContinuous")
  expect_s3_class(theme$scale_fill_diverging(), "ScaleContinuous")
  expect_s3_class(theme$scale_color_diverging(), "ScaleContinuous")
  expect_s3_class(theme$scale_fill_semantic(), "ScaleDiscrete")
  expect_s3_class(theme$scale_color_semantic(), "ScaleDiscrete")
})

test_that("Color utility functions work", {
  expect_equal(lighten("#000000", 1), "#FFFFFF")
  expect_equal(lighten("#000000", 0), "#000000")
  expect_equal(darken("#FFFFFF", 1), "#000000")
  expect_equal(darken("#FFFFFF", 0), "#FFFFFF")
  expect_equal(mix("#000000", "#FFFFFF", 0.5),
               "#808080")
  expect_true(is_dark("#000000"))
  expect_false(is_dark("#FFFFFF"))
  expect_equal(contrast_ratio("#FFFFFF", "#FFFFFF"), 1)
  expect_equal(contrast_ratio("#FFFFFF", "#000000"), 21, tolerance = 0.1)
  expect_error(lighten("#000000", 2), "amount must be")
  expect_error(darken("#000000", -1), "amount must be")
  expect_error(mix("#000000", "#FFFFFF", 2), "amount must be")
  expect_error(hex_to_rgb("#ZZZ"), "Invalid hex color")
})

test_that("IqrPlotterBase exposes palette and scale toolbox", {
  plotter <- IqrPlotterBase$new()
  theme <- IqrTheme$new("academic")

  # palette accessors
  expect_length(plotter$.pal_discrete(theme), 10L)
  expect_length(plotter$.pal_discrete(theme, n = 12L), 12L)
  expect_length(plotter$.pal_sequential(theme), 3L)
  expect_length(plotter$.pal_diverging(theme), 3L)
  expect_true(is.character(plotter$.pal_semantic(theme)))
  expect_true(is.character(plotter$.pal_semantic(theme, name = "pass")))

  # scale factories
  expect_s3_class(plotter$.scale_fill_discrete(theme), "ScaleDiscrete")
  expect_s3_class(plotter$.scale_color_discrete(theme), "ScaleDiscrete")
  expect_s3_class(plotter$.scale_fill_sequential(theme), "ScaleContinuous")
  expect_s3_class(plotter$.scale_color_sequential(theme), "ScaleContinuous")
  expect_s3_class(plotter$.scale_fill_diverging(theme), "ScaleContinuous")
  expect_s3_class(plotter$.scale_color_diverging(theme), "ScaleContinuous")
  expect_s3_class(plotter$.scale_fill_semantic(theme), "ScaleDiscrete")
  expect_s3_class(plotter$.scale_color_semantic(theme), "ScaleDiscrete")

  # paired fill/color returns a list of two scales
  paired <- plotter$.scale_fill_color_paired(theme)
  expect_type(paired, "list")
  expect_s3_class(paired$fill, "ScaleDiscrete")
  expect_s3_class(paired$color, "ScaleDiscrete")

  # base render() still raises
  expect_error(plotter$render(list(), theme), "Not implemented")
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
