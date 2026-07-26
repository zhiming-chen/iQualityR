# Tests for iQualityR.plot package

test_that("as_iqr_theme returns a theme object", {
  thm <- as_iqr_theme("workbench")
  expect_s3_class(thm, "theme")
})

test_that("as_iqr_theme_object returns IqrTheme", {
  obj <- as_iqr_theme_object("workbench")
  expect_s3_class(obj, "IqrTheme")
})

test_that("as_iqr_theme_object handles NULL", {
  obj <- as_iqr_theme_object(NULL)
  expect_s3_class(obj, "IqrTheme")
})

test_that("base_plot creates ggplot object", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  p <- base_plot(df, ggplot2::aes(x = x, y = y))
  expect_s3_class(p, "ggplot")
})

test_that("base_plot applies theme", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- base_plot(df, ggplot2::aes(x = x, y = y), theme = "academic")
  expect_s3_class(p, "ggplot")
})

test_that("set_default_theme sets option", {
  set_default_theme("workbench")
  expect_equal(getOption("iqr.default_theme"), "workbench")
})

test_that("plot_pp creates ggplot object", {
  df <- data.frame(val = rnorm(50))
  p <- plot_pp(df, "val", dist_family = "norm", add_test = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_qq creates ggplot object", {
  df <- data.frame(val = rnorm(50))
  p <- plot_qq(df, "val", dist_family = "norm", add_test = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_acf creates ggplot object", {
  vec <- rnorm(100)
  p <- plot_acf(vec)
  expect_s3_class(p, "ggplot")
})

test_that("plot_pacf creates ggplot object", {
  vec <- rnorm(100)
  p <- plot_pacf(vec)
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter_basic creates ggplot object", {
  df <- data.frame(x = rnorm(30), y = rnorm(30))
  p <- plot_scatter_basic(df, "x", "y", add_regression = FALSE, add_correlation = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter_grouped creates ggplot object", {
  df <- data.frame(
    x = rnorm(40),
    y = rnorm(40),
    g = rep(c("A", "B"), each = 20)
  )
  p <- plot_scatter_grouped(df, "x", "y", "g",
                            add_regression = FALSE,
                            add_correlation = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_scatter_bubble creates ggplot object", {
  df <- data.frame(x = rnorm(20), y = rnorm(20), z = runif(20, 1, 10))
  p <- plot_scatter_bubble(df, "x", "y", "z")
  expect_s3_class(p, "ggplot")
})

test_that("plot_pareto_enhanced creates ggplot object from named vector", {
  counts <- c(A = 10, B = 8, C = 5, D = 3)
  p <- plot_pareto_enhanced(counts, show_table = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("quick_pareto creates ggplot object", {
  counts <- c(X = 10, Y = 5, Z = 3)
  p <- quick_pareto(counts)
  expect_s3_class(p, "ggplot")
})

test_that("layers_histogram_density returns list of layers", {
  layers <- layers_histogram_density(bins = 20)
  expect_type(layers, "list")
  expect_length(layers, 2)
})

test_that("layers_boxplot returns list of layers", {
  layers <- layers_boxplot(add_jitter = TRUE)
  expect_type(layers, "list")
  expect_length(layers, 2)
})

test_that("layers_qq returns list of layers", {
  layers <- layers_qq(distribution = "norm")
  expect_type(layers, "list")
  expect_length(layers, 2)
})

test_that("layers_trend_line returns list of layers", {
  layers <- layers_trend_line(add_points = TRUE)
  expect_type(layers, "list")
})

test_that("layers_violin returns list of layers", {
  layers <- layers_violin(add_boxplot = TRUE)
  expect_type(layers, "list")
  expect_length(layers, 2)
})

test_that("layers_spec_limits returns list of layers", {
  layers <- layers_spec_limits(lsl = 10, usl = 20)
  expect_type(layers, "list")
  expect_length(layers, 4)
})

test_that("layers_control_chart returns list of layers", {
  df <- data.frame(
    x = 1:10,
    y = rnorm(10),
    cl = 0,
    lcl = -3,
    ucl = 3
  )
  layers <- layers_control_chart(df)
  expect_type(layers, "list")
})

test_that("plot_correlation_heatmap creates ggplot object", {
  df <- data.frame(a = rnorm(30), b = rnorm(30), c = rnorm(30))
  p <- plot_correlation_heatmap(df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_variance_components creates ggplot object", {
  df <- data.frame(
    source = c("A", "B", "C"),
    variance_percent = c(50, 30, 20)
  )
  p <- plot_variance_components(df)
  expect_s3_class(p, "ggplot")
})

test_that("combine_plots combines multiple plots", {
  df <- data.frame(x = 1:5, y = 1:5)
  p1 <- base_plot(df, ggplot2::aes(x = x, y = y))
  p2 <- base_plot(df, ggplot2::aes(x = x, y = y))
  combined <- combine_plots(p1, p2, ncol = 2)
  expect_s3_class(combined, "patchwork")
})
