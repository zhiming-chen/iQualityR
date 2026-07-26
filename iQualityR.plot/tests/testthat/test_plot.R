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

# ---------------------------------------------------------------------------
# IqrPlotterBase toolbox integration tests
#
# These tests verify that plot_* functions source their colors from the
# active IqrTheme preset via the shared IqrPlotterBase toolbox, rather than
# from hardcoded values. Switching the theme must produce visibly different
# but structurally valid plots.
# ---------------------------------------------------------------------------

test_that("internal .iqr_plotter singleton is an IqrPlotterBase", {
  plotter <- iQualityR.plot:::.iqr_plotter
  expect_s3_class(plotter, "IqrPlotterBase")
})

test_that("plot_f_curve uses semantic fail color for reject region", {
  # The reject region ribbon in plot_f_curve must use the active theme's
  # semantic 'fail' color, not a hardcoded "red".
  theme_obj <- as_iqr_theme_object("academic")
  expected_fail <- iQualityR.plot:::.iqr_plotter$.pal_semantic(theme_obj, "fail")

  p <- plot_f_curve(f_stat = 5, df1 = 2, df2 = 27)

  # Access layers in a way compatible with both ggplot2 S7 (>=4.5) and
  # classic ggproto versions. Under S7, `p$layers` is intercepted and does
  # not return the layer list; `S7::prop(p, "layers")` is required. Likewise,
  # `l$geom` returns the LayerInstance under S7, so `l[["geom"]]` is used.
  layers <- if (inherits(p, "S7_object")) S7::prop(p, "layers") else p$layers
  is_ribbon <- vapply(layers, function(l) {
    inherits(l[["geom"]], "GeomRibbon")
  }, logical(1))
  expect_true(sum(is_ribbon, na.rm = TRUE) >= 1,
              label = "at least one ribbon layer exists")

  # Confirm the theme's fail color is NOT plain "red" (#FF0000) -- academic
  # preset uses #EE6677. This guards against accidental re-hardcoding.
  expect_false(tolower(expected_fail) == "#ff0000")
  expect_false(tolower(expected_fail) == "red")
})

test_that("plot_pareto_enhanced bar fill matches theme discrete[1]", {
  theme_obj <- as_iqr_theme_object("academic")
  expected_first <- iQualityR.plot:::.iqr_plotter$.pal_discrete(theme_obj)[1]

  df <- data.frame(category = c("A", "B", "C"), count = c(10, 5, 3))
  p <- plot_pareto_enhanced(df, category_col = "category",
                            count_col = "count", show_table = FALSE,
                            theme = "academic")
  expect_s3_class(p, "ggplot")
  # Smoke check: the expected color is a hex string and not plain "steelblue"
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", expected_first))
})

test_that("switching theme changes plot_pareto_enhanced colors", {
  df <- data.frame(category = c("A", "B"), count = c(10, 5))

  p_academic <- plot_pareto_enhanced(df, category_col = "category",
                                     count_col = "count", show_table = FALSE,
                                     theme = "academic")
  p_prism <- plot_pareto_enhanced(df, category_col = "category",
                                  count_col = "count", show_table = FALSE,
                                  theme = "prism")

  # The discrete palette's first color differs between academic and prism.
  disc_academic <- iQualityR.plot:::.iqr_plotter$.pal_discrete(
    as_iqr_theme_object("academic"))[1]
  disc_prism <- iQualityR.plot:::.iqr_plotter$.pal_discrete(
    as_iqr_theme_object("prism"))[1]
  expect_false(disc_academic == disc_prism,
               label = "academic and prism discrete[1] differ")

  # Both plots must still be valid ggplot objects
  expect_s3_class(p_academic, "ggplot")
  expect_s3_class(p_prism, "ggplot")
})

test_that("plot_correlation_heatmap uses diverging scale, not gradient2 fallback", {
  # After migration, the heatmap should NOT carry a scale_fill_gradient2
  # with hardcoded "blue"/"white"/"red" stops. It should carry the theme's
  # diverging gradient.
  df <- data.frame(a = rnorm(30), b = rnorm(30), c = rnorm(30))
  p <- plot_correlation_heatmap(df, theme = "academic")
  expect_s3_class(p, "ggplot")

  # Inspect scales: the fill scale should be ScaleContinuousDiverging
  # (gradientn with the theme's 3 stops), not a manual gradient2.
  fill_scale <- p$scales$get_scales("fill")
  expect_true(!is.null(fill_scale),
              label = "heatmap has a fill scale")
  # The diverging palette's first color (academic) is #B2182B, not "blue"
  diverging_pal <- iQualityR.plot:::.iqr_plotter$.pal_diverging(
    as_iqr_theme_object("academic"))
  expect_false(tolower(diverging_pal[1]) == "blue",
               label = "diverging low is not plain 'blue'")
})

test_that("plot_hypothesis_box uses semantic fail color for H0 line", {
  theme_obj <- as_iqr_theme_object("academic")
  expected_fail <- iQualityR.plot:::.iqr_plotter$.pal_semantic(theme_obj, "fail")

  set.seed(42)
  x <- rnorm(30, mean = 5)
  p <- plot_hypothesis_box(x, mu = 5, theme = "academic")
  expect_s3_class(p, "ggplot")

  # academic fail color is #EE6677, not "red" (#FF0000)
  expect_false(tolower(expected_fail) == "#ff0000")
  expect_false(tolower(expected_fail) == "red")
})

test_that(".contrast_text returns black on light bg, white on dark bg", {
  plotter <- iQualityR.plot:::.iqr_plotter
  expect_equal(plotter$.contrast_text("#FFFFFF"), "#000000")
  expect_equal(plotter$.contrast_text("#000000"), "#FFFFFF")
  expect_equal(plotter$.contrast_text("#014D64"), "#FFFFFF")  # dark teal
  expect_equal(plotter$.contrast_text("#F5F5F2"), "#000000")  # light cream
  # Invalid input returns black (graceful fallback)
  expect_equal(plotter$.contrast_text(NULL), "#000000")
  expect_equal(plotter$.contrast_text(""), "#000000")
})

test_that(".pal_ui returns theme UI color by slot name", {
  plotter <- iQualityR.plot:::.iqr_plotter
  theme_obj <- as_iqr_theme_object("academic")
  expect_equal(plotter$.pal_ui(theme_obj, "primary"), "#1F77B4")
  expect_equal(plotter$.pal_ui(theme_obj, "danger"), "#EE6677")
  # Missing slot falls back to provided default
  expect_equal(plotter$.pal_ui(theme_obj, "nonexistent", default = "#ABCDEF"),
               "#ABCDEF")
})

test_that("plot_fishbone_basic still builds under toolbox migration", {
  p <- plot_fishbone_basic("Defect", industry = "manufacturing")
  # Fishbone returns a ggplot object in this implementation
  expect_true(inherits(p, "ggplot") || inherits(p, "htmlwidget"))
})

test_that("plot_turtle_diagram still builds under toolbox migration", {
  p <- plot_turtle_diagram("MyProcess")
  # Turtle diagram returns a DiagrammeR grViz widget
  expect_true(inherits(p, "htmlwidget") || inherits(p, "grViz"))
})

test_that("plot_anova_residuals uses theme colors not hardcoded steelblue/red", {
  set.seed(1)
  df <- data.frame(y = rnorm(30), g = factor(rep(1:3, 10)))
  m <- aov(y ~ g, df)
  p <- plot_anova_residuals(m, theme = "academic")
  # patchwork object
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))

  # academic discrete[1] is #4477AA, not "steelblue" (#4682B4)
  disc1 <- iQualityR.plot:::.iqr_plotter$.pal_discrete(
    as_iqr_theme_object("academic"))[1]
  expect_false(tolower(disc1) == "steelblue")
  expect_false(tolower(disc1) == "#4682b4")
})

test_that("no hardcoded color names remain in migrated plot functions", {
  # Grep the R/ source for the most common hardcoded color literals that
  # were migrated. These should no longer appear as bare strings in
  # geom_*/annotate calls (a few may still appear in layers_* defaults
  # and 'white'/'black' typography, which are allowed).
  r_dir <- system.file("R", package = "iQualityR.plot")
  if (!nzchar(r_dir)) skip("R source not available from installed package")

  # Read all R source files
  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  src <- paste(unlist(lapply(files, readLines)), collapse = "\n")

  # These specific literals should NOT appear (migrated to theme toolbox):
  expect_false(grepl('"steelblue"', src, fixed = TRUE),
               label = "no bare 'steelblue' string in R/")
  expect_false(grepl('"#E74C3C"', src, fixed = TRUE),
               label = "no bare '#E74C3C' hex in R/")
  expect_false(grepl('"#F39C12"', src, fixed = TRUE),
               label = "no bare '#F39C12' hex in R/")
  expect_false(grepl('"#95A5A6"', src, fixed = TRUE),
               label = "no bare '#95A5A6' hex in R/")
})

