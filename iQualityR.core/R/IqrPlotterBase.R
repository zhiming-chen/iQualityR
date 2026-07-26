#' @title iQualityR Plotter Base Class
#'
#' @description
#' Abstract plotter base class. Defines the interface for rendering plots and
#' provides a toolbox of palette / scale accessors that subclasses inherit so
#' they do not have to know the internals of \code{IqrTheme}.
#'
#' Subclasses typically override \code{render()} and call the \code{.pal_*} /
#' \code{.scale_*} helpers from within. This keeps downstream plotters
#' decoupled from the theme implementation: switching the active theme at the
#' task level is enough to recolor every plot consistently.
#'
#' @importFrom R6 R6Class
#' @export
IqrPlotterBase <- R6::R6Class("IqrPlotterBase",
  public = list(
    #' @description Render plot (virtual method, must be implemented by subclass).
    #' @param results Analysis results (list from Analyzer).
    #' @param theme_obj IqrTheme object.
    #' @param type Character. Plot type selector, default "full".
    #' @param ... Additional parameters passed to subclass implementations.
    render = function(results, theme_obj, type = "full", ...) {
      stop("Not implemented", call. = FALSE)
    },

    # -----------------------------------------------------------------------
    # Palette accessors: return color vectors, no scale attached.
    # -----------------------------------------------------------------------

    #' @description Return the discrete palette, auto-extended to \code{n}.
    #' @param theme_obj IqrTheme object.
    #' @param n Integer. Desired length. If greater than the base length, the
    #'   palette is interpolated via \code{colorRampPalette}.
    .pal_discrete = function(theme_obj, n = NULL) {
      theme_obj$config$get_pal("discrete", n = n)
    },

    #' @description Return the sequential palette (3-stop gradient).
    #' @param theme_obj IqrTheme object.
    .pal_sequential = function(theme_obj) {
      theme_obj$config$get_pal("sequential")
    },

    #' @description Return the diverging palette (3 stops: low / mid / high).
    #' @param theme_obj IqrTheme object.
    .pal_diverging = function(theme_obj) {
      theme_obj$config$get_pal("diverging")
    },

    #' @description Return semantic color(s).
    #' @param theme_obj IqrTheme object.
    #' @param name Optional character. If supplied, return a single color by
    #'   name (e.g. "pass", "fail", "watch"). If NULL, return the full named
    #'   vector.
    .pal_semantic = function(theme_obj, name = NULL) {
      theme_obj$config$get_pal("semantic", name = name)
    },

    #' @description Return a UI color by name (primary / success / warning /
    #'   danger / muted / ...). Falls back to \code{default} when the slot
    #'   is missing.
    #' @param theme_obj IqrTheme object.
    #' @param name Character. UI slot name (e.g. "primary", "danger").
    #' @param default Character. Fallback hex color when the slot is absent.
    .pal_ui = function(theme_obj, name, default = "#888888") {
      ui <- theme_obj$config$get_ui()
      col <- ui[[name]]
      if (is.null(col) || is.na(col)) default else col
    },

    #' @description Pick a readable text color (black or white) for a given
    #'   background color, using WCAG luminance. Promoted from the legacy
    #'   \code{get_contrast_color()} local helper so all subpackages share
    #'   one implementation.
    #' @param bg Character. Background hex color (e.g. "#1F77B4").
    #' @return Character. "#000000" or "#FFFFFF".
    .contrast_text = function(bg) {
      if (is.null(bg) || !nzchar(bg) || !is.character(bg)) return("#000000")
      iqr_is_dark <- iQualityR.core::is_dark(bg)
      if (is.na(iqr_is_dark)) return("#000000")
      if (iqr_is_dark) "#FFFFFF" else "#000000"
    },

    # -----------------------------------------------------------------------
    # Scale factories: return ready-to-add ggplot2 scale objects.
    # -----------------------------------------------------------------------

    #' @description Build a discrete fill scale.
    #' @param theme_obj IqrTheme object.
    #' @param ... Passed to \code{PlotTheme$scale_fill_iqr()}.
    .scale_fill_discrete = function(theme_obj, ...) {
      theme_obj$plot$scale_fill_iqr(discrete = TRUE, ...)
    },

    #' @description Build a discrete color scale.
    #' @param theme_obj IqrTheme object.
    #' @param ... Passed to \code{PlotTheme$scale_color_iqr()}.
    .scale_color_discrete = function(theme_obj, ...) {
      theme_obj$plot$scale_color_iqr(discrete = TRUE, ...)
    },

    #' @description Build a discrete fill + color pair, where the color is a
    #'   darkened variant of the fill so edges remain visible (boxplots, bars).
    #' @param theme_obj IqrTheme object.
    #' @param ... Passed to \code{PlotTheme$scale_*_iqr(style = "paired")}.
    .scale_fill_color_paired = function(theme_obj, ...) {
      list(
        fill  = theme_obj$plot$scale_fill_iqr(discrete = TRUE, style = "paired", ...),
        color = theme_obj$plot$scale_color_iqr(discrete = TRUE, style = "paired", ...)
      )
    },

    #' @description Build a sequential fill gradient.
    #' @param theme_obj IqrTheme object.
    #' @param ... Passed to \code{PlotTheme$scale_fill_sequential()}.
    .scale_fill_sequential = function(theme_obj, ...) {
      theme_obj$plot$scale_fill_sequential(...)
    },

    #' @description Build a sequential color gradient.
    #' @param theme_obj IqrTheme object.
    #' @param ... Passed to \code{PlotTheme$scale_color_sequential()}.
    .scale_color_sequential = function(theme_obj, ...) {
      theme_obj$plot$scale_color_sequential(...)
    },

    #' @description Build a diverging fill gradient.
    #' @param theme_obj IqrTheme object.
    #' @param ... Passed to \code{PlotTheme$scale_fill_diverging()}.
    .scale_fill_diverging = function(theme_obj, ...) {
      theme_obj$plot$scale_fill_diverging(...)
    },

    #' @description Build a diverging color gradient.
    #' @param theme_obj IqrTheme object.
    #' @param ... Passed to \code{PlotTheme$scale_color_diverging()}.
    .scale_color_diverging = function(theme_obj, ...) {
      theme_obj$plot$scale_color_diverging(...)
    },

    #' @description Build a semantic fill scale.
    #' @param theme_obj IqrTheme object.
    #' @param labels Character vector of semantic names.
    #' @param ... Passed to \code{PlotTheme$scale_fill_semantic()}.
    .scale_fill_semantic = function(theme_obj,
                                    labels = c("pass", "fail", "watch"), ...) {
      theme_obj$plot$scale_fill_semantic(labels = labels, ...)
    },

    #' @description Build a semantic color scale.
    #' @param theme_obj IqrTheme object.
    #' @param labels Character vector of semantic names.
    #' @param ... Passed to \code{PlotTheme$scale_color_semantic()}.
    .scale_color_semantic = function(theme_obj,
                                     labels = c("pass", "fail", "watch"), ...) {
      theme_obj$plot$scale_color_semantic(labels = labels, ...)
    }
  )
)
