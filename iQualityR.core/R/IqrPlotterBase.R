#' @title iQualityR Plotter Base Class
#'
#' @description
#' Abstract plotter base class. Defines the interface for rendering plots.
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
    }
  )
)
