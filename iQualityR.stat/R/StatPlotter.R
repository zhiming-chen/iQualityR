# =============================================================================
# File: R/StatPlotter.R
# Description: Unified base class for all .stat Plotter classes.
#              Provides shared theme management (theme_obj field +
#              initialize / set_theme) and a private plot-availability
#              guard so subclasses do not repeat the same boilerplate.
#
#              StatPlotter is a .stat-specific base: it delegates theme
#              resolution to the package-level .resolve_theme() helper
#              (defined in R/package.R) and plot-availability checking to
#              the package-level .check_plot_available() helper.
#              Subclasses remain thin delegation layers over
#              iQualityR.plot and must NOT inline ggplot2 logic.
# =============================================================================

#' @title StatPlotter: Unified Base Class for .stat Plotters
#'
#' @description
#' Provides shared theme management for all Plotter classes in the
#' iQualityR.stat package. Subclasses inherit:
#' \itemize{
#'   \item \code{theme_obj} field -- the active \code{IqrTheme} object.
#'   \item \code{initialize(theme)} -- resolves a theme name / IqrTheme /
#'     NULL into an \code{IqrTheme} object (or NULL).
#'   \item \code{set_theme(theme)} -- replaces the active theme at runtime.
#'   \item \code{.check_plot_available()} (private) -- verifies the
#'     \code{iQualityR.plot} Suggests package is installed.
#' }
#'
#' Subclasses are thin delegation layers and must NOT inline ggplot2
#' logic; all rendering is delegated to \code{iQualityR.plot}.
#'
#' @export
StatPlotter <- R6::R6Class("StatPlotter",
  public = list(
    #' @field theme_obj Active \code{IqrTheme} object used to style plots
    #'   (NULL when no theme has been resolved).
    theme_obj = NULL,

    #' @description Initialize the plotter with a theme
    #'
    #' Resolves the \code{theme} argument via the package-level
    #' \code{.resolve_theme()} helper, which accepts:
    #' \itemize{
    #'   \item a theme name string (e.g. \code{"academic"})
    #'   \item an \code{IqrTheme} object (returned as-is)
    #'   \item \code{NULL} (returns \code{NULL})
    #' }
    #'
    #' @param theme Theme name, \code{IqrTheme} object, or \code{NULL}.
    #' @return Invisible \code{self}.
    initialize = function(theme = "academic") {
      self$theme_obj <- .resolve_theme(theme)
      invisible(self)
    },

    #' @description Set or replace the active theme at runtime
    #'
    #' @param theme Theme name, \code{IqrTheme} object, or \code{NULL}.
    #' @return Invisible \code{self} (for chaining).
    set_theme = function(theme) {
      self$theme_obj <- .resolve_theme(theme)
      invisible(self)
    }
  ),

  private = list(
    # Check that the iQualityR.plot Suggests package is available before any
    # plotting call. Delegates to the package-level .check_plot_available()
    # helper so the error message stays consistent across all subclasses.
    .check_plot_available = function() {
      .check_plot_available()
    }
  )
)
