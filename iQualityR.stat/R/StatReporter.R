# =============================================================================
# File: R/StatReporter.R
# Description: Unified base class for all .stat Reporter classes.
#              Inherits from iQualityR.core::IqrReporter to reuse the
#              register / export / export_excel / export_rmd pipeline, and
#              adds a flexible initialize(theme_obj = NULL) that accepts
#              NULL (no theme), a theme name string, or an IqrTheme object.
#
#              This lets .stat Reporters that do not need a theme
#              (ProbReporter, NormalityReporter) be constructed with no
#              arguments, while theme-aware Reporters (HTestReporter,
#              AnovaReporter) can pass a theme through to the parent.
# =============================================================================

#' @title StatReporter: Unified Base Class for .stat Reporters
#'
#' @description
#' Inherits from \code{\link[iQualityR.core]{IqrReporter}} and provides a
#' flexible \code{initialize(theme_obj = NULL)} that all .stat Reporter
#' subclasses can reuse.
#'
#' Subclasses inherit:
#' \itemize{
#'   \item \code{theme_obj} field -- from \code{IqrReporter}.
#'   \item \code{excel_exporter} field -- from \code{IqrReporter}.
#'   \item \code{templates} field -- from \code{IqrReporter}.
#'   \item \code{register(task_tag, rmd_template, excel_generator)} --
#'     from \code{IqrReporter}.
#'   \item \code{export(results, plan, task_tag, format, path, ...)} --
#'     from \code{IqrReporter}.
#'   \item \code{export_excel(...)} / \code{export_rmd(...)} --
#'     from \code{IqrReporter}.
#' }
#'
#' The \code{initialize} method accepts \code{NULL} (no theme set),
#' a theme name string, or an \code{IqrTheme} object. When a valid
#' \code{IqrTheme} is supplied, \code{super$initialize()} is called so
#' that \code{excel_exporter} is constructed; when \code{NULL} is
#' supplied, both \code{theme_obj} and \code{excel_exporter} remain
#' \code{NULL} and subclasses build exporters on demand.
#'
#' @export
StatReporter <- R6::R6Class("StatReporter",
  inherit = IqrReporter,

  public = list(
    #' @description Initialize the reporter with an optional theme
    #'
    #' @param theme_obj One of:
    #'   \itemize{
    #'     \item \code{NULL} (default) -- no theme is set; \code{theme_obj}
    #'       and \code{excel_exporter} remain \code{NULL}.
    #'     \item A theme name string (e.g. \code{"academic"}) -- resolved
    #'       to an \code{IqrTheme} via \code{.resolve_theme()}.
    #'     \item An \code{IqrTheme} object -- used directly.
    #'   }
    #' @return Invisible \code{self}.
    initialize = function(theme_obj = NULL) {
      if (is.null(theme_obj)) {
        # Allow construction without a theme. Subclasses that need Excel
        # export will build an ExcelExporter on demand from a default or
        # caller-supplied theme.
        self$theme_obj <- NULL
        self$excel_exporter <- NULL
      } else {
        # Resolve theme name strings to IqrTheme objects.
        if (!inherits(theme_obj, "IqrTheme")) {
          theme_obj <- .resolve_theme(theme_obj)
        }
        if (!is.null(theme_obj) && inherits(theme_obj, "IqrTheme")) {
          super$initialize(theme_obj)
        } else {
          self$theme_obj <- NULL
          self$excel_exporter <- NULL
        }
      }
      invisible(self)
    }
  )
)
