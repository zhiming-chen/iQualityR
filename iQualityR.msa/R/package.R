#' iQualityR.msa
#'
#' Measurement system analysis tools for the iQualityR ecosystem.
#'
#' @keywords internal
#' @import iQualityR.core
#' @import ggplot2
#' @import dplyr
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @importFrom data.table as.data.table data.table
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom patchwork wrap_plots
#' @importFrom broom tidy
#' @importFrom tidyr pivot_longer
#' @importFrom reshape2 melt
#' @importFrom rlang .data
#' @importFrom stats rnorm
#' @importFrom utils write.csv
#' @importFrom iQualityR.stat get_d2 get_A2 get_D3 get_D4
#' @importFrom iQualityR.core IqrTheme IqrPlotterBase
"_PACKAGE"

utils::globalVariables("self")

# Shared IqrPlotterBase singleton — unified color pipeline entry point.
# All plotter classes and functional plot helpers in this package source
# colors from this instance via .pal_* / .scale_* methods, ensuring
# consistent theming across the iQualityR ecosystem.
.iqr_plotter <- iQualityR.core::IqrPlotterBase$new()

# Helper to locate an Rmd template bundled with this package.
# Searches the package's own inst/templates directory first, then falls back
# to a repo-wide glob for development convenience.
.msa_find_template <- function(template_file) {
  template <- system.file("templates", template_file, package = "iQualityR.msa")
  if (!identical(template, "")) return(template)

  search_roots <- unique(c(getwd(), dirname(getwd())))
  repo_templates <- unlist(lapply(search_roots, function(root) {
    Sys.glob(file.path(root, "iQualityR.msa", "inst", "templates", template_file))
  }), use.names = FALSE)
  if (length(repo_templates) > 0) return(repo_templates[[1]])

  ""
}

.onLoad <- function(libname, pkgname) {
  invisible()
}
