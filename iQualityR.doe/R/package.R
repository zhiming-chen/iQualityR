# =============================================================================
# File: R/package.R
# Description: Package-level documentation and import directives
# =============================================================================

#' iQualityR.doe
#'
#' Design of Experiments tools for the iQualityR ecosystem.
#'
#' @keywords internal
#' @import iQualityR.core
#' @import ggplot2
#' @importFrom iQualityR.core IqrTheme IqrPlotterBase
#' @importFrom R6 R6Class
#' @importFrom stats lm anova coef predict predict.lm model.matrix optim rnorm
#'   sd var median quantile runif pnorm dnorm qnorm pf qt
#'   setNames as.formula reformulate fitted residuals hatvalues df.residual
#'   update formula
#' @importFrom utils head tail combn
#' @importFrom methods is
#' @importFrom data.table data.table as.data.table
#' @importFrom patchwork plot_layout
"_PACKAGE"

# Shared IqrPlotterBase singleton — unified color pipeline entry point.
# All plotter classes and functional plot helpers in this package source
# colors from this instance via .pal_* / .scale_* methods, ensuring
# consistent theming across the iQualityR ecosystem.
.iqr_plotter <- iQualityR.core::IqrPlotterBase$new()

utils::globalVariables(c("self", "private", "level", "Block"))

.onLoad <- function(libname, pkgname) {
  invisible()
}
