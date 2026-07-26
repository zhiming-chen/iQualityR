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

utils::globalVariables(c("self", "private", "level", "Block"))

.onLoad <- function(libname, pkgname) {
  invisible()
}
