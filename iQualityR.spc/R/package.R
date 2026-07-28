# =============================================================================
# File: R/package.R
# Description: Package-level documentation and import directives for iQualityR.spc
# =============================================================================

#' iQualityR.spc
#'
#' Statistical process control (SPC) charts for the iQualityR ecosystem.
#' Implements the full Minitab Stat > Control Charts menu (variables, attributes,
#' time-weighted, multivariate, rare-event charts) plus Nelson rules detection.
#'
#' @keywords internal
#' @import iQualityR.core
#' @import ggplot2
#' @importFrom R6 R6Class
#' @importFrom stats sd var median qnorm pnorm dnorm qexp pexp qgeom pgeom
#'   cov mahalanobis lm residuals fitted na.pass as.formula runif
#' @importFrom utils head tail
#' @importFrom data.table data.table as.data.table
#' @importFrom patchwork plot_annotation wrap_plots
#' @importFrom iQualityR.stat calc_control_limits detect_spc_violations
#'   sigma_estimate sigma_decomposition summarize_spc_rules list_spc_rules
#'   get_d2 get_d3 get_d4 get_c4 get_A2 get_A3 get_B3 get_B4 get_D3 get_D4 get_E2
#' @importFrom iQualityR.plot layers_control_chart base_plot
#' @importFrom iQualityR.core IqrTheme IqrPlotterBase
"_PACKAGE"

# Shared IqrPlotterBase singleton — unified color pipeline entry point.
# All plotter classes in this package source colors from this instance via
# .pal_* / .scale_* methods, ensuring consistent theming across the iQualityR
# ecosystem.
.iqr_plotter <- iQualityR.core::IqrPlotterBase$new()

# Suppress R CMD check NOTE for ggplot2 aes() column references and
# data.table/non-standard evaluation variable bindings.
utils::globalVariables(c("self", ".", ".N", "value", "x", "y", "cl", "ucl",
                         "lcl", "index", "subgroup", "rule", "description"))

.onLoad <- function(libname, pkgname) {
  invisible()
}
