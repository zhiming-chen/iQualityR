# =============================================================================
# File: R/package.R
# Description: Package-level documentation and import directives
# =============================================================================

#' iQualityR.capa
#'
#' Process capability analysis tools for the iQualityR ecosystem.
#'
#' @keywords internal
#' @import iQualityR.core
#' @import ggplot2
#' @importFrom R6 R6Class
#' @importFrom stats sd var median quantile dnorm pnorm qnorm dweibull pweibull
#'   qweibull dlnorm plnorm qlnorm dgamma pgamma qgamma dexp pexp qexp dlogis
#'   plogis qlogis dbeta pbeta qbeta ks.test rnorm
#' @importFrom utils head tail
#' @importFrom data.table data.table as.data.table
#' @importFrom MASS fitdistr
#' @importFrom patchwork plot_annotation
#' @importFrom iQualityR.stat sigma_decomposition normality_test
#'   capability_interpret capability_to_ppm get_d2 get_D4
#' @importFrom iQualityR.plot base_plot layers_histogram_density
#'   layers_spec_limits layers_qq
"_PACKAGE"

utils::globalVariables("self")

.onLoad <- function(libname, pkgname) {
  invisible()
}
