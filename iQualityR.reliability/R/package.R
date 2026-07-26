# =============================================================================
# File: R/package.R
# Description: Package-level documentation and import directives
# =============================================================================

#' iQualityR.reliability
#'
#' Reliability and survival analysis tools for the iQualityR ecosystem.
#' Provides parametric lifetime distribution fitting, Kaplan-Meier
#' estimation, Cox proportional hazards regression, and reliability metrics.
#'
#' @keywords internal
#' @import iQualityR.core
#' @import ggplot2
#' @importFrom R6 R6Class
#' @importFrom stats pweibull pexp plnorm plogis dweibull dexp dlnorm dlogis
#'   qnorm median sd var quantile na.omit optim dlogis dlnorm dexp
#' @importFrom utils head tail
#' @importFrom patchwork plot_layout plot_annotation wrap_plots
#' @importFrom iQualityR.stat fit_distribution reliability
#' @importFrom iQualityR.plot base_plot
"_PACKAGE"

utils::globalVariables(c("self", "private"))
