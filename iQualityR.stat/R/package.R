# =============================================================================
# File: R/package.R
# Description: Package-level declarations for iQualityR.stat
# =============================================================================

#' @keywords internal
#' @importFrom iQualityR.plot plot_hypothesis_curve plot_hypothesis_box plot_hypothesis_combined
#' @importFrom iQualityR.plot base_plot layers_histogram_density layers_boxplot
#' @importFrom iQualityR.core IqrTheme IqrPlotterBase
"_PACKAGE"

# Shared IqrPlotterBase singleton — unified color pipeline entry point.
# All plotter classes and functional plot helpers in this package source
# colors from this instance via .pal_* / .scale_* methods, ensuring
# consistent theming across the iQualityR ecosystem.
.iqr_plotter <- iQualityR.core::IqrPlotterBase$new()

# Suppress R CMD check NOTE for data.table/dplyr non-standard evaluation
# variable bindings used in desc.R and sigma_estimate.R
utils::globalVariables(c(".", ".N", "metric", "v", "value"))
