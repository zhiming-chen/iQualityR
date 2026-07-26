# =============================================================================
# File: R/package.R
# Description: Package-level declarations for iQualityR.stat
# =============================================================================

#' @keywords internal
#' @importFrom iQualityR.plot plot_hypothesis_curve plot_hypothesis_box plot_hypothesis_combined
#' @importFrom iQualityR.plot plot_qq plot_pp
"_PACKAGE"

# Suppress R CMD check NOTE for data.table/dplyr non-standard evaluation
# variable bindings used in desc.R and sigma_estimate.R
utils::globalVariables(c(".", ".N", "metric", "v", "value"))
