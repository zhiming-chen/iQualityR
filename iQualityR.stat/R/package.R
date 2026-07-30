# =============================================================================
# File: R/package.R
# Description: Package-level declarations for iQualityR.stat
# =============================================================================

#' @keywords internal
#' @importFrom iQualityR.core IqrTheme IqrPlotterBase IqrReporter
"_PACKAGE"

# Shared IqrPlotterBase singleton — unified color pipeline entry point.
# All plotter classes and functional plot helpers in this package source
# colors from this instance via .pal_* / .scale_* methods, ensuring
# consistent theming across the iQualityR ecosystem.
.iqr_plotter <- iQualityR.core::IqrPlotterBase$new()

# Null-coalescing operator (local definition to avoid rlang dependency).
# Returns b when a is NULL, otherwise returns a. Matches rlang::%||% semantics.
`%||%` <- function(a, b) if (is.null(a)) b else a

# Package-level helpers shared by all Plotter classes and plotting functions.
# Defined here (first in Collate) so desc.R and other early-loaded files can
# use them without forward-reference issues.

# Check that the iQualityR.plot Suggests package is available before any
# plotting call. Centralised so callers don't repeat the requireNamespace +
# error-message boilerplate.
.check_plot_available <- function() {
  if (!requireNamespace("iQualityR.plot", quietly = TRUE)) {
    stop("[iQualityR.stat] iQualityR.plot is required for plotting but is not installed. ",
         "Install it with: remotes::install_github('zhiming-chen/iQualityR', subdir='packages/iQualityR.plot')",
         call. = FALSE)
  }
}

# Resolve a theme argument (name string, IqrTheme object, or NULL) to an
# IqrTheme object or NULL. Used by Plotter constructors and plot methods.
.resolve_theme <- function(theme) {
  if (is.null(theme)) return(NULL)
  if (inherits(theme, "IqrTheme")) return(theme)
  tryCatch(
    iQualityR.core::IqrTheme$new(theme),
    error = function(e) NULL
  )
}

# Suppress R CMD check NOTE for ggplot2 aes() column references
# used in desc.R and other plotting functions
utils::globalVariables(c("metric", "v", "value"))
