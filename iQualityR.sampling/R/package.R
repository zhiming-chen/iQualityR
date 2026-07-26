# =============================================================================
# File: R/package.R
# Description: Package-level documentation and import directives
# =============================================================================

#' iQualityR.sampling
#'
#' Sampling plan analysis tools for the iQualityR ecosystem. Provides
#' attribute sampling plans (single, double, multiple), operating
#' characteristic (OC) curves, power analysis, risk evaluation, and
#' average sample number (ASN) curves.
#'
#' @keywords internal
#' @import iQualityR.core
#' @import ggplot2
#' @importFrom R6 R6Class
#' @importFrom stats dbinom pbinom dhyper plnorm qnorm approx
#' @importFrom utils head tail
#' @importFrom patchwork plot_layout plot_annotation wrap_plots
#' @importFrom withr local_seed
"_PACKAGE"

utils::globalVariables(c("self", "private"))

# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

#' Null-coalescing operator (internal)
#'
#' Returns \code{lhs} if not NULL, otherwise \code{rhs}.
#' @param lhs Any R object.
#' @param rhs Any R object.
#' @return \code{lhs} if not NULL, else \code{rhs}.
#' @keywords internal
#' @noRd
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

#' Safely retrieve a primary color from an IqrTheme
#' @param theme_obj IqrTheme object (may be NULL or malformed).
#' @return Character color string (hex). Falls back to "#2563EB" if unavailable.
#' @keywords internal
.safe_primary <- function(theme_obj) {
  color <- tryCatch(
    theme_obj$config$config$ui$primary,
    error = function(e) NULL
  )
  if (is.null(color) || !nzchar(color)) "#2563EB" else color
}

#' Safely retrieve a danger color from an IqrTheme
#' @param theme_obj IqrTheme object (may be NULL or malformed).
#' @return Character color string (hex). Falls back to "#B42318" if unavailable.
#' @keywords internal
.safe_danger <- function(theme_obj) {
  color <- tryCatch(
    theme_obj$config$config$ui$danger,
    error = function(e) NULL
  )
  if (is.null(color) || !nzchar(color)) "#B42318" else color
}

#' Safely retrieve a success color from an IqrTheme
#' @param theme_obj IqrTheme object (may be NULL or malformed).
#' @return Character color string (hex). Falls back to "#0F766E" if unavailable.
#' @keywords internal
.safe_success <- function(theme_obj) {
  color <- tryCatch(
    theme_obj$config$config$ui$success,
    error = function(e) NULL
  )
  if (is.null(color) || !nzchar(color)) "#0F766E" else color
}

#' Safely retrieve a warning color from an IqrTheme
#' @param theme_obj IqrTheme object (may be NULL or malformed).
#' @return Character color string (hex). Falls back to "#B45309" if unavailable.
#' @keywords internal
.safe_warning <- function(theme_obj) {
  color <- tryCatch(
    theme_obj$config$config$ui$warning,
    error = function(e) NULL
  )
  if (is.null(color) || !nzchar(color)) "#B45309" else color
}

#' Safely retrieve base font size from an IqrTheme
#' @param theme_obj IqrTheme object (may be NULL or malformed).
#' @return Numeric base font size; falls back to 12.
#' @keywords internal
.safe_base_size <- function(theme_obj) {
  size <- tryCatch(
    theme_obj$config$config$ui$base_size,
    error = function(e) NULL
  )
  if (is.null(size) || !is.numeric(size)) 12 else size
}

#' Build an empty ggplot with a centered message
#' @param message Character. Message to display.
#' @return A ggplot object.
#' @keywords internal
.empty_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0.5, y = 0.5, label = message,
      hjust = 0.5, vjust = 0.5, size = 5
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_void()
}

#' Safely format a numeric value
#' @param x Numeric value.
#' @param digits Integer. Number of digits.
#' @return Formatted string or "N/A" if x is NULL/NA.
#' @keywords internal
.safe_num <- function(x, digits = 4) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) "N/A"
  else formatC(x, digits = digits, format = "f")
}
