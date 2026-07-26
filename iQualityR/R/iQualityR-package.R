# =============================================================================
# File: R/iQualityR-package.R
# Description: Meta-package that aggregates the iQualityR framework.
# =============================================================================

#' @keywords internal
#' @importFrom utils packageDescription packageVersion
#' @import iQualityR.core iQualityR.plot iQualityR.stat iQualityR.msa
#' @import iQualityR.capa iQualityR.doe iQualityR.sampling iQualityR.reliability
#' @import iQualityR.predict iQualityR.spc
"_PACKAGE"

# The ordered list of member packages. The order respects the dependency
# topology of the framework (foundation first, business layer last) so that
# attaching them in sequence yields a consistent search path.
.iQualityR_packages <- c(
  "iQualityR.core",
  "iQualityR.plot",
  "iQualityR.stat",
  "iQualityR.msa",
  "iQualityR.capa",
  "iQualityR.doe",
  "iQualityR.sampling",
  "iQualityR.reliability",
  "iQualityR.predict",
  "iQualityR.spc"
)

# Attach member packages to the search path. Called from .onAttach so that
# \code{library(iQualityR)} makes every member API immediately available,
# following the tidyverse convention.
.iQualityR_attach <- function() {
  for (pkg in .iQualityR_packages) {
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

#' @title List iQualityR member packages
#'
#' @description Returns a data frame describing the member packages that are
#'   attached when \code{library(iQualityR)} is called. Useful for
#'   introspection, troubleshooting, and reporting.
#'
#' @return A \code{data.frame} with three columns:
#'   \describe{
#'     \item{package}{Character. Name of the member package.}
#'     \item{title}{Character. One-line description from the package DESCRIPTION.}
#'     \item{version}{Character. Installed version of the member package.}
#'   }
#'
#' @examples
#' iQualityR_packages()
#'
#' @export
iQualityR_packages <- function() {
  rows <- vapply(.iQualityR_packages, function(pkg) {
    desc <- tryCatch(
      packageDescription(pkg),
      error = function(e) NULL
    )
    if (is.null(desc)) {
      title <- NA_character_
      version <- NA_character_
    } else {
      title <- desc$Title
      version <- if (is.null(desc$Version)) NA_character_ else desc$Version
      if (is.null(title)) title <- NA_character_
    }
    c(package = pkg, title = title, version = version)
  }, character(3))
  as.data.frame(
    t(rows),
    stringsAsFactors = FALSE,
    row.names = .iQualityR_packages
  )
}

# When the meta-package is attached, verify every member package is installed
# and then attach them in dependency order. Failures are raised eagerly so
# that downstream code does not encounter surprise "could not find symbol"
# errors.
.onAttach <- function(libname, pkgname) {
  missing_pkgs <- .iQualityR_packages[!vapply(
    .iQualityR_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]
  if (length(missing_pkgs) > 0) {
    stop(
      "iQualityR requires the following member packages to be installed: ",
      paste(missing_pkgs, collapse = ", "),
      ".\nRun install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "),
      ")) to install them.",
      call. = FALSE
    )
  }

  .iQualityR_attach()

  packageStartupMessage(
    "-- Attaching the iQualityR framework ------------------------------- ",
    "iQualityR ", packageVersion("iQualityR"), " --"
  )
  invisible()
}
