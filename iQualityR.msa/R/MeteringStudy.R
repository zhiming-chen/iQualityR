# =============================================================================
# File: R/MeteringStudy.R
# Description: Backward-compatible wrapper for the legacy MeteringStudy API.
# =============================================================================

#' @title MeteringStudy
#' @description
#' Backward-compatible alias for \code{MsaTask}. New code should use
#' \code{MsaTask}; this wrapper preserves existing user code that calls
#' \code{MeteringStudy$new()}.
#' @export
MeteringStudy <- R6::R6Class(
  "MeteringStudy",
  inherit = MsaTask
)
