#' Create a standardized app result
#'
#' @param ok Logical scalar indicating success.
#' @param code Stable machine-readable result code.
#' @param message User-facing message.
#' @param data Named list of summary metrics.
#' @param tables Named list of data frames.
#' @param plots Named list of plot file descriptors.
#' @param files Named list of output file paths.
#' @param warnings Character vector of non-fatal warnings.
#' @param metadata Named list of diagnostic metadata.
#'
#' @return A JSON-serializable list.
#' @export
app_result <- function(ok = TRUE,
                       code = "success",
                       message = "Analysis completed.",
                       data = list(),
                       tables = list(),
                       plots = list(),
                       files = list(),
                       warnings = character(),
                       metadata = list()) {
  list(
    ok = isTRUE(ok),
    code = as.character(code),
    message = as.character(message),
    data = data %||% list(),
    tables = tables %||% list(),
    plots = plots %||% list(),
    files = files %||% list(),
    warnings = as.character(warnings %||% character()),
    metadata = metadata %||% list()
  )
}

#' Create a standardized successful app result
#'
#' @inheritParams app_result
#' @return A JSON-serializable list.
#' @export
app_success <- function(code = "success",
                        message = "Analysis completed.",
                        data = list(),
                        tables = list(),
                        plots = list(),
                        files = list(),
                        warnings = character(),
                        metadata = list()) {
  app_result(
    ok = TRUE,
    code = code,
    message = message,
    data = data,
    tables = tables,
    plots = plots,
    files = files,
    warnings = warnings,
    metadata = metadata
  )
}

#' Create a standardized failed app result
#'
#' @param code Stable machine-readable error code.
#' @param message User-facing error message.
#' @param metadata Named list of diagnostic metadata.
#'
#' @return A JSON-serializable list.
#' @export
app_error <- function(code = "analysis_failed",
                      message = "Analysis failed.",
                      metadata = list()) {
  app_result(
    ok = FALSE,
    code = code,
    message = message,
    metadata = metadata
  )
}

#' Validate and create an output directory
#'
#' @param output_dir Directory path to validate.
#' @param create Logical. Create the directory if it does not exist.
#'
#' @return Normalized output directory path.
#' @export
validate_output_dir <- function(output_dir, create = TRUE) {
  if (is.null(output_dir) || !nzchar(output_dir)) {
    stop("output_dir must be a non-empty path.", call. = FALSE)
  }

  if (!dir.exists(output_dir)) {
    if (!isTRUE(create)) {
      stop("output_dir does not exist.", call. = FALSE)
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  normalizePath(output_dir, winslash = "/", mustWork = TRUE)
}
