#' Validate Metadata
#'
#' Validate and ensure the presence of required category slots in a metadata list
#' (man, machine, material, method, environment, project).
#'
#' @param metadata List containing metadata.
#'
#' @return The validated metadata list with all required category slots initialized.
#'
#' @export
validate_metadata <- function(metadata) {
  if (!is.list(metadata)) {
    stop("metadata must be a list", call. = FALSE)
  }

  required_categories <- c("man", "machine", "material", "method", "environment", "project")
  for (category in required_categories) {
    if (!category %in% names(metadata)) {
      metadata[[category]] <- list()
    }
  }

  metadata
}

#' Validate Inputs
#'
#' Check that input data is a data frame and that required columns are present.
#'
#' @param data Data frame to validate.
#' @param required_cols Character vector of required column names.
#'
#' @return TRUE if validation passes; otherwise stops with an error message.
#'
#' @export
validate_inputs <- function(data, required_cols = NULL) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }

  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
  }

  TRUE
}

#' Create Error Message
#'
#' Format an error message with a standardized type prefix.
#'
#' @param message Error message text.
#' @param type Error type (e.g., "input", "calculation", "configuration").
#'
#' @return Formatted error message string.
#'
#' @export
create_error_message <- function(message, type = "error") {
  sprintf("[%s] %s", toupper(type), message)
}

#' Get Configuration Value
#'
#' Retrieve a configuration value from the global \code{iQualityR.config} option,
#' with a fallback default.
#'
#' @param key Configuration key.
#' @param default Default value if the key is not found.
#'
#' @return Configuration value or default.
#'
#' @export
get_config <- function(key, default = NULL) {
  config <- getOption("iQualityR.config", list())
  config[[key]] %||% default
}

#' Generate Anonymous Identifiers
#'
#' Creates a vector of randomized high-entropy strings suitable for anonymizing
#' part IDs, subject IDs, or similar identifiers.
#'
#' @param n Integer. The number of IDs to generate.
#' @param k Integer. The length of the random string (excluding the prefix). Default is 8.
#' @param prefix Character. A string to prepend to the random identifier. Default is "ID".
#'
#' @return A character vector of unique anonymous IDs.
#'
#' @export
generate_anon_id <- function(n, k = 8, prefix = "ID") {
  pool <- c(0:9, letters, LETTERS)

  generate <- function() {
    replicate(n, {
      paste0(prefix, paste(sample(pool, k, replace = TRUE), collapse = ""))
    })
  }

  ids <- generate()
  while (any(duplicated(ids))) {
    ids[duplicated(ids)] <- generate()[1:sum(duplicated(ids))]
  }

  ids
}

#' Calculate Moving Range Statistics
#'
#' Calculate the moving ranges, mean of the moving ranges, and median of the
#' moving ranges for a numeric vector using a given window span.
#'
#' @param data A numeric vector containing the observations.
#' @param m_span An integer specifying the window size (span) for the moving range.
#'
#' @return A named list with elements \code{mr} (numeric vector of moving ranges,
#'   padded with \code{m_span - 1} NA values at the start), \code{mr_bar} (mean),
#'   and \code{mr_median} (median).
#'
#' @importFrom stats embed median setNames
#' @export
moving_range_stats <- function(data, m_span) {
  if (length(data) < m_span) stop("Data length is shorter than the window span.")

  mat <- embed(data, m_span)
  ranges <- apply(mat, 1, function(row) max(row) - min(row))

  list(
    mr = c(rep(NA_real_, m_span - 1), ranges),
    mr_bar = mean(ranges),
    mr_median = median(ranges)
  )
}

#' Format P-values for Display
#'
#' Format numeric p-values into display-friendly character strings.
#' Supports two contexts:
#' - \code{"table"}: Returns significance stars (***, **, *, .) or formatted numbers.
#' - \code{"plot"}: Returns numeric strings suitable for plot annotations (e.g., "<0.001").
#'
#' @param p_value Numeric vector of p-values.
#' @param context Character. Either \code{"table"} (default, with significance stars) or \code{"plot"} (plain numeric format).
#'
#' @return Character vector of formatted p-values.
#'
#' @examples
#' format_p_value(c(0.0001, 0.003, 0.04, 0.08, 0.5))
#' # [1] "***"  "**"   "*"    "."    "0.500"
#'
#' format_p_value(c(0.0001, 0.003, 0.04, 0.08, 0.5), context = "plot")
#' # [1] "<0.001" "0.003"  "0.040"  "0.080"  "0.500"
#'
#' @export
format_p_value <- function(p_value, context = c("table", "plot")) {
  context <- match.arg(context)
  
  if (context == "plot") {
    # Graphic mode: using R's base function format.pval to automatically handle very small p-values.
    return(format.pval(p_value, digits = 3))
  }
  
  # 表格模式（默认）：返回显著性星号或格式化数字
  sapply(p_value, function(p) {
    if (is.na(p)) {
      return("")
    } else if (p <= 0.001) {
      return("***")
    } else if (p <= 0.01) {
      return("**")
    } else if (p <= 0.05) {
      return("*")
    } else if (p <= 0.1) {
      return(".")
    } else {
      return(format(round(p, 3), nsmall = 3))
    }
  })
}

#' Format Scientific Notation
#'
#' Format numbers in scientific notation when the absolute value is below 0.001,
#' otherwise in fixed notation.
#'
#' @param x Numeric vector to format.
#' @param digits Number of significant digits to display.
#'
#' @return Character vector of formatted numbers.
#'
#' @export
format_scientific <- function(x, digits = 3) {
  sapply(x, function(val) {
    if (is.na(val)) {
      return("")
    } else if (abs(val) < 0.001) {
      return(format(val, digits = digits, scientific = TRUE))
    } else {
      return(format(val, digits = digits, scientific = FALSE))
    }
  })
}

#' Safe Tolerance Calculation
#'
#' Calculate the difference between upper and lower specification limits,
#' returning NULL when either value is NULL or NA.
#'
#' @param usl Upper specification limit.
#' @param lsl Lower specification limit.
#'
#' @return Tolerance value (usl - lsl) or NULL if inputs are invalid.
#'
#' @export
safe_tolerance <- function(usl, lsl) {
  if (is.null(usl) || is.null(lsl) || is.na(usl) || is.na(lsl)) {
    return(NULL)
  } else {
    return(usl - lsl)
  }
}

#' Convert NULL to NA
#'
#' Simple helper to convert NULL values to NA.
#'
#' @param x Value to convert.
#'
#' @return NA if \code{x} is NULL, otherwise \code{x} unchanged.
#'
#' @export
null_to_na <- function(x) {
  if (is.null(x)) NA else x
}

#' Create Task Registry
#'
#' Create a registry mapping task names to analyzer, plotter, and reporter class names.
#' Useful for dynamic dispatch of task-specific logic based on naming convention.
#'
#' @param tasks Character vector of task names.
#' @param analyzer_suffix Suffix for analyzer class names (default "Analyzer").
#' @param plotter_suffix Suffix for plotter class names (default "Plotter").
#' @param reporter_suffix Suffix for reporter class names (default "Reporter").
#' @param name_transformer Optional function to transform task names to class names.
#'   Defaults to capitalizing the first letter and lowercasing the rest.
#'
#' @return A named list; each element is itself a list with \code{analyzer},
#'   \code{plotter}, and \code{reporter} character entries.
#'
#' @export
create_task_registry <- function(tasks,
                                 analyzer_suffix = "Analyzer",
                                 plotter_suffix = "Plotter",
                                 reporter_suffix = "Reporter",
                                 name_transformer = function(x) {
                                   paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
                                 }) {
  registry <- list()
  for (task in tasks) {
    base_name <- name_transformer(task)
    registry[[task]] <- list(
      analyzer = paste0(base_name, analyzer_suffix),
      plotter  = paste0(base_name, plotter_suffix),
      reporter = paste0(base_name, reporter_suffix)
    )
  }
  return(registry)
}

# Internal coalesce operator — not exported
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
