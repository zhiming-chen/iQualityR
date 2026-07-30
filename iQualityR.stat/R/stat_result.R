# =============================================================================
# File: R/stat_result.R
# Description: stat_result S3 class definition + print/format methods
#              Lightweight S3: only class attribute + print/format, no generic
#              dispatch beyond the standard print/format generics.
#              Per Architecture Decision 1 (v2.0 plan): unified return structure
#              across all L1 Analyzers so users can uniformly inspect results.
# =============================================================================

# -----------------------------------------------------------------------------
# Constructor: new_stat_result
# -----------------------------------------------------------------------------
# Internal constructor used by L1 Analyzers to wrap a result list with the
# stat_result class attribute. Keeps the class-string convention in one place
# so every domain (htest, anova, regression, ...) stays consistent.
#
# Per contract 3 (STAT_ANALYSIS_PLAN.md), a stat_result MUST contain:
#   domain, test_type, method, statistic, parameter, p.value, conf.int,
#   conf.level, estimate, null.value, alternative, data_name
# Domain-specific extras (n, sigma, se, dist_type, data, ...) are allowed.

new_stat_result <- function(x, domain) {
  if (!is.list(x)) {
    stop("new_stat_result: x must be a list.", call. = FALSE)
  }
  if (!is.character(domain) || length(domain) != 1L) {
    stop("new_stat_result: domain must be a single string.", call. = FALSE)
  }
  x$domain <- domain
  class(x) <- c("stat_result", paste0(domain, "_result"))
  x
}

# -----------------------------------------------------------------------------
# format.stat_result
# -----------------------------------------------------------------------------
# Compact multi-line text representation. Used by both print.stat_result and
# the L2 Reporter's console output path. Layout mirrors base R htest printing
# (method / data / statistic / df / p-value / CI / estimate / alternative) so
# users familiar with t.test() output feel at home, while adding the domain
# tag for quick type identification.

#' @export
#' @method format stat_result
format.stat_result <- function(x, ...) {
  # Mandatory header
  method <- x$method %||% x$test_type %||% "Statistical Result"
  data_name <- x$data_name %||% ""

  lines <- character(0)
  lines <- c(lines, "")
  lines <- c(lines, sprintf("\t%s", method))
  lines <- c(lines, "")
  lines <- c(lines, sprintf("data:  %s", data_name))

  # Statistic (named numeric, e.g. c(t = 2.34))
  if (!is.null(x$statistic) && length(x$statistic) > 0L) {
    stat_name <- names(x$statistic)[1L]
    stat_val <- as.numeric(x$statistic[1L])
    lines <- c(lines, sprintf("%s = %.4f, %s",
                              stat_name,
                              stat_val,
                              .format_df(x$parameter)))
  }

  # P-value
  if (!is.null(x$p.value)) {
    p_str <- if (x$p.value < 1e-4) "<1e-04" else sprintf("%.4f", x$p.value)
    lines <- c(lines, sprintf("p-value = %s", p_str))
  }

  # Alternative hypothesis
  if (!is.null(x$alternative)) {
    alt_text <- switch(x$alternative,
      "two.sided" = "two-sided",
      "less"      = "less",
      "greater"   = "greater",
      x$alternative
    )
    null_text <- .format_null_value(x$null.value)
    lines <- c(lines, sprintf("alternative hypothesis: %s %s", alt_text, null_text))
  }

  # Confidence interval
  if (!is.null(x$conf.int) && length(x$conf.int) == 2L) {
    cl <- (x$conf.level %||% NA) * 100
    ci_lo <- x$conf.int[1L]
    ci_hi <- x$conf.int[2L]
    if (is.infinite(ci_lo) && is.infinite(ci_hi)) {
      # skip degenerate
    } else {
      cl_str <- if (is.na(cl)) "" else sprintf("%.1f percent ", cl)
      lo_str <- if (is.infinite(ci_lo)) "-Inf" else sprintf("%.4f", ci_lo)
      hi_str <- if (is.infinite(ci_hi))  "Inf" else sprintf("%.4f", ci_hi)
      lines <- c(lines, sprintf("%sconfidence interval: [%s, %s]", cl_str, lo_str, hi_str))
    }
  }

  # Sample estimates
  if (!is.null(x$estimate) && length(x$estimate) > 0L) {
    lines <- c(lines, "sample estimates:")
    for (nm in names(x$estimate)) {
      lines <- c(lines, sprintf("  %s: %.4f", nm, x$estimate[[nm]]))
    }
  }

  # Domain tag footer (quick type identification for users / debugging)
  lines <- c(lines, "")
  lines <- c(lines, sprintf("[stat_result: domain=%s | test_type=%s]",
                            x$domain %||% "?",
                            x$test_type %||% "?"))

  paste(lines, collapse = "\n")
}

# -----------------------------------------------------------------------------
# print.stat_result
# -----------------------------------------------------------------------------
# S3 method on the standard print generic. cat's the format output so the
# result prints cleanly at the console and inside R6 chains.

#' @export
#' @method print stat_result
print.stat_result <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Format degrees of freedom / parameter vector as comma-separated string.
# Returns "NA" when parameter is NULL so the statistic line stays readable.
.format_df <- function(parameter) {
  if (is.null(parameter) || length(parameter) == 0L) {
    return("NA")
  }
  vals <- sprintf("%s = %s", names(parameter),
                  vapply(parameter, function(v) {
                    if (is.integer(v)) as.character(v) else sprintf("%.2f", as.numeric(v))
                  }, character(1L)))
  paste(vals, collapse = ", ")
}

# Render the null.value vector as "true mean = 5" / "difference in means = 0"
# for the alternative-hypothesis line. Empty -> "".
.format_null_value <- function(null.value) {
  if (is.null(null.value) || length(null.value) == 0L) {
    return("")
  }
  nm <- names(null.value)[1L]
  val <- null.value[[1L]]
  sprintf("true %s is not equal to %s", nm, format(val, digits = 4L))
}
