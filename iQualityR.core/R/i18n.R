#' Get the active iQualityR locale
#'
#' @param locale Optional locale code. Supported values are `"en-US"` and `"zh-CN"`.
#'
#' @return Locale code.
#' @export
iqr_locale <- function(locale = NULL) {
  if (is.null(locale)) {
    locale <- getOption("iQualityR.locale", "en-US")
  }

  locale <- as.character(locale)[[1]]
  if (!locale %in% c("en-US", "zh-CN")) {
    locale <- "en-US"
  }

  locale
}

#' Set the active iQualityR locale
#'
#' @param locale Locale code.
#'
#' @return The selected locale, invisibly.
#' @export
iqr_set_locale <- function(locale = c("en-US", "zh-CN")) {
  locale <- match.arg(locale)
  options(iQualityR.locale = locale)
  invisible(locale)
}

.iqr_i18n_cache <- new.env(parent = emptyenv())

.iqr_load_messages <- function(locale) {
  locale <- iqr_locale(locale)
  if (exists(locale, envir = .iqr_i18n_cache, inherits = FALSE)) {
    return(get(locale, envir = .iqr_i18n_cache, inherits = FALSE))
  }

  path <- system.file("i18n", paste0(locale, ".csv"), package = "iQualityR.core")
  if (!nzchar(path) && identical(locale, "zh-CN")) {
    path <- system.file("i18n", "en-US.csv", package = "iQualityR.core")
  }

  if (!nzchar(path)) {
    messages <- setNames(character(), character())
  } else {
    data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    messages <- stats::setNames(data$text, data$key)
  }

  assign(locale, messages, envir = .iqr_i18n_cache)
  messages
}

.iqr_interpolate <- function(text, values) {
  if (!length(values)) {
    return(text)
  }

  for (name in names(values)) {
    text <- gsub(
      paste0("\\{", name, "\\}"),
      as.character(values[[name]]),
      text
    )
  }
  text
}

#' Translate an iQualityR message key
#'
#' @param key Message key.
#' @param locale Optional locale code.
#' @param ... Named values used for interpolation.
#' @param default Fallback text when key is missing.
#'
#' @importFrom stats setNames
#' @return Translated character scalar.
#' @export
iqr_t <- function(key, locale = NULL, ..., default = key) {
  locale <- iqr_locale(locale)
  messages <- .iqr_load_messages(locale)
  text <- messages[[key]] %||% default
  .iqr_interpolate(text, list(...))
}
