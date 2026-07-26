#' @title iQualityR Task Base Class
#'
#' @description
#' Base class for all analysis tasks (descriptive statistics, SPC, capability analysis, MSA, etc.).
#' Provides unified data storage, theme management, executor placeholders, and virtual methods.
#'
#' @field data Raw data (data frame).
#' @field theme_obj IqrTheme object.
#' @field results List storing calculation results.
#' @field executor List storing task executors (analyzer, plotter, reporter).
#'
#' @importFrom R6 R6Class
#' @export
IqrTaskBase <- R6::R6Class("IqrTaskBase",
  public = list(
    data = NULL,
    theme_obj = NULL,
    results = NULL,
    executor = list(),

    #' @description Initialize task base class.
    #' @param data Data frame.
    #' @param theme Theme name (character), IqrTheme object, or ThemeConfig object.
    #' @param ... Other parameters (reserved for extension).
    initialize = function(data, theme = "academic", ...) {
      if (is.character(theme)) {
        self$theme_obj <- IqrTheme$new(theme, ...)
      } else if (inherits(theme, "IqrTheme")) {
        self$theme_obj <- theme
      } else if (inherits(theme, "ThemeConfig")) {
        self$theme_obj <- IqrTheme$new(theme_style = "academic", ...)
        self$theme_obj$config <- theme
      } else {
        stop("theme must be a character string, an IqrTheme object, or a ThemeConfig object", call. = FALSE)
      }
      self$data <- data
      invisible(self)
    },

    #' @description Execute computation (virtual method, must be overridden by subclass).
    compute = function() {
      stop("compute() method not implemented for this task.", call. = FALSE)
    },

    #' @description Print summary (virtual method, should be overridden by subclass).
    summary = function() {
      cat("No summary available. Please implement summary() in the child class.\n")
      invisible(self)
    },

    #' @description Draw plot (virtual method, should be overridden by subclass).
    #' @param ... Additional parameters passed to subclass implementation.
    plot = function(...) {
      stop("plot() method not implemented for this task.", call. = FALSE)
    },

    #' @description Generate report (virtual method, should be overridden by subclass).
    #' @param format Character. Output format ("excel", "html", "pdf", "docx", "pptx").
    #' @param path Character. Output file path.
    #' @param ... Additional parameters passed to subclass implementation.
    report = function(format = "excel", path = NULL, ...) {
      stop("report() method not implemented for this task.", call. = FALSE)
    }
  )
)
