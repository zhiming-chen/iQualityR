#' @title iQualityR Analyzer Base Class
#'
#' @description
#' Abstract executor base class. Responsible for defining result storage protocol and handling parameter normalization.
#' Follows v2.0 framework specification: supports Task-Executor matrix architecture.
#'
#' @field results Standardized result container (list with statistics, diagnostics, data_tables, raw_output).
#' @field params Named list of parameters (after setup).
#'
#' @importFrom R6 R6Class
#' @export
IqrAnalyzerBase <- R6::R6Class("IqrAnalyzerBase",
  public = list(
    results = NULL,
    params = list(),

    #' @description Initialize standardized result container.
    #' Container structure is aligned with the needs of Reporter and Plotter.
    initialize = function() {
      self$reset()
    },

    #' @description Reset result container to empty state.
    reset = function() {
      self$results <- list(
        statistics  = list(),
        diagnostics = list(),
        data_tables = list(),
        raw_output  = NULL
      )
      invisible(self)
    },

    #' @description Store a single statistic in the results container.
    #' @param key Character. Name of the statistic.
    #' @param value Value of the statistic.
    set_statistic = function(key, value) {
      self$results$statistics[[key]] <- value
      invisible(self)
    },

    #' @description Store a single diagnostic entry in the results container.
    #' @param key Character. Name of the diagnostic.
    #' @param value Value of the diagnostic.
    set_diagnostic = function(key, value) {
      self$results$diagnostics[[key]] <- value
      invisible(self)
    },

    #' @description Store a single data table in the results container.
    #' @param key Character. Name of the data table.
    #' @param value Data frame to store.
    set_datatable = function(key, value) {
      self$results$data_tables[[key]] <- value
      invisible(self)
    },

    #' @description Store raw output from an analysis.
    #' @param value Arbitrary R object returned by the underlying analysis.
    set_raw_output = function(value) {
      self$results$raw_output <- value
      invisible(self)
    },

    #' @description Set analyzer parameters from a plan object or a list.
    #' @param input An \code{IqrPlanBase} object or a named list.
    setup = function(input = list()) {
      if (inherits(input, "IqrPlanBase")) {
        self$params <- input$to_list()
      } else if (is.list(input)) {
        self$params <- input
      } else {
        stop("[IqrAnalyzerBase] Input must be an IqrPlanBase object or list.")
      }
      invisible(self)
    },

    #' @description Core entry point for executing analysis.
    #' @param data Observation data. Accepts data.frame, data.table, tibble, or matrix.
    #'   data.frame-like objects are passed through unchanged; matrices are coerced to data.frame.
    #' @param ... Additional named parameters merged into \code{params}.
    run = function(data, ...) {
      dots <- list(...)
      if (length(dots) > 0) self$params <- modifyList(self$params, dots)

      if (missing(data) || is.null(data)) stop("Data required.", call. = FALSE)
      if (is.data.frame(data)) {
        dt <- data
      } else {
        dt <- as.data.frame(data)
      }

      self$reset()
      private$.run_logic(dt)
      invisible(self)
    },

    #' @description Retrieve standardized results.
    get_results = function() {
      return(self$results)
    }
  ),
  private = list(
    # Abstract business logic, forced to be implemented by subclass
    .run_logic = function(dt) {
      stop("Subclass must implement '.run_logic()'.", call. = FALSE)
    }
  )
)
