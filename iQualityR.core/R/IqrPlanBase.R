#' @title iQualityR Plan Base Class
#'
#' @description
#' Abstract base class for all analysis plans. Responsible for managing task metadata (4M1E),
#' statistical parameters, report configuration, and experimental protocol generation interface.
#' Follows iQualityR framework specification v2.0.
#'
#' @field task_tag Character. Unique task identifier (e.g., "capability", "msa").
#' @field meta_data List. Stores 4M1E metadata (man, machine, material, method, environment, project).
#' @field stats_params List. Stores statistical parameters for specific algorithms.
#' @field conf_level Numeric. Confidence level, default 0.95.
#' @field criteria List. Stores evaluation criteria for specific tasks.
#'
#' @importFrom R6 R6Class
#' @export
IqrPlanBase <- R6::R6Class("IqrPlanBase",
  public = list(
    task_tag = NULL,
    meta_data = list(),
    stats_params = list(),
    conf_level = 0.95,
    criteria = list(),

    #' @description Initialize Planner base class.
    #' @param task_tag Character. Task tag.
    #' @param conf_level Numeric. Confidence level, between 0 and 1.
    #' @param ... Additional named parameters merged into \code{stats_params}.
    initialize = function(task_tag = "base", conf_level = 0.95, ...) {
      self$task_tag <- task_tag
      self$conf_level <- private$.validate_conf_level(conf_level)

      self$meta_data <- list(
        man = list(),
        machine = list(),
        material = list(),
        method = list(),
        environment = list(),
        project = list()
      )

      extra_args <- list(...)
      if (length(extra_args) > 0) {
        self$stats_params <- modifyList(self$stats_params, extra_args)
      }
    },

    #' @description Set evaluation criteria (e.g., cpk = 1.33).
    #' @param ... Named criteria values.
    set_criteria = function(...) {
      self$criteria <- modifyList(self$criteria, list(...))
      invisible(self)
    },

    #' @description Set metadata for a 4M1E category.
    #' @param category Character. Category name ("man", "machine", "material", "method", "environment", "project").
    #' @param ... Named attribute values to set for the category.
    set_meta = function(category, ...) {
      if (!category %in% names(self$meta_data)) {
        self$meta_data[[category]] <- list()
      }
      new_items <- list(...)
      self$meta_data[[category]] <- modifyList(self$meta_data[[category]], new_items)
      invisible(self)
    },

    #' @description Validate plan configuration (can be overridden by subclasses).
    validate = function() {
      private$.validate_conf_level(self$conf_level)
      invisible(self)
    },

    #' @description Abstract method: generate experimental plan/protocol.
    #' @param ... Additional arguments (reserved for subclasses).
    generate_protocol = function(...) {
      stop("[Planner] generate_protocol method not implemented for current task.")
    },

    #' @description Export configuration as a plain list for the Analyzer.
    to_list = function() {
      list(
        task_tag = self$task_tag,
        conf_level = self$conf_level,
        meta_data = self$meta_data,
        stats_params = self$stats_params,
        criteria = self$criteria
      )
    }
  ),
  private = list(
    .validate_conf_level = function(cl) {
      if (!is.numeric(cl) || cl <= 0 || cl >= 1) {
        stop("[Planner] conf_level must be between 0 and 1.", call. = FALSE)
      }
      return(cl)
    }
  )
)
