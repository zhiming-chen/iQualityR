# =============================================================================
# File: R/IqrDoePlan.R
# Description: DOE plan configurator (inherits IqrPlanBase)
# =============================================================================

#' @title DOE Plan Configurator
#' @description
#' Inherits from [IqrPlanBase]. Stores and manages all configuration
#' parameters for a DOE task. Supports factorial, fractional, orthogonal,
#' response surface, and Taguchi designs.
#'
#' @field design_type Character. Design type ("factorial", "fractional",
#'   "orthogonal", "rsm", "ccd", "box_behnken", "taguchi", "lhs", "maximin").
#' @field factors List. Factor configurations (name/type/levels).
#' @field response_vars Character vector. Response variable names.
#' @field replication Integer. Number of replications.
#' @field blocking Logical. Whether to use blocking.
#' @field center_points Integer. Number of center points.
#' @field alpha Numeric. Axial distance for CCD designs.
#' @field resolution Character. Resolution ("III", "IV", "V").
#' @field optimality Character. Optimality criterion ("D", "A", "G").
#' @field randomize Logical. Whether to randomize run order.
#' @field seed Integer. Random seed.
#'
#' @export
IqrDoePlan <- R6::R6Class("IqrDoePlan",
  inherit = IqrPlanBase,

  public = list(
    #' @field design_type Design type.
    design_type = NULL,
    #' @field factors Factor configuration list.
    factors = NULL,
    #' @field response_vars Response variable names.
    response_vars = NULL,
    #' @field replication Number of replications.
    replication = 1L,
    #' @field blocking Whether to use blocking.
    blocking = FALSE,
    #' @field n_blocks Number of blocks (only used when `blocking = TRUE`).
    #'   Defaults to `2L`. When `blocking = TRUE` the design is partitioned
    #'   into `n_blocks` blocks of (nearly) equal size, with run order
    #'   randomized within each block. This implements a Randomized Complete
    #'   Block Design (RCBD) when each block contains the full set of
    #'   factorial treatment combinations, or an Incomplete Block Design
    #'   (IBD) otherwise.
    n_blocks = 2L,
    #' @field center_points Number of center points.
    center_points = 0L,
    #' @field alpha Axial distance (CCD).
    alpha = NULL,
    #' @field resolution Design resolution.
    resolution = NULL,
    #' @field optimality Optimality criterion.
    optimality = "D",
    #' @field randomize Whether to randomize run order.
    randomize = TRUE,
    #' @field seed Random seed.
    seed = NULL,

    #' @description Initialize DOE plan.
    #' @param task_tag Task tag ("factorial", "rsm", "taguchi", "screening").
    #' @param design_type Design type.
    #' @param factors Factor configuration list.
    #' @param response_vars Response variable names.
    #' @param replication Number of replications (default 1).
    #' @param center_points Number of center points (default 0).
    #' @param blocking Whether to use blocking (default FALSE).
    #' @param n_blocks Number of blocks when `blocking = TRUE` (default 2).
    #'   The design is partitioned into `n_blocks` blocks of (nearly) equal
    #'   size, with run order randomized within each block.
    #' @param resolution Resolution ("III", "IV", "V").
    #' @param alpha Axial distance for CCD designs. May be a positive numeric
    #'   scalar or one of the character keywords `"rotatable"`, `"spherical"`,
    #'   `"face_centered"`, `"orthogonal"` (default `"rotatable"` for CCD). The
    #'   value is resolved into a numeric alpha by `DoeAnalyzer` at design
    #'   generation time.
    #' @param optimality Optimality criterion ("D", "A", "G"). Default `"D"`.
    #' @param randomize Whether to randomize (default TRUE).
    #' @param seed Random seed (optional).
    #' @param ... Additional arguments passed to [IqrPlanBase].
    initialize = function(task_tag,
                          design_type,
                          factors,
                          response_vars = NULL,
                          replication = 1,
                          center_points = 0,
                          blocking = FALSE,
                          n_blocks = 2L,
                          resolution = NULL,
                          alpha = NULL,
                          optimality = "D",
                          randomize = TRUE,
                          seed = NULL,
                          ...) {
      super$initialize(task_tag = task_tag, ...)

      self$design_type <- private$.validate_design_type(design_type)
      self$factors <- private$.validate_factors(factors)
      self$response_vars <- response_vars
      self$replication <- private$.validate_positive_int(replication, "replication")
      self$center_points <- private$.validate_nonnegative_int(center_points, "center_points")
      self$blocking <- isTRUE(blocking)
      self$n_blocks <- private$.validate_positive_int(n_blocks, "n_blocks")
      self$alpha <- private$.validate_alpha(alpha)
      self$resolution <- resolution
      self$optimality <- optimality
      self$randomize <- randomize
      self$seed <- seed
    },

    #' @description Validate data and configuration compatibility.
    #' @param data Data frame (optional).
    validate = function(data = NULL) {
      super$validate()

      n_factors <- length(self$factors)
      if (n_factors < 2) {
        stop("[IqrDoePlan] At least 2 factors are required, currently only ",
             n_factors, " provided.", call. = FALSE)
      }

      if (n_factors > 20) {
        warning("[IqrDoePlan] Large number of factors (", n_factors,
                ") may lead to an excessive experiment size.", call. = FALSE)
      }

      private$.validate_design_compatibility()

      if (!is.null(data)) {
        private$.validate_data(data)
      }

      invisible(self)
    },

    #' @description Get all parameters as a list for the Analyzer.
    #' @return List containing all configuration parameters.
    get_all_params = function() {
      list(
        task_tag = self$task_tag,
        design_type = self$design_type,
        factors = self$factors,
        response_vars = self$response_vars,
        replication = self$replication,
        blocking = self$blocking,
        n_blocks = self$n_blocks,
        center_points = self$center_points,
        alpha = self$alpha,
        resolution = self$resolution,
        optimality = self$optimality,
        randomize = self$randomize,
        seed = self$seed,
        conf_level = self$conf_level
      )
    }
  ),

  private = list(
    .validate_design_type = function(dt) {
      valid_types <- c("factorial", "fractional", "orthogonal", "rsm", "ccd",
                       "box_behnken", "taguchi", "lhs", "maximin", "dsd",
                       "simplex_centroid", "simplex_lattice", "extreme_vertices",
                       "split_plot")
      if (!dt %in% valid_types) {
        stop("[IqrDoePlan] Unsupported design type: ", dt,
             "\n  Valid options: ", paste(valid_types, collapse = ", "),
             call. = FALSE)
      }
      dt
    },

    .validate_factors = function(factors) {
      if (!is.list(factors) || length(factors) == 0) {
        stop("[IqrDoePlan] factors must be a non-empty list.", call. = FALSE)
      }

      for (i in seq_along(factors)) {
        factor <- factors[[i]]

        required_fields <- c("name", "type", "levels")
        missing <- setdiff(required_fields, names(factor))
        if (length(missing) > 0) {
          stop("[IqrDoePlan] Factor ", i, " is missing fields: ",
               paste(missing, collapse = ", "), call. = FALSE)
        }

        if (!factor$type %in% c("continuous", "categorical")) {
          stop("[IqrDoePlan] Factor type must be 'continuous' or 'categorical'.",
               call. = FALSE)
        }

        if (!is.numeric(factor$levels) || length(factor$levels) < 2) {
          stop("[IqrDoePlan] Factor levels must be a numeric vector of length >= 2.",
               call. = FALSE)
        }
      }

      factors
    },

    .validate_positive_int = function(val, name) {
      if (!is.numeric(val) || val < 1 || val != as.integer(val)) {
        stop("[IqrDoePlan] ", name, " must be a positive integer.", call. = FALSE)
      }
      as.integer(val)
    },

    .validate_nonnegative_int = function(val, name) {
      if (!is.numeric(val) || val < 0 || val != as.integer(val)) {
        stop("[IqrDoePlan] ", name, " must be a non-negative integer.",
             call. = FALSE)
      }
      as.integer(val)
    },

    .validate_design_compatibility = function() {
      if (!is.null(self$resolution)) {
        if (!self$design_type %in% c("fractional", "orthogonal")) {
          warning("[IqrDoePlan] Resolution parameter only applies to fractional/orthogonal designs.",
                  call. = FALSE)
        }
        if (!self$resolution %in% c("III", "IV", "V")) {
          stop("[IqrDoePlan] Resolution must be 'III', 'IV', or 'V'.",
               call. = FALSE)
        }
      }

      if (self$center_points > 0 && self$design_type == "orthogonal") {
        warning("[IqrDoePlan] Center points are typically not added to orthogonal designs.",
                call. = FALSE)
      }

      # When alpha is NULL for CCD/RSM designs, default to "rotatable".
      # The numeric value is resolved by DoeAnalyzer at design generation
      # time, because the value depends on the number of factors.
      if (is.null(self$alpha) &&
          self$design_type %in% c("ccd", "rsm", "box_behnken")) {
        self$alpha <- "rotatable"
      }
    },

    .validate_alpha = function(alpha) {
      if (is.null(alpha)) {
        return(NULL)
      }
      if (is.character(alpha) && length(alpha) == 1) {
        valid_keywords <- c("rotatable", "spherical", "face_centered", "orthogonal")
        if (!alpha %in% valid_keywords) {
          stop("[IqrDoePlan] alpha keyword must be one of: ",
               paste(valid_keywords, collapse = ", "), call. = FALSE)
        }
        return(alpha)
      }
      if (is.numeric(alpha) && length(alpha) == 1 && alpha > 0) {
        return(alpha)
      }
      stop("[IqrDoePlan] alpha must be a positive numeric scalar or one of: ",
           "rotatable, spherical, face_centered, orthogonal", call. = FALSE)
    },

    .validate_data = function(data) {
      if (!is.data.frame(data)) {
        stop("[IqrDoePlan] data must be a data frame.", call. = FALSE)
      }

      if (!is.null(self$response_vars)) {
        missing_responses <- setdiff(self$response_vars, names(data))
        if (length(missing_responses) > 0) {
          stop("[IqrDoePlan] The following response variables are not in the data: ",
               paste(missing_responses, collapse = ", "), call. = FALSE)
        }
      }
    }
  )
)
