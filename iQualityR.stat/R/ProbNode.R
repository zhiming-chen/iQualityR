# =============================================================================
# File: R/ProbNode.R
# Description: Probability distribution node class (using registry pattern)
# =============================================================================

#' @title ProbNode: Probability distribution node class
#' @description
#' Handles single distribution mathematical operations (PDF/CDF/Quantile) and parameter validation.
#' Uses distribution registry pattern, d/p/q methods automatically look up corresponding functions from DIST_REGISTRY.
#'
#' @export
ProbNode <- R6::R6Class("ProbNode",
  public = list(
    #' @field id Node unique identifier
    id = NULL,
    #' @field type Distribution type
    type = NULL,
    #' @field params Distribution parameter list
    params = list(),
    #' @field loc Location shift
    loc = 0,
    #' @field is_discrete Whether it is a discrete distribution
    is_discrete = FALSE,
    #' @field last_label_text Most recently generated label text
    last_label_text = NULL,

    #' @description Initialize distribution node
    #' @param id Node unique identifier
    #' @param type Distribution type
    #' @param params Distribution parameter list
    #' @param loc Location shift
    #' @return ProbNode object
    initialize = function(id, type, params, loc = 0) {
      self$id <- id
      self$type <- type

      # Validate and fill parameters
      self$params <- validate_dist_params(type, params)

      self$loc <- loc

      # Get discrete information from registry
      if (type %in% names(DIST_REGISTRY)) {
        self$is_discrete <- DIST_REGISTRY[[type]]$is_discrete
      } else {
        stop("[ProbNode] Unknown distribution type: ", type,
             "\n  Available distributions: ", paste(names(DIST_REGISTRY), collapse = ", "),
             call. = FALSE)
      }
    },

    #' @description Core execution entry: perform calculation based on mode
    #' @param input_val Input value
    #' @param mode Calculation mode ("prob" or "quant")
    #' @param calc_type Calculation type ("lower", "upper", "between", "outside")
    #' @return Structured calculation result
    execute = function(input_val, mode = c("prob", "quant"), calc_type = "lower") {
      mode <- match.arg(mode)
      is_lower <- (calc_type != "upper")

      if (mode == "prob") {
        # Probability mode: known X, find P
        p_val <- self$p(input_val, lower = is_lower)
        return(list(
          mode         = "prob",
          is_prob_mode = TRUE,
          x_val        = input_val,
          val          = p_val,
          target_x     = input_val,
          result_p     = p_val,
          pdf_lbl      = self$gen_label(input_val, p_val, calc_type)
        ))
      } else {
        # Quantile mode: known P, find X
        x_val <- self$q(input_val, lower = is_lower)
        return(list(
          mode         = "quant",
          is_prob_mode = FALSE,
          x_val        = x_val,
          val          = input_val,
          target_p     = input_val,
          result_x     = x_val,
          pdf_lbl      = self$gen_label(x_val, input_val, calc_type)
        ))
      }
    },

    #' @description Calculate probability density/mass function value
    #' @param x Input value
    #' @return Density/mass function value
    d = function(x) {
      reg <- DIST_REGISTRY[[self$type]]
      x_adj <- x - self$loc
      reg$d(x_adj, self$params)
    },

    #' @description Calculate cumulative distribution function value
    #' @param x Input value
    #' @param lower Whether to calculate lower tail probability
    #' @return Cumulative probability
    p = function(x, lower = TRUE) {
      reg <- DIST_REGISTRY[[self$type]]
      x_adj <- x - self$loc
      reg$p(x_adj, self$params, lower.tail = lower)
    },

    #' @description Calculate quantile
    #' @param prob Probability value
    #' @param lower Whether to calculate lower tail quantile
    #' @return Quantile value
    q = function(prob, lower = TRUE) {
      reg <- DIST_REGISTRY[[self$type]]
      val <- reg$q(prob, self$params, lower.tail = lower)
      # Discrete distribution does not involve loc offset
      if (self$is_discrete) val else val + self$loc
    },

    #' @description Generate Minitab-style label text
    #' @param x X value (can be vector)
    #' @param p Probability value
    #' @param calc_type Calculation type
    #' @return Label string
    gen_label = function(x, p, calc_type) {
      if (length(x) == 1) {
        x_fmt <- format(round(x, 4), nsmall = 4)
      } else {
        x_fmt <- format(round(x, 4), nsmall = 4)
      }
      p_fmt <- format(round(p, 4), nsmall = 4)

      res <- switch(calc_type,
        "lower"   = paste0("X \u2264 ", x_fmt, ", P = ", p_fmt),
        "upper"   = paste0("X > ", x_fmt, ", P = ", p_fmt),
        "between" = paste0(x_fmt[1], " \u2264 X \u2264 ", x_fmt[2], ", P = ", p_fmt),
        "outside" = paste0("X < ", x_fmt[1], " or X > ", x_fmt[2], ", P = ", p_fmt)
      )
      self$last_label_text <- res
      res
    },

    #' @description Get distribution metadata
    #' @return Distribution information list
    get_node_info = function() {
      reg <- DIST_REGISTRY[[self$type]]
      list(
        type = self$type,
        description = reg$description,
        support = reg$support,
        is_discrete = self$is_discrete,
        params = self$params
      )
    }
  )
)
