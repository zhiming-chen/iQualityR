# =============================================================================
# File: R/prob/ProbAnalyzer.R
# Description: Probability calculation engine (optimized - split long methods)
# =============================================================================

#' @title ProbAnalyzer: Probability calculation engine
#' @description
#' Handles probability calculation logic: two-tailed probability calculation,
#' quantile loop calculation, shade determination logic.
#' Receives calculation requests from ProbNode, returns structured results.
#'
#' @export
ProbAnalyzer <- R6::R6Class("ProbAnalyzer",
  public = list(
    #' @description Perform analysis
    #' @param node ProbNode object
    #' @param mode Calculation mode ("prob" or "quant")
    #' @param calc_type Calculation type ("lower", "upper", "between", "outside")
    #' @param values Input value vector
    #' @return Structured calculation result
    analyze = function(node, mode, calc_type, values) {
      # Input validation
      if (is.null(values) || length(values) == 0) {
        stop("[ProbAnalyzer] values cannot be empty", call. = FALSE)
      }

      v_orig <- sort(values)

      if (mode == "prob") {
        private$.compute_prob(node, calc_type, v_orig)
      } else {
        private$.compute_quant(node, v_orig)
      }
    }
  ),

  private = list(
    # --- Probability calculation ---

    .compute_prob = function(node, calc_type, values) {
      # Validate calc_type
      valid_types <- c("lower", "upper", "between", "outside")
      if (!calc_type %in% valid_types) {
        stop("[ProbAnalyzer] Invalid calc_type: ", calc_type,
             "\n  Valid values: ", paste(valid_types, collapse = ", "),
             call. = FALSE)
      }

      # Validate values length
      if (calc_type %in% c("between", "outside") && length(values) < 2) {
        stop("[ProbAnalyzer] calc_type = '", calc_type, "' requires at least 2 values",
             call. = FALSE)
      }

      # Calculate probability
      p_val <- private$.calc_probability(node, calc_type, values)

      # Generate label
      lbl <- node$gen_label(values, p_val, calc_type)

      # Build shade function
      shade_f <- private$.build_shade_function(calc_type, values)

      # Return structured result
      list(
        all_res = list(list(
          is_prob_mode = TRUE,
          target_x     = values,
          result_p     = p_val
        )),
        pdf_lbl = lbl,
        shade_f = shade_f,
        mode = "prob"
      )
    },

    # --- Quantile calculation ---

    .compute_quant = function(node, values) {
      # Calculate quantile for each probability value
      all_res <- lapply(values, function(p_in) {
        x_out <- node$q(p_in)
        list(
          is_prob_mode = FALSE,
          target_p     = p_in,
          result_x     = x_out,
          target_x     = x_out
        )
      })

      # Generate label
      lbl <- sprintf("Area = %s", paste(values, collapse = ", "))

      # Shade function: cover to max quantile
      max_x <- max(sapply(all_res, `[[`, "result_x"))
      shade_f <- function(x) x <= max_x

      list(
        all_res = all_res,
        pdf_lbl = lbl,
        shade_f = shade_f,
        mode = "quant"
      )
    },

    # --- Probability calculation helper ---

    .calc_probability = function(node, calc_type, values) {
      switch(calc_type,
        "lower" = node$p(values[1], lower = TRUE),
        "upper" = node$p(values[1], lower = FALSE),
        "between" = node$p(values[2]) - node$p(values[1]),
        "outside" = node$p(values[1], lower = TRUE) + node$p(values[2], lower = FALSE)
      )
    },

    # --- Shade function building ---

    .build_shade_function = function(calc_type, values) {
      switch(calc_type,
        "lower"   = function(x) x <= values[1],
        "upper"   = function(x) x > values[1],
        "between" = function(x) x >= values[1] & x <= values[2],
        "outside" = function(x) x < values[1] | x > values[2]
      )
    }
  )
)
