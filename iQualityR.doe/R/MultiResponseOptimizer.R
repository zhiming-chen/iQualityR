# =============================================================================
# File: R/MultiResponseOptimizer.R
# Description: Multi-response optimizer - Desirability function method +
#              Pareto frontier analysis
# Theory: Derringer & Suich (1980), Multi-Objective Optimization
# Applications: Multi-response process optimization for injection molding,
#               heat treatment, food, pharmaceutical, chemical, welding,
#               and electronics SMT processes
# =============================================================================

#' @title MultiResponseOptimizer: Multi-Response Optimizer
#' @description
#' Implements two complementary approaches for multi-response optimization:
#' 1. **Desirability function method**: Maps each response onto a common `[0, 1]`
#'    scale and aggregates them via the weighted geometric mean.
#' 2. **Pareto frontier analysis**: Identifies the non-dominated solution set
#'    to reveal trade-offs between competing responses.
#'
#' @details
#' **Theoretical references**:
#' - Derringer, G., & Suich, R. (1980). Simultaneous Optimization of Several
#'   Response Variables. *Journal of Quality Technology*, 12(4), 214-219.
#' - Deb, K. (2001). *Multi-Objective Optimization Using Evolutionary
#'   Algorithms*.
#'
#' @export
MultiResponseOptimizer <- R6::R6Class("MultiResponseOptimizer",
  public = list(

    #' @description Create a new MultiResponseOptimizer instance.
    #' @return An `MultiResponseOptimizer` instance (invisibly).
    initialize = function() {
      invisible(self)
    },

    # =========================================================================
    # Method 1: Desirability function method
    # =========================================================================

    #' @description Compute the individual desirability value for a single
    #'   response based on its specification type.
    #' @param y Numeric vector of predicted response values.
    #' @param type Character scalar specifying the desirability type:
    #'   `"max"` (larger-the-better), `"min"` (smaller-the-better), or
    #'   `"target"` (nominal-the-best).
    #' @param lower Numeric scalar, the acceptable lower bound. Required for
    #'   all types.
    #' @param upper Numeric scalar, the acceptable upper bound. Required for
    #'   all types.
    #' @param target Numeric scalar, the target value. Required only when
    #'   `type = "target"`.
    #' @param s Numeric scalar, shape parameter (`> 1` is stricter, `< 1` is
    #'   more lenient, default `1`).
    #' @return Numeric vector of desirability values in `[0, 1]`.
    compute_individual_desirability = function(y, type = "max",
                                               lower = NULL, upper = NULL,
                                               target = NULL, s = 1) {
      d <- numeric(length(y))

      if (type == "max") {
        # Larger-the-better characteristic
        if (is.null(lower) || is.null(upper)) {
          stop("[MRO] Both 'lower' and 'upper' must be specified for type='max'")
        }
        d[y <= lower] <- 0
        d[y >= upper] <- 1
        in_range <- y > lower & y < upper
        d[in_range] <- ((y[in_range] - lower) /
                        (upper - lower + .Machine$double.eps))^s

      } else if (type == "min") {
        # Smaller-the-better characteristic
        if (is.null(lower) || is.null(upper)) {
          stop("[MRO] Both 'lower' and 'upper' must be specified for type='min'")
        }
        d[y >= upper] <- 0
        d[y <= lower] <- 1
        in_range <- y > lower & y < upper
        d[in_range] <- ((upper - y[in_range]) /
                        (upper - lower + .Machine$double.eps))^s

      } else if (type == "target") {
        # Nominal-the-best characteristic
        if (is.null(target) || is.null(lower) || is.null(upper)) {
          stop("[MRO] 'target', 'lower', and 'upper' must be specified for type='target'")
        }
        s1 <- s; s2 <- s  # Left and right shape parameters may be set separately

        for (i in seq_along(y)) {
          if (y[i] < lower || y[i] > upper) {
            d[i] <- 0
          } else if (y[i] < target) {
            d[i] <- ((y[i] - lower) / (target - lower + .Machine$double.eps))^s1
          } else if (y[i] > target) {
            d[i] <- ((upper - y[i]) / (upper - target + .Machine$double.eps))^s2
          } else {
            d[i] <- 1
          }
        }
      } else {
        stop("[MRO] type must be 'max', 'min', or 'target'")
      }

      # Boundary protection
      d <- pmax(pmin(d, 1), 0)
      d
    },

    #' @description Compute the overall desirability index as the weighted
    #'   geometric mean of individual desirability values.
    #' @param desirabilities List whose elements are numeric desirability
    #'   vectors, one per response.
    #' @param weights Numeric vector of response weights (default equal
    #'   weights).
    #' @param tol Numeric scalar, tolerance applied to guard against a single
    #'   zero collapsing the geometric mean (default `0`).
    #' @return Numeric vector of overall desirability values in `[0, 1]`.
    compute_overall_desirability = function(desirabilities, weights = NULL, tol = 0) {
      n_responses <- length(desirabilities)
      n_obs <- length(desirabilities[[1]])

      # Validate input
      if (n_responses == 0) stop("[MRO] desirabilities must not be empty")
      for (i in seq_along(desirabilities)) {
        if (length(desirabilities[[i]]) != n_obs) {
          stop("[MRO] All desirability vectors must have the same length")
        }
      }

      # Default weights
      if (is.null(weights)) {
        weights <- rep(1, n_responses)
      }
      if (length(weights) != n_responses) {
        stop("[MRO] Length of weights must equal the number of responses")
      }

      # Normalize weights
      weights <- weights / sum(weights)

      # Apply tolerance (prevent a zero from collapsing the geometric mean)
      if (tol > 0) {
        desirabilities <- lapply(desirabilities, function(d) pmax(d, tol))
      }

      # Compute weighted geometric mean
      overall <- rep(1, n_obs)
      for (i in seq_along(desirabilities)) {
        overall <- overall * (desirabilities[[i]] ^ weights[i])
      }

      overall
    },

    #' @description Optimize the overall desirability index over the factor
    #'   search space using either grid search or numerical optimization.
    #' @param models List of `lm`/`rsm` model objects, one per response.
    #' @param specs List of response specifications, each containing
    #'   `lower`, `upper`, `target`, `type`, `weight`, and optionally `s`.
    #' @param bounds List defining the search bounds for the process factors.
    #'   Either a named list of `c(lower, upper)` pairs or a legacy
    #'   `list(lower = c(...), upper = c(...))`.
    #' @param method Character scalar selecting the optimization method:
    #'   `"grid"` (grid search) or `"optim"` (numerical optimization).
    #'   Default `"grid"`.
    #' @param grid_resolution Integer scalar, grid resolution (only used when
    #'   `method = "grid"`, default `20`).
    #' @param tol Numeric scalar, tolerance passed to the desirability
    #'   aggregation (default `0.01`).
    #' @return A list containing `best_params`, `best_D`, and
    #'   `full_results` (for grid search) or `convergence` and `message`
    #'   (for numerical optimization).
    optimize_desirability = function(models, specs, bounds,
                                     method = "grid", grid_resolution = 20,
                                     tol = 0.01) {
      # Support both bounds formats
      if (is.null(names(bounds)) || all(c("lower", "upper") %in% names(bounds))) {
        n_factors <- length(bounds$lower)
        factor_names <- names(bounds$lower)
      } else {
        n_factors <- length(bounds)
        factor_names <- names(bounds)
      }

      if (method == "grid") {
        # Grid search (suitable for low dimensions, <= 5 factors)
        result <- private$.optimize_grid(models, specs, bounds, grid_resolution, tol)
      } else if (method == "optim") {
        # Numerical optimization (suitable for higher dimensions)
        result <- private$.optimize_numerical(models, specs, bounds, tol)
      } else {
        stop("[MRO] method must be 'grid' or 'optim'")
      }

      result
    },

    # =========================================================================
    # Method 2: Pareto frontier analysis
    # =========================================================================

    #' @description Identify the Pareto frontier (non-dominated solution set)
    #'   from a set of candidate response predictions.
    #' @param responses Data frame where each column holds the predicted
    #'   values of one response.
    #' @param directions Character vector specifying the optimization direction
    #'   for each response (`"max"`, `"min"`, or `"target"`).
    #' @param targets Numeric vector of target values, used only for the
    #'   responses whose `directions` entry is `"target"`. May be `NULL`
    #'   (the default) when no direction is `"target"`; otherwise its length
    #'   must equal `ncol(responses)` and the entries corresponding to
    #'   `"target"` directions must be finite numbers.
    #' @return A list containing `pareto_frontier` (data frame of non-dominated
    #'   solutions with a `.Pareto_Index` column), `n_solutions`,
    #'   `n_total`, `coverage_pct`, and `is_dominated` (a logical vector
    #'   of length `n_total` flagging which rows are dominated).
    find_pareto_frontier = function(responses, directions, targets = NULL) {
      if (!is.data.frame(responses)) {
        responses <- as.data.frame(responses)
      }

      n_obs  <- nrow(responses)
      n_resp <- ncol(responses)

      if (length(directions) != n_resp) {
        stop("[MRO] Length of directions must equal the number of response columns")
      }

      # Validate directions. "target" is supported as well as "max" / "min".
      invalid_dirs <- setdiff(unique(directions), c("max", "min", "target"))
      if (length(invalid_dirs) > 0) {
        stop("[MRO] directions must be 'max', 'min', or 'target'; found: ",
             paste(invalid_dirs, collapse = ", "))
      }

      # Validate targets for any "target" direction.
      target_dirs <- which(directions == "target")
      if (length(target_dirs) > 0) {
        if (is.null(targets)) {
          stop("[MRO] 'targets' must be supplied when any direction is 'target'")
        }
        if (length(targets) != n_resp) {
          stop("[MRO] Length of 'targets' must equal the number of response columns")
        }
        for (j in target_dirs) {
          if (is.null(targets[[j]]) || is.na(targets[j]) ||
              !is.finite(targets[j])) {
            stop("[MRO] targets[", j,
                 "] must be a finite number for direction='target'")
          }
        }
      }

      # Normalize: convert all responses to a single maximization problem.
      #   - "max":    keep as-is.
      #   - "min":    negate so that "smaller y" becomes "larger -y".
      #   - "target": transform to maximize -|y - target|, i.e. solutions
      #               closer to the target dominate those further away.
      #               This is the standard reduction used by multi-objective
      #               optimizers (Deb 2001) when nominal-the-best responses
      #               are part of a Pareto trade-off analysis.
      resp_normalized <- responses
      for (j in seq_along(directions)) {
        if (directions[j] == "min") {
          resp_normalized[[j]] <- -resp_normalized[[j]]
        } else if (directions[j] == "target") {
          resp_normalized[[j]] <- -abs(resp_normalized[[j]] - targets[j])
        }
      }

      # Non-dominated sorting: find the first Pareto front.
      # An O(n^2) comparison is acceptable here because Pareto frontier
      # analysis is typically applied to a few hundred candidate points at
      # most (the output of a grid search or a small DOE). For very large
      # candidate sets the user should subsample before calling this method.
      is_dominated <- logical(n_obs)

      for (i in seq_len(n_obs)) {
        if (is_dominated[i]) next

        for (j in seq_len(n_obs)) {
          if (i == j || is_dominated[j]) next

          # j dominates i iff j is at least as good as i in every dimension
          # and strictly better in at least one dimension.
          row_i <- as.numeric(resp_normalized[i, ])
          row_j <- as.numeric(resp_normalized[j, ])
          dominates <- all(row_j >= row_i) && any(row_j > row_i)

          if (dominates) {
            is_dominated[i] <- TRUE
            break
          }
        }
      }

      # Return Pareto frontier solutions.
      pareto_set <- responses[!is_dominated, , drop = FALSE]
      pareto_set$.Pareto_Index <- seq_len(sum(!is_dominated))

      list(
        pareto_frontier = pareto_set,
        is_dominated    = is_dominated,
        n_solutions     = sum(!is_dominated),
        n_total         = n_obs,
        coverage_pct    = round(sum(!is_dominated) / n_obs * 100, 1)
      )
    },

    #' @description Visualize the Pareto frontier as a 2D scatter plot,
    #'   optionally colored by a third response. Dominated points can be
    #'   shown as a faded backdrop so the trade-off structure of the
    #'   candidate set is visible at a glance.
    #' @param pareto_result Return value of `find_pareto_frontier()`.
    #' @param x_axis Character scalar, column name of the response to plot on
    #'   the x-axis.
    #' @param y_axis Character scalar, column name of the response to plot on
    #'   the y-axis.
    #' @param z_axis Character scalar, optional column name used for color
    #'   mapping (a third response).
    #' @param show_dominated Logical scalar; when `TRUE` (default), also
    #'   overlay the dominated candidate points in light gray so the frontier
    #'   stands out from the full candidate set.
    #' @param all_responses Data frame of all candidate responses. Required
    #'   only when `show_dominated = TRUE` and `pareto_result` does not carry
    #'   the full candidate set. When `NULL` (the default), dominated points
    #'   are extracted from `pareto_result` if available; otherwise no
    #'   backdrop is drawn.
    #' @param theme_obj An `IqrTheme` object used to style the plot. May be
    #'   `NULL`, in which case `ggplot2::theme_minimal()` is used.
    #' @return A `ggplot` object (invisibly).
    plot_pareto_frontier = function(pareto_result, x_axis, y_axis,
                                    z_axis = NULL, show_dominated = TRUE,
                                    all_responses = NULL, theme_obj = NULL) {
      if (is.null(pareto_result$pareto_frontier)) {
        stop("[MRO] Invalid pareto_result. Use find_pareto_frontier() first.")
      }

      df <- pareto_result$pareto_frontier

      if (!x_axis %in% names(df) || !y_axis %in% names(df)) {
        stop("[MRO] x_axis and y_axis must be column names in responses")
      }

      # Optionally build a backdrop of dominated points so the frontier is
      # visually distinguishable from the candidate set. The backdrop is
      # drawn first so the frontier points overlay it.
      dominated_df <- NULL
      if (show_dominated) {
        if (!is.null(all_responses)) {
          # User-supplied candidate set: every row that is not on the frontier
          # is dominated. Match by row position when lengths align.
          if (nrow(all_responses) == pareto_result$n_total) {
            is_dom <- rep(TRUE, nrow(all_responses))
            is_dom[!pareto_result$is_dominated] <- FALSE
            dominated_df <- all_responses[is_dom, , drop = FALSE]
          } else {
            warning("[MRO] all_responses row count does not match pareto_result$n_total; skipping dominated backdrop.")
          }
        } else if (!is.null(pareto_result$is_dominated) &&
                   !is.null(attr(pareto_result, "responses"))) {
          # Some internal callers may attach the original responses; use them.
          responses_attr <- attr(pareto_result, "responses")
          dominated_df <- responses_attr[pareto_result$is_dominated, , drop = FALSE]
        }
      }

      p <- ggplot()

      # Dominated backdrop (faded gray).
      if (!is.null(dominated_df) && nrow(dominated_df) > 0) {
        p <- p +
          geom_point(data = dominated_df,
                     aes(x = .data[[x_axis]], y = .data[[y_axis]]),
                     size = 2, color = "gray70", alpha = 0.5)
      }

      # Frontier points (highlighted).
      p <- p +
        geom_point(data = df,
                   aes(x = .data[[x_axis]], y = .data[[y_axis]]),
                   size = 3, color = "#2C7BB6", alpha = 0.9) +
        geom_point(data = df,
                   aes(x = .data[[x_axis]], y = .data[[y_axis]]),
                   size = 5, shape = 21, color = "#D9534F", fill = NA) +
        labs(
          title = "Pareto Frontier",
          subtitle = sprintf("%d non-dominated solutions out of %d candidates (%.1f%%)",
                           pareto_result$n_solutions,
                           pareto_result$n_total,
                           pareto_result$coverage_pct),
          x = x_axis,
          y = y_axis
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 14),
          panel.grid.minor = element_blank()
        )

      # If a z-axis is supplied, add a color mapping on top of the frontier.
      if (!is.null(z_axis) && z_axis %in% names(df)) {
        p <- p +
          aes(color = .data[[z_axis]]) +
          scale_color_viridis_c(option = "C")
      }

      p <- private$.apply_theme(p, theme_obj)
      invisible(p)
    },

    # =========================================================================
    # Method 3: Multi-criteria decision making (MCDM) assistance
    # =========================================================================

    #' @description Rank candidate solutions using the TOPSIS
    #'   (Technique for Order Preference by Similarity to Ideal Solution)
    #'   method.
    #' @param candidates Data frame where each column is one criterion
    #'   (response value).
    #' @param weights Numeric vector of criterion weights (default equal
    #'   weights).
    #' @param directions Character vector of optimization directions for each
    #'   criterion (`"max"`, `"min"`, or `"target"`).
    #' @param targets Numeric vector of target values, used only for criteria
    #'   whose `directions` entry is `"target"`. May be `NULL` (the default)
    #'   when no direction is `"target"`; otherwise its length must equal
    #'   `ncol(candidates)` and the entries corresponding to `"target"`
    #'   directions must be finite numbers.
    #' @return A list containing `ranking` (a data frame with `Candidate`,
    #'   `Closeness`, and `Rank` columns), `ideal_best`, and `ideal_worst`.
    rank_with_topsis = function(candidates, weights = NULL, directions = NULL,
                                targets = NULL) {
      if (!is.data.frame(candidates)) {
        candidates <- as.data.frame(candidates)
      }

      n_candidates <- nrow(candidates)
      n_criteria   <- ncol(candidates)

      # Default weights
      if (is.null(weights)) weights <- rep(1, n_criteria)
      if (length(weights) != n_criteria) {
        stop("[MRO] Length of weights must equal the number of criteria")
      }

      # Default directions
      if (is.null(directions)) directions <- rep("max", n_criteria)
      if (length(directions) != n_criteria) {
        stop("[MRO] Length of directions must equal the number of criteria")
      }

      # Validate directions
      invalid_dirs <- setdiff(unique(directions), c("max", "min", "target"))
      if (length(invalid_dirs) > 0) {
        stop("[MRO] directions must be 'max', 'min', or 'target'; found: ",
             paste(invalid_dirs, collapse = ", "))
      }

      # Validate targets for "target" directions
      target_dirs <- which(directions == "target")
      if (length(target_dirs) > 0) {
        if (is.null(targets)) {
          stop("[MRO] 'targets' must be supplied when any direction is 'target'")
        }
        if (length(targets) != n_criteria) {
          stop("[MRO] Length of 'targets' must equal the number of criteria")
        }
        for (j in target_dirs) {
          if (is.null(targets[[j]]) || is.na(targets[j]) ||
              !is.finite(targets[j])) {
            stop("[MRO] targets[", j,
                 "] must be a finite number for direction='target'")
          }
        }
      }

      # Step 1: vector normalization.
      norm_matrix <- as.matrix(candidates)
      col_sums_sq <- sqrt(colSums(norm_matrix^2))
      # Avoid division by zero for constant columns
      col_sums_sq[col_sums_sq == 0] <- 1
      normalized <- sweep(norm_matrix, 2, col_sums_sq, "/")

      # Step 2: weighted normalization.
      weighted_normalized <- sweep(normalized, 2, weights, "*")

      # Step 3: determine ideal and negative-ideal solutions.
      # For "target" criteria, the ideal is the target value (normalized
      # and weighted), and the negative-ideal is the candidate furthest from
      # the target. This follows the standard TOPSIS extension for
      # nominal-the-best criteria (Hwang & Yoon 1981, sec. 5.3).
      ideal_best  <- numeric(n_criteria)
      ideal_worst <- numeric(n_criteria)

      for (j in seq_along(directions)) {
        if (directions[j] == "max") {
          ideal_best[j]  <- max(weighted_normalized[, j])
          ideal_worst[j] <- min(weighted_normalized[, j])
        } else if (directions[j] == "min") {
          ideal_best[j]  <- min(weighted_normalized[, j])
          ideal_worst[j] <- max(weighted_normalized[, j])
        } else if (directions[j] == "target") {
          # Normalized target value on the weighted scale
          target_norm <- (targets[j] / col_sums_sq[j]) * weights[j]
          ideal_best[j]  <- target_norm
          # Negative-ideal: the candidate furthest from the target
          distances <- abs(weighted_normalized[, j] - target_norm)
          ideal_worst[j] <- weighted_normalized[which.max(distances), j]
        }
      }

      # Step 4: compute distances to ideal and negative-ideal solutions.
      dist_best  <- sqrt(rowSums((weighted_normalized -
        matrix(ideal_best, nrow = n_candidates, ncol = n_criteria, byrow = TRUE))^2))
      dist_worst <- sqrt(rowSums((weighted_normalized -
        matrix(ideal_worst, nrow = n_candidates, ncol = n_criteria, byrow = TRUE))^2))

      # Step 5: compute relative closeness.
      closeness <- dist_worst / (dist_best + dist_worst + .Machine$double.eps)

      # Ranking
      ranking <- data.frame(
        Candidate = seq_len(n_candidates),
        Closeness = round(closeness, 4),
        Rank = rank(-closeness, ties.method = "min"),
        stringsAsFactors = FALSE
      )
      ranking <- ranking[order(ranking$Rank), ]

      list(
        ranking = ranking,
        ideal_best = setNames(ideal_best, names(candidates)),
        ideal_worst = setNames(ideal_worst, names(candidates))
      )
    }
  ),

  private = list(
    # =========================================================================
    # Apply the IqrTheme to a ggplot object when available. Falls back to the
    # plot's existing theme (theme_minimal) if theme_obj is NULL or does not
    # expose a plot theme method. The theme_obj should be applied when
    # available to keep plots consistent with the rest of the ecosystem.
    # =========================================================================
    .apply_theme = function(p, theme_obj) {
      if (!is.null(theme_obj) && !is.null(theme_obj$plot)) {
        tryCatch({
          p <- p + theme_obj$plot$theme_iqr()
        }, error = function(e) {
          # theme_obj does not provide theme_iqr(); keep the default theme.
        })
      }
      p
    },

    # =========================================================================
    # Grid search optimization
    # =========================================================================
    .optimize_grid = function(models, specs, bounds, resolution, tol) {
      # Support both formats:
      # 1. Named list: list(P1_temp = c(220, 260), P3_time = c(45, 75))
      # 2. Legacy: list(lower = c(...), upper = c(...))

      if (is.null(names(bounds)) || all(c("lower", "upper") %in% names(bounds))) {
        # Legacy format
        n_factors <- length(bounds$lower)
        factor_names <- names(bounds$lower)
        grid_list <- lapply(seq_len(n_factors), function(i) {
          seq(bounds$lower[i], bounds$upper[i], length.out = resolution)
        })
      } else {
        # Named list format (vignette style)
        n_factors <- length(bounds)
        factor_names <- names(bounds)
        grid_list <- lapply(bounds, function(b) {
          seq(b[1], b[2], length.out = resolution)
        })
      }
      names(grid_list) <- factor_names
      grid <- expand.grid(grid_list, stringsAsFactors = FALSE)

      # Predict each response
      predictions <- list()
      for (i in seq_along(models)) {
        predictions[[i]] <- predict(models[[i]], newdata = grid)
      }

      # Compute individual desirability for each response
      desirabilities <- list()
      for (i in seq_along(specs)) {
        spec <- specs[[i]]
        d <- self$compute_individual_desirability(
          y = predictions[[i]],
          type = spec$type,
          lower = spec$lower,
          upper = spec$upper,
          target = spec$target,
          s = ifelse(is.null(spec$s), 1, spec$s)
        )
        desirabilities[[i]] <- d
      }

      # Compute overall desirability
      weights <- sapply(specs, function(s) ifelse(is.null(s$weight), 1, s$weight))
      overall <- self$compute_overall_desirability(desirabilities, weights, tol)

      # Find the best solution
      best_idx <- which.max(overall)
      best_D <- overall[best_idx]
      best_params <- as.list(grid[best_idx, ])

      # Assemble results
      result_grid <- grid
      result_grid$Overall_Desirability <- overall
      for (i in seq_along(predictions)) {
        result_grid[[paste0("Response_", i)]] <- predictions[[i]]
        result_grid[[paste0("D_", i)]] <- desirabilities[[i]]
      }

      list(
        best_params = best_params,
        best_D = best_D,
        n_evaluations = nrow(grid),
        full_results = result_grid
      )
    },

    # =========================================================================
    # Numerical optimization (L-BFGS-B)
    # =========================================================================
    .optimize_numerical = function(models, specs, bounds, tol) {
      # Support both formats
      if (is.null(names(bounds)) || all(c("lower", "upper") %in% names(bounds))) {
        # Legacy format
        n_factors <- length(bounds$lower)
        factor_names <- names(bounds$lower)
        lower_bounds <- bounds$lower
        upper_bounds <- bounds$upper
      } else {
        # Named list format
        n_factors <- length(bounds)
        factor_names <- names(bounds)
        lower_bounds <- sapply(bounds, function(b) b[1])
        upper_bounds <- sapply(bounds, function(b) b[2])
      }

      # Objective function (negative desirability, since optim minimizes)
      neg_desirability <- function(params) {
        x <- as.data.frame(t(params))
        colnames(x) <- factor_names

        predictions <- lapply(models, function(m) predict(m, newdata = x))

        desirabilities <- list()
        for (i in seq_along(specs)) {
          spec <- specs[[i]]
          d <- self$compute_individual_desirability(
            y = predictions[[i]],
            type = spec$type,
            lower = spec$lower,
            upper = spec$upper,
            target = spec$target,
            s = ifelse(is.null(spec$s), 1, spec$s)
          )
          desirabilities[[i]] <- d
        }

        weights <- sapply(specs, function(s) ifelse(is.null(s$weight), 1, s$weight))
        D <- self$compute_overall_desirability(desirabilities, weights, tol)

        -D  # Convert maximization to minimization
      }

      # Initial point (center of the bounds)
      start <- (lower_bounds + upper_bounds) / 2

      # Optimize
      opt_result <- optim(
        par = start,
        fn = neg_desirability,
        method = "L-BFGS-B",
        lower = lower_bounds,
        upper = upper_bounds,
        control = list(maxit = 1000)
      )

      list(
        best_params = as.list(setNames(opt_result$par, factor_names)),
        best_D = -opt_result$value,
        convergence = opt_result$convergence,
        message = ifelse(opt_result$convergence == 0, "Optimization successful", "Optimization may not have converged")
      )
    }
  )
)
