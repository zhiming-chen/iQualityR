# =============================================================================
# File: R/IqrDoeTask.R
# Description: DOE task coordinator (inherits IqrTaskBase)
# =============================================================================

#' @title DOE Task Coordinator
#' @description
#' Coordinates the full DOE workflow: design generation, ANOVA, effect
#' estimation, plotting, and reporting. Inherits from [IqrTaskBase].
#'
#' @field plan An `IqrDoePlan` object.
#'
#' @export
IqrDoeTask <- R6::R6Class("IqrDoeTask",
  inherit = IqrTaskBase,
  public = list(
    #' @field plan IqrDoePlan object.
    plan = NULL,

    #' @description Create a task instance.
    #' @param plan An `IqrDoePlan` object.
    #' @param data Data frame (optional, contains response data). When provided,
    #'   it is stored on `self$data` and used by `$compute()` for ANOVA.
    #' @param theme Theme name or [IqrTheme] object.
    #' @param ... Additional arguments passed to the base class.
    initialize = function(plan, data = NULL, theme = "academic", ...) {
      super$initialize(data, theme, ...)
      self$plan <- plan

      self$executor$analyzer <- DoeAnalyzer$new()
      self$executor$plotter  <- DoePlotter$new()
      self$executor$reporter <- DoeReporter$new()
    },

    #' @description Execute DOE computation.
    #' @return Self (invisibly). Results stored in `self$results`.
    compute = function() {
      self$results <- self$executor$analyzer$run(
        data = self$data,
        plan = self$plan
      )
      invisible(self)
    },

    #' @description Print summary information.
    #' @return Self (invisibly).
    summary = function() {
      if (is.null(self$results)) {
        cat("No results yet. Run $compute() first.\n")
        return(invisible(self))
      }

      cat("\n========== Design of Experiments Summary ==========\n")
      cat(sprintf("Design Type: %s\n", self$plan$design_type))
      cat(sprintf("Number of Factors: %d\n", length(self$plan$factors)))

      cat("\n--- Factor Configuration ---\n")
      for (i in seq_along(self$plan$factors)) {
        factor <- self$plan$factors[[i]]
        cat(sprintf("  %s (%s): %s\n",
                    factor$name,
                    factor$type,
                    paste(factor$levels, collapse = ", ")))
      }

      cat(sprintf("\nReplication: %d\n", self$plan$replication))
      cat(sprintf("Center Points: %d\n", self$plan$center_points))

      if (!is.null(self$plan$resolution)) {
        cat(sprintf("Resolution: %s\n", self$plan$resolution))
      }

      if (!is.null(self$results$design_info)) {
        cat(sprintf("\nTotal Runs: %d\n", nrow(self$results$design_info)))
      }

      if (!is.null(self$results$anova_results)) {
        for (resp in names(self$results$anova_results)) {
          cat("\n--- ANOVA Results:", resp, "---\n")
          anova_table <- self$results$anova_results[[resp]]$anova
          if (!is.null(anova_table)) {
            print(anova_table)
          }

          fit <- self$results$anova_results[[resp]]$model_fit
          if (!is.null(fit)) {
            cat("\nModel Fit Statistics:\n")
            cat(sprintf("  R-squared: %.4f\n", fit$r_squared))
            cat(sprintf("  Adjusted R-squared: %.4f\n", fit$adj_r_squared))
            if (!is.null(fit$lack_of_fit_p) && !is.na(fit$lack_of_fit_p)) {
              cat(sprintf("  Lack-of-fit p-value: %.4f\n", fit$lack_of_fit_p))
            }
          }
        }
      }

      # Report the stationary point (canonical analysis) for RSM designs.
      if (!is.null(self$results$stationary_point)) {
        sp <- self$results$stationary_point
        cat("\n--- Stationary Point (Canonical Analysis) ---\n")
        if (isTRUE(sp$converged)) {
          x_s <- sp$stationary_point
          for (nm in names(x_s)) {
            cat(sprintf("  %s = %.4f\n", nm, x_s[[nm]]))
          }
          cat(sprintf("\n  Nature: %s\n", sp$nature))
          cat(sprintf("  Predicted response: %.4f\n", sp$predicted_response))
          if (!anyNA(sp$prediction_interval)) {
            cat(sprintf("  95%% prediction interval: [%.4f, %.4f]\n",
                        sp$prediction_interval["lower"],
                        sp$prediction_interval["upper"]))
          }
          cat(sprintf("\n  Eigenvalues of B: %s\n",
                      paste(sprintf("%.4f", sp$eigenvalues), collapse = ", ")))
        } else {
          cat("  B matrix is singular; stationary point could not be computed.\n")
        }
      }

      cat("======================================================\n")
      invisible(self)
    },

    #' @description Generate plots.
    #' @param type Plot type: "design", "main_effects", "interaction",
    #'   "residual", "full", "response_surface", "surface", "contour",
    #'   "overlaid_contour", "half_normal", or "pareto_effects".
    #'   - `"residual"` returns a 3-panel patchwork (Residuals vs Fitted +
    #'     Normal Q-Q + Scale-Location) matching the standard diagnostic
    #'     panel used by Minitab / Design-Expert / R's `plot.lm`.
    #'   - `"full"` returns a 3-row patchwork combining `main_effects`,
    #'     `interaction`, and `residual` panels.
    #'   - `"response_surface"` returns a list with `surface_plot`,
    #'     `contour_plot`, `combined_plot`, and `interactive_plot` (if plotly
    #'     is available).
    #'   - `"surface"` returns the 2D tile + contour surface plot only.
    #'   - `"contour"` returns the filled contour plot only.
    #'   - `"overlaid_contour"` returns an overlaid contour plot for
    #'     multi-response optimization. Requires `response_specs`: a named
    #'     list where each element has `$model`, `$lower`, `$upper` (and
    #'     optionally `$target`, `$color`). The feasible region (where all
    #'     responses meet specs) is highlighted in green.
    #'   - `"half_normal"` returns a half-normal probability plot of effect
    #'     estimates with Lenth (1989) ME and SME reference lines, used to
    #'     identify active effects in unreplicated factorials.
    #'   - `"pareto_effects"` returns a Pareto chart of absolute effect
    #'     estimates with the Lenth ME reference line.
    #' @param theme Theme name or [IqrTheme] object (optional override).
    #' @param ... Additional parameters passed to the plotter. For response
    #'   surface types, `response_name` (character scalar) may be supplied to
    #'   select a specific response variable; otherwise the first response
    #'   is used. `x_var` and `y_var` (character scalars) may be supplied to
    #'   select which two factors appear on the axes; otherwise the first
    #'   two numeric factors are used. For `"overlaid_contour"`,
    #'   `response_specs` (named list) is required.
    #' @return A ggplot, patchwork, or list object (see `type`).
    plot = function(type = "design", theme = NULL, ...) {
      if (is.null(self$results)) {
        stop("No results. Run $compute() first.", call. = FALSE)
      }

      theme_obj_use <- if (!is.null(theme)) {
        if (inherits(theme, "IqrTheme")) theme else IqrTheme$new(theme)
      } else {
        self$theme_obj
      }

      self$executor$plotter$render(
        results = self$results,
        theme_obj = theme_obj_use,
        type = type,
        plan = self$plan,
        ...
      )
    },

    #' @description Generate a report.
    #' @param format Output format: "excel" or "html".
    #' @param path Output file path.
    #' @param ... Additional parameters passed to the reporter.
    #' @return Self (invisibly).
    report = function(format = "excel", path = NULL, ...) {
      if (is.null(self$results)) {
        stop("No results. Run $compute() first.", call. = FALSE)
      }

      self$executor$reporter$output(
        results = self$results,
        plan = self$plan,
        format = format,
        path = path,
        theme_obj = self$theme_obj,
        ...
      )

      invisible(self)
    }
  )
)
