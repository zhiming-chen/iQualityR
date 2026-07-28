# =============================================================================
# File: R/DoePlotter.R
# Description: DOE plot executor
# =============================================================================

#' @title DoePlotter: DOE Plot Executor
#'
#' @description
#' Generates a variety of design of experiments (DOE) charts, including
#' main effects plots, interaction plots, residual plots, and response
#' surface visualizations. Inherits from [IqrPlotterBase].
#'
#' @export
DoePlotter <- R6::R6Class("DoePlotter",
  inherit = IqrPlotterBase,
  public = list(
    #' @description Render DOE plots.
    #' @param results Analysis results list (output from `DoeAnalyzer`).
    #' @param theme_obj An `IqrTheme` object used to style the generated plots.
    #'   May be `NULL`, in which case `ggplot2::theme_minimal()` is used.
    #' @param type Character scalar selecting the plot type. One of
    #'   `"design"`, `"main_effects"`, `"interaction"`, `"residual"`,
    #'   `"full"`, `"response_surface"`, `"surface"`, `"contour"`,
    #'   `"half_normal"`, `"pareto_effects"`, `"cube"`, `"power_curve"`,
    #'   `"wireframe"`, or `"residuals_vs_predictors"`.
    #'   - `"residual"` returns a 3-panel patchwork diagnostic plot:
    #'     Residuals vs Fitted + Normal Q-Q + Scale-Location.
    #'   - `"full"` returns a 3-row patchwork combining `main_effects`,
    #'     `interaction` and `residual` panels.
    #'   - `"response_surface"` returns a list with `surface_plot`,
    #'     `contour_plot`, `combined_plot`, and (if plotly is available)
    #'     `interactive_plot`.
    #'   - `"surface"` returns only the 2D tile + contour surface plot.
    #'   - `"contour"` returns only the filled contour plot.
    #'   - `"half_normal"` returns a half-normal probability plot of effect
    #'     estimates with Lenth (1989) ME and SME reference lines, used to
    #'     identify active effects in unreplicated factorials.
    #'   - `"pareto_effects"` returns a Pareto chart of absolute effect
    #'     estimates sorted by magnitude, with the Lenth ME reference line.
    #' @param ... Additional arguments. `plan` (an `IqrDoePlan` object) must be
    #'   supplied here so the plot methods can access factor information.
    #'   For response surface types, `response_name` (character scalar) may
    #'   be supplied to select a specific response variable; otherwise the
    #'   first response is used. `x_var` and `y_var` (character scalars) may
    #'   be supplied to select which two factors appear on the axes;
    #'   otherwise the first two numeric factors are used.
    #' @return A `ggplot`, `patchwork`, or list object (see `type`).
    render = function(results, theme_obj, type = "design", ...) {
      # Fall back to the default IqrTheme so the IqrPlotterBase palette
      # accessors always receive a non-NULL theme_obj. This preserves the
      # historical behavior of tolerating a NULL theme_obj argument.
      if (is.null(theme_obj)) {
        theme_obj <- tryCatch(IqrTheme$new("academic"), error = function(e) NULL)
      }

      dots <- list(...)
      plan <- dots$plan
      response_name <- dots$response_name
      x_var <- dots$x_var
      y_var <- dots$y_var
      # For overlaid contour plots: a named list of response specifications,
      # each with $model, $lower, $upper, $target (optional), and $color
      # (optional). Used by the "overlaid_contour" type.
      response_specs <- dots$response_specs

      switch(type,
        "design"          = private$.plot_design(results, theme_obj, plan),
        "main_effects"    = private$.plot_main_effects(results, theme_obj, plan),
        "interaction"     = private$.plot_interaction(results, theme_obj, plan,
                                                      response_name = response_name),
        "residual"        = private$.plot_residual(results, theme_obj, plan,
                                                   response_name = response_name),
        "full"            = private$.plot_full(results, theme_obj, plan,
                                               response_name = response_name),
        "response_surface" = private$.plot_response_surface(results, theme_obj, plan,
                                                            response_name = response_name,
                                                            x_var = x_var, y_var = y_var),
        "surface"         = private$.plot_response_surface(results, theme_obj, plan,
                                                           response_name = response_name,
                                                           x_var = x_var, y_var = y_var)$surface_plot,
        "contour"         = private$.plot_response_surface(results, theme_obj, plan,
                                                           response_name = response_name,
                                                           x_var = x_var, y_var = y_var)$contour_plot,
        "overlaid_contour" = private$.plot_overlaid_contour(results, theme_obj, plan,
                                                             response_specs = response_specs,
                                                             x_var = x_var, y_var = y_var),
        "half_normal"     = private$.plot_half_normal(results, theme_obj, plan),
        "normal_effects"  = private$.plot_normal_effects(results, theme_obj, plan),
        "pareto_effects"  = private$.plot_pareto_effects(results, theme_obj, plan),
        "cube"            = private$.plot_cube(results, theme_obj, plan,
                                               response_name = response_name),
        "power_curve"     = private$.plot_power_curve(results, theme_obj, plan),
        "wireframe"       = private$.plot_wireframe(results, theme_obj, plan,
                                                     response_name = response_name,
                                                     x_var = x_var, y_var = y_var),
        "residuals_vs_predictors" = private$.plot_residuals_vs_predictors(
                                                     results, theme_obj, plan,
                                                     response_name = response_name),
        stop("Unsupported plot type: ", type, call. = FALSE)
      )
    }
  ),

  private = list(
    # Apply the IqrTheme to a ggplot/patchwork object when available. Falls
    # back to the plot's existing theme (theme_minimal) if theme_obj is NULL
    # or does not expose a plot theme method. The theme_obj should be applied
    # when available to keep plots consistent with the rest of the ecosystem.
    #
    # When `apply_scales` is TRUE, the IqrTheme discrete color/fill scales
    # are also added so that data-series colors follow the active theme
    # palette rather than hardcoded values. This is opt-in because some
    # plots (e.g. response_surface) use continuous gradients that should
    # not be overridden by a discrete palette.
    .apply_theme = function(p, theme_obj, apply_scales = FALSE) {
      if (!is.null(theme_obj) && !is.null(theme_obj$plot)) {
        tryCatch({
          p <- p + theme_obj$plot$theme_iqr()
          if (apply_scales) {
            p <- p + theme_obj$plot$scale_fill_iqr(discrete = TRUE)
            p <- p + theme_obj$plot$scale_color_iqr(discrete = TRUE)
          }
        }, error = function(e) {
          # theme_obj does not provide theme_iqr(); keep the default theme.
        })
      }
      p
    },

    # Build a descriptive title for the design plot from the plan metadata.
    .design_title = function(plan) {
      if (is.null(plan)) return("Experimental Design")
      dt <- plan$design_type
      label <- switch(dt,
        "ccd"           = "Central Composite Design (CCD)",
        "box_behnken"   = "Box-Behnken Design (BBD)",
        "rsm"           = "Response Surface Design (CCD)",
        "factorial"     = "Full Factorial Design",
        "fractional"    = "Fractional Factorial Design",
        "orthogonal"    = "Orthogonal Array Design",
        "taguchi"       = "Taguchi Robust Design",
        "lhs"           = "Latin Hypercube Sample",
        "maximin"       = "Maximin Space-Filling Design",
        dt
      )
      label
    },

    # Build a subtitle with design metadata (factors, runs, center points,
    # alpha, resolution, randomization).
    .design_subtitle = function(plan, n_runs) {
      parts <- character(0)
      if (!is.null(plan)) {
        parts <- c(parts, sprintf("k = %d", length(plan$factors)))
        parts <- c(parts, sprintf("N = %d", n_runs))
        if (plan$design_type %in% c("ccd", "rsm") && !is.null(plan$alpha)) {
          alpha_label <- if (is.numeric(plan$alpha)) {
            sprintf("alpha = %.3f", plan$alpha)
          } else {
            sprintf("alpha = %s", plan$alpha)
          }
          parts <- c(parts, alpha_label)
        }
        if (!is.null(plan$center_points) && plan$center_points > 0) {
          parts <- c(parts, sprintf("center = %d", plan$center_points))
        }
        if (!is.null(plan$resolution)) {
          parts <- c(parts, sprintf("res = %s", plan$resolution))
        }
        if (isTRUE(plan$randomize)) {
          parts <- c(parts, "randomized")
        }
        if (isTRUE(plan$blocking)) {
          parts <- c(parts, sprintf("blocks = %d", plan$n_blocks))
        }
      }
      paste(parts, collapse = " | ")
    },

    # Convert a design data frame from actual engineering units to coded
    # units (-1 / 0 / +1 / +/-alpha) so the design plot can show geometric
    # symmetry and the alpha circle for CCD.
    .to_coded = function(design, plan) {
      if (is.null(plan) || is.null(plan$factors)) return(design)
      coded <- design
      for (f in plan$factors) {
        if (!f$name %in% names(coded)) next
        mid  <- mean(f$levels)
        half <- (f$levels[2] - f$levels[1]) / 2
        if (half == 0) next
        coded[[f$name]] <- (coded[[f$name]] - mid) / half
      }
      coded
    },

    # Create a single pairwise scatter plot for the design layout. Points
    # are colored and shaped by PointType so cube / axial / center / edge
    # points are visually distinguishable. For CCD designs, a dashed circle
    # of radius alpha is overlaid to illustrate rotatability / face-centered
    # / spherical geometry.
    .make_pair_plot = function(coded_design, x_var, y_var, theme_obj,
                                plan, show_legend = TRUE) {
      # Ensure PointType exists; default to "design" when missing.
      if (is.null(coded_design$PointType)) {
        coded_design$PointType <- "design"
      }
      coded_design$PointType <- factor(coded_design$PointType)

      p <- ggplot(coded_design,
                  aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_point(aes(color = PointType, shape = PointType),
                   size = 3, alpha = 0.8) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
        geom_vline(xintercept = 0, linetype = "dotted", color = "gray60") +
        labs(x = x_var, y = y_var) +
        theme_minimal() +
        coord_equal()

      # For CCD: overlay the alpha circle to show the axial geometry.
      if (!is.null(plan) && plan$design_type %in% c("ccd", "rsm")) {
        alpha_val <- plan$alpha
        if (is.null(alpha_val) || is.character(alpha_val)) {
          # Resolve alpha from the keyword.
          n_factors <- length(plan$factors)
          alpha_val <- if (is.null(alpha_val)) {
            (2^n_factors)^(1 / 4)
          } else if (alpha_val == "rotatable") {
            (2^n_factors)^(1 / 4)
          } else if (alpha_val == "spherical") {
            sqrt(n_factors)
          } else if (alpha_val == "face_centered") {
            1
          } else {
            (2^n_factors)^(1 / 4)
          }
        }
        theta <- seq(0, 2 * pi, length.out = 100)
        circle_df <- data.frame(
          x = alpha_val * cos(theta),
          y = alpha_val * sin(theta)
        )
        p <- p + geom_path(data = circle_df,
                           aes(x = x, y = y),
                           inherit.aes = FALSE,
                           linetype = "dashed", color = "gray50",
                           linewidth = 0.5)
      }

      # Apply theme and discrete color scales so PointType colors follow
      # the active IqrTheme palette.
      p <- private$.apply_theme(p, theme_obj, apply_scales = TRUE)

      if (!show_legend) {
        p <- p + theme(legend.position = "none")
      }

      p
    },

    # Plot the experimental design layout as a factor-space scatter plot.
    # The visualization adapts to the number of factors:
    #   k = 2: single 2D scatter with alpha circle (CCD)
    #   k = 3: 1x3 row of pairwise scatters
    #   k >= 4: 2-column grid of pairwise scatters (first 6 pairs)
    # Points are colored and shaped by PointType (cube / axial / center / edge)
    # so the geometric structure of CCD and BBD designs is visible at a glance.
    .plot_design = function(results, theme_obj, plan) {
      if (is.null(results$design_info)) {
        stop("Design info not available", call. = FALSE)
      }

      design <- results$design_info

      # Identify factor names from the plan, falling back to numeric columns.
      factor_names <- if (!is.null(plan) && !is.null(plan$factors)) {
        vapply(plan$factors, function(f) f$name, character(1))
      } else {
        aux_cols <- c("RunOrder", "Block", "PointType", "Replication")
        numeric_cols <- names(design)[sapply(design, is.numeric)]
        setdiff(numeric_cols, aux_cols)
      }
      k <- length(factor_names)

      if (k < 2) {
        stop("Design plot requires at least 2 numeric factors.", call. = FALSE)
      }

      # Convert to coded units for geometric visualization.
      coded_design <- private$.to_coded(design, plan)

      # Ensure PointType exists.
      if (is.null(coded_design$PointType)) {
        coded_design$PointType <- "design"
      }

      title_str <- private$.design_title(plan)
      subtitle_str <- private$.design_subtitle(plan, nrow(design))

      if (k == 2) {
        # Single 2D scatter.
        p <- private$.make_pair_plot(coded_design, factor_names[1],
                                     factor_names[2], theme_obj, plan,
                                     show_legend = TRUE)
        p <- p + labs(title = title_str, subtitle = subtitle_str)
        p <- private$.apply_theme(p, theme_obj)
        invisible(p)
      } else {
        # Multiple pairwise scatters.
        pair_plots <- list()
        idx <- 1
        for (i in seq_len(k - 1)) {
          for (j in (i + 1):k) {
            if (idx > 6) break  # cap at 6 pairs for readability
            pair_plots[[idx]] <- private$.make_pair_plot(
              coded_design, factor_names[i], factor_names[j],
              theme_obj, plan, show_legend = (idx == 1)
            )
            idx <- idx + 1
          }
          if (idx > 6) break
        }

        n_pairs <- length(pair_plots)
        ncol <- if (n_pairs <= 3) n_pairs else 3

        combined <- patchwork::wrap_plots(pair_plots, ncol = ncol) +
          patchwork::plot_annotation(
            title = title_str,
            subtitle = subtitle_str,
            tag_levels = "A"
          )
        combined <- private$.apply_theme(combined, theme_obj)
        invisible(combined)
      }
    },

    # Plot the main effects of experimental factors as a horizontal bar
    # chart of effect estimates. Bars are colored by sign (positive vs
    # negative) using fixed, theme-independent colors so the direction of
    # each factor's influence is immediately visible and consistent across
    # all renderings regardless of the active IqrTheme preset. A vertical
    # reference line at zero separates positive from negative effects.
    .plot_main_effects = function(results, theme_obj, plan) {
      if (is.null(results$effects)) {
        stop("Effects not available", call. = FALSE)
      }

      effects_data <- data.frame(
        Factor = names(results$effects$main),
        Effect  = unlist(results$effects$main),
        stringsAsFactors = FALSE
      )
      effects_data$Direction <- ifelse(effects_data$Effect >= 0,
                                        "Positive", "Negative")
      # Order by absolute effect (largest at top) for readability.
      effects_data <- effects_data[order(abs(effects_data$Effect)), ]
      effects_data$Factor <- factor(effects_data$Factor,
                                     levels = effects_data$Factor)

      # Use fixed, theme-independent colors for sign coding so bar colors
      # are consistent across all renderings and IqrTheme presets. Mixing
      # theme-dependent UI colors (primary/danger) caused bars to change
      # color depending on the active preset (workbench vs academic vs tech),
      # which made cross-response comparison confusing.
      pos_color <- .iqr_plotter$.pal_ui(theme_obj, "primary")  # cool blue for positive effects
      neg_color <- .iqr_plotter$.pal_ui(theme_obj, "danger")   # warm red  for negative effects

      p <- ggplot(effects_data,
                  aes(x = Factor, y = Effect, fill = Direction)) +
        geom_bar(stat = "identity", width = 0.65) +
        geom_hline(yintercept = 0, linetype = "solid",
                   color = "gray30", linewidth = 0.5) +
        scale_fill_manual(
          values = c(Positive = pos_color, Negative = neg_color),
          guide = "none"
        ) +
        labs(
          title = "Main Effects Plot",
          subtitle = sprintf("Effect estimates for %d factor(s)",
                              nrow(effects_data)),
          x = "Factor",
          y = "Effect Estimate"
        ) +
        coord_flip() +
        theme_minimal() +
        theme(
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold")
        )

      p <- private$.apply_theme(p, theme_obj)
      invisible(p)
    },

    # Plot pairwise interaction effects. For each pair of factors (A, B),
    # produce an individual interaction plot showing the mean response at
    # each level of B (x-axis) stratified by the level of A (lines/legend).
    # Non-parallel lines indicate an interaction between the two factors.
    #
    # Each pair gets its own independent ggplot with its own legend, then
    # the per-pair plots are combined with patchwork. This is the standard
    # layout used by Minitab / Design-Expert: each panel has a clearly
    # labelled legend identifying the stratification factor and its levels,
    # which avoids the critical issue of mixing levels from different factors
    # (with different units/scales) into a single shared legend.
    #
    # When `response_name` is supplied, the model for that response is used;
    # otherwise the top-level (first) model is used.
    .plot_interaction = function(results, theme_obj, plan, response_name = NULL) {
      if (is.null(results$effects) ||
          is.null(results$effects$interaction) ||
          length(results$effects$interaction) == 0) {
        message("Interaction plot: no interaction effects available; ",
                "rendering main effects instead.")
        return(private$.plot_main_effects(results, theme_obj, plan))
      }

      # Resolve the model for the requested response.
      model <- if (!is.null(response_name) &&
                    !is.null(results$anova_results[[response_name]])) {
        results$anova_results[[response_name]]$model
      } else {
        results$model
      }
      if (is.null(model)) {
        stop("Interaction plot requires a fitted model.", call. = FALSE)
      }
      obs_data <- model$model
      if (is.null(response_name)) {
        response_name <- names(obs_data)[1]
      }
      factor_cols <- setdiff(names(obs_data), response_name)
      factor_names <- sapply(plan$factors, function(f) f$name)
      factor_cols <- intersect(factor_cols, factor_names)

      if (length(factor_cols) < 2) {
        stop("Interaction plot requires at least 2 factors.", call. = FALSE)
      }

      # Build individual ggplot objects for each factor pair. Each plot
      # has its own legend so levels from different factors are never
      # mixed in the same legend.
      pair_indices <- utils::combn(length(factor_cols), 2,
                                   simplify = FALSE)
      plot_list <- lapply(pair_indices, function(pair) {
        i <- pair[1]; j <- pair[2]
        fA <- factor_cols[i]  # stratification factor (lines/legend)
        fB <- factor_cols[j]  # x-axis factor

        cell_means <- aggregate(
          as.formula(paste(response_name, "~", fA, "+", fB)),
          data = obs_data, FUN = mean, na.rm = TRUE
        )

        plot_df <- data.frame(
          LevelX       = as.factor(cell_means[[fB]]),
          LevelA       = as.factor(cell_means[[fA]]),
          MeanResponse = cell_means[[response_name]],
          stringsAsFactors = FALSE
        )

        p <- ggplot(plot_df,
                    aes(x = LevelX, y = MeanResponse,
                        group = LevelA, color = LevelA)) +
          geom_line(linewidth = 0.9) +
          geom_point(size = 2.5) +
          labs(
            title = paste(fA, "x", fB),
            x = fB,
            y = response_name,
            color = fA
          ) +
          theme_minimal() +
          theme(
            legend.position = "right",
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", size = 11)
          )

        # Apply IqrTheme discrete color scale per-panel so each pair's
        # stratification colors follow the active theme palette.
        p <- private$.apply_theme(p, theme_obj, apply_scales = TRUE)
        p
      })

      # Combine with patchwork. Use ncol=2 for readability when there are
      # multiple pairs; a single pair returns a single ggplot.
      if (length(plot_list) == 1) {
        combined <- plot_list[[1]]
      } else {
        combined <- patchwork::wrap_plots(plot_list, ncol = 2) +
          patchwork::plot_annotation(
            title = "Interaction Plots",
            subtitle = sprintf("Response: %s", response_name),
            theme = theme(
              plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 11, hjust = 0.5,
                                           color = "gray40")
            )
          )
        combined <- private$.apply_theme(combined, theme_obj)
      }
      invisible(combined)
    },

    # Plot residual diagnostics by delegating to the iQualityR.plot
    # `plot_anova_residuals()` function, which produces a 2x2 patchwork
    # grid (4 panels): Residuals vs Fitted + Normal Q-Q + Scale-Location
    # + Residuals vs Order. This matches the standard diagnostic panel
    # used by Minitab / Design-Expert / R's `plot.lm` and reuses the
    # shared .plot infrastructure (theme, base_plot, plot_qq) instead of
    # reimplementing the panels locally.
    #
    # The 4-panel layout gives a complete picture of regression diagnostics:
    #   - Residuals vs Fitted detects non-linearity / curvature / outliers
    #   - Normal Q-Q checks the normality assumption required for valid
    #     t/F inference on the model coefficients
    #   - Scale-Location detects heteroscedasticity (non-constant variance)
    #   - Residuals vs Order detects time-series / run-order dependence
    #
    # When `response_name` is supplied, the model for that specific response
    # is used; otherwise the top-level (first) model is used.
    .plot_residual = function(results, theme_obj, plan, response_name = NULL) {
      # Resolve the model for the requested response.
      model <- if (!is.null(response_name) &&
                    !is.null(results$anova_results[[response_name]])) {
        results$anova_results[[response_name]]$model
      } else {
        results$model
      }
      if (is.null(model)) {
        stop("Model not available", call. = FALSE)
      }

      # Build a descriptive title that includes the response name when
      # available, so the diagnostic panel is self-documenting.
      title_str <- if (!is.null(response_name)) {
        sprintf("Residual Diagnostics: %s", response_name)
      } else {
        "Residual Diagnostic Plots"
      }

      # Delegate to the shared iQualityR.plot function. It accepts an
      # IqrTheme object via the `theme` argument and applies it to every
      # sub-panel via base_plot() + as_iqr_theme().
      p <- iQualityR.plot::plot_anova_residuals(
        model = model,
        add_qq = TRUE,
        add_scale_location = TRUE,
        theme = theme_obj,
        title = title_str
      )

      invisible(p)
    },

    # Combine the main effects, interaction, and residual plots into a
    # single comprehensive view with an optimized layout:
    #
    #   Row A (height 1.0): Main Effects      — horizontal bar chart, compact
    #   Row B (height 1.4): Interaction Plots  — faceted, multi-panel
    #   Row C (height 2.0): Residual Diagnostics — 2x2 grid, needs the most room
    #
    # Non-equal heights ensure each panel has enough vertical space to be
    # legible without forcing the reader to scroll or resize. Tag levels
    # (A, B, C) and an overall title/subtitle tie the panels together.
    #
    # When `response_name` is supplied, the interaction and residual panels
    # use the model for that specific response; the main-effects panel always
    # uses the top-level (first) effect estimates because it is a factor-level
    # summary that does not depend on a particular response.
    .plot_full = function(results, theme_obj, plan, response_name = NULL) {
      p1 <- private$.plot_main_effects(results, theme_obj, plan)
      p2 <- private$.plot_interaction(results, theme_obj, plan,
                                       response_name = response_name)
      p3 <- private$.plot_residual(results, theme_obj, plan,
                                    response_name = response_name)

      if (!is.null(p1) && !is.null(p2) && !is.null(p3)) {
        # Non-equal heights: residual panel (2x2 grid) gets the most room,
        # interaction (faceted) gets intermediate, main effects (bars)
        # gets the least since horizontal bars are compact.
        combined <- (p1 / p2 / p3) +
          patchwork::plot_layout(heights = c(1, 1.4, 2.0))

        # Add overall annotation with tag levels for cross-referencing.
        title_str <- "Comprehensive DOE Analysis"
        subtitle_str <- if (!is.null(response_name)) {
          sprintf("Response: %s", response_name)
        } else {
          "Combined main effects, interactions, and residual diagnostics"
        }

        combined <- combined +
          patchwork::plot_annotation(
            title = title_str,
            subtitle = subtitle_str,
            tag_levels = "A",
            theme = theme(
              plot.title = element_text(size = 16, face = "bold",
                                        hjust = 0.5),
              plot.subtitle = element_text(size = 12, hjust = 0.5,
                                            color = "gray40")
            )
          )
        combined <- private$.apply_theme(combined, theme_obj)
        invisible(combined)
      } else if (!is.null(p1) && !is.null(p2)) {
        combined <- (p1 / p2) +
          patchwork::plot_layout(heights = c(1, 1.5))
        combined <- private$.apply_theme(combined, theme_obj)
        invisible(combined)
      } else {
        # Fallback: main effects only.
        private$.plot_main_effects(results, theme_obj, plan)
      }
    },

    # Plot a response surface and contour plot for a pair of numeric
    # factors in the model. Returns a list containing the surface plot,
    # contour plot, combined patchwork plot, and (optionally) an interactive
    # plotly surface. By default the first two numeric factors are used;
    # callers may override this by supplying `x_var` and/or `y_var`.
    .plot_response_surface = function(results, theme_obj, plan,
                                      response_name = NULL,
                                      x_var = NULL, y_var = NULL) {
      # Resolve the model for the requested response. Falls back to the
      # top-level (first) model when response_name is NULL or when the
      # requested response is not present in anova_results.
      model <- if (!is.null(response_name) &&
                    !is.null(results$anova_results[[response_name]])) {
        results$anova_results[[response_name]]$model
      } else {
        results$model
      }
      if (is.null(model)) {
        stop("Model is required for response surface plotting", call. = FALSE)
      }

      # Resolve the response variable name
      if (is.null(response_name)) {
        response_name <- attr(model$terms, "variables")[[2]]
        response_name <- as.character(response_name)
      }

      # Identify numeric factors in the model. Only original factor columns
      # are eligible for plotting; derived columns such as `I(Temperature^2)`
      # produced by `I()` in the formula must be excluded because they are
      # not variables the user can set on the prediction grid.
      model_data <- model$model
      response_col <- names(model_data)[1]
      factor_cols <- setdiff(names(model_data), response_col)

      # Prefer factor names declared in the plan when available; this
      # robustly excludes derived terms and stays in sync with the design.
      plan_factor_names <- if (!is.null(plan) && !is.null(plan$factors)) {
        vapply(plan$factors, function(f) f$name, character(1))
      } else {
        character(0)
      }
      if (length(plan_factor_names) > 0) {
        factor_cols <- intersect(factor_cols, plan_factor_names)
      } else {
        # Fallback: drop names that look like derived terms (start with
        # `I(`, contain `:`, or contain `^`).
        factor_cols <- factor_cols[!grepl("^I\\(|:|\\^", factor_cols)]
      }
      numeric_factors <- factor_cols[sapply(model_data[factor_cols], is.numeric)]

      if (length(numeric_factors) < 2) {
        stop("At least 2 numeric factors are required for response surface plotting", call. = FALSE)
      }

      # Select the two factors for visualization. Validate user-supplied names.
      if (is.null(x_var)) x_var <- numeric_factors[1]
      if (is.null(y_var)) y_var <- numeric_factors[2]
      if (!x_var %in% numeric_factors || !y_var %in% numeric_factors) {
        stop("x_var and y_var must be among the numeric factors: ",
             paste(numeric_factors, collapse = ", "), call. = FALSE)
      }
      if (x_var == y_var) {
        stop("x_var and y_var must be different factors.", call. = FALSE)
      }

      # Factor ranges
      x_range <- range(model_data[[x_var]], na.rm = TRUE)
      y_range <- range(model_data[[y_var]], na.rm = TRUE)

      # Build prediction grid
      grid_size <- 50
      x_seq <- seq(x_range[1], x_range[2], length.out = grid_size)
      y_seq <- seq(y_range[1], y_range[2], length.out = grid_size)
      grid <- do.call(expand.grid, setNames(list(x_seq, y_seq), c(x_var, y_var)))

      # Hold other factors at their median
      other_factors <- setdiff(numeric_factors, c(x_var, y_var))
      if (length(other_factors) > 0) {
        for (fac in other_factors) {
          grid[[fac]] <- median(model_data[[fac]], na.rm = TRUE)
        }
      }

      # Add predicted response values. The model's formula environment may
      # point to a transient execution environment (typical when models are
      # created inside R6 methods or other encapsulated contexts). When the
      # formula contains derived terms such as `I(Temperature^2)`, predict.lm
      # evaluates them via model.frame which needs the underlying variable in
      # scope. Rebinding the terms environment to baseenv() before predict
      # forces model.frame to take every variable from `newdata` only, which
      # is exactly what we want for the prediction grid.
      tryCatch({
        m_local <- model
        attr(m_local$terms, ".Environment") <- baseenv()
        environment(m_local$terms) <- baseenv()
        grid$predicted <- predict(m_local, newdata = grid)
      }, error = function(e) {
        stop("Failed to generate predictions: ", e$message, call. = FALSE)
      })

      # Surface plot data
      surface_data <- grid

      # Retrieve IqrTheme diverging palette for the surface gradient.
      # The palette is used to build a diverging gradient centered at the
      # median predicted value so positive/negative deviations are
      # immediately distinguishable.
      cont_colors <- .iqr_plotter$.pal_diverging(theme_obj)
      low_col  <- cont_colors[1]
      high_col <- cont_colors[length(cont_colors)]
      mid_col  <- if (length(cont_colors) >= 3) {
        cont_colors[ceiling(length(cont_colors) / 2)]
      } else {
        "white"
      }
      mid_val  <- median(surface_data$predicted, na.rm = TRUE)

      # 3D surface plot (approximated with ggplot2 geom_tile).
      #
      # The `fill` (tile fill) and `z` (contour) aesthetics are mapped inside
      # the geom_*() calls rather than in the top-level ggplot aes() so that
      # downstream users can safely add geom_point(data = my_df, aes(...))
      # layers without inheriting a `fill = .data[["predicted"]]` mapping
      # that would fail because `predicted` is not present in their data.
      p_surface <- ggplot(surface_data,
          aes(.data[[x_var]], .data[[y_var]])) +
        geom_tile(aes(fill = .data[["predicted"]])) +
        geom_contour(aes(z = .data[["predicted"]]),
                     color = "white", alpha = 0.3, binwidth = 10) +
        scale_fill_gradient2(
          low = low_col, mid = mid_col, high = high_col,
          midpoint = mid_val,
          name = response_name
        ) +
        labs(
          title = paste("Response Surface:", response_name),
          subtitle = sprintf("Factors: %s vs %s", x_var, y_var),
          x = x_var,
          y = y_var
        ) +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          legend.position = "right"
        )

      # Contour plot. Uses viridis continuous fill (scale_fill_viridis_c)
      # which is perceptually uniform, colorblind-safe, and the de-facto
      # standard for scientific contour visualization. Black iso-response
      # lines are overlaid on top of the gradient so exact response values
      # can still be read.
      p_contour <- ggplot(surface_data,
          aes(.data[[x_var]], .data[[y_var]])) +
        geom_tile(aes(fill = .data[["predicted"]])) +
        geom_contour(
          aes(z = .data[["predicted"]]),
          color = "gray20", alpha = 0.5, linewidth = 0.4, bins = 12
        ) +
        scale_fill_viridis_c(name = response_name) +
        labs(
          title = paste("Contour Plot:", response_name),
          subtitle = sprintf("Factors: %s vs %s", x_var, y_var),
          x = x_var,
          y = y_var
        ) +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          legend.position = "right"
        )

      # Apply theme_obj to each subplot when available
      p_surface <- private$.apply_theme(p_surface, theme_obj)
      p_contour <- private$.apply_theme(p_contour, theme_obj)

      # Combined plot
      p_combined <- p_surface + p_contour + patchwork::plot_layout(ncol = 2)
      p_combined <- private$.apply_theme(p_combined, theme_obj)

      # If plotly is available, attempt to build an interactive 3D surface
      interactive_plot <- NULL
      if (requireNamespace("plotly", quietly = TRUE)) {
        tryCatch({
          x_mat <- matrix(surface_data$predicted, nrow = grid_size, byrow = FALSE)
          interactive_plot <- plotly::plot_ly(
            x = x_seq, y = y_seq, z = x_mat,
            type = "surface",
            colorscale = "RdBu",
            scene = list(
              xaxis = list(title = x_var),
              yaxis = list(title = y_var),
              zaxis = list(title = response_name)
            )
          ) %>%
            plotly::layout(title = paste("3D Response Surface:", response_name))
        }, error = function(e) {
          message("Failed to create interactive plot: ", e$message)
        })
      }

      invisible(list(
        surface_plot = p_surface,
        contour_plot = p_contour,
        combined_plot = p_combined,
        interactive_plot = interactive_plot,
        x_var = x_var,
        y_var = y_var,
        response_name = response_name
      ))
    },

    # =========================================================================
    # Half-normal probability plot of effect estimates (Daniel 1959, Lenth 1989)
    # =========================================================================
    #
    # The half-normal plot is the standard graphical method for identifying
    # active effects in unreplicated two-level factorials when no pure-error
    # estimate is available. Points falling off the straight reference line
    # (and beyond the Lenth ME/SME bounds) correspond to active effects.
    .plot_half_normal = function(results, theme_obj, plan) {
      if (is.null(results$effects)) {
        stop("Effects not available; run task$compute() with response data first.",
             call. = FALSE)
      }

      # Gather all effects (main + pairwise interactions).
      all_effects <- c(results$effects$main, results$effects$interaction)
      if (length(all_effects) < 3) {
        stop("At least 3 effects are required for a half-normal plot.",
             call. = FALSE)
      }

      effect_names <- names(all_effects)
      abs_effects  <- abs(unlist(all_effects))
      n <- length(abs_effects)

      # Compute Lenth PSE / ME / SME via the analyzer so the reference lines
      # are consistent with the numerical diagnostics.
      pse_info <- NULL
      tryCatch({
        analyzer <- DoeAnalyzer$new()
        pse_info <- analyzer$compute_lenth_pse(unlist(all_effects))
      }, error = function(e) {
        # If the analyzer fails for any reason (e.g. too few effects), we
        # still render the plot without reference lines.
      })

      # Half-normal quantiles: qnorm((i - 0.5) / n). The (i - 0.5)/n
      # plotting position is the standard choice (Daniel 1959).
      plot_data <- data.frame(
        Effect       = effect_names,
        Abs_Effect   = abs_effects,
        HalfNorm_Q   = stats::qnorm((seq_len(n) - 0.5) / n)
      )
      # Sort by absolute effect so the points lie on a monotone curve.
      plot_data <- plot_data[order(plot_data$Abs_Effect), ]
      plot_data$Order <- seq_len(n)

      # Mark active effects (beyond Lenth ME) for emphasis.
      point_col  <- .iqr_plotter$.pal_ui(theme_obj, "primary")
      active_col <- .iqr_plotter$.pal_ui(theme_obj, "danger")
      label_col  <- .iqr_plotter$.pal_ui(theme_obj, "text", default = "black")
      me_col     <- .iqr_plotter$.pal_ui(theme_obj, "warning")
      sme_col    <- .iqr_plotter$.pal_ui(theme_obj, "danger")

      # Determine which points are "active" (beyond ME) for color emphasis.
      if (!is.null(pse_info)) {
        plot_data$Active <- plot_data$Abs_Effect > pse_info$me
        p <- ggplot(plot_data,
                    aes(x = HalfNorm_Q, y = Abs_Effect,
                        color = Active)) +
          geom_point(size = 3) +
          scale_color_manual(
            values = c(`FALSE` = point_col, `TRUE` = active_col),
            guide = "none"
          )
      } else {
        p <- ggplot(plot_data, aes(x = HalfNorm_Q, y = Abs_Effect)) +
          geom_point(size = 3, color = point_col)
      }

      p <- p +
        geom_text(aes(label = Effect), vjust = -0.6, hjust = 0.4,
                  size = 3, color = label_col) +
        labs(
          title = "Half-Normal Plot of Effects",
          subtitle = "Daniel (1959) - Identifies active effects in unreplicated factorials",
          x = "Half-normal quantiles",
          y = "Absolute effect estimate"
        ) +
        theme_minimal()

      # Add Lenth ME (individual) and SME (simultaneous) reference lines.
      # Both are horizontal because the half-normal quantile axis indexes
      # the rank of the effect, while the y-axis indexes its magnitude.
      if (!is.null(pse_info)) {
        p <- p +
          geom_hline(yintercept = pse_info$me,
                     linetype = "dashed", color = me_col, linewidth = 0.6) +
          geom_hline(yintercept = pse_info$sme,
                     linetype = "dashed", color = sme_col, linewidth = 0.6) +
          annotate("text",
                   x = max(plot_data$HalfNorm_Q),
                   y = pse_info$me,
                   vjust = -0.6, hjust = 1, color = me_col, size = 3,
                   label = sprintf("ME  = %.3f", pse_info$me)) +
          annotate("text",
                   x = max(plot_data$HalfNorm_Q),
                   y = pse_info$sme,
                   vjust = -0.6, hjust = 1, color = sme_col, size = 3,
                   label = sprintf("SME = %.3f", pse_info$sme))
      }

      p <- private$.apply_theme(p, theme_obj)
      invisible(p)
    },

    # =========================================================================
    # Pareto chart of effects (Minitab / Design-Expert standard view)
    # =========================================================================
    #
    # The Pareto chart orders absolute effect estimates from largest to
    # smallest, making it easy to see which factors dominate the response.
    # The Lenth ME reference line marks the 5% individual significance
    # threshold; bars exceeding it correspond to statistically active effects.
    .plot_pareto_effects = function(results, theme_obj, plan) {
      if (is.null(results$effects)) {
        stop("Effects not available; run task$compute() with response data first.",
             call. = FALSE)
      }

      all_effects <- c(results$effects$main, results$effects$interaction)
      if (length(all_effects) < 2) {
        stop("At least 2 effects are required for a Pareto chart.",
             call. = FALSE)
      }

      effect_df <- data.frame(
        Effect = names(all_effects),
        Value  = unlist(all_effects),
        stringsAsFactors = FALSE
      )
      effect_df$Abs_Value <- abs(effect_df$Value)
      effect_df <- effect_df[order(-effect_df$Abs_Value), ]
      # Lock the factor levels so bars are drawn in sorted order regardless
      # of subsequent row reordering.
      effect_df$Effect <- factor(effect_df$Effect, levels = effect_df$Effect)

      # Compute Lenth ME reference line.
      pse_info <- NULL
      tryCatch({
        analyzer <- DoeAnalyzer$new()
        pse_info <- analyzer$compute_lenth_pse(unlist(all_effects))
      }, error = function(e) {
        # Render without the reference line if Lenth PSE is unavailable.
      })

      # Determine active vs inactive effects for bar coloring.
      cont_colors <- .iqr_plotter$.pal_diverging(theme_obj)
      low_col <- cont_colors[1]
      high_col <- cont_colors[length(cont_colors)]
      me_col <- .iqr_plotter$.pal_ui(theme_obj, "warning")

      # Mark bars as active (beyond ME) when Lenth PSE is available.
      if (!is.null(pse_info)) {
        effect_df$Active <- effect_df$Abs_Value > pse_info$me
      } else {
        effect_df$Active <- FALSE
      }

      p <- ggplot(effect_df, aes(x = Effect, y = Abs_Value,
                                  fill = Active)) +
        geom_bar(stat = "identity", width = 0.7) +
        scale_fill_manual(
          values = c(`FALSE` = low_col, `TRUE` = high_col),
          guide = "none"
        ) +
        labs(
          title = "Pareto Chart of Effects",
          subtitle = "Absolute effect estimates sorted by magnitude",
          x = "Effect",
          y = "Absolute effect estimate"
        ) +
        theme_minimal() +
        coord_flip()

      if (!is.null(pse_info)) {
        p <- p +
          geom_hline(yintercept = pse_info$me,
                     linetype = "dashed", color = me_col, linewidth = 0.6) +
          annotate("text",
                   x = nrow(effect_df),
                   y = pse_info$me,
                   vjust = -0.6, hjust = 1, color = me_col, size = 3,
                   label = sprintf("ME = %.3f", pse_info$me))
      }

      p <- private$.apply_theme(p, theme_obj)
      invisible(p)
    },

    # Plot an overlaid contour plot for multiple responses.
    #
    # An overlaid contour plot displays the feasible region where all
    # responses simultaneously meet their specification limits. Each
    # response's constraint is rendered as a contour band; the
    # intersection of all bands is the feasible operating region. This
    # is the standard visualization for multi-response optimization in
    # Minitab / JMP / Design-Expert.
    #
    # The `response_specs` argument is a named list where each element
    # has:
    #   $model  - a fitted `lm` object (second-order RSM model)
    #   $lower  - lower specification bound (or -Inf)
    #   $upper  - upper specification bound (or +Inf)
    #   $target - optional target value (for reference line)
    #   $color  - optional fill color for the constraint band
    #
    # The plot draws a 50x50 prediction grid for the two selected
    # factors (x_var, y_var), with all other factors held at their
    # center value. Each response's contour band is overlaid using
    # semi-transparent fills. The intersection (feasible region) is
    # automatically the darkest area where all bands overlap.
    .plot_overlaid_contour = function(results, theme_obj, plan,
                                       response_specs = NULL,
                                       x_var = NULL, y_var = NULL) {
      if (is.null(response_specs) ||
          !is.list(response_specs) ||
          length(response_specs) < 1) {
        stop("Overlaid contour plot requires `response_specs`: a named list ",
             "where each element has $model, $lower, $upper (and optionally ",
             "$target, $color).", call. = FALSE)
      }

      # Resolve the two axis factors.
      factor_names <- vapply(plan$factors, function(f) f$name,
                              character(1))
      if (is.null(x_var)) x_var <- factor_names[1]
      if (is.null(y_var)) y_var <- factor_names[2]
      if (!x_var %in% factor_names || !y_var %in% factor_names) {
        stop("x_var and y_var must be factor names. Got: ",
             x_var, ", ", y_var, call. = FALSE)
      }

      # Build a 50x50 prediction grid over the two selected factors.
      # Other factors are held at their center value.
      x_factor <- plan$factors[[match(x_var, factor_names)]]
      y_factor <- plan$factors[[match(y_var, factor_names)]]
      x_range <- range(x_factor$levels)
      y_range <- range(y_factor$levels)
      n_grid <- 50
      x_seq <- seq(x_range[1], x_range[2], length.out = n_grid)
      y_seq <- seq(y_range[1], y_range[2], length.out = n_grid)
      grid <- expand.grid(x_seq, y_seq)
      colnames(grid) <- c(x_var, y_var)

      # Add the other factors at their center value.
      for (f in plan$factors) {
        if (f$name %in% c(x_var, y_var)) next
        grid[[f$name]] <- mean(f$levels)
      }

      # Assign a distinct color to each response from the IqrTheme discrete
      # palette, auto-extended to cover the number of responses.
      n_responses <- length(response_specs)
      default_colors <- .iqr_plotter$.pal_discrete(theme_obj, n_responses)
      for (i in seq_len(n_responses)) {
        if (is.null(response_specs[[i]]$color)) {
          response_specs[[i]]$color <-
            default_colors[((i - 1) %% length(default_colors)) + 1]
        }
      }

      # Compute predictions for each response and determine the
      # feasible region (where all specs are met).
      feasibility <- matrix(TRUE, nrow = nrow(grid), ncol = 1)
      contour_layers <- list()

      for (i in seq_len(n_responses)) {
        spec <- response_specs[[i]]
        model <- spec$model
        # Rebind the model terms environment so predict.lm evaluates
        # derived terms from newdata only.
        m_local <- model
        attr(m_local$terms, ".Environment") <- baseenv()
        environment(m_local$terms) <- baseenv()
        pred <- tryCatch({
          as.numeric(predict(m_local, newdata = grid))
        }, error = function(e) rep(NA_real_, nrow(grid)))

        grid[[paste0("pred_", i)]] <- pred

        # Feasibility mask: TRUE where the response meets its spec.
        meets_lower <- if (is.finite(spec$lower)) pred >= spec$lower else TRUE
        meets_upper <- if (is.finite(spec$upper)) pred <= spec$upper else TRUE
        feasible <- meets_lower & meets_upper
        feasibility <- feasibility & feasible

        response_specs[[i]]$pred <- pred
        response_specs[[i]]$feasible <- feasible
      }

      # Build the plot. Use geom_tile for the feasible region
      # (highlighted in green) and geom_contour for each response's
      # constraint boundary.
      plot_df <- grid
      plot_df$Feasible <- as.factor(ifelse(feasibility, "Feasible", "Infeasible"))

      p <- ggplot(plot_df, aes(.data[[x_var]], .data[[y_var]])) +
        # Feasible region: green highlight.
        geom_tile(data = subset(plot_df, Feasible == "Feasible"),
                  fill = .iqr_plotter$.pal_semantic(theme_obj, "pass"), alpha = 0.35) +
        theme_minimal() +
        labs(
          title = "Overlaid Contour Plot",
          subtitle = sprintf("Feasible region (green) where all %d responses meet specs",
                             n_responses),
          x = x_var,
          y = y_var
        ) +
        theme(
          panel.grid = element_blank(),
          legend.position = "right"
        )

      # Add contour lines for each response's constraint boundary.
      for (i in seq_len(n_responses)) {
        spec <- response_specs[[i]]
        contour_col <- spec$color
        # Lower bound contour.
        if (is.finite(spec$lower)) {
          p <- p +
            geom_contour(
              data = plot_df,
              aes(z = .data[[paste0("pred_", i)]]),
              breaks = spec$lower,
              color = contour_col, linewidth = 0.8, linetype = "dashed"
            )
        }
        # Upper bound contour.
        if (is.finite(spec$upper)) {
          p <- p +
            geom_contour(
              data = plot_df,
              aes(z = .data[[paste0("pred_", i)]]),
              breaks = spec$upper,
              color = contour_col, linewidth = 0.8, linetype = "solid"
            )
        }
        # Target line (optional).
        if (!is.null(spec$target) && is.finite(spec$target)) {
          p <- p +
            geom_contour(
              data = plot_df,
              aes(z = .data[[paste0("pred_", i)]]),
              breaks = spec$target,
              color = contour_col, linewidth = 0.5, linetype = "dotted"
            )
        }
      }

      # Build a legend mapping colors to response names.
      legend_df <- data.frame(
        Response = names(response_specs),
        Color = vapply(response_specs, function(s) s$color, character(1)),
        stringsAsFactors = FALSE
      )
      p <- p +
        annotate("text",
                 x = x_range[1], y = y_range[2],
                 hjust = 0, vjust = 1,
                 label = paste(names(response_specs), collapse = "\n"),
                 color = "gray20", size = 3)

      p <- private$.apply_theme(p, theme_obj)
      invisible(p)
    },

    # =========================================================================
    # Normal probability plot of effects (signed)
    # =========================================================================
    #
    # The half-normal plot (Daniel 1959) uses absolute effect values and
    # cannot distinguish positive from negative effects. The normal
    # probability plot of effects plots the SIGNED effect estimates against
    # normal quantiles, so the direction of each effect is visible. Effects
    # that deviate from the reference line are considered active.
    #
    # Reference:
    #   Daniel, C. (1959). Use of Half-Normal Plots in Interpreting
    #   Factorial Two-Level Experiments. Technometrics, 1(4), 311-341.
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments
    #   (10th ed.), sec. 6.5.
    .plot_normal_effects = function(results, theme_obj, plan) {
      if (is.null(results$effects)) {
        stop("Effects not available; run task$compute() with response data first.",
             call. = FALSE)
      }

      all_effects <- c(results$effects$main, results$effects$interaction)
      if (length(all_effects) < 3) {
        stop("At least 3 effects are required for a normal probability plot.",
             call. = FALSE)
      }

      effect_names <- names(all_effects)
      signed_effects <- unlist(all_effects)
      n <- length(signed_effects)

      # Normal quantiles using the Blom plotting position (i - 3/8) / (n + 1/4).
      plot_data <- data.frame(
        Effect    = effect_names,
        Value     = signed_effects,
        Norm_Q    = stats::qnorm((seq_len(n) - 0.375) / (n + 0.25)),
        stringsAsFactors = FALSE
      )
      plot_data <- plot_data[order(plot_data$Value), ]
      plot_data$Rank <- seq_len(n)

      # Compute Lenth PSE for reference lines.
      pse_info <- NULL
      tryCatch({
        analyzer <- DoeAnalyzer$new()
        pse_info <- analyzer$compute_lenth_pse(unlist(all_effects))
      }, error = function(e) { })

      point_col  <- .iqr_plotter$.pal_ui(theme_obj, "primary")
      active_col <- .iqr_plotter$.pal_ui(theme_obj, "danger")
      label_col  <- .iqr_plotter$.pal_ui(theme_obj, "text", default = "black")
      me_col     <- .iqr_plotter$.pal_ui(theme_obj, "warning")

      # Determine which points are "active" (beyond ME).
      if (!is.null(pse_info)) {
        plot_data$Active <- abs(plot_data$Value) > pse_info$me
        p <- ggplot(plot_data, aes(x = Norm_Q, y = Value, color = Active)) +
          geom_point(size = 3) +
          scale_color_manual(
            values = c(`FALSE` = point_col, `TRUE` = active_col),
            guide = "none"
          )
      } else {
        p <- ggplot(plot_data, aes(x = Norm_Q, y = Value)) +
          geom_point(size = 3, color = point_col)
      }

      # Reference line through the quartiles (standard normal probability
      # plot reference).
      q1_idx <- ceiling(n * 0.25)
      q3_idx <- floor(n * 0.75)
      if (q1_idx >= 1 && q3_idx <= n && q1_idx != q3_idx) {
        x1 <- plot_data$Norm_Q[q1_idx]
        y1 <- plot_data$Value[q1_idx]
        x3 <- plot_data$Norm_Q[q3_idx]
        y3 <- plot_data$Value[q3_idx]
        slope <- (y3 - y1) / (x3 - x1)
        intercept <- y1 - slope * x1
        p <- p + geom_abline(slope = slope, intercept = intercept,
                              color = "gray50", linetype = "dashed",
                              linewidth = 0.5)
      }

      p <- p +
        geom_text(aes(label = Effect), vjust = -0.6, hjust = 0.4,
                  size = 3, color = label_col) +
        labs(
          title = "Normal Probability Plot of Effects",
          subtitle = "Signed effects vs normal quantiles; deviations from the line are active",
          x = "Normal quantiles",
          y = "Effect estimate"
        ) +
        theme_minimal()

      # Add Lenth ME and SME reference lines (vertical at +/- ME, +/- SME).
      if (!is.null(pse_info)) {
        p <- p +
          geom_hline(yintercept = pse_info$me,
                     linetype = "dashed", color = me_col, linewidth = 0.6) +
          geom_hline(yintercept = -pse_info$me,
                     linetype = "dashed", color = me_col, linewidth = 0.6) +
          geom_hline(yintercept = pse_info$sme,
                     linetype = "dotted", color = active_col, linewidth = 0.6) +
          geom_hline(yintercept = -pse_info$sme,
                     linetype = "dotted", color = active_col, linewidth = 0.6) +
          annotate("text", x = max(plot_data$Norm_Q),
                   y = pse_info$me, vjust = -0.6, hjust = 1,
                   color = me_col, size = 3,
                   label = sprintf("ME = %.3f", pse_info$me)) +
          annotate("text", x = max(plot_data$Norm_Q),
                   y = pse_info$sme, vjust = -0.6, hjust = 1,
                   color = active_col, size = 3,
                   label = sprintf("SME = %.3f", pse_info$sme))
      }

      p <- private$.apply_theme(p, theme_obj)
      invisible(p)
    },

    # =========================================================================
    # Cube Plot (3-factor corner means)
    # =========================================================================
    #
    # The cube plot displays the mean response at each corner of the
    # 2^k factorial cube (all factors at +/-1). For k=3 factors, the
    # classic Minitab cube is rendered as an isometric 3D projection.
    # For k=2, a 2D square is drawn; for k>3, a faceted panel of 2D
    # slices is shown (first 3 factors by default).
    #
    # Reference:
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments
    #   (10th ed.), sec. 6.3.
    .plot_cube = function(results, theme_obj, plan, response_name = NULL) {
      if (is.null(results$design_info)) {
        stop("Design not available; run task$compute() first.", call. = FALSE)
      }

      factor_names <- vapply(plan$factors, function(f) f$name, character(1))
      k <- length(factor_names)

      # Select up to 3 factors for the cube display.
      if (k < 2) {
        stop("Cube plot requires at least 2 factors.", call. = FALSE)
      }
      cube_factors <- factor_names[1:min(k, 3)]
      k_cube <- length(cube_factors)

      # Need response data to compute corner means.
      if (is.null(results$anova_results)) {
        stop("Response data required for cube plot; run task$compute() ",
             "with response data.", call. = FALSE)
      }
      if (is.null(response_name)) {
        response_name <- names(results$anova_results)[1]
      }
      if (!response_name %in% names(results$anova_results)) {
        stop("Response '", response_name, "' not found in results.",
             call. = FALSE)
      }

      # Get the model and its model frame (contains the design + response).
      model <- results$anova_results[[response_name]]$model
      mf <- stats::model.frame(model)

      # Extract factor columns and the response.
      design_data <- mf[, c(cube_factors, response_name), drop = FALSE]

      # Round factor values to -1/0/+1 for grouping (handles floating
      # point precision from coded designs).
      for (fname in cube_factors) {
        design_data[[fname]] <- round(design_data[[fname]])
      }

      # Keep only the corner points (all factors at +/-1).
      corner_mask <- rowSums(abs(design_data[, cube_factors, drop = FALSE]) != 1) == 0
      corner_data <- design_data[corner_mask, , drop = FALSE]

      if (nrow(corner_data) == 0) {
        stop("No corner points (all factors at +/-1) found in the design.",
             call. = FALSE)
      }

      # Compute mean response at each corner.
      agg_formula <- as.formula(paste(response_name, "~",
                                       paste(cube_factors, collapse = " + ")))
      corner_means <- aggregate(agg_formula, data = corner_data,
                                 FUN = mean, na.rm = TRUE)
      names(corner_means)[ncol(corner_means)] <- "Mean"

      ui <- private$.ui_colors(theme_obj)
      text_col <- ui$text
      edge_col <- "gray40"

      if (k_cube == 2) {
        # 2D square: 4 corners.
        x1 <- cube_factors[1]
        x2 <- cube_factors[2]
        p <- ggplot(corner_means, aes(x = .data[[x1]], y = .data[[x2]])) +
          geom_segment(x = -1, y = -1, xend = 1, yend = -1,
                       color = edge_col, linewidth = 0.6) +
          geom_segment(x = -1, y = 1, xend = 1, yend = 1,
                       color = edge_col, linewidth = 0.6) +
          geom_segment(x = -1, y = -1, xend = -1, yend = 1,
                       color = edge_col, linewidth = 0.6) +
          geom_segment(x = 1, y = -1, xend = 1, yend = 1,
                       color = edge_col, linewidth = 0.6) +
          geom_point(size = 6, color = ui$primary) +
          geom_text(aes(label = sprintf("%.2f", Mean)),
                    color = text_col, size = 3.5, fontface = "bold") +
          scale_x_continuous(breaks = c(-1, 1), labels = c("low", "high")) +
          scale_y_continuous(breaks = c(-1, 1), labels = c("low", "high")) +
          coord_fixed(ratio = 1) +
          labs(
            title = sprintf("Cube Plot of %s", response_name),
            subtitle = sprintf("Mean response at %d corner points", nrow(corner_means)),
            x = x1, y = x2
          ) +
          theme_minimal()
      } else {
        # 3D isometric projection for 3 factors.
        x1 <- cube_factors[1]
        x2 <- cube_factors[2]
        x3 <- cube_factors[3]

        # Isometric projection angles.
        angle_x <- 0.5
        angle_y <- 0.9

        # Project 3D -> 2D.
        project <- function(x, y, z) {
          px <- x + angle_x * z
          py <- y + angle_y * z
          data.frame(px = px, py = py)
        }

        # Corner positions in coded space.
        corners <- expand.grid(x = c(-1, 1), y = c(-1, 1), z = c(-1, 1))
        proj <- project(corners$x, corners$y, corners$z)
        corner_means$px <- proj$px
        corner_means$py <- proj$py

        # Build cube edges.
        edges <- data.frame(
          x = c(-1, 1, -1, 1, -1, 1, -1, 1,
                -1, -1, 1, 1, -1, -1, 1, 1,
                -1, 1, -1, 1, -1, -1, 1, 1),
          y = c(-1, -1, 1, 1, -1, -1, 1, 1,
                -1, 1, -1, 1, -1, 1, -1, 1,
                -1, -1, 1, 1, 1, 1, -1, -1),
          z = c(-1, -1, -1, -1, 1, 1, 1, 1,
                -1, -1, -1, -1, 1, 1, 1, 1,
                -1, -1, -1, 1, -1, -1, -1, 1),
          group = rep(1:12, each = 2)
        )
        edge_proj <- project(edges$x, edges$y, edges$z)

        p <- ggplot() +
          geom_path(data = edge_proj,
                    aes(x = px, y = py, group = edges$group),
                    color = edge_col, linewidth = 0.5) +
          geom_point(data = corner_means, aes(x = px, y = py),
                      size = 7, color = ui$primary) +
          geom_text(data = corner_means,
                    aes(x = px, y = py, label = sprintf("%.2f", Mean)),
                    color = text_col, size = 3.2, fontface = "bold") +
          labs(
            title = sprintf("Cube Plot of %s", response_name),
            subtitle = sprintf("Mean response at 8 corner points (%s, %s, %s)",
                               x1, x2, x3),
            x = "", y = ""
          ) +
          theme_minimal() +
          theme(axis.text = element_blank(), axis.ticks = element_blank())
      }

      p <- private$.apply_theme(p, theme_obj)
      invisible(p)
    },

    # =========================================================================
    # Power Curve Plot
    # =========================================================================
    #
    # Renders the power-vs-effect-size curve computed by
    # DoeAnalyzer$plot_power_curve(). The curve shows how statistical power
    # increases with effect size, holding n_factors, n_replicates, and sigma
    # fixed. Reference lines at 80% (adequate) and 95% (high) power are
    # drawn so the user can read off the minimum detectable effect.
    #
    # Reference:
    #   Montgomery, D. C. (2019). Design and Analysis of Experiments
    #   (10th ed.), sec. 7.2.
    .plot_power_curve = function(results, theme_obj, plan) {
      # Extract power curve data from the results list (if pre-computed)
      # or fall back to computing it from plan parameters.
      curve_data <- results$power_curve_data
      if (is.null(curve_data)) {
        # Try to compute from the plan's power parameters.
        n_factors <- length(plan$factors)
        sigma <- if (!is.null(plan$sigma)) plan$sigma else 1.0
        n_replicates <- plan$replication
        n_center <- plan$center_points
        analyzer <- DoeAnalyzer$new()
        curve_data <- analyzer$plot_power_curve(
          n_factors = n_factors, n_replicates = n_replicates,
          sigma = sigma, n_center_points = n_center
        )
      }

      ui <- private$.ui_colors(theme_obj)
      line_col <- ui$primary
      target_col <- ui$warning
      high_col <- ui$success

      # Inline percent formatter avoids an undeclared dependency on the
      # scales package (ggplot2 pulls it in, but R CMD check still flags
      # undeclared :: usage). Round to integer percent for readability.
      pct_fmt <- function(x) paste0(round(as.numeric(x) * 100), "%")
      n_runs_attr <- attr(curve_data, "n_runs")
      n_runs_label <- if (is.null(n_runs_attr)) nrow(curve_data) else n_runs_attr

      p <- ggplot(curve_data, aes(x = Effect_Size, y = Power)) +
        geom_line(color = line_col, linewidth = 1) +
        geom_hline(yintercept = 0.80, linetype = "dashed",
                   color = target_col, linewidth = 0.6) +
        geom_hline(yintercept = 0.95, linetype = "dotted",
                   color = high_col, linewidth = 0.6) +
        annotate("text", x = max(curve_data$Effect_Size), y = 0.80,
                 vjust = -0.6, hjust = 1, color = target_col, size = 3,
                 label = "80% (adequate)") +
        annotate("text", x = max(curve_data$Effect_Size), y = 0.95,
                 vjust = -0.6, hjust = 1, color = high_col, size = 3,
                 label = "95% (high)") +
        scale_y_continuous(limits = c(0, 1), labels = pct_fmt) +
        labs(
          title = "Power Curve",
          subtitle = sprintf("Power vs effect size (n = %d runs, sigma = %.2f)",
                              n_runs_label,
                              if (!is.null(plan$sigma)) plan$sigma else 1.0),
          x = "Effect size (Delta)",
          y = "Power"
        ) +
        theme_minimal()

      p <- private$.apply_theme(p, theme_obj)
      invisible(p)
    },

    # =========================================================================
    # Private: Wireframe plot (3D surface as line mesh, no tile fill)
    # =========================================================================
    #
    # Minitab offers both a filled surface (geom_tile) and a wireframe
    # (line-mesh) representation. The wireframe is useful when the analyst
    # wants to see the underlying grid structure and iso-response lines
    # without the visual weight of a filled gradient. We approximate the 3D
    # wireframe using geom_path for the x-direction and y-direction grid
    # lines, colored by predicted response so the surface shape is still
    # legible.
    .plot_wireframe = function(results, theme_obj, plan,
                               response_name = NULL,
                               x_var = NULL, y_var = NULL) {
      # Reuse the response-surface prediction grid by calling the shared
      # builder. We replicate the grid construction inline rather than
      # calling .plot_response_surface() to avoid returning the full list
      # (surface + contour + combined + interactive).
      model <- if (!is.null(response_name) &&
                    !is.null(results$anova_results[[response_name]])) {
        results$anova_results[[response_name]]$model
      } else {
        results$model
      }
      if (is.null(model)) {
        stop("Model is required for wireframe plotting", call. = FALSE)
      }
      if (is.null(response_name)) {
        response_name <- attr(model$terms, "variables")[[2]]
        response_name <- as.character(response_name)
      }

      model_data <- model$model
      response_col <- names(model_data)[1]
      factor_cols <- setdiff(names(model_data), response_col)
      plan_factor_names <- if (!is.null(plan) && !is.null(plan$factors)) {
        vapply(plan$factors, function(f) f$name, character(1))
      } else {
        character(0)
      }
      if (length(plan_factor_names) > 0) {
        factor_cols <- intersect(factor_cols, plan_factor_names)
      } else {
        factor_cols <- factor_cols[!grepl("^I\\(|:|\\^", factor_cols)]
      }
      numeric_factors <- factor_cols[sapply(model_data[factor_cols], is.numeric)]
      if (length(numeric_factors) < 2) {
        stop("At least 2 numeric factors are required for wireframe plotting",
             call. = FALSE)
      }
      if (is.null(x_var)) x_var <- numeric_factors[1]
      if (is.null(y_var)) y_var <- numeric_factors[2]
      if (!x_var %in% numeric_factors || !y_var %in% numeric_factors) {
        stop("x_var and y_var must be among the numeric factors: ",
             paste(numeric_factors, collapse = ", "), call. = FALSE)
      }
      if (x_var == y_var) {
        stop("x_var and y_var must be different factors.", call. = FALSE)
      }

      x_range <- range(model_data[[x_var]], na.rm = TRUE)
      y_range <- range(model_data[[y_var]], na.rm = TRUE)
      grid_size <- 30
      x_seq <- seq(x_range[1], x_range[2], length.out = grid_size)
      y_seq <- seq(y_range[1], y_range[2], length.out = grid_size)
      grid <- do.call(expand.grid, setNames(list(x_seq, y_seq), c(x_var, y_var)))
      other_factors <- setdiff(numeric_factors, c(x_var, y_var))
      if (length(other_factors) > 0) {
        for (fac in other_factors) {
          grid[[fac]] <- median(model_data[[fac]], na.rm = TRUE)
        }
      }
      tryCatch({
        m_local <- model
        attr(m_local$terms, ".Environment") <- baseenv()
        environment(m_local$terms) <- baseenv()
        grid$predicted <- predict(m_local, newdata = grid)
      }, error = function(e) {
        stop("Failed to generate predictions: ", e$message, call. = FALSE)
      })

      cont_colors <- private$.continuous_colors(theme_obj)
      low_col  <- cont_colors[1]
      high_col <- cont_colors[length(cont_colors)]
      mid_col  <- if (length(cont_colors) >= 3) {
        cont_colors[ceiling(length(cont_colors) / 2)]
      } else {
        "white"
      }
      mid_val <- median(grid$predicted, na.rm = TRUE)

      # Build line segments along x (fix y) and along y (fix x) so the
      # mesh resembles a 3D wireframe projected onto 2D.
      p <- ggplot(grid, aes(.data[[x_var]], .data[[y_var]])) +
        # Lines of constant y (vary x) — "horizontal" mesh lines.
        geom_path(aes(group = .data[[y_var]],
                      color = .data[["predicted"]]),
                  alpha = 0.7, linewidth = 0.5) +
        # Lines of constant x (vary y) — "vertical" mesh lines.
        geom_path(aes(group = .data[[x_var]],
                      color = .data[["predicted"]]),
                  alpha = 0.7, linewidth = 0.5, orientation = "y") +
        scale_color_gradient2(
          low = low_col, mid = mid_col, high = high_col,
          midpoint = mid_val, name = response_name
        ) +
        labs(
          title = paste("Wireframe Plot:", response_name),
          subtitle = sprintf("Factors: %s vs %s", x_var, y_var),
          x = x_var,
          y = y_var
        ) +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          legend.position = "right"
        )

      p <- private$.apply_theme(p, theme_obj)
      invisible(p)
    },

    # =========================================================================
    # Private: Residuals vs Predictors diagnostic plot
    # =========================================================================
    #
    # Minitab's "Residuals vs variables" plot displays residuals against each
    # predictor. Patterns (curvature, fanning) in these panels indicate model
    # inadequacy — e.g. a missing quadratic term or heteroscedasticity tied
    # to a specific factor. A multi-panel patchwork layout is used when more
    # than one predictor is available.
    .plot_residuals_vs_predictors = function(results, theme_obj, plan,
                                              response_name = NULL) {
      model <- if (!is.null(response_name) &&
                    !is.null(results$anova_results[[response_name]])) {
        results$anova_results[[response_name]]$model
      } else {
        results$model
      }
      if (is.null(model)) {
        stop("Model is required for residuals-vs-predictors plotting",
             call. = FALSE)
      }

      model_data <- model$model
      response_col <- names(model_data)[1]
      factor_cols <- setdiff(names(model_data), response_col)
      plan_factor_names <- if (!is.null(plan) && !is.null(plan$factors)) {
        vapply(plan$factors, function(f) f$name, character(1))
      } else {
        character(0)
      }
      if (length(plan_factor_names) > 0) {
        factor_cols <- intersect(factor_cols, plan_factor_names)
      } else {
        factor_cols <- factor_cols[!grepl("^I\\(|:|\\^", factor_cols)]
      }
      numeric_factors <- factor_cols[sapply(model_data[factor_cols], is.numeric)]
      if (length(numeric_factors) < 1) {
        stop("At least 1 numeric factor is required for residuals-vs-predictors",
             call. = FALSE)
      }

      resid_vals <- stats::residuals(model)
      plot_df <- model_data
      plot_df$.residual <- as.numeric(resid_vals)

      ui <- private$.ui_colors(theme_obj)
      point_col <- ui$primary

      build_panel <- function(var) {
        ggplot(plot_df, aes(.data[[var]], .data[[".residual"]])) +
          geom_point(color = point_col, alpha = 0.7, size = 2) +
          geom_hline(yintercept = 0, linetype = "dashed",
                     color = "gray50") +
          geom_smooth(method = "loess", se = FALSE, color = ui$danger,
                      linewidth = 0.5, formula = y ~ x) +
          labs(title = var, x = var, y = "Residual") +
          theme_minimal()
      }

      panels <- lapply(numeric_factors, build_panel)
      p <- if (length(panels) == 1) {
        panels[[1]]
      } else {
        patchwork::wrap_plots(panels, ncol = 2)
      }
      p <- p + patchwork::plot_annotation(
        title = "Residuals vs Predictors",
        subtitle = sprintf("Response: %s", response_col)
      )
      p <- private$.apply_theme(p, theme_obj)
      invisible(p)
    }
  )
)
