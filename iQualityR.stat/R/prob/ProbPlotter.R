# =============================================================================
# File: R/prob/ProbPlotter.R
# Description: Probability distribution plotting engine (optimized - split long methods + ggrepel)
# =============================================================================

#' @title ProbPlotter: Probability distribution plotting engine
#' @description
#' Responsible for generating PDF/CDF plots, shaded areas, label annotations, etc.
#' Uses ggrepel to avoid label overlap, reuses IqrTheme for consistent visual style.
#'
#' @export
ProbPlotter <- R6::R6Class("ProbPlotter",
  public = list(
    #' @description Render plot
    #' @param nodes Distribution node list
    #' @param calc_results Calculation result list
    #' @param facet Whether to display as facets
    #' @param show_cdf Whether to show CDF plot
    #' @param mode Calculation mode
    #' @param theme_obj IqrTheme object
    #' @return ggplot/patchwork object
    render = function(nodes, calc_results, facet, show_cdf, mode, theme_obj) {
      # 1. Calculate view boundary
      xr <- private$.calc_x_range(nodes, calc_results)

      # 2. Generate plot data
      plot_data <- private$.build_plot_data(nodes, xr, calc_results)

      # 3. Build PDF plot
      p1 <- private$.build_pdf_plot(plot_data, calc_results, facet, themeobj = theme_obj)

      # 4. Determine if CDF plot is needed
      must_show_cdf <- show_cdf || any(sapply(calc_results, function(r) !r$all_res[[1]]$is_prob_mode))

      if (must_show_cdf) {
        p2 <- private$.build_cdf_plot(plot_data, calc_results, nodes, facet, theme_obj)
        return(p1 / p2 + patchwork::plot_layout(guides = "collect") &
                 ggplot2::theme(legend.position = "bottom"))
      }

      return(p1)
    }
  ),

  private = list(
    # --- Calculate X axis range ---

    .calc_x_range = function(nodes, calc_results) {
      # Extract all points from calculation results
      all_points <- unlist(lapply(calc_results, function(r) {
        sapply(r$all_res, function(item) unlist(item$target_x))
      }))

      # Extract range from distribution quantiles
      xr_base <- lapply(nodes, function(n) c(n$q(0.0005), n$q(0.9995)))
      xr <- range(c(unlist(xr_base), all_points), na.rm = TRUE)

      # Expand 10% boundary
      xr + c(-1, 1) * 0.1 * diff(xr)
    },

    # --- Build plot data ---

    .build_plot_data = function(nodes, xr, calc_results) {
      do.call(rbind, lapply(names(nodes), function(id) {
        node <- nodes[[id]]
        x_s <- if (node$is_discrete) {
          seq(floor(xr[1]), ceiling(xr[2]))
        } else {
          seq(xr[1], xr[2], length.out = 600)
        }

        data.frame(x = x_s, group = id, is_discrete = node$is_discrete) %>%
          dplyr::mutate(
            pdf = sapply(x, node$d),
            cdf = sapply(x, node$p),
            in_shade = factor(calc_results[[id]]$shade_f(x), levels = c("TRUE", "FALSE")),
            pdf_shade = ifelse(in_shade == "TRUE", pdf, NA)
          )
      }))
    },

    # --- Build PDF plot ---

    .build_pdf_plot = function(plot_data, calc_results, facet, themeobj = NULL) {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x, pdf, color = group)) +
        ggplot2::labs(title = "Probability Density Analysis", y = "Density", x = NULL)

      if (!is.null(themeobj)) {
        p <- p + themeobj$plot$theme_iqr() +
               themeobj$plot$scale_color_iqr() +
               themeobj$plot$scale_fill_iqr()
      }

      # Discrete distribution uses geom_col
      if (any(plot_data$is_discrete)) {
        p <- p + ggplot2::geom_col(
          data = dplyr::filter(plot_data, is_discrete),
          ggplot2::aes(fill = group, alpha = in_shade),
          position = ggplot2::position_dodge(width = 0.5), width = 0.7
        ) +
          ggplot2::scale_alpha_manual(
            values = c("TRUE" = 0.8, "FALSE" = 0.2), guide = "none"
          )
      }

      # Continuous distribution uses geom_line + geom_ribbon
      if (any(!plot_data$is_discrete)) {
        p <- p +
          ggplot2::geom_line(data = dplyr::filter(plot_data, !is_discrete), linewidth = 1) +
          ggplot2::geom_ribbon(
            data = dplyr::filter(plot_data, !is_discrete),
            ggplot2::aes(ymin = 0, ymax = pdf_shade, fill = group),
            alpha = 0.6, color = NA, show.legend = FALSE
          )
      }

      # Add labels (using ggrepel to prevent overlap)
      anno_df <- private$.prepare_label_data(calc_results, facet)
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p + ggrepel::geom_label_repel(
          data = anno_df,
          ggplot2::aes(x = Inf, y = Inf, label = label),
          hjust = 1.1, vjust = 1, show.legend = FALSE,
          direction = "y", nudge_x = -1, nudge_y = 1
        )
      } else {
        p <- p + ggplot2::geom_label(
          data = anno_df,
          ggplot2::aes(x = Inf, y = Inf, label = label),
          hjust = 1.1, show.legend = FALSE
        )
      }

      if (facet) p <- p + ggplot2::facet_wrap(~group)
      p
    },

    # --- Build CDF plot ---

    .build_cdf_plot = function(plot_data, calc_results, nodes, facet, themeobj) {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x, cdf, color = group)) +
        (if (any(plot_data$is_discrete)) ggplot2::geom_step(linewidth = 1)
         else ggplot2::geom_line(linewidth = 1)) +
        ggplot2::labs(y = "Cumulative F(x)", x = "Value")

      if (!is.null(themeobj)) {
        p <- p + themeobj$plot$theme_iqr() + themeobj$plot$scale_color_iqr()
      }

      # Build guide data
      mk <- private$.build_guide_lines(calc_results, nodes)

      if (!is.null(mk) && nrow(mk) > 0) {
        p <- p +
          # Vertical dashed line
          ggplot2::geom_segment(
            data = mk, ggplot2::aes(x = x, xend = x, y = 0, yend = y),
            linetype = "dashed", alpha = 0.7, show.legend = FALSE
          ) +
          # Horizontal dotted line
          ggplot2::geom_segment(
            data = mk, ggplot2::aes(x = -Inf, xend = x, y = y, yend = y),
            linetype = "dotted", alpha = 0.7, show.legend = FALSE
          ) +
          # X-axis value annotation
          ggplot2::geom_text(
            data = mk, ggplot2::aes(x = x, y = 0, label = round(x, 2)),
            vjust = 1.5, size = 3, fontface = "bold", show.legend = FALSE
          ) +
          # Y-axis value annotation
          ggplot2::geom_text(
            data = mk, ggplot2::aes(x = -Inf, y = y, label = sprintf("%.2f", y)),
            hjust = -0.2, vjust = -0.5, size = 3, show.legend = FALSE
          )
      }

      if (facet) p <- p + ggplot2::facet_wrap(~group)
      p
    },

    # --- Build guide lines ---

    .build_guide_lines = function(calc_results, nodes) {
      mk_list <- list()
      for (id in names(calc_results)) {
        node <- nodes[[id]]
        res_bundle <- calc_results[[id]]
        for (item in res_bundle$all_res) {
          curr_x <- as.numeric(unlist(item$target_x))
          if (length(curr_x) > 0) {
            curr_y <- sapply(curr_x, function(v) node$p(v))
            mk_list[[length(mk_list) + 1]] <- data.frame(
              group = id,
              x = curr_x,
              y = curr_y,
              stringsAsFactors = FALSE
            )
          }
        }
      }
      if (length(mk_list) > 0) do.call(rbind, mk_list) else NULL
    },

    # --- Prepare label data ---

    .prepare_label_data = function(calc_results, facet) {
      anno_df <- data.frame(
        group = names(calc_results),
        label = sapply(calc_results, `[[`, "pdf_lbl"),
        stringsAsFactors = FALSE
      )
      if (!facet && nrow(anno_df) > 1) {
        anno_df$v_offset <- seq(1.1, 1.1 + (nrow(anno_df) - 1) * 1.5, length.out = nrow(anno_df))
      } else {
        anno_df$v_offset <- 1.1
      }
      anno_df
    }
  )
)
