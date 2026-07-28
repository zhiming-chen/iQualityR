# =============================================================================
# File: R/AttrGagePlotter.R
# Description: Attribute agreement plotter.
# =============================================================================

#' @title AttrGagePlotter
#' @description Plotter for attribute agreement analysis.
#'
#' @param results Results list from [AttrGageTask]$build_results() or analyzer raw output.
#' @param theme_obj Theme object produced by IqrTheme.
#' @param type Plot type: `"summary"` or `"list"`.
#' @param top_n Integer scalar number of samples to display in disagreement plot.
#' @param ... Additional arguments passed to downstream methods.
#'
#' @export
AttrGagePlotter <- R6::R6Class(
  "AttrGagePlotter",
  inherit = IqrPlotterBase,
  public = list(
    initialize = function() {
      invisible(self)
    },

    available_plots = function(results = NULL) {
      c(
        "summary",
        "list",
        "kappa_benchmark",
        "kappa_funnel",
        "pairwise_agreement",
        "appraiser_standard",
        "kendall_ordinal",
        "response_agreement",
        "sample_disagreement",
        "confusion_matrix",
        "detection_ci",
        "detection_metrics"
      )
    },

    render = function(results, theme_obj = NULL, type = "summary", ...) {
      dots <- list(...)
      if (is.null(theme_obj) && !is.null(dots$theme)) {
        theme_obj <- dots$theme
      }
      theme_obj <- private$resolve_theme(theme_obj)
      type <- private$normalize_type(type)
      plots <- self$generate_plots(results, theme_obj)

      if (type == "list") {
        return(plots)
      }
      if (type == "summary") {
        if (length(plots) == 0) {
          stop("No attribute agreement plots are available for these results.", call. = FALSE)
        }
        return(patchwork::wrap_plots(plots, ncol = if (length(plots) > 2) 2 else 1))
      }

      plots[[type]] %||% stop("Unknown attribute agreement plot type: ", type, call. = FALSE)
    },

    generate_plots = function(results, theme_obj) {
      kappa <- private$get_kappa_results(results)
      detection <- private$get_detection_results(results)
      plots <- list()

      if (!is.null(kappa)) {
        plots$kappa_benchmark <- self$plot_kappa_benchmark(kappa, theme_obj)
        plots$confusion_matrix <- tryCatch(
          self$plot_confusion_matrix(kappa, theme_obj),
          error = function(e) NULL
        )
        plots$pairwise_agreement <- tryCatch(
          self$plot_pairwise_agreement(kappa, theme_obj),
          error = function(e) NULL
        )
        plots$appraiser_standard <- tryCatch(
          self$plot_appraiser_standard(kappa, theme_obj),
          error = function(e) NULL
        )
        plots$response_agreement <- tryCatch(
          self$plot_response_agreement(kappa, theme_obj),
          error = function(e) NULL
        )
        plots$sample_disagreement <- tryCatch(
          self$plot_sample_disagreement(kappa, theme_obj),
          error = function(e) NULL
        )
        plots$kendall_ordinal <- tryCatch(
          self$plot_kendall_ordinal(kappa, theme_obj),
          error = function(e) NULL
        )
      }

      if (!is.null(detection)) {
        plots$detection_ci <- self$plot_detection_ci(detection, theme_obj)
        plots$detection_metrics <- self$plot_detection_metrics(detection, theme_obj)
      }

      Filter(Negate(is.null), plots)
    },

    plot_kappa_funnel = function(results, theme_obj = NULL) {
      self$plot_kappa_benchmark(results, theme_obj)
    },

    plot_kappa_benchmark = function(results, theme_obj = NULL) {
      theme_obj <- private$resolve_theme(theme_obj)
      results <- private$get_kappa_results(results) %||% results
      kappa_val <- results$kappa %||% results$V
      if (is.null(kappa_val)) {
        stop("Kappa result does not contain a kappa value.", call. = FALSE)
      }

      ci <- results$ci %||% c(NA_real_, NA_real_)
      colors <- private$palette(theme_obj, 5)
      semantic <- private$agreement_palette(theme_obj)
      status_color <- private$kappa_status_color(kappa_val, theme_obj)

      bands <- data.frame(
        xmin = c(-1, 0, 0.2, 0.4, 0.6, 0.8),
        xmax = c(0, 0.2, 0.4, 0.6, 0.8, 1),
        level = factor(
          c("Poor", "Slight", "Fair", "Moderate", "Substantial", "Almost perfect"),
          levels = c("Poor", "Slight", "Fair", "Moderate", "Substantial", "Almost perfect")
        )
      )

      point_data <- data.frame(y = 1, estimate = kappa_val, lower = ci[1], upper = ci[2])

      ggplot2::ggplot() +
        ggplot2::geom_rect(
          data = bands,
          ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = 0.5, ymax = 1.5, fill = .data$level),
          alpha = 0.22,
          color = NA
        ) +
        ggplot2::geom_errorbarh(
          data = point_data,
          ggplot2::aes(xmin = .data$lower, xmax = .data$upper, y = .data$y),
          height = 0.18,
          linewidth = 0.9,
          color = colors[[2]]
        ) +
        ggplot2::geom_point(
          data = point_data,
          ggplot2::aes(x = .data$estimate, y = .data$y),
          size = 3.8,
          color = status_color,
          fill = status_color
        ) +
        ggplot2::scale_x_continuous(
          limits = c(-0.05, 1.05),
          breaks = seq(0, 1, 0.2),
          labels = scales::label_number(accuracy = 0.1)
        ) +
        ggplot2::scale_y_continuous(limits = c(0.45, 1.55), breaks = NULL) +
        ggplot2::scale_fill_manual(values = semantic$bands) +
        ggplot2::labs(
          title = "Kappa Agreement Benchmark",
          subtitle = sprintf(
            "%s = %.3f%s",
            results$method %||% "Kappa",
            kappa_val,
            if (all(!is.na(ci))) sprintf(" (95%% CI %.3f to %.3f)", ci[1], ci[2]) else ""
          ),
          x = "Kappa",
          y = NULL,
          fill = "Agreement"
        ) +
        theme_obj$theme_iqr() +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          legend.position = "bottom"
        )
    },

    plot_confusion_matrix = function(results, theme_obj = NULL) {
      theme_obj <- private$resolve_theme(theme_obj)
      results <- private$get_kappa_results(results) %||% results
      colors <- private$palette(theme_obj, 3)

      if (!is.null(results$confusion_matrix)) {
        cm <- results$confusion_matrix
        mat <- matrix(c(cm$TP, cm$FN, cm$FP, cm$TN), nrow = 2, byrow = TRUE)
        rownames(mat) <- c("Reference positive", "Reference negative")
        colnames(mat) <- c("Test positive", "Test negative")
        long_data <- reshape2::melt(mat)
        names(long_data) <- c("Reference", "Test", "Count")
        title <- "Confusion Matrix"
        subtitle <- "Reference classification versus test classification"
      } else if (!is.null(results$rating_matrix)) {
        long_data <- reshape2::melt(results$rating_matrix)
        names(long_data) <- c("Sample", "Category", "Count")
        long_data$Reference <- long_data$Sample
        long_data$Test <- long_data$Category
        title <- "Rating Matrix"
        subtitle <- "Counts by sample and category"
      } else {
        stop("Kappa result does not contain a confusion matrix or rating matrix.", call. = FALSE)
      }

      ggplot2::ggplot(long_data, ggplot2::aes(x = .data$Test, y = .data$Reference, fill = .data$Count)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = .data$Count), size = 4.2, fontface = "bold") +
        ggplot2::scale_fill_gradient(low = "white", high = colors[[1]]) +
        ggplot2::labs(
          title = title,
          subtitle = subtitle,
          x = "Test result",
          y = "Reference",
          fill = "Count"
        ) +
        theme_obj$theme_iqr() +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
        )
    },

    plot_pairwise_agreement = function(results, theme_obj = NULL) {
      theme_obj <- private$resolve_theme(theme_obj)
      results <- private$get_kappa_results(results) %||% results
      tbl <- results$pairwise_appraisers
      if (is.null(tbl) || nrow(tbl) == 0) {
        stop("Kappa result does not contain pairwise appraiser results.", call. = FALSE)
      }
      colors <- private$palette(theme_obj, 4)
      tbl$Comparison <- factor(tbl$Comparison, levels = rev(tbl$Comparison))
      threshold_data <- private$kappa_thresholds()
      z_crit <- stats::qnorm(1 - (1 - (get0("conf_level", ifnotfound = 0.95) %||% 0.95)) / 2)

      ggplot2::ggplot(tbl, ggplot2::aes(x = .data$Kappa, y = .data$Comparison)) +
        ggplot2::geom_vline(
          data = threshold_data,
          ggplot2::aes(xintercept = .data$x, color = .data$level),
          linetype = "dashed",
          linewidth = 0.65,
          inherit.aes = FALSE
        ) +
        ggplot2::geom_errorbarh(
          ggplot2::aes(
            xmin = pmax(-1, .data$Kappa - z_crit * .data$SE_Kappa),
            xmax = pmin(1, .data$Kappa + z_crit * .data$SE_Kappa)
          ),
          height = 0.18,
          color = colors[[2]],
          linewidth = 0.75
        ) +
        ggplot2::geom_point(size = 3.2, color = colors[[1]]) +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.1f%%", .data$Percent_Agreement)),
          nudge_x = 0.04,
          hjust = 0,
          size = 3.1
        ) +
        ggplot2::scale_x_continuous(limits = c(-0.05, 1.08), breaks = seq(0, 1, 0.2)) +
        ggplot2::scale_color_manual(values = private$threshold_palette(theme_obj), guide = "none") +
        ggplot2::labs(
          title = "Between-Appraiser Agreement",
          subtitle = "Kappa with approximate 95% intervals; labels show percent agreement",
          x = "Kappa",
          y = NULL
        ) +
        theme_obj$theme_iqr()
    },

    plot_appraiser_standard = function(results, theme_obj = NULL) {
      theme_obj <- private$resolve_theme(theme_obj)
      results <- private$get_kappa_results(results) %||% results
      tbl <- results$appraiser_vs_standard
      if (is.null(tbl) || nrow(tbl) == 0) {
        stop("Kappa result does not contain appraiser vs standard results.", call. = FALSE)
      }
      tbl <- tbl[!grepl("^All Appraisers", tbl$Comparison), , drop = FALSE]
      if (nrow(tbl) == 0) stop("No individual appraiser vs standard rows available.", call. = FALSE)
      colors <- private$palette(theme_obj, 4)
      tbl$Comparison <- factor(tbl$Comparison, levels = rev(tbl$Comparison))
      threshold_data <- private$kappa_thresholds()
      z_crit <- stats::qnorm(1 - (1 - (get0("conf_level", ifnotfound = 0.95) %||% 0.95)) / 2)

      ggplot2::ggplot(tbl, ggplot2::aes(x = .data$Kappa, y = .data$Comparison)) +
        ggplot2::geom_vline(
          data = threshold_data,
          ggplot2::aes(xintercept = .data$x, color = .data$level),
          linetype = "dashed",
          linewidth = 0.65,
          inherit.aes = FALSE
        ) +
        ggplot2::geom_errorbarh(
          ggplot2::aes(
            xmin = pmax(-1, .data$Kappa - z_crit * .data$SE_Kappa),
            xmax = pmin(1, .data$Kappa + z_crit * .data$SE_Kappa)
          ),
          height = 0.18,
          color = colors[[2]],
          linewidth = 0.75
        ) +
        ggplot2::geom_point(size = 3.2, color = colors[[1]]) +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.1f%%", .data$Percent_Agreement)),
          nudge_x = 0.04,
          hjust = 0,
          size = 3.1
        ) +
        ggplot2::scale_x_continuous(limits = c(-0.05, 1.08), breaks = seq(0, 1, 0.2)) +
        ggplot2::scale_color_manual(values = private$threshold_palette(theme_obj), guide = "none") +
        ggplot2::labs(
          title = "Each Appraiser vs Standard",
          subtitle = "Kappa against known reference classification",
          x = "Kappa",
          y = NULL
        ) +
        theme_obj$theme_iqr()
    },

    plot_response_agreement = function(results, theme_obj = NULL) {
      theme_obj <- private$resolve_theme(theme_obj)
      results <- private$get_kappa_results(results) %||% results
      tbl <- results$response_table
      if (is.null(tbl) || nrow(tbl) == 0) {
        stop("Kappa result does not contain response-level agreement.", call. = FALSE)
      }
      colors <- private$palette(theme_obj, 4)
      tbl$Response <- factor(tbl$Response, levels = tbl$Response)
      has_standard <- "Percent_Match_to_Standard" %in% names(tbl) &&
        any(!is.na(tbl$Percent_Match_to_Standard))
      has_pair_response <- "Percent_Agreement" %in% names(tbl) &&
        any(!is.na(tbl$Percent_Agreement))

      if (!has_standard && has_pair_response) {
        return(
          ggplot2::ggplot(tbl, ggplot2::aes(x = .data$Response, y = .data$Percent_Agreement, fill = .data$Response)) +
            ggplot2::geom_col(width = 0.62, alpha = 0.9) +
            ggplot2::geom_hline(yintercept = 90, linetype = "dashed", linewidth = 0.55, color = colors[[3]]) +
            ggplot2::geom_text(
              ggplot2::aes(label = sprintf("%.1f%%", .data$Percent_Agreement)),
              vjust = -0.35,
              size = 3.2
            ) +
            ggplot2::scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
            ggplot2::scale_fill_manual(values = rep(colors, length.out = nrow(tbl))) +
            ggplot2::labs(
              title = "Response-Level Agreement",
              subtitle = "Percent agreement by response category",
              x = "Response",
              y = "Agreement",
              fill = NULL
            ) +
            theme_obj$theme_iqr() +
            ggplot2::theme(legend.position = "none")
        )
      }

      if (!has_standard) {
        count_col <- if ("Ratings" %in% names(tbl)) "Ratings" else if ("N" %in% names(tbl)) "N" else NULL
        if (is.null(count_col)) {
          stop("Response table does not contain count or agreement fields.", call. = FALSE)
        }
        tbl$Response_Count <- tbl[[count_col]]
        return(
          ggplot2::ggplot(tbl, ggplot2::aes(x = .data$Response, y = .data$Response_Count, fill = .data$Response)) +
            ggplot2::geom_col(width = 0.62, alpha = 0.9) +
            ggplot2::geom_text(
              ggplot2::aes(label = .data$Response_Count),
              vjust = -0.35,
              size = 3.2
            ) +
            ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
            ggplot2::scale_fill_manual(values = rep(colors, length.out = nrow(tbl))) +
            ggplot2::labs(
              title = "Response Distribution",
              subtitle = "Counts by response category; no standard was supplied for match-to-standard rates",
              x = "Response",
              y = "Ratings",
              fill = NULL
            ) +
            theme_obj$theme_iqr() +
            ggplot2::theme(legend.position = "none")
        )
      }

      ggplot2::ggplot(tbl, ggplot2::aes(x = .data$Response, y = .data$Percent_Match_to_Standard, fill = .data$Response)) +
        ggplot2::geom_col(width = 0.62, alpha = 0.9) +
        ggplot2::geom_hline(yintercept = 90, linetype = "dashed", linewidth = 0.55, color = colors[[3]]) +
        ggplot2::geom_text(
          ggplot2::aes(label = ifelse(is.na(.data$Percent_Match_to_Standard), "NA", sprintf("%.1f%%", .data$Percent_Match_to_Standard))),
          vjust = -0.35,
          size = 3.2
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
        ggplot2::scale_fill_manual(values = rep(colors, length.out = nrow(tbl))) +
        ggplot2::labs(
          title = "Response-Level Agreement",
          subtitle = "Percent of ratings matching the known standard by response category",
          x = "Response",
          y = "Match to standard",
          fill = NULL
        ) +
        theme_obj$theme_iqr() +
        ggplot2::theme(legend.position = "none")
    },

    plot_sample_disagreement = function(results, theme_obj = NULL, top_n = 12) {
      theme_obj <- private$resolve_theme(theme_obj)
      results <- private$get_kappa_results(results) %||% results
      tbl <- results$sample_disagreement
      if (is.null(tbl) || nrow(tbl) == 0) {
        stop("Kappa result does not contain sample disagreement diagnostics.", call. = FALSE)
      }
      colors <- private$palette(theme_obj, 4)
      tbl <- tbl[order(-tbl$Discordant_Ratings, tbl$Percent_Match_to_Standard, tbl$Sample), , drop = FALSE]
      tbl <- utils::head(tbl, top_n)
      tbl$Sample <- factor(tbl$Sample, levels = rev(tbl$Sample))

      ggplot2::ggplot(tbl, ggplot2::aes(x = .data$Discordant_Ratings, y = .data$Sample)) +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, xend = .data$Discordant_Ratings, y = .data$Sample, yend = .data$Sample),
          linewidth = 0.7,
          color = self$.pal_ui(theme_obj, "muted")
        ) +
        ggplot2::geom_point(size = 3.4, color = self$.pal_semantic(theme_obj, "fail"), fill = self$.pal_semantic(theme_obj, "fail")) +
        ggplot2::geom_text(
          ggplot2::aes(label = paste0("modal ", sprintf("%.0f%%", .data$Percent_Modal))),
          hjust = -0.06,
          size = 3
        ) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
        ggplot2::labs(
          title = "Most Disagreed Samples",
          subtitle = paste0("Top ", nrow(tbl), " samples by number of non-modal ratings"),
          x = "Discordant ratings",
          y = "Sample"
        ) +
        theme_obj$theme_iqr() +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    },

    plot_kendall_ordinal = function(results, theme_obj = NULL) {
      theme_obj <- private$resolve_theme(theme_obj)
      results <- private$get_kappa_results(results) %||% results
      ordinal <- results$ordinal
      if (is.null(ordinal)) {
        stop("Kappa result does not contain ordinal Kendall statistics.", call. = FALSE)
      }

      rows <- list()
      if (!is.null(ordinal$within_appraiser) && nrow(ordinal$within_appraiser) > 0) {
        x <- ordinal$within_appraiser
        rows[[length(rows) + 1]] <- data.frame(
          Group = paste("Within", x$Appraiser),
          Family = "Within appraiser",
          Coef = x$Coef
        )
      }
      if (!is.null(ordinal$between_appraisers) && nrow(ordinal$between_appraisers) > 0) {
        x <- ordinal$between_appraisers
        rows[[length(rows) + 1]] <- data.frame(Group = x$Comparison, Family = "Between appraisers", Coef = x$Coef)
      }
      if (!is.null(ordinal$appraiser_vs_standard) && nrow(ordinal$appraiser_vs_standard) > 0) {
        x <- ordinal$appraiser_vs_standard
        rows[[length(rows) + 1]] <- data.frame(Group = x$Comparison, Family = "Appraiser vs standard", Coef = x$Coef)
      }
      if (!is.null(ordinal$all_appraisers_vs_standard) && nrow(ordinal$all_appraisers_vs_standard) > 0) {
        x <- ordinal$all_appraisers_vs_standard
        rows[[length(rows) + 1]] <- data.frame(Group = x$Comparison, Family = "All vs standard", Coef = x$Coef)
      }
      if (length(rows) == 0) stop("No ordinal Kendall rows available.", call. = FALSE)
      df <- do.call(rbind, rows)
      df <- df[!is.na(df$Coef), , drop = FALSE]
      if (nrow(df) == 0) stop("No finite ordinal Kendall coefficients available.", call. = FALSE)
      df$Group <- factor(df$Group, levels = rev(df$Group))

      ggplot2::ggplot(df, ggplot2::aes(x = .data$Coef, y = .data$Group, color = .data$Family)) +
        ggplot2::geom_vline(xintercept = 0.9, linetype = "dashed", linewidth = 0.65, color = "#2f855a") +
        ggplot2::geom_vline(xintercept = 0.75, linetype = "dashed", linewidth = 0.65, color = "#e9b949") +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, xend = .data$Coef, y = .data$Group, yend = .data$Group),
          linewidth = 0.7,
          color = "#b8c6d6",
          inherit.aes = FALSE
        ) +
        ggplot2::geom_point(size = 3.2) +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.3f", .data$Coef)),
          nudge_x = 0.025,
          hjust = 0,
          size = 3.1,
          color = "#1f2933"
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1.08), breaks = seq(0, 1, 0.2)) +
        ggplot2::scale_color_manual(values = c(
          "Within appraiser" = "#4477AA",
          "Between appraisers" = "#AA3377",
          "Appraiser vs standard" = "#228833",
          "All vs standard" = "#CCBB44"
        )) +
        ggplot2::labs(
          title = "Ordinal Kendall Agreement",
          subtitle = "Kendall coefficients account for ordered rating levels",
          x = "Kendall coefficient",
          y = NULL,
          color = NULL
        ) +
        theme_obj$theme_iqr() +
        ggplot2::theme(legend.position = "top")
    },

    plot_detection_ci = function(results, theme_obj = NULL) {
      theme_obj <- private$resolve_theme(theme_obj)
      results <- private$get_detection_results(results) %||% results
      colors <- private$palette(theme_obj, 3)
      status_color <- private$status_color(theme_obj)

      detection <- results$detection_ci
      specificity <- results$specificity_ci
      if (is.null(detection) || is.null(specificity)) {
        stop("Detection result does not contain confidence intervals.", call. = FALSE)
      }

      df <- data.frame(
        metric = factor(c("Sensitivity", "Specificity"), levels = c("Sensitivity", "Specificity")),
        point = c(detection$point %||% results$detection_rate, specificity$point %||% results$specificity),
        lower = c(detection$lower, specificity$lower),
        upper = c(detection$upper, specificity$upper)
      )

      ggplot2::ggplot(df, ggplot2::aes(x = .data$metric, y = .data$point)) +
        ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", linewidth = 0.6, color = colors[[3]]) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
          width = 0.18,
          linewidth = 0.8,
          color = colors[[2]]
        ) +
        ggplot2::geom_point(size = 3.6, color = status_color) +
        ggplot2::scale_y_continuous(
          limits = c(0, 1.05),
          breaks = seq(0, 1, 0.2),
          labels = scales::label_percent(accuracy = 1)
        ) +
        ggplot2::labs(
          title = "Detection Confidence Intervals",
          subtitle = "Point estimates with 95% binomial confidence intervals",
          x = NULL,
          y = "Rate"
        ) +
        theme_obj$theme_iqr()
    },

    plot_detection_radar = function(results, theme_obj = NULL) {
      self$plot_detection_metrics(results, theme_obj)
    },

    plot_detection_metrics = function(results, theme_obj = NULL) {
      theme_obj <- private$resolve_theme(theme_obj)
      results <- private$get_detection_results(results) %||% results
      youden <- results$youden_index %||% NA_real_
      df <- data.frame(
        reference_group = factor(
          c("Positive reference", "Positive reference", "Negative reference", "Negative reference"),
          levels = c("Negative reference", "Positive reference")
        ),
        outcome = factor(
          c("Sensitivity", "False negative", "Specificity", "False positive"),
          levels = c("Sensitivity", "Specificity", "False negative", "False positive")
        ),
        value = c(
          results$detection_rate %||% NA_real_,
          results$false_negative_rate %||% NA_real_,
          results$specificity %||% NA_real_,
          results$false_positive_rate %||% NA_real_
        )
      )
      df$label <- scales::percent(df$value, accuracy = 0.1)
      label_df <- df[!is.na(df$value) & df$value >= 0.08, , drop = FALSE]
      x_min <- if (!is.na(youden) && youden < 0) min(-0.25, youden - 0.05) else 0
      youden_label_y <- factor("Positive reference", levels = levels(df$reference_group))

      ggplot2::ggplot(df, ggplot2::aes(x = .data$value, y = .data$reference_group, fill = .data$outcome)) +
        ggplot2::geom_col(width = 0.46, alpha = 0.92, color = "white", linewidth = 0.4) +
        ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(label = .data$label),
          position = ggplot2::position_stack(vjust = 0.5),
          size = 3.1,
          color = "white",
          fontface = "bold"
        ) +
        ggplot2::geom_vline(
          xintercept = youden,
          linetype = "dashed",
          linewidth = 0.75,
          color = "#102a43",
          na.rm = TRUE
        ) +
        ggplot2::annotate(
          "label",
          x = youden,
          y = youden_label_y,
          label = ifelse(is.na(youden), "Youden index: NA", sprintf("Youden index = %.3f", youden)),
          hjust = ifelse(!is.na(youden) && youden > 0.78, 1.03, -0.03),
          vjust = -1.25,
          size = 3,
          fill = "white",
          color = "#102a43"
        ) +
        ggplot2::scale_x_continuous(
          limits = c(x_min, 1.05),
          breaks = seq(0, 1, 0.2),
          labels = scales::label_percent(accuracy = 1)
        ) +
        ggplot2::scale_fill_manual(values = c(
          "Sensitivity" = "#2f855a",
          "Specificity" = "#2f855a",
          "False negative" = "#d64545",
          "False positive" = "#e07a3f"
        )) +
        ggplot2::labs(
          title = "Detection Performance Metrics",
          subtitle = "Each stacked bar sums to 100% within its reference group; the dashed line marks Youden index",
          x = "Rate (bars) / Youden index (line)",
          y = NULL,
          fill = "Metric"
        ) +
        theme_obj$theme_iqr() +
        ggplot2::theme(
          legend.position = "top",
          panel.grid.major.y = ggplot2::element_blank()
        )
    }
  ),

  private = list(
    normalize_type = function(type) {
      aliases <- c(
        all = "summary",
        full = "summary",
        kappa_funnel = "kappa_benchmark",
        detection_radar = "detection_metrics"
      )
      if (type %in% names(aliases)) type <- aliases[[type]]
      valid <- c("summary", "list", "kappa_benchmark", "pairwise_agreement", "appraiser_standard", "kendall_ordinal", "response_agreement", "sample_disagreement", "confusion_matrix", "detection_ci", "detection_metrics")
      if (!type %in% valid) {
        stop("Unknown attribute agreement plot type: ", type, call. = FALSE)
      }
      type
    },

    resolve_theme = function(theme_obj) {
      if (is.null(theme_obj)) {
        return(IqrTheme$new())
      }
      if (is.character(theme_obj)) {
        return(IqrTheme$new(theme_obj))
      }
      if (!is.null(theme_obj$theme_iqr) && is.function(theme_obj$theme_iqr)) {
        return(theme_obj)
      }
      IqrTheme$new()
    },

    palette = function(theme_obj, n = 4) {
      self$.pal_discrete(theme_obj, n)
    },

    status_color = function(theme_obj) {
      colors <- private$palette(theme_obj, 2)
      colors[[2]]
    },

    agreement_palette = function(theme_obj) {
      disc <- self$.pal_discrete(theme_obj, 6)
      list(
        bands = c(
          "Poor" = self$.pal_semantic(theme_obj, "fail"),
          "Slight" = self$.pal_semantic(theme_obj, "watch"),
          "Fair" = disc[3],
          "Moderate" = disc[4],
          "Substantial" = self$.pal_semantic(theme_obj, "pass"),
          "Almost perfect" = self$.pal_semantic(theme_obj, "good")
        )
      )
    },

    threshold_palette = function(theme_obj) {
      c(
        "Review threshold" = self$.pal_semantic(theme_obj, "fail"),
        "Release threshold" = self$.pal_semantic(theme_obj, "pass")
      )
    },

    kappa_thresholds = function() {
      data.frame(
        x = c(0.4, 0.8),
        level = c("Review threshold", "Release threshold")
      )
    },

    kappa_status_color = function(kappa, theme_obj) {
      if (is.na(kappa)) return(self$.pal_semantic(theme_obj, "neutral"))
      if (kappa < 0.4) return(self$.pal_semantic(theme_obj, "fail"))
      if (kappa < 0.8) return(self$.pal_semantic(theme_obj, "watch"))
      self$.pal_semantic(theme_obj, "pass")
    },

    get_kappa_results = function(results) {
      if (is.null(results)) return(NULL)
      if (!is.null(results$kappa_results$raw_output)) return(results$kappa_results$raw_output)
      if (is.list(results$raw_output) &&
          is.list(results$raw_output$kappa) &&
          !is.null(results$raw_output$kappa$raw_output)) {
        return(results$raw_output$kappa$raw_output)
      }
      if (is.list(results$kappa) && !is.null(results$kappa$raw_output)) {
        return(results$kappa$raw_output)
      }
      if (!is.null(results$kappa) || !is.null(results$V) || !is.null(results$rating_matrix)) return(results)
      NULL
    },

    get_detection_results = function(results) {
      if (is.null(results)) return(NULL)
      if (!is.null(results$detection_results$raw_output)) return(results$detection_results$raw_output)
      if (is.list(results$raw_output) &&
          is.list(results$raw_output$detection) &&
          !is.null(results$raw_output$detection$raw_output)) {
        return(results$raw_output$detection$raw_output)
      }
      if (is.list(results$detection) && !is.null(results$detection$raw_output)) {
        return(results$detection$raw_output)
      }
      if (!is.null(results$detection_rate) || !is.null(results$specificity)) return(results)
      NULL
    }
  )
)
