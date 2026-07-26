# =============================================================================
# File: R/plot_anova.R
# Description: ANOVA-specific plotting functions
# Dependencies: ggplot2, patchwork, gridExtra, grid
# =============================================================================

#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_hline geom_vline
#' @importFrom ggplot2 geom_ribbon geom_line geom_text geom_segment geom_boxplot
#' @importFrom ggplot2 geom_jitter geom_smooth geom_abline geom_tile annotate
#' @importFrom ggplot2 scale_color_manual scale_size scale_fill_gradient2
#' @importFrom ggplot2 coord_flip labs theme element_text element_blank margin
#' @importFrom patchwork wrap_plots plot_annotation plot_layout
#' @importFrom gridExtra tableGrob ttheme_default
#' @importFrom grid unit
#' @importFrom stats df qf residuals fitted sd
#' @importFrom utils modifyList
#' @importFrom iQualityR.core IqrTheme
#' @name iQualityR.plot-anova
#' @title ANOVA Plotting Functions
#' @description Internal helpers for ANOVA visualization (F-curve, effects,
#'   comparisons, residuals). Not exported; called by user-facing wrappers.
#' @keywords internal
NULL

# ============================================================================
# 1. create_anova_table() - Generate ANOVA table grob
# ============================================================================

#' @title Create ANOVA Table Grob
#' @description
#' Converts an ANOVA table data frame into a grid grob for embedding in plots.
#'
#' @param anova_table Data frame containing ANOVA results (typically from summary(aov))
#' @param theme IqrTheme object or theme name
#' @param digits Number of decimal places (default: 4)
#' @param font_size Table font size (default: 9)
#' @param highlight_sig If TRUE, highlights significant p-values (default: TRUE)
#' @param sig_level Significance level for highlighting (default: 0.05)
#' @return A gridExtra tableGrob object
#' @export
#'
#' @examples
#' \dontrun{
#' model <- aov(mpg ~ cyl, data = mtcars)
#' tbl <- create_anova_table(summary(model)[[1]])
#' grid::grid.draw(tbl)
#' }
create_anova_table <- function(anova_table,
                               theme = NULL,
                               digits = 4,
                               font_size = 9,
                               highlight_sig = TRUE,
                               sig_level = 0.05) {
    if (!requireNamespace("gridExtra", quietly = TRUE)) {
        stop("gridExtra package is required for creating table grobs.")
    }
    
    # Convert to data frame if needed
    if (!is.data.frame(anova_table)) {
        anova_table <- as.data.frame(anova_table)
    }
    
    # Round numeric columns
    num_cols <- sapply(anova_table, is.numeric)
    if (any(num_cols)) {
        anova_table[num_cols] <- lapply(anova_table[num_cols], function(x) round(x, digits))
    }
    
    # Add row names as a column
    if (is.null(rownames(anova_table))) {
        rownames(anova_table) <- seq_len(nrow(anova_table))
    }
    anova_table <- cbind(Source = rownames(anova_table), anova_table)
    rownames(anova_table) <- NULL
    
    # If there's a p-value column (Pr(>F) or p.value), format it
    p_col <- grep("Pr\\(>F\\)|p.value", names(anova_table), value = TRUE)
    if (length(p_col) > 0 && highlight_sig) {
        # Highlight significant p-values in red
        p_vals <- as.numeric(anova_table[[p_col]])
        p_vals_highlight <- p_vals < sig_level
        # We'll use custom theme to color cells later
    }
    
    # Get theme colors
    theme_obj <- as_iqr_theme_object(theme)

    # Create table theme
    base_theme <- gridExtra::ttheme_default(
        base_size = font_size,
        base_colour = .iqr_plotter$.pal_ui(theme_obj, "text", default = "black") %||% "black",
        base_family = .iqr_plotter$.pal_ui(theme_obj, "font_family", default = "sans") %||% "sans",
        padding = grid::unit(c(3, 5), "mm")
    )

    # Override header style
    header_theme <- modifyList(base_theme, list(
        colhead = list(
            fg_params = list(fontface = "bold",
                             col = .iqr_plotter$.pal_ui(theme_obj, "surface", default = "white") %||% "white"),
            bg_params = list(fill = .iqr_plotter$.pal_ui(theme_obj, "primary", default = "#1F77B4"))
        )
    ))

    # If highlighting, create custom cell backgrounds
    if (highlight_sig && length(p_col) > 0) {
        # Build a matrix of background colors
        bg_colors <- matrix("white", nrow = nrow(anova_table), ncol = ncol(anova_table))
        p_col_idx <- which(names(anova_table) == p_col)
        for (i in seq_len(nrow(anova_table))) {
            if (!is.na(p_vals[i]) && p_vals[i] < sig_level) {
                bg_colors[i, p_col_idx] <- .iqr_plotter$.pal_ui(theme_obj, "surface_soft", default = "#FDEBD0") %||% "#FDEBD0"
            }
        }
        # Create custom theme with cell background colors
        custom_theme <- modifyList(header_theme, list(
            core = list(
                fg_params = list(col = .iqr_plotter$.pal_ui(theme_obj, "text", default = "black") %||% "black"),
                bg_params = list(fill = bg_colors)
            )
        ))
    } else {
        custom_theme <- header_theme
    }
    
    # Create table grob
    table_grob <- gridExtra::tableGrob(
        anova_table,
        rows = NULL,
        theme = custom_theme
    )
    
    # Add column header background (already handled by header_theme)
    # Return the grob
    table_grob
}


# ============================================================================
# 2. plot_f_curve() - F-distribution rejection region plot
# ============================================================================

#' @title F-Distribution Rejection Region Plot
#' @description
#' Visualizes the F-distribution with rejection region for ANOVA global F-test.
#'
#' @param f_stat F-statistic value
#' @param df1 Numerator degrees of freedom
#' @param df2 Denominator degrees of freedom
#' @param alpha Significance level (default: 0.05)
#' @param p_value Optional p-value (displayed on plot)
#' @param title Optional title
#' @param theme IqrTheme object or theme name
#' @param ... Additional arguments passed to ggplot2::labs()
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_f_curve(f_stat = 5.6, df1 = 2, df2 = 27)
#' }
plot_f_curve <- function(f_stat,
                         df1,
                         df2,
                         alpha = 0.05,
                         p_value = NULL,
                         title = NULL,
                         theme = NULL,
                         ...) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 is required.")
    }

    theme_obj <- as_iqr_theme_object(theme)

    # Calculate critical value
    crit <- stats::qf(1 - alpha, df1, df2)
    
    # Determine x-axis range
    x_max <- max(crit * 1.5, f_stat * 1.3)
    x_seq <- seq(0, x_max, length.out = 1000)
    
    # Density
    y_seq <- stats::df(x_seq, df1, df2)
    
    # Build data frames
    df_curve <- data.frame(x = x_seq, y = y_seq)
    df_reject <- data.frame(x = x_seq[x_seq >= crit], y = y_seq[x_seq >= crit])
    
    # Compute p-value if not provided
    if (is.null(p_value)) {
        p_value <- stats::pf(f_stat, df1, df2, lower.tail = FALSE)
    }
    
    # Build plot
    p <- base_plot(df_curve, ggplot2::aes(x = x, y = y), theme = theme) +
        ggplot2::geom_line(color = .iqr_plotter$.pal_discrete(theme_obj)[1], linewidth = 1.2) +
        ggplot2::geom_ribbon(
            data = df_reject,
            ggplot2::aes(x = x, ymin = 0, ymax = y),
            fill = .iqr_plotter$.pal_semantic(theme_obj, "fail"), alpha = 0.25
        ) +
        # Critical value line
        ggplot2::geom_vline(xintercept = crit,
                            color = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666"),
                            linetype = "dashed") +
        # F-statistic line
        ggplot2::geom_vline(xintercept = f_stat,
                            color = .iqr_plotter$.pal_ui(theme_obj, "primary"), linewidth = 1.2) +
        # Labels
        ggplot2::annotate("text",
                          x = f_stat,
                          y = stats::df(f_stat, df1, df2) * 1.1,
                          label = sprintf("F = %.2f", f_stat),
                          color = .iqr_plotter$.pal_ui(theme_obj, "primary"),
                          hjust = 0.5, fontface = "bold"
        ) +
        ggplot2::annotate("text",
                          x = crit,
                          y = stats::df(crit, df1, df2) * 1.2,
                          label = sprintf("Critical = %.2f", crit),
                          color = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666"),
                          hjust = 0.5
        ) +
        ggplot2::annotate("text",
                          x = x_max * 0.7,
                          y = max(y_seq) * 0.8,
                          label = sprintf("p = %s", iQualityR.core::format_p_value(p_value, context = "plot")),
                          size = 4,
                          color = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666")
        ) +
        ggplot2::labs(
            x = "F-value",
            y = "Density",
            title = title %||% "F-Distribution: ANOVA Global Test",
            subtitle = sprintf("df1 = %.0f, df2 = %.0f, alpha = %.2f", df1, df2, alpha),
            ...
        )
    
    p
}


# ============================================================================
# 3. plot_anova_effects() - Main effects plot (mean +/- SE)
# ============================================================================

#' @title Main Effects Plot for ANOVA
#' @description
#' Creates a plot of factor level means with standard error bars.
#' Optionally adds significance letters or a summary table.
#'
#' @param data Data frame containing the response and factor
#' @param response_col Name of the response variable
#' @param factor_col Name of the factor variable
#' @param means Data frame with columns: group, mean, se, n (optional)
#' @param show_letters If TRUE, adds compact letter display for significant differences
#' @param letters Data frame with columns: group, letter (optional)
#' @param show_table If TRUE, adds a summary table below the plot (requires patchwork)
#' @param point_color Color for points
#' @param error_color Color for error bars
#' @param theme IqrTheme object or theme name
#' @param title Optional title
#' @param ... Additional arguments passed to ggplot2::labs()
#' @return ggplot2 or patchwork object
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   group = rep(c("A","B","C"), each = 10),
#'   response = c(rnorm(10, 5), rnorm(10, 7), rnorm(10, 6))
#' )
#' plot_anova_effects(data, "response", "group")
#' }
plot_anova_effects <- function(data = NULL,
                               response_col = NULL,
                               factor_col = NULL,
                               means = NULL,
                               show_letters = FALSE,
                               letters = NULL,
                               show_table = FALSE,
                               point_color = NULL,
                               error_color = NULL,
                               theme = NULL,
                               title = NULL,
                               ...) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 is required.")
    }
    
    theme_obj <- as_iqr_theme_object(theme)

    # Prepare means data
    if (is.null(means)) {
        if (is.null(data) || is.null(response_col) || is.null(factor_col)) {
            stop("Either provide 'means' or 'data' with 'response_col' and 'factor_col'.")
        }
        y <- data[[response_col]]
        x <- data[[factor_col]]
        means_df <- data.frame(
            group = names(tapply(y, x, mean)),
            mean = as.numeric(tapply(y, x, mean)),
            se = as.numeric(tapply(y, x, function(z) stats::sd(z)/sqrt(length(z)))),
            n = as.numeric(tapply(y, x, length))
        )
    } else {
        means_df <- means
    }
    
    # Ensure group is a factor with proper levels
    means_df$group <- factor(means_df$group, levels = means_df$group)
    
    # Set colors
    if (is.null(point_color)) point_color <- .iqr_plotter$.pal_discrete(theme_obj)[1]
    if (is.null(error_color)) error_color <- .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666") %||% "gray50"
    
    # Base plot
    p <- base_plot(means_df, ggplot2::aes(x = group, y = mean), theme = theme) +
        ggplot2::geom_point(
            size = 3,
            color = point_color,
            shape = 21,
            fill = "white",
            stroke = 1.5
        ) +
        ggplot2::geom_errorbar(
            ggplot2::aes(ymin = mean - se, ymax = mean + se),
            width = 0.15,
            color = error_color,
            linewidth = 1
        ) +
        ggplot2::labs(
            x = factor_col %||% "Factor",
            y = response_col %||% "Mean Response",
            title = title %||% "Main Effects Plot",
            ...
        )
    
    # Add compact letter display if provided
    if (show_letters && !is.null(letters)) {
        # letters should have columns: group, letter
        p <- p + ggplot2::geom_text(
            data = letters,
            ggplot2::aes(x = group, y = max(mean) + 0.1 * diff(range(mean)), label = letter),
            size = 4,
            fontface = "bold",
            color = .iqr_plotter$.pal_semantic(theme_obj, "fail") %||% "red"
        )
    }
    
    # Optionally add summary table via patchwork
    if (show_table) {
        if (!requireNamespace("patchwork", quietly = TRUE)) {
            warning("patchwork required for show_table=TRUE. Returning plot only.")
            return(p)
        }
        # Create table grob
        table_df <- means_df
        if (!is.null(letters)) {
            table_df <- merge(table_df, letters, by = "group", all.x = TRUE)
        }
        table_df <- table_df[, c("group", "n", "mean", "se", if ("letter" %in% names(table_df)) "letter")]
        names(table_df) <- c("Group", "N", "Mean", "SE", "Sig.")
        table_grob <- gridExtra::tableGrob(
            table_df,
            rows = NULL,
            theme = gridExtra::ttheme_default(
                base_size = 9,
                padding = grid::unit(c(3, 5), "mm")
            )
        )
        # Wrap table as a ggplot
        p_table <- ggplot2::ggplot() +
            ggplot2::annotation_custom(table_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
            ggplot2::theme_void()
        # Combine
        p <- p + patchwork::plot_layout(ncol = 1, heights = c(2, 1))
        p <- p / p_table
    }
    
    p
}


# ============================================================================
# 4. plot_anova_comparison() - Multiple comparisons forest plot
# ============================================================================

#' @title Forest Plot for Multiple Comparisons
#' @description
#' Creates a forest plot-style visualization of pairwise comparisons with confidence intervals.
#'
#' @param comparison_data Data frame containing comparison results.
#'        Must have columns: contrast, estimate, lower, upper, (optionally) p_value.
#' @param estimate_col Name of estimate column (default: "estimate")
#' @param lower_col Name of lower CI column (default: "lower")
#' @param upper_col Name of upper CI column (default: "upper")
#' @param p_col Name of p-value column (optional)
#' @param alpha Significance level for coloring significant comparisons (default: 0.05)
#' @param sort_by_estimate If TRUE, sorts by estimate value (default: TRUE)
#' @param reference_line x-axis reference line (default: 0)
#' @param point_color Color for point estimates
#' @param error_color Color for confidence intervals
#' @param highlight_sig If TRUE, colors significant intervals differently
#' @param theme IqrTheme object or theme name
#' @param title Optional title
#' @param ... Additional arguments passed to ggplot2::labs()
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' comp <- data.frame(
#'   contrast = c("B-A", "C-A", "C-B"),
#'   estimate = c(1.2, 0.5, -0.7),
#'   lower = c(0.3, -0.2, -1.5),
#'   upper = c(2.1, 1.2, 0.1),
#'   p_value = c(0.01, 0.15, 0.08)
#' )
#' plot_anova_comparison(comp)
#' }
plot_anova_comparison <- function(comparison_data,
                                  estimate_col = "estimate",
                                  lower_col = "lower",
                                  upper_col = "upper",
                                  p_col = NULL,
                                  alpha = 0.05,
                                  sort_by_estimate = TRUE,
                                  reference_line = 0,
                                  point_color = NULL,
                                  error_color = NULL,
                                  highlight_sig = TRUE,
                                  theme = NULL,
                                  title = NULL,
                                  ...) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 is required.")
    }
    
    theme_obj <- as_iqr_theme_object(theme)

    # Prepare data
    df <- comparison_data
    # Ensure required columns exist
    required <- c(estimate_col, lower_col, upper_col)
    missing <- required[!required %in% names(df)]
    if (length(missing) > 0) {
        stop("Missing required columns: ", paste(missing, collapse = ", "))
    }
    
    # Rename for consistency
    names(df)[names(df) == estimate_col] <- "estimate"
    names(df)[names(df) == lower_col] <- "lower"
    names(df)[names(df) == upper_col] <- "upper"
    
    # Optional p-value column
    if (!is.null(p_col) && p_col %in% names(df)) {
        names(df)[names(df) == p_col] <- "p_value"
    }
    
    # Determine significance
    if (highlight_sig && "p_value" %in% names(df)) {
        df$significant <- df$p_value < alpha
    } else {
        # Use CI crossing reference line as significance proxy
        df$significant <- !(df$lower < reference_line & df$upper > reference_line)
    }
    
    # Sort
    if (sort_by_estimate) {
        df <- df[order(df$estimate), ]
    } else {
        df <- df[order(rownames(df)), ]
    }
    df$contrast <- factor(df$contrast, levels = df$contrast)
    
    # Set colors
    if (is.null(point_color)) point_color <- .iqr_plotter$.pal_discrete(theme_obj)[1]
    if (is.null(error_color)) {
        error_color <- if (highlight_sig) {
            c("TRUE" = .iqr_plotter$.pal_semantic(theme_obj, "fail") %||% "red",
              "FALSE" = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666") %||% "gray50")
        } else {
            .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666") %||% "gray50"
        }
    }

    # Build plot
    p <- base_plot(df, ggplot2::aes(x = contrast, y = estimate), theme = theme) +
        ggplot2::geom_hline(yintercept = reference_line, linetype = "dashed",
                            color = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666")) +
        ggplot2::geom_errorbar(
            ggplot2::aes(ymin = lower, ymax = upper, color = significant),
            width = 0.2,
            linewidth = 1
        ) +
        ggplot2::geom_point(
            ggplot2::aes(color = significant),
            size = 3,
            shape = 18
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(
            x = "Comparison",
            y = "Estimate (Difference)",
            title = title %||% "Multiple Comparisons Forest Plot",
            ...
        )
    
    # Color scale
    if (highlight_sig && is.vector(error_color) && length(error_color) == 2) {
        p <- p + ggplot2::scale_color_manual(
            values = error_color,
            name = "Significant",
            labels = c("TRUE" = "p < alpha", "FALSE" = "p >= alpha")
        )
    } else if (highlight_sig && is.character(error_color) && length(error_color) == 1) {
        # Single color for all
        p <- p + ggplot2::scale_color_identity()
    }
    
    p
}


# ============================================================================
# 5. plot_anova_residuals() - Residual diagnostic four-in-one plot
# ============================================================================

#' @title Residual Diagnostic Plots for ANOVA
#' @description
#' Creates a comprehensive set of residual diagnostic plots:
#' - Residuals vs Fitted
#' - Normal Q-Q plot
#' - Scale-Location (sqrt|standardized residuals| vs Fitted)
#' - Residuals vs Factor (or Observation Order)
#'
#' @param model An lm/aov model object
#' @param factor_col Optional factor column name for residuals vs factor plot
#' @param add_qq If TRUE, includes Q-Q plot (default: TRUE)
#' @param add_scale_location If TRUE, includes Scale-Location plot (default: TRUE)
#' @param theme IqrTheme object or theme name
#' @param title Optional title
#' @param ... Additional arguments passed to ggplot2 functions
#' @return patchwork object
#' @export
#'
#' @examples
#' \dontrun{
#' model <- aov(mpg ~ cyl, data = mtcars)
#' plot_anova_residuals(model)
#' }
plot_anova_residuals <- function(model,
                                 factor_col = NULL,
                                 add_qq = TRUE,
                                 add_scale_location = TRUE,
                                 theme = NULL,
                                 title = NULL,
                                 ...) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 is required.")
    }
    if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("patchwork is required for residual plots.")
    }
    
    theme_obj <- as_iqr_theme_object(theme)

    # Extract residuals and fitted values
    res <- stats::residuals(model)
    fitted <- stats::fitted(model)
    std_res <- rstandard(model)
    n <- length(res)

    # 1. Residuals vs Fitted
    df_rf <- data.frame(fitted = fitted, residual = res)
    p_rf <- base_plot(df_rf, ggplot2::aes(x = fitted, y = residual), theme = theme) +
        ggplot2::geom_point(alpha = 0.6, color = .iqr_plotter$.pal_discrete(theme_obj)[1]) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
        ggplot2::geom_smooth(method = "loess", se = TRUE,
                             color = .iqr_plotter$.pal_ui(theme_obj, "primary"), alpha = 0.2) +
        ggplot2::labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted") +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"))

    # 2. Normal Q-Q plot
    if (add_qq) {
        # Use existing plot_qq if available, else custom
        if (exists("plot_qq", where = asNamespace("iQualityR.plot"))) {
            p_qq <- iQualityR.plot::plot_qq(
                data = data.frame(res = res),
                sample_col = "res",
                dist_family = "norm",
                theme = theme,
                add_test = TRUE
            ) + ggplot2::labs(title = "Normal Q-Q Plot") +
                ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"))
        } else {
            p_qq <- ggplot2::ggplot(data.frame(res = res), ggplot2::aes(sample = res)) +
                ggplot2::stat_qq(color = .iqr_plotter$.pal_discrete(theme_obj)[1], alpha = 0.6) +
                ggplot2::stat_qq_line(color = .iqr_plotter$.pal_semantic(theme_obj, "fail"),
                                      linetype = "dashed") +
                as_iqr_theme(theme) +
                ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plot") +
                ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"))
        }
    }

    # 3. Scale-Location
    if (add_scale_location) {
        sqrt_abs_res <- sqrt(abs(std_res))
        df_sl <- data.frame(fitted = fitted, sqrt_abs_res = sqrt_abs_res)
        p_sl <- base_plot(df_sl, ggplot2::aes(x = fitted, y = sqrt_abs_res), theme = theme) +
            ggplot2::geom_point(alpha = 0.6, color = .iqr_plotter$.pal_discrete(theme_obj)[1]) +
            ggplot2::geom_smooth(method = "loess", se = TRUE,
                                 color = .iqr_plotter$.pal_ui(theme_obj, "primary"), alpha = 0.2) +
            ggplot2::labs(x = "Fitted Values", y = "sqrt|Standardized Residuals|",
                          title = "Scale-Location") +
            ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"))
    }

    # 4. Residuals vs Factor or Observation Order
    if (!is.null(factor_col) && factor_col %in% names(model$model)) {
        # Residuals vs factor levels
        df_factor <- data.frame(
            factor = model$model[[factor_col]],
            residual = res
        )
        p_factor <- base_plot(df_factor, ggplot2::aes(x = factor, y = residual), theme = theme) +
            ggplot2::geom_boxplot(fill = .iqr_plotter$.pal_discrete(theme_obj)[1], alpha = 0.3) +
            ggplot2::geom_jitter(width = 0.1, alpha = 0.4,
                                 color = .iqr_plotter$.pal_ui(theme_obj, "muted", default = "#666666")) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
            ggplot2::labs(x = factor_col, y = "Residuals", title = "Residuals by Factor") +
            ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"),
                           axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    } else {
        # Residuals vs observation order
        df_seq <- data.frame(order = seq_along(res), residual = res)
        p_factor <- base_plot(df_seq, ggplot2::aes(x = order, y = residual), theme = theme) +
            ggplot2::geom_point(alpha = 0.6, color = .iqr_plotter$.pal_discrete(theme_obj)[1]) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
            ggplot2::geom_smooth(method = "loess", se = TRUE,
                                 color = .iqr_plotter$.pal_ui(theme_obj, "primary"), alpha = 0.2) +
            ggplot2::labs(x = "Observation Order", y = "Residuals", title = "Residuals vs Order") +
            ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"))
    }
    
    # Combine
    if (add_qq && add_scale_location) {
        p <- (p_rf + p_qq) / (p_sl + p_factor) +
            patchwork::plot_annotation(
                title = title %||% "Residual Diagnostic Plots",
                theme = as_iqr_theme(theme)
            )
    } else if (add_qq) {
        p <- (p_rf + p_qq) / p_factor +
            patchwork::plot_annotation(
                title = title %||% "Residual Diagnostic Plots",
                theme = as_iqr_theme(theme)
            )
    } else if (add_scale_location) {
        p <- (p_rf + p_sl) / p_factor +
            patchwork::plot_annotation(
                title = title %||% "Residual Diagnostic Plots",
                theme = as_iqr_theme(theme)
            )
    } else {
        p <- p_rf / p_factor +
            patchwork::plot_annotation(
                title = title %||% "Residual Diagnostic Plots",
                theme = as_iqr_theme(theme)
            )
    }
    
    p
}


# ============================================================================
# 6. plot_anova_summary() - Combined summary plot
# ============================================================================

#' @title ANOVA Summary Dashboard
#' @description
#' Creates a comprehensive summary plot combining main effects, interaction, residuals, and ANOVA table.
#'
#' @param model An lm/aov model object
#' @param factor_cols Character vector of factor names (for effects and interaction)
#' @param show_interaction If TRUE and two factors exist, shows interaction plot
#' @param show_residuals If TRUE, includes residual diagnostic plots
#' @param show_table If TRUE, includes ANOVA table
#' @param layout Layout type: "2x2" or "1x3" (default: "2x2")
#' @param theme IqrTheme object or theme name
#' @param title Optional title
#' @param ... Additional arguments passed to sub-functions
#' @return patchwork object
#' @export
#'
#' @examples
#' \dontrun{
#' model <- aov(mpg ~ cyl * am, data = mtcars)
#' plot_anova_summary(model, factor_cols = c("cyl", "am"))
#' }
plot_anova_summary <- function(model,
                               factor_cols = NULL,
                               show_interaction = TRUE,
                               show_residuals = TRUE,
                               show_table = TRUE,
                               layout = "2x2",
                               theme = NULL,
                               title = NULL,
                               ...) {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("patchwork is required for summary plots.")
    }

    theme_obj <- as_iqr_theme_object(theme)

    # Extract factors from model if not provided
    if (is.null(factor_cols)) {
        # Get factor columns from model terms
        terms <- attr(stats::terms(model), "factors")
        if (!is.null(terms) && ncol(terms) > 0) {
            factor_cols <- colnames(terms)
        } else {
            factor_cols <- setdiff(names(model$model), all.vars(stats::formula(model))[1])
        }
    }
    
    # Remove response variable
    response <- all.vars(stats::formula(model))[1]
    factor_cols <- setdiff(factor_cols, response)
    
    # 1. Main effects plot (for first factor)
    if (length(factor_cols) >= 1) {
        p1 <- plot_anova_effects(
            data = model$model,
            response_col = response,
            factor_col = factor_cols[1],
            theme = theme,
            title = sprintf("Main Effects: %s", factor_cols[1])
        )
    } else {
        p1 <- NULL
    }
    
    # 2. Interaction plot (if two factors and requested)
    if (show_interaction && length(factor_cols) >= 2) {
        if (exists("plot_interaction_line", where = asNamespace("iQualityR.plot"))) {
            p2 <- iQualityR.plot::plot_interaction_line(
                data = model$model,
                x_var = factor_cols[1],
                y_var = response,
                group_var = factor_cols[2],
                fun = "mean",
                theme = theme
            ) + ggplot2::labs(title = sprintf("Interaction: %s x %s", factor_cols[1], factor_cols[2]))
        } else {
            # Fallback using base interaction plot
            df <- model$model
            y <- df[[response]]
            x1 <- df[[factor_cols[1]]]
            x2 <- df[[factor_cols[2]]]
            means <- tapply(y, list(x1, x2), mean)
            df_plot <- as.data.frame.table(means)
            names(df_plot) <- c(factor_cols[1], factor_cols[2], "mean")
            p2 <- base_plot(df_plot, ggplot2::aes(x = .data[[factor_cols[1]]], y = mean,
                                                  color = .data[[factor_cols[2]]],
                                                  group = .data[[factor_cols[2]]]),
                            theme = theme) +
                ggplot2::geom_line(linewidth = 1.2) +
                ggplot2::geom_point(size = 3) +
                ggplot2::labs(y = "Mean", title = sprintf("Interaction: %s x %s", factor_cols[1], factor_cols[2]))
        }
    } else {
        p2 <- NULL
    }
    
    # 3. Residuals plot
    if (show_residuals) {
        p3 <- plot_anova_residuals(model,
                                   factor_col = if (length(factor_cols) >= 1) factor_cols[1] else NULL,
                                   theme = theme,
                                   title = "Residual Diagnostics")
    } else {
        p3 <- NULL
    }
    
    # 4. ANOVA table
    if (show_table) {
        anova_tbl <- summary(model)[[1]]
        table_grob <- create_anova_table(anova_tbl, theme = theme)
        p4 <- ggplot2::ggplot() +
            ggplot2::annotation_custom(table_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
            ggplot2::theme_void()
    } else {
        p4 <- NULL
    }
    
    # Combine based on layout
    if (layout == "2x2") {
        plots <- list()
        if (!is.null(p1)) plots <- c(plots, list(p1))
        if (!is.null(p2)) plots <- c(plots, list(p2))
        if (!is.null(p3)) plots <- c(plots, list(p3))
        if (!is.null(p4)) plots <- c(plots, list(p4))
        # Fill with empty plots if needed
        while (length(plots) < 4) {
            plots <- c(plots, list(ggplot2::ggplot() + ggplot2::theme_void()))
        }
        p <- patchwork::wrap_plots(plots, ncol = 2) +
            patchwork::plot_annotation(
                title = title %||% "ANOVA Summary Dashboard",
                theme = as_iqr_theme(theme)
            )
    } else { # 1x3
        plots <- list()
        if (!is.null(p1)) plots <- c(plots, list(p1))
        if (!is.null(p2)) plots <- c(plots, list(p2))
        if (!is.null(p4)) plots <- c(plots, list(p4))
        while (length(plots) < 3) {
            plots <- c(plots, list(ggplot2::ggplot() + ggplot2::theme_void()))
        }
        p <- patchwork::wrap_plots(plots, ncol = 3) +
            patchwork::plot_annotation(
                title = title %||% "ANOVA Summary Dashboard",
                theme = as_iqr_theme(theme)
            )
    }
    
    p
}



# File: iQualityR.plot/R/plot_anova.R (fully using ggplot2)

#' Residual diagnostic plots (ggplot2 version, fully resolves par issue)
#'
#' @param x An object of class `iqr_anova` (typically returned by
#'   `iQualityR.stat::iqr_anova()` or an equivalent ANOVA wrapper).
#' @param ... Additional arguments passed to internal plotting helpers
#'   (e.g. `title`, `theme`).
#' @return A `patchwork` object combining up to four diagnostic panels.
#' @export
plot_anova_diagnostic <- function(x, ...) {
    if (!inherits(x, "iqr_anova")) {
        stop("x must be an iqr_anova object")
    }

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 package required")
    }
    if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("patchwork package required")
    }

    # Extract optional theme/title from ...
    args <- list(...)
    theme <- if (!is.null(args$theme)) args$theme else NULL
    theme_obj <- as_iqr_theme_object(theme)

    model <- x$model
    res <- residuals(model)
    fit <- fitted(model)
    df <- x$data
    response_var <- x$response_var

    # ---- Plot 1: Residuals vs Fitted ----
    df1 <- data.frame(fitted = fit, residual = res)
    p1 <- ggplot2::ggplot(df1, ggplot2::aes(x = fitted, y = residual)) +
        ggplot2::geom_point(alpha = 0.6, color = .iqr_plotter$.pal_discrete(theme_obj)[1]) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
        ggplot2::geom_smooth(method = "loess", se = TRUE,
                             color = .iqr_plotter$.pal_ui(theme_obj, "warning"), alpha = 0.2) +
        ggplot2::labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted") +
        as_iqr_theme(theme)

    # ---- Plot 2: QQ Plot ----
    df2 <- data.frame(residual = res)
    p2 <- ggplot2::ggplot(df2, ggplot2::aes(sample = residual)) +
        ggplot2::stat_qq(color = .iqr_plotter$.pal_discrete(theme_obj)[1]) +
        ggplot2::stat_qq_line(color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
        ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q") +
        as_iqr_theme(theme)

    # ---- Plot 3: Scale-Location ----
    df3 <- data.frame(fitted = fit, sqrt_res = sqrt(abs(scale(res))))
    p3 <- ggplot2::ggplot(df3, ggplot2::aes(x = fitted, y = sqrt_res)) +
        ggplot2::geom_point(alpha = 0.6, color = .iqr_plotter$.pal_discrete(theme_obj)[1]) +
        ggplot2::geom_smooth(method = "loess", se = TRUE,
                             color = .iqr_plotter$.pal_ui(theme_obj, "warning"), alpha = 0.2) +
        ggplot2::labs(x = "Fitted values", y = "sqrt|Standardized Residuals|", title = "Scale-Location") +
        as_iqr_theme(theme)

    # ---- Plot 4: Residuals vs Factor ----
    if (length(x$factors) >= 1 && !is.null(response_var)) {
        factor_name <- x$factors[1]
        df4 <- data.frame(factor = df[[factor_name]], residual = res)
        p4 <- ggplot2::ggplot(df4, ggplot2::aes(x = factor, y = residual)) +
            ggplot2::geom_boxplot(fill = .iqr_plotter$.pal_discrete(theme_obj)[1], alpha = 0.3) +
            ggplot2::geom_jitter(width = 0.2, alpha = 0.5) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
            ggplot2::labs(x = factor_name, y = "Residuals", title = "Residuals by Factor") +
            as_iqr_theme(theme)
    } else {
        # By observation order
        df4 <- data.frame(index = seq_along(res), residual = res)
        p4 <- ggplot2::ggplot(df4, ggplot2::aes(x = index, y = residual)) +
            ggplot2::geom_point(alpha = 0.6, color = .iqr_plotter$.pal_discrete(theme_obj)[1]) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                color = .iqr_plotter$.pal_semantic(theme_obj, "fail")) +
            ggplot2::labs(x = "Observation Order", y = "Residuals", title = "Residuals vs Order") +
            as_iqr_theme(theme)
    }

    # ---- Combine ----
    p <- (p1 + p2) / (p3 + p4) +
        patchwork::plot_annotation(title = "Residual Diagnostic Plots",
                                   theme = as_iqr_theme(theme))

    print(p)
    invisible(p)
}
