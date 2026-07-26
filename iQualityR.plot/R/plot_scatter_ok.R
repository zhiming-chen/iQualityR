#' @title Create Basic Scatter Plot with Regression Line
#'
#' @description
#' Creates a basic scatter plot with optional regression line and confidence interval band.
#' Automatically calculates and displays correlation coefficient and regression equation.
#'
#' @importFrom ggplot2 aes geom_point geom_smooth annotate labs
#' @importFrom ggplot2 scale_color_manual scale_size
#' @importFrom stats cor lm coef na.exclude na.omit
#' @importFrom utils modifyList
#' @importFrom iQualityR.core IqrTheme
#' @name iQualityR.plot-scatter
#' @keywords internal
NULL

#' @title Create Basic Scatter Plot with Regression Line
#'
#' @description
#' Creates a basic scatter plot with optional regression line and confidence interval band.
#' Automatically calculates and displays correlation coefficient and regression equation.
#'
#' @param data A data.frame containing the data.
#' @param x_var Name of the x-axis variable.
#' @param y_var Name of the y-axis variable.
#' @param add_regression If TRUE, adds a regression line (default: TRUE).
#' @param add_ci If TRUE, adds confidence interval band around regression line (default: TRUE).
#' @param add_correlation If TRUE, displays correlation coefficient on the plot (default: TRUE).
#' @param correlation_method Correlation method: "pearson", "spearman", or "kendall" (default: "pearson").
#' @param point_color Color for data points.
#' @param point_size Size for data points (default: 2).
#' @param point_alpha Alpha transparency for data points (default: 0.8).
#' @param line_color Color for regression line. Defaults to the theme danger color.
#' @param line_size Size for regression line (default: 1.2).
#' @param ci_fill Fill color for confidence interval band. Defaults to the theme primary color.
#' @param ci_alpha Alpha transparency for confidence interval band (default: 0.2).
#' @param theme Theme to use.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param ... Additional arguments passed to ggplot2 functions.
#'
#' @return A ggplot object.
#'
#' @examples
#' # Basic scatter plot
#' data <- data.frame(
#'   x = rnorm(50),
#'   y = rnorm(50) + rnorm(50)
#' )
#' plot_scatter_basic(data, "x", "y")
#'
#' # Without regression line
#' plot_scatter_basic(data, "x", "y", add_regression = FALSE)
#'
#' @export
plot_scatter_basic <- function(data,
                               x_var,
                               y_var,
                               add_regression = TRUE,
                               add_ci = TRUE,
                               add_correlation = TRUE,
                               correlation_method = "pearson",
                               point_color = NULL,
                               point_size = 2,
                               point_alpha = 0.8,
                               line_color = NULL,
                               line_size = 1.2,
                               ci_fill = NULL,
                               ci_alpha = 0.2,
                               theme = NULL,
                               title = NULL,
                               subtitle = NULL,
                               ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }

  iqr_theme <- as_iqr_theme_object(theme)
  ui_colors <- iqr_theme$get_ui_colors()
  data_colors <- iqr_theme$get_data_colors("discrete")

  # Create base plot
  p <- base_plot(data,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]]
    ),
    theme = theme
  ) +
    ggplot2::geom_point(
      color = point_color %||% data_colors[[1]],
      size = point_size,
      alpha = point_alpha,
      ...
    )

  # Add regression line and confidence interval
  if (add_regression) {
    if (add_ci) {
      p <- p +
        ggplot2::geom_smooth(
          method = "lm",
          color = line_color %||% ui_colors$danger,
          fill = ci_fill %||% ui_colors$primary,
          alpha = ci_alpha,
          linewidth = line_size,
          ...
        )
    } else {
      p <- p +
        ggplot2::geom_smooth(
          method = "lm",
          color = line_color %||% ui_colors$danger,
          se = FALSE,
          linewidth = line_size,
          ...
        )
    }
  }

  # Calculate and add correlation coefficient
  if (add_correlation) {
    x_vals <- data[[x_var]]
    y_vals <- data[[y_var]]

    cor_value <- stats::cor(x_vals, y_vals,
      method = correlation_method,
      use = "complete.obs"
    )

    # Build correlation label
    cor_label <- paste0(
      toupper(correlation_method), " r = ", sprintf("%.3f", cor_value)
    )

    # Add regression equation if regression is added
    if (add_regression) {
      model <- stats::lm(y_vals ~ x_vals, na.action = stats::na.exclude)
      coef_vals <- stats::coef(model)
      eq_label <- sprintf("y = %.3f + %.3f*x", coef_vals[1], coef_vals[2])
      cor_label <- paste0(cor_label, "\n", eq_label)
    }

    # Determine position for annotation (top-left)
    x_range <- range(x_vals, na.rm = TRUE)
    y_range <- range(y_vals, na.rm = TRUE)
    x_pos <- x_range[1] + 0.05 * (x_range[2] - x_range[1])
    y_pos <- y_range[2] - 0.05 * (y_range[2] - y_range[1])

    p <- p +
      ggplot2::annotate(
        "text",
        x = x_pos,
        y = y_pos,
        label = cor_label,
        hjust = 0,
        vjust = 1,
        size = 4,
        fontface = "bold",
        color = line_color
      )
  }

  # Add title and labels
  p <- p +
    ggplot2::labs(
      x = x_var,
      y = y_var,
      title = title %||% "Scatter Plot",
      subtitle = subtitle %||% if (add_correlation) paste0("Correlation: ", sprintf("%.3f", cor_value)) else NULL
    )

  return(p)
}

#' @title Create Grouped Scatter Plot
#'
#' @description
#' Creates a scatter plot with points colored and grouped by a categorical variable.
#' Supports separate regression lines for each group.
#'
#' @param data A data.frame containing the data.
#' @param x_var Name of the x-axis variable.
#' @param y_var Name of the y-axis variable.
#' @param group_var Name of the grouping variable.
#' @param add_regression If TRUE, adds regression lines for each group (default: TRUE).
#' @param add_ci If TRUE, adds confidence interval bands (default: FALSE).
#' @param add_correlation If TRUE, displays correlation coefficients for each group (default: TRUE).
#' @param correlation_method Correlation method: "pearson", "spearman", or "kendall" (default: "pearson").
#' @param point_size Size for data points (default: 2).
#' @param point_alpha Alpha transparency for data points (default: 0.8).
#' @param line_size Size for regression lines (default: 1.2).
#' @param ci_alpha Alpha transparency for confidence interval bands (default: 0.2).
#' @param palette Color palette to use (default: NULL, uses theme palette).
#' @param theme Theme to use.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param ... Additional arguments passed to ggplot2 functions.
#'
#' @return A ggplot object.
#'
#' @examples
#' # Grouped scatter plot
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   group = rep(c("A", "B"), each = 50)
#' )
#' plot_scatter_grouped(data, "x", "y", "group")
#'
#' @export
plot_scatter_grouped <- function(data,
                                 x_var,
                                 y_var,
                                 group_var,
                                 add_regression = TRUE,
                                 add_ci = FALSE,
                                 add_correlation = TRUE,
                                 correlation_method = "pearson",
                                 point_size = 2,
                                 point_alpha = 0.8,
                                 line_size = 1.2,
                                 ci_alpha = 0.2,
                                 palette = NULL,
                                 theme = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }

  # iqr_theme <- as_iqr_theme(theme)

  # Ensure group variable is a factor for discrete color scale
  if (!is.factor(data[[group_var]])) {
    data[[group_var]] <- factor(data[[group_var]])
  }

  # Create base plot with color aesthetic
  p <- base_plot(data,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      color = .data[[group_var]]
    ),
    theme = theme
  ) +
    ggplot2::geom_point(
      size = point_size,
      alpha = point_alpha,
      ...
    )

  # Add regression lines for each group
  if (add_regression) {
    if (add_ci) {
      p <- p +
        ggplot2::geom_smooth(
          method = "lm",
          alpha = ci_alpha,
          linewidth = line_size,
          ...
        )
    } else {
      p <- p +
        ggplot2::geom_smooth(
          method = "lm",
          se = FALSE,
          linewidth = line_size,
          ...
        )
    }
  }

  # Apply custom palette if provided
  if (!is.null(palette)) {
    p <- p + ggplot2::scale_color_manual(values = palette)
  } else if (inherits(theme, "IqrTheme")) {
    if (is.null(theme)) {
      scale_theme <- "prism"
    } else {
      scale_theme <- theme
    }
    iqr_theme <- IqrTheme$new(theme_style = scale_theme)
    p <- p + iqr_theme$scale_color_iqr(discrete = TRUE)
  }

  # Add correlation coefficients for each group
  if (add_correlation) {
    groups <- unique(data[[group_var]])
    cor_labels <- character(length(groups))

    for (i in seq_along(groups)) {
      group <- groups[i]
      group_data <- data[data[[group_var]] == group, ]
      x_vals <- group_data[[x_var]]
      y_vals <- group_data[[y_var]]

      cor_value <- stats::cor(x_vals, y_vals, method = correlation_method, use = "complete.obs")

      if (add_regression) {
        model <- stats::lm(y_vals ~ x_vals, na.action = stats::na.exclude)
        coef_vals <- stats::coef(model)
        eq_label <- sprintf("y = %.2f + %.2f*x", coef_vals[1], coef_vals[2])
        cor_labels[i] <- paste0(group, ": r = ", sprintf("%.3f", cor_value), "\n", eq_label)
      } else {
        cor_labels[i] <- paste0(group, ": r = ", sprintf("%.3f", cor_value))
      }
    }

    # Create annotation data frame
    x_range <- range(data[[x_var]], na.rm = TRUE)
    y_range <- range(data[[y_var]], na.rm = TRUE)
    x_pos <- x_range[1] + 0.05 * (x_range[2] - x_range[1])
    y_pos <- y_range[2] - 0.05 * (y_range[2] - y_range[1])

    annotation_data <- data.frame(
      x = x_pos,
      y = y_pos - seq(0, by = 0.08 * (y_range[2] - y_range[1]), length.out = length(groups)),
      label = cor_labels,
      group = groups
    )

    p <- p +
      ggplot2::geom_text(
        data = annotation_data,
        ggplot2::aes(x = x, y = y, label = label, color = group),
        hjust = 0,
        vjust = 1,
        size = 3.5,
        fontface = "bold",
        show.legend = FALSE
      )
  }

  # Add title and labels
  p <- p +
    ggplot2::labs(
      x = x_var,
      y = y_var,
      color = group_var,
      title = title %||% "Grouped Scatter Plot",
      subtitle = subtitle
    )

  return(p)
}

#' @title Create Bubble Scatter Plot
#'
#' @description
#' Creates a scatter plot with a third variable represented by bubble size.
#' Useful for visualizing three dimensions of data simultaneously.
#'
#' @param data A data.frame containing the data.
#' @param x_var Name of the x-axis variable.
#' @param y_var Name of the y-axis variable.
#' @param size_var Name of the variable to represent bubble size.
#' @param group_var Optional name of the grouping variable for coloring (default: NULL).
#' @param size_range Range for bubble sizes (default: c(2, 10)).
#' @param add_labels If TRUE, adds labels to bubbles (default: FALSE).
#' @param label_var Name of the variable to use for labels (default: NULL, uses row names).
#' @param label_size Size for labels (default: 3).
#' @param point_alpha Alpha transparency for bubbles (default: 0.7).
#' @param theme Theme to use.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param ... Additional arguments passed to ggplot2 functions.
#'
#' @return A ggplot object.
#'
#' @examples
#' # Bubble scatter plot
#' data <- data.frame(
#'   x = rnorm(20),
#'   y = rnorm(20),
#'   size = runif(20, 1, 10),
#'   group = rep(c("A", "B"), each = 10)
#' )
#' plot_scatter_bubble(data, "x", "y", "size")
#'
#' @export
plot_scatter_bubble <- function(data,
                                x_var,
                                y_var,
                                size_var,
                                group_var = NULL,
                                size_range = c(2, 10),
                                add_labels = FALSE,
                                label_var = NULL,
                                label_size = 3,
                                point_alpha = 0.7,
                                theme = NULL,
                                title = NULL,
                                subtitle = NULL,
                                ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }

  iqr_theme <- as_iqr_theme_object(theme)
  data_colors <- iqr_theme$get_data_colors("discrete")
  ui_colors <- iqr_theme$get_ui_colors()

  # Ensure group variable is a factor for discrete color scale
  if (!is.null(group_var) && !is.factor(data[[group_var]])) {
    data[[group_var]] <- factor(data[[group_var]])
  }

  # Create base plot
  if (!is.null(group_var)) {
    p <- base_plot(data,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        size = .data[[size_var]],
        color = .data[[group_var]]
      ),
      theme = theme
    ) +
      ggplot2::geom_point(alpha = point_alpha, ...)

    if (inherits(theme, "IqrTheme")) {
      if (is.null(theme)) {
        scale_theme <- "prism"
      } else {
        scale_theme <- theme
      }
      iqr_theme <- IqrTheme$new(theme_style = scale_theme)
      p <- p + iqr_theme$scale_color_iqr(discrete = TRUE)
    }
  } else {
    p <- base_plot(data,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        size = .data[[size_var]]
      ),
      theme = theme
    ) +
      ggplot2::geom_point(
        color = data_colors[[1]],
        alpha = point_alpha,
        ...
      )
  }

  # Add size scale
  p <- p +
    ggplot2::scale_size(
      range = size_range,
      name = size_var
    )

  # Add labels if requested
  if (add_labels) {
    if (!is.null(label_var)) {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(label = .data[[label_var]]),
          size = label_size,
          vjust = -1,
          show.legend = FALSE
        )
    } else {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(label = rownames(data)),
          size = label_size,
          vjust = -1,
          show.legend = FALSE
        )
    }
  }

  # Add title and labels
  p <- p +
    ggplot2::labs(
      x = x_var,
      y = y_var,
      title = title %||% "Bubble Scatter Plot",
      subtitle = subtitle
    )

  return(p)
}

#' @title Create Scatter Plot with Density Handling
#'
#' @description
#' Creates a scatter plot with various methods to handle overlapping points (overplotting).
#' Supports alpha blending, jittering, and 2D binning for high-density data.
#'
#' @param data A data.frame containing the data.
#' @param x_var Name of the x-axis variable.
#' @param y_var Name of the y-axis variable.
#' @param method Method to handle overlapping: "alpha" (transparency), "jitter" (offset),
#'   "bins" (2D binning), "hex" (hexagonal binning), or "density" (density coloring) (default: "alpha").
#' @param point_alpha Alpha transparency for points (default: 0.5, used for "alpha" method).
#' @param point_size Size for points (default: 2).
#' @param jitter_width Width of jitter offset (default: 0.1, used for "jitter" method).
#' @param jitter_height Height of jitter offset (default: 0.1, used for "jitter" method).
#' @param bins Number of bins for 2D binning (default: 30, used for "bins" method).
#' @param color_low Color for low density. Defaults to the theme soft surface color.
#' @param color_high Color for high density. Defaults to the theme primary color.
#' @param add_smooth If TRUE, adds a smooth line (default: FALSE).
#' @param smooth_method Smoothing method: "loess", "gam", or "lm" (default: "loess").
#' @param theme Theme to use.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param ... Additional arguments passed to ggplot2 functions.
#'
#' @return A ggplot object.
#'
#' @examples
#' # High-density scatter plot with alpha blending
#' data <- data.frame(
#'   x = rnorm(1000),
#'   y = rnorm(1000) + 0.5 * rnorm(1000)
#' )
#' plot_scatter_density(data, "x", "y", method = "alpha")
#'
#' # With jittering
#' plot_scatter_density(data, "x", "y", method = "jitter")
#'
#' # With 2D binning
#' plot_scatter_density(data, "x", "y", method = "bins")
#'
#' @export
plot_scatter_density <- function(data,
                                 x_var,
                                 y_var,
                                 method = c("alpha", "jitter", "bins", "hex", "density"),
                                 point_alpha = 0.5,
                                 point_size = 2,
                                 jitter_width = 0.1,
                                 jitter_height = 0.1,
                                 bins = 30,
                                 color_low = NULL,
                                 color_high = NULL,
                                 add_smooth = FALSE,
                                 smooth_method = "loess",
                                 theme = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }

  method <- match.arg(method)
  iqr_theme <- as_iqr_theme_object(theme)
  data_colors <- iqr_theme$get_data_colors("discrete")
  ui_colors <- iqr_theme$get_ui_colors()
  color_low <- color_low %||% ui_colors$surface_soft
  color_high <- color_high %||% ui_colors$primary

  # Create plot based on method
  if (method == "alpha") {
    # Alpha blending method
    p <- base_plot(data, ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]]
    ),
    theme = theme
    ) +
      ggplot2::geom_point(
        alpha = point_alpha,
        size = point_size,
        color = data_colors[[1]],
        ...
      )
  } else if (method == "jitter") {
    # Jitter method
    p <- base_plot(data,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]]
      ),
      theme = theme
    ) +
      ggplot2::geom_jitter(
        width = jitter_width,
        height = jitter_height,
        alpha = point_alpha,
        size = point_size,
        color = data_colors[[1]],
        ...
      )
  } else if (method == "bins") {
    # 2D binning method
    p <- base_plot(data,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]]
      ),
      theme = theme
    ) +
      ggplot2::geom_bin2d(
        bins = bins,
        ...
      ) +
      ggplot2::scale_fill_gradient2(
        low = color_low,
        high = color_high,
        mid = ui_colors$surface,
        midpoint = 0,
        name = "Count"
      )
  } else if (method == "hex") {
    # Hexagonal binning method
    if (!requireNamespace("hexbin", quietly = TRUE)) {
      stop("hexbin package is required for hexagonal binning. Install with: install.packages('hexbin')")
    }

    p <- base_plot(data,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]]
      ),
      theme = theme
    ) +
      ggplot2::geom_hex(
        bins = bins,
        ...
      ) +
      ggplot2::scale_fill_gradient2(
        low = color_low,
        high = color_high,
        mid = ui_colors$surface,
        midpoint = 0,
        name = "Count"
      )
  } else if (method == "density") {
    # Density coloring method
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("MASS package is required for density coloring.")
    }

    # Calculate 2D density
    kde <- MASS::kde2d(data[[x_var]],
      data[[y_var]],
      n = 100
    )

    # Create density data frame
    density_df <- expand.grid(x = kde$x, y = kde$y)
    density_df$z <- as.vector(kde$z)

    # Create base plot
    p <- base_plot(data,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]]
      ),
      theme = theme
    ) +
      ggplot2::stat_density_2d(
        ggplot2::aes(fill = ggplot2::after_stat(level)),
        geom = "polygon",
        bins = bins,
        ...
      ) +
      ggplot2::scale_fill_gradient2(
        low = color_low,
        high = color_high,
        mid = ui_colors$surface,
        midpoint = 0,
        name = "Density"
      ) +
      ggplot2::geom_point(
        alpha = 0.3,
        size = point_size / 2,
        color = ui_colors$text
      )
  }

  # Add smooth line if requested
  if (add_smooth) {
    p <- p +
      ggplot2::geom_smooth(
        method = smooth_method,
        color = "red",
        linewidth = 1.2,
        se = TRUE,
        alpha = 0.2,
        fill = "red"
      )
  }

  # Add title and labels
  p <- p +
    ggplot2::labs(
      x = x_var,
      y = y_var,
      title = title %||% paste0("Scatter Plot (", method, " method)"),
      subtitle = subtitle
    )

  return(p)
}
