#' @title Enhanced Pareto Chart
#'
#' @description
#' Creates an enhanced Pareto chart with additional features like grouping,
#' weighted analysis, and interactive table display. Also supports facetting
#' and "Other" category aggregation.
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point geom_text
#' @importFrom ggplot2 geom_tile scale_y_continuous sec_axis labs theme
#' @importFrom ggplot2 element_text element_blank margin facet_wrap
#' @importFrom dplyr group_by summarise mutate arrange desc ungroup group_modify bind_rows `%>%`
#' @importFrom scales percent
#' @importFrom patchwork plot_layout
#' @importFrom grid unit
#' @importFrom utils modifyList
#' @importFrom stats na.omit
#' @importFrom iQualityR.core IqrTheme
#' @name iQualityR.plot-pareto
#' @keywords internal
NULL

#' @title Enhanced Pareto Chart
#'
#' @description
#' Creates an enhanced Pareto chart with additional features like grouping,
#' weighted analysis, and interactive table display. Also supports facetting
#' and "Other" category aggregation.
#'
#' @param data A data.frame, list, or named vector containing the data.
#' @param category_col Name of the category column (if data is a data.frame).
#' @param count_col Name of the count/value column (if data is a data.frame).
#' @param group_col Name of the grouping column for facetting (optional).
#' @param weight_col Name of the weight column for weighted analysis (optional).
#' @param threshold Threshold for grouping small categories into "Other" (default: 0.05).
#' @param show_table Whether to show a summary table below the chart (default: FALSE).
#' @param theme Theme to use (default: "academic").
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param ... Additional arguments passed to ggplot2 functions.
#'
#' @return A ggplot object or a patchwork object (if show_table is TRUE).
#'
#' @examples
#' # From data.frame
#' data <- data.frame(
#'   defect = c("Scratch", "Dent", "Missing Part", "Color Issue", "Size Error", "Weight Issue"),
#'   count = c(45, 23, 15, 8, 5, 3)
#' )
#' plot_pareto_enhanced(data, "defect", "count")
#'
#' \donttest{
#' # From named vector
#' counts <- c(Scratch = 45, Dent = 23, Missing = 15, Other = 7)
#' plot_pareto_enhanced(counts, show_table = TRUE)
#'
#' # With grouping
#' grouped_data <- data.frame(
#'   defect = rep(c("Scratch", "Dent", "Missing Part"), 2),
#'   count = c(45, 23, 15, 30, 18, 10),
#'   period = rep(c("Before", "After"), each = 3)
#' )
#' plot_pareto_enhanced(grouped_data, "defect", "count", group_col = "period", show_table = TRUE)
#' }
#'
#' @export
plot_pareto_enhanced <- function(data,
                               category_col = NULL,
                               count_col = NULL,
                               group_col = NULL,
                               weight_col = NULL,
                               threshold = 0.05,
                               show_table = FALSE,
                               theme = "academic",
                               title = NULL,
                               subtitle = NULL,
                               ...) {
  # Check for required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr is required.")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("scales is required.")
  }
  if (show_table && !requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork is required for show_table = TRUE.")
  }

  # Process input data
  processed_data <- process_pareto_input(data, category_col, count_col, group_col, weight_col)

  # Create IqrTheme object via shared factory (used by .iqr_plotter toolbox)
  theme_obj <- as_iqr_theme_object(theme)
  # Prepare data for plotting
  plot_data <- prepare_pareto_data(processed_data, category_col, count_col,
                                  group_col, weight_col, threshold)

  # Create top plot (Pareto chart)
  plot_top <- create_pareto_plot(
    data = plot_data,
    category_col = category_col,
    count_col = count_col,
    group_col = group_col,
    weight_col = weight_col,
    iqr_theme = theme_obj,
    title = title,
    subtitle = subtitle,
    ...
  )

  if (!show_table) {
    return(plot_top)
  }

  # Create bottom table
  plot_bottom <- create_pareto_table(
    data = plot_data,
    category_col = category_col,
    count_col = count_col,
    group_col = group_col,
    weight_col = weight_col,
    iqr_theme = theme_obj
  )

  # Combine plots
  combined <- plot_top / plot_bottom +
    patchwork::plot_layout(heights = c(2, 1))

  return(combined)
}

#' @title Process Pareto Input Data
#' @description
#' Internal function to process different types of input data for Pareto chart.
#' @param data Input data (data.frame, list, or named vector)
#' @param category_col Category column name
#' @param count_col Count column name
#' @param group_col Group column name
#' @param weight_col Weight column name
#' @return Processed data.frame
process_pareto_input <- function(data, category_col = NULL, count_col = NULL, group_col = NULL, weight_col = NULL) {
  if (is.atomic(data) && is.character(names(data))) {
    # Named atomic vector input (e.g., named numeric vector)
    df <- data.frame(
      category = names(data),
      count = as.numeric(data)
    )
    return(df)
  } else if (is.list(data) && !is.data.frame(data) && inherits(data, "list")) {
    # List input (not a data.frame, but a true list)
    if (all(c("category", "count") %in% names(data))) {
      # List with category and count
      # Ensure category and count are vectors
      category_vec <- unlist(data$category)
      count_vec <- unlist(data$count)

      df <- data.frame(
        category = as.character(category_vec),
        count = as.numeric(count_vec)
      )
      return(df)
    } else if (length(data) == 2 && !is.null(names(data))) {
      # List with named elements
      df <- data.frame(
        category = names(data),
        count = as.numeric(unlist(data))
      )
      return(df)
    } else {
      stop("List input must have 'category' and 'count' elements or be a named list with two elements.")
    }
  } else if (is.data.frame(data)) {
    # Data.frame input
    if (is.null(category_col) || is.null(count_col)) {
      if (ncol(data) == 2) {
        # Assume first column is category, second is count
        df <- data.frame(
          category = data[[1]],
          count = as.numeric(data[[2]])
        )
        return(df)
      } else {
        stop("category_col and count_col must be specified for data.frame input with more than 2 columns")
      }
    } else {
      # Rename columns for consistency
      df <- data.frame(
        category = data[[category_col]],
        count = as.numeric(data[[count_col]])
      )

      # Add group column if specified
      if (!is.null(group_col) && group_col %in% names(data)) {
        df$group <- data[[group_col]]
      }

      # Add weight column if specified
      if (!is.null(weight_col) && weight_col %in% names(data)) {
        df$weight <- as.numeric(data[[weight_col]])
      }

      return(df)
    }
  } else {
    stop("Unsupported input type. Use data.frame, named vector, or list with category and count.")
  }
}

#' @title Prepare Pareto Data
#' @description
#' Internal function to prepare data for Pareto chart, including sorting and "Other" aggregation.
#' @param data Processed data.frame
#' @param category_col Category column name
#' @param count_col Count column name
#' @param group_col Group column name
#' @param weight_col Weight column name
#' @param threshold Threshold for "Other" aggregation
#' @return Prepared data.frame
prepare_pareto_data <- function(data, category_col = NULL, count_col = NULL, group_col = NULL, weight_col = NULL, threshold = 0.05) {
  # Check if data already has standard column names
  if ("category" %in% colnames(data) && "count" %in% colnames(data)) {
    # Data is already in standard format, skip renaming
  } else {
    # Rename columns for consistent processing
    if (!is.null(category_col) && category_col != "category" && category_col %in% colnames(data)) {
      colnames(data)[colnames(data) == category_col] <- "category"
    }
    if (!is.null(count_col) && count_col != "count" && count_col %in% colnames(data)) {
      colnames(data)[colnames(data) == count_col] <- "count"
    }
  }

  # Process group column
  if (!is.null(group_col) && group_col != "group" && group_col %in% colnames(data)) {
    colnames(data)[colnames(data) == group_col] <- "group"
  }

  # Process weight column
  if (!is.null(weight_col) && weight_col != "weight" && weight_col %in% colnames(data)) {
    colnames(data)[colnames(data) == weight_col] <- "weight"
  }

  # Calculate weighted values if weight is provided
  if ("weight" %in% names(data)) {
    data$value <- data$count * data$weight
  } else {
    data$value <- data$count
  }

  # Process data by group if group is provided
  if ("group" %in% names(data)) {
    # Use dplyr::group_modify instead of dplyr::do
    processed_data <- data %>%
      dplyr::group_by(group) %>%
      dplyr::group_modify(~ process_group(., threshold)) %>%
      dplyr::ungroup()
  } else {
    processed_data <- process_group(data, threshold)
  }

  return(processed_data)
}

#' @title Process Group Data
#' @description
#' Internal function to process a single group for Pareto chart.
#' @param group_data Data for a single group
#' @param threshold Threshold for "Other" aggregation
#' @return Processed group data
process_group <- function(group_data, threshold) {
  # Sort by value
  sorted_data <- group_data[order(group_data$value, decreasing = TRUE), ]

  # Calculate cumulative percentage
  total_value <- sum(sorted_data$value, na.rm = TRUE)
  sorted_data$pct <- sorted_data$value / total_value * 100
  sorted_data$cum_pct <- cumsum(sorted_data$pct)

  # Handle "Other" aggregation
  if (threshold > 0) {
    cutoff_idx <- which(sorted_data$cum_pct >= (1 - threshold) * 100)[1]
    if (!is.na(cutoff_idx) && cutoff_idx > 1) {
      # Keep top categories
      top_data <- sorted_data[1:(cutoff_idx-1), ]

      # Create "Other" category
      other_data <- data.frame(
        category = "Other",
        count = sum(sorted_data$count[cutoff_idx:nrow(sorted_data)]),
        value = sum(sorted_data$value[cutoff_idx:nrow(sorted_data)]),
        pct = sum(sorted_data$pct[cutoff_idx:nrow(sorted_data)]),
        cum_pct = 100
      )

      if ("weight" %in% names(sorted_data)) {
        other_data$weight <- NA  # Weight doesn't make sense for aggregated data
      }

      sorted_data <- rbind(top_data, other_data)
    }
  }

  # Factorize category for proper ordering
  sorted_data$category <- factor(sorted_data$category, levels = sorted_data$category)

  return(sorted_data)
}

#' @title Create Pareto Plot
#' @description
#' Internal function to create the Pareto chart (top part).
#' @param data Prepared data.frame
#' @param category_col Original category column name
#' @param count_col Original count column name
#' @param group_col Original group column name
#' @param weight_col Original weight column name
#' @param iqr_theme IqrTheme object
#' @param title Chart title
#' @param subtitle Chart subtitle
#' @param ... Additional arguments
#' @return ggplot object
create_pareto_plot <- function(data,
                               category_col = NULL,
                               count_col = NULL,
                               group_col = NULL,
                               weight_col = NULL,
                               iqr_theme,
                               title = NULL,
                               subtitle = NULL, ...) {
  # Calculate max value for secondary axis
  max_value <- max(data$value, na.rm = TRUE)

  # Resolve theme-derived colors once.
  # - col fill uses the primary discrete palette color
  # - cumulative line/points/text use the 3rd discrete color for contrast
  c <- .iqr_aes(iqr_theme)
  cum_color <- .iqr_plotter$.pal_discrete(c$theme_obj)[3]

  # Create base plot. We do NOT map fill/color to a column (the bar fill is a
  # single fixed color), so base_plot() will not auto-inject any scale; we
  # simply use the theme via as_iqr_theme() and apply colors manually.
  p <- ggplot2::ggplot(data, ggplot2::aes(x = category, y = value)) +
    ggplot2::geom_col(fill = c$data, ...) +
    as_iqr_theme(iqr_theme)

  # Add cumulative line and points
  p <- p +
    ggplot2::geom_line(
      ggplot2::aes(y = cum_pct / 100 * max_value, group = 1),
      linewidth = 1,
      color = cum_color
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = cum_pct / 100 * max_value),
      size = 3,
      color = cum_color
    )

  # Add data labels to points
  p <- p +
    ggplot2::geom_text(
      ggplot2::aes(y = cum_pct / 100 * max_value, label = scales::percent(cum_pct / 100)),
      vjust = -1,
      size = 3.5,
      color = cum_color
    )

  # Add secondary y-axis
  y_label <- ifelse(!is.null(weight_col),
                   paste(count_col %||% "Count", "* Weight"),
                   count_col %||% "Count")

  p <- p +
    ggplot2::scale_y_continuous(
      name = y_label,
      sec.axis = ggplot2::sec_axis(
        ~ . / max_value,
        name = "Cumulative %",
        labels = scales::percent
      )
    )

  # Add facet if group is provided
  if ("group" %in% names(data)) {
    p <- p +
      ggplot2::facet_wrap(~ group, nrow = 1)
  }

  # Add title and labels
  p <- p +
    ggplot2::labs(
      x = category_col %||% "Category",
      y = y_label,
      title = title %||% "Pareto Chart",
      subtitle = subtitle %||% "with cumulative percentage"
    ) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 0, l = 10)
    )

  return(p)
}

#' @title Create Pareto Table
#' @description
#' Internal function to create the summary table (bottom part).
#' @param data Prepared data.frame
#' @param category_col Original category column name
#' @param count_col Original count column name
#' @param group_col Original group column name
#' @param weight_col Original weight column name
#' @param iqr_theme IqrTheme object
#' @return ggplot object
create_pareto_table <- function(data, category_col = NULL, count_col = NULL, group_col = NULL, weight_col = NULL, iqr_theme) {
  # Prepare table data
  table_data <- data

  # Reshape data for table
  if ("group" %in% names(table_data)) {
    table_data <- dplyr::group_by(table_data, group, category)
  } else {
    table_data <- dplyr::group_by(table_data, category)
  }

  # Create statistics data
  stat_data <- list()

  # Count
  count_data <- dplyr::summarise(table_data, value = sum(count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(stat_name = "Count")
  stat_data[[1]] <- count_data

  # Percentage
  pct_data <- dplyr::summarise(table_data, value = unique(pct), .groups = "drop") %>%
    dplyr::mutate(stat_name = "Percentage")
  stat_data[[2]] <- pct_data

  # Cumulative percentage
  cum_pct_data <- dplyr::summarise(table_data, value = unique(cum_pct), .groups = "drop") %>%
    dplyr::mutate(stat_name = "Cumulative%")
  stat_data[[3]] <- cum_pct_data

  # Add weight if present
  if ("weight" %in% names(data)) {
    weight_data <- dplyr::summarise(table_data, value = sum(value, na.rm = TRUE) / sum(count, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(stat_name = "Avg weight")
    stat_data[[4]] <- weight_data
  }

  # Combine statistics
  stat_data <- dplyr::bind_rows(stat_data)

  # Factorize stat_name for consistent ordering
  stat_order <- c("Count", "Percentage", "Cumulative%")
  if ("weight" %in% names(data)) {
    stat_order <- c(stat_order, "Avg weight")
  }
  stat_data$stat_name <- factor(stat_data$stat_name, levels = stat_order)
  stat_data$stat_order <- as.integer(stat_data$stat_name)

  # Create table plot
  # Resolve theme-derived colors for tile border, text, and zebra striping.
  # The "surface" slot gives the tile border / stripe base color (matches the
  # plot background), "surface_soft" gives the alternate stripe, and "text"
  # gives the cell text color — so the table follows the active theme.
  c <- .iqr_aes(iqr_theme)
  p <- ggplot2::ggplot(stat_data, ggplot2::aes(x = category, y = stat_name)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = factor(stat_order %% 2)),
      color = c$surface,
      linewidth = 0.5,
      height = 0.9,
      width = 0.9
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = format_stat_value(value, stat_name)),
      size = 3.5,
      color = c$text
    )

  # Add facet if group is provided
  if ("group" %in% names(stat_data)) {
    p <- p +
      ggplot2::facet_wrap(~ group, nrow = 1)
  }

  # Apply theme
  p <- p +
    ggplot2::scale_fill_manual(
      values = c(c$surface, c$surface_soft),
      guide = "none"
    ) +
    iqr_theme$theme_iqr() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),  # Hide facet titles (match top plot)
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 0, r = 10, b = 10, l = 10)
    )

  return(p)
}

#' @title Format Statistic Values
#' @description
#' Internal function to format statistic values for table display.
#' @param x Value to format
#' @param stat_name Statistic name
#' @return Formatted string

format_stat_value <- function(x, stat_name) {
  # Ensure stat_name is same length as x
  if (length(stat_name) == 1) {
    stat_name <- rep(stat_name, length(x))
  }
  # Convert factor to character for switch()
  stat_name <- as.character(stat_name)
  # Initialize result character vector
  result <- character(length(x))
  # Process each element
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- ""
    } else {
      result[i] <- switch(stat_name[i],
                          "Count"       = as.character(round(x[i])),
                          "Percentage"  = sprintf("%.1f%%", x[i]),
                          "Cumulative%" = sprintf("%.1f%%", x[i]),
                          "Avg weight"  = sprintf("%.2f", x[i]),
                          sprintf("%.2f", x[i])
      )
    }
  }
  result
}

#' @title Quick Pareto Chart
#'
#' @description
#' Creates a quick Pareto chart from different input types.
#'
#' @param data A data.frame, list, or named vector.
#' @param category_col Name of the category column (if data is a data.frame).
#' @param count_col Name of the count column (if data is a data.frame).
#' @param ... Additional arguments passed to plot_pareto_enhanced.
#'
#' @return A ggplot object or patchwork object.
#'
#' @examples
#' # From named vector
#' counts <- c(A = 10, B = 8, C = 5, D = 3)
#' quick_pareto(counts)
#'
#' # From data.frame
#' df <- data.frame(category = c("A", "B", "C"), count = c(10, 8, 5))
#' quick_pareto(df, "category", "count")
#'
#' @export
quick_pareto <- function(data, category_col = NULL, count_col = NULL, ...) {
  plot_pareto_enhanced(data, category_col, count_col, ...)
}

#' @title Pareto Chart with Image Labels
#'
#' @description
#' Creates a Pareto chart with images as X-axis labels.
#' Useful for quality management to visualize defect types with images.
#'
#' @param data A data.frame containing the data.
#' @param category_col Name of the category column.
#' @param count_col Name of the count/value column.
#' @param image_paths Named character vector mapping categories to image file paths.
#' @param image_size Size of images in points (default: 20).
#' @param theme Theme to use.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param ... Additional arguments passed to ggplot2::geom_col.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' # Example with image paths
#' data <- data.frame(
#'   defect = c("Scratch", "Dent", "Missing Part"),
#'   count = c(45, 23, 15)
#' )
#' image_paths <- c(
#'   "Scratch" = "path/to/scratch.png",
#'   "Dent" = "path/to/dent.png",
#'   "Missing Part" = "path/to/missing.png"
#' )
#' plot_pareto_image_labels(data, "defect", "count", image_paths)
#' }
#'
#' @export
plot_pareto_image_labels <- function(data,
                                     category_col,
                                     count_col,
                                     image_paths,
                                     image_size = 20,
                                     theme = NULL,
                                     title = NULL,
                                     subtitle = NULL,
                                     ...) {
  # Check for required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("grid package is required.")
  }
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("png package is required for image labels. Install with: install.packages('png')")
  }
  theme_obj <- as_iqr_theme_object(theme)
  iqr_theme <- theme_obj$theme_iqr()
  c <- .iqr_aes(theme_obj)
  # Sort data
  df <- data[order(data[[count_col]], decreasing = TRUE), ]
  categories <- df[[category_col]]
  df[[category_col]] <- factor(df[[category_col]], levels = categories)
  # Calculate cumulative percentage
  df$.cum_pct <- cumsum(df[[count_col]]) / sum(df[[count_col]], na.rm = TRUE) * 100
  max_count <- max(df[[count_col]], na.rm = TRUE)
  df$.cum_y <- df$.cum_pct / 100 * max_count
  # Create base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[category_col]], y = .data[[count_col]])) +
    ggplot2::geom_col(...)
  # Add cumulative line
  p <- p +
    ggplot2::geom_line(
      ggplot2::aes(x = .data[[category_col]], y = .data$.cum_y, group = 1),
      linewidth = 1,
      color = c$danger
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data[[category_col]], y = .data$.cum_y),
      shape = 19,
      size = 2,
      color = c$danger
    )
  # Apply theme and styling
  p <- p +
    iqr_theme +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),  # Remove default text labels
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "none",
      # Reduce spacing
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = image_size + 10)),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 20, l = 10)
    ) +
    ggplot2::labs(
      x = category_col,
      y = count_col,
      title = title %||% "Pareto Chart",
      subtitle = subtitle %||% "with cumulative percentage"
    )
  # Add images as X-axis labels
  for (i in seq_along(categories)) {
    category <- categories[i]
    if (category %in% names(image_paths)) {
      img_path <- image_paths[category]
      if (file.exists(img_path)) {
        # Read image
        img <- png::readPNG(img_path)
        img_grob <- grid::rasterGrob(img, interpolate = TRUE, height = grid::unit(image_size, "points"))
        # Add image to plot
        p <- p +
          ggplot2::annotation_custom(
            grob = img_grob,
            xmin = i - 0.4,
            xmax = i + 0.4,
            ymin = -max_count * 0.05,
            ymax = -max_count * 0.05 + image_size / 72 * 1.5 * max_count / 100
          )
      }
    }
  }
  # Adjust y-axis limits to make room for images
  p <- p +
    ggplot2::ylim(c(-max_count * 0.1, max_count * 1.1))
  return(p)
}


#' @title Pareto Chart with Emoji Labels
#'
#' @description
#' Creates a Pareto chart with emoji as X-axis labels.
#' Useful for quality management to visualize defect types with emojis.
#'
#' @param data A data.frame containing the data.
#' @param category_col Name of the category column.
#' @param count_col Name of the count/value column.
#' @param emojis Named character vector mapping categories to emojis.
#' @param emoji_size Size of emojis (default: 5).
#' @param theme Theme to use.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param ... Additional arguments passed to ggplot2::geom_col.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   defect = c("Scratch", "Dent", "Missing Part"),
#'   count = c(45, 23, 15)
#' )
#' emojis <- c(
#'   "Scratch" = "X",
#'   "Dent" = "O",
#'   "Missing Part" = "!"
#' )
#' plot_pareto_emoji_labels(data, "defect", "count", emojis)
#' }
#'
#' @export
plot_pareto_emoji_labels <- function(data,
                                     category_col,
                                     count_col,
                                     emojis,
                                     emoji_size = 5,
                                     theme = NULL,
                                     title = NULL,
                                     subtitle = NULL,
                                     ...) {
  # Check for required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }

  theme_obj <- as_iqr_theme_object(theme)
  iqr_theme <- theme_obj$theme_iqr()
  c <- .iqr_aes(theme_obj)

  # Sort data
  df <- data[order(data[[count_col]], decreasing = TRUE), ]
  categories <- df[[category_col]]

  # Add emoji column
  df$.emoji <- as.character(emojis[df[[category_col]]])
  df$.emoji[is.na(df$.emoji)] <- "[box]"  # Default emoji

  df[[category_col]] <- factor(df[[category_col]], levels = categories)

  # Calculate cumulative percentage
  df$.cum_pct <- cumsum(df[[count_col]]) / sum(df[[count_col]], na.rm = TRUE) * 100
  max_count <- max(df[[count_col]], na.rm = TRUE)
  df$.cum_y <- df$.cum_pct / 100 * max_count

  # Create base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[category_col]], y = .data[[count_col]])) +
    ggplot2::geom_col(...)

  # Add cumulative line
  p <- p +
    ggplot2::geom_line(
      ggplot2::aes(x = .data[[category_col]], y = .data$.cum_y, group = 1),
      linewidth = 1,
      color = c$danger
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data[[category_col]], y = .data$.cum_y),
      shape = 19,
      size = 2,
      color = c$danger
    )

  # Apply theme and styling
  p <- p +
    iqr_theme +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = emoji_size * 3.5,
        margin = ggplot2::margin(t = 5)
      ),
      legend.position = "none",
      # Reduce spacing
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
    ) +
    ggplot2::scale_x_discrete(
      labels = df$.emoji
    ) +
    ggplot2::labs(
      x = category_col,
      y = count_col,
      title = title %||% "Pareto Chart",
      subtitle = subtitle %||% "with emoji labels"
    )

  return(p)
}
