# =============================================================================
# File: iQualityR.plot/R/plot_fishbone.R
# Description: Fishbone diagram (cause-and-effect diagram) series functions
# Includes: basic fishbone diagram, weighted fishbone diagram, faceted fishbone diagram
# Dependencies: ggplot2, patchwork
# =============================================================================

#' @importFrom ggplot2 ggplot aes geom_segment geom_label geom_text
#' @importFrom ggplot2 scale_color_manual scale_linewidth labs theme
#' @importFrom ggplot2 element_blank element_text arrow unit coord_fixed
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom stats na.omit
#' @name iQualityR.plot-fishbone
#' @title Fishbone diagram functions
#' @description Fishbone (cause-and-effect) diagram series for quality management root cause analysis.
#' @keywords internal
NULL

# NULL coalesce operator - already defined in utils in the .core package.
# `%||%` <- function(a, b) if (!is.null(a)) a else b

# -----------------------------------------------------------------------------
# Basic fishbone diagram
# -----------------------------------------------------------------------------

#' @title Basic fishbone diagram
#' @description
#' Create a standard fishbone diagram (Ishikawa diagram) without weight and importance encoding.
#' Suitable for simple root cause analysis scenarios.
#'
#' @param problem Problem statement (displayed at the fish head position).
#' @param categories Named list, each element is a main bone (level-1 category),
#'   containing a character vector of sub-causes (level-2 categories).
#' @param industry Industry template: "manufacturing" (5M1E) or "service" (5P).
#' @param font_size Base font size (default 10).
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param theme Theme (reserved).
#'
#' @return ggplot object.
#'
#' @details
#' Fishbone diagram structure:
#' \itemize{
#'   \item Spine: central horizontal line, arrow pointing to the problem
#'   \item Level-1 categories (main bones): 45-degree angle with the spine, alternating top and bottom
#'   \item Level-2 categories (sub-bones): horizontal lines, parallel to the spine
#' }
#'
#' @examples
#' \dontrun{
#' # Manufacturing example
#' categories <- list(
#'   Manpower = c("Lack of training", "Low skills", "Low motivation"),
#'   Machine = c("Poor maintenance", "Aging equipment", "Missing calibration"),
#'   Material = c("Raw material defects", "Supplier changes"),
#'   Method = c("Unclear SOP", "Unreasonable process"),
#'   Measurement = c("Measurement error", "Inconsistent standards"),
#'   Environment = c("Temperature fluctuation", "High humidity")
#' )
#'
#' plot_fishbone_basic(
#'   problem = "Product quality issue",
#'   categories = categories
#' )
#' }
#'
#' @export
plot_fishbone_basic <- function(problem,
                                categories = NULL,
                                industry = "manufacturing",
                                font_size = 10,
                                title = NULL,
                                subtitle = NULL,
                                theme = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }

  # Industry templates
  industry_templates <- list(
    manufacturing = c("Manpower", "Machine", "Material", "Method", "Measurement", "Environment"),
    service = c("People", "Process", "Place", "Product", "Promotion")
  )

  if (is.null(categories)) {
    if (industry %in% names(industry_templates)) {
      categories <- lapply(industry_templates[[industry]], function(cat) {
        c("Cause 1", "Cause 2", "Cause 3")
      })
      names(categories) <- industry_templates[[industry]]
    } else {
      stop("Please provide categories or use a valid industry template.")
    }
  }

  # Convert to weighted format (uniform weight and importance)
  weighted_categories <- lapply(categories, function(subs) {
    lapply(subs, function(sub_name) {
      list(
        name = sub_name,
        weight = 5,
        importance = "normal"
      )
    })
  })

  # Call weighted version, disable weight and importance display
  plot_fishbone_weighted(
    problem = problem,
    categories = weighted_categories,
    industry = industry,
    weight_range = c(1.5, 1.5),
    show_weights = FALSE,
    show_importance = FALSE,
    font_size = font_size,
    title = title %||% "Fishbone Diagram (Cause-Effect Diagram)",
    subtitle = subtitle,
    theme = theme
  )
}

# -----------------------------------------------------------------------------
# Weighted fishbone diagram
# -----------------------------------------------------------------------------

#' @title Weighted fishbone diagram
#' @description
#' Create an enhanced fishbone diagram with weight (line thickness) and importance (color) encoding.
#' Supports manufacturing (5M1E) and service (5P) templates.
#'
#' @param problem Problem statement (displayed at the fish head position).
#' @param categories Named list, each element is a main bone (level-1 category),
#'   containing a list of sub-causes. Each sub-cause should contain:
#'   \itemize{
#'     \item name: cause name
#'     \item weight: weight (numeric, controls line thickness)
#'     \item importance: importance ("critical", "important", "normal")
#'     \item sub_categories: (optional) level-3 categories, same format as sub-causes
#'   }
#' @param industry Industry template: "manufacturing" (5M1E) or "service" (5P).
#' @param weight_range Line thickness range (default c(0.8, 3.5)).
#' @param importance_colors Importance color mapping.
#'   Default: critical = "red", important = "orange", normal = "gray60".
#' @param show_weights Whether to show weight values (default TRUE).
#' @param show_importance Whether to show importance labels (default TRUE).
#' @param font_size Base font size (default 12).
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param theme Theme (reserved).
#'
#' @return ggplot object.
#'
#' @details
#' Fishbone diagram structure:
#' \itemize{
#'   \item Line thickness represents the cause weight
#'   \item Color represents importance level (red = critical, orange = important, gray = normal)
#'   \item Main bones alternate above and below the spine
#'   \item Sub-bones (level-2 categories) are parallel to the spine (horizontal lines)
#'   \item Level-3 categories are parallel to the main bones (45-degree angle)
#' }
#'
#' @examples
#' \dontrun{
#' # Manufacturing example with weights and importance
#' categories <- list(
#'   Manpower = list(
#'     list(name = "Lack of training", weight = 8, importance = "critical"),
#'     list(name = "Low skills", weight = 6, importance = "important"),
#'     list(name = "Low motivation", weight = 3, importance = "normal")
#'   ),
#'   Machine = list(
#'     list(name = "Poor maintenance", weight = 7, importance = "important"),
#'     list(name = "Aging equipment", weight = 9, importance = "critical"),
#'     list(name = "Missing calibration", weight = 4, importance = "normal")
#'   ),
#'   Material = list(
#'     list(name = "Raw material defects", weight = 8, importance = "critical"),
#'     list(name = "Supplier changes", weight = 5, importance = "important")
#'   ),
#'   Method = list(
#'     list(name = "Unclear SOP", weight = 6, importance = "important"),
#'     list(name = "Unreasonable process", weight = 7, importance = "important")
#'   ),
#'   Measurement = list(
#'     list(name = "Measurement error", weight = 5, importance = "important"),
#'     list(name = "Inconsistent standards", weight = 4, importance = "normal")
#'   ),
#'   Environment = list(
#'     list(name = "Temperature fluctuation", weight = 3, importance = "normal"),
#'     list(name = "High humidity", weight = 4, importance = "normal")
#'   )
#' )
#'
#' plot_fishbone_weighted(
#'   problem = "Product quality issue",
#'   categories = categories,
#'   industry = "manufacturing"
#' )
#' }
#'
#' @export
plot_fishbone_weighted <- function(problem,
                                   categories = NULL,
                                   industry = "manufacturing",
                                   weight_range = c(0.8, 3.5),
                                   importance_colors = NULL,
                                   show_weights = TRUE,
                                   show_importance = TRUE,
                                   font_size = 12,
                                   title = NULL,
                                   subtitle = NULL,
                                   theme = NULL) {
  iqr_theme <- as_iqr_theme_object(theme)
  ui_colors <- iqr_theme$get_ui_colors()

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }

  # Default importance colors (with fallback if ui_colors lacks semantic keys)
  if (is.null(importance_colors)) {
    importance_colors <- c(
      "critical"   = if (!is.null(ui_colors$danger) && nzchar(ui_colors$danger)) ui_colors$danger else "#E74C3C",
      "important"  = if (!is.null(ui_colors$warning) && nzchar(ui_colors$warning)) ui_colors$warning else "#F39C12",
      "normal"     = if (!is.null(ui_colors$muted) && nzchar(ui_colors$muted)) ui_colors$muted else "#95A5A6"
    )
  }

  # Industry templates
  industry_templates <- list(
    manufacturing = c("Manpower", "Machine", "Material", "Method", "Measurement", "Environment"),
    service = c("People", "Process", "Place", "Product", "Promotion")
  )

  if (is.null(categories)) {
    if (industry %in% names(industry_templates)) {
      categories <- lapply(industry_templates[[industry]], function(cat) {
        list(
          list(name = "Cause 1", weight = 5, importance = "normal"),
          list(name = "Cause 2", weight = 3, importance = "normal")
        )
      })
      names(categories) <- industry_templates[[industry]]
    } else {
      stop("Please provide categories or use a valid industry template.")
    }
  }

  # Prepare plotting data
  n_categories <- length(categories)
  category_names <- names(categories)

  # Layout parameters
  spine_length <- 14
  main_bone_length <- 5.0
  sub_bone_length <- 2.5
  main_bone_angle <- 45 # Angle between main bone and spine (degrees)

  # Create data frame
  segments_data <- data.frame(
    x = numeric(), y = numeric(), xend = numeric(), yend = numeric(),
    category = character(), bone_type = character(),
    weight = numeric(), importance = character(),
    stringsAsFactors = FALSE
  )

  labels_data <- data.frame(
    x = numeric(), y = numeric(), label = character(),
    category = character(), bone_type = character(),
    weight = numeric(), importance = character(),
    angle = numeric(),
    stringsAsFactors = FALSE
  )

  # Angle conversion
  angle_rad <- main_bone_angle * pi / 180
  cos_angle <- cos(angle_rad)
  sin_angle <- sin(angle_rad)

  # Calculate main bone and sub-bone positions
  for (i in seq_along(categories)) {
    cat_name <- category_names[i]
    sub_categories <- categories[[i]]
    n_subs <- length(sub_categories)

    # Main bone connection point on the spine (uniformly distributed)
    main_bone_x <- 1.5 + (i - 1) * (spine_length - 2) / max(n_categories - 1, 1)

    # Main bone direction: odd up, even down
    if (i %% 2 == 1) {
      main_bone_end_x <- main_bone_x + main_bone_length * cos_angle
      main_bone_end_y <- main_bone_length * sin_angle
      label_angle <- 1
    } else {
      main_bone_end_x <- main_bone_x + main_bone_length * cos_angle
      main_bone_end_y <- -main_bone_length * sin_angle
      label_angle <- -1
    }

    # Main bone segment
    main_bone <- data.frame(
      x = main_bone_x, y = 0,
      xend = main_bone_end_x, yend = main_bone_end_y,
      category = cat_name, bone_type = "main",
      weight = 2, importance = "normal",
      stringsAsFactors = FALSE
    )
    segments_data <- rbind(segments_data, main_bone)

    # Main bone label - add offset to avoid contact with the line
    label_offset_x <- 0.8
    label_offset_y <- 0.5

    main_label <- data.frame(
      x = main_bone_end_x + label_offset_x,
      y = main_bone_end_y + label_offset_y * label_angle,
      label = cat_name,
      category = cat_name, bone_type = "main",
      weight = 2, importance = "normal",
      angle = 0,
      stringsAsFactors = FALSE
    )
    labels_data <- rbind(labels_data, main_label)

    # Level-2 categories (middle bones): horizontal lines, parallel to the spine
    if (n_subs > 0) {
      for (j in seq_along(sub_categories)) {
        sub <- sub_categories[[j]]
        sub_name <- sub$name
        sub_weight <- sub$weight
        sub_importance <- sub$importance

        # Level-2 category connection point on level-1 category (uniformly distributed)
        t <- (j - 0.5) / n_subs
        connect_x <- main_bone_x + t * (main_bone_end_x - main_bone_x)
        connect_y <- 0 + t * (main_bone_end_y - 0)

        # Level-2 category line: horizontal line, parallel to the spine
        sub_end_x <- connect_x + sub_bone_length
        sub_end_y <- connect_y

        sub_bone <- data.frame(
          x = connect_x, y = connect_y,
          xend = sub_end_x, yend = sub_end_y,
          category = cat_name, bone_type = "sub",
          weight = sub_weight, importance = sub_importance,
          stringsAsFactors = FALSE
        )
        segments_data <- rbind(segments_data, sub_bone)

        # Label position
        label_x <- sub_end_x + 0.1
        sub_label_y <- sub_end_y + 0.5 * label_angle

        sub_label <- data.frame(
          x = label_x, y = sub_label_y,
          label = sub_name,
          category = cat_name, bone_type = "sub",
          weight = sub_weight, importance = sub_importance,
          angle = 0,
          stringsAsFactors = FALSE
        )
        labels_data <- rbind(labels_data, sub_label)

        # Level-3 categories (small bones): parallel to main bone (45-degree angle)
        if (!is.null(sub$sub_categories) && length(sub$sub_categories) > 0) {
          tertiary_cats <- sub$sub_categories
          n_tertiary <- length(tertiary_cats)

          # Reserve 25% of sub-bone end for level-2 text, level-3 connection points only in first 75%
          ter_zone_end <- connect_x + 0.75 * (sub_end_x - connect_x)

          for (k in seq_along(tertiary_cats)) {
            ter <- tertiary_cats[[k]]
            ter_name <- ter$name
            ter_weight <- ter$weight
            ter_importance <- ter$importance

            # Level-3 category connection point on level-2 category (uniformly distributed along first 75% of sub-bone)
            t_ter <- k / (n_tertiary + 1)
            ter_connect_x <- connect_x + t_ter * (ter_zone_end - connect_x)
            ter_connect_y <- connect_y

            # Level-3 category line: parallel to main bone (45-degree angle)
            ter_bone_length <- 1.5
            ter_end_x <- ter_connect_x + ter_bone_length * cos_angle
            ter_end_y <- ter_connect_y + ter_bone_length * sin_angle * label_angle

            ter_bone <- data.frame(
              x = ter_connect_x, y = ter_connect_y,
              xend = ter_end_x, yend = ter_end_y,
              category = cat_name, bone_type = "tertiary",
              weight = ter_weight, importance = ter_importance,
              stringsAsFactors = FALSE
            )
            segments_data <- rbind(segments_data, ter_bone)

            # Level-3 label: arranged parallel along the fishbone direction
            ter_label <- data.frame(
              x = ter_end_x + 0.15 * cos_angle,
              y = ter_end_y + 0.15 * sin_angle * label_angle,
              label = ter_name,
              category = cat_name, bone_type = "tertiary",
              weight = ter_weight, importance = ter_importance,
              angle = 45 * label_angle,
              stringsAsFactors = FALSE
            )
            labels_data <- rbind(labels_data, ter_label)
          }
        }

        # Weight label
        if (show_weights) {
          weight_label <- data.frame(
            x = label_x,
            y = sub_label_y + 0.4 * label_angle,
            label = paste0("w=", sub_weight),
            category = cat_name, bone_type = "weight",
            weight = sub_weight, importance = sub_importance,
            angle = 0,
            stringsAsFactors = FALSE
          )
          labels_data <- rbind(labels_data, weight_label)
        }

        # Importance label
        if (show_importance) {
          imp_label_text <- switch(sub_importance,
            "critical" = "Critical",
            "important" = "Important",
            "normal" = "Normal"
          )
          imp_label <- data.frame(
            x = label_x,
            y = sub_label_y + 0.8 * label_angle,
            label = imp_label_text,
            category = cat_name, bone_type = "importance",
            weight = sub_weight, importance = sub_importance,
            angle = 0,
            stringsAsFactors = FALSE
          )
          labels_data <- rbind(labels_data, imp_label)
        }
      }
    }
  }

  # Spine
  spine_data <- data.frame(
    x = 0, y = 0,
    xend = spine_length + 1, yend = 0,
    category = "spine", bone_type = "spine",
    weight = 2, importance = "normal",
    stringsAsFactors = FALSE
  )
  segments_data <- rbind(segments_data, spine_data)

  # Problem box (fish head)
  problem_box <- data.frame(
    x = spine_length + 2, y = 0,
    label = problem,
    category = "problem", bone_type = "problem",
    weight = 2, importance = "normal",
    angle = 0,
    stringsAsFactors = FALSE
  )
  labels_data <- rbind(labels_data, problem_box)

  # Normalize weights to line thickness range
  all_weights <- segments_data$weight[segments_data$bone_type == "sub"]
  if (length(all_weights) > 0) {
    min_w <- min(all_weights)
    max_w <- max(all_weights)
    if (max_w > min_w) {
      segments_data$line_width <- weight_range[1] +
        (segments_data$weight - min_w) / (max_w - min_w) * (weight_range[2] - weight_range[1])
    } else {
      segments_data$line_width <- mean(weight_range)
    }
  } else {
    segments_data$line_width <- mean(weight_range)
  }

  # Set colors based on importance
  segments_data$color <- importance_colors[segments_data$importance]
  labels_data$color <- importance_colors[labels_data$importance]

  # Calculate coordinate range
  x_max <- max(segments_data$xend, labels_data$x) + 2
  y_max <- max(abs(segments_data$y), abs(segments_data$yend), abs(labels_data$y)) + 2

  # Create ggplot
  p <- ggplot2::ggplot() +
    # Spine
    ggplot2::geom_segment(
      data = segments_data[segments_data$bone_type == "spine", ],
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      color = ui_colors$text,
      linewidth = 2,
      lineend = "round",
      arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed")
    ) +
    # Main bones
    ggplot2::geom_segment(
      data = segments_data[segments_data$bone_type == "main", ],
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      color = ui_colors$muted,
      linewidth = 1.5,
      lineend = "round"
    ) +
    # Sub-bones (level-2 categories)
    ggplot2::geom_segment(
      data = segments_data[segments_data$bone_type == "sub", ],
      ggplot2::aes(
        x = x, y = y, xend = xend, yend = yend,
        color = importance, linewidth = line_width
      ),
      lineend = "round"
    ) +
    # Level-3 categories
    ggplot2::geom_segment(
      data = segments_data[segments_data$bone_type == "tertiary", ],
      ggplot2::aes(
        x = x, y = y, xend = xend, yend = yend,
        color = importance
      ),
      linewidth = 1,
      lineend = "round"
    ) +
    # Problem box
    ggplot2::geom_label(
      data = labels_data[labels_data$bone_type == "problem", ],
      ggplot2::aes(x = x * 1.05, y = y, label = label),
      fill = ui_colors$danger,
      color = ui_colors$surface,
      fontface = "bold",
      size = font_size * 0.6,
      linewidth = 1
    ) +
    # Main bone labels
    ggplot2::geom_text(
      data = labels_data[labels_data$bone_type == "main", ],
      ggplot2::aes(x = x, y = y, label = label),
      color = ui_colors$text,
      fontface = "bold",
      size = font_size * 0.6,
      hjust = 0.5
    ) +
    # Sub-bone labels (level-2 categories)
    ggplot2::geom_text(
      data = labels_data[labels_data$bone_type == "sub", ],
      ggplot2::aes(x = x, y = y, label = label, color = importance),
      size = font_size * 0.5,
      fontface = "bold",
      hjust = 0.5
    ) +
    # Level-3 category labels (arranged parallel along the fishbone direction)
    ggplot2::geom_text(
      data = labels_data[labels_data$bone_type == "tertiary", ],
      ggplot2::aes(x = x, y = y, label = label, color = importance, angle = angle),
      size = font_size * 0.4,
      hjust = 0,
      vjust = 0.5
    ) +
    # Color scale
    ggplot2::scale_color_manual(
      values = importance_colors,
      name = "Importance",
      drop = FALSE,
      labels = c("critical" = "Critical", "important" = "Important", "normal" = "Normal")
    ) +
    # Line thickness scale
    ggplot2::scale_linewidth(
      range = weight_range,
      name = "Weight",
      guide = "legend"
    ) +
    # Theme
    iqr_theme$theme_iqr() +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, size = font_size * 2, face = "bold"),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = font_size * 1.5,
        color = ui_colors$muted
      )
    ) +
    ggplot2::labs(
      title = title %||% "Fishbone Diagram (Cause-Effect Diagram)",
      subtitle = subtitle %||% problem
    ) +
    # Fixed coordinate ratio
    ggplot2::coord_fixed(
      xlim = c(-1, x_max),
      ylim = c(-y_max, y_max),
      expand = FALSE
    )

  return(p)
}

# -----------------------------------------------------------------------------
# Faceted fishbone diagram
# -----------------------------------------------------------------------------

#' @title Faceted fishbone diagram
#' @description
#' Create a faceted layout of multiple fishbone diagrams to compare root cause analysis across different strata (e.g., shifts, departments, time periods).
#'
#' @param problem Problem statement.
#' @param strata_data Data frame, containing columns: stratum (stratification variable),
#'   category (main bone name), sub_category (cause name),
#'   weight (numeric), importance ("critical", "important", "normal").
#' @param industry Industry template.
#' @param weight_range Line thickness range (default c(0.5, 3)).
#' @param importance_colors Importance color mapping.
#' @param ncol Number of facet columns (default 2).
#' @param show_weights Whether to show weight values (default FALSE).
#' @param show_importance Whether to show importance labels (default FALSE).
#' @param font_size Base font size (default 8).
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param theme Optional theme specification.
#'
#' @return ggplot object (patchwork composition).
#'
#' @examples
#' \dontrun{
#' strata_data <- data.frame(
#'   stratum = rep(c("Morning shift", "Night shift"), each = 12),
#'   category = rep(c("Manpower", "Machine", "Material"), times = 8),
#'   sub_category = rep(c("Cause A", "Cause B"), times = 12),
#'   weight = c(
#'     8, 6, 7, 5, 9, 4, 6, 7, 5, 8, 4, 6,
#'     7, 5, 6, 4, 8, 3, 5, 6, 4, 7, 3, 5
#'   ),
#'   importance = c(
#'     "critical", "important", "important", "normal", "critical", "normal",
#'     "important", "important", "normal", "critical", "normal", "important",
#'     "important", "normal", "important", "normal", "critical", "normal",
#'     "normal", "important", "normal", "important", "normal", "normal"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' plot_fishbone_faceted(
#'   problem = "Product defect analysis",
#'   strata_data = strata_data,
#'   ncol = 2
#' )
#' }
#'
#' @export
plot_fishbone_faceted <- function(problem,
                                  strata_data,
                                  industry = "manufacturing",
                                  weight_range = c(0.5, 3),
                                  importance_colors = NULL,
                                  ncol = 2,
                                  show_weights = FALSE,
                                  show_importance = FALSE,
                                  font_size = 8,
                                  title = NULL,
                                  subtitle = NULL,
                                  theme = NULL) {
  iqr_theme <- as_iqr_theme_object(theme)
  ui_colors <- iqr_theme$get_ui_colors()

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.")
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork is required for faceted fishbone diagrams.")
  }

  # Default importance colors (with fallback if ui_colors lacks semantic keys)
  if (is.null(importance_colors)) {
    importance_colors <- c(
      "critical"   = if (!is.null(ui_colors$danger) && nzchar(ui_colors$danger)) ui_colors$danger else "#E74C3C",
      "important"  = if (!is.null(ui_colors$warning) && nzchar(ui_colors$warning)) ui_colors$warning else "#F39C12",
      "normal"     = if (!is.null(ui_colors$muted) && nzchar(ui_colors$muted)) ui_colors$muted else "#95A5A6"
    )
  }

  # Get all strata
  strata <- unique(strata_data$stratum)
  n_strata <- length(strata)

  # Create fishbone diagram for each stratum
  plots <- list()

  for (i in seq_along(strata)) {
    stratum <- strata[i]
    stratum_data <- strata_data[strata_data$stratum == stratum, ]

    # Convert to categories format
    categories <- list()
    for (cat in unique(stratum_data$category)) {
      cat_data <- stratum_data[stratum_data$category == cat, ]
      categories[[cat]] <- lapply(1:nrow(cat_data), function(j) {
        list(
          name = cat_data$sub_category[j],
          weight = cat_data$weight[j],
          importance = cat_data$importance[j]
        )
      })
    }

    # Create single fishbone diagram
    p <- plot_fishbone_weighted(
      problem = problem,
      categories = categories,
      industry = industry,
      weight_range = weight_range,
      importance_colors = importance_colors,
      show_weights = show_weights,
      show_importance = show_importance,
      font_size = font_size,
      title = stratum,
      theme = iqr_theme
    )

    # Remove legend (except for the last one)
    if (i < n_strata) {
      p <- p + ggplot2::theme(legend.position = "none")
    }

    plots[[i]] <- p
  }

  # Combine all charts
  combined <- patchwork::wrap_plots(plots, ncol = ncol) +
    patchwork::plot_annotation(
      title = title %||% "Faceted Fishbone Diagram",
      subtitle = subtitle,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = ui_colors$muted)
      )
    )

  return(combined)
}
