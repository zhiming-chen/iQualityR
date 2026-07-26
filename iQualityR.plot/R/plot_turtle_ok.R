#' Draw Turtle Diagram, integrated with IqrTheme theme system, supports export
#'
#' @importFrom DiagrammeR grViz
#' @importFrom utils modifyList
#' @name iQualityR.plot-turtle
#' @keywords internal
NULL

#' Draw Turtle Diagram, integrated with IqrTheme theme system, supports export
#'
#' @param process_name Character string, process name
#' @param process_owner Character string, process owner
#' @param activities Character vector, core activity steps
#' @param inputs Inputs, character vector or category list
#' @param outputs Outputs, character vector or category list
#' @param resources Resources, character vector
#' @param procedures Methods/procedures, character vector
#' @param metrics Metrics, character vector
#' @param responsibilities Responsibilities, character vector
#' @param direction Turtle head direction: `"T"`(top), `"B"`(bottom), `"L"`(left), `"R"`(right)
#' @param theme_style Theme style: `"tech"` or `"academic"`
#' @param iqr_theme Optional, pass `IqrTheme` object directly (takes precedence over `theme_style`)
#' @param user_colors Optional named vector of user-defined colors.
#' @param title Main title (optional)
#' @param subtitle Subtitle (optional)
#' @param subtitle_fontsize Subtitle font size, default 14
#' @param annotation Footnote/annotation (optional, displayed at the bottom of the chart)
#' @param annotation_fontsize Footnote font size, default 8
#' @param output_file Output file path (e.g., "turtle.png"), if NULL then no export
#' @param output_type Output type, `"png"`, `"svg"`, `"pdf"`, default `"png"`
#' @param ... Other arguments passed to `IqrTheme$new()`
#'
#' @return DiagrammeR::grViz object (invisible), with optional file export
#' @export
#'
#' @examples
#' \dontrun{
#' g <- plot_turtle_diagram(
#'   process_name = "Assembly process",
#'   title = "Turtle diagram example",
#'   subtitle = "Version 1.0",
#'   annotation = "Note: based on IATF 16949 standard",
#'   output_file = "turtle.png"
#' )
#' }
#'
plot_turtle_diagram <- function(process_name,
                                process_owner = NULL,
                                activities = NULL,
                                inputs = NULL,
                                outputs = NULL,
                                resources = NULL,
                                procedures = NULL,
                                metrics = NULL,
                                responsibilities = NULL,
                                direction = c("T", "B", "L", "R"),
                                theme_style = "workbench",
                                iqr_theme = NULL,
                                user_colors = NULL,
                                title = NULL,
                                subtitle = NULL,
                                subtitle_fontsize = 14,
                                annotation = NULL,
                                annotation_fontsize = 10,
                                output_file = NULL,
                                output_type = c("png", "svg", "pdf"),
                                ...) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Package 'DiagrammeR' is required. Please install it.")
  }

  direction <- match.arg(direction)
  output_type <- match.arg(output_type)

  iqr_theme_obj <- if (is.null(iqr_theme)) {
    as_iqr_theme_object(theme_style)
  } else {
    as_iqr_theme_object(iqr_theme)
  }
  ui_colors <- iqr_theme_obj$get_ui_colors()
  data_colors <- iqr_theme_obj$get_data_colors("discrete")
  data_colors <- rep(data_colors, length.out = 6)
  theme_fontname <- ui_colors$font_family %||% "sans"
  theme_fontsize_body <- ui_colors$fontsize_base %||% 16
  theme_fontsize_title <- ui_colors$fontsize_title %||% 20
  bgcolor <- ui_colors$bg %||% "#FFFFFF"
  theme_preset_fontcolor <- ui_colors$text %||% NULL

  default_fills <- c(
    process = ui_colors$primary,
    inputs = data_colors[[1]],
    outputs = data_colors[[2]],
    resources = data_colors[[3]],
    procedures = data_colors[[4]],
    metrics = data_colors[[5]],
    responsibilities = data_colors[[6]]
  )

  # ---- Escape functions ----
  escape_percent <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    gsub("%", "%%", x)
  }
  escape_html <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    x <- gsub("&", "&amp;", x)
    x <- gsub("<", "&lt;", x)
    x <- gsub(">", "&gt;", x)
    x
  }

  get_contrast_color <- function(hex_color) {
    if (is.null(hex_color) || hex_color == "") {
      return("black")
    }
    if (grepl("^#[0-9A-Fa-f]{6}$", hex_color)) {
      r <- strtoi(substr(hex_color, 2, 3), 16)
      g <- strtoi(substr(hex_color, 4, 5), 16)
      b <- strtoi(substr(hex_color, 6, 7), 16)
      brightness <- (r * 299 + g * 587 + b * 114) / 1000
      return(if (brightness > 128) "black" else "white")
    } else {
      return("black")
    }
  }

  # Theme colors - support both named vectors and lists
  if (!is.null(user_colors)) {
    theme_colors <- default_fills
    if (is.list(user_colors) && !is.atomic(user_colors)) {
      # user_colors is a proper list
      for (comp in names(default_fills)) {
        if (!is.null(user_colors[[comp]])) theme_colors[comp] <- user_colors[[comp]]
      }
    } else if (is.atomic(user_colors) && !is.null(names(user_colors))) {
      # user_colors is a named vector
      for (comp in names(default_fills)) {
        if (comp %in% names(user_colors)) theme_colors[comp] <- user_colors[comp]
      }
    }
  } else {
    theme_colors <- default_fills
  }

  # Other theme configuration
  dots <- list(...)
  user_theme <- dots$theme
  force_fontcolor <- NULL
  title_color <- NULL
  if (!is.null(user_theme) && is.list(user_theme)) {
    if (!is.null(user_theme$fontcolor)) force_fontcolor <- user_theme$fontcolor
    if (!is.null(user_theme$title_color)) title_color <- user_theme$title_color
    if (!is.null(user_theme$bgcolor)) bgcolor <- user_theme$bgcolor
  }

  if (is.null(process_name)) stop("process_name is required.")
  if (is.null(process_owner)) process_owner <- "Unspecified"
  if (is.null(activities)) activities <- c("Step 1", "Step 2", "Step 3")

  standardize_io <- function(x, type = "inputs") {
    if (is.null(x)) {
      return(list())
    }
    if (is.character(x)) {
      if (type == "inputs") {
        result <- list()
        if (length(x) >= 1) result$material <- x[1]
        if (length(x) >= 2) result$information <- x[2]
        if (length(x) >= 3) result$from_process <- x[3]
        # Preserve additional items as extra1, extra2, ...
        if (length(x) > 3) {
          for (j in 4:length(x)) {
            result[[paste0("extra", j - 3)]] <- x[j]
          }
        }
        return(result)
      } else {
        result <- list()
        if (length(x) >= 1) result$product <- x[1]
        if (length(x) >= 2) result$record <- x[2]
        if (length(x) >= 3) result$to_process <- x[3]
        # Preserve additional items as extra1, extra2, ...
        if (length(x) > 3) {
          for (j in 4:length(x)) {
            result[[paste0("extra", j - 3)]] <- x[j]
          }
        }
        return(result)
      }
    }
    return(x)
  }

  inputs_list <- standardize_io(inputs, "inputs")
  outputs_list <- standardize_io(outputs, "outputs")

  if (is.null(resources)) resources <- c("Resource 1", "Resource 2")
  if (is.null(procedures)) procedures <- c("Procedure 1", "Procedure 2")
  if (is.null(metrics)) metrics <- c("Metric 1", "Metric 2")
  if (is.null(responsibilities)) responsibilities <- c("Role 1", "Role 2")

  # ---- Escaped vec_to_html ----
  vec_to_html <- function(vec, prefix = NULL, bullet = "*") {
    if (length(vec) == 0) {
      return("")
    }
    vec <- sapply(vec, function(v) {
      v <- escape_percent(v)
      v <- escape_html(v)
      v
    })
    lines <- paste0(escape_html(bullet), " ", vec, "<br/>")
    if (!is.null(prefix)) {
      res <- paste0("<b>", prefix, ":</b><br/>", paste(lines, collapse = ""))
    } else {
      res <- paste(lines, collapse = "")
    }
    return(res)
  }

  # Center process label (needs escaping)
  process_label <- sprintf(
    '<b>%s</b><br/><font point-size="%s">Owner: %s</font><br/><font point-size="%s"><i>%s</i></font>',
    escape_html(escape_percent(process_name)),
    theme_fontsize_body,
    escape_html(escape_percent(process_owner)),
    theme_fontsize_body - 1,
    escape_html(escape_percent(paste(activities, collapse = " -> ")))
  )

  input_parts <- c()
  if (!is.null(inputs_list$material)) {
    input_parts <- c(input_parts, vec_to_html(inputs_list$material, "Material", "*"))
  }
  if (!is.null(inputs_list$information)) {
    input_parts <- c(input_parts, vec_to_html(inputs_list$information, "Information", "*"))
  }
  if (!is.null(inputs_list$from_process)) {
    input_parts <- c(input_parts, vec_to_html(inputs_list$from_process, "Upstream process", "->"))
  }
  inputs_label <- sprintf("<b>INPUTS (Tail)</b><br/>%s", paste(input_parts, collapse = ""))

  output_parts <- c()
  if (!is.null(outputs_list$product)) {
    output_parts <- c(output_parts, vec_to_html(outputs_list$product, "Product", "*"))
  }
  if (!is.null(outputs_list$record)) {
    output_parts <- c(output_parts, vec_to_html(outputs_list$record, "Record", "*"))
  }
  if (!is.null(outputs_list$to_process)) {
    output_parts <- c(output_parts, vec_to_html(outputs_list$to_process, "Downstream process", "->"))
  }
  outputs_label <- sprintf("<b>OUTPUTS (Head)</b><br/>%s", paste(output_parts, collapse = ""))

  resources_label <- sprintf("<b>RESOURCES</b><br/>%s", vec_to_html(resources, bullet = "*"))
  procedures_label <- sprintf("<b>PROCEDURES</b><br/>%s", vec_to_html(procedures, bullet = "*"))
  metrics_label <- sprintf("<b>METRICS</b><br/>%s", vec_to_html(metrics, bullet = "*"))
  responsibilities_label <- sprintf("<b>RESPONSIBILITIES</b><br/>%s", vec_to_html(responsibilities, bullet = "*"))

  # Coordinate calculation (keep unchanged)
  offset_body <- c(0, 0)
  offset_head <- switch(direction,
    T = c(0, 5),
    B = c(0, -5),
    L = c(-5, 0),
    R = c(5, 0)
  )
  offset_tail <- -offset_head

  if (direction %in% c("T", "B")) {
    leg_up_left <- c(-4, 3)
    leg_up_right <- c(4, 3)
    leg_down_left <- c(-4, -3)
    leg_down_right <- c(4, -3)
    if (direction == "T") {
      pos_resources <- leg_up_left
      pos_procedures <- leg_up_right
      pos_responsibilities <- leg_down_left
      pos_metrics <- leg_down_right
    } else {
      pos_resources <- leg_down_left
      pos_procedures <- leg_down_right
      pos_responsibilities <- leg_up_left
      pos_metrics <- leg_up_right
    }
  } else {
    leg_left_up <- c(-3, 4)
    leg_left_down <- c(-3, -4)
    leg_right_up <- c(3, 4)
    leg_right_down <- c(3, -4)
    if (direction == "L") {
      pos_resources <- leg_left_up
      pos_procedures <- leg_left_down
      pos_responsibilities <- leg_right_up
      pos_metrics <- leg_right_down
    } else {
      pos_resources <- leg_right_up
      pos_procedures <- leg_right_down
      pos_responsibilities <- leg_left_up
      pos_metrics <- leg_left_down
    }
  }

  fmt_pos <- function(xy) sprintf("%.1f,%.1f!", xy[1], xy[2])

  pos_process <- fmt_pos(offset_body)
  pos_inputs <- fmt_pos(offset_tail)
  pos_outputs <- fmt_pos(offset_head)
  pos_resources <- fmt_pos(pos_resources)
  pos_procedures <- fmt_pos(pos_procedures)
  pos_responsibilities <- fmt_pos(pos_responsibilities)
  pos_metrics <- fmt_pos(pos_metrics)

  node_width <- 2.5
  node_height <- 0.8
  process_width <- 3.0
  process_height <- 1.3

  if (!is.null(force_fontcolor)) {
    process_fc <- inputs_fc <- outputs_fc <- resources_fc <- procedures_fc <- metrics_fc <- responsibilities_fc <- force_fontcolor
  } else {
    if (!is.null(theme_preset_fontcolor)) {
      process_fc <- inputs_fc <- outputs_fc <- resources_fc <- procedures_fc <- metrics_fc <- responsibilities_fc <- theme_preset_fontcolor
    } else {
      process_fc <- get_contrast_color(theme_colors["process"])
      inputs_fc <- get_contrast_color(theme_colors["inputs"])
      outputs_fc <- get_contrast_color(theme_colors["outputs"])
      resources_fc <- get_contrast_color(theme_colors["resources"])
      procedures_fc <- get_contrast_color(theme_colors["procedures"])
      metrics_fc <- get_contrast_color(theme_colors["metrics"])
      responsibilities_fc <- get_contrast_color(theme_colors["responsibilities"])
    }
  }

  if (!is.null(title_color)) {
    title_fontcolor <- title_color
  } else if (!is.null(theme_preset_fontcolor)) {
    title_fontcolor <- theme_preset_fontcolor
  } else {
    title_fontcolor <- get_contrast_color(bgcolor)
  }

  node_attrs <- sprintf(
    'node [shape = box, style = filled, fontname = "%s", margin = "0.2,0.1", width = %.1f, height = %.1f]',
    theme_fontname, node_width, node_height
  )

  process_node <- sprintf(
    'process [label = <%s>, fillcolor = "%s", fontcolor = "%s", shape = ellipse, penwidth = 3, width = %.1f, height = %.1f, pos = "%s"]',
    process_label, theme_colors["process"], process_fc, process_width, process_height, pos_process
  )

  inputs_node <- sprintf(
    'inputs [label = <%s>, fillcolor = "%s", fontcolor = "%s", pos = "%s"]',
    inputs_label, theme_colors["inputs"], inputs_fc, pos_inputs
  )
  outputs_node <- sprintf(
    'outputs [label = <%s>, fillcolor = "%s", fontcolor = "%s", pos = "%s"]',
    outputs_label, theme_colors["outputs"], outputs_fc, pos_outputs
  )
  resources_node <- sprintf(
    'resources [label = <%s>, fillcolor = "%s", fontcolor = "%s", pos = "%s"]',
    resources_label, theme_colors["resources"], resources_fc, pos_resources
  )
  procedures_node <- sprintf(
    'procedures [label = <%s>, fillcolor = "%s", fontcolor = "%s", pos = "%s"]',
    procedures_label, theme_colors["procedures"], procedures_fc, pos_procedures
  )
  responsibilities_node <- sprintf(
    'responsibilities [label = <%s>, fillcolor = "%s", fontcolor = "%s", pos = "%s"]',
    responsibilities_label, theme_colors["responsibilities"], responsibilities_fc, pos_responsibilities
  )
  metrics_node <- sprintf(
    'metrics [label = <%s>, fillcolor = "%s", fontcolor = "%s", pos = "%s"]',
    metrics_label, theme_colors["metrics"], metrics_fc, pos_metrics
  )

  edges <- "
    inputs -> process
    process -> outputs
    resources -> process
    procedures -> process
    responsibilities -> process
    metrics -> process
  "

  graph_attrs <- sprintf(
    'graph [layout = neato, overlap = false, splines = true, nodesep = 0.8, ranksep = 0.8, fontname = "%s", bgcolor = "%s"]',
    theme_fontname, bgcolor
  )

  # Title and subtitle (also need escaping)
  graph_title_attr <- ""
  if (!is.null(title) || !is.null(subtitle)) {
    html_parts <- c()
    if (!is.null(title)) {
      html_parts <- c(html_parts, sprintf(
        '<font point-size="%d" color="%s"><b>%s</b></font>',
        theme_fontsize_title, title_fontcolor,
        escape_html(escape_percent(title))
      ))
    }
    if (!is.null(subtitle)) {
      html_parts <- c(html_parts, sprintf(
        '<font point-size="%d" color="%s">%s</font>',
        subtitle_fontsize, title_fontcolor,
        escape_html(escape_percent(subtitle))
      ))
    }
    label_html <- paste0("<", paste(html_parts, collapse = "<br/>"), ">")
    graph_title_attr <- paste0('labelloc="t"; label=', label_html, ";")
  }

  annotation_node <- ""
  if (!is.null(annotation) && nchar(annotation) > 0) {
    annotation_label <- sprintf(
      '<font point-size="%s" color="%s">%s</font>',
      annotation_fontsize, title_fontcolor,
      escape_html(escape_percent(annotation))
    )
    annotation_node <- sprintf(
      'annotation [label = <%s>, shape = plaintext, fillcolor = "transparent", fontcolor = "%s", fontsize = %d, pos = "0,-6!"];',
      annotation_label, title_fontcolor, annotation_fontsize
    )
  }

  dot_str <- sprintf(
    "digraph turtle_diagram {
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
    }",
    graph_attrs,
    graph_title_attr,
    node_attrs,
    process_node,
    inputs_node,
    outputs_node,
    resources_node,
    procedures_node,
    responsibilities_node,
    metrics_node,
    edges,
    annotation_node,
    if (annotation_node != "") "" else ""
  )

  dot_str <- gsub("\\n\\s*\\n", "\n", dot_str)

  g <- DiagrammeR::grViz(dot_str)
  return(g)
}
