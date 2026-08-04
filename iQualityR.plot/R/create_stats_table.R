# =============================================================================
# File: R/create_stats_table.R
# Description: Generic data.frame -> themed tableGrob converter, modeled after
#   create_anova_table() but generalised for any statistics table (capability
#   indices, PPM performance, process summary, etc.). Supports row-level
#   semantic colouring driven by a status column (pass/watch/fail/good/bad/...)
#   and column-level highlights. Every colour is resolved through .iqr_aes(),
#   no hard-coded hex.
# =============================================================================

#' Create a Themed Statistics Table Grob
#'
#' Converts any data frame into a \code{gridExtra::tableGrob} object styled by
#' the active \code{IqrTheme}. Useful for embedding a Minitab-style numeric
#' summary table alongside a ggplot (see \code{\link{compose_table_plot}}).
#'
#' @param df A data.frame. Row names (if present) are prepended as the first
#'   column labelled by \code{rowname_col}.
#' @param theme Theme spec (NULL / string / function / IqrTheme).
#' @param digits Integer. Decimal places for numeric columns (default 4).
#' @param font_size Numeric. Table font size in pt (default 9).
#' @param rowname_col Character. Column name to use for the prepended row-names
#'   column (default "Variable").
#' @param status_col Character or NULL. If non-NULL, the named column is used
#'   to colour whole rows by semantic role: values matching
#'   \code{c("pass","good")} -> success, \code{c("watch","warn","neutral")} ->
#'   warning, \code{c("fail","bad")} -> fail. Unknown values fall back to
#'   \code{surface}. The status column itself is dropped from the displayed
#'   table (set \code{keep_status_col = TRUE} to retain it).
#' @param keep_status_col Logical. If FALSE (default) the \code{status_col}
#'   is dropped from the displayed table after colouring; if TRUE it is kept.
#' @param highlight_cols Character vector of column names to highlight with
#'   \code{surface_soft} background (e.g. the point estimate column).
#' @param zebra Logical. If TRUE (default), alternating rows get a
#'   \code{surface_soft} background to improve readability of wide tables.
#' @param na_string Character. Display string for NA values (default "-").
#' @return A \code{gridExtra::tableGrob} object.
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(Index = c("Cp","Cpk","Pp","Ppk"),
#'                  Value = c(1.45, 1.20, 1.50, 1.25),
#'                  Status = c("pass","watch","pass","watch"))
#' g <- create_stats_table(df, status_col = "Status")
#' grid::grid.draw(g)
#' }
create_stats_table <- function(df, theme = NULL, digits = 4, font_size = 9,
                               rowname_col = "Variable",
                               status_col = NULL, keep_status_col = FALSE,
                               highlight_cols = NULL, zebra = TRUE,
                               na_string = "-") {
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("gridExtra package is required for creating table grobs.")
  }
  if (!is.data.frame(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)

  # ---- 1. Prepend row names as a column (like create_anova_table) -----------
  if (is.null(rownames(df)) || all(rownames(df) == as.character(seq_len(nrow(df))))) {
    # no meaningful row names; do not prepend
  } else {
    df <- cbind(setNames(list(rownames(df)), rowname_col), df)
    rownames(df) <- NULL
  }

  # ---- 2. Format numeric columns ------------------------------------------
  num_cols <- vapply(df, is.numeric, logical(1))
  if (any(num_cols)) {
    df[num_cols] <- lapply(df[num_cols], function(x) {
      out <- vapply(x, function(v) {
        if (is.na(v)) na_string
        else formatC(as.numeric(v), format = "f", digits = digits)
      }, character(1))
      out
    })
  }
  # Replace NA in non-numeric columns too
  if (any(!num_cols)) {
    df[!num_cols] <- lapply(df[!num_cols], function(x) {
      x[is.na(x)] <- na_string; x
    })
  }

  # ---- 3. Resolve colours via .iqr_aes (no hard-coding) --------------------
  c <- .iqr_aes(theme)
  font_family <- tryCatch(
    .iqr_plotter$.pal_ui(c$theme_obj, "font_family", default = "sans") %||% "sans",
    error = function(e) "sans"
  )

  base_theme <- gridExtra::ttheme_default(
    base_size = font_size,
    base_colour = c$text,
    base_family = font_family,
    padding = grid::unit(c(2.5, 4), "mm")
  )
  header_theme <- utils::modifyList(base_theme, list(
    colhead = list(
      fg_params = list(fontface = "bold", col = c$surface),
      bg_params = list(fill = c$primary)
    )
  ))

  # ---- 4. Build per-cell background colour matrix -------------------------
  n_r <- nrow(df); n_c <- ncol(df)
  bg_colors <- matrix(c$surface, nrow = n_r, ncol = n_c)

  # 4a. Zebra striping
  if (zebra && n_r > 1) {
    even_rows <- seq(2, n_r, by = 2)
    bg_colors[even_rows, ] <- c$surface_soft
  }

  # 4b. Column highlight
  if (!is.null(highlight_cols)) {
    hi_idx <- which(names(df) %in% highlight_cols)
    if (length(hi_idx) > 0) {
      bg_colors[, hi_idx] <- c$surface_soft
    }
  }

  # 4c. Row-level semantic colouring via status_col
  status_present <- !is.null(status_col) && status_col %in% names(df)
  if (status_present) {
    s_vals <- tolower(as.character(df[[status_col]]))
    row_status <- vapply(s_vals, function(s) {
      if (s %in% c("pass", "good", "capable", "ok")) "pass"
      else if (s %in% c("watch", "warn", "neutral", "marginal")) "watch"
      else if (s %in% c("fail", "bad", "not capable", "nc")) "fail"
      else "neutral"
    }, character(1))
    row_fill <- c(pass = c$success, watch = c$warning, fail = c$fail,
                  neutral = c$surface)
    bg_colors[] <- row_fill[row_status][col(bg_colors)]
    # Remove the status column from display unless explicitly kept
    if (!keep_status_col) {
      df[[status_col]] <- NULL
      bg_colors <- bg_colors[, -which(colnames(bg_colors) == status_col),
                             drop = FALSE]
    }
  }

  # ---- 5. Final theme ------------------------------------------------------
  custom_theme <- utils::modifyList(header_theme, list(
    core = list(
      fg_params = list(col = c$text, cex = 1),
      bg_params = list(fill = bg_colors)
    )
  ))

  gridExtra::tableGrob(df, rows = NULL, theme = custom_theme)
}

#' Compose a Table Grob and One or More ggplot Objects
#'
#' Patchwork-based layout helper for the "Minitab-style table + chart" pattern.
#' Places a \code{tableGrob} (from \code{\link{create_stats_table}}) above or
#' beside one or more ggplot objects at a configurable height/width ratio.
#'
#' @param table A \code{tableGrob} object (from \code{create_stats_table}).
#' @param plots A single ggplot object or a \code{list} of ggplot objects.
#' @param direction Character: \code{"vertical"} (table on top, plots below) or
#'   \code{"horizontal"} (table on left, plots on right).
#' @param ratios Numeric vector of length 2 giving the relative size of
#'   \code{table} vs \code{plots} (default \code{c(1, 3)} for vertical,
#'   \code{c(1, 4)} for horizontal).
#' @param theme Theme spec (only used to resolve annotation title colour).
#' @param title Optional overall title for the composed patchwork.
#' @param subtitle Optional overall subtitle.
#' @return A \code{patchwork} object.
#' @export
#' @examples
#' \dontrun{
#' g <- create_stats_table(df)
#' p <- plot_capability_histogram(...)
#' compose_table_plot(g, p, direction = "vertical", ratios = c(1, 4))
#' }
compose_table_plot <- function(table, plots, direction = c("vertical", "horizontal"),
                               ratios = NULL, theme = NULL,
                               title = NULL, subtitle = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork package is required for compose_table_plot.")
  }
  direction <- match.arg(direction)
  if (inherits(plots, "ggplot")) plots <- list(plots)

  if (is.null(ratios)) {
    ratios <- if (direction == "vertical") c(1, 3) else c(1, 4)
  }

  c <- .iqr_aes(theme)
  # patchwork can wrap arbitrary grobs via wrap_plots + area_spans; the simplest
  # reliable approach is to convert the tableGrob into a one-panel ggplot via
  # patchwork::wrap_elements() so it composes with ggplot objects.
  table_patch <- patchwork::wrap_elements(table)

  if (length(plots) == 1) {
    if (direction == "vertical") {
      comp <- table_patch / plots[[1]] + patchwork::plot_layout(heights = ratios)
    } else {
      comp <- table_patch + plots[[1]] + patchwork::plot_layout(widths = ratios)
    }
  } else {
    # multiple plots: stack plots vertically (or horizontally) first, then
    # attach the table along the chosen direction
    if (direction == "vertical") {
      plots_block <- patchwork::wrap_plots(plots, ncol = 1)
      comp <- table_patch / plots_block + patchwork::plot_layout(heights = ratios)
    } else {
      plots_block <- patchwork::wrap_plots(plots, nrow = 1)
      comp <- table_patch + plots_block + patchwork::plot_layout(widths = ratios)
    }
  }

  if (!is.null(title) || !is.null(subtitle)) {
    comp <- comp + patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = ggplot2::theme(plot.title = ggplot2::element_text(
        face = "bold", color = c$text, size = 13))
    )
  }
  comp
}
