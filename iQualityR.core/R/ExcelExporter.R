#' @title ExcelExporter — Independent Excel Export Utility
#'
#' @description
#' Responsible for exporting data frame lists to formatted Excel files, with style
#' configuration obtained from a ThemeConfig object. Does not depend on IqrTheme, only
#' on ThemeConfig for UI colors and layout.
#'
#' @field config ThemeConfig object providing colors and layout configuration.
#' @field excel_styles_cache Cached list of openxlsx style objects.
#'
#' @importFrom R6 R6Class
#' @importFrom openxlsx createWorkbook addWorksheet writeData mergeCells addStyle setColWidths saveWorkbook createStyle
#' @export
ExcelExporter <- R6::R6Class("ExcelExporter",
  public = list(
    config = NULL,
    excel_styles_cache = NULL,

    #' @description Create an ExcelExporter instance.
    #' @param config ThemeConfig object providing UI colors.
    initialize = function(config) {
      if (!inherits(config, "ThemeConfig")) {
        stop("config must be a ThemeConfig object.", call. = FALSE)
      }
      self$config <- config
      self$excel_styles_cache <- self$generate_excel_styles()
      invisible(self)
    },

    #' @description Temporarily override Excel styles (useful for one-off exports).
    #' @param ... Named overrides for UI color slots (e.g., table_header_bg = "#FF0000").
    set_excel_theme = function(...) {
      overrides <- list(...)
      self$excel_styles_cache <- self$generate_excel_styles(overrides)
      invisible(self)
    },

    #' @description Reset Excel styles to the default ThemeConfig values.
    reset_excel_theme = function() {
      self$excel_styles_cache <- self$generate_excel_styles()
      invisible(self)
    },

    #' @description Export data to an Excel file with themed formatting.
    #' @param data Data frame or named list (each element corresponds to a worksheet).
    #' @param path Character. Output file path (auto-generated from timestamp if NULL).
    #' @param title Optional character vector. Title(s) for the first row of each sheet.
    #' @param sheet_names Optional character vector of worksheet names.
    export_excel = function(data, path = NULL, title = NULL, sheet_names = NULL) {
      if (is.null(path)) {
        path <- paste0("iqr_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      }

      if (is.data.frame(data)) {
        data_list <- list(data)
        if (is.null(sheet_names)) sheet_names <- "Sheet1"
      } else if (is.list(data)) {
        data_list <- data
        if (is.null(sheet_names)) {
          sheet_names <- names(data_list)
          if (is.null(sheet_names)) sheet_names <- paste0("Sheet", seq_along(data_list))
        } else {
          if (length(sheet_names) != length(data_list)) {
            stop("Length of sheet_names must equal number of data frames.", call. = FALSE)
          }
        }
      } else {
        stop("data must be a data frame or a list of data frames.", call. = FALSE)
      }

      sheet_names <- make.unique(sheet_names, sep = "_")
      sheet_names <- substr(sheet_names, 1, 31)

      wb <- openxlsx::createWorkbook()
      styles <- self$excel_styles_cache

      for (i in seq_along(data_list)) {
        df <- data_list[[i]]
        s_name <- sheet_names[i]
        openxlsx::addWorksheet(wb, s_name)

        row_cur <- 1
        col_end <- ncol(df)

        if (!is.null(title)) {
          curr_title <- if (length(title) >= i) title[i] else title[1]
          openxlsx::writeData(wb, s_name, curr_title, startRow = row_cur)
          openxlsx::mergeCells(wb, s_name, cols = 1:col_end, rows = row_cur)
          openxlsx::addStyle(wb, s_name, style = styles$main_title, rows = row_cur, cols = 1:col_end)
          row_cur <- row_cur + 1
        }

        openxlsx::writeData(wb, s_name, df, startRow = row_cur)
        openxlsx::addStyle(wb, s_name, style = styles$header, rows = row_cur, cols = 1:col_end, gridExpand = TRUE)

        if (nrow(df) > 0) {
          row_end <- row_cur + nrow(df)
          openxlsx::addStyle(wb, s_name, style = styles$body, rows = (row_cur + 1):row_end, cols = 1:col_end, gridExpand = TRUE)
          for (j in seq_len(nrow(df))) {
            if (j %% 2 == 0) {
              openxlsx::addStyle(wb, s_name, style = styles$stripe, rows = row_cur + j, cols = 1:col_end, gridExpand = TRUE)
            }
          }
          openxlsx::addStyle(wb, s_name, style = styles$t_thick, rows = row_cur, cols = 1:col_end, stack = TRUE)
          openxlsx::addStyle(wb, s_name, style = styles$b_thick, rows = row_end, cols = 1:col_end, stack = TRUE)
          openxlsx::addStyle(wb, s_name, style = styles$l_thick, rows = row_cur:row_end, cols = 1, stack = TRUE, gridExpand = TRUE)
          openxlsx::addStyle(wb, s_name, style = styles$r_thick, rows = row_cur:row_end, cols = col_end, stack = TRUE, gridExpand = TRUE)
        }

        openxlsx::setColWidths(wb, s_name, cols = 1:col_end, widths = "auto")
      }

      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
      message("File saved to: ", path)
      invisible(path)
    },

    #' @description Generate openxlsx style objects from ThemeConfig UI settings.
    #' @param override Optional list of UI overrides (e.g., list(table_header_bg = "#000")).
    generate_excel_styles = function(override = NULL) {
      base_ui <- self$config$config$ui
      ui <- if (!is.null(override)) modifyList(base_ui, override) else base_ui
      bd_col <- ui$table_border
      list(
        main_title = openxlsx::createStyle(
          fontColour     = ui$title,
          fontSize       = 16,
          textDecoration = "bold",
          halign         = "center",
          valign         = "center"
        ),
        header = openxlsx::createStyle(
          fgFill         = ui$table_header_bg,
          fontColour     = ui$table_header_tx,
          textDecoration = "bold",
          halign         = "center",
          valign         = "center",
          border         = "Bottom",
          borderColour   = bd_col
        ),
        body = openxlsx::createStyle(
          fgFill         = "white",
          fontColour     = ui$text,
          border         = "bottom",
          borderColour   = ui$grid
        ),
        stripe = openxlsx::createStyle(
          fgFill         = ui$table_stripe,
          fontColour     = ui$text,
          border         = "bottom",
          borderColour   = ui$grid
        ),
        t_thick = openxlsx::createStyle(border = "Top", borderStyle = "medium", borderColour = bd_col),
        b_thick = openxlsx::createStyle(border = "Bottom", borderStyle = "medium", borderColour = bd_col),
        l_thick = openxlsx::createStyle(border = "Left", borderStyle = "medium", borderColour = bd_col),
        r_thick = openxlsx::createStyle(border = "Right", borderStyle = "medium", borderColour = bd_col)
      )
    }
  )
)
