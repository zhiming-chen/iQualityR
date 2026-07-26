# =============================================================================
# ThemeConfig: Theme configuration management
# =============================================================================

#' @title Theme Configuration Class
#'
#' @description
#' Manages built-in ggplot2 themes, color palettes, and UI settings for the iQualityR framework.
#' Supports external themes from ggthemes and prism packages.
#'
#' @field config Named list with UI and data palette configuration.
#' @field style_presets Named list of built-in style presets (workbench, tech, academic).
#' @field external_theme_fun Function object for the active external theme, or NULL.
#'
#' @importFrom R6 R6Class
#' @importFrom ggplot2 theme element_line element_rect element_text element_blank
#' @importFrom ggplot2 scale_fill_manual scale_fill_gradientn scale_color_manual scale_color_gradientn
#' @importFrom ggplot2 unit rel margin
#' @export
ThemeConfig <- R6::R6Class(
  "ThemeConfig",
  public = list(
    config = NULL,
    style_presets = NULL,
    external_theme_fun = NULL,

    #' @description Create a new ThemeConfig instance.
    #' @param style Character. Name of a built-in style ("workbench", "tech", "academic"),
    #'   or the name of an external theme, or a function object.
    #' @param ... Additional named configuration values that override the selected preset.
    initialize = function(style = "academic", ...) {
      self$style_presets <- list(
        workbench = list(
          ui = list(
            bg = "#EEF2F6",
            surface = "#FFFFFF",
            surface_soft = "#F7F9FC",
            text = "#182230",
            muted = "#667085",
            title = "#182230",
            grid = "#D8DEE8",
            border = "#D8DEE8",
            primary = "#2563EB",
            success = "#0F766E",
            warning = "#B45309",
            danger = "#B42318",
            table_header_bg = "#182230",
            table_header_tx = "#FFFFFF",
            table_stripe = "#F7F9FC",
            table_border = "#D8DEE8"
          ),
          data = list(
            discrete = c(
              "#2563EB", "#0F766E", "#B45309", "#B42318", "#7C3AED",
              "#0891B2", "#475467", "#65A30D", "#C2410C", "#BE185D"
            ),
            continuous = c("#F7F9FC", "#2563EB")
          )
        ),
        tech = list(
          ui = list(
            bg = "#D4E5EF",
            surface = "#FFFFFF",
            text = "#24292F",
            title = "#051D3B",
            grid = "#E1E4E8",
            border = "#0969DA",
            table_header_bg = "#051D3B",
            table_header_tx = "#FFFFFF",
            table_stripe = "#F1F4F8",
            table_border = "#0969DA"
          ),
          data = list(
            discrete = c(
              "#215AF0", "#22A06B", "#D1242F", "#8250DF", "#E16F07",
              "#00A2AD", "#6FDD8B", "#388BFD", "#F69D50", "#B392F0"
            ),
            continuous = c("#F6F8FA", "#215AF0")
          )
        ),
        academic = list(
          ui = list(
            bg = "#FFFFFF",
            surface = "#FFFFFF",
            text = "#24292F",
            title = "#000000",
            grid = "#F0F0F0",
            border = "#000000",
            table_header_bg = "#F0F0F0",
            table_header_tx = "#000000",
            table_stripe = "#FAFAFA",
            table_border = "#000000"
          ),
          data = list(
            discrete = c(
              "#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE",
              "#AA3377", "#BBBBBB", "#332288", "#117733", "#999933"
            ),
            continuous = c("#F7FBFF", "#084594")
          )
        )
      )
      self$set_theme(style, ...)
      invisible(self)
    },

    #' @description Build a mapping from package name to ggplot2 theme function,
    #'   when external packages such as ggthemes or ggprism are installed.
    get_external_theme_map = function() {
      map <- list()
      if (requireNamespace("ggthemes", quietly = TRUE)) {
        map$economist <- ggthemes::theme_economist
        map$wsj <- ggthemes::theme_wsj
        map$gdocs <- ggthemes::theme_gdocs
        map$tufte <- ggthemes::theme_tufte
        map$few <- ggthemes::theme_few
        map$solarized <- ggthemes::theme_solarized
      }
      if (requireNamespace("ggprism", quietly = TRUE)) {
        map$prism <- ggprism::theme_prism
      }
      map
    },

    #' @description Select and configure a theme preset or external theme.
    #' @param style Character or function. Style name or theme function.
    #' @param ... Additional named values that override the preset configuration.
    set_theme = function(style, ...) {
      external_map <- self$get_external_theme_map()

      if (is.character(style) && style %in% names(external_map)) {
        self$external_theme_fun <- external_map[[style]]
        base_config <- self$style_presets[["academic"]]
      } else if (is.function(style)) {
        self$external_theme_fun <- style
        base_config <- self$style_presets[["academic"]]
      } else if (is.character(style) && style %in% names(self$style_presets)) {
        self$external_theme_fun <- NULL
        base_config <- self$style_presets[[style]]
      } else {
        all_names <- unique(c(names(self$style_presets), names(external_map)))
        stop(
          "Unknown style. Must be one of: ",
          paste(all_names, collapse = ", "),
          ", or a function.",
          call. = FALSE
        )
      }

      user_config <- list(...)
      self$config <- self$merge_config(base_config, user_config)
      invisible(self)
    },

    #' @description Retrieve UI configuration.
    #' @param name Optional character. Name of a specific UI slot to return.
    get_ui = function(name = NULL) {
      if (is.null(name)) {
        return(self$config$ui)
      }
      self$config$ui[[name]]
    },

    #' @description Retrieve data palette.
    #' @param type Character. Either "discrete" or "continuous".
    #' @param custom Optional custom palette to return instead of the preset.
    get_data = function(type = c("discrete", "continuous"), custom = NULL) {
      self$get_pal(type = type, custom = custom)
    },

    #' @description Retrieve a color palette.
    #' @param type Character. Either "discrete" or "continuous".
    #' @param custom Optional custom palette to return instead of the preset.
    get_pal = function(type = c("discrete", "continuous"), custom = NULL) {
      type <- match.arg(type)
      if (!is.null(custom)) {
        return(custom)
      }
      opt_name <- paste0("iqr.custom_", type)
      custom_opt <- getOption(opt_name)
      if (!is.null(custom_opt)) {
        return(custom_opt)
      }
      return(self$config$data[[type]])
    },

    #' @description Merge two configuration lists recursively.
    #' @param base List. Base configuration.
    #' @param overlay List. Values that override or extend the base.
    merge_config = function(base, overlay) {
      for (name in names(overlay)) {
        if (is.list(overlay[[name]]) && !is.null(base[[name]]) && is.list(base[[name]])) {
          base[[name]] <- self$merge_config(base[[name]], overlay[[name]])
        } else {
          base[[name]] <- overlay[[name]]
        }
      }
      base
    }
  )
)

# =============================================================================
# PlotTheme: ggplot2 related functionality
# =============================================================================

#' @title Plot Theme Class
#'
#' @description
#' Provides ggplot2 scales and theme components based on a ThemeConfig object.
#'
#' @field config ThemeConfig object providing colors and layout.
#'
#' @export
PlotTheme <- R6::R6Class(
  "PlotTheme",
  public = list(
    config = NULL,

    #' @description Create a PlotTheme from a ThemeConfig.
    #' @param config ThemeConfig object.
    initialize = function(config) {
      if (!inherits(config, "ThemeConfig")) {
        stop("config must be a ThemeConfig object.")
      }
      self$config <- config
      invisible(self)
    },

    #' @description Build a ggplot2 discrete or continuous fill scale.
    #' @param discrete Logical. If TRUE, discrete; if FALSE, continuous.
    #' @param values Optional custom color vector.
    #' @param ... Additional arguments forwarded to the underlying scale.
    scale_fill_iqr = function(discrete = TRUE, values = NULL, ...) {
      pal <- self$config$get_pal(if (discrete) "discrete" else "continuous", values)
      if (discrete) {
        ggplot2::scale_fill_manual(values = pal, ...)
      } else {
        ggplot2::scale_fill_gradientn(colors = pal, ...)
      }
    },

    #' @description Build a ggplot2 discrete or continuous color scale.
    #' @param discrete Logical. If TRUE, discrete; if FALSE, continuous.
    #' @param values Optional custom color vector.
    #' @param ... Additional arguments forwarded to the underlying scale.
    scale_color_iqr = function(discrete = TRUE, values = NULL, ...) {
      pal <- self$config$get_pal(if (discrete) "discrete" else "continuous", values)
      if (discrete) {
        ggplot2::scale_color_manual(values = pal, ...)
      } else {
        ggplot2::scale_color_gradientn(colors = pal, ...)
      }
    },

    #' @description Build a complete ggplot2 theme following the active ThemeConfig.
    #' @param base_size Numeric. Base font size in points.
    #' @param base_family Character. Base font family name.
    #' @param chinese Logical. If TRUE, try to detect and register a CJK font via systemfonts/sysfonts.
    #' @param horizontal Logical. If TRUE, the horizontal grid lines are omitted (and vice versa).
    #' @param font_family_cn Character vector of candidate CJK font family names for Chinese text rendering.
    #' @param ... Additional theme components added to the final theme.
    theme_iqr = function(base_size = 12,
                         base_family = "sans",
                         chinese = FALSE,
                         horizontal = TRUE,
                         font_family_cn = c("Microsoft YaHei", "SimHei", "Noto Sans SC", "Noto Sans CJK SC"),
                         ...) {
      ui <- self$config$config$ui

      if (chinese) {
        has_font_helpers <- suppressWarnings(
          requireNamespace("systemfonts", quietly = TRUE) &&
          requireNamespace("sysfonts", quietly = TRUE) &&
          requireNamespace("showtext", quietly = TRUE)
        )

        if (has_font_helpers) {
          fonts <- suppressWarnings(systemfonts::system_fonts())
          match_idx <- match(font_family_cn, fonts$family)
          match_idx <- match_idx[!is.na(match_idx)]

          font_path <- if (length(match_idx) > 0) fonts$path[match_idx[1]] else ""
          if (nzchar(font_path) && file.exists(font_path)) {
            try(
              suppressWarnings({
                sysfonts::font_add("iqr_cn", regular = font_path)
                showtext::showtext_auto()
              }), silent = TRUE)
            chosen <- "iqr_cn"
          } else {
            chosen <- base_family
          }
        } else {
          chosen <- base_family
        }
        title_family <- chosen
        main_family <- chosen
      } else {
        title_family <- base_family
        main_family <- base_family
      }

      thm <- ggplot2::theme(
        line = ggplot2::element_line(colour = ui$text, linewidth = ggplot2::rel(0.6)),
        rect = ggplot2::element_rect(fill = ui$bg, colour = NA, linetype = 1),
        text = ggplot2::element_text(colour = ui$text, family = main_family, size = base_size),
        axis.line = ggplot2::element_line(linewidth = ggplot2::rel(0.8)),
        axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), face = "bold"),
        axis.ticks = ggplot2::element_line(),
        axis.ticks.length = ggplot2::unit(-base_size * 0.2, "points"),
        axis.title = ggplot2::element_text(size = ggplot2::rel(1), face = "bold"),
        plot.title = ggplot2::element_text(
          size = ggplot2::rel(1.2), hjust = 0, face = "bold",
          family = title_family, colour = ui$title,
          margin = ggplot2::margin(b = 0.5 * base_size, unit = "pt")
        ),
        plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1), hjust = 0, family = main_family, colour = ui$text),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.position = "top",
        legend.justification = "left",
        legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
        panel.background = ggplot2::element_rect(fill = ui$bg, colour = NA),
        panel.border = ggplot2::element_rect(fill = NA, colour = ui$text),
        panel.grid.major = ggplot2::element_line(colour = ui$grid, linewidth = ggplot2::rel(0.5)),
        panel.grid.minor = ggplot2::element_blank(),
        panel.spacing = ggplot2::unit(1, "lines"),
        strip.background = ggplot2::element_rect(fill = ui$grid, colour = NA),
        strip.text = ggplot2::element_text(size = ggplot2::rel(0.8), face = "bold"),
        plot.margin = ggplot2::unit(c(10, 10, 10, 10), "points"),
        complete = TRUE
      )

      if (horizontal) {
        thm <- thm + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
      } else {
        thm <- thm + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      }

      thm <- thm + ggplot2::theme(...)

      if (!is.null(self$config$external_theme_fun)) {
        thm <- thm + self$config$external_theme_fun()
      }

      thm
    }
  )
)

# =============================================================================
# IqrTheme: Facade class (unified entry point)
# =============================================================================

#' @title iQualityR Theme Class
#'
#' @description
#' Theme management class for iQualityR, providing unified access to theme configuration,
#' plot styling, and export functionality.
#'
#' @field config ThemeConfig object.
#' @field plot PlotTheme object.
#'
#' @export
IqrTheme <- R6::R6Class(
  "IqrTheme",
  public = list(
    config = NULL,
    plot = NULL,

    #' @description Create a new IqrTheme instance.
    #' @param theme_style Character. Theme style (built-in "academic", "tech", or external name/function).
    #' @param ... Additional arguments passed to ThemeConfig$new().
    initialize = function(theme_style = "academic", ...) {
      self$config <- ThemeConfig$new(theme_style, ...)
      self$plot <- PlotTheme$new(self$config)
      invisible(self)
    },

    #' @description Set the active theme (delegated to ThemeConfig).
    #' @param style Character or function.
    #' @param ... Additional arguments passed to ThemeConfig$set_theme().
    set_theme = function(style, ...) {
      self$config$set_theme(style, ...)
      invisible(self)
    },

    #' @description Get color palette (delegated to ThemeConfig).
    #' @param ... Arguments passed to ThemeConfig$get_pal().
    get_pal = function(...) {
      self$config$get_pal(...)
    },

    #' @description Get UI theme colors.
    get_ui_colors = function() {
      self$config$get_ui()
    },

    #' @description Get data colors.
    #' @param type Character. Either "discrete" or "continuous".
    #' @param custom Optional custom color vector to use instead.
    get_data_colors = function(type = c("discrete", "continuous"), custom = NULL) {
      self$config$get_data(type = type, custom = custom)
    },

    #' @description Create ggplot2 fill scale (delegated to PlotTheme).
    #' @param ... Arguments passed to PlotTheme$scale_fill_iqr().
    scale_fill_iqr = function(...) {
      self$plot$scale_fill_iqr(...)
    },

    #' @description Create ggplot2 color scale (delegated to PlotTheme).
    #' @param ... Arguments passed to PlotTheme$scale_color_iqr().
    scale_color_iqr = function(...) {
      self$plot$scale_color_iqr(...)
    },

    #' @description Create ggplot2 theme (delegated to PlotTheme).
    #' @param ... Arguments passed to PlotTheme$theme_iqr().
    theme_iqr = function(...) {
      self$plot$theme_iqr(...)
    }
  )
)
