# =============================================================================
# Color utility functions
# =============================================================================
#
# Lightweight, dependency-free helpers for manipulating hex colors. They are
# used internally by the theme system to derive paired fill/color combinations
# and are exported so downstream packages can build on the same primitives.
#

#' Convert a hex color string to an RGB triplet
#'
#' Internal helper. Accepts "#RRGGBB" or "#RGB" and returns a numeric vector
#' of length 3 in 0-255 range.
#'
#' @param color Character. A hex color string.
#'
#' @return Numeric vector of length 3 (R, G, B) in 0-255 range.
#' @noRd
hex_to_rgb <- function(color) {
  color <- sub("^#", "", trimws(color))
  if (nchar(color) == 3L) {
    color <- paste0(
      substr(color, 1, 1), substr(color, 1, 1),
      substr(color, 2, 2), substr(color, 2, 2),
      substr(color, 3, 3), substr(color, 3, 3)
    )
  }
  if (nchar(color) != 6L) {
    stop("Invalid hex color: #", color, call. = FALSE)
  }
  rgb <- c(
    R = strtoi(substr(color, 1, 2), base = 16L),
    G = strtoi(substr(color, 3, 4), base = 16L),
    B = strtoi(substr(color, 5, 6), base = 16L)
  )
  if (any(is.na(rgb))) {
    stop("Invalid hex color: #", color, call. = FALSE)
  }
  rgb
}

#' Convert an RGB triplet to a hex color string
#'
#' Internal helper. Returns "#RRGGBB".
#'
#' @param rgb Numeric vector of length 3 (R, G, B) in 0-255 range.
#'
#' @return Character scalar like "#1A2B3C".
#' @noRd
rgb_to_hex <- function(rgb) {
  rgb <- pmax(0L, pmin(255L, as.integer(round(rgb))))
  sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
}

#' Lighten a color
#'
#' Mix a color with white by the given amount. Useful for producing soft fill
#' variants of a base color.
#'
#' @param color Character. A hex color string (e.g. "#2563EB").
#' @param amount Numeric between 0 and 1. 0 returns the original color, 1 returns white.
#'
#' @return Character. A hex color string.
#' @export
#'
#' @examples
#' lighten("#2563EB", 0.2)
lighten <- function(color, amount = 0.2) {
  if (!is.numeric(amount) || amount < 0 || amount > 1) {
    stop("amount must be a numeric in [0, 1].", call. = FALSE)
  }
  rgb <- hex_to_rgb(color)
  rgb <- rgb + (255 - rgb) * amount
  rgb_to_hex(rgb)
}

#' Darken a color
#'
#' Mix a color with black by the given amount. Useful for producing edge /
#' outline colors that contrast with a base fill.
#'
#' @param color Character. A hex color string.
#' @param amount Numeric between 0 and 1. 0 returns the original color, 1 returns black.
#'
#' @return Character. A hex color string.
#' @export
#'
#' @examples
#' darken("#2563EB", 0.3)
darken <- function(color, amount = 0.2) {
  if (!is.numeric(amount) || amount < 0 || amount > 1) {
    stop("amount must be a numeric in [0, 1].", call. = FALSE)
  }
  rgb <- hex_to_rgb(color)
  rgb <- rgb * (1 - amount)
  rgb_to_hex(rgb)
}

#' Mix two colors
#'
#' Linearly interpolate between two colors.
#'
#' @param color1 Character. A hex color string.
#' @param color2 Character. A hex color string.
#' @param amount Numeric between 0 and 1. Proportion of \code{color2} in the result.
#'
#' @return Character. A hex color string.
#' @export
#'
#' @examples
#' mix("#FF0000", "#0000FF", 0.5)
mix <- function(color1, color2, amount = 0.5) {
  if (!is.numeric(amount) || amount < 0 || amount > 1) {
    stop("amount must be a numeric in [0, 1].", call. = FALSE)
  }
  rgb1 <- hex_to_rgb(color1)
  rgb2 <- hex_to_rgb(color2)
  rgb_to_hex(rgb1 * (1 - amount) + rgb2 * amount)
}

#' Whether a color is dark
#'
#' Uses the relative luminance definition from WCAG. Returns TRUE if the color
#' is dark enough that white text reads well on it.
#'
#' @param color Character. A hex color string.
#'
#' @return Logical.
#' @export
#'
#' @examples
#' is_dark("#000000")
#' is_dark("#FFFFFF")
is_dark <- function(color) {
  rgb <- hex_to_rgb(color) / 255
  # sRGB to linear luminance, simplified WCAG formula
  lin <- vapply(rgb, function(v) {
    if (v <= 0.03928) v / 12.92 else ((v + 0.055) / 1.055)^2.4
  }, numeric(1))
  luminance <- 0.2126 * lin[1] + 0.7152 * lin[2] + 0.0722 * lin[3]
  luminance < 0.4
}

#' Contrast ratio between two colors
#'
#' WCAG 2.x contrast ratio. Returns a numeric between 1 and 21. A ratio >= 4.5 is
#' the AA threshold for normal text.
#'
#' @param fg Character. Foreground hex color.
#' @param bg Character. Background hex color.
#'
#' @return Numeric scalar.
#' @export
#'
#' @examples
#' contrast_ratio("#FFFFFF", "#000000")
contrast_ratio <- function(fg, bg) {
  lum <- function(color) {
    rgb <- hex_to_rgb(color) / 255
    lin <- vapply(rgb, function(v) {
      if (v <= 0.03928) v / 12.92 else ((v + 0.055) / 1.055)^2.4
    }, numeric(1))
    0.2126 * lin[1] + 0.7152 * lin[2] + 0.0722 * lin[3]
  }
  l1 <- lum(fg)
  l2 <- lum(bg)
  lighter <- max(l1, l2)
  darker <- min(l1, l2)
  (lighter + 0.05) / (darker + 0.05)
}

# =============================================================================
# ThemeConfig: Theme configuration management
# =============================================================================

#' @title Theme Configuration Class
#'
#' @description
#' Manages built-in ggplot2 themes and four classes of color palettes
#' (discrete, sequential, diverging, semantic) for the iQualityR framework.
#' Supports external themes from ggthemes and ggprism packages.
#'
#' Palette types:
#' \describe{
#'   \item{discrete}{Qualitative palette for categorical variables. Auto-extends
#'     beyond its base length via \code{colorRampPalette} when \code{n} is requested.}
#'   \item{sequential}{Single-hue or ordered multi-stop palette for continuous
#'     non-negative magnitudes (heatmaps, contours, density).}
#'   \item{diverging}{Three-stop palette running from a negative hue through a
#'     neutral midpoint to a positive hue (correlations, residuals, effects).}
#'   \item{semantic}{Named colors mapped to domain verdicts (pass / fail /
#'     watch / good / bad / neutral) and traffic-light roles.}
#' }
#'
#' @field config Named list with UI and data palette configuration.
#' @field style_presets Named list of built-in style presets (workbench, tech, academic).
#' @field external_theme_fun Function object for the active external theme, or NULL.
#'
#' @importFrom R6 R6Class
#' @importFrom ggplot2 theme element_line element_rect element_text element_blank
#' @importFrom ggplot2 scale_fill_manual scale_fill_gradientn scale_color_manual scale_color_gradientn
#' @importFrom ggplot2 scale_fill_gradient2 scale_color_gradient2
#' @importFrom grDevices colorRampPalette
#' @importFrom utils modifyList
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
            sequential = c("#F7F9FC", "#93B4DC", "#1D4ED8"),
            diverging = c("#B42318", "#FFFFFF", "#0F766E"),
            semantic = list(
              pass = "#0F766E", fail = "#B42318", watch = "#B45309",
              good = "#0F766E", bad = "#B42318", neutral = "#667085",
              green = "#0F766E", yellow = "#B45309", red = "#B42318"
            )
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
            primary = "#0969DA",
            success = "#1A7F37",
            warning = "#BF8700",
            danger = "#CF222E",
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
            sequential = c("#F6F8FA", "#7DAEF6", "#0A3697"),
            diverging = c("#CF222E", "#FFFFFF", "#1A7F37"),
            semantic = list(
              pass = "#1A7F37", fail = "#CF222E", watch = "#BF8700",
              good = "#1A7F37", bad = "#CF222E", neutral = "#57606A",
              green = "#1A7F37", yellow = "#BF8700", red = "#CF222E"
            )
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
            primary = "#1F77B4",
            success = "#228833",
            warning = "#CCBB44",
            danger = "#EE6677",
            table_header_bg = "#F0F0F0",
            table_header_tx = "#000000",
            table_stripe = "#FAFAFA",
            table_border = "#000000"
          ),
          data = list(
            # Paul Tol's colorblind-safe qualitative palette
            discrete = c(
              "#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE",
              "#AA3377", "#BBBBBB", "#332288", "#117733", "#999933"
            ),
            # ColorBrewer Blues (3-stop, colorblind-safe)
            sequential = c("#F7FBFF", "#6BAED6", "#08306B"),
            # ColorBrewer RdBu (3-stop, colorblind-safe)
            diverging = c("#B2182B", "#F7F7F7", "#2166AC"),
            semantic = list(
              pass = "#228833", fail = "#EE6677", watch = "#CCBB44",
              good = "#228833", bad = "#EE6677", neutral = "#BBBBBB",
              green = "#228833", yellow = "#CCBB44", red = "#EE6677"
            )
          )
        ),
        # ----- External theme presets (full palettes, not just theme funs) -----
        # Each external theme ships its own four-class data palette so that
        # picking "economist" / "wsj" / etc. recolors plots consistently,
        # matching the look-and-feel of the source publication / tool.
        # Semantic colors (pass / fail / watch) reuse each source's green /
        # red / amber trio so verdict semantics stay consistent with the
        # active theme's overall palette.
        economist = list(
          ui = list(
            bg = "#F5F5F2", surface = "#FFFFFF", text = "#1A1A1A",
            title = "#014D64", grid = "#D9D9D9", border = "#014D64",
            primary = "#014D64", success = "#00887D", warning = "#E3A21A",
            danger = "#C72E29", table_header_bg = "#014D64",
            table_header_tx = "#FFFFFF", table_stripe = "#F0F0EC",
            table_border = "#6794A7"
          ),
          data = list(
            discrete = c("#6794A7", "#014D64", "#7AD2F6", "#01A2D9",
                         "#76C0C1", "#00887D", "#E3A21A", "#C72E29",
                         "#BFD3DE", "#8C8C8C"),
            sequential = c("#F0F8FB", "#7AD2F6", "#014D64"),
            diverging  = c("#C72E29", "#F5F5F2", "#014D64"),
            semantic = list(
              pass = "#00887D", fail = "#C72E29", watch = "#E3A21A",
              good = "#00887D", bad = "#C72E29", neutral = "#8C8C8C",
              green = "#00887D", yellow = "#E3A21A", red = "#C72E29"
            )
          )
        ),
        wsj = list(
          ui = list(
            bg = "#FFFFFF", surface = "#FFFFFF", text = "#000000",
            title = "#000000", grid = "#CCCCCC", border = "#000000",
            primary = "#016392", success = "#098154", warning = "#BE9C2E",
            danger = "#C72E29", table_header_bg = "#000000",
            table_header_tx = "#FFFFFF", table_stripe = "#F7F7F7",
            table_border = "#000000"
          ),
          data = list(
            discrete = c("#C72E29", "#016392", "#BE9C2E", "#098154",
                         "#FB832D", "#000000", "#4C4C4C", "#8C8C8C",
                         "#BFD3DE", "#E5E5E5"),
            sequential = c("#FFFFFF", "#7FAFD6", "#016392"),
            diverging  = c("#C72E29", "#FFFFFF", "#016392"),
            semantic = list(
              pass = "#098154", fail = "#C72E29", watch = "#BE9C2E",
              good = "#098154", bad = "#C72E29", neutral = "#8C8C8C",
              green = "#098154", yellow = "#BE9C2E", red = "#C72E29"
            )
          )
        ),
        gdocs = list(
          ui = list(
            bg = "#FFFFFF", surface = "#FFFFFF", text = "#202124",
            title = "#202124", grid = "#E8EAED", border = "#4285F4",
            primary = "#4285F4", success = "#34A853", warning = "#FBBC04",
            danger = "#EA4335", table_header_bg = "#4285F4",
            table_header_tx = "#FFFFFF", table_stripe = "#F8F9FA",
            table_border = "#DADCE0"
          ),
          data = list(
            discrete = c("#4285F4", "#EA4335", "#FBBC04", "#34A853",
                         "#FF6D01", "#46BDC6", "#9AA0A6", "#5F6368",
                         "#E8F0FE", "#FCE8E6"),
            sequential = c("#FFFFFF", "#8AB4F8", "#1A73E8"),
            diverging  = c("#EA4335", "#FFFFFF", "#1A73E8"),
            semantic = list(
              pass = "#34A853", fail = "#EA4335", watch = "#FBBC04",
              good = "#34A853", bad = "#EA4335", neutral = "#9AA0A6",
              green = "#34A853", yellow = "#FBBC04", red = "#EA4335"
            )
          )
        ),
        tufte = list(
          ui = list(
            bg = "#FFFFFF", surface = "#FFFFFF", text = "#000000",
            title = "#000000", grid = "#EEEEEE", border = "#000000",
            primary = "#000000", success = "#595959", warning = "#B3B3B3",
            danger = "#000000", table_header_bg = "#EEEEEE",
            table_header_tx = "#000000", table_stripe = "#FAFAFA",
            table_border = "#000000"
          ),
          data = list(
            # Tufte: minimal ink, grayscale plus subtle accent
            discrete = c("#000000", "#595959", "#8C8C8C", "#B3B3B3",
                         "#D9D9D9", "#4D4D4D", "#737373", "#A6A6A6",
                         "#262626", "#595959"),
            sequential = c("#FFFFFF", "#B3B3B3", "#000000"),
            diverging  = c("#595959", "#FFFFFF", "#000000"),
            semantic = list(
              pass = "#595959", fail = "#000000", watch = "#8C8C8C",
              good = "#595959", bad = "#000000", neutral = "#B3B3B3",
              green = "#595959", yellow = "#8C8C8C", red = "#000000"
            )
          )
        ),
        few = list(
          ui = list(
            bg = "#FFFFFF", surface = "#FFFFFF", text = "#333333",
            title = "#333333", grid = "#E6E6E6", border = "#999999",
            primary = "#5DA5DA", success = "#60BD68", warning = "#FAA43A",
            danger = "#F15854", table_header_bg = "#5DA5DA",
            table_header_tx = "#FFFFFF", table_stripe = "#F7F7F7",
            table_border = "#CCCCCC"
          ),
          data = list(
            discrete = c("#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0",
                         "#B2912F", "#B276B2", "#DECF3F", "#F15854",
                         "#9E9E9E", "#7F8C8D"),
            sequential = c("#FFFFFF", "#A6C8E0", "#2766B2"),
            diverging  = c("#F15854", "#FFFFFF", "#5DA5DA"),
            semantic = list(
              pass = "#60BD68", fail = "#F15854", watch = "#FAA43A",
              good = "#60BD68", bad = "#F15854", neutral = "#9E9E9E",
              green = "#60BD68", yellow = "#FAA43A", red = "#F15854"
            )
          )
        ),
        solarized = list(
          ui = list(
            bg = "#FDF6E3", surface = "#EEE8D5", text = "#073642",
            title = "#073642", grid = "#EEE8D5", border = "#93A1A1",
            primary = "#268BD2", success = "#859900", warning = "#B58900",
            danger = "#DC322F", table_header_bg = "#073642",
            table_header_tx = "#FDF6E3", table_stripe = "#EEE8D5",
            table_border = "#93A1A1"
          ),
          data = list(
            discrete = c("#268BD2", "#B58900", "#CB4B16", "#DC322F",
                         "#D33682", "#6C71C4", "#2AA198", "#859900",
                         "#93A1A1", "#586E75"),
            sequential = c("#FDF6E3", "#8FA9C9", "#1A4B7A"),
            diverging  = c("#DC322F", "#FDF6E3", "#268BD2"),
            semantic = list(
              pass = "#859900", fail = "#DC322F", watch = "#B58900",
              good = "#859900", bad = "#DC322F", neutral = "#93A1A1",
              green = "#859900", yellow = "#B58900", red = "#DC322F"
            )
          )
        ),
        prism = list(
          ui = list(
            bg = "#FFFFFF", surface = "#FFFFFF", text = "#1F1F1F",
            title = "#000000", grid = "#E5E5E5", border = "#000000",
            primary = "#077E97", success = "#056943", warning = "#800080",
            danger = "#C000C0", table_header_bg = "#1F1F1F",
            table_header_tx = "#FFFFFF", table_stripe = "#F7F7F7",
            table_border = "#000000"
          ),
          data = list(
            # ggprism "winter_bright" palette
            discrete = c("#077E97", "#800080", "#000080", "#8D8DFF",
                         "#C000C0", "#056943", "#5DA5DA", "#FAA43A",
                         "#60BD68", "#F17CB0"),
            sequential = c("#FFFFFF", "#7CB5C9", "#077E97"),
            diverging  = c("#C000C0", "#FFFFFF", "#077E97"),
            semantic = list(
              pass = "#056943", fail = "#C000C0", watch = "#800080",
              good = "#056943", bad = "#C000C0", neutral = "#8D8DFF",
              green = "#056943", yellow = "#800080", red = "#C000C0"
            )
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
        map$economist  <- ggthemes::theme_economist
        map$wsj        <- ggthemes::theme_wsj
        map$gdocs      <- ggthemes::theme_gdocs
        map$tufte      <- ggthemes::theme_tufte
        map$few        <- ggthemes::theme_few
        map$solarized  <- ggthemes::theme_solarized
      }
      if (requireNamespace("ggprism", quietly = TRUE)) {
        map$prism      <- ggprism::theme_prism
      }
      map
    },

    #' @description Select and configure a theme preset or external theme.
    #'
    #'   External themes (economist / wsj / gdocs / tufte / few / solarized /
    #'   prism) are now full presets: they carry their own UI colors and
    #'   four-class data palettes, and the external ggplot2 theme function is
    #'   layered on top in \code{theme_iqr()}. When the external package is
    #'   not installed but the preset name is recognized, the preset's color
    #'   configuration is still applied (only the theme function is skipped).
    #'
    #' @param style Character or function. Style name or theme function.
    #' @param ... Additional named values that override the preset configuration.
    set_theme = function(style, ...) {
      external_map <- self$get_external_theme_map()

      if (is.character(style) && style %in% names(external_map)) {
        # External theme available (package installed): use the matching
        # full preset (it carries its own ui + data palette), and also
        # attach the external theme function for theme_iqr() to layer on.
        self$external_theme_fun <- external_map[[style]]
        base_config <- self$style_presets[[style]]
      } else if (is.function(style)) {
        # A user-supplied theme function: no matching preset, fall back to
        # academic so downstream scales still work.
        self$external_theme_fun <- style
        base_config <- self$style_presets[["academic"]]
      } else if (is.character(style) && style %in% names(self$style_presets)) {
        # Built-in preset (academic / workbench / tech) OR an external
        # preset whose package is not installed (we still keep the preset
        # dictionary entry; only the theme function is unavailable).
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

    #' @description Retrieve data palette. Legacy alias for \code{get_pal}.
    #' @param type Character. Either "discrete" or "continuous" (legacy alias
    #'   for "sequential").
    #' @param custom Optional custom palette to return instead of the preset.
    get_data = function(type = c("discrete", "continuous"), custom = NULL) {
      type <- match.arg(type)
      if (type == "continuous") type <- "sequential"
      self$get_pal(type = type, custom = custom)
    },

    #' @description Retrieve a color palette.
    #'
    #' Resolution order: \code{custom} argument, then \code{options("iqr.custom_<type>")},
    #' then preset. For \code{type = "discrete"}, the palette is auto-extended to
    #' \code{n} colors via \code{colorRampPalette} when \code{n} exceeds the
    #' base length. For \code{type = "semantic"}, a single color or named
    #' vector is returned depending on \code{name}.
    #'
    #' @param type Character. One of "discrete", "sequential", "diverging",
    #'   "semantic", or the legacy alias "continuous" (mapped to "sequential").
    #' @param n Integer. Desired length of the discrete palette. Ignored for
    #'   non-discrete types unless explicitly meaningful.
    #' @param name Character. For \code{type = "semantic"}, the name of the
    #'   semantic slot (e.g. "pass", "fail", "watch", "good", "bad", "neutral",
    #'   "green", "yellow", "red"). If NULL, the full named vector is returned.
    #' @param custom Optional custom palette (vector or list) to return instead
    #'   of the preset.
    get_pal = function(type = c("discrete", "sequential", "diverging",
                                "semantic", "continuous"),
                       n = NULL, name = NULL, custom = NULL) {
      type <- match.arg(type)
      if (type == "continuous") type <- "sequential"

      # Resolution order: custom > option > preset
      if (!is.null(custom)) {
        pal <- custom
      } else {
        opt_name <- paste0("iqr.custom_", type)
        opt_val <- getOption(opt_name)
        if (!is.null(opt_val)) {
          pal <- opt_val
        } else {
          pal <- self$config$data[[type]]
        }
      }

      if (type == "discrete") {
        if (!is.null(n) && n > length(pal)) {
          pal <- grDevices::colorRampPalette(pal)(n)
        }
        return(pal)
      }

      if (type == "semantic") {
        if (!is.null(name)) {
          if (!name %in% names(pal)) {
            stop("Unknown semantic color name: ", name,
                 ". Available: ", paste(names(pal), collapse = ", "),
                 call. = FALSE)
          }
          return(pal[[name]])
        }
        return(unlist(pal))
      }

      # sequential, diverging
      pal
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
#' Scales cover four palette types:
#' \itemize{
#'   \item \code{scale_fill_iqr} / \code{scale_color_iqr} -- discrete or
#'     sequential, controlled by \code{discrete} flag. Supports the \code{style}
#'     argument ("same" or "paired") to derive a coordinated fill/color pair.
#'   \item \code{scale_fill_sequential} / \code{scale_color_sequential} --
#'     multi-stop sequential gradient for continuous magnitudes.
#'   \item \code{scale_fill_diverging} / \code{scale_color_diverging} --
#'     three-stop diverging gradient for signed values.
#'   \item \code{scale_fill_semantic} / \code{scale_color_semantic} --
#'     discrete scale bound to the theme's semantic palette (pass / fail / etc.).
#' }
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

    #' @description Build a ggplot2 discrete or sequential fill scale.
    #'
    #'   When \code{discrete = TRUE} and \code{style = "paired"}, the fill
    #'   uses the discrete palette and the matching color scale uses a darkened
    #'   variant of each fill color, so that bar / boxplot edges remain visible.
    #'
    #' @param discrete Logical. If TRUE, use the discrete palette; if FALSE,
    #'   use the sequential palette as a gradient.
    #' @param values Optional custom color vector overriding the preset.
    #' @param style Character. Either "same" (default, backward compatible) or
    #'   "paired" (fill and color are coordinated, requires adding both
    #'   \code{scale_fill_iqr} and \code{scale_color_iqr} with the same style).
    #' @param n Integer. Desired length of the discrete palette. Auto-extends
    #'   via \code{colorRampPalette} when greater than the base length.
    #' @param ... Additional arguments forwarded to the underlying scale.
    scale_fill_iqr = function(discrete = TRUE, values = NULL,
                              style = c("same", "paired"), n = NULL, ...) {
      style <- match.arg(style)
      pal <- self$config$get_pal(
        type = if (discrete) "discrete" else "sequential",
        custom = values, n = if (discrete) n else NULL
      )
      if (discrete) {
        ggplot2::scale_fill_manual(values = pal, ...)
      } else {
        ggplot2::scale_fill_gradientn(colors = pal, ...)
      }
    },

    #' @description Build a ggplot2 discrete or sequential color scale.
    #'
    #'   When \code{style = "paired"}, the color uses a darkened variant of
    #'   the discrete fill palette, so it visually pairs with
    #'   \code{scale_fill_iqr(style = "paired")}.
    #'
    #' @param discrete Logical. If TRUE, use the discrete palette; if FALSE,
    #'   use the sequential palette as a gradient.
    #' @param values Optional custom color vector overriding the preset.
    #' @param style Character. Either "same" (default) or "paired".
    #' @param n Integer. Desired length of the discrete palette.
    #' @param ... Additional arguments forwarded to the underlying scale.
    scale_color_iqr = function(discrete = TRUE, values = NULL,
                               style = c("same", "paired"), n = NULL, ...) {
      style <- match.arg(style)
      if (discrete) {
        pal <- self$config$get_pal(type = "discrete", custom = values, n = n)
        if (style == "paired") {
          pal <- vapply(pal, darken, character(1), amount = 0.25,
                        USE.NAMES = FALSE)
        }
        ggplot2::scale_color_manual(values = pal, ...)
      } else {
        pal <- self$config$get_pal(type = "sequential", custom = values)
        ggplot2::scale_color_gradientn(colors = pal, ...)
      }
    },

    #' @description Build a sequential fill gradient for continuous magnitudes.
    #' @param name Character. Optional scale title.
    #' @param reverse Logical. If TRUE, reverse the palette direction.
    #' @param ... Additional arguments forwarded to \code{scale_fill_gradientn}.
    scale_fill_sequential = function(name = ggplot2::waiver(), reverse = FALSE, ...) {
      pal <- self$config$get_pal("sequential")
      if (reverse) pal <- rev(pal)
      ggplot2::scale_fill_gradientn(colors = pal, name = name, ...)
    },

    #' @description Build a sequential color gradient for continuous magnitudes.
    #' @param name Character. Optional scale title.
    #' @param reverse Logical. If TRUE, reverse the palette direction.
    #' @param ... Additional arguments forwarded to \code{scale_color_gradientn}.
    scale_color_sequential = function(name = ggplot2::waiver(), reverse = FALSE, ...) {
      pal <- self$config$get_pal("sequential")
      if (reverse) pal <- rev(pal)
      ggplot2::scale_color_gradientn(colors = pal, name = name, ...)
    },

    #' @description Build a diverging fill gradient for signed continuous values.
    #' @param name Character. Optional scale title.
    #' @param midpoint Numeric. The value mapped to the palette midpoint
    #'   (default 0).
    #' @param ... Additional arguments forwarded to \code{scale_fill_gradient2}.
    scale_fill_diverging = function(name = ggplot2::waiver(), midpoint = 0, ...) {
      pal <- self$config$get_pal("diverging")
      ggplot2::scale_fill_gradient2(
        low = pal[1], mid = pal[2], high = pal[3],
        midpoint = midpoint, name = name, ...
      )
    },

    #' @description Build a diverging color gradient for signed continuous values.
    #' @param name Character. Optional scale title.
    #' @param midpoint Numeric. The value mapped to the palette midpoint
    #'   (default 0).
    #' @param ... Additional arguments forwarded to \code{scale_color_gradient2}.
    scale_color_diverging = function(name = ggplot2::waiver(), midpoint = 0, ...) {
      pal <- self$config$get_pal("diverging")
      ggplot2::scale_color_gradient2(
        low = pal[1], mid = pal[2], high = pal[3],
        midpoint = midpoint, name = name, ...
      )
    },

    #' @description Build a discrete fill scale bound to the semantic palette.
    #'
    #'   Maps factor levels to semantic colors (pass / fail / watch / etc.).
    #'   Pass \code{labels} to control level order and labelling.
    #'
    #' @param labels Character vector of semantic names to use, in order. Must
    #'   be a subset of names in the semantic palette. Default \code{c("pass", "fail", "watch")}.
    #' @param name Character. Optional scale title.
    #' @param ... Additional arguments forwarded to \code{scale_fill_manual}.
    scale_fill_semantic = function(labels = c("pass", "fail", "watch"),
                                   name = ggplot2::waiver(), ...) {
      pal <- self$config$get_pal("semantic")
      missing <- setdiff(labels, names(pal))
      if (length(missing) > 0) {
        stop("Unknown semantic labels: ",
             paste(missing, collapse = ", "),
             ". Available: ", paste(names(pal), collapse = ", "),
             call. = FALSE)
      }
      ggplot2::scale_fill_manual(values = pal[labels], name = name, ...)
    },

    #' @description Build a discrete color scale bound to the semantic palette.
    #' @param labels Character vector of semantic names to use, in order.
    #' @param name Character. Optional scale title.
    #' @param ... Additional arguments forwarded to \code{scale_color_manual}.
    scale_color_semantic = function(labels = c("pass", "fail", "watch"),
                                    name = ggplot2::waiver(), ...) {
      pal <- self$config$get_pal("semantic")
      missing <- setdiff(labels, names(pal))
      if (length(missing) > 0) {
        stop("Unknown semantic labels: ",
             paste(missing, collapse = ", "),
             ". Available: ", paste(names(pal), collapse = ", "),
             call. = FALSE)
      }
      ggplot2::scale_color_manual(values = pal[labels], name = name, ...)
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
#' Theme management class for iQualityR, providing unified access to theme
#' configuration, plot styling, and export functionality.
#'
#' The facade delegates to \code{ThemeConfig} for palette storage and
#' \code{PlotTheme} for ggplot2 scale / theme construction. Users typically
#' only need to:
#' \enumerate{
#'   \item Construct an \code{IqrTheme} with a preset name
#'     (\code{"academic"}, \code{"workbench"}, \code{"tech"}) or an external
#'     theme name / function.
#'   \item Call \code{$plot$scale_*()} to obtain ggplot2 scales.
#'   \item Call \code{$plot$theme_iqr()} to obtain the ggplot2 theme.
#' }
#'
#' All four palette types (discrete, sequential, diverging, semantic) are
#' coordinated by the chosen theme, so downstream packages and end users do
#' not need to pick colors manually.
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
    #' @param theme_style Character. Theme style (built-in "academic", "tech",
    #'   "workbench", or external name/function).
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

    #' @description Get data colors. Legacy alias for \code{get_pal}.
    #' @param type Character. Either "discrete" or "continuous" (legacy alias
    #'   for "sequential").
    #' @param custom Optional custom color vector to use instead.
    get_data_colors = function(type = c("discrete", "continuous"), custom = NULL) {
      type <- match.arg(type)
      if (type == "continuous") type <- "sequential"
      self$config$get_pal(type = type, custom = custom)
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

    #' @description Create ggplot2 sequential fill scale.
    #' @param ... Arguments passed to PlotTheme$scale_fill_sequential().
    scale_fill_sequential = function(...) {
      self$plot$scale_fill_sequential(...)
    },

    #' @description Create ggplot2 sequential color scale.
    #' @param ... Arguments passed to PlotTheme$scale_color_sequential().
    scale_color_sequential = function(...) {
      self$plot$scale_color_sequential(...)
    },

    #' @description Create ggplot2 diverging fill scale.
    #' @param ... Arguments passed to PlotTheme$scale_fill_diverging().
    scale_fill_diverging = function(...) {
      self$plot$scale_fill_diverging(...)
    },

    #' @description Create ggplot2 diverging color scale.
    #' @param ... Arguments passed to PlotTheme$scale_color_diverging().
    scale_color_diverging = function(...) {
      self$plot$scale_color_diverging(...)
    },

    #' @description Create ggplot2 semantic fill scale.
    #' @param ... Arguments passed to PlotTheme$scale_fill_semantic().
    scale_fill_semantic = function(...) {
      self$plot$scale_fill_semantic(...)
    },

    #' @description Create ggplot2 semantic color scale.
    #' @param ... Arguments passed to PlotTheme$scale_color_semantic().
    scale_color_semantic = function(...) {
      self$plot$scale_color_semantic(...)
    },

    #' @description Create ggplot2 theme (delegated to PlotTheme).
    #' @param ... Arguments passed to PlotTheme$theme_iqr().
    theme_iqr = function(...) {
      self$plot$theme_iqr(...)
    }
  )
)
