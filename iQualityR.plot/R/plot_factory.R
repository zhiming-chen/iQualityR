#' @title Plot Factory Functions
#' @description A collection of modular and elegant ggplot2 plotting functions
#'   that are consistent with the iQualityR ecosystem.
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_line geom_col
#' @importFrom ggplot2 geom_histogram geom_density geom_boxplot geom_jitter
#' @importFrom ggplot2 geom_violin geom_vline geom_errorbar geom_smooth
#' @importFrom ggplot2 geom_segment geom_hline geom_tile geom_text geom_ribbon
#' @importFrom ggplot2 stat_qq stat_qq_line stat_density_2d geom_bin2d geom_hex
#' @importFrom ggplot2 scale_color_manual scale_fill_manual scale_fill_gradient2
#' @importFrom ggplot2 scale_color_brewer scale_fill_brewer scale_size
#' @importFrom ggplot2 scale_x_discrete scale_y_continuous sec_axis
#' @importFrom ggplot2 coord_fixed coord_flip coord_cartesian
#' @importFrom ggplot2 labs theme element_text element_blank element_rect
#' @importFrom ggplot2 element_line rel margin annotate arrow
#' @importFrom ggplot2 after_stat facet_wrap
#' @importFrom stats na.omit acf pacf cor ks.test quantile qnorm pnorm
#' @importFrom stats qexp pexp qweibull pweibull qunif punif dt pt qt
#' @importFrom stats sd runif density median lag na.pass rstandard
#' @importFrom utils modifyList
#' @importFrom dplyr group_by summarise mutate arrange desc ungroup group_modify
#' @importFrom dplyr `%>%`
#' @importFrom scales percent
#' @importFrom patchwork wrap_plots plot_layout plot_annotation
#' @importFrom tidyr pivot_longer
#' @importFrom grid unit
#' @importFrom rlang .data
#' @importFrom iQualityR.core IqrTheme
#' @importFrom iQualityR.core IqrPlotterBase
#' @name iQualityR.plot-package
#' @keywords internal
NULL

# Shared IqrPlotterBase instance used as a stateless color/scale toolbox by
# all plot_* functions. Keeps the function-style public API unchanged while
# routing every color decision through the unified IqrPlotterBase helpers
# (.pal_*, .scale_*, .contrast_text). The instance is created at load time.
.iqr_plotter <- IqrPlotterBase$new()

# Internal aesthetic toolbox: resolves a theme spec (NULL / string / IqrTheme)
# once and returns a named list of all commonly-used semantic role colors.
# Use this instead of calling .iqr_plotter$.pal_ui(...) / .pal_semantic(...)
# repeatedly at the top of every plot_* function. Centralizing the role
# mapping here guarantees that "muted"/"fail"/"data"/... always resolve to
# the same slot across the whole .plot subpackage.
#
# Usage:
#   c <- .iqr_aes(theme)
#   geom_line(color = c$muted)
#   geom_ribbon(fill = c$fail, alpha = 0.25)
#   c$theme_obj$plot$scale_fill_iqr(style = "paired")   # for advanced scales
.iqr_aes <- function(theme = NULL) {
  theme_obj <- as_iqr_theme_object(theme)
  list(
    # Primary data color (first discrete palette entry)
    data         = .iqr_plotter$.pal_discrete(theme_obj)[1],
    # UI semantic slots (with safe defaults for external themes missing slots)
    muted        = .iqr_plotter$.pal_ui(theme_obj, "muted",        default = "#666666"),
    text         = .iqr_plotter$.pal_ui(theme_obj, "text",         default = "#000000"),
    primary      = .iqr_plotter$.pal_ui(theme_obj, "primary",      default = "#2563EB"),
    success      = .iqr_plotter$.pal_ui(theme_obj, "success",      default = "#0F766E"),
    warning      = .iqr_plotter$.pal_ui(theme_obj, "warning",      default = "#B45309"),
    danger       = .iqr_plotter$.pal_ui(theme_obj, "danger",       default = "#B42318"),
    # Surface colors (for tiles, inner boxplot fill, point centers, etc.)
    surface      = .iqr_plotter$.pal_ui(theme_obj, "surface",      default = "#FFFFFF"),
    surface_soft = .iqr_plotter$.pal_ui(theme_obj, "surface_soft", default = "#F7F9FC"),
    bg           = .iqr_plotter$.pal_ui(theme_obj, "bg",           default = "#FFFFFF"),
    grid         = .iqr_plotter$.pal_ui(theme_obj, "grid",         default = "#D8DEE8"),
    border       = .iqr_plotter$.pal_ui(theme_obj, "border",       default = "#D8DEE8"),
    # Semantic palette (pass / fail / watch / neutral / ...)
    pass         = .iqr_plotter$.pal_semantic(theme_obj, "pass"),
    fail         = .iqr_plotter$.pal_semantic(theme_obj, "fail"),
    watch        = .iqr_plotter$.pal_semantic(theme_obj, "watch"),
    neutral      = .iqr_plotter$.pal_semantic(theme_obj, "neutral"),
    good         = .iqr_plotter$.pal_semantic(theme_obj, "good"),
    bad          = .iqr_plotter$.pal_semantic(theme_obj, "bad"),
    # Expose theme_obj so callers can access advanced APIs
    # (e.g. c$theme_obj$plot$scale_fill_iqr(style = "paired"))
    theme_obj    = theme_obj
  )
}

# Declare ggplot2 aes() column names used via non-standard evaluation.
# These variables are never assigned in R code; they are evaluated inside
# aes() against a data frame at plot-build time.
utils::globalVariables(c(
  ".data", "Var1", "Var2", "angle", "category", "cl", "contrast", "count",
  "cum_pct", "empirical", "estimate", "fpr", "group",
  "importance", "index", "label", "label_pos", "lcl", "letter", "level",
  "line_width", "lower", "pct", "residual", "response", "se", "significant",
  "sqrt_res", "stat_name", "tpr", "ucl", "upper", "value", "variance_percent",
  "x", "xend", "y", "yend"
))

#' Convert theme specification to a ggplot2 theme object
#'
#' @param theme A character string (theme name), a function (returning a ggplot2 theme),
#'   an IqrTheme object, or NULL.
#' @return A ggplot2 theme object (can be directly added with `+`).
#' @export
as_iqr_theme <- function(theme = NULL) {
  as_iqr_theme_object(theme)$theme_iqr()
}

#' Convert a Theme Specification to an IqrTheme Object
#'
#' Resolves a theme specification (NULL, character string, function, or
#' IqrTheme object) into a concrete IqrTheme object. When NULL, falls back
#' to the global \code{iqr.default_theme} option (default "academic").
#'
#' @param theme NULL, a theme name string (e.g. "academic"), a theme
#'   generation function, or an IqrTheme object.
#' @return An IqrTheme object.
#' @export
as_iqr_theme_object <- function(theme = NULL) {
  if (is.null(theme)) {
    theme <- getOption("iqr.default_theme", "academic")
  }

  if (is.character(theme)) {
    return(iQualityR.core::IqrTheme$new(theme_style = theme))
  }

  if (inherits(theme, "IqrTheme")) {
    return(theme)
  }

  if (is.function(theme)) {
    return(iQualityR.core::IqrTheme$new(theme_style = theme))
  }

  stop("theme must be NULL, a character string, a function, or an IqrTheme object", call. = FALSE)
}

#' Create a Base ggplot Object with iQualityR Theme
#'
#' Creates the base ggplot object with the iQualityR theme applied and
#' automatically injects theme-aware scales for any \code{fill} / \code{color}
#' (or \code{colour}) aesthetic present in \code{mapping}. This means that
#' whenever the user maps a fill or color to a variable, the resulting plot
#' follows the active theme palette without any extra code.
#'
#' \strong{Auto-injected scales} (based on column type):
#' \itemize{
#'   \item discrete column  -> \code{theme_obj$plot$scale_fill_iqr(discrete=TRUE)}
#'   \item continuous column -> \code{theme_obj$plot$scale_fill_sequential()}
#' }
#'
#' \strong{Scales that cannot be auto-inferred} (attach explicitly using the
#' IqrTheme object retrieved via \code{attr(p, "iqr_theme")}):
#' \itemize{
#'   \item paired mode (fill + darkened color, for boxplots/bars) ->
#'         \code{theme_obj$plot$scale_fill_iqr(discrete=TRUE, style="paired")}
#'         + \code{theme_obj$plot$scale_color_iqr(discrete=TRUE, style="paired")}
#'   \item diverging gradient (for residuals, correlations) ->
#'         \code{theme_obj$plot$scale_fill_diverging(midpoint=0)}
#'   \item semantic palette (pass / fail / watch) ->
#'         \code{theme_obj$plot$scale_fill_semantic(labels=c("pass","fail","watch"))}
#' }
#'
#' The resolved \code{IqrTheme} object is stashed as attribute
#' \code{"iqr_theme"} on the returned ggplot object so downstream code can
#' retrieve it via \code{attr(p, "iqr_theme")} and use the full
#' \code{theme_obj$plot$*} / \code{theme_obj$config$*} API directly.
#'
#' @param data A data.frame for plotting.
#' @param mapping An aesthetic mapping created by \code{aes()}.
#' @param theme Theme to use: NULL (global default), character string
#'   (e.g. "prism"), theme function, or IqrTheme object.
#' @return A \code{ggplot} object with theme and (if applicable) fill/color
#'   scales already attached.
#' @export
base_plot <- function(data, mapping, theme = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This function requires the 'ggplot2' package.")
  }

  stopifnot(is.data.frame(data))

  # Resolve to a full IqrTheme object (handles NULL / string / function / IqrTheme)
  theme_obj <- as_iqr_theme_object(theme)

  # Build the base ggplot with the theme applied. We do the `+` FIRST, then
  # attach the theme object as an attribute, because ggplot2's `+` operator
  # rebuilds the object and would drop attributes added before the `+`.
  p <- ggplot2::ggplot(data, mapping) + theme_obj$plot$theme_iqr()

  # Stash the IqrTheme object so downstream code can retrieve it via
  # `attr(p, "iqr_theme")` and use theme_obj$plot$* / theme_obj$config$*
  # directly (paired, diverging, semantic scales, palette accessors, etc.).
  # Verified to survive subsequent `+ geom_*()` additions on ggplot2 4.5+ (S7).
  attr(p, "iqr_theme") <- theme_obj

  # --- Auto-inject BASIC scales for mapped fill / color aesthetics ---
  #
  # We only auto-inject the scales that can be unambiguously inferred from
  # the column type:
  #   - discrete column  -> discrete scale
  #   - continuous column -> sequential gradient
  #
  # Scales that require user intent (paired, diverging, semantic) are NOT
  # auto-injected. The caller retrieves the IqrTheme object via
  # `attr(p, "iqr_theme")` and attaches them explicitly using the .core API
  # (theme_obj$plot$scale_*), exactly as shown in the .core vignettes.
  #
  # If the mapped value is a computed variable (e.g. after_stat(count)) it
  # will not be a column name in `data`; we treat such cases as continuous.

  mapped <- names(mapping)

  if ("fill" %in% mapped) {
    p <- p + .build_auto_scale(theme_obj, mapping$fill, data, "fill")
  }

  if ("color" %in% mapped) {
    p <- p + .build_auto_scale(theme_obj, mapping$color, data, "color")
  } else if ("colour" %in% mapped) {
    p <- p + .build_auto_scale(theme_obj, mapping$colour, data, "color")
  }

  p
}

#' Pick a theme-aware scale based on the mapped aesthetic
#'
#' Internal helper. Given a quosure-style aesthetic value (from
#' \code{mapping$fill} / \code{mapping$color}), determine whether it refers
#' to a discrete or continuous column in \code{data} and return the
#' appropriate ggplot2 scale object.
#'
#' @param theme_obj IqrTheme object.
#' @param quo An aesthetic quosure (e.g. \code{mapping$fill}).
#' @param data The data frame passed to \code{base_plot()}.
#' @param aes_type Character: "fill" or "color".
#' @return A ggplot2 scale object.
#' @noRd
.build_auto_scale <- function(theme_obj, quo, data, aes_type) {
  col_name <- rlang::quo_name(quo)

  # If the mapped value is a column in data, use its type to pick the scale.
  # Otherwise (computed aesthetics like after_stat(count), expressions, etc.)
  # default to sequential/continuous.
  is_discrete <- if (col_name %in% names(data)) {
    !is.numeric(data[[col_name]])
  } else {
    FALSE
  }

  if (aes_type == "fill") {
    if (is_discrete) {
      .iqr_plotter$.scale_fill_discrete(theme_obj)
    } else {
      .iqr_plotter$.scale_fill_sequential(theme_obj)
    }
  } else {
    if (is_discrete) {
      .iqr_plotter$.scale_color_discrete(theme_obj)
    } else {
      .iqr_plotter$.scale_color_sequential(theme_obj)
    }
  }
}


#' PP Plot with Goodness-of-Fit Test (Console Output)
#'
#' @param data Data frame.
#' @param sample_col Name of the column containing the sample data.
#' @param theme Optional theme object from `as_iqr_theme()`.
#' @param distribution Distribution function for CDF (e.g., `pnorm`, `punif`).
#'        Ignored if `dist_family` is specified.
#' @param dist_params List of parameters for the distribution (e.g., `list(mean=0, sd=1)`).
#' @param dist_family Shortcut for common distributions: "none", "norm", "unif", "exp", "weibull".
#'        Overrides `distribution` if provided.
#' @param add_test If `TRUE`, prints Kolmogorov-Smirnov test results to the console.
#' @param ... Additional arguments passed to `ggplot2::labs()`.
#'
#' @return A ggplot2 object.
#' @export
plot_pp <- function(data, sample_col, theme = NULL,
                    distribution = pnorm,
                    dist_params = list(),
                    dist_family = c("none", "norm", "unif", "exp", "weibull"),
                    add_test = TRUE,
                    ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This function requires the 'ggplot2' package.")
  }

  # ----- 1. Handle missing values and sort -----
  x <- data[[sample_col]]
  if (is.null(x)) stop("Column '", sample_col, "' not found in data.")
  x <- sort(na.omit(x))
  n <- length(x)
  if (n == 0) stop("No non-missing values in column '", sample_col, "'.")

  # ----- 2. Determine distribution function and parameters -----
  dist_family <- match.arg(dist_family)
  if (dist_family != "none") {
    dist_func <- switch(dist_family,
      norm = pnorm,
      unif = punif,
      exp = pexp,
      weibull = pweibull
    )
    # Provide reasonable default params for common distributions (if user did not provide)
    default_params <- switch(dist_family,
      norm    = list(mean = mean(x), sd = sd(x)),
      unif    = list(min = min(x), max = max(x)),
      exp     = list(rate = 1 / mean(x)),
      weibull = list(shape = 1, scale = mean(x))
    )
    dist_params <- utils::modifyList(default_params, dist_params)
  } else {
    if (!is.function(distribution)) {
      stop("`distribution` must be a function (e.g., pnorm, punif).")
    }
    dist_func <- distribution
  }

  # ----- 3. Compute theoretical cumulative probabilities -----
  theoretical <- do.call(dist_func, c(list(x), dist_params))

  # ----- 4. Build data frame -----
  df <- data.frame(
    empirical = (1:n - 0.5) / n,
    theoretical = theoretical
  )

  # ----- 5. Goodness-of-fit test (Kolmogorov-Smirnov) and prepare annotation text -----
  test_label <- NULL
  if (add_test) {
    tryCatch(
      {
        ks_result <- do.call(stats::ks.test, c(list(x = x, y = dist_func), dist_params))
        # Format p-value
        p_val <- ks_result$p.value
        p_formatted <- ifelse(p_val < 0.001, "<0.001", sprintf("%.4f", p_val))
        # Get distribution name
        dist_name <- if (dist_family != "none") dist_family else deparse(substitute(distribution))
        # Note whether params are estimated
        est_note <- ""
        if (dist_family != "none" &&
          ((dist_family == "norm" && identical(dist_params, list(mean = mean(x), sd = sd(x)))) ||
            (dist_family == "exp" && identical(dist_params, list(rate = 1 / mean(x)))) ||
            (dist_family == "weibull" && identical(dist_params, list(shape = 1, scale = mean(x)))))) {
          est_note <- " (params est.)"
        }
        test_label <- sprintf("KS test: D = %.3f, p = %s%s", ks_result$statistic, p_formatted, est_note)
      },
      error = function(e) {
        test_label <- "KS test failed"
      }
    )
  }

  # ----- 6. Plot (add test annotation) -----
  c <- .iqr_aes(theme)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = empirical)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = c$fail) +
    ggplot2::coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Theoretical Cumulative Probability",
      y = "Empirical Cumulative Probability", ...
    ) +
    as_iqr_theme(theme)

  # Add test annotation (placed at bottom-right to avoid overlap)
  if (!is.null(test_label)) {
    p <- p + ggplot2::annotate("text",
      y = 0.95, x = 0.02,
      label = test_label,
      hjust = 0, vjust = 0.5,
      size = 4, color = c$muted
    )
  }

  return(p)
}


#' Quantile-Quantile (QQ) Plot with Goodness-of-Fit Test
#'
#' @param data Data frame.
#' @param sample_col Name of the column containing the sample data.
#' @param theme Optional theme object from `as_iqr_theme()`.
#' @param distribution Distribution function for quantiles (e.g., `qnorm`, `qunif`).
#'        Ignored if `dist_family` is specified.
#' @param dist_params List of parameters for the distribution (e.g., `list(mean=0, sd=1)`).
#' @param dist_family Shortcut for common distributions: "none", "norm", "unif", "exp", "weibull".
#'        Overrides `distribution` if provided.
#' @param qq_line Type of reference line: "quartiles" (line through Q1-Q3, default) or
#'        "identity" (y = x, only meaningful when distribution parameters are known).
#' @param add_confidence If `TRUE`, adds a simulated confidence band (default `FALSE`).
#' @param conf_level Confidence level for the band (default 0.95).
#' @param n_sim Number of simulations for the confidence band (default 1000).
#' @param add_test If `TRUE`, adds goodness-of-fit test annotation on the plot.
#' @param test_method Character: "ks" (Kolmogorov-Smirnov, general) or "shapiro" (only for normal).
#' @param ... Additional arguments passed to `ggplot2::labs()`.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_qq(mtcars, "mpg", dist_family = "norm")
#' plot_qq(mtcars, "mpg", distribution = qexp, dist_params = list(rate = 0.1))
#' plot_qq(mtcars, "mpg", dist_family = "norm", add_confidence = TRUE)
#' }
plot_qq <- function(data, sample_col, theme = NULL,
                    distribution = qnorm,
                    dist_params = list(),
                    dist_family = c("none", "norm", "unif", "exp", "weibull"),
                    qq_line = c("quartiles", "identity"),
                    add_confidence = FALSE,
                    conf_level = 0.95,
                    n_sim = 1000,
                    add_test = TRUE,
                    test_method = c("auto", "ad", "ks"),
                    ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This function requires the 'ggplot2' package.")
  }

  # ----- 1. Data preprocessing -----
  x <- data[[sample_col]]
  if (is.null(x)) stop("Column '", sample_col, "' not found.")
  x <- sort(na.omit(x))
  n <- length(x)
  if (n < 2) stop("Need at least 2 non-missing values.")

  # ----- 2. Determine distribution -----
  dist_family <- match.arg(dist_family)
  qq_line <- match.arg(qq_line)
  test_method <- match.arg(test_method)


  if (dist_family != "none") {
    qfunc <- switch(dist_family,
      norm    = qnorm,
      unif    = qunif,
      exp     = qexp,
      weibull = qweibull
    )
    default_params <- switch(dist_family,
      norm    = list(mean = mean(x), sd = sd(x)),
      unif    = list(min = min(x), max = max(x)),
      exp     = list(rate = 1 / mean(x)),
      weibull = list(shape = 1, scale = mean(x))
    )
    dist_params <- utils::modifyList(default_params, dist_params)
  } else {
    if (!is.function(distribution)) stop("distribution must be a quantile function.")
    qfunc <- distribution
  }

  # ----- 3. Compute theoretical and sample quantiles -----
  probs <- (1:n - 0.5) / n
  theoretical <- do.call(qfunc, c(list(p = probs), dist_params))
  sample_quantiles <- x
  df <- data.frame(theoretical = theoretical, sample = sample_quantiles)

  # ----- Goodness-of-fit test (prefer AD, fall back to KS) -----
  test_label <- NULL
  if (add_test) {
    tryCatch(
      {
        # Determine test method to use
        use_ad <- FALSE
        if (test_method == "ad") {
          if (dist_family == "norm") {
            if (requireNamespace("nortest", quietly = TRUE)) {
              use_ad <- TRUE
            } else {
              warning("nortest package required for AD test. Falling back to KS test.")
            }
          } else {
            warning("AD test is only supported for normal distribution. Falling back to KS test.")
          }
        } else if (test_method == "ks") {
          use_ad <- FALSE
        } else {
          # Default: prefer AD for normal, else KS
          if (dist_family == "norm" && requireNamespace("nortest", quietly = TRUE)) {
            use_ad <- TRUE
          } else {
            use_ad <- FALSE
          }
        }

        if (use_ad) {
          # Anderson-Darling test (normal only)
          ad_result <- nortest::ad.test(x)
          p_val <- ad_result$p.value
          p_formatted <- ifelse(p_val < 0.001, "<0.001", sprintf("%.4f", p_val))
          test_label <- sprintf("AD test: A = %.3f, p = %s", ad_result$statistic, p_formatted)
        } else {
          # Kolmogorov-Smirnov test (general)
          if (dist_family != "none") {
            cfunc <- switch(dist_family,
              norm = pnorm,
              unif = punif,
              exp = pexp,
              weibull = pweibull
            )
          } else {
            # Try to match common distributions
            if (identical(distribution, qnorm)) {
              cfunc <- pnorm
            } else if (identical(distribution, qunif)) {
              cfunc <- punif
            } else if (identical(distribution, qexp)) {
              cfunc <- pexp
            } else if (identical(distribution, qweibull)) {
              cfunc <- pweibull
            } else {
              stop("KS test requires a known distribution (use dist_family).")
            }
          }
          ks_result <- do.call(stats::ks.test, c(list(x = x, y = cfunc, exact = FALSE), dist_params))
          p_val <- ks_result$p.value
          p_formatted <- ifelse(p_val < 0.001, "<0.001", sprintf("%.4f", p_val))
          test_label <- sprintf("KS test: D = %.3f, p = %s", ks_result$statistic, p_formatted)
        }

        # Parameter estimation marker
        if (dist_family != "none" &&
          ((dist_family == "norm" && identical(dist_params, list(mean = mean(x), sd = sd(x)))) ||
            (dist_family == "exp" && identical(dist_params, list(rate = 1 / mean(x)))) ||
            (dist_family == "weibull" && identical(dist_params, list(shape = 1, scale = mean(x)))))) {
          test_label <- paste0(test_label, " (params est.)")
        }
      },
      error = function(e) {
        test_label <- "Test failed"
      }
    )
  }

  # ----- 5. Confidence band (simulation) -----
  band_df <- NULL
  if (add_confidence && n >= 5) {
    sim_quants <- replicate(n_sim, sort(do.call(qfunc, c(list(p = runif(n)), dist_params))))
    # sim_quants is an n x n_sim matrix; each column is a simulated sample's quantiles
    lower <- apply(sim_quants, 1, quantile, probs = (1 - conf_level) / 2)
    upper <- apply(sim_quants, 1, quantile, probs = 1 - (1 - conf_level) / 2)
    band_df <- data.frame(theoretical = theoretical, lower = lower, upper = upper)
  }

  # ----- 6. Reference line -----
  if (qq_line == "identity") {
    line_intercept <- 0
    line_slope <- 1
  } else {
    q_x <- stats::quantile(theoretical, c(0.25, 0.75), na.rm = TRUE)
    q_y <- stats::quantile(sample_quantiles, c(0.25, 0.75), na.rm = TRUE)
    line_slope <- diff(q_y) / diff(q_x)
    line_intercept <- q_y[1] - line_slope * q_x[1]
  }

  # ----- 7. Build plot (add step by step to avoid conflicts) -----
  c <- .iqr_aes(theme)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(
      intercept = line_intercept, slope = line_slope,
      linetype = "dashed", color = c$fail
    ) +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles", ...)

  # Add confidence band (key: use new data frame and explicitly set inherit.aes = FALSE)
  if (!is.null(band_df)) {
    p <- p + ggplot2::geom_ribbon(
      data = band_df,
      ggplot2::aes(x = theoretical, ymin = lower, ymax = upper),
      alpha = 0.2, fill = c$muted,
      inherit.aes = FALSE
    )
  }

  # Add test annotation
  if (!is.null(test_label)) {
    x_range <- range(df$theoretical, na.rm = TRUE)
    y_range <- range(df$sample, na.rm = TRUE)
    p <- p + ggplot2::annotate("text",
      x = x_range[1] + 0.01 * diff(x_range),
      y = y_range[2] - 0.01 * diff(y_range),
      label = test_label,
      hjust = 0, vjust = 1, size = 4, color = c$muted
    )
  }

  # 8. Theme handling (simplified)
  if (is.null(theme)) {
    p <- p + as_iqr_theme()
  } else {
    tryCatch(
      {
        p <- p + as_iqr_theme(theme)
      },
      error = function(e) {
        warning("Invalid theme specification, using default theme. Error: ", e$message)
        p <<- p + as_iqr_theme()
      }
    )
  }

  p
}


# ============================================================================
# 1. Interaction plot (improved)
# ============================================================================

#' Create an Interaction Plot
#'
#' @param data data.frame
#' @param x_var name of x-axis variable (factor or numeric)
#' @param y_var name of y-axis variable (continuous)
#' @param group_var name of grouping variable (factor)
#' @param fun aggregation function, "mean" or "median"
#' @param theme theme specification
#' @return ggplot object
#' @export
plot_interaction_line <- function(data, x_var, y_var, group_var,
                                  fun = c("mean", "median"),
                                  theme = NULL) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("'dplyr' is required for interaction plots.")
  }
  # Input validation
  required <- c(x_var, y_var, group_var)
  missing_cols <- required[!required %in% names(data)]
  if (length(missing_cols) > 0) {
    stop("Columns missing: ", paste(missing_cols, collapse = ", "))
  }

  fun <- match.arg(fun)
  agg_fun <- switch(fun,
    mean = mean,
    median = median
  )

  summary_data <- data %>%
    dplyr::group_by(.data[[x_var]], .data[[group_var]]) %>%
    dplyr::summarise(
      response = agg_fun(.data[[y_var]], na.rm = TRUE),
      .groups = "drop"
    )

  # base_plot already auto-injects the discrete color scale for the mapped
  # group_var, so no explicit scale_*_iqr() is needed here.
  base_plot(summary_data,
    ggplot2::aes(
      x = .data[[x_var]], y = response,
      color = .data[[group_var]], group = .data[[group_var]]
    ),
    theme = theme
  ) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(y = paste(fun, "of", y_var))
}

#' Create a Correlation Heatmap
# ============================================================================
# 2. Correlation heatmap (use tidyr, add optional params)
# ============================================================================
# IqrTheme$new(theme)$scale_color_iqr(discrete = TRUE)
#' Create a Correlation Heatmap
#'
#' @param data data.frame
#' @param theme theme specification
#' @param digits number of digits for correlation labels
#' @param diagonal logical, whether to show diagonal (default FALSE, set to NA)
#' @return ggplot object
#' @export
plot_correlation_heatmap <- function(data, theme = NULL,
                                     digits = 2, diagonal = FALSE) {
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("'tidyr' package is required for heatmaps.")
  }

  numeric_data <- data[sapply(data, is.numeric)]
  # Remove all-missing columns
  numeric_data <- numeric_data[, colSums(is.na(numeric_data)) < nrow(numeric_data)]
  if (ncol(numeric_data) < 2) {
    stop("Heatmap requires at least two numeric columns.")
  }

  cormat <- round(stats::cor(numeric_data, use = "complete.obs"), digits)
  if (!diagonal) diag(cormat) <- NA

  # Use tidyr instead of reshape2
  melted_cormat <- as.data.frame(as.table(cormat))
  names(melted_cormat) <- c("Var1", "Var2", "value")
  melted_cormat <- stats::na.omit(melted_cormat)

  c <- .iqr_aes(theme)
  # base_plot auto-injects a sequential fill scale for the continuous `value`
  # column. Correlations are signed in [-1, 1], so we deliberately override
  # with the diverging scale (midpoint = 0) below.
  p <- base_plot(melted_cormat,
    ggplot2::aes(x = Var1, y = Var2, fill = value),
    theme = theme
  ) +
    ggplot2::geom_tile(color = c$surface) +
    ggplot2::geom_text(ggplot2::aes(label = value), color = c$text, size = 3) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(fill = "Correlation") +
    c$theme_obj$plot$scale_fill_diverging(midpoint = 0)
  p
}


# ============================================================================
# 3. ACF / PACF plots (add significance markers and missing value handling)
# ============================================================================

#' Create an ACF Plot
#'
#' @param data_vec numeric vector
#' @param theme theme specification
#' @param lag.max maximum lag, default NULL (auto)
#' @param ci confidence level, default 0.95
#' @param highlight_sig logical, highlight bars exceeding CI
#' @return ggplot object
#' @export
plot_acf <- function(data_vec,
                     theme = NULL,
                     lag.max = NULL,
                     ci = 0.95,
                     highlight_sig = TRUE) {
  data_vec <- stats::na.omit(data_vec)
  n <- length(data_vec)
  if (n < 3) stop("Need at least 3 observations")

  acf_data <- stats::acf(data_vec,
    lag.max = lag.max,
    plot = FALSE,
    na.action = na.pass
  )
  acf_df <- data.frame(
    lag = acf_data$lag,
    acf = acf_data$acf
  )

  ci_val <- qnorm((1 + ci) / 2) / sqrt(n)

  c <- .iqr_aes(theme)
  p <- base_plot(acf_df,
    ggplot2::aes(
      x = lag,
      y = acf
    ),
    theme = theme
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "solid",
      color = c$muted
    ) +
    ggplot2::geom_hline(
      yintercept = c(ci_val, -ci_val),
      linetype = "dashed",
      color = c$fail
    )

  if (highlight_sig) {
    p <- p + ggplot2::geom_segment(
      ggplot2::aes(xend = lag, yend = 0, color = abs(acf) > ci_val)
    ) + ggplot2::scale_color_manual(
      values = c(
        "FALSE" = c$text,
        "TRUE" = c$fail
      ),
      guide = "none"
    )
  } else {
    p <- p +
      ggplot2::geom_segment(ggplot2::aes(
        xend = lag,
        yend = 0
      ))
  }
  p + ggplot2::geom_point() +
    ggplot2::labs(
      x = "Lag",
      y = "ACF"
    )
}

#' Create a PACF Plot
#'
#' Returns a complete PACF plot object.
#'
#' @param data_vec A numeric vector.
#' @param theme Theme to use.
#' @param lag.max Max lag to calculate. If NULL, uses default.
#' @return A ggplot object.
#' @inheritParams plot_acf
#' @export
plot_pacf <- function(data_vec,
                      theme = NULL,
                      lag.max = NULL,
                      ci = 0.95,
                      highlight_sig = TRUE) {
  data_vec <- stats::na.omit(data_vec)
  n <- length(data_vec)
  if (n < 3) stop("Need at least 3 observations")

  pacf_data <- stats::pacf(data_vec,
    lag.max = lag.max,
    plot = FALSE,
    na.action = na.pass
  )
  pacf_df <- data.frame(
    lag = pacf_data$lag,
    pacf = pacf_data$acf
  )

  ci_val <- qnorm((1 + ci) / 2) / sqrt(n)

  c <- .iqr_aes(theme)
  p <- base_plot(pacf_df,
    ggplot2::aes(
      x = lag,
      y = pacf
    ),
    theme = theme
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "solid",
      color = c$muted
    ) +
    ggplot2::geom_hline(
      yintercept = c(ci_val, -ci_val),
      linetype = "dashed",
      color = c$fail
    )

  if (highlight_sig) {
    p <- p + ggplot2::geom_segment(
      ggplot2::aes(
        xend = lag,
        yend = 0,
        color = abs(pacf) > ci_val
      )
    ) + ggplot2::scale_color_manual(
      values = c(
        "FALSE" = c$text,
        "TRUE" = c$fail
      ),
      guide = "none"
    )
  } else {
    p <- p + ggplot2::geom_segment(ggplot2::aes(
      xend = lag,
      yend = 0
    ))
  }
  p + ggplot2::geom_point() +
    ggplot2::labs(x = "Lag", y = "PACF")
}


# ============================================================================
# 4. ROC curve (fully rewritten)
# ============================================================================

#' Create a ROC Curve Plot
#'
#' @param data data.frame
#' @param labels_var name of true labels column (binary, 0/1 or factor)
#' @param predictions_var name of predicted probabilities column
#' @param theme theme specification
#' @param ci_auc logical, add confidence interval for AUC (bootstrap)
#' @return ggplot object
#' @export
plot_roc_curve <- function(data, labels_var, predictions_var,
                           theme = NULL, ci_auc = FALSE) {
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("'pROC' package is required. Please install it with: install.packages('pROC')")
  }

  # Input validation
  if (!all(c(labels_var, predictions_var) %in% names(data))) {
    stop("Column names not found in data")
  }
  labels <- data[[labels_var]]
  predictions <- data[[predictions_var]]

  # Ensure labels are binary
  if (length(unique(stats::na.omit(labels))) != 2) {
    stop("labels_var must be binary (0/1 or two-level factor)")
  }

  roc_obj <- pROC::roc(labels, predictions, quiet = TRUE, ci = ci_auc)
  auc_value <- pROC::auc(roc_obj)

  # Extract ROC coordinates
  roc_df <- data.frame(
    fpr = 1 - roc_obj$specificities,
    tpr = roc_obj$sensitivities
  )

  c <- .iqr_aes(theme)
  p <- base_plot(roc_df,
    ggplot2::aes(
      x = fpr,
      y = tpr
    ),
    theme = theme
  ) +
    ggplot2::geom_line(
      color = c$data,
      linewidth = 1.2
    ) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = c$muted
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      x = "1 - Specificity (False Positive Rate)",
      y = "Sensitivity (True Positive Rate)"
    )

  # Add AUC text
  auc_label <- sprintf(
    "AUC = %.3f",
    auc_value
  )
  if (ci_auc && !is.null(roc_obj$ci)) {
    auc_label <- sprintf(
      "AUC = %.3f (95%% CI: %.3f-%.3f)",
      auc_value,
      roc_obj$ci[1],
      roc_obj$ci[3]
    )
  }
  p <- p + ggplot2::annotate("text",
    x = 0.75,
    y = 0.25,
    label = auc_label,
    hjust = 0,
    size = 3.5
  )
  p
}

#
# ============================================================================
# 6. Variance components plot (renamed and improved sorting)
# ============================================================================

#' Create a Variance Components Bar Plot (Stacked)
#'
#' @param data data.frame with columns `source` and `variance_percent`
#' @param theme theme specification
#' @param sort_by_variance logical, sort components by variance size
#' @return ggplot object
#' @export
plot_variance_components <- function(data,
                                     theme = NULL,
                                     sort_by_variance = TRUE) {
  if (!requireNamespace("dplyr",
    quietly = TRUE
  )) {
    stop("'dplyr' is required.")
  }

  if (sort_by_variance) {
    data <- data %>% dplyr::arrange(.data$variance_percent)
  } else {
    data <- data %>% dplyr::arrange(dplyr::desc(.data$source))
  }

  data <- data %>%
    dplyr::mutate(label_pos = cumsum(variance_percent) - 0.5 * variance_percent)

  c <- .iqr_aes(theme)
  # base_plot auto-injects the discrete fill scale for the mapped `source`
  # column, so no explicit .scale_fill_discrete() is needed here.
  base_plot(data, ggplot2::aes(
    x = 1,
    y = variance_percent,
    fill = source
  ),
  theme = theme
  ) +
    ggplot2::geom_col(
      position = "stack",
      width = 0.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = label_pos,
        label = paste0(round(variance_percent, 1), "%")
      ),
      color = .iqr_plotter$.contrast_text(c$data),
      size = 4,
      fontface = "bold"
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(fill = "Variance Source") +
    ggplot2::labs(title = "Variance Components", x = NULL, y = NULL)
}

# Keep old name for backward compatibility, but new name is recommended
#' @rdname plot_variance_components
#' @export
plot_variational_funnel <- plot_variance_components


# ============================================================================
# 7. Combine plots (optimized theme application) -- low value, will be deprecated later, kept for now
# ============================================================================

#' Combine Multiple Plots into a Dashboard Layout
#'
#' @param ... plot objects
#' @param plotlist list of plot objects
#' @param ncol number of columns
#' @param nrow number of rows
#' @param theme theme specification, applied to all plots
#' @return combined plot (patchwork)
#' @export
combine_plots <- function(...,
                          plotlist = NULL,
                          ncol = NULL,
                          nrow = NULL,
                          theme = NULL) {
  if (!requireNamespace("patchwork",
    quietly = TRUE
  )) {
    stop("'patchwork' package is required. Please install it with: install.packages('patchwork')")
  }
  plots <- c(
    list(...),
    plotlist
  )
  if (length(plots) == 0) stop("No plots provided")

  if (!is.null(theme)) {
    iqr_theme <- as_iqr_theme(theme)
    plots <- lapply(plots, function(p) {
      if (inherits(p, "ggplot")) {
        p + iqr_theme
      } else {
        p
      }
    })
  }
  patchwork::wrap_plots(plots, ncol = ncol, nrow = nrow)
}

# ============================================================================
# 8. Set default theme (add validation)
# ============================================================================

#' Set Global Default Theme
#'
#' @param theme theme name (character) or theme object
#' @return invisible NULL
#' @export
set_default_theme <- function(theme = "academic") {
  # Try to validate whether theme is available
  tryCatch(
    {
      tmp <- as_iqr_theme(theme)
    },
    error = function(e) {
      warning("The theme '", theme, "' may not be available. Setting anyway.")
    }
  )
  options(iqr.default_theme = theme)
  invisible(NULL)
}

#' Create Layers for Histogram and Density
#'
#' @param bins Number of bins for the histogram. If NULL, uses default.
#' @param theme Theme spec (NULL / string / function / IqrTheme). Currently
#'   not used for color resolution (histogram/density colors are usually
#'   mapped via \code{base_plot()}'s auto-injection); accepted for API
#'   consistency with the other \code{layers_*} functions.
#' @param ... Common arguments passed to both geoms (e.g., mapping, data, na.rm).
#' @param hist_args List of arguments passed to geom_histogram.
#' @param density_args List of arguments passed to geom_density.
#' @return A list of ggplot2 layers.
#' @export
layers_histogram_density <- function(bins = NULL, theme = NULL, ...,
                                     hist_args = list(),
                                     density_args = list()) {
  common <- list(...)

  hist_default <- list(mapping = ggplot2::aes(y = ggplot2::after_stat(density)), bins = bins)
  final_hist <- utils::modifyList(
    utils::modifyList(hist_default, common),
    hist_args
  )

  density_default <- list()
  final_density <- utils::modifyList(
    utils::modifyList(density_default, common),
    density_args
  )

  list(
    do.call(ggplot2::geom_histogram, final_hist),
    do.call(ggplot2::geom_density, final_density)
  )
}


#' Create Layers for Q-Q Plot
#'
#' @param distribution Distribution function name (e.g. "norm").
#' @param dparams Parameters for the distribution function as a list.
#' @param theme Theme spec (NULL / string / function / IqrTheme). Currently
#'   not used for color resolution; accepted for API consistency with the
#'   other \code{layers_*} functions.
#' @param ... Common arguments passed to stat_qq and stat_qq_line.
#' @param qq_args List of arguments passed to stat_qq.
#' @param line_args List of arguments passed to stat_qq_line.
#' @return A list of ggplot2 layers.
#' @export
layers_qq <- function(distribution = "norm", dparams = list(), theme = NULL, ...,
                      qq_args = list(),
                      line_args = list()) {
  common <- list(...)

  q_dist <- get(paste0("q", distribution))
  qq_default <- list(distribution = q_dist, dparams = dparams)
  final_qq <- utils::modifyList(
    utils::modifyList(qq_default, common),
    qq_args
  )

  line_default <- list(distribution = q_dist, dparams = dparams)
  final_line <- utils::modifyList(
    utils::modifyList(line_default, common),
    line_args
  )

  list(
    do.call(ggplot2::stat_qq, final_qq),
    do.call(ggplot2::stat_qq_line, final_line)
  )
}


#' Create Layers for Boxplot
#'
#' @param add_jitter If TRUE, adds jittered points.
#' @param theme Theme spec (NULL / string / function / IqrTheme). Currently
#'   not used for color resolution (boxplot fill is usually mapped via
#'   \code{base_plot()}'s auto-injection); accepted for API consistency with
#'   the other \code{layers_*} functions.
#' @param ... Common arguments passed to both geoms.
#' @param boxplot_args List of arguments passed to geom_boxplot.
#' @param jitter_args List of arguments passed to geom_jitter (if used).
#' @return A list of ggplot2 layers.
#' @export
layers_boxplot <- function(add_jitter = TRUE, theme = NULL, ...,
                           boxplot_args = list(),
                           jitter_args = list()) {
  common <- list(...)

  boxplot_default <- list()
  final_boxplot <- utils::modifyList(
    utils::modifyList(boxplot_default, common),
    boxplot_args
  )

  layers <- list(do.call(ggplot2::geom_boxplot, final_boxplot))

  if (add_jitter) {
    jitter_default <- list(width = 0.2, alpha = 0.5)
    final_jitter <- utils::modifyList(
      utils::modifyList(jitter_default, common),
      jitter_args
    )
    layers <- c(layers, list(do.call(ggplot2::geom_jitter, final_jitter)))
  }

  layers
}


#' Create Layers for Trend Line
#'
#' @param mapping A ggplot2 aesthetic mapping.
#' @param data A data frame.
#' @param add_points If TRUE, adds points to the line.
#' @param smoothing Smoothing method (e.g. "lm", "loess"). If NULL, no smoothing.
#' @param theme Theme spec (NULL / string / function / IqrTheme). Currently
#'   not used for color resolution; accepted for API consistency with the
#'   other \code{layers_*} functions.
#' @param line_args List of arguments passed to geom_line.
#' @param point_args List of arguments passed to geom_point (if used).
#' @param smooth_args List of arguments passed to geom_smooth (if used).
#' @return A list of ggplot2 layers.
#' @export
layers_trend_line <- function(mapping = NULL, data = NULL,
                              add_points = TRUE,
                              smoothing = NULL,
                              theme = NULL,
                              line_args = list(),
                              point_args = list(),
                              smooth_args = list()) {
  # Base args: data and mapping are inherited by each geom
  base_args <- list(data = data, mapping = mapping)

  # Line layer
  line_args_full <- utils::modifyList(base_args, line_args)
  layers <- list(do.call(ggplot2::geom_line, line_args_full))

  # Point layer
  if (add_points) {
    point_args_full <- utils::modifyList(base_args, point_args)
    layers <- c(layers, list(do.call(ggplot2::geom_point, point_args_full)))
  }

  # Smoothing layer
  if (!is.null(smoothing)) {
    smooth_default <- list(method = smoothing, se = FALSE, linetype = "dashed")
    smooth_args_full <- utils::modifyList(
      utils::modifyList(base_args, smooth_default),
      smooth_args
    )
    layers <- c(layers, list(do.call(ggplot2::geom_smooth, smooth_args_full)))
  }

  layers
}

#' Create Layers for Percentile Band (Forest Plot)
#'
#' @param theme Theme spec (NULL / string / function / IqrTheme). Used to
#'   resolve the default point \code{fill} from the theme's \code{"surface"}
#'   UI slot so the point center follows the active theme background.
#' @param ... Common arguments passed to geom_errorbar and geom_point.
#' @param errorbar_args List of arguments passed to geom_errorbar.
#' @param point_args List of arguments passed to geom_point.
#' @return A list of ggplot2 layers.
#' @export
layers_percentile_band <- function(theme = NULL, ...,
                                   errorbar_args = list(),
                                   point_args = list()) {
  common <- list(...)
  c <- .iqr_aes(theme)

  errorbar_default <- list(orientation = "y")
  final_errorbar <- utils::modifyList(
    utils::modifyList(errorbar_default, common),
    errorbar_args
  )

  point_default <- list(shape = 21, fill = c$surface, stroke = 1.5)
  final_point <- utils::modifyList(
    utils::modifyList(point_default, common),
    point_args
  )

  list(
    do.call(ggplot2::geom_errorbar, final_errorbar),
    do.call(ggplot2::geom_point, final_point)
  )
}


#' Create Layers for Violin Plot
#'
#' @param add_boxplot If TRUE, adds a small boxplot inside.
#' @param theme Theme spec (NULL / string / function / IqrTheme). Used to
#'   resolve the inner boxplot \code{fill} from the theme's \code{"surface"}
#'   UI slot so the inner boxplot contrasts with the violin fill.
#' @param ... Common arguments passed to geom_violin and (optionally) geom_boxplot.
#' @param violin_args List of arguments passed to geom_violin.
#' @param boxplot_args List of arguments passed to geom_boxplot (if used).
#' @return A list of ggplot2 layers.
#' @export
layers_violin <- function(add_boxplot = TRUE, theme = NULL, ...,
                          violin_args = list(),
                          boxplot_args = list()) {
  common <- list(...)
  c <- .iqr_aes(theme)

  violin_default <- list()
  final_violin <- utils::modifyList(
    utils::modifyList(violin_default, common),
    violin_args
  )

  layers <- list(do.call(ggplot2::geom_violin, final_violin))

  if (add_boxplot) {
    # Inner boxplot uses the theme's surface color so it contrasts with the
    # violin fill regardless of which theme is active.
    boxplot_default <- list(width = 0.1, fill = c$surface, alpha = 0.7)
    final_boxplot <- utils::modifyList(
      utils::modifyList(boxplot_default, common),
      boxplot_args
    )
    layers <- c(layers, list(do.call(ggplot2::geom_boxplot, final_boxplot)))
  }

  layers
}

#' Create Layers for Specification Limits
#'
#' @param lsl Lower specification limit. If NULL, not drawn.
#' @param usl Upper specification limit. If NULL, not drawn.
#' @param theme Theme spec (NULL / string / function / IqrTheme). Used to
#'   resolve \code{spec_color} when it is NULL. The color is taken from the
#'   theme's semantic palette slot \code{"fail"}.
#' @param spec_color Color for specification lines and labels. If NULL
#'   (default), the color is resolved from \code{theme} via
#'   \code{theme_obj$get_pal("semantic", name="fail")}, so the lines follow
#'   the active theme automatically. Pass an explicit color to override.
#' @param lsl_label Label for LSL.
#' @param usl_label Label for USL.
#' @param orientation Direction of the spec lines. \code{"v"} (default) draws
#'   vertical lines via \code{geom_vline} — appropriate when the spec applies
#'   to the x-axis variable (e.g. histograms). \code{"h"} draws horizontal
#'   lines via \code{geom_hline} — appropriate when the spec applies to the
#'   y-axis variable (e.g. individual values charts, trend charts).
#' @param vline_args List of arguments passed to geom_vline/geom_hline.
#' @param annotate_args List of arguments passed to annotate (applies to both).
#' @return A list of ggplot2 layers.
#' @export
layers_spec_limits <- function(lsl = NULL, usl = NULL,
                               theme = NULL,
                               spec_color = NULL,
                               lsl_label = paste("LSL =", .fmt_spec(lsl)),
                               usl_label = paste("USL =", .fmt_spec(usl)),
                               orientation = c("v", "h"),
                               vline_args = list(),
                               annotate_args = list()) {
  orientation <- match.arg(orientation)

  # Resolve spec_color from theme's semantic "fail" slot when not supplied.
  if (is.null(spec_color)) {
    theme_obj <- as_iqr_theme_object(theme)
    spec_color <- theme_obj$get_pal("semantic", name = "fail")
  }

  result <- list()

  # geom function: geom_vline for vertical, geom_hline for horizontal
  geom_fn <- if (orientation == "v") ggplot2::geom_vline else ggplot2::geom_hline
  intercept_arg <- if (orientation == "v") "xintercept" else "yintercept"

  # Label positioning: for vertical lines, label at top (y=Inf); for
  # horizontal lines, label at right edge (x=Inf).
  if (orientation == "v") {
    label_x_lsl <- lsl; label_y_lsl <- Inf
    label_x_usl <- usl; label_y_usl <- Inf
    lsl_annot <- list(x = label_x_lsl, y = label_y_lsl, vjust = 1.5, hjust = -0.1)
    usl_annot <- list(x = label_x_usl, y = label_y_usl, vjust = 1.5, hjust = 1.1)
  } else {
    label_x_lsl <- Inf; label_y_lsl <- lsl
    label_x_usl <- Inf; label_y_usl <- usl
    lsl_annot <- list(x = label_x_lsl, y = label_y_lsl, vjust = -0.5, hjust = 1.05)
    usl_annot <- list(x = label_x_usl, y = label_y_usl, vjust = 1.5, hjust = 1.05)
  }

  if (!is.null(lsl)) {
    line_default <- stats::setNames(list(
      lsl, color = spec_color, linetype = "longdash", linewidth = 1
    ), c(intercept_arg, "color", "linetype", "linewidth"))
    final_line <- utils::modifyList(line_default, vline_args)
    result <- c(result, list(do.call(geom_fn, final_line)))

    annot_default <- utils::modifyList(c(list("text",
      label = lsl_label, color = spec_color, fontface = "bold"), lsl_annot),
      annotate_args)
    result <- c(result, list(do.call(ggplot2::annotate, annot_default)))
  }

  if (!is.null(usl)) {
    line_default <- stats::setNames(list(
      usl, color = spec_color, linetype = "longdash", linewidth = 1
    ), c(intercept_arg, "color", "linetype", "linewidth"))
    final_line <- utils::modifyList(line_default, vline_args)
    result <- c(result, list(do.call(geom_fn, final_line)))

    annot_default <- utils::modifyList(c(list("text",
      label = usl_label, color = spec_color, fontface = "bold"), usl_annot),
      annotate_args)
    result <- c(result, list(do.call(ggplot2::annotate, annot_default)))
  }

  result
}

# Format a spec-limit value for annotation labels. Trims full-precision
# floats (e.g. from Box-Cox transformed specs) to a readable 4-significant
# digit string, avoiding both scientific notation and trailing zeros.
.fmt_spec <- function(x) {
  if (is.null(x) || length(x) == 0) return("NA")
  if (is.na(x)) return("NA")
  if (is.character(x)) return(x)
  if (!is.finite(x)) return(as.character(x))
  # 4 significant digits in plain ("fg") notation; this trims trailing zeros
  # automatically and avoids scientific notation for typical spec values.
  formatC(signif(x, 4), format = "fg", big.mark = "")
}


#' Create Layers for Control Chart
#'
#' Returns layers for a base control chart.
#'
#' @param data A data.frame with columns x, y, cl, lcl, ucl.
#' @param theme Theme spec (NULL / string / function / IqrTheme). Used to
#'   resolve any of \code{ucl_color}, \code{cl_color}, \code{data_color}
#'   that are NULL. Colors are taken from the theme's semantic and discrete
#'   palettes:
#'   \itemize{
#'     \item \code{ucl_color} / \code{lcl_color} <- semantic "fail"
#'     \item \code{cl_color} <- semantic "neutral"
#'     \item \code{data_color} <- discrete palette, first color
#'   }
#' @param ucl_color Color for UCL/LCL lines. If NULL (default), resolved
#'   from \code{theme}. Pass an explicit color to override.
#' @param cl_color Color for center line. If NULL (default), resolved from
#'   \code{theme}.
#' @param data_color Color for data points and line. If NULL (default),
#'   resolved from \code{theme}.
#' @return A list of ggplot2 layers.
#' @export
layers_control_chart <- function(data,
                                 theme = NULL,
                                 ucl_color = NULL,
                                 cl_color = NULL,
                                 data_color = NULL) {
  # Resolve colors from theme when not supplied explicitly.
  # - ucl/lcl lines use semantic "fail" (red-ish) to signal control limits
  # - cl line uses semantic "neutral" (grey-ish) as a reference baseline
  # - data line/points use the first discrete palette color
  # Explicit color arguments override the theme-derived defaults.
  if (is.null(ucl_color) || is.null(cl_color) || is.null(data_color)) {
    theme_obj <- as_iqr_theme_object(theme)
    if (is.null(ucl_color))  ucl_color  <- theme_obj$get_pal("semantic", name = "fail")
    if (is.null(cl_color))   cl_color   <- theme_obj$get_pal("semantic", name = "neutral")
    if (is.null(data_color)) data_color <- theme_obj$get_pal("discrete")[1]
  }

  y_range <- range(c(data$y, data$lcl, data$ucl), na.rm = TRUE)
  y_buffer <- (y_range[2] - y_range[1]) * 0.1
  y_lim <- c(y_range[1] - y_buffer, y_range[2] + y_buffer)

  # IMPORTANT: every geom layer maps `x = .data$x` explicitly and sets
  # `inherit.aes = FALSE`, so the layer does NOT inherit the parent base_plot's
  # aesthetic mapping (which typically references columns like `index` or
  # `value` that do not exist in this control-chart `data` frame). Without
  # `inherit.aes = FALSE`, ggplot2 would look up `.data$index` against this
  # data and abort with "Column `index` not found in `.data`" whenever the
  # caller passes a renamed frame (x/y/cl/ucl/lcl).
  list(
    ggplot2::geom_line(data = data, ggplot2::aes(x = .data$x, y = .data$ucl),
                       color = ucl_color, linetype = "dashed", linewidth = 1,
                       inherit.aes = FALSE),
    ggplot2::geom_line(data = data, ggplot2::aes(x = .data$x, y = .data$lcl),
                       color = ucl_color, linetype = "dashed", linewidth = 1,
                       inherit.aes = FALSE),
    ggplot2::geom_line(data = data, ggplot2::aes(x = .data$x, y = .data$cl),
                       color = cl_color, linewidth = 1, inherit.aes = FALSE),
    ggplot2::geom_line(data = data, ggplot2::aes(x = .data$x, y = .data$y),
                       color = data_color, linewidth = 0.5, inherit.aes = FALSE),
    ggplot2::geom_point(data = data, ggplot2::aes(x = .data$x, y = .data$y),
                       color = data_color, size = 2, inherit.aes = FALSE),
    ggplot2::annotate("text",
      x = max(data$x, na.rm = TRUE), y = data$ucl[1],
      label = "UCL", hjust = -0.2, vjust = -0.5, size = 3
    ),
    ggplot2::annotate("text",
      x = max(data$x, na.rm = TRUE), y = data$lcl[1],
      label = "LCL", hjust = -0.2, vjust = 1.5, size = 3
    ),
    ggplot2::annotate("text",
      x = max(data$x, na.rm = TRUE), y = data$cl[1],
      label = "CL", hjust = -0.2, vjust = -0.5, size = 3
    ),
    ggplot2::coord_cartesian(ylim = y_lim, expand = TRUE)
  )
}
