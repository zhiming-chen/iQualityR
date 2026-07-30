# =============================================================================
# File: R/iqr_prob.R
# Description: Probability distribution module user entry point (integrator + convenience functions)
# =============================================================================

#' @title iqr_prob: Probability distribution analysis entry class
#' @description
#' Top-level interface for iQualityR probability distribution module, coordinating computation, plotting, reporting, and interpretation.
#'
#' **Core functions**:
#' - Supports 16 probability distributions (12 continuous + 4 discrete)
#' - Probability calculation: Given X find P (left tail/right tail/interval/outside interval)
#' - Quantile calculation: Given P find X
#' - Visualization: PDF/CDF graphics, shaded areas, annotation lines
#' - Report export: Console print, data frame export, Excel report
#' - Result interpretation: Translates statistical terminology into quality personnel-friendly language
#'
#' **Dual interface design**:
#' - R6 class interface: `iqr_prob$new()$calc()$plot()` (suitable for chaining)
#' - Convenience function interface: `prob_calc()`, `prob_plot()` (suitable for one-time use)
#'
#' @examples
#' # Normal distribution: P(X > 108) given mean = 100, sd = 5
#' prob <- iqr_prob$new(type = "norm", params = list(mean = 100, sd = 5))
#' prob$calc(values = 108, mode = "prob", calc_type = "upper")
#'
#' # Plotting requires the iQualityR.plot Suggests package
#' if (requireNamespace("iQualityR.plot", quietly = TRUE)) {
#'   prob$plot(show_cdf = TRUE)
#' }
#'
#' @export
iqr_prob <- R6::R6Class("iqr_prob",
  public = list(
    #' @field nodes Distribution node library
    nodes = list(),
    #' @field last_results Cached computation results
    last_results = NULL,
    #' @field analyzer Computation engine
    analyzer = NULL,
    #' @field plotter Plotting engine
    plotter = NULL,
    #' @field reporter Report engine
    reporter = NULL,
    #' @field interpreter Statistics translator
    interpreter = NULL,
    #' @field theme_obj Theme object
    theme_obj = NULL,

    #' @description Initialize probability analysis module
    #' @param type Distribution type (e.g., "norm", "binom")
    #' @param params Distribution parameter list
    #' @param loc Location offset
    #' @param dist_list List for batch adding distributions
    #' @param theme Theme name or IqrTheme object
    #' @param ... Other parameters
    #' @return iqr_prob object
    initialize = function(type = NULL, params = list(), loc = 0,
                          dist_list = NULL, theme = "academic", ...) {
      self$analyzer     <- ProbAnalyzer$new()
      self$plotter      <- ProbPlotter$new(theme = theme)
      self$reporter     <- ProbReporter$new()
      self$interpreter  <- StatInterpreter$new()

      # Theme instantiation
      if (inherits(theme, "IqrTheme")) {
        self$theme_obj <- theme
      } else {
        tryCatch({
          self$theme_obj <- IqrTheme$new(theme, ...)
        }, error = function(e) {
          self$theme_obj <<- NULL
        })
      }

      # Add distributions
      if (!is.null(type) || !is.null(dist_list)) {
        self$add_dist(type = type, params = params, loc = loc, dist_list = dist_list, reset = TRUE)
      }
    },

    #' @description Add distribution node
    #' @param id Node ID (optional, auto-generated)
    #' @param type Distribution type
    #' @param params Distribution parameters
    #' @param loc Location offset
    #' @param dist_list List for batch adding distributions
    #' @param reset Whether to clear existing nodes
    #' @return Self-reference
    #' @examples
    #' prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
    #' prob$add_dist(type = "weibull", params = list(shape = 2, scale = 1))
    #' prob$add_dist(dist_list = list(
    #'   list(id = "D1", type = "norm", params = list(mean = 0, sd = 1)),
    #'   list(id = "D2", type = "norm", params = list(mean = 1, sd = 1))
    #' ), reset = TRUE)
    add_dist = function(id = NULL, type = NULL, params = list(),
                        loc = 0, dist_list = NULL, reset = FALSE) {
      if (reset) self$nodes <- list()

      # Batch add
      if (!is.null(dist_list)) {
        for (d in dist_list) {
          d_id <- d$id %||% paste0("D", length(self$nodes) + 1)
          self$nodes[[d_id]] <- ProbNode$new(d_id, d$type, d$params, d$loc %||% 0)
        }
      }

      # Single add
      if (!is.null(type)) {
        d_id <- id %||% paste0("D", length(self$nodes) + 1)
        self$nodes[[d_id]] <- ProbNode$new(d_id, type, params, loc)
      }

      invisible(self)
    },

    #' @description Execute calculation
    #' @param values Input value (X value for probability mode, P value for quantile mode)
    #' @param mode Calculation mode ("prob" or "quant")
    #' @param calc_type Calculation type ("lower", "upper", "between", "outside")
    #' @param type Distribution type (if provided, will add distribution)
    #' @param params Distribution parameters (if provided, will add distribution)
    #' @param loc Location offset
    #' @param dist_list Distribution list
    #' @return Invisible self-reference
    #' @examples
    #' prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
    #'
    #' # Probability calculation: P(X > 1.96) for N(0,1)
    #' prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
    #'
    #' # Quantile calculation: X value corresponding to P = 0.975
    #' prob$calc(values = 0.975, mode = "quant")
    #'
    #' # Interval probability: P(1 < X < 2)
    #' prob$calc(values = c(1, 2), mode = "prob", calc_type = "between")
    calc = function(values = NULL, mode = "prob", calc_type = "lower",
                    type = NULL, params = list(), loc = 0, dist_list = NULL) {
      # If distribution info provided, add first
      if (!is.null(type) || !is.null(dist_list)) {
        self$add_dist(type = type, params = params, loc = loc, dist_list = dist_list, reset = TRUE)
      }

      if (length(self$nodes) == 0) {
        stop("[iqr_prob] Distribution nodes not configured. Use add_dist() or specify type/params during initialization.",
             call. = FALSE)
      }
      if (is.null(values)) {
        stop("[iqr_prob] Must provide values to analyze.", call. = FALSE)
      }

      # Execute calculation
      self$last_results <- lapply(self$nodes, function(node) {
        self$analyzer$analyze(node, mode, calc_type, values)
      })

      # Auto print console report
      self$reporter$print_console(self$last_results)
      invisible(self)
    },

    #' @description Plot graphics
    #' @param show_cdf Whether to show CDF plot
    #' @param facet Whether to display as facets
    #' @param theme_obj Theme object (optional)
    #' @param ... Other parameters passed to theme settings
    #' @return ggplot/patchwork object
    #' @examples
    #' # Plotting requires the iQualityR.plot Suggests package
    #' if (requireNamespace("iQualityR.plot", quietly = TRUE)) {
    #'   prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
    #'   prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
    #'   prob$plot()                    # Default PDF plot
    #'   prob$plot(show_cdf = TRUE)     # PDF + CDF combined plot
    #' }
    plot = function(show_cdf = FALSE, facet = FALSE, theme_obj = NULL, ...) {
      if (is.null(self$last_results)) {
        stop("[iqr_prob] Please run calc() first.", call. = FALSE)
      }
      if (!is.null(theme_obj)) {
        self$set_theme(theme_style = theme_obj, ...)
      }

      self$plotter$render(
        nodes        = self$nodes,
        calc_results = self$last_results,
        facet        = facet,
        show_cdf     = show_cdf,
        mode         = self$last_results[[1]]$mode,
        theme_obj    = self$theme_obj
      )
    },

    #' @description Interpret statistical results (translate to quality personnel-friendly language)
    #' @param audience Audience level ("manager", "technical", "client")
    #' @param ... Other parameters
    #' @return Interpretation string (also printed to console)
    #' @examples
    #' prob <- iqr_prob$new(type = "binom", params = list(size = 50, prob = 0.05))
    #' prob$calc(values = 3, mode = "prob", calc_type = "lower")
    #' prob$interpret(audience = "manager")
    interpret = function(audience = "manager", ...) {
      if (is.null(self$last_results)) {
        stop("[iqr_prob] Please run calc() first.", call. = FALSE)
      }

      # Generate interpretation for each node
      explanations <- list()
      for (id in names(self$nodes)) {
        node <- self$nodes[[id]]
        info <- node$get_node_info()

        dist_result <- list(
          type = info$type,
          params = info$params,
          calc_result = self$last_results[[id]]
        )

        explanations[[id]] <- self$interpreter$interpret(dist_result, audience = audience)
      }

      cat(paste(explanations, collapse = "\n\n"), "\n")
      invisible(explanations)
    },

    #' @description Structured data export
    #' @return Data frame
    #' @examples
    #' prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
    #' prob$calc(values = 1.96, mode = "prob", calc_type = "upper")
    #' df <- prob$report()
    #' head(df)
    report = function() {
      if (is.null(self$last_results)) return(NULL)
      self$reporter$to_dataframe(self$last_results)
    },

    #' @description Export to Excel report
    #' @param path Output path
    #' @param excel_exporter ExcelExporter instance (optional, auto-created)
    #' @return Invisible self-reference
    #' @examples
    #' # Excel export requires the iQualityR.core ExcelExporter.
    #' prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
    #' prob$calc(values = 1.96, mode = "prob")
    #' if (requireNamespace("iQualityR.core", quietly = TRUE)) {
    #'   tf <- tempfile(fileext = ".xlsx")
    #'   prob$report_excel(path = tf)
    #' }
    report_excel = function(path = NULL, excel_exporter = NULL) {
      if (is.null(self$last_results)) {
        stop("[iqr_prob] Please run calc() first.", call. = FALSE)
      }

      if (is.null(path)) {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        path <- paste0("probability_analysis_report_", timestamp, ".xlsx")
      }

      # If ExcelExporter not provided, try to create one from iQualityR.core
      if (is.null(excel_exporter)) {
        if (!requireNamespace("iQualityR.core", quietly = TRUE) ||
            !"ExcelExporter" %in% getNamespaceExports("iQualityR.core")) {
          stop("[iqr_prob] ExcelExporter class not found. Install/attach iQualityR.core.",
               call. = FALSE)
        }
        if (!is.null(self$theme_obj)) {
          excel_exporter <- iQualityR.core::ExcelExporter$new(self$theme_obj$config)
        } else {
          excel_exporter <- iQualityR.core::ExcelExporter$new()
        }
      }

      self$reporter$export_excel(
        calc_results = self$last_results,
        nodes = self$nodes,
        path = path,
        excel_exporter = excel_exporter
      )

      message("[iqr_prob] Report exported: ", path)
      invisible(self)
    },

    #' @description One-click analysis (calculation + plotting)
    #' @param values Input value
    #' @param mode Calculation mode
    #' @param calc_type Calculation type
    #' @param show_cdf Whether to show CDF plot
    #' @param facet Whether to display as facets
    #' @param type Distribution type
    #' @param params Distribution parameters
    #' @param loc Location offset
    #' @param dist_list Distribution list
    #' @param ... Other parameters
    #' @return ggplot/patchwork object
    #' @examples
    #' # Plotting requires the iQualityR.plot Suggests package
    #' if (requireNamespace("iQualityR.plot", quietly = TRUE)) {
    #'   prob <- iqr_prob$new(type = "norm", params = list(mean = 0, sd = 1))
    #'   prob$analyze(values = 1.96, mode = "prob", calc_type = "upper",
    #'                show_cdf = TRUE)
    #' }
    analyze = function(values, mode = "prob", calc_type = "lower",
                      show_cdf = FALSE, facet = FALSE,
                      type = NULL, params = list(), loc = 0, dist_list = NULL, ...) {
      self$calc(values = values, mode = mode, calc_type = calc_type,
                type = type, params = params, loc = loc, dist_list = dist_list)
      self$plot(show_cdf = show_cdf, facet = facet, ...)
    },

    #' @description Set theme
    #' @param theme_style Theme name or IqrTheme object
    #' @param ... Other parameters
    #' @return Invisible self-reference
    set_theme = function(theme_style = NULL, ...) {
      if (is.null(self$theme_obj) && !is.null(theme_style)) {
        tryCatch({
          self$theme_obj <- IqrTheme$new(theme_style %||% "academic", ...)
        }, error = function(e) {
          self$theme_obj <<- NULL
        })
      }
      if (!is.null(self$theme_obj) && !is.null(theme_style)) {
        tryCatch({
          if (is.character(theme_style)) {
            self$theme_obj$set_theme(theme_style, ...)
          } else if (inherits(theme_style, "IqrTheme")) {
            self$theme_obj <- theme_style
          }
        }, error = function(e) {})
      }
      invisible(self)
    },

    #' @description List all available distributions
    #' @return Data frame
    list_distributions = function() {
      list_available_dists()
    },

    #' @description Get currently configured distribution information
    #' @return List
    get_node_info = function() {
      lapply(self$nodes, function(node) node$get_node_info())
    }
  )
)

# =============================================================================
# Convenience functions (stateless interface, suitable for one-time use)
# =============================================================================

#' @title Convenience probability calculation function
#' @description
#' Calculate probability or quantile directly without creating R6 object.
#' Suitable for quick query scenarios.
#'
#' @param type Distribution type
#' @param params Distribution parameter list
#' @param x Input value (X value for probability mode, P value for quantile mode)
#' @param mode Calculation mode ("prob" or "quant")
#' @param calc_type Calculation type ("lower", "upper", "between", "outside")
#' @param loc Location offset
#' @param plot Whether to plot
#' @param show_cdf Whether to show CDF plot
#' @param interpret Whether to output interpretation
#' @param audience Audience level for interpretation
#' @param ... Other parameters
#'
#' @return Calculation result list (or plot object if plot = TRUE)
#' @export
#'
#' @examples
#' # Normal distribution: P(X > 1.96)
#' prob_calc(type = "norm", params = list(mean = 0, sd = 1),
#'           x = 1.96, calc_type = "upper")
#'
#' # Binomial distribution: P(X <= 3) where n=50, p=0.05
#' prob_calc(type = "binom", params = list(size = 50, prob = 0.05),
#'           x = 3, calc_type = "lower", interpret = TRUE)
#'
#' # Quantile calculation
#' prob_calc(type = "norm", params = list(mean = 100, sd = 5),
#'           x = 0.975, mode = "quant")
prob_calc <- function(type, params = list(), x, mode = "prob",
                      calc_type = "lower", loc = 0,
                      plot = FALSE, show_cdf = FALSE,
                      interpret = FALSE, audience = "manager", ...) {
  # Create temporary object
  prob <- iqr_prob$new(type = type, params = params, loc = loc, ...)

  # Execute calculation
  prob$calc(values = x, mode = mode, calc_type = calc_type)

  # Get results
  result <- prob$last_results

  # Optional plotting
  if (plot) {
    p <- prob$plot(show_cdf = show_cdf)
    print(p)
  }

  # Optional interpretation
  if (interpret) {
    prob$interpret(audience = audience)
  }

  invisible(result)
}

#' @title Convenience probability plotting function
#' @description
#' Directly plot probability distribution without creating R6 object.
#'
#' @param type Distribution type
#' @param params Distribution parameter list
#' @param x Input value (for annotation)
#' @param calc_type Calculation type
#' @param show_cdf Whether to show CDF plot
#' @param facet Whether to display as facets
#' @param loc Location offset
#' @param ... Other parameters
#'
#' @return ggplot/patchwork object
#' @export
#'
#' @examples
#' prob_plot(type = "norm", params = list(mean = 0, sd = 1),
#'           x = 1.96, calc_type = "upper", show_cdf = TRUE)
prob_plot <- function(type, params = list(), x = NULL, calc_type = "lower",
                      show_cdf = TRUE, facet = FALSE, loc = 0, ...) {
  prob <- iqr_prob$new(type = type, params = params, loc = loc, ...)

  if (!is.null(x)) {
    prob$calc(values = x, calc_type = calc_type)
  } else {
    # No input value, only plot distribution
    prob$calc(values = prob$nodes[[1]]$q(0.5), calc_type = "lower")
  }

  prob$plot(show_cdf = show_cdf, facet = facet)
}

#' @title List all available probability distributions
#' @description
#' Returns names and descriptions of all registered distributions.
#'
#' @return Data frame
#' @export
#'
#' @examples
#' list_prob_distributions()
list_prob_distributions <- function() {
  list_available_dists()
}

#' @title Get distribution information
#' @description
#' Returns detailed information for specified distribution.
#'
#' @param type Distribution type
#' @return Distribution information list

#' @export
#'
#' @examples
#' get_prob_dist_info("norm")
get_prob_dist_info <- function(type) {
  get_dist_info(type)
}
