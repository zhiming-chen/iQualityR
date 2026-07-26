# =============================================================================
# File: R/IqrSpcTask.R
# Description: SPC task coordinator (inherits IqrTaskBase)
# =============================================================================

#' @title IqrSpcTask
#' @description
#' Task coordinator for statistical process control analysis. Inherits
#' `IqrTaskBase` and orchestrates `SpcAnalyzer` and `SpcPlotter`.
#'
#' @field plan `SpcPlan` object holding chart configuration.
#'
#' @param data Data frame containing measurements (for variables/multivariate
#'   charts) or a simple placeholder for attributes charts.
#' @param measurement Measurement column name (variables / time-weighted charts).
#' @param count Optional count column name or numeric vector (attributes charts).
#' @param sample_size Optional sample-size column name or numeric vector.
#' @param plan `SpcPlan` object.
#' @param theme Theme name or `IqrTheme` object.
#' @param ... Additional arguments.
#'
#' @export
IqrSpcTask <- R6::R6Class("IqrSpcTask",
  inherit = IqrTaskBase,
  public = list(
    plan = NULL,

    #' @description Create a task instance.
    initialize = function(data = NULL, measurement = NULL,
                          count = NULL, sample_size = NULL,
                          plan, theme = "academic", ...) {
      super$initialize(data %||% data.frame(), theme, ...)
      self$plan <- plan
      private$measurement <- measurement
      private$count <- count
      private$sample_size <- sample_size

      self$executor$analyzer <- SpcAnalyzer$new()
      self$executor$plotter  <- SpcPlotter$new()
    },

    #' @description Execute SPC analysis computation.
    compute = function() {
      plan <- self$plan
      chart <- plan$chart_type

      if (chart %in% c("xbar_r", "xbar_s", "imr", "imr_rs",
                       "ewma", "cusum", "ma", "g", "t",
                       "adaptive", "arima_resid", "aewma",
                       "changepoint", "kde",
                       "lstm", "bocpd")) {
        # Univariate charts including v0.6 LSTM/BOCPD: extract x from data
        if (is.null(self$data) || nrow(self$data) == 0) {
          stop("data is required for variables/time-weighted/rare-event charts.",
               call. = FALSE)
        }
        if (is.null(private$measurement)) {
          stop("measurement column name is required.", call. = FALSE)
        }
        if (!private$measurement %in% names(self$data)) {
          stop("Column '", private$measurement, "' not found in data.",
               call. = FALSE)
        }
        x <- as.numeric(self$data[[private$measurement]])
        x <- x[!is.na(x)]
        subgroup_vec <- NULL
        if (!is.null(plan$subgroup) && plan$subgroup %in% names(self$data)) {
          subgroup_vec <- self$data[[plan$subgroup]]
        }
        self$executor$analyzer$run(
          x = x, subgroup = subgroup_vec, plan = plan)
      } else if (chart %in% c("p", "np", "u", "c", "p_laney", "u_laney")) {
        # Attributes charts
        count <- private$.resolve_count()
        sample_size <- private$.resolve_sample_size()
        self$executor$analyzer$run(
          count = count, sample_size = sample_size, plan = plan)
      } else if (chart %in% c("t2", "mewma", "t2_mewma",
                              "autoencoder", "iforest")) {
        # Multivariate ML: pass the whole data frame (numeric columns)
        self$executor$analyzer$run(data = self$data, plan = plan)
      } else {
        stop("Unsupported chart_type: ", chart, call. = FALSE)
      }

      self$results <- self$executor$analyzer$get_results()
      invisible(self)
    },

    #' @description Print summary.
    summary = function() {
      if (is.null(self$results)) {
        cat("No results yet. Run $compute() first.\n")
        return(invisible(self))
      }
      r <- self$results
      cat("\n========== SPC Analysis Summary ==========\n")
      cat(sprintf("Chart type:       %s\n", r$statistics$chart_type))
      cat(sprintf("Sample size:      %d\n", r$statistics$n_points))
      cat(sprintf("Center line:      %.4f\n", r$statistics$center))
      cat(sprintf("Sigma:            %.4f (%s)\n",
        r$statistics$sigma, r$statistics$sigma_method))
      cat(sprintf("UCL:              %.4f\n", r$statistics$ucl))
      cat(sprintf("LCL:              %.4f\n", r$statistics$lcl))
      cat(sprintf("Violations:       %d\n", r$statistics$n_violations))
      cat(sprintf("In control:       %s\n",
        if (isTRUE(r$statistics$is_in_control)) "YES" else "NO"))
      if (length(r$diagnostics$rules_triggered) > 0) {
        cat(sprintf("Rules triggered:  %s\n",
          paste(r$diagnostics$rules_triggered, collapse = ", ")))
      }
      cat("==========================================\n")
      invisible(self)
    },

    #' @description Generate plots.
    #' @param type Plot type: "single", "full", or "summary".
    #' @param theme Optional theme override.
    #' @param ... Additional arguments.
    plot = function(type = "full", theme = NULL, ...) {
      if (is.null(self$results)) {
        stop("No results. Run $compute() first.", call. = FALSE)
      }
      theme_obj_use <- if (!is.null(theme)) {
        if (inherits(theme, "IqrTheme")) theme else IqrTheme$new(theme)
      } else {
        self$theme_obj
      }
      self$executor$plotter$render(
        results = self$results,
        theme_obj = theme_obj_use,
        type = type,
        plan = self$plan,
        ...
      )
    },

    #' @description Generate report.
    #' @param format Report format (e.g. `"excel"`, `"pdf"`, `"html"`).
    #' @param path Output file path. If NULL, a temp file is used.
    #' @param ... Additional arguments passed to the reporter.
    report = function(format = "excel", path = NULL, ...) {
      if (is.null(self$results)) {
        stop("No results. Run $compute() first.", call. = FALSE)
      }
      reporter <- iQualityR.core::IqrReporter$new(self$theme_obj)
      reporter$export(
        results = self$results,
        plan = self$plan,
        task_tag = "spc",
        format = format,
        path = path,
        ...
      )
      invisible(self)
    }
  ),

  private = list(
    measurement = NULL,
    count = NULL,
    sample_size = NULL,

    .resolve_count = function() {
      count <- private$count
      if (is.character(count) && length(count) == 1) {
        if (!count %in% names(self$data)) {
          stop("Count column '", count, "' not found in data.", call. = FALSE)
        }
        return(as.numeric(self$data[[count]]))
      }
      as.numeric(count)
    },

    .resolve_sample_size = function() {
      ss <- private$sample_size
      if (is.character(ss) && length(ss) == 1) {
        if (!ss %in% names(self$data)) {
          stop("Sample-size column '", ss, "' not found in data.", call. = FALSE)
        }
        return(as.numeric(self$data[[ss]]))
      }
      if (is.null(ss)) return(NULL)
      as.numeric(ss)
    }
  )
)
