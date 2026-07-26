# =============================================================================
# File: R/IqrCapabilityTask.R
# Description: Capability analysis task coordinator (inherits IqrTaskBase)
# =============================================================================

#' @title IqrCapabilityTask
#' @description
#' Task coordinator for process capability analysis. Inherits `IqrTaskBase`
#' and orchestrates the analyzer, plotter, and reporter.
#'
#' @field plan [CapabilityPlan] object holding study configuration.
#'
#' @param data Data frame containing measurements.
#' @param measurement Measurement column name.
#' @param plan [CapabilityPlan] object.
#' @param theme Theme name or [IqrTheme] object.
#' @param ... Additional arguments passed to `IqrTaskBase$initialize()`.
#'
#' @export
IqrCapabilityTask <- R6::R6Class("IqrCapabilityTask",
  inherit = IqrTaskBase,
  public = list(
    plan = NULL,

    #' @description Create a task instance
    #' @param data Data frame.
    #' @param measurement Measurement column name.
    #' @param plan CapabilityPlan object.
    #' @param theme IqrTheme object or theme name.
    #' @param ... Additional arguments.
    initialize = function(data, measurement, plan, theme = "academic", ...) {
      super$initialize(data, theme, ...)
      self$plan <- plan
      private$measurement <- measurement

      # Instantiate executors
      self$executor$analyzer <- CapabilityAnalyzer$new()
      self$executor$plotter  <- CapabilityPlotter$new()
    },

    #' @description Execute capability analysis computation
    compute = function() {
      x <- self$data[[private$measurement]]
      subgroup_vec <- NULL

      # Compute keep mask first, then filter x and subgroup together to keep aligned
      if (!is.null(self$plan$subgroup) && self$plan$subgroup %in% names(self$data)) {
        subgroup_vec <- self$data[[self$plan$subgroup]]
        # Filter both x and subgroup by the same NA mask
        keep <- !is.na(x) & !is.na(subgroup_vec)
        if (any(!keep)) {
          warning("Missing values detected and removed.", call. = FALSE)
        }
        x <- x[keep]
        subgroup_vec <- subgroup_vec[keep]
      } else {
        # No subgroup: only filter x for NAs
        keep <- !is.na(x)
        if (any(!keep)) {
          warning("Missing values detected and removed.", call. = FALSE)
        }
        x <- x[keep]
      }

      # Call analyzer's run method; results are stored in analyzer's results
      self$executor$analyzer$run(x = x, subgroup = subgroup_vec, plan = self$plan)
      # Promote analyzer results to the task level
      self$results <- self$executor$analyzer$get_results()
      invisible(self)
    },

    #' @description Print summary information
    summary = function() {
      if (is.null(self$results)) {
        cat("No results yet. Run $compute() first.\n")
        return(invisible(self))
      }
      stats <- self$results$statistics
      diag <- self$results$diagnostics
      ppm_overall <- self$results$data_tables$ppm_overall

      # Determine analysis type
      is_nonnormal <- self$plan$analysis_type == "nonnormal"
      is_nonparametric <- self$plan$analysis_type == "nonparametric"

      if (is_nonparametric) {
        cat("\n========== Process Capability Analysis (Non-Parametric) ==========\n")
        cat(sprintf("Specifications: LSL = %.4f, USL = %.4f\n", self$plan$lsl, self$plan$usl))
        cat(sprintf("Sample size: %d\n", stats$n))
        cat(sprintf("Mean (Overall): %.4f\n", stats$mean))
        cat(sprintf("StdDev (Overall): %.4f\n", stats$sd_overall))
        cat(sprintf("Method: %s\n", diag$method))
        if (stats$n < 50) {
          cat("Note: Small sample size may affect the reliability of nonparametric analysis.\n")
        }
      } else if (is_nonnormal) {
        cat("\n========== Process Capability Analysis (Non-Normal) ==========\n")
        cat(sprintf("Specifications: LSL = %.4f, USL = %.4f\n", self$plan$lsl, self$plan$usl))
        cat(sprintf("Sample size: %d\n", stats$n))
        cat(sprintf("Mean (Overall): %.4f\n", stats$mean))
        cat(sprintf("StdDev (Overall): %.4f\n", stats$sd_overall))
        cat(sprintf("Distribution: %s\n", diag$distribution))
        if (!is.null(diag$aic)) cat(sprintf("AIC: %.2f\n", diag$aic))
        if (!is.null(diag$bic)) cat(sprintf("BIC: %.2f\n", diag$bic))
        if (!is.null(diag$ks_p_value)) cat(sprintf("Kolmogorov-Smirnov p-value: %.4f\n", diag$ks_p_value))
      } else {
        ppm_within <- self$results$data_tables$ppm_within
        cat("\n========== Process Capability Analysis (Normal) ==========\n")
        cat(sprintf("Specifications: LSL = %.4f, USL = %.4f\n", self$plan$lsl, self$plan$usl))
        cat(sprintf("Sample size: %d\n", stats$n))
        cat(sprintf("Mean (Overall): %.4f\n", stats$mean))
        cat(sprintf("StdDev (Within): %.4f\n", stats$sd_within))
        cat(sprintf("StdDev (Overall): %.4f\n", stats$sd_overall))
      }

      cat("\n--- Capability Indices ---\n")
      cat(sprintf("Cp  = %.4f\n", stats$cp))
      cat(sprintf("Cpk = %.4f\n", stats$cpk))
      cat(sprintf("Pp  = %.4f\n", stats$pp))
      cat(sprintf("Ppk = %.4f\n", stats$ppk))
      if (!is.null(stats$cpm)) cat(sprintf("Cpm = %.4f\n", stats$cpm))

      cat("\n--- Expected PPM ---\n")
      if (!is_nonnormal && !is_nonparametric && !is.null(ppm_within)) {
        cat(sprintf("Within: Below LSL = %.0f, Above USL = %.0f, Total = %.0f\n",
                    ppm_within$PPM[1], ppm_within$PPM[2], ppm_within$PPM[3]))
      }
      if (!is.null(ppm_overall)) {
        cat(sprintf("Overall: Below LSL = %.0f, Above USL = %.0f, Total = %.0f\n",
                    ppm_overall$PPM[1], ppm_overall$PPM[2], ppm_overall$PPM[3]))
      }

      cat("\n--- Diagnosis ---\n")
      if (is_nonparametric) {
        cat("Non-parametric analysis based on empirical distribution.\n")
      } else if (!is_nonnormal && !is.null(diag$normality_p_value)) {
        cat(sprintf("Normality test (%s): p-value = %.4f\n", diag$normality_method, diag$normality_p_value))
      }
      if (length(diag$warnings) > 0) {
        cat("Warnings:\n")
        for (w in diag$warnings) cat("  ", w, "\n")
      }
      cat("==========================================================\n")
      invisible(self)
    },

    #' @description Generate plots
    #' @param type Plot type: "basic", "qq", or "full".
    #' @param theme Optional temporary theme override.
    #' @param ... Additional arguments passed to the plotter.
    plot = function(type = "full", theme = NULL, ...) {
      if (is.null(self$results)) stop("No results. Run $compute() first.", call. = FALSE)
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

    #' @description Generate report
    #' @param format Report format: "excel", "html", "pdf", "docx", or "word".
    #' @param path Output file path. If NULL, a default path is generated.
    #' @param ... Additional arguments passed to the reporter.
    report = function(format = "excel", path = NULL, ...) {
      if (is.null(self$results)) stop("No results. Run $compute() first.", call. = FALSE)
      # Use IqrReporter from the framework; do not depend on global option
      reporter <- iQualityR.core::IqrReporter$new(self$theme_obj)
      reporter$export(
        results = self$results,
        plan = self$plan,
        task_tag = "capability",
        format = format,
        path = path,
        ...
      )
      invisible(self)
    }
  ),
  private = list(
    measurement = NULL
  )
)
