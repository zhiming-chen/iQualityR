# =============================================================================
# File: R/MultivarCapabilityPlan.R
# Description: Plan + Task + Plotter + wrapper for multivariate capability.
# =============================================================================

#' MultivarCapabilityPlan
#'
#' @title MultivarCapabilityPlan
#'
#' @description Configuration for multivariate process capability analysis.
#'   Inherits `IqrPlanBase`.
#'
#' @field lsl_vec Numeric vector of lower specs (length p).
#' @field usl_vec Numeric vector of upper specs (length p).
#' @field target_vec Optional target vector.
#' @field method Reserved for future: `"both"` (default), `"mcpv"`, `"hpci"`.
#'
#' @export
MultivarCapabilityPlan <- R6::R6Class("MultivarCapabilityPlan",
  inherit = IqrPlanBase,
  public = list(
    lsl_vec = NULL,
    usl_vec = NULL,
    target_vec = NULL,
    method = "both",

    #' @description Create a new multivariate capability plan
    #' @param lsl_vec Numeric vector of lower specs.
    #' @param usl_vec Numeric vector of upper specs.
    #' @param target_vec Optional target vector.
    #' @param method Reserved: `"both"`, `"mcpv"`, or `"hpci"`.
    #' @param conf_level Confidence level.
    #' @param task_tag Task tag.
    #' @param ... Additional arguments.
    initialize = function(lsl_vec, usl_vec, target_vec = NULL,
                          method = "both", conf_level = 0.95,
                          task_tag = "multivar_capability", ...) {
      super$initialize(task_tag = task_tag, conf_level = conf_level, ...)
      if (length(lsl_vec) != length(usl_vec)) {
        stop("lsl_vec and usl_vec must have the same length.", call. = FALSE)
      }
      if (any(lsl_vec >= usl_vec)) {
        stop("Each LSL must be strictly less than its USL.", call. = FALSE)
      }
      if (!is.null(target_vec) && length(target_vec) != length(lsl_vec)) {
        stop("target_vec must have the same length as lsl_vec.", call. = FALSE)
      }
      self$lsl_vec <- lsl_vec
      self$usl_vec <- usl_vec
      self$target_vec <- target_vec
      self$method <- match.arg(method, c("both", "mcpv", "hpci"))
      # Multivariate capability has no universal acceptance threshold.
      # Use MCPV_p >= 1 as a default "pass" criterion (process volume < spec volume).
      self$set_criteria(mcpv_p = 1.0, npc = 1.0)
      invisible(self)
    },

    #' @description Export configuration as a list (overrides base method)
    to_list = function() {
      base_list <- super$to_list()
      base_list$lsl_vec <- self$lsl_vec
      base_list$usl_vec <- self$usl_vec
      base_list$target_vec <- self$target_vec
      base_list$method <- self$method
      base_list
    }
  )
)

# =============================================================================
# MultivariateCapabilityPlotter
# =============================================================================

# Internal: format multivariate capability value for display
.fmt_mv <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !is.finite(x)) return("NA")
  formatC(signif(x, 4), format = "fg")
}

#' MultivariateCapabilityPlotter
#'
#' @title MultivariateCapabilityPlotter
#'
#' @description Thin delegation plotter for multivariate process capability
#'   analysis (MCPV / HPCI). Dispatches every panel to the matching
#'   `iQualityR.plot::plot_*` function. No ggplot2 geom_* / stat_* logic
#'   lives here.
#'
#' @export
MultivariateCapabilityPlotter <- R6::R6Class("MultivariateCapabilityPlotter",
  inherit = IqrPlotterBase,
  public = list(

    #' @description Render a plot by type.
    #' @param results Analysis results list.
    #' @param theme_obj IqrTheme object.
    #' @param type Plot type: "full" (Sixpack), "ellipse", "mcpv_bar",
    #'   "marginal", "t2", "mcpv", "joint_ppm".
    #' @param plan MultivarCapabilityPlan object.
    #' @param ... Additional arguments (unused).
    render = function(results, theme_obj, type = "full", plan, ...) {
      if (is.null(results)) stop("No results.", call. = FALSE)
      switch(type,
        full      = self$.plot_sixpack(results, theme_obj, plan),
        ellipse   = self$.plot_ellipse(results, theme_obj, plan),
        mcpv_bar  = self$.plot_mcpv_bar(results, theme_obj),
        marginal  = self$.plot_marginal(results, theme_obj),
        t2        = self$.plot_t2(results, theme_obj),
        mcpv      = self$.plot_mcpv(results, theme_obj),
        joint_ppm = self$.plot_joint_ppm(results, theme_obj),
        stop("Unknown plot type: ", type, call. = FALSE)
      )
    },

    # -----------------------------------------------------------------
    # Sixpack (full): 6 panels in 3x2 layout
    # Row 1: ellipse | mcpv_bar
    # Row 2: marginal | t2
    # Row 3: mcpv | joint_ppm
    # -----------------------------------------------------------------
    .plot_sixpack = function(results, theme_obj, plan) {
      panels <- list(
        ellipse   = self$.plot_ellipse(results, theme_obj, plan),
        mcpv_bar  = self$.plot_mcpv_bar(results, theme_obj),
        marginal  = self$.plot_marginal(results, theme_obj),
        t2        = self$.plot_t2(results, theme_obj),
        mcpv      = self$.plot_mcpv(results, theme_obj),
        joint_ppm = self$.plot_joint_ppm(results, theme_obj)
      )
      iQualityR.plot::plot_multivariate_sixpack(
        panels   = panels,
        title    = sprintf("Multivariate Capability Sixpack (p = %d)",
                          results$statistics$p),
        subtitle = self$.build_subtitle(results),
        theme    = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Panel 1: Bivariate spec ellipse (only for p == 2)
    # -----------------------------------------------------------------
    .plot_ellipse = function(results, theme_obj, plan) {
      p <- results$statistics$p
      if (p != 2) {
        return(self$.placeholder(results, theme_obj,
          "Spec Ellipse", "Available only for p = 2."))
      }
      X <- results$data_tables$X_matrix
      specs <- results$data_tables$spec_region
      ctq_names <- results$statistics$ctq_names
      mean_vec <- results$statistics$mean_vector
      target_vec <- plan$target_vec
      iQualityR.plot::plot_spec_ellipse(
        X          = X,
        specs      = specs,
        ctq_names  = ctq_names,
        mean_vec   = mean_vec,
        target_vec = target_vec,
        theme      = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Panel 2: HPCI three-component bar (Shahriari 1995)
    # -----------------------------------------------------------------
    .plot_mcpv_bar = function(results, theme_obj) {
      s <- results$statistics
      iQualityR.plot::plot_mcpv_bar(
        npc   = s$npc,
        pv    = s$pv,
        lri   = s$lri,
        theme = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Panel 3: Marginal capability bar chart
    # -----------------------------------------------------------------
    .plot_marginal = function(results, theme_obj) {
      iQualityR.plot::plot_marginal_capability_matrix(
        per_ctq = results$data_tables$per_ctq,
        theme   = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Panel 4: Hotelling T^2 control chart
    # -----------------------------------------------------------------
    .plot_t2 = function(results, theme_obj) {
      d2_df <- results$data_tables$mahalanobis_d2
      ucl <- results$statistics$chi2_q_9973
      iQualityR.plot::plot_hotelling_t2(
        t2_values = d2_df$d2,
        ucl       = ucl,
        theme     = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Panel 5: MCPV volume ratio
    # -----------------------------------------------------------------
    .plot_mcpv = function(results, theme_obj) {
      s <- results$statistics
      iQualityR.plot::plot_mcpv_volume(
        v_spec   = s$V_spec,
        v_process = s$V_process,
        mcpv_p   = s$mcpv_p,
        theme    = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Panel 6: Joint yield & PPM summary bar
    # -----------------------------------------------------------------
    .plot_joint_ppm = function(results, theme_obj) {
      s <- results$statistics
      verdict <- results$diagnostics$capability_judgment$overall_verdict
      if (is.null(verdict) || (length(verdict) == 1 && is.na(verdict))) {
        verdict <- s$hpci_overall
      }
      iQualityR.plot::plot_joint_ppm_bar(
        yield_prob   = s$yield_prob,
        ppm_expected = s$ppm_expected,
        verdict      = verdict,
        theme        = theme_obj
      )
    },

    # -----------------------------------------------------------------
    # Helpers
    # -----------------------------------------------------------------
    .build_subtitle = function(results) {
      s <- results$statistics
      sprintf("MCPV = %s | HPCI: NPC=%s PV=%.3f LRI=%.3f | %s",
              .fmt_mv(s$mcpv_p), .fmt_mv(s$npc), s$pv, s$lri,
              toupper(s$hpci_overall))
    },

    .placeholder = function(results, theme_obj, title, message) {
      df <- data.frame(x = 0.5, y = 0.5, label = message)
      ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_text(ggplot2::aes(label = .data$label),
                           color = "gray50", size = 4) +
        ggplot2::labs(title = title, x = NULL, y = NULL) +
        ggplot2::theme_void() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }
  )
)

# =============================================================================
# IqrMultivarCapabilityTask
# =============================================================================

#' IqrMultivarCapabilityTask
#'
#' @title IqrMultivarCapabilityTask
#'
#' @description Task coordinator for multivariate capability analysis.
#'   Inherits `IqrTaskBase`.
#'
#' @param data Data frame with CTQ columns.
#' @param ctqs Character vector of CTQ column names.
#' @param plan [MultivarCapabilityPlan] object.
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#'
#' @export
IqrMultivarCapabilityTask <- R6::R6Class("IqrMultivarCapabilityTask",
  inherit = IqrTaskBase,
  public = list(
    plan = NULL,

    #' @description Create a task instance
    initialize = function(data, ctqs, plan, theme = "academic", ...) {
      super$initialize(data, theme, ...)
      self$plan <- plan
      private$ctqs <- ctqs
      self$executor$analyzer <- MultivariateCapabilityAnalyzer$new()
      self$executor$plotter  <- MultivariateCapabilityPlotter$new()
    },

    #' @description Execute multivariate capability analysis
    compute = function() {
      X <- self$data[, private$ctqs, drop = FALSE]
      # NA-mask is delegated to the analyzer (it does complete.cases)
      self$executor$analyzer$run(
        X = X, lsl_vec = self$plan$lsl_vec,
        usl_vec = self$plan$usl_vec,
        target_vec = self$plan$target_vec,
        plan = self$plan)
      self$results <- self$executor$analyzer$get_results()
      invisible(self)
    },

    #' @description Print summary
    summary = function() {
      if (is.null(self$results)) {
        cat("No results yet. Run $compute() first.\n")
        return(invisible(self))
      }
      stats <- self$results$statistics
      verdict <- self$results$diagnostics$capability_judgment
      t2 <- self$results$diagnostics$hotelling_t2

      cat(sprintf("\n===== Multivariate Capability (p = %d, n = %d) =====\n",
                  stats$p, stats$n))

      cat("\n--- Volumes ---\n")
      cat(sprintf("  Spec region volume:           %.4f\n", stats$V_spec))
      cat(sprintf("  Process 99.73%% ellipsoid:     %.4f\n", stats$V_process))

      cat("\n--- MCPV (Taam 1993) ---\n")
      cat(sprintf("  MCPV_p  (Cp-equiv):  %.4f\n", stats$mcpv_p))
      cat(sprintf("  MCPV_pk (Cpk-equiv): %.4f   (penalty = %.2f%%)\n",
                  stats$mcpv_pk, 100 * stats$centering_penalty))

      cat("\n--- HPCI (Shahriari 1995) ---\n")
      cat(sprintf("  npc (volume ratio):     %.4f  %s\n",
                  stats$npc, ifelse(verdict$hpci_pass_volume, "(pass)", "(fail)")))
      cat(sprintf("  pv  (T^2 centering p):  %.4f  %s\n",
                  ifelse(is.na(stats$pv), NA, stats$pv),
                  ifelse(verdict$hpci_pass_center, "(pass)", "(fail)")))
      cat(sprintf("  lri (location ratio):    %.4f  %s\n",
                  stats$lri, ifelse(verdict$hpci_pass_location, "(pass)", "(fail)")))

      cat(sprintf("\nHotelling T^2 = %.3f, F(%d,%d) = %.3f, p = %.4f\n",
                  ifelse(is.na(t2$statistic), NA, t2$statistic),
                  t2$df1, t2$df2,
                  ifelse(is.na(t2$F_stat), NA, t2$F_stat),
                  ifelse(is.na(t2$p_value), NA, t2$p_value)))

      cat("\n--- Yield (joint) ---\n")
      cat(sprintf("  P(X in spec region) = %.4f%%\n",
                  100 * ifelse(is.na(stats$yield_prob), NA, stats$yield_prob)))
      cat(sprintf("  Expected PPM        = %.1f\n",
                  ifelse(is.na(stats$ppm_expected), NA, stats$ppm_expected)))

      cat("\n--- Per-CTQ univariate ---\n")
      print(self$results$data_tables$per_ctq[, c("CTQ", "LSL", "USL", "Target",
                                                  "Mean", "Cpk", "In_Spec")],
            row.names = FALSE)

      cat(sprintf("\nOverall verdict: %s\n", toupper(verdict$overall_verdict)))

      if (length(self$results$diagnostics$warnings) > 0) {
        cat("\n--- Warnings ---\n")
        for (w in self$results$diagnostics$warnings) cat("  ", w, "\n")
      }
      cat("========================================\n")
      invisible(self)
    },

    #' @description Render plot
    plot = function(type = "full", theme = NULL, ...) {
      if (is.null(self$results)) stop("No results. Run $compute() first.", call. = FALSE)
      theme_obj_use <- if (!is.null(theme)) {
        if (inherits(theme, "IqrTheme")) theme else IqrTheme$new(theme)
      } else self$theme_obj
      self$executor$plotter$render(
        results = self$results, theme_obj = theme_obj_use,
        type = type, plan = self$plan, ...)
    },

    #' @description Generate Excel report
    report = function(format = "excel", path = NULL, ...) {
      if (is.null(self$results)) stop("No results. Run $compute() first.", call. = FALSE)
      reporter <- iQualityR.core::IqrReporter$new(self$theme_obj)
      reporter$register(
        task_tag = "multivar_capability",
        excel_generator = function(results, plan) {
          list(
            Overview = data.frame(
              Metric = c("p (CTQs)", "n", "MCPV_p", "MCPV_pk",
                         "HPCI_npc", "HPCI_pv", "HPCI_lri",
                         "Yield", "PPM_expected", "Verdict"),
              Value = c(results$statistics$p, results$statistics$n,
                        results$statistics$mcpv_p, results$statistics$mcpv_pk,
                        results$statistics$npc, results$statistics$pv,
                        results$statistics$lri, results$statistics$yield_prob,
                        results$statistics$ppm_expected,
                        results$diagnostics$capability_judgment$overall_verdict),
              stringsAsFactors = FALSE),
            Per_CTQ = results$data_tables$per_ctq,
            Spec_Region = results$data_tables$spec_region,
            Mahalanobis_QQ = results$data_tables$mahalanobis_qq,
            Raw_Data = results$data_tables$raw_data
          )
        }
      )
      reporter$export(
        results = self$results, plan = self$plan,
        task_tag = "multivar_capability",
        format = format, path = path, ...)
      invisible(self)
    }
  ),
  private = list(
    ctqs = NULL
  )
)

# =============================================================================
# Convenience wrapper
# =============================================================================

#' Multivariate process capability analysis
#'
#' Performs multivariate process capability analysis for a set of correlated
#' CTQs. Computes the Taam (1993) MCPV scalar family and the Shahriari-
#' Hubele-Lawrence (1995) HPCI three-component vector. Uses `mvtnorm` for
#' the probability content of the spec hyper-rectangle. Fills a gap that
#' even Minitab v21 and JMP v19 do not fully cover.
#'
#' @param data Data frame with CTQ columns.
#' @param ctqs Character vector of CTQ column names.
#' @param lsl_vec Numeric vector of lower specs (length = `length(ctqs)`).
#' @param usl_vec Numeric vector of upper specs (length = `length(ctqs)`).
#' @param target_vec Optional target vector.
#' @param conf_level Confidence level.
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments.
#' @return An `IqrMultivarCapabilityTask` object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(bore = rnorm(50, 50, 0.5),
#'                  stroke = rnorm(50, 90, 0.8))
#' task <- capability_multivariate(
#'   data = df, ctqs = c("bore", "stroke"),
#'   lsl_vec = c(48, 87), usl_vec = c(52, 93),
#'   target_vec = c(50, 90))
#' task$summary()
#' task$plot(type = "full")
#' }
capability_multivariate <- function(data, ctqs, lsl_vec, usl_vec,
                                     target_vec = NULL,
                                     conf_level = 0.95,
                                     theme = "academic", ...) {
  plan <- MultivarCapabilityPlan$new(
    lsl_vec = lsl_vec, usl_vec = usl_vec,
    target_vec = target_vec, conf_level = conf_level
  )
  task <- IqrMultivarCapabilityTask$new(
    data = data, ctqs = ctqs, plan = plan, theme = theme, ...
  )
  task$compute()
  invisible(task)
}
