# =============================================================================
# File: R/iqr_htest.R
# Description: Hypothesis test module user entry point (L3 integrator).
#              Per Contract 2 (STAT_ANALYSIS_PLAN.md v2.0): exposes the unified
#              5-method R6 surface ($new / $run / $plot / $interpret / $report)
#              plus 4 convenience functions (htest_run / htest_plot /
#              htest_interpret / htest_report). $run returns a stat_result;
#              $plot / $report delegate to L2 with the fixed 4-param signature.
# =============================================================================

#' @title iqr_htest: Hypothesis test entry class
#' @description
#' Top-level L3 integrator for the iQualityR hypothesis test module, coordinating
#' computation (L1 `HTestAnalyzer`), plotting (L2 `HTestPlotter`), reporting
#' (L2 `HTestReporter`), and interpretation (`StatInterpreter`).
#'
#' **Supported test types**:
#' - `z_test_1s`: One-sample Z test (population standard deviation known)
#' - `t_test_1s`: One-sample t test (population standard deviation unknown)
#' - `t_test_2s`: Two-sample t test (independent samples)
#' - `t_test_paired`: Paired t test
#' - `prop_test_1s`: One-sample proportion test
#' - `prop_test_2s`: Two-sample proportion test
#' - `f_test`: Variance equality test (F test)
#' - `chisq_test`: Chi-square test
#' - `wilcoxon_signed_rank`: Wilcoxon signed rank test (one-sample / paired)
#' - `wilcoxon_rank_sum`: Wilcoxon rank sum test (Mann-Whitney U)
#' - `kruskal_wallis`: Kruskal-Wallis rank sum test (k independent groups)
#' - `friedman`: Friedman rank sum test (randomized complete block)
#' - `tost_mean`: TOST for mean equivalence (one-sample / two-sample)
#' - `tost_proportion`: TOST for two-sample proportion equivalence
#' - `non_inferiority`: Non-inferiority test (mean or proportion, one-sided)
#' - `superiority`: Superiority test (mean or proportion, one-sided)
#' - `poisson_test_1s`: One-sample Poisson rate test (exact)
#' - `poisson_test_2s`: Two-sample Poisson rate test (rate ratio, exact)
#' - `cor_test_pearson`: Pearson product-moment correlation test
#' - `cor_test_spearman`: Spearman's rank correlation test
#' - `cor_test_kendall`: Kendall's tau correlation test
#' - `levene_test`: Levene's test for equality of variances (k groups, robust)
#' - `bartlett_test`: Bartlett's test for equality of variances (k groups, normality assumed)
#'
#' **Dual interface design** (per Contract 2):
#' - R6 class interface: `iqr_htest$new()$run()$plot()` (suitable for chaining)
#' - Convenience function interface: `htest_run()`, `htest_plot()`,
#'   `htest_interpret()`, `htest_report()` (suitable for one-time use)
#'
#' @examples
#' # One-sample t-test: does mean of x differ from 100?
#' set.seed(123)
#' x <- rnorm(30, mean = 102, sd = 5)
#' htest <- iqr_htest$new()
#' htest$run("t_test_1s", x = x, mu = 100, alternative = "two.sided")
#' htest$interpret(audience = "manager")
#' result <- htest_run("t_test_1s", x = x, mu = 100)
#'
#' # Wilcoxon rank sum test on two independent samples (no normality assumption)
#' set.seed(123)
#' g1 <- rnorm(20, mean = 50, sd = 5)
#' g2 <- rnorm(20, mean = 55, sd = 5)
#' htest$run("wilcoxon_rank_sum", x = g1, y = g2, alternative = "two.sided")
#'
#' # Kruskal-Wallis across 3 groups (pass x as a list of vectors)
#' htest$run("kruskal_wallis", x = list(g1, g2, rnorm(20, mean = 60, sd = 5)))
#'
#' # TOST for mean equivalence: is mean of x practically equal to 50 (within +/-1)?
#' set.seed(123)
#' x <- rnorm(100, mean = 50.3, sd = 5)
#' htest$run("tost_mean", x = x, mu = 50, delta = 1.0)
#'
#' # Non-inferiority test for two proportions (treatment not worse than control by delta)
#' htest$run("non_inferiority", type = "proportion",
#'           x1 = 45, n1 = 100, x2 = 42, n2 = 100, delta = 0.1)
#'
#' # One-sample Poisson rate test: 12 defects observed over 2 hours, H0 rate = 5/hour
#' htest$run("poisson_test_1s", x = 12, T_exposure = 2, r = 5)
#'
#' # Two-sample Poisson rate test: compare defect rates of two lines
#' htest$run("poisson_test_2s", x1 = 15, T1 = 3, x2 = 25, T2 = 3)
#'
#' # Pearson correlation test: is temperature correlated with yield?
#' set.seed(123)
#' temp <- rnorm(30, mean = 80, sd = 5)
#' yield <- 50 + 0.6 * temp + rnorm(30, sd = 2)
#' htest$run("cor_test_pearson", x = temp, y = yield)
#'
#' # Spearman rank correlation (robust to non-linear monotonic relations)
#' htest$run("cor_test_spearman", x = temp, y = yield)
#'
#' # Levene's test: do 3 production lines have equal variance in diameter?
#' set.seed(123)
#' line_a <- rnorm(20, mean = 10, sd = 0.5)
#' line_b <- rnorm(20, mean = 10, sd = 0.8)
#' line_c <- rnorm(20, mean = 10, sd = 1.2)
#' htest$run("levene_test", x = list(line_a, line_b, line_c))
#'
#' # Bartlett's test: same question, assuming normality (more powerful if normal)
#' htest$run("bartlett_test", x = list(line_a, line_b, line_c))
#'
#' # Plotting requires the iQualityR.plot Suggests package
#' if (requireNamespace("iQualityR.plot", quietly = TRUE)) {
#'   htest$plot()
#'   htest_plot(result, plot_type = "combined")
#' }
#'
#' @export
iqr_htest <- R6::R6Class("iqr_htest",
  public = list(
    #' @field last_results Cached computation result (a `stat_result` S3 object).
    last_results = NULL,
    #' @field analyzer L1 computation engine (`HTestAnalyzer`).
    analyzer = NULL,
    #' @field plotter L2 plotting engine (`HTestPlotter`).
    plotter = NULL,
    #' @field reporter L2 reporting engine (`HTestReporter`).
    reporter = NULL,
    #' @field interpreter L2 interpreter (`StatInterpreter`).
    interpreter = NULL,
    #' @field theme_obj Active `IqrTheme` object.
    theme_obj = NULL,

    #' @description Initialize the hypothesis test module
    #' @param theme Theme name or `IqrTheme` object
    #' @param ... Forwarded to `IqrTheme$new()` when `theme` is a string.
    #' @return An `iqr_htest` object (invisibly).
    initialize = function(theme = "academic", ...) {
      self$analyzer     <- HTestAnalyzer$new()
      self$plotter      <- HTestPlotter$new(theme = theme)
      self$reporter     <- HTestReporter$new(theme = theme)
      self$interpreter  <- StatInterpreter$new()
      self$theme_obj    <- .resolve_theme(theme)
      invisible(self)
    },

    #' @description Execute a hypothesis test
    #'
    #' Runs the L1 analyzer and caches the `stat_result` on `self$last_results`.
    #' Optionally prints a console report and/or plots in one call (for
    #' convenience in interactive use).
    #'
    #' @param test_type Test type code (see class description).
    #' @param ... Test parameters forwarded to `HTestAnalyzer$analyze()`.
    #' @param plot Logical; print a plot immediately after running.
    #' @param plot_type Plot type forwarded to `$plot()`.
    #' @param interpret Logical; print an interpretation immediately.
    #' @param audience Audience level for the interpretation.
    #' @return Invisible self (for chaining). The `stat_result` is stored on
    #'   `self$last_results`.
    run = function(test_type, ..., plot = FALSE, plot_type = "auto",
                   interpret = FALSE, audience = "manager") {
      # Execute computation; the stat_result embeds raw data on its `data` field
      # so downstream $plot() no longer needs a separate last_data cache.
      self$last_results <- self$analyzer$analyze(test_type, ...)

      # Console report (uses stat_result format() method).
      self$reporter$print_console(self$last_results, interpret = FALSE)

      if (plot) {
        p <- self$plot(plot_type = plot_type)
        print(p)
      }

      if (interpret) {
        self$interpret(audience = audience)
      }

      invisible(self)
    },

    #' @description Plot the last result (Contract 2 signature)
    #' @param plot_type One of `"auto"`, `"curve"`, `"box"`, `"combined"`.
    #' @param show_table Logical; overlay a stats table on box plots.
    #' @param theme_obj Optional `IqrTheme` overriding the module-level theme.
    #' @return A `ggplot` or `patchwork` object.
    plot = function(plot_type = "auto", show_table = FALSE, theme_obj = NULL) {
      if (is.null(self$last_results)) {
        stop("[iqr_htest] Please run $run() first.", call. = FALSE)
      }
      self$plotter$plot(
        result    = self$last_results,
        plot_type = plot_type,
        show_table = show_table,
        theme_obj = theme_obj %||% self$theme_obj
      )
    },

    #' @description Interpret the last result
    #' @param audience Audience level (`"manager"`, `"technical"`, `"client"`).
    #' @param context Optional business context string.
    #' @return Interpretation string (invisibly; also cat'd to stdout).
    interpret = function(audience = "manager", context = NULL) {
      if (is.null(self$last_results)) {
        stop("[iqr_htest] Please run $run() first.", call. = FALSE)
      }
      explanation <- self$interpreter$interpret(
        self$last_results,
        audience = audience,
        context = context
      )
      cat(explanation, "\n")
      invisible(explanation)
    },

    #' @description Report the last result (Contract 2 signature)
    #'
    #' Unified entry point: dispatches to console / data.frame / excel output.
    #'
    #' @param format Output format: `"data.frame"` (default), `"console"`, `"excel"`.
    #' @param path File path for `format = "excel"`. Auto-timestamped if NULL.
    #' @param audience Audience level for console interpretation.
    #' @return For `"data.frame"`: a data frame. For `"console"`/`"excel"`:
    #'   invisible NULL.
    report = function(format = c("data.frame", "console", "excel"),
                      path = NULL, audience = "manager") {
      if (is.null(self$last_results)) {
        stop("[iqr_htest] Please run $run() first.", call. = FALSE)
      }
      self$reporter$report(
        result   = self$last_results,
        format   = format,
        path     = path,
        audience = audience
      )
    },

    #' @description One-click analysis (run + plot + interpret)
    #' @param test_type Test type code.
    #' @param ... Test parameters.
    #' @param plot_type Plot type.
    #' @param audience Audience level.
    #' @param context Optional business context.
    #' @return Invisible self (for chaining).
    analyze = function(test_type, ..., plot_type = "auto",
                       audience = "manager", context = NULL) {
      self$run(test_type, ..., plot = TRUE, plot_type = plot_type,
               interpret = TRUE, audience = audience)
      invisible(self)
    },

    #' @description Set / replace the active theme
    #' @param theme_style Theme name or `IqrTheme` object.
    #' @return Invisible self.
    set_theme = function(theme_style = NULL) {
      self$theme_obj <- .resolve_theme(theme_style %||% "academic")
      self$plotter$set_theme(self$theme_obj)
      self$reporter$theme_obj <- self$theme_obj
      invisible(self)
    }
  )
)


# =============================================================================
# Convenience functions (stateless interface, suitable for one-time use)
# Per Contract 2: 4 convenience functions per domain.
# =============================================================================

#' @title Convenience hypothesis test function
#' @description
#' Execute a hypothesis test without creating an R6 object. Suitable for quick
#' analysis scenarios. Returns a `stat_result` S3 object so the caller can
#' still feed it to `htest_plot()` / `htest_interpret()` / `htest_report()`.
#'
#' @param test_type Test type code (see [iqr_htest]).
#' @param ... Test parameters forwarded to `HTestAnalyzer$analyze()`.
#' @param plot Logical; print a plot immediately.
#' @param plot_type Plot type forwarded to `htest_plot()`.
#' @param interpret Logical; print an interpretation immediately.
#' @param audience Audience level for the interpretation.
#' @param theme Theme name or `IqrTheme` object.
#' @return A `stat_result` S3 object (invisibly).
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 102, sd = 5)
#' result <- htest_run("t_test_1s", x = x, mu = 100)
#' print(result)
htest_run <- function(test_type, ..., plot = FALSE, plot_type = "auto",
                      interpret = FALSE, audience = "manager", theme = "academic") {
  htest <- iqr_htest$new(theme = theme)
  htest$run(test_type, ..., plot = plot, plot_type = plot_type,
            interpret = interpret, audience = audience)
  invisible(htest$last_results)
}

#' @title Convenience hypothesis test plotting function
#' @description
#' Plot a `stat_result` returned by [htest_run()] (or by `HTestAnalyzer`
#' directly). Delegates to `HTestPlotter$plot()` with the unified Contract 2
#' signature.
#'
#' @param result A `stat_result` from [htest_run()] or `HTestAnalyzer`.
#' @param plot_type One of `"auto"`, `"curve"`, `"box"`, `"combined"`.
#' @param show_table Logical; overlay a stats table on box plots.
#' @param theme Theme name or `IqrTheme` object.
#' @return A `ggplot` or `patchwork` object.
#' @export
#'
#' @examples
#' # Plotting requires the iQualityR.plot Suggests package
#' if (requireNamespace("iQualityR.plot", quietly = TRUE)) {
#'   set.seed(123)
#'   x <- rnorm(30, mean = 102, sd = 5)
#'   result <- htest_run("t_test_1s", x = x, mu = 100)
#'   htest_plot(result, plot_type = "combined")
#' }
htest_plot <- function(result, plot_type = "auto", show_table = FALSE,
                       theme = "academic") {
  plotter <- HTestPlotter$new(theme = theme)
  plotter$plot(result, plot_type = plot_type, show_table = show_table)
}

#' @title Convenience hypothesis test interpretation function
#' @description
#' Interpret a `stat_result` returned by [htest_run()] without creating an
#' R6 object.
#'
#' @param result A `stat_result` from [htest_run()] or `HTestAnalyzer`.
#' @param audience Audience level (`"manager"`, `"technical"`, `"client"`).
#' @param context Optional business context string.
#' @return Interpretation string (invisibly; also cat'd to stdout).
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 102, sd = 5)
#' result <- htest_run("t_test_1s", x = x, mu = 100)
#' htest_interpret(result, audience = "manager")
htest_interpret <- function(result, audience = "manager", context = NULL) {
  interpreter <- StatInterpreter$new()
  explanation <- interpreter$interpret(result, audience = audience, context = context)
  cat(explanation, "\n")
  invisible(explanation)
}

#' @title Convenience hypothesis test report function
#' @description
#' Report a `stat_result` returned by [htest_run()] without creating an R6
#' object. Dispatches on `format` to console / data.frame / excel output.
#'
#' @param result A `stat_result` from [htest_run()] or `HTestAnalyzer`.
#' @param format Output format: `"data.frame"` (default), `"console"`, `"excel"`.
#' @param path File path for `format = "excel"`. Auto-timestamped if NULL.
#' @param audience Audience level for console interpretation.
#' @param theme Theme name or `IqrTheme` object (for Excel styling).
#' @return For `"data.frame"`: a data frame. For `"console"`/`"excel"`:
#'   invisible NULL.
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 102, sd = 5)
#' result <- htest_run("t_test_1s", x = x, mu = 100)
#' htest_report(result, format = "data.frame")
htest_report <- function(result, format = c("data.frame", "console", "excel"),
                         path = NULL, audience = "manager", theme = "academic") {
  reporter <- HTestReporter$new(theme = theme)
  reporter$report(result, format = format, path = path, audience = audience)
}
