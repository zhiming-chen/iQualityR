# =============================================================================
# File: R/anova/anova.R
# Description: ANOVA module for the iQualityR.stat sub-package. Provides the
#              AnovaAnalyzer computation engine, AnovaPlotter plotting
#              coordinator, AnovaReporter report generator, and the
#              iqr_anova / anova_run / anova_report user entry points.
# =============================================================================

# Shared IqrPlotterBase instance used as a stateless color/scale toolbox by
# all AnovaPlotter methods. Routes every color decision through the unified
# IqrPlotterBase helpers (.pal_*, .scale_*, .contrast_text) so hard-coded
# colors are avoided and theme switching stays consistent.
.iqr_plotter <- iQualityR.core::IqrPlotterBase$new()


# -----------------------------------------------------------------------------
# Internal helpers: effect sizes and multiple comparisons
# -----------------------------------------------------------------------------

#' Compute effect sizes (eta squared, partial eta squared, omega squared)
#'
#' @param model An `lm`/`aov` or `lmerMod` model.
#' @param type Effect size type: `"eta"`, `"partial_eta"`, or `"omega"`.
#' @return Numeric vector of effect sizes, or a matrix for mixed models.
#' @keywords internal
.calc_effect_size <- function(model, type = c("eta", "partial_eta", "omega")) {
    type <- match.arg(type)
    if (inherits(model, "lmerMod")) {
        # Mixed models use MuMIn for R-squared GLMM
        if (requireNamespace("MuMIn", quietly = TRUE)) {
            return(MuMIn::r.squaredGLMM(model))
        } else {
            warning("MuMIn package required for mixed model effect size.")
            return(NULL)
        }
    }
    # Standard lm/aov path
    s <- summary(model)
    aov_table <- as.data.frame(s[[1]])
    ss <- aov_table[, "Sum Sq"]
    df <- aov_table[, "Df"]
    ss_total <- sum(ss)
    ss_res <- ss[length(ss)]

    if (type == "eta") {
        eta <- ss / ss_total
        names(eta) <- rownames(aov_table)
        return(eta)
    } else if (type == "partial_eta") {
        partial_eta <- ss / (ss + ss_res)
        names(partial_eta) <- rownames(aov_table)
        return(partial_eta)
    } else {  # omega
        ms <- ss / df
        ms_res <- ss_res / df[length(df)]
        omega <- (ss - df * ms_res) / (ss_total + ms_res)
        names(omega) <- rownames(aov_table)
        return(omega)
    }
}

#' Multiple comparisons
#'
#' @param model An `lm`/`aov` model.
#' @param factor Factor name (for one-way), or a list of factor names for
#'   multi-factor interactions.
#' @param method Adjustment method: `"tukey"`, `"bonferroni"`,
#'   `"dunnett"`, `"lsd"`, or `"scheffe"`.
#' @param alpha Significance level.
#' @return A data frame of multiple comparison results.
#' @keywords internal
.multiple_comparisons <- function(model, factor, method = "tukey", alpha = 0.05) {
    if (!requireNamespace("multcomp", quietly = TRUE)) {
        stop("multcomp package required for multiple comparisons.")
    }
    if (!requireNamespace("emmeans", quietly = TRUE)) {
        stop("emmeans package required for multiple comparisons.")
    }

    # Compute estimated marginal means via emmeans
    emm <- emmeans::emmeans(model, as.formula(paste("~", factor)))
    contrast_method <- switch(method,
                              "tukey" = "pairwise",
                              "bonferroni" = "pairwise",
                              "dunnett" = "dunnett",
                              "lsd" = "pairwise",
                              "scheffe" = "pairwise"
    )
    contrast <- emmeans::contrast(emm, method = contrast_method)
    p_adj <- switch(method,
                    "tukey" = "tukey",
                    "bonferroni" = "bonferroni",
                    "dunnett" = "dunnett",
                    "lsd" = "none",
                    "scheffe" = "scheffe"
    )
    summary_contrast <- summary(contrast, adjust = p_adj, level = 1 - alpha)
    return(summary_contrast)
}


# =============================================================================
# AnovaAnalyzer: ANOVA computation engine (pure computation, no graphics)
# =============================================================================

#' @title AnovaAnalyzer: ANOVA Computation Engine
#' @description
#' Performs analysis of variance and returns structured results.
#'
#' Supports:
#' - One-way ANOVA
#' - Two-way ANOVA (with interaction)
#' - Multi-factor ANOVA
#' - Repeated measures ANOVA
#' - Mixed-effects models (linear mixed models)
#' - Multivariate ANOVA (MANOVA)
#'
#' @export
AnovaAnalyzer <- R6::R6Class("AnovaAnalyzer",
  public = list(
    #' @description One-way ANOVA
    #' @param formula Formula (`response ~ group`).
    #' @param data Data frame.
    #' @param ... Additional arguments passed to `aov`.
    #' @return Structured results list.
    anova_oneway = function(formula, data, ...) {
      private$.anova_oneway(list(formula = formula, data = data, ...))
    },

    #' @description Two-way ANOVA (with interaction)
    #' @param formula Formula (`response ~ factor1 * factor2`).
    #' @param data Data frame.
    #' @param ... Additional arguments.
    #' @return Structured results list.
    anova_twoway = function(formula, data, ...) {
      private$.anova_twoway(list(formula = formula, data = data, ...))
    },

    #' @description Multi-factor ANOVA (3+ factors)
    #' @param formula Formula.
    #' @param data Data frame.
    #' @param ... Additional arguments.
    #' @return Structured results list.
    anova_multifactor = function(formula, data, ...) {
      private$.anova_multifactor(list(formula = formula, data = data, ...))
    },

    #' @description Repeated measures ANOVA
    #' @param formula Formula (`response ~ factor1 + Error(subject/factor)`).
    #' @param data Data frame.
    #' @param ... Additional arguments passed to `aov`.
    #' @return Structured results list.
    anova_repeated = function(formula, data, ...) {
      private$.anova_repeated(list(formula = formula, data = data, ...))
    },

    #' @description Mixed-effects model (linear mixed model)
    #' @param formula Formula (`response ~ fixed + (1|random)`).
    #' @param data Data frame.
    #' @param method Fitting method (`"REML"` or `"ML"`).
    #' @param ... Additional arguments passed to `lmer`.
    #' @return Structured results list.
    anova_mixed = function(formula, data, method = "REML", ...) {
      private$.anova_mixed(list(formula = formula, data = data, method = method, ...))
    },

    #' @description Multivariate ANOVA (MANOVA)
    #' @param formula Formula (`cbind(y1, y2) ~ group`).
    #' @param data Data frame.
    #' @param test Test statistic (`"Wilks"`, `"Pillai"`,
    #'   `"Hotelling-Lawley"`, or `"Roy"`).
    #' @return Structured results list.
    manova = function(formula, data, test = c("Wilks", "Pillai", "Hotelling-Lawley", "Roy")) {
      private$.manova(list(formula = formula, data = data, test = match.arg(test)))
    },

    #' @description Unified entry point: auto-selects method from formula structure
    #' @param formula Formula.
    #' @param data Data frame.
    #' @param ... Additional arguments.
    #' @return Structured results list.
    analyze = function(formula, data, ...) {
      # Detect formula type and dispatch
      private$.detect_and_run(formula, data, ...)
    },

    #' @description Generate a report from ANOVA results
    #' @param results ANOVA results list (from `analyze`). If `NULL`, uses
    #'   cached `last_results`.
    #' @param format Output format (`"excel"`, `"html"`, `"pdf"`, `"docx"`,
    #'   `"pptx"`).
    #' @param path Output file path (optional).
    #' @param theme_obj IqrTheme object (optional).
    #' @return Output file path (invisible).
    report = function(results = NULL, format = "excel", path = NULL, theme_obj = NULL) {
      if (is.null(results)) {
        if (is.null(self$last_results)) {
          stop("No results available. Run analysis first.")
        }
        results <- self$last_results
      }
      if (is.null(theme_obj)) {
        theme_obj <- IqrTheme$new()
      }
      reporter <- AnovaReporter$new(theme_obj)
      reporter$report(results, format = format, path = path)
    }
  ),

  private = list(
    .detect_and_run = function(formula, data, ...) {
      # Detect Error() term -> repeated measures
      if (grepl("Error\\(", deparse(formula))) {
        return(self$anova_repeated(formula, data, ...))
      }
      # Detect random term (1|...) -> mixed model
      if (grepl("\\(1\\|", deparse(formula))) {
        return(self$anova_mixed(formula, data, ...))
      }
      # Detect multivariate response cbind()
      response <- as.character(formula[[2]])
      if (grepl("^cbind\\(", response)) {
        return(self$manova(formula, data, ...))
      }
      # Otherwise decide by number of factors
      factors <- attr(stats::terms(formula), "factors")
      n_factors <- ncol(factors) - 1  # subtract response column
      if (n_factors == 1) {
        return(self$anova_oneway(formula, data, ...))
      } else if (n_factors == 2) {
        return(self$anova_twoway(formula, data, ...))
      } else {
        return(self$anova_multifactor(formula, data, ...))
      }
    },

    .anova_oneway = function(args) {
      model <- do.call(stats::aov, args)
      model_summary <- summary(model)
      anova_table <- as.data.frame(model_summary[[1]])
      # Multiple comparisons
      factors <- labels(stats::terms(model))
      comp <- tryCatch(.multiple_comparisons(model, factors[1], method = "tukey"),
                       error = function(e) NULL)
      # Effect sizes
      eta <- .calc_effect_size(model, type = "eta")
      partial_eta <- .calc_effect_size(model, type = "partial_eta")

      list(
        test_type = "One-way ANOVA",
        formula = deparse(args$formula),
        data_name = deparse(substitute(args$data)),
        model = model,
        anova_table = anova_table,
        summary = model_summary,
        coefficients = coef(model),
        r_squared = summary(model)$r.squared,
        adj_r_squared = summary(model)$adj.r.squared,
        multiple_comparisons = comp,
        effect_size = list(eta = eta, partial_eta = partial_eta),
        residuals = residuals(model),
        fitted = fitted(model),
        n = length(residuals(model)),
        factors = labels(stats::terms(model)),
        method = "aov"
      )
    },

    .anova_twoway = function(args) {
      model <- do.call(stats::aov, args)
      anova_table <- as.data.frame(summary(model)[[1]])
      factors <- labels(stats::terms(model))
      comp <- tryCatch(.multiple_comparisons(model, factors[1], method = "tukey"),
                       error = function(e) NULL)
      eta <- .calc_effect_size(model, type = "eta")
      partial_eta <- .calc_effect_size(model, type = "partial_eta")

      list(
        test_type = "Two-way ANOVA",
        formula = deparse(args$formula),
        data_name = deparse(substitute(args$data)),
        model = model,
        anova_table = anova_table,
        summary = summary(model),
        coefficients = coef(model),
        r_squared = summary(model)$r.squared,
        adj_r_squared = summary(model)$adj.r.squared,
        multiple_comparisons = comp,
        effect_size = list(eta = eta, partial_eta = partial_eta),
        residuals = residuals(model),
        fitted = fitted(model),
        n = length(residuals(model)),
        factors = labels(stats::terms(model)),
        method = "aov"
      )
    },

    .anova_multifactor = function(args) {
      # Similar to two-way but handles more factors
      model <- do.call(stats::aov, args)
      anova_table <- as.data.frame(summary(model)[[1]])
      factors <- labels(stats::terms(model))
      # Multiple comparisons target the first factor
      comp <- tryCatch(.multiple_comparisons(model, factors[1], method = "tukey"),
                       error = function(e) NULL)
      eta <- .calc_effect_size(model, type = "eta")
      partial_eta <- .calc_effect_size(model, type = "partial_eta")

      list(
        test_type = "Multi-factor ANOVA",
        formula = deparse(args$formula),
        data_name = deparse(substitute(args$data)),
        model = model,
        anova_table = anova_table,
        summary = summary(model),
        coefficients = coef(model),
        r_squared = summary(model)$r.squared,
        adj_r_squared = summary(model)$adj.r.squared,
        multiple_comparisons = comp,
        effect_size = list(eta = eta, partial_eta = partial_eta),
        residuals = residuals(model),
        fitted = fitted(model),
        n = length(residuals(model)),
        factors = labels(stats::terms(model)),
        method = "aov"
      )
    },

    .anova_repeated = function(args) {
      model <- do.call(stats::aov, args)
      # Handle Error() structure
      anova_summary <- summary(model)
      # Extract Error strata
      error_terms <- names(anova_summary)
      anova_tables <- lapply(anova_summary, as.data.frame)
      names(anova_tables) <- error_terms

      list(
        test_type = "Repeated Measures ANOVA",
        formula = deparse(args$formula),
        data_name = deparse(substitute(args$data)),
        model = model,
        anova_tables = anova_tables,
        summary = anova_summary,
        coefficients = coef(model),
        residuals = residuals(model),
        fitted = fitted(model),
        n = length(residuals(model)),
        method = "aov"
      )
    },

    .anova_mixed = function(args) {
      if (!requireNamespace("lme4", quietly = TRUE)) {
        stop("lme4 package required for mixed models.")
      }
      if (!requireNamespace("lmerTest", quietly = TRUE)) {
        warning("lmerTest not installed; using lme4 without p-values.")
      }
      # Use lmerTest when available, otherwise fall back to lme4
      dots <- args[!names(args) %in% c("formula", "data", "method")]
      model <- if (requireNamespace("lmerTest", quietly = TRUE)) {
        do.call(lmerTest::lmer, c(list(formula = args$formula, data = args$data,
                                       REML = (args$method == "REML")), dots))
      } else {
        do.call(lme4::lmer, c(list(formula = args$formula, data = args$data,
                                   REML = (args$method == "REML")), dots))
      }
      anova_table <- if (inherits(model, "lmerModLmerTest")) {
        as.data.frame(lmerTest::anova(model))
      } else {
        # Use lme4 anova approximation
        as.data.frame(stats::anova(model))
      }
      # Random effect variances
      var_rand <- as.data.frame(lme4::VarCorr(model))
      # Fixed effect coefficients
      coef_fixed <- lme4::fixef(model)
      # Multiple comparisons for fixed factors
      factors <- labels(stats::terms(model))
      comp <- NULL
      if (length(factors) > 0) {
        comp <- tryCatch(.multiple_comparisons(model, factors[1], method = "tukey"),
                         error = function(e) NULL)
      }

      list(
        test_type = "Linear Mixed Model",
        formula = deparse(args$formula),
        data_name = deparse(substitute(args$data)),
        model = model,
        anova_table = anova_table,
        fixed_effects = coef_fixed,
        random_effects = var_rand,
        multiple_comparisons = comp,
        residuals = residuals(model),
        fitted = fitted(model),
        n = length(residuals(model)),
        method = "lmer"
      )
    },

    .manova = function(args) {
      model <- stats::manova(args$formula, data = args$data)
      summary_manova <- summary(model, test = args$test)
      summary_manova_aov <- summary(model)

      list(
        test_type = "MANOVA",
        formula = deparse(args$formula),
        data_name = deparse(substitute(args$data)),
        model = model,
        summary = summary_manova,
        test_statistic = args$test,
        coefficients = coef(model),
        residuals = residuals(model),
        fitted = fitted(model),
        n = length(residuals(model)),
        method = "manova"
      )
    }
  )
)


# =============================================================================
# AnovaPlotter: ANOVA plotting coordinator (reuses iQualityR.plot functions)
# =============================================================================

#' @title AnovaPlotter: ANOVA Plotting Coordinator
#' @description
#' Plotting coordinator for ANOVA results, built on top of the
#' `iQualityR.plot` package functions. Maximizes reuse of existing
#' plot helpers to minimize duplicated code.
#'
#' @export
AnovaPlotter <- R6::R6Class("AnovaPlotter",
  public = list(
    #' @field theme_obj Active IqrTheme object used to style plots.
    theme_obj = NULL,

    #' @description Initialize the plotter
    #' @param theme Theme name or IqrTheme object.
    #' @return An AnovaPlotter object (invisible).
    initialize = function(theme = "academic") {
      if (inherits(theme, "IqrTheme")) {
        self$theme_obj <- theme
      } else {
        tryCatch({
          self$theme_obj <- IqrTheme$new(theme)
        }, error = function(e) {
          self$theme_obj <<- NULL
        })
      }
      invisible(self)
    },

    #' @description Auto-dispatch plotting
    #' @param result ANOVA result list (from AnovaAnalyzer).
    #' @param plot_type Plot type: `"auto"`, `"residual"`, `"effects"`,
    #'   `"interaction"`, `"comparison"`, `"variance"`, `"f_curve"`,
    #'   `"summary"`.
    #' @param ... Additional arguments passed to the selected plot method.
    #' @return A ggplot2 or patchwork object.
    plot = function(result, plot_type = "auto", ...) {
      plot_type <- match.arg(plot_type, c("auto", "residual", "effects", "interaction",
                                          "comparison", "variance", "f_curve", "summary"))
      if (plot_type == "auto") {
        plot_type <- private$.auto_select(result)
      }
      switch(plot_type,
             "residual"    = self$plot_residuals(result, ...),
             "effects"     = self$plot_effects(result, ...),
             "interaction" = self$plot_interaction(result, ...),
             "comparison"  = self$plot_comparison(result, ...),
             "variance"    = self$plot_variance(result, ...),
             "f_curve"     = self$plot_f_curve(result, ...),
             "summary"     = self$plot_summary(result, ...)
      )
    },

    # ------------------------------------------------------------------------
    # 1. Residual diagnostics -- reuses plot_qq plus custom panels
    # ------------------------------------------------------------------------

    #' @description Residual diagnostic four-panel plot
    #' @param result ANOVA result.
    #' @param add_qq Whether to include the Q-Q plot panel.
    #' @param ... Additional arguments (unused).
    #' @return A patchwork object.
    plot_residuals = function(result, add_qq = TRUE, ...) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("patchwork package required.")
      }

      model <- result$model
      res <- residuals(model)
      fitted <- fitted(model)

      # Q-Q plot via iQualityR.plot::plot_qq (always available in Imports)
      if (add_qq) {
        p_qq <- iQualityR.plot::plot_qq(
          data = data.frame(res = res),
          sample_col = "res",
          dist_family = "norm",
          theme = self$theme_obj,
          add_test = TRUE
        ) + ggplot2::labs(title = "Normal Q-Q Plot")
      } else {
        # Fallback: simple ggplot Q-Q without the test annotation
        p_qq <- ggplot2::ggplot(data.frame(res = res), ggplot2::aes(sample = res)) +
          ggplot2::stat_qq() +
          ggplot2::stat_qq_line(color = .iqr_plotter$.pal_semantic(self$theme_obj, "fail")) +
          ggplot2::labs(title = "Normal Q-Q Plot") +
          as_iqr_theme(self$theme_obj)
      }

      # Residuals vs fitted
      df_rf <- data.frame(fitted = fitted, residual = res)
      p_rf <- base_plot(df_rf, ggplot2::aes(x = fitted, y = residual), theme = self$theme_obj) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            color = .iqr_plotter$.pal_semantic(self$theme_obj, "fail")) +
        ggplot2::geom_smooth(method = "loess", se = TRUE,
                             color = .iqr_plotter$.pal_ui(self$theme_obj, "primary"), alpha = 0.2) +
        ggplot2::labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted")

      # Scale-Location plot
      df_sl <- data.frame(fitted = fitted, sqrt_res = sqrt(abs(scale(res))))
      p_sl <- base_plot(df_sl, ggplot2::aes(x = fitted, y = sqrt_res), theme = self$theme_obj) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::geom_smooth(method = "loess", se = TRUE,
                             color = .iqr_plotter$.pal_ui(self$theme_obj, "primary"), alpha = 0.2) +
        ggplot2::labs(x = "Fitted Values", y = "sqrt(|Standardized Residuals|)",
                      title = "Scale-Location")

      # Residuals by factor (one-way ANOVA) or vs observation order
      if (length(result$factors) == 1 && "model" %in% names(result)) {
        df <- result$model$model
        factor_name <- result$factors[1]
        df_plot <- data.frame(factor = df[[factor_name]], residual = res)
        p_factor <- base_plot(df_plot, ggplot2::aes(x = factor, y = residual),
                              theme = self$theme_obj) +
          layers_boxplot(add_jitter = TRUE,
                         boxplot_args = list(fill = .iqr_plotter$.pal_discrete(self$theme_obj)[1],
                                             alpha = 0.3)) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                              color = .iqr_plotter$.pal_semantic(self$theme_obj, "fail")) +
          ggplot2::labs(x = factor_name, y = "Residuals", title = "Residuals by Factor")
      } else {
        # Residuals vs observation order (time sequence)
        df_seq <- data.frame(index = seq_along(res), residual = res)
        p_factor <- base_plot(df_seq, ggplot2::aes(x = index, y = residual),
                              theme = self$theme_obj) +
          ggplot2::geom_point(alpha = 0.6) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                              color = .iqr_plotter$.pal_semantic(self$theme_obj, "fail")) +
          ggplot2::labs(x = "Observation Order", y = "Residuals", title = "Residuals vs Order")
      }

      # Combine into a 2x2 patchwork
      p <- (p_rf + p_qq) / (p_sl + p_factor) +
        patchwork::plot_annotation(
          title = "Residual Diagnostic Plots",
          subtitle = sprintf("ANOVA: %s", result$formula %||% ""),
          theme = as_iqr_theme(self$theme_obj)
        )
      p
    },

    # ------------------------------------------------------------------------
    # 2. Main effects plot -- reuses plot_scatter_basic
    # ------------------------------------------------------------------------

    #' @description Main effects plot (mean +/- SE)
    #' @param result ANOVA result.
    #' @param factor Factor name. If `NULL`, uses the first factor.
    #' @param show_table Whether to overlay a statistics table.
    #' @param ... Additional arguments (unused).
    #' @return A ggplot2 object.
    plot_effects = function(result, factor = NULL, show_table = FALSE, ...) {
      if (is.null(factor)) factor <- result$factors[1]
      if (is.null(factor)) stop("No factor available for effects plot.")

      # Compute group means and standard errors
      df <- result$model$model
      y_var <- names(df)[1]
      y <- df[[1]]
      x <- df[[factor]]

      means <- tapply(y, x, mean)
      se <- tapply(y, x, function(x) sd(x) / sqrt(length(x)))
      n <- tapply(y, x, length)

      df_plot <- data.frame(
        group = factor(names(means), levels = names(means)),
        mean = as.numeric(means),
        se = as.numeric(se),
        n = as.numeric(n)
      )

      # Direct call: iQualityR.plot::plot_scatter_basic is always available
      p <- iQualityR.plot::plot_scatter_basic(
        data = df_plot,
        x_var = "group",
        y_var = "mean",
        add_regression = FALSE,
        add_correlation = FALSE,
        theme = self$theme_obj,
        title = sprintf("Main Effects Plot: %s", factor),
        subtitle = sprintf("Means +/- 1 SE (n = %s)", paste(df_plot$n, collapse = ", "))
      )

      # Optional: overlay a statistics table (gridExtra fallback is the only
      # path; create_stat_table does not exist in iQualityR.plot, so the
      # dead if-block was removed and gridExtra::tableGrob is used directly).
      if (show_table) {
        table_grob <- gridExtra::tableGrob(df_plot)
        p <- p + ggplot2::annotation_custom(
          grob = table_grob,
          xmin = Inf, xmax = Inf,
          ymin = -Inf, ymax = -Inf
        )
      }

      p
    },

    # ------------------------------------------------------------------------
    # 3. Interaction plot -- reuses plot_interaction_line
    # ------------------------------------------------------------------------

    #' @description Interaction effects plot
    #' @param result ANOVA result.
    #' @param factor1 First factor name. If `NULL`, uses the first factor.
    #' @param factor2 Second factor name. If `NULL`, uses the second factor.
    #' @param ... Additional arguments (unused).
    #' @return A ggplot2 object.
    plot_interaction = function(result, factor1 = NULL, factor2 = NULL, ...) {
      if (is.null(factor1)) factor1 <- result$factors[1]
      if (is.null(factor2)) factor2 <- result$factors[2]

      if (is.null(factor1) || is.null(factor2)) {
        stop("Two factors required for interaction plot.")
      }

      # Direct call: iQualityR.plot::plot_interaction_line is always available
      p <- iQualityR.plot::plot_interaction_line(
        data = result$model$model,
        x_var = factor1,
        y_var = names(result$model$model)[1],
        group_var = factor2,
        fun = "mean",
        theme = self$theme_obj
      ) + ggplot2::labs(title = sprintf("Interaction Plot: %s x %s", factor1, factor2))

      p
    },

    # ------------------------------------------------------------------------
    # 4. Multiple comparisons plot (forest-plot style)
    # ------------------------------------------------------------------------

    #' @description Multiple comparisons confidence interval plot (forest style)
    #' @param result ANOVA result.
    #' @param factor Factor name. If `NULL`, uses the first factor.
    #' @param alpha Significance level.
    #' @param ... Additional arguments (unused).
    #' @return A ggplot2 object.
    plot_comparison = function(result, factor = NULL, alpha = 0.05, ...) {
      if (is.null(factor)) factor <- result$factors[1]
      if (is.null(result$multiple_comparisons)) {
        # No cached comparisons; try computing via emmeans
        if (requireNamespace("emmeans", quietly = TRUE)) {
          emm <- emmeans::emmeans(result$model, as.formula(paste("~", factor)))
          comp <- summary(emmeans::contrast(emm, method = "pairwise"), adjust = "tukey")
        } else {
          stop("No multiple comparisons available. Install 'emmeans' package.")
        }
      } else {
        comp <- result$multiple_comparisons
      }

      # Extract contrast data
      if (inherits(comp, "summary_em")) {
        df_plot <- data.frame(
          contrast = comp$contrast,
          estimate = comp$estimate,
          se = comp$SE,
          lower = comp$lower.CL,
          upper = comp$upper.CL,
          p_value = comp$p.value,
          significant = comp$p.value < alpha
        )
      } else if (is.data.frame(comp)) {
        # Attempt automatic column mapping
        df_plot <- comp
        if (!all(c("estimate", "lower", "upper") %in% names(df_plot))) {
          stop("Cannot parse comparison result format.")
        }
      } else {
        stop("Unsupported comparison result format.")
      }

      # Sort by estimate
      df_plot <- df_plot[order(df_plot$estimate), ]
      df_plot$contrast <- factor(df_plot$contrast, levels = df_plot$contrast)

      # Forest plot
      p <- base_plot(df_plot, ggplot2::aes(x = contrast, y = estimate), theme = self$theme_obj) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = lower, ymax = upper, color = significant),
          width = 0.2, linewidth = 1
        ) +
        ggplot2::geom_point(
          ggplot2::aes(color = significant, size = -log10(p_value + 1e-10)),
          shape = 18
        ) +
        ggplot2::scale_color_manual(
          values = c("TRUE" = .iqr_plotter$.pal_semantic(self$theme_obj, "fail"),
                     "FALSE" = .iqr_plotter$.pal_discrete(self$theme_obj)[1]),
          name = "Significant",
          labels = c("TRUE" = "p < alpha", "FALSE" = "p >= alpha")
        ) +
        ggplot2::scale_size_continuous(
          range = c(2, 6),
          name = "-log10(p-value)"
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "Comparison",
          y = "Estimate (Difference)",
          title = sprintf("Multiple Comparisons: %s", factor),
          subtitle = sprintf("alpha = %.2f, method: Tukey HSD", alpha)
        )

      p
    },

    # ------------------------------------------------------------------------
    # 5. Variance components plot -- reuses plot_variance_components
    # ------------------------------------------------------------------------

    #' @description Variance components plot (for random effects / mixed models)
    #' @param result ANOVA result.
    #' @param ... Additional arguments (unused).
    #' @return A ggplot2 object.
    plot_variance = function(result, ...) {
      # Only applicable to mixed models
      if (!inherits(result$model, "lmerMod")) {
        stop("Variance components plot is only available for mixed models (lmer).")
      }

      # Extract variance components
      var_comp <- as.data.frame(lme4::VarCorr(result$model))
      var_comp_df <- data.frame(
        source = var_comp$grp,
        variance_percent = var_comp$vcov / sum(var_comp$vcov) * 100
      )

      # Direct call: iQualityR.plot::plot_variance_components is always available
      p <- iQualityR.plot::plot_variance_components(
        data = var_comp_df,
        theme = self$theme_obj,
        sort_by_variance = TRUE
      )

      p
    },

    # ------------------------------------------------------------------------
    # 6. F-distribution rejection region plot
    # ------------------------------------------------------------------------

    #' @description F-distribution rejection region plot (ANOVA global F test)
    #' @param result ANOVA result.
    #' @param alpha Significance level.
    #' @param ... Additional arguments (unused).
    #' @return A ggplot2 object.
    plot_f_curve = function(result, alpha = 0.05, ...) {
      # Extract F statistic and degrees of freedom from the result
      if (!is.null(result$anova_table)) {
        f_stat <- result$anova_table[1, "F value"]
        df1 <- result$anova_table[1, "Df"]
        df2 <- result$anova_table[2, "Df"]
      } else if (!is.null(result$summary)) {
        f_summary <- summary(result$model)
        f_stat <- f_summary$fstatistic[1]
        df1 <- f_summary$fstatistic[2]
        df2 <- f_summary$fstatistic[3]
      } else {
        stop("Cannot extract F statistic from result.")
      }

      # Critical value
      crit <- qf(1 - alpha, df1, df2)

      # Build F-distribution curve data
      x_max <- max(crit * 1.5, f_stat * 1.3)
      x_seq <- seq(0, x_max, length.out = 1000)
      y_seq <- df(x_seq, df1, df2)

      df_curve <- data.frame(x = x_seq, y = y_seq)
      df_reject <- data.frame(x = x_seq[x_seq >= crit], y = y_seq[x_seq >= crit])

      p <- base_plot(df_curve, ggplot2::aes(x = x, y = y), theme = self$theme_obj) +
        ggplot2::geom_line(color = .iqr_plotter$.pal_discrete(self$theme_obj)[1], linewidth = 1.2) +
        ggplot2::geom_ribbon(
          data = df_reject,
          ggplot2::aes(x = x, ymin = 0, ymax = y),
          fill = .iqr_plotter$.pal_semantic(self$theme_obj, "fail"), alpha = 0.3
        ) +
        ggplot2::geom_vline(xintercept = f_stat,
                            color = .iqr_plotter$.pal_ui(self$theme_obj, "primary"), linewidth = 1.2) +
        ggplot2::geom_vline(xintercept = crit, color = "gray50", linetype = "dashed") +
        ggplot2::annotate("text", x = f_stat, y = df(f_stat, df1, df2) * 1.1,
                          label = sprintf("F = %.2f", f_stat),
                          color = .iqr_plotter$.pal_ui(self$theme_obj, "primary")) +
        ggplot2::annotate("text", x = crit, y = df(crit, df1, df2) * 1.2,
                          label = sprintf("Critical = %.2f", crit), color = "gray50") +
        ggplot2::annotate("text", x = x_max * 0.7, y = max(y_seq) * 0.3,
                          label = sprintf("p = %s",
                                          iQualityR.core::format_p_value(result$anova_table[1, "Pr(>F)"])),
                          size = 4) +
        ggplot2::labs(
          x = "F-value",
          y = "Density",
          title = "F-distribution: ANOVA Global Test",
          subtitle = sprintf("df1 = %.0f, df2 = %.0f, alpha = %.2f", df1, df2, alpha)
        )

      p
    },

    # ------------------------------------------------------------------------
    # 7. Combined summary plot
    # ------------------------------------------------------------------------

    #' @description Combined summary plot (effects + residuals + table)
    #' @param result ANOVA result.
    #' @param layout Layout: `"2x2"` or `"1x3"`.
    #' @param ... Additional arguments passed to sub-plots.
    #' @return A patchwork object.
    plot_summary = function(result, layout = "2x2", ...) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("patchwork package required for summary plot.")
      }

      # Build sub-plots
      p1 <- self$plot_effects(result, ...)
      p2 <- tryCatch(self$plot_residuals(result, add_qq = TRUE, ...),
                     error = function(e) NULL)

      # Add interaction plot if there are 2+ factors
      if (length(result$factors) >= 2) {
        p3 <- self$plot_interaction(result, ...)
      } else {
        p3 <- NULL
      }

      # ANOVA table grob via iQualityR.plot::create_anova_table
      table_grob <- iQualityR.plot::create_anova_table(result$anova_table, theme = self$theme_obj)
      p4 <- ggplot2::ggplot() +
        ggplot2::annotation_custom(table_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
        ggplot2::theme_void()

      # Combine
      if (layout == "2x2") {
        plots <- list(p1, p3 %||% p2, p2, p4)
        p <- patchwork::wrap_plots(plots, ncol = 2) +
          patchwork::plot_annotation(
            title = "ANOVA Summary",
            subtitle = sprintf("Model: %s", result$formula %||% ""),
            theme = as_iqr_theme(self$theme_obj)
          )
      } else {
        # 1x3 layout
        plots <- list(p1, p3 %||% p2, p4)
        p <- patchwork::wrap_plots(plots, ncol = 3) +
          patchwork::plot_annotation(
            title = "ANOVA Summary",
            subtitle = sprintf("Model: %s", result$formula %||% ""),
            theme = as_iqr_theme(self$theme_obj)
          )
      }

      p
    },

    #' @description Set the active theme
    #' @param theme Theme name or IqrTheme object.
    #' @return Self-reference (invisible).
    set_theme = function(theme) {
      if (inherits(theme, "IqrTheme")) {
        self$theme_obj <- theme
      } else if (is.character(theme)) {
        tryCatch({
          self$theme_obj <- IqrTheme$new(theme)
        }, error = function(e) {
          warning("Invalid theme name, keeping existing theme.")
        })
      }
      invisible(self)
    }
  ),

  private = list(
    .auto_select = function(result) {
      # Auto-select plot type based on result structure
      if (inherits(result$model, "lmerMod")) {
        return("variance")
      }
      if (length(result$factors) >= 2) {
        return("interaction")
      }
      if (!is.null(result$multiple_comparisons)) {
        return("comparison")
      }
      return("effects")
    }
  )
)


# =============================================================================
# AnovaReporter: ANOVA report generator (reuses iQualityR.core::IqrReporter)
# =============================================================================

#' @title AnovaReporter: ANOVA Report Output Engine
#' @description
#' Inherits from `IqrReporter` to provide standardized report generation for
#' the ANOVA module. Supports Excel, HTML, PDF, Word, and PowerPoint formats.
#'
#' @export
AnovaReporter <- R6::R6Class("AnovaReporter",
  inherit = IqrReporter,

  public = list(
    #' @description Initialize the ANOVA reporter
    #' @param theme_obj IqrTheme object.
    #' @return Self-reference (invisible).
    initialize = function(theme_obj) {
      super$initialize(theme_obj)
      # Register the ANOVA task template (Rmd template + Excel generator)
      self$register(
        task_tag = "anova",
        rmd_template = system.file("templates", "anova_template.Rmd",
                                   package = "iQualityR.stat"),
        excel_generator = function(results, plan) {
          anova_to_excel_data(results, plan)
        }
      )
      invisible(self)
    },

    #' @description Generate an ANOVA report (convenience entry point)
    #' @param results ANOVA result list (from AnovaAnalyzer).
    #' @param plan Optional plan object.
    #' @param format Output format (`"excel"`, `"html"`, `"pdf"`, `"docx"`,
    #'   `"pptx"`).
    #' @param path Output file path (optional).
    #' @param ... Additional arguments passed to the underlying export method.
    #' @return Output file path (invisible).
    report = function(results, plan = NULL, format = "excel", path = NULL, ...) {
      self$export(
        results = results,
        plan = plan,
        task_tag = "anova",
        format = format,
        path = path,
        ...
      )
    }
  )
)


# =============================================================================
# anova_to_excel_data: convert ANOVA results to Excel sheet list
# =============================================================================

#' Convert ANOVA results to an Excel data list
#'
#' @param results Result list returned by AnovaAnalyzer.
#' @param plan Optional plan object (reserved for future use).
#' @return Named list; each element is a data frame corresponding to one
#'   Excel worksheet.
#' @keywords internal
anova_to_excel_data <- function(results, plan = NULL) {
    sheets <- list()

    # 1. ANOVA main table
    if (!is.null(results$anova_table)) {
        sheets[["ANOVA"]] <- cbind(
            Source = rownames(results$anova_table),
            results$anova_table
        )
    } else if (!is.null(results$anova_tables)) {
        # Repeated measures ANOVA may have multiple tables
        for (nm in names(results$anova_tables)) {
            sheets[[paste0("ANOVA_", nm)]] <- cbind(
                Source = rownames(results$anova_tables[[nm]]),
                results$anova_tables[[nm]]
            )
        }
    }

    # 2. Model summary (R-squared, adjusted R-squared, coefficients, etc.)
    if (!is.null(results$summary)) {
        # Extract F statistic, R-squared, etc.
        summ <- summary(results$model)
        if (!is.null(summ$r.squared)) {
            model_stats <- data.frame(
                Metric = c("R-squared", "Adj R-squared", "Residual SE",
                           "F-statistic", "F p-value"),
                Value = c(
                    round(summ$r.squared, 4),
                    round(summ$adj.r.squared, 4),
                    round(summ$sigma, 4),
                    if (!is.null(summ$fstatistic)) round(summ$fstatistic[1], 2) else NA,
                    if (!is.null(summ$fstatistic))
                        format.pval(pf(summ$fstatistic[1], summ$fstatistic[2],
                                       summ$fstatistic[3], lower.tail = FALSE), digits = 4)
                    else NA
                )
            )
            sheets[["Model_Summary"]] <- model_stats
        }
    }

    # 3. Coefficients table (fixed effects)
    if (!is.null(results$coefficients)) {
        coef_df <- as.data.frame(results$coefficients)
        if (ncol(coef_df) > 0) {
            coef_df <- cbind(Term = rownames(coef_df), coef_df)
            sheets[["Coefficients"]] <- coef_df
        }
    }

    # 4. Multiple comparisons (if present)
    if (!is.null(results$multiple_comparisons)) {
        if (is.data.frame(results$multiple_comparisons)) {
            sheets[["Multiple_Comparisons"]] <- results$multiple_comparisons
        } else if (is.list(results$multiple_comparisons)) {
            # Possibly a list grouped by factor
            for (nm in names(results$multiple_comparisons)) {
                df <- results$multiple_comparisons[[nm]]
                if (is.data.frame(df)) {
                    sheets[[paste0("MC_", nm)]] <- df
                }
            }
        }
    }

    # 5. Effect sizes
    if (!is.null(results$effect_size)) {
        eff_df <- data.frame()
        if (!is.null(results$effect_size$eta)) {
            eff_df <- rbind(eff_df, data.frame(
                Effect = names(results$effect_size$eta),
                Eta_Squared = round(results$effect_size$eta, 4),
                stringsAsFactors = FALSE
            ))
        }
        if (!is.null(results$effect_size$partial_eta)) {
            eff_df <- rbind(eff_df, data.frame(
                Effect = names(results$effect_size$partial_eta),
                Partial_Eta_Squared = round(results$effect_size$partial_eta, 4),
                stringsAsFactors = FALSE
            ))
        }
        if (nrow(eff_df) > 0) sheets[["Effect_Sizes"]] <- eff_df
    }

    # 6. Diagnostic data (residuals, fitted values)
    if (!is.null(results$residuals)) {
        diag_df <- data.frame(
            Residual = results$residuals,
            Fitted = results$fitted,
            stringsAsFactors = FALSE
        )
        if (!is.null(results$n) && length(diag_df$Residual) == results$n) {
            diag_df$Index <- seq_along(diag_df$Residual)
            sheets[["Diagnostics"]] <- diag_df
        }
    }

    # 7. Raw data (if included in the result)
    if (!is.null(results$data)) {
        sheets[["Data"]] <- results$data
    }

    sheets
}


# =============================================================================
# iqr_anova: user-facing ANOVA entry class + convenience functions
# =============================================================================

#' @title iqr_anova: ANOVA Entry Class
#' @description
#' Top-level interface for the iQualityR ANOVA module, coordinating
#' computation (AnovaAnalyzer), plotting (AnovaPlotter), and reporting
#' (AnovaReporter).
#'
#' @export
iqr_anova <- R6::R6Class("iqr_anova",
  public = list(
    #' @field last_results Cached computation results from the last run.
    last_results = NULL,
    #' @field analyzer Computation engine (AnovaAnalyzer).
    analyzer = NULL,
    #' @field plotter Plotting engine (AnovaPlotter).
    plotter = NULL,
    #' @field theme_obj Active IqrTheme object.
    theme_obj = NULL,

    #' @description Initialize the ANOVA module
    #' @param theme Theme name or IqrTheme object.
    #' @return Self-reference (invisible).
    initialize = function(theme = "academic") {
      self$analyzer <- AnovaAnalyzer$new()
      self$plotter  <- AnovaPlotter$new(theme = theme)
      if (inherits(theme, "IqrTheme")) {
        self$theme_obj <- theme
      } else {
        tryCatch({
          self$theme_obj <- IqrTheme$new(theme)
        }, error = function(e) {
          self$theme_obj <<- NULL
        })
      }
      invisible(self)
    },

    #' @description Execute ANOVA analysis
    #' @param formula Model formula.
    #' @param data Data frame.
    #' @param ... Additional arguments passed to AnovaAnalyzer.
    #' @param plot Whether to auto-plot after analysis.
    #' @param plot_type Plot type (passed to AnovaPlotter).
    #' @param interpret Whether to print a brief interpretation.
    #' @param audience Audience level (`"manager"`, `"technical"`, `"client"`).
    #' @return Self-reference (invisible).
    run = function(formula, data, ..., plot = FALSE, plot_type = "auto",
                   interpret = FALSE, audience = "manager") {
      self$last_results <- self$analyzer$analyze(formula, data, ...)

      if (plot) {
        tryCatch(print(self$plot(plot_type = plot_type)),
                 error = function(e) {
                   warning("Plotting failed: ", conditionMessage(e))
                 })
      }

      if (interpret) {
        cat(sprintf("ANOVA test type: %s\n", self$last_results$test_type %||% "unknown"))
        cat(sprintf("Formula: %s\n", self$last_results$formula %||% ""))
      }

      invisible(self)
    },

    #' @description Plot the last analysis results
    #' @param plot_type Plot type (passed to AnovaPlotter).
    #' @return A ggplot2 or patchwork object.
    plot = function(plot_type = "auto") {
      if (is.null(self$last_results)) {
        stop("[iqr_anova] Run analysis first.", call. = FALSE)
      }
      self$plotter$plot(self$last_results, plot_type = plot_type)
    },

    #' @description Generate a report from the last analysis results
    #' @param format Output format (`"excel"`, `"html"`, `"pdf"`, `"docx"`,
    #'   `"pptx"`).
    #' @param path Output file path (optional).
    #' @return Output file path (invisible).
    report = function(format = "excel", path = NULL) {
      if (is.null(self$last_results)) {
        stop("[iqr_anova] Run analysis first.", call. = FALSE)
      }
      reporter <- AnovaReporter$new(self$theme_obj %||% IqrTheme$new())
      reporter$report(self$last_results, format = format, path = path)
    }
  )
)


# -----------------------------------------------------------------------------
# Convenience functions
# -----------------------------------------------------------------------------

#' @title Convenience ANOVA analysis function
#' @description
#' Run ANOVA analysis without explicitly creating an R6 object. Suitable for
#' quick exploratory analysis.
#'
#' @param formula Model formula.
#' @param data Data frame.
#' @param ... Additional arguments passed to AnovaAnalyzer.
#' @param plot Whether to auto-plot after analysis.
#' @param plot_type Plot type (passed to AnovaPlotter).
#' @param interpret Whether to print a brief interpretation.
#' @param audience Audience level (`"manager"`, `"technical"`, `"client"`).
#' @param theme Theme name or IqrTheme object.
#' @return Analysis result list (invisible).
#' @export
#'
#' @examples
#' \dontrun{
#' result <- anova_run(Adhesion ~ PaintType * Pressure, data = painting_data)
#' result <- anova_run(Strength ~ Supplier, data = supplier_data, plot = TRUE)
#' }
anova_run <- function(formula, data, ..., plot = FALSE, plot_type = "auto",
                      interpret = FALSE, audience = "manager", theme = "academic") {
    obj <- iqr_anova$new(theme = theme)
    obj$run(formula, data, ..., plot = plot, plot_type = plot_type,
            interpret = interpret, audience = audience)
    invisible(obj$last_results)
}

#' @title Generate an ANOVA report (convenience function)
#' @description
#' Create a formatted report from ANOVA results.
#'
#' @param results ANOVA result list (from `anova_run` or AnovaAnalyzer).
#' @param format Output format (`"excel"`, `"html"`, `"pdf"`, `"docx"`,
#'   `"pptx"`).
#' @param path Output file path (optional).
#' @param theme Theme name or IqrTheme object.
#' @param ... Additional arguments passed to AnovaReporter.
#' @return Output file path (invisible).
#' @export
anova_report <- function(results, format = "excel", path = NULL,
                         theme = "academic", ...) {
    theme_obj <- if (inherits(theme, "IqrTheme")) theme else IqrTheme$new(theme)
    reporter <- AnovaReporter$new(theme_obj)
    reporter$report(results, format = format, path = path, ...)
}
