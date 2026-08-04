# =============================================================================
# File: R/RegressionPlotter.R
# Description: Regression visualization (L2).
#              Residual plots + coefficient plots + subset-selection plots +
#              classification curves (ROC / PR / Lift) for logit_fit +
#              spline curve overlay for spline_fit / mars_fit.
# =============================================================================
#' @title RegressionPlotter: Regression Visualization
#' @description L2 presentation engine for regression models.
#' $plot(result, plot_type = "auto", show_table = FALSE, theme_obj = NULL)
#'
#' Plot types: "residual", "coef", "subset" (best_subset_fit),
#' "roc", "pr", "lift" (logit_fit classification curves),
#' "spline" (spline_fit / mars_fit data + fitted curve).
#' @export
RegressionPlotter <- R6::R6Class("RegressionPlotter",
  inherit = StatPlotter,
  public = list(
    initialize = function(theme = "academic") { super$initialize(theme) },
    plot = function(result, plot_type = "auto", show_table = FALSE, theme_obj = NULL) {
      private$.check_plot_available()
      pt <- if (plot_type == "auto") private$.auto_select(result) else plot_type
      switch(pt,
        "residual" = private$.plot_residual(result),
        "coef"     = private$.plot_coef(result),
        "subset"   = private$.plot_subset(result),
        "roc"      = private$.plot_roc(result),
        "pr"       = private$.plot_pr(result),
        "lift"     = private$.plot_lift(result),
        "spline"   = private$.plot_spline(result),
        stop("RegressionPlotter: unknown plot_type '", pt, "'.", call. = FALSE)
      )
    }
  ),
  private = list(
    .auto_select = function(result) {
      tt <- result$test_type
      if (tt == "best_subset_fit") return("subset")
      if (tt == "logit_fit") return("roc")
      # spline_fit / mars_fit: data + fitted curve is the most informative view
      if (tt %in% c("spline_fit", "mars_fit")) return("spline")
      if (is.null(result$residuals) || is.null(result$fitted)) return("coef")
      "residual"
    },
    # Extract binary response + predicted probabilities for logit_fit.
    # Returns list(y, p) or NULL if not available.
    .classification_data = function(result) {
      y <- NULL; p <- result$fitted
      if (!is.null(result$model)) {
        mf <- stats::model.frame(result$model)
        y <- as.numeric(stats::model.response(mf))
      }
      if (is.null(y) || is.null(p) || length(y) != length(p)) return(NULL)
      if (!all(y %in% c(0, 1))) return(NULL)
      list(y = y, p = p)
    },
    .auc = function(y, p) {
      n_pos <- sum(y == 1); n_neg <- sum(y == 0)
      if (n_pos == 0 || n_neg == 0) return(NA_real_)
      ord <- order(p, decreasing = TRUE)
      y_sorted <- y[ord]
      # Trapezoidal AUC via rank-sum (Mann-Whitney U)
      ranks <- rank(p)
      (sum(ranks[y == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
    },
    .plot_residual = function(result) {
      df <- data.frame(fitted = result$fitted, resid = result$residuals)
      r2 <- result$model_stats$r_squared
      subtitle <- sprintf("n = %d, R-squared = %s",
                          result$model_stats$n,
                          if (is.na(r2)) "NA" else sprintf("%.4f", r2))
      ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = resid)) +
        ggplot2::geom_point(alpha = 0.5) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
        ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "blue") +
        ggplot2::labs(title = result$method %||% result$test_type, subtitle = subtitle,
                      x = "Fitted values", y = "Residuals") +
        ggplot2::theme_minimal()
    },
    .plot_coef = function(result) {
      cf <- result$coefficients
      if (is.null(cf)) stop("RegressionPlotter: no coefficient table available for this model.", call. = FALSE)
      cf$lower <- cf$Estimate - 1.96 * cf$Std_Error
      cf$upper <- cf$Estimate + 1.96 * cf$Std_Error
      ggplot2::ggplot(cf, ggplot2::aes(x = Term, y = Estimate)) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.2) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = result$method %||% result$test_type, x = "Term", y = "Estimate (95% CI)") +
        ggplot2::theme_minimal()
    },
    .plot_subset = function(result) {
      ss <- result$subset_summary
      if (is.null(ss)) stop("RegressionPlotter: no subset_summary available.", call. = FALSE)
      ggplot2::ggplot(ss, ggplot2::aes(x = n_vars, y = BIC)) +
        ggplot2::geom_line(colour = "blue") +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_point(data = ss[which.min(ss$BIC), ],
                            ggplot2::aes(x = n_vars, y = BIC),
                            colour = "red", size = 4) +
        ggplot2::labs(title = result$method %||% result$test_type,
                      subtitle = sprintf("Best by BIC: %d vars", result$best_by_bic$n_vars),
                      x = "Number of variables", y = "BIC") +
        ggplot2::theme_minimal()
    },
    .plot_roc = function(result) {
      cd <- private$.classification_data(result)
      if (is.null(cd)) stop("RegressionPlotter: ROC curve requires a logit_fit result with binary response.", call. = FALSE)
      y <- cd$y; p <- cd$p
      n_pos <- sum(y == 1); n_neg <- sum(y == 0)
      if (n_pos == 0 || n_neg == 0) stop("RegressionPlotter: ROC requires both classes present.", call. = FALSE)
      ord <- order(p, decreasing = TRUE)
      y_sorted <- y[ord]
      TP <- cumsum(y_sorted); FP <- cumsum(1 - y_sorted)
      TPR <- TP / n_pos; FPR <- FP / n_neg
      # Prepend origin
      TPR <- c(0, TPR); FPR <- c(0, FPR)
      auc_val <- private$.auc(y, p)
      df <- data.frame(FPR = FPR, TPR = TPR)
      ggplot2::ggplot(df, ggplot2::aes(x = FPR, y = TPR)) +
        ggplot2::geom_line(colour = "blue", linewidth = 1) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
        ggplot2::labs(title = "ROC Curve",
                      subtitle = sprintf("AUC = %.4f", auc_val),
                      x = "False Positive Rate (1 - Specificity)",
                      y = "True Positive Rate (Sensitivity)") +
        ggplot2::coord_equal() +
        ggplot2::theme_minimal()
    },
    .plot_pr = function(result) {
      cd <- private$.classification_data(result)
      if (is.null(cd)) stop("RegressionPlotter: PR curve requires a logit_fit result with binary response.", call. = FALSE)
      y <- cd$y; p <- cd$p
      n_pos <- sum(y == 1)
      if (n_pos == 0) stop("RegressionPlotter: PR requires positive cases.", call. = FALSE)
      ord <- order(p, decreasing = TRUE)
      y_sorted <- y[ord]
      TP <- cumsum(y_sorted); FP <- cumsum(1 - y_sorted)
      recall <- TP / n_pos
      precision <- TP / (TP + FP)
      # Prepend starting point (recall=0, precision=1 convention)
      recall <- c(0, recall); precision <- c(1, precision)
      df <- data.frame(recall = recall, precision = precision)
      ggplot2::ggplot(df, ggplot2::aes(x = recall, y = precision)) +
        ggplot2::geom_line(colour = "darkgreen", linewidth = 1) +
        ggplot2::labs(title = "Precision-Recall Curve",
                      subtitle = sprintf("Baseline precision = %.4f", n_pos / length(y)),
                      x = "Recall", y = "Precision") +
        ggplot2::ylim(0, 1) +
        ggplot2::theme_minimal()
    },
    .plot_lift = function(result) {
      cd <- private$.classification_data(result)
      if (is.null(cd)) stop("RegressionPlotter: Lift curve requires a logit_fit result with binary response.", call. = FALSE)
      y <- cd$y; p <- cd$p
      n <- length(y); n_pos <- sum(y == 1)
      if (n_pos == 0) stop("RegressionPlotter: Lift requires positive cases.", call. = FALSE)
      ord <- order(p, decreasing = TRUE)
      y_sorted <- y[ord]
      cum_pos <- cumsum(y_sorted)
      decile <- seq_len(n) / n  # proportion of sample
      cum_response_rate <- cum_pos / seq_len(n)
      baseline_rate <- n_pos / n
      lift <- cum_response_rate / baseline_rate
      df <- data.frame(sample_pct = decile, lift = lift,
                       cum_capture = cum_pos / n_pos)
      ggplot2::ggplot(df, ggplot2::aes(x = sample_pct, y = lift)) +
        ggplot2::geom_line(colour = "purple", linewidth = 1) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
        ggplot2::labs(title = "Lift Curve",
                      subtitle = sprintf("Baseline positive rate = %.4f", baseline_rate),
                      x = "Proportion of sample (sorted by predicted prob.)",
                      y = "Lift") +
        ggplot2::theme_minimal()
    },
    # Spline / MARS curve overlay: scatter raw data + fitted values vs the
    # primary spline predictor. For spline_fit the predictor is stored on
    # the result; for MARS we use the first term in the model's terms().
    .plot_spline = function(result) {
      if (is.null(result$fitted) || is.null(result$model)) {
        stop("RegressionPlotter: 'spline' plot requires fitted values and a model.",
             call. = FALSE)
      }
      # Locate the primary predictor and its raw values.
      x_var <- NULL; x_vals <- NULL
      if (!is.null(result$spline_predictor)) {
        x_var <- result$spline_predictor
        # spline_fit stores the *original* formula; its model.frame may
        # contain the basis expansion rather than the raw predictor, so
        # fall back to the original data via the formula's variables.
        mf <- tryCatch(stats::model.frame(result$model), error = function(e) NULL)
        if (!is.null(mf) && x_var %in% names(mf)) {
          x_vals <- mf[[x_var]]
        } else if (!is.null(result$formula)) {
          # Re-build a frame from the original formula to recover raw x
          orig_mf <- tryCatch(
            stats::model.frame(result$formula, data = stats::model.frame(result$model)),
            error = function(e) NULL
          )
          if (!is.null(orig_mf) && x_var %in% names(orig_mf)) {
            x_vals <- orig_mf[[x_var]]
          }
        }
      } else if (!is.null(result$model)) {
        # MARS path: pull the first non-intercept predictor from the call's terms
        trms <- tryCatch(stats::terms(stats::formula(result$model)),
                         error = function(e) NULL)
        if (!is.null(trms)) {
          labs <- attr(trms, "term.labels")
          if (length(labs) >= 1L) x_var <- labs[1]
        }
        mf <- tryCatch(stats::model.frame(result$model), error = function(e) NULL)
        if (!is.null(mf) && !is.null(x_var) && x_var %in% names(mf)) {
          x_vals <- mf[[x_var]]
        }
      }
      if (is.null(x_var) || is.null(x_vals)) {
        stop("RegressionPlotter: cannot locate primary predictor for spline plot.",
             call. = FALSE)
      }
      # Response values: prefer model.response of the (original) frame
      y_vals <- NULL
      mf_resp <- tryCatch(stats::model.frame(result$model), error = function(e) NULL)
      if (!is.null(mf_resp)) {
        y_vals <- as.numeric(stats::model.response(mf_resp))
      }
      if (is.null(y_vals) || length(y_vals) != length(x_vals)) {
        # Fallback: use fitted + residuals
        y_vals <- as.numeric(result$fitted) + as.numeric(result$residuals)
      }
      df <- data.frame(x = x_vals, y = y_vals, fitted = result$fitted)
      # Sort by x for a clean fitted line
      df <- df[order(df$x), ]
      r2 <- result$model_stats$r_squared
      subtitle <- sprintf("R-squared = %s, n = %d",
                          if (is.na(r2)) "NA" else sprintf("%.4f", r2),
                          result$model_stats$n)
      ggplot2::ggplot(df, ggplot2::aes(x = x)) +
        ggplot2::geom_point(ggplot2::aes(y = y), alpha = 0.5) +
        ggplot2::geom_line(ggplot2::aes(y = fitted),
                           colour = "firebrick", linewidth = 1) +
        ggplot2::labs(
          title = result$method %||% result$test_type,
          subtitle = subtitle,
          x = x_var, y = "Response"
        ) +
        ggplot2::theme_minimal()
    }
  )
)
