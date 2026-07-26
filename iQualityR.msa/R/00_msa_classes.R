# =============================================================================
# File: R/00_msa_classes.R
# Description: Gage R&R MSA analyzer, plotter and task classes.
# =============================================================================

#' @title MsaAnalyzer
#' @description Analyzer for crossed/nested Gage R&R studies.
#'
#' @param data Data frame of measurements with columns `MeasurementValue`, `Part`, and `Operator`.
#' @param plan Optional MSA plan object providing parameters and specifications.
#' @param analysis_method Character scalar: `"anova"` or `"xbar_r"`.
#' @param alpha_lim Numeric scalar p-value threshold for keeping the interaction term in ANOVA (default 0.25).
#' @param sigma Multiplier for study variation (default 6).
#'
#' @export
MsaAnalyzer <- R6::R6Class(
  "MsaAnalyzer",
  inherit = IqrAnalyzerBase,
  public = list(
    #' @field anova_results List holding ANOVA tables (with/without interaction) and metadata after `run()`.
    anova_results = NULL,
    #' @field variance_components Tibble of variance components (Source, Variance, PercentContribution, StdDev, StudyVar, etc.).
    variance_components = NULL,
    #' @field statistics List of summary statistics (ndc, total_gage_rr_percent, tolerance_percent) after computation.
    statistics = NULL,
    #' @field interpretation List with acceptance status, message, gage_rr_percent, and ndc.
    interpretation = NULL,

    run = function(data, plan = NULL, analysis_method = c("anova", "xbar_r"),
                   alpha_lim = 0.25, sigma = 6) {
      self$reset()
      analysis_method <- match.arg(analysis_method)
      params <- if (!is.null(plan)) plan$to_list() else list()
      params$plan <- plan
      params$analysis_method <- analysis_method
      params$alpha_lim <- alpha_lim
      params$sigma <- sigma
      self$params <- params

      dt <- self$prepare_data(data)
      if (analysis_method == "anova") {
        self$compute_anova(dt, alpha_lim, sigma)
      } else {
        self$compute_xbar_r(dt, sigma)
      }
      self$compute_statistics()
      self$interpret_results()

      self$results <- list(
        statistics = self$statistics,
        diagnostics = self$interpretation,
        data_tables = list(
          variance_components = self$variance_components,
          gage_evaluation = self$build_gage_evaluation_table(),
          anova_table = self$build_anova_table()
        ),
        raw_output = list(
          analysis_method = analysis_method,
          anova_results = self$anova_results,
          variance_components = self$variance_components,
          gage_evaluation = self$build_gage_evaluation_table(),
          anova_table = self$build_anova_table(),
          statistics = self$statistics,
          interpretation = self$interpretation,
          data = dt
        )
      )
      invisible(self)
    },

    validate_data = function(data) {
      required_cols <- c("MeasurementValue", "Part", "Operator")
      missing <- setdiff(required_cols, names(data))
      if (length(missing) > 0) {
        stop("Data missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
      }
      if (!is.numeric(data$MeasurementValue)) {
        stop("MeasurementValue column must be numeric.", call. = FALSE)
      }
      if (any(!is.finite(data$MeasurementValue))) {
        stop("MeasurementValue contains NA/Inf/NaN values; please clean the data first.",
             call. = FALSE)
      }
      if (length(unique(data$Part)) < 2) {
        stop("MSA requires at least 2 distinct parts; found ",
             length(unique(data$Part)), ".", call. = FALSE)
      }
      if (length(unique(data$Operator)) < 2) {
        stop("MSA requires at least 2 operators; found ",
             length(unique(data$Operator)), ".", call. = FALSE)
      }
      invisible(self)
    },

    prepare_data = function(data) {
      self$validate_data(data)
      data %>%
        dplyr::mutate(
          Operator = as.factor(.data$Operator),
          Part = as.factor(.data$Part)
        )
    },

    compute_anova = function(data, alpha_lim, sigma) {
      method <- ifelse(is.null(self$params$plan), "crossed", self$params$plan$method)

      if (method == "nested") {
        model_without <- stats::aov(MeasurementValue ~ Operator + Part, data = data)
        anova_without <- broom::tidy(model_without)
        self$anova_results <- list(
          with_interaction = NULL,
          without_interaction = anova_without,
          has_interaction = FALSE,
          alpha_lim = alpha_lim,
          design_method = method
        )
      } else {
        model_with <- stats::aov(MeasurementValue ~ Part * Operator, data = data)
        anova_with <- broom::tidy(model_with)

        interaction_p <- anova_with %>%
          dplyr::filter(.data$term == "Part:Operator") %>%
          dplyr::pull(.data$p.value)

        if (is.na(interaction_p) || interaction_p > alpha_lim) {
          model_without <- stats::aov(MeasurementValue ~ Part + Operator, data = data)
          anova_without <- broom::tidy(model_without)
          has_interaction <- FALSE
        } else {
          anova_without <- NULL
          has_interaction <- TRUE
        }

        self$anova_results <- list(
          with_interaction = anova_with,
          without_interaction = anova_without,
          has_interaction = has_interaction,
          alpha_lim = alpha_lim,
          design_method = method
        )
      }
      self$variance_components <- self$calculate_variance_components_anova(data, sigma)
      invisible(self)
    },

    compute_xbar_r = function(data, sigma) {
      data <- data %>% dplyr::arrange(.data$Operator, .data$Part, dplyr::row_number())
      unique_ops <- unique(as.character(data$Operator))
      unique_parts <- unique(as.character(data$Part))

      # Subgroup = (Operator, Part) cell; subgroup size n = repeats per cell
      cell_stats <- data %>%
        dplyr::group_by(.data$Operator, .data$Part) %>%
        dplyr::summarize(
          Mean     = mean(.data$MeasurementValue),
          R        = max(.data$MeasurementValue) - min(.data$MeasurementValue),
          n        = dplyr::n(),
          .groups  = "drop"
        )

      grand_mean <- mean(data$MeasurementValue)
      R_bar <- mean(cell_stats$R)
      # Subgroup size for d2 is the number of repeat measurements per cell
      subgroup_n <- as.integer(stats::median(cell_stats$n))
      if (is.na(subgroup_n) || subgroup_n < 2) subgroup_n <- 2
      d2 <- iQualityR.stat::get_d2(subgroup_n)

      # --- Variance components per AIAG MSA 4th ed., Section III-B ---
      # Repeatability (equipment variation, EV)
      var_repeatability <- (R_bar / d2)^2

      # Operator means (X-bar-bar per operator) and Part means
      op_means  <- cell_stats %>%
        dplyr::group_by(.data$Operator) %>%
        dplyr::summarize(op_mean = mean(.data$Mean), n_parts_op = dplyr::n(), .groups = "drop")
      part_means <- cell_stats %>%
        dplyr::group_by(.data$Part) %>%
        dplyr::summarize(part_mean = mean(.data$Mean), n_ops_part = dplyr::n(), .groups = "drop")

      num_ops   <- nrow(op_means)
      num_parts <- nrow(part_means)
      # Repeats per Operator-Part cell (assume balanced; fall back to median)
      num_meas  <- subgroup_n

      # Reproducibility (appraiser variation, AV):
      #   Var_AV = max(0, R_o_bar^2 / d2*^2 - Var_EV / (n_parts * n_meas))
      # where R_o_bar is the range of operator means and d2* is the d2 for
      # a subgroup of size = number of operators.
      R_op <- max(op_means$op_mean) - min(op_means$op_mean)
      d2_op <- iQualityR.stat::get_d2(num_ops)
      var_operator <- max(0, (R_op / d2_op)^2 - var_repeatability / (num_parts * num_meas))

      # Part-to-Part variation (PV):
      #   Var_PV = max(0, R_p_bar^2 / d2*^2 - Var_EV / (num_ops * num_meas))
      # where R_p_bar is the range of part means and d2* is the d2 for
      # a subgroup of size = number of parts.
      R_part <- max(part_means$part_mean) - min(part_means$part_mean)
      d2_part <- iQualityR.stat::get_d2(num_parts)
      var_part <- max(0, (R_part / d2_part)^2 - var_repeatability / (num_ops * num_meas))

      var_total_gage_rr <- var_repeatability + var_operator
      total_var <- var_total_gage_rr + var_part

      self$anova_results <- list(
        method      = "xbar_r",
        grand_mean  = grand_mean,
        R_bar       = R_bar,
        d2          = d2,
        subgroup_n  = subgroup_n
      )
      self$variance_components <- dplyr::tibble(
        Source = c("Total Gage R&R", "  Repeatability", "  Reproducibility", "Part-to-Part", "Total Variation"),
        Variance = c(var_total_gage_rr, var_repeatability, var_operator, var_part, total_var)
      ) %>%
        dplyr::mutate(
          PercentContribution = if (total_var > 0) round(100 * .data$Variance / total_var, 2) else NA_real_,
          StdDev = sqrt(pmax(.data$Variance, 0)),
          StudyVar = .data$StdDev * sigma
        )
      total_study_var <- self$variance_components$StudyVar[self$variance_components$Source == "Total Variation"]
      self$variance_components <- self$variance_components %>%
        dplyr::mutate(PercentStudyVar = if (!is.na(total_study_var) && total_study_var > 0) round(100 * .data$StudyVar / total_study_var, 2) else NA_real_)
      tolerance <- private$get_tolerance_from_plan(self$params$plan)
      if (!is.null(tolerance) && tolerance > 0) {
        self$variance_components <- self$variance_components %>%
          dplyr::mutate(PercentTolerance = round(100 * .data$StudyVar / tolerance, 2))
      }
      invisible(self)
    },

    calculate_variance_components_anova = function(data, sigma) {
      plan <- self$params$plan
      method <- ifelse(is.null(plan), "crossed", plan$method)

      if (method == "crossed" && self$anova_results$has_interaction) {
        anova_table <- self$anova_results$with_interaction
        MS_part <- anova_table %>% dplyr::filter(.data$term == "Part") %>% dplyr::pull(.data$meansq)
        MS_operator <- anova_table %>% dplyr::filter(.data$term == "Operator") %>% dplyr::pull(.data$meansq)
        MS_interaction <- anova_table %>% dplyr::filter(.data$term == "Part:Operator") %>% dplyr::pull(.data$meansq)
        MS_error <- anova_table %>% dplyr::filter(.data$term == "Residuals") %>% dplyr::pull(.data$meansq)

        num_ops <- length(unique(data$Operator))
        num_parts <- length(unique(data$Part))
        num_meas <- nrow(data) / (num_ops * num_parts)

        var_repeatability <- MS_error
        var_interaction <- max((MS_interaction - MS_error) / num_meas, 0)
        var_operator <- max((MS_operator - MS_interaction) / (num_parts * num_meas), 0)
        var_part <- max((MS_part - MS_interaction) / (num_ops * num_meas), 0)
        var_reproducibility <- var_operator + var_interaction
      } else {
        anova_table <- self$anova_results$without_interaction
        MS_part <- anova_table %>% dplyr::filter(.data$term == "Part") %>% dplyr::pull(.data$meansq)
        MS_operator <- anova_table %>% dplyr::filter(.data$term == "Operator") %>% dplyr::pull(.data$meansq)
        MS_error <- anova_table %>% dplyr::filter(.data$term == "Residuals") %>% dplyr::pull(.data$meansq)

        num_ops <- length(unique(data$Operator))
        if (method == "nested") {
          parts_per_operator <- data %>%
            dplyr::distinct(.data$Operator, .data$Part) %>%
            dplyr::count(.data$Operator, name = "n_parts") %>%
            dplyr::pull(.data$n_parts)
          num_parts <- stats::median(parts_per_operator)
          reps <- data %>%
            dplyr::count(.data$Operator, .data$Part, name = "n_repeats") %>%
            dplyr::pull(.data$n_repeats)
          num_meas <- stats::median(reps)
        } else {
          num_parts <- length(unique(data$Part))
          num_meas <- nrow(data) / (num_ops * num_parts)
        }

        var_repeatability <- MS_error
        if (method == "nested") {
          var_operator <- max((MS_operator - MS_part) / (num_parts * num_meas), 0)
          var_part <- max((MS_part - MS_error) / num_meas, 0)
        } else {
          var_operator <- max((MS_operator - MS_error) / (num_parts * num_meas), 0)
          var_part <- max((MS_part - MS_error) / (num_ops * num_meas), 0)
        }
        var_reproducibility <- var_operator
        var_interaction <- 0
      }

      var_total_gage_rr <- var_repeatability + var_reproducibility
      var_total <- var_total_gage_rr + var_part

      tolerance <- private$get_tolerance_from_plan(plan)

      result <- dplyr::tibble(
        Source = c("Total Gage R&R", "  Repeatability", "  Reproducibility",
                   "    Operator", "    Operator:Part", "Part-to-Part", "Total Variation"),
        Variance = c(var_total_gage_rr, var_repeatability, var_reproducibility,
                     var_operator, var_interaction, var_part, var_total)
      ) %>%
        dplyr::filter(.data$Variance >= 0 | .data$Source %in% c("Total Gage R&R", "Total Variation")) %>%
        dplyr::mutate(
          PercentContribution = round(100 * .data$Variance / var_total, 2),
          StdDev = sqrt(.data$Variance),
          StudyVar = .data$StdDev * sigma,
          PercentStudyVar = if (sqrt(var_total) * sigma > 0) round(100 * .data$StudyVar / (sqrt(var_total) * sigma), 2) else NA_real_
        )

      if (!is.null(tolerance) && tolerance > 0) {
        result <- result %>% dplyr::mutate(PercentTolerance = round(100 * .data$StudyVar / tolerance, 2))
      }
      result
    },

    build_gage_evaluation_table = function() {
      vc <- self$variance_components
      if (is.null(vc) || nrow(vc) == 0) return(data.frame())

      # Minitab-style Gage Evaluation table
      # Compact format: Source, VarComp, %Contribution, StdDev, StudyVar, %Tolerance/%StudyVar
      display <- vc %>%
        dplyr::mutate(
          # Column name mapping: Minitab style
          VarComp = .data$Variance,
          # Remove leading spaces for display
          Source = trimws(.data$Source)
        ) %>%
        dplyr::select(
          "Source",
          "VarComp",
          "PercentContribution",
          "StdDev",
          "StudyVar"
        )

      # Add %Tolerance or %StudyVar column if available
      if ("PercentTolerance" %in% names(vc)) {
        display <- display %>% dplyr::mutate(`%Tolerance` = vc$PercentTolerance)
      } else if ("PercentStudyVar" %in% names(vc)) {
        display <- display %>% dplyr::mutate(`%StudyVar` = vc$PercentStudyVar)
      }

      display
    },

    build_anova_table = function() {
      if (is.null(self$anova_results)) return(data.frame())

      # Determine which ANOVA table to use
      tbl <- if (!is.null(self$anova_results$with_interaction)) {
        self$anova_results$with_interaction
      } else if (!is.null(self$anova_results$without_interaction)) {
        self$anova_results$without_interaction
      } else {
        return(data.frame())
      }

      if (is.null(tbl) || nrow(tbl) == 0) return(data.frame())

      # Column name mapping to Minitab style
      col_mapping <- c(
        "term" = "Source",
        "df" = "DF",
        "sumsq" = "SS",
        "meansq" = "MS",
        "statistic" = "F",
        "p.value" = "P"
      )

      # Select and rename columns
      keep <- intersect(names(col_mapping), names(tbl))
      result <- tbl[, keep, drop = FALSE]
      names(result) <- col_mapping[keep]

      # Clean up Source column format
      result$Source <- gsub("Part:Operator|Operator:Part", "Part * Operator", result$Source)
      result$Source <- gsub("Residuals", "Repeatability", result$Source)

      # Add Total row
      ss_total <- sum(tbl$sumsq, na.rm = TRUE)
      df_total <- sum(tbl$df, na.rm = TRUE)

      # Do not compute F and P for the Total row
      total_row <- data.frame(
        Source = "Total",
        DF = df_total,
        SS = ss_total,
        MS = NA_real_,
        F = NA_real_,
        P = NA_real_,
        stringsAsFactors = FALSE
      )

      result <- rbind(result, total_row)

      result
    },

    compute_statistics = function() {
      vc <- self$variance_components
      total_gage_rr_var <- vc %>% dplyr::filter(.data$Source == "Total Gage R&R") %>% dplyr::pull(.data$Variance)
      part_var <- vc %>% dplyr::filter(.data$Source == "Part-to-Part") %>% dplyr::pull(.data$Variance)
      ndc <- max(1, floor(1.41 * sqrt(part_var / total_gage_rr_var)))
      self$statistics <- list(
        ndc = ndc,
        total_gage_rr_percent = vc %>% dplyr::filter(.data$Source == "Total Gage R&R") %>% dplyr::pull(.data$PercentContribution),
        tolerance_percent = if ("PercentTolerance" %in% names(vc)) {
          vc %>% dplyr::filter(.data$Source == "Total Gage R&R") %>% dplyr::pull(.data$PercentTolerance)
        } else {
          NA_real_
        }
      )
      invisible(self)
    },

    interpret_results = function() {
      gage_rr_pct <- self$statistics$total_gage_rr_percent
      ndc <- self$statistics$ndc

      # AIAG MSA 4th ed.: %GRR and NDC are evaluated independently.
      # %GRR: <10% accept, 10-30% conditional, >30% reject.
      # NDC:  >=5 acceptable, >=4 conditional, <4 reject.
      gage_status <- if (is.na(gage_rr_pct)) "unknown" else if (gage_rr_pct < 10) "accept" else if (gage_rr_pct < 30) "marginal" else "reject"
      ndc_status  <- if (is.na(ndc) || ndc < 4) "reject" else if (ndc < 5) "marginal" else "accept"

      # Overall status takes the worst of the two independent assessments.
      # Rank: accept (1) < marginal (2) < unknown (3) < reject (4).
      status_rank <- c(accept = 1, marginal = 2, unknown = 3, reject = 4)
      worst_rank  <- max(status_rank[gage_status], status_rank[ndc_status])
      overall_status <- names(which(status_rank == worst_rank))[1]
      # Treat "unknown" as "marginal" for the overall verdict so the message
      # never says "unknown" when at least one criterion is known.
      if (overall_status == "unknown") overall_status <- "marginal"

      message_text <- switch(overall_status,
        accept   = "Measurement system is acceptable (%GRR < 10% and NDC >= 5)",
        marginal = "Measurement system is conditionally acceptable (10%% <= %GRR < 30%% or NDC == 4)",
        reject   = "Measurement system is not acceptable (%GRR >= 30% or NDC < 4)",
        "Measurement system status unknown"
      )

      self$interpretation <- list(
        status              = overall_status,
        message             = message_text,
        gage_rr_percent     = gage_rr_pct,
        gage_rr_status      = gage_status,
        ndc                 = ndc,
        ndc_status          = ndc_status,
        criteria            = "AIAG MSA 4th edition: %GRR < 10% accept / 10-30% marginal / >30% reject; NDC >= 5 accept / 4 marginal / <4 reject"
      )
      invisible(self)
    }
  ),

  private = list(
    get_tolerance_from_plan = function(plan) {
      if (is.null(plan) || is.null(plan$specifications) || length(plan$specifications) == 0) return(NULL)
      spec <- plan$specifications[[1]]
      if (!is.null(spec$tolerance) && is.finite(spec$tolerance) && spec$tolerance > 0) return(spec$tolerance)
      if (!is.null(spec$usl) && !is.null(spec$lsl) && is.finite(spec$usl) && is.finite(spec$lsl)) {
        return(spec$usl - spec$lsl)
      }
      NULL
    }
  )
)

#' @title MsaPlotter
#' @description Plotter for Gage R&R MSA studies.
#'
#' @param results Results list produced by the analyzer or task (must expose `variance_components`, `data`, and `design_method`).
#' @param theme_obj Theme object providing `config` and `theme_iqr()` for styling.
#' @param type Plot type: `"summary"`, `"list"`, `"variation"`, `"by_part"`, `"by_operator"`, `"interaction"`, `"xbar_chart"`, or `"r_chart"`.
#' @param data Optional data frame; defaults to `results$data` when `NULL`.
#' @param ... Additional arguments passed to downstream methods.
#' @param vc Variance components tibble used by `plot_variation()`.
#' @param colors Character vector of discrete colors used by per-plot helpers.
#' @param plots Named list of ggplot/patchwork objects to save via `save_assets()`.
#' @param output_dir Directory where chart image files are written.
#' @param prefix Filename prefix for saved chart images (default `"chart"`).
#' @param width Plot width in inches passed to [ggplot2::ggsave()] (default 10).
#' @param height Plot height in inches passed to [ggplot2::ggsave()] (default 6).
#' @param dpi Resolution in dots per inch passed to [ggplot2::ggsave()] (default 150).
#'
#' @export
MsaPlotter <- R6::R6Class(
  "MsaPlotter",
  inherit = IqrPlotterBase,
  public = list(
    available_plots = function(results = NULL) {
      c("summary", "list", "variation", "by_part", "by_operator", "interaction", "xbar_chart", "r_chart")
    },

    render = function(results, theme_obj, type = "summary", data = NULL, ...) {
      if (is.null(data)) data <- results$data
      type <- self$normalize_type(type)
      plots <- self$generate_plots(results, data, theme_obj)
      if (type == "list") return(plots)
      if (type == "summary") {
        # Minitab-style 6-panel layout (3 rows x 2 cols)
        # Crossed: (variation + interaction) / (xbar + by_operator) / (r + by_part)
        # Nested:  (variation) / (xbar + by_operator) / (r + by_part)
        is_nested <- identical(results$design_method, "nested") || is.null(plots$interaction)
        if (is_nested) {
          p <- (plots$variation) /
               (plots$xbar_chart + plots$by_operator) /
               (plots$r_chart + plots$by_part)
        } else {
          p <- (plots$variation + plots$interaction) /
               (plots$xbar_chart + plots$by_operator) /
               (plots$r_chart + plots$by_part)
        }
        return(p)
      }
      plots[[type]] %||% stop("Unknown MSA plot type: ", type, call. = FALSE)
    },

    normalize_type = function(type) {
      aliases <- c(all = "summary", full = "summary")
      if (type %in% names(aliases)) type <- aliases[[type]]
      valid <- self$available_plots()
      if (!type %in% valid) {
        stop("Unknown MSA plot type: ", type, ". Valid types: ", paste(valid, collapse = ", "), call. = FALSE)
      }
      type
    },

    generate_plots = function(results, data, theme_obj) {
      config <- theme_obj$config
      colors <- config$data$discrete
      if (is.null(colors) || length(colors) < 4) {
        colors <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377")
      }
      colors <- rep(colors, length.out = 8)
      plots <- list(
        variation = self$plot_variation(results$variance_components, colors, theme_obj),
        by_part = self$plot_by_part(data, colors, theme_obj),
        by_operator = self$plot_by_operator(data, colors, theme_obj),
        interaction = self$plot_interaction(data, colors, theme_obj),
        xbar_chart = self$plot_xbar(data, theme_obj),
        r_chart = self$plot_r(data, theme_obj)
      )
      if (identical(results$design_method, "nested")) {
        plots$interaction <- NULL
      }
      plots
    },

    plot_variation = function(vc, colors, theme_obj) {
      # Standard Minitab-style components of variation chart
      # Fixed 4 x-axis groups, grouped bars for %Contribution, %StudyVar, %Tolerance (if available)
      target_sources <- c("Total Gage R&R", "  Repeatability", "  Reproducibility", "Part-to-Part")
      short_labels <- c(
        "Total Gage R&R" = "Gage R&R",
        "  Repeatability" = "Repeat",
        "  Reproducibility" = "Reprod",
        "Part-to-Part" = "Part-to-Part"
      )

      plot_data <- vc %>%
        dplyr::filter(.data$Source %in% target_sources) %>%
        dplyr::mutate(SourceLabel = factor(
          short_labels[.data$Source],
          levels = unname(short_labels)
        ))

      # Determine which metrics to plot
      metric_cols <- c("PercentContribution", "PercentStudyVar")
      metric_labels <- c("PercentContribution" = "% Contribution",
                         "PercentStudyVar" = "% Study Var")
      if ("PercentTolerance" %in% names(vc)) {
        metric_cols <- c(metric_cols, "PercentTolerance")
        metric_labels["PercentTolerance"] <- "% Tolerance"
      }

      plot_data <- plot_data %>%
        dplyr::select("SourceLabel", dplyr::all_of(metric_cols)) %>%
        tidyr::pivot_longer(-"SourceLabel", names_to = "Metric", values_to = "Value") %>%
        dplyr::mutate(
          SourceLabel = factor(.data$SourceLabel, levels = unname(short_labels)),
          Metric = factor(.data$Metric, levels = metric_cols, labels = metric_labels)
        )

      # Determine y-axis upper bound for label placement
      y_max <- max(plot_data$Value, na.rm = TRUE) * 1.15

      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$SourceLabel, y = .data$Value, fill = .data$Metric)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), alpha = 0.9, width = 0.7) +
        # GRR acceptance thresholds: 10% (accept) and 30% (marginal)
        ggplot2::geom_hline(yintercept = 10, color = "#228833", linetype = "dashed", linewidth = 0.4, alpha = 0.6) +
        ggplot2::geom_hline(yintercept = 30, color = "#CCBB44", linetype = "dashed", linewidth = 0.4, alpha = 0.6) +
        ggplot2::labs(
          title = "Components of Variation",
          x = NULL,
          y = "Percent"
        ) +
        ggplot2::scale_fill_manual(values = colors[seq_along(metric_cols)]) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)), limits = c(0, NA)) +
        theme_obj$theme_iqr() +
        ggplot2::theme(
          legend.position = "top",
          legend.title = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
          plot.subtitle = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank()
        )

      p
    },

    plot_by_part = function(data, colors, theme_obj) {
      ggplot2::ggplot(data, ggplot2::aes(x = .data$Part, y = .data$MeasurementValue, color = .data$Operator)) +
        ggplot2::geom_point(alpha = 0.7, size = 1.5, position = ggplot2::position_jitter(width = 0.1)) +
        ggplot2::stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = colors[[5]]) +
        ggplot2::stat_summary(fun = "mean", geom = "line", group = 1, color = colors[[5]], linewidth = 0.6) +
        ggplot2::labs(
          title = "Response by Part",
          x = "Part",
          y = "Measurement",
          color = "Operator"
        ) +
        ggplot2::scale_color_manual(values = colors) +
        theme_obj$theme_iqr() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       legend.position = "top",
                       legend.title = ggplot2::element_blank(),
                       plot.subtitle = ggplot2::element_blank())
    },

    plot_by_operator = function(data, colors, theme_obj) {
      ggplot2::ggplot(data, ggplot2::aes(x = .data$Operator, y = .data$MeasurementValue, fill = .data$Operator)) +
        ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = 4, color = "grey30") +
        ggplot2::stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = colors[[5]]) +
        ggplot2::stat_summary(fun = mean, geom = "line", ggplot2::aes(group = 1), color = colors[[5]], linewidth = 0.6, linetype = "dashed") +
        ggplot2::labs(
          title = "Response by Operator",
          x = "Operator",
          y = "Measurement"
        ) +
        ggplot2::scale_fill_manual(values = colors) +
        theme_obj$theme_iqr() +
        ggplot2::theme(legend.position = "none",
                       plot.subtitle = ggplot2::element_blank())
    },

    plot_interaction = function(data, colors, theme_obj) {
      ggplot2::ggplot(data, ggplot2::aes(x = .data$Part, y = .data$MeasurementValue, color = .data$Operator, group = .data$Operator, shape = .data$Operator)) +
        ggplot2::stat_summary(fun = "mean", geom = "point", size = 2.5) +
        ggplot2::stat_summary(fun = "mean", geom = "line", linewidth = 0.7) +
        ggplot2::labs(
          title = "Part x Operator Interaction",
          x = "Part",
          y = "Average measurement",
          color = "Operator",
          shape = "Operator"
        ) +
        ggplot2::scale_color_manual(values = colors) +
        theme_obj$theme_iqr() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "top")
    },

    plot_xbar = function(data, theme_obj) {
      colors <- theme_obj$config$data$discrete %||% c("#4477AA", "#EE6677", "#228833")
      if (length(colors) < 3) colors <- c("#4477AA", "#EE6677", "#228833")
      cl_color <- "#3B7DD8"
      lim_color <- "#E74C3C"

      data <- data %>% dplyr::arrange(.data$Operator, .data$Part, dplyr::row_number())
      unique_parts <- unique(data$Part)
      data$Part <- factor(data$Part, levels = unique_parts)
      data$Operator <- factor(data$Operator, levels = unique(data$Operator))

      subgroup_stats <- data %>%
        dplyr::group_by(.data$Operator, .data$Part) %>%
        dplyr::summarize(
          Mean = mean(.data$MeasurementValue),
          n = dplyr::n(),
          .groups = "drop"
        )

      grand_mean <- mean(data$MeasurementValue)
      range_values <- data %>%
        dplyr::group_by(.data$Operator, .data$Part) %>%
        dplyr::summarize(R = max(.data$MeasurementValue) - min(.data$MeasurementValue), .groups = "drop")
      R_bar <- mean(range_values$R)
      subgroup_n <- stats::median(subgroup_stats$n)

      constants <- private$control_chart_constants(subgroup_n)
      UCL <- grand_mean + constants$A2 * R_bar
      LCL <- grand_mean - constants$A2 * R_bar

      subgroup_stats <- subgroup_stats %>%
        dplyr::mutate(out_of_control = .data$Mean > UCL | .data$Mean < LCL)

      n_parts <- length(unique_parts)
      label_x <- n_parts + 0.5

      p <- ggplot2::ggplot(subgroup_stats, ggplot2::aes(x = .data$Part, y = .data$Mean)) +
        ggplot2::geom_hline(yintercept = grand_mean, color = cl_color, linewidth = 0.5) +
        ggplot2::geom_hline(yintercept = UCL, color = lim_color, linewidth = 0.5, linetype = "dashed") +
        ggplot2::geom_hline(yintercept = LCL, color = lim_color, linewidth = 0.5, linetype = "dashed") +
        ggplot2::geom_line(ggplot2::aes(group = .data$Operator), color = colors[[1]], linewidth = 0.5) +
        ggplot2::geom_point(color = colors[[1]], size = 1.5) +
        ggplot2::annotate("text", x = label_x, y = UCL, label = sprintf("UCL=%.3f", UCL),
                          hjust = 1, vjust = -0.5, size = 2.5, color = lim_color) +
        ggplot2::annotate("text", x = label_x, y = grand_mean, label = sprintf("X=%.3f", grand_mean),
                          hjust = 1, vjust = -0.5, size = 2.5, color = cl_color) +
        ggplot2::annotate("text", x = label_x, y = LCL, label = sprintf("LCL=%.3f", LCL),
                          hjust = 1, vjust = 1.3, size = 2.5, color = lim_color)

      if (any(subgroup_stats$out_of_control)) {
        p <- p + ggplot2::geom_point(
          data = subset(subgroup_stats, out_of_control),
          ggplot2::aes(x = .data$Part, y = .data$Mean),
          color = "#E74C3C", size = 2.5, shape = 1, stroke = 1.2
        )
      }

      # Horizontal facet by Operator with near-zero spacing (Minitab style)
      p <- p + ggplot2::facet_grid(. ~ Operator) +
        ggplot2::labs(
          title = "X-bar Chart by Operator",
          x = "Part",
          y = "Sample Mean"
        ) +
        theme_obj$theme_iqr() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
          strip.background = ggplot2::element_rect(fill = "grey85", color = NA),
          strip.text = ggplot2::element_text(face = "bold", size = 8),
          panel.spacing.x = ggplot2::unit(0.01, "cm"),
          plot.subtitle = ggplot2::element_blank()
        )

      p
    },

    plot_r = function(data, theme_obj) {
      colors <- theme_obj$config$data$discrete %||% c("#4477AA", "#EE6677", "#228833")
      if (length(colors) < 3) colors <- c("#4477AA", "#EE6677", "#228833")
      cl_color <- "#3B7DD8"
      lim_color <- "#E74C3C"

      data <- data %>% dplyr::arrange(.data$Operator, .data$Part, dplyr::row_number())
      unique_parts <- unique(data$Part)
      data$Part <- factor(data$Part, levels = unique_parts)
      data$Operator <- factor(data$Operator, levels = unique(data$Operator))

      range_data <- data %>%
        dplyr::group_by(.data$Operator, .data$Part) %>%
        dplyr::summarize(
          R = max(.data$MeasurementValue) - min(.data$MeasurementValue),
          n = dplyr::n(),
          .groups = "drop"
        )

      R_bar <- mean(range_data$R)
      subgroup_n <- stats::median(range_data$n)

      constants <- private$control_chart_constants(subgroup_n)
      UCL <- constants$D4 * R_bar
      LCL <- max(0, constants$D3 * R_bar)

      range_data <- range_data %>%
        dplyr::mutate(out_of_control = .data$R > UCL | .data$R < LCL)

      n_parts <- length(unique_parts)
      label_x <- n_parts + 0.5

      p <- ggplot2::ggplot(range_data, ggplot2::aes(x = .data$Part, y = .data$R)) +
        ggplot2::geom_hline(yintercept = R_bar, color = cl_color, linewidth = 0.5) +
        ggplot2::geom_hline(yintercept = UCL, color = lim_color, linewidth = 0.5, linetype = "dashed") +
        ggplot2::geom_hline(yintercept = LCL, color = lim_color, linewidth = 0.5, linetype = "dashed") +
        ggplot2::geom_line(ggplot2::aes(group = .data$Operator), color = colors[[1]], linewidth = 0.5) +
        ggplot2::geom_point(color = colors[[1]], size = 1.5) +
        ggplot2::annotate("text", x = label_x, y = UCL, label = sprintf("UCL=%.3f", UCL),
                          hjust = 1, vjust = -0.5, size = 2.5, color = lim_color) +
        ggplot2::annotate("text", x = label_x, y = R_bar, label = sprintf("R=%.3f", R_bar),
                          hjust = 1, vjust = -0.5, size = 2.5, color = cl_color) +
        ggplot2::annotate("text", x = label_x, y = LCL, label = sprintf("LCL=%.3f", LCL),
                          hjust = 1, vjust = 1.3, size = 2.5, color = lim_color)

      if (any(range_data$out_of_control)) {
        p <- p + ggplot2::geom_point(
          data = subset(range_data, out_of_control),
          ggplot2::aes(x = .data$Part, y = .data$R),
          color = "#E74C3C", size = 2.5, shape = 1, stroke = 1.2
        )
      }

      # Horizontal facet by Operator with near-zero spacing (Minitab style)
      p <- p + ggplot2::facet_grid(. ~ Operator) +
        ggplot2::labs(
          title = "R Chart by Operator",
          x = "Part",
          y = "Sample Range"
        ) +
        theme_obj$theme_iqr() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
          strip.background = ggplot2::element_rect(fill = "grey85", color = NA),
          strip.text = ggplot2::element_text(face = "bold", size = 8),
          panel.spacing.x = ggplot2::unit(0.01, "cm"),
          plot.subtitle = ggplot2::element_blank()
        )

      p
    },

    save_assets = function(plots, output_dir, prefix = "chart", width = 10, height = 6, dpi = 150) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      chart_paths <- list()
      for (name in names(plots)) {
        if (inherits(plots[[name]], "ggplot") || inherits(plots[[name]], "patchwork")) {
          chart_paths[[name]] <- file.path(output_dir, paste0(prefix, "_", name, ".png"))
          ggplot2::ggsave(chart_paths[[name]], plots[[name]], width = width, height = height, dpi = dpi)
        }
      }
      chart_paths
    }
  ),
  private = list(
    control_chart_constants = function(n) {
      n <- as.integer(round(n))
      if (n < 2) n <- 2
      list(
        A2 = iQualityR.stat::get_A2(n),
        D3 = iQualityR.stat::get_D3(n),
        D4 = iQualityR.stat::get_D4(n)
      )
    }
  )
)

#' @title MsaTask
#' @description Task coordinator for Gage R&R MSA studies.
#'
#' @param data Data frame of measurements with columns `MeasurementValue`, `Part`, and `Operator`.
#' @param plan Optional MSA plan object providing parameters and specifications.
#' @param theme Theme name or object accepted by the base task (default `"academic"`).
#' @param analysis_method Character scalar: `"anova"` or `"xbar_r"`.
#' @param alpha_lim Numeric scalar p-value threshold for keeping the interaction term in ANOVA (default 0.25).
#' @param sigma Multiplier for study variation (default 6).
#' @param type Plot type: `"summary"`, `"list"`, `"variation"`, `"by_part"`, `"by_operator"`, `"interaction"`, `"xbar_chart"`, or `"r_chart"`.
#' @param ... Additional arguments passed to downstream methods.
#' @param format Report format: `"excel"`, `"html"`, `"pdf"`, `"word"`, or `"docx"`.
#' @param path Output file path. If `NULL`, a default path is generated.
#' @param template_path Optional RMD template path for custom report rendering.
#'
#' @export
MsaTask <- R6::R6Class(
  "MsaTask",
  inherit = IqrTaskBase,
  public = list(
    #' @field plan Optional MSA plan object with study parameters.
    plan = NULL,
    #' @field analysis_method Character scalar: `"anova"` or `"xbar_r"`.
    analysis_method = NULL,
    #' @field anova_results List holding ANOVA tables and metadata after `compute()`.
    anova_results = NULL,
    #' @field variance_components Tibble of variance components after `compute()`.
    variance_components = NULL,
    #' @field statistics List of summary statistics (ndc, total_gage_rr_percent, tolerance_percent) after `compute()`.
    statistics = NULL,
    #' @field plots Named list of generated plots after `generate_plots()`.
    plots = NULL,
    #' @field interpretation List with acceptance status, message, gage_rr_percent, and ndc.
    interpretation = NULL,

    initialize = function(data, plan = NULL, theme = "academic",
                          analysis_method = c("anova", "xbar_r")) {
      analysis_method <- match.arg(analysis_method)
      super$initialize(data = data, theme = theme)
      self$plan <- plan
      self$analysis_method <- analysis_method
      self$executor$analyzer <- MsaAnalyzer$new()
      self$executor$plotter <- MsaPlotter$new()
      invisible(self)
    },

    compute = function(alpha_lim = 0.25, sigma = 6) {
      self$executor$analyzer$run(
        self$data,
        plan = self$plan,
        analysis_method = self$analysis_method,
        alpha_lim = alpha_lim,
        sigma = sigma
      )
      analyzer <- self$executor$analyzer
      self$data <- analyzer$results$raw_output$data
      self$anova_results <- analyzer$anova_results
      self$variance_components <- analyzer$variance_components
      self$statistics <- analyzer$statistics
      self$interpretation <- analyzer$interpretation
      self$results <- analyzer$results
      invisible(self)
    },

    generate_plots = function() {
      if (is.null(self$variance_components)) stop("Please call compute() first.", call. = FALSE)
      report_results <- self$build_report_results()
      self$plots <- self$executor$plotter$render(report_results, self$theme_obj, type = "list", data = self$data)
      self$plots
    },

    plot = function(type = "summary", ...) {
      if (is.null(self$variance_components)) stop("Please call compute() first.", call. = FALSE)
      self$executor$plotter$render(self$build_report_results(), self$theme_obj, type = type, data = self$data, ...)
    },

    build_report_results = function() {
      list(
        study_type = "msa",
        analysis_method = self$analysis_method,
        variance_components = self$variance_components,
        gage_evaluation = self$executor$analyzer$build_gage_evaluation_table(),
        anova_table = self$executor$analyzer$build_anova_table(),
        statistics = self$statistics,
        interpretation = self$interpretation,
        anova_results = self$anova_results,
        data = self$data,
        num_operators = length(unique(self$data$Operator)),
        num_parts = length(unique(self$data$Part)),
        num_repeats = {
          reps <- self$data %>% dplyr::count(.data$Operator, .data$Part) %>% dplyr::pull(.data$n)
          if (length(unique(reps)) == 1) unique(reps) else NA_integer_
        },
        total_measurements = nrow(self$data),
        design_method = if (!is.null(self$plan)) self$plan$method else "crossed",
        num_measurements = {
          reps <- self$data %>% dplyr::count(.data$Operator, .data$Part) %>% dplyr::pull(.data$n)
          if (length(unique(reps)) == 1) unique(reps) else NA_integer_
        },
        plots = self$plots,
        to_excel = function(plan = self$plan) self$build_excel_sheets()
      )
    },

    build_excel_sheets = function() {
      sheets <- list(
        Variance_Components = self$variance_components,
        Gage_Evaluation = .msa_round_numeric_columns(self$executor$analyzer$build_gage_evaluation_table(), 4),
        Data = self$data
      )
      anova_tbl <- self$executor$analyzer$build_anova_table()
      if (nrow(anova_tbl) > 0) sheets$ANOVA <- .msa_round_numeric_columns(anova_tbl, 4)
      if (!is.null(self$anova_results$with_interaction)) {
        sheets$ANOVA_With_Interaction <- self$anova_results$with_interaction
      }
      if (!is.null(self$anova_results$without_interaction)) {
        sheets$ANOVA_Without_Interaction <- self$anova_results$without_interaction
      }
      sheets
    },

    summary = function() {
      cat("\n========== MSA Analysis Summary ==========\n")
      cat("Analysis Method:", self$analysis_method, "\n")
      cat("\n--- Gage R&R Study Variation ---\n")
      vc <- self$variance_components
      display <- data.frame(
        Source = trimws(vc$Source),
        VarComp = .msa_fmt_num(vc$Variance, 6),
        Percent_Contribution = .msa_fmt_pct(vc$PercentContribution, 2),
        StdDev = .msa_fmt_num(vc$StdDev, 6),
        StudyVar = .msa_fmt_num(vc$StudyVar, 6)
      )
      if ("PercentStudyVar" %in% names(vc)) {
        display$Percent_StudyVar <- .msa_fmt_pct(vc$PercentStudyVar, 2)
      }
      if ("PercentTolerance" %in% names(vc)) {
        display$Percent_Tolerance <- .msa_fmt_pct(vc$PercentTolerance, 2)
      }
      .msa_print_table(display)
      cat("\n--- Key Statistics ---\n")
      .msa_print_table(data.frame(
        Metric = c("Total Gage R&R %Contribution", "Total Gage R&R %Tolerance", "Number of Distinct Categories"),
        Value = c(
          .msa_fmt_pct(self$statistics$total_gage_rr_percent, 2),
          ifelse(is.na(self$statistics$tolerance_percent), "NA", .msa_fmt_pct(self$statistics$tolerance_percent, 2)),
          self$statistics$ndc
        )
      ))
      cat("\n--- Interpretation ---\n")
      cat("Status:", self$interpretation$status, "-", self$interpretation$message, "\n")
      cat("==========================================\n\n")
      invisible(self)
    },

    report = function(format = c("excel", "html", "pdf", "word", "docx"),
                      path = NULL,
                      template_path = NULL) {
      report_format <- .msa_format_report(format, allowed = c("excel", "html", "pdf", "word", "docx"))
      if (is.null(self$variance_components)) {
        stop("No results available. Please call compute() first.", call. = FALSE)
      }

      if (is.null(path)) {
        path <- .msa_default_report_path("msa", report_format, prefix = "msa_report")
      }
      out_dir <- dirname(path)
      if (out_dir == "." || out_dir == "") out_dir <- getwd()
      base_name <- basename(path)
      if (grepl("\\.(xlsx|html|pdf|docx)$", base_name)) {
        base_name <- sub("\\.(xlsx|html|pdf|docx)$", "", base_name)
      }

      output_file_path <- path
      chart_dir <- NULL
      chart_paths <- NULL
      if (report_format != "excel") {
        if (is.null(self$plots)) self$generate_plots()
        chart_dir <- file.path(out_dir, paste0(base_name, "_figures"))
        chart_paths <- self$executor$plotter$save_assets(self$plots, chart_dir, prefix = "chart")
      }

      study_type <- if (!is.null(self$plan)) {
        paste("MSA Plan:", self$plan$meta_data$project$plan_name)
      } else {
        "Gage R&R Study"
      }

      if (!is.null(template_path)) {
        reporter <- .msa_get_reporter(self$theme_obj)
        reporter$register("msa", rmd_template = template_path)
      }

      output_path <- .msa_export_report(
        results = self$build_report_results(),
        plan = self$plan,
        task_tag = "msa",
        format = report_format,
        path = path,
        theme_obj = self$theme_obj,
        study_type = study_type,
        chart_paths = chart_paths,
        report_date = Sys.Date()
      )
      message("Report generated: ", output_file_path)
      message("Chart directory: ", chart_dir)
      invisible(output_path)
    },

    to_excel = function(path = NULL) {
      if (is.null(path)) path <- .msa_default_report_path("msa", "excel", prefix = "msa_report")
      return(self$report(format = "excel", path = path))
      message("Report exported to: ", path)
      invisible(path)
    }
  )
)
