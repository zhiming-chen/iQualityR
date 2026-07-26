# =============================================================================
# File: iQualityR.stat/R/desc.R
# Description: Descriptive statistical analysis - functional implementation
# Simple and efficient, no R6 framework required
# Dependencies: iQualityR.plot (for base_plot theme support)
#       ggplot2, patchwork, nortest, moments, openxlsx
# =============================================================================

# -----------------------------------------------------------------------------
# Core calculation functions
# -----------------------------------------------------------------------------

#' @importFrom iQualityR.plot base_plot layers_histogram_density layers_boxplot
#' @title Descriptive statistics calculation
#' @param x Numeric vector
#' @param conf Confidence level
#' @return Descriptive statistics list
#' @export
desc_calc <- function(x, conf = 0.95) {
  v <- na.omit(as.numeric(x))
  n <- length(v)
  m <- mean(v)
  s <- sd(v)

  se <- s / sqrt(n)
  error <- qt(conf + (1 - conf) / 2, df = n - 1) * se
  ad <- nortest::ad.test(v)

  list(
    n = n, mean = m, stdev = s, median = median(v),
    min = min(v), max = max(v), range = max(v) - min(v),
    q1 = quantile(v, 0.25), q3 = quantile(v, 0.75), iqr = quantile(v, 0.75) - quantile(v, 0.25),
    skew = moments::skewness(v), kurt = moments::kurtosis(v) - 3,
    cv = if (m != 0) s / abs(m) * 100 else NA,
    ad_stat = ad$statistic, p_value = ad$p.value,
    ci_mean = c(m - error, m + error), se = se, raw = v
  )
}

#' @title Batch descriptive statistics
#' @param data Data frame
#' @param vars Variable vector
#' @param conf Confidence level
#' @return Statistics result list
#' @export
desc_analyze <- function(data, vars = NULL, conf = 0.95) {
  if (!is.data.frame(data)) stop("data must be a data frame")

  if (is.null(vars)) vars <- names(data)[sapply(data, is.numeric)]
  if (length(vars) == 0) stop("No numeric variables found")

  results <- list()
  for (v in vars) {
    results[[v]] <- desc_calc(data[[v]], conf)
  }
  results
}

# -----------------------------------------------------------------------------
# Plotting functions - depends on iQualityR.plot base functions
# -----------------------------------------------------------------------------

#' @title Descriptive statistics histogram
#' @param s Result returned by desc_calc
#' @param theme Theme (default "prism")
#' @return ggplot object
#' @export
desc_hist <- function(s, theme = "prism") {
  df <- data.frame(v = s$raw)
  lims <- c(min(df$v) - diff(range(df$v)) * 0.15, max(df$v) + diff(range(df$v)) * 0.15)

  p <- base_plot(df, ggplot2::aes(x = v), theme = theme) +
    layers_histogram_density(
      bins = 20,      
      fill = "#1259aa",
      color = "white", density_args = list(alpha = 0.6,fill = "#A9C4E3")
    ) +
    ggplot2::stat_function(
      fun = dnorm, args = list(mean = s$mean, sd = s$stdev),
      color = "red", linewidth = 1
    ) +
    ggplot2::scale_x_continuous(limits = lims) +
    ggplot2::labs(title = sprintf("Histogram: %s", s$var_name %||% "Variable"), x = NULL, y = "Density")
  p
}

#' @title Descriptive statistics boxplot
#' @param s Result returned by desc_calc
#' @param theme Theme (default "prism")
#' @return ggplot object
#' @export
desc_box <- function(s, theme = "prism") {
  df <- data.frame(v = s$raw)
  lims <- c(min(df$v) - diff(range(df$v)) * 0.15, max(df$v) + diff(range(df$v)) * 0.15)

  p <- base_plot(df, ggplot2::aes(x = v, y = ""), theme = theme) +
    layers_boxplot(
      add_jitter = TRUE,
      boxplot_args = list(fill = "#F2F2F2", outlier.color = "red", width = 0.6)
    ) +
    ggplot2::scale_x_continuous(limits = lims) +
    ggplot2::labs(title = sprintf("Boxplot: %s", s$var_name %||% "Variable"), x = "Value", y = NULL) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank())
  p
}

#' @title Descriptive statistics boxplot (with statistics table)
#' @param s Result returned by desc_calc
#' @param theme Theme (default "prism")
#' @return ggplot object
#' @export
desc_box_with_stats <- function(s, theme = "prism") {
  df <- data.frame(v = s$raw)
  lims <- c(min(df$v) - diff(range(df$v)) * 0.15, max(df$v) + diff(range(df$v)) * 0.15)

  stats_df <- data.frame(
    Metric = c("N", "Mean", "SD", "Median", "CV(%)", "Skewness", "Kurtosis"),
    Value = c(
      s$n,
      sprintf("%.2f", s$mean),
      sprintf("%.2f", s$stdev),
      sprintf("%.2f", s$median),
      sprintf("%.2f%%", s$cv),
      sprintf("%.3f", s$skew),
      sprintf("%.3f", s$kurt)
    ),
    stringsAsFactors = FALSE
  )

  table_grob <- gridExtra::tableGrob(
    stats_df,
    rows = NULL,
    theme = gridExtra::ttheme_default(
      base_size = 9,
      base_colour = "gray30",
      padding = grid::unit(c(3, 5), "mm")
    )
  )

  p <- base_plot(df, ggplot2::aes(x = 1, y = v), theme = theme) +
    layers_boxplot(
      add_jitter = TRUE,
      jitter_args = list(width = 0.08, alpha = 0.3, size = 0.8, color = "gray40"),
      boxplot_args = list(fill = "#A9C4E3", width = 0.3, outlier.color = "red")
    ) +
    ggplot2::scale_x_continuous(limits = c(0.5, 2.0), breaks = NULL) +
    ggplot2::labs(title = "Boxplot", x = NULL, y = "Value") +
    ggplot2::annotation_custom(
      grob = table_grob,
      xmin = 1.3, xmax = 1.95,
      ymin = median(df$v) - stats::IQR(df$v) * 1.2,
      ymax = median(df$v) + stats::IQR(df$v) * 1.2
    )

  p
}

#' @title Descriptive statistics combined plot
#' @param s Result returned by desc_calc
#' @param theme Theme (default "prism")
#' @return patchwork object
#' @export
desc_plot <- function(s, theme = "prism") {
  p_hist <- desc_hist(s, theme)
  p_box_stats <- desc_box_with_stats(s, theme)
  p_hist + p_box_stats + patchwork::plot_layout(widths = c(1, 1.3)) +
    patchwork::plot_annotation(title = sprintf("Descriptive Analysis: %s", s$var_name %||% "Variable"))
}

#' @title Statistics table plot
#' @param s Result returned by desc_calc
#' @param theme Theme (default "prism")
#' @return ggplot object
#' @export
desc_stats_table <- function(s, theme = "prism") {
  stats_data <- data.frame(
    metric = c("N", "Mean", "SD", "Median", "CV(%)", "Skewness", "Kurtosis", "AD", "P-value"),
    value = c(s$n, sprintf("%.4f", s$mean), sprintf("%.4f", s$stdev),
              sprintf("%.4f", s$median), sprintf("%.2f", s$cv),
              sprintf("%.3f", s$skew), sprintf("%.3f", s$kurt),
              sprintf("%.3f", s$ad_stat),
              if (s$p_value < 0.001) "< 0.001" else sprintf("%.3f", s$p_value))
  )

  p <- base_plot(stats_data, ggplot2::aes(x = metric, y = value), theme = theme) +
    ggplot2::geom_text(ggplot2::aes(label = value), size = 4, hjust = 0.5, vjust = 0.5) +
    ggplot2::scale_y_discrete() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(title = "Statistics", x = NULL, y = NULL)
  p
}

# -----------------------------------------------------------------------------
# Report functions
# -----------------------------------------------------------------------------

#' @title Descriptive statistics summary table
#' @param results Result returned by desc_analyze
#' @return data.frame
#' @export
desc_summary_table <- function(results) {
  rows <- lapply(names(results), function(var) {
    s <- results[[var]]
    data.frame(
      Variable = var, N = s$n, Mean = sprintf("%.4f", s$mean),
      SD = sprintf("%.4f", s$stdev), Median = sprintf("%.4f", s$median),
      CV = sprintf("%.2f%%", s$cv), Skew = sprintf("%.3f", s$skew),
      Kurt = sprintf("%.3f", s$kurt),
      P_Value = if (s$p_value < 0.001) "< 0.001" else sprintf("%.3f", s$p_value),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @title Export to Excel report
#' @param results Result returned by desc_analyze
#' @param path Output path
#' @param excel_exporter ExcelExporter instance (from iQualityR.core)
#' @export
desc_to_excel <- function(results, path = NULL, excel_exporter = NULL) {
  if (!is.null(excel_exporter)) {
    summary_df <- desc_summary_table(results)

    details <- lapply(names(results), function(var) {
      s <- results[[var]]
      data.frame(
        Variable = var, Min = s$min, Q1 = s$q1, Mean = s$mean,
        Q3 = s$q3, Max = s$max, Range = s$range, IQR = s$iqr,
        CI_Lower = s$ci_mean[1], CI_Upper = s$ci_mean[2],
        stringsAsFactors = FALSE
      )
    })
    details_df <- do.call(rbind, details)

    data_list <- list(
      "Summary" = summary_df,
      "Details" = details_df
    )
    sheet_names <- c("Summary", "Details")

    if (!is.null(path)) {
      excel_exporter$export_excel(data_list, path = path, sheet_names = sheet_names)
    } else {
      excel_exporter$export_excel(data_list, sheet_names = sheet_names)
    }
  } else {
    if (is.null(path)) path <- paste0("desc_report_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::writeData(wb, "Summary", desc_summary_table(results))

    openxlsx::addWorksheet(wb, "Details")
    details <- lapply(names(results), function(var) {
      s <- results[[var]]
      data.frame(Variable = var, Min = s$min, Q1 = s$q1, Mean = s$mean,
                 Q3 = s$q3, Max = s$max, Range = s$range, IQR = s$iqr,
                 CI_Lower = s$ci_mean[1], CI_Upper = s$ci_mean[2])
    })
    openxlsx::writeData(wb, "Details", do.call(rbind, details))

    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    message("[iQualityR] Report saved: ", path)
    invisible(path)
  }
}

# -----------------------------------------------------------------------------
# Quick entry function
# -----------------------------------------------------------------------------

#' @title Descriptive statistics quick analysis function
#' @param data Data frame or numeric vector
#' @param vars Variable vector
#' @param conf Confidence level
#' @param plot Whether to plot
#' @param report Whether to generate report
#' @param theme Theme
#' @return Invisibly returns results list, or outputs plots and report simultaneously
#' @export
iqr_desc <- function(data, vars = NULL, conf = 0.95, plot = FALSE, report = FALSE, theme = "prism") {
  if (is.numeric(data) && length(data) > 1) {
    data <- data.frame(Val = data, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (is.null(vars)) vars <- names(data)[sapply(data, is.numeric)]

  results <- desc_analyze(data, vars, conf)
  for (n in names(results)) results[[n]]$var_name <- n

  if (plot) {
    for (v in names(results)) {
      print(desc_plot(results[[v]], theme = theme))
    }
  }

  if (report) {
    desc_to_excel(results)
  }

  invisible(results)
}


