# =============================================================================
# File: R/plot_capability_process_table.R
# Description: Process data & capability summary table grob for the Capability
#   Sixpack "7th panel" (bottom data row). Three columns: Process Data,
#   Capability Indices, Performance — each showing Within vs Overall side by
#   side. Uses create_stats_table() for consistent theming.
# =============================================================================

#' Build Process Data & Capability Summary Table
#'
#' Creates a three-column horizontal table grob summarising the capability
#' analysis results:
#'   1. Process Data (Mean, SD, Variance, N, Subgroup size — Within vs Overall)
#'   2. Capability Indices (Cp, Cpk, Pp, Ppk, CCpk, K — Value + 95% CI + Verdict)
#'   3. Performance (PPM obs/exp, Yield, Z.Bench, Sigma — Within vs Overall)
#'
#' Uses \code{\link{create_stats_table}} internally so all colours resolve via
#' the IqrTheme toolbox.
#'
#' @param stats A list of statistics from the capability analysis (typically
#'   \code{results$statistics}). Expected fields: mean, sd_within, sd_overall,
#'   variance_within, variance_overall, n_subgroups, subgroup_size, n_obs,
#'   cp, cpk, pp, ppk, ccpk, k, cp_ci (length-2), cpk_ci, pp_ci, ppk_ci,
#'   ppm_obs, ppm_exp_within, ppm_exp_overall, yield_within, yield_overall,
#'   z_bench_within, z_bench_overall, sigma_level, overall_verdict.
#' @param theme Theme spec.
#' @param digits Integer. Decimal places (default 4 for values, 0 for PPM).
#' @return A \code{gridExtra::tableGrob} object combining the three columns.
#' @export
plot_capability_process_table <- function(stats, theme = NULL, digits = 4) {
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("gridExtra is required for plot_capability_process_table.")
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("grid is required for plot_capability_process_table.")
  }

  # ---- Resolve theme aesthetics ------------------------------------------
  c <- .iqr_aes(theme)
  font_family <- tryCatch(
    .iqr_plotter$.pal_ui(c$theme_obj, "font_family", default = "sans") %||% "sans",
    error = function(e) "sans"
  )
  # Derive table font size from theme; fall back to 11 (readable in a multi-panel layout).
  # The table text needs to be slightly smaller than the plot base font (typically 12),
  # but large enough to be readable when the Sixpack is rendered at typical sizes.
  table_font_size <- tryCatch(
    .iqr_plotter$.pal_ui(c$theme_obj, "base_font_size", default = 11) %||% 11,
    error = function(e) 11
  )
  if (!is.numeric(table_font_size) || table_font_size < 8) table_font_size <- 11

  # ---- Build the three sub-tables ----------------------------------------
  null_to_dash <- function(x) if (is.null(x)) "-" else x

  # 1. Process Data
  pd_mean_w <- null_to_dash(stats$mean)
  pd_mean_o <- null_to_dash(stats$mean)
  pd_sd_w <- null_to_dash(stats$sd_within %||% stats$sd)
  pd_sd_o <- null_to_dash(stats$sd_overall %||% stats$sd)
  pd_var_w <- null_to_dash(stats$variance_within)
  pd_var_o <- null_to_dash(stats$variance_overall)
  pd_n_sub <- null_to_dash(stats$n_subgroups)
  pd_sub_size <- null_to_dash(stats$subgroup_size)
  pd_n_obs <- null_to_dash(stats$n_obs)

  df_process <- data.frame(
    Metric = c("Mean", "SD", "Variance", "N (subgroups)",
               "Subgroup size", "Total obs"),
    Within = c(pd_mean_w, pd_sd_w, pd_var_w, pd_n_sub, pd_sub_size, pd_n_obs),
    Overall = c(pd_mean_o, pd_sd_o, pd_var_o, "-", "-", pd_n_obs),
    stringsAsFactors = FALSE
  )
  # Format numeric columns
  for (col in c("Within", "Overall")) {
    is_num <- !is.na(suppressWarnings(as.numeric(df_process[[col]]))) &
              df_process[[col]] != "-"
    df_process[is_num, col] <- vapply(
      df_process[is_num, col],
      function(v) formatC(as.numeric(v), format = "f", digits = digits),
      character(1)
    )
  }

  # 2. Capability Indices
  ci_to_str <- function(ci) {
    if (is.null(ci) || length(ci) < 2) "-"
    else sprintf("[%s, %s]", .fmt_spec(ci[1]), .fmt_spec(ci[2]))
  }

  idx_names <- c("Cp", "Cpk", "Pp", "Ppk", "CCpk", "K")
  idx_vals <- list(stats$cp, stats$cpk, stats$pp, stats$ppk,
                   stats$ccpk, stats$k)
  idx_cis <- list(stats$cp_ci, stats$cpk_ci, stats$pp_ci, stats$ppk_ci,
                  stats$ccpk_ci, NULL)
  # Convert list to vector (NULL -> NA), then compute verdicts
  idx_vals_vec <- vapply(idx_vals, function(v) {
    if (is.null(v) || length(v) == 0) NA_real_ else as.numeric(v[1])
  }, numeric(1))
  idx_verdicts <- vapply(idx_vals_vec, function(v) {
    if (is.na(v)) "neutral"
    else if (v >= 1.33) "pass"
    else if (v >= 1.00) "watch"
    else "fail"
  }, character(1))

  df_indices <- data.frame(
    Index = idx_names,
    Value = vapply(idx_vals, function(v) {
      if (is.null(v) || length(v) == 0 || is.na(v[1])) "-"
      else formatC(as.numeric(v[1]), format = "f", digits = digits)
    }, character(1)),
    CI_95 = vapply(idx_cis, ci_to_str, character(1)),
    Verdict = toupper(idx_verdicts),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  # 3. Performance
  ppm_o <- null_to_dash(stats$ppm_obs)
  ppm_w <- null_to_dash(stats$ppm_exp_within %||% stats$ppm_expected)
  ppm_oall <- null_to_dash(stats$ppm_exp_overall %||% stats$ppm_expected)
  yld_w <- null_to_dash(stats$yield_within)
  yld_o <- null_to_dash(stats$yield_overall)
  zb_w <- null_to_dash(stats$z_bench_within %||% stats$z_bench)
  zb_o <- null_to_dash(stats$z_bench_overall %||% stats$z_bench)
  sig <- null_to_dash(stats$sigma_level)
  verdict <- null_to_dash(stats$overall_verdict)

  df_perf <- data.frame(
    Metric = c("PPM (obs)", "PPM (exp)", "Yield %", "Z.Bench",
               "Sigma (+1.5)", "Verdict"),
    Within = c(ppm_o, ppm_w, yld_w, zb_w, sig, verdict),
    Overall = c(ppm_o, ppm_oall, yld_o, zb_o, sig, verdict),
    stringsAsFactors = FALSE
  )
  # Format numeric values in performance table
  for (col in c("Within", "Overall")) {
    is_num <- !is.na(suppressWarnings(as.numeric(df_perf[[col]]))) &
              df_perf[[col]] != "-"
    df_perf[is_num, col] <- vapply(
      df_perf[is_num, col],
      function(v) formatC(as.numeric(v), format = "f", digits = digits),
      character(1)
    )
  }

  # ---- Build three tableGrobs --------------------------------------------
  t1 <- create_stats_table(df_process, theme = theme, digits = digits,
                           status_col = NULL, zebra = TRUE,
                           font_size = table_font_size)
  t2 <- create_stats_table(df_indices, theme = theme, digits = digits,
                           status_col = "Verdict", zebra = TRUE,
                           font_size = table_font_size)
  t3 <- create_stats_table(df_perf, theme = theme, digits = digits,
                           status_col = NULL, zebra = TRUE,
                           font_size = table_font_size)

  # ---- Combine horizontally with column titles ---------------------------
  # Build a header row with the three column titles.
  # Use theme-derived font_family and base_size for consistency.
  title_grob <- gridExtra::tableGrob(
    data.frame(C1 = "Process Data", C2 = "Capability Indices",
               C3 = "Performance", stringsAsFactors = FALSE),
    rows = NULL,
    theme = gridExtra::ttheme_default(
      base_size = table_font_size + 2,
      base_colour = c$text,
      base_family = font_family,
      padding = grid::unit(c(4, 6), "mm"),
      colhead = list(
        fg_params = list(fontface = "bold", col = c$surface),
        bg_params = list(fill = c$primary)
      )
    )
  )

  # Arrange: title row on top, three tables side by side below.
  # CRITICAL: Use arrangeGrob (returns grob) NOT grid.arrange (returns NULL).
  # The returned grob is consumed by patchwork::wrap_elements() in the Sixpack.
  # Title row uses generous "cm" units so it is large enough to be readable;
  # table body gets the remaining space via "null".
  combined <- gridExtra::arrangeGrob(
    title_grob,
    gridExtra::arrangeGrob(t1, t2, t3, ncol = 3),
    nrow = 2,
    heights = grid::unit(c(2.8, 5), c("cm", "null"))
  )

  combined
}

# Local helper (avoid depending on rlang `%||%` import)
`%||%` <- function(a, b) if (is.null(a)) b else a