# Internal helpers for the iQualityR.msa task layer.

.msa_task_tags <- list(
  type1 = "type1",
  attr_gage = "attr_gage",
  msa = "msa"
)

.msa_default_report_path <- function(task_tag, format = "excel", prefix = NULL) {
  ext <- switch(format,
    excel = "xlsx",
    html = "html",
    pdf = "pdf",
    word = "docx",
    docx = "docx",
    rmd = "Rmd",
    format
  )
  prefix <- prefix %||% paste0(task_tag, "_report")
  timestamp <- base::format(Sys.time(), "%Y%m%d_%H%M%S")
  paste0(prefix, "_", timestamp, ".", ext)
}

.msa_template_path <- function(task_tag) {
  template_file <- paste0(task_tag, "_template.Rmd")

  template <- system.file("templates", template_file, package = "iQualityR.msa")
  if (template != "" && file.exists(template)) {
    return(template)
  }

  repo_template <- file.path(getwd(), "iQualityR.msa", "inst", "templates", template_file)
  if (file.exists(repo_template)) {
    return(repo_template)
  }

  stop("MSA report template not found: ", template_file, call. = FALSE)
}

.msa_get_reporter <- function(theme_obj = NULL) {
  reporter <- getOption("iqr_reporter")
  if (!is.null(reporter)) {
    return(reporter)
  }

  if (is.null(theme_obj)) {
    theme_obj <- IqrTheme$new()
  }
  reporter <- IqrReporter$new(theme_obj)
  options(iqr_reporter = reporter)
  reporter
}

.msa_register_template <- function(reporter, task_tag) {
  if (!is.null(reporter$register) && is.function(reporter$register)) {
    reporter$register(task_tag, rmd_template = .msa_template_path(task_tag))
  }
  invisible(reporter)
}

.msa_plan_name <- function(plan, default = NULL) {
  default <- default %||% "MSA_Study"
  if (is.null(plan) || is.null(plan$meta_data$project$plan_name)) {
    return(default)
  }
  plan$meta_data$project$plan_name
}

.msa_format_report <- function(format, allowed = c("excel", "html", "pdf", "docx", "word")) {
  format <- match.arg(format, allowed)
  if (identical(format, "word")) "docx" else format
}

.msa_export_report <- function(results, plan, task_tag, format, path, theme_obj, ...) {
  reporter <- .msa_get_reporter(theme_obj)
  .msa_register_template(reporter, task_tag)
  if (identical(format, "excel")) {
    return(reporter$export(
      results = results,
      plan = plan,
      task_tag = task_tag,
      format = format,
      path = path
    ))
  }
  reporter$export(
    results = results,
    plan = plan,
    task_tag = task_tag,
    format = format,
    path = path,
    ...
  )
}

.msa_fmt_num <- function(x, digits = 4) {
  ifelse(is.na(x), "NA", formatC(as.numeric(x), format = "f", digits = digits))
}

.msa_fmt_pct <- function(x, digits = 2, scale = FALSE) {
  value <- if (scale) as.numeric(x) * 100 else as.numeric(x)
  paste0(.msa_fmt_num(value, digits), "%")
}

.msa_fmt_p <- function(x, digits = 4) {
  x <- as.numeric(x)
  ifelse(is.na(x), "NA", ifelse(x < 10^-digits, paste0("<", formatC(10^-digits, format = "f", digits = digits)), .msa_fmt_num(x, digits)))
}

.msa_print_table <- function(x, row.names = FALSE) {
  print(as.data.frame(x), row.names = row.names, right = FALSE)
  invisible(x)
}

.msa_round_numeric_columns <- function(x, digits = 4) {
  x <- as.data.frame(x)
  num_cols <- vapply(x, is.numeric, logical(1))
  x[num_cols] <- lapply(x[num_cols], round, digits = digits)
  x
}

# --- Run-chart stability helpers (Nelson rules, simplified for Type 1) -------
# Used by Type1Analyzer$compute_bias() to support the MSA 4th ed. requirement
# that the measurement process be in statistical control before capability
# indices are interpreted.

# Longest run of consecutive equal non-zero signs (ignores zeros by skipping).
.msa_max_consecutive_run <- function(sgn) {
  sgn <- sgn[sgn != 0]
  if (length(sgn) == 0) return(0L)
  rle_out <- rle(as.integer(sign(sgn)))
  max(rle_out$lengths)
}

# TRUE if there exists a strictly increasing or decreasing run of length >= k
# anywhere in the series.
.msa_has_trend <- function(x, k = 6) {
  n <- length(x)
  if (n < k) return(FALSE)
  d <- diff(x)
  # A strictly increasing/decreasing run of length k in x corresponds to
  # k-1 consecutive same-sign differences.
  sgn <- sign(d)
  sgn <- sgn[sgn != 0]
  if (length(sgn) < k - 1) return(FALSE)
  rle_out <- rle(as.integer(sign(sgn)))
  any(rle_out$lengths >= k - 1)
}

# Nelson rule 5: 2 of 3 consecutive points beyond +-2 sigma on the same side.
.msa_two_of_three_beyond_2s <- function(z) {
  n <- length(z)
  if (n < 3) return(FALSE)
  for (i in seq_len(n - 2)) {
    tri <- z[i:(i + 2)]
    pos <- sum(tri > 2)
    neg <- sum(tri < -2)
    if (pos >= 2 || neg >= 2) return(TRUE)
  }
  FALSE
}
