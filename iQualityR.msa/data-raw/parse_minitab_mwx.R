# =============================================================================
# Script: parse_minitab_mwx.R
# Description: Parse Minitab .MWX worksheet files (ZIP + JSON) into CSV files
#              for use in iQualityR.msa vignettes.
#
# Data source: Minitab sample data sets
#   https://support.minitab.com/zh-cn/datasets/measurement-systems-analysis-data-sets/
# Original reference: AIAG Measurement Systems Analysis Reference Manual,
#                     4th edition.
#
# Usage:
#   source("data-raw/parse_minitab_mwx.R")
# =============================================================================

#' Parse a single Minitab .MWX file into a data.frame
#'
#' .MWX is a ZIP archive containing JSON worksheet metadata. The sheet data
#' lives at `sheets/0/sheet.json` inside the archive. Each column is described
#' by an entry in `Data$Columns` with `WorksheetVarBody$Name` and either
#' `VarData$VarDataBody$NumericData` (numeric) or `TextData` (character).
#'
#' @param file Path to a .MWX file.
#' @return A data.frame with columns in worksheet order.
#' @noRd
parse_mwx <- function(file) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(file, exdir = tmp)

  sheet_json <- file.path(tmp, "sheets", "0", "sheet.json")
  if (!file.exists(sheet_json)) {
    stop(sprintf("sheet.json not found inside %s", file))
  }
  # simplifyVector = FALSE keeps every JSON object as a named list, which is
  # safer because MWX columns are heterogeneous (some have nested VarDataBody,
  # some get simplified to atomic vectors when simplifyVector = TRUE).
  data <- jsonlite::fromJSON(sheet_json, simplifyVector = FALSE)
  cols <- data$Data$Columns

  col_list <- lapply(cols, function(col) {
    name <- col$WorksheetVarBody$Name
    # MWX layout: col$WorksheetVarBody$VarData$VarDataBody holds the data
    body <- col$WorksheetVarBody$VarData$VarDataBody
    if (isTRUE(body$HasNumericData) && length(body$NumericData) > 0) {
      vals <- unlist(body$NumericData, recursive = TRUE, use.names = FALSE)
    } else if (isTRUE(body$HasTextData) && length(body$TextData) > 0) {
      vals <- unlist(body$TextData, recursive = TRUE, use.names = FALSE)
    } else {
      # Empty column - derive length from CellCt if possible, else 0
      n <- if (is.numeric(body$CellCt) && length(body$CellCt) == 1) {
        as.integer(body$CellCt)
      } else 0L
      vals <- rep(NA_character_, n)
    }
    list(name = name, vals = vals)
  })

  # All columns should have equal length; build data.frame
  n <- max(vapply(col_list, function(x) length(x$vals), integer(1)), 0L)
  out <- data.frame(row.names = seq_len(n))
  for (cc in col_list) {
    # Pad/truncate to common length n
    v <- cc$vals
    if (length(v) < n) v <- c(v, rep(NA, n - length(v)))
    out[[cc$name]] <- v
  }
  out
}

# -----------------------------------------------------------------------------
# Batch-convert all .MWX files in inst/extdata/minitab_msa to CSV
# -----------------------------------------------------------------------------

mwx_dir <- "inst/extdata/minitab_msa"
mwx_files <- list.files(mwx_dir, pattern = "\\.MWX$", full.names = TRUE)

cat(sprintf("Found %d .MWX file(s) to convert:\n", length(mwx_files)))

for (f in mwx_files) {
  base <- tools::file_path_sans_ext(basename(f))
  out_csv <- file.path(mwx_dir, paste0(base, ".csv"))
  tryCatch({
    df <- parse_mwx(f)
    utils::write.csv(df, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
    cat(sprintf("  OK   %-20s  -> %s  (%d rows, %d cols)\n",
                basename(f), basename(out_csv), nrow(df), ncol(df)))
  }, error = function(e) {
    cat(sprintf("  FAIL %-20s  %s\n", basename(f), conditionMessage(e)))
  })
}

cat("\nDone. CSV files written to:", mwx_dir, "\n")
