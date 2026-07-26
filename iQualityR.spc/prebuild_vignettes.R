# Manually pre-build vignettes for iQualityR.spc
# Uses rmarkdown::render() directly to avoid devtools dependency installation

cat("=== Pre-building vignettes manually ===\n")

vignettes <- c("spc-shewhart", "spc-advanced", "spc-enhancement", "spc-ml")
doc_dir <- "inst/doc"

# Create inst/doc/ directory
if (!dir.exists(doc_dir)) {
  dir.create(doc_dir, recursive = TRUE)
  cat("Created", doc_dir, "\n")
}

# Render each vignette
for (v in vignettes) {
  rmd_file <- file.path("vignettes", paste0(v, ".Rmd"))
  html_file <- file.path(doc_dir, paste0(v, ".html"))

  cat("\n--- Rendering", rmd_file, "---\n")
  tryCatch({
    rmarkdown::render(
      input = rmd_file,
      output_format = "rmarkdown::html_vignette",
      output_file = paste0(v, ".html"),
      output_dir = doc_dir,
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    cat("Success:", html_file, "\n")
  }, error = function(e) {
    cat("FAILED:", conditionMessage(e), "\n")
  })
}

# Copy .Rmd source files to inst/doc/
for (v in vignettes) {
  rmd_file <- file.path("vignettes", paste0(v, ".Rmd"))
  dest_file <- file.path(doc_dir, paste0(v, ".Rmd"))
  file.copy(rmd_file, dest_file, overwrite = TRUE)
}
cat("\nCopied .Rmd source files to", doc_dir, "\n")

# Create Meta/ directory with vignette.rds (vignette index)
if (!dir.exists("Meta")) {
  dir.create("Meta", recursive = TRUE)
}

# Build vignette index
vignette_index <- data.frame(
  File = paste0(vignettes, ".Rmd"),
  Title = c(
    "Shewhart Control Charts: Variables and Attributes",
    "Advanced SPC: Time-Weighted, Multivariate, and Rare-Event Charts",
    "Statistical Enhancement Charts: Adaptive, ARIMA, AEWMA, Change-Point, KDE, T2+MEWMA",
    "ML Enhancement Charts: LSTM, Autoencoder, Isolation Forest, BOCPD, SHAP"
  ),
  PDF = character(length(vignettes)),
  stringsAsFactors = FALSE
)

# Save as Meta/vignette.rds
saveRDS(vignette_index, file.path("Meta", "vignette.rds"))
cat("Created Meta/vignette.rds\n")

# Also copy vignette.rds to inst/doc/Meta/
inst_meta_dir <- file.path(doc_dir, "Meta")
if (!dir.exists(inst_meta_dir)) {
  dir.create(inst_meta_dir, recursive = TRUE)
}
saveRDS(vignette_index, file.path(inst_meta_dir, "vignette.rds"))
cat("Created inst/doc/Meta/vignette.rds\n")

cat("\n--- inst/doc/ contents ---\n")
print(list.files(doc_dir, recursive = TRUE))

cat("\n--- Meta/ contents ---\n")
print(list.files("Meta", recursive = TRUE))

cat("\n=== Pre-build complete ===\n")
