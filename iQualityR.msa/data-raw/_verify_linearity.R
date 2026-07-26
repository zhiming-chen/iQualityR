library(iQualityR.msa)
library(data.table)

# Load bearing data
bearing_path <- system.file("extdata/minitab_msa/轴承直径.csv", package = "iQualityR.msa")
bearing_data <- read.csv(bearing_path, fileEncoding = "UTF-8")
names(bearing_data) <- c("Part", "Reference", "Measurement")
cat("=== Raw data summary ===\n")
cat(sprintf("Rows: %d, Cols: %d\n", nrow(bearing_data), ncol(bearing_data)))
cat(sprintf("Unique reference values: %s\n", paste(unique(bearing_data$Reference), collapse=", ")))
cat(sprintf("Measurements per reference: %d\n", sum(bearing_data$Reference == 2)))

# Build analysis-ready data
df <- data.frame(
  part        = bearing_data$Part,
  reference   = bearing_data$Reference,
  measurement = bearing_data$Measurement
)

# Run linearity analysis
task <- iqr_linearity_bias(
  df,
  reference_values = unique(df$reference),
  lsl = 0,
  usl = 12
)

cat("\n=== Task summary ===\n")
task$summary()

cat("\n=== Detailed statistics ===\n")
stat <- task$results$statistics
for (nm in names(stat)) {
  v <- stat[[nm]]
  if (is.numeric(v) && length(v) == 1) {
    cat(sprintf("%-25s = %s\n", nm, formatC(v, format="f", digits=8)))
  } else {
    cat(sprintf("%-25s = [complex value]\n", nm))
  }
}

cat("\n=== Per-reference summary ===\n")
print(task$results$data_tables$ref_summary)

cat("\n=== Diagnostics ===\n")
print(task$results$diagnostics)

# Now independently compute what Minitab *should* produce per AIAG MSA 4th ed.
cat("\n\n=== INDEPENDENT COMPARISON vs MINITAB/AIAG MSA 4th ===\n\n")
ref_summary <- task$results$data_tables$ref_summary
lm_model <- lm(bias ~ reference, data = ref_summary)
lm_sum <- summary(lm_model)
cat("=== Regression: bias = intercept + slope * reference ===\n")
cat(sprintf("Intercept (a) = %.8f\n", coef(lm_model)[1]))
cat(sprintf("Slope     (b) = %.8f\n", coef(lm_model)[2]))
cat(sprintf("R-squared     = %.8f\n", lm_sum$r.squared))
cat(sprintf("SE(slope)     = %.8f\n", lm_sum$coefficients[2, 2]))
cat(sprintf("t(slope)      = %.8f\n", lm_sum$coefficients[2, 3]))
cat(sprintf("p(slope)      = %.8f\n", lm_sum$coefficients[2, 4]))

tolerance <- 12
process_var <- 6 * sd(bearing_data$Measurement)
cat("\n=== Process variation (PV) - two possible definitions ===\n")
cat(sprintf("6*SD(all measurements)           = %.8f\n", process_var))
cat(sprintf("6*SD(per-reference means)        = %.8f\n", 6 * sd(ref_summary$mean)))

# Per AIAG MSA 4th ed., linearity formula:
# Linearity = |slope| * Process Variation, where Process Variation = 6*SD
# %Linearity = Linearity / Process Variation * 100 = |slope| * 100
# (because both numerator and denominator use the same PV)
cat("\n=== AIAG MSA 4th ed. Linearity formula ===\n")
cat("Linearity = |slope| * Process Variation\n")
cat("%Linearity = Linearity / Process Variation * 100 = |slope| * 100 (if same PV)\n")
slope <- coef(lm_model)[2]
linearity_aiag <- abs(slope) * process_var
cat(sprintf("Linearity (|slope| * 6*SD_all)  = %.8f\n", linearity_aiag))
cat(sprintf("%%Linearity (|slope| * 100)       = %.4f%%\n", abs(slope) * 100))

# What the code computes
cat("\n=== What the code currently computes ===\n")
pred_bias <- predict(lm_model)
code_linearity <- max(abs(pred_bias))
cat(sprintf("Linearity (max|pred_bias|)      = %.8f\n", code_linearity))
cat(sprintf("%%Linearity (max|pred|/T * 100)   = %.4f%%\n", (code_linearity / tolerance) * 100))

# Compare bias (avg of per-reference biases, vs Minitab 'Average Bias')
cat("\n=== Bias comparison ===\n")
avg_bias <- mean(ref_summary$bias)
cat(sprintf("Avg bias (mean of per-ref biases) = %.8f\n", avg_bias))

# Per-reference bias statistics - what Minitab reports
cat("\n=== Per-reference bias and t-test ===\n")
for (i in seq_len(nrow(ref_summary))) {
  r  <- ref_summary$reference[i]
  mm <- ref_summary$mean[i]
  sd_r <- ref_summary$sd[i]
  n_r <- ref_summary$n[i]
  bias_r <- mm - r
  se <- sd_r / sqrt(n_r)
  t_stat <- bias_r / se
  p_val  <- 2 * pt(abs(t_stat), df = n_r - 1, lower.tail = FALSE)
  cat(sprintf("Ref=%4.1f  mean=%.5f  sd=%.6f  bias=%.6f  t=%.4f  p=%.4f\n",
              r, mm, sd_r, bias_r, t_stat, p_val))
}

# =============================================================================
# KEY: Minitab regresses INDIVIDUAL biases (60 points), not per-reference means (5 points)
# =============================================================================
cat("\n\n=== KEY: Regression on INDIVIDUAL data (Minitab's approach) ===\n")
indiv_df <- data.frame(
  reference = bearing_data$Reference,
  bias      = bearing_data$Measurement - bearing_data$Reference
)
lm_indiv <- lm(bias ~ reference, data = indiv_df)
lm_indiv_sum <- summary(lm_indiv)
cat(sprintf("Intercept (a) = %.8f\n", coef(lm_indiv)[1]))
cat(sprintf("Slope     (b) = %.8f\n", coef(lm_indiv)[2]))
cat(sprintf("R-squared     = %.8f\n", lm_indiv_sum$r.squared))
cat(sprintf("SE(slope)     = %.8f\n", lm_indiv_sum$coefficients[2, 2]))
cat(sprintf("t(slope)      = %.8f\n", lm_indiv_sum$coefficients[2, 3]))
cat(sprintf("p(slope)      = %.8f\n", lm_indiv_sum$coefficients[2, 4]))
cat(sprintf("95%% CI slope  = [%.8f, %.8f]\n",
            confint(lm_indiv)[2, 1], confint(lm_indiv)[2, 2]))

cat("\n=== Comparison: 5-point (code) vs 60-point (Minitab) regression ===\n")
cat(sprintf("%-20s  %-15s  %-15s\n", "", "5-point (code)", "60-point (Minitab)"))
cat(sprintf("%-20s  %-15.8f  %-15.8f\n", "Intercept",
            coef(lm_model)[1], coef(lm_indiv)[1]))
cat(sprintf("%-20s  %-15.8f  %-15.8f\n", "Slope",
            coef(lm_model)[2], coef(lm_indiv)[2]))
cat(sprintf("%-20s  %-15.8f  %-15.8f\n", "R-squared",
            lm_sum$r.squared, lm_indiv_sum$r.squared))
cat(sprintf("%-20s  %-15.8f  %-15.8f\n", "SE(slope)",
            lm_sum$coefficients[2, 2], lm_indiv_sum$coefficients[2, 2]))
cat(sprintf("%-20s  %-15.8f  %-15.8f\n", "t(slope)",
            lm_sum$coefficients[2, 3], lm_indiv_sum$coefficients[2, 3]))
cat(sprintf("%-20s  %-15.8f  %-15.8f\n", "p(slope)",
            lm_sum$coefficients[2, 4], lm_indiv_sum$coefficients[2, 4]))

# Linearity per AIAG MSA 4th: Linearity = |slope| * Process Variation
# %Linearity = Linearity / PV * 100 = |slope| * 100
cat("\n=== AIAG MSA 4th ed. Linearity (using 60-pt regression) ===\n")
slope_indiv <- coef(lm_indiv)[2]
PV_study <- 6 * sd(bearing_data$Measurement)  # study-based PV proxy
PV_hist <- 6 * sd(ref_summary$mean)           # alternative using per-ref means
cat(sprintf("Process Variation (study, 6*SD_all)  = %.6f\n", PV_study))
cat(sprintf("Process Variation (per-ref, 6*SD_mean) = %.6f\n", PV_hist))
cat(sprintf("Linearity (|slope|*PV_study)        = %.6f\n", abs(slope_indiv) * PV_study))
cat(sprintf("Linearity (|slope|*PV_per-ref)      = %.6f\n", abs(slope_indiv) * PV_hist))
cat(sprintf("%%Linearity (|slope|*100)            = %.4f%%\n", abs(slope_indiv) * 100))

# %Bias per AIAG: |bias| / PV * 100, not /Tolerance
cat("\n=== Per-reference %Bias: /PV (Minitab) vs /Tolerance (code) ===\n")
cat(sprintf("%-6s  %-10s  %-15s  %-15s\n", "Ref", "Bias", "%Bias/PV", "%Bias/Tol(code)"))
for (i in seq_len(nrow(ref_summary))) {
  r <- ref_summary$reference[i]
  bias_r <- ref_summary$bias[i]
  pct_pv  <- (abs(bias_r) / PV_study) * 100
  pct_tol <- (abs(bias_r) / tolerance) * 100
  cat(sprintf("%-6.1f  %-10.6f  %-15.4f  %-15.4f\n",
              r, bias_r, pct_pv, pct_tol))
}
