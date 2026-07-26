# =============================================================================
# Verification script: Gage Linearity & Bias Study formulas vs Minitab
#
# Data: Minitab official example "轴承直径.csv" (60 measurements, 5 references)
# Minitab session output (Gage Linearity and Bias Study):
#   Reference values: 2, 4, 6, 8, 10
#   Process Variation: 14.93 (6*sigma from historical data)
#   Regression on individual biases (n=60):
#     Intercept = 0.736667
#     Slope     = -0.131667
#     R-Sq      = 71.43%
#     S         = 0.0643
#   Linearity  = |slope| * PV = 0.131667 * 14.93 = 1.966
#   %Linearity = Linearity / PV * 100 = 13.17%
#   %Bias(avg) = |avg_bias| / PV * 100
#
# Source: Minitab Gage Linearity and Bias Study methods and formulas
#   https://support.minitab.com/.../gage-linearity-and-bias-study/methods-and-formulas/
#   Linearity = |slope| * Process Variation
#   %Linearity = (Linearity / Process Variation) * 100
#   %Bias = 100 * |average bias| / Process Variation
#   Regression on individual biases y_ij (not per-reference means)
# =============================================================================

library(iQualityR.msa)

# --- Load data ---------------------------------------------------------
bearing_path <- system.file("extdata/minitab_msa/轴承直径.csv", package = "iQualityR.msa")
bearing_raw <- read.csv(bearing_path, fileEncoding = "UTF-8")
# CSV columns: 部件(part id), 主水平(reference), 响(measurement)
bearing <- data.frame(
  reference   = bearing_raw[[2]],   # 主水平 = reference value
  measurement = bearing_raw[[3]]    # 响应 = measurement
)
head(bearing)

# --- Minitab parameters ------------------------------------------------
process_variation <- 14.93  # from Minitab example (6*sigma historical)
tolerance <- 12             # USL - LSL for this example

# --- Manual calculation (individual bias regression) -------------------
bias_long <- bearing
bias_long$bias <- bias_long$measurement - bias_long$reference

cat("=== Individual Bias Regression (Minitab spec) ===\n")
cat(sprintf("N individual points = %d (should be 60)\n", nrow(bias_long)))

lm_fit <- lm(bias ~ reference, data = bias_long)
lm_sum <- summary(lm_fit)
intercept <- coef(lm_fit)[1]
slope     <- coef(lm_fit)[2]
r_sq      <- lm_sum$r.squared
s_reg     <- lm_sum$sigma

cat(sprintf("Intercept = %.6f  (Minitab: 0.736667)\n", intercept))
cat(sprintf("Slope     = %.6f  (Minitab: -0.131667)\n", slope))
cat(sprintf("R-squared = %.4f  (Minitab: 0.7143)\n", r_sq))
cat(sprintf("S (sigma) = %.6f  (Minitab: ~0.0643)\n", s_reg))

# --- Linearity = |slope| * Process Variation ---------------------------
linearity <- abs(slope) * process_variation
pct_lin   <- (linearity / process_variation) * 100
cat(sprintf("\n=== Linearity (Minitab spec) ===\n"))
cat(sprintf("Linearity = |slope| * PV = %.6f * %.2f = %.4f\n",
            abs(slope), process_variation, linearity))
cat(sprintf("Minitab: 1.966  ->  %s\n", ifelse(abs(linearity - 1.966) < 0.01, "MATCH", "CLOSE")))
cat(sprintf("%%Linearity = Linearity/PV*100 = %.2f%%\n", pct_lin))
cat(sprintf("Minitab: 13.17%%  ->  %s\n", ifelse(abs(pct_lin - 13.17) < 0.1, "MATCH", "CLOSE")))

# --- %Bias = 100 * |avg_bias| / PV -------------------------------------
avg_bias <- mean(bias_long$bias)
pct_bias <- 100 * abs(avg_bias) / process_variation
cat(sprintf("\n=== %%Bias (Minitab spec) ===\n"))
cat(sprintf("Avg bias  = %.6f\n", avg_bias))
cat(sprintf("%%Bias = 100*|avg_bias|/PV = 100*%.6f/%.2f = %.2f%%\n",
            abs(avg_bias), process_variation, pct_bias))

# --- Old (wrong) formulas for comparison -------------------------------
linearity_old <- max(abs(predict(lm_fit)))
pct_lin_old   <- (linearity_old / tolerance) * 100
pct_bias_old  <- (abs(avg_bias) / tolerance) * 100
cat(sprintf("\n[Old wrong: Linearity=max|pred|=%.4f, %%Lin/Tol=%.2f%%, %%Bias/Tol=%.2f%%]\n",
            linearity_old, pct_lin_old, pct_bias_old))

# --- Per-reference bias t-tests (sample SD method) ---------------------
cat("\n=== Per-Reference Bias Tests ===\n")
ref_summary <- aggregate(bias ~ reference, data = bias_long,
                         FUN = function(x) c(n = length(x), mean = mean(x),
                                             sd = sd(x)))
ref_summary <- do.call(data.frame, ref_summary)
ref_summary$t_stat <- ref_summary$bias.mean / (ref_summary$bias.sd / sqrt(ref_summary$bias.n))
ref_summary$p_value <- 2 * pt(abs(ref_summary$t_stat), df = ref_summary$bias.n - 1,
                              lower.tail = FALSE)
print(ref_summary)

# --- Run via iQualityR.msa package -------------------------------------
cat("\n=== Package run (iqr_linearity_bias) ===\n")
task <- iqr_linearity_bias(
  data = bearing,
  tolerance = tolerance,
  process_variation = process_variation
)
s <- task$results$statistics
cat(sprintf("N total          = %d  (should be 60)\n", s$n_total))
cat(sprintf("Process Variation= %.2f  (input)\n", s$process_variation))
cat(sprintf("Intercept        = %.6f  (Minitab: 0.736667)\n", s$intercept))
cat(sprintf("Slope            = %.6f  (Minitab: -0.131667)\n", s$slope))
cat(sprintf("R-squared        = %.4f  (Minitab: 0.7143)\n", s$r_squared))
cat(sprintf("S (regression)   = %.6f  (Minitab: ~0.0643)\n", s$s_regression))
cat(sprintf("Linearity        = %.4f  (Minitab: 1.966)\n", s$linearity))
cat(sprintf("%%Linearity      = %.2f%%  (Minitab: 13.17%%)\n", s$percent_linearity))
cat(sprintf("Avg bias         = %.6f\n", s$avg_bias))
cat(sprintf("%%Avg bias       = %.2f%%\n", s$percent_avg_bias))
cat(sprintf("Linearity CI     = [%.4f, %.4f]\n", s$ci_linearity[1], s$ci_linearity[2]))

# --- Per-reference summary from package --------------------------------
cat("\n=== Per-Reference Summary (from package) ===\n")
print(task$results$data_tables$ref_summary)

# --- Fallback test: process_variation = NULL ----------------------------
cat("\n=== Fallback Test: process_variation = NULL ===\n")
task_fb <- iqr_linearity_bias(
  data = bearing,
  tolerance = tolerance
)
cat(sprintf("Fallback PV = 6*sd(meas) = %.4f\n", task_fb$results$statistics$process_variation))
cat(sprintf("Linearity with fallback PV = %.4f\n", task_fb$results$statistics$linearity))
cat("(Note: different from Minitab because PV source differs)\n")

cat("\n=== Linearity Verification Complete ===\n")
