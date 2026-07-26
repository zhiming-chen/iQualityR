# =============================================================================
# Verification script: Type1 Gage Study formulas vs Minitab official output
#
# Data: Minitab official example "硬盖厚度.csv" (50 measurements, hardcoat thickness)
# Minitab session output (Type 1 Gage Study):
#   Reference  = 0.025
#   Tolerance  = 0.0007
#   Mean       = 0.0249852
#   StDev      = 0.0000438
#   6*StDev    = 0.0002625
#   Bias       = -0.0000148
#   T          = -2.39
#   P-value    = 0.021
#   Cg         = 0.53
#   Cgk        = 0.42
#   %Var(Rep)  = 37.50%
#   %Var(Rep&Bias) = 47.52%
#
# Source: Minitab Type 1 Gage Study methods and formulas
#   https://support.minitab.com/.../type-1-gage-study/methods-and-formulas/
# =============================================================================

library(iQualityR.msa)

# --- Load data ---------------------------------------------------------
hardcoat_path <- system.file("extdata/minitab_msa/硬盖厚度.csv", package = "iQualityR.msa")
hardcoat <- read.csv(hardcoat_path, fileEncoding = "UTF-8")
meas <- hardcoat[[1]]

# --- Parameters from Minitab example -----------------------------------
ref_val   <- 0.025
tolerance <- 0.0007
k_factor  <- 0.2

# --- Manual calculation ------------------------------------------------
n         <- length(meas)
mean_meas <- mean(meas)
sd_meas   <- sd(meas)
bias      <- mean_meas - ref_val
sv_6sigma <- 6 * sd_meas

cat("=== Basic Statistics ===\n")
cat(sprintf("N         = %d  (Minitab: 50)\n", n))
cat(sprintf("Mean      = %.7f  (Minitab: 0.0249852)\n", mean_meas))
cat(sprintf("StDev     = %.7f  (Minitab: 0.0000438)\n", sd_meas))
cat(sprintf("6*StDev   = %.7f  (Minitab: 0.0002625)\n", sv_6sigma))
cat(sprintf("Bias      = %.7f  (Minitab: -0.0000148)\n", bias))

# --- Formula 1: Cg = (k * T) / (6 * s) ---------------------------------
Cg <- (k_factor * tolerance) / (6 * sd_meas)
cat(sprintf("\n=== Formula 1: Cg ===\n"))
cat(sprintf("Cg = (k*T)/(6*s) = (%.2f * %.4f) / (6 * %.7f) = %.4f\n",
            k_factor, tolerance, sd_meas, Cg))
cat(sprintf("Minitab: 0.53  ->  %s\n", ifelse(abs(Cg - 0.53) < 0.01, "MATCH", "MISMATCH")))

# --- Formula 2: Cgk = (k*T/2 - |bias|) / (3*s) -------------------------
Cgk <- (k_factor * tolerance / 2 - abs(bias)) / (3 * sd_meas)
cat(sprintf("\n=== Formula 2: Cgk (corrected, no hardcode) ===\n"))
cat(sprintf("Cgk = (k*T/2 - |bias|) / (3*s) = (%.6f - %.7f) / (3 * %.7f) = %.4f\n",
            k_factor * tolerance / 2, abs(bias), sd_meas, Cgk))
cat(sprintf("Minitab: 0.42  ->  %s\n", ifelse(abs(Cgk - 0.42) < 0.01, "MATCH", "MISMATCH")))

# --- Formula 3: %Var(Rep) = (6s / T) * 100 -----------------------------
pct_rep <- (sv_6sigma / tolerance) * 100
cat(sprintf("\n=== Formula 3: %%Var(Repeatability) ===\n"))
cat(sprintf("%%Var(Rep) = (6s/T)*100 = (%.7f / %.4f) * 100 = %.2f%%\n",
            sv_6sigma, tolerance, pct_rep))
cat(sprintf("Minitab: 37.50%%  ->  %s\n", ifelse(abs(pct_rep - 37.50) < 0.1, "MATCH", "MISMATCH")))

# --- Formula 4: %Var(Rep&Bias) = k * 100 / Cgk -------------------------
pct_rb <- (k_factor * 100) / Cgk
cat(sprintf("\n=== Formula 4: %%Var(Repeatability and Bias) ===\n"))
cat(sprintf("%%Var(Rep&Bias) = k*100/Cgk = %.2f * 100 / %.4f = %.2f%%\n",
            k_factor, Cgk, pct_rb))
cat(sprintf("Minitab: 47.52%%  ->  %s\n", ifelse(abs(pct_rb - 47.52) < 0.1, "MATCH", "MISMATCH")))

# --- Old (wrong) formula for comparison --------------------------------
pct_rb_old <- ((sv_6sigma + abs(bias)) / tolerance) * 100
cat(sprintf("\n[Old wrong formula: (6s+|bias|)/T*100 = %.2f%% -> MISMATCH confirmed]\n",
            pct_rb_old))

# --- Run via iQualityR.msa package -------------------------------------
cat("\n=== Package run (iqr_type1_bias) ===\n")
task <- iqr_type1_bias(
  data = meas,
  reference_value = ref_val,
  tolerance = tolerance,
  k_factor = k_factor
)
s <- task$results$statistics
cat(sprintf("Cg         = %.4f  (Minitab: 0.53)\n", s$Cg))
cat(sprintf("Cgk        = %.4f  (Minitab: 0.42)\n", s$Cgk))
cat(sprintf("%%Var(Rep)  = %.2f%%  (Minitab: 37.50%%)\n", s$percent_repeatability))
cat(sprintf("%%Var(R&B)  = %.2f%%  (Minitab: 47.52%%)\n", s$percent_repeatability_bias))
cat(sprintf("k_factor   = %.2f  (now explicitly stored)\n", s$k_factor))
cat(sprintf("df         = %d  (now explicitly stored)\n", s$df))
cat(sprintf("VDA5 uLIN  = %.6f  (now explicitly 0 for Type1)\n", s$vda5_u_lin))
cat(sprintf("VDA5 ExpU  = %.6f  (Expanded U = 2*uMS, new)\n", s$vda5_expanded_u))
cat(sprintf("VDA5 Cap   = %s  (capability decision, new)\n", s$vda5_qms_capability))

# --- Tolerance multi-input test ----------------------------------------
cat("\n=== Tolerance Multi-Input Test ===\n")
task_tol <- iqr_type1_bias(
  data = meas,
  reference_value = ref_val,
  tolerance = 0.0007
)
cat(sprintf("Direct tolerance input: Cg = %.4f (should match above)\n",
            task_tol$results$statistics$Cg))

task_spec <- iqr_type1_bias(
  data = meas,
  reference_value = ref_val,
  lsl = 0.02465, usl = 0.02535
)
cat(sprintf("Spec limits input:      Cg = %.4f (should match above, T=0.0007)\n",
            task_spec$results$statistics$Cg))

task_nz <- iqr_type1_bias(
  data = meas,
  reference_value = ref_val,
  usl = 0.0007,
  natural_zero = TRUE
)
cat(sprintf("Natural zero input:     Cg = %.4f (T=USL=0.0007)\n",
            task_nz$results$statistics$Cg))

cat("\n=== Type1 Verification Complete ===\n")
