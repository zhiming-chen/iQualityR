library(iQualityR.msa)

# Load Minitab hardcoat data
hardcoat_path <- system.file("extdata/minitab_msa/硬盖厚度.csv", package = "iQualityR.msa")
hardcoat <- read.csv(hardcoat_path, fileEncoding = "UTF-8")
meas <- hardcoat[[1]]

# Minitab official values (from https://support.minitab.com/.../type-1-gage-study/before-you-start/example/)
# Cg = 0.53, Cgk = 0.42
# %Var (Repeatability) = 37.50%
# %Var (Repeatability and Bias) = 47.52%
# mean = 0.024985, sd = 0.0000438, bias p-value = 0.021

ref_val <- 0.025
tolerance <- 0.0007
k_factor <- 0.2

n <- length(meas)
mean_meas <- mean(meas)
sd_meas <- sd(meas)
bias <- mean_meas - ref_val
sv_6sigma <- 6 * sd_meas

cat("=== Actual computed values ===\n")
cat(sprintf("n = %d\n", n))
cat(sprintf("mean_meas = %.8f (Minitab says 0.024985)\n", mean_meas))
cat(sprintf("sd_meas   = %.8f (Minitab says 0.0000438)\n", sd_meas))
cat(sprintf("bias      = %.8f (mean - ref)\n", bias))
cat(sprintf("|bias|    = %.8f\n", abs(bias)))
cat(sprintf("sv_6sigma = %.8f\n", sv_6sigma))
cat(sprintf("tolerance = %.8f\n", tolerance))
cat(sprintf("k_factor  = %.2f\n", k_factor))

cat("\n=== Formula 1: Cg = (k_factor * tolerance) / (6 * sd_meas) ===\n")
Cg <- (k_factor * tolerance) / (6 * sd_meas)
cat(sprintf("Code result:   Cg = %.4f\n", Cg))
cat(sprintf("Minitab says:  Cg = 0.53\n"))
cat(sprintf("Match: %s\n", ifelse(abs(Cg - 0.53) < 0.01, "YES", "NO")))

cat("\n=== Formula 2: Cgk = (0.1 * tolerance - abs(bias)) / (3 * sd_meas) ===\n")
Cgk_code <- (0.1 * tolerance - abs(bias)) / (3 * sd_meas)
Cgk_correct <- (k_factor * tolerance / 2 - abs(bias)) / (3 * sd_meas)
cat(sprintf("Code result (0.1 hardcode):  Cgk = %.4f\n", Cgk_code))
cat(sprintf("Formula (k_factor/2):         Cgk = %.4f\n", Cgk_correct))
cat(sprintf("Minitab says:                Cgk = 0.42\n"))
cat(sprintf("Match (code):    %s\n", ifelse(abs(Cgk_code - 0.42) < 0.01, "YES", "NO")))
cat(sprintf("Match (k_factor): %s\n", ifelse(abs(Cgk_correct - 0.42) < 0.01, "YES", "NO")))

cat("\n=== Formula 3: percent_repeatability = (sv_6sigma / tolerance) * 100 ===\n")
pct_rep_code <- (sv_6sigma / tolerance) * 100
cat(sprintf("Code result:   %%Var (Repeat) = %.4f%%\n", pct_rep_code))
cat(sprintf("Minitab says:  %%Var (Repeat) = 37.50%%\n"))
cat(sprintf("Match: %s\n", ifelse(abs(pct_rep_code - 37.50) < 0.1, "YES", "NO")))

cat("\n=== Formula 4: percent_repeatability_bias = ((sv_6sigma + abs(bias)) / tolerance) * 100 ===\n")
pct_rb_code <- ((sv_6sigma + abs(bias)) / tolerance) * 100
cat(sprintf("Code result:   %%Var (Repeat+Bias) = %.4f%%\n", pct_rb_code))
cat(sprintf("Minitab says:  %%Var (Repeat+Bias) = 47.52%%\n"))
cat(sprintf("Match: %s\n", ifelse(abs(pct_rb_code - 47.52) < 0.1, "YES", "NO")))

cat("\n=== Try alternative formulas for %Var (Repeat+Bias) ===\n")
# Try: (SV + 2*|bias|) / T * 100
f1 <- ((sv_6sigma + 2*abs(bias)) / tolerance) * 100
cat(sprintf("(SV + 2*|bias|)/T * 100       = %.4f%%\n", f1))

# Try: (SV/2 + |bias|) / (T/2) * 100
f2 <- ((sv_6sigma/2 + abs(bias)) / (tolerance/2)) * 100
cat(sprintf("(SV/2 + |bias|)/(T/2) * 100   = %.4f%%\n", f2))

# Try: (6s + |bias|*k) / T * 100 for various k
for (k in c(2, 3, 4, 5, 6, 7, 8, 9, 10)) {
  f <- ((sv_6sigma + k*abs(bias)) / tolerance) * 100
  cat(sprintf("(SV + %d*|bias|)/T * 100         = %.4f%%\n", k, f))
}

# Try: (k1*s + |bias|) / T * 100 for various k1
cat("\n")
for (k1 in c(6, 7, 8, 9, 10, 11, 12)) {
  f <- ((k1*sd_meas + abs(bias)) / tolerance) * 100
  cat(sprintf("(%d*s + |bias|)/T * 100          = %.4f%%\n", k1, f))
}

# Try: 100/Cgk
cat("\n100/Cgk = %.4f%%\n", 100/Cgk_code)

# Try: (SV + |bias|) / (k*T) * 100
f_k <- ((sv_6sigma + abs(bias)) / (k_factor * tolerance)) * 100
cat(sprintf("(SV + |bias|)/(k*T) * 100      = %.4f%%\n", f_k))

# What value of X gives 47.52%?
# X / T * 100 = 47.52 => X = 0.4752 * T = 0.4752 * 0.0007 = 0.00033264
target_X <- 0.4752 * tolerance
cat(sprintf("\nTarget X for 47.52%%: %.8f\n", target_X))
cat(sprintf("SV = %.8f, |bias| = %.8f\n", sv_6sigma, abs(bias)))
cat(sprintf("X - SV = %.8f\n", target_X - sv_6sigma))
cat(sprintf("(X - SV) / |bias| = %.4f\n", (target_X - sv_6sigma) / abs(bias)))
