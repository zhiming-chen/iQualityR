# Smoke test: verify all capability plot types render without error
devtools::load_all("g:/iQualityR-foundation/packages/iQualityR.plot")
devtools::load_all("g:/iQualityR-foundation/packages/iQualityR.capa")
set.seed(42)

# === 1. Normal Capability ===
cat("=== Normal Capability ===\n")
set.seed(42)
df_norm <- data.frame(measurement = rnorm(80, 10, 0.12))
task <- capability_normal(data = df_norm, measurement = "measurement",
                           lsl = 9.7, usl = 10.3, target = 10)
for (t in c("full", "basic", "qq", "capbar", "individual", "mr", "trend")) {
  p <- task$plot(type = t)
  cat(t, "->", paste(class(p), collapse = ","), "\n")
}

# === 2. Attribute Binomial ===
cat("\n=== Attribute Binomial ===\n")
set.seed(42)
n_sub <- 25; ss <- 100
defects <- rbinom(n_sub, ss, 0.025)
df_bin <- data.frame(defects = defects, sample_sizes = ss)
task_bin <- capability_binomial(
  data = df_bin, defects = "defects", sample_sizes = "sample_sizes",
  target_proportion = 0.02
)
for (t in c("full", "control", "histogram", "cumulative", "defects", "gauge", "summary")) {
  p <- task_bin$plot(type = t)
  cat(t, "->", paste(class(p), collapse = ","), "\n")
}

# === 3. Attribute Poisson ===
cat("\n=== Attribute Poisson ===\n")
set.seed(42)
defects_p <- rpois(25, 2.5)
expo <- rep(50, 25)
df_poi <- data.frame(defects = defects_p, sample_sizes = expo)
task_poi <- capability_poisson(
  data = df_poi, defects = "defects", sample_sizes = "sample_sizes",
  target_rate = 0.05
)
for (t in c("full", "control", "histogram", "cumulative", "defects", "gauge", "summary")) {
  p <- task_poi$plot(type = t)
  cat(t, "->", paste(class(p), collapse = ","), "\n")
}

# === 4. Multivariate Capability ===
cat("\n=== Multivariate Capability ===\n")
set.seed(42)
n <- 60
bore <- rnorm(n, 50, 0.3)
stroke <- rnorm(n, 90, 0.5) + 0.1 * bore
df_mv <- data.frame(bore = bore, stroke = stroke)
task_mv <- capability_multivariate(
  data = df_mv, ctqs = c("bore", "stroke"),
  lsl_vec = c(48, 87), usl_vec = c(52, 93),
  target_vec = c(50, 90)
)
for (t in c("full", "ellipse", "radar", "marginal", "t2", "mcpv", "summary")) {
  p <- task_mv$plot(type = t)
  cat(t, "->", paste(class(p), collapse = ","), "\n")
}

cat("\nALL_SMOKE_OK\n")
