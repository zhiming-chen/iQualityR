## Generate sample datasets for the attribute & multivariate vignettes
set.seed(2025)

# ---- 1. Binomial: PCB assembly line defectives -------------------
# 30 daily batches of 100 PCBs each; true p = 0.018 (1.8% defective).
# Typical industry target: 2% defective.
n_subgroups <- 30
n_per_batch <- 100
true_p <- 0.018
defects_binom <- rbinom(n_subgroups, size = n_per_batch, prob = true_p)
df_binom <- data.frame(
  batch = seq_len(n_subgroups),
  date = seq.Date(as.Date("2025-01-01"), by = "day", length.out = n_subgroups),
  defectives = defects_binom,
  inspected  = rep(n_per_batch, n_subgroups)
)
write.csv(df_binom,
  "inst/extdata/capability_attribute_binomial.csv",
  row.names = FALSE)

# ---- 2. Poisson: Textile weaving defects per 50m roll -------------
# 30 rolls of 50m each; true rate = 0.06 defects/m (3 defects per 50m roll).
n_rolls <- 30
meters_per_roll <- 50
true_rate <- 0.06
defects_pois <- rpois(n_rolls, lambda = true_rate * meters_per_roll)
df_pois <- data.frame(
  roll = seq_len(n_rolls),
  date = seq.Date(as.Date("2025-01-01"), by = "day", length.out = n_rolls),
  defects = defects_pois,
  meters = rep(meters_per_roll, n_rolls)
)
write.csv(df_pois,
  "inst/extdata/capability_attribute_poisson.csv",
  row.names = FALSE)

# ---- 3. Multivariate: Engine cylinder bore & stroke ----------------
# 50 engine cylinder blocks; bore and stroke are positively correlated
# (both machined on the same fixture). Specs:
#   bore:   99.5 - 100.5 mm  (target 100 mm)
#   stroke: 79.5 - 80.5 mm   (target 80 mm)
# True mean = (100, 80), true SD = (0.15, 0.18), correlation = 0.6
n_engines <- 60
mu <- c(bore = 100.05, stroke = 79.95)   # slightly off-target
sigma <- c(0.18, 0.20)
rho <- 0.6
cov_mat <- matrix(c(sigma[1]^2, rho * sigma[1] * sigma[2],
                    rho * sigma[1] * sigma[2], sigma[2]^2),
                  nrow = 2, byrow = TRUE)
X <- MASS::mvrnorm(n_engines, mu = mu, Sigma = cov_mat)
df_multi <- as.data.frame(X)
df_multi$engine_id <- seq_len(n_engines)
write.csv(df_multi,
  "inst/extdata/capability_multivariate.csv",
  row.names = FALSE)

message("Generated 3 datasets in inst/extdata/")
