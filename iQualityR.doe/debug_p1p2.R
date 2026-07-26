library(iQualityR.doe)
an <- DoeAnalyzer$new()

# --- fold_over debug ---
factors <- list(
  list(name = "A", type = "continuous", levels = c(-1, 1)),
  list(name = "B", type = "continuous", levels = c(-1, 1)),
  list(name = "C", type = "continuous", levels = c(-1, 1))
)
design <- data.frame(A = c(-1, 1, -1, 1), B = c(-1, -1, 1, 1), C = c(-1, 1, 1, -1))
folded <- an$fold_over(design, factors)
cat("fold_over nrow =", nrow(folded), " Foldover =", paste(unique(folded$Foldover), collapse = ","), "\n")
cat("mirror signs:\n"); print(folded[folded$Foldover == "foldover", c("A", "B", "C")])
cat("orig signs:\n"); print(design)

# --- get_uncoded_equation debug ---
factors2 <- list(
  list(name = "A", type = "continuous", levels = c(80, 120)),
  list(name = "B", type = "continuous", levels = c(10, 30))
)
coded_df <- data.frame(A = c(-1, 1, -1, 1), B = c(-1, -1, 1, 1))
coded_df$Y <- 5 + 2 * coded_df$A + 3 * coded_df$B
model <- lm(Y ~ A + B, data = coded_df)
cat("coef(model):\n"); print(coef(model))
unc <- an$get_uncoded_equation(model, factors2)
cat("unc coefficients:\n"); print(unc$coefficients)
cat("unc equation:\n"); print(unc$equation)

# --- plot_power_curve debug ---
curve <- an$plot_power_curve(n_factors = 3, n_replicates = 1, sigma = 1, n_points = 10)
cat("power curve:\n"); print(curve)

# --- compute_power debug ---
pw <- an$compute_power(n_factors = 3, n_replicates = 1, delta = 4, sigma = 1,
                       n_center_points = 0, alpha = 0.05, model_order = "main")
cat("compute_power delta=4:\n"); print(pw)
