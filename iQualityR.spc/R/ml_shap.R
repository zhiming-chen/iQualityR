# =============================================================================
# File: R/ml_shap.R
# Description: SHAP (SHapley Additive exPlanations) attribution wrapper.
#   Primary: fastshap::explain (KernelSHAP) - soft dependency
#   Fallback: simple feature ablation (drop-column delta)
# =============================================================================

# ---------------------------------------------------------------------------
# Internal: Compute SHAP values for an anomaly detection model
# ---------------------------------------------------------------------------
# Inputs:
#   data: data.frame of numeric features (n x p)
#   predict_fn: function(data) -> numeric vector of anomaly scores
#   nsample: number of Monte Carlo samples for KernelSHAP
# Returns:
#   list with:
#     shap_values: data.frame n x p (per-feature SHAP contribution)
#     backend: "fastshap" or "ablation"
#     feature_names: character vector
# ---------------------------------------------------------------------------
.shap_explain <- function(data, predict_fn, nsample = 50L) {
  data <- as.data.frame(data)
  num_cols <- vapply(data, is.numeric, logical(1))
  data <- data[, num_cols, drop = FALSE]
  n <- nrow(data)
  p <- ncol(data)

  if (n < 2 || p < 1) {
    return(list(
      shap_values = data.frame(),
      backend = "none",
      feature_names = character(0)
    ))
  }

  if (requireNamespace("fastshap", quietly = TRUE)) {
    result <- tryCatch(
      .shap_fastshap(data, predict_fn, nsample),
      error = function(e) NULL
    )
    if (!is.null(result)) return(result)
  }

  # Fallback: feature ablation
  .shap_ablation(data, predict_fn)
}

# ---------------------------------------------------------------------------
# Backend 1: fastshap (KernelSHAP)
# ---------------------------------------------------------------------------
.shap_fastshap <- function(data, predict_fn, nsample) {
  feature_names <- names(data)

  # Build a wrapped prediction function suitable for fastshap
  # fastshap::explain expects: predict_fn(X, newdata)
  # where X is the data and newdata is the instance to explain
  wrapped <- function(object, newdata) {
    predict_fn(newdata)
  }

  # Explain all instances at once
  shap_df <- fastshap::explain(
    object = data,
    newdata = data,
    predict_function = wrapped,
    nsim = nsample,
    verbose = FALSE
  )

  # shap_df is a data.frame of SHAP values, n x p
  list(
    shap_values = as.data.frame(shap_df),
    backend = "fastshap",
    feature_names = feature_names
  )
}

# ---------------------------------------------------------------------------
# Backend 2: Feature ablation (drop-column delta)
# ---------------------------------------------------------------------------
.shap_ablation <- function(data, predict_fn) {
  n <- nrow(data)
  p <- ncol(data)
  feature_names <- names(data)

  # Baseline: predict on full data
  full_pred <- predict_fn(data)

  # Per-feature: replace with mean, measure delta
  shap_mat <- matrix(0, nrow = n, ncol = p)
  colnames(shap_mat) <- feature_names
  for (j in seq_len(p)) {
    data_perturbed <- data
    data_perturbed[[j]] <- mean(data[[j]], na.rm = TRUE)
    perturbed_pred <- predict_fn(data_perturbed)
    # SHAP approximation: contribution = full - perturbed
    shap_mat[, j] <- full_pred - perturbed_pred
  }

  list(
    shap_values = as.data.frame(shap_mat),
    backend = "ablation",
    feature_names = feature_names
  )
}

# ---------------------------------------------------------------------------
# Helper: Build a unified ai_diagnostic structure
# ---------------------------------------------------------------------------
# Inputs:
#   method: string identifying the ML method
#   anomaly_score: numeric vector
#   shap_values: data.frame (may be empty)
#   rule_attribution: character vector of human-readable rules
#   confidence: numeric scalar in [0, 1]
#   extras: optional named list of additional method-specific info
# Returns:
#   Standardized ai_diagnostic list
# ---------------------------------------------------------------------------
.build_ai_diagnostic <- function(method, anomaly_score, shap_values,
                                    rule_attribution, confidence, extras = NULL) {
  ai_diag <- list(
    method = method,
    anomaly_score = anomaly_score,
    shap_values = shap_values,
    rule_attribution = rule_attribution,
    confidence = confidence
  )
  if (!is.null(extras)) {
    for (nm in names(extras)) {
      ai_diag[[nm]] <- extras[[nm]]
    }
  }
  ai_diag
}
