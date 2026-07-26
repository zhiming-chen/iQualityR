# =============================================================================
# File: R/ml_autoencoder.R
# Description: Autoencoder anomaly detection.
#   Primary: keras/tensorflow deep autoencoder (soft dependency)
#   Fallback: PCA-based reconstruction error (always available, no extra deps)
# =============================================================================

# ---------------------------------------------------------------------------
# Internal: Train autoencoder and compute reconstruction error
# ---------------------------------------------------------------------------
# Inputs:
#   data: data.frame of numeric features (n x p)
#   encoding_dim: bottleneck dimension
#   hidden_dim: hidden layer dimension
#   epochs: training epochs (keras only)
#   batch_size: training batch size (keras only)
#   threshold_quantile: quantile for anomaly threshold (0-1)
# Returns:
#   list with:
#     reconstruction_error: numeric vector length n
#     threshold: numeric threshold value
#     anomaly_score: normalized 0-1 anomaly score
#     feature_contrib: data.frame n x p (per-feature reconstruction error)
#     backend: "keras" or "pca"
#     model: fitted model (or NULL for PCA)
#     encoder: PCA rotation or keras encoder
# ---------------------------------------------------------------------------
.ae_fit_predict <- function(data, encoding_dim, hidden_dim, epochs,
                              batch_size, threshold_quantile) {
  data <- as.data.frame(data)
  num_cols <- vapply(data, is.numeric, logical(1))
  data <- data[, num_cols, drop = FALSE]
  n <- nrow(data)
  p <- ncol(data)

  if (n < 5 || p < 1) {
    return(list(
      reconstruction_error = numeric(n),
      threshold = 0,
      anomaly_score = numeric(n),
      feature_contrib = data.frame(),
      backend = "none",
      model = NULL,
      encoder = NULL
    ))
  }

  # Standardize features
  means <- colMeans(data, na.rm = TRUE)
  sds <- vapply(data, function(x) sd(x, na.rm = TRUE), numeric(1))
  sds[sds == 0] <- 1
  data_std <- as.data.frame(sweep(sweep(data, 2, means, "-"), 2, sds, "/"))

  if (requireNamespace("keras", quietly = TRUE) &&
      requireNamespace("tensorflow", quietly = TRUE)) {
    result <- tryCatch(
      .ae_keras(data_std, encoding_dim, hidden_dim, epochs, batch_size,
                 threshold_quantile),
      error = function(e) NULL
    )
    if (!is.null(result)) return(result)
  }

  # Fallback: PCA reconstruction
  .ae_pca(data_std, encoding_dim, threshold_quantile, means, sds)
}

# ---------------------------------------------------------------------------
# Backend 1: keras deep autoencoder
# ---------------------------------------------------------------------------
.ae_keras <- function(data_std, encoding_dim, hidden_dim, epochs,
                       batch_size, threshold_quantile) {
  # Build symmetric autoencoder: input -> hidden -> encoding -> hidden -> input
  p <- ncol(data_std)
  if (encoding_dim >= p) encoding_dim <- max(1L, p - 1L)
  if (hidden_dim <= encoding_dim) hidden_dim <- encoding_dim + 2L

  # NOTE: Use function call form (not %>% pipe) to avoid importing magrittr
  # as a hard dependency. keras layers accept the input as the first argument.
  input <- keras::layer_input(shape = p)
  enc_h <- keras::layer_dense(input, units = hidden_dim, activation = "relu")
  encoded <- keras::layer_dense(enc_h, units = encoding_dim, activation = "relu")
  dec_h <- keras::layer_dense(encoded, units = hidden_dim, activation = "relu")
  decoded <- keras::layer_dense(dec_h, units = p, activation = "linear")

  autoencoder <- keras::keras_model(input, decoded)
  keras::compile(autoencoder,
    optimizer = "adam",
    loss = "mse"
  )

  # Convert data to matrix
  x_train <- as.matrix(data_std)
  # Train
  history <- keras::fit(autoencoder, x_train, x_train,
                          epochs = epochs, batch_size = batch_size,
                          verbose = 0)
  # Reconstruct
  x_recon <- keras::predict(autoencoder, x_train)

  # Per-feature reconstruction error
  feature_err <- (x_train - x_recon)^2
  # Total reconstruction error per sample
  recon_err <- rowMeans(feature_err)
  threshold <- stats::quantile(recon_err, threshold_quantile)
  # Anomaly score: 0-1 normalized
  max_err <- max(recon_err)
  if (max_err > 0) {
    anomaly_score <- recon_err / max_err
  } else {
    anomaly_score <- rep(0, length(recon_err))
  }

  list(
    reconstruction_error = recon_err,
    threshold = threshold,
    anomaly_score = anomaly_score,
    feature_contrib = as.data.frame(feature_err),
    backend = "keras",
    model = autoencoder,
    encoder = NULL
  )
}

# ---------------------------------------------------------------------------
# Backend 2: PCA-based reconstruction (fallback)
# ---------------------------------------------------------------------------
.ae_pca <- function(data_std, encoding_dim, threshold_quantile, means, sds) {
  x_mat <- as.matrix(data_std)
  p <- ncol(x_mat)
  if (encoding_dim >= p) encoding_dim <- max(1L, p - 1L)

  # Fit PCA via SVD
  svd_fit <- svd(x_mat)
  # Keep top encoding_dim components
  k <- min(encoding_dim, min(dim(x_mat)) - 1)
  k <- max(1L, k)
  rotation <- svd_fit$v[, 1:k, drop = FALSE]
  # Project and reconstruct
  scores <- x_mat %*% rotation
  x_recon <- scores %*% t(rotation)

  # Per-feature reconstruction error
  feature_err <- (x_mat - x_recon)^2
  recon_err <- rowMeans(feature_err)
  threshold <- stats::quantile(recon_err, threshold_quantile)
  max_err <- max(recon_err)
  if (max_err > 0) {
    anomaly_score <- recon_err / max_err
  } else {
    anomaly_score <- rep(0, length(recon_err))
  }

  list(
    reconstruction_error = recon_err,
    threshold = threshold,
    anomaly_score = anomaly_score,
    feature_contrib = as.data.frame(feature_err),
    backend = "pca",
    model = NULL,
    encoder = list(rotation = rotation, means = means, sds = sds, k = k)
  )
}
