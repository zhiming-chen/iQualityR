# =============================================================================
# File: R/ml_iforest.R
# Description: Isolation Forest anomaly detection with isotree soft dependency.
#             Falls back to a simple R-native isolation forest when isotree
#             is not available.
# =============================================================================

# ---------------------------------------------------------------------------
# Internal: Train and score isolation forest
# ---------------------------------------------------------------------------
# Inputs:
#   data: data.frame of numeric features (n x p)
#   ntree: number of isolation trees
#   sample_size: subsample size per tree
# Returns:
#   list with:
#     anomaly_score: numeric vector length n (0-1, higher = more anomalous)
#     feature_contrib: data.frame n x p (SHAP-like contribution per feature)
#     backend: "isotree" or "native"
#     model: fitted model object (or NULL for native)
# ---------------------------------------------------------------------------
.iforest_fit_predict <- function(data, ntree, sample_size) {
  data <- as.data.frame(data)
  # Keep only numeric columns
  num_cols <- vapply(data, is.numeric, logical(1))
  data <- data[, num_cols, drop = FALSE]
  n <- nrow(data)
  p <- ncol(data)

  if (n < 2 || p < 1) {
    return(list(
      anomaly_score = numeric(n),
      feature_contrib = data.frame(),
      backend = "none",
      model = NULL))
  }

  if (requireNamespace("isotree", quietly = TRUE)) {
    .iforest_isotree(data, ntree, sample_size)
  } else {
    .iforest_native(data, ntree, sample_size)
  }
}

# ---------------------------------------------------------------------------
# Backend 1: isotree (preferred)
# ---------------------------------------------------------------------------
.iforest_isotree <- function(data, ntree, sample_size) {
  n <- nrow(data)
  model <- isotree::isolation.forest(
    data = data,
    ntrees = ntree,
    sample_size = min(sample_size, n),
    ndim = 1L,
    ntry = 1L,
    penalize_range = TRUE,
    coalesce = TRUE,
    random_state = 2026L
  )
  # Anomaly score: isotree returns expected average path length (smaller = more anomalous)
  # Convert to 0-1 scale where higher = more anomalous via transformation.
  pred <- isotree::predict.isolation_forest(model, data)
  # pred is average depth; convert to anomaly score in (0, 1)
  # Using s(x) = 2^(-E[h(x)] / c(n)) where c(n) = 2*H(n-1) - 2*(n-1)/n
  # For simplicity, use a monotone transform: score = 1 - pred / max(pred)
  # Alternatively, use the built-in score that's already in 0-1.
  # isotree's predict with score_samples = TRUE returns log-density; we use
  # the simpler transform.
  raw_score <- pred
  # Normalize to [0, 1] using min-max
  if (length(unique(raw_score)) > 1) {
    anomaly_score <- 1 - (raw_score - min(raw_score)) /
      (max(raw_score) - min(raw_score))
  } else {
    anomaly_score <- rep(0.5, n)
  }

  # Feature contributions: isotree supports terminal node attributes
  # Fall back to simple proxy: per-feature (x - mean)^2 / sum
  feature_contrib <- .iforest_feature_proxy(data, anomaly_score)

  list(
    anomaly_score = anomaly_score,
    feature_contrib = feature_contrib,
    backend = "isotree",
    model = model
  )
}

# ---------------------------------------------------------------------------
# Backend 2: Native R isolation forest (fallback)
# ---------------------------------------------------------------------------
.iforest_native <- function(data, ntree, sample_size) {
  n <- nrow(data)
  p <- ncol(data)
  # Average path length normalization c(n)
  c_n <- function(m) {
    if (m <= 1) return(1)
    2 * (log(m - 1) + 0.5772156649) - 2 * (m - 1) / m
  }
  # Build trees and compute path length per observation
  path_lengths <- matrix(0, n, ntree)
  for (b in seq_len(ntree)) {
    # Subsample
    ss <- if (n <= sample_size) seq_len(n) else
      sample.int(n, sample_size, replace = FALSE)
    sub_data <- data[ss, , drop = FALSE]
    # Build a single tree: random splits on random features until depth log2(ss)
    max_depth <- ceiling(log2(max(2, length(ss))))
    tree <- .iforest_build_tree(sub_data, 1, max_depth, 1)
    # Compute path length for each observation in original data
    for (i in seq_len(n)) {
      path_lengths[i, b] <- .iforest_path_length(data[i, , drop = FALSE], tree, 0)
    }
  }
  avg_path <- rowMeans(path_lengths)
  # Anomaly score: s(x) = 2^(-E[h(x)] / c(n))
  anomaly_score <- 2^(-avg_path / c_n(sample_size))

  # Feature contributions proxy
  feature_contrib <- .iforest_feature_proxy(data, anomaly_score)

  list(
    anomaly_score = anomaly_score,
    feature_contrib = feature_contrib,
    backend = "native",
    model = NULL
  )
}

# Build a single isolation tree recursively
.iforest_build_tree <- function(data, depth, max_depth, node_id) {
  n <- nrow(data)
  if (n <= 1 || depth >= max_depth) {
    return(list(
      is_leaf = TRUE,
      size = n,
      depth = depth,
      node_id = node_id
    ))
  }
  # Random feature and split
  p <- ncol(data)
  feat <- sample.int(p, 1)
  feat_vals <- data[[feat]]
  feat_min <- min(feat_vals)
  feat_max <- max(feat_vals)
  if (feat_min == feat_max) {
    return(list(
      is_leaf = TRUE,
      size = n,
      depth = depth,
      node_id = node_id
    ))
  }
  split <- runif(1, feat_min, feat_max)
  left_idx <- which(data[[feat]] < split)
  right_idx <- which(data[[feat]] >= split)
  if (length(left_idx) == 0 || length(right_idx) == 0) {
    return(list(
      is_leaf = TRUE,
      size = n,
      depth = depth,
      node_id = node_id
    ))
  }
  list(
    is_leaf = FALSE,
    split_feature = feat,
    split_value = split,
    left = .iforest_build_tree(data[left_idx, , drop = FALSE],
                                depth + 1, max_depth, node_id * 2),
    right = .iforest_build_tree(data[right_idx, , drop = FALSE],
                                 depth + 1, max_depth, node_id * 2 + 1),
    depth = depth,
    node_id = node_id
  )
}

# Compute path length for a single observation
.iforest_path_length <- function(x_row, tree, current_depth) {
  if (isTRUE(tree$is_leaf)) {
    # Add c(size) for the trailing path length
    c_size <- if (tree$size <= 1) 0 else
      2 * (log(tree$size - 1) + 0.5772156649) - 2 * (tree$size - 1) / tree$size
    return(current_depth + c_size)
  }
  feat <- tree$split_feature
  if (x_row[[feat]] < tree$split_value) {
    .iforest_path_length(x_row, tree$left, current_depth + 1)
  } else {
    .iforest_path_length(x_row, tree$right, current_depth + 1)
  }
}

# Simple feature contribution proxy: deviation from mean scaled by anomaly score
.iforest_feature_proxy <- function(data, anomaly_score) {
  p <- ncol(data)
  n <- nrow(data)
  if (p == 0 || n == 0) {
    return(data.frame())
  }
  means <- colMeans(data, na.rm = TRUE)
  devs <- sweep(data, 2, means, "-")
  # Weighted by anomaly_score
  contrib <- sweep(devs, 1, anomaly_score, "*")
  as.data.frame(contrib)
}
