# =============================================================================
# File: R/ml_lstm.R
# Description: LSTM-based time series anomaly detection.
#   Primary: keras/tensorflow LSTM autoencoder (soft dependency)
#   Fallback: ARIMA residual + exponential smoothing state (ESS) - always
#            available via base R + stats.
# =============================================================================

# ---------------------------------------------------------------------------
# Internal: Train LSTM and compute anomaly scores
# ---------------------------------------------------------------------------
# Inputs:
#   x: numeric vector of time series
#   window: lookback window size for input sequences
#   units: LSTM units per layer
#   epochs: training epochs (keras only)
#   batch_size: training batch size (keras only)
#   threshold_z: anomaly threshold in z-score units
# Returns:
#   list with:
#     anomaly_score: numeric vector length n (0-1)
#     reconstruction_error: numeric vector length n
#     threshold: numeric threshold value
#     backend: "keras" or "arima_ess"
#     model: fitted keras model or NULL
#     fit_values: numeric vector of fitted/predicted values
# ---------------------------------------------------------------------------
.lstm_fit_predict <- function(x, window, units, epochs, batch_size,
                                threshold_z) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 10) {
    return(list(
      anomaly_score = numeric(n),
      reconstruction_error = numeric(n),
      threshold = 0,
      backend = "none",
      model = NULL,
      fit_values = numeric(n)
    ))
  }

  if (requireNamespace("keras", quietly = TRUE) &&
      requireNamespace("tensorflow", quietly = TRUE)) {
    result <- tryCatch(
      .lstm_keras(x, window, units, epochs, batch_size, threshold_z),
      error = function(e) NULL
    )
    if (!is.null(result)) return(result)
  }

  # Fallback: ARIMA(1,0,0) + EWMA combination
  .lstm_arima_ess(x, threshold_z)
}

# ---------------------------------------------------------------------------
# Backend 1: keras LSTM autoencoder
# ---------------------------------------------------------------------------
.lstm_keras <- function(x, window, units, epochs, batch_size, threshold_z) {
  n <- length(x)

  # Standardize the series
  x_mean <- mean(x)
  x_sd <- sd(x)
  if (!is.finite(x_sd) || x_sd <= 0) x_sd <- 1
  x_std <- (x - x_mean) / x_sd

  # Build input sequences: for each t in [window+1, n],
  # input = x_std[(t-window):(t-1)], target = x_std[t]
  if (n <= window + 2) {
    return(.lstm_arima_ess(x, threshold_z))
  }
  n_seq <- n - window
  X <- matrix(0, nrow = n_seq, ncol = window)
  y <- numeric(n_seq)
  for (i in seq_len(n_seq)) {
    X[i, ] <- x_std[i:(i + window - 1)]
    y[i] <- x_std[i + window]
  }
  # Reshape to LSTM format: (samples, timesteps, features)
  X_arr <- array(dim = c(n_seq, window, 1))
  X_arr[, , 1] <- X

  # Build LSTM model: input -> LSTM -> Dense(1) for next-step prediction.
  # NOTE: Use function call form (not %>% pipe) to avoid importing magrittr
  # as a hard dependency. keras layers accept the input as the first argument.
  input <- keras::layer_input(shape = c(window, 1))
  lstm_out <- keras::layer_lstm(input, units = units, activation = "tanh")
  output <- keras::layer_dense(lstm_out, units = 1, activation = "linear")
  model <- keras::keras_model(input, output)
  keras::compile(model, optimizer = "adam", loss = "mse")

  # Train
  keras::fit(model, X_arr, y,
             epochs = epochs, batch_size = batch_size, verbose = 0)

  # Predict
  y_pred <- as.numeric(keras::predict(model, X_arr))

  # Reconstruction error per timestep (only available for t >= window+1)
  err_full <- numeric(n)
  err_full[(window + 1):n] <- (y - y_pred)^2
  # For first `window` observations, use mean error
  if (window > 0) {
    err_full[1:window] <- mean(err_full[(window + 1):n])
  }

  # Convert to anomaly score (z-score of error)
  err_sd <- sd(err_full)
  if (!is.finite(err_sd) || err_sd <= 0) err_sd <- 1
  err_z <- (err_full - mean(err_full)) / err_sd
  anomaly_score <- 1 / (1 + exp(-err_z))  # sigmoid to [0, 1]

  # Fit values (for plotting)
  fit_full <- numeric(n)
  fit_full[(window + 1):n] <- y_pred * x_sd + x_mean
  if (window > 0) {
    fit_full[1:window] <- x_mean
  }

  list(
    anomaly_score = anomaly_score,
    reconstruction_error = err_full,
    threshold = threshold_z^2,  # squared error threshold
    backend = "keras",
    model = model,
    fit_values = fit_full
  )
}

# ---------------------------------------------------------------------------
# Backend 2: ARIMA + ESS (fallback)
# ---------------------------------------------------------------------------
.lstm_arima_ess <- function(x, threshold_z) {
  n <- length(x)

  # Fit ARIMA(1,0,1) by default, fall back to AR(1) if fails
  fit <- tryCatch(
    stats::arima(x, order = c(1L, 0L, 1L)),
    error = function(e) {
      tryCatch(stats::arima(x, order = c(1L, 0L, 0L)),
               error = function(e2) NULL)
    }
  )

  if (is.null(fit)) {
    # Ultimate fallback: mean + EWMA
    fit_values <- stats::filter(x, filter = 0.2, method = "recursive")
    fit_values[1] <- x[1]
  } else {
    # Fitted values via one-step-ahead prediction.
    # NOTE: stats::fitted.Arima() returns a 0-length vector on some R versions
    # (e.g., R 4.5.x) when the ARIMA model uses method = "CSS-ML" with an
    # intercept. Reconstruct fitted values as x - residuals to be safe.
    resid <- as.numeric(stats::residuals(fit))
    if (length(resid) == n) {
      fit_values <- x - resid
    } else if (length(resid) > 0) {
      # residuals shorter than x (e.g., due to differencing); align
      fit_values <- rep(NA_real_, n)
      fit_values[(n - length(resid) + 1):n] <- x[(n - length(resid) + 1):n] - resid
      fit_values[is.na(fit_values)] <- x[1]
    } else {
      fit_values <- stats::filter(x, filter = 0.2, method = "recursive")
      fit_values[1] <- x[1]
    }
  }

  # Compute residuals
  err <- x - fit_values
  # Use ARIMA residuals + exponentially weighted moving average of squared errors
  # as anomaly score
  err_sq <- err^2
  # EWMA of squared errors
  ewma_err <- stats::filter(err_sq, filter = 0.2, method = "recursive")
  ewma_err[1] <- err_sq[1]

  # Z-score of EWMA error
  err_mean <- mean(ewma_err)
  err_sd <- sd(ewma_err)
  if (!is.finite(err_sd) || err_sd <= 0) err_sd <- 1
  err_z <- (ewma_err - err_mean) / err_sd

  # Anomaly score: 0-1 via sigmoid
  anomaly_score <- 1 / (1 + exp(-err_z))

  list(
    anomaly_score = anomaly_score,
    reconstruction_error = ewma_err,
    threshold = threshold_z^2,
    backend = "arima_ess",
    model = fit,
    fit_values = fit_values
  )
}
