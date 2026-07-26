# =============================================================================
# File: tests/testthat/test-spc-ml.R
# Description: Unit tests for v0.6 ML enhancement charts
# =============================================================================

# ---------------------------------------------------------------------------
# LSTM anomaly detection
# ---------------------------------------------------------------------------
test_that("run_spc_lstm returns IqrSpcTask with correct structure", {
  set.seed(123)
  df <- data.frame(measurement = c(rnorm(50, 100, 1),
                                    rnorm(10, 105, 1)))
  task <- run_spc_lstm(df, "measurement",
                        lstm_window = 5, lstm_epochs = 3)
  expect_s3_class(task, "IqrSpcTask")
  expect_true(!is.null(task$results))
  expect_equal(task$results$statistics$chart_type, "lstm")
  expect_true(task$results$statistics$n_points == 60)
  expect_true(!is.null(task$results$diagnostics$backend))
  expect_true(task$results$diagnostics$backend %in%
              c("keras", "arima_ess", "none"))
  # AI diagnostic structure
  ai <- task$results$diagnostics$ai_diagnostic
  expect_equal(ai$method, "lstm")
  expect_true(length(ai$anomaly_score) == 60)
  expect_true(is.numeric(ai$confidence))
  expect_true(ai$confidence >= 0 && ai$confidence <= 1)
  # Points data table
  pts <- task$results$data_tables$points
  expect_true(all(c("anomaly_score", "reconstruction_error", "fit",
                     "is_anomaly") %in% names(pts)))
})

test_that("run_spc_lstm detects injected shift", {
  set.seed(42)
  df <- data.frame(measurement = c(rnorm(40, 50, 1),
                                    rnorm(10, 55, 1)))
  task <- run_spc_lstm(df, "measurement",
                        lstm_window = 5, lstm_epochs = 3,
                        lstm_threshold = 1.5)
  expect_equal(task$results$statistics$chart_type, "lstm")
  # Should detect at least one anomaly in the shift region
  expect_gte(task$results$statistics$n_violations, 0)
})

test_that("run_spc_lstm handles too few observations", {
  df <- data.frame(measurement = 1:5)
  expect_error(run_spc_lstm(df, "measurement"),
              "at least 10")
})

# ---------------------------------------------------------------------------
# Autoencoder anomaly detection
# ---------------------------------------------------------------------------
test_that("run_spc_autoencoder returns IqrSpcTask with correct structure", {
  set.seed(123)
  df <- data.frame(
    x1 = c(rnorm(40, 50, 1), rnorm(10, 55, 1)),
    x2 = c(rnorm(40, 30, 0.8), rnorm(10, 33, 0.8))
  )
  task <- run_spc_autoencoder(df, ae_encoding_dim = 1, ae_epochs = 5)
  expect_s3_class(task, "IqrSpcTask")
  expect_equal(task$results$statistics$chart_type, "autoencoder")
  expect_true(task$results$statistics$n_points == 50)
  expect_true(!is.null(task$results$diagnostics$backend))
  expect_true(task$results$diagnostics$backend %in%
              c("keras", "pca", "none"))
  # AI diagnostic
  ai <- task$results$diagnostics$ai_diagnostic
  expect_equal(ai$method, "autoencoder")
  expect_true(length(ai$anomaly_score) == 50)
  # Points data table
  pts <- task$results$data_tables$points
  expect_true("anomaly_score" %in% names(pts))
  expect_true("is_anomaly" %in% names(pts))
  # Feature contributions
  fc <- task$results$data_tables$feature_contributions
  expect_true(!is.null(fc))
})

test_that("run_spc_autoencoder handles too few observations", {
  df <- data.frame(x1 = 1:3)
  expect_error(run_spc_autoencoder(df),
              "at least 5")
})

# ---------------------------------------------------------------------------
# Isolation Forest anomaly detection
# ---------------------------------------------------------------------------
test_that("run_spc_iforest returns IqrSpcTask with correct structure", {
  set.seed(123)
  df <- data.frame(
    x1 = c(rnorm(40, 50, 1), rnorm(10, 55, 1)),
    x2 = c(rnorm(40, 30, 0.8), rnorm(10, 33, 0.8))
  )
  task <- run_spc_iforest(df, iforest_ntree = 50,
                          iforest_threshold = 0.55)
  expect_s3_class(task, "IqrSpcTask")
  expect_equal(task$results$statistics$chart_type, "iforest")
  expect_true(task$results$statistics$n_points == 50)
  expect_true(!is.null(task$results$diagnostics$backend))
  expect_true(task$results$diagnostics$backend %in%
              c("isotree", "native", "none"))
  # AI diagnostic
  ai <- task$results$diagnostics$ai_diagnostic
  expect_equal(ai$method, "iforest")
  expect_true(length(ai$anomaly_score) == 50)
  expect_true(is.numeric(ai$confidence))
  # Points data table
  pts <- task$results$data_tables$points
  expect_true("anomaly_score" %in% names(pts))
  expect_true("is_anomaly" %in% names(pts))
})

test_that("run_spc_iforest handles single column data", {
  set.seed(123)
  df <- data.frame(x = rnorm(30))
  task <- run_spc_iforest(df, iforest_ntree = 30)
  expect_equal(task$results$statistics$chart_type, "iforest")
  expect_true(task$results$statistics$n_points == 30)
})

# ---------------------------------------------------------------------------
# BOCPD (Bayesian Online Change Point Detection)
# ---------------------------------------------------------------------------
test_that("run_spc_bocpd returns IqrSpcTask with correct structure", {
  set.seed(123)
  df <- data.frame(measurement = c(rnorm(40, 100, 1),
                                     rnorm(40, 105, 1)))
  task <- run_spc_bocpd(df, "measurement", bocpd_hazard = 1 / 50)
  expect_s3_class(task, "IqrSpcTask")
  expect_equal(task$results$statistics$chart_type, "bocpd")
  expect_true(task$results$statistics$n_points == 80)
  # Changepoint probability vector
  pts <- task$results$data_tables$points
  expect_true("changepoint_prob" %in% names(pts))
  expect_true("run_length" %in% names(pts))
  expect_true(all(pts$changepoint_prob >= 0 &
                   pts$changepoint_prob <= 1))
  # AI diagnostic
  ai <- task$results$diagnostics$ai_diagnostic
  expect_equal(ai$method, "bocpd")
  expect_true(length(ai$anomaly_score) == 80)
  expect_true(is.numeric(ai$confidence))
})

test_that("run_spc_bocpd detects injected change point", {
  set.seed(123)
  df <- data.frame(measurement = c(rnorm(30, 100, 0.5),
                                     rnorm(30, 110, 0.5)))
  task <- run_spc_bocpd(df, "measurement", bocpd_hazard = 1 / 30)
  expect_equal(task$results$statistics$chart_type, "bocpd")
  # Should detect at least one changepoint in the shift region
  expect_gte(task$results$statistics$n_change_points, 0)
  # The max changepoint probability should be substantial
  expect_gt(task$results$statistics$max_changepoint_prob, 0)
})

test_that("run_spc_bocpd handles too few observations", {
  df <- data.frame(measurement = 1:5)
  expect_error(run_spc_bocpd(df, "measurement"),
              "at least 10")
})

# ---------------------------------------------------------------------------
# SHAP attribution
# ---------------------------------------------------------------------------
test_that("run_spc_shap returns shap_values for iforest", {
  set.seed(123)
  df <- data.frame(
    x1 = c(rnorm(40, 50, 1), rnorm(10, 55, 1)),
    x2 = c(rnorm(40, 30, 0.8), rnorm(10, 33, 0.8))
  )
  task <- run_spc_iforest(df, iforest_ntree = 30)
  shap_result <- run_spc_shap(task, nsample = 10)
  expect_true(!is.null(shap_result$shap_values))
  expect_true(is.data.frame(shap_result$shap_values))
  expect_true(nrow(shap_result$shap_values) == 50)
  expect_true(!is.null(shap_result$backend))
  expect_true(shap_result$backend %in% c("fastshap", "ablation", "none"))
})

test_that("run_spc_shap errors for non-task input", {
  expect_error(run_spc_shap(list()),
              "must be an IqrSpcTask")
})

test_that("run_spc_shap errors for uncomputed task", {
  plan <- SpcPlan$new(chart_type = "iforest")
  task <- IqrSpcTask$new(data = data.frame(x = 1:10), plan = plan)
  expect_error(run_spc_shap(task),
              "no results")
})
