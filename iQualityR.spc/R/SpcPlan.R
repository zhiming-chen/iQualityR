# =============================================================================
# File: R/SpcPlan.R
# Description: SPC analysis plan configuration (inherits IqrPlanBase)
# =============================================================================

#' @title SpcPlan
#' @description
#' Plan configuration for statistical process control (SPC) analysis.
#' Inherits `IqrPlanBase` and adds SPC-specific parameters such as chart type,
#' sigma estimation method, subgroup size, Nelson rules, and chart-specific
#' tuning constants (e.g. EWMA lambda, CUSUM k/h).
#'
#' @field chart_type Character. Chart type, one of:
#'   `"xbar_r"`, `"xbar_s"`, `"imr"`, `"imr_rs"`,
#'   `"p"`, `"np"`, `"u"`, `"c"`, `"p_laney"`, `"u_laney"`,
#'   `"ewma"`, `"cusum"`, `"ma"`,
#'   `"t2"`, `"mewma"`, `"g"`, `"t"`,
#'   `"adaptive"`, `"arima_resid"`, `"aewma"`, `"changepoint"`,
#'   `"kde"`, `"t2_mewma"`.
#' @field sigma_method Character. Sigma estimation method, one of
#'   `"r_bar"`, `"s_bar"`, `"pooled_s"`, `"mr_bar"`, `"mr_median"`,
#'   `"mssd"`, `"total"`.
#' @field subgroup_size Integer. Subgroup size for variables charts (>=1).
#'   For I-MR charts set to 1.
#' @field subgroup Optional subgroup column name.
#' @field nelson_rules Integer vector. Nelson rule numbers to enable (default 1:8).
#' @field lambda Numeric. EWMA smoothing parameter (0,1]. Default 0.2.
#' @field k Numeric. CUSUM reference value (in sigma units). Default 0.5.
#' @field h Numeric. CUSUM decision interval (in sigma units). Default 4.77.
#' @field ma_window Integer. Moving-average window size. Default 3.
#' @field phase Character. `"phase1"` or `"phase2"`. Default `"phase1"`.
#' @field phase_boundaries Integer vector. Indices marking phase boundaries
#'   (for stage analysis). Default `NULL`.
#' @field target Optional numeric target value (used by some charts).
#' @field window_size Integer. Rolling window size for adaptive chart. Default 20.
#' @field arima_order Integer vector of length 3. ARIMA order (p,d,q). Default c(1,0,1).
#' @field aewma_lambda Numeric. Base smoothing for adaptive EWMA. Default 0.2.
#' @field aewma_k Numeric. Sensitivity parameter for adaptive EWMA. Default 1.5.
#' @field cp_method Character. Change-point method. Default `"PELT"`.
#' @field cp_penalty Character. Change-point penalty. Default `"MBIC"`.
#' @field kde_bandwidth Optional numeric KDE bandwidth. NULL uses SJ. Default NULL.
#'
#' v0.6 ML enhancement fields:
#' @field lstm_units Integer. Number of LSTM units per layer. Default 32.
#' @field lstm_window Integer. Lookback window for LSTM input sequences. Default 10.
#' @field lstm_epochs Integer. Training epochs. Default 20.
#' @field lstm_batch_size Integer. Training batch size. Default 16.
#' @field lstm_threshold Numeric. Anomaly score threshold (z-scale). Default 3.
#' @field ae_encoding_dim Integer. Autoencoder bottleneck dimension. Default 2.
#' @field ae_hidden_dim Integer. Autoencoder hidden layer dimension. Default 8.
#' @field ae_epochs Integer. Autoencoder training epochs. Default 50.
#' @field ae_batch_size Integer. Autoencoder training batch size. Default 16.
#' @field ae_threshold_quantile Numeric. Quantile for threshold (0-1). Default 0.99.
#' @field iforest_ntree Integer. Number of isolation trees. Default 200.
#' @field iforest_sample_size Integer. Subsample size per tree. Default 256.
#' @field iforest_threshold Numeric. Anomaly score threshold (0-1). Default 0.6.
#' @field bocpd_hazard Numeric. Hazard rate (1/expected run length). Default 1/250.
#' @field bocpd_prior_mu Numeric. Prior mean for Gaussian conjugate model. Default 0.
#' @field bocpd_prior_kappa Numeric. Prior precision weight. Default 1.
#' @field bocpd_prior_alpha Numeric. Prior alpha for inverse-gamma. Default 1.
#' @field bocpd_prior_beta Numeric. Prior beta for inverse-gamma. Default 1.
#' @field ml_use_gpu Logical. Use GPU for keras training if available. Default FALSE.
#'
#' @param chart_type Chart type string.
#' @param sigma_method Sigma method string.
#' @param subgroup_size Subgroup size.
#' @param subgroup Optional subgroup column name.
#' @param nelson_rules Nelson rule numbers enabled.
#' @param lambda EWMA lambda.
#' @param k CUSUM k.
#' @param h CUSUM h.
#' @param ma_window Moving-average window.
#' @param phase Phase string.
#' @param phase_boundaries Phase boundary indices.
#' @param target Target value.
#' @param window_size Adaptive rolling window size.
#' @param arima_order ARIMA order (p,d,q).
#' @param aewma_lambda Adaptive EWMA base smoothing.
#' @param aewma_k Adaptive EWMA sensitivity.
#' @param cp_method Change-point method.
#' @param cp_penalty Change-point penalty.
#' @param kde_bandwidth KDE bandwidth.
#' @param lstm_units LSTM units per layer.
#' @param lstm_window LSTM lookback window.
#' @param lstm_epochs LSTM training epochs.
#' @param lstm_batch_size LSTM training batch size.
#' @param lstm_threshold LSTM anomaly threshold (z-scale).
#' @param ae_encoding_dim Autoencoder bottleneck dimension.
#' @param ae_hidden_dim Autoencoder hidden dimension.
#' @param ae_epochs Autoencoder training epochs.
#' @param ae_batch_size Autoencoder training batch size.
#' @param ae_threshold_quantile Autoencoder threshold quantile.
#' @param iforest_ntree Number of isolation trees.
#' @param iforest_sample_size Subsample size per tree.
#' @param iforest_threshold Isolation forest threshold.
#' @param bocpd_hazard BOCPD hazard rate.
#' @param bocpd_prior_mu BOCPD prior mean.
#' @param bocpd_prior_kappa BOCPD prior kappa.
#' @param bocpd_prior_alpha BOCPD prior alpha.
#' @param bocpd_prior_beta BOCPD prior beta.
#' @param ml_use_gpu Use GPU for keras training.
#' @param conf_level Confidence level.
#' @param task_tag Task tag.
#' @param ... Additional arguments.
#'
#' @export
SpcPlan <- R6::R6Class("SpcPlan",
  inherit = IqrPlanBase,
  public = list(
    chart_type = NULL,
    sigma_method = NULL,
    subgroup_size = NULL,
    subgroup = NULL,
    nelson_rules = 1:8,
    lambda = 0.2,
    k = 0.5,
    h = 4.77,
    ma_window = 3L,
    phase = "phase1",
    phase_boundaries = NULL,
    target = NULL,
    window_size = 20L,
    arima_order = c(1L, 0L, 1L),
    aewma_lambda = 0.2,
    aewma_k = 1.5,
    cp_method = "PELT",
    cp_penalty = "MBIC",
    kde_bandwidth = NULL,
    # v0.6 ML enhancement fields
    lstm_units = 32L,
    lstm_window = 10L,
    lstm_epochs = 20L,
    lstm_batch_size = 16L,
    lstm_threshold = 3,
    ae_encoding_dim = 2L,
    ae_hidden_dim = 8L,
    ae_epochs = 50L,
    ae_batch_size = 16L,
    ae_threshold_quantile = 0.99,
    iforest_ntree = 200L,
    iforest_sample_size = 256L,
    iforest_threshold = 0.6,
    bocpd_hazard = 1 / 250,
    bocpd_prior_mu = 0,
    bocpd_prior_kappa = 1,
    bocpd_prior_alpha = 1,
    bocpd_prior_beta = 1,
    ml_use_gpu = FALSE,

    #' @description Create a new SpcPlan object
    initialize = function(chart_type = "imr",
                          sigma_method = NULL,
                          subgroup_size = NULL,
                          subgroup = NULL,
                          nelson_rules = 1:8,
                          lambda = 0.2,
                          k = 0.5,
                          h = 4.77,
                          ma_window = 3L,
                          phase = c("phase1", "phase2"),
                          phase_boundaries = NULL,
                          target = NULL,
                          window_size = 20L,
                          arima_order = c(1L, 0L, 1L),
                          aewma_lambda = 0.2,
                          aewma_k = 1.5,
                          cp_method = "PELT",
                          cp_penalty = "MBIC",
                          kde_bandwidth = NULL,
                          lstm_units = 32L,
                          lstm_window = 10L,
                          lstm_epochs = 20L,
                          lstm_batch_size = 16L,
                          lstm_threshold = 3,
                          ae_encoding_dim = 2L,
                          ae_hidden_dim = 8L,
                          ae_epochs = 50L,
                          ae_batch_size = 16L,
                          ae_threshold_quantile = 0.99,
                          iforest_ntree = 200L,
                          iforest_sample_size = 256L,
                          iforest_threshold = 0.6,
                          bocpd_hazard = 1 / 250,
                          bocpd_prior_mu = 0,
                          bocpd_prior_kappa = 1,
                          bocpd_prior_alpha = 1,
                          bocpd_prior_beta = 1,
                          ml_use_gpu = FALSE,
                          conf_level = 0.95,
                          task_tag = "spc",
                          ...) {
      super$initialize(task_tag = task_tag, conf_level = conf_level, ...)
      chart_type <- match.arg(chart_type, .SPC_CHART_TYPES)
      phase <- match.arg(phase)
      self$chart_type <- chart_type
      self$sigma_method <- sigma_method %||% .default_sigma_method(chart_type)
      if (is.null(subgroup_size)) {
        subgroup_size <- .default_subgroup_size(chart_type)
      }
      if (!is.numeric(subgroup_size) || subgroup_size < 1) {
        stop("subgroup_size must be a positive integer.", call. = FALSE)
      }
      self$subgroup_size <- as.integer(subgroup_size)
      self$subgroup <- subgroup
      self$nelson_rules <- as.integer(nelson_rules)
      if (!is.numeric(lambda) || lambda <= 0 || lambda > 1) {
        stop("lambda must be in (0, 1].", call. = FALSE)
      }
      self$lambda <- lambda
      if (!is.numeric(k) || k <= 0) stop("k must be positive.", call. = FALSE)
      self$k <- k
      if (!is.numeric(h) || h <= 0) stop("h must be positive.", call. = FALSE)
      self$h <- h
      if (!is.numeric(ma_window) || ma_window < 1) {
        stop("ma_window must be a positive integer.", call. = FALSE)
      }
      self$ma_window <- as.integer(ma_window)
      self$phase <- phase
      self$phase_boundaries <- phase_boundaries
      self$target <- target
      if (!is.numeric(window_size) || window_size < 5) {
        stop("window_size must be an integer >= 5.", call. = FALSE)
      }
      self$window_size <- as.integer(window_size)
      if (!is.numeric(arima_order) || length(arima_order) != 3 ||
          any(arima_order < 0)) {
        stop("arima_order must be a length-3 integer vector (p,d,q) with non-negative entries.", call. = FALSE)
      }
      self$arima_order <- as.integer(arima_order)
      if (!is.numeric(aewma_lambda) || aewma_lambda <= 0 || aewma_lambda > 1) {
        stop("aewma_lambda must be in (0, 1].", call. = FALSE)
      }
      self$aewma_lambda <- aewma_lambda
      if (!is.numeric(aewma_k) || aewma_k <= 0) {
        stop("aewma_k must be positive.", call. = FALSE)
      }
      self$aewma_k <- aewma_k
      if (!is.character(cp_method) || length(cp_method) != 1) {
        stop("cp_method must be a single string.", call. = FALSE)
      }
      self$cp_method <- cp_method
      if (!is.character(cp_penalty) || length(cp_penalty) != 1) {
        stop("cp_penalty must be a single string.", call. = FALSE)
      }
      self$cp_penalty <- cp_penalty
      if (!is.null(kde_bandwidth)) {
        if (!is.numeric(kde_bandwidth) || kde_bandwidth <= 0) {
          stop("kde_bandwidth must be NULL or a positive number.", call. = FALSE)
        }
      }
      self$kde_bandwidth <- kde_bandwidth
      # v0.6 ML enhancement parameter validation
      .validate_pos_int <- function(x, name) {
        if (!is.numeric(x) || length(x) != 1 || x < 1) {
          stop(name, " must be a positive integer.", call. = FALSE)
        }
        as.integer(x)
      }
      .validate_pos_num <- function(x, name) {
        if (!is.numeric(x) || length(x) != 1 || x <= 0) {
          stop(name, " must be a positive number.", call. = FALSE)
        }
        as.numeric(x)
      }
      self$lstm_units <- .validate_pos_int(lstm_units, "lstm_units")
      self$lstm_window <- .validate_pos_int(lstm_window, "lstm_window")
      self$lstm_epochs <- .validate_pos_int(lstm_epochs, "lstm_epochs")
      self$lstm_batch_size <- .validate_pos_int(lstm_batch_size, "lstm_batch_size")
      self$lstm_threshold <- .validate_pos_num(lstm_threshold, "lstm_threshold")
      self$ae_encoding_dim <- .validate_pos_int(ae_encoding_dim, "ae_encoding_dim")
      self$ae_hidden_dim <- .validate_pos_int(ae_hidden_dim, "ae_hidden_dim")
      self$ae_epochs <- .validate_pos_int(ae_epochs, "ae_epochs")
      self$ae_batch_size <- .validate_pos_int(ae_batch_size, "ae_batch_size")
      if (!is.numeric(ae_threshold_quantile) || ae_threshold_quantile <= 0 ||
          ae_threshold_quantile >= 1) {
        stop("ae_threshold_quantile must be in (0, 1).", call. = FALSE)
      }
      self$ae_threshold_quantile <- ae_threshold_quantile
      self$iforest_ntree <- .validate_pos_int(iforest_ntree, "iforest_ntree")
      self$iforest_sample_size <- .validate_pos_int(iforest_sample_size, "iforest_sample_size")
      if (!is.numeric(iforest_threshold) || iforest_threshold <= 0 ||
          iforest_threshold >= 1) {
        stop("iforest_threshold must be in (0, 1).", call. = FALSE)
      }
      self$iforest_threshold <- iforest_threshold
      if (!is.numeric(bocpd_hazard) || bocpd_hazard <= 0 || bocpd_hazard >= 1) {
        stop("bocpd_hazard must be in (0, 1).", call. = FALSE)
      }
      self$bocpd_hazard <- bocpd_hazard
      self$bocpd_prior_mu <- .validate_pos_num(bocpd_prior_mu + 1, "bocpd_prior_mu") - 1
      self$bocpd_prior_kappa <- .validate_pos_num(bocpd_prior_kappa, "bocpd_prior_kappa")
      self$bocpd_prior_alpha <- .validate_pos_num(bocpd_prior_alpha, "bocpd_prior_alpha")
      self$bocpd_prior_beta <- .validate_pos_num(bocpd_prior_beta, "bocpd_prior_beta")
      self$ml_use_gpu <- isTRUE(ml_use_gpu)
      invisible(self)
    },

    #' @description Validate plan configuration.
    validate = function() {
      super$validate()
      if (!self$chart_type %in% .SPC_CHART_TYPES) {
        stop("Unsupported chart_type: ", self$chart_type, call. = FALSE)
      }
      if (!self$sigma_method %in% .SPC_SIGMA_METHODS) {
        stop("Unsupported sigma_method: ", self$sigma_method, call. = FALSE)
      }
      invisible(self)
    },

    #' @description Export configuration as a list.
    to_list = function() {
      base_list <- super$to_list()
      base_list$chart_type <- self$chart_type
      base_list$sigma_method <- self$sigma_method
      base_list$subgroup_size <- self$subgroup_size
      base_list$subgroup <- self$subgroup
      base_list$nelson_rules <- self$nelson_rules
      base_list$lambda <- self$lambda
      base_list$k <- self$k
      base_list$h <- self$h
      base_list$ma_window <- self$ma_window
      base_list$phase <- self$phase
      base_list$phase_boundaries <- self$phase_boundaries
      base_list$target <- self$target
      base_list$window_size <- self$window_size
      base_list$arima_order <- self$arima_order
      base_list$aewma_lambda <- self$aewma_lambda
      base_list$aewma_k <- self$aewma_k
      base_list$cp_method <- self$cp_method
      base_list$cp_penalty <- self$cp_penalty
      base_list$kde_bandwidth <- self$kde_bandwidth
      # v0.6 ML enhancement fields
      base_list$lstm_units <- self$lstm_units
      base_list$lstm_window <- self$lstm_window
      base_list$lstm_epochs <- self$lstm_epochs
      base_list$lstm_batch_size <- self$lstm_batch_size
      base_list$lstm_threshold <- self$lstm_threshold
      base_list$ae_encoding_dim <- self$ae_encoding_dim
      base_list$ae_hidden_dim <- self$ae_hidden_dim
      base_list$ae_epochs <- self$ae_epochs
      base_list$ae_batch_size <- self$ae_batch_size
      base_list$ae_threshold_quantile <- self$ae_threshold_quantile
      base_list$iforest_ntree <- self$iforest_ntree
      base_list$iforest_sample_size <- self$iforest_sample_size
      base_list$iforest_threshold <- self$iforest_threshold
      base_list$bocpd_hazard <- self$bocpd_hazard
      base_list$bocpd_prior_mu <- self$bocpd_prior_mu
      base_list$bocpd_prior_kappa <- self$bocpd_prior_kappa
      base_list$bocpd_prior_alpha <- self$bocpd_prior_alpha
      base_list$bocpd_prior_beta <- self$bocpd_prior_beta
      base_list$ml_use_gpu <- self$ml_use_gpu
      base_list
    }
  )
)

# ---------------------------------------------------------------------------
# Internal lookup tables for SpcPlan
# ---------------------------------------------------------------------------

.SPC_CHART_TYPES <- c(
  # Variables charts
  "xbar_r", "xbar_s", "imr", "imr_rs",
  # Attributes charts
  "p", "np", "u", "c", "p_laney", "u_laney",
  # Time-weighted charts
  "ewma", "cusum", "ma",
  # Multivariate charts
  "t2", "mewma",
  # Rare-event charts
  "g", "t",
  # v0.2: Adaptive / statistical enhancement charts
  "adaptive", "arima_resid", "aewma", "changepoint", "kde", "t2_mewma",
  # v0.6: ML enhancement charts
  "lstm", "autoencoder", "iforest", "bocpd"
)

.SPC_SIGMA_METHODS <- c(
  "r_bar", "s_bar", "pooled_s", "mr_bar", "mr_median", "mssd", "total"
)

.default_sigma_method <- function(chart_type) {
  if (chart_type %in% c("imr", "imr_rs")) "mr_bar"
  else if (chart_type %in% c("xbar_r")) "r_bar"
  else if (chart_type %in% c("xbar_s")) "s_bar"
  else if (chart_type %in% c("ewma", "cusum", "ma")) "mr_bar"
  else if (chart_type %in% c("p", "np", "u", "c", "p_laney", "u_laney")) "total"
  else if (chart_type %in% c("g", "t")) "total"
  else if (chart_type %in% c("adaptive", "arima_resid", "aewma")) "mr_bar"
  else if (chart_type %in% c("changepoint", "kde", "t2_mewma")) "total"
  else if (chart_type %in% c("lstm", "autoencoder", "iforest", "bocpd")) "total"
  else "r_bar"
}

.default_subgroup_size <- function(chart_type) {
  if (chart_type %in% c("imr", "imr_rs", "p", "np", "u", "c",
                        "p_laney", "u_laney", "ewma", "cusum", "ma",
                        "g", "t", "adaptive", "arima_resid", "aewma",
                        "changepoint", "kde", "t2_mewma",
                        "lstm", "autoencoder", "iforest", "bocpd")) 1L
  else if (chart_type %in% c("xbar_r", "xbar_s")) 5L
  else if (chart_type %in% c("t2", "mewma")) 1L
  else 1L
}
