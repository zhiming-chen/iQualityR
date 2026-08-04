# =============================================================================
# File: R/Capability_multivariate.R
# Description: Multivariate process capability analysis. Implements the
#              Taam (1993) MCPV scalar family and the Shahriari-Hubele-
#              Lawrence (1995) HPCI vector. Uses mvtnorm for the
#              probability content of the spec hyper-rectangle, and a
#              regularized covariance inverse (SVD trick from iQualityR.spc)
#              for robust Mahalanobis distance. Fills a gap that even
#              Minitab and JMP do not fully cover.
# =============================================================================

# ---- helpers ---------------------------------------------------------------

# Regularized inverse of a covariance matrix using SVD. Mirrors the pattern
# used in iQualityR.spc::SpcAnalyzer (.run_t2). Guards against singular /
# near-singular cov matrices that arise when CTQs are highly correlated.
.regularized_cov_inv <- function(cov_mat) {
  sv <- svd(cov_mat)
  d_reg <- pmax(sv$d, max(sv$d) * 1e-8)
  cov_inv <- sv$v %*% diag(1 / d_reg, nrow = length(d_reg)) %*% t(sv$u)
  cov_inv
}

# Volume of a p-dimensional ellipsoid defined by the covariance matrix Sigma
# at probability level alpha. Formula:
#   V = (2*pi)^(p/2) / Gamma(p/2 + 1) * |Sigma|^(1/2) * chi2_{p, alpha}^(p/2)
# where Gamma is the gamma function and chi2_{p, alpha} is the alpha quantile
# of the chi-square distribution with p degrees of freedom (the squared
# Mahalanobis radius that contains `alpha` of the multivariate normal mass).
.ellipsoid_volume <- function(cov_mat, alpha = 0.9973) {
  p <- ncol(cov_mat)
  chi2_q <- stats::qchisq(alpha, df = p)
  det_sigma <- det(cov_mat)
  if (det_sigma <= 0) det_sigma <- .Machine$double.eps
  unit_ball_vol <- (2 * pi)^(p / 2) / gamma(p / 2 + 1)
  unit_ball_vol * sqrt(det_sigma) * chi2_q^(p / 2)
}

# Hotelling T^2 test for H0: mu == mu0 (target vector). Returns statistic,
# F-statistic, degrees of freedom, and p-value. Uses the standard Phase I
# (historical) T^2 form with the F distribution.
.hotelling_t2_test <- function(x, mu0, conf_level = 0.95) {
  n <- nrow(x)
  p <- ncol(x)
  if (n <= p) {
    return(list(statistic = NA_real_, F_stat = NA_real_,
                df1 = p, df2 = n - p, p_value = NA_real_,
                method = "Hotelling T^2 (n <= p, insufficient data)"))
  }
  x_bar <- colMeans(x)
  S <- stats::cov(x)
  diff <- x_bar - mu0
  S_inv <- .regularized_cov_inv(S)
  T2 <- as.numeric(n * t(diff) %*% S_inv %*% diff)
  # Transformation to F:
  F_stat <- (n - p) / (p * (n - 1)) * T2
  df1 <- p
  df2 <- n - p
  p_value <- 1 - stats::pf(F_stat, df1, df2)
  list(statistic = T2, F_stat = F_stat, df1 = df1, df2 = df2,
       p_value = p_value, method = "Hotelling T^2 (Phase I, F-transformed)",
       x_bar = x_bar)
}

# =============================================================================
# MultivariateCapabilityAnalyzer
# =============================================================================

#' MultivariateCapabilityAnalyzer
#'
#' @title MultivariateCapabilityAnalyzer
#'
#' @description Multivariate process capability analyzer. Computes the
#'   Taam (1993) MCPV scalar family and the Shahriari-Hubele-Lawrence
#'   (1995) HPCI three-component vector for a set of correlated CTQs
#'   (critical-to-quality characteristics). Uses `mvtnorm::pmvnorm` for
#'   the probability content of the spec hyper-rectangle and a regularized
#'   covariance inverse (SVD) for robust Mahalanobis distances.
#'
#'   This implementation fills a gap that even Minitab v21 and JMP v19 do
#'   not fully cover — neither ships dedicated MCPV / HPCI indices.
#'
#' @param X Numeric matrix or data.frame of CTQ observations (n x p).
#' @param lsl_vec Numeric vector of lower spec limits (length p).
#' @param usl_vec Numeric vector of upper spec limits (length p).
#' @param target_vec Optional numeric vector of target values (length p).
#' @param plan A `MultivarCapabilityPlan` object.
#'
#' @export
MultivariateCapabilityAnalyzer <- R6::R6Class("MultivariateCapabilityAnalyzer",
  inherit = IqrAnalyzerBase,
  public = list(

    #' @description Run multivariate capability analysis
    #' @param X Numeric matrix or data.frame of CTQs (n x p).
    #' @param lsl_vec Numeric vector (length p) of lower specs.
    #' @param usl_vec Numeric vector (length p) of upper specs.
    #' @param target_vec Optional target vector (length p).
    #' @param plan [MultivarCapabilityPlan] object.
    run = function(X, lsl_vec, usl_vec, target_vec = NULL, plan) {
      X <- as.data.frame(X)
      # Coerce to numeric, NA-mask row-wise
      for (col in names(X)) {
        X[[col]] <- suppressWarnings(as.numeric(X[[col]]))
      }
      keep <- stats::complete.cases(X)
      if (any(!keep)) {
        warning(sprintf("%d rows with missing values removed.",
                        sum(!keep)), call. = FALSE)
      }
      X <- X[keep, , drop = FALSE]
      X <- as.matrix(X)

      n <- nrow(X)
      p <- ncol(X)
      if (n < p + 1) {
        stop(sprintf("Multivariate capability requires n > p (got n=%d, p=%d).",
                     n, p), call. = FALSE)
      }
      if (length(lsl_vec) != p || length(usl_vec) != p) {
        stop("lsl_vec and usl_vec must have length p (= ncol(X)).", call. = FALSE)
      }
      if (any(lsl_vec >= usl_vec)) {
        stop("Each LSL must be strictly less than its USL.", call. = FALSE)
      }
      if (!is.null(target_vec) && length(target_vec) != p) {
        stop("target_vec must have length p or be NULL.", call. = FALSE)
      }
      storage.mode(X) <- "double"

      self$.compute_multivariate(X, lsl_vec, usl_vec, target_vec, plan)
      invisible(self)
    },

    .compute_multivariate = function(X, lsl_vec, usl_vec, target_vec, plan) {
      n <- nrow(X); p <- ncol(X)
      ctq_names <- colnames(X)
      if (is.null(ctq_names)) {
        ctq_names <- paste0("CTQ", seq_len(p))
      }

      # ---- 1. Parameter estimates -------------------------------------
      x_bar <- colMeans(X)
      S <- stats::cov(X)
      S_inv <- .regularized_cov_inv(S)
      det_S <- det(S)
      if (det_S <= 0) det_S <- .Machine$double.eps

      # ---- 2. Volume of the 99.73% process ellipsoid ------------------
      alpha <- 0.9973   # matches normal ±3 sigma
      chi2_q <- stats::qchisq(alpha, df = p)
      unit_ball_vol <- (2 * pi)^(p / 2) / gamma(p / 2 + 1)
      V_process <- unit_ball_vol * sqrt(det_S) * chi2_q^(p / 2)

      # ---- 3. Volume of the spec hyper-rectangle ----------------------
      V_spec <- prod(usl_vec - lsl_vec)

      # ---- 4. MCPV (Taam 1993) ----------------------------------------
      # Original Taam MCPV modifies the spec region by intersecting it with
      # the 99.73% ellipsoid centred at the target, then takes the volume
      # ratio. The widely-used scalar simplification is:
      #   C_pm,M = V_spec / V_process   (analog of Cp)
      #   C_pmk,M = C_pm,M * (1 - |mu - T|_Sigma / |spec_radius|)  (analog of Cpk)
      # We use both: the volume ratio (Cp-equivalent) and a centering-
      # penalised version (Cpk-equivalent). If no target is given, the
      # midpoint of the spec rectangle is used as the implied target.
      if (is.null(target_vec)) {
        T_vec <- (lsl_vec + usl_vec) / 2
        target_supplied <- FALSE
      } else {
        T_vec <- target_vec
        target_supplied <- TRUE
      }

      mcpv_p <- V_spec / V_process

      # Centering penalty: Mahalanobis distance from process mean to target,
      # normalised by the chi-square 99.73% radius (so penalty in [0, 1]).
      diff_T <- x_bar - T_vec
      d2_T <- as.numeric(t(diff_T) %*% S_inv %*% diff_T)
      r_max <- chi2_q   # squared Mahalanobis radius of the 99.73% ellipsoid
      centering_penalty <- min(1, d2_T / r_max)  # 0 = perfectly centred, 1 = on edge
      mcpv_pk <- mcpv_p * (1 - centering_penalty)

      # ---- 5. HPCI (Shahriari, Hubele, Lawrence 1995) ----------------
      # 3-component vector (npc, pv, lri):
      #   1. npc = V_spec / V_process   (volume ratio; same as MCPV)
      #   2. pv  = p-value from Hotelling T^2 test of H0: mu == T
      #   3. lri = location ratio index: for each CTQ, indicator that
      #            (mean - LSL) / 3*sigma_within and (USL - mean) / 3*sigma_within
      #            are >= some threshold (we use 1, the Cpk >= 1 criterion).
      #            Product over CTQs.
      if (target_supplied) {
        t2 <- .hotelling_t2_test(X, mu0 = T_vec, conf_level = plan$conf_level)
      } else {
        # No target supplied: test against spec midpoint (weaker but useful).
        t2 <- .hotelling_t2_test(X, mu0 = T_vec, conf_level = plan$conf_level)
      }
      npc <- V_spec / V_process
      pv  <- t2$p_value

      # Location ratio index (LRI): for each CTQ, compute the univariate
      # "centering ratio" min((USL - mu)/(USL - LSL), (mu - LSL)/(USL - LSL)).
      # If > 0.5 the mean is in the inner half of the spec band (centered);
      # otherwise penalised. The HPCI LRI is the product over CTQs.
      sigma_j <- sqrt(diag(S))
      lri_per_ctq <- pmin((usl_vec - x_bar) / (usl_vec - lsl_vec),
                          (x_bar - lsl_vec) / (usl_vec - lsl_vec))
      lri <- prod(pmin(1, pmax(0, lri_per_ctq * 2)))   # in [0, 1]

      # Overall HPCI verdict (Shahriari convention, with practical thresholds):
      #   pass  -> all three components pass
      #   watch -> volume OK but centering or location off (fixable)
      #   fail  -> volume fails (process spread exceeds spec: not fixable
      #            by simple adjustment; needs variance reduction)
      hpci_pass_volume <- npc >= 1
      hpci_pass_center <- !is.na(pv) && pv >= (1 - plan$conf_level)
      hpci_pass_location <- lri >= 0.9
      hpci_overall <- if (hpci_pass_volume && hpci_pass_center && hpci_pass_location) {
                        "pass"
                      } else if (hpci_pass_volume) {
                        # Volume is OK but centering/location is off
                        "watch"
                      } else {
                        # Volume fails -> fundamentally incapable
                        "fail"
                      }

      # ---- 6. Probability content of spec rectangle ------------------
      # P(X in spec rectangle) under multivariate normal with mean x_bar and
      # cov S. This is the multivariate analog of "expected in-spec yield".
      # Use mvtnorm::pmvnorm for the integration.
      yield_prob <- NA_real_
      ppm_expected <- NA_real_
      tryCatch({
        pmv <- mvtnorm::pmvnorm(
          lower = lsl_vec, upper = usl_vec,
          mean = x_bar, sigma = S,
          keepAttr = FALSE
        )
        yield_prob <- as.numeric(pmv)
        ppm_expected <- (1 - yield_prob) * 1e6
      }, error = function(e) {
        warning(sprintf("mvtnorm::pmvnorm failed: %s", conditionMessage(e)),
                call. = FALSE)
      })

      # ---- 7. Per-CTQ univariate summary (for the table) -------------
      per_ctq <- data.frame(
        CTQ = ctq_names,
        LSL = lsl_vec,
        USL = usl_vec,
        Target = T_vec,
        Mean = x_bar,
        SD = sigma_j,
        Cpu = (usl_vec - x_bar) / (3 * sigma_j),
        Cpl = (x_bar - lsl_vec) / (3 * sigma_j),
        stringsAsFactors = FALSE
      )
      per_ctq$Cpk <- pmin(per_ctq$Cpu, per_ctq$Cpl)
      per_ctq$In_Spec <- mapply(function(col, l, u) {
        sum(X[, col] >= l & X[, col] <= u) / n
      }, seq_len(p), lsl_vec, usl_vec)

      # ---- 8. Mahalanobis-distance diagnostics (for stability) -------
      d2 <- apply(X, 1, function(row) {
        as.numeric(t(row - x_bar) %*% S_inv %*% (row - x_bar))
      })
      # Chi-square Q-Q data (multivariate normality check via Mahalanobis d^2)
      qq_df <- data.frame(
        d2 = sort(d2),
        theoretical = stats::qchisq((seq_len(n) - 0.5) / n, df = p)
      )

      # ---- 9. Verdict --------------------------------------------------
      verdict <- list(
        mcpv_p = mcpv_p,
        mcpv_pk = mcpv_pk,
        npc = npc,
        pv = pv,
        lri = lri,
        hpci_overall = hpci_overall,
        hpci_pass_volume = hpci_pass_volume,
        hpci_pass_center = hpci_pass_center,
        hpci_pass_location = hpci_pass_location,
        centering_penalty = centering_penalty,
        yield_prob = yield_prob,
        ppm_expected = ppm_expected,
        overall_verdict = hpci_overall
      )

      # ---- 10. Populate standardized result container -----------------
      self$reset()
      self$set_statistic("n", n)
      self$set_statistic("p", p)
      self$set_statistic("ctq_names", ctq_names)
      self$set_statistic("mcpv_p", mcpv_p)
      self$set_statistic("mcpv_pk", mcpv_pk)
      self$set_statistic("npc", npc)
      self$set_statistic("lri", lri)
      self$set_statistic("pv", pv)
      self$set_statistic("hpci_overall", hpci_overall)
      self$set_statistic("det_S", det_S)
      self$set_statistic("V_spec", V_spec)
      self$set_statistic("V_process", V_process)
      self$set_statistic("yield_prob", yield_prob)
      self$set_statistic("ppm_expected", ppm_expected)
      self$set_statistic("chi2_q_9973", chi2_q)
      self$set_statistic("d2_mean_to_target", d2_T)
      self$set_statistic("centering_penalty", centering_penalty)
      self$set_statistic("mean_vector", x_bar)
      self$set_statistic("cov_matrix", S)
      self$set_diagnostic("hotelling_t2", t2)
      self$set_diagnostic("capability_judgment", verdict)
      self$set_diagnostic("target_supplied", target_supplied)

      warnings <- character()
      if (n < 50) {
        warnings <- c(warnings,
          sprintf("Sample size n = %d is small for multivariate capability; the covariance matrix is unstable. Aim for n >= 50 (rule of thumb: n >= 10*p).", n))
      }
      if (n < 10 * p) {
        warnings <- c(warnings,
          sprintf("n = %d < 10*p = %d; covariance estimation is unreliable.", n, 10 * p))
      }
      if (is.na(yield_prob)) {
        warnings <- c(warnings,
          "mvtnorm::pmvnorm failed; expected PPM is NA. Check that S is positive-definite.")
      }
      if (det_S <= .Machine$double.eps * 10) {
        warnings <- c(warnings,
          "Covariance matrix is near-singular; CTQs may be collinear. Consider dropping redundant CTQs or using PCA.")
      }
      if (!target_supplied) {
        warnings <- c(warnings,
          "No target vector supplied; using the spec midpoint as the target. Provide an explicit target for a defensible centering test.")
      }
      if (length(warnings) > 0) self$set_diagnostic("warnings", warnings)

      # Data tables (consumed by plotter + reporter)
      self$set_datatable("per_ctq", per_ctq)
      self$set_datatable("raw_data", as.data.frame(X))
      self$set_datatable("mahalanobis_qq", qq_df)
      self$set_datatable("mahalanobis_d2", data.frame(index = seq_len(n), d2 = d2))
      self$set_datatable("spec_region", data.frame(
        CTQ = ctq_names, LSL = lsl_vec, USL = usl_vec,
        Target = T_vec, stringsAsFactors = FALSE))
      # For 2D / 3D plotting convenience:
      self$set_datatable("X_matrix", as.data.frame(X))

      invisible(self)
    }
  )
)
