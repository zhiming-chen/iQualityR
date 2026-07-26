# =============================================================================
# File: R/BayesianDOE.R
# Description: Bayesian Optimization for Experimental Design
# =============================================================================

#' @title BayesianOptimizer: Bayesian Optimization for Experimental Design
#' @description
#'   An R6 class that implements Bayesian optimization for sequential
#'   experimental design. Uses a Gaussian Process (GP) surrogate model
#'   to approximate an expensive black-box response function, and an
#'   Expected Improvement (EI) acquisition function to guide the search
#'   for optimal experimental conditions.
#'
#'   **Kernels supported**:
#'   - `"rbf"`: Radial Basis Function (squared exponential) kernel
#'   - `"matern"`: Matern 5/2 kernel (less smooth, more flexible)
#'
#'   **Fallback behaviour**:
#'   When `kernlab` is not installed the optimizer falls back to a
#'   simple linear model with polynomial features (`lm`). Predictions
#'   still work but uncertainty estimates are approximated from
#'   leave-one-out residuals.
#'
#' @field kernel Name of the covariance kernel in use
#' @field gp_model Fitted Gaussian Process model (or lm fallback)
#' @field X Training input matrix
#' @field y Training response vector
#' @field loo_residuals Leave-one-out residuals for SE approximation
#'
#' @importFrom R6 R6Class
#' @export
BayesianOptimizer <- R6::R6Class(
  "BayesianOptimizer",

  public = list(

    #' @field kernel Kernel name ("rbf" or "matern")
    kernel = NULL,

    #' @field gp_model Fitted surrogate model
    gp_model = NULL,

    #' @field X Training input data (matrix / data frame)
    X = NULL,

    #' @field y Training response values
    y = NULL,

    #' @field loo_residuals LOO residuals for SE fallback
    loo_residuals = NULL,

    # -------------------------------------------------------------------------
    # Constructor
    # -------------------------------------------------------------------------

    #' @description Create a new BayesianOptimizer
    #' @param kernel Character. Covariance kernel for the GP. One of
    #'   `"rbf"` (default) or `"matern"`.
    #' @return A `BayesianOptimizer` instance
    initialize = function(kernel = "rbf") {
      valid_kernels <- c("rbf", "matern")
      if (!kernel %in% valid_kernels) {
        stop(
          "[iQualityR] Invalid kernel '", kernel,
          "'. Choose from: ", paste(valid_kernels, collapse = ", "),
          call. = FALSE
        )
      }
      self$kernel <- kernel
      invisible(self)
    },

    # -------------------------------------------------------------------------
    # Fit surrogate model
    # -------------------------------------------------------------------------

    #' @description Fit the GP surrogate model to observed data.
    #' @param X Data frame or matrix of factor settings (n x p).
    #' @param y Numeric vector of responses (length n).
    #' @return Self, for chaining.
    fit = function(X, y) {
      if (!is.numeric(y)) {
        stop("[iQualityR] y must be a numeric vector.", call. = FALSE)
      }
      if (nrow(as.matrix(X)) != length(y)) {
        stop(
          "[iQualityR] nrow(X) must equal length(y).",
          call. = FALSE
        )
      }

      self$X <- as.matrix(X)
      self$y <- as.numeric(y)

      has_kernlab <- requireNamespace("kernlab", quietly = TRUE)

      if (has_kernlab) {
        self$gp_model <- private$.fit_gp(self$X, self$y)
      } else {
        message(
          "[iQualityR] kernlab not available; falling back to lm surrogate."
        )
        self$gp_model <- private$.fit_lm(self$X, self$y)
      }

      # Compute LOO residuals for SE approximation
      self$loo_residuals <- private$.loo_residuals(self$X, self$y)

      invisible(self)
    },

    # -------------------------------------------------------------------------
    # Predict
    # -------------------------------------------------------------------------

    #' @description Predict response and standard error at new points.
    #' @param X_new Data frame or matrix of new factor settings (m x p).
    #' @return A list with elements `predict` (numeric vector) and
    #'   `se.fit` (numeric vector of standard errors).
    predict = function(X_new) {
      if (is.null(self$gp_model)) {
        stop(
          "[iQualityR] Model not fitted yet. Call fit() first.",
          call. = FALSE
        )
      }

      X_new <- as.matrix(X_new)
      has_kernlab <- requireNamespace("kernlab", quietly = TRUE)

      if (has_kernlab && inherits(self$gp_model, "gausspr")) {
        preds <- private$.predict_gp(self$gp_model, X_new, self$X, self$y)
      } else {
        preds <- private$.predict_lm(self$gp_model, X_new, self$loo_residuals)
      }

      return(list(predict = preds$mean, se.fit = preds$se))
    },

    # -------------------------------------------------------------------------
    # Expected Improvement
    # -------------------------------------------------------------------------

    #' @description Compute the Expected Improvement acquisition function.
    #'   For minimisation: EI(x) = (y_best - mu) * Phi(z) + sigma * phi(z)
    #'   where z = (y_best - mu) / sigma.
    #' @param X_new Data frame or matrix of candidate points.
    #' @param y_best Numeric. Current best (minimum) response observed.
    #' @return Numeric vector of EI values (non-negative).
    expected_improvement = function(X_new, y_best) {
      preds <- self$predict(X_new)
      mu <- preds$predict
      # Clamp zero/negative SE to machine epsilon to avoid division by zero
      # when computing z = (y_best - mu) / sigma downstream.
      sigma <- pmax(preds$se.fit, .Machine$double.eps)

      z <- (y_best - mu) / sigma
      ei <- (y_best - mu) * stats::pnorm(z) + sigma * stats::dnorm(z)
      ei[ei < 0] <- 0
      return(ei)
    },

    # -------------------------------------------------------------------------
    # Suggest next point (optimise acquisition function)
    # -------------------------------------------------------------------------

    #' @description Optimise the EI acquisition function over the search
    #'   space to find the next most promising experimental condition.
    #'   Renamed from `optimize()` to avoid shadowing `base::optimize`.
    #' @param bounds List with `lower` and `upper` numeric vectors defining
    #'   the per-dimension search bounds.
    #' @param n_starts Integer. Number of random starting points for the
    #'   local optimiser (default 20).
    #' @param seed Integer or NULL. Random seed for reproducible multi-start
    #'   optimisation. Passed to `withr::local_seed()`. When `NULL` the
    #'   global RNG state is used as-is (default).
    #' @return A list with elements `point` (named numeric vector of the
    #'   recommended next trial point) and `ei` (the achieved EI value).
    suggest_next = function(bounds, n_starts = 20, seed = NULL) {
      withr::local_seed(seed)

      p <- length(bounds$lower)

      if (length(bounds$upper) != p) {
        stop("[iQualityR] bounds$lower and $upper must have same length.",
             call. = FALSE)
      }

      y_best <- min(self$y)

      # EI wrapper taking a flat numeric vector
      ei_scalar <- function(par) {
        xmat <- matrix(par, nrow = 1)
        colnames(xmat) <- colnames(self$X)
        -as.numeric(self$expected_improvement(xmat, y_best))
      }

      best_val <- Inf
      best_par <- runif(p, bounds$lower, bounds$upper)  # default random start

      # Multi-start L-BFGS-B
      for (i in seq_len(n_starts)) {
        start <- runif(p, bounds$lower, bounds$upper)
        res <- tryCatch(
          stats::optim(
            par = start, fn = ei_scalar,
            method = "L-BFGS-B",
            lower = bounds$lower, upper = bounds$upper,
            control = list(maxit = 500)
          ),
          error = function(e) list(value = Inf, par = start)
        )
        if (!is.null(res$value) && res$value < best_val) {
          best_val <- res$value
          best_par <- res$par
        }
      }

      result <- best_par
      if (!is.null(colnames(self$X))) {
        names(result) <- colnames(self$X)
      }
      return(list(point = result, ei = -best_val))
    },

    # -------------------------------------------------------------------------
    # Deprecated alias: optimize() -> suggest_next()
    # -------------------------------------------------------------------------
    #
    # The original public method was named `optimize()`. It was renamed to
    # `suggest_next()` to avoid shadowing `base::optimize()` (which would
    # break common R workflows that rely on the base generic). The rename
    # was made silently in an earlier release, which broke downstream code
    # that still called `$optimize()`. We now re-introduce `optimize()` as
    # a thin deprecation wrapper that:
    #   1. Emits a clear `.Deprecated()` warning so users see the migration
    #      path in their console / log output.
    #   2. Forwards every argument to `suggest_next()` unchanged so
    #      existing scripts continue to work.
    # The wrapper will be removed in a future major release.
    #' @description Deprecated alias for [suggest_next()]. `optimize()` was
    #'   renamed to `suggest_next()` to avoid shadowing `base::optimize()`.
    #'   Existing scripts that call `$optimize()` continue to work but emit
    #'   a deprecation warning; please migrate to `$suggest_next()`.
    #' @param bounds List with `lower` and `upper` numeric vectors defining
    #'   the per-dimension search bounds.
    #' @param n_starts Integer. Number of random starting points for the
    #'   local optimiser (default 20).
    #' @param seed Integer or NULL. Random seed for reproducible multi-start
    #'   optimisation.
    #' @return A list with elements `point` and `ei` (see [suggest_next()]).
    optimize = function(bounds, n_starts = 20, seed = NULL) {
      warning("[iQualityR] BayesianOptimizer$optimize() is deprecated; ",
              "use $suggest_next() instead. ",
              "$optimize() will be removed in a future release.",
              call. = FALSE)
      self$suggest_next(bounds = bounds, n_starts = n_starts, seed = seed)
    },

    # -------------------------------------------------------------------------
    # Sequential Bayesian optimisation loop
    # -------------------------------------------------------------------------

    #' @description Run the full Bayesian optimisation loop: start from an
    #'   initial Latin Hypercube (or any) design, sequentially propose new
    #'   points via EI maximisation, evaluate the response, and update the
    #'   surrogate model.
    #' @param initial_design Data frame of initial experimental runs (n x p).
    #' @param response_function Function that accepts a single-row data frame
    #'   and returns a numeric response (the black-box to optimise).
    #' @param n_iterations Integer. Number of sequential iterations after the
    #'   initial design (default 20).
    #' @param seed Integer or NULL. Random seed for reproducible sequential
    #'   optimisation. Passed to `withr::local_seed()` so the whole run is
    #'   reproducible (default `NULL`). The seed is set once at the start so
    #'   that RNG state flows naturally across iterations.
    #' @return A list with:
    #'   \describe{
    #'     \item{history}{Data frame of all evaluated points and responses.}
    #'     \item{best_y}{Best response found.}
    #'     \item{best_x}{Parameter settings of the best response.}
    #'     \item{ei_trace}{Vector of max EI values per iteration.}
    #'   }
    run_sequential = function(initial_design, response_function,
                              n_iterations = 20, seed = NULL) {
      withr::local_seed(seed)

      X_all <- as.data.frame(initial_design)
      y_all <- numeric(nrow(X_all))

      message("[iQualityR] === Bayesian Optimization Loop ===")
      message("[iQualityR] Initial design: ", nrow(X_all), " runs")

      # Evaluate initial design
      for (i in seq_len(nrow(X_all))) {
        y_all[i] <- response_function(X_all[i, , drop = FALSE])
      }

      # Fit initial model
      self$fit(X_all, y_all)

      bounds <- list(
        lower = sapply(X_all, min),
        upper = sapply(X_all, max)
      )

      ei_trace <- numeric(n_iterations)

      for (iter in seq_len(n_iterations)) {
        # Propose next point. seed is intentionally left NULL so the RNG
        # stream set above flows across iterations (re-seeding each call
        # would repeat the same starting points every iteration).
        next_res <- self$suggest_next(bounds, n_starts = 20)
        next_x <- next_res$point  # named numeric vector

        # Evaluate response; wrapped in tryCatch for robustness against a
        # failing user-supplied black-box function.
        next_y <- tryCatch(
          response_function(as.data.frame(t(next_x))),
          error = function(e) {
            warning(
              "[iQualityR] response_function failed at iteration ", iter,
              ": ", conditionMessage(e),
              call. = FALSE
            )
            NA_real_
          }
        )

        # Append
        X_all <- rbind(X_all, as.data.frame(t(next_x)))
        y_all <- c(y_all, next_y)

        # Refit
        self$fit(as.matrix(X_all), y_all)

        # Record
        current_best <- min(y_all)
        ei_trace[iter] <- max(self$expected_improvement(
          as.matrix(X_all[nrow(X_all), , drop = FALSE]), current_best
        ))

        message(
          sprintf(
            "[iQualityR] Iter %02d: y = %.4f | best = %.4f | EI = %.4f",
            iter, next_y, current_best, ei_trace[iter]
          )
        )
      }

      best_idx <- which.min(y_all)
      history <- cbind(X_all, response = y_all)

      list(
        history  = as.data.frame(history),
        best_y   = y_all[best_idx],
        best_x   = X_all[best_idx, , drop = FALSE],
        ei_trace = ei_trace
      )
    }
  ),

  # ==========================================================================
  # Private helpers
  # ==========================================================================

  private = list(

    # --- GP fit via kernlab ---------------------------------------------------

    .fit_gp = function(X, y) {
      kern <- kernlab::rbfdot()
      model <- kernlab::gausspr(
        x = X, y = y,
        kernel = kern,
        scaled = TRUE
      )
      return(model)
    },

    # --- GP predict -----------------------------------------------------------

    .predict_gp = function(model, X_new, X_train, y_train) {
      # Point prediction from gausspr
      mu <- tryCatch(
        as.numeric(kernlab::predict(model, X_new)),
        error = function(e) {
          warning("GP predict failed, using mean: ", e$message)
          rep(mean(y_train), nrow(X_new))
        }
      )

      # Approximate predictive SE using distance-weighted LOO residuals.
      loo_res <- private$.loo_residuals(X_train, y_train)
      sigma_loo <- sd(loo_res)

      # Shrink SE where training points are nearby
      n <- nrow(X_new)
      se <- numeric(n)
      for (i in seq_len(n)) {
        dists <- sqrt(rowSums((t(t(X_train) - X_new[i, ]))^2))
        w <- exp(-dists / (2 * median(dists) + .Machine$double.eps))
        w <- w / (sum(w) + .Machine$double.eps)
        se[i] <- sigma_loo * sqrt(sum(w^2))
      }
      se <- pmax(se, .Machine$double.eps)

      list(mean = mu, se = se)
    },

    # --- LM fallback fit -----------------------------------------------------

    .fit_lm = function(X, y) {
      # Build polynomial design matrix up to degree 2
      df <- as.data.frame(X)
      fo <- stats::as.formula(
        paste("y ~ (.)^2 +", paste(paste0("I(", names(df), "^2)"),
                                   collapse = " + "))
      )
      df$y <- y
      model <- stats::lm(fo, data = df)
      return(model)
    },

    # --- LM fallback predict --------------------------------------------------

    .predict_lm = function(model, X_new, loo_residuals) {
      df_new <- as.data.frame(X_new)
      # Attach dummy y for model.matrix compatibility
      df_new$y <- 0
      mm <- stats::model.matrix(model, data = df_new)
      mu <- as.numeric(mm %*% stats::coef(model))
      sigma_approx <- sd(loo_residuals)
      se <- rep(sigma_approx, nrow(X_new))
      list(mean = mu, se = se)
    },

    # --- Leave-one-out residuals ----------------------------------------------

    .loo_residuals = function(X, y) {
      n <- length(y)
      residuals <- numeric(n)
      has_kernlab <- requireNamespace("kernlab", quietly = TRUE)

      for (i in seq_len(n)) {
        idx <- seq_len(n)[-i]
        if (n <= 2) {
          residuals[i] <- 0
          next
        }
        X_sub <- X[idx, , drop = FALSE]
        y_sub <- y[idx]
        X_i <- X[i, , drop = FALSE]

        if (has_kernlab) {
          kern <- if (self$kernel == "rbf") kernlab::rbfdot() else
            kernlab::materndot()
          m <- tryCatch(
            kernlab::gausspr(x = X_sub, y = y_sub,
                             kernel = kern, scaled = TRUE),
            error = function(e) NULL
          )
          if (!is.null(m)) {
            pred <- tryCatch(
              kernlab::predict(m, X_i),
              error = function(e) NA
            )
            if (!is.na(pred)) {
              residuals[i] <- y[i] - as.numeric(pred)
            } else {
              residuals[i] <- 0
            }
          } else {
            residuals[i] <- 0
          }
        } else {
          df_sub <- as.data.frame(X_sub); df_sub$y <- y_sub
          fo <- stats::as.formula(
            paste("y ~ (.)^2 +",
                  paste(paste0("I(", names(as.data.frame(X)), "^2)"),
                        collapse = " + "))
          )
          m <- tryCatch(stats::lm(fo, data = df_sub),
                        error = function(e) NULL)
          if (!is.null(m)) {
            df_i <- as.data.frame(X_i); df_i$y <- 0
            residuals[i] <- y[i] - stats::predict.lm(m, newdata = df_i)
          } else {
            residuals[i] <- 0
          }
        }
      }
      return(residuals)
    }
  )
)
