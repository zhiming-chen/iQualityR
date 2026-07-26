# =============================================================================
# File: R/type1_functions.R
# Description: Quick Entry Functions for Type1 Gage Study
# =============================================================================

#' @title Quick Single Reference Type1 Gage Study
#' @description
#' Quick entry for single reference value bias study
#'
#' @param data Numeric vector or data.frame with measurements
#' @param reference_value Numeric reference value
#' @param lsl Lower specification limit
#' @param usl Upper specification limit
#' @param tolerance Numeric, tolerance directly (T). If given, overrides lsl/usl.
#' @param natural_zero Logical, if TRUE and `usl` is given, tolerance = usl - 0.
#' @param k_factor Multiplier for Cg/Cgk (default 0.2)
#' @param study_multiplier Study variation multiplier (default 6). Minitab
#'   historically supports 6 (modern default) and 5.15 (AIAG MSA 3rd ed.).
#'   SV = study_multiplier * sigma; affects Cg, Cgk, %Var(Repeatability).
#' @param alternative Direction of bias t-test alternative hypothesis:
#'   `"two.sided"` (default), `"greater"`, or `"less"`.
#' @param historical_sd Numeric, known/historical sigma. When supplied, used
#'   for capability indices and bias test switches to z-test.
#' @param resolution Measurement resolution for VDA5
#' @param u_cal Calibration uncertainty for VDA5
#' @param u_lin Linearity uncertainty for VDA5 (default 0)
#' @param u_rest List of other uncertainties
#' @param conf_level Confidence level
#' @param theme Theme name
#'
#' @return Type1Task object
#' @export
iqr_type1_bias <- function(data, reference_value, lsl = NULL, usl = NULL,
                           tolerance = NULL, natural_zero = FALSE,
                           k_factor = 0.2,
                           study_multiplier = 6,
                           alternative = c("two.sided", "greater", "less"),
                           historical_sd = NULL,
                           resolution = NULL,
                           u_cal = 0,
                           u_lin = 0,
                           u_rest = list(),
                           conf_level = 0.95,
                           theme = "academic") {
  alternative <- match.arg(alternative)
  # Process data
  if (is.vector(data)) {
    dt <- data.frame(measurement = data)
  } else if (is.data.frame(data)) {
    dt <- data
  } else {
    stop("data must be a numeric vector or data.frame", call. = FALSE)
  }

  # Create and run task
  task <- Type1Task$new(dt, study_type = "bias", theme = theme)
  task$compute(
    reference_value = reference_value,
    spec_limits = list(lsl = lsl, usl = usl),
    tolerance = tolerance,
    natural_zero = natural_zero,
    k_factor = k_factor,
    study_multiplier = study_multiplier,
    alternative = alternative,
    historical_sd = historical_sd,
    resolution = resolution,
    u_cal = u_cal,
    u_lin = u_lin,
    u_rest = u_rest,
    conf_level = conf_level
  )
  return(task)
}

#' @title Quick Gage Linearity & Bias Study
#' @description
#' Quick entry for multiple reference value linearity & bias study.
#'
#' Process Variation (PV) is required by Minitab's Gage Linearity and Bias
#' Study for Linearity = |slope|*PV and %Linearity/%Bias calculations.
#' `process_variation` accepts:
#'   - numeric scalar (typically 6*sigma from a capability study)
#'   - `"from_study"`: use 6*s_pooled(within-reference) with a warning
#'   - `"from_historical_sigma"`: use 6*historical_sd (requires `historical_sd`)
#'   - `NULL`: degrades to `"from_study"` with a warning (E3 degradation
#'     strategy - do not hard-stop the analysis)
#'
#' @param data Data.frame with 'reference' and 'measurement' columns (long
#'   format) OR a data.frame/matrix whose columns are reference values (wide
#'   format). Wide format is auto-converted to long via `data.table::melt()`.
#' @param reference_values Numeric vector of reference values (optional, auto-detected)
#' @param lsl Lower specification limit
#' @param usl Upper specification limit
#' @param tolerance Numeric, tolerance directly (T). If given, overrides lsl/usl.
#' @param natural_zero Logical, if TRUE and `usl` is given, tolerance = usl - 0.
#' @param process_variation Numeric, process variation (PV) for linearity
#'   calculations. See description for accepted values.
#' @param historical_sd Numeric, historical sigma for
#'   `process_variation = "from_historical_sigma"` mode.
#' @param resolution Numeric, measurement system resolution for VDA5 (optional)
#' @param u_cal Numeric, calibration uncertainty for VDA5 (default 0)
#' @param u_lin Numeric, linearity uncertainty for VDA5. Default NULL lets the
#'   analyzer compute u_LIN = |Linearity|/sqrt(3) per VDA 5 §5.3.4.
#' @param u_rest List, additional uncertainty factors for VDA5 (optional)
#' @param linearity_corrected Logical, if TRUE the gage's linearity is software
#'   compensated in production and u_LIN falls back to s_regression (VDA 5 §5.3.4).
#' @param conf_level Confidence level
#' @param theme Theme name
#'
#' @return Type1Task object
#' @export
iqr_linearity_bias <- function(data, reference_values = NULL,
                               lsl = NULL, usl = NULL,
                               tolerance = NULL, natural_zero = FALSE,
                               process_variation = "from_study",
                               historical_sd = NULL,
                               resolution = NULL,
                               u_cal = 0,
                               u_lin = NULL,
                               u_rest = list(),
                               linearity_corrected = FALSE,
                               conf_level = 0.95,
                               theme = "academic") {

  # --- Wide format auto-conversion (C-L-7) -------------------------------
  # If data has no 'reference'/'measurement' columns but is a data.frame/matrix
  # with numeric columns, treat each column as a reference value (wide format).
  if (!"reference" %in% names(data) || !"measurement" %in% names(data)) {
    if (is.matrix(data)) data <- as.data.frame(data)
    if (is.data.frame(data)) {
      # Heuristic: all columns numeric and column names parse as numbers.
      # data.frame() mangles names like "2" into "X2" by default, so strip
      # a leading "X" before parsing.
      colnm <- names(data)
      colnm_stripped <- sub("^X", "", colnm)
      col_refs <- suppressWarnings(as.numeric(colnm_stripped))
      all_numeric <- all(vapply(data, is.numeric, logical(1)))
      if (all_numeric && !anyNA(col_refs)) {
        # Wide -> long via data.table::melt
        long_dt <- data.table::as.data.table(data)
        long_dt[, row_idx := .I]
        long_dt <- data.table::melt(long_dt, id.vars = "row_idx",
                                    variable.name = "reference_col",
                                    value.name = "measurement")
        long_dt[, reference := as.numeric(sub("^X", "", as.character(reference_col)))]
        data <- long_dt[, .(reference, measurement)]
        if (is.null(reference_values)) {
          reference_values <- sort(unique(data$reference))
        }
        message("[iqr_linearity_bias] Wide-format data detected; converted to ",
                "long format with reference values: ",
                paste(reference_values, collapse = ", "), ".")
      } else {
        stop("data must have 'reference' and 'measurement' columns (long format) ",
             "or be a wide-format data.frame/matrix with numeric column names ",
             "that parse as reference values.", call. = FALSE)
      }
    } else {
      stop("data must have 'reference' and 'measurement' columns", call. = FALSE)
    }
  }

  # Auto-detect reference values if not provided
  if (is.null(reference_values)) {
    reference_values <- sort(unique(data$reference))
  }

  # --- PV degradation strategy (E3) --------------------------------------
  # If process_variation is NULL, degrade to "from_study" with a warning
  # instead of hard-stopping. The downstream analyzer still validates.
  if (is.null(process_variation)) {
    warning("[iqr_linearity_bias] process_variation is NULL; degrading to ",
            "'from_study' (6*s_pooled within-reference). For publishable ",
            "results, supply an explicit Process Variation from a capability ",
            "study or historical data.", call. = FALSE)
    process_variation <- "from_study"
  }

  # "from_historical_sigma" mode: requires historical_sd
  if (is.character(process_variation) &&
      process_variation == "from_historical_sigma") {
    if (is.null(historical_sd) || !is.numeric(historical_sd) ||
        length(historical_sd) != 1 || historical_sd <= 0) {
      stop("[iqr_linearity_bias] process_variation = 'from_historical_sigma' ",
           "requires a positive numeric historical_sd.", call. = FALSE)
    }
    process_variation <- 6 * historical_sd
    message("[iqr_linearity_bias] process_variation = 6 * historical_sd = ",
            round(process_variation, 6), ".")
  }

  # Create and run task
  task <- Type1Task$new(data, study_type = "linearity", theme = theme)
  task$compute(
    reference_values = reference_values,
    spec_limits = list(lsl = lsl, usl = usl),
    tolerance = tolerance,
    natural_zero = natural_zero,
    process_variation = process_variation,
    historical_sd = historical_sd,
    resolution = resolution,
    u_cal = u_cal,
    u_lin = u_lin,
    u_rest = u_rest,
    linearity_corrected = linearity_corrected,
    conf_level = conf_level
  )
  return(task)
}

#' @title Create Type1 Gage Study Example Data
#' @description
#' Create synthetic data for testing Type1 Gage Study
#'
#' @param study_type "bias" or "linearity"
#' @param n_rep Number of replicate measurements per reference
#' @param reference_value Single reference for bias study
#' @param reference_values Reference values for linearity study
#' @param bias Bias amount for bias study
#' @param sd Standard deviation for measurements
#' @param lsl Lower specification limit
#' @param usl Upper specification limit
#' @param seed Random seed
#'
#' @return List with data, reference, and spec limits
#' @export
create_type1_example <- function(study_type = c("bias", "linearity"),
                                 n_rep = 25,
                                 reference_value = 10,
                                 reference_values = c(8, 9, 10, 11, 12),
                                 bias = 0.1,
                                 sd = 0.5,
                                 lsl = 7,
                                 usl = 13,
                                 seed = 12345) {
  study_type <- match.arg(study_type)
  set.seed(seed)

  if (study_type == "bias") {
    measurements <- rnorm(n_rep, mean = reference_value + bias, sd = sd)
    data <- data.frame(measurement = measurements)
    return(list(
      data = data,
      reference_value = reference_value,
      lsl = lsl,
      usl = usl
    ))
  } else {
    n_ref <- length(reference_values)
    bias_slope <- 0.05
    measurements <- numeric(n_rep * n_ref)
    references <- numeric(n_rep * n_ref)
    idx <- 1
    for (i in seq_len(n_ref)) {
      ref <- reference_values[i]
      bias_val <- bias + bias_slope * (ref - reference_values[1])
      meas <- rnorm(n_rep, mean = ref + bias_val, sd = sd)
      measurements[idx:(idx + n_rep - 1)] <- meas
      references[idx:(idx + n_rep - 1)] <- ref
      idx <- idx + n_rep
    }
    data <- data.frame(reference = references, measurement = measurements)
    return(list(
      data = data,
      reference_values = reference_values,
      lsl = lsl,
      usl = usl
    ))
  }
}