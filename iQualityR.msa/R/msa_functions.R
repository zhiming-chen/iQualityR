# =============================================================================
# File: iQualityR.msa/R/msa_functions.R
# Description: User-facing convenience entry functions for MSA analysis
# Follows iQualityR framework conventions v3.0
# =============================================================================

#' @title MSA Entry Function
#' @description Dispatch to the requested MSA study type.
#' @param data Measurement data (data.frame).
#' @param study Study type: "type1_bias", "type1_linearity", "attr_gage", or "gage_rr".
#' @param ... Additional arguments passed to the underlying study function
#'   (including `theme` if the user wants to override the default).
#' @param theme Theme name (default "academic"). Forwarded to the dispatched
#'   study function; passing `theme` inside `...` takes precedence and is
#'   not allowed (will raise a "multiple actual arguments" error from R).
#' @return A study task object.
#' @export
iqr_msa <- function(data,
                    study = c("type1_bias", "type1_linearity", "attr_gage", "gage_rr"),
                    ...,
                    theme = "academic") {
  study <- match.arg(study)

  # Forward `theme` explicitly only when the caller has not already supplied
  # it via `...`. This avoids the "multiple actual arguments" error while
  # keeping the documented default visible in the signature.
  args <- list(...)
  has_theme <- "theme" %in% names(args)
  extras <- if (has_theme) args else c(args, theme = theme)

  switch(study,
    type1_bias      = do.call(iqr_type1_bias, c(list(data = data), extras)),
    type1_linearity = do.call(iqr_linearity_bias, c(list(data = data), extras)),
    attr_gage       = do.call(iqr_attr_gage, c(list(data = data), extras)),
    gage_rr         = do.call(gage_rr_study, c(list(data = data), extras))
  )
}

#' @title MSA Gage R&R Study (ANOVA Method)
#' @description Perform a Gage Repeatability and Reproducibility study using the ANOVA method.
#' @param data Measurement data (data.frame).
#' @param measurement_col Measurement value column name (default "MeasurementValue").
#' @param part_col Part column name (default "Part").
#' @param operator_col Operator column name (default "Operator").
#' @param method Analysis method: "crossed" or "nested".
#' @param theme Theme name (default "academic").
#' @param alpha_lim Interaction significance level (default 0.25).
#' @param sigma Process variation multiplier (default 6).
#' @param tolerance Study tolerance width used for Percent Tolerance.
#' @param lsl Lower specification limit.
#' @param usl Upper specification limit.
#' @return An [MsaTask] object containing analysis results, plots, and report methods.
#' @export
gage_rr_study <- function(data,
                          measurement_col = "MeasurementValue",
                          part_col = "Part",
                          operator_col = "Operator",
                          method = c("crossed", "nested"),
                          theme = "academic",
                          alpha_lim = 0.25,
                          sigma = 6,
                          tolerance = NULL,
                          lsl = NULL,
                          usl = NULL) {
  method <- match.arg(method)

  col_names <- c(measurement_col, part_col, operator_col)
  if (!all(col_names %in% names(data))) {
    stop("Data missing required columns: ", paste(setdiff(col_names, names(data)), collapse = ", "),
         call. = FALSE)
  }

  msa_data <- data %>%
    dplyr::rename(
      MeasurementValue = all_of(measurement_col),
      Part = all_of(part_col),
      Operator = all_of(operator_col)
    )

  plan <- MSAPlan$new(
    plan_name = "Gage_RR_Study",
    objectives = "Evaluate repeatability and reproducibility",
    operators = as.character(unique(msa_data$Operator)),
    parts = as.character(unique(msa_data$Part)),
    measurements = max(table(msa_data$Operator, msa_data$Part)),
    method = method,
    randomize = FALSE
  )

  if (!is.null(tolerance) || (!is.null(lsl) && !is.null(usl))) {
    if (is.null(tolerance)) tolerance <- usl - lsl
    if (is.null(lsl) || is.null(usl)) {
      lsl <- 0
      usl <- tolerance
    }
    plan$add_specification(
      spec_name = "Study characteristic",
      spec_id = "SPEC1",
      nominal_value = mean(c(lsl, usl)),
      lsl = lsl,
      usl = usl
    )
  }

  study <- MsaTask$new(
    data = msa_data,
    plan = plan,
    theme = theme,
    analysis_method = "anova"
  )

  study$plan$method <- method

  study$compute(alpha_lim = alpha_lim, sigma = sigma)
  invisible(study)
}

#' @title MSA Gage R&R Study (Xbar-R Method)
#' @description Perform a Gage Repeatability and Reproducibility study using the Xbar-R method.
#' @param data Measurement data (data.frame).
#' @param measurement_col Measurement value column name.
#' @param part_col Part column name.
#' @param operator_col Operator column name.
#' @param theme Theme name.
#' @param sigma Process variation multiplier.
#' @param tolerance Study tolerance width used for Percent Tolerance.
#' @param lsl Lower specification limit.
#' @param usl Upper specification limit.
#' @return An [MsaTask] object containing analysis results, plots, and report methods.
#' @export
gage_rr_xbar_r <- function(data,
                           measurement_col = "MeasurementValue",
                           part_col = "Part",
                           operator_col = "Operator",
                           theme = "academic",
                           sigma = 6,
                           tolerance = NULL,
                           lsl = NULL,
                           usl = NULL) {
  col_names <- c(measurement_col, part_col, operator_col)
  if (!all(col_names %in% names(data))) {
    stop("Data missing required columns: ", paste(setdiff(col_names, names(data)), collapse = ", "),
         call. = FALSE)
  }

  msa_data <- data %>%
    dplyr::rename(
      MeasurementValue = all_of(measurement_col),
      Part = all_of(part_col),
      Operator = all_of(operator_col)
    )

  plan <- MSAPlan$new(
    plan_name      = "Gage_RR_Xbar_R_Study",
    objectives     = "Evaluate repeatability and reproducibility (Xbar-R)",
    operators      = as.character(unique(msa_data$Operator)),
    parts          = as.character(unique(msa_data$Part)),
    measurements   = max(table(msa_data$Operator, msa_data$Part)),
    method         = "crossed",
    randomize      = FALSE
  )
  if (!is.null(tolerance) || (!is.null(lsl) && !is.null(usl))) {
    if (is.null(tolerance)) tolerance <- usl - lsl
    if (is.null(lsl) || is.null(usl)) {
      lsl <- 0
      usl <- tolerance
    }
    plan$add_specification(
      spec_name      = "Study characteristic",
      spec_id        = "SPEC1",
      nominal_value  = mean(c(lsl, usl)),
      lsl            = lsl,
      usl            = usl
    )
  }

  study <- MsaTask$new(
    data = msa_data,
    plan = plan,
    theme = theme,
    analysis_method = "xbar_r"
  )

  study$compute(sigma = sigma)
  invisible(study)
}

#' @title Create MSA Plan
#' @description Create an MSA measurement plan.
#' @param plan_name Plan name.
#' @param objectives Analysis objectives.
#' @param operators Number of operators or a character vector of operator names.
#' @param parts Number of parts or a character vector of part names.
#' @param measurements Number of measurements per part-operator cell.
#' @param method Design method: "crossed" or "nested".
#' @param randomize Whether to randomize run order.
#' @param seed Randomization seed.
#' @return An MSAPlan object.
#' @export
create_msa_plan <- function(plan_name,
                           objectives,
                           operators = 3,
                           parts = 10,
                           measurements = 3,
                           method = c("crossed", "nested"),
                           randomize = TRUE,
                           seed = NULL) {
  method <- match.arg(method)

  MSAPlan$new(
    plan_name = plan_name,
    objectives = objectives,
    operators = operators,
    parts = parts,
    measurements = measurements,
    method = method,
    randomize = randomize,
    randomization_seed = seed
  )
}

#' @title Run MSA Analysis with Plan
#' @description Run an MSA analysis using a pre-defined plan.
#' @param plan An MSAPlan object.
#' @param measurement_data Measurement data (optional; uses the plan design matrix if not provided).
#' @param theme Theme name.
#' @param analysis_method Analysis method: "anova" or "xbar_r".
#' @param alpha_lim Interaction significance level.
#' @param sigma Process variation multiplier.
#' @return An [MsaTask] object containing analysis results, plots, and report methods.
#' @export
run_msa_analysis <- function(plan,
                             measurement_data = NULL,
                             theme = "academic",
                             analysis_method = c("anova", "xbar_r"),
                             alpha_lim = 0.25,
                             sigma = 6) {
  analysis_method <- match.arg(analysis_method)

  if (is.null(measurement_data)) {
    measurement_data <- plan$design_matrix
  }

  study <- MsaTask$new(
    data = measurement_data,
    plan = plan,
    theme = theme,
    analysis_method = analysis_method
  )

  study$compute(alpha_lim = alpha_lim, sigma = sigma)
  invisible(study)
}

#' @title Infer MSA Plan from Data
#' @description Infer MSA plan parameters from existing measurement data.
#' @param data Measurement data.
#' @param measurement_col Measurement value column name.
#' @param part_col Part column name.
#' @param operator_col Operator column name.
#' @return An MSAPlan object.
#' @export
infer_msa_plan <- function(data,
                          measurement_col = "MeasurementValue",
                          part_col = "Part",
                          operator_col = "Operator") {
  col_names <- c(measurement_col, part_col, operator_col)
  if (!all(col_names %in% names(data))) {
    stop("Data missing required columns: ", paste(setdiff(col_names, names(data)), collapse = ", "),
         call. = FALSE)
  }

  msa_data <- data %>%
    dplyr::rename(
      MeasurementValue = all_of(measurement_col),
      Part = all_of(part_col),
      Operator = all_of(operator_col)
    )

  operators <- unique(msa_data$Operator)
  parts <- unique(msa_data$Part)

  measurements_per_cell <- msa_data %>%
    dplyr::group_by(.data$Operator, .data$Part) %>%
    dplyr::summarize(n = dplyr::n(), .groups = "drop") %>%
    dplyr::pull(.data$n)

  is_balanced <- length(unique(measurements_per_cell)) == 1
  measurements <- if (is_balanced) unique(measurements_per_cell) else min(measurements_per_cell)

  # Crossed design: every part is measured by every operator
  # (every Part x Operator cell is non-empty).
  # Nested design: each part is measured by only one operator
  # (each part appears in exactly one operator column).
  cross_tab <- table(msa_data$Part, msa_data$Operator)
  n_nonzero_per_part <- rowSums(cross_tab > 0)
  all_cells_filled <- all(cross_tab > 0)
  each_part_single_op <- all(n_nonzero_per_part == 1)
  inferred_method <- if (all_cells_filled) "crossed" else if (each_part_single_op) "nested" else "crossed"

  plan <- MSAPlan$new(
    plan_name = "Inferred_MSA_Plan",
    objectives = "From existing data",
    operators = as.character(operators),
    parts = as.character(parts),
    measurements = measurements,
    method = inferred_method,
    randomize = FALSE
  )

  plan$design_matrix <- msa_data %>%
    dplyr::mutate(
      RunOrder = seq_len(dplyr::n()),
      StandardOrder = seq_len(dplyr::n()),
      Replication = rep(1:measurements, length.out = dplyr::n()),
      Comments = ""
    )

  invisible(plan)
}

#' @title Multi-Spec MSA Analysis
#' @description Run MSA analysis across multiple specifications in one dataset.
#' @param data Measurement data (must contain a SpecID column).
#' @param spec_id_col Specification ID column name.
#' @param measurement_col Measurement value column name.
#' @param part_col Part column name.
#' @param operator_col Operator column name.
#' @param theme Theme name.
#' @param alpha_lim Interaction significance level.
#' @param sigma Process variation multiplier.
#' @return A list of [MsaTask] objects, one per specification.
#' @export
multi_spec_msa <- function(data,
                          spec_id_col = "SpecID",
                          measurement_col = "MeasurementValue",
                          part_col = "Part",
                          operator_col = "Operator",
                          theme = "academic",
                          alpha_lim = 0.25,
                          sigma = 6) {
  if (!spec_id_col %in% names(data)) {
    stop("Data missing the SpecID column; specify it via the spec_id_col argument",
         call. = FALSE)
  }

  spec_ids <- unique(data[[spec_id_col]])
  results <- list()

  for (spec_id in spec_ids) {
    spec_data <- data %>% dplyr::filter(.data[[spec_id_col]] == !!spec_id)

    msa_data <- spec_data %>%
      dplyr::rename(
        MeasurementValue = all_of(measurement_col),
        Part = all_of(part_col),
        Operator = all_of(operator_col)
      )

    # Build a per-spec plan so the analyzer preserves the design method and
    # allows tolerance extraction downstream.
    plan <- MSAPlan$new(
      plan_name    = paste0("MultiSpec_", spec_id),
      objectives   = "Multi-specification MSA",
      operators    = as.character(unique(msa_data$Operator)),
      parts        = as.character(unique(msa_data$Part)),
      measurements = max(table(msa_data$Operator, msa_data$Part)),
      method       = "crossed",
      randomize    = FALSE
    )

    study <- MsaTask$new(
      data = msa_data,
      plan = plan,
      theme = theme,
      analysis_method = "anova"
    )

    study$compute(alpha_lim = alpha_lim, sigma = sigma)
    results[[spec_id]] <- study
  }

  invisible(results)
}
