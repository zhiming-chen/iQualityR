# =============================================================================
# File: iQualityR.msa/R/attr_gage_functions.R
# Description: Attribute agreement analysis shortcut entry functions
# =============================================================================

#' @title Attribute agreement analysis shortcut function
#' @description Direct analysis entry without explicitly creating a Plan
#'
#' @param data Data frame
#' @param mode Analysis mode: "kappa" | "detection" | "all"
#' @param method Kappa method: "cohen" | "fleiss" | "wagner"
#' @param scale_type Attribute scale type: "nominal" or "ordinal"
#' @param category_order Ordered category levels for ordinal ratings
#' @param theme Theme name (default "academic").
#' @return AttrGageTask object
#'
#' @examples
#' \dontrun{
#' # Kappa analysis
#' task <- iqr_attr_gage(data, mode = "kappa", method = "fleiss")
#' task$summary()
#' task$plot("kappa_funnel")
#'
#' # Detection rate analysis
#' task <- iqr_attr_gage(data, mode = "detection")
#' task$summary()
#' task$plot("detection_ci")
#' }
#' @export
iqr_attr_gage <- function(data, mode = c("kappa", "detection", "all"),
                          method = c("cohen", "fleiss", "wagner"),
                          scale_type = c("nominal", "ordinal"),
                          category_order = NULL,
                          theme = "academic") {
  mode <- match.arg(mode)
  method <- match.arg(method)
  scale_type <- match.arg(scale_type)

  plan <- AttrGagePlan$new(
    plan_name = "Quick_Attr_Gage",
    objectives = "Attribute agreement analysis",
    comparison_mode = if (mode == "kappa" && method == "wagner") "two_way" else "one_way",
    kappa_method = method,
    categories = category_order,
    category_order = category_order,
    scale_type = scale_type,
    conf_level = 0.95
  )

  task <- AttrGageTask$new(data = data, plan = plan, theme = theme, mode = mode)
  task$compute()

  return(task)
}

#' @title Fleiss Kappa analysis shortcut
#' @param data Data frame.
#' @param sample_col Sample column name.
#' @param rater_col Appraiser/rater column name.
#' @param rating_col Rating column name.
#' @param standard_col Optional known standard column name.
#' @param trial_col Optional repeated trial column name.
#' @param scale_type Attribute scale type: "nominal" or "ordinal".
#' @param category_order Ordered category levels for ordinal ratings.
#' @param theme Theme name (default "academic").
#' @export
iqr_fleiss_kappa <- function(data, sample_col = "Sample", rater_col = "Appraiser",
                             rating_col = "Rating", standard_col = NULL,
                             trial_col = NULL, scale_type = c("nominal", "ordinal"),
                             category_order = NULL, theme = "academic") {
  scale_type <- match.arg(scale_type)
  plan <- AttrGagePlan$new(
    plan_name = "Fleiss_Kappa",
    objectives = "Fleiss Kappa analysis",
    comparison_mode = "one_way",
    kappa_method = "fleiss",
    categories = category_order,
    category_order = category_order,
    scale_type = scale_type
  )
  plan$set_meta(
    "data",
    sample_col = sample_col,
    rater_col = rater_col,
    rating_col = rating_col,
    standard_col = standard_col,
    trial_col = trial_col
  )

  task <- AttrGageTask$new(data = data, plan = plan, theme = theme, mode = "kappa")
  task$compute()
  task
}

#' @title Cohen's Kappa analysis shortcut entry
#' @param data Data frame, must contain two evaluation result columns
#' @param eval1_col First evaluator column name
#' @param eval2_col Second evaluator column name
#' @param theme Theme name (default "academic").
#' @export
iqr_cohen_kappa <- function(data, eval1_col = "Evaluator1", eval2_col = "Evaluator2", theme = "academic") {
  plan <- AttrGagePlan$new(
    plan_name = "Cohen_Kappa",
    objectives = "Cohen's Kappa analysis",
    comparison_mode = "one_way",
    kappa_method = "cohen"
  )
  plan$set_meta("data", eval1_col = eval1_col, eval2_col = eval2_col)

  task <- AttrGageTask$new(data = data, plan = plan, theme = theme, mode = "kappa")
  task$compute()

  return(task)
}

#' @title Wagner method analysis shortcut entry
#' @param data Data frame
#' @param eval1_col First evaluator column name
#' @param eval2_col Second evaluator column name
#' @param theme Theme name (default "academic").
#' @export
iqr_wagner <- function(data, eval1_col = "Evaluator1", eval2_col = "Evaluator2", theme = "academic") {
  plan <- AttrGagePlan$new(
    plan_name = "Wagner_Method",
    objectives = "Wagner method analysis",
    comparison_mode = "two_way",
    kappa_method = "wagner"
  )
  plan$set_meta("data", eval1_col = eval1_col, eval2_col = eval2_col)

  task <- AttrGageTask$new(data = data, plan = plan, theme = theme, mode = "kappa")
  task$compute()

  return(task)
}

#' @title Detection rate analysis shortcut entry
#' @param data Data frame
#' @param reference_col Reference result column name
#' @param test_col Test result column name
#' @param positive_category Positive category
#' @param negative_category Negative category
#' @param conf_level Confidence level
#' @param theme Theme name (default "academic").
#' @export
iqr_detection <- function(data, reference_col = "Reference", test_col = "Test",
                         positive_category = NULL, negative_category = NULL,
                         conf_level = 0.95, theme = "academic") {
  plan <- AttrGagePlan$new(
    plan_name = "Detection_Analysis",
    objectives = "Detection and miss rate analysis",
    comparison_mode = "one_way",
    conf_level = conf_level
  )
  plan$set_meta("data",
                reference_col = reference_col,
                test_col = test_col,
                positive_category = positive_category,
                negative_category = negative_category)

  task <- AttrGageTask$new(data = data, plan = plan, theme = theme, mode = "detection")
  task$compute()

  return(task)
}
