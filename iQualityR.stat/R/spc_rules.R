# =============================================================================
# File: R/spc_rules.R
# Description: SPC out-of-control rules detection (Western Electric / Nelson Rules)
# =============================================================================

#' @title List available SPC rules
#' @description Returns a list of all available SPC out-of-control rules with descriptions.
#' @return A data.frame with rule numbers and descriptions.
#' @export
#' @examples
#' list_spc_rules()
list_spc_rules <- function() {
  rules <- data.frame(
    Rule = 1:8,
    Description = c(
      "1 point beyond 3-sigma control limit",
      "9 consecutive points on same side of center line",
      "6 consecutive points increasing or decreasing (trend)",
      "14 consecutive points alternating up and down",
      "2 out of 3 consecutive points beyond 2-sigma (same side)",
      "4 out of 5 consecutive points beyond 1-sigma (same side)",
      "15 consecutive points within 1-sigma (stratification)",
      "8 consecutive points outside 1-sigma (mixture)"
    ),
    stringsAsFactors = FALSE
  )
  return(rules)
}

#' @title Western Electric out-of-control rules detection
#' @description
#' Detects out-of-control signals in control charts, based on Western Electric Rules.
#'
#' **8 Rules**:
#' - Rule 1: 1 point beyond 3-sigma control limit
#' - Rule 2: 9 consecutive points on same side of center line
#' - Rule 3: 6 consecutive points increasing or decreasing (trend)
#' - Rule 4: 14 consecutive points alternating up and down
#' - Rule 5: 2 out of 3 consecutive points beyond 2-sigma (same side)
#' - Rule 6: 4 out of 5 consecutive points beyond 1-sigma (same side)
#' - Rule 7: 15 consecutive points within 1-sigma (stratification)
#' - Rule 8: 8 consecutive points outside 1-sigma (both sides)
#'
#' @param x Numeric vector (in time order)
#' @param center Center line (default mean)
#' @param sigma Standard deviation (default sample standard deviation)
#' @param rules Rule numbers enabled (default 1:8)
#'
#' @return List containing violations (violation details), n_violations (total violations),
#'   is_in_control (whether in control), rules_triggered (triggered rules)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- c(rnorm(20, mean = 10, sd = 1), 13.5, rnorm(10, mean = 10, sd = 1))
#' detect_spc_violations(x, center = 10, sigma = 1)
detect_spc_violations <- function(x, center = NULL, sigma = NULL,
                                   rules = 1:8) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 2) stop("Need at least 2 observations.")

  if (is.null(center)) center <- mean(x)
  if (is.null(sigma)) sigma <- sd(x)

  if (sigma == 0) {
    return(list(
      violations = list(),
      n_violations = 0,
      is_in_control = TRUE,
      rules_triggered = character(0),
      center = center,
      sigma = sigma,
      n = n
    ))
  }

  violations <- list()
  rules_triggered <- character(0)

  rule_functions <- list(
    "1" = private_rule_1,
    "2" = private_rule_2,
    "3" = private_rule_3,
    "4" = private_rule_4,
    "5" = private_rule_5,
    "6" = private_rule_6,
    "7" = private_rule_7,
    "8" = private_rule_8
  )

  for (rule_id in as.character(rules)) {
    if (rule_id %in% names(rule_functions)) {
      result <- rule_functions[[rule_id]](x, center, sigma)
      if (result$triggered) {
        rules_triggered <- c(rules_triggered, sprintf("Rule %s", rule_id))
        violations[[sprintf("Rule %s", rule_id)]] <- result
      }
    }
  }

  list(
    violations = violations,
    n_violations = length(rules_triggered),
    is_in_control = length(rules_triggered) == 0,
    rules_triggered = rules_triggered,
    center = center,
    sigma = sigma,
    ucl_3 = center + 3 * sigma,
    lcl_3 = center - 3 * sigma,
    ucl_2 = center + 2 * sigma,
    lcl_2 = center - 2 * sigma,
    ucl_1 = center + 1 * sigma,
    lcl_1 = center - 1 * sigma,
    n = n
  )
}

#' @title Single rule detection
#' @description
#' Detects whether a specific rule is triggered.
#'
#' @param x Numeric vector
#' @param rule Rule number (1-8)
#' @param center Center line
#' @param sigma Standard deviation
#'
#' @return Rule detection result
#' @export
#'
#' @examples
#' x <- rnorm(30, mean = 10, sd = 1)
#' check_spc_rule(x, rule = 2, center = 10, sigma = 1)
check_spc_rule <- function(x, rule, center = NULL, sigma = NULL) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]

  if (is.null(center)) center <- mean(x)
  if (is.null(sigma)) sigma <- sd(x)

  rule <- as.character(rule)
  if (!(rule %in% as.character(1:8))) {
    stop("Rule must be between 1 and 8.")
  }

  rule_functions <- list(
    "1" = private_rule_1,
    "2" = private_rule_2,
    "3" = private_rule_3,
    "4" = private_rule_4,
    "5" = private_rule_5,
    "6" = private_rule_6,
    "7" = private_rule_7,
    "8" = private_rule_8
  )

  rule_functions[[rule]](x, center, sigma)
}

#' @title Control limit calculation
#' @description
#' Calculates control limits based on subgroup data (Xbar-R / Xbar-S / I-MR).
#'
#' @param data Numeric vector or data frame
#' @param subgroup_size Subgroup size (1 for I-MR)
#' @param chart_type Control chart type ("xbar_r", "xbar_s", "imr")
#'
#' @return List containing center, sigma, ucl, lcl, constants
#' @export
#'
#' @examples
#' data <- rnorm(100, mean = 10, sd = 1)
#' calc_control_limits(data, subgroup_size = 5, chart_type = "xbar_r")
calc_control_limits <- function(data, subgroup_size = NULL,
                                 chart_type = c("xbar_r", "xbar_s", "imr")) {
  chart_type <- match.arg(chart_type)

  if (chart_type == "imr") {
    subgroup_size <- 1
  }

  if (is.null(subgroup_size)) {
    stop("subgroup_size must be specified.")
  }

  data <- as.numeric(data)
  data <- data[!is.na(data)]

  if (chart_type == "imr") {
    mr <- abs(diff(data))
    mr_bar <- mean(mr)
    center <- mean(data)
    sigma <- mr_bar / get_d2(2)

    list(
      center = center,
      sigma = sigma,
      ucl_x = center + 3 * sigma,
      lcl_x = center - 3 * sigma,
      ucl_mr = 3.267 * mr_bar,
      lcl_mr = 0,
      mr_bar = mr_bar,
      chart_type = "I-MR",
      n = length(data)
    )
  } else {
    n_subgroups <- length(data) %/% subgroup_size
    subgroups <- matrix(data[1:(n_subgroups * subgroup_size)],
                        ncol = subgroup_size, byrow = TRUE)

    xbar <- rowMeans(subgroups)

    if (chart_type == "xbar_r") {
      ranges <- apply(subgroups, 1, function(row) max(row) - min(row))
      r_bar <- mean(ranges)
      center <- mean(xbar)
      sigma <- r_bar / get_d2(subgroup_size)

      list(
        center = center,
        sigma = sigma,
        ucl_x = center + get_A2(subgroup_size) * r_bar,
        lcl_x = center - get_A2(subgroup_size) * r_bar,
        ucl_r = get_D4(subgroup_size) * r_bar,
        lcl_r = get_D3(subgroup_size) * r_bar,
        r_bar = r_bar,
        chart_type = "Xbar-R",
        subgroup_size = subgroup_size,
        n_subgroups = n_subgroups
      )
    } else {
      s <- apply(subgroups, 1, sd)
      s_bar <- mean(s)
      center <- mean(xbar)
      sigma <- s_bar / get_c4(subgroup_size)

      list(
        center = center,
        sigma = sigma,
        ucl_x = center + get_A3(subgroup_size) * s_bar,
        lcl_x = center - get_A3(subgroup_size) * s_bar,
        ucl_s = get_B4(subgroup_size) * s_bar,
        lcl_s = get_B3(subgroup_size) * s_bar,
        s_bar = s_bar,
        chart_type = "Xbar-S",
        subgroup_size = subgroup_size,
        n_subgroups = n_subgroups
      )
    }
  }
}

#' @title Rule result summary
#' @description
#' Formats out-of-control rule detection results into readable report.
#'
#' @param result Result returned by detect_spc_violations
#' @param format Output format ("text", "data.frame")
#'
#' @return Text report or data frame
#' @export
#'
#' @examples
#' x <- c(rnorm(20, mean = 10, sd = 1), 13.5)
#' result <- detect_spc_violations(x, center = 10, sigma = 1)
#' summarize_spc_rules(result)
summarize_spc_rules <- function(result, format = c("text", "data.frame")) {
  format <- match.arg(format)

  rule_descriptions <- c(
    "Rule 1" = "1 point beyond 3-sigma control limit",
    "Rule 2" = "9 consecutive points on same side of center line",
    "Rule 3" = "6 consecutive points increasing or decreasing",
    "Rule 4" = "14 consecutive points alternating up and down",
    "Rule 5" = "2 out of 3 consecutive points beyond 2-sigma",
    "Rule 6" = "4 out of 5 consecutive points beyond 1-sigma",
    "Rule 7" = "15 consecutive points within 1-sigma (stratification)",
    "Rule 8" = "8 consecutive points outside 1-sigma (mixture)"
  )

  if (format == "text") {
    header <- "SPC Out-of-Control Rule Detection Report"
    separator <- paste(rep("-", nchar(header)), collapse = "")

    lines <- c(
      separator,
      header,
      separator,
      "",
      sprintf("  Sample size: %d", result$n),
      sprintf("  Center line: %.4f", result$center),
      sprintf("  Standard deviation: %.4f", result$sigma),
      "",
      sprintf("  Control limits: UCL = %.4f, LCL = %.4f", result$ucl_3, result$lcl_3),
      "",
      "[Detection Conclusion]",
      if (result$is_in_control) {
        "  Process in control, no rules triggered."
      } else {
        sprintf("  Process out of control! %d rules triggered:", result$n_violations)
      }
    )

    if (!result$is_in_control) {
      for (rule in result$rules_triggered) {
        desc <- rule_descriptions[[rule]] %||% ""
        violation <- result$violations[[rule]]
        indices <- violation$indices %||% integer(0)

        lines <- c(lines,
          sprintf("  * %s: %s", rule, desc),
          sprintf("    Trigger positions: %s", paste(indices, collapse = ", "))
        )
      }
    }

    paste(lines, collapse = "\n")
  } else {
    data.frame(
      rule = result$rules_triggered,
      description = rule_descriptions[result$rules_triggered],
      n_violations = sapply(result$rules_triggered, function(r) {
        v <- result$violations[[r]]
        length(v$indices %||% integer(0))
      }),
      indices = sapply(result$rules_triggered, function(r) {
        v <- result$violations[[r]]
        paste(v$indices %||% integer(0), collapse = ",")
      })
    )
  }
}

# =============================================================================
# Western Electric Rules implementation
# =============================================================================

private_rule_1 <- function(x, center, sigma) {
  ucl <- center + 3 * sigma
  lcl <- center - 3 * sigma
  violations <- which(x > ucl | x < lcl)

  list(
    rule = "Rule 1",
    description = "1 point beyond 3-sigma control limit",
    triggered = length(violations) > 0,
    indices = violations,
    values = x[violations]
  )
}

private_rule_2 <- function(x, center, sigma) {
  n <- length(x)
  above <- x > center
  violations <- integer(0)

  run_length <- 1
  if (n >= 2) for (i in 2:n) {
    if (above[i] == above[i - 1]) {
      run_length <- run_length + 1
      if (run_length >= 9) {
        violations <- c(violations, i)
      }
    } else {
      run_length <- 1
    }
  }

  list(
    rule = "Rule 2",
    description = "9 consecutive points on same side of center line",
    triggered = length(violations) > 0,
    indices = violations
  )
}

private_rule_3 <- function(x, center, sigma) {
  n <- length(x)
  violations <- integer(0)

  if (n >= 7) for (i in 7:n) {
    window <- x[(i - 6):i]
    diffs <- diff(window)

    if (all(diffs > 0) || all(diffs < 0)) {
      violations <- c(violations, i)
    }
  }

  list(
    rule = "Rule 3",
    description = "6 consecutive points steadily increasing or decreasing",
    triggered = length(violations) > 0,
    indices = violations
  )
}

private_rule_4 <- function(x, center, sigma) {
  n <- length(x)
  violations <- integer(0)

  if (n >= 15) for (i in 15:n) {
    window <- x[(i - 14):i]
    diffs <- diff(window)
    signs <- sign(diffs)

    if (all(signs == c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1)) ||
        all(signs == c(-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1))) {
      violations <- c(violations, i)
    }
  }

  list(
    rule = "Rule 4",
    description = "14 consecutive points alternating up and down",
    triggered = length(violations) > 0,
    indices = violations
  )
}

private_rule_5 <- function(x, center, sigma) {
  n <- length(x)
  ucl_2 <- center + 2 * sigma
  lcl_2 <- center - 2 * sigma
  violations <- integer(0)

  if (n >= 3) for (i in 3:n) {
    window <- x[(i - 2):i]
    above_2 <- sum(window > ucl_2)
    below_2 <- sum(window < lcl_2)

    if (above_2 >= 2 || below_2 >= 2) {
      violations <- c(violations, i)
    }
  }

  list(
    rule = "Rule 5",
    description = "2 out of 3 consecutive points beyond 2-sigma (same side)",
    triggered = length(violations) > 0,
    indices = violations
  )
}

private_rule_6 <- function(x, center, sigma) {
  n <- length(x)
  ucl_1 <- center + 1 * sigma
  lcl_1 <- center - 1 * sigma
  violations <- integer(0)

  if (n >= 5) for (i in 5:n) {
    window <- x[(i - 4):i]
    above_1 <- sum(window > ucl_1)
    below_1 <- sum(window < lcl_1)

    if (above_1 >= 4 || below_1 >= 4) {
      violations <- c(violations, i)
    }
  }

  list(
    rule = "Rule 6",
    description = "4 out of 5 consecutive points beyond 1-sigma (same side)",
    triggered = length(violations) > 0,
    indices = violations
  )
}

private_rule_7 <- function(x, center, sigma) {
  n <- length(x)
  ucl_1 <- center + 1 * sigma
  lcl_1 <- center - 1 * sigma
  violations <- integer(0)

  if (n >= 15) for (i in 15:n) {
    window <- x[(i - 14):i]
    if (all(window >= lcl_1 & window <= ucl_1)) {
      violations <- c(violations, i)
    }
  }

  list(
    rule = "Rule 7",
    description = "15 consecutive points within 1-sigma (stratification)",
    triggered = length(violations) > 0,
    indices = violations
  )
}

private_rule_8 <- function(x, center, sigma) {
  n <- length(x)
  ucl_1 <- center + 1 * sigma
  lcl_1 <- center - 1 * sigma
  violations <- integer(0)

  if (n >= 8) for (i in 8:n) {
    window <- x[(i - 7):i]
    if (all(window > ucl_1 | window < lcl_1)) {
      violations <- c(violations, i)
    }
  }

  list(
    rule = "Rule 8",
    description = "8 consecutive points outside 1-sigma (mixture)",
    triggered = length(violations) > 0,
    indices = violations
  )
}
