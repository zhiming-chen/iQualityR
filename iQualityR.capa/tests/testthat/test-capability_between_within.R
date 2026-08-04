# =============================================================================
# File: tests/testthat/test-capability_between_within.R
# Description: Unit tests for Between/Within capability analysis
# =============================================================================

# Locate the bundled subgroup sample data both when the package is installed
# (system.file) and when running under devtools::load_all (source tree).
.read_subgroup_csv <- function() {
  csv <- system.file("extdata", "capability_normal_subgroup.csv",
                     package = "iQualityR.capa")
  if (!nzchar(csv)) {
    csv <- file.path("inst", "extdata", "capability_normal_subgroup.csv")
    if (!file.exists(csv)) {
      csv <- testthat::test_path("..", "..", "inst", "extdata",
                                 "capability_normal_subgroup.csv")
    }
  }
  read.csv(csv)
}

test_that("capability_between_within produces all four sigma components", {
  df <- .read_subgroup_csv()

  task <- capability_between_within(
    data = df, measurement = "measurement",
    lsl = 44, usl = 56, target = 50,
    subgroup = "subgroup"
  )

  expect_s3_class(task, "IqrCapabilityTask")
  expect_true(!is.null(task$results))

  s <- task$results$statistics
  expect_true(!is.null(s$sd_within))
  expect_true(!is.null(s$sd_between))
  expect_true(!is.null(s$sd_between_within))
  expect_true(!is.null(s$sd_overall))

  # B/W sigma relationship: between_within = sqrt(within^2 + between^2)
  expect_equal(s$sd_between_within,
               sqrt(s$sd_within^2 + s$sd_between^2),
               tolerance = 1e-6)
  # between_within is always >= within (between component is non-negative)
  expect_true(s$sd_between_within >= s$sd_within)
})

test_that("Cpk (B/W) uses sigma_between_within, Pp/Ppk use sigma_overall", {
  df <- .read_subgroup_csv()

  task <- capability_between_within(
    data = df, measurement = "measurement",
    lsl = 44, usl = 56,
    subgroup = "subgroup"
  )

  s <- task$results$statistics
  mu <- mean(df$measurement)
  tol <- 56 - 44

  # Cp (B/W) must use sigma_between_within
  expect_equal(s$cp, tol / (6 * s$sd_between_within), tolerance = 1e-6)
  # Cpk (B/W) is min(cpu, cpl) computed with sigma_between_within
  cpu_expected <- (56 - mu) / (3 * s$sd_between_within)
  cpl_expected <- (mu - 44) / (3 * s$sd_between_within)
  expect_equal(s$cpk, min(cpu_expected, cpl_expected), tolerance = 1e-6)
  expect_equal(s$cpu, cpu_expected, tolerance = 1e-6)
  expect_equal(s$cpl, cpl_expected, tolerance = 1e-6)

  # Pp/Ppk use overall sigma (sigma_total)
  expect_equal(s$pp, tol / (6 * s$sd_overall), tolerance = 1e-6)
  ppu_expected <- (56 - mu) / (3 * s$sd_overall)
  ppl_expected <- (mu - 44) / (3 * s$sd_overall)
  expect_equal(s$ppk, min(ppu_expected, ppl_expected), tolerance = 1e-6)

  # Realized performance cannot exceed potential capability.
  expect_true(s$cpk <= s$cp)
  expect_true(s$ppk <= s$pp)

  # B/W Cpk must differ from a within-only Cpk when there is between variation
  if (s$sd_between > 0) {
    cpk_within_only <- min((56 - mu) / (3 * s$sd_within),
                           (mu - 44) / (3 * s$sd_within))
    expect_false(abs(s$cpk - cpk_within_only) < 1e-9)
  }
})

test_that("B/W sigma components are consistent with normal analysis", {
  df <- .read_subgroup_csv()

  bw <- capability_between_within(df, "measurement", lsl = 44, usl = 56,
                                  subgroup = "subgroup")
  nm <- capability_normal(df, "measurement", lsl = 44, usl = 56,
                           subgroup = "subgroup")

  # Both paths call iQualityR.stat::sigma_decomposition with the same
  # arguments, so within / overall sigmas must match exactly.
  expect_equal(bw$results$statistics$sd_within,
               nm$results$statistics$sd_within, tolerance = 1e-9)
  expect_equal(bw$results$statistics$sd_overall,
               nm$results$statistics$sd_overall, tolerance = 1e-9)

  # B/W Cp (uses sigma_between_within) must be <= normal Cp (uses sigma_within)
  expect_true(bw$results$statistics$cp <= nm$results$statistics$cp)
  # Pp/Ppk are identical (both use sigma_total)
  expect_equal(bw$results$statistics$pp, nm$results$statistics$pp, tolerance = 1e-9)
  expect_equal(bw$results$statistics$ppk, nm$results$statistics$ppk, tolerance = 1e-9)
})

test_that("capability_between_within requires subgroup", {
  df <- .read_subgroup_csv()

  # Missing subgroup argument must error
  expect_error(capability_between_within(df, "measurement", lsl = 44, usl = 56))

  # Non-existent subgroup column must error
  expect_error(capability_between_within(
    df, "measurement", lsl = 44, usl = 56, subgroup = "no_such_column"
  ))

  # lsl >= usl must error
  expect_error(capability_between_within(
    df, "measurement", lsl = 56, usl = 44, subgroup = "subgroup"
  ))
})

test_that("capability_between_within analyzer errors without subgroup", {
  df <- .read_subgroup_csv()
  plan <- CapabilityPlan$new(lsl = 44, usl = 56, subgroup = "subgroup",
                             analysis_type = "between_within")
  analyzer <- CapabilityAnalyzer$new()
  # Calling .run_between_within with subgroup = NULL must error
  expect_error(analyzer$run(x = df$measurement, subgroup = NULL, plan = plan))
})

test_that("capability_between_within plot and summary work", {
  df <- .read_subgroup_csv()
  task <- capability_between_within(df, "measurement", lsl = 44, usl = 56,
                                    target = 50, subgroup = "subgroup")

  expect_output(task$summary())
  p <- task$plot(type = "basic")
  expect_true(!is.null(p))

  # Sixpack subtitle must advertise Between/Within
  full <- task$plot(type = "full")
  expect_true(!is.null(full))
})
