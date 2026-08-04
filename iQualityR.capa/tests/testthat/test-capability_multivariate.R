# =============================================================================
# File: tests/testthat/test-capability_multivariate.R
# Description: Unit tests for multivariate capability (MCPV / HPCI)
# =============================================================================

test_that("capability_multivariate computes MCPV and HPCI on correlated data", {
  set.seed(2025)
  # Bivariate normal: well-centred, narrow -> should be "pass"
  Sigma <- matrix(c(0.04, 0.02, 0.02, 0.04), nrow = 2)
  X <- MASS::mvrnorm(100, mu = c(0, 0), Sigma = Sigma)
  df <- as.data.frame(X)
  names(df) <- c("V1", "V2")
  task <- capability_multivariate(
    data = df, ctqs = c("V1", "V2"),
    lsl_vec = c(-3, -3), usl_vec = c(3, 3),
    target_vec = c(0, 0)
  )
  expect_s3_class(task, "IqrMultivarCapabilityTask")

  s <- task$results$statistics
  expect_equal(s$p, 2)
  expect_equal(s$n, 100)

  # MCPV_p (volume ratio, Cp-equiv): spec rect area = 6*6 = 36; 99.73%
  # ellipsoid area for our Sigma should be much smaller -> ratio > 1.
  expect_true(s$mcpv_p > 1)
  expect_true(s$mcpv_pk > 0)
  expect_true(s$mcpv_pk <= s$mcpv_p)   # centering penalty in [0,1]

  # HPCI components
  expect_true(s$npc > 1)
  expect_true(s$pv >= 0 && s$pv <= 1)
  expect_true(s$lri >= 0 && s$lri <= 1)

  # Hotelling T^2 test result present
  t2 <- task$results$diagnostics$hotelling_t2
  expect_true(!is.null(t2$p_value))
  expect_equal(t2$df1, 2)
  expect_equal(t2$df2, 98)

  # Yield from mvtnorm should be high for this wide spec
  expect_true(s$yield_prob > 0.95)
  expect_true(s$ppm_expected < 50000)
})

test_that("capability_multivariate detects off-target mean", {
  set.seed(2025)
  # Off-target: mean shifted to (1, 1) but target = (0, 0)
  Sigma <- matrix(c(0.04, 0.02, 0.02, 0.04), nrow = 2)
  X <- MASS::mvrnorm(100, mu = c(1, 1), Sigma = Sigma)
  df <- as.data.frame(X)
  names(df) <- c("V1", "V2")
  task <- capability_multivariate(
    data = df, ctqs = c("V1", "V2"),
    lsl_vec = c(-3, -3), usl_vec = c(3, 3),
    target_vec = c(0, 0)
  )
  s <- task$results$statistics
  v <- task$results$diagnostics$capability_judgment

  # Centering penalty should be > 0
  expect_true(s$centering_penalty > 0)
  expect_true(s$mcpv_pk < s$mcpv_p)

  # Hotelling T^2 p-value should be tiny (off-target)
  expect_true(s$pv < 0.05)
  expect_false(v$hpci_pass_center)
})

test_that("capability_multivariate detects too-wide process", {
  set.seed(2025)
  # Process spread larger than spec
  Sigma <- matrix(c(4, 1, 1, 4), nrow = 2)  # SD = 2 each
  X <- MASS::mvrnorm(100, mu = c(0, 0), Sigma = Sigma)
  df <- as.data.frame(X)
  names(df) <- c("V1", "V2")
  task <- capability_multivariate(
    data = df, ctqs = c("V1", "V2"),
    lsl_vec = c(-3, -3), usl_vec = c(3, 3),
    target_vec = c(0, 0)
  )
  s <- task$results$statistics
  v <- task$results$diagnostics$capability_judgment

  # Volume ratio < 1 (process ellipsoid larger than spec rect)
  expect_true(s$mcpv_p < 1)
  expect_true(s$npc < 1)
  expect_false(v$hpci_pass_volume)
  expect_equal(v$overall_verdict, "fail")

  # Yield should be well below 99.73%
  expect_true(s$yield_prob < 0.95)
  expect_true(s$ppm_expected > 50000)
})

test_that("capability_multivariate rejects n <= p", {
  # Only 2 rows for p = 2 -> insufficient
  df <- data.frame(V1 = c(1, 2), V2 = c(3, 4))
  expect_error(
    capability_multivariate(df, c("V1", "V2"),
                            lsl_vec = c(0, 0), usl_vec = c(5, 5)),
    "n > p"
  )
})

test_that("capability_multivariate handles p > 2 (no scatter panel)", {
  set.seed(1)
  Sigma <- diag(0.1, 3) + 0.05
  X <- MASS::mvrnorm(80, mu = c(0, 0, 0), Sigma = Sigma)
  df <- as.data.frame(X)
  names(df) <- c("X1", "X2", "X3")
  task <- capability_multivariate(
    data = df, ctqs = c("X1", "X2", "X3"),
    lsl_vec = c(-2, -2, -2), usl_vec = c(2, 2, 2),
    target_vec = c(0, 0, 0)
  )
  expect_equal(task$results$statistics$p, 3)
  expect_equal(task$results$statistics$n, 80)

  # Plot full should produce a patchwork (3 panels for p=3)
  p <- task$plot(type = "full")
  expect_s3_class(p, c("patchwork", "ggplot", "gg"))
})

test_that("capability_multivariate plot returns objects for each type", {
  set.seed(1)
  Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  X <- MASS::mvrnorm(50, mu = c(0, 0), Sigma = Sigma)
  df <- as.data.frame(X)
  names(df) <- c("V1", "V2")
  task <- capability_multivariate(
    df, ctqs = c("V1", "V2"),
    lsl_vec = c(-3, -3), usl_vec = c(3, 3),
    target_vec = c(0, 0)
  )
  for (t in c("scatter", "qq", "per_ctq", "summary", "full")) {
    p <- task$plot(type = t)
    expect_s3_class(p, c("ggplot", "gg", "patchwork"))
  }
})

test_that("capability_multivariate warns when n is small", {
  set.seed(1)
  Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  X <- MASS::mvrnorm(20, mu = c(0, 0), Sigma = Sigma)
  df <- as.data.frame(X)
  names(df) <- c("V1", "V2")
  task <- capability_multivariate(
    df, ctqs = c("V1", "V2"),
    lsl_vec = c(-3, -3), usl_vec = c(3, 3)
  )
  expect_true(length(task$results$diagnostics$warnings) > 0)
  # Should warn about small sample AND missing target
  expect_true(any(grepl("small", task$results$diagnostics$warnings)))
  expect_true(any(grepl("target", task$results$diagnostics$warnings)))
})

test_that("per-CTQ table is correct", {
  set.seed(2)
  df <- data.frame(
    V1 = rnorm(100, 0, 1),
    V2 = rnorm(100, 0, 1)
  )
  task <- capability_multivariate(
    df, ctqs = c("V1", "V2"),
    lsl_vec = c(-3, -3), usl_vec = c(3, 3),
    target_vec = c(0, 0)
  )
  per <- task$results$data_tables$per_ctq
  expect_equal(nrow(per), 2)
  expect_true(all(c("CTQ", "LSL", "USL", "Target", "Mean", "Cpk") %in% names(per)))
  expect_equal(per$CTQ, c("V1", "V2"))
  expect_equal(per$LSL, c(-3, -3))
  expect_equal(per$USL, c(3, 3))
  # Cpk for V1 should be approximately (3 - 0) / (3 * 1) = 1 for off-centre at 0
  # but mean != 0 exactly, so just check Cpk in [0, 1.5]
  expect_true(all(per$Cpk > 0.5 & per$Cpk < 1.5))
})

test_that("capability_multivariate report exports Excel", {
  skip_if_not_installed("openxlsx")
  set.seed(5)
  Sigma <- matrix(c(1, 0.4, 0.4, 1), nrow = 2)
  X <- MASS::mvrnorm(50, mu = c(0, 0), Sigma = Sigma)
  df <- as.data.frame(X)
  names(df) <- c("V1", "V2")
  task <- capability_multivariate(
    df, ctqs = c("V1", "V2"),
    lsl_vec = c(-3, -3), usl_vec = c(3, 3),
    target_vec = c(0, 0)
  )
  path <- tempfile(fileext = ".xlsx")
  task$report(format = "excel", path = path)
  expect_true(file.exists(path))
  expect_true(file.size(path) > 0)
})
