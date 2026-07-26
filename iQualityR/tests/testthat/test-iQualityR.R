# Member package registry ----------------------------------------------------

test_that("iQualityR_packages returns a data.frame with expected columns", {
  df <- iQualityR_packages()
  expect_s3_class(df, "data.frame")
  expect_named(df, c("package", "title", "version"))
})

test_that("iQualityR_packages lists exactly nine member packages", {
  df <- iQualityR_packages()
  expect_equal(nrow(df), 9L)
})

test_that("iQualityR_packages covers all expected members", {
  df <- iQualityR_packages()
  expected <- c(
    "iQualityR.core", "iQualityR.plot", "iQualityR.stat",
    "iQualityR.msa", "iQualityR.capa", "iQualityR.doe",
    "iQualityR.sampling", "iQualityR.reliability", "iQualityR.predict"
  )
  expect_setequal(df$package, expected)
})

test_that("all member packages are installed and loadable", {
  df <- iQualityR_packages()
  all_installed <- vapply(
    df$package,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
  expect_true(all(all_installed))
})

test_that("all member packages are attached after library(iQualityR)", {
  attached <- paste0("package:", iQualityR_packages()$package)
  on_path <- attached %in% search()
  expect_true(all(on_path))
})
