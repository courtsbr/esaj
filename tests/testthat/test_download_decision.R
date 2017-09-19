library(esaj)
context("download_decision")

test_that("download_decision() is working", {

  # Skip tests when not run locally
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Create temporary directory
  path <- tempdir()

  # Download one decision
  file <- download_decision("10000034", path)
  expect_gt(file.info(file)$size, 40000)

  # Download more than one decision
  files <- download_decision(c("10800758", "10000034"), path)
  expect_true(all(file.info(files)$size > 40000))
})
