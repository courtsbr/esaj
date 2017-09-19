library(esaj)
context("download_2deg_lawsuit")

test_that("download_2deg_lawsuit() is working", {

  # Skip tests when not run locally
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Create temporary directory
  path <- tempdir()

  # Download one file
  file <- download_2deg_lawsuit("1001869-51.2017.8.26.0562", path)
  expect_gt(file.info(file)$size, 80000)

  # Run more than one download
  more <- download_2deg_lawsuit(
    c("1001869-51.2017.8.26.0562",
      "1001214-07.2016.8.26.0565"), path)
  expect_true(all(file.info(more)$size > 80000))

  # Check if we get the right error
  expect_error(download_2deg_lawsuit(".", path), "Invalid ID")
})
