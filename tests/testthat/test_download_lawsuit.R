library(esaj)
context("download_lawsuit")

test_that("download_lawsuit() is working", {

  # Skip tests when not run locally
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Create temporary directory
  path <- tempdir()

  # Run downloads
  am <- download_lawsuit("02575182220138040001", path)        # Amazonas
  sc <- download_lawsuit("0303349-44.2014.8.24.0020", path)   # Santa Catarina
  ba <- download_lawsuit("0552486-62.2015.8.05.0001", path)   # Bahia
  sp <- download_lawsuit("0123479-07.2012.8.26.0100", path)   # São Paulo

  # Expectations
  expect_gt(file.info(am)$size, 100000)
  expect_gt(file.info(sc)$size, 100000)
  expect_gt(file.info(ba)$size, 100000)
  expect_gt(file.info(sp)$size, 100000)

  # Run more than one download
  more <- download_lawsuit(
    c("0123479-07.2012.8.26.0100",
    "0552486-62.2015.8.05.0001"), path)
  expect_true(all(file.info(more)$size > 100000))
})
