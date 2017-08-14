library(esaj)
context("download")

test_that("download_lawsuit() is working", {

  # Skip tests when not run locally
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Create temporary directory
  path <- tempdir()

  # Run downloads
  am <- download_lawsuit("02575182220138040001", path)      # Amazonas
  sc <- download_lawsuit("0303349-44.2014.8.24.0020", path) # Santa Catarina
  ba <- download_lawsuit("0552486-62.2015.8.05.0001", path) # Bahia
  sp <- download_lawsuit("0123479-07.2012.8.26.0100", path) # São Paulo

  # Expectations
  expect_true(stringr::str_detect(am, ".html$"))
  expect_true(stringr::str_detect(sc, ".html$"))
  expect_true(stringr::str_detect(ba, ".html$"))
  expect_true(stringr::str_detect(sp, ".html$"))
})
