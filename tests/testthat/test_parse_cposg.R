library(esaj)
context("parse_cposg")

test_that("Function for parsing CPOSG is working", {

  # Skip tests when not run locally
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Download 2nd degree lawsuits
  path <- tempdir()
  more <- download_2deg_lawsuit(
    c("1001869-51.2017.8.26.0562",
      "1001214-07.2016.8.26.0565"), path)

  # Create and run parser
  parser <- parse_parts(parse_data(parse_movs(make_parser())))
  info <- run_parser(more, parser, path)

  # Check info's shape
  expect_equal(nrow(info), 2)
  expect_equal(dim(info$movs[[1]]), c(20, 2))
  expect_equal(dim(info$data[[1]]), c(11, 2))
  expect_equal(dim(info$parts[[1]]), c(5, 4))

  # Run parser on multiple cores
  # info <- run_parser(more, parser, path, 4)

  # Check info's shape
  # expect_equal(nrow(info), 2)
  # expect_equal(dim(info$movs[[1]]), c(19, 2))
  # expect_equal(dim(info$data[[1]]), c(11, 2))
  # expect_equal(dim(info$parts[[1]]), c(5, 4))
})
