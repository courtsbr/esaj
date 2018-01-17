library(esaj)
context("parse_cpoxg")

test_that("Function for parsing CPOSG is working", {

  # Skip tests when not run locally
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Download 2nd degree lawsuits
  path <- tempdir()
  more <- download_cposg(
    c("1001869-51.2017.8.26.0562",
      "1001214-07.2016.8.26.0565"), path)

  # Create and run parser
  parser <- parse_decisions(parse_parts(parse_data(parse_movs(make_parser()))))
  info <- run_parser(more, parser, path)

  # Check info's shape
  expect_equal(nrow(info), 2)
  expect_equal(dim(info$movs[[1]]), c(21, 2))
  expect_equal(dim(info$data[[1]]), c(11, 2))
  expect_equal(dim(info$parts[[1]]), c(5, 4))
  expect_equal(dim(info$decisions[[1]]), c(1, 2))

  # Run parser on multiple cores
  # info <- run_parser(more, parser, path, 4)

  # Check info's shape
  # expect_equal(nrow(info), 2)
  # expect_equal(dim(info$movs[[1]]), c(19, 2))
  # expect_equal(dim(info$data[[1]]), c(11, 2))
  # expect_equal(dim(info$parts[[1]]), c(5, 4))
})

test_that("Function for parsing CPOPG is working", {

  # Skip tests when not run locally
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Download 2nd degree lawsuits
  path <- tempdir()
  more <- download_cpopg(
    c("0123479-07.2012.8.26.0100"), path)

  # Create and run parser
  parser <- parse_pd(parse_hist(parse_hearings(parse_parts(parse_data(parse_movs(make_parser("cpopg")))))))
  info <- run_parser(more, parser, path)

  # Check info's shape
  expect_equal(nrow(info), 1)
  expect_equal(dim(info$movs[[1]]), c(127, 3))
  expect_equal(dim(info$data[[1]]), c(14, 2))
  expect_equal(dim(info$parts[[1]]), c(4, 4))
  expect_equal(dim(info$hearings[[1]]), c(1, 1))
  expect_equal(dim(info$hist[[1]]), c(0, 5))
  expect_equal(dim(info$pd[[1]]), c(0, 0))

})
