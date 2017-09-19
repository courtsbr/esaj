library(esaj)
context("download_cjsg")

test_that("download_cjsg() is working", {

  # Skip tests when not run locally
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Create temporary directory
  path <- tempdir()

  # Check courts and registration
  courts <- download_cjsg("recurso", path, courts = "0-56",
                registration_start = "1998-01-01",
                registration_end = "1998-12-31")
  expect_gt(file.info(courts)$size, 50000)

  # Check classes and trial
  classes <- download_cjsg("recurso", path, classes = c("1231", "1232"),
                trial_start = "2009-01-01", trial_end = "2009-12-31")
  expect_gt(file.info(classes)$size, 90000)

  # Check subjects and page
  subjects <- download_cjsg("recurso", path, subjects = "0", max_page = 5)
  expect_length(subjects, 5)
  expect_true(all(file.info(subjects)$size > 100000))

  # Check cores
  # cores <- download_cjsg("recurso", path, subjects = "0", max_page = 20, cores = 4)
  # expect_length(cores, 20)
  # expect_true(all(file.info(cores)$size > 100000))

  # See if peek_cjsg() works
  expect_message(peek_cjsg("recurso", classes = c("1231", "1232")),
                 "This should take around")
})
