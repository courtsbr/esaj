library(esaj)
context("download_cjxg")

test_that("download_cjxg() is working", {

  # Skip tests when not run locally
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Create temporary directory
  path <- tempdir()

  # Check courts and registration
  courts_p <- download_cjpg("recurso", path, courts = "2-1",
                date_start = "2016-01-01", date_end = "2016-12-31")
  courts_s <- download_cjsg("recurso", path, courts = "0-56",
                registration_start = "1998-01-01",
                registration_end = "1998-12-31")
  expect_gt(file.info(courts_p)$size[2], 50000)
  expect_gt(file.info(courts_s)$size[2], 50000)

  # Check classes and trial
  classes_p <- download_cjpg("recurso", path, classes = c("8727", "8742"))
  classes_s <- download_cjsg("recurso", path, classes = c("1231", "1232"),
                trial_start = "2009-01-01", trial_end = "2009-12-31")
  expect_gt(file.info(classes_p)$size[2], 90000)
  expect_gt(file.info(classes_s)$size[2], 90000)

  # Check subjects and page
  subjects_p <- download_cjpg("recurso", path, subjects = "3372", max_page = 5)
  expect_length(subjects_p, 6)
  expect_true(all(file.info(subjects_p)$size > 100000))
  subjects_s <- download_cjsg("recurso", path, subjects = "0", max_page = 5)
  expect_length(subjects_s, 6)
  expect_true(all(file.info(subjects_s)$size > 100000))

  # Check cores
  # cores_p <- download_cjpg("recurso", path, subjects = "3372", max_page = 20, cores = 4)
  # expect_length(cores_p, 20)
  # expect_true(all(file.info(cores_p)$size > 100000))
  # cores_s <- download_cjsg("recurso", path, subjects = "0", max_page = 20, cores = 4)
  # expect_length(cores_s, 20)
  # expect_true(all(file.info(cores_s)$size > 100000))

  # See if peek_cjxg() works
  expect_message(peek_cjpg("recurso", classes = "8727"),
                 "This should take around")
  expect_message(peek_cjsg("recurso", classes = c("1231", "1232")),
                 "This should take around")
})
