library(esaj)
context("cjxg_table")

test_that("CJXG tables are working", {

  # Skip tests when not run locally
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Get the tables for classes and subjects
  classes_p <- cjpg_table("classes")
  subjects_p <- cjpg_table("subjects")
  courts_p <- cjpg_table("courts")

  classes_s <- cjsg_table("classes")
  subjects_s <- cjsg_table("subjects")
  courts_s <- cjsg_table("courts")

  testthat::expect_is(classes_p, "tbl")
  testthat::expect_is(subjects_p, "tbl")
  testthat::expect_is(courts_p, "tbl")
  testthat::expect_is(classes_s, "tbl")
  testthat::expect_is(subjects_s, "tbl")
  testthat::expect_is(courts_s, "tbl")

  # # Check dimensions of tables
  # expect_equal(dim(classes_p), c(769, 12))
  # expect_equal(dim(subjects_p), c(6521, 12))
  # expect_equal(dim(courts_p), c(2061, 3))
  #
  # expect_equal(dim(classes_s), c(122, 12))
  # expect_equal(dim(subjects_s), c(3088, 12))
  # expect_equal(dim(courts_s), c(1208, 3))
  #
  # # Check browsing works
  # expect_equal(dim(browse_table(classes_s, list(c("ADM", "CRIMINAL"), "", "", "", "", "Recurso"))), c(4, 12))
  # expect_equal(dim(browse_table(subjects_s, list("DIREITO", "", "", "", "", c("Carta", "Parcelamento")))), c(9, 12))
})
