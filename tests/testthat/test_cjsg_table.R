library(esaj)
context("cjsg_table")

test_that("CJSG tables are working", {

  # Skip tests when not run locally
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Get the tables for classes and subjects
  classes <- cjsg_table("classes")
  subjects <- cjsg_table("subjects")

  # Check dimensions of tables
  expect_equal(dim(classes), c(121, 12))
  expect_equal(dim(subjects), c(3088, 12))
  expect_equal(dim(cjsg_table("courts")), c(1208, 3))

  # Check browsing works
  expect_equal(dim(browse_cjsg(classes, list(c("ADM", "CRIMINAL"), "", "", "", "", "Recurso"))), c(4, 12))
  expect_equal(dim(browse_cjsg(subjects, list("DIREITO", "", "", "", "", c("Carta", "Parcelamento")))), c(9, 12))
})
