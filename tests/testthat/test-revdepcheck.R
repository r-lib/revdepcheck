context("revdepcheck")

test_that("inputs are checked for defunct arguments", {
  expect_error(revdep_check(".", "Depends"), "no longer takes dependencies")
  expect_error(revdep_check(".", bioc = TRUE), "defunct")
  expect_error(revdep_check(".", dependencies = "Depends"), "defunct")
})
