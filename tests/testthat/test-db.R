context("db")


# metadata ----------------------------------------------------------------

test_that("can get and set metadata", {
  db_setup(":memory:")

  db_metadata_set(":memory:", "x", "abc")
  expect_equal(db_metadata_get(":memory:", "x"), "abc")
})

test_that("setting metadata replaces previous value", {
  db_metadata_set(":memory:", "y", "abc")
  db_metadata_set(":memory:", "y", "xyz")

  expect_equal(db_metadata_get(":memory:", "y"), "xyz")
})
