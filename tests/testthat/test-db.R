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

test_that("package name is set", {
  pkg <- pkg_check("fixtures/rvdcTestPkg/")
  dir_setup(pkg)
  db_setup(pkg)
  expect_identical(db_metadata_get(pkg, "package"), "rvdcTestPkg")

  db_setup(":memory:")
  expect_identical(db_metadata_get(":memory:", "package"), ":memory:")
})

test_that("db_insert() removes package from `todo`", {
  db_setup(":memory:")
  db_todo_add(":memory:", c("a", "b"))

  db_insert(":memory:",
    package = "b",
    status = "OK",
    duration = 0,
    starttime = 0,
    result = "",
    summary = ""
  )

  expect_identical(db_todo(":memory:")$package, "a")
})

test_that("results can be retrieved from data base at any time", {
  empty_results <- tibble(
    package = chr(),
    groups = tibble(),
    old = list(),
    new = list(),
    comparisons = list()
  )

  db_setup(":memory:")
  expect_identical(db_results(":memory:"), empty_results)

  db_todo_add(":memory:", tibble(package = "a"))
  expect_identical(db_results(":memory:"), empty_results)

  # Succeeds because `old` and `new` don't need to both be in the db
  value(db_insert(":memory:", !!!new_pkg_list("a", "old")))
  expect_identical(db_results(":memory:"), empty_results)

  res <- list(errors = chr(), warnings = chr(), notes = chr())
  exp_result <- tibble(
    package = "a",
    groups = structure(tibble(), row.names = 1L),
    old = list(tibble(!!!new_pkg_result_list("old"))),
    new = list(tibble(!!!new_pkg_result_list("new"))),
    comparisons = list(rcmdcheck_error("a", old = res, new = res))
  )

  value(db_insert(":memory:", !!!new_pkg_list("a", "new")))
  expect_identical(db_results(":memory:"), exp_result)
})

test_that("default group table has `package` column", {
  db_setup(":memory:")
  db_todo_add(":memory:", c("a", "b"))
  expect_identical(sort(db_todo(":memory:")$package), c("a", "b"))
  expect_identical(db_groups(":memory:"), tibble(package = c("a", "b")))
})

test_that("group metadata is set", {
  db_setup(":memory:")

  groups <- tibble(group = c("g1", "g2", "g1"), package = c("a", "b", "c"))
  db_todo_add(":memory:", groups)

  expect_identical(sort(db_todo(":memory:")$package), c("a", "b", "c"))
  expect_identical(db_groups(":memory:"), groups)
})

test_that("existing groups are checked when adding ungrouped packages", {
  db_setup(":memory:")

  groups <- tibble(group = c("g1", "g2", "g1"), package = c("a", "b", "c"))
  db_todo_add(":memory:", groups)

  value(db_insert(":memory:", !!!new_pkg_list("b", "old")))
  expect_identical(sort(db_todo(":memory:")$package), c("a", "c"))

  db_todo_add(":memory:", "b")
  expect_identical(sort(db_todo(":memory:")$package), c("a", "b", "c"))

  value(db_insert(":memory:", !!!new_pkg_list("b", "new")))
  results <- db_results(":memory:")
  expect_identical(results$groups$group, "g2")
})
