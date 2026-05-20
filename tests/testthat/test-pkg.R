test_that("can retrieve basic package metadata", {
  pkg <- local_package(list(
    Package = "test",
    Version = "0.0.1",
    BugReports = "BUGS"
  ))

  expect_equal(pkg_name(pkg), "test")
  expect_equal(pkg_version(pkg), "0.0.1")
  expect_equal(pkg_bug_reports(pkg), "BUGS")
})

test_that("pkg_check validates its inputs", {
  expect_error(pkg_check(1), "string")
  expect_error(pkg_check("NOT-FOUND"), "existing directory")
  expect_error(pkg_check("."), "DESCRIPTION")
})
