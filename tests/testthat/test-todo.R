test_that("can add and remove from db", {
  pkg <- local_package()

  todo <- revdep_todo(pkg)
  expect_equal(nrow(todo), 0)

  todo <- revdep_add(pkg, c("x", "y"))
  expect_equal(todo$package, c("x", "y"))

  todo <- revdep_rm(pkg, "x")
  expect_equal(todo$package, c("x", "y"))
  expect_equal(todo$status, c("ignore", "todo"))
})
