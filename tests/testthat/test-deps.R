
context("dependencies")

test_that("parse_deps", {
  deps <- c("foobar", "foobar (>= 1.0.0)", "foo, bar", "foo,bar",
            "foo(>= 1.0.0), bar", "foo,\n    bar", "")
  expect_equal(
    parse_deps(deps),
    list("foobar", "foobar", c("foo", "bar"), c("foo", "bar"),
         c("foo", "bar"), c("foo", "bar"), character())
  )
})

test_that("parse_deps extreme cases", {
  deps <- c(NA, "", "  ")
  expect_equal(
    parse_deps(deps),
    list(character(), character(), character())
  )

  expect_equal(parse_deps(character()), list())
})
