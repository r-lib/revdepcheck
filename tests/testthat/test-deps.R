
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

test_that("pkgs_validate() checks types", {
  expect_error(pkgs_validate(10), "must be")
  expect_error(pkgs_validate(list()), "must be")
  expect_error(pkgs_validate(data.frame(x = "foo")), "must be")
})

test_that("pkgs_validate() accepts character vectors", {
  out <- pkgs_validate(c("foo", "bar"))
  exp <- tibble(package = c("foo", "bar"))
  expect_identical(out, exp)
})

test_that("pkgs_validate() coerces to tibble", {
  expect_is(pkgs_validate(data.frame(package = "foo")), "tbl_df")
})

test_that("pkgs_validate() removes duplicated rows", {
  df <- data.frame(
    repo = c("a", "b", "b"),
    package = c("foo", "bar", "foo"),
    stringsAsFactors = FALSE
  )

  out <- pkgs_validate(df)
  exp <- tibble(repo = c("a", "b"), package = c("foo", "bar"))
  expect_identical(out, exp)
})

test_that("revdep_pkgs() returns tibble", {
  skip_if_offline()
  scoped_options(revdepcheck__limit_revdeps = TRUE)

  pkgs <- revdep_pkgs("tidyverse")
  expect_named(pkgs, c("repo", "package"))
})

