context("join")

test_that("unduplicate() works with empty tibbles", {
  expect_identical(unduplicate(data.frame()), data.frame())
})

test_that("nesting join() handles fully unmatched keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "c", "d")),
    baz = tibble(z = 7L, key = "d")
  )
  dfs <- map(dfs, nesting)

  exp <- tibble(
    key = chr(),
    foo = list(),
    bar = list(),
    baz = list()
  )
  expect_identical(join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("nesting join() matches single key", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "b", "d")),
    baz = tibble(z = 7L, key = "b")
  )
  dfs <- map(dfs, nesting)

  exp <- tibble(
    key = "b",
    foo = list(tibble(x = 2L)),
    bar = list(tibble(y = 5L)),
    baz = list(tibble(z = 7L))
  )
  expect_identical(join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("nesting join() matches two keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "b", "d")),
    baz = tibble(z = 7L, key = c("b", "a"))
  )
  dfs <- map(dfs, nesting)

  exp <- tibble(
    key = c("a", "b"),
    foo = list(tibble(x = 1L), tibble(x = 2L)),
    bar = list(tibble(y = 4L), tibble(y = 5L)),
    baz = list(tibble(z = 7L), tibble(z = 7L))
  )
  expect_identical(join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("nesting join() matches on all keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("c", "b", "a")),
    baz = tibble(z = 7L, key = c("b", "a", "c"))
  )
  dfs <- map(dfs, nesting)

  exp <- tibble(
    key = c("a", "b", "c"),
    foo = list(tibble(x = 1L), tibble(x = 2L), tibble(x = 3L)),
    bar = list(tibble(y = 6L), tibble(y = 5L), tibble(y = 4L)),
    baz = list(tibble(z = 7L), tibble(z = 7L), tibble(z = 7L))
  )
  expect_identical(join("key", !!!dfs, .unmatched = "drop"), exp)
  expect_identical(join("key", !!!dfs, .unmatched = "error"), exp)
})

test_that("nesting join() fails with duplicated keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "a", "b")),
    bar = tibble(y = 4:6, key = c("a", "b", "c"))
  )
  dfs <- map(dfs, nesting)
  expect_error(join("key", !!!dfs), "can't be duplicated")
})

test_that("df-col join() handles fully unmatched keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "c", "d")),
    baz = tibble(z = 7L, key = "d")
  )

  exp <- tibble(
    key = chr(),
    foo = tibble(x = int()),
    bar = tibble(y = int()),
    baz = tibble(z = int())
  )
  expect_identical(join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("df-col join() matches single key", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "b", "d")),
    baz = tibble(z = 7L, key = "b")
  )

  exp <- tibble(
    key = "b",
    foo = tibble(x = 2L),
    bar = tibble(y = 5L),
    baz = tibble(z = 7L)
  )
  expect_identical(join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("df-col join() matches two keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "b", "d")),
    baz = tibble(z = 7L, key = c("b", "a"))
  )

  exp <- tibble(
    key = c("a", "b"),
    foo = tibble(x = c(1L, 2L)),
    bar = tibble(y = c(4L, 5L)),
    baz = tibble(z = c(7L, 7L))
  )
  expect_identical(join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("df-col join() matches on all keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("c", "b", "a")),
    baz = tibble(z = 7L, key = c("b", "a", "c"))
  )

  exp <- tibble(
    key = c("a", "b", "c"),
    foo = tibble(x = 1:3),
    bar = tibble(y = 6:4),
    baz = tibble(z = c(7L, 7L, 7L))
  )
  expect_identical(join("key", !!!dfs, .unmatched = "drop"), exp)
  expect_identical(join("key", !!!dfs, .unmatched = "error"), exp)
})

test_that("df-col join() fails when keys are duplicated", {
  expect_error(
    join("key",
      foo = tibble(x = 1:3, key = c("a", "a", "b")),
      bar = tibble(y = 3:1, key = c("a", "b", "c"))
    ),
    "can't be duplicated"
  )
})

test_that("can mix nesting and df-col join()", {
  foo <- tibble(x = 1:3, key = c("a", "b", "c"))
  bar <- tibble(y = 4:6, key = c("a", "c", "d"))

  exp <- tibble(
    key = c("a", "c"),
    foo = tibble(x = c(1L, 3L)),
    bar = list(tibble(y = 4L), tibble(y = 5L))
  )
  expect_identical(join("key", foo = foo, bar = nesting(bar)), exp)

  exp <- tibble(
    key = c("a", "c"),
    foo = list(tibble(x = 1L), tibble(x = 3L)),
    bar = tibble(y = c(4L, 5L))
  )
  expect_identical(join("key", foo = nesting(foo), bar = bar), exp)
})

test_that("can join-splice with unnamed arguments to join()", {
  foo <- tibble(x = 1:3, key = c("a", "b", "c"))
  bar <- tibble(y = 4:6, key = c("a", "c", "d"))

  expect_error(join("key", foo, foo, bar = bar), "homonym columns")

  exp <- tibble(
    key = c("a", "c"),
    x = c(1L, 3L),
    bar = tibble(y = c(4L, 5L))
  )
  expect_identical(join("key", foo, bar = bar), exp)

  exp <- tibble(
    key = c("a", "c"),
    x = c(1L, 3L),
    bar = tibble(y = c(4L, 5L), key = c("a", "c"))
  )
  expect_identical(join("key", foo, bar = bar, .keep = TRUE), exp)
})

test_that("can join-splice with nesting unnamed arguments to join()", {
  foo <- tibble(x = 1:3, y = 4:6, key = c("a", "b", "c"))
  bar <- tibble(z = 4:6, key = c("a", "c", "d"))

  exp <- tibble(
    key = c("a", "c"),
    x = list(1L, 3L),
    y = list(4L, 6L),
    bar = tibble(z = 4:5)
  )
  expect_identical(join("key", nesting(foo), bar = bar), exp)
})

test_that("all inputs can be unnamed", {
  foo <- tibble(x1 = 1:3, x2 = 4:6, key = c("a", "b", "c"))
  bar <- tibble(y1 = 7:9, y2 = 10:12, key = c("a", "c", "d"))
  exp <- tibble(
    key = c("a", "c"),
    x1 = c(1L, 3L),
    x2 = c(4L, 6L),
    y1 = c(7L, 8L),
    y2 = c(10L, 11L),
  )
  expect_identical(join("key", foo, bar), exp)
})
