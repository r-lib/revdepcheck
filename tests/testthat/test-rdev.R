## A stand-in for an R executable: rdev_init() only checks that it exists
## and is executable, the real one is only needed by rdev_check()
fake_r <- function(dir = tempfile("fake-r-")) {
  dir.create(file.path(dir, "bin"), recursive = TRUE)
  path <- file.path(dir, "bin", "R")
  writeLines(c("#!/bin/sh", "exit 0"), path)
  Sys.chmod(path, "0755")
  path
}

## A finished check worker for check_done(), whose process returns `check`
fake_check_worker <- function(which, check) {
  list(
    package = check$package,
    task = task("check", check$package, which),
    process = list(
      get_start_time = function() Sys.time() - 1,
      parse_results = function() check
    )
  )
}

## Event loop state for a single package `foo` in rdev mode
fake_state <- function(root, package_state, reuse_old = TRUE) {
  list(
    options = list(
      pkgdir = root,
      num_workers = 1,
      rdev = list(reuse_old = reuse_old)
    ),
    progress_bar = list(tick = function(...) NULL),
    workers = list(),
    packages = data.frame(
      package = "foo",
      state = package_state,
      stringsAsFactors = FALSE
    )
  )
}

## An installed package as installed.packages() sees it. Older R takes the
## Built version from the metadata list, newer R from the DESCRIPTION field.
fake_installed <- function(lib, pkg, version, r = "4.7.0") {
  dir.create(
    file.path(lib, pkg, "Meta"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  desc <- c(
    Package = pkg,
    Version = version,
    Built = paste0("R ", r, "; ; 2026-02-20 10:00:00 UTC; unix")
  )
  saveRDS(
    list(DESCRIPTION = desc, Built = list(R = package_version(r))),
    file.path(lib, pkg, "Meta", "package.rds")
  )
}

fake_pkg <- function(dir = tempfile("fake-pkg-")) {
  dir.create(dir)
  writeLines(
    c("Package: fakepkg", "Version: 0.0.1"),
    file.path(dir, "DESCRIPTION")
  )
  dir
}

test_that("rdev_init() creates a root that pkg_check() and dir_find() accept", {
  skip_on_os("windows")

  root <- tempfile("rdev-root-")
  r_old <- fake_r()
  r_new <- fake_r()

  expect_identical(rdev_init(root, r_old, r_new), normalizePath(root))
  on.exit(db_disconnect(normalizePath(root)), add = TRUE)
  root <- normalizePath(root)

  expect_true(is_rdev(root))
  expect_identical(pkg_check(root), root)
  expect_identical(rdev_root(root), root)

  config <- rdev_config(root)
  expect_identical(config$r_old, normalizePath(r_old))
  expect_identical(config$r_new, normalizePath(r_new))
  expect_false(config$bioc)
  expect_null(config$cache_dir)

  ## The root is the check directory itself, and there is no package under
  ## test with old/new libraries
  expect_identical(dir_find(root, "root"), root)
  expect_identical(dir_find(root, "db"), file.path(root, "data.sqlite"))
  expect_null(dir_find(root, "old"))
  expect_null(dir_find(root, "new"))
  expect_identical(
    dir_find(root, "pkgold", "foo"),
    dir_find(root, "pkg", "foo")
  )
  expect_identical(
    dir_find(root, "pkgnew", "foo"),
    dir_find(root, "pkg", "foo")
  )
  expect_true(startsWith(dir_find(root, "tools"), root))
  expect_true(startsWith(dir_find(root, "cache"), root))

  expect_true(db_exists(root))
  expect_identical(db_metadata_get(root, "todo"), "install")
  expect_length(db_metadata_get(root, "package"), 0)

  opts <- rdev_options(root)
  expect_identical(opts$r_old, normalizePath(r_old))
  expect_identical(opts$cache, dir_find(root, "cache"))
  expect_true(opts$reuse_old)
  expect_identical(rdev_cache_env(opts), c(CRANCACHE_DIR = opts$cache))
  expect_identical(rdev_cache_env(NULL), character())
  expect_identical(
    rdev_lib_env(c("/a", "/b")),
    c(R_LIBS_USER = "/a:/b", R_LIBS_SITE = "/a:/b")
  )
})

test_that("rdev_init() keeps the database of an existing root", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  db_todo_add(root, "foo")
  db_metadata_set(root, "todo", "done")

  rdev_init(root, fake_r(), fake_r(), cache_dir = tempfile("cache-"))
  expect_identical(db_todo(root), "foo")
  expect_identical(db_metadata_get(root, "todo"), "install")
  expect_identical(rdev_options(root)$cache, rdev_config(root)$cache_dir)
})

test_that("rdev_r_binary() accepts an R home and rejects missing files", {
  skip_on_os("windows")

  r <- fake_r()
  expect_identical(rdev_r_binary(r), normalizePath(r))
  expect_identical(rdev_r_binary(dirname(dirname(r))), normalizePath(r))
  expect_error(rdev_r_binary(tempfile()), "not found")
  expect_error(rdev_r_binary(tempdir()), "not found")
})

test_that("rdev roots and package directories are told apart", {
  skip_on_os("windows")

  pkg <- fake_pkg()
  expect_false(is_rdev(pkg))
  expect_error(rdev_root(pkg), "not a directory created by")
  expect_error(rdev_init(pkg, fake_r(), fake_r()), "must not be a package")

  plain <- tempfile()
  dir.create(plain)
  expect_error(pkg_check(plain), "DESCRIPTION file, or be a root")
})

test_that("rdev_tools_built_for() reads the R version from the Built field", {
  tools <- tempfile("tools-")
  expect_null(rdev_tools_built_for(tools))

  ## installed.packages() only looks at the installed metadata
  for (pkg in c("crancache", "withr")) {
    fake_installed(tools, pkg, "1.0.0")
  }
  expect_identical(rdev_tools_built_for(tools), "4.7")

  unlink(file.path(tools, "withr"), recursive = TRUE)
  expect_null(rdev_tools_built_for(tools))
})

test_that("rdev_r_info() fingerprints a build by more than its version", {
  skip_on_cran()
  skip_on_os("windows")

  rbin <- file.path(R.home("bin"), "R")
  info <- rdev_r_info(rbin)

  expect_identical(info$version, R.version.string)
  expect_identical(
    info$minor,
    paste(R.version$major, sub("[.].*$", "", R.version$minor), sep = ".")
  )
  expect_true(startsWith(info$fingerprint, paste0(rbin, " | ", info$version)))
  expect_match(info$fingerprint, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")

  ## The same R reached through another path is a different build
  alias <- file.path(tempfile("alias-"), "R")
  dir.create(dirname(alias))
  file.symlink(rbin, alias)
  expect_false(identical(rdev_r_info(alias)$fingerprint, info$fingerprint))
})

test_that("tarball_version()", {
  expect_identical(tarball_version("/x/y/foo_1.2.3.tar.gz"), "1.2.3")
  expect_identical(tarball_version("data.table_1.15.0.tar.gz"), "1.15.0")
  expect_identical(tarball_version("foo_1.0-1.tar.gz"), "1.0-1")
})

test_that("old results are only reused for complete checks of the same version", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  expect_false(rdev_old_result_usable(root, "foo", "foo_1.0.tar.gz"))

  ## A result without a dependency snapshot is never reused
  db_insert(
    root,
    "foo",
    version = "1.0",
    status = "OK",
    which = "old",
    duration = 1,
    starttime = Sys.time(),
    result = "{}",
    summary = NULL
  )
  expect_false(rdev_old_result_usable(root, "foo", "foo_1.0.tar.gz"))

  ## No dependencies then, none installed now
  check <- structure(list(libraries = character()), class = "rcmdcheck")
  db_insert(
    root,
    "foo",
    version = "1.0",
    status = "OK",
    which = "old",
    duration = 1,
    starttime = Sys.time(),
    result = unclass(toJSON(check)),
    summary = NULL
  )
  expect_true(rdev_old_result_usable(root, "foo", "foo_1.0.tar.gz"))
  expect_false(rdev_old_result_usable(root, "foo", "foo_1.1.tar.gz"))
  expect_false(rdev_old_result_usable(root, "bar", "bar_1.0.tar.gz"))

  for (status in c("TIMEOUT", "PREPERROR")) {
    db_insert(
      root,
      "foo",
      version = "1.0",
      status = status,
      which = "old",
      duration = 1,
      starttime = Sys.time(),
      result = "{}",
      summary = NULL
    )
    expect_false(rdev_old_result_usable(root, "foo", "foo_1.0.tar.gz"))
  }
})

test_that("old results are only reused against the same dependency versions", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  ## A dependency library with one installed package
  lib <- dir_find(root, "pkg", "foo")
  fake_installed(lib, "dep", "2.0")
  expect_identical(rdev_library_snapshot(lib), "dep@2.0")
  expect_identical(rdev_library_snapshot(tempfile()), character())
  empty <- tempfile("empty-lib-")
  dir.create(empty)
  expect_identical(rdev_library_snapshot(empty), character())

  check <- structure(
    list(package = "foo", version = "1.0", libraries = "dep@2.0"),
    class = "rcmdcheck"
  )
  db_insert(
    root,
    "foo",
    version = "1.0",
    status = "OK",
    which = "old",
    duration = 1,
    starttime = Sys.time(),
    result = unclass(toJSON(check)),
    summary = NULL
  )
  expect_true(rdev_old_result_usable(root, "foo", "foo_1.0.tar.gz"))

  ## The dependency was updated since
  fake_installed(lib, "dep", "2.1")
  expect_false(rdev_old_result_usable(root, "foo", "foo_1.0.tar.gz"))
})

test_that("download_done() reuses a usable old result and only the new check runs", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)
  dir_setup_package(root, "foo")
  db_todo_add(root, "foo")

  ## A dependency library and a downloaded tarball
  lib <- dir_find(root, "pkg", "foo")
  fake_installed(lib, "dep", "2.0")
  file.create(file.path(dir_find(root, "check", "foo"), "foo_1.0.tar.gz"))

  ## An old result against the same dependencies
  check <- structure(
    list(
      package = "foo",
      version = "1.0",
      status = 0L,
      timeout = FALSE,
      errors = character(),
      warnings = character(),
      notes = character(),
      libraries = "dep@2.0",
      description = "Package: foo\nVersion: 1.0\nMaintainer: A B <a@b.com>\n"
    ),
    class = "rcmdcheck"
  )
  db_insert(
    root,
    "foo",
    version = "1.0",
    status = "OK",
    which = "old",
    duration = 1,
    starttime = "old start",
    result = unclass(toJSON(check)),
    summary = NULL
  )

  worker <- list(package = "foo", task = task("download", "foo", 1L))

  state <- download_done(
    fake_state(root, "downloading", reuse_old = FALSE),
    worker
  )
  expect_identical(state$packages$state, "downloaded")

  state <- download_done(
    fake_state(root, "downloading", reuse_old = TRUE),
    worker
  )
  expect_identical(state$packages$state, "done-downloaded")
  expect_identical(schedule_next_task(state)$args[[2]], "new")

  ## Only the new check runs, then the package is done
  state$packages$state <- "done-checking"
  capture.output(state <- check_done(state, fake_check_worker("new", check)))
  expect_identical(state$packages$state, "done")
  expect_false(dir.exists(lib))

  results <- db_get_results(root, "foo")
  expect_identical(results$old$starttime, "old start")
  expect_identical(nrow(results$new), 1L)
  expect_identical(db_todo_status(root)$status, "done")
})

test_that("rdev_install() starts over when the old build changes", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  ## A finished package and a populated default cache
  db_todo_add(root, "foo")
  for (which in c("old", "new")) {
    db_insert(
      root,
      "foo",
      version = "1.0",
      status = "OK",
      which = which,
      duration = 1,
      starttime = Sys.time(),
      result = "{}",
      summary = NULL
    )
  }
  cache <- dir_find(root, "cache")

  fingerprint <- "build A"
  local_mocked_bindings(
    rdev_r_info = function(rbin) {
      list(version = "R x", minor = "4.7", svn = "1", fingerprint = fingerprint)
    },
    rdev_install_tools = function(...) NULL,
    rdev_warm_cache = function(...) NULL
  )

  rdev_install(root)
  expect_identical(db_metadata_get(root, "r_old_fingerprint"), "build A")
  expect_identical(db_metadata_get(root, "todo"), "run")
  expect_identical(db_todo_status(root)$status, "done")
  writeLines("x", file.path(cache, "binary"))

  ## Same build: nothing happens
  rdev_install(root)
  expect_identical(db_todo_status(root)$status, "done")
  expect_true(file.exists(file.path(cache, "binary")))

  ## Other build: old results dropped, package re-queued, cache cleared
  fingerprint <- "build B"
  expect_message(rdev_install(root), "checked again")
  expect_identical(db_metadata_get(root, "r_old_fingerprint"), "build B")
  expect_identical(db_get_results(root, NULL)$old$package, character())
  expect_identical(db_todo(root), "foo")
  expect_false(file.exists(file.path(cache, "binary")))
})

test_that("rdev_check() re-verifies the builds when resuming a run", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)
  db_metadata_set(root, "todo", "run")

  installed <- FALSE
  local_mocked_bindings(
    rdev_install = function(root, quiet) {
      installed <<- TRUE
      db_metadata_set(root, "todo", "done")
    }
  )

  rdev_check(root)
  expect_true(installed)
})

test_that("worker processes run with the old build in rdev mode", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)
  rdev <- rdev_options(root)

  opts <- download_opts(root, "foo", bioc = FALSE, cran = TRUE, rdev = rdev)
  expect_identical(opts$arch, rdev$r_old)
  expect_identical(opts$libpath, rdev$tools)
  expect_identical(opts$env[["CRANCACHE_DIR"]], rdev$cache)
  expect_identical(opts$env[["R_LIBS_USER"]], rdev$tools)

  ## Ordinary revdep checks are untouched
  opts <- download_opts(root, "foo", bioc = FALSE, cran = TRUE)
  expect_identical(opts$arch, "same")
  expect_false("CRANCACHE_DIR" %in% names(opts$env))
})

test_that("revdep_check() refuses an rdev root", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  expect_error(revdep_check(root), "use `rdev_check\\(\\)`")
})

test_that("reusing an old result still installs dependencies first", {
  ## The reuse decision is taken after download, and download is only
  ## scheduled once the dependencies are installed
  state <- list(
    options = list(num_workers = 1, rdev = list(reuse_old = TRUE)),
    workers = list(),
    packages = data.frame(
      package = "foo",
      state = "todo",
      stringsAsFactors = FALSE
    )
  )
  expect_identical(schedule_next_task(state)$name, "deps_install")

  state$packages$state <- "deps_installed"
  expect_identical(schedule_next_task(state)$name, "download")

  state$packages$state <- "done-downloaded"
  task <- schedule_next_task(state)
  expect_identical(task$name, "check")
  expect_identical(task$args[[2]], "new")
})

test_that("rdev_reset() keeps the tools library and its fingerprint", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  tools <- dir_find(root, "tools")

  expect_null(rdev_fingerprint_read(tools))
  dir_create(tools)
  writeLines("some build", rdev_fingerprint_path(tools))
  expect_identical(rdev_fingerprint_read(tools), "some build")

  rdev_reset(root)

  expect_false(file.exists(dir_find(root, "db")))
  expect_identical(rdev_fingerprint_read(tools), "some build")
})

test_that("the package cache is cleared when the old build changes", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  cache <- dir_find(root, "cache")

  ## First use: created and stamped
  rdev_check_cache(root, NULL, "build A")
  expect_identical(rdev_fingerprint_read(cache), "build A")

  ## Same build: contents are kept
  writeLines("x", file.path(cache, "binary"))
  rdev_check_cache(root, NULL, "build A")
  expect_true(file.exists(file.path(cache, "binary")))

  ## Other build: cleared and re-stamped
  expect_message(rdev_check_cache(root, NULL, "build B"), "Clearing")
  expect_false(file.exists(file.path(cache, "binary")))
  expect_identical(rdev_fingerprint_read(cache), "build B")

  ## A shared cache is only warned about, on every run until it is cleared
  shared <- tempfile("shared-cache-")
  rdev_check_cache(root, shared, "build A")
  writeLines("x", file.path(shared, "binary"))
  expect_warning(rdev_check_cache(root, shared, "build B"), "not cleared")
  expect_warning(rdev_check_cache(root, shared, "build B"), "not cleared")
  expect_true(file.exists(file.path(shared, "binary")))
  expect_identical(rdev_fingerprint_read(shared), "build A")
})

test_that("check_done() snapshots dependencies before the library is removed", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  lib <- dir_find(root, "pkg", "foo")
  fake_installed(lib, "dep", "2.0")
  db_todo_add(root, "foo")

  check <- structure(
    list(
      package = "foo",
      version = "1.0",
      status = 0L,
      timeout = FALSE,
      errors = character(),
      warnings = character(),
      notes = character(),
      description = "Package: foo\nVersion: 1.0\nMaintainer: A B <a@b.com>\n"
    ),
    class = "rcmdcheck"
  )
  state <- fake_state(root, "checking-checking")

  ## Both checks run at once and the new one finishes first
  state <- check_done(state, fake_check_worker("new", check))
  expect_identical(state$packages$state, "checking-done")
  expect_true(dir.exists(lib))

  ## The old one finishing removes the library, after recording it
  capture.output(state <- check_done(state, fake_check_worker("old", check)))
  expect_identical(state$packages$state, "done")
  expect_false(dir.exists(lib))

  old <- checkFromJSON(db_get_results(root, "foo")$old$result)
  expect_identical(old$libraries, "dep@2.0")
  expect_identical(db_todo_status(root)$status, "done")
})

test_that("rdev_invalidate_old() drops old results and re-queues done packages", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  db_todo_add(root, "foo")
  for (which in c("old", "new")) {
    db_insert(
      root,
      "foo",
      version = "1.0",
      status = "OK",
      which = which,
      duration = 1,
      starttime = Sys.time(),
      result = "{}",
      summary = NULL
    )
  }
  expect_identical(db_todo_status(root)$status, "done")

  rdev_invalidate_old(root)

  expect_identical(db_get_results(root, NULL)$old$package, character())
  expect_identical(db_get_results(root, NULL)$new$package, "foo")
  expect_identical(db_todo(root), "foo")
})

test_that("results from an rdev root are tagged for the reports", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  check <- structure(
    list(
      package = "foo",
      version = "1.0",
      status = 0L,
      timeout = FALSE,
      errors = character(),
      warnings = character(),
      notes = "N1"
    ),
    class = "rcmdcheck"
  )
  for (which in c("old", "new")) {
    db_insert(
      root,
      "foo",
      version = "1.0",
      status = "NOTE",
      which = which,
      duration = 1,
      starttime = Sys.time(),
      result = unclass(toJSON(check)),
      summary = NULL
    )
  }

  results <- db_results(root, NULL)
  expect_identical(results$foo$new$type, "rdev")
  expect_identical(side_labels(results$foo), list(old = "Old R", new = "New R"))
  expect_identical(
    side_labels(list(new = list(type = NULL))),
    list(old = "CRAN", new = "Devel")
  )
})

test_that("rdev_packages() resolves selections against the repositories", {
  skip_on_cran()
  skip_if_offline()

  expect_identical(rdev_packages(), character())

  expect_warning(
    pkgs <- rdev_packages(c("prettyunits", "notarealpackage123")),
    "notarealpackage123"
  )
  expect_identical(pkgs, "prettyunits")

  pkgs <- rdev_packages(revdeps_of = "prettyunits")
  expect_true("prettyunits" %in% pkgs)
  expect_gt(length(pkgs), 1)
  expect_identical(pkgs, pkgs[order(tolower(pkgs))])
})

test_that("rdev_check_process runs R CMD check with the given R executable", {
  skip_on_cran()
  skip_on_os("windows")

  pkg <- tempfile("rdevtest")
  dir.create(pkg)
  writeLines(
    c(
      "Package: rdevtest",
      "Version: 0.0.1",
      "Title: Test Package",
      "Description: A test package.",
      "License: MIT + file LICENSE",
      "Encoding: UTF-8",
      "Authors@R: person('A', 'B', email = 'a@b.com', role = c('aut', 'cre'))"
    ),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines(
    c("YEAR: 2026", "COPYRIGHT HOLDER: A B"),
    file.path(pkg, "LICENSE")
  )
  writeLines("", file.path(pkg, "NAMESPACE"))
  tarball <- pkgbuild::build(pkg, dest_path = tempdir(), quiet = TRUE)

  out <- tempfile("rdevtest-out")
  dir.create(out)

  ## A wrapper around the current R that records that it was the one used
  marker <- tempfile("rdevtest-marker")
  wrapper <- tempfile("fake-R-")
  writeLines(
    c(
      "#!/bin/sh",
      paste0("echo used > '", marker, "'"),
      paste0("exec '", file.path(R.home("bin"), "R"), "' \"$@\"")
    ),
    wrapper
  )
  Sys.chmod(wrapper, "0755")

  px <- rdev_check_process$new(
    tarball,
    args = c("--no-manual", "-o", out),
    rbin = wrapper
  )
  on.exit(px$kill(), add = TRUE)

  while (px$is_alive()) {
    px$poll_io(1000)
    px$read_output()
  }

  res <- px$parse_results()
  expect_true(file.exists(marker))
  expect_s3_class(res, "rcmdcheck")
  expect_identical(res$package, "rdevtest")
  expect_identical(res$version, "0.0.1")
  expect_length(res$errors, 0)
  ## rcmdcheck does not parse devel version strings into `rversion`, so look
  ## at the check output itself
  expect_match(res$stdout, paste0("using ", R.version.string), fixed = TRUE)
  expect_true(file.exists(file.path(out, "rdevtest.Rcheck", "00check.log")))
})
