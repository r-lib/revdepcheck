#' Check CRAN packages with two builds of R
#'
#' @description
#' The `rdev_*()` functions run `R CMD check` on a set of CRAN packages with
#' two builds of R and report the differences. The typical use is testing a
#' local change to R itself: build R once from the unmodified sources (the
#' *old* build) and once with your change applied (the *new* build), then
#' check the packages most likely to be affected. Packages that pass with the
#' old build and fail with the new one point at regressions caused by the
#' change.
#'
#' This reuses the [revdep_check()] machinery, with the two R builds taking
#' the place of the CRAN and development versions of a package. Results and
#' reports have the same shape.
#'
#' @details
#' A check is driven from a *root* directory, created by `rdev_init()`. It
#' holds the configuration (`rdev.yml`), the results database, the package
#' libraries and check output, and the reports. The usual workflow is:
#'
#' 1. `rdev_init()` records the paths of the two R binaries.
#' 2. `rdev_add()` selects the packages to check: an explicit list, the
#'    reverse dependencies of some packages, or both.
#' 3. `rdev_check()` installs the dependencies of each package into a
#'    private library, checks it with both builds, and writes the reports
#'    (`README.md`, `problems.md` and `failures.md`) to the root.
#'
#' After changing and rebuilding the new R, call `rdev_add_broken()` (or
#' `rdev_add()`) and `rdev_check()` again. With `reuse_old = TRUE` the
#' results of the old build are kept, and only the new build is re-run. If
#' the old build itself changes (its path, version, svn revision, or the
#' modification time of its binaries or base packages), all packages are
#' checked again and the tools library is rebuilt.
#'
#' Both builds share the dependency libraries, which are installed by the
#' old build. This assumes the two builds have the same `major.minor`
#' version and a compatible ABI. A change to R's C API, byte-code or
#' serialization format breaks that assumption and makes the comparison
#' unreliable.
#'
#' Dependencies are installed with the crancache package, which the old
#' build must be able to load. The first `rdev_check()` installs crancache
#' (from its GitHub repository, it is not on CRAN) and its dependencies into
#' a private tools library in the root, using the old build. Built package
#' binaries are cached per root by default; use `cache_dir` to share a cache
#' between roots that use the same version of R.
#'
#' `rdev_summary()` and `rdev_details()` show results while a check is
#' running in another session, like [revdep_summary()] and
#' [revdep_details()]. `rdev_reset()` removes the results, libraries and
#' reports, but keeps the tools library and the package cache.
#'
#' @param root Path to the root directory of the check.
#' @param r_old,r_new Paths to the `R` executables of the old (baseline) and
#'   new (patched) builds, or to their R home or build directories. A build
#'   tree can be used directly, no `make install` is needed.
#' @param bioc Also consider Bioconductor packages when looking up reverse
#'   dependencies and installing dependencies?
#' @param cache_dir Directory for the crancache package cache. Defaults to
#'   `crancache/` in the root. Do not share it between roots that use
#'   different `major.minor` versions of R, cached binaries are not keyed by
#'   R version.
#' @param packages Character vector of package names to check.
#' @param revdeps_of Character vector of package names. These packages and
#'   all their reverse dependencies are checked.
#' @param dependencies Which types of reverse dependencies to include for
#'   `revdeps_of`, see [cran_revdeps()].
#' @param reuse_old Reuse results of the old build that are already in the
#'   database? A result is only reused if the package version and the
#'   versions of all installed dependencies are the same as when it was
#'   produced. Set to `FALSE` to check every package with both builds.
#' @param package Name of a checked package.
#' @inheritParams revdep_check
#' @inheritParams revdep_add
#' @inheritParams revdep_report_summary
#' @return `rdev_init()` returns the normalized `root`, invisibly.
#'   `rdev_add()`, `rdev_add_broken()`, `rdev_rm()` and `rdev_todo()`
#'   return the to-do list.
#'
#' @examples
#' \dontrun{
#' root <- "~/rcheck/my-branch"
#' rdev_init(
#'   root,
#'   r_old = "~/r/build-trunk/bin/R",
#'   r_new = "~/r/build-my-branch/bin/R"
#' )
#'
#' # Explicit packages, plus everything that depends on Rcpp
#' rdev_add(root, packages = c("data.table", "dplyr"), revdeps_of = "Rcpp")
#'
#' # Or a random sample of CRAN
#' rdev_add(root, packages = sample(rownames(available.packages()), 100))
#'
#' rdev_check(root, num_workers = 8)
#'
#' # Rebuild the new R, then re-check what broke
#' rdev_add_broken(root)
#' rdev_check(root, num_workers = 8)
#' }
#' @name rdev_check
NULL

#' @export
#' @rdname rdev_check

rdev_init <- function(root, r_old, r_new, bioc = FALSE, cache_dir = NULL) {
  assert_that(is_string(root), is_string(r_old), is_string(r_new))

  r_old <- rdev_r_binary(r_old)
  r_new <- rdev_r_binary(r_new)
  if (!is.null(cache_dir)) {
    cache_dir <- normalizePath(path.expand(cache_dir), mustWork = FALSE)
  }

  root <- path.expand(root)
  if (file.exists(file.path(root, "DESCRIPTION"))) {
    stop("`root` must not be a package directory", call. = FALSE)
  }
  dir_create(root)
  root <- normalizePath(root, mustWork = TRUE)

  config <- list(
    r_old = r_old,
    r_new = r_new,
    bioc = bioc,
    cache_dir = cache_dir
  )
  rdev_config_write(root, config)

  dir_setup(root)
  if (!db_exists(root)) {
    db_setup(root)
  }
  db_metadata_set(root, "todo", "install")

  invisible(root)
}

#' @export
#' @rdname rdev_check

rdev_check <- function(
  root = ".",
  quiet = TRUE,
  timeout = as.difftime(10, units = "mins"),
  num_workers = 1,
  env = revdep_env_vars(),
  reuse_old = TRUE
) {
  root <- rdev_root(root)
  config <- rdev_config(root)

  dir_setup(root)
  if (!db_exists(root)) {
    db_setup(root)
  }

  ## An interrupted run resumes through the install stage, so that an old
  ## build rebuilt in the meantime is noticed before its results are reused
  if (identical(db_metadata_get(root, "todo"), "run")) {
    db_metadata_set(root, "todo", "install")
  }

  repeat {
    stage <- db_metadata_get(root, "todo") %|0|% "install"
    switch(
      stage,
      init = ,
      install = rdev_install(root, quiet = quiet),
      run = revdep_run(
        root,
        quiet = quiet,
        timeout = timeout,
        num_workers = num_workers,
        bioc = config$bioc,
        env = env,
        rdev = rdev_options(root, reuse_old = reuse_old)
      ),
      report = revdep_final_report(root, bioc = config$bioc),
      done = break
    )
  }

  invisible()
}

#' @export
#' @rdname rdev_check

rdev_add <- function(
  root = ".",
  packages = NULL,
  revdeps_of = NULL,
  dependencies = c("Depends", "Imports", "Suggests", "LinkingTo")
) {
  root <- rdev_root(root)
  config <- rdev_config(root)

  packages <- rdev_packages(
    packages,
    revdeps_of,
    dependencies = dependencies,
    bioc = config$bioc
  )
  if (!length(packages)) {
    message("No packages to add")
    return(invisible(rdev_todo(root)))
  }

  message(
    "Adding ",
    length(packages),
    " package(s) to the TODO list, run `rdev_check()` to check them"
  )
  db_todo_add(root, packages)
  db_metadata_set(root, "todo", "install")

  invisible(rdev_todo(root))
}

#' @export
#' @rdname rdev_check

rdev_add_broken <- function(
  root = ".",
  install_failures = FALSE,
  timeout_failures = FALSE
) {
  root <- rdev_root(root)

  results <- db_results(root, NULL)
  broken <- map_lgl(results, is_broken, install_failures, timeout_failures)

  to_add <- names(broken[broken])
  if (length(to_add) == 0) {
    message("No broken packages to re-check")
    return(invisible(rdev_todo(root)))
  }

  rdev_add(root, packages = to_add)
}

#' @export
#' @rdname rdev_check

rdev_rm <- function(root = ".", packages) {
  root <- rdev_root(root)
  db_todo_rm(root, packages)

  invisible(rdev_todo(root))
}

#' @export
#' @rdname rdev_check

rdev_todo <- function(root = ".") {
  db_todo_status(rdev_root(root))
}

#' @export
#' @rdname rdev_check

rdev_reset <- function(root = ".") {
  root <- rdev_root(root)

  db_disconnect(root)

  unlink(dir_find(root, "lib"), recursive = TRUE)
  unlink(dir_find(root, "checks"), recursive = TRUE)
  unlink(dir_find(root, "db"), recursive = TRUE)
  unlink(file.path(root, c("README.md", "problems.md", "failures.md")))

  invisible()
}

#' @export
#' @rdname rdev_check

rdev_summary <- function(root = ".") {
  structure(
    db_results(rdev_root(root), NULL),
    class = "revdepcheck_results"
  )
}

#' @export
#' @rdname rdev_check

rdev_details <- function(root = ".", package) {
  assert_that(is_string(package))

  structure(
    db_results(rdev_root(root), package)[[1]],
    class = "revdepcheck_details"
  )
}

#' @export
#' @rdname rdev_check

rdev_report <- function(root = ".", all = FALSE) {
  root <- rdev_root(root)
  revdep_report(root, all = all, bioc = rdev_config(root)$bioc)
}

# Stages ------------------------------------------------------------------

rdev_install <- function(root, quiet = TRUE) {
  config <- rdev_config(root)
  status("INSTALL", "R builds")

  old <- rdev_r_info(config$r_old)
  new <- rdev_r_info(config$r_new)
  message("Old R: ", old$version, "\n  ", config$r_old)
  message("New R: ", new$version, "\n  ", config$r_new)

  if (old$minor != new$minor) {
    warning(
      "The two R builds have different major.minor versions (",
      old$minor,
      " vs ",
      new$minor,
      "). They cannot share package libraries, so the results ",
      "are not reliable.",
      call. = FALSE
    )
  }

  ## Results from a different baseline are not comparable, start over
  previous <- db_metadata_get(root, "r_old_fingerprint")
  if (length(previous) && previous != old$fingerprint) {
    message("The old R build has changed, all packages will be checked again")
    rdev_invalidate_old(root)
  }

  rdev_check_cache(root, config$cache_dir, old$fingerprint)

  db_metadata_set(root, "r_old", config$r_old)
  db_metadata_set(root, "r_new", config$r_new)
  db_metadata_set(root, "r_old_version", old$version)
  db_metadata_set(root, "r_new_version", new$version)
  db_metadata_set(root, "r_old_fingerprint", old$fingerprint)
  db_metadata_set(root, "r_new_fingerprint", new$fingerprint)

  rdev_install_tools(root, config$r_old, old, quiet = quiet)
  rdev_warm_cache(root)

  db_metadata_set(root, "todo", "run")
  invisible()
}

## Several workers initializing an empty crancache at once trip over each
## other (half-written metadata files, "table packages already exists"), so
## create its repositories and fetch the repository metadata once, serially,
## with the R build that will use the cache.
rdev_warm_cache <- function(root) {
  rdev <- rdev_options(root)

  func <- function(repos) {
    invisible(crancache::available_packages(repos = repos))
  }

  ## The same repositories the dependency installs use, see deps_opts()
  callr::r(
    func,
    args = list(repos = get_repos(bioc = TRUE, cran = TRUE)),
    arch = rdev$r_old,
    libpath = rdev$tools,
    env = c(
      CRANCACHE_REPOS = "cran,bioc",
      CRANCACHE_QUIET = "yes",
      rdev_cache_env(rdev),
      rdev_lib_env(rdev$tools)
    ),
    system_profile = FALSE,
    user_profile = FALSE
  )

  invisible()
}

rdev_invalidate_old <- function(root) {
  con <- db(root)

  dbExecute(con, "DELETE FROM revdeps WHERE which = 'old'")

  done <- dbGetQuery(con, "SELECT package FROM todo WHERE status = 'done'")
  if (nrow(done)) {
    db_todo_add(root, done$package)
  }

  invisible()
}

## crancache (and withr, used by the install code) must be loadable by the R
## build that installs the dependencies, so they get a private library
## built by that R, and rebuilt whenever that build changes. The fingerprint
## of that build lives next to the library, which rdev_reset() keeps.
rdev_install_tools <- function(root, rbin, info, quiet = TRUE) {
  tools <- dir_find(root, "tools")
  same_build <- identical(rdev_fingerprint_read(tools), info$fingerprint)
  if (same_build && identical(rdev_tools_built_for(tools), info$minor)) {
    return(invisible())
  }

  message("Installing crancache for the old R build into ", tools)
  unlink(tools, recursive = TRUE)
  dir_create(tools)

  ## crancache is not on CRAN, so its dependencies come from CRAN and
  ## crancache itself from a GitHub source tarball. The tarball is unpacked
  ## here because its extra pax header entry confuses desc and R CMD INSTALL.
  targz <- tempfile("crancache-", fileext = ".tar.gz")
  exdir <- tempfile("crancache-")
  on.exit(unlink(c(targz, exdir), recursive = TRUE), add = TRUE)
  curl::curl_download(crancache_url, targz, quiet = TRUE)
  utils::untar(targz, exdir = exdir)
  src <- dirname(list.files(
    exdir,
    "^DESCRIPTION$",
    recursive = TRUE,
    full.names = TRUE
  )[1])

  deps <- desc::desc(file = src)$get_deps()
  deps <- deps$package[deps$type %in% c("Depends", "Imports", "LinkingTo")]
  deps <- setdiff(deps, c("R", base_packages()))

  func <- function(lib, repos, deps, src, quiet) {
    utils::install.packages(deps, lib = lib, repos = repos, quiet = quiet)
    utils::install.packages(
      src,
      lib = lib,
      repos = NULL,
      type = "source",
      quiet = quiet
    )

    packages <- c("crancache", "withr")
    missing <- setdiff(packages, rownames(utils::installed.packages(lib)))
    if (length(missing)) {
      stop("Failed to install: ", paste(missing, collapse = ", "))
    }
  }

  callr::r(
    func,
    args = list(
      lib = tools,
      repos = get_repos(bioc = FALSE, cran = TRUE),
      deps = c(deps, "withr"),
      src = src,
      quiet = quiet
    ),
    arch = rbin,
    libpath = tools,
    env = rdev_lib_env(tools),
    system_profile = FALSE,
    user_profile = FALSE,
    show = !quiet
  )

  writeLines(info$fingerprint, rdev_fingerprint_path(tools))
  invisible()
}

## The binaries in the package cache were built by the old R build, so the
## default per-root cache is cleared when that build changes. Its fingerprint
## is kept in the cache directory itself, which rdev_reset() leaves alone.
## A user-supplied cache may be shared with other roots, so only warn.
rdev_check_cache <- function(root, cache_dir, fingerprint) {
  if (!is.null(cache_dir)) {
    previous <- rdev_fingerprint_read(cache_dir)
    if (!is.null(previous) && previous != fingerprint) {
      ## Keep the old stamp, so the warning repeats until the cache is cleared
      warning(
        "The old R build has changed, but the package cache ",
        cache_dir,
        " is shared and was not cleared. It may hold binaries built by ",
        "the previous build. Remove it to silence this warning.",
        call. = FALSE
      )
      return(invisible())
    }
    dir_create(cache_dir)
    writeLines(fingerprint, rdev_fingerprint_path(cache_dir))
    return(invisible())
  }

  cache <- dir_find(root, "cache")
  if (
    dir.exists(cache) && !identical(rdev_fingerprint_read(cache), fingerprint)
  ) {
    message("Clearing the package cache built by the previous old R build")
    unlink(cache, recursive = TRUE)
  }
  dir_create(cache)
  writeLines(fingerprint, rdev_fingerprint_path(cache))

  invisible()
}

rdev_fingerprint_path <- function(dir) {
  file.path(dir, "fingerprint")
}

rdev_fingerprint_read <- function(dir) {
  path <- rdev_fingerprint_path(dir)
  if (!file.exists(path)) {
    return(NULL)
  }
  readLines(path, n = 1, warn = FALSE)
}


crancache_url <- "https://github.com/r-lib/crancache/archive/HEAD.tar.gz"

rdev_tools_built_for <- function(tools) {
  if (!dir.exists(tools)) {
    return(NULL)
  }

  installed <- installed.packages(tools)
  if (!all(c("crancache", "withr") %in% rownames(installed))) {
    return(NULL)
  }

  ## The Built field looks like "R 4.7.0; ; 2026-02-20 10:00:00 UTC; unix"
  built <- installed["crancache", "Built"]
  regmatches(built, regexpr("[0-9]+[.][0-9]+", built))
}

# Helpers -----------------------------------------------------------------

rdev_config_path <- function(root) {
  file.path(root, "rdev.yml")
}

is_rdev <- function(dir) {
  is_string(dir) && file.exists(rdev_config_path(dir))
}

rdev_config_write <- function(root, config) {
  yaml::write_yaml(compact(config), rdev_config_path(root))
}

rdev_config <- function(root) {
  yaml::read_yaml(rdev_config_path(root))
}

rdev_root <- function(root) {
  root <- pkg_check(root)
  if (!is_rdev(root)) {
    stop(
      "`root` is not a directory created by `rdev_init()`: ",
      root,
      call. = FALSE
    )
  }
  root
}

## What the worker processes need to know about the R builds. This is
## `state$options$rdev` in the event loop, NULL for ordinary revdep checks.
rdev_options <- function(root, reuse_old = TRUE) {
  config <- rdev_config(root)
  list(
    r_old = config$r_old,
    r_new = config$r_new,
    tools = dir_find(root, "tools"),
    cache = config$cache_dir %||% dir_find(root, "cache"),
    reuse_old = reuse_old
  )
}

rdev_cache_env <- function(rdev) {
  if (is.null(rdev)) {
    character()
  } else {
    c(CRANCACHE_DIR = rdev$cache)
  }
}

## callr sets .libPaths() of the child from `libpath`, but leaves the
## R_LIBS_USER and R_LIBS_SITE of this session in its environment. The R
## processes the child starts (R CMD INSTALL, R CMD check) would pick up
## this session's libraries through them, and packages built by another R
## crash when loaded into a devel build of the same major.minor. Point
## both at the libraries meant for that R.
rdev_lib_env <- function(lib) {
  lib <- paste(lib, collapse = .Platform$path.sep)
  c(R_LIBS_USER = lib, R_LIBS_SITE = lib)
}

rdev_r_binary <- function(path) {
  path <- path.expand(path)

  ## Accept an R home or build directory as well
  if (dir.exists(path) && file.exists(file.path(path, "bin", "R"))) {
    path <- file.path(path, "bin", "R")
  }

  if (!file.exists(path) || dir.exists(path)) {
    stop("R executable not found: ", path, call. = FALSE)
  }
  if (file.access(path, mode = 1) != 0) {
    stop("R executable is not executable: ", path, call. = FALSE)
  }

  normalizePath(path)
}

## Identity of an R build. The fingerprint is what decides whether results
## and the tools library made with an earlier build are still valid, so it
## covers more than the version string, which a rebuild of the same revision
## keeps, and a git mirror build may not know its svn revision at all. The
## newest modification time of the binaries and of the base packages'
## lazy-load databases moves for C-level and R-level changes respectively.
rdev_r_info <- function(rbin) {
  func <- function() {
    home <- R.home()
    base <- rownames(utils::installed.packages(.Library, priority = "base"))
    files <- c(
      file.path(home, "bin", "exec", "R"),
      list.files(file.path(home, "lib"), "^libR[.]", full.names = TRUE),
      file.path(home, "library", base, "R", paste0(base, ".rdb"))
    )
    files <- files[file.exists(files)]

    list(
      version = R.version.string,
      minor = paste(
        R.version$major,
        sub("[.].*$", "", R.version$minor),
        sep = "."
      ),
      svn = R.version[["svn rev"]],
      mtime = if (length(files)) max(file.info(files)$mtime) else NA
    )
  }

  info <- callr::r(
    func,
    arch = rbin,
    libpath = character(),
    system_profile = FALSE,
    user_profile = FALSE
  )

  mtime <- if (is.na(info$mtime)) file.info(rbin)$mtime else info$mtime
  info$fingerprint <- paste(
    rbin,
    info$version,
    info$svn,
    format(mtime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    sep = " | "
  )

  info
}

## Can the result of the old build be reused for this tarball? Only complete
## checks of the same package version, against the same dependency versions
## as installed now, qualify.
rdev_old_result_usable <- function(root, package, tarball) {
  old <- db_get_results(root, package)$old
  if (
    nrow(old) != 1 ||
      !old$status %in% c("OK", "NOTE", "WARNING", "ERROR") ||
      !identical(old$version, tarball_version(tarball))
  ) {
    return(FALSE)
  }

  ## Results without a snapshot predate the dependency gate
  check <- checkFromJSON(old$result)
  if (is.null(check$libraries)) {
    return(FALSE)
  }

  identical(
    sort(as.character(check$libraries)),
    rdev_library_snapshot(dir_find(root, "pkg", package))
  )
}

## Installed package versions of a library, in a stable order
rdev_library_snapshot <- function(lib) {
  if (!dir.exists(lib)) {
    return(character())
  }

  ## The library changes between checks, so bypass the per-session cache
  installed <- installed.packages(lib, noCache = TRUE)
  if (!nrow(installed)) {
    return(character())
  }

  sort(paste0(installed[, "Package"], "@", installed[, "Version"]))
}

tarball_version <- function(path) {
  sub("^.*_([^_]+)\\.tar\\.gz$", "\\1", basename(path))
}

## The default available.packages() filters minus R_version: whether a
## package needs a newer R than this session is irrelevant, the builds
## under test decide that
rdev_filters <- c("OS_type", "subarch", "duplicates")

## Resolve a package selection against the repositories. Unknown packages
## are dropped with a warning, so that a typo does not stall the run.
#' @importFrom utils available.packages
rdev_packages <- function(
  packages = NULL,
  revdeps_of = NULL,
  dependencies = c("Depends", "Imports", "Suggests", "LinkingTo"),
  bioc = FALSE
) {
  if (length(revdeps_of)) {
    revdeps <- cran_revdeps(
      revdeps_of,
      dependencies,
      bioc = bioc,
      filters = rdev_filters
    )
    packages <- c(packages, revdeps_of, revdeps)
  }
  packages <- unique(as.character(packages))
  if (!length(packages)) {
    return(character())
  }

  repos <- get_repos(bioc = bioc, cran = TRUE)
  available <- rownames(available.packages(
    repos = repos,
    filters = rdev_filters
  ))

  unknown <- setdiff(packages, available)
  if (length(unknown)) {
    warning(
      "Dropping package(s) not available in the repositories: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  packages <- intersect(packages, available)
  packages[order(tolower(packages))]
}

rdev_report_builds <- function(root) {
  data.frame(
    field = c("path", "version"),
    old = c(
      db_metadata_get(root, "r_old") %|0|% "",
      db_metadata_get(root, "r_old_version") %|0|% ""
    ),
    new = c(
      db_metadata_get(root, "r_new") %|0|% "",
      db_metadata_get(root, "r_new_version") %|0|% ""
    ),
    stringsAsFactors = FALSE
  )
}
