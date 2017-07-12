#' Run revdep checks
#'
#' `revdep_check()` is designed to work even if interrupted. If for some reason
#' you need to stop the checks, or the session dies, just rerun `revdep_check()`
#' and it will resume from where it last stopped. To completed reset the
#' results of a previous run, use `revdep_reset()`.
#'
#' @param pkg Path to package
#' @param dependencies Which types of revdeps to check
#' @param quiet Suppress output from internal processes?
#' @param timeout Maximum time to wait (in seconds) for `R CMD check` to
#'   complete.
#' @param num_workers Number of parallel workers to use
#' @param bioc Also check revdeps in BioConductor?
#' @param warm_cache Warm the CRAN cache before performing the check?
#'   Useful for fetching all dependencies (and, on Linux, also compiling them)
#'   in a first run.
#'
#' @export
#' @importFrom remotes install_local
#' @importFrom withr with_libpaths with_envvar
#' @importFrom crancache install_packages

revdep_check <- function(pkg = ".", dependencies = c("Depends", "Imports",
                                      "Suggests", "LinkingTo"),
                         quiet = TRUE,
                         timeout = as.difftime(10, units = "mins"),
                         num_workers = 1, bioc = TRUE, warm_cache = FALSE) {

  pkg <- pkg_check(pkg)

  did_something <- FALSE
  ## Creates and initializes database, including computing revdeps
  if (!db_exists(pkg)) {
    revdep_setup(pkg, dependencies = dependencies, bioc = bioc)
    did_something <- TRUE
  }

  has_todo <- length(db_todo(pkg)) > 0

  ## Install CRAN and dev versions
  if (!pkglib_exists(pkg) && has_todo) {
    revdep_install(pkg, quiet = quiet)
    did_something <- TRUE
  }

  ## Run checks
  if (has_todo) {
    if (warm_cache) revdep_warm_cache(pkg)

    revdep_run_check(pkg, quiet = quiet, timeout = timeout, num_workers = num_workers)
    did_something <- TRUE
  }

  if (pkglib_exists(pkg)) {
    revdep_clean(pkg)
    did_something <- TRUE
  }

  if (!report_exists(pkg)) {
    revdep_report(pkg)
    did_something <- TRUE
  }

  if (!did_something) {
    message("Nothing happened. Do you need to run revdep_reset()?")
  }
}

revdep_setup <- function(pkg = ".",
                         dependencies = c("Depends", "Imports",
                                          "Suggests", "LinkingTo"),
                         bioc = TRUE) {
  pkg <- pkg_check(pkg)
  pkgname <- pkg_name(pkg)

  status("SETUP")

  message("Creating directories and database")
  dir_setup(pkg)
  db_setup(pkg)              # Make sure it exists
  db_clean(pkg)              # Delete all records

  "!DEBUG getting reverse dependencies for `basename(pkg)`"
  message("Computing revdeps")
  revdeps <- cran_revdeps(pkgname, dependencies, bioc = bioc)
  db_todo_add(pkg, revdeps)

  invisible()
}

revdep_install <- function(pkg = ".", quiet = FALSE) {
  pkg <- pkg_check(pkg)
  pkgname <- pkg_name(pkg)

  status("INSTALL", "2 versions")

  dir_create(dir_find(pkg, "old"))
  dir_create(dir_find(pkg, "new"))

  ## Install the package itself, both versions, first the CRAN version
  ## We instruct crancache to only use the cache of CRAN packages
  ## (to avoid installing locally installed newer versions.
  "!DEBUG Installing CRAN (old) version"
  message("Installing CRAN version of ", pkgname)
  package_name <- pkg_name(pkg)[[1]]

  with_envvar(
    c(CRANCACHE_REPOS = "cran,bioc", CRANCACHE_QUIET = "yes"),
    with_libpaths(
      dir_find(pkg, "old"),
      with_options(
        list(warn = 2),
        install_packages(pkgname, quiet = quiet, repos = get_repos(bioc = TRUE))
      )
    )
  )

  ## Now the new version
  "!DEBUG Installing new version from `pkg`"
  message("Installing DEV version of ", pkgname)
  with_envvar(
    c(CRANCACHE_REPOS = "cran,bioc", CRANCACHE_QUIET = "yes"),
    with_libpaths(
      dir_find(pkg, "new"),
      with_options(
        list(warn = 2),
        install_local(pkg, quiet = quiet)
      )
    )
  )

  # Record libraries
  lib <- library_compare(pkg)
  utils::write.csv(lib, file.path(pkg, "revdep", "checks", "libraries.csv"),
    row.names = FALSE, quote = FALSE)

  invisible()
}

pkglib_exists <- function(pkgdir) {
  file.exists(dir_find(pkgdir, "old")) && file.exists(dir_find(pkgdir, "new"))
}

#' @importFrom crancache install_packages

revdep_warm_cache <- function(pkg) {
  "!DEBUG warming up revdep cache"
  todo <- db_todo(pkg)
  args <- deps_opts(todo)

  package <- args$package
  '!DEBUG package = `paste(package, collapse = ", ")`'
  repos <- args$repos
  '!DEBUG repos = `paste(repos, collapse = ", ")`'


  withr::with_temp_libpaths({
    # Installing in chunks of size 10 for eager caching
    package_chunks <- split_size(package, 10)
    lapply(package_chunks, install_packages, repos = repos, dependencies = FALSE)
  })
}

split_size <- function(x, n) {
  split(x, trunc(seq(0, by = 1 / n, length.out = length(x))))
}

#' @importFrom prettyunits vague_dt

revdep_run_check <- function(pkg = ".", quiet = TRUE,
                          timeout = as.difftime(10, units = "mins"),
                          num_workers = 1, bioc = TRUE) {

  pkg <- pkg_check(pkg)
  pkgname <- pkg_name(pkg)

  todo <- db_todo(pkg)
  status("CHECK", paste0(length(todo), " packages"))
  start <- Sys.time()

  state <- list(
    options = list(
      pkgdir = pkg,
      pkgname = pkgname,
      quiet = quiet,
      timeout = timeout,
      num_workers = num_workers),
    packages = data.frame(
      package = todo,
      state = if (length(todo)) "todo" else character(),
      stringsAsFactors = FALSE)
  )

  run_event_loop(state)
  end <- Sys.time()

  status <- report_status(pkg)
  cat_line(green("OK: "), status$ok)
  cat_line(red("BROKEN: "), status$broken)
  cat_line("Total time: ", vague_dt(end - start, format = "short"))

  invisible()
}

check_existing_checks <- function(pkg) {
  length(db_list(pkg)) != 0
}

revdep_clean <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  status("CLEAN")

  # Delete local installs
  unlink(dir_find(pkg, "lib"), recursive = TRUE)

  # Delete all sources/binaries cached by R CMD check
  check_dir <- dir_find(pkg, "checks")
  package <- dir(check_dir)
  rcheck <- c(
    file.path(check_dir, package, "new", paste0(package, ".Rcheck")),
    file.path(check_dir, package, "old", paste0(package, ".Rcheck"))
  )

  unlink(file.path(rcheck, "00_pkg_src"), recursive = TRUE)
  unlink(file.path(rcheck, package), recursive = TRUE)

  invisible()
}

revdep_report <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  status("REPORT")

  root <- dir_find(pkg, "root")

  message("Writing summary to 'revdep/README.md'")
  revdep_report_summary(pkg, file = file.path(root, "README.md"))

  message("Writing problems to 'revdep/problems.md'")
  revdep_report_problems(pkg, file = file.path(root, "problems.md"))
}

report_exists <- function(pkg) {
  root <- dir_find(pkg, "root")
  file.exists(file.path(root, "README.md")) && file.exists(file.path(root, "problems.md"))
}

#' @export
#' @rdname revdep_check

revdep_reset <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  if (exists(pkg, envir = dbenv))
    rm(list = pkg, envir = dbenv)

  unlink(dir_find(pkg, "lib"), recursive = TRUE)
  unlink(dir_find(pkg, "checks"), recursive = TRUE)
  unlink(dir_find(pkg, "db"), recursive = TRUE)

  invisible()
}


#' Manually add packages to check
#'
#' Use `revdep_add()` to add a single package to the to do list. Use
#' `revdep_add_broken()` to add all broken packages from the last check
#' (this is useful if you think you've fixed the underlying problem in
#' your package)
#'
#' @inheritParams revdep_check
#' @param packages Character vector of package names to add
#' @export

revdep_add <- function(pkg = ".", packages) {
  pkg <- pkg_check(pkg)
  db_todo_add(pkg, packages)
}

#' @export
#' @rdname revdep_add

revdep_add_broken <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  packages <- revdep_results(pkg, db_list(pkg))
  broken <- vapply(packages, is_broken, integer(1))

  to_add <- db_list(pkg)[broken]
  if (length(to_add) == 0) {
    message("No broken packages to re-test")
  } else {
    message(
      "Re-checking broken packages: ",
      str_trunc(paste(to_add, collapse = ","), 100)
    )
    revdep_add(pkg, to_add)

  }

}


#' @importFrom crayon bold

status <- function(title, info = "") {
  cat_line(rule(left = bold(title), right = info))
}
