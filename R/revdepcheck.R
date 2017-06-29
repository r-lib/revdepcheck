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
#'
#' @export
#' @importFrom remotes install_local
#' @importFrom withr with_libpaths with_envvar
#' @importFrom crancache install_packages

revdep_check <- function(pkg = ".", dependencies = c("Depends", "Imports",
                                      "Suggests", "LinkingTo"),
                         quiet = TRUE,
                         timeout = as.difftime(10, units = "mins"),
                         num_workers = 1, bioc = TRUE) {

  pkg <- pkg_check(pkg)

  ## Creates and initializes database, including computing revdeps
  if (!db_exists(pkg))
    revdep_setup(pkg, dependencies = dependencies, bioc = bioc)

  ## Install CRAN and dev versions
  if (!pkglib_exists(pkg))
    revdep_install(pkg, quiet = quiet)

  ## Resume also works from an empty table
  if (length(db_todo(pkg)) > 0)
    revdep_resume(pkg, quiet = quiet, timeout = timeout, num_workers = num_workers)

  revdep_clean(pkg)
}

revdep_setup <- function(pkg = ".",
                         dependencies = c("Depends", "Imports",
                                          "Suggests", "LinkingTo"),
                         bioc = TRUE) {

  pkg <- pkg_check(pkg)

  dir_setup(pkg)

  db_setup(pkg)              # Make sure it exists
  db_clean(pkg)              # Delete all records

  "!DEBUG getting reverse dependencies for `basename(pkg)`"
  pkgname <- pkg_name(pkg)
  message("Determining revdeps for ", pkgname)
  revdeps <- cran_revdeps(pkgname, dependencies, bioc = bioc)
  db_todo_add(pkg, revdeps)
}

revdep_install <- function(pkg = ".", quiet = FALSE) {
  pkg <- pkg_check(pkg)

  message(rule(center = "INSTALL", line_color = "black"))

  dir_create(dir_find(pkg, "old"))
  dir_create(dir_find(pkg, "new"))

  ## Install the package itself, both versions, first the CRAN version
  ## We instruct crancache to only use the cache of CRAN packages
  ## (to avoid installing locally installed newer versions.
  "!DEBUG Installing CRAN (old) version"
  message("Installing CRAN version of package")
  package_name <- pkg_name(pkg)[[1]]

  with_envvar(
    c(CRANCACHE_REPOS = "cran,bioc"),
    with_libpaths(
      dir_find(pkg, "old"),
      install_packages(package_name, quiet = quiet)
    )
  )

  ## Now the new version
  "!DEBUG Installing new version from `pkg`"
  message("Installing DEV version of package")
  with_envvar(
    c(CRANCACHE_REPOS = "cran,bioc"),
    with_libpaths(
      dir_find(pkg, "new"),
      install_local(pkg, quiet = quiet)
    )
  )
}

pkglib_exists <- function(pkgdir) {
  file.exists(dir_find(pkgdir, "old")) && file.exists(dir_find(pkgdir, "new"))
}

revdep_resume <- function(pkg = ".", quiet = TRUE,
                          timeout = as.difftime(10, units = "mins"),
                          num_workers = 1, bioc = TRUE) {

  pkg <- pkg_check(pkg)
  pkgname <- pkg_name(pkg)
  message(center = rule(center = "REVDEP CHECKS", line_color = "black"))

  todo <- db_todo(pkg)

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

  if (length(todo)) {
    message("Starting checks")
    run_event_loop(state)
  } else {
    message("All reverse dependencies were checked already")
  }

  invisible(revdep_results(pkg))
}

check_existing_checks <- function(pkg) {
  length(db_list(pkg)) != 0
}

revdep_clean <- function(pkg = ".") {
  pkg <- pkg_check(pkg)
  message(center = rule(center = "REVDEP CHECKS", line_color = "black"))

  # Delete local installs
  unlink(dir_find(pkg, "library"), recursive = TRUE)

  # Delete all sources/binaries cached by R CMD check
  check_dir <- dir_find(pkg, "checks")
  package <- dir(check_dir)
  rcheck <- c(
    file.path(check_dir, package, "new", paste0(package, ".Rcheck")),
    file.path(check_dir, package, "old", paste0(package, ".Rcheck"))
  )

  unlink(file.path(rcheck, "00_pkg_src"), recursive = TRUE)
  unlink(file.path(rcheck, package), recursive = TRUE)
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
}


#' @export

revdep_add <- function(pkg = ".", packages) {
  pkg <- pkg_check(pkg)
  db_todo_add(pkg, packages)
}

#' @export

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
