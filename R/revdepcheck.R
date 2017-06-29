
#' @export
#' @importFrom remotes install_local
#' @importFrom withr with_libpaths with_envvar
#' @importFrom crancache install_packages
#' @importFrom curl curl

revdep_check <- function(pkg = ".", dependencies = c("Depends", "Imports",
                                      "Suggests", "LinkingTo"),
                         quiet = TRUE,
                         timeout = as.difftime(10, units = "mins"),
                         num_workers = 1, bioc = TRUE) {

  pkg <- normalizePath(pkg, mustWork = FALSE)
  stopifnot(is_package_dir(pkg))

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

#' @export

revdep_setup <- function(pkg,
                         dependencies = c("Depends", "Imports",
                                          "Suggests", "LinkingTo"),
                         bioc = TRUE) {

  db_setup(pkg)              # Make sure it exists
  db_clean(pkg)              # Delete all records

  "!DEBUG getting reverse dependencies for `basename(pkg)`"
  pkgname <- get_package_name(pkg)
  message("Determining revdeps for ", pkgname)
  revdeps <- cran_revdeps(pkgname, dependencies, bioc = bioc)
  db_todo_add(pkg, revdeps)
}

#' @export

revdep_install <- function(pkg, quiet = FALSE) {

  message(rule(center = "INSTALL", line_color = "black"))

  ## Install the package itself, both versions, first the CRAN version
  ## We instruct crancache to only use the cache of CRAN packages
  ## (to avoid installing locally installed newer versions.
  "!DEBUG Installing CRAN (old) version"
  message("Installing CRAN version of package")
  package_name <- get_package_name(pkg)[[1]]

  with_envvar(
    c(CRANCACHE_REPOS = "cran,bioc"),
    with_libpaths(
      check_dir(pkg, "old"),
      install_packages(package_name, quiet = quiet)
    )
  )

  ## Now the new version
  "!DEBUG Installing new version from `pkg`"
  message("Installing DEV version of package")
  with_envvar(
    c(CRANCACHE_REPOS = "cran,bioc"),
    with_libpaths(
      check_dir(pkg, "new"),
      install_local(pkg, quiet = quiet)
    )
  )
}

pkglib_exists <- function(pkgdir) {
  pkg <- get_package_name(pkgdir)

  file.exists(file.path(pkgdir, "revdep", "library", pkg, "old")) &&
    file.exists(file.path(pkgdir, "revdep", "library", pkg, "new"))
}

#' @export

revdep_resume <- function(pkg = ".", quiet = TRUE,
                          timeout = as.difftime(10, units = "mins"),
                          num_workers = 1, bioc = TRUE) {

  pkg <- normalizePath(pkg, mustWork = FALSE)
  pkgname <- get_package_name(pkg)
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

#' @export

revdep_clean <- function(pkg = ".") {
  message(center = rule(center = "REVDEP CHECKS", line_color = "black"))

  # Delete local installs
  unlink(file.path(pkg, "revdep", "install"), recursive = TRUE)
  unlink(file.path(pkg, "revdep", "library"), recursive = TRUE)

  # Delete all sources/binaries cached by R CMD check
  check_dir <- file.path(pkg, "revdep", "checks")
  package <- dir(check_dir)
  rcheck <- c(
    file.path(check_dir, package, "new", paste0(package, ".Rcheck")),
    file.path(check_dir, package, "old", paste0(package, ".Rcheck"))
  )

  unlink(file.path(rcheck, "00_pkg_src"), recursive = TRUE)
  unlink(file.path(rcheck, package), recursive = TRUE)
}


#' @export

revdep_add <- function(pkg = ".", packages) {
  db_todo_add(pkg, packages)
}

#' @export

revdep_add_broken <- function(pkg = ".") {
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
