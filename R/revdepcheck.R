
#' @export
#' @importFrom remotes install_local
#' @importFrom withr with_libpaths with_envvar
#' @importFrom crancache install_packages
#' @importFrom curl curl

revdep_check <- function(pkg = ".", dependencies = c("Depends", "Imports",
                                      "Suggests", "LinkingTo"),
                         overwrite = FALSE, quiet = TRUE,
                         timeout = as.difftime(10, units = "mins"),
                         num_workers = 1, bioc = TRUE) {

  pkg <- normalizePath(pkg)
  stopifnot(is_package_dir(pkg))

  ## Creates if needed; stops if exists & !overwrite
  revdep_setup(pkg, overwrite = overwrite)

  ## Install CRAN and dev versions
  revdep_install(pkg, quiet = quiet)

  ## Resume also works from an empty table
  revdep_resume(pkg, dependencies = dependencies, quiet = quiet,
                timeout = timeout, num_workers = num_workers, bioc = bioc)
}

#' @export

revdep_setup <- function(pkg, overwrite = TRUE) {
  if (!overwrite && check_existing_checks(pkg)) {
    stop("Reverse dependency results already exist, call\n",
         "  revdep_check() with `overwrite = TRUE`, or use\n",
         "  revdep_resume()")
  }

  db_setup(pkg)              # Make sure it exists
  db_clean(pkg)              # Delete all records
}

#' @export

revdep_install <- function(pkg, quiet = FALSE) {

  message(rule(center = "INSTALL", line_color = "black"))

  ## Install the package itself, both versions, first the CRAN version
  ## We instruct crancache to only use the cache of CRAN packages
  ## (to avoid installing locally installed newer versions.
  "!DEBUG Installing CRAN (old) version"
  message("Installing CRAN version of package")
  with_envvar(
    c(CRANCACHE_REPOS = "cran,bioc"),
    with_libpaths(
      check_dir(pkg, "old"), {
        package_name <- get_package_name(pkg)[[1]]
        install_packages(package_name, quiet = quiet)
      }
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

#' @export

revdep_resume <- function(pkg = ".", dependencies = c("Depends", "Imports",
                                       "Suggests", "LinkingTo"),
                          quiet = TRUE,
                          timeout = as.difftime(10, units = "mins"),
                          num_workers = 1, bioc = TRUE) {

  pkg <- normalizePath(pkg)
  pkgname <- get_package_name(pkg)
  message(center = rule(center = "REVDEP CHECKS", line_color = "black"))

  "!DEBUG getting reverse dependencies for `basename(pkg)`"
  revdeps <- cran_revdeps(pkgname, dependencies, bioc = bioc)
  done <- db_list(pkg)
  todo <- setdiff(revdeps, done)

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
    run_event_loop(state)
  } else {
    message("All reverse dependencies were checked already:")
  }

  invisible(revdep_results(pkg))
}

check_existing_checks <- function(pkg) {
  length(db_list(pkg)) != 0
}
