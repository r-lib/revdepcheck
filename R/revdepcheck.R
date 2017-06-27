
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

  pkg <- normalizePath(pkg, mustWork = FALSE)
  stopifnot(is_package_dir(pkg))

  ## Creates and initializes database, including computing revdeps
  revdep_setup(pkg, overwrite = overwrite, dependencies = dependencies, bioc = bioc)

  ## Install CRAN and dev versions
  revdep_install(pkg, quiet = quiet)

  ## Resume also works from an empty table
  revdep_resume(pkg, quiet = quiet, timeout = timeout, num_workers = num_workers)
}

#' @export

revdep_setup <- function(pkg, overwrite = FALSE,
                         dependencies = c("Depends", "Imports",
                                          "Suggests", "LinkingTo"),
                         bioc = TRUE) {
  if (!overwrite && check_existing_checks(pkg)) {
    if (!interactive()) {
      stop("Reverse dependency results already exist, call\n",
         "  revdep_check() with `overwrite = TRUE`, or use\n",
         "  revdep_resume()", call. = FALSE)
    } else {
      choice <- menu(
        title = "Reverse dependency results already exist",
        c("Overwrite", "Resume")
      )

      if (choice == 2) {
        return()
      }
    }
  }

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
