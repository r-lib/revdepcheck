
#' @export
#' @importFrom remotes install_local
#' @importFrom withr with_libpaths

revdep_check <- function(pkg = ".", dependencies = c("Depends", "Imports",
                                      "Suggests", "LinkingTo"),
                         overwrite = FALSE, quiet = TRUE,
                         timeout = as.difftime(10, units = "mins"),
                         num_workers = 1) {

  pkg <- normalizePath(pkg)

  stopifnot(is_package_dir(pkg))

  if (!overwrite && check_existing_checks(pkg)) {
    stop("Reverse dependency results already exist, call\n",
         "  revdep_check() with `overwrite = TRUE`, or use\n",
         "  revdep_resume()")
  }

  ## Also creates it if needed
  revdep_clean(pkg)

  ## Install the package itself
  with_libpaths(
    check_dir(pkg, "library"),
    install_local(pkg, quiet = quiet)
  )

  ## Resume also works from an empty table
  revdep_resume(pkg, dependencies = dependencies, quiet = quiet,
                timeout = timeout, num_workers = num_workers)
}

#' @export

revdep_resume <- function(pkg = ".", dependencies = c("Depends", "Imports",
                                       "Suggests", "LinkingTo"),
                          quiet = TRUE,
                          timeout = as.difftime(10, units = "mins"),
                          num_workers = 1) {

  pkg <- normalizePath(pkg)

  revdeps <- cran_revdeps(get_package_name(pkg), dependencies)
  done <- db_list(pkg)
  todo <- setdiff(revdeps, done)

  state <- list(
    options = list(
      chkdir = check_dir(pkg, "check"),
      libdir = check_dir(pkg, "library"),
      quiet = quiet,
      timeout = timeout,
      num_workers = num_workers),
    packages = data.frame(
      package = todo,
      state = "todo",
      stringsAsFactors = FALSE)
  )

  run_event_loop(state)
}

revdep_clean <- function(pkg) {
  db_setup(pkg)              # Make sure it exists
  db_clean(pkg)              # Delete all records
}

check_existing_checks <- function(pkg) {
  length(db_list(pkg)) != 0
}
