
#' @export
#' @importFrom remotes install_local

revdep_check <- function(package = ".", overwrite = FALSE, quiet = TRUE) {

  package <- normalizePath(package)

  stopifnot(is_package_dir(package))

  if (!overwrite && check_existing_checks(package)) {
    stop("Reverse dependency results already exist, call\n",
         "  revdep_check() with `overwrite = TRUE`, or use\n",
         "  revdep_resume()")
  }

  ## Also creates it if needed
  revdep_clean(package)

  ## Install the package itself
  install_local(package, lib = check_dir(package, "library"), quiet = quiet)

  ## Resume also works from an empty table
  revdep_resume(package, quiet = quiet)
}

revdep_results <- function(revdep) {
  ## TODO
}

#' @export

revdep_resume <- function(package, quiet = TRUE) {

  package <- normalizePath(package)

  revdeps <- cran_revdeps(get_package_name(package))
  done <- db_list(package)
  todo <- setdiff(revdeps, done)

  chkdir <- check_dir(package, "check")
  libdir <- check_dir(package, "library")
  for (pkg in todo) {
    message("Checking ", pkg)
    res <- check_cran_package(
      pkg, check_dir = chkdir, libdir = libdir, quiet = quiet
    )
    db_insert(package, res)
  }
}

revdep_clean <- function(package) {
  db_setup(package)              # Make sure it exists
  db_clean(package)              # Delete all records
}

check_existing_checks <- function(package) {
  length(db_list(package)) != 0
}
