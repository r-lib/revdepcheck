
#' @export
#' @importFrom remotes install_local

revdepcheck <- function(package = ".", overwrite = FALSE) {

  package <- normalizePath(package)

  stopifnot(is_package_dir(package))

  if (!overwrite && check_existing_checks(package)) {
    stop("Reverse dependency results already exist, call\n",
         "revdepcheck() with `overwrite = TRUE`, or use\n",
         "revdepcheck$resume()")
  }

  ## Also creates it if needed
  revdepcheck$clean(package)

  ## Install the package itself
  install_local(package, lib = check_dir(package, "library"), quiet = TRUE)

  ## Resume also works from an empty table
  revdepcheck$resume(package)
}

class(revdepcheck) <- "revdepcheck_package"

`$.revdepcheck_package` <- function (x, name) {
  if (name %in% names(revdepcheck_functions)) {
    revdepcheck_functions[[name]]

  } else {
    stop("Unknown 'revdepcheck' function")
  }
}

revdepcheck_functions <- list(
  "results" = function(package = ".") { check_results(package) },
  "resume"  = function(package = ".") { check_resume(package) },
  "clean"   = function(package = ".") { check_clean(package) }
)

check_results <- function(package) {
  ## TODO
}

check_resume <- function(package) {

  revdeps <- cran_revdeps(get_package_name(package))
  done <- db_list(package)
  todo <- setdiff(revdeps, done)

  chkdir <- check_dir(package, "check")
  libdir <- check_dir(package, "library")
  for (pkg in todo) {
    message("Checking ", pkg)
    res <- check_cran_package(
      pkg, check_dir = chkdir, libdir = libdir, quiet = TRUE
    )
    db_insert(package, res)
  }
}

check_clean <- function(package) {
  db_setup(package)                     # Make sure it exists
  db_clean(package)                     # Delete all records
}

check_existing_checks <- function(package) {
  length(db_list(package)) != 0
}
