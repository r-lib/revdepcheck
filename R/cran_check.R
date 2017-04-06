
#' @importFrom rcmdcheck rcmdcheck
#' @importFrom withr with_libpaths
#' @importFrom crancache install.packages
#' @importFrom utils download.packages

check_cran_package <- function(package, check_dir = tempfile(),
                               libdir = file.path(check_dir, "library"),
                               quiet = FALSE) {

  stopifnot(is_string(package))

  libdir <- normalizePath(libdir)

  if (!file.exists(check_dir)) dir.create(check_dir)
  if (!file.exists(libdir)) dir.create(libdir)

  down_dir <- download_dir()
  if (!file.exists(down_dir)) dir.create(down_dir)

  ## To install dependencies
  with_libpaths(
    libdir,
    install.packages(
      dependencies = TRUE,
      package,
      lib = libdir,
      destdir = down_dir,
      quiet = quiet
    )
  )

  ## Download the source package
  spkg <- download.packages(
    package,
    down_dir,
    type = "source",
    quiet = quiet
  )[, 2]

  ## Check it, return the result
  rcmdcheck(spkg, libpath = libdir, quiet = quiet)
}
