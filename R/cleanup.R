
cleanup_library <- function(state, worker) {
  pkgdir <- state$options$pkgdir
  package <- worker$package
  if (is.null(pkgdir) || is.null(package)) return()

  libdir <- dir_find(pkgdir, "pkg", package)
  unlink(libdir, recursive = TRUE)
}

cleanup_chkres <- function(state, worker, iam_old) {
  pkgdir <- state$options$pkgdir
  package <- worker$package

  # Delete all sources/binaries cached by R CMD check
  check_dir <- dir_find(pkgdir, "check", package)
  rcheck <- file.path(
    check_dir,
    package,
    if (iam_old) "old" else "new",
    paste0(package, ".Rcheck")
  )

  unlink(file.path(rcheck, "00_pkg_src"), recursive = TRUE)
  unlink(file.path(rcheck, package), recursive = TRUE)
}
