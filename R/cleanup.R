
cleanup_library <- function(state, worker) {
  pkgdir <- state$options$pkgdir
  package <- worker$package
  if (is.null(pkgdir) || is.null(package)) return()

  libdir <- dir_find(pkgdir, "pkg", package)
  unlink(libdir, recursive = TRUE)
}
