#' List maintainers of all reverse dependencies
#'
#' @inheritParams revdep_check
revdep_maintainers <- function(pkg) {
  m <- db_maintainers(pkg)[[1]]
  structure(m, class = "maintainers")
}

#' @export
print.maintainers <- function(x, ...) {
  cat_line(paste0(x, collapse = ",\n"))
}


revdep_emails <- function(pkg = ".", revdeps = NULL) {
  addr <- db_maintainers(pkg)
  results <- revdep_results(pkg)
  ## TODO
}
