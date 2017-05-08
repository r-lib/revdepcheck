
#' Summary of reverse dependency check results
#'
#' @family revdep check functions
#' @export

revdep_results <- function(pkg = ".", revdeps = NULL) {
  res <- db_results(pkg, revdeps)
  class(res) <- "revdepcheck_results"
  res
}

#' Details of reverse dependency check results
#'
#' @family revdep check functions
#' @export

revdep_details <- function(pkg = ".", revdeps = NULL) {
  db_details(pkg, revdeps)
}

#' Markdown report of reverse dependency check results
#' @family revdep check functions
#' @export

revdep_report <- function(pkg = ".",
                          revdeps = NULL) {

  details <- revdep_details(pkg, revdeps)
  ## TODO
}

#' Email reverse dependency maintainers about potential problems
#'
#' @family revdep check functions
#' @export

revdep_emails <- function(pkg = ".", revdeps = NULL) {
  addr <- db_maintainers(pkg, revdeps)
  results <- revdep_results(pkg, revdeps)
  TODO
}
