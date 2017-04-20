
#' Summary of reverse dependency check results
#'
#' @family revdep check functions
#' @export

revdep_results <- function(pkg = ".", revdeps = NULL) {
  db_results(pkg, revdeps)
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
}
