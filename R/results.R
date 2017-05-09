
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
#' @param pkg Path to the revdep-checked package tree.
#' @param revdep Name of the reverse dependency.
#'
#' @family revdep check functions
#' @export

revdep_details <- function(pkg = ".", revdep) {
  res <- db_details(pkg, revdep)
  class(res) <- "revdepcheck_details"
  res
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
