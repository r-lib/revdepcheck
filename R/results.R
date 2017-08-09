revdep_results <- function(pkg = ".", revdeps = NULL) {
  res <- db_results(pkg, revdeps)
  class(res) <- "revdepcheck_results"
  res
}

#' Display details of a single revdepcheck
#'
#' Use this to see nicely formatted results of processed packages while
#' [revdep_check()] is running in another process.
#'
#' @export
#' @param pkg Path to package
#' @param revdep Name of revdep package.
#' @importFrom rcmdcheck compare_checks

revdep_details <- function(pkg = ".", revdep) {
  assert_that(is_string(revdep))
  record <- db_details(pkg, revdep)
  old <- checkFromJSON(record$old$result[[1]])
  new <- checkFromJSON(record$new$result[[1]])
  res <- compare_checks(old, new)
  class(res) <- "revdepcheck_details"
  res
}

#' List packages remaining in the todo list
#'
#' @inheritParams revdep_check
#' @export

revdep_todo <- function(pkg = ".") {
  db_list(pkg)
}

is_broken <- function(x) {
  rcmdcheck_status(x) != "+"
}

