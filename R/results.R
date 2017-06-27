
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

#' @export

revdep_list_packages <- function(pkg = ".") {
  db_list(pkg)
}

#' Markdown report of reverse dependency check results
#' @family revdep check functions
#' @export
#' @importFrom crayon black red yellow green

revdep_report <- function(pkg = ".",
                          revdeps = NULL) {

  n_todo <- length(db_todo(pkg))
  if (n_todo > 0) {
    message(black("* ", yellow(as.character(n_todo)), " packages left to check"))
  }

  if (is.null(revdeps)) {
    revdeps <- db_list(pkg)
  }

  packages <- revdep_results(pkg, revdeps)
  broken <- vapply(packages, is_broken, integer(1))

  n_ok <- sum(!broken)
  if (n_ok > 0) {
    message(black("* ", green(as.character(n_ok)), " packages with no new failures"))
  }

  n_broken <- sum(broken)
  if (n_broken > 0) {
    message(black("* ", red(as.character(n_broken)), "packages with new failures"))
    details <- lapply(revdeps[broken], revdep_details, pkg = pkg)
    print(details)
  }

  invisible()
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

is_broken <- function(x) {
  # TODO: use better code from rcmdcheck
  n_broken_type <- function(x, type) {
    recs <- x$cmp[x$cmp$type == type, , drop = FALSE]
    old <- unique(recs$hash[recs$which == "old"])
    new <- unique(recs$hash[recs$which == "new"])

    length(setdiff(new, old))
  }

  n_broken <- n_broken_type(x, "error") +
    n_broken_type(x, "warning") +
    n_broken_type(x, "note")

  n_broken > 0
}

