
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
