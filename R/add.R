
#' Manually add packages to check
#'
#' Use `revdep_add()` to add a single package to the to do list. Use
#' `revdep_add_broken()` to add all broken packages from the last check
#' (this is useful if you think you've fixed the underlying problem in
#' your package)
#'
#' @inheritParams revdep_check
#' @param packages Character vector of package names to add
#' @export

revdep_add <- function(pkg = ".", packages) {
  pkg <- pkg_check(pkg)
  db_todo_add(pkg, packages)
  db_metadata_set(pkg, "todo", "run")
  invisible()
}

#' @export
#' @rdname revdep_add

revdep_add_broken <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  packages <- revdep_results(pkg, db_list(pkg))
  broken <- vapply(packages, is_broken, integer(1))

  to_add <- db_list(pkg)[broken]
  if (length(to_add) == 0) {
    message("No broken packages to re-test")
  } else {
    message(
      "Re-checking broken packages: ",
      str_trunc(paste(to_add, collapse = ","), 100)
    )
    revdep_add(pkg, to_add)

  }

}
