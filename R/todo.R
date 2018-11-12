
#' Manage the package checking to do list.
#'
#' `revdep_todo()` tells you what packages still need to be check.
#' `revdep_add()` adds a single package to the to do list.
#' `revdep_add_broken()` re-adds all broken packages from the last check
#' (this is useful if you think you've fixed the underlying problem in
#' your package).
#'
#' @inheritParams revdep_check
#' @param packages Character vector of package names to add
#' @export

revdep_add <- function(pkg = ".", packages) {
  pkg <- pkg_check(pkg)
  db_todo_add(pkg, packages)

  # If you're re-checking packages, it's because the package has
  # changed, so you'll want to re-install it
  db_metadata_set(pkg, "todo", "install")
  invisible()
}

#' @export
#' @rdname revdep_add

revdep_add_broken <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  comparisons <- db_results(pkg)$comparisons
  broken <- map_lgl(comparisons, is_broken)

  to_add <- names(broken[broken])
  if (length(to_add) == 0) {
    message("No broken packages to re-test")
  } else {
    message(
      "Adding broken packages TODO list: \n",
      paste("*", to_add, "\n", collapse = ""),
      "\n",
      "Run revdepcheck::revdep_check() to check"
    )
    revdep_add(pkg, to_add)
  }

}

#' @export
#' @rdname revdep_add

revdep_todo <- function(pkg = ".") {
  db_todo(pkg)
}

