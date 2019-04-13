
#' Manage the package checking to-do list.
#'
#' `revdep_todo()` tells you which packages still need to be checked.
#' `revdep_add()` adds a single package to the to-do list.
#' `revdep_rm()` removes packages from the todo list.

#' `revdep_add_broken()` re-adds all broken packages from the last check
#' (this is useful if you think you've fixed the underlying problem in
#' your package).
#'
#' @inheritParams revdep_check
#' @param packages Character vector of package names to add
#' @param install_failures Whether to re-add packages that failed to
#'   install.
#' @param timeout_failures Whether to re-add packages that timed out.
#'
#' @export

revdep_add <- function(pkg = ".", packages) {
  pkg <- pkg_check(pkg)

  db_todo_add(pkg, packages, silent = FALSE)

  # If you're re-checking packages, it's because the package has
  # changed, so you'll want to re-install it
  db_metadata_set(pkg, "todo", "install")

  invisible(revdep_todo(pkg))
}

#' @export
#' @rdname revdep_add

revdep_add_broken <- function(pkg = ".", install_failures = FALSE,
                              timeout_failures = FALSE) {
  pkg <- pkg_check(pkg)

  packages <- db_results(pkg, NULL)
  broken <- map_lgl(packages, is_broken, install_failures, timeout_failures)

  to_add <- names(broken[broken])
  if (length(packages) == 0) {
    message("No broken packages to re-test")
  } else {
    revdep_add(pkg, to_add, silent = FALSE)
  }

  invisible(revdep_todo(pkg))
}

#' @export
#' @rdname revdep_add

revdep_add_new <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  pkgname <- db_metadata_get(pkg, "package")
  bioc <- db_metadata_get(pkg, "bioc") %|0|% "TRUE"
  dependencies <- db_metadata_get(pkg, "dependencies") %|0|%
    "Depends;Imports;Suggests;LinkingTo"
  bioc <- as.logical(bioc)
  dependencies <- strsplit(dependencies, ";", fixed = TRUE)[[1]]

  revdeps <- cran_revdeps_versions(pkgname, dependencies, bioc = bioc)

  todo <- db_todo_add_new(pkg, revdeps, silent = FALSE)
  if (length(todo)) db_metadata_set(pkg, "todo", "install")

  invisible(revdep_todo(pkg))
}

#' @export
#' @rdname revdep_add

revdep_todo <- function(pkg = ".") {
  db_todo_status(pkg)
}

#' @export
#' @rdname revdep_add

revdep_rm <- function(pkg = ".", packages) {
  pkg <- pkg_check(pkg)
  db_todo_rm(pkg, packages)

  invisible(revdep_todo(pkg))
}
