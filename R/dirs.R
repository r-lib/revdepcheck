
#' Set up the directory structure for the checks
#'
#' Currently the following files and directories are used.
#' They are all in the main revdep directory, which is `revdep` in the
#' package tree.
#' * `library`: a collection of package libraries
#' * `library/data.sqlite`: the SQLite database that contains the check
#'   data.
#' * `library/<checked-pkg>/old`: library that contains the *old* version
#'   of the revdep-checked package, together with its dependencies.
#' * `library/<checked-pkg>/new`: library that contains the *new* version
#'   of the revdep-checked package, together with its dependencies.
#' * `library/<pkg>` are the libraries for the reverse dependencies.
#'
#' @param pkgdir Path to the package we are revdep-checking.
#' @param what Directory to query:
#'   * `"check"`: the root of the check directory,
#'   * `"db"`: the database file,
#'   * `"old"`: the library of the old version of the package.
#'   * `"new"`: the library of the new version of the package.
#'   * `"pkg"`: the library of the reverse dependency, the `package`
#'     argument must be supplied as well.
#' @param package The name of the package, if `what` is `"pkg"`.
#' @return Character scalar, the requested path.
#'
#' @keywords internal

check_dir <- function(pkgdir,
                      what = c("check", "db", "old", "new", "pkg"),
                      package = NULL) {

  what <- match.arg(what)
  
  pkg <- get_package_name(pkgdir)

  create_dir(file.path(pkgdir, "revdep"))
  create_dir(file.path(pkgdir, "revdep", "library"))
  create_dir(file.path(pkgdir, "revdep", "library", pkg, "old"))
  create_dir(file.path(pkgdir, "revdep", "library", pkg, "new"))

  if (what == "check") {
    file.path(pkgdir, "revdep")

  } else if (what == "db") {
    file.path(pkgdir, "revdep", "data.sqlite")

  } else if (what == "old") {
    file.path(pkgdir, "revdep", "library", pkg, "old")

  } else if (what == "new") {
    file.path(pkgdir, "revdep", "library", pkg, "new")

  } else if (what == "pkg") {
    create_dir(path <- file.path(pkgdir, "revdep", "library", package))
    path
  }
}
