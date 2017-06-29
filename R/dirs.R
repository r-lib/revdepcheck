
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
#'   * `"root"`: the root of the check directory,
#'   * `"db"`: the database file,
#'   * `"old"`: the library of the old version of the package.
#'   * `"new"`: the library of the new version of the package.
#'   * `"pkg"`: the library of the reverse dependency, the `package`
#'     argument must be supplied as well.
#'   * `"check"`: the check directory of the reverse dependency, the
#'     `package` argument must be supplied as well.
#'   * `"pkgold"`: package libraries to use when checking `package` with
#'     the old version.
#'   * `"pkgnew"`: package libraries to use when checking `package` with
#'     the new version.
#' @param package The name of the package, if `what` is `"pkg"`, `"check"`,
#'     `"pkgold"` or `"pkgnew"`.
#' @return Character scalar, the requested path.
#'
#' @keywords internal

check_dir <- function(pkgdir,
                      what = c("root", "db", "old", "new", "pkg", "check",
                               "pkgold", "pkgnew"),
                      package = NULL) {

  what <- match.arg(what)

  pkg <- pkg_name(pkgdir)

  create_dir(file.path(pkgdir, "revdep"))
  create_dir(file.path(pkgdir, "revdep", "library"))
  create_dir(file.path(pkgdir, "revdep", "library", pkg, "old"))
  create_dir(file.path(pkgdir, "revdep", "library", pkg, "new"))

  if (what == "root") {
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

  } else if (what == "check") {
    create_dir(path <- file.path(pkgdir, "revdep", "checks", package))
    path

  } else if (what == "pkgold") {
    ## Order is important here, because installs should go to the first
    create_dir(
      paths <- c(file.path(pkgdir, "revdep", "library", package),
                 file.path(pkgdir, "revdep", "library", pkg, "old"))
    )
    paths

  } else if (what == "pkgnew") {
    ## Order is important here, because installs should go to the first
    create_dir(
      paths <- c(file.path(pkgdir, "revdep", "library", package),
                 file.path(pkgdir, "revdep", "library", pkg, "new"))
    )
    paths
  }
}
