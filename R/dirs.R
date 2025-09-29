#' Set up/retrieve the directory structure for the checks
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

dir_find <- function(
  pkgdir,
  what = c(
    "root",
    "db",
    "old",
    "new",
    "pkg",
    "check",
    "checks",
    "lib",
    "pkgold",
    "pkgnew",
    "cloud"
  ),
  package = NULL
) {
  pkgdir <- pkg_check(pkgdir)
  pkg <- pkg_name(pkgdir)

  idx <- if (Sys.info()[["sysname"]] == "Darwin") {
    function(x) paste0(x, ".noindex")
  } else {
    function(x) x
  }

  switch(
    match.arg(what),
    root = file.path(pkgdir, "revdep"),
    db = file.path(pkgdir, "revdep", "data.sqlite"),

    checks = file.path(pkgdir, "revdep", idx("checks")),
    check = file.path(pkgdir, "revdep", idx("checks"), package),

    lib = file.path(pkgdir, "revdep", idx("library")),
    pkg = file.path(pkgdir, "revdep", idx("library"), package),
    old = file.path(pkgdir, "revdep", idx("library"), pkg, "old"),
    new = file.path(pkgdir, "revdep", idx("library"), pkg, "new"),

    ## Order is important here, because installs should go to the first
    pkgold = c(
      file.path(pkgdir, "revdep", idx("library"), package),
      file.path(pkgdir, "revdep", idx("library"), pkg, "old")
    ),
    pkgnew = c(
      file.path(pkgdir, "revdep", idx("library"), package),
      file.path(pkgdir, "revdep", idx("library"), pkg, "new")
    ),
    cloud = c(file.path(pkgdir, "revdep", idx("cloud")))
  )
}

#' @export
#' @rdname dir_find

dir_setup <- function(pkgdir) {
  dir_create(dir_find(pkgdir, "root"))
  dir_create(dir_find(pkgdir, "checks"))
}

#' @export
#' @rdname dir_find

dir_setup_package <- function(pkgdir, package) {
  dir_create(dir_find(pkgdir, "pkgold", package))
  dir_create(dir_find(pkgdir, "pkgnew", package))
  dir_create(dir_find(pkgdir, "check", package))
}

dir_create <- function(paths) {
  map_lgl(paths, dir.create, recursive = TRUE, showWarnings = FALSE)
}
