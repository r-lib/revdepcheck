#' Set up/retrieve the directory structure for the checks
#'
#' Currently the following files and directories are used.
#' They are all in the main revdep directory, which is `revdep` in the
#' package tree, or the root directory itself for a check set up with
#' [rdev_init()].
#' * `library`: a collection of package libraries
#' * `data.sqlite`: the SQLite database that contains the check data.
#' * `library/<checked-pkg>/old`: library that contains the *old* version
#'   of the revdep-checked package, together with its dependencies.
#' * `library/<checked-pkg>/new`: library that contains the *new* version
#'   of the revdep-checked package, together with its dependencies.
#' * `library/<pkg>` are the libraries for the reverse dependencies.
#' * `tools`: library with the packages the worker processes need when
#'   they run a different R build than the current session, see
#'   [rdev_check()].
#' * `crancache`: the default crancache package cache of [rdev_check()].
#'
#' @param pkgdir Path to the package we are revdep-checking, or a root
#'   created by [rdev_init()].
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
#'   * `"tools"`: the tools library, see above.
#'   * `"cache"`: the default crancache cache directory, see above.
#'
#'   An [rdev_init()] root has no package under test, so `"old"` and
#'   `"new"` are `NULL` there, and `"pkgold"` and `"pkgnew"` are just the
#'   library of the reverse dependency.
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
    "cloud",
    "tools",
    "cache"
  ),
  package = NULL
) {
  pkgdir <- pkg_check(pkgdir)
  what <- match.arg(what)
  rdev <- is_rdev(pkgdir)

  idx <- if (Sys.info()[["sysname"]] == "Darwin") {
    function(x) paste0(x, ".noindex")
  } else {
    function(x) x
  }

  root <- if (rdev) pkgdir else file.path(pkgdir, "revdep")
  lib <- file.path(root, idx("library"))

  ## Libraries of the package under test; there is none in rdev mode
  pkg_libs <- if (rdev) {
    list(old = NULL, new = NULL)
  } else {
    pkg <- pkg_name(pkgdir)
    list(old = file.path(lib, pkg, "old"), new = file.path(lib, pkg, "new"))
  }

  switch(
    what,
    root = root,
    db = file.path(root, "data.sqlite"),

    checks = file.path(root, idx("checks")),
    check = file.path(root, idx("checks"), package),

    lib = lib,
    pkg = file.path(lib, package),
    old = pkg_libs$old,
    new = pkg_libs$new,

    ## Order is important here, because installs should go to the first
    pkgold = c(file.path(lib, package), pkg_libs$old),
    pkgnew = c(file.path(lib, package), pkg_libs$new),

    cloud = file.path(root, idx("cloud")),
    tools = file.path(root, idx("tools")),
    cache = file.path(root, idx("crancache"))
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
