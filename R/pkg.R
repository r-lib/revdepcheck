pkg_check <- function(pkgdir) {
  if (!is_string(pkgdir)) {
    stop("`pkgdir` must be a string", call. = FALSE)
  }

  if (!file.exists(pkgdir) || !file.info(pkgdir)$isdir) {
    stop("`pkgdir` must be an existing directory", call. = FALSE)
  }

  if (!file.exists(file.path(pkgdir, "DESCRIPTION"))) {
    stop("`pkgdir` must contain a DESCRIPTION file", call. = FALSE)
  }

  normalizePath(pkgdir, mustWork = FALSE)
}

pkg_name <- function(pkgdir) {
  read.dcf(file.path(pkgdir, "DESCRIPTION"))[, "Package"][[1]]
}

pkg_version <- function(pkgdir) {
  read.dcf(file.path(pkgdir, "DESCRIPTION"))[, "Version"][[1]]
}

pkg_bug_reports <- function(pkgdir) {
  read.dcf(file.path(pkgdir, "DESCRIPTION"))[, "BugReports"][[1]]
}

# Helper for testing ------------------------------------------------------

local_package <- function(fields = list(), env = parent.frame()) {
  dir <- tempfile()
  dir.create(dir)
  dir.create(file.path(dir, "revdep"))
  withr::defer(unlink(dir, recursive = TRUE), envir = env)

  defaults <- list(
    Package = "Test",
    Version = "1.0.0"
  )

  fields <- utils::modifyList(defaults, fields)
  write.dcf(fields, file.path(dir, "DESCRIPTION"))

  db_setup(dir)

  dir
}
