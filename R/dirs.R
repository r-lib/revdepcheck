
check_dir <- function(package, what = c("check", "library", "db")) {
  what <- match.arg(what)

  chkdir <- file.path(package, "revdep")
  if (!file.exists(chkdir)) dir.create(chkdir)
  libdir <- file.path(package, "revdep", "library")
  if (!file.exists(libdir)) dir.create(libdir)

  if (what == "check") {
    chkdir

  } else if (what == "library") {
    libdir

  } else if (what == "db") {
    file.path(package, "revdep", "data.sqlite")

  }
}
