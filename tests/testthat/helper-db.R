
new_pkg_list <- function(package, which) {
  list(
    package = package,
    status = "OK",
    which = which,
    duration = "0",
    starttime = "0",
    result = "[]",
    summary = ""
  )
}

new_pkg_result_list <- function(which) {
  list(
    version = "",
    maintainer = "",
    status = "OK",
    which = which,
    duration = "0",
    starttime = "0",
    result = "[]",
    summary = ""
  )
}
