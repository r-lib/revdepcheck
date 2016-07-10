
is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_package_dir <- function(x) {
  is_string(x) &&
    file.info(x)$isdir &&
    file.exists(file.path(x, "DESCRIPTION"))
}

download_dir <- function() {
  file.path(tempdir(), "downloaded_packages")
}

get_package_name <- function(path) {
  read.dcf(file.path(path, "DESCRIPTION"))[, "Package"]
}
