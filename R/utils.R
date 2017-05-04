
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

create_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

#' @importFrom tools package_dependencies

deps_for_package <- function(package) {
  direct_deps <- unlist(package_dependencies(package, which = "most"))
  indirect_deps <- unlist(package_dependencies(direct_deps))
  all_deps <- unique(unname(c(direct_deps, indirect_deps)))
  setdiff(all_deps, base_packages())
}

#' @importFrom utils installed.packages

base_packages <- function() {
  rownames(installed.packages(priority="base"))
}
