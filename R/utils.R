
is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_package_dir <- function(x) {
  is_string(x) &&
    file.info(x)$isdir &&
    file.exists(file.path(x, "DESCRIPTION"))
}

get_package_name <- function(path) {
  read.dcf(file.path(path, "DESCRIPTION"))[, "Package"]
}

create_dir <- function(paths) {
  vapply(
    paths, FUN = dir.create, FUN.VALUE = logical(1),
    recursive = TRUE, showWarnings = FALSE
  )
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

lapply_with_names <- function(X, FUN, ...) {
  structure(lapply(X, FUN, ...), names = names(X))
}
