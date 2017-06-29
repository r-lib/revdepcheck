
#' @importFrom tools package_dependencies
#' @importFrom remotes bioc_install_repos
#' @importFrom crancache available_packages

cran_revdeps <- function(package, dependencies, bioc) {
  stopifnot(is_string(package))
  repos <- c(
    if (bioc) bioc_install_repos(),
    getOption("repos"),
    c("CRAN-cloud" = "https://cloud.r-project.org")
  )

  package_dependencies(
    package,
    recursive = FALSE,
    reverse = TRUE,
    db = available_packages(repos = repos),
    which = dependencies
  )[[1]]
}

#' @importFrom tools package_dependencies

cran_deps <- function(package) {
  direct_deps <- unlist(package_dependencies(package, which = "most"))
  indirect_deps <- unlist(package_dependencies(
    direct_deps, recursive = TRUE))
  all_deps <- unique(unname(c(direct_deps, indirect_deps)))
  setdiff(all_deps, base_packages())
}
