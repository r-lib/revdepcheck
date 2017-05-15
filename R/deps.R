
#' @importFrom tools dependsOnPkgs
#' @importFrom remotes bioc_install_repos
#' @importFrom crancache available_packages

cran_revdeps <- function(package, dependencies, bioc) {
  stopifnot(is_string(package))
  repos <- c(
    if (bioc) bioc_install_repos(),
    getOption("repos"),
    c("CRAN-cloud" = "https://cloud.r-project.org")
  )

  dependsOnPkgs(
    package,
    recursive = FALSE,
    installed = available_packages(repos = repos),
    dependencies = dependencies
  )
}
