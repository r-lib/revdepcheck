
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

  res <- dependsOnPkgs(
    package,
    recursive = FALSE,
    installed = available_packages(repos = repos),
    dependencies = dependencies
  )

  ## dependsOnPkgs sometimes return and empty string, not sure why,
  ## but we just remove it for now
  setdiff(res, "")
}
