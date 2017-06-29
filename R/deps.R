
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
  allpkgs <- available_packages()
  current <- deps <- package
  dependencies <- c("Depends", "Imports", "LinkingTo", "Suggests")
  while (TRUE) {
    deprecs <- allpkgs[ allpkgs[, "Package"] %in% deps, dependencies ]
    newdeps <- unlist(parse_deps(deprecs))
    deps <- unique(sort(c(deps, newdeps)))
    if (identical(current, deps)) break
    dependencies <- c("Depends", "Imports", "LinkingTo")
    current <- deps
  }

  setdiff(deps, c(package, base_packages()))
}

parse_deps <- function(deps) {
  deps <- lapply(strsplit(deps, ","), str_trim)
  deps <- lapply(deps, function(x) lapply(strsplit(x, "\\("), str_trim))
  deps <- lapply(
    deps,
    function(x) lapply(x, sub, pattern = "\\)$", replacement = "")
  )
  deps <- lapply(deps, function(x) vapply(x, "[", "", 1))
  lapply(deps, setdiff, y = c("R", base_packages()))
}
