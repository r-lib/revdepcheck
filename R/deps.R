
#' Retrieve the reverse dependencies for a package
#'
#' Uses the value of `getOption("repo")[["CRAN"]]` to determine where to find
#' CRAN metadata.
#'
#' @param package The package (or packages) to search for reverse dependencies.
#' @inheritParams revdep_check
#' @export
cran_revdeps <- function(package, dependencies = TRUE, bioc = FALSE) {
  cran_revdeps_versions(package, dependencies, bioc)$package
}

cran_revdeps_versions <- function(packages, dependencies = TRUE, bioc = FALSE) {
  stopifnot(is.character(packages))

  cache <- pkgcache::cranlike_metadata_cache$new(
    repos = get_repos(bioc = bioc),
    platforms = "source"
  )

  revdeps <- cache$revdeps(packages, dependencies = dependencies, recursive = FALSE)
  revdeps[c("package", "version")]
}

get_repos <- function(bioc) {
  repos <- c(
    getOption("repos"),
    if (bioc) remotes::bioc_install_repos()
  )
  if (! "CRAN" %in% names(repos) || repos["CRAN"] == "@CRAN@") {
    repos["CRAN"] <- "https://cloud.r-project.org"
  }

  ## Drop duplicated repos (by name only)
  names <- names(repos)
  repos <- repos[!(nzchar(names) & duplicated(names))]

  repos
}

cran_deps <- function(package, repos) {
  allpkgs <- available_packages(repos = repos)
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
  deps[is.na(deps)] <- ""
  deps <- gsub("\\s+", "", deps)
  deps <- gsub("\\([^)]+\\)", "", deps)
  notempty <- nzchar(deps)
  res <- replicate(length(deps), character())
  deps <- deps[notempty]
  deps <- strsplit(deps, ",", fixed = TRUE)

  base <- base_packages()
  deps <- map(deps, setdiff, y = c("R", base))

  res[notempty] <- deps
  res
}
