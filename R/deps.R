
#' Retrieve the reverse dependencies for a package
#'
#' Uses a hard coded CRAN mirror of <https://cloud.r-project.org> to ensure
#' that all users get the same results.
#'
#' @param package The package (or packages) to search for reverse dependencies.
#' @inheritParams revdep_check
#' @export
cran_revdeps <- function(package, dependencies = TRUE, bioc = FALSE) {
  pkgs <- lapply(package, function(pkg) cran_revdeps_versions(pkg, dependencies, bioc)$package)
  pkgs <- unique(unlist(pkgs))
  pkgs[order(tolower(pkgs))]
}

cran_revdeps_versions <- function(package, dependencies = TRUE, bioc = FALSE) {
  stopifnot(is_string(package))

  cache <- pkgcache::cranlike_metadata_cache$new(
    repos = list(CRAN = "https://cloud.r-project.org"),
    platforms = "source",
    bioc = FALSE
  )

  revdeps <- cache$revdeps(package, dependencies = dependencies, recursive = FALSE)
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
