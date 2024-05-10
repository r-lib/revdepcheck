
#' Retrieve the reverse dependencies for a package
#'
#' @param package The package (or packages) to search for reverse dependencies.
#' @inheritParams revdep_check
#' @export
cran_revdeps <- function(package, dependencies = TRUE, bioc = FALSE, cran = TRUE) {
  pkgs <- lapply(package, function(pkg) cran_revdeps_versions(pkg, dependencies, bioc, cran)$package)
  pkgs <- unique(unlist(pkgs))
  pkgs[order(tolower(pkgs))]
}

#' @importFrom remotes bioc_install_repos
#' @importFrom crancache available_packages

cran_revdeps_versions <- function(package, dependencies = TRUE, bioc = FALSE, cran = TRUE) {
  stopifnot(is_string(package))
  repos <- get_repos(bioc, cran)

  allpkgs <- available_packages(repos = repos)
  alldeps <- allpkgs[, dependencies, drop = FALSE]
  alldeps[is.na(alldeps)] <- ""
  deps <- apply(alldeps, 1, paste, collapse = ",")
  rd <- grepl(sprintf("(,| |\\n)(%s)(,| |\\n)", package), deps)

  data.frame(
    stringsAsFactors = FALSE,
    package = unname(allpkgs[rd, "Package"]),
    version = unname(allpkgs[rd, "Version"])
  )
}

get_repos <- function(bioc, cran) {
  repos <- c(
    getOption("repos"),
    if (bioc) bioc_install_repos()
  )
  
  if ((! "CRAN" %in% names(repos) || repos["CRAN"] == "@CRAN@") && cran) {
    repos["CRAN"] <- "https://cloud.r-project.org"
  }

  ## Drop duplicated repos (by name only)
  ## If the repos is not a named vector, names would be a NULL
  ## and duplicated(names) would be a logical(0) resulting in dropping entire 
  ## vector
  names <- names(repos) %|0|% rep("", times = length(repos))
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
