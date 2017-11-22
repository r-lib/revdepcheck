
#' @importFrom remotes bioc_install_repos
#' @importFrom crancache available_packages

cran_revdeps <- function(package, dependencies = TRUE, bioc = FALSE) {
  stopifnot(is_string(package))
  repos <- get_repos(bioc)

  allpkgs <- available_packages(repos = repos)
  alldeps <- allpkgs[, dependencies, drop = FALSE]
  alldeps[is.na(alldeps)] <- ""
  deps <- apply(alldeps, 1, paste, collapse = ",")
  rd <- grepl(paste0("\\b", package, "\\b"), deps)

  pkgs <- unname(allpkgs[rd, "Package"])
  pkgs[order(tolower(pkgs))]
}

get_repos <- function(bioc) {
  repos <- c(
    getOption("repos"),
    if (bioc) bioc_install_repos()
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
  '!DEBUG cran_deps package = `paste(package, collapse = ", ")`'
  '!DEBUG ==='

  allpkgs <- available_packages(repos = repos)
  direct_deps <- get_deps_as_vector(
    package, allpkgs,
    c("Depends", "Imports", "LinkingTo", "Suggests")
  )

  dep_df <- get_recursive_dep_df(
    direct_deps, allpkgs = allpkgs,
    dependencies = c("Depends", "Imports", "LinkingTo"))
  deps <- topo_sort_dep_df(dep_df)

  '!DEBUG diff `paste(setdiff(direct_deps, deps), collapse = ", ")`'
  stopifnot(all(direct_deps %in% deps))

  '!DEBUG deps (final) = `paste(deps, collapse = ", ")`'

  deps
}

get_deps_as_vector <- function(package, allpkgs, dependencies) {
  deprecs <- allpkgs[ allpkgs[, "Package"] %in% package, dependencies ]
  newdeps <- unique(unlist(parse_deps(deprecs)))
  newdeps
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
  deps <- lapply(deps, setdiff, y = c("R", base))

  res[notempty] <- deps
  res
}

get_recursive_dep_df <- function(packages, allpkgs, dependencies) {
  # We start with dummy entries to ensure that each of our input packages
  # is present at least once in the depends_on column
  dep_df <- new_dep_df("00dummy", packages)

  # Completed packages are tracked separately, because packages without
  # dependencies will never appear in dep_df
  todo <- packages
  done <- character()
  while (TRUE) {
    '!DEBUG todo = `paste(todo, collapse = ", ")`'

    nextdeps_df <- get_deps_as_df(todo, allpkgs, dependencies)
    done <- c(done, todo)

    '!DEBUG nrow(nextdeps_df) = `nrow(nextdeps_df)`'

    if (nrow(nextdeps_df) == 0) break

    dep_df <- rbind(dep_df, nextdeps_df)
    todo <- setdiff(unique(dep_df$depends_on), done)
  }

  dep_df <- dep_df[!(dep_df$depends_on %in% base_packages()), ]

  dep_df
}

new_dep_df <- function(package, depends_on) {
  data.frame(package, depends_on, stringsAsFactors = FALSE)
}

get_deps_as_df <- function(package, allpkgs, dependencies) {
  deps_list <- lapply(
    package, get_deps_as_vector,
    allpkgs = allpkgs, dependencies = dependencies
  )
  deps_df <- new_dep_df(
    rep(package, vapply(deps_list, length, integer(1L))),
    unlist(deps_list)
  )
  deps_df
}

topo_sort_dep_df <- function(dep_df) {
  orig_deps <- unique(dep_df$depends_on)

  deps <- character()
  while (nrow(dep_df) != 0) {
    ready <- setdiff(dep_df$depends_on, dep_df$package)
    '!DEBUG ready = `paste(ready, collapse = ", ")`'
    stopifnot(length(intersect(deps, ready)) == 0L)

    deps <- c(deps, ready)
    dep_df <- dep_df[!(dep_df$depends_on %in% ready), ]
  }

  stopifnot(all(orig_deps %in% deps))
  deps
}
