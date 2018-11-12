#' Compute package revdeps
#'
#' `pkg_revdeps()` returns a tibble of reverse dependencies that you
#' can pass to `revdep_check()`.
#'
#' @param pkg A character vector of package names whose reverse
#'   dependencies should be checked. If `NULL` or empty, all CRAN
#'   packages (and all Bioconductor packages if `bioc` is `TRUE`) are
#'   returned.
#' @param dependencies Which types of revdeps should be checked. For
#'   CRAN release, we recommend using the default.
#' @param bioc Also check revdeps that live in BioConductor?
#'
#' @return A tibble with a `package` column. If `pkg` contains
#'   multiple packages, these are recorded in an additional column
#'   `set` that allows you to match revdeps to elements of `pkg`.
#'
#' @export
#' @examples
#' if (FALSE) {
#'
#' # Compute revdeps of rlang
#' revdep_pkgs("rlang")
#'
#' # Don't include Bioconductor revdeps
#' revdep_pkgs("rlang", bioc = FALSE)
#'
#' # Compute revdeps of rlang, dplyr and purrr
#' revdep_pkgs(c("rlang", "dplyr", "purrr"))
#'
#' # Return all packages of CRAN and Bioconductor:
#' revdep_pkgs(NULL)
#'
#' # Return all packages of CRAN:
#' revdep_pkgs(NULL, bioc = FALSE)
#'
#' }
revdep_pkgs <- function(pkg,
                        dependencies = c("Depends", "Imports",
                                         "Suggests", "LinkingTo"),
                        bioc = TRUE) {
  stopifnot(is_null(pkg) || is_character(pkg))

  n <- length(pkg)
  repos <- get_repos(bioc = bioc)
  if (n < 2) {
    data <- revdep_pkgs_data(repos, pkg, dependencies)
    data <- revdep_pkgs_subset(data)
    return(data)
  }

  pkgs_data <- map(set_names(pkg), revdep_pkgs_data, repos = repos, dependencies)
  data <- value(rbind(!!!pkgs_data))

  set <- imap(pkgs_data, function(df, n) rep_len(n, NROW(df)))
  set <- value(c(!!!unname(set)))

  data <- tibble(set = set, !!!data)
  revdep_pkgs_subset(data)
}

revdep_pkgs_subset <- function(pkgs) {
  pkgs <- pkgs[!duplicated(pkgs$package), ]

  # For easier debugging
  if (is_true(peek_option("revdepcheck__limit_revdeps"))) {
    group_nms <- names(pkgs)[-match("package", names(pkgs))]
    subset_groups(pkgs, group_nms, 2)
  } else {
    pkgs
  }
}

revdep_pkgs_data <- function(repos, package, dependencies) {
  pkgs <- flatten_names(map(repos, get_packages, package, dependencies))
  pkgs <- map(pkgs, tibble::enframe, name = "repo", value = "package")
  pkgs <- value(rbind(!!!pkgs))
  pkgs
}

#' @importFrom remotes bioc_install_repos
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

#' @importFrom crancache available_packages
get_packages <- function(repos, package, dependencies) {
  if (!length(repos)) {
    return(chr())
  }

  allpkgs <- available_packages(repos = repos)
  alldeps <- allpkgs[, dependencies, drop = FALSE]
  alldeps[is.na(alldeps)] <- ""
  deps <- apply(alldeps, 1, paste, collapse = ",")

  if (is_null(package)) {
    rd <- TRUE
  } else {
    rd <- grepl(paste0("\\b", package, "\\b"), deps)
  }

  pkgs <- unname(allpkgs[rd, "Package"])
  pkgs[order(tolower(pkgs))]
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

pkgs_validate <- function(packages) {
  if (is_character(packages)) {
    data <- tibble(package = packages)
    return(data)
  }

  if (!is_pkgs_revdeps(packages)) {
    abort("`packages` must be a character vector or a data frame with a `package column`")
  }

  packages <- as_tibble(packages)
  unduplicate(packages, "package")
}
is_pkgs_revdeps <- function(x) {
  is.data.frame(x) && has_name(x, "package")
}
