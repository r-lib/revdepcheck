
#' @importFrom processx process
#' @importFrom callr r_process r_process_options
#' @importFrom crancache available_packages
#' @importFrom withr with_envvar

do_deps_install <- function(state, task) {

  pkgdir <- state$options$pkgdir
  pkgname <- task$args[[1]]

  "!DEBUG Install dependencies for package `pkgname`"
  func <- function(libdir, packages, quiet, repos) {
    ip <- crancache::install_packages
    withr::with_libpaths(
      libdir,
      ip(
        packages,
        dependencies = FALSE,
        lib = libdir[1],
        quiet = quiet,
        repos = repos
      )
    )
  }

  ## We set repos, so that dependencies from BioConductor are installed
  ## automatically
  repos <- c(
    bioc_install_repos(),
    getOption("repos"),
    c("CRAN-cloud" = "https://cloud.r-project.org")
  )

  ## We have to do this "manually", because some of the dependencies
  ## might be also dependencies of crancache, so they will be already
  ## installed in another library directory, and also loaded.
  ## But we want to install everything into the package's specific library,
  ## because this is the only library used for the check.
  "!DEBUG Querying dependencies of `pkgname`"
  packages <- deps_for_package(pkgname)

  ## We don't want to install the revdep checked package again,
  ## that's in a separate library
  packages <- setdiff(packages, state$options$pkgname)

  ## We do this, because if a package is not available,
  ## utils::install.packages does not install anything, just gives a
  ## warning
  "!DEBUG dropping unavailable dependencies for `pkgname`"
  available <- with_envvar(
    c(CRANCACHE_REPOS = "cran,bioc", CRANCACHE_QUIET = "yes"),
    rownames(available_packages(repos = repos))
  )
  packages <- intersect(available, packages)

  args <- list(
    libdir = check_dir(pkgdir, "pkg", pkgname),
    package = packages,
    quiet = state$options$quiet,
    repos = repos
  )

  ## CRANCACHE_REPOS makes sure that we only use cached CRAN packages,
  ## but not packages that were installed from elsewhere
  px_opts <- r_process_options(
    func = func,
    args = args,
    system_profile = FALSE,
    user_profile = FALSE,
    env = c(CRANCACHE_REPOS = "cran,bioc", CRANCACHE_QUIET = "yes")
  )
  px <- r_process$new(px_opts)

  ## Update state
  worker <- list(process = px, package = pkgname,
                 stdout = character(), stderr = character(), task = task)
  state$workers <- c(state$workers, list(worker))

  wpkg <- match(worker$package, state$packages$package)
  state$packages$state[wpkg] <- "deps_installing"

  state
}

handle_finished_deps_install <- function(state, worker) {
  starttime <- worker$process$get_start_time()
  duration <- as.numeric(Sys.time() - starttime)
  wpkg <- match(worker$package, state$packages$package)

  worker$process$wait(timeout = 1000)
  worker$process$kill()
  if (worker$process$get_exit_status()) {
    ## failed, we just stop the whole package
    cleanup_library(state, worker)
    state$packages$state[wpkg] <- "done"

    rresult <- if (isTRUE(worker$killed)) {
      "Process was killed while installing dependencies"
      status <- "TIMEOUT"
    } else {
      status <- "PREPERROR"
      tryCatch(
        worker$process$get_result(),
        error = function(e) conditionMessage(e)
      )
    }

    result <- list(
      stdout = worker$stdout,
      stderr = worker$stderr,
      errormsg = rresult
    )

    for (which in c("old", "new")) {
      db_insert(
        state$options$pkgdir, worker$package, version = NULL,
        status = status, which = which, duration = duration,
        starttime = starttime, result = unclass(toJSON(result)),
        summary = NULL
      )
    }

  } else {
    ## succeeded
    state$packages$state[wpkg] <- "deps_installed"
  }

  state
}
