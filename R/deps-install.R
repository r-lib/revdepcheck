#' @importFrom processx process
#' @importFrom callr r r_process r_process_options
#' @importFrom crancache available_packages
#' @importFrom withr with_envvar

deps_install_opts <- function(
  pkgdir,
  pkgname,
  quiet = FALSE,
  env = character(),
  rdev = NULL
) {
  func <- function(libdir, packages, quiet, repos) {
    ip <- crancache::install_packages
    withr::with_libpaths(
      libdir,
      {
        ip(
          packages,
          dependencies = FALSE,
          lib = libdir[1],
          quiet = quiet,
          repos = repos
        )
        stopifnot(all(packages %in% rownames(installed.packages(libdir[1]))))
      }
    )
  }

  ## We don't want to install the revdep checked package again,
  ## that's in a separate library, hence the `exclude` argument.
  ## There is no such package in rdev mode.
  exclude <- if (is.null(rdev)) pkg_name(pkgdir) else character()

  args <- c(
    deps_opts(pkgname, exclude = exclude, rdev = rdev),

    list(
      libdir = dir_find(pkgdir, "pkg", pkgname),
      quiet = quiet
    )
  )

  ## In rdev mode the dependencies must be installed by the R build that
  ## will load them, and that build finds crancache in the tools library
  libpath <- if (is.null(rdev)) .libPaths() else rdev$tools
  libenv <- if (is.null(rdev)) character() else rdev_lib_env(rdev$tools)

  ## CRANCACHE_REPOS makes sure that we only use cached CRAN packages,
  ## but not packages that were installed from elsewhere
  r_process_options(
    func = func,
    args = args,
    libpath = libpath,
    arch = rdev$r_old %||% "same",
    system_profile = FALSE,
    user_profile = FALSE,
    env = c(
      CRANCACHE_REPOS = "cran,bioc",
      CRANCACHE_QUIET = if (quiet) "yes" else "no",
      rdev_cache_env(rdev),
      libenv,
      env
    )
  )
}

deps_opts <- function(pkgname, exclude = character(), rdev = NULL) {
  ## We set repos, so that dependencies from Bioconductor are installed
  ## automatically
  repos <- get_repos(bioc = TRUE, cran = TRUE)

  ## In rdev mode availability is judged for the R build under test, not
  ## for this session, so the R version filter is left out and the root's
  ## package cache is used
  filters <- if (is.null(rdev)) NULL else rdev_filters
  env <- c(
    CRANCACHE_REPOS = "cran,bioc",
    CRANCACHE_QUIET = "yes",
    rdev_cache_env(rdev)
  )

  ## We have to do this "manually", because some of the dependencies
  ## might be also dependencies of crancache, so they will be already
  ## installed in another library directory, and also loaded.
  ## But we want to install everything into the package's specific library,
  ## because this is the only library used for the check.
  '!DEBUG Querying dependencies of `paste(pkgname, collapse = ", ")`'
  packages <- with_envvar(env, cran_deps(pkgname, repos, filters = filters))

  packages <- setdiff(packages, exclude)

  ## We do this, because if a package is not available,
  ## utils::install.packages does not install anything, just gives a
  ## warning
  "!DEBUG dropping unavailable dependencies"
  available <- with_envvar(
    env,
    rownames(available_packages_with(repos, filters))
  )
  packages <- intersect(packages, available)

  list(
    package = packages,
    repos = repos
  )
}

deps_install_task <- function(state, task) {
  pkgdir <- state$options$pkgdir
  pkgname <- task$args[[1]]

  dir_setup_package(pkgdir, pkgname)

  "!DEBUG Install dependencies for package `pkgname`"
  px_opts <- deps_install_opts(
    pkgdir,
    pkgname,
    quiet = state$options$quiet,
    env = state$options$env,
    rdev = state$options$rdev
  )
  px <- r_process$new(px_opts)

  ## Update state
  worker <- list(
    process = px,
    package = pkgname,
    stdout = character(),
    stderr = character(),
    task = task
  )
  state$workers <- c(state$workers, list(worker))

  wpkg <- match(worker$package, state$packages$package)
  state$packages$state[wpkg] <- "deps_installing"

  state
}

deps_install_done <- function(state, worker) {
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
        state$options$pkgdir,
        worker$package,
        version = NULL,
        status = status,
        which = which,
        duration = duration,
        starttime = starttime,
        result = unclass(toJSON(result)),
        summary = NULL
      )
    }
  } else {
    ## succeeded
    state$packages$state[wpkg] <- "deps_installed"
  }

  state
}

# Not used by other methods, but simplifies debugging
deps_install <- function(pkgdir, pkgname, quiet = FALSE, new_session = FALSE) {
  px_opts <- deps_install_opts(pkgdir, pkgname, quiet = FALSE)
  execute_r(px_opts, new_session = new_session)
}
