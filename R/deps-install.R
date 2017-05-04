
#' @importFrom processx process
#' @importFrom callr r_process r_process_options

do_deps_install <- function(state, task) {

  pkgdir <- state$options$pkgdir
  pkgname <- task$args[[1]]

  "!DEBUG Install dependencies for package `pkgname`"
  func <- function(libdir, packages, quiet) {
    ip <- crancache::install_packages
    withr::with_libpaths(
      libdir,
      ip(
        packages,
        dependencies = TRUE,
        lib = libdir[1],
        quiet = quiet,
        use_cache = "cran"
      )
    )
  }

  ## We have to do this "manually", because some of the dependencies
  ## might be also dependencies of crancache, so they will be already
  ## installed in another library directory, and also loaded.
  ## But we want to install everything into the package's specific library,
  ## because this is the only library used for the check.
  packages <- deps_for_package(pkgname)

  ## We don't want to install the revdep checked package again,
  ## that's in a separate library
  packages <- setdiff(packages, state$options$pkgname)

  args <- list(
    libdir = check_dir(pkgdir, "pkgold", pkgname),
    package = packages,
    quiet = state$options$quiet
  )

  px_opts <- r_process_options(
    func = func,
    args = args,
    system_profile = FALSE,
    user_profile = FALSE
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
  wpkg <- match(worker$package, state$packages$package)

  if (worker$process$get_exit_status()) {
    ## failed, we just stop the whole package
    state$packages$state[wpkg] <- "done"
    ## TODO: update DB that we failed

  } else {
    ## succeeded
    state$packages$state[wpkg] <- "deps_installed"
  }

  state
}
