
#' @importFrom processx process
#' @importFrom callr r_process r_process_options

do_deps_install <- function(state, task) {

  package_name <- task$args[[1]]

  "!DEBUG Install dependencies for package `package_name`"
  func <- function(libdir, packages, quiet) {
    ifun <- crancache::install_packages
    withr::with_libpaths(
      libdir, {
        print(.libPaths())
        ifun(
          packages,
          dependencies = TRUE,
          lib = libdir,
          quiet = quiet,
          use_cache = "cran"
        )
      }
    )
  }

  ## We have to do this "manually", because some of the dependencies
  ## might be also dependencies of crancache, so they will be already
  ## installed in another library directory, and also loaded.
  ## But we want to install everything into the package's specific library,
  ## because this is the only library used for the check.
  packages <- deps_for_package(package_name)

  args <- list(
    libdir = check_dir(state$options$pkgdir, "pkg", package_name),
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

  worker <- list(process = px, package = package_name,
                 stdout = character(), stderr = character(), task = task)
  state$workers <- c(state$workers, list(worker))
  state
}

handle_finished_deps_install <- function(state, worker) {
  wpkg <- match(worker$package, state$packages$package)

  if (worker$process$get_exit_status()) {
    ## failed
    state$packages$state[wpkg] <- "done"
    ## TODO: update DB

  } else {
    ## succeeded
    state$packages$state[wpkg] <- "deps_installed"
    ## TODO:update DB
  }

  state
}
