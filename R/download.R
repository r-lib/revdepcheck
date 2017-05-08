
do_download <- function(state, task) {

  pkgdir <- state$options$pkgdir
  pkgname <- task$args[[1]]

  "!DEBUG Downloading source of `pkgname`"

  dir <- check_dir(pkgdir, "check", pkgname)

  func <- function(pkgname, dir) {
    crancache::download_packages(pkgname, dir)[,2]
  }

  px_opts <- r_process_options(
    func = func,
    args = list(pkgname = pkgname, dir = dir),
    system_profile = FALSE,
    user_profile = FALSE,
    env = c(CRANCACHE_REPOS = "cran", CRANCACHE_QUIET = "yes")
  )
  px <- r_process$new(px_opts)

  ## update state
  worker <- list(process = px, package = pkgname,
                 stdout = character(), stderr = character(), task = task)
  state$workers <- c(state$workers, list(worker))

  wpkg <- match(worker$package, state$packages$package)
  state$packages$state[wpkg] <- "downloading"

  state
}

handle_finished_download <- function(state, worker) {
  wpkg <- match(worker$package, state$packages$package)
  state$packages$state[wpkg] <- "downloaded"
  state
}
