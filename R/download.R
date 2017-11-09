download_opts <- function(pkgdir, pkgname) {
  dir <- dir_find(pkgdir, "check", pkgname)

  func <- function(pkgname, dir, repos) {
    dest <- crancache::download_packages(pkgname, dir, repos = repos)[,2]
    file.copy(dest, dir)
  }

  r_process_options(
    func = func,
    args = list(pkgname = pkgname, dir = dir, repos = get_repos(bioc = TRUE)),
    system_profile = FALSE,
    user_profile = FALSE,
    env = c(CRANCACHE_REPOS = "cran,bioc", CRANCACHE_QUIET = "yes")
  )
}

download_task <- function(state, task) {
  pkgdir <- state$options$pkgdir
  pkgname <- task$args[[1]]

  "!DEBUG Downloading source of `pkgname`"
  px_opts <- download_opts(pkgdir, pkgname)
  px <- r_process$new(px_opts)

  ## update state
  worker <- list(process = px, package = pkgname,
                 stdout = character(), stderr = character(), task = task)
  state$workers <- c(state$workers, list(worker))

  wpkg <- match(worker$package, state$packages$package)
  state$packages$state[wpkg] <- "downloading"

  state
}

download_done <- function(state, worker) {
  wpkg <- match(worker$package, state$packages$package)
  state$packages$state[wpkg] <- "downloaded"
  state
}

download <- function(pkgdir, pkgname, new_session = FALSE) {
  px_opts <- download_opts(pkgdir, pkgname)
  execute_r(px_opts, new_session = new_session)
}
