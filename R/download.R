download_opts <- function(pkgdir, pkgname, bioc, cran) {
  dir <- dir_find(pkgdir, "check", pkgname)

  func <- function(pkgname, dir, repos) {
    dest <- crancache::download_packages(pkgname, dir, repos = repos)[, 2]
    file.copy(dest, dir)
  }

  r_process_options(
    func = func,
    args = list(
      pkgname = pkgname,
      dir = dir,
      repos = get_repos(bioc = bioc, cran = cran)
    ),
    system_profile = FALSE,
    user_profile = FALSE,
    env = c(CRANCACHE_REPOS = "cran,bioc", CRANCACHE_QUIET = "yes")
  )
}

download_task <- function(state, task) {
  pkgdir <- state$options$pkgdir
  pkgname <- task$args[[1]]
  bioc <- state$options$bioc
  cran <- state$options$cran

  "!DEBUG Downloading source of `pkgname`"
  px_opts <- download_opts(pkgdir, pkgname, bioc, cran)
  px <- r_process$new(px_opts)

  ## update state
  worker <- list(
    process = px,
    package = pkgname,
    stdout = character(),
    stderr = character(),
    task = task
  )
  state$workers <- c(state$workers, list(worker))

  wpkg <- match(worker$package, state$packages$package)
  state$packages$state[wpkg] <- "downloading"

  state
}


pkg_tarball <- function(pkgdir, pkgname) {
  dir <- dir_find(pkgdir, "check", pkgname)
  dir(dir, pattern = "\\.tar\\.gz$", full.names = TRUE)
}

download_done <- function(state, worker) {
  pkgdir <- state$options$pkgdir
  pkgname <- worker$task$args[[1]]

  tarball <- pkg_tarball(pkgdir, pkgname)
  if (!length(tarball)) {
    n_attempts <- worker$task$args[[2]]
    if (n_attempts > 20L) {
      # Package download failed after max retries - skip this package gracefully
      wpkg <- match(worker$package, state$packages$package)
      cleanup_library(state, worker)
      state$packages$state[wpkg] <- "done"

      status <- "PREPERROR"
      starttime <- worker$process$get_start_time()
      duration <- as.numeric(Sys.time() - starttime)

      result <- list(
        stdout = worker$stdout,
        stderr = worker$stderr,
        errormsg = sprintf(
          "Failed downloading package %s after %d attempts. Package may be archived or unavailable.",
          pkgname,
          n_attempts
        )
      )

      for (which in c("old", "new")) {
        db_insert(
          pkgdir,
          pkgname,
          version = NULL,
          status = status,
          which = which,
          duration = duration,
          starttime = as.character(starttime),
          result = unclass(toJSON(result)),
          summary = NULL
        )
      }

      message(
        sprintf(
          "Skipping package %s: download failed (package may be archived or unavailable)",
          pkgname
        )
      )
      return(state)
    } else {
      return(download_task(state, task("download", pkgname, n_attempts + 1L)))
    }
  }

  wpkg <- match(worker$package, state$packages$package)
  state$packages$state[wpkg] <- "downloaded"
  state
}

download <- function(pkgdir, pkgname, new_session = FALSE) {
  px_opts <- download_opts(pkgdir, pkgname)
  execute_r(px_opts, new_session = new_session)
}
