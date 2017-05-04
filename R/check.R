
#' @importFrom rcmdcheck rcmdcheck_process

do_check <- function(state, task) {

  pkgdir <- state$options$pkgdir
  pkgname <- task$args[[1]]
  iam_old <- task$args[[2]] == "old"

  "!DEBUG Checking `pkgname`"

  dir <- check_dir(pkgdir, "check", pkgname)
  lib <- check_dir(pkgdir, if (iam_old) "pkgold" else "pkgnew", pkgname)
  tarball <- crancache::download_packages(pkgname, dir)[,2]

  ## We reverse the library, because the new version of the revdep checked
  ## package might have custom non-CRAN dependencies, and we want these
  ## to be first on the library path
  px <- rcmdcheck_process$new(
    path = tarball,
    libpath = rev(lib)
  )

  ## Update state
  worker <- list(process = px, package = pkgname,
                 stdout = character(), stderr = character(), task = task)
  state$workers <- c(state$workers, list(worker))

  wpkg <- match(worker$package, state$packages$package)
  current_state <- state$packages$state[wpkg]

  new_state <-
    if (current_state == "deps_installed" && iam_old) {
      "checking"

    } else if (current_state == "checking" && !iam_old) {
      "checking-checking"

    } else if (current_state == "done-deps_installed" && !iam_old) {
      "done-checking"

    } else {
      stop("Internal revdepcheck error, invalid state")
    }
  state$packages$state[wpkg] <- new_state

  state
}

handle_finished_check <- function(state, worker) {
  wpkg <- match(worker$package, state$packages$package)

  current_state <- state$packages$state[wpkg]
  my_task <- worker$task
  iam_old <- my_task$args[[2]] == "old"

  new_state <-
    if (current_state == "checking" && iam_old) {
      "done-deps_installed"

    } else if (current_state == "checking-checking" && iam_old) {
      "done-checking"

    } else if (current_state == "checking-checking" && !iam_old) {
      "checking-done"

    } else if (current_state == "checking-done" && iam_old) {
      "done"

    } else if (current_state == "done-checking" && !iam_old) {
      "done"

    } else {
      stop("Internal revdepcheck error, invalid state")
    }
  state$packages$state[wpkg] <- new_state

  ## TODO: update DB

  state
}
