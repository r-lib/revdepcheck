
#' @importFrom rcmdcheck rcmdcheck_process

do_check <- function(state, task) {

  pkgdir <- state$options$pkgdir
  pkgname <- task$args[[1]]
  iam_old <- task$args[[2]] == "old"

  "!DEBUG Checking `pkgname`"

  dir <- check_dir(pkgdir, "check", pkgname)
  lib <- check_dir(pkgdir, if (iam_old) "pkgold" else "pkgnew", pkgname)
  tarball <- with_envvar(
    c(CRANCACHE_REPOS = "cran", CRANCACHE_QUIET = "yes"),
    crancache::download_packages(pkgname, dir)[,2]
  )

  outdir <- file.path(dir, task$args[[2]])
  unlink(outdir, recursive = TRUE)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  ## We reverse the library, because the new version of the revdep checked
  ## package might have custom non-CRAN dependencies, and we want these
  ## to be first on the library path
  px <- rcmdcheck_process$new(
    path = tarball,
    libpath = rev(lib),
    args = c("-o", outdir)
  )

  ## Update state
  worker <- list(process = px, package = pkgname,
                 stdout = character(), stderr = character(), task = task)
  state$workers <- c(state$workers, list(worker))

  wpkg <- match(worker$package, state$packages$package)
  current_state <- state$packages$state[wpkg]

  new_state <-
    if (current_state == "downloaded" && iam_old) {
      "checking"

    } else if (current_state == "checking" && !iam_old) {
      "checking-checking"

    } else if (current_state == "done-downloaded" && !iam_old) {
      "done-checking"

    } else {
      stop("Internal revdepcheck error, invalid state")
    }
  state$packages$state[wpkg] <- new_state

  state
}

handle_finished_check <- function(state, worker) {
  starttime <- worker$process$get_start_time()
  duration <- as.numeric(Sys.time() - starttime)
  wpkg <- match(worker$package, state$packages$package)

  current_state <- state$packages$state[wpkg]
  my_task <- worker$task
  iam_old <- my_task$args[[2]] == "old"

  new_state <-
    if (current_state == "checking" && iam_old) {
      "done-downloaded"

    } else if (current_state == "checking-checking" && iam_old) {
      "done-checking"

    } else if (current_state == "checking-checking" && !iam_old) {
      "checking-done"

    } else if (current_state == "checking-done" && iam_old) {
      cleanup_library(state, worker)
      "done"

    } else if (current_state == "done-checking" && !iam_old) {
      cleanup_library(state, worker)
      "done"

    } else {
      stop("Internal revdepcheck error, invalid state")
    }
  state$packages$state[wpkg] <- new_state

  chkres <- tryCatch(
    worker$process$parse_results(),
    error = function(e) e
  )

  status <- if (!inherits(chkres, "rcmdcheck")) {
    "PREPERROR"
  } else if (length(chkres$errors)) {
    "ERROR"
  } else if (length(chkres$warnings)) {
    "WARNING"
  } else if (length(chkres$notes)) {
    "NOTE"
  } else {
    "OK"
  }

  summary <- list(
    errors = length(chkres$errors),
    warnings = length(chkres$warnings),
    notes = length(chkres$notes)
  )

  description <- desc::desc(text = chkres$output$description)
  maintainer <- description$get_maintainer()

  db_insert(
    state$options$pkgdir, worker$package,
    version = chkres$version, maintainer = maintainer, status = status,
    which = my_task$args[[2]], duration = duration,
    starttime = as.character(starttime), result = unclass(toJSON(chkres)),
    summary = unclass(toJSON(summary))
  )

  if (new_state == "done") {
    cat("\r")
    print(revdep_results(state$options$pkgdir, worker$package))
    state$progress_bar$tick(tokens = list(packages = checking_now(state)))
  }

  state
}
