
#' @importFrom rcmdcheck rcmdcheck_process

check_proc <- function(pkgdir, pkgname, version = c("old", "new"),
                       env = character()) {
  version <- match.arg(version)

  dir <- dir_find(pkgdir, "check", pkgname)
  tarball <- latest_file(dir(dir, pattern = "\\.tar\\.gz$", full.names = TRUE))
  if (length(tarball) == 0) {
    stop(sprintf(
      "Internal error for package %s. No *.tar.gz file found.",
      pkgname
    ), call. = FALSE)
  }

  out <- file.path(dir, version)
  unlink(out, recursive = TRUE)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  ## We reverse the library, because the new version of the revdep checked
  ## package might have custom non-CRAN dependencies, and we want these
  ## to be first on the library path
  lib <- rev(dir_find(pkgdir, paste0("pkg", version), pkgname))
  library_info(file.path(out, "libraries.txt"), lib)

  with_envvar(
    c("R_ENVIRON_USER" = tempdir(), "R_LIBS" = "", "NO_COLOR" = "true", env),
    rcmdcheck_process$new(
      path = tarball,
      libpath = lib,
      args = c("--no-manual", "--no-build-vignettes", "-o", out)
    )
  )
}

check_task <- function(state, task) {
  pkgdir <- state$options$pkgdir
  pkgname <- task$args[[1]]
  version <- task$args[[2]]

  "!DEBUG Checking `pkgname`"
  px <- check_proc(pkgdir, pkgname, version = version, env = state$options$env)

  ## Update state
  worker <- list(process = px, package = pkgname,
                 stdout = character(), stderr = character(), task = task)
  state$workers <- c(state$workers, list(worker))

  wpkg <- match(worker$package, state$packages$package)
  current_state <- state$packages$state[wpkg]

  new_state <-
    if (current_state == "downloaded" && version == "old") {
      "checking"

    } else if (current_state == "checking" && version == "new") {
      "checking-checking"

    } else if (current_state == "done-downloaded" && version == "new") {
      "done-checking"

    } else {
      stop("Internal revdepcheck error, invalid state")
    }
  state$packages$state[wpkg] <- new_state

  state
}

#' Environment variables to set for install and check processes while
#' running the reverse dependency check
#'
#' @param force_suggests Whether to force the installation of the
#'   suggested packages.
#' @return Named character vector.
#'
#' @export

revdep_env_vars <- function(force_suggests = FALSE) {
  c(
    # Switch off expensive check for package version
    # https://github.com/hadley/devtools/issues/1271
    if (getRversion() >= "3.4.0" && as.numeric(R.version[["svn rev"]]) >= 70944) {
      c("_R_CHECK_CRAN_INCOMING_REMOTE_" = "FALSE")
    } else {
      c("_R_CHECK_CRAN_INCOMING_" = "FALSE")
    },
    "_R_CHECK_FORCE_SUGGESTS_" = as.character(force_suggests),
    "RGL_USE_NULL" = "TRUE",
    DISPLAY = "",
    RSTUDIO = 0,
    RSTUDIO_CONSOLE_WIDTH = 80,
    R_COMPILE_AND_INSTALL_PACKAGES = "never"
  )
}

check_done <- function(state, worker) {
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

  chkres <- if (isTRUE(worker$killed)) {
    "Process was killed while checking"
  } else {
    tryCatch(
      worker$process$parse_results(),
      error = function(e) e
    )
  }

  cleanup_chkres(state, worker, iam_old)

  status <- if (isTRUE(worker$killed)) {
    "TIMEOUT"
  } else if (!inherits(chkres, "rcmdcheck")) {
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

  description <- desc::desc(text = chkres$description)
  maintainer <- description$get_maintainer()

  db_insert(
    state$options$pkgdir, worker$package,
    version = chkres$version, maintainer = maintainer, status = status,
    which = my_task$args[[2]], duration = duration,
    starttime = as.character(starttime), result = unclass(toJSON(chkres)),
    summary = unclass(toJSON(summary))
  )

  if (new_state == "done") {
    clear_line()

    comparison <- db_results(state$options$pkgdir, worker$package)[[1]]
    print(summary(comparison))

    state$progress_bar$tick(tokens = list(packages = checking_now(state)))
  }

  state
}

check <- function(pkgdir, pkgname, iam_old = TRUE) {
  proc <- check_proc(pkgdir, pkgname, iam_old)
  proc$wait()

  res <- proc$parse_results()
  print(res)

  invisible(res)
}

library_info <- function(file = "", libpath = .libPaths()) {
  libraries <- map(libpath, installed.packages)

  package_list <- function(library) {
    nv <- paste0(format(library[, "Package"]), " (", library[, "Version"], ")")
    paste0(nv, "\n", collapse = "")
  }

  library_sum <- map_chr(libraries, package_list)
  cat(
    paste0("Library: ", libpath, "\n", library_sum, collapse = "\n"),
    file = file
  )
}

