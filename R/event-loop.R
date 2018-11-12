
#' This is the event loop of the revdep check process
#'
#' @param state The full state of the check process:
#'   * `options` contains all check parameters.
#'   * `packages` is a data frame with the packages to check.
#'   See details below.
#'
#' @details
#' `state$packages` is a data frame with columns:
#' * `package`: the name of the package
#' * `state`: where we are with its check. Possible values:
#'     * `todo`: haven't done anything yet
#'     * `deps_installing`: the dependencies are being installed now
#'     * `deps_installed`: the dependencies were already installed
#'     * `downloading`: the source package to check is being downloaded
#'     * `downloaded`: the source package was downloaded
#'     * `checking`: checking with the old version right now
#'     * `checking-checking`: checking with both versions right now
#'     * `done-checking`: done with the old version, checking with the new
#'        version right now
#'     * `checking-done`: checking with the old version, new version was
#'        already done.
#'     * `done-downloaded`: done with the old version, check with new
#'        version has not started yet
#'     * `done`: packages was checked with both versions
#'
#' We only start the check with the new version after the check with the
#' old version, which simplifies the state transitions a bit.
#'
#' @keywords internal
#' @importFrom progress progress_bar

run_event_loop <- function(state) {
  "!DEBUG running event loop"

  if (nrow(state$packages) == 0) {
    return()
  }

  ## Kill all child processes if we quit from this function
  on.exit(remove_workers(state), add = TRUE)

  ## This is a list of worker processes
  state$workers <- list()

  ## Our global progress bar
  state$progress_bar <- progress_bar$new(
    total = state$total,
    format = "[:current/:total] :elapsedfull | ETA: :eta | :packages"
  )

  if (!is_null(state$elapsed)) {
    state$progress_bar$tick(state$elapsed)
  }

  # Initialise one task for each worker
  for (i in seq_len(state$options$num_workers)) {
    state$progress_bar$tick(0, tokens = list(packages = checking_now(state)))
    task <- schedule_next_task(state)
    state <- do_task(state, task)
  }

  while (1) {
    "!DEBUG event loop iteration, `length(state$workers)` workers"
    check_for_timeouts(state)
    if (are_we_done(state)) break;
    state$progress_bar$tick(0, tokens = list(packages = checking_now(state)))
    events <- poll(state)
    state <- handle_events(state, events)
    task  <- schedule_next_task(state)
    state <- do_task(state, task)
    if (package_version(getNamespaceVersion(asNamespace("processx"))) <=
        "3.0.0") gc()
  }

  "!DEBUG event loop is done"
  NULL
}

## In case of a timeout, we just kill the process here.
## This will trigger an event for it, that will be picked up by
## handle_events(). If we failed to kill it (because it finished, just
## before the kill signal, that is fine, too, then handle_events()
## will consider it as a normal termination.

check_for_timeouts <- function(state) {
  now <- Sys.time()
  for (w in state$workers) {
    if (now - w$process$get_start_time() > state$options$timeout &&
        w$process$is_alive()) {
      "!DEBUG Killing worker for package `w$package`"
      w$killed <- TRUE
      w$process$kill()
    }
  }
}

are_we_done <- function(state) {
  all(state$packages$state == "done")
}

checking_now <- function(state) {
  workers <- compact(state$workers)
  if (length(workers) == 0) {
    return("")
  }

  pkgs <- map_chr(workers, "[[", "package")

  tasks <- map_chr(workers, function(x) x$task$name)
  task_lookup <- c("download" = "D", "deps_install" = "I", "check" = "C")
  tasks_abbr <- unname(task_lookup[tasks])
  pkg_tasks <- split(tasks_abbr, pkgs)
  pkg_sum <- map_chr(pkg_tasks, paste, collapse = "")

  width <- getOption("width") - 38 # conservative estimate
  str <- paste0(names(pkg_tasks), " [", pkg_sum, "]", collapse = ", ")
  paste0("(", length(pkgs), ") ", str_trunc(str, width))
}

poll <- function(state) {
  if (length(state$workers)) {
    timeout <- get_timeout(state)
    procs <- map(state$workers, function(x) x$process)

    "!DEBUG poll with timeout of `timeout` ms"
    res <- processx::poll(procs, ms = timeout)
    map_lgl(res, function(x) "ready" %in% x)

  } else {
    "!DEBUG nothing to poll"
    logical()
  }
}

get_timeout <- function(state) {
  ts <- map_dbl(
    state$workers,
    get_process_waiting_time,
    timeout = state$options$timeout
  )
  max(min(ts, 200), 0)
}

get_process_waiting_time <- function(worker, timeout) {
  have_time <- timeout - (Sys.time() - worker$process$get_start_time())
  units(have_time) <- "secs"
  as.integer(max(as.numeric(have_time) * 1000, 0))
}

handle_events <- function(state, events) {
  for (i in which(events)) state <- handle_event(state, i)
  state$workers <- drop_nulls(state$workers)
  state
}

handle_event <- function(state, which) {

  "!DEBUG handle event, package `state$workers[[which]]$package`"
  proc <- state$workers[[which]]$process

  ## Read out stdout and stderr. If process is done, then read out all
  if (proc$is_alive()) {
    state$workers[[which]]$stdout <-
      c(state$workers[[which]]$stdout, out <- proc$read_output(n = 10000))
    state$workers[[which]]$stderr <-
      c(state$workers[[which]]$stderr, err <- proc$read_error(n = 10000))
  } else {
    state$workers[[which]]$stdout <-
      c(state$workers[[which]]$stdout, out <- proc$read_all_output())
    state$workers[[which]]$stderr <-
      c(state$workers[[which]]$stderr, err <- proc$read_all_error())
  }

  "!DEBUG read out `nchar(out)`/`nchar(err)` characters"

  ## If there is still output, then wait a bit more
  if (proc$is_incomplete_output() || proc$is_incomplete_error()) {
    return(state)
  }

  ## Otherwise update the state, and the DB
  worker <- state$workers[[which]]
  state$workers[which] <- list(NULL)

  ## Cut stdout and stderr to lines
  worker$stdout <- cut_into_lines(worker$stdout)
  worker$stderr <- cut_into_lines(worker$stderr)

  if (worker$task$name == "deps_install") {
    deps_install_done(state, worker)

  } else if (worker$task$name == "download") {
    download_done(state, worker)

  } else if (worker$task$name == "check") {
    check_done(state, worker)
  }
}

#' Decide what to do next, from the current state
#'
#' In we have reached the allowed number of workers, then we schedule an
#' idle job, we just need to wait until a worker is done.
#'
#' Otherwise we schedule a job. In general the strategy is to finish check
#' as soon as possible, so if a package is in `deps_installed`, then we
#' schedule a check. Otherwise, if a package is in `todo`, then we
#' schedule a dependency install.
#'
#' If there is nothing we can do now, then we schedule an idle job, i.e.
#' just wait until a worker gets done.
#'
#' @param state See [run_event_loop()] for a description.
#'
#' @keywords internal

schedule_next_task <- function(state) {
  "!DEBUG schedule next task"

  ## Cannot run more workers?
  if (length(state$workers) >= state$options$num_workers) {
    "!DEBUG schedule an idle task"
    return(task("idle"))
  }

  ## done-downloaded -> done-checking
  ready <- state$packages$state == "done-downloaded"
  if (any(ready)) {
    pkg <- state$packages$package[ready][1]
    return(task("check", pkg, "new"))
  }

  ## checking -> checking-checking
  ready <- state$packages$state == "checking"
  if (any(ready)) {
    pkg <- state$packages$package[ready][1]
    "!DEBUG schedule checking `pkg` with the new version"
    return(task("check", pkg, "new"))
  }

  ## downloaded -> checking
  ready <- state$packages$state == "downloaded"
  if (any(ready)) {
    pkg <- state$packages$package[ready][1]
    "!DEBUG schedule checking `pkg` with the old version"
    return(task("check", pkg, "old"))
  }

  ## deps_installed -> downloading
  ready <- state$packages$state == "deps_installed"
  if (any(ready)) {
    pkg <- state$packages$package[ready][1]
    "!DEBUG schedule downloading `pkg` with the old version"
    return(task("download", pkg, 1L))
  }

  ## todo -> deps_installing
  ready <- state$packages$state == "todo"
  if (any(ready)) {
    pkg <- state$packages$package[ready][1]
    "!DEBUG schedule dependency installs for `pkg`"
    return(task("deps_install", pkg))
  }

  task("idle")
}

task <- function(name, ...) {
  list(name = name, args = list(...))
}

do_task <- function(state, task) {
  if (task$name == "idle") {
    ## Do nothing, return the state as it is
    "!DEBUG do an idle task"
    state

  } else if (task$name == "deps_install") {
    "!DEBUG do a dependency install task: `task[[2]]`"
    deps_install_task(state, task)

  } else if (task$name == "download") {
    "!DEBUG do a download task: `task[[2]]`"
    download_task(state, task)

  } else if (task$name == "check") {
    "!DEBUG do a check task: `task[[2]]`"
    check_task(state, task)

  } else {
    stop("Unknown task")
  }
}

remove_workers <- function(state) {
  "!DEBUG remove `length(state$workers)` workers"
  for (w in state$workers) w$process$kill()
}
