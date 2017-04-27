
#' @importFrom processx process

do_deps_install <- function(state, task) {
  ## TODO
  px <- process$new("sleep", "5", stdout = "|", stderr = "|")
  worker <- list(process = px, package = task[[2]], stdout = character(),
                 stderr = character(), task = task)
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
