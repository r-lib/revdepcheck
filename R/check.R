
#' @importFrom processx process

do_check <- function(state, task) {
  ## TODO
  px <- process$new("sleep", "1", stdout = "|", stderr = "|")
  worker <- list(process = px, package = task[[2]], stdout = character(),
                 stderr = character(), task = task)
  state$workers <- c(state$workers, list(worker))
  state
}

handle_finished_check <- function(state, worker) {
  wpkg <- match(worker$package, state$packages$package)

  state$packages$state[wpkg] <- "done"

  ## TODO: update DB

  state
}
