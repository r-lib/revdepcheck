#' @importFrom R6 R6Class

## A check process that runs `R CMD check` with a chosen R build.
##
## rcmdcheck::rcmdcheck_process goes through callr, and callr always starts
## the R of the current session for `R CMD` processes (its `arch` option is
## only honoured for plain R processes). So this starts the process itself,
## keeping just enough of rcmdcheck's process class for its result parsing:
## the accumulated output, the killed flag, and the DESCRIPTION. It only
## accepts a source package tarball, which is all we need.

rdev_check_process <- R6Class(
  "rdev_check_process",
  inherit = processx::process,
  public = list(
    initialize = function(
      path,
      args = character(),
      libpath = .libPaths(),
      env = character(),
      rbin = file.path(R.home("bin"), "R")
    ) {
      rdev_process_init(self, private, super, path, args, libpath, env, rbin)
    },

    parse_results = function() {
      rdev_process_parse(self, private)
    },

    read_output_lines = function(...) {
      lines <- super$read_output_lines(...)
      private$cstdout <- c(private$cstdout, paste0(lines, "\n"))
      lines
    },

    read_output = function(...) {
      out <- super$read_output(...)
      private$cstdout <- c(private$cstdout, out)
      out
    },

    kill = function(...) {
      private$killed <- TRUE
      res <- super$kill(...)
      ## a killed check is never parsed, so clean up here
      unlink(private$check_dir, recursive = TRUE)
      res
    }
  ),
  private = list(
    description = NULL,
    check_dir = NULL,
    cstdout = character(),
    killed = FALSE
  )
)

rdev_process_init <- function(
  self,
  private,
  super,
  path,
  args,
  libpath,
  env,
  rbin
) {
  path <- normalizePath(path, mustWork = TRUE)
  if (file.info(path)$isdir) {
    stop("`path` must be a source package tarball", call. = FALSE)
  }

  ## The tarball is checked from a scratch directory, removed by parse_results()
  check_dir <- tempfile("rdev-check-")
  dir.create(check_dir, recursive = TRUE)
  targz <- file.path(check_dir, basename(path))
  file.copy(path, targz)

  private$description <- desc::desc(file = path)
  private$check_dir <- check_dir

  ## No user profile, and only the libraries we were given. Callers may
  ## still override any of these through `env`.
  chkenv <- c(
    callr::rcmd_safe_env(),
    R_LIBS = paste(libpath, collapse = .Platform$path.sep),
    R_PROFILE_USER = file.path(check_dir, "no-profile")
  )
  if (length(env)) {
    chkenv[names(env)] <- env
  }

  super$initialize(
    command = rbin,
    args = c("CMD", "check", basename(targz), args),
    stdout = "|",
    stderr = "2>&1",
    poll_connection = TRUE,
    env = c("current", chkenv),
    wd = check_dir
  )

  invisible(self)
}

rdev_process_parse <- function(self, private) {
  if (self$is_alive()) {
    stop("Process still alive", call. = FALSE)
  }
  if (self$has_output_connection()) {
    self$read_output_lines()
  }
  on.exit(unlink(private$check_dir, recursive = TRUE), add = TRUE)

  ## rcmdcheck does not export its result constructor (checked against
  ## rcmdcheck 1.4.0). stderr is merged into stdout above.
  rcmdcheck:::new_rcmdcheck(
    stdout = paste(private$cstdout, collapse = ""),
    stderr = "",
    description = private$description,
    status = self$get_exit_status(),
    duration = as.double(Sys.time() - self$get_start_time(), units = "secs"),
    timeout = private$killed
  )
}
