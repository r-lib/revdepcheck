#' Run revdep checks
#'
#' @description
#' `revdep_check()` runs `R CMD check` on all reverse dependencies of your
#' package. To avoid false positives, it runs `R CMD check` twice: once for
#' released version on CRAN and once for the local development version. It
#' then reports the differences so you can see what checks were previously
#' ok but now fail.
#'
#' Once your package has been successfully submitted to CRAN, you should
#' run `revdep_reset()`. This deletes all files used for checking, freeing
#' up disk space and leaving you in a clean state for the next release.
#'
#' @details
#' `revdep_check()` proceeds in four steps:
#'
#' 1.  **Init**: create the `revdep/` subdirectory if it doesn't already exist,
#'     and save the list of reverse dependencies to check.
#'
#' 1.  **Install**: install the CRAN (released) and local (development)
#'     versions of your package, including all dependencies.
#'
#' 1.  **Run**: run `R CMD check` twice for each reverse dependency, once
#'     for the CRAN version and one for the local version. The checks are
#'     run in parallel using `num_worker` processes.
#'
#' 1.  **Report**: generate reports showing differences between the check
#'     results for the CRAN and local versions of your package. The focus of
#'     the report is on new failures. The reports are saved in `revdep/`.
#'
#' `revdep_check()` is designed to seamlessly resume in the case of failure:
#' just re-run `revdep_check()` and it will start from where it left off.
#' If you want to start again from scratch, run `revdep_reset()`.
#'
#' @param pkg Path to package.
#' @param revdeps A tibble as returned from [revdep_pkgs()]. If
#'   `NULL` (the default), all the revdeps of `pkg` are checked,
#'   including those of Bioconductor.
#'
#'   If a tibble, it must include a column `package` and can contain
#'   additional columns that will be used as groups in the final
#'   report.
#' @param quiet Suppress output from internal processes?
#' @param timeout Maximum time to wait (in seconds) for `R CMD check` to
#'   complete. Default is 10 minutes.
#' @param num_workers Number of parallel workers to use
#' @param dependencies,bioc Deprecated. See [revdep_pkgs()].
#'
#' @seealso To see more details of problems during a run, call
#'   [revdep_summary()] and [revdep_details()] in another process.
#'
#' @section Structure of the revdeps tibble:
#'
#' You normally use [revdep_pkgs()] to create a tibble suitable as
#' `revdeps` argument. You can also create it manually:
#'
#' * There must be a `package` column containing a character vector of
#'   packages to check.
#'
#' * All other columns determine groups of packages. The groups are
#'   checked sequentially and are included in the summary report
#'   created by [revdep_report_summary()].
#'
#' * If a group column named `repo` is present, it is used by
#'   [revdep_report_cran()] to distinguish between CRAN and
#'   Bioconductor packages. It should be a character vector. The value
#'   `"CRAN"` determines whether a package comes from CRAN.
#'
#' @export
#' @importFrom remotes install_local
#' @importFrom withr with_libpaths with_envvar
#' @importFrom crancache install_packages
#' @importFrom tibble tibble as_tibble

revdep_check <- function(pkg = ".",
                         revdeps = NULL,
                         quiet = TRUE,
                         timeout = as.difftime(10, units = "mins"),
                         num_workers = 1,
                         bioc = NULL,
                         dependencies = NULL) {

  if (!is_null(bioc)) {
    abort("The `bioc` argument is defunct. Please use `revdep_pkgs()` instead.")
  }
  if (!is_null(dependencies)) {
    abort("The `dependencies` argument is defunct. Please use `revdep_pkgs()` instead.")
  }
  if (is_character(revdeps)) {
    abort("`revdep_check()` no longer takes dependencies. Please use `revdep_pkgs()` instead.")
  }

  pkg <- pkg_check(pkg)
  dir_setup(pkg)
  if (!db_exists(pkg)) {
    db_setup(pkg)
  }

  if (is_null(revdeps)) {
    revdeps <- revdep_pkgs(pkg_name(pkg))
  } else if (!is_pkgs_revdeps(revdeps)) {
    abort("`revdeps` must be `NULL` or a tibble as returned by `revdep_pkgs()`")
  }

  did_something <- FALSE
  repeat {
    stage <- db_metadata_get(pkg, "todo") %|0|% "init"
    switch(stage,
      init =    revdep_init(pkg, revdeps),
      install = revdep_install(pkg, quiet = quiet),
      run =     revdep_run(pkg, quiet = quiet, timeout = timeout, num_workers = num_workers),
      report =  revdep_final_report(pkg),
      done =    break
    )
    did_something <- TRUE
  }

  if (!did_something) {
    message(
      "* See results of previous run in 'revdep/README.md'\n",
      "* Reset for another run with `revdepcheck::revdep_reset()`"
    )
  }

  invisible()
}

revdep_setup <- function(pkg = ".") {
  pkg <- pkg_check(pkg)
  status("SETUP")

  message("Creating directories and database")

  invisible()
}


revdep_init <- function(pkg = ".", revdeps = chr()) {

  pkg <- pkg_check(pkg)
  db_clean(pkg)              # Delete all records

  "!DEBUG getting reverse dependencies for `basename(pkg)`"
  status("INIT", "Computing revdeps")

  db_todo_add(pkg, revdeps)

  db_metadata_set(pkg, "todo", "install")
  invisible()
}

revdep_install <- function(pkg = ".", quiet = FALSE) {
  pkg <- pkg_check(pkg)
  pkgname <- pkg_name(pkg)

  status("INSTALL", "2 versions")

  dir_create(dir_find(pkg, "old"))
  dir_create(dir_find(pkg, "new"))

  ## Install the package itself, both versions, first the CRAN version
  ## We instruct crancache to only use the cache of CRAN packages
  ## (to avoid installing locally installed newer versions.
  "!DEBUG Installing CRAN (old) version"
  message("Installing CRAN version of ", pkgname)
  package_name <- pkg_name(pkg)[[1]]

  with_envvar(
    c(CRANCACHE_REPOS = "cran,bioc", CRANCACHE_QUIET = "yes"),
    with_libpaths(
      dir_find(pkg, "old"),
      rlang::with_options(
        warn = 2,
        install_packages(pkgname, quiet = quiet, repos = get_repos(bioc = TRUE))
      )
    )
  )

  ## Now the new version
  "!DEBUG Installing new version from `pkg`"
  message("Installing DEV version of ", pkgname)
  with_envvar(
    c(CRANCACHE_REPOS = "cran,bioc", CRANCACHE_QUIET = "yes"),
    with_libpaths(
      dir_find(pkg, "new"),
      rlang::with_options(
        warn = 2,
        install_local(pkg, quiet = quiet, repos = get_repos(bioc = TRUE))
      )
    )
  )

  # Record libraries
  lib <- library_compare(pkg)
  utils::write.csv(lib, file.path(dir_find(pkg, "checks"), "libraries.csv"),
    row.names = FALSE, quote = FALSE)

  db_metadata_set(pkg, "todo", "run")
  invisible()
}

#' @importFrom prettyunits vague_dt

revdep_run <- function(pkg = ".", quiet = TRUE,
                       timeout = as.difftime(10, units = "mins"),
                       num_workers = 1, bioc = TRUE) {

  pkg <- pkg_check(pkg)

  if (!inherits(timeout, "difftime")) {
    timeout <- as.difftime(timeout, units = "secs")
  }

  start <- Sys.time()

  todo <- db_todo(pkg)
  total <- nrow(todo)
  elapsed <- 0L

  groups <- unduplicate(todo$groups)
  n_groups <- nrow(groups)

  if (n_groups > 1L) {
    for (i in seq_len(n_groups)) {
      group <- unlist(groups[i, ])
      group_label <- paste(group, collapse = ":")

      matches <- pmap(todo$groups, function(...) identical(c(...), group))
      idx <- which(unlist(matches))
      group_todo <- todo[idx, ]

      header1 <- sprintf("CHECK %s (%s/%s)", group_label, i, n_groups)
      header2 <- paste0(nrow(group_todo), " packages")
      status(header1, header2)

      revdep_run_group(
        pkg = pkg,
        todo = group_todo$package,
        total = total,
        elapsed = elapsed,
        quiet = quiet,
        timeout = timeout,
        num_workers = num_workers,
        bioc = bioc
      )

      elapsed <- elapsed + nrow(group_todo)

      if (i != n_groups) {
        cat("\n")
      }
    }
  } else {
    status("CHECK", paste0(nrow(todo), " packages"))
    revdep_run_group(
      pkg = pkg,
      todo = todo$package,
      total = total,
      elapsed = elapsed,
      quiet = quiet,
      timeout = timeout,
      num_workers = num_workers,
      bioc = bioc
    )
  }

  end <- Sys.time()

  status <- report_status(pkg)
  cat_line(green("OK: "), status$ok)
  cat_line(red("BROKEN: "), status$broken)
  cat_line("Total time: ", vague_dt(end - start, format = "short"))

  db_metadata_set(pkg, "todo", "report")
  invisible()
}
revdep_run_group <- function(pkg, todo, total, elapsed = 0, quiet = TRUE,
                             timeout = as.difftime(10, units = "mins"),
                             num_workers = 1, bioc = TRUE) {
  pkgname <- pkg_name(pkg)

  state <- list(
    options = list(
      pkgdir = pkg,
      pkgname = pkgname,
      quiet = quiet,
      timeout = timeout,
      num_workers = num_workers
    ),
    packages = data.frame(
      package = todo,
      state = if (length(todo)) "todo" else character(),
      stringsAsFactors = FALSE
    ),
    total = total,
    elapsed = elapsed
  )

  run_event_loop(state)
}

revdep_final_report <- function(pkg = ".") {
  db_metadata_set(pkg, "todo", "done")
  status("REPORT")
  revdep_report(pkg)
}

report_exists <- function(pkg) {
  root <- dir_find(pkg, "root")
  file.exists(file.path(root, "README.md")) && file.exists(file.path(root, "problems.md"))
}

#' @export
#' @rdname revdep_check

revdep_reset <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  if (exists(pkg, envir = dbenv))
    rm(list = pkg, envir = dbenv)

  unlink(dir_find(pkg, "lib"), recursive = TRUE)
  unlink(dir_find(pkg, "checks"), recursive = TRUE)
  unlink(dir_find(pkg, "db"), recursive = TRUE)

  invisible()
}

#' @importFrom crayon bold

status <- function(title, info = "") {
  cat_line(rule(left = bold(title), right = info))
}
