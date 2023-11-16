#' Monitor the status of a cloud job
#'
#' The format of the status bar is
#' `[jobs_queued/jobs_running/jobs_succeeded/jobs_failed - total_jobs] time_elapsed | ETA: estimate_time_remaining`
#'
#' @param update_interval The number of seconds between querying for updates
#' @family cloud
#' @importFrom cli cli_format cli_status_update col_green col_blue col_red
#'   style_bold cli_status_clear cli_status cli_alert
#' @inheritParams cloud_report
#' @export
cloud_status <- function(job_name = cloud_job(), update_interval = 10) {
  status_id <- cli_status("Status of {.val {job_name}}")

  info <- cloud_job_info(job_name)

  started_time <- as.POSIXct(info$started_timestamp, tz = "UTC", format="%Y-%m-%dT%H:%M:%OS")

  cloud_status_check <- function(job_name) {

    status <- cloud_job_status(job_name)

    if (length(status) == 0) {
      stop("No job with name: '", job_name, call. = FALSE)
    }

    size <- status$size
    results <- unlist(status$statusSummary)

    if (!is.integer(results)) {
      return(NA)
    }

    names(results) <- tolower(names(results))
    results <- results[c("pending", "runnable", "starting", "running", "succeeded", "failed")]

    num_completed <- sum(results[c("succeeded", "failed")])
    num_queued <- sum(results[c("pending", "runnable")])
    num_running <- sum(results[c("starting", "running")])

    current_time <- Sys.time()

    elapsed <- hms::as_hms(as.integer(difftime(current_time, started_time, units = "secs")))

    status_bar_text <- "[{num_queued}/{col_blue(num_running)}/{col_green(results[['succeeded']])}/{col_red(results[['failed']])} - {.strong {size}}] {elapsed}"

    if (results[["failed"]] > 0) {
        cli_status_clear(id = status_id, result = "failed", msg_failed = paste0("{.emph FAILED}: ", status_bar_text))
        cli_alert("run {.run revdepcheck::cloud_summary()} for interactive results")
        cli_alert("run {.run revdepcheck::cloud_report()} for markdown reports")
        return(FALSE)
    }

    if (num_completed == length(info$revdep_packages)) {
        cli_status_clear(id = status_id, result = "done", msg_done = paste0("{.emph SUCCEEDED}: ", status_bar_text))
        cli_alert("run {.run revdepcheck::cloud_summary()} for interactive results")
        cli_alert("run {.run revdepcheck::cloud_report()} for markdown reports")
        return(TRUE)
    }

    cli::cli_status_update(id = status_id, status_bar_text)
    return(NA)
  }

  while(is.na(res <- cloud_status_check(job_name))) {
    Sys.sleep(update_interval)
  }

  return(invisible(res))
}

#' Fetch results from the cloud
#'
#' Intended mainly for internal and expert use. This function when needed by
#' [cloud_report()] and [cloud_summary()], so it is unlikely you will need to
#' call it explicitly.
#'
#' @keywords internal
#' @family cloud
#' @inheritParams cloud_report
#' @importFrom curl new_handle handle_setheaders new_pool multi_add multi_run handle_setopt
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done pb_percent
#' @export
cloud_fetch_results <- function(job_name = cloud_job(pkg = pkg), pkg = ".") {
  pkg <- pkg_check(pkg)
  cloud <- dir_find(pkg, "cloud")

  info <- cloud_job_info(job_name)

  out_dir <- file.path(cloud, job_name)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE, mode = "0744")

  rel_out_dir <- sub(paste0(pkg_check(pkg), "/"), "", out_dir, fixed = TRUE)
  cli_alert_info("Syncing results to {.file {rel_out_dir}}")

  packages <- info$revdep_packages

  out_files <- file.path(out_dir, paste0(packages, ".tar.gz"))

  to_download <- !file.exists(out_files)

  pb <- cli_progress_bar(format = "Downloading package results: {pb_percent}", total = sum(to_download))
  handle_success <- function(res) {
    if (res$status_code >= 400) {
      out_file <- sprintf("%s/%s.tar.gz", out_dir, basename(dirname(res$url)))
      unlink(out_file)
    }
    cli_progress_update(id = pb)
  }
  pool <- new_pool()
  for (i in which(to_download)) {
    out_file <- out_files[[i]]
    package <- packages[[i]]
    url <- sprintf("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com/staging/check/%s/packages/%s/results.tar.gz", job_name, package)

    handle <- new_handle()
    handle_setopt(handle, url = enc2utf8(url))
    handle_setheaders(handle, "x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY"), Accept = "application/x-gzip")
    multi_add(handle = handle, done = handle_success, pool = pool, data = out_file)
  }
  out <- multi_run(pool = pool)
  cli_progress_done(id = pb)

  to_extract <- file.exists(out_files) & !dir.exists(file.path(out_dir, packages))

  pb2 <- cli_progress_bar(format = "Extracting package results: {pb_percent}", total = sum(to_extract))
  for (i in which(to_extract)) {
    out_file <- out_files[[i]]
    utils::untar(out_file, exdir = out_dir)
    cli_progress_update(id = pb2)
  }
  cli_progress_done(id = pb2)
}

#' Submit a reverse dependency checking job to the cloud
#'
#' @param tarball A pre-built package tarball, if `NULL` a tarball will be
#'   automatically built for the package at `pkg` by [pkgbuild::build()].
#' @param revdep_packages A character vector of packages to check, if `NULL`
#'   equal to [cran_revdeps()]
#' @param r_version The R version to use.
#' @param check_args Additional argument to pass to `R CMD check`
#' @param extra_revdeps Additional packages to use as source for reverse
#'   dependencies.
#' @param bioc Also check revdeps that live in Bioconductor? Default `FALSE`.
#'   Note that the cloud revdep check service does not currently include system
#'   dependencies of Bioconductor packages, so there is potential for more
#'   failed checks.
#' @returns The AWS Batch job name
#' @inheritParams revdep_check
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger
#' @importFrom httr GET PATCH POST stop_for_status add_headers content
#' @family cloud
#' @export
cloud_check <- function(pkg = ".",
  tarball = NULL,
  revdep_packages = NULL,
  extra_revdeps = NULL,
  r_version = "4.3.1",
  check_args = "--no-manual",
  bioc = FALSE) {
  if (is.null(tarball)) {
    cli::cli_alert_info("Building package tarball")
    pkg <- pkg_check(pkg)
    tarball <- pkgbuild::build(path = pkg, quiet = TRUE)
  }

  package_name <- desc::desc_get_field("Package", file = tarball)
  package_version <- as.character(desc::desc_get_version(file = tarball))

  # Lookup revdeps with R, as the RSPM db seems not quite right, for instance
  # it seems to include archived packages.
  if (is.null(revdep_packages)) {
    revdep_packages <- setdiff(
      cran_revdeps(c(package_name, extra_revdeps), bioc = bioc),
      package_name
    )
  }

  if (length(revdep_packages) == 1) {
    stop("`revdepcheck::cloud_check()` can't work with exactly 1 revdep package (AWS batch jobs must have more than one job)", call. = FALSE)
  }

  post_response <- POST("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com/staging/check",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    body = list(
      package_name = package_name,
      package_version = package_version,
      revdep_packages = revdep_packages,
      r_version = r_version,
      check_args = check_args
    ),
    encode = "json"
  )

  cloud_stop_for_status(post_response)

  post_content <- content(post_response)
  presigned_url <- post_content[["_source_presigned_url"]]
  job_name <- post_content[["id"]]

  cli_alert_info("Creating cloud job {.val {job_name}}")

  cli_alert_info("Uploading package tarball")
  curl::curl_upload(tarball, presigned_url, verbose = FALSE)
  cli_alert_success("Uploaded package tarball")

  cli_alert_info("Spawning batch job")
  patch_response <- PATCH("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    path = paste0("staging/check", "/", job_name),
    body = list(status = "running"),
    encode = "json"
  )
  cloud_stop_for_status(patch_response)
  cli_alert_success("Spawned batch job")

  patch_content <- content(patch_response)
  job_name <- patch_content$id

  # Create output directory and set as active job
  cloud_job(job_name = job_name)
  cloud <- dir_find(pkg, "cloud")
  out_dir <- file.path(cloud, job_name)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE, mode = "744")
  cloud_job(job_name)

  cli_alert("Run {.run revdepcheck::cloud_status()} to monitor job status")

  invisible(job_name)
}

#' Cancel a running cloud run
#'
#' @inheritParams cloud_report
#' @family cloud
#' @export
cloud_cancel <- function(job_name = cloud_job()) {

  patch_response <- PATCH("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    path = paste0("staging/check", "/", job_name),
    body = list(status = "cancelled"),
    encode = "json"
  )

  cloud_stop_for_status(patch_response)
}

#' @importFrom httr status_code content headers http_status
cloud_stop_for_status <- function(response) {
  if (status_code(response) < 300) {
    return()
  }

  heads <- headers(response)
  res <- content(response)
  status <- status_code(response)

  msg <- c(
    paste0("Cloud error (", status, "): ", http_status(status)$reason),
    paste0("Message: ", res$invalid_values %||% res$message)
  )

  call <- sys.call(-1)

  cond <- structure(list(
    message = paste0(msg, collapse = "\n")
  ),
  class = c(
    "cloud_error",
    paste0("http_error_", status),
    "error",
    "condition"
  ))

  stop(cond)
}

cloud_check_result <- function(check_log, description, dependency_error) {
  check_dir <- dirname(check_log)

  if (!file.exists(check_log)) {
    return(structure(
      list(
        stdout = character(),
        timeout = FALSE,
        status = -1L,

        rversion = NA_character_,
        platform = NA_character_,
        errors = NA_character_,
        warnings = NA_character_,
        notes = NA_character_,

        description = description$str(normalize = FALSE),
        # DESCRIPTION can exist but be empty, e.g. for a Bioconductor package
        # or a when package's minimum R version isn't met
        # at the VERY LEAST, let's get a package name
        package     = description$get_field("Package", sub("[.]Rcheck$", "", basename(check_dir))),
        version     = description$get("Version")[[1]],
        cran        = description$get_field("Repository", "") == "CRAN",
        bioc        = description$has_fields("biocViews"),

        checkdir    = check_dir,
        test_fail   = rcmdcheck:::get_test_fail(check_dir),
        install_out = rcmdcheck:::get_install_out(check_dir),

        type = "cloud"
        ),
      class = "rcmdcheck"
      )
    )
  }

  stdout <- brio::read_file(check_log)
  # Fix invalid characters
  stdout <- iconv(stdout, "UTF-8", "UTF-8", sub = "bytes")
  # Strip \r
  stdout <- gsub("\r\n", "\n", stdout, fixed = TRUE)

  entries <- strsplit(paste0("\n", stdout), "\n\\*+[ ]")[[1]][-1]

  notdone <- function(x) grep("^DONE", x, invert = TRUE, value = TRUE)

  res <- structure(
    list(
      stdout      = stdout,
      timeout     = FALSE,
      status = if (isTRUE(dependency_error)) -1L else 0L,

      rversion    = rcmdcheck:::parse_rversion(entries),
      platform    = rcmdcheck:::parse_platform(entries),
      errors      = notdone(grep("ERROR\n",   entries, value = TRUE)),
      warnings    = notdone(grep("WARNING\n", entries, value = TRUE)),
      notes       = notdone(grep("NOTE\n",    entries, value = TRUE)),

      description = description$str(normalize = FALSE),
      package     = description$get("Package"),
      version     = description$get("Version")[[1]],
      cran        = description$get_field("Repository", "") == "CRAN",
      bioc        = description$has_fields("biocViews"),

      checkdir    = check_dir,
      test_fail   = rcmdcheck:::get_test_fail(check_dir),
      install_out = rcmdcheck:::get_install_out(check_dir),

      type = "cloud"
    ),
    class = "rcmdcheck"
  )

  res
}

cloud_compare <- function(pkg) {
  desc_path <- file.path(pkg, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    return(rcmdcheck_error(basename(pkg), old = NULL, new = NULL))
  }
  description <- desc::desc(file = desc_path)

  old <- file.path(pkg, "old", paste0(basename(pkg), ".Rcheck"), "00check.log")
  new <- file.path(pkg, "new", paste0(basename(pkg), ".Rcheck"), "00check.log")

  dependency_path <- file.path(pkg, "dependency_install.log")
  dependency_error <- any(grep("ERROR: .*is not available for package", readLines(dependency_path, warn = FALSE))) || !(file.exists(old) && file.exists(new))
  old <- cloud_check_result(old, description, dependency_error)
  new <- cloud_check_result(new, description, dependency_error)
  if (isTRUE(dependency_error)) {
    # DESCRIPTION can exist but be empty, e.g. for a Bioconductor package
    # or a when package's minimum R version isn't met
    # at the VERY LEAST, let's get a package name
    res <- rcmdcheck_error(description$get_field("Package", basename(pkg)), old, new)
    res$version <- description$get("Version")[[1]]
    return(res)
  }
  rcmdcheck::compare_checks(old, new)
}

#' Display revdep results
#'
#' Displays nicely formatted results of processed packages run in the cloud.
#' @inheritParams cloud_report
#' @family cloud
#' @export
cloud_summary <- function(job_name = cloud_job(pkg = pkg), pkg = ".") {
  results <- cloud_results(job_name = job_name, pkg = pkg)
  structure(
    results,
    class = "revdepcheck_results"
  )
}

#' Display detailed revdep results from a cloud run
#'
#' @param revdep Name of the revdep package
#' @inheritParams cloud_report
#' @family cloud
#' @export
cloud_details <- function(job_name = cloud_job(pkg = pkg), revdep, pkg = ".") {
  pkg <- pkg_check(pkg)
  cloud <- dir_find(pkg, "cloud")

  res <- cloud_compare(file.path(cloud, job_name, revdep))

  class(res) <- "revdepcheck_details"
  res
}

#' Markdown report of reverse dependency check results from the cloud
#'
#' You can use these functions to get intermediate reports of a running cloud check.
#' @inheritParams revdep_report_summary
#' @param results Results from [cloud_results()]. Expert use only.
#' @param job_name The job name, as returned by [cloud_check()].
#' @param failures Save failures to disk?
#' @inheritParams revdep_report
#' @family cloud
#' @export
cloud_report <- function(job_name = cloud_job(pkg = pkg), pkg = ".", file = "", all = FALSE, results = NULL, failures = TRUE) {
  pkg <- pkg_check(pkg)
  root <- dir_find(pkg, "root")

  if (is.null(results)) {
    results <- cloud_results(job_name, pkg)
  }

  cli_alert_info("Generating reports")

  cli_alert_info("Writing summary to {.file revdep/README.md}")
  cloud_report_summary(file = file.path(root, "README.md"), all = all, results = results, pkg = pkg)

  cli_alert_info("Writing problems to {.file revdep/problems.md}")
  cloud_report_problems(file = file.path(root, "problems.md"), all = all, results = results, pkg = pkg)

  if (failures) {
    cli_alert_info("Writing failures to {.file revdep/failures.md}")
    cloud_report_failures(file = file.path(root, "failures.md"), results = results, pkg = pkg)
  } else {
    unlink(file.path(root, "failures.md"))
  }

  cli_alert_info("Writing CRAN comments to {.file revdep/cran.md}")
  revdep_report_cran(file = file.path(root, "cran.md"), results = results, pkg = pkg)

  invisible()
}

#' @rdname cloud_report
#' @export
cloud_report_summary <- function(job_name = cloud_job(pkg = pkg), file = "", all = FALSE, pkg = ".", results = NULL) {
  if (is.null(results)) {
    results <- cloud_results(job_name, pkg)
  }

  if (is_string(file) && !identical(file, "")) {
    file <- file(file, encoding = "UTF-8", open = "w")
    on.exit(close(file), add = TRUE)

    opts <- options("crayon.enabled" = FALSE)
    on.exit(options(opts), add = TRUE)
  }

  cat_header("Revdeps", file = file)
  revdeps <- report_revdeps(pkg = pkg, all = all, results = results)

  status <- revdeps$status
  n_issues <- revdeps$issues
  revdeps$status <- revdeps$issues <- NULL
  failed <- !(status %in% c("+", "-"))
  broken <- status == "-"
  if (!all) {
    broken <- broken & n_issues > 0
  }

  revdep_report_section("Failed to check", revdeps[failed, ], file = file)
  revdep_report_section("New problems", revdeps[broken, ], file = file)
  if (all) revdep_report_section("All", revdeps, file = file)

  invisible()
}

#' @rdname cloud_report
#' @export
cloud_report_problems <- function(job_name = cloud_job(pkg = pkg), pkg = ".", file = "", all = FALSE, results = NULL) {
  if (is.null(results)) {
    results <- cloud_results(job_name, pkg)
  }
  revdep_report_problems(pkg = pkg, file = file, all = all, results = results)
}

#' @rdname cloud_report
#' @export
cloud_report_failures <- function(job_name = cloud_job(pkg = pkg), pkg = ".", file = "", results = NULL) {
  if (is.null(results)) {
    results <- cloud_results(job_name, pkg)
  }
  revdep_report_failures(pkg = pkg, file = file, results = results)
}

#' @rdname cloud_report
#' @export
cloud_report_cran <- function(job_name = cloud_job(pkg = pkg), pkg = ".", results = NULL) {
  if (is.null(results)) {
    results <- cloud_results(job_name, pkg)
  }
  revdep_report_cran(pkg = pkg, results = results)
}

#' Retrieve cloud results
#'
#' Intended for expert use only, this can be used as input to the [cloud_report()] and other functions.
#' @inheritParams cloud_report
#' @family cloud
#' @keywords internal
#' @export
cloud_results <- function(job_name = cloud_job(pkg = pkg), pkg = ".") {
  pkg <- pkg_check(pkg)
  cloud <- dir_find(pkg, "cloud")

  cloud_fetch_results(job_name, pkg = pkg)

  cli_alert_info("Comparing results")
  pkgs <- list.dirs(file.path(cloud, job_name), full.names = TRUE, recursive = FALSE)

  pb <- cli_progress_bar(format = "Processing package results: {pb_percent} ({basename(pkg)})", total = length(pkgs))
  out <- lapply(pkgs, function(pkg) {
    cli_progress_update(id = pb)
    cloud_compare(pkg)
  })
  cli_progress_done(id = pb)
  out
}

#' @inheritParams cloud_report
#' @inherit revdep_email
#' @export
cloud_email <- function(type = c("broken", "failed"), job_name = cloud_job(pkg = pkg), pkg = ".", packages = NULL, draft = FALSE) {
  type <- match.arg(type)

  package_results <- cloud_results(job_name, pkg)

  if (!is.null(packages)) {
    to_keep <- map_lgl(package_results, function(x) x$package %in% packages)
    package_results <- package_results[to_keep]
  }

  status <- map_chr(package_results, rcmdcheck_status)

  cond <- switch(type,
    broken = status %in% c("-", "t-", "i-"),
    failed = status %in% c("i+", "t+")
  )
  revdep_email_by_type(pkg, package_results[cond], type, draft = draft)

  invisible()
}

#' Return the current cloud job
#'
#' The `job_name` is automatically set by [cloud_check()] and is remembered for
#' the duration of the current R session. If there is no active `job_name`, but
#' there are local cloud check results, `job_name` is inferred from the most
#' recently modified cloud check results.
#'
#' @param job_name If not `NULL`, sets the active `job_name` to the input.
#' @inheritParams cloud_report
#' @export
cloud_job <- function(job_name = NULL, pkg = ".") {
  cloud_data$job_name <- job_name %||% cloud_data$job_name
  if (!is.null(cloud_data$job_name)) {
    return(invisible(cloud_data$job_name))
  }

  pkg <- pkg_check(pkg)
  cloud <- dir_find(pkg, "cloud")
  if (dir.exists(cloud)) {
    cloud_dirs <- list.dirs(cloud, recursive = FALSE)
  } else {
    cloud_dirs <- character()
  }
  if (length(cloud_dirs) < 1) {
    stop("Can't find any previous `revdepcheck::cloud_check()` results locally, can't discover `job_name`", call. = FALSE)
  }

  latest <- cloud_dirs[which.max(file.info(cloud_dirs)$mtime)]
  cloud_data$job_name <- basename(latest)
  cli_alert_success("Most recent cloud job {.arg job_name}: {.val {cloud_data$job_name}}")
  invisible(cloud_data$job_name)
}

cloud_data <- new.env(parent = emptyenv())

list_job_to_tbl <- function(x, status) {
  if (length(x$jobSummaryList) == 0) {
    return(
      data.frame(
        name = character(),
        index = integer(),
        created = .POSIXct(double()),
        started = .POSIXct(double()),
        stopped = .POSIXct(double()),
        status = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  data.frame(
    name = x$jobSummaryList$jobId,
    index = x$jobSummaryList$arrayProperties$index,
    created = .POSIXct(x$jobSummaryList$createdAt / 1000),
    started = .POSIXct(x$jobSummaryList$startedAt / 1000),
    stopped = .POSIXct(x$jobSummaryList$stoppedAt / 1000),
    status = status,
    stringsAsFactors = FALSE
  )
}

#' Plot the running time per package of a cloud job
#'
#' @inheritParams cloud_report
#' @family cloud
#' @export
cloud_plot <- function(job_name = cloud_job()) {
  job_info <- cloud_job_info(job_name)

  packages <- data.frame(
    index = seq_along(job_info$revdep_packages) - 1,
    package = unlist(job_info$revdep_packages),
    stringsAsFactors = FALSE
  )

  succeeded <- list_job_to_tbl(cloud_job_status(job_name, "SUCCEEDED"), "succeeded")

  failed <- list_job_to_tbl(cloud_job_status(job_name, "FAILED"), "failed")

  data <- rbind(succeeded, failed)

  data <- merge(data, packages)

  data$package <- forcats::fct_reorder(data$package, data$stopped, .desc = TRUE)

  ggplot2::ggplot(data) +
    ggplot2::geom_segment(
      ggplot2::aes(
        y = package,
        yend = ggplot2::after_stat(y),
        x = hms::as_hms(started - created),
        xend = hms::as_hms(stopped - created),
        color = status
      )
      ) +
    ggplot2::scale_color_manual(values = c("succeeded" = "darkgrey", "failed" = "red")) +
    ggplot2::scale_y_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
    ggplot2::guides(color = "none") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}

utils::globalVariables(c("package", "y", "started", "created", "stopped"))

cloud_job_info <- function(job_name = cloud_job()) {

  response <- GET("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    path = paste0("staging/check", "/", job_name),
    encode = "json"
  )

  cloud_stop_for_status(response)
  content(response, simplifyVector = TRUE)
}

cloud_job_status <- function(job_name = cloud_job(pkg = pkg), status = c("ALL", "RUNNING", "SUBMITTED", "PENDENG", "RUNNABLE", "STARTING", "RUNNING", "SUCCEEDED", "FAILED"), pkg = ".") {
  status <- match.arg(status)

  if (status == "ALL") {
    status <- ""
  } else {
    status <- paste0("/", status)
  }

  response <- GET("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    path = paste0("staging/check", "/", job_name, "/", "status", status),
    encode = "json"
  )

  stop_for_status(response)
  content(response)
}

#' Get a tibble of batch sub-job ids for all checked packages
#'
#' @inheritParams cloud_report
#' @export
cloud_job_mapping <- function(job_name = cloud_job()) {
  info <- cloud_job_info(job_name)

  tibble::tibble(package = info$revdep_packages, id = seq_along(info$revdep_packages) - 1)
}

#' Retrieve the names broken or failed packages
#'
#' Broken packages are those whose checks got worse with the dev version.
#' Failed packages are those whose cloud jobs failed, either because the spot
#' instance was shut down by AWS or because the checks used too much memory and
#' were killed.
#' @inheritParams cloud_report
#' @param install_failures Whether to include packages that failed to install.
#' @param timeout_failures Whether to include packages that timed out.
#' @family cloud
#' @returns A character vector with the names of broken packages, to be passed to `cloud_check()`.
#' @export
cloud_broken <- function(job_name = cloud_job(pkg = pkg), pkg = ".", install_failures = FALSE, timeout_failures = FALSE) {
  results <- cloud_results(job_name = job_name, pkg = pkg)
  broken <- map_lgl(results, is_broken, install_failures, timeout_failures)

  map_chr(results[broken], `[[`, "package")
}

#' @rdname cloud_broken
#' @export
cloud_failed <- function(job_name = cloud_job(pkg = pkg), pkg = ".") {
  unlist(cloud_job_status(job_name, status = "FAILED")$packages)
}

#' Browse to the AWS url for the job
#'
#' This is useful for closer inspection of individual jobs while they are
#' running or after the fact.
#' @param package If `NULL` browses to the URL of the overall job. If a package
#'   name, browses to the URL for that specific package job.
#' @inheritParams cloud_report
#' @export
cloud_browse <- function(job_name = cloud_job(), package = NULL) {

  info <- cloud_job_info(job_name)

  job_id <- info$batch_job_id

  if (is.null(package)) {
    utils::browseURL(sprintf("https://console.aws.amazon.com/batch/home?region=us-east-1#jobs/array-job/%s", job_id))
    return(invisible())
  }

  mapping <- cloud_job_mapping(job_name)

  array_num <- mapping$id[mapping$package == package]

  utils::browseURL(sprintf("https://console.aws.amazon.com/batch/home?region=us-east-1#jobs/detail/%s:%i", job_id, array_num))
}
