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
cloud_status <- function(job_id = cloud_job(), update_interval = 10) {
  status_id <- cli_status("Status of {.val {job_id}}")

  cloud_status_check <- function(job_id) {

    info <- cloud_job_describe(job_id)

    status <- info$jobs$status

    if (length(status) == 0) {
      stop("No job with Id: '", job_id, call. = FALSE)
    }

    size <- info$jobs$arrayProperties$size
    results <- info$jobs$arrayProperties$statusSummary
    if (!is.data.frame(results)) {
      return(NA)
    }
    colnames(results) <- tolower(colnames(results))
    results <- results[c("pending", "runnable", "starting", "running", "succeeded", "failed")]

    num_completed <- sum(results[c("succeeded", "failed")])
    num_queued <- sum(results[c("pending", "runnable")])
    num_running <- sum(results[c("starting", "running")])

    created_time <- .POSIXct(info$jobs$createdAt / 1000, tz = "UTC")
    current_time <- Sys.time()

    elapsed <- hms::as_hms(as.integer(difftime(current_time, created_time, units = "secs")))

    eta <- calc_eta(created_time, current_time, num_running, num_completed, size)

    status_bar_text <- "[{num_queued}/{col_blue(num_running)}/{col_green(results$succeeded)}/{col_red(results$failed)} - {.strong {size}}] {elapsed} | ETA: {eta}"

    switch(status,
      FAILED = {
        cli_status_clear(id = status_id, result = "failed", msg_failed = paste0("{.emph FAILED}: ", status_bar_text))
        cli_alert("run {.fun cloud_summary} for interactive results")
        cli_alert("run {.fun cloud_report} for markdown reports")
        return(FALSE)
      },
      SUCCEEDED = {
        cli_status_clear(id = status_id, result = "done", msg_done = paste0("{.emph SUCCEEDED}: ", status_bar_text))
        cli_alert("run {.fun cloud_summary} for interactive results")
        cli_alert("run {.fun cloud_report} for markdown reports")
        return(TRUE)
      },
      {
        cli::cli_status_update(id = status_id, status_bar_text)
        return(NA)
      }
    )
  }

  while(is.na(res <- cloud_status_check(job_id))) {
    Sys.sleep(update_interval)
  }

  return(invisible(res))
}

calc_eta <- function(creation_time, current_time, running, completed, total) {
  if (completed >= total) {
    return("Done")
  }

  infinity <- "\U221E"

  if (running == 0) {
    return(infinity)
  }

  time_diff <- as.integer(difftime(current_time, creation_time, units = "secs"))
  to_go <- total - completed
  secs_to_go <- time_diff / completed * to_go
  if (secs_to_go == Inf) {
    return(infinity)
  }

  prettyunits::pretty_sec(secs_to_go)
}

#' Fetch results from the cloud
#'
#' Intended mainly for internal and expert use. This function when needed by
#' [cloud_report()] and `[cloud_summary()]`, so it is unlikely you will need to
#' call it explicitly.
#'
#' @keywords internal
#' @family cloud
#' @inheritParams cloud_report
#' @export
cloud_fetch_results <- function(job_id = cloud_job(), pkg = ".") {
  pkg <- pkg_check(pkg)
  root <- dir_find(pkg, "root")

  info <- cloud_job_describe(job_id)

  job_envir <- info$jobs$container$environment[[1]]
  aws_url <- job_envir$value[job_envir$name == "OUTPUT_S3_PATH"] %||% sprintf("s3://rstudio-revdepcheck-cloud-staging/%s/results", job_id)

  out_dir <- file.path(root, job_id)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  cli_alert_info("Downloading from AWS")
  processx::run("aws", c("s3", "sync", aws_url, out_dir))

  cli_alert_info("Extracting results")
  for (file in list.files(out_dir, full.names = TRUE, pattern = ".*[.]tar[.]gz")) {
    pkg <- sub("[.]tar[.]gz", "", basename(file))
    if (!dir.exists(file.path(out_dir, pkg))) {
      utils::untar(file, exdir = out_dir)
    }
  }
}

#' Submit a reverse dependency checking job to the cloud
#'
#' @param tarball A pre-built package tarball, if `NULL` a tarball will be
#'   automatically built for the package at `pkg` by [pkgbuild::build()].
#' @param revdep_packages A character vector of packages to check, if `NULL`
#'   equal to [cran_revdeps()]
#' @returns The AWS Batch job-id
#' @inheritParams revdep_check
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger
#' @importFrom httr GET PATCH POST stop_for_status add_headers content
#' @family cloud
#' @export
cloud_check <- function(pkg = ".", tarball = NULL, revdep_packages = NULL) {
  if (is.null(tarball)) {
    pkg <- pkg_check(pkg)
    tarball <- pkgbuild::build(path = pkg)
  }

  package_name <- desc::desc_get_field("Package", file = tarball)
  package_version <- as.character(desc::desc_get_version(file = tarball))

  # Lookup revdeps with R, as the RSPM db seems not quite right, for instance
  # it seems to include archived packages.
  if (is.null(revdep_packages)) {
    revdep_packages <- setdiff(cran_revdeps(package_name), package_name)
  }

  post_response <- POST("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com/staging/check",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    body = list(
      package_name = package_name,
      package_version = package_version,
      revdep_packages = revdep_packages
    ),
    encode = "json"
  )

  cloud_stop_for_status(post_response)

  post_content <- content(post_response)
  presigned_url <- post_content[["_source_presigned_url"]]
  job_name <- post_content[["id"]]

  cli_alert_success("Creating cloud job {.arg job_name}: {.val {job_name}}")

  cli_alert_info("Uploading {.file {tarball}}")

  curl::curl_upload(tarball, presigned_url, verbose = FALSE)

  cli_alert_success("Uploaded {.file {tarball}}")

  cli_alert_info("Spawning batch job for cloud job {.arg job_name}: {.val {job_name}}")

  patch_response <- PATCH("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    path = paste0("staging/check", "/", job_name),
    body = list(status = "running"),
    encode = "json"
  )

  cloud_stop_for_status(patch_response)

  patch_content <- content(patch_response)

  job_id <- patch_content$batch_job_id

  cli_alert_success("Created batch job {.arg job_id}: {.val {job_id}}")

  cli_alert("Run {.fun cloud_status} to monitor job status")

  cloud_data$job_id <- job_id

  invisible(job_id)
}

#' Cancel a running cloud run
#'
#' @inheritParams cloud_report
#' @family cloud
#' @export
cloud_cancel <- function(job_id = cloud_job()) {
  info <- cloud_job_describe(job_id)

  job_name <- info$jobs$jobName

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

  stdout <- readChar(check_log, nchars = file.size(check_log))

  # Make sure we don't have \r on windows
  stdout <- rcmdcheck:::win2unix(stdout)

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
      install_out = rcmdcheck:::get_install_out(check_dir)
    ),
    class = "rcmdcheck"
  )

  res
}

cloud_compare <- function(old, new) {
  desc_path <- file.path(dirname(dirname(dirname(old))), "DESCRIPTION")
  description <- desc::desc(file = file.path(dirname(dirname(dirname(old))), "DESCRIPTION"))
  dependency_path <- file.path(dirname(dirname(dirname(old))), "dependency_install.log")
  dependency_error <- any(grep("ERROR: .*is not available for package", readLines(dependency_path)))
  old <- cloud_check_result(old, description, dependency_error)
  new <- cloud_check_result(new, description, dependency_error)
  if (isTRUE(dependency_error)) {
    res <- rcmdcheck_error(description$get("Package"), old, new)
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
cloud_summary <- function(job_id = cloud_job(), pkg = ".") {
  results <- cloud_results(job_id = job_id, pkg = pkg)
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
cloud_details <- function(job_id = cloud_job(), revdep, pkg = ".") {
  pkg <- pkg_check(pkg)
  root <- dir_find(pkg, "root")

  old <- file.path(root, job_id, revdep, "old", paste0(revdep, ".Rcheck"), "00check.log")
  new <- file.path(root, job_id, revdep, "new", paste0(revdep, ".Rcheck"), "00check.log")

  res <- cloud_compare(old, new)
  class(res) <- "revdepcheck_details"
  res
}

#' Markdown report of reverse dependency check results from the cloud
#'
#' You can use these functions to get intermediate reports of a running cloud check.
#' @inheritParams revdep_report_summary
#' @param results Results from [cloud_results()]. Expert use only.
#' @param job_id The batch job_id, as returned by [cloud_check()].
#' @inheritParams revdep_report
#' @family cloud
#' @export
cloud_report <- function(job_id = cloud_job(), pkg = ".", file = "", all = FALSE, results = NULL) {
  pkg <- pkg_check(pkg)
  root <- dir_find(pkg, "root")

  if (is.null(results)) {
    results <- cloud_results(job_id, pkg)
  }

  message("Writing summary to 'revdep/README.md'")
  cloud_report_summary(file = file.path(root, "README.md"), all = all, results = results, pkg = pkg)

  message("Writing problems to 'revdep/problems.md'")
  cloud_report_problems(file = file.path(root, "problems.md"), all = all, results = results, pkg = pkg)

  message("Writing failures to 'revdep/failures.md'")
  cloud_report_failures(file = file.path(root, "failures.md"), results = results, pkg = pkg)

  invisible()
}

#' @rdname cloud_report
#' @export
cloud_report_summary <- function(job_id = cloud_job(), file = "", all = FALSE, pkg = ".", results = NULL) {
  if (is.null(results)) {
    results <- cloud_results(job_id, pkg)
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
cloud_report_problems <- function(job_id = cloud_job(), pkg = ".", file = "", all = FALSE, results = NULL) {
  if (is.null(results)) {
    results <- cloud_results(job_id, pkg)
  }
  revdep_report_problems(pkg = pkg, file = file, all = all, results = results)
}

#' @rdname cloud_report
#' @export
cloud_report_failures <- function(job_id = cloud_job(), pkg = ".", file = "", results = NULL) {
  if (is.null(results)) {
    results <- cloud_results(job_id, pkg)
  }
  revdep_report_failures(pkg = pkg, file = file, results = results)
}

#' Retrieve cloud results
#'
#' Intended for expert use only, this can be used as input to the [cloud_report()] and other functions.
#' @inheritParams cloud_report
#' @family cloud
#' @keywords internal
#' @export
cloud_results <- function(job_id = cloud_job(), pkg = ".") {
  pkg <- pkg_check(pkg)
  root <- dir_find(pkg, "root")

  cloud_fetch_results(job_id, pkg = pkg)

  check_files <- list.files(file.path(root, job_id), recursive = TRUE, full.names = TRUE, pattern = "00check.log$")
  old_checks <- grep("/old/", check_files, value = TRUE, fixed = TRUE)
  new_checks <- grep("/new/", check_files, value = TRUE, fixed = TRUE)
  stopifnot(length(old_checks) == length(new_checks))
  lapply(seq_along(old_checks), function(i) {
    cloud_compare(old_checks[[i]], new_checks[[i]])
  })
}

#' @inheritParams cloud_report
#' @inherit revdep_email
#' @export
cloud_email <- function(type = c("broken", "failed"), job_id = cloud_job(), pkg = ".", packages = NULL, draft = FALSE) {
  type <- match.arg(type)

  package_results <- cloud_results(job_id, pkg)

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
#' This is automatically set by [cloud_check()] and only lasts for the current R session.
#' @export
cloud_job <- function() {
  if (is.null(cloud_data$job_id)) {
    stop("No current job, please specify the `job_id` explicitly, or run a job with `cloud_check()`", call. = FALSE)
  }
  cloud_data$job_id
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
cloud_plot <- function(job_id = cloud_job()) {
  job_info <- cloud_job_info(job_id)

  packages <- data.frame(
    index = seq_along(job_info$revdep_packages) - 1,
    package = unlist(job_info$revdep_packages),
    stringsAsFactors = FALSE
  )

  succeeded <- list_job_to_tbl(cloud_job_list(job_id, "SUCCEEDED"), "succeeded")

  failed <- list_job_to_tbl(cloud_job_list(job_id, "FAILED"), "failed")

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

cloud_job_info <- function(job_id = cloud_job()) {
  info <- cloud_job_describe(job_id)

  job_name <- info$jobs$jobName

  response <- GET("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    path = paste0("staging/check", "/", job_name),
    encode = "json"
  )

  stop_for_status(response)
  content(response)
}

cloud_job_describe <- function(job_id = cloud_job()) {
  res <- processx::run("aws", c("batch", "describe-jobs", "--jobs", job_id))

  jsonlite::fromJSON(txt = res$stdout)
}

cloud_job_list <- function(job_id = cloud_job(), status = c("RUNNING", "SUBMITTED", "PENDENG", "RUNNABLE", "STARTING", "RUNNING", "SUCCEEDED", "FAILED")) {
  status <- match.arg(status)

  res <- processx::run("aws", c("batch", "list-jobs", "--array-job-id", job_id, "--job-status", status))
  jsonlite::fromJSON(txt = res$stdout)
}

#' Retrieve the names broken packages
#'
#' @inheritParams cloud_report
#' @param install_failures Whether to include packages that failed to install.
#' @param install_failures Whether to include packages that timed out.
#' @family cloud
#' @returns A character vector with the names of broken packages, to be passed to `cloud_check()`.
#' @export
cloud_broken <- function(job_id = cloud_job(), pkg = ".", install_failures = FALSE, timeout_failures = FALSE) {
  results <- cloud_results(job_id = job_id, pkg = pkg)
  broken <- map_lgl(results, is_broken, install_failures, timeout_failures)

  map_chr(results[broken], `[[`, "package")
}
