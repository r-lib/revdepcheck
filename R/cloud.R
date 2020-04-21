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

    info <- cloud_job_info(job_id)

    status <- info$jobs$status

    if (length(status) == 0) {
      stop("No job with Id: '", job_id, call. = FALSE)
    }

    switch(status,
      FAILED = {
        cli_status_clear(id = status_id, result = "failed", msg_failed = "{.emph FAILED}")
        cli_alert("run {.fun cloud_report} for results")
        return(FALSE)
      },
      SUCCEEDED = {
        cli_status_clear(id = status_id, result = "done", msg_done = "{.emph SUCCEEDED}")
        cli_alert("run {.fun cloud_report} for results")
        return(TRUE)
      }
    )

    size <- info$jobs$arrayProperties$size
    results <- info$jobs$arrayProperties$statusSummary
    colnames(results) <- tolower(colnames(results))
    results <- results[c("pending", "runnable", "starting", "running", "succeeded", "failed")]

    num_completed <- sum(results[c("succeeded", "failed")])
    num_queued <- sum(results[c("pending", "runnable")])
    num_running <- sum(results[c("starting", "running")])

    created_time <- .POSIXct(info$jobs$createdAt / 1000, tz = "UTC")
    current_time <- Sys.time()

    elapsed <- hms::as_hms(as.integer(difftime(current_time, created_time, units = "secs")))

    eta <- calc_eta(created_time, current_time, num_running, num_completed, size)

    cli::cli_status_update(
      id = status_id,
      "[{num_queued}/{col_blue(num_running)}/{col_green(results$succeeded)}/{col_red(results$failed)} - {.strong {size}}] {elapsed} | ETA: {eta}"
    )

    return(NA)
  }

  while(is.na(res <- cloud_status_check(job_id))) {
    Sys.sleep(update_interval)
  }

  return(invisible(res))
}

calc_eta <- function(creation_time, current_time, running, completed, total) {
  if (completed >= total) {
    return(0)
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

cloud_job_info <- function(job_id = cloud_job()) {
  res <- processx::run("aws", c("batch", "describe-jobs", "--jobs", job_id))

  jsonlite::fromJSON(txt = res$stdout)
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

  info <- cloud_job_info(job_id)

  job_envir <- info$jobs$container$environment[[1]]
  aws_url <- job_envir$value[job_envir$name == "OUTPUT_S3_PATH"]

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
    revdep_packages <- cran_revdeps(package_name)
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

  stop_for_status(post_response)

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

  stop_for_status(patch_response)

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
  info <- cloud_job_info(job_id)

  job_name <- info$jobs$jobName

  patch_response <- PATCH("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    path = paste0("staging/check", "/", job_name),
    body = list(status = "cancelled"),
    encode = "json"
  )

  stop_for_status(patch_response)
}

cloud_check_result <- function(check_log) {
  check_dir <- dirname(check_log)

  stdout <- readChar(check_log, nchars = file.size(check_log))

  # Make sure we don't have \r on windows
  stdout <- rcmdcheck:::win2unix(stdout)

  entries <- strsplit(paste0("\n", stdout), "\n\\*+[ ]")[[1]][-1]

  notdone <- function(x) grep("^DONE", x, invert = TRUE, value = TRUE)

  get_package_name <- function(x) {
    sub("^this is package .([[:alnum:].]+).*", "\\1", grep("^this is package ", x, value = TRUE))
  }

  package_name <- get_package_name(entries)

  description <- get_description(package_name)

  res <- structure(
    list(
      stdout      = stdout,
      timeout     = FALSE,

      rversion    = rcmdcheck:::parse_rversion(entries),
      platform    = rcmdcheck:::parse_platform(entries),
      errors      = notdone(grep("ERROR\n",   entries, value = TRUE)),
      warnings    = notdone(grep("WARNING\n", entries, value = TRUE)),
      notes       = notdone(grep("NOTE\n",    entries, value = TRUE)),

      description = description$str(normalize = FALSE),
      package     = package_name,
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

get_description <- function(package) {
  res <- try(desc::desc(package = package), silent = TRUE)
  if (!inherits(res, "try-error")) {
    return(res)
  }

  res <- try(get_cran_description(package), silent = TRUE)
  if (!inherits(res, "try-error")) {
    return(res)
  }

  return(desc::desc(text=""))
}

get_cran_description <- memoise::memoise(function(package) {
  res <- httr::GET("https://cloud.r-project.org", path = c("web/packages", package, "DESCRIPTION"))
  httr::stop_for_status(res)

  suppressMessages(desc::desc(text = httr::content(res, as = "text")))
})

cloud_compare <- function(old, new) {
  old <- cloud_check_result(old)
  new <- cloud_check_result(new)
  rcmdcheck::compare_checks(old, new)
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
