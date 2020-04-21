calc_eta <- function(creation_time, current_time, running, completed, total) {
  if (completed < total) {
    if (running == 0) {
      eta <- "\U221E"
    } else {
      time_diff <- as.integer(difftime(current_time, creation_time, units = "secs"))
      to_go <- total - completed
      secs_to_go <- time_diff / completed * to_go
      if (secs_to_go == Inf) {
        eta <- "\U221E"
      } else {
        eta <- prettyunits::pretty_sec(secs_to_go)
      }
    }
  }
  eta
}

drop_attributes <- function(x) {
  mostattributes(x) <-  NULL
  x
}

#' @importFrom cli cli_format cli_status_update col_green col_blue col_red style_bold cli_status_clear cli_status
#' @export
cloud_status <- function(job_id) {
  status_id <- cli_status("Status of {.val {job_id}}")

  cloud_status_check <- function(job_id) {
    res <- processx::run("aws", c("batch", "describe-jobs", "--jobs", job_id))

    data <- jsonlite::fromJSON(txt = res$stdout)

    status <- data$jobs$status

    if (length(status) == 0) {
      stop("No job with Id: '", job_id, call. = FALSE)
    }

    switch(status,
      FAILED = {
        cli_status_clear(id = status_id, result = "failed", msg_failed = "{.emph FAILED}: run {.code cloud_results(\"{job_id}\")} for results")
        return(TRUE)
      },
      SUCCEEDED = {
        cli_status_clear(id = status_id, result = "done", msg_done = "{.emph SUCCEEDED}: run {.code cloud_results(\"{job_id}\")} for results")
        return(TRUE)
      }
    )

    size <- data$jobs$arrayProperties$size
    results <- data$jobs$arrayProperties$statusSummary
    colnames(results) <- tolower(colnames(results))
    results <- results[c("pending", "runnable", "starting", "running", "succeeded", "failed")]

    num_completed <- sum(results[c("succeeded", "failed")])
    num_queued <- sum(results[c("pending", "runnable")])
    num_running <- sum(results[c("starting", "running")])

    created_time <- .POSIXct(data$jobs$createdAt / 1000, tz = "UTC")
    current_time <- Sys.time()

    elapsed <- hms::as_hms(as.integer(difftime(current_time, created_time, units = "secs")))

    eta <- calc_eta(created_time, current_time, num_running, num_completed, size)

    cli::cli_status_update(id = status_id, "[{num_queued}/{col_blue(num_running)}/{col_green(results$succeeded)}/{col_red(results$failed)} - {.strong {size}}] {elapsed} | ETA: {eta}")
    return(FALSE)
  }

  while(cloud_status_check(job_id) != TRUE) {
    Sys.sleep(10)
  }
  return(invisible())
}

#' @export
cloud_fetch_results <- function(job_id, path = ".") {
  res <- processx::run("aws", c("batch", "describe-jobs", "--jobs", job_id))

  data <- jsonlite::fromJSON(txt = res$stdout)

  job_envir <- data$jobs$container$environment[[1]]
  aws_url <- job_envir$value[job_envir$name == "OUTPUT_S3_PATH"]

  out_dir <- file.path(path, job_id)

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

#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger
#' @importFrom httr GET PATCH POST stop_for_status add_headers content
#' @export
cloud_submit <- function(path = ".", tarball = NULL, revdep_packages = NULL) {
  if (is.null(tarball)) {
    tarball <- pkgbuild::build(path = path)
  }

  package_name <- desc::desc_get_field("Package", file = tarball)
  package_version <- as.character(desc::desc_get_version(file = tarball))

  # Lookup revdeps with R, as the RSPM db seems not quite right, for instance
  # it seems to include archived packages.
  if (is.null(revdep_packages)) {
    revdep_packages <- cran_revdeps(package_name, dependencies = TRUE)
  }

  post_response <- POST("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com/staging/check",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    body = list(package_name = package_name, package_version = package_version, revdep_packages = revdep_packages),
    encode = "json"
  )

  stop_for_status(post_response)

  post_content <- content(post_response)
  presigned_url <- post_content[["_source_presigned_url"]]
  job_name <- post_content[["id"]]

  cli_alert_success("Created job {.val {job_name}}")

  #print(post_content)

  cli_alert_info("Uploading {.file {tarball}}")

  curl::curl_upload(tarball, presigned_url, verbose = FALSE)

  cli_alert_success("Successfully uploaded {.file {tarball}}")

  cli_alert_info("Starting batch job {.val {job_name}}")

  patch_response <- PATCH("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    path = paste0("staging/check", "/", job_name),
    body = list(status = "running"),
    encode = "json"
  )

  stop_for_status(patch_response)

  patch_content <- content(patch_response)

  job_id <- patch_content$batch_job_id

  cli_alert_info("Use {.code cloud_status(\"{job_id}\")} to monitor job status")

  invisible(job_id)
}

#' @export
cloud_cancel <- function(job_id) {
  patch_response <- PATCH("https://xgyefaepu5.execute-api.us-east-1.amazonaws.com",
    config = add_headers("x-api-key" = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY")),
    path = paste0("staging/check", "/", job_id),
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

cloud_details <- function(job_id, revdep) {
  old <- file.path(job_id, revdep, "old", paste0(revdep, ".Rcheck"), "00check.log")
  new <- file.path(job_id, revdep, "new", paste0(revdep, ".Rcheck"), "00check.log")

  res <- cloud_compare(old, new)
  class(res) <- "revdepcheck_details"
  res
}

cloud_results <- function(job_id) {
  cloud_fetch_results(job_id)
  check_files <- list.files(job_id, recursive = TRUE, full.names = TRUE, pattern = "00check.log$")
  old_checks <- grep("/old/", check_files, value = TRUE, fixed = TRUE)
  new_checks <- grep("/new/", check_files, value = TRUE, fixed = TRUE)
  stopifnot(length(old_checks) == length(new_checks))
  lapply(seq_along(old_checks), function(i) {
    cloud_compare(old_checks[[i]], new_checks[[i]])
  })
}

cloud_report_summary <- function(results, file = "", all = FALSE, pkg = ".") {

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

cloud_report_problems <- function(results, file = "", all = FALSE, pkg = ".") {
  revdep_report_problems(file = file, results = results, all = all, pkg = pkg)
}

cloud_report_failures <- function(results, file = "", pkg = ".") {
  revdep_report_failures(file = file, results = results, pkg = pkg)
}


cloud_report <- function(job_id, all = FALSE, pkg = ".", results = cloud_results(job_id)) {
  requireNamespace("revdepcheck")
  pkg <- pkg_check(pkg)
  root <- dir_find(pkg, "root")

  force(results)

  message("Writing summary to 'revdep/README.md'")
  cloud_report_summary(file = file.path(root, "README.md"), all = all, results = results, pkg = pkg)

  message("Writing problems to 'revdep/problems.md'")
  cloud_report_problems(file = file.path(root, "problems.md"), all = all, results = results, pkg = pkg)

  message("Writing failures to 'revdep/failures.md'")
  cloud_report_failures(file = file.path(root, "failures.md"), results = results, pkg = pkg)

  invisible()
}
