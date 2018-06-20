#' List maintainers of all reverse dependencies
#'
#' @export
#' @inheritParams revdep_check
revdep_maintainers <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  m <- unique(db_maintainers(pkg)[[1]])
  structure(m, class = "maintainers")
}

#' @export
print.maintainers <- function(x, ...) {
  cat_line(paste0(x, collapse = ",\n"))
}

#' Notify revdep maintainers about problems
#'
#' This function uses gmail to automatically notify all maintainers of revdeps
#' that have failures with the new version of the package. The form of the
#' email is fixed, but it uses template parameters so that you can control
#' the details: set the variables in `revdeps/email.yaml`. You'll be prompted to
#' review the template before any emails are sent; or you can use
#' `revdep_email_draft()` to see a draft version.
#'
#' To use this function, you'll need to give the gmailr app authority to
#' send emails from gmail. To revoke that authority, delete the `.httr-oauth`
#' file created in your working directory.
#'
#' @inheritParams revdep_check
#' @param type Type of problems to notify about; either "broken" (i.e. there
#'   is a new `R CMD check` failure that did not currently occur) or
#'   "failed" (i.e. the check failure either during installation or because
#'   of a timeout).
#' @param packages A character vector of package names. Use this if some emails
#'   failed to send in the previous round. If omitted uses all packages.
#' @param data Optionally, supply a named list to provide your own parameters
#'   to fill in the template
#' @export

revdep_email <- function(type = c("broken", "failed"), pkg = ".", packages = NULL) {
  type <- match.arg(type)

  packages <- db_results(pkg, packages)
  status <- vapply(packages, rcmdcheck_status, character(1), USE.NAMES = FALSE)

  cond <- switch(type,
    broken = status == "-",
    failed = status %in% c("i", "t")
  )
  revdep_email_by_type(pkg, packages[cond], type)

  invisible()
}

revdep_email_by_type <- function(pkg, packages, type = "broken") {
  if (length(packages) == 0) {
    message("All ok :D")
    return(invisible())
  }

  # Generate email templates
  package_data <- package_data(pkg = pkg, packages = packages)

  # Show draft email (using first package) and check we're good
  revdep_email_draft(pkg = pkg, type = type, data = package_data[[1]])

  ready <- utils::menu(
    title = paste0("Ready to send ", length(package_data), " emails?"),
    c("Yes", "No")
  )

  if (ready != 1L) {
    return(invisible())
  }

  ok <- logical(length(package_data))

  # Construct and send each email
  for (i in seq_along(package_data)) {
    data <- package_data[[i]]

    body <- email_build(type = type, data = data)
    to <- data$your_email
    subject <- glue_data(data, "{your_package} and upcoming CRAN release of {my_package}")

    ok[[i]] <- email_send(to, body, subject, draft = FALSE)
  }

  if (any(!ok)) {
    failed <- package_data[!ok]
    pkgs <- map_chr(failed, function(x) x$your_package)

    message("Failed to send:")
    cat(deparse(pkgs), sep = "\n")
  }

  invisible()
}

#' @export
#' @rdname revdep_email

revdep_email_draft <- function(type = "broken", pkg = ".", data = email_data(pkg)) {
  cat_line(rule("Draft email"))

  data <- lapply(data, bold)
  cat(email_build(type = type, data = data))

  cat_line()
  cat_line()
  cat_line(rule())
  cat_line("Please carefully review this draft")
  cat_line("Missing variables look like ", red("--field_name--"))
  cat_line("Add these to revdep/email.yml")
  cat_line(rule())
}


# Internal --------------------------------------------------------------

package_data <- function(packages, pkg = ".") {
  data_base <- email_data(pkg)
  data_package <- lapply(packages, function(x) {
    cmp <- x$cmp
    old <- unique(cmp$hash[cmp$which == "old"])
    new <- unique(cmp$hash[cmp$which == "new"])
    broke <- setdiff(new, old)

    out <- cmp$output[cmp$hash %in% broke & cmp$which == "new"]
    your_results <- crayon::strip_style(format_details_bullets(out))

    desc <- desc::desc(text = x$new$description)
    maintainer <- utils::as.person(x$maintainer)[[1]]

    list(
      your_package = x$package,
      your_version = desc$get_version(),
      your_results = glue::collapse(your_results),
      your_name = format(maintainer, c("given", "family")),
      your_email = format(maintainer, "email", braces = list(email = ""))
    )
  })
  lapply(data_package, function(x) utils::modifyList(data_base, x))
}

#' @importFrom gmailr mime send_message

email_send <- function(to, body, subject, draft = TRUE) {
  email <- mime(To = to, Subject = subject, body = body)

  send <- if (draft) gmailr::create_draft else gmailr::send_message
  msg <- if (draft) "Drafting" else "Sending"
  tryCatch(
    {
      message(msg, ": ", gmailr::subject(email))
      send(email)
      TRUE
    },
    interrupt = function(e) {
      message("Aborted by user")
      invokeRestart("abort")
    },
    error = function(e) {
      message("Failed")
      FALSE
    }
  )
}


#' @importFrom glue glue_data

email_build <- function(type = "broken", data = email_data(".")) {
  name <- paste0("email-", type, ".txt")
  template_path <- system.file(
    "templates", name,
    package = "revdepcheck",
    mustWork = TRUE
  )

  template <- paste(readLines(template_path), collapse = "\n")
  glue_data(data, template)
}

#' @importFrom whoami fullname
#' @importFrom yaml yaml.load_file as.yaml

email_data <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  defaults <- email_data_defaults(pkg)

  yaml_path <- file.path(pkg, "revdep", "email.yml")
  if (!file.exists(yaml_path)) {
    return(defaults)
  }

  manual <- compact(yaml.load_file(yaml_path))
  utils::modifyList(defaults, manual)
}

email_data_defaults <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  list(
    my_package = pkg_name(pkg),
    my_version = pkg_version(pkg),
    my_name = fullname(red("--my_name--")),
    my_news_url = red("--my_news_url---"),
    my_issues_url = pkg_bug_reports(pkg),

    release_date = red("--release_date--"),
    rel_release_date = red("--rel_release_date---"),
    release_version = red("--release_version--"),

    your_package = green("your_package"),
    your_version = green("your_version"),
    your_results = green("your_results"),
    your_name = green("your_name")
  )
}
