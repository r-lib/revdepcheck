
#' Markdown report of reverse dependency check results
#'
#' You can use these functions to get intermediate reports of a [revdep_check()]
#' running in another session.
#'
#' `revdep_report_summary()` writes the contents of `README.md`, by
#' default to the console. This is handy to quickly inspect the (current)
#' list of problematic packages.
#'
#' @inheritParams revdep_check
#' @param file File to write output to. Default will write to console.
#' @param all Whether to report all problems, including the ones that
#'   were already present in the old version of the package. This potentially
#'   generated a lot of output, most of which was irrelevant, so they are
#'   omitted by default, and only problems seen with the new version of
#'   the package are reported.
#' @param results Cached results from `db_results()`. Expert use only.
#' @export
#' @importFrom crayon black red yellow green
#' @importFrom sessioninfo platform_info

revdep_report_summary <- function(pkg = ".", file = "", all = FALSE, results = NULL) {
  pkg <- pkg_check(pkg)
  if (is_string(file) && !identical(file, "")) {
    file <- file(file, encoding = "UTF-8", open = "w")
    on.exit(close(file), add = TRUE)

    opts <- options("crayon.enabled" = FALSE)
    on.exit(options(opts), add = TRUE)
  }

  cat_header("Platform", file = file)
  cat_kable(report_platform(), file = file)

  cat_header("Dependencies", file = file)
  cat_kable(report_libraries(pkg), file = file)

  cat_header("Revdeps", file = file)
  revdeps <- report_revdeps(pkg, all = all, results = results)

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

revdep_report_section <- function(title, rows, file) {
  if (nrow(rows) == 0) {
    return()
  }

  cat_header(title, " (", nrow(rows), ")", level = 2, file = file)
  cat_kable(rows, file = file)
}

#' `revdep_report_problems()` generates a report about packages with check
#' problems.
#'
#' @export
#' @rdname revdep_report_summary

revdep_report_problems <- function(pkg = ".", file = "", all = FALSE, results = NULL) {
  ## We show the packages that
  ## 1. are newly broken
  ## 2. still broken, if all == TRUE
  problem <- function(x) {
    any(x$cmp$change == 1) || (all && any(x$cmp$change == 0))
  }
  revdep_report_if(pkg = pkg, file = file, predicate = problem, results = results)
}

#' `revdep_report_failures()` generates a report about packages that failed
#' to check (i.e. couldn't install, or timed out).
#'
#' @export
#' @rdname revdep_report_summary

revdep_report_failures <- function(pkg = ".", file = "", results = NULL) {
  problem <- function(x) {
    !x$status %in% c("+", "-")
  }
  revdep_report_if(pkg = pkg, file = file, predicate = problem, results = results)
}

revdep_report_if <- function(pkg = ".", file = "", predicate, results = NULL) {

  if (is_string(file) && !identical(file, "")) {
    file <- file(file, encoding = "UTF-8", open = "w")
    on.exit(close(file), add = TRUE)

    opts <- options("crayon.enabled" = FALSE)
    on.exit(options(opts), add = TRUE)
  }

  results <- results %||% db_results(pkg, NULL)
  show <- map_lgl(results, predicate)

  if (sum(show)) {
    map(results[show], failure_details, file = file)
  } else {
    cat("*Wow, no problems at all. :)*", file = file)
  }

  invisible()
}

failure_details <- function(x, file = "") {
  cat_header(x$package, file = file)
  cat_package_info(x, file = file)
  cat_line(file = file)

  if (x$status == "E") {
    cat_header("Error before installation", level = 2, file = file)
    cat_header("Devel", level = 3, file = file)
    cat_line("```", file = file)
    cat_line(x$new$stdout, sep = "\n", file = file)
    cat_line(x$new$stderr, sep = "\n", file = file)
    cat_line("```", file = file)
    cat_header("CRAN", level = 3, file = file)
    cat_line("```", file = file)
    cat_line(x$old$stdout, sep = "\n", file = file)
    cat_line(x$old$stderr, sep = "\n", file = file)
    cat_line("```", file = file)

    } else {
      rows <- x$cmp
      cat_failure_section("Newly broken", rows[rows$change == +1, ], file = file)
      cat_failure_section("Newly fixed",  rows[rows$change == -1, ], file = file)
      cat_failure_section("In both",      rows[rows$change ==  0, ], file = file)

      if (x$status %in% c("i-", "i+")) {
        cat_header("Installation", level = 2, file = file)
        cat_header("Devel", level = 3, file = file)
        cat_line("```", file = file)
        cat_line(x$new$install_out, file = file)
        cat_line("```", file = file)
        cat_header("CRAN", level = 3, file = file)
        cat_line("```", file = file)
        cat_line(x$old[[1]]$install_out, file = file)
        cat_line("```", file = file)
      }
    }

  invisible()
}

cat_package_info <- function(cmp, file) {
  chk <- cmp$new
  desc <-
    tryCatch(desc::desc(text = chk$description), error = function(x) NULL)

  addifx <- function(field) {
    if (is.null(desc)) return(NULL)
    if (!desc$has_fields(field)) return(NULL)
    paste0("* ", field, ": ", normalize_space(desc$get_field(field)))
  }
  out <- c(
    paste0("* Version: ", chk$version),
    paste0("* Source code: ", pkg_source_link(chk)),
    addifx("URL"),
    addifx("BugReports"),
    addifx("Date/Publication"),
    paste0("* Number of recursive dependencies: ", num_deps(chk$package)),
    paste0("\nRun `revdep_details(,\"", chk$package, "\")` for more info")
  )
  out <- wrap_tag("details", out)
  cat(out, file = file)
}

num_deps <- function(pkg) {
  repos <- get_repos(bioc = TRUE)
  length(cran_deps(pkg, repos))
}

pkg_source_link <- function(chk) {
  if (!is.null(chk$cran)) {
    paste0("https://github.com/cran/", chk$package)
  } else {
    "???"
  }
}

wrap_tag <- function(tag, txt) {
  txt <- paste0(txt, collapse = "\n")
  paste0("<", tag, ">\n\n", txt, "\n\n</", tag, ">\n")
}

normalize_space <- function(x) {
  gsub("\\s+", " ", x)
}

cat_failure_section <- function(title, rows, file) {
  if (nrow(rows) == 0) {
    return()
  }

  cat_header(title, level = 2, file = file)
  cat(format_details_bullets(rows$output), sep = "", file = file)
}

format_details_bullets <- function(x, max_lines = 20) {
  map_chr(x, format_details_bullet, max_lines = max_lines)
}

format_details_bullet <- function(x, max_lines = 20) {
  lines <- strsplit(x, "\n")[[1]]

  title <- trimws(lines[[1]])
  details <- lines[-1]

  n <- length(details)
  if (n > max_lines) {
    details <- c("...", details[(n - max_lines):n])
  }
  if (n > 0) {
    details <- c("```", details, "```")
  }

  pad <- strrep(" ", 4)
  paste0(
    "*   ", red(title), "\n",
    paste0(pad, details, "\n", collapse = ""),
    "\n"
  )
}

#' `revdep_report_cran()` prints a short summary of the reverse dependency
#' checks, that is suitable for a CRAN submission.
#'
#' @export
#' @rdname revdep_report_summary
#' @importFrom utils available.packages

revdep_report_cran <- function(pkg = ".", results = db_results(pkg, NULL)) {
  opts <- options("crayon.enabled" = FALSE)
  on.exit(options(opts), add = TRUE)

  comparisons <- results

  status <- map_chr(comparisons, function(x) x$status %|0|% "i-")
  package <- map_chr(comparisons, "[[", "package")
  on_cran <- map_lgl(comparisons, on_cran)

  broke <- status == "-" & on_cran
  failed <- !(status %in% c("+", "-")) & on_cran

  cat_line("## revdepcheck results")
  cat_line()
  cat_line(
    "We checked ", length(comparisons), " reverse dependencies",
    if (any(!on_cran))
      paste0(" (", sum(on_cran), " from CRAN + ", sum(!on_cran), " from BioConductor)"),
    ", comparing R CMD check results across CRAN and dev versions of this package."
  )
  cat_line()
  cat_line(" * We saw ", sum(broke), " new problems")
  cat_line(" * We failed to check ", sum(failed), " packages")
  if (any(broke | failed)) {
      cat_line()
      cat_line("Issues with CRAN packages are summarised below.")
  }
  cat_line()

  if (any(broke)) {
    cat_line("### New problems")
    cat_line("(This reports the first line of each new failure)")
    cat_line()

    issues <- map(comparisons[broke], "[[", "cmp")
    new <- map(issues, function(x) x$output[x$change == 1])
    first_line <- map(new, function(x) map_chr(strsplit(x, "\n"), "[[", 1))
    collapsed <- map_chr(first_line, function(x) paste0("  ", x, "\n", collapse = ""))

    cat(paste0("* ", package[broke], "\n", collapsed, "\n", collapse = ""))
  }

  if (any(failed)) {
    cat_line("### Failed to check")
    cat_line()
    desc <- unname(c(i = "failed to install", t = "check timed out")[status])
    cat(paste0("* ", format(package[failed]), " (", desc[failed], ")\n"), sep = "")
  }

  invisible()
}

on_cran <- function(x) {
  isTRUE(x$new$cran)
}

#' `revdep_report()` writes the `README.md` and `problems.md`. This is
#' normally done automatically when the checks are complete, but you
#' can also do it when checks are in progress to get a partial report.
#'
#' @export
#' @rdname revdep_report_summary

revdep_report <- function(pkg = ".", all = FALSE, results = NULL) {
  pkg <- pkg_check(pkg)
  root <- dir_find(pkg, "root")

  # Open file here because we might write a partial note before
  # calling `revdep_report_summary()`
  readme_path <- file.path(root, "README.md")
  readme_file <- file(readme_path, encoding = "UTF-8", open = "w")
  on.exit(close(readme_file), add = TRUE)

  opts <- options("crayon.enabled" = FALSE)
  on.exit(options(opts), add = TRUE)

  if (!identical(db_metadata_get(pkg, "todo"), "done")) {
    message("Writing *partial* report")
    cat("These are *partial* results!\n\n", file = readme_file)
  }

  results <- results %||% db_results(pkg, NULL)

  message("Writing summary to 'revdep/README.md'")
  revdep_report_summary(pkg, file = readme_file, all = all, results = results)

  message("Writing problems to 'revdep/problems.md'")
  revdep_report_problems(pkg, file = file.path(root, "problems.md"), all = all, results = results)

  message("Writing failures to 'revdep/failures.md'")
  revdep_report_failures(pkg, file = file.path(root, "failures.md"), results = results)

  invisible()
}

# Helpers -----------------------------------------------------------------

report_platform <- function() {
  platform <- platform_info()
  data.frame(field = names(platform), value = unlist(platform))
}

report_libraries <- function(pkg) {
  path <- file.path(dir_find(pkg, "checks"), "libraries.csv")

  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  names(df)[4] <- "\u0394"

  df
}

report_status <- function(pkg = ".") {
  packages <- db_results(pkg, NULL)
  broken <- map_lgl(packages, is_broken)

  list(
    todo = length(db_todo(pkg)),
    ok = sum(!broken),
    broken = sum(broken)
  )
}

report_revdeps <- function(pkg = ".", all = FALSE, results = NULL) {
  results <- results %||% db_results(pkg, NULL)

  make_summary <- function(x, type) {
    rows <- x$cmp[x$cmp$type == type, , drop = FALSE]

    both <- sum(rows$change == 0)
    fixed <- sum(rows$change == -1)
    broke <- sum(rows$change == 1)

    paste0(
      if (both) both else "",
      if (fixed) paste0(" -", fixed),
      if (broke) paste0(" __+", broke, "__")
    )
  }

  problem_link <- function(pkg, status) {
    path <- ifelse(!status %in% c("+", "-"), "failures.md", "problems.md")
    slug <- gsub("[.]", "", tolower(pkg))
    paste0("[", pkg, "](", path, "#", slug, ")")
  }

  md_link <- function(pkg, lnk) {
    paste0("[", pkg, "](", lnk, ")")
  }

  if (all) {
    n_issues <- map_int(results, function(x) sum(x$cmp$change %in% c(0, 1)))
  } else {
    n_issues <- map_int(results, function(x) sum(x$cmp$change == 1))
  }

  status <-  map_chr(results, rcmdcheck_status)
  pkgname <- map_chr(results, "[[", "package")

  data.frame(
    status = status,
    issues = n_issues,
    package = ifelse(n_issues > 0, problem_link(pkgname, status), pkgname),
    version = map_chr(results, rcmdcheck_version),
    error = map_chr(results, make_summary, "error"),
    warning = map_chr(results, make_summary, "warning"),
    note = map_chr(results, make_summary, "note"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Styling -----------------------------------------------------------------

#' @importFrom knitr kable
#' @importFrom crayon bold

cat_line <- function(..., file = "", sep = "") {
  cat(..., "\n", sep = sep, file = file)
}

cat_rule <- function(..., file = "") {
  cat_line(rule(...), file = file)
}

cat_kable <- function(x, ..., file = "") {
  cat(kable(x, row.names = FALSE), sep = "\n", file = file)
  cat_line(file = file)
}

cat_header <- function(..., level = 1, file = "") {
  cat(bold(paste0(strrep("#", level), " ", ...)), "\n", sep = "", file = file)
  cat_line(file = file)
}

cat_bullet <- function(..., file = "") {
  cat_line("* ", ..., file = file)
}

cat_print <- function(x, file = "") {
  if (!identical(file, "")) {
    sink(file)
    on.exit(sink(NULL))
  }

  print(x)
}
