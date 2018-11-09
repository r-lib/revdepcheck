
#' Markdown report of reverse dependency check results
#'
#' You can use these functions to get intermediate reports of a [revdep_check()]
#' running in another session.
#'
#' @inheritParams revdep_check
#' @param file File to write output to. Default will write to console.
#' @export
#' @importFrom crayon black red yellow green
#' @importFrom sessioninfo platform_info

revdep_report_summary <- function(pkg = ".", file = "") {
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
  revdeps <- report_revdeps(pkg)

  status <- revdeps$status
  revdeps$status <- NULL
  broken <- status == "-"
  failed <- !(status %in% c("+", "-"))

  revdep_report_section("Couldn't check", revdeps[failed, ], file = file)
  revdep_report_section("Broken", revdeps[broken, ], file = file)
  revdep_report_section("All", revdeps, file = file)

  invisible()
}

revdep_report_section <- function(title, rows, file) {
  if (nrow(rows) == 0) {
    return()
  }

  cat_header(title, " (", nrow(rows), ")", level = 2, file = file)
  cat_kable(rows, file = file)
}

#' @export
#' @rdname revdep_report_summary

revdep_report_problems <- function(pkg = ".", file = "") {
  if (is_string(file) && !identical(file, "")) {
    file <- file(file, encoding = "UTF-8", open = "w")
    on.exit(close(file), add = TRUE)

    opts <- options("crayon.enabled" = FALSE)
    on.exit(options(opts), add = TRUE)
  }

  comparisons <- db_results(pkg, NULL)
  n_issues <- map_int(comparisons, function(x) sum(x$cmp$change %in% c(0, 1)))

  map(comparisons[n_issues > 0], failure_details, file = file)

  invisible()
}

failure_details <- function(x, file = "") {
  cat_header(x$package, file = file)
  cat_line("Version: ", x$versions[[1]], file = file)
  cat_line(file = file)

  rows <- x$cmp
  cat_failure_section("Newly broken", rows[rows$change == +1, ], file = file)
  cat_failure_section("Newly fixed",  rows[rows$change == -1, ], file = file)
  cat_failure_section("In both",      rows[rows$change ==  0, ], file = file)

  if (x$status == "i") {
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

  invisible()
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

#' @export
#' @rdname revdep_report_summary
#' @importFrom utils available.packages

revdep_report_cran <- function(pkg = ".") {
  opts <- options("crayon.enabled" = FALSE)
  on.exit(options(opts), add = TRUE)

  comparisons <- db_results(pkg, NULL)

  status <- map_chr(comparisons, function(x) x$status %|0|% "i")
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
  desc <- desc::desc(text = x$new$description)
  identical(desc$get("Repository")[[1]], "CRAN")
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

report_revdeps <- function(pkg = ".") {
  comparisons <- db_results(pkg, NULL)

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

  problem_link <- function(pkg) {
    slug <- gsub("[.]", "", tolower(pkg))
    paste0("[", pkg, "](problems.md#", slug, ")")
  }

  n_issues <- map_int(comparisons, function(x) sum(x$cmp$change %in% c(0, 1)))

  status <-  map_chr(comparisons, rcmdcheck_status)
  pkgname <- map_chr(comparisons, "[[", "package")

  data.frame(
    status = status,
    package = ifelse(n_issues > 0, problem_link(pkgname), pkgname),
    version = map_chr(comparisons, rcmdcheck_version),
    error = map_chr(comparisons, make_summary, "error"),
    warning = map_chr(comparisons, make_summary, "warning"),
    note = map_chr(comparisons, make_summary, "note"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Styling -----------------------------------------------------------------

#' @importFrom knitr kable
#' @importFrom crayon bold

cat_line <- function(..., file = "") {
  cat(..., "\n", sep = "", file = file)
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
