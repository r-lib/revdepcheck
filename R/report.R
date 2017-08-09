
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

  comparisons <- revdep_results(pkg, NULL)
  n_issues <- map_int(comparisons, function(x) sum(x$cmp$change %in% c(0, 1)))

  lapply(comparisons[n_issues > 0], failure_details, file = file)

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
  vapply(x, format_details_bullet, max_lines = max_lines, FUN.VALUE = character(1))
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


# Helpers -----------------------------------------------------------------

report_platform <- function() {
  platform <- platform_info()
  data.frame(field = names(platform), value = unlist(platform))
}

report_libraries <- function(pkg) {
  path <- file.path(pkg, "revdep", "checks", "libraries.csv")

  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  names(df)[4] <- "\u0394"

  df
}

report_status <- function(pkg = ".") {
  packages <- revdep_results(pkg, NULL)
  broken <- vapply(packages, is_broken, logical(1))

  list(
    todo = length(db_todo(pkg)),
    ok = sum(!broken),
    broken = sum(broken)
  )
}

report_revdeps <- function(pkg = ".") {
  comparisons <- revdep_results(pkg, NULL)

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
    paste0("[", pkg, "](problems.md#", tolower(pkg), ")")
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

map_chr <- function(x, fun, ...) {
  vapply(x, fun, ..., FUN.VALUE = character(1), USE.NAMES = FALSE)
}
map_lgl <- function(x, fun, ...) {
  vapply(x, fun, ..., FUN.VALUE = logical(1), USE.NAMES = FALSE)
}
map_int <- function(x, fun, ...) {
  vapply(x, fun, ..., FUN.VALUE = integer(1), USE.NAMES = FALSE)
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
