
#' Markdown report of reverse dependency check results
#'
#' @inheritParams revdep_check
#' @param revdeps Optionally, supply a character vector of package names
#'   to report on.  If omitted, reports on all revdeps.
#'
#' @export
#' @importFrom crayon black red yellow green
#' @importFrom sessioninfo platform_info

revdep_report_summary <- function(pkg = ".", revdeps = NULL) {
  cat_header("Platform")
  cat_line()
  platform <- platform_info()
  platform_df <- data.frame(field = names(platform), value = unlist(platform))
  cat_kable(platform_df)

  cat_header("Libraries")
  cat_line()
  cat_kable(library_compare(pkg))

  cat_header("Revdeps")
  cat_line()

  packages <- revdep_results(pkg, revdeps)
  broken <- vapply(packages, is_broken, logical(1))

  n_todo <- length(db_todo(pkg))
  n_ok <- sum(!broken)
  n_broken <- sum(broken)

  cat_bullet("BROKEN: ", red(as.character(n_broken)))
  cat_bullet("OK: ", green(as.character(n_ok)))
  if (n_todo > 0) {
    cat_bullet("TODO: ", yellow(as.character(n_todo)))
  }
  cat_line()
  print(packages)

  invisible()
}

#' @export
#' @rdname revdep_report_summary

revdep_report_problems <- function(pkg = ".", revdeps = NULL) {
  packages <- revdep_results(pkg, revdeps)
  broken <- vapply(packages, is_broken, logical(1))

  lapply(packages[broken], failure_details)

  invisible()
}

failure_details <- function(x) {
  old <- x$old[[1]]
  cat_header(old$package, " (", old$version, ")")
  cat_line()

  new <- x$cmp[x$cmp$which == "new", , drop = FALSE]
  cat(format_details_bullets(new$output), sep = "")

  invisible()
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
  details <- c("```", details, "```")

  pad <- strrep(" ", 4)
  paste0(
    "*   ", red(title), "\n",
    paste0(pad, details, "\n", collapse = ""),
    "\n"
  )
}

#' @importFrom knitr kable
#' @importFrom crayon bold

cat_line <- function(...) cat(..., "\n", sep = "")
cat_kable <- function(x, ...) {
  cat(kable(x, row.names = FALSE), sep = "\n")
  cat_line()
}
cat_header <- function(..., level = 1) {
  cat(bold(paste0(strrep("#", level), " ", ...)), "\n", sep = "")
}
cat_bullet <- function(...) cat_line("* ", ...)
