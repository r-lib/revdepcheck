
#' Markdown report of reverse dependency check results
#'
#' You can use these functions to get intermediate reports of a [revdep_check()]
#' running in another session.
#'
#' @inheritParams revdep_check
#' @export
#' @importFrom crayon black red yellow green
#' @importFrom sessioninfo platform_info

revdep_report_summary <- function(pkg = ".") {
  pkg <- pkg_check(pkg)

  cat_header("Platform")
  cat_kable(report_platform())

  cat_header("Libraries")
  cat_kable(report_libraries(pkg))

  cat_header("Revdeps")
  status <- report_status(pkg)
  cat_bullet("OK: ", green(as.character(status$ok)))
  cat_bullet("BROKEN: ", red(as.character(status$broken)))
  if (status$todo > 0) {
    cat_bullet("TODO: ", yellow(as.character(status$todo)))
  }
  cat_bullet("TOTAL: ", status$ok + status$broken + status$todo)

  cat_line()
  packages <- revdep_results(pkg, NULL)
  print(packages)

  invisible()
}

#' @export
#' @rdname revdep_report_summary

revdep_report_problems <- function(pkg = ".") {
  packages <- revdep_results(pkg, NULL)
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


# Helpers -----------------------------------------------------------------

report_platform <- function() {
  platform <- platform_info()
  data.frame(field = names(platform), value = unlist(platform))
}

report_libraries <- function(pkg) {
  path <- file.path(pkg, "revdep", "checks", "libraries.csv")

  df <- read.csv(path, stringsAsFactors = FALSE)
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

# Styling -----------------------------------------------------------------

#' @importFrom knitr kable
#' @importFrom crayon bold

cat_line <- function(...) cat(..., "\n", sep = "")
cat_kable <- function(x, ...) {
  cat(kable(x, row.names = FALSE), sep = "\n")
  cat_line()
}
cat_header <- function(..., level = 1) {
  cat(bold(paste0(strrep("#", level), " ", ...)), "\n", sep = "")
  cat_line()
}
cat_bullet <- function(...) cat_line("* ", ...)


