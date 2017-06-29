
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
  problems <- revdeps$problems
  revdeps$problems <- NULL
  revdeps_broken <- revdeps[problems, ]

  cat_header("Broken (", nrow(revdeps_broken), ")", level = 2, file = file)
  cat_kable(revdeps_broken, file = file)

  cat_header("All (", nrow(revdeps), ")", level = 2, file = file)
  cat_kable(revdeps, file = file)

  invisible()
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

  packages <- revdep_results(pkg, NULL)
  broken <- vapply(packages, is_broken, logical(1))

  lapply(packages[broken], failure_details, file = file)

  invisible()
}

failure_details <- function(x, file = "") {
  old <- x$old[[1]]
  cat_header(old$package, file = file)
  cat_line("Version: ", old$version, file = file)
  cat_line(file = file)

  cmp <- x$cmp
  old <- unique(cmp$hash[cmp$which == "old"])
  new <- unique(cmp$hash[cmp$which == "new"])

  broke <- setdiff(new, old)
  if (length(broke) > 0) {
    cat_header("Newly broken", level = 2, file = file)
    out <- cmp$output[cmp$hash %in% broke & cmp$which == "new"]
    cat(format_details_bullets(out), sep = "", file = file)
  }

  fixed <- setdiff(old, new)
  if (length(fixed) > 0) {
    cat_header("Newly fixed", level = 2, file = file)
    out <- cmp$output[cmp$hash %in% fixed & cmp$which == "old"]
    cat(format_details_bullets(out), sep = "", file = file)
  }

  both <- intersect(old, new)
  if (length(both) > 0) {
    cat_header("In both", level = 2, file = file)
    out <- cmp$output[cmp$hash %in% both & cmp$which == "new"]
    cat(format_details_bullets(out), sep = "", file = file)
  }


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
  packages <- revdep_results(pkg, NULL)

  # Hacked out of rcmdcheck
  make_summary <- function(x, type) {
    recs <- x$cmp[x$cmp$type == type, , drop = FALSE]
    old <- unique(recs$hash[recs$which == "old"])
    new <- unique(recs$hash[recs$which == "new"])

    both <- length(intersect(old, new))
    fixed <- length(setdiff(old, new))
    broke <- length(setdiff(new, old))

    paste0(
      if (both) both else "",
      if (fixed) paste0(" -", fixed),
      if (broke) paste0(" __+", broke, "__")
    )
  }

  problems <- function(x) {
    old <- unique(x$cmp$hash[x$cmp$which == "old"])
    new <- unique(x$cmp$hash[x$cmp$which == "new"])
    length(setdiff(new, old)) > 0
  }

  problem_link <- function(pkg) {
    paste0("[", pkg, "](problems.md#", pkg, ")")
  }

  probs <- map_lgl(packages, problems)
  pkgname <- map_chr(packages, function(x) x$new$package)

  data.frame(
    problems = probs,
    package = ifelse(probs, problem_link(pkgname), pkgname),
    version = map_chr(packages, function(x) x$new$version),
    error = map_chr(packages, make_summary, "error"),
    warning = map_chr(packages, make_summary, "warning"),
    note = map_chr(packages, make_summary, "note"),
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

# Styling -----------------------------------------------------------------

#' @importFrom knitr kable
#' @importFrom crayon bold

cat_line <- function(..., file = "") {
  cat(..., "\n", sep = "", file = file)
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
