revdep_results <- function(pkg = ".", revdeps = NULL) {
  res <- db_results(pkg, revdeps)
  class(res) <- "revdepcheck_results"
  res
}

#' @export

print.revdepcheck_results <- function(x, ...) {
  for (package in x) summary(package)
  invisible(x)
}


#' Display details of a single revdepcheck
#'
#' Use this to see nicely formatted results of processed packages while
#' [revdep_check()] is running in another process.
#'
#' @export
#' @param pkg Path to package
#' @param revdep Name of revdep package.
#' @importFrom rcmdcheck compare_checks

revdep_details <- function(pkg = ".", revdep) {
  assert_that(is_string(revdep))
  record <- db_details(pkg, revdep)
  old <- checkFromJSON(record$old$result[[1]])
  new <- checkFromJSON(record$new$result[[1]])
  res <- compare_checks(old, new)
  class(res) <- "revdepcheck_details"
  res
}

#' @export
#' @importFrom boxes rule
#' @importFrom crayon cyan

print.revdepcheck_details <- function(x, ...) {
  package <- x$package %||% x$new$package
  version <- x$version %||% x$new$version

  ## Header
  cat(rule(
    left = cyan("Reverse dependency check"),
    right = cyan(package, version),
    line_color = "cyan",
    line = 2
  ))
  cat("\n")

  ## First a summary
  cat("\n")
  cat(rule(left = "Summary"))
  cat("\n")
  x2 <- x
  class(x2) <- "rcmdcheck_comparison"
  print(x2, header = FALSE)

  ## Old version
  cat(rule(left = "Before"))
  cat("\n")
  if (inherits(x$old, "error")) {
    cat(red("<Error before the package check started>"))
  } else {
    print(x$old[[1]], header = FALSE)
    print_install_out(x$old[[1]])
  }

  ## New version
  cat("\n")
  cat(rule(left = "After"))
  cat("\n")
  if (inherits(x$new, "error")) {
    cat(red("<Error before the package check started>"))
  } else {
    print(x$new, header = FALSE)
    print_install_out(x$new)
  }
}

#' @importFrom rcmdcheck check_details
#' @importFrom utils tail

print_install_out <- function(x) {
  details <- check_details(x)
  if (any(grepl("Installation failed.*00install.out.*for details",
                details$errors))) {
    out <- strsplit(details$install_out, "\n")[[1]]
    cat("\n", symbol$line, symbol$line, sep = "")
    if (length(out) > 15) {
      cat(" 'install.out' contents (last 13 lines):\n")
      out <- c("...", tail(out, 13))
    } else {
      cat(" 'install.out' contents:\n")
    }
    cat(out, sep = "\n")
  }
}
