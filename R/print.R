
#' @export

print.revdepcheck_results <- function(x, ...) {
  for (package in x) summary(package)
  invisible(x)
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
  x2 <- x
  class(x2) <- "rcmdcheck_comparison"
  print(x2, header = FALSE)

  ## Old version
  cat(rule(left = "Before"))
  if (inherits(x$old, "error")) {
    cat(red("<Error before the package check started>"))
  } else {
    print(x$old[[1]], header = FALSE)
  }

  ## New version
  cat("\n")
  cat(rule(left = "After"))
  if (inherits(x$new, "error")) {
    cat(red("<Error before the package check started>"))
  } else {
    print(x$new, header = FALSE)
  }
}
