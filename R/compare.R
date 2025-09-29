#' @importFrom rcmdcheck compare_checks

try_compare_checks <- function(package, old, new) {
  if (!inherits(old, "rcmdcheck") || !inherits(new, "rcmdcheck")) {
    rcmdcheck_error(package, old, new)
  } else {
    compare_checks(old, new)
  }
}

rcmdcheck_error <- function(package, old, new) {
  structure(
    list(
      package = package,
      status = "E",
      old = old,
      new = new
    ),
    class = "rcmdcheck_error"
  )
}

rcmdcheck_status <- function(x) UseMethod("rcmdcheck_status")
#' @export
rcmdcheck_status.rcmdcheck_error <- function(x) "?"
#' @export
rcmdcheck_status.rcmdcheck_comparison <- function(x) x$status

rcmdcheck_version <- function(x) UseMethod("rcmdcheck_version")
#' @export
rcmdcheck_version.rcmdcheck_error <- function(x) "?"
#' @export
rcmdcheck_version.rcmdcheck_comparison <- function(x) x$versions[[1]]

is_broken <- function(x, install_failures = FALSE, timeout_failures = FALSE) {
  stat <- rcmdcheck_status(x)
  stat == "-" ||
    (install_failures && stat %in% c("i-", "E", "?")) ||
    (timeout_failures && stat == "t-")
}

#' @importFrom clisymbols symbol
#' @importFrom crayon make_style bgRed white
#' @export

summary.rcmdcheck_error <- function(object, ...) {
  header <- paste(white(bgRed("E")), object$package, object$version)

  counts <- function(x) {
    if (!inherits(x, "rcmdcheck")) {
      c("?", "?", "?")
    } else {
      lhs <- c(length(x$errors), length(x$warnings), length(x$notes))
    }
  }

  lhs <- counts(object$old)
  rhs <- counts(object$new)
  comp <- paste0(lhs, "/", rhs, "  ")

  structure(
    list(header = header, comp = comp),
    class = "rcmdcheck_error_summary"
  )
}

#' @export
print.rcmdcheck_error_summary <- function(x, ...) {
  pale <- make_style("darkgrey")
  cat_line(pale(paste0(
    col_align(x$header, width = 40),
    " ",
    symbol$line,
    symbol$line,
    " ",
    "E: ",
    red(x$comp[1]),
    " | ",
    "W: ",
    red(x$comp[2]),
    " | ",
    "N: ",
    red(x$comp[3])
  )))
}
