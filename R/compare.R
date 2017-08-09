
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

is_broken <- function(x) {
  rcmdcheck_status(x) != "+"
}

#' @importFrom clisymbols symbol
#' @importFrom crayon make_style bgRed white
#' @export

summary.rcmdcheck_error <- function(object, ...) {
  pale <- make_style("darkgrey")
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

  cat_line(pale(paste0(
    col_align(header, width = 40),
    " ", symbol$line, symbol$line, " ",
    "E: ", red(comp[1]), " | ",
    "W: ", red(comp[2]), " | ",
    "N: ", red(comp[3])
  )))
}
