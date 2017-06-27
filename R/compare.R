
#' @importFrom rcmdcheck compare_checks

try_compare_checks <- function(old, new, package, version) {
  if (inherits(old, "error") || inherits(new, "error")) {
    structure(
      list(
        old = old,
        new = new,
        cmp = NULL,
        package = package,
        version = version
      ),
      class = "rcmdcheck_error"
    )

  } else {
    tryCatch(
      compare_checks(old, new),
      error = function(e) {
        structure(
          list(
            old = old,
            new = new,
            cmp = NULL,
            package = package,
            version = version
          ),
          class = "rcmdcheck_error"
        )
      }
    )
  }
}

#' @importFrom clisymbols symbol
#' @importFrom crayon make_style red

summary.rcmdcheck_error <- function(object, ...) {
  pale <- make_style("darkgrey")

  header <- paste(red(symbol$cross), object$package, object$version)

  if (inherits(object$old, "error") && inherits(object$new, "error")) {
    cat(
      pale(paste0(
        col_align(header, width = 40),
        " ", symbol$line, symbol$line, " ",
        "E: ", red("?-?+?"), " | ",
        "W: ", red("?-?+?"), " | ",
        "N: ", red("?-?+?")
      )),
      "\n",
      sep = ""
    )

  } else if (inherits(object$old, "error")) {
    cat(
      pale(paste0(
        col_align(header, width = 40),
        " ", symbol$line, symbol$line, " ",
        "E: ", red(paste0("?  +", length(object$new$errors))), " | ",
        "W: ", red(paste0("?  +", length(object$new$warnings))), " | ",
        "N: ", red(paste0("?  +", length(object$new$notes))), " | "
      )),
      "\n",
      sep = ""
    )

  } else {
    cat(
      pale(paste0(
        col_align(header, width = 40),
        " ", symbol$line, symbol$line, " ",
        "E: ", red(paste0(length(object$old$errors), "-?+?")), " | ",
        "W: ", red(paste0(length(object$old$warnings), "-?+?")), " | ",
        "N: ", red(paste0(length(object$old$notes), "-?+?")), " | "
      )),
      "\n",
      sep = ""
    )
  }
}
