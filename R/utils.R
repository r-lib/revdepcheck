
`%||%` <- function(l, r) if (length(l) == 0) r else l

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

#' @importFrom utils installed.packages

base_packages <- function() {
  rownames(installed.packages(priority="base"))
}

lapply_with_names <- function(X, FUN, ...) {
  n <- if (!is.null(names(X))) names(X) else if (is.character(X)) X
  structure(lapply(X, FUN, ...), names = n)
}

drop_nulls <- function(x) {
  is_null <- vapply(x, is.null, logical(1))
  x[!is_null]
}

#' @importFrom crayon col_nchar

col_align <- function(text, width = getOption("width"),
                      align = c("left", "center", "right")) {

  align <- match.arg(align)
  nc <- col_nchar(text)

  if (width <= nc) {
    text

  } else if (align == "left") {
    paste0(text, make_space(width - nc))

  } else if (align == "center") {
    paste0(make_space(ceiling((width - nc) / 2)),
           text,
           make_space(floor((width - nc) / 2)))

  } else {
    paste0(make_space(width - nc), text)
  }
}

make_space <- function(num, filling = " ") {
  strrep(filling, num)
}

compact <- function(x) Filter(Negate(is.null), x)

clear_line <- function(width = getOption("width")) {
  spaces <- paste(rep(" ", width), collapse = "")
  cat("\r", spaces, "\r", sep = "")
}

str_trunc <- function(x, n) {
  if (n <= 3) {
    substr("...", 1, n)
  } else if (nchar(x) < n) {
    x
  } else {
    paste0(substr(x, 1, n - 3), "...")
  }
}


#' @importFrom withr with_options with_libpaths with_envvar

execute_r <- function(px_opts, new_session = FALSE) {
  if (new_session) {
    do.call(r, px_opts)
  } else {
    with_options(
      list(repos = px_opts$repos),
      with_libpaths(px_opts$libpath,
        with_envvar(px_opts$env,
          do.call(px_opts$func, px_opts$args)
        )
      )
    )
  }
}

str_trim <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}
