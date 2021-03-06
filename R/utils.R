#' @import rlang

`%|0|%` <- function(x, y) {
  if (!length(x)) y else x
}

#' @importFrom utils installed.packages
base_packages <- function() {
  rownames(installed.packages(priority="base"))
}

lapply_with_names <- function(X, FUN, ...) {
  n <- if (!is.null(names(X))) names(X) else if (is.character(X)) X
  structure(map(X, FUN, ...), names = n)
}

drop_nulls <- function(x) {
  is_null <- map_lgl(x, is.null)
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

line_trunc <- function(x, n = 10) {
  if (length(x) == 1 && grepl("\n", x, fixed = TRUE)) {
    x <- strsplit(x, "\n")[[1]]
  }

  if (length(x) < n * 2) {
    return(x)
  }

  c(x[1:n], "...", x[(length(x) - n + 1):length(x)])
}

#' @importFrom withr with_libpaths with_envvar

execute_r <- function(px_opts, new_session = FALSE) {
  if (new_session) {
    do.call(r, px_opts)
  } else {
    rlang::with_options(
      repos = px_opts$repos,
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

cut_into_lines <- function(x) {
  x <- do.call(paste0, as.list(x))
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- strsplit(x, "\n", fixed = TRUE)[[1]]
  if (length(x)) x else ""
}

latest_file <- function(x) {
  mtime <- file.info(x)$mtime
  tail(x[order(mtime)], 1)
}
