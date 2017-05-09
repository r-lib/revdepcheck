
`%||%` <- function(l, r) if (is.null(l)) r else l

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_package_dir <- function(x) {
  is_string(x) &&
    file.info(x)$isdir &&
    file.exists(file.path(x, "DESCRIPTION"))
}

get_package_name <- function(path) {
  read.dcf(file.path(path, "DESCRIPTION"))[, "Package"]
}

create_dir <- function(paths) {
  vapply(
    paths, FUN = dir.create, FUN.VALUE = logical(1),
    recursive = TRUE, showWarnings = FALSE
  )
}

#' @importFrom tools package_dependencies

deps_for_package <- function(package) {
  direct_deps <- unlist(package_dependencies(package, which = "most"))
  indirect_deps <- unlist(package_dependencies(direct_deps))
  all_deps <- unique(unname(c(direct_deps, indirect_deps)))
  setdiff(all_deps, base_packages())
}

#' @importFrom utils installed.packages

base_packages <- function() {
  rownames(installed.packages(priority="base"))
}

lapply_with_names <- function(X, FUN, ...) {
  structure(lapply(X, FUN, ...), names = names(X))
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
