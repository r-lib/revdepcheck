
#' @export

print.revdepcheck_results <- function(x, ...) {
  for (package in x) summary(package)
  invisible(x)
}
