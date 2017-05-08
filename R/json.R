
toJSON <- function(x, force = TRUE, ...) {
  jsonlite::toJSON(
    list(
      class = class(x),
      object = unclass(x)
    ),
    force = force,
    ...
  )
}

fromJSON <- function(txt, ...) {
  obj <- jsonlite::fromJSON(txt, ...)
  if (is.list(obj) && identical(names(obj), c("class", "object"))) {
    structure(obj$object, class = obj$class)
  } else {
    obj
  }
}

checkFromJSON <- function(txt, ...) {
  check <- fromJSON(txt, ...)
  check$errors   <- as.character(check$errors)
  check$warnings <- as.character(check$warnings)
  check$notes    <- as.character(check$notes)
  check
}
