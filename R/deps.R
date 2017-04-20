
#' @importFrom tools dependsOnPkgs

cran_revdeps <- function(package, dependencies) {
  stopifnot(is_string(package))
  dependsOnPkgs(
    package,
    recursive = FALSE,
    installed = cran_rds(),
    dependencies = dependencies
  )
}

cran_rds <- (function() {
  rds <- NULL
  function() {
    if (is.null(rds)) {
      con <- gzcon(
        url("http://cran.R-project.org/web/packages/packages.rds", "rb")
      )
      on.exit(close(con))
      rds <<- readRDS(con)
      rownames(rds) <<- rds[, "Package"]
    }
    rds
  }
})()
