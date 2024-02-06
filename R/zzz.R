# nolint start
.onLoad <- function(libname, pkgname) {
  # nolint end
  compare_checks <<- memoise::memoise(rcmdcheck::compare_checks)
  cloud_check_result <<- memoise::memoise(cloud_check_result)
}
