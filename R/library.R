library_compare <- function(pkg) {
  lib_new <- library_meta(dir_find(pkg, "new"))
  lib_old <- library_meta(dir_find(pkg, "old"))

  lib_cmp <- merge(
    data.frame(package = lib_old$Package, old = lib_old$Version, stringsAsFactors = FALSE),
    data.frame(package = lib_new$Package, new = lib_new$Version, stringsAsFactors = FALSE),
    all = TRUE
  )

  same <- function(x, y) (is.na(x) == is.na(y)) & (x == y)
  lib_cmp[["\u0394"]] <- ifelse(same(lib_cmp$new, lib_cmp$old), "", "*")

  # Move tested package to top
  pkgname <- pkg_name(pkg)

  idx <- which(lib_cmp$package == pkgname)
  lib_cmp[union(idx, seq_len(nrow(lib_cmp))), , drop = FALSE]
}

library_meta <- function(libpath) {
  lib <- installed.packages(libpath)
  rownames(lib) <- NULL
  as.data.frame(lib, stringsAsFactors = FALSE)
}
