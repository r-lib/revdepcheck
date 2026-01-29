# Submit a reverse dependency checking job to the cloud

Submit a reverse dependency checking job to the cloud

## Usage

``` r
cloud_check(
  pkg = ".",
  tarball = NULL,
  revdep_packages = NULL,
  extra_revdeps = NULL,
  r_version = "4.5.1",
  check_args = "--no-manual",
  bioc = FALSE
)
```

## Arguments

- pkg:

  Path to package.

- tarball:

  A pre-built package tarball, if `NULL` a tarball will be automatically
  built for the package at `pkg` by
  [`pkgbuild::build()`](https://pkgbuild.r-lib.org/reference/build.html).

- revdep_packages:

  A character vector of packages to check, if `NULL` equal to
  [`cran_revdeps()`](https://revdepcheck.r-lib.org/reference/cran_revdeps.md)

- extra_revdeps:

  Additional packages to use as source for reverse dependencies.

- r_version:

  The R version to use.

- check_args:

  Additional argument to pass to `R CMD check`

- bioc:

  Also check revdeps that live in Bioconductor? Default `FALSE`. Note
  that the cloud revdep check service does not currently include system
  dependencies of Bioconductor packages, so there is potential for more
  failed checks.

## Value

The AWS Batch job name

## See also

Other cloud:
[`cloud_broken()`](https://revdepcheck.r-lib.org/reference/cloud_broken.md),
[`cloud_browse()`](https://revdepcheck.r-lib.org/reference/cloud_browse.md),
[`cloud_cancel()`](https://revdepcheck.r-lib.org/reference/cloud_cancel.md),
[`cloud_details()`](https://revdepcheck.r-lib.org/reference/cloud_details.md),
[`cloud_fetch_results()`](https://revdepcheck.r-lib.org/reference/cloud_fetch_results.md),
[`cloud_plot()`](https://revdepcheck.r-lib.org/reference/cloud_plot.md),
[`cloud_report()`](https://revdepcheck.r-lib.org/reference/cloud_report.md),
[`cloud_results()`](https://revdepcheck.r-lib.org/reference/cloud_results.md),
[`cloud_status()`](https://revdepcheck.r-lib.org/reference/cloud_status.md),
[`cloud_summary()`](https://revdepcheck.r-lib.org/reference/cloud_summary.md)
