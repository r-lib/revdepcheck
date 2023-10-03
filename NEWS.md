# revdepcheck (development version)

* `cloud_check()` gains the ability to check Bioconductor packages via a new
  `bioc` argument, with default `TRUE` (#362)

* updated pkgdown template and url to https://revdepcheck.r-lib.org.

* `cloud_check()` gains the ability to add additional packages as the source
  of reverse dependencies.

* `cran_revdeps()` now accepts multiple packge names.

* `cloud_results()` gains a progress bar so you can see what's happening
  for large revdep runs (#273)
  
* `cloud_report()` can opt-out of saving failures, and saves the CRAN report
  the same way as `revdep_report()` (#271).

* `revdep_report()` now saves the results of `revdep_report_cran()` to
  `revdep/cran.md` (#204)

* Exported `revdep_report()` to write the current results to README.md
  and problems.md.


# revdepcheck 1.0.0

First public release.
