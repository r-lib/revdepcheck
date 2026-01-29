# Changelog

## revdepcheck (development version)

- `cloud_check(r_version = "4.3.1")` is the updated default
  ([\#361](https://github.com/r-lib/revdepcheck/issues/361)).

- [`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md)
  gains the ability to check Bioconductor packages via a new `bioc`
  argument, with a default of `FALSE` due to a relatively high
  likelihood of failed checks since Bioconductor system dependencies are
  currently not installed in the cloud check service
  ([\#362](https://github.com/r-lib/revdepcheck/issues/362),
  [\#369](https://github.com/r-lib/revdepcheck/issues/369)).

- updated pkgdown template and url to <https://revdepcheck.r-lib.org>.

- [`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md)
  gains the ability to add additional packages as the source of reverse
  dependencies.

- [`cran_revdeps()`](https://revdepcheck.r-lib.org/reference/cran_revdeps.md)
  now accepts multiple packge names.

- [`cloud_results()`](https://revdepcheck.r-lib.org/reference/cloud_results.md)
  gains a progress bar so you can see what’s happening for large revdep
  runs ([\#273](https://github.com/r-lib/revdepcheck/issues/273))

- [`cloud_report()`](https://revdepcheck.r-lib.org/reference/cloud_report.md)
  can opt-out of saving failures, and saves the CRAN report the same way
  as
  [`revdep_report()`](https://revdepcheck.r-lib.org/reference/revdep_report_summary.md)
  ([\#271](https://github.com/r-lib/revdepcheck/issues/271)).

- [`revdep_report()`](https://revdepcheck.r-lib.org/reference/revdep_report_summary.md)
  now saves the results of
  [`revdep_report_cran()`](https://revdepcheck.r-lib.org/reference/revdep_report_summary.md)
  to `revdep/cran.md`
  ([\#204](https://github.com/r-lib/revdepcheck/issues/204))

- Exported
  [`revdep_report()`](https://revdepcheck.r-lib.org/reference/revdep_report_summary.md)
  to write the current results to README.md and problems.md.

## revdepcheck 1.0.0

First public release.
