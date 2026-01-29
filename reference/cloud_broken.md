# Retrieve the names broken or failed packages

Broken packages are those whose checks got worse with the dev version.
Failed packages are those whose cloud jobs failed, either because the
spot instance was shut down by AWS or because the checks used too much
memory and were killed.

## Usage

``` r
cloud_broken(
  job_name = cloud_job(pkg = pkg),
  pkg = ".",
  install_failures = FALSE,
  timeout_failures = FALSE
)

cloud_failed(job_name = cloud_job(pkg = pkg), pkg = ".")
```

## Arguments

- job_name:

  The job name, as returned by
  [`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md).

- pkg:

  Path to package.

- install_failures:

  Whether to include packages that failed to install.

- timeout_failures:

  Whether to include packages that timed out.

## Value

A character vector with the names of broken packages, to be passed to
[`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md).

## See also

Other cloud:
[`cloud_browse()`](https://revdepcheck.r-lib.org/reference/cloud_browse.md),
[`cloud_cancel()`](https://revdepcheck.r-lib.org/reference/cloud_cancel.md),
[`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md),
[`cloud_details()`](https://revdepcheck.r-lib.org/reference/cloud_details.md),
[`cloud_fetch_results()`](https://revdepcheck.r-lib.org/reference/cloud_fetch_results.md),
[`cloud_plot()`](https://revdepcheck.r-lib.org/reference/cloud_plot.md),
[`cloud_report()`](https://revdepcheck.r-lib.org/reference/cloud_report.md),
[`cloud_results()`](https://revdepcheck.r-lib.org/reference/cloud_results.md),
[`cloud_status()`](https://revdepcheck.r-lib.org/reference/cloud_status.md),
[`cloud_summary()`](https://revdepcheck.r-lib.org/reference/cloud_summary.md)
