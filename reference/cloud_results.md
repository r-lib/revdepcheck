# Retrieve cloud results

Intended for expert use only, this can be used as input to the
[`cloud_report()`](https://revdepcheck.r-lib.org/reference/cloud_report.md)
and other functions.

## Usage

``` r
cloud_results(job_name = cloud_job(pkg = pkg), pkg = ".")
```

## Arguments

- job_name:

  The job name, as returned by
  [`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md).

- pkg:

  Path to package.

## See also

Other cloud:
[`cloud_broken()`](https://revdepcheck.r-lib.org/reference/cloud_broken.md),
[`cloud_browse()`](https://revdepcheck.r-lib.org/reference/cloud_browse.md),
[`cloud_cancel()`](https://revdepcheck.r-lib.org/reference/cloud_cancel.md),
[`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md),
[`cloud_details()`](https://revdepcheck.r-lib.org/reference/cloud_details.md),
[`cloud_fetch_results()`](https://revdepcheck.r-lib.org/reference/cloud_fetch_results.md),
[`cloud_plot()`](https://revdepcheck.r-lib.org/reference/cloud_plot.md),
[`cloud_report()`](https://revdepcheck.r-lib.org/reference/cloud_report.md),
[`cloud_status()`](https://revdepcheck.r-lib.org/reference/cloud_status.md),
[`cloud_summary()`](https://revdepcheck.r-lib.org/reference/cloud_summary.md)
