# Browse to the AWS url for the job

This is useful for closer inspection of individual jobs while they are
running or after the fact.

## Usage

``` r
cloud_browse(job_name = cloud_job(), package = NULL)
```

## Arguments

- job_name:

  The job name, as returned by
  [`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md).

- package:

  If `NULL` browses to the URL of the overall job. If a package name,
  browses to the URL for that specific package job.

## See also

Other cloud:
[`cloud_broken()`](https://revdepcheck.r-lib.org/reference/cloud_broken.md),
[`cloud_cancel()`](https://revdepcheck.r-lib.org/reference/cloud_cancel.md),
[`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md),
[`cloud_details()`](https://revdepcheck.r-lib.org/reference/cloud_details.md),
[`cloud_fetch_results()`](https://revdepcheck.r-lib.org/reference/cloud_fetch_results.md),
[`cloud_plot()`](https://revdepcheck.r-lib.org/reference/cloud_plot.md),
[`cloud_report()`](https://revdepcheck.r-lib.org/reference/cloud_report.md),
[`cloud_results()`](https://revdepcheck.r-lib.org/reference/cloud_results.md),
[`cloud_status()`](https://revdepcheck.r-lib.org/reference/cloud_status.md),
[`cloud_summary()`](https://revdepcheck.r-lib.org/reference/cloud_summary.md)
