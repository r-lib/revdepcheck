# Monitor the status of a cloud job

The format of the status bar is
`[jobs_queued/jobs_running/jobs_succeeded/jobs_failed - total_jobs] time_elapsed | ETA: estimate_time_remaining`

## Usage

``` r
cloud_status(job_name = cloud_job(), update_interval = 10)
```

## Arguments

- job_name:

  The job name, as returned by
  [`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md).

- update_interval:

  The number of seconds between querying for updates

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
[`cloud_results()`](https://revdepcheck.r-lib.org/reference/cloud_results.md),
[`cloud_summary()`](https://revdepcheck.r-lib.org/reference/cloud_summary.md)
