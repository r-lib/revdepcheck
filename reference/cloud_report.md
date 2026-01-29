# Markdown report of reverse dependency check results from the cloud

You can use these functions to get intermediate reports of a running
cloud check.

## Usage

``` r
cloud_report(
  job_name = cloud_job(pkg = pkg),
  pkg = ".",
  file = "",
  all = FALSE,
  results = NULL,
  failures = TRUE
)

cloud_report_summary(
  job_name = cloud_job(pkg = pkg),
  file = "",
  all = FALSE,
  pkg = ".",
  results = NULL
)

cloud_report_problems(
  job_name = cloud_job(pkg = pkg),
  pkg = ".",
  file = "",
  all = FALSE,
  results = NULL
)

cloud_report_failures(
  job_name = cloud_job(pkg = pkg),
  pkg = ".",
  file = "",
  results = NULL
)

cloud_report_cran(job_name = cloud_job(pkg = pkg), pkg = ".", results = NULL)

cloud_report_checklist(
  job_name = cloud_job(pkg = pkg),
  pkg = ".",
  results = NULL
)
```

## Arguments

- job_name:

  The job name, as returned by
  [`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md).

- pkg:

  Path to package.

- file:

  File to write output to. Default will write to console.

- all:

  Whether to report all problems, including the ones that were already
  present in the old version of the package. This potentially generated
  a lot of output, most of which was irrelevant, so they are omitted by
  default, and only problems seen with the new version of the package
  are reported.

- results:

  Results from
  [`cloud_results()`](https://revdepcheck.r-lib.org/reference/cloud_results.md).
  Expert use only.

- failures:

  Save failures to disk?

## See also

Other cloud:
[`cloud_broken()`](https://revdepcheck.r-lib.org/reference/cloud_broken.md),
[`cloud_browse()`](https://revdepcheck.r-lib.org/reference/cloud_browse.md),
[`cloud_cancel()`](https://revdepcheck.r-lib.org/reference/cloud_cancel.md),
[`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md),
[`cloud_details()`](https://revdepcheck.r-lib.org/reference/cloud_details.md),
[`cloud_fetch_results()`](https://revdepcheck.r-lib.org/reference/cloud_fetch_results.md),
[`cloud_plot()`](https://revdepcheck.r-lib.org/reference/cloud_plot.md),
[`cloud_results()`](https://revdepcheck.r-lib.org/reference/cloud_results.md),
[`cloud_status()`](https://revdepcheck.r-lib.org/reference/cloud_status.md),
[`cloud_summary()`](https://revdepcheck.r-lib.org/reference/cloud_summary.md)
