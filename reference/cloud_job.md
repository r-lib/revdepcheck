# Return the current cloud job

The `job_name` is automatically set by
[`cloud_check()`](https://revdepcheck.r-lib.org/reference/cloud_check.md)
and is remembered for the duration of the current R session. If there is
no active `job_name`, but there are local cloud check results,
`job_name` is inferred from the most recently modified cloud check
results.

## Usage

``` r
cloud_job(job_name = NULL, pkg = ".")
```

## Arguments

- job_name:

  If not `NULL`, sets the active `job_name` to the input.

- pkg:

  Path to package.
