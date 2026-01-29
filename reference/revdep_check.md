# Run revdep checks

`revdep_check()` runs `R CMD check` on all reverse dependencies of your
package. To avoid false positives, it runs `R CMD check` twice: once for
released version on CRAN and once for the local development version. It
then reports the differences so you can see what checks were previously
ok but now fail.

It requires to use a repos option that provides the source code of the
packages not binaries.

Once your package has been successfully submitted to CRAN, you should
run `revdep_reset()`. This deletes all files used for checking, freeing
up disk space and leaving you in a clean state for the next release.

## Usage

``` r
revdep_check(
  pkg = ".",
  dependencies = c("Depends", "Imports", "Suggests", "LinkingTo"),
  quiet = TRUE,
  timeout = as.difftime(10, units = "mins"),
  num_workers = 1,
  bioc = TRUE,
  cran = TRUE,
  env = revdep_env_vars()
)

revdep_reset(pkg = ".")
```

## Arguments

- pkg:

  Path to package.

- dependencies:

  Which types of revdeps should be checked. For CRAN release, we
  recommend using the default.

- quiet:

  Suppress output from internal processes?

- timeout:

  Maximum time to wait (in seconds) for `R CMD check` to complete.
  Default is 10 minutes.

- num_workers:

  Number of parallel workers to use

- bioc:

  Also check revdeps that live in Bioconductor?

- cran:

  Should cran mirror be attached to getOpion("repos") if it is not
  already present.

- env:

  Environment variables to set for the install and check processes. See
  [`revdep_env_vars()`](https://revdepcheck.r-lib.org/reference/revdep_env_vars.md).

## Details

`revdep_check()` proceeds in four steps:

1.  **Init**: create the `revdep/` subdirectory if it doesn't already
    exist, and save the list of reverse dependencies to check.

2.  **Install**: install the CRAN (released) and local (development)
    versions of your package, including all dependencies.

3.  **Run**: run `R CMD check` twice for each reverse dependency, once
    for the CRAN version and one for the local version. The checks are
    run in parallel using `num_worker` processes.

4.  **Report**: generate reports showing differences between the check
    results for the CRAN and local versions of your package. The focus
    of the report is on new failures. The reports are saved in
    `revdep/`.

`revdep_check()` is designed to seamlessly resume in the case of
failure: just re-run `revdep_check()` and it will start from where it
left off. If you want to start again from scratch, run `revdep_reset()`.

## See also

To see more details of problems during a run, call
[`revdep_summary()`](https://revdepcheck.r-lib.org/reference/revdep_details.md)
and
[`revdep_details()`](https://revdepcheck.r-lib.org/reference/revdep_details.md)
in another process.
