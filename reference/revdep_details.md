# Display revdep results

Use this to see nicely formatted results of processed packages while
[`revdep_check()`](https://revdepcheck.r-lib.org/reference/revdep_check.md)
is running in another process. `revdep_summary()` displays summary
results for all complete checks. `revdep_details()` shows you the
details for one

## Usage

``` r
revdep_details(pkg = ".", revdep)

revdep_summary(pkg = ".")
```

## Arguments

- pkg:

  Path to package

- revdep:

  Name of revdep package.
