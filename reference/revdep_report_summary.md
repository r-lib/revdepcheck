# Markdown report of reverse dependency check results

You can use these functions to get intermediate reports of a
[`revdep_check()`](https://revdepcheck.r-lib.org/reference/revdep_check.md)
running in another session.

## Usage

``` r
revdep_report_summary(pkg = ".", file = "", all = FALSE, results = NULL)

revdep_report_problems(
  pkg = ".",
  file = "",
  all = FALSE,
  results = NULL,
  bioc = TRUE,
  cran = TRUE
)

revdep_report_failures(
  pkg = ".",
  file = "",
  results = NULL,
  bioc = TRUE,
  cran = TRUE
)

revdep_report_cran(pkg = ".", file = "", results = NULL)

revdep_report(pkg = ".", all = FALSE, results = NULL, bioc = TRUE, cran = TRUE)
```

## Arguments

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

  Cached results from `db_results()`. Expert use only.

- bioc:

  Also check revdeps that live in Bioconductor?

- cran:

  Should cran mirror be attached to getOpion("repos") if it is not
  already present.

## Details

`revdep_report_summary()` writes the contents of `README.md`, by default
to the console. This is handy to quickly inspect the (current) list of
problematic packages.
