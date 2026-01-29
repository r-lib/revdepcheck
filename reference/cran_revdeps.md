# Retrieve the reverse dependencies for a package

Retrieve the reverse dependencies for a package

## Usage

``` r
cran_revdeps(package, dependencies = TRUE, bioc = FALSE, cran = TRUE)
```

## Arguments

- package:

  The package (or packages) to search for reverse dependencies.

- dependencies:

  Which types of revdeps should be checked. For CRAN release, we
  recommend using the default.

- bioc:

  Also check revdeps that live in Bioconductor?

- cran:

  Should cran mirror be attached to getOpion("repos") if it is not
  already present.
