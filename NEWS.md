
# 1.0.0.9000

* New `revdep_pkgs()` function returns a tibble of revdep
  packages. You can filter revdeps with the `dependencies` and `bioc`
  arguments (which were originally arguments to `revdep_check()`).
  The tibble of reverse dependencies always includes `package` and
  `repo` column. The latter matches a repository (CRAN or
  Bioconductor) to each revdep.

  ```
  revdep_pkgs("rlang")
  #> # A tibble: 339 x 2
  #>    repo  package
  #>    <chr> <chr>
  #>  1 CRAN  AMR
  #>  2 CRAN  amt
  #> # … with 337 more rows
  ```

  `revdep_pkgs()` typically takes one package name. You can also pass
  a character vector of packages whose reverse dependencies should be
  checked. This is particularly useful to check higher-order
  dependencies of your package. In this case, an additional column
  `set` is added to the tibble to match packages to their dependency:

  ```
  revdep_pkgs(c("rlang", "purrr"))
  #> # A tibble: 603 x 3
  #>     set   repo  package
  #>     <chr> <chr> <chr>
  #>   1 rlang CRAN  AMR
  #>   2 rlang CRAN  amt
  #>   3 rlang CRAN  anomalize
  #>
  #> ...
  #>
  #> 601 purrr CRAN     zeligverse
  #> 602 purrr BioCsoft cymruservices
  #> 603 purrr BioCsoft SemNetCleaner
  ```

  Finally, if you pass `NULL` instead of a character vector, you get
  all CRAN packages in the tibble. This is useful in combination with
  `dplyr::sample_n()` to check multiple versions of R with a random
  sample of CRAN packages (infrastructure to support this will be
  added later).

* `revdep_check()` no longer accepts `dependencies` and `bioc`
  arguments. Instead, it optionally takes a tibble of revdep packages,
  such as returned by `revdep_pkgs()`. The tibble must contain a
  `package` column. All other columns are taken to define groups of
  packages, such as CRAN versus Bioconductor packages. The groupings
  are included in the summary report.

* `revdep_todo()` now returns a tibble instead of a character
  vector. The groups are stored in a df-col (this requires a recent
  version of tibble).

* Exported `revdep_report()` to write the current results to README.md
  and problems.md.


# 1.0.0

First public release.
