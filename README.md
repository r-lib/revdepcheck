# revdepcheck

> Automated, Isolated, Reverse Dependency Checking

[![Linux Build Status](https://travis-ci.org/r-lib/revdepcheck.svg?branch=master)](https://travis-ci.org/r-lib/revdepcheck)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/r-lib/revdepcheck?svg=true)](https://ci.appveyor.com/project/gaborcsardi/revdepcheck)
[![](http://www.r-pkg.org/badges/version/revdepcheck)](http://www.r-pkg.org/pkg/revdepcheck)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/revdepcheck)](http://www.r-pkg.org/pkg/revdepcheck)

## Features

* To avoid false positives due to existing failures, revdepcheck runs 
  `R CMD check` twice for each revdep, once with the CRAN version of your 
  package, and once with the local development version. revdepcheck
  reports the difference, so you can see exactly what has changed.

* To speed up installation of revdeps and their dependencies, revdepcheck 
  relies on [crancache](https://github.com/r-lib/crancache). You can see what 
  packages are currently cached with `crancache::crancache_list()`.
  
* revdepcheck is carefully designed to make long running checks as pleasant
  as possible. You run checks in parallel, check time is limited to 10 minutes,
  and an elegant progress bar keeps you up-to-date with what's happening
  (including an estimate of how much time is remaining).

## Installation

Once on CRAN, install the package as usual:

```r
install.packages("revdepcheck")
```

## Usage

```r
library(revdepcheck)
```

Check package in working directory, creating "revdep/" directory if it doesn't already exist:
```r
revdep_check(num_workers = 4)
```
If the run fails to complate, run again and it will pick up where it left off:
```r
revdep_check(num_workers = 4)
```

During execution, run these in a *separate R process* to view status completed checks:
```r
revdep_summary()                 # table of results by package 
revdep_details(".", "<package>") # full details for the specified package
```
Generate human-friendly summary documents in `revdep/`:
```r
revdep_report()
## Writing *partial* report
## Writing summary to 'revdep/README.md'
## Writing problems to 'revdep/problems.md'
## Writing failures to 'revdep/failures.md'
```

Manage a "todo" list of packages to examine:
```r
revdep_add(pkg = ".", <packages>)  # add <packages> to the list
revdep_rm(pkg = ".", <packages>).  # remove <packages> from list

revdep_add_broken()  # add all broken packages
revdep_add_new()     # add newly available packages
revdep_todo()        # list packages in the todo list
```

Clear out all previous results
```r
revdep_reset()
```

We recommend running `revdep_check()` in a separate process (e.g. new terminal under RStudio). That way, while it runs in a background tab, you can easily use your `revdep_details(revdep = "pkg")` to see what's gone wrong with "pkg".

## Status Flags:

* install newly fails:  `i-`
* install still fails:  `i+`
* install/check newly timeouts: `t-`
* install/check still timeouts: `t+`
* No new failures, success: `+`
* Some new failures: `-`


## License

MIT ©
[Gábor Csárdi](https://github.com/gaborcsardi),
[R Consortium](https://github.com/rconsortium),
[RStudio Inc](https://github.com/rstudio)
