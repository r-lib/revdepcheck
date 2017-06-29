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

```r
source("https://install-github.me/r-lib/revdepcheck")
```

## Usage

```r
library(revdepcheck)

# Check package in working directory
# Will automatically create revdep/ directory if it doesn't already exist
revdep_check(num_workers = 4)

# Clear out all previous results
revdep_reset()
```

## License

MIT ©
[Gábor Csárdi](https://github.com/gaborcsardi),
[R Consortium](https://github.com/rconsortium),
[RStudio Inc](https://github.com/rstudio)
