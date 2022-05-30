# 1.0.0.9002

* Add `cran` parameter to the `get_repos()` internal and propagate it to the
  upstream functions including `revdep_check()`. It allow user to decide
  whether htey want to always append CRAN mirror to repos or not (@maksymis)

# 1.0.0.9000

* `cloud_results()` gains a progress bar so you can see what's happening
  for large revdep runs (#273)
  
* `cloud_report()` can opt-out of saving failures, and saves the CRAN report
  the same way as `revdep_report()` (#271).

* `revdep_report()` now saves the results of `revdep_report_cran()` to
  `revdep/cran.md` (#204)

* Exported `revdep_report()` to write the current results to README.md
  and problems.md.


# 1.0.0

First public release.
