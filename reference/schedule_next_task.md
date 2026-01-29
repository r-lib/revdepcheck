# Decide what to do next, from the current state

In we have reached the allowed number of workers, then we schedule an
idle job, we just need to wait until a worker is done.

## Usage

``` r
schedule_next_task(state)
```

## Arguments

- state:

  See
  [`run_event_loop()`](https://revdepcheck.r-lib.org/reference/run_event_loop.md)
  for a description.

## Details

Otherwise we schedule a job. In general the strategy is to finish check
as soon as possible, so if a package is in `deps_installed`, then we
schedule a check. Otherwise, if a package is in `todo`, then we schedule
a dependency install.

If there is nothing we can do now, then we schedule an idle job, i.e.
just wait until a worker gets done.
