# This is the event loop of the revdep check process

This is the event loop of the revdep check process

## Usage

``` r
run_event_loop(state)
```

## Arguments

- state:

  The full state of the check process:

  - `options` contains all check parameters.

  - `packages` is a data frame with the packages to check. See details
    below.

## Details

`state$packages` is a data frame with columns:

- `package`: the name of the package

- `state`: where we are with its check. Possible values:

  - `todo`: haven't done anything yet

  - `deps_installing`: the dependencies are being installed now

  - `deps_installed`: the dependencies were already installed

  - `downloading`: the source package to check is being downloaded

  - `downloaded`: the source package was downloaded

  - `checking`: checking with the old version right now

  - `checking-checking`: checking with both versions right now

  - `done-checking`: done with the old version, checking with the new
    version right now

  - `checking-done`: checking with the old version, new version was
    already done.

  - `done-downloaded`: done with the old version, check with new version
    has not started yet

  - `done`: packages was checked with both versions

We only start the check with the new version after the check with the
old version, which simplifies the state transitions a bit.
