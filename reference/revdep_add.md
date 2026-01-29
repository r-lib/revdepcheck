# Manage the package checking to-do list.

`revdep_todo()` tells you which packages still need to be checked.
`revdep_add()` adds a single package to the to-do list. `revdep_rm()`
removes packages from the todo list. `revdep_add_broken()` re-adds all
broken packages from the last check (this is useful if you think you've
fixed the underlying problem in your package).

## Usage

``` r
revdep_add(pkg = ".", packages)

revdep_add_broken(
  pkg = ".",
  install_failures = FALSE,
  timeout_failures = FALSE
)

revdep_add_new(pkg = ".")

revdep_todo(pkg = ".")

revdep_rm(pkg = ".", packages)
```

## Arguments

- pkg:

  Path to package.

- packages:

  Character vector of package names to add

- install_failures:

  Whether to re-add packages that failed to install.

- timeout_failures:

  Whether to re-add packages that timed out.
