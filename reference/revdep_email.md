# Notify revdep maintainers about problems

This function uses gmail to automatically notify all maintainers of
revdeps that have failures with the new version of the package. The form
of the email is fixed, but it uses template parameters so that you can
control the details: set the variables in `revdeps/email.yaml`. You'll
be prompted to review the template before any emails are sent; or you
can use `revdep_email_draft()` to see a draft version.

## Usage

``` r
revdep_email(
  type = c("broken", "failed"),
  pkg = ".",
  packages = NULL,
  draft = FALSE
)

revdep_email_draft(type = "broken", pkg = ".", data = email_data(pkg))
```

## Arguments

- type:

  Type of problems to notify about; either "broken" (i.e. there is a new
  `R CMD check` failure that did not currently occur) or "failed" (i.e.
  the check failure either during installation or because of a timeout).

- pkg:

  Path to package.

- packages:

  A character vector of package names. Use this if some emails failed to
  send in the previous round. If omitted uses all packages.

- draft:

  If `TRUE`, create a gmail draft rather than sending the email
  directly.

- data:

  Optionally, supply a named list to provide your own parameters to fill
  in the template

## Details

To use this function, you'll need to give the gmailr app authority to
send emails from gmail. To revoke that authority, delete the
`.httr-oauth` file created in your working directory.
