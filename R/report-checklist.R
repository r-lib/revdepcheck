revdep_report_checklist <- function(pkg, results, file = "") {
  worse <- map_lgl(results, \(x) any(x$cmp$change == 1))
  problems <- results[worse]

  for (problem in problems) {
    url <- pkg_links(problem)[[1]]
    cat_glue("* [ ] [{problem$package}]({url}) — ", file = file)
  }
}

# Create a vector of links in order of desirability
pkg_links <- function(result) {
  links <- list()

  desc <- tryCatch(
    desc::desc(text = result$new$description),
    error = function(x) NULL
  )
  if (!is.null(desc)) {
    links[["GitHub"]] <- pkg_github(desc)

    maintainer <- desc$get_maintainer()
    email <- rematch2::re_match(desc$get_maintainer(), "<(.+)>")[[1]]
    if (!is.na(email)) {
      links[["Email"]] <- paste0("mailto:", email)
    }
  }

  if (isTRUE(result$new$cran)) {
    links[["GitHub mirror"]] <- paste0(
      "https://github.com/cran/",
      result$package
    )
  }

  unlist(links)
}
