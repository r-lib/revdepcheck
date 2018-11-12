
dbenv <- new.env()

db_version <- "2.0.0"

#' @importFrom RSQLite dbIsValid dbConnect SQLite

db <- function(package) {
  if (is(package, "SQLiteConnection")) {
    package
  } else if (exists(package, envir = dbenv) &&
      dbIsValid(con <- dbenv[[package]])) {
    con
  } else if (package == ":memory:") {
    dbenv[[package]] <- dbConnect(SQLite(), ":memory:")
    dbenv[[package]]
  } else {
    if (!file.exists(dir_find(package))) {
      stop("Please start by running `revdep_check()`", call. = FALSE)
    }

    dbenv[[package]] <- dbConnect(SQLite(), dir_find(package, "db"))
    dbenv[[package]]
  }
}

#' Make sure that the database exists
#'
#' @param package The name of the package under revdep cheking.
#' @return Nothing
#'
#' @keywords internal
#' @importFrom DBI dbExecute

db_setup <- function(package) {

  db <- db(package)

  dbExecute(db, "DROP TABLE IF EXISTS revdeps")
  dbExecute(db, "DROP TABLE IF EXISTS metadata")
  dbExecute(db, "DROP TABLE IF EXISTS todo")
  dbExecute(db, "DROP TABLE IF EXISTS groups")

  dbExecute(db, "CREATE TABLE metadata (name TEXT, value TEXT)")

  db_metadata_init(package)

  ## Every NOTE, WARNING or ERROR is a separate record in the DB.
  ## The whole standard output is also stored with type 'OUTPUT'.
  ## Contents 00install.out file will be stored as INSTALL_OUT, if
  ## there were any errors. PREPERROR means that there was an error
  ## before starting the actual check
  dbExecute(db,
    "CREATE TABLE revdeps (
      package TEXT,
      version TEXT,
      maintainer TEXT,
      status TEXT,         -- PREPERROR, INSTALLERROR, ERROR, WARNING, OK
      which TEXT,
      duration TEXT,       -- seconds
      starttime TEXT,      -- when the check was performed
      result TEXT,         -- JSON, unparsed outputs
      summary TEXT         -- JSON, parsed outputs
    )"
  )
  dbExecute(db, "CREATE INDEX idx_revdeps_package ON revdeps(package)")

  dbExecute(db, "CREATE TABLE todo (package TEXT)")
  dbExecute(db, "CREATE TABLE groups (package TEXT)")

  invisible(db)
}

db_metadata_init <- function(package) {
  db_metadata_set(package, "dbversion", db_version)

  # Allow `:memory:` for unit tests
  if (package == ":memory:") {
    name <- package
  } else {
    name <- pkg_name(package)
  }
  db_metadata_set(package, "package", name)
}

#' @importFrom DBI dbExecute sqlInterpolate

db_metadata_set <- function(package, name, value) {
  db <- db(package)

  sql <- sqlInterpolate(
    db, "DELETE FROM metadata WHERE name = ?name",
    name = name
  )
  dbExecute(db, sql)

  sql <- sqlInterpolate(
    db,
    "INSERT INTO metadata VALUES (?name, ?value)",
    name = name,
    value = value
  )
  dbExecute(db, sql)
}

#' @importFrom DBI dbGetQuery sqlInterpolate

db_metadata_get <- function(package, name) {
  db <- db(package)
  sql <- sqlInterpolate(db,
    "SELECT value FROM metadata WHERE name = ?name",
    name = name
  )
  dbGetQuery(db, sql)[[1]]
}

#' @importFrom DBI dbExecute

db_clean <- function(package) {
  ## Do not use the cache, might be from an old run
  if (exists(package, envir = dbenv)) rm(list = package, envir = dbenv)

  dbExecute(db(package), "DELETE FROM revdeps")
  dbExecute(db(package), "DELETE FROM metadata")
  db_metadata_init(package)

  ## Remove the cache
  if (exists(package, envir = dbenv)) rm(list = package, envir = dbenv)
}

#' @importFrom DBI dbGetQuery
#' @importFrom RSQLite dbExistsTable

db_exists <- function(package) {
  if (!file.exists(dir_find(package, "db"))) return(FALSE)
  if (!dbExistsTable(db(package), "revdeps")) return(FALSE)

  TRUE
}

db_list <- function(package) {
  if (!db_exists(package)) return(character())
  pkgs <- dbGetQuery(
    db(package),
    "SELECT DISTINCT package, which FROM revdeps"
  )

  ## Check if both the old and new run is done
  package_names <- unique(pkgs$package)
  Filter(
    function(p) sum(pkgs$package == p) == 2,
    package_names
  )
}

#' @importFrom DBI dbGetQuery

db_todo <- function(pkgdir) {
  db <- db(pkgdir)

  todo <- dbGetQuery(db, "SELECT DISTINCT package FROM todo")
  groups <- db_groups(db)

  # Join todo packages with known groups
  join("package", .unmatched = "drop",
    todo,
    groups = groups
  )
}

#' @importFrom DBI dbWriteTable

db_todo_add <- function(pkgdir, packages) {
  db <- db(pkgdir)
  data <- pkgs_validate(packages)

  todo <- data["package"]
  dbWriteTable(db, "todo", todo, append = TRUE)

  # Merge new groups (if any) and known groups with a full outer join
  groups <- db_groups(db)
  groups <- unduplicate(full_join(data, groups, "package"))
  dbWriteTable(db, "groups", groups, overwrite = TRUE)

  invisible(pkgdir)
}

db_groups <- function(pkgdir) {
  groups <- dbGetQuery(db(pkgdir), "SELECT DISTINCT * FROM groups")
  as_tibble(groups)
}

#' @importFrom DBI dbExecute sqlInterpolate

db_insert <- function(pkgdir, package, version = NULL, maintainer = NULL,
                      status, which = c("old", "new"), duration, starttime,
                      result, summary) {

  which <- match.arg(which)

  db <- db(pkgdir)

  ## To avoid duplicate records in the DB
  dbExecute(db,
    sqlInterpolate(db,
      "DELETE FROM revdeps WHERE package = ?package AND which = ?which",
      package = package,
      which = which
    )
  )
  dbExecute(db,
    sqlInterpolate(db,
      "DELETE FROM todo WHERE package = ?package",
      package = package
    )
  )

  q <- "INSERT INTO revdeps
         (package, version, maintainer, status, which, duration,
          starttime, result, summary) VALUES
         (?package, ?version, ?maintainer, ?status, ?which, ?duration,
          ?starttime, ?result, ?summary)"


  ## TODO: better way to get version, maintainer, so they are never NULL
  dbExecute(db,
    sqlInterpolate(db, q,
      package = package, version = version %|0|% "",
      maintainer = maintainer %|0|% "",
      status = status, which = which, duration = duration,
      starttime = as.character(starttime), result = result,
      summary = summary %|0|% ""
    )
  )
}

filter_result_pkgs <- function(res, revdeps) {
  if (!is.null(revdeps)) {
    res <- res[res$package %in% revdeps, ]
    if (any(miss <- ! revdeps %in% res$package)) {
      warning(
        "No results for packages: ",
        paste(sQuote(revdeps[miss]), collapse = ", ")
      )
    }
  }
  res
}

db_get_results <- function(pkg, revdeps = NULL) {
  db <- db(pkg)

  if (is.null(revdeps)) {
    old <- dbGetQuery(db,
      "SELECT * FROM revdeps WHERE which = 'old'
       ORDER BY package COLLATE NOCASE")
    new <- dbGetQuery(db,
      "SELECT * FROM revdeps WHERE which = 'new'
       ORDER BY package COLLATE NOCASE")

  } else {
    revdepstr <- paste0("(", paste0('"', revdeps, '"', collapse = ","), ")")
    old <- dbGetQuery(db, paste0(
      "SELECT * FROM revdeps
       WHERE which = 'old' AND package IN ", revdepstr,
      "ORDER BY package COLLATE NOCASE"))
    new <- dbGetQuery(db, paste0(
      "SELECT * FROM revdeps
       WHERE which = 'new' AND package IN ", revdepstr,
      "ORDER BY package COLLATE NOCASE"))
  }

  # Match old and new on `package`, nested in list-columns. The
  # list-columns are suitable for mapping (e.g. a comparison
  # function). Because we drop unmatched rows (partial results) the
  # returned results are always in a consistent state, which is useful
  # for creating preliminary reports while the checks are running.
  results <- join("package", .unmatched = "drop",
    old = nesting(old),
    new = nesting(new)
  )

  # Match current results to known groups
  results <- join("package", .unmatched = "drop",
    groups = db_groups(db),
    results
  )

  results
}

db_results <- function(pkg, revdeps = NULL) {
  res <- db_get_results(pkg, revdeps)
  db_results_compare(res)
}
db_results_compare <- function(data) {
  # In case groups is empty (e.g. in tests, there should normally be a
  # `repo` group)
  subset <- data[c("package", "old", "new")]

  data$comparisons <- pmap(subset, function(package, old, new) {
    oldcheck <- checkFromJSON(old$result)
    newcheck <- checkFromJSON(new$result)
    try_compare_checks(package, oldcheck, newcheck)
  })

  data
}

db_maintainers <- function(pkg) {
  res <- dbGetQuery(db(pkg), "SELECT DISTINCT maintainer, package FROM revdeps")
  set_names(res$maintainer, res$package)
}
