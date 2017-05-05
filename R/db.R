
dbenv <- new.env()

db_version <- "2.0.0"

#' @importFrom RSQLite dbIsValid dbConnect SQLite

db <- function(package) {
  if (exists(package, envir = dbenv) &&
      dbIsValid(con <- dbenv[[package]])) {
    con
  } else {
    dbenv[[package]] <- dbConnect(SQLite(), check_dir(package, "db"))
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
}

#' @importFrom DBI dbExecute sqlInterpolate

db_metadata_init <- function(package) {
  db <- db(package)
  q <- "INSERT INTO metadata VALUES (?name, ?val)"

  dbExecute(db, sqlInterpolate(db, q, name = "dbversion", val = db_version))
  dbExecute(db, sqlInterpolate(db, q, name = "package",
                                 val = get_package_name(package)))
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

db_list <- function(package) {
  if (!file.exists(check_dir(package, "db"))) return(character())
  if (!dbExistsTable(db(package), "revdeps")) return(character())
  dbGetQuery(db(package), "SELECT DISTINCT package FROM revdeps")[[1]]
}

#' @importFrom DBI dbExecute sqlInterpolate

db_insert <- function(pkgdir, package, version, maintainer, status,
                      which = c("old", "new"),
                      duration, starttime, result, summary) {

  which <- match.arg(which)

  db <- db(pkgdir)
  q <- "INSERT INTO revdeps
         (package, version, maintainer, status, which, duration,
          starttime, result, summary) VALUES
         (?package, ?version, ?maintainer, ?status, ?which, ?duration,
          ?starttime, ?result, ?summary)"

  dbExecute(db,
    sqlInterpolate(db, q,
      package = package, version = version, maintainer = maintainer,
      status = status, which = which, duration = duration,
      starttime = starttime, result = result, summary = summary
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

db_results <- function(pkg, revdeps) {
  TODO
}

db_details <- function(pkg, revdeps) {
  TODO
}

db_maintainers <- function(pkg, revdeps) {
  TODO
}
