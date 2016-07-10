
dbenv <- new.env()

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
#' @importFrom RSQLite dbSendQuery

db_setup <- function(package) {

  db <- db(package)

  dbSendQuery(db, "DROP TABLE IF EXISTS revdeps")
  dbSendQuery(db, "DROP TABLE IF EXISTS metadata")

  dbSendQuery(db, "CREATE TABLE metadata (name TEXT, value TEXT)")

  db_metadata_init(package)

  ## Every NOTE, WARNING or ERROR is a separate record in the DB.
  ## The whole standard output is also stored with type 'OUTPUT'
  dbSendQuery(db,
    "CREATE TABLE revdeps (
      package TEXT,
      type VARCHAR(10),      -- OUTPUT, NOTE, WARNING, ERROR
      output TEXT
    )"
  )
  dbSendQuery(db, "CREATE INDEX idx_revdeps_package ON revdeps(package)")
}

#' @importFrom RSQLite dbSendQuery
#' @importFrom DBI sqlInterpolate

db_metadata_init <- function(package) {
  db <- db(package)
  q <- "INSERT INTO metadata VALUES (?name, ?val)"

  dbSendQuery(db, sqlInterpolate(db, q, name = "dbversion", val = "1.0.0"))
  dbSendQuery(db, sqlInterpolate(db, q, name = "package",
                                 val = get_package_name(package)))
}

#' @importFrom RSQLite dbSendQuery

db_clean <- function(package) {
  ## Do not use the cache, might be from an old run
  if (exists(package, envir = dbenv)) rm(list = package, envir = dbenv)

  dbSendQuery(db(package), "DELETE FROM revdeps")
  dbSendQuery(db(package), "DELETE FROM metadata")
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

#' @importFrom RSQLite dbSendQuery
#' @importFrom DBI sqlInterpolate

db_insert <- function(package, results) {
  db <- db(package)
  q <- "INSERT INTO revdeps VALUES (?package, ?type, ?output)"
  ins <- function(type, output) {
    dbSendQuery(db,
      sqlInterpolate(db, q,
        package = results$package, type = type, output = output
      )
    )
  }

  dbSendQuery(db, "BEGIN")
  on.exit(try(dbSendQuery(db, "ROLLBACK"), silent = TRUE))
  ins("OUTPUT", results$output$stdout)
  for (ent in results$errors)   ins("ERROR", ent)
  for (ent in results$warnings) ins("WARNING", ent)
  for (ent in results$notes)    ins("NOTE", ent)
  dbSendQuery(db, "COMMIT")
  on.exit()
}

db_results <- function(package) {
  ## TODO
}
