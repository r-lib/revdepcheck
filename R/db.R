
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
  ## there were any errors.
  dbExecute(db,
    "CREATE TABLE revdeps (
      package TEXT,
      type VARCHAR(10),      -- OUTPUT, NOTE, WARNING, ERROR, INSTALL_OUT
      output TEXT
    )"
  )
  dbExecute(db, "CREATE INDEX idx_revdeps_package ON revdeps(package)")
}

#' @importFrom DBI dbExecute sqlInterpolate

db_metadata_init <- function(package) {
  db <- db(package)
  q <- "INSERT INTO metadata VALUES (?name, ?val)"

  dbExecute(db, sqlInterpolate(db, q, name = "dbversion", val = "1.0.0"))
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

db_insert <- function(package, results) {
  db <- db(package)
  q <- "INSERT INTO revdeps VALUES (?package, ?type, ?output)"
  ins <- function(type, output) {
    dbExecute(db,
      sqlInterpolate(db, q,
        package = results$package, type = type, output = output
      )
    )
  }

  dbExecute(db, "BEGIN")
  on.exit(try(dbExecute(db, "ROLLBACK"), silent = TRUE))
  ins("OUTPUT", results$output$stdout)
  for (ent in results$errors)   ins("ERROR", ent)
  for (ent in results$warnings) ins("WARNING", ent)
  for (ent in results$notes)    ins("NOTE", ent)
  if (!is.null(results$install_out)) {
    ins("INSTALL_OUT", results$install_out)
  }
  dbExecute(db, "COMMIT")
  on.exit()
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
  res <- dbGetQuery(db(pkg),
    "SELECT package, SUM(type=='NOTE') AS note,
            SUM(type=='WARNING') AS warning, SUM(type=='ERROR') AS error
       FROM revdeps GROUP BY package")
  filter_result_pkgs(res, revdeps)
}

db_details <- function(pkg, revdeps) {
  res <- dbGetQuery(db(pkg),
    "SELECT package, type, output
       FROM revdeps")
  fres <- filter_result_pkgs(res, revdeps)
  fres[fres$type %in% c('NOTE', 'WARNING', 'ERROR'), ]
}

db_maintainers <- function(pkg, revdeps) {
  ## TODO
}
