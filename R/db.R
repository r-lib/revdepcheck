
dbenv <- new.env()

db_version <- "2.0.0"

#' @importFrom RSQLite dbIsValid dbConnect SQLite

db <- function(package) {
  if (exists(package, envir = dbenv) &&
      dbIsValid(con <- dbenv[[package]])) {
    con
  } else {
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
}

#' @importFrom DBI dbExecute sqlInterpolate

db_metadata_init <- function(package) {
  db <- db(package)
  q <- "INSERT INTO metadata VALUES (?name, ?val)"

  dbExecute(db, sqlInterpolate(db, q, name = "dbversion", val = db_version))
  dbExecute(db, sqlInterpolate(db, q, name = "package",
                                 val = pkg_name(package)))
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

  dbGetQuery(db, "SELECT package FROM todo")[[1]]
}

#' @importFrom DBI dbWriteTable

db_todo_add <- function(pkgdir, packages) {
  db <- db(pkgdir)

  df <- data.frame(package = packages, stringsAsFactors = FALSE)
  dbWriteTable(db, "todo", df, append = TRUE)

  invisible(pkgdir)
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
      package = package, version = version %||% "",
      maintainer = maintainer %||% "",
      status = status, which = which, duration = duration,
      starttime = as.character(starttime), result = result,
      summary = summary %||% ""
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

db_get_results <- function(pkg, revdeps) {
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

  list(old = old, new = new)
}

db_results <- function(pkg, revdeps) {
  res <- db_get_results(pkg, revdeps)

  oldpackages <- res$old$package
  newpackages <- res$new$package

  onlynew <- setdiff(newpackages, oldpackages)
  onlyold <- setdiff(oldpackages, newpackages)
  if (length(onlynew) || length(onlyold)) {
    warning(
      "Some packages were not checked with both versions: ",
      paste(sQuote(c(onlynew, onlyold)), collapse = ", ")
    )
  }

  packages <- intersect(oldpackages, newpackages)

  lapply_with_names(packages, function(p) {
    version <- res$old$version[match(p, res$old$package)]
    oldcheck <- checkFromJSON(res$old$result[match(p, res$old$package)])
    newcheck <- checkFromJSON(res$new$result[match(p, res$new$package)])
    try_compare_checks(oldcheck, newcheck, p, version)
  })
}

db_details <- function(pkg, revdep) {
  db_get_results(pkg, revdep)
}

db_maintainers <- function(pkg, revdeps) {
  ## TODO
}
