MiroDb <- R6::R6Class("MiroDb", public = list(
  initialize = function(dbConnectionInfo) {
    private$conn <- dbConnect(
      drv = RPostgres::Postgres(),
      dbname = dbConnectionInfo$name,
      host = dbConnectionInfo$host,
      port = dbConnectionInfo$port,
      user = dbConnectionInfo$username,
      password = dbConnectionInfo$password,
      bigint = "integer"
    )
    flog.debug("Db: MiroDb initialized.")
    return(invisible(self))
  },
  schemaExists = function(appId) {
    schemaExistsQuery <- paste0(
      "SELECT schema_name FROM information_schema.schemata WHERE schema_name=",
      dbQuoteString(private$conn, private$getDbAppId(appId))
    )
    return(length(dbGetQuery(private$conn, SQL(schemaExistsQuery))[[1]]) > 0L)
  },
  createAppSchema = function(appId) {
    if (nchar(appId) > 60L) {
      stop("App ID must not exceed 60 characters!", call. = FALSE)
    }
    dbAppId <- private$getDbAppId(appId)
    appDbCredentials <- list(
      user = dbAppId,
      password = private$genPassword()
    )
    roleExistsQuery <- paste0(
      "SELECT 1 FROM pg_roles WHERE rolname=",
      dbQuoteString(private$conn, dbAppId)
    )
    if (length(dbGetQuery(private$conn, SQL(roleExistsQuery))[[1]]) > 0L) {
      private$runQuery(paste0(
        "ALTER USER ",
        dbQuoteIdentifier(private$conn, dbAppId),
        " WITH PASSWORD ",
        dbQuoteString(private$conn, appDbCredentials$password), ";"
      ),
      mask = appDbCredentials$password
      )
    } else {
      private$runQuery(paste0(
        "CREATE ROLE ",
        dbQuoteIdentifier(private$conn, dbAppId), " LOGIN PASSWORD ",
        dbQuoteString(private$conn, appDbCredentials$password), ";"
      ),
      mask = appDbCredentials$password
      )
    }
    private$runQuery(paste0(
      "CREATE SCHEMA IF NOT EXISTS AUTHORIZATION ",
      dbQuoteIdentifier(private$conn, dbAppId), ";"
    ))
    # Make sure search path is set to user's schema only to exclude public tables
    private$runQuery(paste0(
      "ALTER ROLE ",
      dbQuoteIdentifier(private$conn, dbAppId),
      " SET search_path = ",
      dbQuoteIdentifier(private$conn, dbAppId)
    ))
    return(appDbCredentials)
  },
  removeAppSchema = function(appId) {
    private$runQuery(paste0(
      "DROP SCHEMA IF EXISTS ",
      dbQuoteIdentifier(private$conn, private$getDbAppId(appId)), " CASCADE;"
    ))
    return(invisible(self))
  },
  finalize = function() {
    flog.debug("Db: Database connection ended as Db object was gced.")
    dbDisconnect(private$conn)
  }
), private = list(
  conn = NULL,
  runQuery = function(query, mask = NULL) {
    if (is.null(mask)) {
      queryToLog <- query
    } else {
      queryToLog <- gsub(mask, "xxx", query, fixed = TRUE)
    }
    flog.trace("Running query: '%s'", query)
    return(dbExecute(private$conn, SQL(query)))
  },
  getDbAppId = function(appId) {
    return(paste0("M_", toupper(appId)))
  },
  genPassword = function() {
    passwordTmp <- system("tr -dc A-Za-z0-9 </dev/urandom | head -c 60 ; echo ''",
      intern = TRUE
    )
    if (!identical(nchar(passwordTmp), 60L)) {
      stop("Issues generating database password", call. = FALSE)
    }
    return(passwordTmp)
  }
))
