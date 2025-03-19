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
    private$setRolePrefix()
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
      private$runQuery(
        paste0(
          "ALTER USER ",
          dbQuoteIdentifier(private$conn, dbAppId),
          " WITH PASSWORD ",
          dbQuoteString(private$conn, appDbCredentials$password), ";"
        ),
        mask = appDbCredentials$password
      )
    } else {
      private$runQuery(
        paste0(
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
  rolePrefix = NULL,
  runQuery = function(query, mask = NULL, get = FALSE) {
    if (is.null(mask)) {
      queryToLog <- query
    } else {
      queryToLog <- gsub(mask, "xxx", query, fixed = TRUE)
    }
    flog.trace("Running query: '%s'", queryToLog)
    if (get) {
      return(dbGetQuery(private$conn, SQL(query)))
    }
    return(dbExecute(private$conn, SQL(query)))
  },
  getDbAppId = function(appId) {
    return(paste0(private$rolePrefix, toupper(appId)))
  },
  genPassword = function() {
    passwordTmp <- system("tr -dc A-Za-z0-9 </dev/urandom | head -c 60 ; echo ''",
      intern = TRUE
    )
    if (!identical(nchar(passwordTmp), 60L)) {
      stop("Issues generating database password", call. = FALSE)
    }
    return(passwordTmp)
  },
  setRolePrefix = function() {
    if (!IN_KUBERNETES) {
      # For backward compatibility reasons,
      # MIRO Server One does not support shared Postgres database
      private$rolePrefix <- "M_"
      return()
    }
    private$runQuery(paste0(
      "CREATE TABLE IF NOT EXISTS sys_config ",
      "(key TEXT PRIMARY KEY,",
      "value TEXT NOT NULL,",
      "verified BOOLEAN DEFAULT TRUE,",
      "updated TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP);"
    ))
    rolePrefix <- private$runQuery("SELECT * FROM sys_config WHERE key='role_prefix'", get = TRUE)
    if (nrow(rolePrefix) == 0L) {
      private$rolePrefix <- private$generateRolePrefix()
      flog.info("MiroDb: Created new role prefix: %s", private$rolePrefix)
    } else if (isTRUE(rolePrefix[["verified"]])) {
      private$rolePrefix <- rolePrefix[["value"]]
      flog.info("MiroDb: Got role prefix: %s", private$rolePrefix)
    } else {
      if (as.POSIXct(rolePrefix[["updated"]], tz = "UTC") < (Sys.time() - 60)) {
        flog.warn("Attempt to update the role failed. Will try again as more than a minute passed since last attempt.")
        private$rolePrefix <- private$generateRolePrefix(update = TRUE)
        flog.info(
          "MiroDb: Updated role prefix due to previously crashed process to: %s",
          private$rolePrefix
        )
      } else {
        flog.error("Another process is currently trying to update 'role_prefix' (race condition). Shutting down (try again later)...")
        stop("Race condition error (MiroDb.setRolePrefix)", call. = FALSE)
      }
    }
  },
  generateRolePrefix = function(update = FALSE, attempt = 1L) {
    repeat {
      newRolePrefix <- paste0(
        stringi::stri_rand_strings(1L, 1L, pattern = "[A-Za-z]")[[1L]],
        stringi::stri_rand_strings(1L, 2L, pattern = "[A-Za-z0-9_]")[[1L]]
      )
      if (!newRolePrefix %in% c("GMS", "pg_")) break
    }
    tryCatch(
      {
        if (update) {
          private$runQuery(
            sprintf(
              "UPDATE sys_config SET value = %s, verified = FALSE WHERE key='role_prefix;",
              dbQuoteString(private$conn, newRolePrefix)
            )
          )
        } else {
          private$runQuery(
            sprintf(
              "INSERT INTO sys_config (key, value, verified) VALUES ('role_prefix',%s,FALSE);",
              dbQuoteString(private$conn, newRolePrefix)
            )
          )
        }
      },
      error = function(err) {
        if (grepl("duplicate key value violates unique constraint", conditionMessage(err), fixed = TRUE)) {
          flog.error(
            "While trying to generate role prefix (update: %d:) another process inserted into the same table (race condition). Shutting down (try again later)...",
            update
          )
        } else {
          flog.error(
            "Unexpected error while trying to set: 'role_prefix' (update: %d). Error message: %s",
            update, conditionMessage(err)
          )
        }
        stop(err)
      }
    )
    tryCatch(
      {
        private$runQuery(sprintf(
          "CREATE ROLE %s;", dbQuoteIdentifier(private$conn, newRolePrefix)
        ))
        private$runQuery("UPDATE sys_config SET verified = TRUE WHERE key='role_prefix';")
      },
      error = function(err) {
        if (grepl("already exists", conditionMessage(err), fixed = TRUE)) {
          if (attempt > MAX_ROLE_PREFIX_CREATE_ATTEMPTS) {
            flog.error(
              "Generated a role prefix (%s) that is already in use. Maximum attempts exceeded (attempt: %d). Shutting down..",
              newRolePrefix, attempt
            )
            stop(err)
          }
          flog.info(
            "Generated a role prefix (%s) that is already in use. Trying again (attempt: %d)",
            newRolePrefix, attempt
          )
          return(private$generateRolePrefix(update = TRUE, attempt = attempt + 1L))
        } else {
          flog.error(
            "Unexpected error while trying to create role for role prefix: %s. Error message: %s",
            newRolePrefix, conditionMessage(err)
          )
          stop(err)
        }
      }
    )
    return(newRolePrefix)
  }
))
