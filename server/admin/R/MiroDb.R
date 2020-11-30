MiroDb <- R6::R6Class("MiroDb", public = list(
  initialize = function(dbConnectionInfo){
    private$conn <- DBI::dbConnect(drv = RPostgres::Postgres(),
        dbname = dbConnectionInfo$name,
        host = dbConnectionInfo$host,
        port = dbConnectionInfo$port, 
        user = dbConnectionInfo$username,
        password = dbConnectionInfo$password,
        bigint = "integer")
    flog.debug("Db: MiroDb initialized.")
    return(invisible(self))
  },
  getOrphans = function(appIds){
    dbTables <- private$getAllTables();
    return(dbTables[any(vapply(dbTables, private$tableBelongsToApp,
        logical(length(appIds)), appIds, USE.NAMES = FALSE))])
  },
  removeAppDbTables = function(appId){
    dbTables <- private$getAllTables()
    tablesToRemove <- dbTables[vapply(dbTables, private$tableBelongsToApp,
        logical(1), appId, USE.NAMES = FALSE)]
    return(self$removeTables(tablesToRemove))
  },
  removeTables = function(tablesToRemove){
    if(!length(tablesToRemove)){
        return(invisible(self))
    }
    query <- paste0("DROP TABLE IF EXISTS ",
        paste(DBI::dbQuoteIdentifier(private$conn, tablesToRemove),
            collapse = ", "), " CASCADE;")
    DBI::dbExecute(private$conn, query)
    flog.info("Db: Database tables: '%s' deleted.", paste(tablesToRemove, collapse = "', '"))
    return(invisible(self))
  },
  appTablesExist = function(appId){
    dbTables <- private$getAllTables();
    return(any(vapply(dbTables, private$tableBelongsToApp,
        logical(1), appId, USE.NAMES = FALSE)))
  },
  finalize = function(){
    flog.debug("Db: Database connection ended as Db object was gced.")
    DBI::dbDisconnect(private$conn)
  }), private = list(
    conn = NULL,
    getAllTables = function(){
        query <- DBI::SQL(paste0("SELECT table_name FROM information_schema.tables",
            " WHERE table_schema='public' AND table_type='BASE TABLE';"))
        return(DBI::dbGetQuery(private$conn, query)[[1L]])
    },
    tableBelongsToApp = function(tableName, appIds){
        return(startsWith(tableName, paste0(escapeAppIds(appIds), "_")) | 
            (startsWith(tableName, "_sys_") & endsWith(tableName, paste0("_", tolower(appIds)))))
    }
  )
)
