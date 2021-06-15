DbSchema <- R6Class("DbSchema", public = list(
  initialize = function(symbolSchema = NULL){
    private$dbSymbols <- c(names(ioConfig$modelOut), ioConfig$inputDsNames)
    private$dbTableNames <- names(symbolSchema$schema)
    private$dbViews <- symbolSchema$views
    
    private$schema <- c(list(
      '_scenMeta' = list(tabName = "_sys_metadata_",
                         colNames = c(sid = "_sid",
                                      uid = "_uid",
                                      sname = "_sname",
                                      stime = "_stime",
                                      stag = "_stag",
                                      accessR = "_accessr",
                                      accessW = "_accessw",
                                      accessX = "_accessx", 
                                      scode = "_scode"),
                         colTypes = "iccTcccci"
      ),
      '_scenLock' = list(tabName = "_sys_scenlocks_",
                         colNames = c(uid = "_uid",
                                      sid = "_sid",
                                      lock = "_slocktime"),
                         colTypes = "ciT"
      ),
      '_scenTrc' = list(tabName = "_sys_trace_",
                        colNames = c("_sid", TRACE_COL_NAMES),
                        colTypes = "icccccdidddddiiiddddddc"
      ),
      '_scenAttach' = list(tabName = "_sys_attach_",
                           colNames = c(sid = "_sid",
                                        fn = "fileName",
                                        fExt = "fileExt", 
                                        execPerm = "execPerm",
                                        content = "fileContent",
                                        time = "timestamp"),
                           colTypes = "icclbT"
      ),
      '_scenViews' = list(tabName = "_sys_views_",
                          colNames = c(sid = "_sid",
                                       symname = "symName",
                                       id = "id",
                                       data = "data",
                                       time = "timestamp"),
                          colTypes = "icccT"
      ),
      '_scenScripts' = list(tabName = "_sys_scripts_",
                            colNames = c(sid = "_sid",
                                         id = "id",
                                         content = "scriptContent"),
                            colTypes = "icc"
      ),
      '_jobMeta' = list(tabName = "_sys_jobs_",
                        colNames = c(jid = "_jid",
                                     uid = "_uid",  
                                     status = "_status",
                                     time = "_jtime", 
                                     tag = "_stag",
                                     pid = "_pid", 
                                     sid = "_sid",
                                     gamsret = "_gamsret",
                                     scode = "_scode",
                                     sname = "_sname"),
                        colTypes = "iciTcciiic"
      ),
      '_dataHash' = list(tabName = "_sys__data_hashes",
                         colNames = c(fn = "filename",
                                      hash = "hash"),
                         colTypes = "cc"
      ),
      '_scenHash' = list(tabName = "_sys_shash_",
                         colNames = c(sid = "_sid",
                                      hash = "hash"),
                         colTypes = "ic"),
      '_hcScalars' = list(tabName = "_sys__hc_scalars",
                          colNames = c(sid = "_sid",
                                       `_hash` = "_hash",
                                       scalar = "scalar",
                                       value = "value"),
                          colTypes = "iccc")
    ), symbolSchema$schema)
    return(invisible(self))
  },
  getTableNamesCurrentSchema = function(){
    return(vapply(private$schema, "[[", character(1L), "tabName", USE.NAMES = FALSE))
  },
  setConn = function(conn){
    private$conn <- conn
    return(invisible(self))
  },
  getDbTableName = function(tableName){
    if(tableName %in% c(scalarsFileName, scalarsOutName)){
      return(tableName)
    }
    if(LAUNCHHCUBEMODE){
      if(tableName %in% ioConfig$hcubeScalars){
        return(paste0("_hc_", tableName))
      }
      if(identical(tableName, "_hc__scalars")){
        return("_hc__scalars")
      }
    }
    return(private$schema[[tableName]]$tabName)
  },
  getDbIndexName = function(tableName){
    return(paste0("sid_index_", self$getDbTableName(tableName)))
  },
  getDbSchema = function(tableName){
    return(private$schema[[tableName]])
  },
  getDbTableNames = function(){
    return(private$dbTableNames)
  },
  getDbViews = function(viewName = NULL){
    if(is.null(viewName)){
      return(private$dbViews)
    }
    return(private$dbViews[[viewName]])
  },
  getAllSymbols = function(){
    return(private$dbSymbols)
  },
  getSymIdx = function(symName){
    return(match(symName, private$dbSymbols))
  },
  getCreateTableQuery = function(tableName){
    if(identical(tableName, "_scenMeta")){
      return(private$getCreateScenMetaTableQuery())
    }else if(identical(tableName, "_jobMeta")){
      return(private$getCreateJobMetaTableQuery())
    }else if(identical(tableName, "_scenLock")){
      return(private$getCreateScenlocksTableQuery())
    }else if(identical(tableName, "_scenAttach")){
      return(self$getCreateTableQueryRaw(tableName, includeForeignKey = FALSE))
    }else if(identical(tableName, "_hc__scalars")){
      return(private$getCreateHcScalarsTableQuery())
    }
    return(self$getCreateTableQueryRaw(tableName))
  },
  getCreateTableQueryRaw = function(symName, includeForeignKey = TRUE,
                                    dbTableName = NULL, symSchema = NULL){
    if(is.null(symSchema)){
      symSchema <- private$schema[[symName]]
    }
    if(is.null(dbTableName)){
      dbTableName <- self$getDbTableName(symName)
    }
    if(includeForeignKey){
      foreignKeyConstraint <- paste0(", CONSTRAINT foreign_key FOREIGN KEY (", 
                                     dbQuoteIdentifier(private$conn, "_sid"), 
                                     ") REFERENCES ",
                                     dbQuoteIdentifier(private$conn,
                                                       self$getDbTableName("_scenMeta")), 
                                     "(",
                                     dbQuoteIdentifier(private$conn, "_sid"), 
                                     ") ON DELETE CASCADE")
    }else{
      foreignKeyConstraint <- ""
    }
    if(symName %in% private$dbTableNames){
      # need to prepend sid column
      symSchema$colNames <- c("_sid", symSchema$colNames)
      symSchema$colTypes <- paste0("i", symSchema$colTypes)
    }
    return(paste0("CREATE TABLE ", 
                  dbQuoteIdentifier(private$conn, dbTableName), 
                  " (", paste(dbQuoteIdentifier(private$conn, symSchema$colNames), 
                              self$getColTypesSQL(symSchema$colTypes), collapse = ", "),
                  foreignKeyConstraint, ");"))
  },
  getCreateIndexQuery = function(tableName){
    return(self$getCreateIndexQueryRaw(self$getDbTableName(tableName)))
  },
  getCreateIndexQueryRaw = function(dbTableName){
    return(paste0("CREATE INDEX ",
                  dbQuoteIdentifier(private$conn, paste0("sid_index_", dbTableName)),
                  " ON ",
                  dbQuoteIdentifier(private$conn, dbTableName),
                  " (",
                  dbQuoteIdentifier(private$conn, "_sid"),
                  ");"))
  },
  getColTypesSQL = function(colTypes){
    return(vapply(strsplit(colTypes, "", fixed = TRUE)[[1]], function(colType){
      if(colType %in% c("i", "integer")){
        return("INTEGER")
      }
      if(colType %in% c("d", "numeric")){
        return("DOUBLE PRECISION")
      }
      if(colType %in% c("c", "character")){
        return("TEXT")
      }
      if(colType %in% c("b", "blob")){
        if(inherits(private$conn, "PqConnection")){
          return("BYTEA")
        }
        return("BLOB")
      }
      if(identical(colType, "T")){
        if(inherits(private$conn, "PqConnection")){
          return("TIMESTAMPTZ")
        }
        return("TEXT")
      }
      if(identical(colType, "l")){
        if(inherits(private$conn, "PqConnection")){
          return("BOOLEAN")
        }
        return("INTEGER")
      }
      stop_custom("error_config", sprintf("Invalid migration config: Invalid column type: %s",
                                          colType),
                  call. = FALSE)
    }, character(1L), USE.NAMES = FALSE))
  },
  getCreateScalarViewQuery = function(viewName, scalarNames){
    escapedScalarNames <- dbQuoteIdentifier(private$conn, scalarNames)
    escapedTableNameMeta <- dbQuoteIdentifier(private$conn,  private$schema[["_scenMeta"]]$tabName)
    return(paste0("CREATE VIEW ", dbQuoteIdentifier(private$conn, viewName), " AS SELECT ",
                  escapedTableNameMeta, "._sid,",
                  paste(escapedScalarNames, collapse = ","), " FROM ",
                  escapedTableNameMeta, " ",
                  paste(paste0("LEFT JOIN ", escapedScalarNames, " ON ",
                               escapedTableNameMeta, "._sid=", escapedScalarNames, "._sid"),
                        collapse = " ")))
  },
  getCreateScalarViewTriggerFnQuery = function(viewName, scalarNames){
    return(paste0("CREATE OR REPLACE FUNCTION ",
                  dbQuoteIdentifier(private$conn, paste0(viewName, "_insert_fn")),
                  "() RETURNS TRIGGER AS $$ BEGIN ",
                  private$getCreateScalarViewInsertQueries(
                    dbQuoteIdentifier(private$conn, scalarNames)),
                  " RETURN NULL; END; $$ LANGUAGE plpgsql;"))
  },
  getDropScalarTriggerQuery = function(viewName){
    if(inherits(private$conn, "PqConnection")){
      return(paste0("DROP FUNCTION IF EXISTS ",
                    dbQuoteIdentifier(private$conn, paste0(viewName, "_insert_fn")),
                    " CASCADE;"))
    }
    return(paste0("DROP TRIGGER IF EXISTS ",
                  dbQuoteIdentifier(private$conn, paste0(viewName, "_insert"))))
  },
  getCreateScalarViewTriggerQuery = function(viewName, scalarNames){
    escapedScalarNames <- dbQuoteIdentifier(private$conn, scalarNames)
    if(inherits(private$conn, "PqConnection")){
      triggerFn <- paste0(" FOR EACH ROW EXECUTE PROCEDURE ",
                          dbQuoteIdentifier(private$conn, paste0(viewName, "_insert_fn")), "()")
    }else{
      triggerFn <- paste0(" BEGIN ", private$getCreateScalarViewInsertQueries(escapedScalarNames), " END")
    }
    return(paste0("CREATE TRIGGER ", dbQuoteIdentifier(private$conn, paste0(viewName, "_insert")),
                  " INSTEAD OF INSERT ON ", dbQuoteIdentifier(private$conn, viewName),
                  triggerFn))
  }
), private = list(
  schema = NULL,
  dbViews = NULL,
  dbTableNames = NULL,
  dbSymbols = NULL,
  conn = NULL,
  getCreateScenMetaTableQuery = function(){
    schema <- private$schema[["_scenMeta"]]
    
    return(paste0("CREATE TABLE ",
                  dbQuoteIdentifier(private$conn, schema$tabName),
                  " (",
                  dbQuoteIdentifier(private$conn, schema$colNames[['sid']]),
                  if(inherits(private$conn, "PqConnection"))
                    " serial PRIMARY KEY," else " integer PRIMARY KEY,",
                  dbQuoteIdentifier(private$conn, schema$colNames[['uid']]),
                  " varchar(50) NOT NULL,",
                  dbQuoteIdentifier(private$conn, schema$colNames[['sname']]),
                  " varchar(255) NOT NULL,",
                  dbQuoteIdentifier(private$conn, schema$colNames[['stime']]),
                  if(inherits(private$conn, "PqConnection"))
                    " timestamp with time zone," else " text,",
                  dbQuoteIdentifier(private$conn, schema$colNames[['stag']]),
                  " text,",
                  dbQuoteIdentifier(private$conn, schema$colNames[['accessR']]),
                  " text NOT NULL,",
                  dbQuoteIdentifier(private$conn, schema$colNames[['accessW']]),
                  " text NOT NULL,",
                  dbQuoteIdentifier(private$conn, schema$colNames[['accessX']]),
                  " text NOT NULL,",
                  dbQuoteIdentifier(private$conn, schema$colNames[['scode']]),
                  " integer);"))
  },
  getCreateJobMetaTableQuery = function(){
    schema <- private$schema[["_jobMeta"]]
    
    return(paste0("CREATE TABLE ", 
                  dbQuoteIdentifier(private$conn, schema$tabName), 
                  " (", 
                  dbQuoteIdentifier(private$conn, schema$colNames[["jid"]]), 
                  if(inherits(private$conn, "PqConnection")) 
                    " serial PRIMARY KEY," else " integer PRIMARY KEY,",
                  dbQuoteIdentifier(private$conn, schema$colNames[["uid"]]), 
                  " varchar(50) NOT NULL,",
                  dbQuoteIdentifier(private$conn, schema$colNames[["status"]]), 
                  " integer,", 
                  dbQuoteIdentifier(private$conn, schema$colNames[["time"]]), 
                  if(inherits(private$conn, "PqConnection")) 
                    " timestamp with time zone," else " text,",
                  dbQuoteIdentifier(private$conn, schema$colNames[["tag"]]), 
                  " text,",
                  dbQuoteIdentifier(private$conn, schema$colNames[["pid"]]), 
                  " varchar(255) NOT NULL,",
                  dbQuoteIdentifier(private$conn, schema$colNames[["sid"]]),
                  " integer,",
                  dbQuoteIdentifier(private$conn, schema$colNames[["gamsret"]]),
                  if(inherits(private$conn, "PqConnection")) 
                    " smallint," else " integer,",
                  dbQuoteIdentifier(private$conn, schema$colNames[["scode"]]),
                  " integer,",
                  dbQuoteIdentifier(private$conn, schema$colNames[["sname"]]),
                  " varchar(255));"))
  },
  getCreateScenlocksTableQuery = function(){
    schema <- private$schema[["_scenLock"]]
    
    return(paste0("CREATE TABLE ",
                  dbQuoteIdentifier(private$conn, schema$tabName),
                  " (",
                  dbQuoteIdentifier(private$conn, schema$colNames[["uid"]]),
                  " text,",
                  dbQuoteIdentifier(private$conn, schema$colNames[["sid"]]),
                  " int UNIQUE,",
                  dbQuoteIdentifier(private$conn, schema$colNames[["lock"]]),
                  if(inherits(private$conn, "PqConnection"))
                    " timestamp with time zone);" else " text);"))
  },
  getCreateScalarViewInsertQueries = function(escapedScalarNames){
    return(paste(paste0("INSERT INTO",
                        escapedScalarNames,
                        "(_sid,",
                        escapedScalarNames,
                        ") VALUES (NEW._sid,NEW.",
                        escapedScalarNames, ");"), collapse = " "))
  },
  getCreateHcScalarsTableQuery = function(){
    return(self$getCreateTableQueryRaw("_hc__scalars", dbTableName = "_hc__scalars",
                                       symSchema = list(tabName = "_hc__scalars",
                                                        colNames = c("_sid", "scalar",
                                                                     "description", "value"),
                                                        colTypes = "iccc")))
  }
))