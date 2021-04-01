DbSchema <- R6Class("DbSchema", public = list(
  initialize = function(symbolSchema = NULL){
    private$dbSymbols <- c(names(ioConfig$modelOut), ioConfig$inputDsNames)
    stopifnot(identical(length(symbolSchema), length(private$dbSymbols)))
    
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
                        colNames = c("_sid", traceColNames),
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
      )
    ), symbolSchema)
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
    return(private$schema[[tableName]]$tabName)
  },
  getDbIndexName = function(tableName){
    return(paste0("sid_index_", private$schema[[tableName]]$tabName))
  },
  getDbSchema = function(tableName){
    return(private$schema[[tableName]])
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
    }
    return(self$getCreateTableQueryRaw(tableName))
  },
  getCreateTableQueryRaw = function(symName, includeForeignKey = TRUE, dbTableName = NULL){
    symSchema <- private$schema[[symName]]
    if(is.null(dbTableName)){
      dbTableName <- symSchema$tabName
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
    if(symName %in% private$dbSymbols){
      # need to prepend sid column
      symSchema$colNames <- c("_sid", symSchema$colNames)
      symSchema$colTypes <- paste0("i", symSchema$colTypes)
    }
    return(paste0("CREATE TABLE ", 
                  dbQuoteIdentifier(private$conn, dbTableName), 
                  " (", paste(dbQuoteIdentifier(private$conn, symSchema$colNames), 
                              private$getColTypesSQL(symSchema$colTypes), collapse = ", "),
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
  }
), private = list(
  schema = NULL,
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
                  if(inherits(private$conn, "PqConnection")) " smallint);" else " integer);"))
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
                  if(inherits(private$conn, "PqConnection")) 
                    " smallint," else " integer,",
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
  }
))