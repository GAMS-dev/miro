# R6 class for database related functions
Db <- R6Class("Db",
              public = list(
                initialize        = function(uid, dbConf, dbSchema, slocktimeLimit, modelName,
                                             traceColNames = NULL, attachmentConfig = NULL, 
                                             hcubeActive = FALSE){
                  # Initialize database class
                  #
                  # Args:
                  #   dbConf:              list with database connection configuration
                  #                        includes: type, host, port(optional), username, 
                  #                        password and name elements
                  #   dbSchema:            database schema
                  #   modelName:           name of the current model
                  #   slocktimeLimit:      maximum duration a lock is allowed to persist 
                  #   attachmentConfig:    attachment module configuration
                  #   hcubeActive:         boolean that specifies whether Hypercube mode is currently active
                  
                  #BEGIN error checks 
                  if(is.null(private$info$isInitialized)){
                    private$info$isInitialized <- 1L
                  }else{
                    flog.error("Db: Tried to create more than one Db object.")
                    stop("An Object of class Db has already been initialized. Only one Db object allowed.", 
                         call. = FALSE)
                  }
                  stopifnot(is.character(uid), length(uid) == 1L)
                  if(!identical(dbConf$type, "sqlite")){
                    dbConf$type <- "postgres"
                    stopifnot(is.character(dbConf$host), length(dbConf$host) == 1L)
                    stopifnot(is.character(dbConf$username), length(dbConf$username) == 1L)
                    stopifnot(is.character(dbConf$password), length(dbConf$password) == 1L)
                  }
                  stopifnot(is.character(dbConf$name), length(dbConf$name) == 1L)
                  stopifnot(is.numeric(slocktimeLimit), length(slocktimeLimit) >= 1L)
                  if(!is.null(dbConf$port)){
                    stopifnot(is.numeric(dbConf$port) && length(dbConf$port) == 1L)
                  }
                  stopifnot(is.character(dbConf$type), length(dbConf$type) == 1L)
                  stopifnot(is.list(dbSchema), !is.null(dbSchema$tabName), !is.null(dbSchema$colNames),
                            !is.null(dbSchema$colTypes))
                  if(!is.null(attachmentConfig)){
                    stopifnot(is.list(attachmentConfig), length(attachmentConfig) >= 1L)
                  }
                  stopifnot(is.character(modelName), length(modelName) == 1L)
                  stopifnot(is.logical(hcubeActive), length(hcubeActive) == 1L)
                  #END error checks 
                  
                  private$uid                         <- uid
                  private$userAccessGroups            <- uid
                  private$modelName                   <- modelName
                  private$dbSchema                    <- dbSchema
                  private$scenMetaColnames            <- dbSchema$colNames[['_scenMeta']]
                  private$slocktimeIdentifier         <- dbSchema$colNames[['_scenLock']][['lock']]
                  private$tableNameMetadata           <- dbSchema$tabName[['_scenMeta']]
                  private$tableNameMetaHcube          <- dbSchema$tabName[['_hcubeMeta']]
                  private$tableNameScenLocks          <- dbSchema$tabName[['_scenLock']]
                  private$tableNamesScenario          <- dbSchema$tabName[!startsWith(dbSchema$tabName, "_")]
                  private$slocktimeLimit              <- slocktimeLimit
                  private$attachmentConfig            <- attachmentConfig
                  private$hcubeActive                 <- hcubeActive
                  
                  if(identical(dbConf$type, "postgres")){
                    tryCatch({
                      private$conn <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbConf$name, 
                                                     host = dbConf$host, port = dbConf$port, 
                                                     user = dbConf$username, password = dbConf$password)
                    }, error = function(e){
                      stop(sprintf("Db: Database connection could not be established. Error message: %s", e), 
                           call. = FALSE)
                    })
                  }else if(identical(dbConf$type, "sqlite")){
                    tryCatch({
                      private$conn <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = dbConf$name)
                      # turn foreign key usage on
                      dbExecute(private$conn, "PRAGMA foreign_keys = ON;")
                    }, error = function(e){
                      stop(sprintf("Db: Database connection could not be established. Error message: %s", e), 
                           call. = FALSE)
                    })
                  }else{
                    stop(sprintf("Db: A non supported database type (%s) was specified. Could not establish connection.", 
                                 dbConf$type), call. = FALSE) 
                  }
                },
                getConn               = function() private$conn,
                getUid                = function() private$uid,
                getUserAccessGroups   = function() private$userAccessGroups,
                getDbSchema           = function() private$dbSchema,
                getScenMetaColnames   = function() private$scenMetaColnames,
                getSlocktimeIdentifier= function() private$slocktimeIdentifier,
                getTableNameMetadata  = function() private$tableNameMetadata,
                getTableNameMetaHcube = function() private$tableNameMetaHcube,
                getTableNameScenLocks = function() private$tableNameScenLocks,
                getTableNamesScenario = function() private$tableNamesScenario,
                getAttachmentConfig   = function() private$attachmentConfig,
                getHcubeActive        = function() private$hcubeActive,
                getOrphanedTables     = function(hcubeScalars = NULL){
                  # find orphaned database tables 
                  #
                  # Args:
                  #   hcubeScalars:        name of scalars that are transferred to 
                  #                        tables in Hypercube mode
                  #
                  # Returns:
                  #   list with names of orphaned database tables
                  
                  if(inherits(private$conn, "PqConnection")){
                    query <- SQL(paste0("SELECT table_name FROM information_schema.tables", 
                                        " WHERE table_schema='public' AND table_type='BASE TABLE'", 
                                        " AND table_name LIKE ", 
                                        dbQuoteString(private$conn, private$modelName %+% "\\_%"), ";"))
                  }else{
                    query <- SQL(paste0("SELECT name FROM sqlite_master WHERE type = 'table'",
                                        " AND name LIKE ", 
                                        dbQuoteString(private$conn, private$modelName %+% "\\_%"), " ESCAPE '\\';"))
                  }
                  
                  tryCatch({
                    dbTables <- dbGetQuery(private$conn, query)[[1L]]
                  }, error = function(e){
                    stop(sprintf("Db: An error occurred while fetching table names from database (Db.getOrphanTables). Error message: '%s'.",
                                 e), call. = FALSE)
                  })
                  orphanedTables <- dbTables[!dbTables %in% private$tableNamesScenario]
                  if(!is.null(hcubeScalars)){
                    hcubeScalarsIdx <-  orphanedTables %in% paste0(private$modelName, "_", 
                                                                   hcubeScalars)
                    orphanedTables <- orphanedTables[!hcubeScalarsIdx]
                  }
                  return(orphanedTables)
                },
                getInconsistentTables = function(strictMode = TRUE){
                  errMsg  <- NULL
                  colNames <- private$dbSchema$colNames
                  colTypes <- private$dbSchema$colTypes
                  headers <- colNames
                  numericTypes <- c("float", "real", 
                                    "numeric", "double",
                                    "double precision",
                                    "int", "integer",
                                    "smallint", "bigint")
                  
                  badTables <- vapply(private$tableNamesScenario, function(tabName){
                    tabNameRaw  <- tolower(gsub("^[^_]+_", "", tabName))
                    confHeaders <- colNames[[tabNameRaw]]
                    if(!is.null(confHeaders) && dbExistsTable(private$conn, tabName)){
                      tryCatch({
                        if(inherits(private$conn, "PqConnection")){
                          query <- SQL(paste0("SELECT column_name,data_type  FROM information_schema.columns 
                                            WHERE table_name = ", 
                                              dbQuoteString(private$conn, tabName), ";"))
                          tabInfo     <- dbGetQuery(private$conn, query)
                          tabColNames <- tabInfo$column_name[-1L]
                          tabColTypes <- tabInfo$data_type[-1L]
                          
                        }else{
                          query <- SQL(paste0("PRAGMA table_info(", 
                                              dbQuoteIdentifier(private$conn, tabName), ");"))
                          tabInfo     <- dbGetQuery(private$conn, query)
                          tabColNames <- tabInfo$name[-1L]
                          tabColTypes <- tabInfo$type[-1L]
                        }
                      }, error = function(e){
                        stop(sprintf("Db: An error occurred while fetching table headers from database (Db.getInconsistentTables, table: '%s').\nError message: '%s'.",
                                     tabName, e), call. = FALSE)
                      })
                      errMsgTmp <- paste(errMsg, sprintf("Database table headers ('%s') are different from those in current configuration ('%s').\nPlease fix the database schema or change your GAMS model!\n",
                                                         paste(tabColNames, collapse = "', '"),
                                                         paste(confHeaders, collapse = "', '")))
                      if(!identical(length(tabColNames), length(confHeaders))){
                        errMsg <<- errMsgTmp
                        return(tabNameRaw)
                      }else if(any(is.na(match(confHeaders, tabColNames)))){
                        if(strictMode || any(vapply(seq_along(tabColTypes), function(i){
                          switch(tolower(substr(colTypes[tabNameRaw], i, i)),
                                 "d" = {
                                   if(tabColTypes[[i]] %in% numericTypes){
                                     return(FALSE)
                                   }else{
                                     return(TRUE)
                                   }
                                 },
                                 "c" = {
                                   if(tabColTypes[[i]] %in% numericTypes){
                                     return(TRUE)
                                   }else{
                                     return(FALSE)
                                   }
                                 },
                                 { 
                                   stop(sprintf("Unsupported column type detected: %s.", 
                                                substr(colTypes[tabNameRaw], i, i)), call. = FALSE)
                                }
                          )
                        }, logical(1L), USE.NAMES = FALSE))){
                          errMsg <<- errMsgTmp
                        }else{
                          warning(paste(errMsg, sprintf("Database table headers ('%s') are different from those in current configuration ('%s').\nConfiguration was adjusted accordingly. You might want to turn strict mode on in case you would like the execution to stop in such case instead of seeing this warning.",
                                                           paste(tabColNames, collapse = "', '"),
                                                           paste(confHeaders, collapse = "', '"))))
                          headers[[tabNameRaw]] <<- tabColNames
                        }
                        return(tabNameRaw)
                      }
                    }
                    return(NA_character_)
                  }, character(1L), USE.NAMES = FALSE)
                  badTables <- badTables[!is.na(badTables)]
                  return(list(names = badTables, headers = headers[names(headers) %in% badTables], errMsg = errMsg))
                },
                removeTablesModel     = function(){
                  tableNames <- private$getTableNamesModel()
                  # bring metadata table to front as others depend on it
                  if(inherits(private$conn, "PqConnection")){
                    query <- paste0("DROP TABLE IF EXISTS ",  
                                    paste(dbQuoteIdentifier(private$conn, tableNames),
                                          collapse = ", "), " CASCADE;")
                    dbExecute(private$conn, query)
                    flog.info("Database tables: '%s' deleted.", paste(tableNames, "', '"))
                    return(invisible(self))
                  }
                  # turn foreign key usage off
                  dbExecute(private$conn, "PRAGMA foreign_keys = OFF;")
                  for(tableName in tableNames){
                    query <- paste0("DROP TABLE IF EXISTS ",  
                                    dbQuoteIdentifier(private$conn, tableName), " ;")
                    dbExecute(private$conn, query)
                  }
                  # turn foreign key usage on again
                  dbExecute(private$conn, "PRAGMA foreign_keys = ON;")
                  return(invisible(self))
                },
                saveTablesModel       = function(tempDir){
                  stopifnot(is.character(tempDir), length(tempDir) == 1)
                  limit <- 1e7 + 1L
                  tableNames <- private$getTableNamesModel()
                  for(tableName in tableNames){
                    data <- self$importDataset(tableName, limit = limit)
                    if(length(nrow(data)) &&  nrow(data) > limit){
                      stop("maxRowException", call. = FALSE)
                    }
                    write_csv(data, file.path(tempDir, tableName %+% ".csv"), 
                              na = "", append = FALSE, col_names = TRUE,
                              quote_escape = "double")
                  }
                  return(invisible(self))
                },
                getMetadata           = function(sid, uid, sname, stime, stag = character(0L), 
                                                 readPerm = character(0L), writePerm = character(0L), 
                                                 uidAlias = private$scenMetaColnames[['uid']], 
                                                 snameAlias = private$scenMetaColnames[['sname']], 
                                                 stimeAlias = private$scenMetaColnames[['stime']],
                                                 stagAlias = private$scenMetaColnames[['stag']],
                                                 readPermAlias = private$scenMetaColnames[['accessR']],
                                                 writePermAlias = private$scenMetaColnames[['accessW']]){
                  # Generate dataframe containing scenario metadata
                  #
                  # Args:
                  #   sid:                      scenario ID
                  #   uid:                      user ID
                  #   sname:                    name of the scenario
                  #   stime:                    time the scenario was generated
                  #   stag:                     tags of the scenario (optional)
                  #   readPerm:                 read permissions of scenario (optional)
                  #   writePerm:                write permissions of scenario (optional)
                  #   uidAlias:                 User ID Description (optional)
                  #   snameAlias:               Scenario name Description (optional)
                  #   stimeAlias:               Scenario time description (optional)
                  #   stagAlias:                Scenario tag description (optional)
                  #   readPermAlias:            Scenario read permissions description (optional)
                  #   writePermAlias:           Scenario write permissions description (optional)
                  #
                  # Returns:
                  #   dataframe with metadata
                  
                  #BEGIN error checks
                  stopifnot(is.integer(sid), length(sid) == 1L)
                  stopifnot(is.character(uid), length(uid) == 1L)
                  stopifnot(is.character(sname), length(sname) == 1L)
                  stime <- as.POSIXct(stime)
                  stopifnot(inherits(stime, "POSIXct"), length(stime) == 1L)
                  stopifnot(is.character(stag))
                  stopifnot(is.character(readPerm))
                  stopifnot(is.character(writePerm))
                  stopifnot(is.character(uidAlias), length(uidAlias) == 1L)
                  stopifnot(is.character(snameAlias), length(snameAlias) == 1L)
                  stopifnot(is.character(stimeAlias), length(stimeAlias) == 1L)
                  stopifnot(is.character(stagAlias))
                  stopifnot(is.character(readPermAlias))
                  stopifnot(is.character(writePermAlias))
                  #END error checks
                  
                  metadata <- tibble(sid, uid, sname, stime)
                  names(metadata) <- c(private$scenMetaColnames["sid"], uidAlias, snameAlias, stimeAlias)
                  if(length(stag)){
                    if(length(stag) > 1L){
                      stag <- vector2Csv(stag)
                    }
                    metadata[[stagAlias]] <- stag
                  }
                  if(length(readPerm)){
                    if(length(readPerm) > 1L){
                      readPerm <- vector2Csv(readPerm)
                    }
                    metadata[[readPermAlias]] <- readPerm
                  }
                  if(length(writePerm)){
                    if(length(writePerm) > 1L){
                      writePerm <- vector2Csv(writePerm)
                    }
                    metadata[[writePermAlias]] <- writePerm
                  }
                  
                  return(metadata)
                },
                checkSnameExists      = function(sname, uid = NULL){
                  # test whether scenario with the given name already exists 
                  # for the user specified
                  # 
                  # Args:
                  #   sname:                scenario name
                  #   uid:                  user ID (optional)
                  #
                  # Returns: 
                  #   boolean:              TRUE if id exists, FALSE otherwise, 
                  #   throws exception in case of error
                  
                  #BEGIN error checks 
                  if(!is.null(uid)){
                    stopifnot(is.character(uid), length(uid) == 1)
                  }else{
                    uid <- private$uid
                  }
                  stopifnot(is.character(sname), length(sname) == 1)
                  #END error checks 
                  
                  #check whether table exists
                  if(!DBI::dbExistsTable(private$conn, private$tableNameMetadata)){
                    flog.debug("Db: Db.checkScenExists returns FALSE (SID does not yet exist " %+% 
                                 "as metadata table does not exist either)")
                    return(FALSE)
                  }
                  
                  # check whether scenario name already exists
                  scenExists <- self$importDataset(private$tableNameMetadata, tibble(
                    c(private$scenMetaColnames['uid'], private$scenMetaColnames['sname'],
                      private$scenMetaColnames['scode']), 
                    c(uid, sname, if(private$hcubeActive) 2L else 0L)), count = TRUE, limit = 1L)[[1]]
                  if(scenExists >= 1){
                    flog.trace("Db: Scenario with name: '%s' alreaddy exists for user: '%s' " %+%
"(Db.checkScenExists returns FALSE).", sname, uid)
                    return(TRUE)
                  }else{
                    flog.trace("Db: Scenario with name: '%s' does not yet exist for user: '%s' " %+% 
                                 "(Db.checkScenExists returns TRUE).", sname, uid)
                    return(FALSE)
                  }
                },
                getSid = function(sname, uid = NULL){
                  # fetch scenario ID for a given scenario name/ user ID 
                  # combination
                  #
                  # Args:
                  #    sname:           scenario name
                  #    uid:             user ID (optional)
                  #
                  # Returns:
                  #    integer: scenario ID for the given scenario name
                  #             or 0 in case scenario does not exist
                  
                  # BEGIN error checks
                  if(is.null(uid)){
                    uid <- private$uid
                  }else{
                    stopifnot(is.character(uid), length(uid) == 1)
                  }
                  stopifnot(is.character(sname), length(sname) == 1)
                  # END error checks
                  
                  metadataRow <- self$importDataset(private$tableNameMetadata, tibble(
                    c(private$scenMetaColnames['uid'], 
                      private$scenMetaColnames['sname']),
                    c(uid, sname)), limit = 1L)
                  if(nrow(metadataRow)){
                    return(as.integer(metadataRow[[private$scenMetaColnames['sid']]][1]))
                  }else{
                    return(0L)
                  }
                },
                getLatestSid          = function(){
                  # Fetch the last inserted sid from database 
                  #
                  # Args:
                  # 
                  # Returns:
                  # integer: latest scenario Id exported to database
                  
                  if(inherits(private$conn, "PqConnection")){
                    tryCatch({
                      query <- SQL(paste0("SELECT nextval(pg_get_serial_sequence(",
                                                        DBI::dbQuoteString(private$conn, private$tableNameMetadata), 
                                                        ", ", DBI::dbQuoteString(private$conn, private$scenMetaColnames['sid']), "));"))
                      nextVal <- DBI::dbGetQuery(private$conn, query)
                      return(as.integer(nextVal[[1L]][1]))
                    }, error = function(e){
                      return(0L)
                    })
                  }else{
                    tryCatch({
                      query <- SQL(paste0("SELECT MAX(",
                                          dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), ")  FROM ",
                                          dbQuoteIdentifier(private$conn, private$tableNameMetadata), ";"))
                      nextVal <- DBI::dbGetQuery(private$conn, query)
                      return(as.integer(nextVal[[1L]][1]))
                    }, error = function(e){
                      return(0L)
                    })
                  }
                },
                loadScenarios = function(sids, limit = 1e7, msgProgress){
                  # Load multiple scenarios from database
                  #
                  # Args:
                  #   sids:             scenario IDs to load 
                  #   limit:            maxmimum number of rows to fetch per dataset
                  #   msgProgress:      title and progress info for the progress bar
                  #
                  # Returns:
                  #   list of scenario datasets
                  
                  #BEGIN error checks 
                  sids <- suppressWarnings(as.integer(sids))
                  stopifnot(!any(is.na(sids)), length(sids) >= 1)
                  stopifnot(is.numeric(limit), length(limit) == 1)
                  stopifnot(is.character(msgProgress$title), length(msgProgress$title) == 1)
                  stopifnot(is.character(msgProgress$progress), length(msgProgress$progress) == 1)
                  #END error checks
                  
                  #initialize progress bar
                  prog <- Progress$new()
                  on.exit(prog$close())
                  prog$set(message = msgProgress$title, value = 0)
                  incAmount <- 1/(length(sids) * length(private$tableNamesScenario))
                  updateProgress <- function(detail = NULL) {
                    prog$inc(amount = incAmount, detail = detail)
                  }
                  scenData <- lapply(seq_along(sids), function(i){
                    lapply(private$tableNamesScenario, function(tableName){
                      dataset <- self$importDataset(tableName = tableName, 
                                                    subsetSids = sids[i], limit = limit)
                      dataset[, private$scenMetaColnames['sid']] <- NULL
                      updateProgress(detail = msgProgress$progress %+% i)
                      return(dataset)
                    })
                  })
                  return(scenData)
                },
                deleteRows        = function(tableName, colNames = NULL, values = NULL, conditionSep = c("AND", "OR"), 
                                             subsetSids = NULL){
                  # remove rows from table where rows have given values
                  #
                  # Args:
                  #   tableName:        name of the table where entries should be removed from
                  #   colNames:         character vector of column names (optional)
                  #   values:           character vector of values that should be removed (optional)
                  #   conditionSep:     seperator used for concatenating subsetting conditions (AND or OR) (optional)
                  #   subsetSids:       vector of scenario IDs that query should be filtered on (optional)
                  #
                  # Returns:
                  #   integer: number of rows deleted
                  
                  #BEGIN error checks 
                  stopifnot(is.character(tableName), length(tableName) == 1)
                  if(!is.null(colNames)){
                    stopifnot(is.character(colNames), length(colNames) >= 1)
                    values <- as.character(values)
                    stopifnot(is.character(values), length(values) >= 1)
                  }else if(!is.null(subsetSids)){
                    subsetSids <- as.integer(subsetSids)
                    stopifnot(!any(is.na(subsetSids)))
                  }else{
                    stop("Can't delete entire table. Please specify subset.", call. = FALSE)
                  }
                  #END error checks 
                  
                  conditionSep <- match.arg(conditionSep)
                  
                  affectedRows <- 0
                  
                  subsetSidSQL <- NULL
                  
                  subsetRows <- NULL
                  if(!is.null(colNames)){
                    subsetRows <- paste(paste(DBI::dbQuoteIdentifier(private$conn, colNames), 
                                              DBI::dbQuoteLiteral(private$conn, values), sep = " = "), 
                                        collapse = paste0(" ", conditionSep, " "))
                  }
                  if(!is.null(subsetSids) && length(subsetSids) >= 1L){
                    subsetSidSQL <- paste0(DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), 
                                           " IN (", paste(DBI::dbQuoteLiteral(private$conn, subsetSids), collapse = ","), ") ")
                    if(length(subsetRows) && nchar(subsetRows)){
                      subsetRows <- DBI::SQL(paste(subsetRows, subsetSidSQL, sep = " AND "))
                    }else{
                      subsetRows <- DBI::SQL(subsetSidSQL)
                    }
                  }
                  if(!length(subsetRows) || nchar(subsetRows) < 1L){
                    flog.error("No condition provided for rows to delete on table: '%s'.", tableName)
                    stop(sprintf("No condition provided for rows to delete on table: '%s'.", tableName), 
                         call. = FALSE)
                  }
                  subsetWritePerm <- NULL
                  if(identical(tableName, private$tableNameMetadata)){
                    subsetWritePerm <- paste0(" AND (", 
                                              private$buildSQLSubsetString(
                                                private$getCsvSubsetClause(private$scenMetaColnames['accessW'],
                                                                           private$userAccessGroups), " OR "), ")")
                  }
                  if(DBI::dbExistsTable(private$conn, tableName)){
                    tryCatch({
                      query <- paste0("DELETE FROM ", DBI::dbQuoteIdentifier(private$conn, tableName),
                                      " WHERE ", subsetRows, subsetWritePerm)
                      affectedRows <- as.integer(DBI::dbExecute(private$conn, query))
                      flog.debug("Db: %s rows in table: '%s' were deleted. (Db.deleteRows)", affectedRows, tableName)
                    }, error = function(e){
                      stop(sprintf("Db: An error occurred while deleting rows from the database (Db.deleteRows, " %+% 
                                     "table: '%s'). Error message: %s.", tableName, e), call. = FALSE)
                    })
                  }
                  return(affectedRows)
                },
                updateRows = function(tableName, ..., colNames, values, innerSepAND = TRUE, subsetSids = NULL){
                  # Update records in database table based on subset conditions
                  #
                  # Args:
                  #   tableName:        name of the table where entries should be removed from
                  #   ...:              row subsetting: dataframes with 3 columns (column, value, operator)
                  #                     these will be handled as a block of AND conditions
                  #                     (if innerSep AND is TRUE) different blocks are concatenated with OR
                  #   colNames:         column names to update
                  #   values:           character vector of new values that colNames should be set to
                  #   innerSepAND:      boolean that specifies whether inner seperator in 
                  #                     substrings should be AND (TRUE) or OR (FALSE)  
                  #   subsetSids:       vector of scenario IDs that query should be filtered on
                  #
                  # Returns:
                  #   integer with number of affected rows
                  
                  dots <- list(...)
                  stopifnot(all(vapply(dots, private$isValidSubsetGroup, logical(1L), 
                                       USE.NAMES = FALSE)))
                  if(innerSepAND){
                    innerSep <- " AND "
                    outerSep <- " OR "
                  }else{
                    innerSep <- " OR "
                    outerSep <- " AND "
                  }
                  if(!is.null(subsetSids)){
                    subsetSids <- as.integer(subsetSids)
                    stopifnot(!any(is.na(subsetSids)))
                  }
                  stopifnot(is.character(tableName), length(tableName) == 1)
                  if(!is.null(colNames)){
                    stopifnot(is.character(colNames), length(colNames) >= 1)
                    stopifnot(length(values) >= 1)
                  }
                  
                  subsetRows <- self$buildRowSubsetSubquery(dots, innerSep, outerSep)
                  stopifnot(length(subsetRows) >= 0L)
                  
                  if(!is.null(subsetSids) && length(subsetSids) >= 1L){
                    subsetSidSQL <- paste0(DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), 
                                           " IN (", paste(DBI::dbQuoteLiteral(private$conn, subsetSids), 
                                                          collapse = ","), ") ")
                    subsetRows <- DBI::SQL(paste(subsetRows, subsetSidSQL, sep = " AND "))
                  }
                  subsetWritePerm <- NULL
                  if(identical(tableName, private$tableNameMetadata)){
                    subsetWritePerm <- paste0(" AND (", 
                                              private$buildSQLSubsetString(
                                                private$getCsvSubsetClause(private$scenMetaColnames['accessW'],
                                                                           private$userAccessGroups), " OR "), ")")
                  }
                  tryCatch({
                    query <- SQL(paste0("UPDATE ", DBI::dbQuoteIdentifier(private$conn, tableName), " SET ",
                                      paste(paste(DBI::dbQuoteIdentifier(private$conn, colNames), 
                                                  DBI::dbQuoteLiteral(private$conn, values), sep = " = "), 
                                            collapse = ", "), " WHERE ", subsetRows, subsetWritePerm, ";"))
                    affectedRows <- DBI::dbExecute(private$conn, query)
                    flog.debug("Db: %s rows in table: '%s' were updated (Db.updateRows)", affectedRows, tableName)
                  }, error = function(e){
                    stop(sprintf("Db: An error occurred while querying the database (Db.updateRows, " %+%
                                   "table: '%s'). Error message: %s.",
                                 tableName, e), call. = FALSE)
                  })
                  return(affectedRows)
                },
                importDataset = function(tableName, ..., colNames = NULL, count = FALSE, limit = 1e7, 
                                         innerSepAND = TRUE, distinct = FALSE, subsetSids = NULL){
                  # Import the data corresponding to the table name provided from the database by 
                  # considering scenario IDs specified.
                  #
                  # Args:
                  #   tableName :       name of the table to import dataframe from
                  #   ...:              row subsetting: dataframes with 3 columns (column, value, operator)
                  #                     these will be handled as a block of AND conditions
                  #                     (if innerSep AND is TRUE) different blocks are concatenated with OR
                  #   colNames:         column names to select, if NULL all will be selected (optional)
                  #   count :           boolean that specifies whether to merely count number 
                  #                     of rows or return actual values (optional)
                  #   limit:            maxmimum number of rows to fetch (optional)
                  #   innerSepAND:      boolean that specifies whether inner seperator in 
                  #                     substrings should be AND (TRUE) or OR (FALSE)  
                  #   distinct:         boolean that specifies whether to remove duplicate rows 
                  #                     (TRUE) or not (FALSE)
                  #   subsetSids:       vector of scenario IDs that query should be filtered on
                  #
                  # Returns:
                  #   tibble: dataset (cleaned of metadata columns) with data coming from the table selected.
                  #   In case table does not exist or no rows in dataset, returns NULL
                  
                  #BEGIN error checks 
                  dots <- list(...)
                  stopifnot(all(vapply(dots, private$isValidSubsetGroup, logical(1L), 
                                       USE.NAMES = FALSE)))
                  stopifnot(is.character(tableName), length(tableName) == 1)
                  if(!is.null(colNames)){
                    stopifnot(is.character(colNames), length(colNames) >= 1)
                  }
                  stopifnot(is.logical(count), length(count) == 1)
                  stopifnot(is.numeric(limit), length(limit) == 1)
                  stopifnot(is.logical(innerSepAND), length(innerSepAND) == 1)
                  stopifnot(is.logical(distinct), length(distinct) == 1)
                  if(innerSepAND){
                    innerSep <- " AND "
                    outerSep <- " OR "
                  }else{
                    innerSep <- " OR "
                    outerSep <- " AND "
                  }
                  if(!is.null(subsetSids)){
                    subsetSids <- as.integer(subsetSids)
                    stopifnot(!any(is.na(subsetSids)))
                  }
                  #END error checks 
                  
                  if(!dbExistsTable(private$conn, tableName)){
                    flog.debug("Db: A table named: '%s' does not exist in the database (Db.importDataset).", 
                               tableName)
                    return(tibble())
                  }else{
                    if(!is.null(colNames)){
                      colNames <- paste(DBI::dbQuoteIdentifier(private$conn, colNames),
                                        collapse = ",")
                    }else{
                      colNames <- "*"
                    }
                    if(distinct){
                      colNames <- "DISTINCT " %+% colNames
                    }
                    if(count){
                      colNames <- "COUNT(" %+% colNames %+% ")"
                    }
                    innerJoin <- NULL
                    subsetRows <- NULL
                    
                    if(length(dots)){
                      subsetRows <- self$buildRowSubsetSubquery(dots, innerSep, outerSep)
                    }
                    if(!is.null(subsetSids) && length(subsetSids) >= 1L){
                      if(inherits(private$conn, "PqConnection")){
                        # POSTGRES subsetting
                        innerJoin <- paste0(" INNER JOIN (VALUES ", paste("(" %+% subsetSids, collapse = "), "),
                          ")) vals(_v) ON ",  DBI::dbQuoteIdentifier(private$conn, 
                                                                        private$scenMetaColnames['sid']), "=_v")
                      }else{
                        subsetSidSQL <- paste0(DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), 
                                               " IN (", paste(DBI::dbQuoteLiteral(private$conn, subsetSids), 
                                                              collapse = ","), ") ")
                        if(length(subsetRows)){
                          subsetRows <- DBI::SQL(paste0(subsetRows, " AND ", subsetSidSQL))
                        }else{
                          subsetRows <- DBI::SQL(subsetSidSQL)
                        }
                      }
                    }
                    subsetReadPerm <- NULL
                    if(identical(tableName, private$tableNameMetadata)){
                      subsetReadPerm <- private$buildSQLSubsetString(
                        private$getCsvSubsetClause(private$scenMetaColnames['accessR'],
                                                   private$userAccessGroups), " OR ")
                      if(length(subsetRows)){
                        subsetRows <- paste0(subsetRows, " AND (", subsetReadPerm, ")")
                      }else{
                        subsetRows <- paste0(" (", subsetReadPerm, ")")
                      }
                    }
                    tryCatch({
                      sql     <- DBI::SQL(paste0("SELECT ", colNames, " FROM ", 
                                                 DBI::dbQuoteIdentifier(private$conn, tableName),
                                                 innerJoin, 
                                                 if(length(subsetRows)) " WHERE ", subsetRows,
                                                 " LIMIT ?lim ;"))
                      query   <- DBI::sqlInterpolate(private$conn, sql, lim = limit)
                      dataset <- as_tibble(DBI::dbGetQuery(private$conn, query))
                      dataset[['_v']] <- NULL
                      flog.debug("Db: Data was imported from table: '%s' (Db.importDataset).", tableName)
                    }, error = function(e){
                      stop(sprintf("Db: An error occurred while querying the database (Db.importDataset, " %+%
                                     "table: '%s'). Error message: %s.",
                                   tableName, e), call. = FALSE)
                    })
                  }
                  return(dataset)
                },
                exportScenDataset       = function(dataset, tableName, addForeignKey = TRUE){
                  # Saves scenario dataset to database
                  #
                  # Args:
                  #   dataset:             dataframe to save
                  #   tableName:           name of the table to export dataframe to
                  #   addForeignKey:       boolean that specifies whether a foreign key 
                  #                        should be added to table
                  #
                  # Returns:
                  #   Db object: invisibly returns reference to object in case of success, throws Exception if error
                  
                  #BEGIN error checks 
                  if(!hasContent(dataset)){
                    dataset <- NULL
                  }
                  stopifnot(inherits(dataset, "data.frame") || is.null(dataset))
                  stopifnot(is.character(tableName), length(tableName) == 1)
                  #END error checks 

                  dataset <- dateColToChar(private$conn, dataset)
                  if(DBI::dbExistsTable(private$conn, tableName)){
                    if(!is.null(dataset)){
                      tryCatch({
                        DBI::dbWriteTable(private$conn, tableName, dataset, row.names = FALSE, append = TRUE)
                        flog.debug("Db: Data was added to table: '%s' (Db.exportScenDataset).", 
                                   tableName)
                      }, error = function(e){
                        stop(sprintf("Db: An error occurred writing to database (Db.exportScenDataset, 
               table: '%s'). Error message: %s", tableName, e),
                             call. = FALSE)
                      })
                    }else{
                      flog.debug("Db: Nothing was written to table '%s' as no data was provided (Db.exportScenDataset).", 
                                 tableName)
                    }
                  }else if(!is.null(dataset)){
                    tryCatch({
                      fieldTypes <- private$getFieldTypes(dataset)
                      foreignKeyQuery <- ""
                      if(addForeignKey){
                        foreignKeyQuery <- paste0(", CONSTRAINT foreign_key FOREIGN KEY (", 
                                                  DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), 
                                                  ") REFERENCES ",
                                                  DBI::dbQuoteIdentifier(private$conn, private$tableNameMetadata), 
                                                  "(",
                                                  DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), 
                                                  ") ON DELETE CASCADE")
                      }
                      query <- paste0("CREATE TABLE ", 
                                      DBI::dbQuoteIdentifier(private$conn, tableName), 
                                      " (", paste(vapply(seq_along(dataset), function(i){
                                        paste(DBI::dbQuoteIdentifier(private$conn, names(dataset)[[i]]), 
                                              fieldTypes[[i]])
                                      }, character(1L), USE.NAMES = FALSE), collapse = ", "),
                                      foreignKeyQuery, ");")
                      DBI::dbExecute(private$conn, query)
                    }, error = function(e){
                      stop(sprintf("Table: '%s' could not be created (Db.exportScenDataset). " %+%
                                     "Error message: %s.", tableName, e), call. = FALSE)
                    })
                    if(addForeignKey){
                      tryCatch({
                        query <- paste0("CREATE INDEX ", DBI::dbQuoteIdentifier(private$conn, "sid_index_" %+% tableName), " ON ", 
                                        DBI::dbQuoteIdentifier(private$conn, tableName), 
                                        " (", DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), ");")
                        DBI::dbExecute(private$conn, query)
                      }, error = function(e){
                        stop(sprintf("Index on table: '%s' could not be created (Db.exportScenDataset). " %+%
                                       "Error message: %s.", tableName, e), call. = FALSE)
                      })
                    }
                    flog.debug("Db: A database table named: '%s' did not yet exist. 
        Therefore it was created (Db.exportScenDataset).", tableName)
                    tryCatch({
                      DBI::dbWriteTable(private$conn, tableName, dataset, row.names = FALSE, append = TRUE)
                      flog.info("Db: First data was written to table: '%s' (Db.exportScenDataset).", tableName)
                    }, error = function(e){
                      stop(sprintf("Db: An error occurred writing to database (Db.exportScenDataset, table: '%s'). 
          Error message: %s", tableName, e), call. = FALSE)
                    })
                  }else{
                    flog.debug("Db: Nothing was written to table '%s' as no data was provided.", tableName)
                  }
                  invisible(self)
                },
                writeMetadata = function(metadata, update = FALSE, hcubeMetadata = FALSE){
                  # Write scenario metadata to database
                  #
                  # Args:
                  #   metadata:      dataframe containing metadata for one up 
                  #                  to many scenarios
                  #   update:        boolean that specifies whether existing metadata 
                  #                  shall be updated
                  #   hcubeMetadata: boolean that specifies whether metadata is hcube run 
                  #                  metadata or not (scenario metadata)
                  #
                  # Returns:
                  #   reference to itself (Db R6 object)
                  
                  # BEGIN error checks
                  stopifnot(inherits(metadata, "data.frame"))
                  stopifnot(is.logical(update), length(update) == 1L)
                  stopifnot(is.logical(hcubeMetadata), length(hcubeMetadata) == 1L)
                  # END error checks
                  if(hcubeMetadata){
                    metaTabName <- private$tableNameMetaHcube
                  }else{
                    metaTabName <- private$tableNameMetadata
                  }
                  
                  metadata <- dateColToChar(private$conn, metadata)
                  if(!DBI::dbExistsTable(private$conn, metaTabName)){
                    tryCatch({
                      query <- paste0("CREATE TABLE ", 
                                      DBI::dbQuoteIdentifier(private$conn, metaTabName), 
                                      " (", 
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), 
                                      if(inherits(private$conn, "PqConnection")) 
                                        " serial PRIMARY KEY," else " integer PRIMARY KEY,",
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['uid']), 
                                      " varchar(50) NOT NULL,", 
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sname']), 
                                      " varchar(255) NOT NULL,",
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['stime']), 
                                      if(inherits(private$conn, "PqConnection")) 
                                        " timestamp with time zone," else " text,",
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['stag']), 
                                      " text,",
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['accessR']), 
                                      " text NOT NULL,",
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['accessW']), 
                                      " text NOT NULL,", 
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['accessX']), 
                                      " text NOT NULL,",
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['scode']), 
                                      if(inherits(private$conn, "PqConnection")) " smallint);" else " integer);")
                      DBI::dbExecute(private$conn, query)
                    }, error = function(e){
                      stop(sprintf("Metadata table could not be created (Db.writeMetadata). " %+%
                                     "Error message: %s.", e), call. = FALSE)
                    })
                    flog.debug("Db: A table named: '%s' did not yet exist (Db.writeMetadata). " %+%
                                 "Therefore it was created.", metaTabName)
                  }
                  if(update){
                    sql     <- DBI::SQL(paste0("UPDATE ", DBI::dbQuoteIdentifier(private$conn, 
                                                                                 metaTabName), 
                                               " SET (", 
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['uid']), ", ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sname']), ", ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['stime']), ", ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['stag']), ", ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['accessR']), ", ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['accessW']), ", ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['accessX']), ") = (",
                                               "?uid, ?sname, ?stime, ?stag, ?accessR, ?accessW, ?accessX) WHERE ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), " = ?sid;"))
                    query   <- DBI::sqlInterpolate(private$conn, sql, uid = metadata[[private$scenMetaColnames['uid']]],
                                                   sname = metadata[[private$scenMetaColnames['sname']]][[1]], 
                                                   stime = as.character(metadata[[private$scenMetaColnames['stime']]][[1]], usetz = TRUE), 
                                                   stag = metadata[[private$scenMetaColnames['stag']]][[1]], 
                                                   accessR = metadata[[private$scenMetaColnames['accessR']]][[1]], 
                                                   accessW = metadata[[private$scenMetaColnames['accessW']]][[1]], 
                                                   accessX = metadata[[private$scenMetaColnames['accessX']]][[1]], 
                                                   sid = metadata[[private$scenMetaColnames['sid']]][[1]])
                    tryCatch({
                      DBI::dbExecute(private$conn, query)
                      flog.debug("Db: Metadata (table: '%s') was written to database. (Db.writeMetadata).", 
                                 metaTabName)
                    }, error = function(e){
                      stop(sprintf("Db: Metadata (table: '%s') could not " %+% "
        be written to database (Db.writeMetadata). Error message: %s.", 
                                   metaTabName, e), call. = FALSE)
                    })
                  }else{
                    # write new metadata
                    tryCatch({
                      DBI::dbWriteTable(private$conn, metaTabName, 
                                        metadata, row.names = FALSE, append = TRUE)
                      flog.debug("Db: Metadata (table: '%s') was written to database. (Db.writeMetadata).", 
                                 metaTabName)
                    }, error = function(e){
                      stop(sprintf("Db: Metadata (table: '%s') could not " %+% "
        be written to database (Db.writeMetadata). Error message: %s.", 
                                   metaTabName, e), call. = FALSE)
                    })
                  }
                  
                  invisible(self)
                },
                writeMetaHcube = function(hcubeTags = character(1L), manual = FALSE, noScen = 0L){
                  # adds new entry to hcube run metadata table
                  #
                  # Args:
                  #   hcubeTags:         character vector with tags to save for hcube run (optional)
                  #   manual:            boolean that specifies whether Hypercube job is manually imported
                  #   noScen:            integer: number of scenarios that are part of Hypercube job
                  #
                  # Returns:
                  #   hcube job Id (integer)
                  if(is.null(hcubeTags)){
                    hcubeTags <- character(1L)
                  }
                  stopifnot(is.character(hcubeTags))
                  stopifnot(is.logical(manual), length(manual) == 1L)
                  stopifnot(is.integer(noScen), length(noScen) == 1L)
                    
                  now <- Sys.time()
                  if(manual){
                    statusCode <- "_corrupted(man)"
                  }else{
                    statusCode <- "_scheduled"
                  }
                  uAccessGroups <- vector2Csv(private$userAccessGroups)
                  metadata <- tibble(private$uid, statusCode, 
                                     now, vector2Csv(hcubeTags), permR = uAccessGroups,
                                     permW = uAccessGroups, permX = uAccessGroups, 
                                     scode = noScen)
                  names(metadata) <- private$scenMetaColnames[-1]
                  
                  self$writeMetadata(metadata, update = FALSE, hcubeMetadata = TRUE)
                  
                  if(inherits(private$conn, "PqConnection")){
                    query <- "SELECT lastval();"
                  }else{
                    query <- "SELECT last_insert_rowid();"
                  }
                  
                  jID <- DBI::dbGetQuery(private$conn, query)
                  if(length(jID) && length(jID[[1L]])){
                    return(jID[[1]][[1]])
                  }
                  stop("Job ID could not be identified. Something went wrong while writing hcube metadata to database.", 
                       call. = FALSE)
                },
                getMetaHcube = function(onlyActive = FALSE){
                  # fetches hcube job metadata
                  #
                  # Args:
                  #   onlyActive:   logical that specifies whether to fetch all 
                  #                 or only active jobs
                  #
                  # Returns:
                  #   tibble with metadata
                  stopifnot(is.logical(onlyActive), length(onlyActive) == 1L)
                  
                  accessRights <- private$getCsvSubsetClause(private$scenMetaColnames['accessR'], 
                                                             private$userAccessGroups)
                  
                  if(onlyActive){
                    hcubeMeta <- self$importDataset(private$tableNameMetaHcube, accessRights,
                                                      tibble(private$scenMetaColnames['sname'],
                                                             "%_", "NOT LIKE"),
                                                    innerSepAND = FALSE)
                  }else{
                    hcubeMeta <- self$importDataset(private$tableNameMetaHcube, accessRights, 
                                                    innerSepAND = FALSE)
                  }
                  
                  
                  return(hcubeMeta)
                },
                updateHypercubeJob = function(jid, pid = NULL, tags = NULL, status = NULL,
                                              accessR = NULL, accessW = NULL, accessX = NULL, 
                                              scode = NULL){
                  # set process id for hcube job
                  # 
                  # Args:
                  #   jid:           ID of Hypercube job to update
                  #   pid:           process ID of hcube job
                  #   tags:          new tags for Hypercube job
                  #   status:        new status for Hypercube job
                  #   accessR:       new read permissions
                  #   accessW:       new write permissions
                  #   accessX:       new execute permissions
                  #   scode:         new status code
                  #
                  # Returns:
                  #   invisibly returns R6 object (reference to Db class)
                  stopifnot(is.integer(jid), length(jid) == 1L)
                  
                  colNames <- NULL
                  values   <- NULL
                  if(!is.null(pid)){
                    stopifnot(is.integer(pid), length(pid) == 1L)
                    stopifnot(is.null(status))
                    colNames <- private$scenMetaColnames['sname']
                    values   <- paste0(pid, "_running")
                  }
                  if(!is.null(tags)){
                    stopifnot(is.character(tags))
                    colNames <- c(colNames, private$scenMetaColnames['stag'])
                    values   <- c(values, vector2Csv(tags))
                  }
                  if(!is.null(status)){
                    stopifnot(is.character(status), length(status) == 1L)
                    if(!startsWith(status, "_")){
                      status <- paste0("_", status)
                    }
                    colNames <- c(colNames, private$scenMetaColnames['sname'])
                    values   <- c(values, status)
                  }
                  if(is.null(colNames)){
                    stop("No Hypercube metadata was updated as no data was provided.", call. = FALSE)
                  }
                  if(!is.null(accessR)){
                    stopifnot(is.character(accessR), length(accessR) > 0L)
                    colNames <- c(colNames, private$scenMetaColnames['accessR'])
                    values   <- c(values, vector2Csv(accessR))
                  }
                  if(!is.null(accessW)){
                    stopifnot(is.character(accessW), length(accessW) > 0L)
                    colNames <- c(colNames, private$scenMetaColnames['accessW'])
                    values   <- c(values, vector2Csv(accessW))
                  }
                  if(!is.null(accessX)){
                    stopifnot(is.character(accessX), length(accessX) > 0L)
                    colNames <- c(colNames, private$scenMetaColnames['accessX'])
                    values   <- c(values, vector2Csv(accessX))
                  }
                  if(!is.null(scode)){
                    scode <- suppressWarnings(as.integer(scode))
                    stopifnot(!is.na(scode), length(scode) == 1L)
                    colNames <- c(colNames, private$scenMetaColnames['scode'])
                    values   <- c(values, scode)
                  }
                  
                  noRowsUpdated <- self$updateRows(private$tableNameMetaHcube, 
                                                   colNames = colNames, 
                                                   values = values, subsetSids = jid, 
                                                   innerSepAND = FALSE)
                  if(!noRowsUpdated){
                    stop("Job metadata was not updated. This might be due to insufficient write permissions.", call. = FALSE)
                  }
                  
                  invisible(self)
                },
                fetchScenList = function(scode = 0L){
                  # returns list of scenarios that the current user has access to
                  #
                  # Args:
                  #   scode:           Fetch only scenarios with either of these scenario codes
                  #
                  # Returns:
                  #   tibble: tibble with all scenarios user has access to read as well 
                  #   as their metadata, throws exception in case of error
                  stopifnot(is.integer(scode))

                  scenList <- self$importDataset(private$tableNameMetadata, 
                                                 tibble(private$scenMetaColnames['scode'], 
                                                        scode), innerSepAND = FALSE)
                
                  
                  return(scenList)
                  
                },
                formatScenList = function(scenList, orderBy = NULL, desc = FALSE, limit = 100L){
                  # returns list of scenarios (formatted for dropdown menu)
                  #
                  # Args:
                  #   scenList:          dataframe with scenario metadata
                  #   orderBy:           column to use for ordering data frame (optional)
                  #   desc:              boolean that specifies whether ordering should be 
                  #                      descending(FALSE) or ascending (TRUE) (optional)
                  #   limit:             maximum number of scenarios to format
                  #
                  # Returns:
                  #   character vector: named vector formatted to be used in dropdown menus, 
                  #   returns NULL in case no scenarios found
                  
                  #BEGIN error checks
                  if(!hasContent(scenList)){
                    return(NULL)
                  }
                  stopifnot(inherits(scenList, "data.frame"))
                  if(!is.null(orderBy)){
                    stopifnot(is.character(orderBy) && length(orderBy) == 1)
                  }
                  stopifnot(is.logical(desc), length(desc) == 1)
                  limit <- as.integer(limit)
                  stopifnot(!is.na(limit))
                  # END error checks
                  
                  limit <- min(nrow(scenList), limit)
                  scenList <- scenList[1:limit, , drop = FALSE]
                  if(!is.null(orderBy)){
                    if(desc){
                      scenList <- dplyr::arrange(scenList, desc(!!as.name(orderBy)))
                    }else{
                      scenList <- dplyr::arrange(scenList, !!as.name(orderBy))
                    }
                  }
                  
                  setNames(paste0(scenList[[private$scenMetaColnames['sid']]], "_", 
                                  scenList[[private$scenMetaColnames['uid']]]), 
                           paste0(vapply(scenList[[private$scenMetaColnames['uid']]], 
                                         function(el){ 
                                           if(identical(el, private$uid)) "" else paste0(el, ": ")}, 
                                         character(1), USE.NAMES = FALSE), 
                                  scenList[[private$scenMetaColnames['sname']]], " (", 
                                  scenList[[private$scenMetaColnames['stime']]], ")"))
                },
                buildRowSubsetSubquery = function(subsetData, innerSep, outerSep, SQL = TRUE){
                  brackets <- NULL
                  if(identical(innerSep, " OR "))
                    brackets <- c("(", ")")
                  if(SQL){
                    query <- paste(brackets[1], unlist(lapply(subsetData, private$buildSQLSubsetString, 
                                                       innerSep), use.names = FALSE), brackets[2],  
                                   collapse = outerSep)
                    return(DBI::SQL(query))
                  }else{
                    query <- paste(brackets[1], unlist(lapply(subsetData, private$buildRSubsetString, 
                                                       innerSep), use.names = FALSE), brackets[2],  
                                   collapse = outerSep)
                    return(query)
                  }
                },
                escapePatternPivot = function(pattern){
                  if(inherits(private$conn, "PqConnection")){
                    return(self$escapePattern(pattern))
                  }else{
                    bsEscaped <- gsub("\\", "\\\\", pattern, fixed = TRUE)
                    return(gsub("([.|()\\^{}+$*?'\"]|\\[|\\])", 
                                "\\\\\\1", bsEscaped))
                  }
                },
                escapePattern = function(pattern){
                  if(inherits(private$conn, "PqConnection")){
                    pattern <- gsub("([%_\\])", "\\\\\\1", pattern)
                  }else{
                    pattern <- gsub("([%_])", "\\1\\1", pattern)
                  }
                  return(pattern)
                },
                removeExpiredAttachments = function(fileNames, maxDuration){
                  # removes attachments that have exceeded maximum storage duration
                  # 
                  # Args:
                  #    fileNames:     file names to remove
                  #    maxDuration:   maximum storage duration (in days)
                  # 
                  # Returns:
                  #    integer: number of rows affected
                  stopifnot(is.character(fileNames), length(fileNames) >= 1L)
                  stopifnot(is.integer(maxDuration), length(maxDuration) == 1L)
                  
                  tableName      <- private$dbSchema$tabName['_scenAttach']
                  colNames       <- private$dbSchema$colNames[['_scenAttach']]
                  expirationTime <- as.character(Sys.time() - 3600*24*maxDuration, 
                                                 usetz = TRUE, tz = "GMT")
                  if(DBI::dbExistsTable(private$conn, tableName)){
                    tryCatch({
                      query <- paste0("DELETE FROM ", dbQuoteIdentifier(private$conn, tableName),
                                      " WHERE ", dbQuoteIdentifier(private$conn, colNames[['fn']]), " IN (", 
                                      paste(dbQuoteString(private$conn, fileNames), collapse = ", "), ") AND ",
                                      dbQuoteIdentifier(private$conn, colNames[['time']]), "<", 
                                      dbQuoteString(private$conn, expirationTime))
                      affectedRows <- DBI::dbExecute(private$conn, query)
                      flog.debug("Db: %s attachments in table: '%s' were deleted due to surpassing the expiration date (Db.removeExpiredAttachments).", 
                                 affectedRows, tableName)
                    }, error = function(e){
                      stop(sprintf("Db: An error occurred while deleting rows from the database (Db.removeExpiredAttachments, " %+% 
                                     "table: '%s'). Error message: %s.", tableName, e), call. = FALSE)
                    })
                  }
                  
                  return(invisible(self))
                },
                finalize = function(){
                  DBI::dbDisconnect(private$conn)
                  flog.info("Db: Database connection ended as Db object was gced.")
                }
              ),
              active = list(
                accessGroups= function(groups){
                  if(missing(groups))
                    return(private$userAccessGroups)
                  
                  stopifnot(is.character(groups), length(groups) >=1)
                  private$userAccessGroups <- groups
                }
              ),
              private = list(
                conn                = NULL,
                uid                 = character(1L),
                modelName           = character(1L),
                dbSchema            = vector("list", 3L),
                scenMetaColnames    = character(1L),
                slocktimeIdentifier = character(1L),
                userAccessGroups    = character(1L),
                tableNameMetadata   = character(1L),
                tableNameMetaHcube  = character(1L),
                tableNameScenLocks  = character(1L),
                tableNamesScenario  = character(1L),
                slocktimeLimit      = character(1L),
                hcubeActive         = logical(1L),
                info                = new.env(),
                attachmentConfig    = vector("list", 2L),
                isValidSubsetGroup  = function(dataFrame){
                  if(inherits(dataFrame, "data.frame") 
                     && length(dataFrame) <= 3L
                     && is.character(dataFrame[[1]])
                     && (is.character(dataFrame[[2]])
                         || is.numeric(dataFrame[[2]])
                         || is.logical(dataFrame[[2]]))){
                    if(length(dataFrame) < 3L){
                      return(TRUE)
                    }else if(all(dataFrame[[3]] %in% 
                                 c("=", "<", ">", "<=", ">=", "!=", "<>", 
                                   "SIMILAR TO", "NOT SIMILAR TO", 
                                   "LIKE", "NOT LIKE"))){
                      return(TRUE)
                    }
                  }
                  return(FALSE)
                },
                getCsvSubsetClause = function(colName, vector){
                  subsetClause <- tibble(colName,
                                         paste0("%,", self$escapePattern(vector), ",%"), "LIKE")
                  return(subsetClause)
                },
                buildRSubsetString = function(dataFrame, sep = " "){
                  if(!length(dataFrame) || !nrow(dataFrame)){
                    return(NULL)
                  }else if(length(dataFrame) < 3L){
                    dataFrame[[3]] <- "="
                  }
                  fields <- dataFrame[[1]]
                  vals   <- self$escapePatternPivot(dataFrame[[2]])
                  if(identical(length(dataFrame), 4L)){
                    fields <- paste0(dataFrame[[4]], ".", fields)
                  }
                  fields <- paste0("`", fields, "`")
                  # replace operators that are different in R and SQL
                  cond <- vapply(seq_along(dataFrame[[3]]), function(i){
                    field <- fields[i]
                    op    <- dataFrame[[3]][i]
                    val   <- vals[i]
                    switch(op,
                           "=" = {
                             return(paste0(field, "=='", val, "'"))
                           },
                           "%LIKE" = {
                             return(paste0("grepl('", val, "$', ", 
                                           field, ", perl = TRUE)"))
                           },
                           "LIKE%" = {
                             return(paste0("grepl('^", val, "', ", 
                                           field, ", perl = TRUE)"))
                           },
                           "%LIKE%" = {
                             return(paste0("grepl('", val, "', ", 
                                           field, ", perl = TRUE)"))
                           },
                           "%NOTLIKE%" = {
                             return(paste0("!grepl('", val, "', ", 
                                           field, ", perl = TRUE)"))
                           },
                           "%LIKE," = {
                             return(paste0("grepl('", val, ",', ", 
                                           field, ", perl = TRUE)"))
                           },
                           ",LIKE%" = {
                             return(paste0("grepl(',", val, "', ", 
                                           field, ", perl = TRUE)"))
                           },
                           "%,LIKE,%" = {
                             return(paste0("grepl(',", val, ",', ", 
                                           field, ", perl = TRUE)"))
                           },
                           "%,NOTLIKE,%" = {
                             return(paste0("!grepl(',", val, ",', ", 
                                           field, ", perl = TRUE)"))
                           },
                           {
                             return(paste0(field, op, "'", val, "'"))
                           })
                  }, character(1L), USE.NAMES = FALSE)
                  
                  query <- paste(cond, collapse = sep)
                  return(query)
                },
                buildSQLSubsetString = function(dataFrame, sep = " "){
                  if(!length(dataFrame) || !nrow(dataFrame)){
                    return(NULL)
                  }else if(length(dataFrame) < 3L){
                    dataFrame[[3]] <- "="
                  }
                  fields <- dataFrame[[1]]
                  vals   <- as.character(dataFrame[[2]])
                  fields <- DBI::dbQuoteIdentifier(private$conn, fields)
                  vals   <- DBI::dbQuoteLiteral(private$conn, vals)
                  
                  if(identical(length(dataFrame), 4L)){
                    fields <- paste0(dbQuoteIdentifier(private$conn, dataFrame[[4]]),
                                     ".", fields)
                  }
                  query <- paste(paste(fields, dataFrame[[3]], vals), collapse = sep)
                  return(SQL(query))
                },
                getFieldTypes = function(data){
                  # returns vector with field types of data frame and changes integer type to numeric
                  fieldTypes <- vapply(seq_along(data), function(i){
                    if(identical(names(data)[[i]], private$scenMetaColnames[['sid']])){
                      return("int")
                    }else if(typeof(data[[i]]) == "integer" && !is.factor(data[[i]])){
                      data[[i]] <- as.numeric(data[[i]])
                    }
                    dType <- dbDataType(private$conn, data[[i]])
                    if(identical(dType, "REAL")){
                      dType <- "DOUBLE PRECISION"
                    }
                    return(dType)
                  }, character(1), USE.NAMES = FALSE)
                  names(fieldTypes) <- names(data)
                  flog.trace("Db: Returned database field types: '%s'.", 
                             paste(fieldTypes, collapse = ", "))
                  return(fieldTypes)
                },
                getMaximum = function(tableName, colName){
                  # Returns number of unique entries in the specified column
                  #
                  # Args:
                  #   tableName :      name of the table to retrieve maximum value from 
                  #   colName:         column name to fetch maximum from
                  #
                  # Returns:
                  #   numeric: largest value in the column specified, 
                  #   throws exception in case of error
                  
                  
                  if(!dbExistsTable(private$conn, tableName)){
                    return(0L)
                  }
                  tryCatch({
                    query <- DBI::SQL(paste0("SELECT ", DBI::dbQuoteIdentifier(private$conn, colName), 
                                             " FROM ", DBI::dbQuoteIdentifier(private$conn, tableName), 
                                             " ORDER BY ", DBI::dbQuoteIdentifier(private$conn, colName), 
                                             " DESC LIMIT 1;"))
                    max   <- suppressWarnings(as.integer(dbGetQuery(private$conn, query)[[1]][1]))
                  }, error = function(e){
                    flog.error("Db: An error occurred while querying the database (Db.getMaximum). " %+%
"Error message: %s.", e)
                    stop(sprintf("An error occurred while querying the database (Db.getMaximum). " %+%
"Error message: %s.", e), call. = FALSE)
                  })
                  
                  if(!is.na(max)){
                    return(max)
                  }else{
                    return(0L)
                  }
                },
                getTableNamesModel = function(){
                  # attachment table currently not fetched
                  if(inherits(private$conn, "PqConnection")){
                    query <- SQL(paste0("SELECT table_name FROM information_schema.tables", 
                                        " WHERE table_schema='public' AND table_type='BASE TABLE'", 
                                        " AND (table_name IN (", 
                                        paste(dbQuoteString(private$conn, c(private$tableNameMetadata,
                                                                            private$tableNameMetaHcube,
                                                                            private$tableNameScenLocks,
                                                                            private$tableNamesScenario,
                                                                            private$dbSchema$tabName[['_scenTrc']])),
                                              collapse = ", "),
                                        ") OR table_name LIKE ", 
                                        dbQuoteString(private$conn, modelName %+% "\\_%"), 
                                        ");"))
                  }else{
                    query <- SQL(paste0("SELECT name FROM sqlite_master WHERE type = 'table'",
                                        " AND (name IN (", 
                                        paste(dbQuoteString(private$conn, c(private$tableNameMetadata,
                                                                            private$tableNameMetaHcube,
                                                                            private$tableNameScenLocks,
                                                                            private$tableNamesScenario,
                                                                            private$dbSchema$tabName[['_scenTrc']])),
                                              collapse = ", "),
                                        ") OR name LIKE ", 
                                        dbQuoteString(private$conn, modelName %+% "\\_%"), " ESCAPE '\\');"))
                  }
                  return(dbGetQuery(private$conn, query)[[1L]])
                }
              )
)
