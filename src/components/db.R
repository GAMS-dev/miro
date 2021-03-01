# R6 class for database related functions
Db <- R6Class("Db",
              public = list(
                initialize        = function(uid, dbConf, dbSchema, slocktimeLimit, modelName,
                                             traceColNames = NULL, hcubeActive = FALSE,
                                             ugroups = character(0L), forceNew = FALSE){
                  # Initialize database class
                  #
                  # Args:
                  #   dbConf:              list with database connection configuration
                  #                        includes: type, host, port(optional), username, 
                  #                        password and name elements
                  #   dbSchema:            database schema
                  #   modelName:           name of the current model
                  #   slocktimeLimit:      maximum duration a lock is allowed to persist
                  #   hcubeActive:         boolean that specifies whether Hypercube mode is currently active
                  #   ugroups:             user group(s) (optional)
                  #   forceNew:            force creating new db object even if one already exists
                  
                  #BEGIN error checks 
                  if(is.null(private$info$isInitialized) || forceNew){
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
                  stopifnot(is.character(modelName), length(modelName) == 1L)
                  stopifnot(is.logical(hcubeActive), length(hcubeActive) == 1L)
                  #END error checks 
                  
                  private$uid                         <- uid
                  if(length(ugroups) >= 1L && is.character(ugroups)){
                    private$userAccessGroups <- c(uid, ugroups)
                  }else{
                    private$userAccessGroups <- uid
                  }
                  private$modelName                   <- modelName
                  private$modelNameDb                 <- gsub("_", "", modelName, fixed = TRUE)
                  private$dbSchema                    <- dbSchema
                  private$scenMetaColnames            <- dbSchema$colNames[['_scenMeta']]
                  private$slocktimeIdentifier         <- dbSchema$colNames[['_scenLock']][['lock']]
                  private$tableNameMetadata           <- dbSchema$tabName[['_scenMeta']]
                  private$tableNameScenLocks          <- dbSchema$tabName[['_scenLock']]
                  private$tableNamesScenario          <- dbSchema$tabName[!startsWith(dbSchema$tabName, "_")]
                  private$slocktimeLimit              <- slocktimeLimit
                  private$hcubeActive                 <- hcubeActive
                  
                  if(identical(dbConf$type, "postgres")){
                    tryCatch({
                      private$conn <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbConf$name, 
                                                     host = dbConf$host, port = dbConf$port, 
                                                     user = dbConf$username, password = dbConf$password,
                                                     bigint = "integer")
                    }, error = function(e){
                      stop(sprintf("Db: Database connection could not be established. Error message: %s", e), 
                           call. = FALSE)
                    })
                    private$connectionInfo <- list(loc = paste0(dbConf$host, ":", dbConf$port),
                                                   name = dbConf$name)
                  }else if(identical(dbConf$type, "sqlite")){
                    tryCatch({
                      private$conn <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = dbConf$name, bigint = "integer")
                      # turn foreign key usage on
                      dbExecute(private$conn, "PRAGMA foreign_keys = ON;")
                    }, error = function(e){
                      stop(sprintf("Db: Database connection could not be established. Error message: %s", e), 
                           call. = FALSE)
                    })
                    private$connectionInfo <- list(loc = dbConf$name,
                                                   name = NULL)
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
                getTableNameScenLocks = function() private$tableNameScenLocks,
                getTableNamesScenario = function() private$tableNamesScenario,
                getHcubeActive        = function() private$hcubeActive,
                getModelNameDb        = function() private$modelNameDb,
                getInfo               = function(){
                  # Returns some connection info
                  #
                  # Returns:
                  #   list with strings: $loc (location info), $name (database name)
                  return(private$connectionInfo)
                },
                getMetadata           = function(sid, uid, sname, stime, stag = character(0L), 
                                                 readPerm = character(0L), writePerm = character(0L),
                                                 execPerm = character(0L),
                                                 uidAlias = private$scenMetaColnames[['uid']], 
                                                 snameAlias = private$scenMetaColnames[['sname']], 
                                                 stimeAlias = private$scenMetaColnames[['stime']],
                                                 stagAlias = private$scenMetaColnames[['stag']],
                                                 readPermAlias = private$scenMetaColnames[['accessR']],
                                                 writePermAlias = private$scenMetaColnames[['accessW']],
                                                 execPermAlias = private$scenMetaColnames[['accessX']]){
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
                  #   execPerm:                 execute permissions of scenario (optional)
                  #   uidAlias:                 User ID Description (optional)
                  #   snameAlias:               Scenario name Description (optional)
                  #   stimeAlias:               Scenario time description (optional)
                  #   stagAlias:                Scenario tag description (optional)
                  #   readPermAlias:            Scenario read permissions description (optional)
                  #   writePermAlias:           Scenario write permissions description (optional)
                  #   execPermAlias:            Scenario execute permissions description (optional)
                  #
                  # Returns:
                  #   dataframe with metadata
                  
                  #BEGIN error checks
                  if(!length(sid))
                    sid <- NA_integer_
                  stopifnot(is.integer(sid),
                            is.character(uid),
                            is.character(sname))
                  stime <- as.POSIXct(stime)
                  stopifnot(inherits(stime, "POSIXct"))
                  stopifnot(is.character(stag), is.character(readPerm),
                            is.character(writePerm), is.character(execPerm),
                            is.character(uidAlias), length(uidAlias) == 1L,
                            is.character(snameAlias), length(snameAlias) == 1L,
                            is.character(stimeAlias), length(stimeAlias) == 1L,
                            is.character(stagAlias), is.character(readPermAlias),
                            is.character(writePermAlias), is.character(execPermAlias))
                  #END error checks
                  
                  metadata <- tibble(sid, uid, sname, stime)
                  names(metadata) <- c(private$scenMetaColnames["sid"], uidAlias, snameAlias, stimeAlias)
                  if(length(stag)){
                    if(length(stag) > 1L){
                      stag <- vector2Csv(stag)
                    }
                    metadata[[stagAlias]] <- stag
                  }
                  permList <- list(perm = list(readPerm, writePerm, execPerm),
                                   alias = list(readPermAlias, writePermAlias,
                                                execPermAlias))
                  for(i in seq_len(3L)){
                    perm <- permList$perm[[i]]
                    if(length(perm)){
                      metadata[[permList$alias[[i]]]] <- vector2Csv(perm)
                    }
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
                    c(uid, sname, if(private$hcubeActive) -1L else 0L)), count = TRUE, limit = 1L)[[1]]
                  if(scenExists >= 1){
                    flog.trace("Db: Scenario with name: '%s' already exists for user: '%s' " %+%
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
                      private$scenMetaColnames['sname'],
                      private$scenMetaColnames['scode']),
                    c(uid, sname, if(private$hcubeActive) SCODEMAP[['hcube_jobconfig']] 
                      else SCODEMAP[['scen']])), 
                    limit = 1L)
                  
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
                loadScriptResults = function(sids, limit = 1e7, msgProgress){
                  # Load script results from database
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
                  scriptData <- lapply(seq_along(sids), function(i){
                    dataset <- self$importDataset(tableName = private$dbSchema$tabName[["_scenScripts"]], 
                                                  subsetSids = sids[i], limit = limit)
                    dataset[, private$scenMetaColnames['sid']] <- NULL
                    updateProgress(detail = msgProgress$progress %+% i)
                    return(dataset)
                  })
                  return(scriptData)
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
                    if(length(subsetRows)){
                      subsetRows <- DBI::SQL(paste(subsetRows, subsetSidSQL, sep = " AND "))
                    }else{
                      subsetRows <- DBI::SQL(subsetSidSQL)
                    }
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
                exportDataset = function(tableName, data, checkColNames = FALSE){
                  # Export the dataframe to database
                  #
                  # Args:
                  #   tableName :       name of the table to import dataframe from
                  #   data:             dataframe with data to write
                  #   checkColNames:    boolean that specifies whether col name should be
                  #                     checked. Non existing columns will be appended
                  #
                  # Returns:
                  #   reference to class or throws exception in case of failure
                  
                  stopifnot(inherits(data, "data.frame"),is.character(tableName), 
                            identical(length(tableName), 1L), 
                            is.logical(checkColNames), 
                            identical(length(checkColNames), 1L))
                  if(!dbExistsTable(private$conn, tableName)){
                    dbWriteTable(private$conn, tableName, data, row.names = FALSE)
                    return(self)
                  }
                  if(!checkColNames){
                    dbWriteTable(private$conn, tableName, data, row.names = FALSE, append = TRUE)
                    return(self)
                  }
                  dataValidate <- self$importDataset(limit = 1L)
                  nonMatchingColNames <- is.na(match(names(data), 
                                                     names(dataValidate)))
                  if(any(nonMatchingColNames)){
                    stop(sprintf("Some columns could not be found in the table: '%s'",
                                 paste(names(data)[nonMatchingColNames], 
                                       collapse = "', '")), call. = FALSE)
                  }
                  if(identical(length(dataValidate, nonMatchingColNames))){
                    dbWriteTable(private$conn, tableName, data, row.names = FALSE, append = TRUE)
                    return(self)
                  }
                  nrowData <- nrow(data)
                  dataList <- lapply(names(dataValidate), function(colName){
                    colId <- match(colName, names(data))
                    if(!is.na(colId)){
                      return(data[[colId]])
                    }
                    col <- dataValidate[[colName]]
                    if(any(class(col) == "POSIXt")){
                      return(rep(NA_character_, nrowData))
                    }
                    switch(class(col), 
                           logical = {
                             return(logical(nrowData))
                           },
                           integer = {
                             return(rep(NA_integer_, nrowData))
                           },
                           numeric = {
                             return(rep(NA_real_, nrowData))
                           },
                           character = {
                             return(rep(NA_character_, nrowData))
                           },
                           {
                             stop(sprintf("Unknown data type: '%s' of column: '%s'", 
                                          class(col), colName), call. = FALSE)
                           }
                    )
                  })
                  names(dataList) <- names(dataValidate)
                  dbWriteTable(private$conn, tableName, as_tibble(dataList), 
                               row.names = FALSE, append = TRUE)
                  return(self)
                },
                importDataset = function(tableName, ..., colNames = NULL, count = FALSE, limit = 1e7, 
                                         innerSepAND = TRUE, distinct = FALSE, subsetSids = NULL,
                                         orderBy = character(0L), orderAsc = TRUE, isAdmin = FALSE){
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
                  #   orderBy:          character vector of columns to order by
                  #   orderAsc:         boolean that specifies whether to order in ascending (TRUE)
                  #                     or descending (FALSE) order
                  #   isAdmin:          boolean that specifies whether user accessing data is admin (can read all data)
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
                  stopifnot(is.character(orderBy))
                  stopifnot(is.logical(orderAsc), length(orderAsc) == 1)
                  stopifnot(is.logical(isAdmin), length(isAdmin) == 1)
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
                    orderByQuery <- character(0L)
                    if(length(orderBy)){
                      orderByQuery <- paste0(" ORDER BY ", 
                                             paste(DBI::dbQuoteIdentifier(private$conn, 
                                                                          orderBy), collapse = ", "),
                                             if(orderAsc) " ASC" else " DESC")
                    }
                    subsetReadPerm <- NULL
                    if(!isAdmin && identical(tableName, private$tableNameMetadata)){
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
                                                 if(length(subsetRows)) " WHERE ", subsetRows, orderByQuery,
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
                writeMetadata = function(metadata, update = FALSE){
                  # Write scenario metadata to database
                  #
                  # Args:
                  #   metadata:      dataframe containing metadata for one up 
                  #                  to many scenarios
                  #   update:        boolean that specifies whether existing metadata 
                  #                  shall be updated
                  #
                  # Returns:
                  #   reference to itself (Db R6 object)
                  
                  # BEGIN error checks
                  stopifnot(inherits(metadata, "data.frame"))
                  stopifnot(is.logical(update), length(update) == 1L)
                  # END error checks
                  metaTabName <- private$tableNameMetadata
                  
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
                createJobMeta = function(){
                  tabName  <- private$dbSchema$tabName[["_jobMeta"]]
                  colNames <- private$dbSchema$colNames[["_jobMeta"]]
                  if(DBI::dbExistsTable(private$conn, tabName)){
                    return(invisible(self))
                  }
                  query <- paste0("CREATE TABLE ", 
                                  DBI::dbQuoteIdentifier(private$conn, tabName), 
                                  " (", 
                                  DBI::dbQuoteIdentifier(private$conn, colNames[[1]]), 
                                  if(inherits(private$conn, "PqConnection")) 
                                    " serial PRIMARY KEY," else " integer PRIMARY KEY,",
                                  DBI::dbQuoteIdentifier(private$conn, colNames[[2]]), 
                                  " varchar(50) NOT NULL,",
                                  DBI::dbQuoteIdentifier(private$conn, colNames[[3]]), 
                                  " integer,", 
                                  DBI::dbQuoteIdentifier(private$conn, colNames[[4]]), 
                                  if(inherits(private$conn, "PqConnection")) 
                                    " timestamp with time zone," else " text,",
                                  DBI::dbQuoteIdentifier(private$conn, colNames[[5]]), 
                                  " text,",
                                  DBI::dbQuoteIdentifier(private$conn, colNames[[6]]), 
                                  " varchar(255) NOT NULL,",
                                  DBI::dbQuoteIdentifier(private$conn, colNames[[7]]),
                                  if(inherits(private$conn, "PqConnection")) 
                                    " smallint," else " integer,",
                                  DBI::dbQuoteIdentifier(private$conn, colNames[[8]]),
                                  if(inherits(private$conn, "PqConnection")) 
                                    " smallint," else " integer,",
                                  DBI::dbQuoteIdentifier(private$conn, colNames[[9]]),
                                  " integer,",
                                  DBI::dbQuoteIdentifier(private$conn, colNames[[10]]),
                                  " varchar(255));")
                  DBI::dbExecute(private$conn, query)
                  return(invisible(self))
                },
                fetchScenList = function(scode = SCODEMAP[['scen']], gt = FALSE){
                  # returns list of scenarios that the current user has access to
                  #
                  # Args:
                  #   scode:           Fetch only scenarios with either of these scenario codes
                  #   gt:              boolean that specifies whether to fetch equal status codes or greater/equal
                  #
                  # Returns:
                  #   tibble: tibble with all scenarios user has access to read as well 
                  #   as their metadata, throws exception in case of error
                  stopifnot(is.integer(scode))
                  stopifnot(is.logical(gt), length(gt) == 1L)
                  
                  if(gt){
                    return(self$importDataset(private$tableNameMetadata, 
                                              tibble(private$scenMetaColnames['scode'], 
                                                     scode, ">="), innerSepAND = FALSE))
                  }
                  return(self$importDataset(private$tableNameMetadata, 
                                                 tibble(private$scenMetaColnames['scode'], 
                                                        scode), innerSepAND = FALSE))
                  
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
                  scenList <- scenList[seq_len(limit), , drop = FALSE]
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
                    query <- unlist(lapply(subsetData, private$buildSQLSubsetString, 
                                           innerSep), use.names = FALSE)
                    if(length(query))
                      query <- paste(brackets[1], query, brackets[2],  
                                     collapse = outerSep)
                    else
                      return(character(0L))
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
                    bsEscaped <- gsub("\\", "\\\\\\\\", pattern, fixed = TRUE)
                    return(gsub("([.|()^{}+$*?'\"]|\\[|\\])", 
                                "\\\\\\\\\\1", stringi::stri_escape_unicode(bsEscaped)))
                  }
                },
                escapePattern = function(pattern){
                  pattern <- gsub("([%_\\])", "\\\\\\1", pattern)
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
                      if(affectedRows > 0L)
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
                  flog.debug("Db: Database connection ended as Db object was gced.")
                }
              ),
              active = list(
                accessGroups= function(groups){
                  if(missing(groups))
                    return(private$userAccessGroups)
                  
                  stopifnot(is.character(groups), length(groups) >=1)
                  private$userAccessGroups <- unique(groups)
                }
              ),
              private = list(
                conn                = NULL,
                connectionInfo      = NULL,
                uid                 = character(1L),
                modelName           = character(1L),
                modelNameDb         = character(1L),
                dbSchema            = vector("list", 3L),
                scenMetaColnames    = character(1L),
                slocktimeIdentifier = character(1L),
                userAccessGroups    = character(1L),
                tableNameMetadata   = character(1L),
                tableNameScenLocks  = character(1L),
                tableNamesScenario  = character(1L),
                slocktimeLimit      = character(1L),
                hcubeActive         = logical(1L),
                info                = new.env(),
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
                                         paste0("%,", self$escapePattern(vector), ",%"), 
                                         "LIKE")
                  return(subsetClause)
                },
                buildRSubsetString = function(dataFrame, sep = " "){
                  if(!length(dataFrame) || !nrow(dataFrame)){
                    return(NULL)
                  }else if(length(dataFrame) < 3L){
                    dataFrame[[3]] <- "="
                  }
                  fields  <- dataFrame[[1]]
                  valsRaw <- dataFrame[[2]]
                  vals    <- self$escapePatternPivot(valsRaw)
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
                           "!=" = {
                             if(is.na(valsRaw[i]) || (length(valsRaw[i]) && !nchar(valsRaw[i]))){
                               return(paste0("!is.na(", field, ")"))
                             }
                             valNum <- suppressWarnings(as.numeric(valsRaw[i]))
                             if(is.na(valNum)){
                               val <- paste0("'", val, "'")
                             }else{
                               val <- valNum
                             }
                             return(paste0(field, "!=", val))
                           },
                           "=" = {
                             if(is.na(valsRaw[i]) || (length(valsRaw[i]) && !nchar(valsRaw[i]))){
                               return(paste0("is.na(", field, ")"))
                             }
                             valNum <- suppressWarnings(as.numeric(valsRaw[i]))
                             if(is.na(valNum)){
                               val <- paste0("'", val, "'")
                             }else{
                               val <- valNum
                             }
                             return(paste0(field, "==", val))
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
                             valNum <- suppressWarnings(as.numeric(valsRaw[i]))
                             if(is.na(valNum)){
                               val <- paste0("'", val, "'")
                             }else{
                               val <- valNum
                             }
                             return(paste0(field, op, val))
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
                  operator <- dataFrame[[3]]
                  if(!inherits(private$conn, "PqConnection")){
                    # SQLite needs explicit mention of escape character in clause
                    vals[operator == "LIKE"] <- paste0(vals[operator == "LIKE"], "  ESCAPE '\\'")
                  }
                  query <- paste(paste(fields, operator, vals), collapse = sep)
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
                                                                            private$dbSchema$tabName[["_jobMeta"]],
                                                                            private$tableNameScenLocks,
                                                                            private$tableNamesScenario,
                                                                            private$dbSchema$tabName[['_scenTrc']])),
                                              collapse = ", "),
                                        ") OR table_name LIKE ", 
                                        dbQuoteString(private$conn, self$escapePattern(private$modelNameDb) %+% "\\_%"), 
                                        ");"))
                  }else{
                    query <- SQL(paste0("SELECT name FROM sqlite_master WHERE type = 'table'",
                                        " AND (name IN (", 
                                        paste(dbQuoteString(private$conn, c(private$tableNameMetadata,
                                                                            private$dbSchema$tabName[["_jobMeta"]],
                                                                            private$tableNameScenLocks,
                                                                            private$tableNamesScenario,
                                                                            private$dbSchema$tabName[['_scenTrc']])),
                                              collapse = ", "),
                                        ") OR name LIKE ", 
                                        dbQuoteString(private$conn, self$escapePattern(private$modelNameDb) %+% "\\_%"), " ESCAPE '\\');"))
                  }
                  return(dbGetQuery(private$conn, query)[[1L]])
                },
                generateUserCredentials = function(tableName, uid, namespace){
                  self$exportDataset(tableName, tibble(uid = uid,
                                                       username = paste0(namespace, "_", uid),
                                                       password = stringi::stri_rand_strings(1L, 50L)))
                }
              )
)
