# R6 class for database related functions
Db <- R6Class("Db",
              public = list(
                initialize        = function(uid, host, username, password, dbname, uidIdentifier, sidIdentifier, 
                                             snameIdentifier, stimeIdentifier, slocktimeIdentifier, stagIdentifier,
                                             accessIdentifier, tableNameMetadata, tableNameScenLocks, 
                                             tableNamesScenario, slocktimeLimit, port = NULL, type = "postgres",
                                             tableNameTrace = NULL, traceColNames = NULL){
                  # Initialize database class
                  #
                  # Args:
                  #   uid:                 user ID
                  #   host:                host of the database
                  #   username :           database username
                  #   password:            password of the specified user
                  #   dbname:              name of database to connect to
                  #   uidIdentifier:       user ID column name
                  #   sidIdentifier:       scenario ID column name
                  #   snameIdentifier:     column name for scenario namecolumn
                  #   stimeIdentifier:     column name for scenario time column
                  #   slocktimeIdentifier: column name for scenario locktime column
                  #   stagIdentifier:      column name for scenario tags column
                  #   accessIdentifier:    column name for access information column 
                  #   tableNameMetadata:   name of the database table where scenario metadata is stored
                  #   tableNameScenLocks:  name of the table where scenario locks are saved
                  #   tableNamesScenario:  names of tables where scenario data is saved
                  #   slocktimeLimit:      maximum duration a lock is allowed to persist 
                  #                        (without being refreshed), before it will be deleted (optional)
                  #   port:                port to connect to (optional)
                  #   type:                type of database used (optional)
                  #   tableNameTrace:      table name where trace data is saved
                  #   traceColNames:       column names of trace data table
                  
                  #BEGIN error checks 
                  if(is.null(private$info$isInitialized)){
                    private$info$isInitialized <- 1
                  }else{
                    flog.error("Db: Tried to create more than one Db object.")
                    stop("An Object of class Db has already been initialized. Only one Db object allowed.", 
                         call. = FALSE)
                  }
                  stopifnot(is.character(uid), length(uid) == 1)
                  stopifnot(is.character(host), length(host) == 1)
                  stopifnot(is.character(username), length(username) == 1)
                  stopifnot(is.character(password), length(password) == 1)
                  stopifnot(is.character(dbname), length(dbname) == 1)
                  stopifnot(is.character(uidIdentifier), length(uidIdentifier) == 1)
                  stopifnot(is.character(sidIdentifier), length(sidIdentifier) == 1)
                  stopifnot(is.character(snameIdentifier), length(snameIdentifier) == 1)
                  stopifnot(is.character(stimeIdentifier), length(stimeIdentifier) == 1)
                  stopifnot(is.character(slocktimeIdentifier), length(slocktimeIdentifier) == 1)
                  stopifnot(is.character(stagIdentifier), length(stagIdentifier) == 1)
                  stopifnot(is.character(accessIdentifier), length(accessIdentifier) == 1)
                  stopifnot(is.character(tableNameMetadata), length(tableNameMetadata) == 1)
                  stopifnot(is.character(tableNameScenLocks), length(tableNameScenLocks) == 1)
                  stopifnot(is.character(tableNamesScenario), length(tableNamesScenario) >= 1)
                  stopifnot(is.numeric(slocktimeLimit), length(slocktimeLimit) >= 1)
                  if(!is.null(port)){
                    stopifnot(is.numeric(port) && length(port) == 1)
                  }
                  stopifnot(is.character(type), length(type) == 1)
                  if(!is.null(tableNameTrace)){
                    stopifnot(is.character(tableNameTrace), length(tableNameTrace) == 1)
                    stopifnot(is.character(traceColNames), length(traceColNames) >= 1)
                  }
                  #END error checks 
                  
                  private$uid                         <- uid
                  private$userAccessGroups            <- uid
                  private$scenMetaColnames['sid']     <- sidIdentifier
                  private$scenMetaColnames['uid']     <- uidIdentifier
                  private$scenMetaColnames['sname']   <- snameIdentifier
                  private$scenMetaColnames['stime']   <- stimeIdentifier
                  private$scenMetaColnames['stag']    <- stagIdentifier
                  private$scenMetaColnames['accessR'] <- accessIdentifier %+% "r"
                  private$scenMetaColnames['accessW'] <- accessIdentifier
                  private$slocktimeIdentifier         <- slocktimeIdentifier
                  private$tableNameMetadata           <- tableNameMetadata
                  private$tableNameScenLocks          <- tableNameScenLocks
                  private$tableNamesScenario          <- tableNamesScenario
                  private$slocktimeLimit              <- slocktimeLimit
                  private$traceConfig[['tabName']]    <- tableNameTrace
                  private$traceConfig[['colNames']]   <- traceColNames
                  
                  if(type == "postgres"){
                    tryCatch({
                      private$conn <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname, host = host, 
                                                     port = port, user = username, password = password)
                    }, error = function(e){
                      stop(sprintf("Db: Database connection could not be established. Error message: %s", e), 
                           call. = FALSE)
                    })
                  }else{
                    stop(sprintf("Db: A non supported database type (%s) was specified. Could not establish connection.", 
                                 type), call. = FALSE) 
                  }
                },
                getConn               = function() private$conn,
                getUid                = function() private$uid,
                getScenMetaColnames   = function() private$scenMetaColnames,
                getSlocktimeIdentifier= function() private$slocktimeIdentifier,
                getTableNameMetadata  = function() private$tableNameMetadata,
                getTableNameScenLocks = function() private$tableNameScenLocks,
                getTableNamesScenario = function() private$tableNamesScenario,
                getTraceConfig        = function() private$traceConfig,
                getMetadata           = function(uid, sname, stime, stag = character(0L), 
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
                  
                  metadata <- tibble(uid, sname, stime)
                  names(metadata) <- c(uidAlias, snameAlias, stimeAlias)
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
                    c(private$scenMetaColnames['uid'], private$scenMetaColnames['sname']), 
                    c(uid, sname)), count = TRUE, limit = 1L)[[1]]
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
                  # Fetch the latest exported sid from database 
                  # This is a small wrapper around private getMaximum method
                  #
                  # Args:
                  # 
                  # Returns:
                  # integer: latest scenario Id exported to database
                  private$getMaximum(private$tableNameMetadata, private$scenMetaColnames['sid'])
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
                  prog <- shiny::Progress$new()
                  on.exit(prog$close())
                  prog$set(message = msgProgress$title, value = 0)
                  incAmount <- 1/(length(sids) * length(private$tableNamesScenario))
                  updateProgress <- function(detail = NULL) {
                    prog$inc(amount = incAmount, detail = detail)
                  }
                  scenData <- lapply(seq_along(sids), function(i){
                    lapply(private$tableNamesScenario, function(tableName){
                      dataset <- self$importDataset(tableName = tableName, 
                                                    tibble(private$scenMetaColnames['sid'], 
                                                           sids[i]), limit = limit)
                      # remove metadata column
                      dataset[, private$scenMetaColnames['sid']] <- NULL
                      # increment progress bar
                      updateProgress(detail = msgProgress$progress %+% i)
                      return(dataset)
                    })
                  })
                  return(scenData)
                },
                deleteRows        = function(tableName, colNames, values){
                  # remove rows from table where rows have given values
                  #
                  # Args:
                  #   tableName:        name of the table where entries should be removed from
                  #   colNames:         character vector of column names
                  #   values:           character vector of values that should be removed
                  #
                  # Returns:
                  #   Db object: invisibly returns reference to object in case of success, 
                  #   throws exception if error
                  
                  #BEGIN error checks 
                  stopifnot(is.character(tableName), length(tableName) == 1)
                  stopifnot(is.character(colNames), length(colNames) >= 1)
                  values <- as.character(values)
                  stopifnot(is.character(values), length(values) >= 1)
                  #END error checks 
                  
                  affectedRows <- 0
                  subsetRows <- DBI::SQL(paste(paste(DBI::dbQuoteIdentifier(private$conn, colNames), 
                                                     DBI::dbQuoteString(private$conn, values), sep = " = "), 
                                               collapse = " AND "))
                  if(DBI::dbExistsTable(private$conn, tableName)){
                    tryCatch({
                      query <- paste0("DELETE FROM ", DBI::dbQuoteIdentifier(private$conn, tableName),
                                      " WHERE ", subsetRows)
                      DBI::dbExecute(private$conn, query)
                      flog.debug("Db: %s rows in table: '%s' were deleted. (Db.deleteRows)", affectedRows, tableName)
                    }, error = function(e){
                      stop(sprintf("Db: An error occurred while deleting rows from the database (Db.deleteRows, " %+% 
                                     "table: '%s'). Error message: %s.", tableName, e), call. = FALSE)
                    })
                  }
                  invisible(self)
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
                    return(data.frame())
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
                    if(!is.null(subsetSids) && length(subsetSids) >= 1L){
                      innerJoin <- " INNER JOIN (VALUES " %+% paste("(" %+% subsetSids, collapse = "), ") %+%
                        ")) vals(_v) ON " %+%  DBI::dbQuoteIdentifier(private$conn, 
                                                                      private$scenMetaColnames['sid']) %+% "=_v"
                      
                    }
                    tryCatch({
                      if(length(dots)){
                        subsetRows <- self$buildRowSubsetSubquery(dots, innerSep, outerSep)
                        sql     <- DBI::SQL(paste0("SELECT ", colNames, " FROM ", 
                                                   DBI::dbQuoteIdentifier(private$conn, tableName),
                                                   innerJoin, if(length(subsetRows)) " WHERE ", 
                                                   subsetRows, " LIMIT ?lim ;"))
                      }else{
                        sql     <- DBI::SQL(paste0("SELECT ", colNames, " FROM ",
                                                   DBI::dbQuoteIdentifier(private$conn, tableName), 
                                                   innerJoin, " LIMIT ?lim ;"))
                      }
                      query   <- DBI::sqlInterpolate(private$conn, sql, lim = limit)
                      dataset <- as_tibble(DBI::dbGetQuery(private$conn, query))
                      flog.debug("Db: Data was imported from table: '%s' (Db.importDataset).", tableName)
                    }, error = function(e){
                      stop(sprintf("Db: An error occurred while querying the database (Db.importDataset, " %+%
                                     "table: '%s'). Error message: %s.",
                                   tableName, e), call. = FALSE)
                    })
                  }
                  return(dataset)
                },
                exportScenDataset       = function(dataset, tableName){
                  # Saves scenario dataset to database
                  #
                  # Args:
                  #   dataset:             dataframe to save
                  #   tableName:           name of the table to export dataframe to
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
                    flog.debug("Db: A database table named: '%s' did not yet exist. 
        Therefore it was created (Db.exportScenDataset).", tableName)
                    tryCatch({
                      DBI::dbWriteTable(private$conn, tableName, dataset, row.names = FALSE, append = FALSE, 
                                        field.types = private$getFieldTypes(dataset))
                      query    <- paste0("ALTER TABLE ", DBI::dbQuoteIdentifier(private$conn, tableName), 
                                         " ADD CONSTRAINT foreign_key FOREIGN KEY (", 
                                         DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), 
                                         ") REFERENCES ",
                                         DBI::dbQuoteIdentifier(private$conn, private$tableNameMetadata), 
                                         "(",
                                         DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), 
                                         ") ON DELETE CASCADE;")
                      DBI::dbExecute(private$conn, query)
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
                  #   metadata:     dataframe containing metadata for one up 
                  #                 to many scenarios
                  #   update:       whether existing scenario metadata shall be updated
                  #
                  # Returns:
                  #   reference to itself (Db R6 object)
                  
                  # BEGIN error checks
                  stopifnot(inherits(metadata, "data.frame"))
                  # END error checks
                  
                  if(!DBI::dbExistsTable(private$conn, private$tableNameMetadata)){
                    tryCatch({
                      query <- paste0("CREATE TABLE ", 
                                      DBI::dbQuoteIdentifier(private$conn, private$tableNameMetadata), 
                                      " (", 
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), 
                                      " serial PRIMARY KEY,",
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['uid']), 
                                      " varchar(50) NOT NULL,", 
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sname']), 
                                      " varchar(255) NOT NULL,",
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['stime']), 
                                      " timestamp with time zone,",
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['stag']), 
                                      " text,",
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['accessR']), 
                                      " text NOT NULL,",
                                      DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['accessW']), 
                                      " text NOT NULL);")
                      DBI::dbExecute(private$conn, query)
                    }, error = function(e){
                      stop(sprintf("Metadata table could not be created (Scenario.writeMetadata). " %+%
                                     "Error message: %s.", e), call. = FALSE)
                    })
                    flog.debug("Db: A table named: '%s' did not yet exist (Scenario.writeMetadata). " %+%
                                 "Therefore it was created.", private$tableNameMetadata)
                  }
                  if(update){
                    sql     <- DBI::SQL(paste0("UPDATE ", DBI::dbQuoteIdentifier(private$conn, 
                                                                                 private$tableNameMetadata), 
                                               " SET (", 
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['uid']), ", ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sname']), ", ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['stime']), ", ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['stag']), ", ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['accessR']), ", ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['accessW']), ") = (",
                                               "?uid, ?sname, ?stime, ?stag, ?accessR, ?accessW) WHERE ",
                                               DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), " = ?sid;"))
                    query   <- DBI::sqlInterpolate(private$conn, sql, uid = metadata[[private$scenMetaColnames['uid']]],
                                                   sname = metadata[[private$scenMetaColnames['sname']]][[1]], 
                                                   stime = as.character(metadata[[private$scenMetaColnames['stime']]][[1]]), 
                                                   stag = metadata[[private$scenMetaColnames['stag']]][[1]], 
                                                   accessR = metadata[[private$scenMetaColnames['accessR']]][[1]], 
                                                   accessW = metadata[[private$scenMetaColnames['accessW']]][[1]], 
                                                   sid = metadata[[private$scenMetaColnames['sid']]][[1]])
                    tryCatch({
                      DBI::dbExecute(private$conn, query)
                      flog.debug("Db: Metadata (table: '%s') was written to database. (Db.writeMetadata).", 
                                 private$tableNameMetadata)
                    }, error = function(e){
                      stop(sprintf("Db: Metadata (table: '%s') could not " %+% "
        be written to database (Db.writeMetadata). Error message: %s.", 
                                   private$tableNameMetadata, e), call. = FALSE)
                    })
                  }else{
                    # write new metadata
                    tryCatch({
                      DBI::dbWriteTable(private$conn, private$tableNameMetadata, 
                                        metadata, row.names = FALSE, append = TRUE)
                      flog.debug("Db: Metadata (table: '%s') was written to database. (Db.writeMetadata).", 
                                 private$tableNameMetadata)
                    }, error = function(e){
                      stop(sprintf("Db: Metadata (table: '%s') could not " %+% "
        be written to database (Db.writeMetadata). Error message: %s.", 
                                   private$tableNameMetadata, e), call. = FALSE)
                    })
                  }
                  
                  invisible(self)
                },
                fetchScenList = function(noBatch = FALSE){
                  # returns list of scenarios that the current user has access to
                  #
                  # Args:
                  #   noBatch:           boolean that specifies whether to include scenarios that 
                  #                      have been solved in batch mode or not (optional)
                  #
                  # Returns:
                  #   tibble: tibble with all scenarios user has access to read as well 
                  #   as their metadata, throws exception in case of error
                  stopifnot(is.logical(noBatch), length(noBatch) == 1L)
                  
                  accessRights <- tibble(private$scenMetaColnames['accessR'],
                                      private$userAccessGroups, "=")
                  if(noBatch){
                    noBatchRuns <- tibble(private$scenMetaColnames['sname'], 
                                         "[0-9a-z]{64}", "NOT SIMILAR TO")
                  }
                  
                  scenList <- self$importDataset(private$tableNameMetadata, accessRights, 
                                                 noBatchRuns, innerSepAND = FALSE)
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
                  print(orderBy)
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
                  query <- paste(brackets[1], vapply(subsetData, private$buildSQLSubstring, 
                                        character(1L), innerSep, SQL,
                                        USE.NAMES = FALSE), brackets[2],  
                                 collapse = outerSep)
                  if(SQL){
                    return(DBI::SQL(query))
                  }else{
                    return(query)
                  }
                },
                escapePattern = function(pattern){
                  pattern <- gsub("([%_\\])", "\\\\\\1", pattern)
                  return(pattern)
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
                uid                 = character(0L),
                scenMetaColnames    = character(0L),
                slocktimeIdentifier = character(0L),
                userAccessGroups    = character(0L),
                tableNameMetadata   = character(0L),
                tableNameScenLocks  = character(0L),
                tableNamesScenario  = character(0L),
                slocktimeLimit      = character(0L),
                traceConfig         = vector("list", 2L),
                info                = new.env(),
                isValidSubsetGroup  = function(dataFrame){
                  if(inherits(dataFrame, "data.frame") 
                     && length(dataFrame) <= 3L
                     && is.character(dataFrame[[1]])
                     && (is.character(dataFrame[[2]])
                         || is.numeric(dataFrame[[2]]))){
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
                buildSQLSubstring = function(dataFrame, sep = " ", SQL = TRUE){
                  if(length(dataFrame) < 3L){
                    dataFrame[[3]] <- "="
                  }
                  fields <- dataFrame[[1]]
                  vals   <- as.character(dataFrame[[2]])
                  if(SQL){
                    fields <- dbQuoteIdentifier(private$conn, fields)
                    vals   <- dbQuoteString(private$conn, vals)
                  }else{
                    fields <- "`" %+% fields %+% "`"
                  }
                  if(identical(length(dataFrame), 4L) && SQL){
                    fields <- dbQuoteIdentifier(private$conn, dataFrame[[4]]) %+%
                      "."  %+% fields 
                  }
                  query <- paste(paste(fields, dataFrame[[3]], vals), collapse = sep)
                  if(SQL){
                    return(SQL(query))
                  }else{
                    return(query)
                  }
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
                    max   <- suppressWarnings(as.integer(DBI::dbGetQuery(private$conn, query)[[1]][1]))
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
                }
              )
)
