# R6 class for database related functions
Db <- R6Class("Db",
              public = list(
                initialize        = function(uid, host, username, password, dbname, uidIdentifier, sidIdentifier, 
                                             snameIdentifier, stimeIdentifier, slocktimeIdentifier, stagIdentifier,
                                             accessIdentifier, tableNameMetadata, tableNameScenLocks, 
                                             tableNamesScenario, slocktimeLimit, port = NULL, type = "postgres"){
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
                  #   slocktimeLimit:      maximum duration a lock is allowed to persist (without being refreshed), before it will be deleted (optional)
                  #   port:                port to connect to (optional)
                  #   type:                type of database used (optional)
                  
                  #BEGIN error checks 
                  if(is.null(private$info$isInitialized)){
                    private$info$isInitialized <- 1
                  }else{
                    flog.error("Db: Tried to create more than one Db object.")
                    stop("An Object of class Db has already been initialized. Only one Db object allowed.", call. = FALSE)
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
                  stopifnot(!is.null(port) || (is.numeric(port) && length(port) == 1))
                  stopifnot(is.character(type), length(type) == 1)
                  #END error checks 
                  
                  private$uid                 <- uid
                  private$userAccessGroups    <- uid
                  private$uidIdentifier       <- uidIdentifier
                  private$sidIdentifier       <- sidIdentifier
                  private$snameIdentifier     <- snameIdentifier
                  private$stimeIdentifier     <- stimeIdentifier
                  private$slocktimeIdentifier <- slocktimeIdentifier
                  private$stagIdentifier      <- stagIdentifier
                  private$accessIdentifier    <- accessIdentifier
                  private$tableNameMetadata   <- tableNameMetadata
                  private$tableNameScenLocks  <- tableNameScenLocks
                  private$tableNamesScenario  <- tableNamesScenario
                  private$slocktimeLimit      <- slocktimeLimit
                  
                  # specify column names for readonly permission column
                  private$accessRIdentifier   <- paste0(accessIdentifier, "r")
                  
                  if(type == "postgres"){
                    tryCatch({
                      private$conn <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname, host = host, port = port, user = username, password = password)
                    }, error = function(e){
                      stop(sprintf("Db: Database connection could not be established. Error message: %s", e), call. = FALSE)
                    })
                  }else{
                    stop(sprintf("Db: A non supported database type (%s) was specified. Could not establish connection.", type), call. = FALSE) 
                  }
                },
                getConn               = function() private$conn,
                getUid                = function() private$uid,
                getUidIdentifier      = function() private$uidIdentifier,
                getSidIdentifier      = function() private$sidIdentifier,
                getSnameIdentifier    = function() private$snameIdentifier,
                getStimeIdentifier    = function() private$stimeIdentifier,
                getSlocktimeIdentifier= function() private$slocktimeIdentifier,
                getStagIdentifier     = function() private$stagIdentifier,
                getAccessIdentifier   = function() private$accessIdentifier,
                getAccessRIdentifier  = function() private$accessRIdentifier,
                getTableNameMetadata  = function() private$tableNameMetadata,
                getTableNameScenLocks = function() private$tableNameScenLocks,
                getTableNamesScenario = function() private$tableNamesScenario,
                getMetadata           = function(uid, sname, stime, uidAlias = private$uidIdentifier, 
                                                 snameAlias = private$snameIdentifier, stimeAlias = private$stimeIdentifier){
                  # Generate dataframe containing scenario metadata
                  #
                  # Args:
                  #   uid:                      user ID
                  #   sname:                    name of the scenario
                  #   stime:                    time the scenario was generated
                  #   uidAlias:                 User ID Description
                  #   snameAlias:               Scenario name Description
                  #   stimeAlias:               Scenario time description
                  #
                  # Returns:
                  #   dataframe with metadata
                  
                  #BEGIN error checks
                  stopifnot(is.character(uid), length(uid) == 1)
                  stopifnot(is.character(sname), length(sname) == 1)
                  stime <- as.POSIXct(stime)
                  stopifnot(inherits(stime, "POSIXct"), length(stime) == 1)
                  stopifnot(is.character(uidAlias), length(uidAlias) == 1)
                  stopifnot(is.character(snameAlias), length(snameAlias) == 1)
                  stopifnot(is.character(stimeAlias), length(stimeAlias) == 1)
                  #END error checks
                  
                  metadata <- data.frame(uid, sname, stime, stringsAsFactors = F)
                  names(metadata) <- c(uidAlias, snameAlias, stimeAlias)
                  
                  return(metadata)
                },
                checkSnameExists      = function(sname, uid = NULL){
                  # test whether scenario with the given name already exists for the user specified
                  # 
                  # Args:
                  #   sname:                scenario name
                  #   uid:                  user ID (optional)
                  #
                  # Returns: 
                  #   boolean:              TRUE if id exists, FALSE otherwise, throws exception in case of error
                  
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
                    flog.debug("Db: Db.checkScenExists returns FALSE (SID does not yet exist as metadata table does not exist either)")
                    return(FALSE)
                  }
                  
                  # check whether scenario name already exists
                  scenExists <- self$importDataset(private$tableNameMetadata, colNames = c(private$uidIdentifier, private$snameIdentifier), 
                                                    values = c(uid, sname), count = TRUE, limit = 1L)[[1]]
                  if(scenExists >= 1){
                    flog.trace("Db: Scenario with name: '%s' alreaddy exists for user: '%s' (Db.checkScenExists returns FALSE).", sname, uid)
                    return(TRUE)
                  }else{
                    flog.trace("Db: Scenario with name: '%s' does not yet exist for user: '%s' (Db.checkScenExists returns TRUE).", sname, uid)
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
                  
                  metadataRow <- self$importDataset(private$tableNameMetadata, 
                                                    c(private$uidIdentifier, 
                                                      private$snameIdentifier),
                                                    c(uid, sname), limit = 1)
                  if(nrow(metadataRow)){
                    return(as.integer(metadataRow[[private$sidIdentifier]][1]))
                  }else{
                    return(0L)
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
                  prog <- shiny::Progress$new()
                  on.exit(prog$close())
                  prog$set(message = msgProgress$title, value = 0)
                  incAmount <- 1/(length(sids) * length(private$tableNamesScenario))
                  updateProgress <- function(detail = NULL) {
                    prog$inc(amount = incAmount, detail = detail)
                  }
                  scenData <- lapply(seq_along(sids), function(i){
                    lapply(private$tableNamesScenario, function(tableName){
                      dataset <- self$importDataset(tableName = tableName, colNames = private$sidIdentifier, 
                                                    values = sids[i], limit = limit)
                      # remove metadata column
                      dataset[, private$sidIdentifier] <- NULL
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
                  #   Db object: invisibly returns reference to object in case of success, throws exception if error
                  
                  #BEGIN error checks 
                  stopifnot(is.character(tableName), length(tableName) == 1)
                  stopifnot(is.character(colNames), length(colNames) >= 1)
                  values <- as.character(values)
                  stopifnot(is.character(values), length(values) >= 1)
                  #END error checks 
                  
                  affectedRows <- 0
                  subsetRows <- DBI::SQL(paste(paste(DBI::dbQuoteIdentifier(private$conn, colNames), 
                                                     DBI::dbQuoteString(private$conn, values), sep = " = "), collapse = " AND "))
                  if(DBI::dbExistsTable(private$conn, tableName)){
                    tryCatch({
                      query <- paste0("DELETE FROM ", DBI::dbQuoteIdentifier(private$conn, tableName),
                                      " WHERE ", subsetRows)
                      DBI::dbExecute(private$conn, query)
                      flog.debug("Db: %s rows in table: '%s' were deleted. (Db.deleteRows)", affectedRows, tableName)
                    }, error = function(e){
                      stop(sprintf("Db: An error occurred while deleting rows from the database (Db.deleteRows, table: '%s'). Error message: %s.", tableName, e), call. = FALSE)
                    })
                  }
                  invisible(self)
                },
                importDataset = function(tableName, colNames = NULL, values = NULL, cols = NULL,
                                         count = FALSE, limit = 1e7, union = FALSE){
                  # Import the data corresponding to the table name provided from the database by considering scenario IDs specified.
                  #
                  # Args:
                  #   tableName :       name of the table to import dataframe from
                  #   colNames:         column names to subset on (optional)
                  #   cols:             column names to select, if NULL all will be selected (optional)
                  #   values:           values to subset on (optional)
                  #   count :           boolean that specifies whether to merely count number of rows or return actual values (optional)
                  #   limit:            maxmimum number of rows to fetch (optional)
                  #   union:            boolean that specifies whether subset of rows should be union (OR) (optional)
                  #                     or intersection (AND) of WHERE clauses (optional)
                  #
                  # Returns:
                  #   tibble: dataset (cleaned of metadata columns) with data coming from the table selected.
                  #   In case table does not exist or no rows in dataset, returns NULL
                  
                  #BEGIN error checks 
                  stopifnot(is.character(tableName), length(tableName) == 1)
                  if(!is.null(colNames)){
                    stopifnot(is.character(colNames), length(colNames) >= 1)
                    values <- as.character(values)
                    stopifnot(is.character(values), length(values) >= 1)
                  }
                  if(!is.null(cols)){
                    stopifnot(is.character(cols), length(cols) >= 1)
                  }
                  stopifnot(is.logical(count), length(count) == 1)
                  stopifnot(is.numeric(limit), length(limit) == 1)
                  stopifnot(is.logical(union), length(union) == 1)
                  #END error checks 
                  
                  
                  # check if table exists
                  if(!dbExistsTable(private$conn, tableName)){
                    flog.debug("Db: A table named: '%s' does not exist in the database (Db.importDataset).", tableName)
                    return(data.frame())
                  }else{
                    # select which columns to fetch
                    if(count){
                      cols <- "COUNT(*)"
                    }else if(!is.null(cols)){
                      cols <- paste0("(", paste(DBI::dbQuoteIdentifier(private$conn, colNames),
                                                  collapse = ","), ")")
                    }else{
                      cols <- "*"
                    }
                    # fetch dataframe
                    tryCatch({
                      if(!is.null(colNames)){
                        subsetRows <- DBI::SQL(paste(paste(DBI::dbQuoteIdentifier(private$conn, colNames), 
                                                           DBI::dbQuoteString(private$conn, values), sep = " = "), 
                                                     collapse = if(union) " OR " else " AND "))
                        sql     <- paste0("SELECT ", cols, " FROM ", 
                                          DBI::dbQuoteIdentifier(private$conn, tableName), " WHERE ", 
                                          subsetRows, " LIMIT ?lim ;")
                      }else{
                        sql     <- paste0("SELECT ", cols, " FROM ", 
                                          DBI::dbQuoteIdentifier(private$conn, tableName), " LIMIT ?lim ;")
                      }
                      query   <- DBI::sqlInterpolate(private$conn, sql, lim = limit)
                      dataset <- as_tibble(DBI::dbGetQuery(private$conn, query))
                      flog.debug("Db: Data was imported from table: '%s' (Db.importDataset).", tableName)
                    }, error = function(e){
                      stop(sprintf("Db: An error occurred while querying the database (Db.importDataset, table: '%s'). Error message: %s.",
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
                  
                  ## remove existing data
                  if(DBI::dbExistsTable(private$conn, tableName)){
                    if(!is.null(dataset)){
                      # write data to database
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
                    # get field types for dataframe and change integer to numeric type
                    fieldTypes <- private$getFieldTypes(dataset)
                    # write data to database
                    tryCatch({
                      DBI::dbWriteTable(private$conn, tableName, dataset, row.names = FALSE, append = FALSE, 
                                        field.types = fieldTypes)
                      query    <- paste0("ALTER TABLE ", DBI::dbQuoteIdentifier(private$conn, tableName), 
                                         " ADD CONSTRAINT foreign_key FOREIGN KEY (", 
                                         DBI::dbQuoteIdentifier(private$conn, private$sidIdentifier), ") REFERENCES ",
                                         DBI::dbQuoteIdentifier(private$conn, private$tableNameMetadata), "(",
                                         DBI::dbQuoteIdentifier(private$conn, private$sidIdentifier), ") ON DELETE CASCADE;")
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
                fetchScenList = function(){
                  # returns list of scenarios that the current user has access to
                  #
                  # Args:
                  #
                  # Returns:
                  #   tibble: tibble with all scenarios user has access to read as well as their metadata, throws exception in case of error
                  
                  scenList <- self$importDataset(private$tableNameMetadata, colNames = private$accessRIdentifier, 
                                                 values = private$userAccessGroups, union = TRUE)
                  #scenList[[private$accessIdentifier]]
                  return(scenList)
                  
                },
                formatScenList = function(scenList, orderBy = NULL, desc = FALSE){
                  # returns list of scenarios (formatted for dropdown menu)
                  #
                  # Args:
                  #   scenList:          dataframe with scenario metadata
                  #   orderBy:           column to use for ordering data frame (optional)
                  #   desc:              boolean that specifies whether ordering should be descending(FALSE) or ascending (TRUE) (optional)
                  #
                  # Returns:
                  #   character vector: named vector formatted to be used in dropdown menus, returns NULL in case no scenarios found
                  
                  #BEGIN error checks
                  if(!hasContent(scenList)){
                    return(NULL)
                  }
                  stopifnot(inherits(scenList, "data.frame"))
                  if(!is.null(orderBy)){
                    stopifnot(is.character(orderBy) && length(orderBy) == 1)
                  }
                  stopifnot(is.logical(desc), length(desc) == 1)
                  # END error checks
                  
                  if(!is.null(orderBy)){
                    if(desc){
                      scenList <- dplyr::arrange(scenList, desc(!!as.name(orderBy)))
                    }else{
                      scenList <- dplyr::arrange(scenList, !!as.name(orderBy))
                    }
                  }
                  
                  setNames(paste0(scenList[[private$sidIdentifier]], "_", scenList[[private$uidIdentifier]]), 
                           paste0(vapply(scenList[[private$uidIdentifier]], function(el){ if(identical(el, private$uid)) "" else paste0(el, ": ")}, 
                                         character(1), USE.NAMES = FALSE), scenList[[private$snameIdentifier]], " (", scenList[[private$stimeIdentifier]], ")"))
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
                uidIdentifier       = character(0L),
                sidIdentifier       = character(0L),
                snameIdentifier     = character(0L),
                stimeIdentifier     = character(0L),
                slocktimeIdentifier = character(0L),
                stagIdentifier      = character(0L),
                accessIdentifier    = character(0L),
                accessRIdentifier   = character(0L),
                userAccessGroups    = character(0L),
                tableNameMetadata   = character(0L),
                tableNameScenLocks  = character(0L),
                tableNamesScenario  = character(0L),
                slocktimeLimit      = character(0L),
                info                = new.env(),
                getFieldTypes = function(data){
                  # returns vector with field types of data frame and changes integer type to numeric
                  fieldTypes <- vapply(seq_along(data), function(i){
                    if(typeof(data[[i]]) == "integer" && !is.factor(data[[i]])
                       && !identical(names(data)[i], private$sidIdentifier)){
                      data[[i]] <- as.numeric(data[[i]])
                    }
                    return(DBI::dbDataType(private$conn, data[[i]]))
                  }, character(1), USE.NAMES = FALSE)
                  names(fieldTypes) <- names(data)
                  flog.trace("Db: Returned database field types: '%s'.", paste(fieldTypes, collapse = ", "))
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
                  #   numeric: largest value in the column specified, throws exception in case of error
                  
                  
                  if(!dbExistsTable(private$conn, tableName)){
                    return(0L)
                  }
                  tryCatch({
                    query <- DBI::SQL(paste0("SELECT ", DBI::dbQuoteIdentifier(private$conn, colName), " FROM ", DBI::dbQuoteIdentifier(private$conn, tableName), 
                                             " ORDER BY ", DBI::dbQuoteIdentifier(private$conn, colName), " DESC LIMIT 1;"))
                    max   <- suppressWarnings(as.integer(DBI::dbGetQuery(private$conn, query)[[1]][1]))
                  }, error = function(e){
                    flog.error("Db: An error occurred while querying the database (Db.getMaximum). Error message: %s.", e)
                    stop(sprintf("An error occurred while querying the database (Db.getMaximum). Error message: %s.", e), call. = FALSE)
                  })
                  
                  if(!is.na(max)){
                    return(max)
                  }else{
                    return(0L)
                  }
                }
              )
)