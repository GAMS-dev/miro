Auth <- R6Class("Auth", 
                public = list(
                  initialize      = function(conn, uid, defaultGroup, tableNameGroups, tableNameElements, tableNameHierarchy, tableNameMetadata, 
                                             uidIdentifier, accessIdentifier, accessElIdentifier, groupLimit = 20L){
                    # initialises Auth class
                    #
                    # Args:
                    #   conn:                database connection object
                    #   uid:                 user ID
                    #   defaultGroup:        name of the default user group
                    #   tableNameGroups:     name of the table where user access groups are stored
                    #   tableNameElements:   name of the table where access elements and permissions are stored
                    #   tableNameHierarchy:  name of the table where access group hierarchies are stored
                    #   tableNameMetadata:   name of the table where scenario metadata is stored
                    #   uidIdentifier:       name of userID column
                    #   accessIdentifier:    name of access identifier column
                    #   accessElIdentifier:  name of access element identifier column
                    #   groupLimit:          maximum number of access groups a user can belong to
                    
                    #BEGIN error checks 
                    stopifnot(inherits(conn, "PqConnection") || inherits(conn, "SQLiteConnection"))
                    stopifnot(is.character(uid), length(uid) == 1)
                    stopifnot(is.character(defaultGroup), length(defaultGroup) == 1)
                    stopifnot(is.character(tableNameGroups), length(tableNameGroups) == 1)
                    stopifnot(is.character(tableNameElements), length(tableNameElements) == 1)
                    stopifnot(is.character(tableNameHierarchy), length(tableNameHierarchy) == 1)
                    stopifnot(is.character(tableNameMetadata), length(tableNameMetadata) == 1)
                    stopifnot(is.character(uidIdentifier), length(uidIdentifier) == 1)
                    stopifnot(is.character(accessIdentifier), length(accessIdentifier) == 1)
                    stopifnot(is.character(accessElIdentifier), length(accessElIdentifier) == 1)
                    stopifnot(is.integer(groupLimit), length(groupLimit) == 1)
                    #END error checks 
                    
                    private$conn               <- conn
                    private$uid                <- uid
                    private$defaultGroup       <- defaultGroup
                    private$uidIdentifier      <- uidIdentifier
                    private$tableNameGroups    <- tableNameGroups
                    private$tableNameElements  <- tableNameElements
                    private$tableNameHierarchy <- tableNameHierarchy
                    private$tableNameMetadata  <- tableNameMetadata
                    private$uidIdentifier      <- uidIdentifier
                    private$accessIdentifier   <- accessIdentifier
                    private$accessElIdentifier <- accessElIdentifier
                    private$accessGroups       <- private$fetchAccessGroups(limit = groupLimit)
                    
                    # specify column names for read / write permission columns
                    private$accessRIdentifier   <- paste0(accessIdentifier, "r")
                  },
                  getAccessGroups = function() private$accessGroups,
                  getGroupVector  = function(accessGroup){
                    # given an access group, fetches all inherited access groups recursively
                    #
                    # Args:
                    #   accessGroup:        name of the parent access group
                    #
                    # Returns:
                    #   character vector: vector with strings corresponding to inherited user groups including parent group, throws exception if error
                    
                    #BEGIN error checks 
                    stopifnot(is.character(accessGroup), length(accessGroup) == 1)
                    stopifnot(DBI::dbExistsTable(private$conn, private$tableNameHierarchy))
                    #END error checks 
                    
                    #initialise return value
                    accessGroups     <- accessGroup
                    inheritedGroups  <- accessGroup
                    groupsToConsider <- accessGroup
                    repeat{
                      # atleast one inherited group was found, so call function recursively
                      tryCatch({
                        inheritedGroups <- private$getInheritedGroups(groupsToConsider[1], private$accessIdentifier)
                        flog.debug("Auth: %s: Inherited group IDs fetched for access group: '%s' (Auth.getGroupVector).", private$uid, accessGroup)
                      }, error = function(e){
                        flog.error("Auth: %s: An error occurred while attempting to fetch inherited access groups for group: '%s' (Auth.getGroupVector, table: '%s'). Error message: '%s'.", private$uid, accessGroup, private$tableNameHierarchy, e)
                        stop(sprintf("Auth: %s: An error occurred while attempting to fetch inherited access groups for group: '%s'. Error message: '%s'.", private$uid, accessGroup, e), call. = FALSE)
                      })
                      
                      groupsToConsider <- c(groupsToConsider[-1], inheritedGroups)
                      if(!length(groupsToConsider)){
                        # group has no inheritants, so break
                        break
                      }else{
                        # atleast one element was found in nested level, so add them
                        accessGroups <- c(accessGroups, inheritedGroups)
                        if(anyDuplicated(accessGroups)){
                          flog.error("Auth: %s: Node with more than one ancestor found in access group hierarchy graph. (Auth.getGroupVector, table: '%s').", private$uid, private$tableNameHierarchy)
                          stop("Access group hierarchy contains atleast one node with more than one ancestor. Please make sure every user group has only one ancestor.", call. = FALSE)
                        }
                      }
                    }
                    return(accessGroups)
                  },
                  checkAccess     = function(accessEl){
                    # checks whether user has sufficient privileges to access element
                    #
                    # Args:
                    #   accessEl:           element that privileges should be checked
                    #
                    # Returns:
                    #   logical: TRUE in case user has enough privileges to access element, FALSE in case not, throws exception if error
                    
                    #BEGIN error checks 
                    stopifnot(is.character(accessEl), length(accessEl) == 1)
                    #END error checks 
                    
                    if(DBI::dbExistsTable(private$conn, private$tableNameElements)){
                      tryCatch({
                        sql           <- paste0("SELECT COUNT(*) FROM ", DBI::dbQuoteIdentifier(private$conn, private$tableNameElements), 
                                                " WHERE ", DBI::dbQuoteIdentifier(private$conn, private$accessIdentifier), " IN (", 
                                                DBI::SQL(paste(DBI::dbQuoteString(private$conn, private$accessGroups), collapse = ", ")), ") AND ", 
                                                DBI::dbQuoteIdentifier(private$conn, private$accessIdentifier), " = ?el")
                        query         <- DBI::sqlInterpolate(private$conn, sql, el = private$accessIdentifier)
                        access        <- DBI::dbGetQuery(private$conn, query)[[1]]
                      }, error = function(e){
                        flog.error("Auth: %s: An error occurred while querying the database (Auth.checkAccess, table: '%s'). Error message: %s.", private$uid, private$tableNameElements, e)
                        stop(sprintf("Auth: %s: Some error occurred while fetching user access groups from the database. Error message: '%s'.", private$uid, e), call. = FALSE)
                      })
                    }else{
                      return(FALSE)
                    }
                    if(!is.null(access) && access == 1){
                      return(TRUE)
                    }else{
                      return(FALSE)
                    }
                  },
                  removeAccessEl  = function(accessEl){
                    # reemoves access element from database
                    #
                    # Args:
                    #   accessEl:           element that should get removed
                    #
                    # Returns:
                    #   logical: invisibly returns TRUE in case element was removed successfully, throws exception if not
                    
                    #BEGIN error checks 
                    stopifnot(is.character(accessEl), length(accessEl) == 1)
                    #END error checks 
                    
                    affectedRows <- 0
                    if(DBI::dbExistsTable(private$conn, private$tableNameElements)){
                      tryCatch({
                        sql          <- paste0("DELETE FROM ", DBI::dbQuoteIdentifier(private$conn, private$tableNameElements),
                                               " WHERE ", DBI::dbQuoteIdentifier(private$conn, private$accessElIdentifier), " = &el;")
                        query        <- DBI::sqlInterpolate(private$conn, sql, el = accessEl)
                        dbExecute(private$conn, sql)
                        flog.debug("Auth: %s: %d rows in table: '%s' were deleted. (Auth.removeAccessEl)", affectedRows, private$uid, private$tableNameElements)
                      }, error = function(e){
                        stop(sprintf("Auth: %s: An error occurred while querying the database (Auth.removeAccessEl, table: '%s'). Error message: %s", private$uid, private$tableNameElements, e), call. = FALSE)
                      })
                      ivisible(TRUE)
                    }else{
                      stop(sprintf("Auth: %s: The table: '%s' does not exist. (Auth.removeAccessEl).", private$uid, private$tableNameElements), call. = FALSE)
                    }
                    
                    
                  },
                  setAccess       = function(accessEl, newGroups){
                    # specifies  whether user has sufficient privileges to access element
                    #
                    # Args:
                    #   accessEl:           element that privileges should be checked
                    #   newGroups:          vector of new access groups for the element
                    #
                    # Returns:
                    #   logical: invisibly returns TRUE in case access groups were updated successfully, throws exception if not
                    
                    #BEGIN error checks 
                    stopifnot(is.character(accessEl), length(accessEl) == 1)
                    #END error checks 
                    
                    dataset           <- data.frame(accessEl, newGroups, stringsAsFactors = FALSE)
                    colnames(dataset) <- c(private$accessElIdentifier, private$accessIdentifier)
                    
                    # remove existing data
                    self$removeAccessEl(accessEl)
                    
                    # insert new data
                    tryCatch({
                      DBI::dbWriteTable(private$conn, private$tableNameElements, dataset, row.names = FALSE, append = TRUE)
                      flog.debug("Auth: %s: Access group(s) were added for element: '%s' (Auth.setAccess).", private$uid, accessEl)
                    }, error = function(e){
                      stop(sprintf("Auth: %s: An error occurred while attempting to set user access groups for element: '%s' (Auth.setAccess, table: '%s'). Error message: '%s'.", private$uid, accessEl, private$tableNameElements, e), call. = FALSE)
                    })
                    
                    return(invisible(TRUE))
                  },
                  importShared    = function(tableName, accessIdentifier = private$accessIdentifier, keyCol = character(0L),limit = 1e6){
                    # Imports shared dataset from specified table with only data that user has permission to see
                    #
                    # Args:
                    #   tableName :       name of the table to import dataframe from
                    #   limit:            maximum number of rows to fetch (optional)
                    #   keyCol:           column that specifies key (used for join)
                    #   accessIdentifier: name of the column where access permissions are specified
                    #
                    # Returns:
                    #   tibble with data coming from the table selected
                    
                    #BEGIN error checks 
                    stopifnot(is.character(tableName), length(tableName) == 1)
                    stopifnot(is.character(accessIdentifier), length(accessIdentifier) == 1)
                    stopifnot(is.character(keyCol), length(keyCol) <= 1)
                    stopifnot(is.numeric(limit), length(limit) == 1)
                    #END error checks 
                    dataset <- dplyr::tibble()
                    if(dbExistsTable(private$conn, tableName)) {
                      joinSQL <- ""
                      if(length(keyCol) && nchar(keyCol) && dbExistsTable(private$conn, paste0(tableName, "_data"))){
                        joinSQL <- paste0(" INNER JOIN ", dbQuoteIdentifier(private$conn, paste0(tableName, "_data")),
                                          " ON ", dbQuoteIdentifier(private$conn, tableName), ".", 
                                          dbQuoteIdentifier(private$conn, keyCol), " = ", 
                                          dbQuoteIdentifier(private$conn, paste0(tableName, "_data")), ".", 
                                          dbQuoteIdentifier(private$conn, keyCol))
                      }
                      tryCatch({
                        sql     <- paste0("SELECT * FROM ", dbQuoteIdentifier(private$conn, tableName), joinSQL, " WHERE ", 
                                          dbQuoteIdentifier(private$conn, tableName), ".",
                                          dbQuoteIdentifier(private$conn, accessIdentifier), " IN (", 
                                          SQL(paste(dbQuoteString(private$conn, private$accessGroups), collapse = ", ")), ") LIMIT ?lim ;")
                        query   <- DBI::sqlInterpolate(private$conn, sql, lim = limit)
                        dataset <- as_tibble(DBI::dbGetQuery(private$conn, query))
                      }, error = function(e){
                        flog.error("DB: %s: An error occurred while querying the database (Auth.importShared, table: '%s'). Error message: %s.", 
                                   private$uid, tableName, e)
                        stop(paste0("An error occurred while querying the database. Error message: ", e), call. = FALSE)
                      })
                    }else{
                      flog.debug("Auth: %s: The table: '%s' does not exist. (Auth.importShared).", private$uid, tableName)
                    }
                    return(dataset)
                  }
                ),
                private = list(
                  conn               = NULL,
                  uid                = character(0),
                  defaultGroup       = character(0),
                  accessGroups       = character(0),
                  tableNameGroups    = character(0),
                  tableNameElements  = character(0),
                  tableNameHierarchy = character(0),
                  tableNameMetadata  = character(0),
                  uidIdentifier      = character(0),
                  accessIdentifier   = character(0),
                  accessRIdentifier  = character(0),
                  accessElIdentifier = character(0),
                  getInheritedGroups = function(accessGroup, accessIdentifier){
                    # helper function that fetches user groups of next inheritance level
                    sql            <- paste0("SELECT ", DBI::dbQuoteIdentifier(private$conn, paste0(accessIdentifier, "2")), " FROM ", DBI::dbQuoteIdentifier(private$conn, private$tableNameHierarchy), 
                                             " WHERE ", DBI::dbQuoteIdentifier(private$conn, accessIdentifier), " = ?gid")
                    query          <- DBI::sqlInterpolate(private$conn, sql, gid = accessGroup)
                    return(DBI::dbGetQuery(private$conn, query)[[1]])
                  },
                  fetchAccessGroups    = function(limit){
                    # fetches users access groups from the database
                    
                    accessGroups <- c(private$uid, private$defaultGroup)
                    
                    if(DBI::dbExistsTable(private$conn, private$tableNameGroups)){
                      tryCatch({
                        sql           <- paste0("SELECT ", DBI::dbQuoteIdentifier(private$conn, private$accessIdentifier), " FROM ", DBI::dbQuoteIdentifier(private$conn, private$tableNameGroups), 
                                                " WHERE ", DBI::dbQuoteIdentifier(private$conn, private$uidIdentifier), " = ?uid LIMIT ?lim")
                        query         <- DBI::sqlInterpolate(private$conn, sql, uid = private$uid, lim = limit)
                        accessGroups  <- c(accessGroups, DBI::dbExecute(private$conn, query))
                      }, error = function(e){
                        stop(sprintf("Auth: %s: An error occurred while querying the database (Auth.getAccessGroups, table: '%s'). Error message: %s.", private$uid, private$tableNameGroups, e), call. = FALSE)
                      })
                    }
                    return(accessGroups)
                  }
                )
                
)