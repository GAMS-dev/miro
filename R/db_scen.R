# R6 class for Scenario object
Scenario <- R6Class("Scenario", 
                    inherit = Db,
                    public = list(
                      initialize                    = function(db, sid = NULL, sname = NULL,
                                                               readPerm = NULL, writePerm = NULL,
                                                               tags = NULL, overwrite = FALSE){
                        # Initialize scenario class
                        #
                        # Args:
                        #   db:                R6 Database object
                        #   sid:               scenario Id (optional)
                        #   sname:             scenario name (optional)
                        #   readPerm:          users/groups that have read permissions (optional)
                        #   writePerm:         users/groups that have write permissions (optional)
                        #   tags:              vector of tags that can be specified (optional)
                        #   overwrite:         logical that specifies whether data should be overwritten or appended
                        
                        #BEGIN error checks 
                        stopifnot(is.R6(db))
                        private$conn <- db$getConn()
                        private$uid  <- db$getUid()
                        if(is.null(sid)){
                          stopifnot(is.character(sname), length(sname) == 1)
                          if(length(tags)){
                            stopifnot(is.character(tags), length(tags) >= 1)
                          }else{
                            tags <- character(0L)
                          }
                          # if permissions not explicitly set, restrict read/write access to active user
                          if(length(readPerm)){
                            stopifnot(is.character(readPerm), length(readPerm) >=1)
                          }else{
                            readPerm <- private$uid
                          }
                          if(length(writePerm)){
                            stopifnot(is.character(writePerm), length(writePerm) >=1)  
                          }else{
                            writePerm <- private$uid
                          }
                        }else{
                          sid <- suppressWarnings(as.integer(sid))
                          stopifnot(!is.na(sid), length(sid) == 1)
                        }
                        stopifnot(is.logical(overwrite), length(overwrite) == 1)
                        #END error checks 
                        
                        private$dbSchema            <- db$getDbSchema()
                        private$slocktimeLimit      <- db$getSlocktimeLimit
                        private$scenMetaColnames    <- db$getScenMetaColnames()
                        private$slocktimeIdentifier <- db$getSlocktimeIdentifier()
                        private$userAccessGroups    <- db$accessGroups
                        private$tableNameMetadata   <- db$getTableNameMetadata()
                        private$tableNameScenLocks  <- db$getTableNameScenLocks()
                        private$tableNamesScenario  <- db$getTableNamesScenario()
                        private$attachmentConfig    <- db$getAttachmentConfig()
                        private$tags                <- vector2Csv(unique(tags))
                        private$readPerm            <- vector2Csv(readPerm)
                        private$writePerm           <- vector2Csv(writePerm)
                        
                        if(is.null(sid)){
                          tryCatch({
                            private$fetchMetadata(sname = sname, uid = private$uid)
                            if(overwrite){
                              private$writeMetadata()
                            }
                            },
                                   error = function(e){
                                     # scenario with name/uid combination does not exist, so create it
                                     private$stime     <- Sys.time()
                                     private$suid      <- private$uid
                                     private$sname     <- sname
                                     private$writeMetadata()
                                     private$fetchMetadata(sname = sname, uid = private$uid)
                                   })
                        }else{
                          private$fetchMetadata(sid = sid)
                        }
                        # set/refresh lock for scenario
                        private$lock()
                      },
                      getSid      = function() private$sid,
                      getScenUid  = function() private$suid,
                      getScenName = function() private$sname,
                      getScenTime = function() private$stime,
                      getMetadata = function(aliases = character(0L), noPermFields = TRUE){
                        # Generates dataframe containing scenario metadata
                        #
                        # Args:
                        #   aliases:        list or named vector of metadata field aliases (optional)
                        #   noPermFields:   do not include scenario permission fields (optional)
                        #
                        # Returns:
                        #   dataframe with metadata
                        
                        #BEGIN error checks 
                        stopifnot(!is.null(private$sid))
                        stopifnot(is.logical(noPermFields), length(noPermFields) == 1L)
                        #END error checks
                        
                        stag <- character(0L)
                        readPerm <- character(0L)
                        writePerm <- character(0L)
                        
                        if(!noPermFields){
                          readPerm <- private$readPerm
                        }
                        if(!noPermFields){
                          writePerm <- private$writePerm
                        }
                        super$getMetadata(uid = private$suid, sname = private$sname, stime = private$stime,
                                          stag = private$tags, readPerm = readPerm, writePerm = writePerm,
                                          uidAlias = aliases[["uid"]], snameAlias = aliases[["sname"]], 
                                          stimeAlias = aliases[["stime"]],
                                          stagAlias = aliases[["stag"]], readPermAlias = aliases[["readPerm"]], 
                                          writePermAlias = aliases[["writePerm"]])
                      },
                      save = function(datasets, msgProgress){
                        # Saves multiple dataframes to database
                        #
                        # Args:
                        #   datasets :           dataframes to save
                        #   msgProgress:         title and progress info for the progress bar
                        #
                        # Returns:
                        #   object: invisibly returns reference to itself
                        
                        #BEGIN error checks
                        if(private$isReadonly()){
                          stop("Scenario is readonly. Saving failed.", call. = FALSE)
                        }
                        stopifnot(!is.null(private$sid))
                        stopifnot(length(private$tableNamesScenario) >= 1)
                        stopifnot(is.list(datasets), identical(length(datasets), length(private$tableNamesScenario)))
                        stopifnot(is.character(msgProgress$title), length(msgProgress$title) == 1)
                        stopifnot(is.character(msgProgress$progress), length(msgProgress$progress) == 1)
                        #END error checks 
                        
                        # initialize progress bar
                        prog <- shiny::Progress$new()
                        on.exit(prog$close())
                        prog$set(message = msgProgress$title, value = 0)
                        updateProgress <- NULL
                        incAmount      <- 1/length(private$tableNamesScenario)
                        updateProgress <- function(detail = NULL) {
                          prog$inc(amount = incAmount, detail = detail)
                        }
                        # save current time stamp
                        private$stime <- Sys.time()
                        # write scenario metadata
                        private$writeMetadata()
                        Map(function(dataset, tableName){
                          if(!is.null(dataset) && nrow(dataset)){
                            super$exportScenDataset(private$bindSidCol(dataset), tableName)
                          }
                          # increment progress bar
                          updateProgress(detail = msgProgress$progress)
                        }, datasets, private$tableNamesScenario)
                        
                        # refresh lock for scenario
                        private$lock()
                        
                        invisible(self)
                      },
                      saveTraceData = function(traceData){
                        # Saves trace data 
                        # 
                        # Args:
                        #   traceData:   data frame with trace data
                        #
                        # Returns:
                        #   R6 object (reference to itself)
                        
                        stopifnot(!is.null(private$sid))
                        
                        super$exportScenDataset(private$bindSidCol(traceData), private$dbSchema$tabName[["_scenTrc"]])
                        invisible(self)
                      },
                      addAttachments = function(filePaths, fileNames = NULL, forbiddenFnames = NULL){
                        # Saves attachments
                        # 
                        # Args:
                        #   filePaths:        character vector with file paths to read data from
                        #   fileNames:        names of the files in case custom name should be chosen
                        #   forbiddenFnames:  character vector with forbidden file names
                        #
                        # Returns:
                        #   R6 object (reference to itself)
                        
                        stopifnot(!is.null(private$sid))
                        stopifnot(is.character(filePaths), length(filePaths) >= 1L)
                        if(length(forbiddenFnames)){
                          stopifnot(is.character(forbiddenFnames), length(forbiddenFnames) >= 1L)
                        }
                        
                        if(!is.null(fileNames)){
                          stopifnot(is.character(fileNames), length(fileNames) >= 1L)
                        }else{
                          fileNames <- basename(filePaths)
                        }
                        
                        if(length(forbiddenFnames) && 
                           any(gsub("\\.[^\\.]+$", "", fileNames) %in% forbiddenFnames)){
                          stop("forbiddenFnameException", call. = FALSE)
                        }
                        
                        fileNamesDb    <- self$fetchAttachmentList()[["name"]]
                        
                        if(any(fileNames %in% fileNamesDb)){
                          stop("duplicateException", call. = FALSE)
                        }
                        if(length(fileNamesDb) + length(filePaths) > private$attachmentConfig[["maxNo"]]){
                          stop("maxNoException", call. = FALSE)
                        }
                        
                        attachmentData <- private$readBlob(filePaths, private$attachmentConfig[["maxSize"]], fileNames = fileNames)
                        
                        super$exportScenDataset(private$bindSidCol(attachmentData), private$dbSchema$tabName[["_scenAttach"]])
                        self$updateMetadata()
                        invisible(self)
                      },
                      fetchAttachmentList = function(){
                        # Fetches file names of saved attachments from database
                        # 
                        # Args:
                        #
                        # Returns:
                        #   tibble with columns name and execPerm
                        
                        stopifnot(!is.null(private$sid))
                        
                        attachments <- dplyr::arrange(super$importDataset(private$dbSchema$tabName[["_scenAttach"]], 
                                                           colNames = c("fileName", "execPerm"), 
                                                           subsetSids = private$sid), fileName)
                        if(length(attachments)){
                          return(tibble(name = attachments[[1]], execPerm = attachments[[2]]))
                        }else{
                          return(tibble(name = character(0L), execPerm = logical(0L)))
                        }
                      },
                      downloadAttachmentData = function(filePath, fileNames = NULL, 
                                                        fullPath = FALSE, allExecPerm = FALSE){
                        # Fetches attachment data from db
                        # 
                        # Args:
                        #   filePath:      1d character vector where to save files
                        #   fileNames:     character vector with names of the files to download (optional)
                        #   fullPath:      whether filePath includes file name + extension or not (optional)
                        #   allExecPerm:   whether to download all files with execution permission (optional)
                        #
                        # Returns:
                        #   R6 object (reference to itself)
                        
                        stopifnot(!is.null(private$sid))
                        stopifnot(is.character(filePath), length(filePath) == 1L)
                        stopifnot(is.logical(fullPath), length(fullPath) == 1L)
                        stopifnot(is.logical(allExecPerm), length(allExecPerm) == 1L)
                        
                        if(fullPath){
                          if(length(fileNames) != 1L){
                            stop("Only single file name allowed when full path is specified.", call. = FALSE)
                          }
                          filePaths <- filePath
                        }else{
                          filePaths <- file.path(filePath, fileNames)
                        }
                        
                        if(!allExecPerm){
                          stopifnot(is.character(fileNames), length(fileNames) >= 1L)
                        }
                        
                        data <- super$importDataset(private$dbSchema$tabName[["_scenAttach"]],
                                                    if(allExecPerm) 
                                                      tibble("execPerm", TRUE) 
                                                    else 
                                                      tibble(rep.int("fileName", length(fileNames)), fileNames),
                                                    colNames = c("fileName", "fileContent"), innerSepAND = FALSE,
                                                    subsetSids = private$sid)
                        if(length(data)){
                          if(allExecPerm){
                            filePaths <- file.path(filePath, data[["fileName"]])
                          }
                          Map(writeBin, data[["fileContent"]], filePaths)
                        }
                        
                        invisible(self)
                      },
                      setAttachmentExecPerm = function(fileName, value){
                        # Sets execute permission for particular attachment
                        # 
                        # Args:
                        #   fileName:  name of the file whose data to fetch
                        #   value:     logical that specifies whether data can be executed by GAMS
                        #
                        # Returns:
                        #   R6 object (reference to itself)
                        
                        stopifnot(!is.null(private$sid))
                        stopifnot(is.character(fileName), length(fileName) == 1L)
                        stopifnot(is.logical(value), length(value) == 1L)
                        
                        
                        super$updateRows(private$dbSchema$tabName[["_scenAttach"]], tibble("fileName", fileName), 
                                         colNames = "execPerm", values = value, subsetSids = private$sid)
                        self$updateMetadata()
                        invisible(self)
                      },
                      removeAttachments = function(fileNames){
                        # Deletes attachments from scenario
                        # 
                        # Args:
                        #   fileNames:   file names of attachments to remove
                        #
                        # Returns:
                        #   R6 object (reference to itself)
                        
                        stopifnot(!is.null(private$sid))
                        stopifnot(is.character(fileNames), length(fileNames) > 0L)
                        
                        super$deleteRows(private$dbSchema$tabName[["_scenAttach"]], "fileName", fileNames, 
                                         conditionSep = "OR", subsetSids = private$sid)
                        self$updateMetadata()
                        invisible(self)
                      },
                      delete = function(){
                        # Wrapper to deleteRows function to easily remove scenario from database
                        #
                        # Args:
                        #
                        # Returns:
                        #   R6 object: reference to itself,
                        #   throws exception in case of error
                        
                        #BEGIN error checks 
                        stopifnot(!is.null(private$sid))
                        #END error checks 
                        noErr <- TRUE
                        self$deleteRows(private$tableNameMetadata, 
                                        private$scenMetaColnames['sid'], 
                                        private$sid)
                        
                        flog.debug("Scenario: '%s' unlocked.", private$sid)
                        private$unlock()
                        
                        private$sid   <- integer(0)
                        
                        invisible(self)
                      },
                      updateMetadata = function(newName = character(0L), newTags = character(0L), 
                                                newReadPerm = character(0L), newWritePerm = character(0L)){
                        # Edit scenario metadata (wrapper around db.writeMetadata method)
                        #
                        # Args:
                        #   newName:      updated scenario name (optional)
                        #   newTags:      updated scenario tags (optional)
                        #   newReadPerm:  updated scenario read permissions (optional)
                        #   newWritePerm: updated scenario write permissions (optional)
                        
                        # Returns:
                        #   R6 object: reference to itself,
                        #   throws exception in case of error
                        
                        #BEGIN error checks 
                        stopifnot(is.character(newName), length(newName) <= 1L)
                        if(length(newTags)){
                          stopifnot(is.character(newTags)) 
                        }
                        stopifnot(is.character(newReadPerm))
                        stopifnot(is.character(newWritePerm))
                        #END error checks 
                        
                        if(length(newName)){
                          private$sname <- newName
                        }
                        if(length(newTags)){
                          private$tags <- vector2Csv(newTags)
                        }
                        if(length(newReadPerm)){
                          private$readPerm <- vector2Csv(newReadPerm)
                        }
                        if(length(newWritePerm)){
                          private$writePerm <- vector2Csv(newWritePerm)
                        }
                        
                        private$stime <- Sys.time()
                        metadata <- tibble(private$sid, private$suid, private$sname, 
                                           private$stime, private$tags, private$readPerm,
                                           private$writePerm)
                        names(metadata) <- private$scenMetaColnames
                        super$writeMetadata(metadata, update = TRUE)
                        
                        # refresh lock for scenario
                        private$lock()
                        
                        invisible(self)
                      },
                      finalize = function(){
                        if(length(private$sid)){
                          flog.debug("Scenario: '%s' unlocked.", private$sid)
                          private$unlock()
                        }
                      }
                    ),
                    active = list(
                      isReadonlyOrLocked = function(x){
                        # Determines whether scenario is either readonly or locked
                        #
                        # Args:
                        #
                        # Returns:
                        # logical: returns TRUE if scenario is either readonly or locked, FALSE otherwise
                        
                        #BEGIN error checks
                        stopifnot(missing(x))
                        stopifnot(!is.null(private$sid))
                        #END error checks
                        
                        if(private$isLocked() || private$isReadonly())
                          return(TRUE)
                        else
                          return(FALSE)
                      }
                    ),
                    private = list(
                      sid                 = integer(0L),
                      suid                = character(0L),
                      sname               = character(0L),
                      stime               = character(0L),
                      tags                = character(1L),
                      readPerm            = character(0L),
                      writePerm           = character(0L),
                      dbSchema            = vector("list", 3L),
                      fetchMetadata       = function(sid = NULL, sname = NULL, uid = NULL){
                        # fetches scenario metadata from database
                        #
                        # Args:
                        #   sid:           scenario ID (optional)
                        #   sname:         scenario name (optional)
                        #   uid:           user ID (optional)
                        #
                        #   object: invisibly returns reference to itself
                        
                        #BEGIN error checks
                        if(!is.null(sid)){
                          stopifnot(is.integer(sid), length(sid) == 1)
                        }else{
                          stopifnot(is.character(sname), length(sname) == 1)
                          stopifnot(is.character(uid), length(uid) == 1)
                        }
                        #END error checks
                        
                        if(!is.null(sid)){
                          metadata <- self$importDataset(private$tableNameMetadata, 
                                                         subsetSids = sid)
                          if(!nrow(metadata)){
                            stop(sprintf("A scenario with ID: '%s' could not be found.", sid), call. = FALSE)
                          }
                          private$suid      <- metadata[[private$scenMetaColnames['uid']]][1]
                          private$sname     <- metadata[[private$scenMetaColnames['sname']]][1]
                          private$stime     <- metadata[[private$scenMetaColnames['stime']]][1]
                          private$tags      <- metadata[[private$scenMetaColnames['stag']]][1]
                          private$readPerm  <- metadata[[private$scenMetaColnames['accessR']]][1]
                          private$writePerm <- metadata[[private$scenMetaColnames['accessW']]][1]
                        }else{
                          metadata <- self$importDataset(private$tableNameMetadata, 
                                                         tibble(c(private$scenMetaColnames['uid'],
                                                                  private$scenMetaColnames['sname']),
                                                                c(uid, sname)))
                          if(!nrow(metadata)){
                            stop(sprintf("A scenario with name: '%s' could not be found for user: '%s'.", 
                                         sname, uid), call. = FALSE)
                          }
                          private$suid      <- uid
                          private$sname     <- sname
                          private$stime     <- Sys.time()
                        }
                        private$sid       <- as.integer(metadata[[private$scenMetaColnames['sid']]][1])
                        
                        invisible(self)
                      },
                      getUidLock = function(){
                        # checks whether a scenario is currently locked
                        #
                        # Args:
                        #
                        # Returns:
                        #   character: returns uid of user who locked scenario in case scenario is locked and NA_character_ otherwise, throws exception in case of error
                        
                        #BEGIN error checks 
                        stopifnot(!is.null(private$sid))
                        #END error checks
                        
                        sid <- private$sid
                        
                        if(!DBI::dbExistsTable(private$conn, private$tableNameScenLocks)){
                          return(NA_character_)
                        }
                        lockData <- self$importDataset(private$tableNameScenLocks, 
                                                       tibble(private$scenMetaColnames['sid'], 
                                                              sid), limit = 2L)
                        if(!is.null(lockData) && nrow(lockData)){
                          if(nrow(lockData) > 1){
                            stop(sprintf("Db: %s: More than one lock was found for the scenario. This should never happen. (Scenario.getUidLock, table: '%s', scenario: '%s').", 
                                         private$uid, private$tableNameScenLocks, sid), call. = FALSE)
                          }
                          if(!is.null(private$slocktimeLimit)){
                            if((Sys.time() - as.POSIXct(lockData[[private$slocktimeIdentifier]])) > private$slocktimeLimit){
                              # lock has expired, so remove it
                              private$unlock()
                              return(NA_character_)
                            }else{
                              # scenario is locked and lock has not yet expired
                              return(lockData[[private$scenMetaColnames['uid']]])
                            }
                          }else{
                            # scenario is locked and no time limit was provided
                            return(lockData[[private$scenMetaColnames['uid']]])
                          }
                        }else{
                          # no lock found
                          return(NA_character_)
                        }
                      },
                      writeMetadata   = function(){
                        # Write scenario metadata to metadata table
                        #
                        # Args:
                        #
                        # Returns:
                        #   Db class object:  invisibly returns reference to object if successful, 
                        #   throws exception in case of error
                        if(!length(private$sid)){
                          # new scenario
                          metadata           <- data.frame(private$suid, private$sname,
                                                           private$stime, private$tags, 
                                                           private$readPerm, private$writePerm,
                                                           stringsAsFactors = FALSE)
                          colnames(metadata) <- private$scenMetaColnames[-1]
                        }else{
                          if(private$isReadonly()){
                            flog.error("Db: Metadata could not be overwritten as scenario is readonly (Scenario.writeMetadata).")
                            stop("Scenario is readonly. Saving failed.", call. = FALSE)
                          }
                          self$deleteRows(private$tableNameMetadata, 
                                          private$scenMetaColnames['sid'], 
                                          private$sid)
                          metadata           <- data.frame(private$sid, private$suid, private$sname,
                                                           private$stime, private$tags, 
                                                           private$readPerm, private$writePerm,
                                                           stringsAsFactors = FALSE)
                          colnames(metadata) <- private$scenMetaColnames
                        }
                        super$writeMetadata(metadata)
                        flog.debug("Db: Metadata (table: '%s') was added for scenario: '%s' (Scenario.writeMetadata).", 
                                   private$tableNameMetadata, private$sid, private$suid)
                        invisible(self)
                      },
                      lock = function(){
                        # adds new lock for scenario
                        #
                        # Args:
                        #
                        # Returns:
                        #   Db object: invisibly returns object reference in case lock was set successfully, throws exception otherwise
                        
                        #BEGIN error checks 
                        stopifnot(!is.null(private$sid))
                        #END error checks
                        
                        if(!DBI::dbExistsTable(private$conn, private$tableNameScenLocks)){
                          # in case table does not yet exist, create it
                          tryCatch({
                            query <- paste0("CREATE TABLE ", DBI::dbQuoteIdentifier(private$conn, private$tableNameScenLocks), 
                                            " (", 
                                            DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['uid']), 
                                            " text,",
                                            DBI::dbQuoteIdentifier(private$conn, private$scenMetaColnames['sid']), 
                                            " int UNIQUE,", 
                                            DBI::dbQuoteIdentifier(private$conn, private$slocktimeIdentifier), 
                                            if(inherits(private$conn, "PqConnection")) 
                                              " timestamp with time zone);" else " text);")
                            DBI::dbExecute(private$conn, query)
                            flog.debug("Db: %s: Table with locks ('%s') was created as it did not yet exist. (Scenario.lock)", private$uid, private$tableNameScenLocks)
                          }, error = function(e){
                            stop(sprintf("Db: %s: An error occurred while trying to create table. (Scenario.lock, table: '%s'). Error message: %s.", 
                                         private$uid, private$tableNameScenLocks, e), call. = FALSE)
                          })
                        }
                        # check whether scenario is already locked
                        uidLocked <- private$getUidLock()
                        if(!is.na(uidLocked)){
                          # a lock already exists for the scenario
                          if(!identical(uidLocked, private$uid)){
                            # user who has currently locked the scenario is not the same as the active user provided
                            stop(sprintf("Db: %s: The scenario: '%s' is already locked by user: '%s' (Scenario.lock).", private$uid, private$sid, uidLocked), call. = FALSE)
                          }else{
                            # user who currently has scenario locked is the same, so refresh lock
                            private$unlock()
                          }
                        }
                        lockData           <- data.frame(as.character(private$uid), as.integer(private$sid), Sys.time(), stringsAsFactors = FALSE)
                        colnames(lockData) <- c(private$scenMetaColnames['uid'], 
                                                private$scenMetaColnames['sid'], 
                                                private$slocktimeIdentifier)
                        lockData <- dateColToChar(private$conn, lockData)
                        tryCatch({
                          DBI::dbWriteTable(private$conn, private$tableNameScenLocks, lockData, row.names = FALSE, append = TRUE)
                          flog.debug("Db: %s: Lock was added for scenario: '%s' (Scenario.lock).", private$uid, private$sid)
                        }, error = function(e){
                          stop(sprintf("Db: %s: An error occurred writing to database (Scenario.lock, table: '%s', scenario: '%s'). Error message: %s", 
                                       private$uid, private$tableNameScenLocks, private$sid, e), call. = FALSE)
                        })
                        invisible(self)
                      },
                      unlock = function(){
                        # removes lock for scenario
                        #
                        # Args:
                        #
                        # Returns:
                        #   Db object: invisibly returns object reference in case lock was removed, throws exception in case something went wrong
                        
                        #BEGIN error checks
                        stopifnot(!is.null(private$sid))
                        #END error checks
                        
                        if(DBI::dbExistsTable(private$conn, private$tableNameScenLocks)){
                          self$deleteRows(private$tableNameScenLocks, 
                                          colNames = private$scenMetaColnames['sid'], 
                                          values = private$sid)
                        }
                        invisible(self)
                      },
                      isLocked = function(){
                        # Determines whether a scenario is locked by a user other than the user logged in
                        #
                        # Args:
                        #
                        # Returns:
                        #   logical: returns TRUE in scenario is locked or error occurred, FALSE otherwise
                        
                        #BEGIN error checks
                        stopifnot(!is.null(private$sid))
                        #END error checks
                        
                        errMsg <- NULL
                        tryCatch({
                          uidLock <- private$getUidLock()
                          if(is.na(uidLock) || identical(uidLock, private$uid)){
                            # scenario is not locked or locked by current user
                            return(FALSE)
                          }else{
                            # scenario is locked
                            return(TRUE)
                          }
                        }, error = function(e){
                          stop(sprintf("Db: Problems locking scenario: '%s'. Error message: %s.", private$sid, e), call. = FALSE)
                        })
                      },
                      isReadonly = function(){
                        # checks whether current scenario is readOnly
                        # 
                        # Args:
                        #
                        # Returns:
                        #   logical: returns TRUE if scenario is readonly, FALSE otherwise 
                        if(any(private$userAccessGroups %in% csv2Vector(private$writePerm))){
                          return(FALSE)
                        }else{
                          return(TRUE)
                        }
                      },
                      spreadScalarData = function(dataset){
                        # Spreads scalar dataset
                        dataset <- dataset[, -2, drop = FALSE]
                        spread(dataset, 1, 2)
                      },
                      bindSidCol = function(data){
                        # binds sid column to dataset
                        sidCol <- private$scenMetaColnames['sid']
                        if(!identical(names(data)[[1]], sidCol)){
                          data <- dplyr::bind_cols(!!sidCol := rep.int(private$sid, nrow(data)), 
                                                   data)
                        }
                        return(data)
                      },
                      readBlob = function(filePaths, maxSize, fileNames){
                        # reads blob data and returns tibble with meta data and file content
                        
                        stopifnot(is.character(filePaths), length(filePaths) >= 1L)
                        stopifnot(is.numeric(maxSize), length(maxSize) == 1L)
                        
                        
                        fileSize <- file.info(filePaths, extra_cols = FALSE)$size
                        
                        if(any(fileSize > maxSize)){
                          stop("maxSizeException", call. = FALSE)
                        }
                        content <- blob::new_blob(lapply(seq_along(filePaths), 
                                                         function(i) readBin(filePaths[[i]], "raw", n = fileSize[[i]])))
                        return(tibble(fileName = fileNames, fileExt = tools::file_ext(filePaths), 
                                      execPerm = rep.int(TRUE, length(filePaths)), fileContent = content))
                      }
                    )
)