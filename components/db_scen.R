# R6 class for Scenario object
Scenario <- R6Class("Scenario", 
                    inherit = Db,
                    public = list(
                      initialize                    = function(db, sid = NULL, sname = NULL,
                                                               readPerm = NULL, writePerm = NULL, execPerm = NULL,
                                                               tags = NULL, overwrite = FALSE, isNewScen = FALSE, 
                                                               duplicatedMetadata = NULL, uid = NULL, views = NULL,
                                                               attachments = NULL){
                        # Initialize scenario class
                        #
                        # Args:
                        #   db:                R6 Database object
                        #   sid:               scenario Id (optional)
                        #   sname:             scenario name (optional)
                        #   readPerm:          users/groups that have read permissions (optional)
                        #   writePerm:         users/groups that have write permissions (optional)
                        #   execPerm:          users/groups that have execute permissions (optional)
                        #   tags:              vector of tags that can be specified (optional)
                        #   overwrite:         logical that specifies whether data should be overwritten or appended
                        #   isNewScen:         whether the scenario is not yet stored in the database
                        #   duplicatedMetadata: metadata information of to apply
                        #   uid:               can be used to declare owner of the scenario (default is current user)
                        #   views:             views object
                        #   attachments:       attachments object
                        
                        #BEGIN error checks 
                        stopifnot(is.R6(db), is.logical(isNewScen), length(isNewScen) == 1L)
                        private$conn <- db$getConn()
                        if(length(uid) == 1L && is.character(uid)){
                          private$uid <- uid
                          private$userAccessGroups <- uid
                        }else{
                          private$uid  <- db$getUid()
                          private$userAccessGroups    <- db$accessGroups
                        }
                        if(is.null(sid)){
                          stopifnot(is.character(sname), length(sname) == 1)
                        }else{
                          sid <- suppressWarnings(as.integer(sid))
                          stopifnot(!is.na(sid), length(sid) == 1)
                        }
                        stopifnot(is.logical(overwrite), length(overwrite) == 1)
                        #END error checks 
                        
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
                        if(length(execPerm)){
                          stopifnot(is.character(execPerm), length(execPerm) >=1)  
                        }else{
                          execPerm <- private$uid
                        }
                        
                        private$dbSchema            <- db$getDbSchema()
                        private$slocktimeLimit      <- db$getSlocktimeLimit
                        private$scenMetaColnames    <- db$getScenMetaColnames()
                        private$slocktimeIdentifier <- db$getSlocktimeIdentifier()
                        private$tableNameMetadata   <- db$getTableNameMetadata()
                        private$tableNameScenLocks  <- db$getTableNameScenLocks()
                        private$tableNamesScenario  <- db$getTableNamesScenario()
                        private$tags                <- vector2Csv(unique(tags))
                        private$readPerm            <- vector2Csv(readPerm)
                        private$writePerm           <- vector2Csv(writePerm)
                        private$execPerm            <- vector2Csv(execPerm)
                        private$views               <- views
                        private$attachments         <- attachments
                        
                        savedAttachConfig <- NULL
                        
                        if(db$getHcubeActive())
                          private$scode <- SCODEMAP[['hcube_jobconfig']]
                        else
                          private$scode <- SCODEMAP[['scen']]
                        
                        if(isNewScen){
                          private$stime     <- Sys.time()
                          private$suid      <- private$uid
                          private$sname     <- sname
                          private$removeAllExistingAttachments <- TRUE
                          if(length(duplicatedMetadata$attach)){
                            savedAttachConfig <- list(localAttachments = duplicatedMetadata$attach$localAttachments,
                                                      attachmentsUpdateExec = duplicatedMetadata$attach$attachmentsUpdateExec)
                            if(length(duplicatedMetadata$attach$sidToDuplicate)){
                              private$sidToDuplicate <- as.integer(duplicatedMetadata$attach$sidToDuplicate)
                              private$duplicateAttachmentsOnNextSave <- TRUE
                            }
                          }
                          if(length(duplicatedMetadata$perm)){
                            private$readPerm  <- vector2Csv(
                              unique(c(private$uid, 
                                       csv2Vector(duplicatedMetadata$perm$readPerm))))
                            private$writePerm <- vector2Csv(
                              unique(c(private$uid, 
                                       csv2Vector(duplicatedMetadata$perm$writePerm))))
                            private$execPerm  <- vector2Csv(
                              unique(c(private$uid, 
                                       csv2Vector(duplicatedMetadata$perm$execPerm))))
                          }
                        }else{
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
                            })
                          }else{
                            private$fetchMetadata(sid = sid)
                          }
                          # set/refresh lock for scenario
                          private$lock()
                        }
                        if(!is.null(private$attachments)){
                          private$attachments$initScenData(private$sid)
                          if(!is.null(savedAttachConfig)){
                            private$attachments$setConfig(savedAttachConfig)
                          }
                        }
                        return(invisible(self))
                      },
                      getSid      = function() private$sid,
                      getScenUid  = function() private$suid,
                      getScenName = function() private$sname,
                      getScenTime = function() private$stime,
                      getStags    = function() csv2Vector(private$tags),
                      getReadPerm = function() csv2Vector(private$readPerm),
                      getWritePerm = function() csv2Vector(private$writePerm),
                      getExecPerm = function() csv2Vector(private$execPerm),
                      getLockUid = function() private$lockUid,
                      getMetadataInfo = function(discardAttach = FALSE, discardPerm = FALSE){
                        stopifnot(length(private$sid) > 0L)
                        attachmentConfig <- list()
                        permissionConfig <- list()
                        if(!isTRUE(discardAttach) && !is.null(private$attachments)){
                          attachmentConfig <- private$attachments$getConfig()
                          attachmentConfig$sidToDuplicate <- private$sid
                        }
                        if(!isTRUE(discardPerm)){
                          permissionConfig <- list(readPerm = private$readPerm,
                                                   writePerm = private$writePerm,
                                                   execPerm = private$execPerm)
                        }
                        return(list(attach = attachmentConfig,
                                    perm = permissionConfig))
                      },
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
                        stopifnot(is.logical(noPermFields), length(noPermFields) == 1L)
                        #END error checks
                        
                        stag      <- character(0L)
                        readPerm  <- character(0L)
                        writePerm <- character(0L)
                        execPerm  <- character(0L)
                        
                        if(!noPermFields){
                          readPerm <- private$readPerm
                          writePerm <- private$writePerm
                          execPerm <- private$execPerm
                        }
                        super$getMetadata(sid = private$sid, uid = private$suid, sname = private$sname, 
                                          stime = private$stime, stag = private$tags, readPerm = readPerm, 
                                          writePerm = writePerm, execPerm = execPerm,
                                          uidAlias = aliases[["uid"]], snameAlias = aliases[["sname"]], 
                                          stimeAlias = aliases[["stime"]],
                                          stagAlias = aliases[["stag"]], readPermAlias = aliases[["readPerm"]], 
                                          writePermAlias = aliases[["writePerm"]],
                                          execPermAlias = aliases[["execPerm"]])
                      },
                      resetAccessPerm = function(){
                        if(private$isReadonly()){
                          stop("Db: Metadata wasn't updated as scenario is readonly (Scenario.resetAccessPerm).", 
                               call. = FALSE)
                        }
                        private$readPerm  <- vector2Csv(private$uid)
                        private$writePerm <- vector2Csv(private$uid)
                        private$execPerm  <- vector2Csv(private$uid)
                        return(invisible(self))
                      },
                      save = function(datasets, msgProgress = NULL){
                        # Saves multiple dataframes to database
                        #
                        # Args:
                        #   datasets :           dataframes to save
                        #   msgProgress:         title and progress info for the progress bar (optional)
                        #
                        # Returns:
                        #   object: invisibly returns reference to itself
                        
                        #BEGIN error checks
                        if(private$isReadonly()){
                          stop("Scenario is readonly. Saving failed (Scenario.save).", call. = FALSE)
                        }
                        addScenIdAttach <- FALSE
                        if(!length(private$sid)){
                          addScenIdAttach <- TRUE
                          try(private$fetchMetadata(sname = private$sname, uid = private$uid), silent = TRUE)
                        }
                        stopifnot(length(private$tableNamesScenario) >= 1)
                        stopifnot(is.list(datasets), identical(length(datasets), length(private$tableNamesScenario)))
                        if(!is.null(msgProgress)){
                          stopifnot(is.character(msgProgress$title), length(msgProgress$title) == 1)
                          stopifnot(is.character(msgProgress$progress), length(msgProgress$progress) == 1)
                          # initialize progress bar
                          prog <- Progress$new()
                          on.exit(prog$close())
                          prog$set(message = msgProgress$title, value = 0)
                          updateProgress <- NULL
                          incAmount      <- 1/length(private$tableNamesScenario)
                          updateProgress <- function(detail = NULL) {
                            prog$inc(amount = incAmount, detail = detail)
                          }
                        }
                        #END error checks
                        
                        # save current time stamp
                        private$stime <- Sys.time()
                        
                        if(!is.null(msgProgress)){
                          # increment progress bar
                          updateProgress(detail = msgProgress$attachments)
                        }
                        # write scenario metadata
                        private$writeMetadata()
                        if(!is.null(private$attachments) && addScenIdAttach){
                          private$attachments$setSid(private$sid)
                        }
                        
                        private$saveAttachmentData()
                        
                        Map(function(dataset, tableName){
                          if(!is.null(dataset) && nrow(dataset)){
                            super$exportScenDataset(private$bindSidCol(dataset), tableName)
                          }
                          if(!is.null(msgProgress)){
                            # increment progress bar
                            updateProgress(detail = msgProgress$progress)
                          }
                        }, datasets, private$tableNamesScenario)
                        
                        private$saveViewData()
                        
                        private$scenSaved <- TRUE
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
                        
                        stopifnot(length(private$sid) > 0L)
                        if(private$isReadonly()){
                          stop("Scenario is readonly. Saving trace data failed (Scenario.saveTraceData).", call. = FALSE)
                        }
                        super$exportScenDataset(private$bindSidCol(traceData), private$dbSchema$tabName[["_scenTrc"]])
                        private$scenSaved <- TRUE
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
                        stopifnot(length(private$sid) > 0L)
                        #END error checks 
                        if(private$isReadonly()){
                          stop("Scenario could not be removed as it is readonly (Scenario.delete).", call. = FALSE)
                        }
                        noErr <- TRUE
                        self$deleteRows(private$tableNameMetadata, 
                                        private$scenMetaColnames['sid'], 
                                        private$sid)
                        self$deleteRows(private$dbSchema$tabName[["_scenAttach"]], 
                                        private$scenMetaColnames['sid'], 
                                        private$sid)
                        
                        private$unlock()
                        flog.debug("Scenario: '%s' unlocked.", private$sid)
                        
                        private$sid   <- integer(0)
                        
                        invisible(self)
                      },
                      updateMetadata = function(newName = character(0L), newTags = character(0L), 
                                                newReadPerm = character(0L), newWritePerm = character(0L),
                                                newExecPerm = character(0L)){
                        # Edit scenario metadata (wrapper around db.writeMetadata method)
                        #
                        # Args:
                        #   newName:      updated scenario name (optional)
                        #   newTags:      updated scenario tags (optional)
                        #   newReadPerm:  updated scenario read permissions (optional)
                        #   newWritePerm: updated scenario write permissions (optional)
                        #   newWritePerm: updated scenario execute permissions (optional)
                        
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
                        stopifnot(is.character(newExecPerm))
                        #END error checks 
                        if(private$isReadonly() && sum(length(newReadPerm),
                                                       length(newWritePerm),
                                                       length(newExecPerm)) > 0){
                          stop("Db: Metadata wasn't updated as scenario is readonly (Scenario.updateMetadata).", 
                               call. = FALSE)
                        }
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
                        if(length(newExecPerm)){
                          private$execPerm <- vector2Csv(newExecPerm)
                        }
                        
                        invisible(self)
                      },
                      hasExecPerm = function(){
                        # checks whether current scenario can be executed
                        # 
                        # Args:
                        #
                        # Returns:
                        #   logical: returns TRUE if scenario can be executed, FALSE otherwise 
                        
                        if(any(private$userAccessGroups %in% csv2Vector(private$execPerm))){
                          return(TRUE)
                        }else{
                          return(FALSE)
                        }
                      },
                      saveScriptResults = function(scriptResults){
                        # save script results in database
                        #
                        # Args:
                        #
                        # Returns:
                        #   R6 object (referece to self)
                        
                        stopifnot(inherits(scriptResults, "data.frame"), 
                                  length(scriptResults) == 2L)
                        
                        names(scriptResults) <- private$dbSchema$colNames[['_scenScripts']][-1]
                        
                        for(i in 1:2){
                          if(!is.character(scriptResults[[i]])){
                            scriptResults[[i]] <- as.character(scriptResults[[i]])
                          }
                        }
                        
                        super$exportScenDataset(private$bindSidCol(scriptResults), 
                                                private$dbSchema$tabName[['_scenScripts']])
                      },
                      finalize = function(){
                        if(length(private$sid)){
                          if(identical(private$getUidLock(), private$uid)){
                            flog.debug("Scenario: '%s' unlocked.", private$sid)
                            private$unlock()
                          }
                          if(private$newScen && !private$scenSaved){
                            flog.debug("Scenario was not saved. Thus, it will be removed.")
                            self$delete()
                          }
                          private$sid <- integer(0L)
                        }
                        return(invisible(self))
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
                      execPerm            = character(0L),
                      lockUid             = character(0L),
                      dbSchema            = vector("list", 3L),
                      scode               = integer(1L),
                      scenSaved           = logical(1L),
                      newScen             = logical(1L),
                      traceData           = tibble(),
                      views               = NULL,
                      attachments         = NULL,
                      duplicateAttachmentsOnNextSave = FALSE,
                      removeAllExistingAttachments = FALSE,
                      sidToDuplicate      = integer(0L),
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
                          metadata     <- self$importDataset(private$tableNameMetadata, 
                                                             subsetSids = sid, innerSepAND = FALSE)
                          if(!nrow(metadata)){
                            stop(sprintf("A scenario with ID: '%s' could not be found. This could be due to the scenario being deleted in the meantime or due to the user tampering with the app!", 
                                         sid), call. = FALSE)
                          }
                          
                          if(metadata[[private$scenMetaColnames['scode']]][1] != private$scode){
                            flog.debug("The scenario loaded was generated from a different mode. A copy will be created (Scen.fetchMetadata)")
                            private$sname     <- metadata[[private$scenMetaColnames['sname']]][1]
                            private$suid      <- private$uid
                            private$stime     <- Sys.time()
                            private$tags      <- metadata[[private$scenMetaColnames['stag']]][1]
                            traceData         <- super$importDataset(private$dbSchema$tabName[["_scenTrc"]],
                                                                     subsetSids = sid, limit = 1L)
                            if(length(traceData) && nrow(traceData)){
                              private$traceData <- traceData[-1]
                            }
                            existingSids <- self$importDataset(private$tableNameMetadata, 
                                                               colNames = private$scenMetaColnames['sid'],
                                                               tibble(c(private$scenMetaColnames['uid'],
                                                                        private$scenMetaColnames['sname'],
                                                                        private$scenMetaColnames['scode']),
                                                                      c(private$suid, private$sname,
                                                                        private$scode)))[[1]]
                            if(length(existingSids) > 0L){
                              private$sid <- existingSids[1L]
                            }
                            private$writeMetadata()
                          }else{
                            private$suid      <- metadata[[private$scenMetaColnames['uid']]][1]
                            private$sname     <- metadata[[private$scenMetaColnames['sname']]][1]
                            private$stime     <- metadata[[private$scenMetaColnames['stime']]][1]
                            private$tags      <- metadata[[private$scenMetaColnames['stag']]][1]
                            private$readPerm  <- metadata[[private$scenMetaColnames['accessR']]][1]
                            private$writePerm <- metadata[[private$scenMetaColnames['accessW']]][1]
                            private$execPerm  <- metadata[[private$scenMetaColnames['accessX']]][1]
                            private$sid       <- as.integer(sid)
                          }
                        }else{
                          metadata <- self$importDataset(private$tableNameMetadata, 
                                                         tibble(c(private$scenMetaColnames['uid'],
                                                                  private$scenMetaColnames['sname']),
                                                                c(uid, sname)))
                          if(!nrow(metadata)){
                            stop(sprintf("A scenario with name: '%s' could not be found for user: '%s'.", 
                                         sname, uid), call. = FALSE)
                          }
                          scenFromOtherMode <- metadata[[private$scenMetaColnames['scode']]] != private$scode
                          if(all(scenFromOtherMode))
                            stop("No scenario with this name exists in the current mode.",
                                 call. = FALSE)
                            
                          private$suid      <- uid
                          private$sname     <- sname
                          private$stime     <- Sys.time()
                          sidTmp            <- as.integer(metadata[[private$scenMetaColnames['sid']]][!scenFromOtherMode])
                          
                          private$sid       <- sidTmp
                          if(length(private$traceData) && nrow(private$traceData)){
                            self$saveTraceData(private$traceData)
                            private$traceData <- tibble()
                          }
                        }
                        
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
                        stopifnot(length(private$sid) > 0L)
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
                        if(length(private$sid)){
                          if(private$isReadonly()){
                            stop("Db: Metadata could not be overwritten as scenario is readonly (Scenario.writeMetadata).", 
                                 call. = FALSE)
                          }
                          self$deleteRows(private$tableNameMetadata, 
                                          private$scenMetaColnames['sid'], 
                                          private$sid)
                          metadata           <- tibble(private$sid, private$suid, private$sname,
                                                       private$stime, private$tags, 
                                                       private$readPerm, private$writePerm,
                                                       private$execPerm, private$scode)
                          colnames(metadata) <- private$scenMetaColnames
                        }else{
                          # new scenario
                          private$newScen    <- TRUE
                          metadata           <- tibble(private$suid, private$sname,
                                                       private$stime, private$tags, 
                                                       private$readPerm, private$writePerm,
                                                       private$execPerm, private$scode)
                          colnames(metadata) <- private$scenMetaColnames[-1]
                        }
                        super$writeMetadata(metadata)
                        flog.debug("Db: Metadata (table: '%s') was added for scenario: '%s' (Scenario.writeMetadata).", 
                                   private$tableNameMetadata, private$sname)
                        if(!length(private$sid))
                          private$fetchMetadata(sname = private$sname, 
                                                uid = private$suid)
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
                        if(!length(private$sid))
                          return(invisible(self))
                        #END error checks
                        if(private$isReadonly()){
                          flog.debug("Scenario wasn't locked as it is readonly (Scenario.lock).")
                          return(invisible(self))
                        }
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
                            flog.debug("Scenario loaded in readonly mode as it is locked (Scenario.lock).")
                            private$lockUid <- uidLocked
                            return(invisible(self))
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
                        if(!length(private$sid))
                          return(invisible(self))
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
                        if(!length(private$sid))
                          return(FALSE)
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
                        if(length(private$lockUid)){
                          return(TRUE)
                        }
                        if(any(private$userAccessGroups %in% csv2Vector(private$writePerm))){
                          return(FALSE)
                        }else{
                          return(TRUE)
                        }
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
                      saveAttachmentData = function(){
                        if(is.null(private$attachments)){
                          return(invisible(self))
                        }
                        # remove existing attachments if scenario is overwritten
                        if(isTRUE(private$removeAllExistingAttachments)){
                          if(identical(private$sidToDuplicate, private$sid)){
                            # in case scenario id is identical to the one 
                            # where attachments should be duplicated, we do nothing
                            private$duplicateAttachmentsOnNextSave <- FALSE
                          }else{
                            super$deleteRows(private$dbSchema$tabName[["_scenAttach"]],
                                             subsetSids = private$sid)
                          }
                          private$removeAllExistingAttachments <- FALSE
                        }
                        if(isTRUE(private$duplicateAttachmentsOnNextSave)){
                          private$duplicateAttachments()
                        }
                        attachmentOpQueue <- private$attachments$flushOpQueue()
                        if(length(attachmentOpQueue$remove)){
                          # save dirty attachments 
                          super$deleteRows(private$dbSchema$tabName[["_scenAttach"]], "fileName", 
                                           attachmentOpQueue$remove, 
                                           conditionSep = "OR", subsetSids = private$sid)
                        }
                        
                        if(length(attachmentOpQueue$updateExec$name)){
                          fnHasExecPerm <- attachmentOpQueue$updateExec$
                            name[attachmentOpQueue$updateExec$execPerm]
                          fnHasNoExecPerm <- attachmentOpQueue$updateExec$
                            name[!attachmentOpQueue$updateExec$execPerm]
                          
                          if(length(fnHasExecPerm))
                            super$updateRows(private$dbSchema$tabName[["_scenAttach"]], 
                                             tibble("fileName", fnHasExecPerm), 
                                             colNames = "execPerm", 
                                             values = TRUE, 
                                             subsetSids = private$sid, innerSepAND = FALSE)
                          
                          if(length(fnHasNoExecPerm))
                            super$updateRows(private$dbSchema$tabName[["_scenAttach"]], 
                                             tibble("fileName", fnHasNoExecPerm), 
                                             colNames = "execPerm", 
                                             values = FALSE, 
                                             subsetSids = private$sid, innerSepAND = FALSE)
                        }
                        if(length(attachmentOpQueue$save)){
                          super$exportScenDataset(private$bindSidCol(
                            attachmentOpQueue$save), 
                            private$dbSchema$tabName[["_scenAttach"]], addForeignKey = FALSE)
                          
                        }
                        return(invisible(self))
                      },
                      saveViewData = function(){
                        if(is.null(private$views)){
                          return(invisible(self))
                        }
                        viewConf <- private$views$getConf()
                        if(!length(viewConf) || !nrow(viewConf)){
                          return(invisible(self))
                        }
                        viewConfToSave <- fixColTypes(viewConf %>%
                                                        add_column(time = Sys.time()),
                                                      substring(private$dbSchema$colTypes[["_scenViews"]],
                                                                2))
                        names(viewConfToSave) <- private$dbSchema$colNames[["_scenViews"]][-1]
                        super$exportScenDataset(private$bindSidCol(viewConfToSave),
                                                private$dbSchema$tabName[["_scenViews"]])
                        invisible(self)
                      },
                      duplicateAttachments = function(){
                        stopifnot(is.integer(private$sidToDuplicate), 
                                  length(private$sidToDuplicate) == 1L, !is.na(private$sidToDuplicate),
                                  length(private$sid) == 1L)
                        tableName      <- private$dbSchema$tabName[["_scenAttach"]]
                        if(!dbExistsTable(private$conn, tableName)){
                          return(invisible(self))
                        }
                        colNames       <- private$dbSchema$colNames[["_scenAttach"]]
                        dbExecute(private$conn, 
                                  paste0("INSERT INTO ", dbQuoteIdentifier(private$conn, tableName),
                                         "(", paste0(dbQuoteIdentifier(private$conn, colNames), 
                                                     collapse = ","), ") SELECT ", private$sid, ",", 
                                         paste0(dbQuoteIdentifier(private$conn, colNames[-1]),
                                                collapse = ","), " FROM ", 
                                         dbQuoteIdentifier(private$conn, tableName),
                                         " WHERE ", dbQuoteIdentifier(private$conn, colNames[1]),
                                         "=", private$sidToDuplicate, ";"))
                        private$duplicateAttachmentsOnNextSave <- FALSE
                        return(invisible(self))
                      }
                    )
)
