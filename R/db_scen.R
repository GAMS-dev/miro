# R6 class for Scenario object
Scenario <- R6Class("Scenario", 
                    inherit = Db,
                    public = list(
                      initialize                    = function(db, sid = NULL, sname = NULL,
                                                               readPerm = NULL, writePerm = NULL, execPerm = NULL,
                                                               tags = NULL, overwrite = FALSE, isNewScen = FALSE){
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
                        
                        #BEGIN error checks 
                        stopifnot(is.R6(db), is.logical(isNewScen), length(isNewScen) == 1L)
                        private$conn <- db$getConn()
                        private$uid  <- db$getUid()
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
                        private$userAccessGroups    <- db$accessGroups
                        private$tableNameMetadata   <- db$getTableNameMetadata()
                        private$tableNameScenLocks  <- db$getTableNameScenLocks()
                        private$tableNamesScenario  <- db$getTableNamesScenario()
                        private$attachmentConfig    <- db$getAttachmentConfig()
                        private$tags                <- vector2Csv(unique(tags))
                        private$readPerm            <- vector2Csv(readPerm)
                        private$writePerm           <- vector2Csv(writePerm)
                        private$execPerm            <- vector2Csv(execPerm)
                        if(db$getHcubeActive())
                          private$scode <- SCODEMAP[['hcube_jobconfig']]
                        else
                          private$scode <- SCODEMAP[['scen']]
                        
                        if(isNewScen){
                          private$stime     <- Sys.time()
                          private$suid      <- private$uid
                          private$sname     <- sname
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
                      isFromOtherMode = function() private$dataFromOtherMode,
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
                        if(!length(private$sid)){
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
                        # save dirty attachments 
                        if(length(private$attachmentsToRemove)){
                          super$deleteRows(private$dbSchema$tabName[["_scenAttach"]], "fileName", 
                                           private$attachmentsToRemove, 
                                           conditionSep = "OR", subsetSids = private$sid)
                          private$attachmentsToRemove <- character(0L)
                        }
                        
                        if(length(private$attachmentsUpdateExec$name)){
                          fnHasExecPerm <- private$attachmentsUpdateExec$
                            name[private$attachmentsUpdateExec$execPerm]
                          fnHasNoExecPerm <- private$attachmentsUpdateExec$
                            name[!private$attachmentsUpdateExec$execPerm]
                          
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
                          
                          private$attachmentsUpdateExec <- list(name = character(0L),
                                                                execPerm = logical(0L))
                        }
                        if(length(private$localAttachments$filePaths)){
                          super$exportScenDataset(private$bindSidCol(
                            private$readBlob()), 
                            private$dbSchema$tabName[["_scenAttach"]], addForeignKey = FALSE)
                          private$localAttachments <- list(filePaths = character(0L), 
                                                           execPerm = logical(0L))
                          
                        }
                        Map(function(dataset, tableName){
                          if(!is.null(dataset) && nrow(dataset)){
                            super$exportScenDataset(private$bindSidCol(dataset), tableName)
                          }
                          if(!is.null(msgProgress)){
                            # increment progress bar
                            updateProgress(detail = msgProgress$progress)
                          }
                        }, datasets, private$tableNamesScenario)
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
                      addAttachments = function(filePaths, fileNames = NULL, forbiddenFnames = NULL, 
                                                overwrite = FALSE, execPerm = NULL, workDir = NULL){
                        # Saves attachments
                        # 
                        # Args:
                        #   filePaths:        character vector with file paths to read data from
                        #   fileNames:        names of the files in case custom name should be chosen
                        #   forbiddenFnames:  character vector with forbidden file names
                        #   overwrite:        boolean that specifies whether existing files should 
                        #                     be overwritten
                        #   workDir:          working directory where attachment will be stored until scenario is saved
                        #
                        # Returns:
                        #   R6 object (reference to itself)
                        
                        stopifnot(is.character(filePaths), length(filePaths) >= 1L,
                                  is.logical(overwrite), length(overwrite) == 1L)
                        if(!is.null(workDir))
                          stopifnot(is.character(workDir), length(workDir) == 1L)
                        if(is.null(execPerm)){
                          execPerm <- rep.int(TRUE, length(filePaths))
                        }else{
                          stopifnot(is.logical(execPerm), length(filePaths) == length(execPerm))
                        }
                        
                        if(private$isReadonly()){
                          stop("roException", call. = FALSE)
                        }
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
                          if(!overwrite)
                            stop("duplicateException", call. = FALSE)
                          fnToOverwrite <- fileNames %in% fileNamesDb
                          fileNamesDb   <- fileNamesDb[!fnToOverwrite]
                          self$removeAttachments(fileNames[fnToOverwrite])
                        }
                        if(length(fileNamesDb) + length(filePaths) > private$attachmentConfig[["maxNo"]]){
                          stop("maxNoException", call. = FALSE)
                        }
                        if(!is.null(workDir)){
                          fileNamesTmp <- vapply(seq_along(fileNames), function(fnIdx){
                            if(execPerm[fnIdx])
                              return(fileNames[fnIdx])
                            return(file.path("_miro_attach_", fileNames[fnIdx]))
                          }, character(1L), USE.NAMES = FALSE)
                          
                          filePathsTmp <- file.path(workDir, fileNamesTmp)
                          filePaths    <- filePathsTmp[file.move(filePaths, filePathsTmp)]
                          if(length(filePathsTmp) != length(filePaths)){
                            failedFileMoves <- !filePaths %in% filePathsTmp
                            flog.warn("Some attachments could not be relocated: '%s' (Scenario.addAttachments).", 
                                      paste(filePathsTmp[failedFileMoves], collapse = "', '"))
                            execPerm <- execPerm[!failedFileMoves]
                          }
                        }
                        
                        private$localAttachments$filePaths <- c(private$localAttachments$filePaths, filePaths)
                        private$localAttachments$execPerm  <- c(private$localAttachments$execPerm, execPerm)
                        
                        invisible(self)
                      },
                      fetchAttachmentList = function(){
                        # Fetches file names of saved attachments from database
                        # 
                        # Args:
                        #
                        # Returns:
                        #   tibble with columns name and execPerm
                        attachmentNames    <- basename(private$localAttachments$filePaths)
                        attachmentExecPerm <- private$localAttachments$execPerm
                        
                        if(!length(private$sid)){
                          return(dplyr::arrange(tibble(name = attachmentNames,
                                                       execPerm = attachmentExecPerm), name))
                        }
                        attachments <- super$importDataset(private$dbSchema$tabName[["_scenAttach"]], 
                                                           colNames = c("fileName", "execPerm"), 
                                                           subsetSids = private$sid)
                        if(length(attachments)){
                          attachmentNamesDb    <- attachments[[1]]
                          attachmentsToKeep    <- !attachmentNamesDb %in% attachmentNames
                          attachmentNamesDb    <- attachmentNamesDb[attachmentsToKeep]
                          attachmentExecPermDb <- attachments[[2]][attachmentsToKeep]
                          if(length(private$attachmentsToRemove)){
                            attachmentsToKeep     <- !attachmentNamesDb %in% private$attachmentsToRemove
                            attachmentNamesDb     <- attachmentNamesDb[attachmentsToKeep]
                            attachmentExecPermDb  <- attachmentExecPermDb[attachmentsToKeep]
                          }
                          if(length(private$attachmentsUpdateExec$name)){
                            attachmentsToKeep <- !attachmentNamesDb %in% private$attachmentsUpdateExec$name
                            attachmentNamesDb <- c(attachmentNamesDb[attachmentsToKeep], 
                                                   private$attachmentsUpdateExec$name)
                            attachmentExecPermDb  <- c(attachmentExecPermDb[attachmentsToKeep], 
                                                     private$attachmentsUpdateExec$execPerm)
                          }
                        }
                        
                        
                        return(dplyr::arrange(tibble(name = c(attachmentNamesDb, attachmentNames),
                                                     execPerm = c(attachmentExecPermDb, attachmentExecPerm)), name))
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
                        #   filepaths of downloaded files
                        
                        stopifnot(is.character(filePath), length(filePath) == 1L,
                                  is.logical(fullPath), length(fullPath) == 1L,
                                  is.logical(allExecPerm), length(allExecPerm) == 1L)
                        
                        if(fullPath){
                          if(length(fileNames) != 1L){
                            stop("Only single file name allowed when full path is specified.", call. = FALSE)
                          }
                          fileDir   <- dirname(filePath)
                          filePaths <- filePath
                        }else{
                          fileDir   <- filePath
                          filePaths <- file.path(filePath, fileNames)
                        }
                        
                        localPaths <- character(0L)
                        
                        if(allExecPerm){
                          if(any(private$localAttachments$execPerm)){
                            localPaths <- private$localAttachments$filePaths[private$localAttachments$execPerm]
                            localPathNeedsRelocation <- dirname(localPaths) != fileDir
                            
                            if(any(localPathNeedsRelocation)){
                              if(fullPath){
                                toDir <- filePaths
                              }else{
                                toDir <- file.path(fileDir, basename(localPaths[localPathNeedsRelocation]))
                              }
                              file.copy(localPaths[localPathNeedsRelocation], 
                                        toDir, 
                                        overwrite = TRUE)
                            }
                          }
                        }else{
                          stopifnot(is.character(fileNames), length(fileNames) >= 1L)
                          
                          isLocalAttachment <- fileNames %in% basename(private$localAttachments$filePaths)
                          localPaths <- filePaths[isLocalAttachment]
                          
                          if(length(localPaths)){
                            localPathNeedsRelocation <- basename(private$localAttachments$filePaths) %in% fileNames &
                              dirname(private$localAttachments$filePaths) != fileDir
                            
                            if(any(localPathNeedsRelocation)){
                              if(fullPath){
                                toDir <- filePaths
                              }else{
                                toDir <- file.path(fileDir, basename(private$localAttachments$filePaths[localPathNeedsRelocation]))
                              }
                              file.copy(private$localAttachments$filePaths[localPathNeedsRelocation], 
                                        toDir,
                                        overwrite = TRUE)
                            }
                              
                            if(length(localPaths) == length(filePaths)){
                              return(filePaths)
                            }
                            
                            fileNames <- fileNames[!isLocalAttachment]
                            filePaths <- filePaths[!isLocalAttachment]
                          }
                        }
                        
                        if(!length(private$sid)){
                          return(localPaths)
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
                        
                        return(c(filePaths, localPaths))
                      },
                      setAttachmentExecPerm = function(fileName, value, workDir){
                        # Sets execute permission for particular attachment
                        # 
                        # Args:
                        #   fileName:  name of the file whose data to update
                        #   value:     logical that specifies whether data can be executed by GAMS
                        #   workDir:   working directory where attachment will be stored until scenario is saved
                        #
                        # Returns:
                        #   R6 object (reference to itself)
                        
                        stopifnot(is.character(fileName), length(fileName) == 1L,
                                  is.logical(value), length(value) == 1L)
                        
                        if(private$isReadonly()){
                          stop("Scenario is readonly. Updating attachment data failed. (Scenario.setAttachmentExecPerm).", 
                               call. = FALSE)
                        }
                        localFileId <- match(fileName, basename(private$localAttachments$filePaths))
                        
                        if(is.na(localFileId)){
                          private$attachmentsUpdateExec$name <- c(private$attachmentsUpdateExec$name, 
                                                                  fileName)
                          private$attachmentsUpdateExec$execPerm <- c(private$attachmentsUpdateExec$execPerm, 
                                                                      value)
                          return(invisible(self))
                        }
                        
                        if(value)
                          newPath <- file.path(workDir, fileName)
                        else
                          newPath <- file.path(workDir, "_miro_attach_", fileName)
                        
                        if(!file.move(private$localAttachments$filePaths[localFileId], 
                                      newPath))
                          flog.warn("Problems moving attachment: '%s' (Scenario.setAttachmentExecPerm).", fileName)
                        
                        private$localAttachments$filePaths[localFileId] <- newPath
                        private$localAttachments$execPerm[localFileId]  <- value
                        
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
                        
                        stopifnot(is.character(fileNames), length(fileNames) > 0L)
                        if(private$isReadonly()){
                          stop("Attachment could not be removed as scenario is readonly (Scenario.removeAttachments).", call. = FALSE)
                        }
                        localFiles <- match(fileNames, basename(private$localAttachments$filePaths))
                        localFilesToRemoveId <- localFiles[!is.na(localFiles)]
                        
                        if(length(localFilesToRemoveId)){
                          localFilesToRemove <- private$localAttachments$
                            filePaths[localFilesToRemoveId]
                          
                          removedLocalFiles <- file.remove(localFilesToRemove)
                          if(any(!removedLocalFiles))
                            flog.warn("Some local attachments could not be removed: '%s'.",
                                      paste(localFilesToRemove[!removedLocalFiles], 
                                            collapse = "', '"))
                          private$localAttachments$filePaths <- private$localAttachments$filePaths[-localFilesToRemoveId]
                          private$localAttachments$execPerm  <- private$localAttachments$execPerm[-localFilesToRemoveId]
                          
                          updatesToRemove <- match(fileNames, basename(private$attachmentsUpdateExec$name))
                          updatesToRemove <- updatesToRemove[!is.na(updatesToRemove)]
                          
                          if(length(updatesToRemove)){
                            private$attachmentsUpdateExec$name <- private$
                              attachmentsUpdateExec$name[-updatesToRemove]
                            private$attachmentsUpdateExec$execPerm <- private$
                              attachmentsUpdateExec$execPerm[-updatesToRemove]
                          }
                          
                          fileNames <- fileNames[is.na(localFiles)]
                        }
                        private$attachmentsToRemove <- c(private$attachmentsToRemove, fileNames)
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
                        private$cleanLocalFiles()
                        
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
                        if(private$isReadonly()){
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
                      finalize = function(){
                        private$cleanLocalFiles()
                        if(length(private$sid)){
                          flog.debug("Scenario: '%s' unlocked.", private$sid)
                          private$unlock()
                          if(private$newScen && !private$scenSaved){
                            flog.debug("Scenario was not saved. Thus, it will be removed.")
                            self$delete()
                          }
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
                      dbSchema            = vector("list", 3L),
                      scode               = integer(1L),
                      scenSaved           = logical(1L),
                      newScen             = logical(1L),
                      traceData           = tibble(),
                      dataFromOtherMode   = logical(1L),
                      localAttachments    = list(filePaths = character(0L), 
                                                 execPerm = logical(0L)),
                      attachmentsToRemove = character(0L),
                      attachmentsUpdateExec = list(name = character(0L),
                                                   execPerm = logical(0L)),
                      dirtyMetadata       = FALSE,
                      dirtyAttachments    = FALSE,
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
                            private$dataFromOtherMode <- TRUE
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
                          flog.debug("Db: Scenario wasn't locked as it is readonly (Scenario.lock).")
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
                                            if(inherits(private$conn, "PostgreSQL")) 
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
                      readBlob = function(){
                        # reads blob data and returns tibble with meta data and file content
                        filePaths <- private$localAttachments$filePaths
                        fileNames <- basename(filePaths)
                        execPerm  <- private$localAttachments$execPerm
                        
                        fileSize <- file.info(filePaths, extra_cols = FALSE)$size
                        
                        if(any(fileSize > private$attachmentConfig[["maxSize"]])){
                          stop("maxSizeException", call. = FALSE)
                        }
                        content <- blob::new_blob(lapply(seq_along(filePaths), 
                                                         function(i) readBin(filePaths[[i]], "raw", n = fileSize[[i]])))
                        return(tibble(fileName = fileNames, fileExt = tools::file_ext(filePaths), 
                                      execPerm = execPerm, 
                                      fileContent = content, timestamp = as.character(Sys.time(), usetz = TRUE, tz = "GMT")))
                      },
                      cleanLocalFiles = function(){
                        if(length(private$localAttachments$filePaths)){
                          removedLocalFiles <- file.remove(private$localAttachments$filePaths)
                          if(any(!removedLocalFiles))
                            flog.warn("Some local attachments could not be removed: '%s'.",
                                      paste(private$localAttachments$filePaths[!removedLocalFiles], 
                                            collapse = "', '"))
                        }
                        return(invisible(self))
                      }
                    )
)