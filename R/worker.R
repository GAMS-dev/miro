Worker <- R6Class("Worker", public = list(
  initialize = function(metadata, remote, hcube, db = NULL){
    stopifnot(is.list(metadata), is.logical(remote), 
              identical(length(remote), 1L), is.logical(hcube), 
              identical(length(hcube), 1L))
    private$remote   <- remote
    private$hcube    <- hcube
    private$metadata <- metadata
    if(length(db)){
      private$db <- db
      private$conn <- db$getConn()
      dbSchema <- db$getDbSchema()
      private$dbColNames <- dbSchema$colNames[['_jobMeta']]
      private$dbTabName <- dbSchema$tabName[['_jobMeta']]
    }
    return(invisible(self))
  },
  logout = function(){
    private$metadata$url       <- ""
    private$metadata$username  <- private$metadata$uid
    private$metadata$password  <- ""
    private$metadata$namespace <- ""
    private$metadata$useRegistered <- FALSE
    private$authHeader <- character(0L)
    unlink(private$metadata$rememberMeFileName, force = TRUE)
  },
  setCredentials = function(url, username, password, namespace,
                            useRegistered){
    private$metadata$url       <- url
    private$metadata$username  <- username
    private$metadata$useRegistered <- useRegistered
    private$metadata$password  <- password
    private$metadata$namespace <- namespace
    private$buildAuthHeader(TRUE)
    return(invisible(self))
  },
  login = function(url, username, password, namespace, 
                   rememberMeFlag = FALSE, 
                   useRegistered = FALSE){
    stopifnot(isFALSE(private$metadata$noNeedCred))
    url <- trimws(url, "right", whitespace = "/")
    private$metadata$url       <- url
    
    if(!startsWith(url, "https://")){
      stop(426, call. = FALSE)
    }
    ret <- HEAD(url, timeout(2L))$url
    if(!startsWith(ret, "https://")){
      stop(426, call. = FALSE)
    }
    private$metadata$username  <- username
    private$metadata$useRegistered <- useRegistered
    private$metadata$password  <- password
    private$buildAuthHeader(FALSE)
    
    private$metadata$namespace <- namespace
    ret <- GET(url = paste0(url, "/namespaces/", namespace, "/permissions/me"), 
                add_headers(Authorization = private$authHeader,
                            Timestamp = as.character(Sys.time(), usetz = TRUE)), 
                timeout(2L))
    retContent <- tryCatch({
      content(ret, type = "application/json", 
              encoding = "utf-8")
    }, error = function(e){
      stop(404L, call. = FALSE)
    })
    if(identical(status_code(ret), 404L)){
      stop(400L, call. = FALSE)
    }
    
    if(identical(status_code(ret), 200L)){
      permissionLevel <- retContent$permission
      
      if(identical(useRegistered, TRUE)){
        if(permissionLevel < 5L)
          stop(403L, call. = FALSE)
        if(rememberMeFlag){
          private$saveLoginCredentials()
        }
        return(200L)
      }
      if(permissionLevel < 7L)
        stop(403L, call. = FALSE)
      if(rememberMeFlag){
        private$saveLoginCredentials()
      }
      return(200L)
    }else if(status_code(ret) %in% c(401L, 403L)){
      stop(401L, call. = FALSE)
    }else{
      stop(500L, call. = FALSE)
    }
    return(invisible(self))
  },
  getCredentials = function(){
    return(list(url = private$metadata$url,
                user = private$metadata$username,
                ns = private$metadata$namespace,
                reg = private$metadata$useRegistered))
  },
  validateCredentials = function(){
    cred <- c(private$metadata$url,
              private$metadata$password, 
              private$metadata$namespace)
    if(private$remote &&
       !identical(private$metadata$noNeedCred, TRUE) &&
       (!length(cred) ||
        any(vapply(cred, private$isEmptyString, 
                   logical(1L), USE.NAMES = FALSE))))
      return(FALSE)
    return(TRUE)
  },
  setWorkDir = function(workDir){
    private$workDir <- workDir
  },
  run = function(inputData, pfFileContent = NULL, sid = NULL){
    if(length(pfFileContent)){
      stopifnot(is.character(pfFileContent), length(pfFileContent) > 0L)
      private$pfFileContent <- pfFileContent
    }else{
      private$pfFileContent <- NULL
    }
    private$initRun(sid)
    
    if(private$remote){
      private$runRemote(inputData)
      return(0L)
    }
    private$runLocal(inputData)
    return(0L)
  },
  runHcube = function(staticData = NULL, dynamicPar = NULL, sid = NULL, tags = NULL, 
                      attachmentFilePaths = NULL){
    req(length(private$db) > 0L)
    
    private$initRun(sid)
    private$jID <- self$addJobDb("", sid, tags = tags)
    flog.trace("Metadata for Hypercube job was written to database. Hypercube job ID: '%d' was assigned to job.", private$jID)
    
    if(private$remote){
      stop("not implemented", call. = FALSE)
      private$runHcubeRemote(dynamicPar, staticData)
      pID <- private$process
    }else{
      private$runHcubeLocal(dynamicPar, staticData, attachmentFilePaths)
      pID <- private$process$get_pid()
    }
    
    flog.trace("Hypercube job submitted successfuly. Hypercube job process ID: '%d'.", pID)
    self$updateJobStatus(status = JOBSTATUSMAP[["running"]], updatePid = !private$remote)
    flog.trace("Process ID: '%d' added to Hypercube job ID: '%d'.", pID, private$jID)
    return(0L)
  },
  interrupt = function(hardKill = NULL, process = NULL){
    if(!is.null(private$status)){
      return()
    }
    
    if(is.null(hardKill)){
      hardKill <- FALSE
      if(private$hardKill){
        hardKill <- TRUE
      }else{
        private$hardKill <- TRUE
      }
    }
    if(is.null(process)){
      process <- private$process
    }
    
    if(private$remote)
      return(private$interruptRemote(hardKill, process))
    
    return(private$interruptLocal(hardKill, process))
  },
  updateJobStatus = function(status, jID = NULL, tags = NULL, updatePid = FALSE){
    if(!private$hcube){
      tags <- NULL
    }else if(length(tags)){
      tags <- vector2Csv(tags)
    }
    if(is.null(jID)){
      req(length(private$process) > 0L)
      
      jID <- private$jID
      if(private$remote){
        pID <- private$process
      }else{
        pID <- private$process$get_pid()
      }
    }else{
      pID <- self$getPid(jID)
    }
    if(!is.integer(jID) || jID < 0L){
      flog.warn("Could not update job status as job ID is invalid.")
      return()
    }
    if(length(pID) == 0L){
      flog.warn("Could not update job status as job process ID is invalid.")
      return()
    }
    gamsRetCode <- NULL
    if(status %in% c(JOBSTATUSMAP[['imported']], 
                     JOBSTATUSMAP[['discarded']])){
      gamsRetCode <- private$getGAMSRetCode(pID)
      
      if(status == JOBSTATUSMAP[['discarded']]){
        self$interrupt(hardKill = TRUE, process = pID)
        if(length(gamsRetCode)){
          status <- JOBSTATUSMAP[['discarded(completed)']]
        }else{
          status <- JOBSTATUSMAP[['discarded(running)']]
        }
      }
    }
    
    colNames <- private$dbColNames[['status']]
    values   <- status
    
    if(length(gamsRetCode)){
      colNames <- c(colNames, private$dbColNames[['gamsret']])
      values   <- c(values, gamsRetCode)
    }
    if(length(tags)){
      colNames <- c(colNames, private$dbColNames[['tag']])
      values   <- c(values, tags)
    }
    if(updatePid){
      colNames <- c(colNames, private$dbColNames[['pid']])
      values   <- c(values, pID)
    }
    private$db$updateRows(private$dbTabName, 
                          tibble(private$dbColNames[[1L]], jID), 
                          colNames = colNames, values = values)
    
    return(invisible(self))
  },
  getJobList = function(jobHist = FALSE){
    newCompleted <- FALSE
    jobList <- private$db$importDataset(private$dbTabName, 
                                        tibble(c(private$dbColNames[['uid']], 
                                                 private$dbColNames[['status']],
                                                 private$dbColNames[['scode']]),
                                               c(private$metadata$uid,
                                                 JOBSTATUSMAP[['discarded']],
                                                 if(private$hcube)
                                                   SCODEMAP[['hcube_jobconfig']]
                                                 else
                                                   SCODEMAP[['scen']]), 
                                               c('=', if(jobHist) '>=' else '<', '=')),
                                        orderBy = private$dbColNames[['time']], orderAsc = FALSE)
    if(jobHist)
      return(list(jobList = jobList, newCompleted = FALSE))
    
    private$jobList <- jobList
    
    if(!length(jobList) || !nrow(jobList)){
      private$jobListInit <- TRUE
      return(list(jobList = private$jobList, newCompleted = newCompleted))
    }
    
    jIDs <- private$jobList[[1]]
    pIDs <- private$jobList[[6]]
    jStatus <- private$jobList[[3]]
    
    for(i in seq_along(jIDs)){
      if(jStatus[i] == JOBSTATUSMAP[['completed']])
        next
      newStatus <- NULL
      if(private$remote){
        gamsRetCode <- private$getGAMSRetCode(pIDs[i])
        if(length(gamsRetCode)){
          if(!private$jobListInit)
            newCompleted <- TRUE
          newStatus <- JOBSTATUSMAP[['completed']]
        }
      }else{
        jobDir <- file.path(private$metadata$currentModelDir, hcubeDirName, jIDs[i])
        if(dir.exists(jobDir)){
          if(file.exists(file.path(jobDir, "4upload.zip"))){
            newStatus <- JOBSTATUSMAP[['completed']]
          }else if(jStatus[i] < JOBSTATUSMAP[["corrupted"]]){
            if(!pidExists(pIDs[i])){
              flog.info("Job with ID: '%s' is not running anymore, but results archive could not be found. Job was marked: 'corrupted'.",
                        private$jID)
              newStatus <- JOBSTATUSMAP[['corrupted(noProcess)']]
            }
          } 
        }else if(jStatus[i] < JOBSTATUSMAP[["corrupted"]]){
          flog.info("Job with ID: '%s' could not be accessed (directory is missing or lacks read permissions). Job was marked: 'corrupted'.", 
                    private$jID)
          newStatus <- JOBSTATUSMAP[['corrupted(noDir)']]
        }
      }
      if(length(newStatus)){
        self$updateJobStatus(newStatus, jIDs[i])
        private$jobList[i, 3] <- newStatus
      }
    }
    private$jobListInit <- TRUE
    return(list(jobList = private$jobList, newCompleted = newCompleted))
  },
  getPid = function(jID){
    if(!length(private$jobList) || !nrow(private$jobList))
      return(NULL)
    jIDs <- private$jobList[[1]]
    jIdx <- match(jID, jIDs)
    if(is.na(jIdx))
      return(NULL)
    return(private$jobList[[6]][[jIdx]])
  },
  getSid = function(jID){
    if(!length(private$jobList) || !nrow(private$jobList))
      return(NULL)
    jIDs <- private$jobList[[1]]
    jIdx <- match(jID, jIDs)
    if(is.na(jIdx))
      return(NULL)
    return(private$jobList[[7]][[jIdx]])
  },
  getStatus = function(jID){
    if(!length(private$jobList) || !nrow(private$jobList))
      return(NULL)
    jIDs <- private$jobList[[1]]
    jIdx <- match(jID, jIDs)
    if(is.na(jIdx))
      return(NULL)
    return(private$jobList[[3]][[jIdx]])
  },
  readOutput = function(jID){
    req(private$remote)
    
    tmpdir <- tempdir(TRUE)
    tmpdir <- file.path(tmpdir, jID)
    if(dir.exists(tmpdir)){
      if(!identical(unlink(tmpdir, recursive = TRUE, force = TRUE), 0L)){
        stop(sprintf("Problems removing existing directory: '%s'.", tmpdir), call. = FALSE)
      }
    }
    if(!dir.create(tmpdir)){
      stop("Problems creating temporary directory for saving results.", call. = FALSE)
    }
    retCode <- private$readRemoteOutput(jID, tmpdir)
    if(!identical(retCode, 0L)){
      stop(retCode, call. = FALSE)
    }
    
    
    return(tmpdir)
  },
  readTextEntity = function(name, jID){
    req(private$remote)
    return(private$readRemoteTextEntity(name, jID, saveDisk = FALSE, 
                                        maxSize = private$metadata$maxSizeToRead))
  },
  pingProcess = function(){
    if(is.integer(private$status)){
      return(private$status)
    }
    if(inherits(private$process, "process")){
      return(private$pingLocalProcess())
    }
    return(private$pingRemoteProcess())
  },
  getReactiveLog = function(session){
    return(reactivePoll2(500, session, checkFunc = function(){
      private$updateLog
    }, valueFunc = function(){
      private$log
    }))
  },
  getReactiveStatus = function(session){
    return(reactivePoll2(1000, session, checkFunc = function(){
      self$pingProcess()
    }, valueFunc = function(){
      private$status
    }))
  },
  addJobDb = function(pid, sid, tags = NULL, status = NULL){
    stopifnot(length(pid) == 1)
    
    if(!is.null(sid)){
      stopifnot(length(sid) == 1, is.integer(sid))
    }
    if(length(status)){
      stopifnot(length(status) == 1L, status %in% JOBSTATUSMAP)
    }else{
      status <- JOBSTATUSMAP[['running']]
    }
    colNames <- private$dbColNames
    tabName <- private$dbTabName
    err <- FALSE
    tryCatch({
      private$db$createJobMeta()
    }, error = function(e){
      flog.error("Problems creating job metadata table. Error message: '%s'.", 
                 conditionMessage(e))
      err <<- TRUE
    })
    if(err)
      return(-1L)
    tryCatch({
      query <- paste0("INSERT INTO",
                      DBI::dbQuoteIdentifier(private$conn, tabName), 
                      " (", DBI::dbQuoteIdentifier(private$conn, colNames[[2]]), ",",
                      DBI::dbQuoteIdentifier(private$conn, colNames[[3]]), ",",
                      DBI::dbQuoteIdentifier(private$conn, colNames[[4]]), ",",
                      DBI::dbQuoteIdentifier(private$conn, colNames[[5]]), ",",
                      DBI::dbQuoteIdentifier(private$conn, colNames[[6]]), ",",
                      DBI::dbQuoteIdentifier(private$conn, colNames[[7]]), ",",
                      DBI::dbQuoteIdentifier(private$conn, colNames[[9]]),
                      ") VALUES (",  
                      DBI::dbQuoteLiteral(private$conn, private$metadata$uid), ",", 
                      status, ",",
                      DBI::dbQuoteLiteral(private$conn, as.character(Sys.time(), usetz = TRUE)), ",",
                      DBI::dbQuoteLiteral(private$conn, vector2Csv(tags)), ",",
                      DBI::dbQuoteLiteral(private$conn, as.character(pid)), ",",
                      DBI::dbQuoteLiteral(private$conn, sid), ",", 
                      DBI::dbQuoteLiteral(private$conn, 
                                          if(private$hcube) 
                                            SCODEMAP[['hcube_jobconfig']]
                                          else
                                            SCODEMAP[['scen']]), ")",
                      if(inherits(private$conn, "PostgreSQL")) 
                        paste0(" RETURNING", DBI::dbQuoteIdentifier(private$conn, colNames[[1]]))
      )
      if(inherits(private$conn, "PostgreSQL")) {
        jID <- DBI::dbGetQuery(private$conn, query)[[1L]][1L]
      }else{
        DBI::dbExecute(private$conn, query)
        # need to send second SQL statement because SQLite doesn't support RETURNING function
        query <- paste0("SELECT last_insert_rowid() FROM ", 
                        DBI::dbQuoteIdentifier(private$conn, tabName))
        jID <- DBI::dbGetQuery(private$conn, query)[[1]][1L]
      }
      return(jID)
    }, error = function(e){
      flog.error("Problems writing job metadata. Error message: '%s'.", 
                 conditionMessage(e))
    })
    return(-1L)
  }
), private = list(
  remote = logical(1L),
  hcube = logical(1L),
  status = NULL, 
  jID = NULL,
  db = NULL,
  conn = NULL,
  jobListInit = FALSE,
  dbTabName = character(1L),
  dbColNames = character(1L),
  sid = NULL,
  metadata = NULL,
  inputData = NULL,
  log = character(1L),
  authHeader = character(1L),
  pfFileContent = NULL,
  process = NULL,
  workDir = NULL,
  hardKill = FALSE,
  updateLog = 0L,
  gamsRet = NULL,
  waitCnt = integer(1L),
  wait = integer(1L),
  fRemoteSub = NULL,
  fRemoteRes = NULL,
  jobList = NULL,
  runLocal = function(inputData){
    private$status  <- NULL
    inputData$writeCSV(private$workDir, delim = private$metadata$csvDelim)
    
    gamsArgs <- c(if(length(private$metadata$extraClArgs)) private$metadata$extraClArgs, 
                  paste0('idir1="', gmsFilePath(private$metadata$currentModelDir), '"'),
                  if(private$metadata$includeParentDir) paste0('idir2="', gmsFilePath(dirname(private$metadata$currentModelDir)), '"'), 
                  paste0('curdir="', private$workDir, '"'), "lo=3", paste0("execMode=", private$metadata$gamsExecMode), 
                  private$metadata$MIROSwitch, "LstTitleLeftAligned=1")
    if(private$metadata$saveTraceFile){
      gamsArgs <- c(gamsArgs, paste0('trace="', tableNameTracePrefix, private$metadata$modelName, '.trc"'), "traceopt=3")
    }
    pfFilePath <- gmsFilePath(paste0(private$workDir, tolower(private$metadata$modelName), ".pf"))
    writeLines(c(private$pfFileContent, gamsArgs), pfFilePath)
    
    private$process <- process$new(paste0(private$metadata$gamsSysDir, "gams"), 
                                   args = c(private$metadata$modelGmsName, "pf", pfFilePath), 
                                   stdout = "|", windows_hide_window = TRUE)
    return(self)
  },
  runHcubeLocal = function(dynamicPar, staticData = NULL, attachmentFilePaths = NULL){
    
    hcubeDir <- file.path(private$metadata$currentModelDir, hcubeDirName, private$jID)
    
    if(dir.exists(hcubeDir)){
      flog.warn("Hypercube job directory: '%s' already existed. Removing old data.", hcubeDir)
      if(!identical(unlink(hcubeDir, recursive = TRUE, force = TRUE), 0L))
        stop(sprintf("Problems removing Hypercube job directory: '%s'.", hcubeDir), call. = FALSE)
    }
    
    if(!dir.create(hcubeDir, recursive = TRUE, showWarnings = FALSE))
      stop(sprintf("Problems creating Hypercube job directory: '%s'.", hcubeDir), call. = FALSE)
    
    staticDir <- file.path(hcubeDir, "static")
    
    if(!dir.create(staticDir))
      stop(sprintf("Problems creating static directory: '%s'.", staticDir), call. = FALSE)
    
    if(length(staticData))
      staticData$writeCSV(staticDir, delim = private$metadata$csvDelim)
    
    if(length(attachmentFilePaths)){
      if(!dir.create(staticDir) || !all(file.copy(attachmentFilePaths, staticDir)))
        stop(sprintf("Problems writing attachment data to: '%s'.", 
                     staticDir), call. = FALSE)
    }
    
    scenGmsPar <- paste0(dynamicPar, ' idir1="', gmsFilePath(private$metadata$currentModelDir), '"')
    
    if(identical(private$metadata$includeParentDir, TRUE))
      scenGmsPar <- paste0(scenGmsPar, ' idir2="', gmsFilePath(dirname(private$metadata$currentModelDir)), '"')
    
    writeLines(scenGmsPar, file.path(hcubeDir, tolower(private$metadata$modelName) %+% ".hcube"))
    
    flog.trace("New folder for Hypercube job was created: '%s'.", hcubeDir)
    
    # create daemon to execute Hypercube job
    hcubeSubmDir <- gmsFilePath(file.path(getwd(), "resources", hcubeSubmissionFile %+% ".gms"))
    curdir       <- gmsFilePath(hcubeDir)
    
    tryCatch({
      writeChar(paste0("1/ ", length(dynamicPar), "\n"), file.path(hcubeDir, private$jID %+% ".log"), 
                eos = NULL)
    }, error = function(e){
      flog.warn("Log file: '%s' could not be written. Check whether you have sufficient permissions to write files to: '%s'.",
                private$jID %+% ".log", hcubeDir)
    })
    private$process <- process$new(paste0(private$metadata$gamsSysDir, "gams"), 
                                   args = c(hcubeSubmDir, 'curdir', curdir, "lo=3",
                                            paste0("--jobID=", private$jID)),
                                   cleanup = FALSE, cleanup_tree = FALSE, supervise = FALSE,
                                   windows_hide_window = TRUE)
    return(invisible(self))
  },
  runRemote = function(inputData){
    private$status  <- "s"
    private$fRemoteSub  <- future({
      suppressWarnings(suppressMessages({
        library(zip)
        library(httr)
        library(R6)
        library(readr)
      }))
      gamsArgs <- c(if(length(metadata$extraClArgs)) metadata$extraClArgs, 
                    paste0("execMode=", metadata$gamsExecMode), 
                    metadata$MIROSwitch)
      if(metadata$saveTraceFile){
        gamsArgs <- c(gamsArgs, paste0('trace="', tableNameTracePrefix, metadata$modelName, '.trc"'), "traceopt=3")
      }
      pfFilePath <- gmsFilePath(paste0(workDir, tolower(metadata$modelName), ".pf"))
      writeLines(c(pfFileContent, gamsArgs), pfFilePath)
      requestBody <- list(model = metadata$modelName,
                          use_pf_file = TRUE, text_entities = paste(metadata$text_entities, collapse = ","),
                          stdout_filename = paste0(metadata$modelName, ".log"),
                          namespace = metadata$namespace,
                          data = upload_file(inputData$
                                               writeCSV(workDir, delim = metadata$csvDelim)$
                                               addFilePaths(pfFilePath)$
                                               compress(), 
                                             type = 'application/zip'))
      
      if(identical(metadata$useRegistered, FALSE)){
        requestBody$model_data <- upload_file(DataInstance$new()$
                                                addFilePaths(metadata$modelData)$
                                                compress(recurse = TRUE), 
                                              type = 'application/zip')
      }
      ret <- POST(paste0(metadata$url, "/jobs"), encode = "multipart", 
                  body = requestBody,
                  add_headers(Authorization = authHeader, 
                              Timestamp = as.character(Sys.time(), 
                                                       usetz = TRUE)),
                  timeout(metadata$timeout))
      if(identical(status_code(ret), 201L)){
        return(content(ret)$token)
      }else{
        msg <- tryCatch(content(ret, type = 'application/json')$message,
                        error = function(e){
                          return("")
                        })
        return(paste0("error:", status_code(ret), msg))
      }
    }, globals = list(metadata = private$metadata, workDir = private$workDir,
                      pfFileContent = private$pfFileContent, inputData = inputData,
                      tableNameTracePrefix = tableNameTracePrefix, authHeader = private$authHeader,
                      gmsFilePath = gmsFilePath, DataInstance = DataInstance, 
                      isWindows = isWindows))
    return(self)
  },
  readRemoteTextEntity = function(text_entity, jID = NULL, saveDisk = TRUE, maxSize = NULL){
    if(is.null(jID)){
      jID <- private$process
    }
    if(!is.null(maxSize)){
      ret <- HEAD(paste0(private$metadata$url, "/jobs/", jID, "/text-entity/", 
                         URLencode(text_entity)),
                  add_headers(Authorization = private$authHeader,
                              Timestamp = as.character(Sys.time(), usetz = TRUE)),
                  timeout(2L))
      
      if(!identical(status_code(ret), 200L)){
        return(status_code(ret))
      }
      teLength <- tryCatch({
        suppressWarnings(as.numeric(headers(ret)[['char_length']]))
      }, error = function(e){
        return(404L)
      })
      if(identical(teLength, 404L) || is.na(teLength)){
        return(404L)
      }
    }else{
      teLength <- NULL
    }
    
    ret <- GET(paste0(private$metadata$url, "/jobs/", jID, "/text-entity/", 
                      URLencode(text_entity)),
               query = if(!is.null(teLength)) 
                 list(start_position = teLength - maxSize, length = teLength),
               if(saveDisk)
                 write_disk(file.path(private$workDir, text_entity), overwrite = TRUE),
               add_headers(Authorization = private$authHeader,
                           Timestamp = as.character(Sys.time(), usetz = TRUE)),
               timeout(10L))
    if(saveDisk){
      return(status_code(ret))
    }
    if(identical(status_code(ret), 200L)){
      return(content(ret, encoding = "utf-8"))
    }
    return(status_code(ret))
  },
  pingLocalProcess = function(){
    tryCatch(
      private$log <- private$process$read_output(),
    error = function(e){
      private$log <- ""
    })
    if(length(private$gamsRet)){
      private$status <- private$gamsRet
      return(private$status)
    }
    exitStatus  <- private$process$get_exit_status()
    if(!identical(private$log, "")){
      private$updateLog <- private$updateLog + 1L
    }
    if(length(exitStatus)){
      private$gamsRet <- exitStatus
    }
    return(private$status)
  },
  pingRemoteProcess = function(){
    if(private$wait > 0L){
      private$wait <- private$wait - 1L
      return(private$status)
    }
    if(length(private$fRemoteSub)){
      if(resolved(private$fRemoteSub)){
        noError <- TRUE
        tryCatch({
          remoteSubValue <- value(private$fRemoteSub)
        }, error = function(e){
          errMsg <- conditionMessage(e)
          flog.error(errMsg)
          if(startsWith(errMsg, "Could not") || startsWith(errMsg, "Timeout was")){
            private$status <- -404L
          }else{
            private$status <- -500L
          }
          noError <<- FALSE
        })
        if(!noError){
          return(private$status)
        }
        private$wait    <- 0L
        private$waitCnt <- 0L
        if(startsWith(remoteSubValue, "error:")){
          errCode <- suppressWarnings(as.integer(substring(remoteSubValue, 7)))
          flog.info(paste0("Could not execute model remotely. Error code: ", errCode))
          if(is.na(errCode)){
            private$status <- -500L
          }else{
            private$status <- -errCode
          }
        }else{
          private$process <- value(private$fRemoteSub)
          if(length(private$db)){
            tryCatch({
              private$jID <- self$addJobDb(private$process, private$sid)
            }, error = function(e){
              flog.warn("Could not add job to database. Error message; '%s'.", 
                        conditionMessage(e))
            })
          }
          private$status  <- "q"
        }
        private$fRemoteSub <- NULL
      }else{
        private$wait <- bitwShiftL(2L, private$waitCnt)
        if(private$waitCnt < private$metadata$timeout){
          private$waitCnt <- private$waitCnt + 1L
        }else{
          private$status <- -404L
        }
      }
      return(private$status)
    }
    if(length(private$gamsRet)){
      if(resolved(private$fRemoteRes)){
        resVal <- value(private$fRemoteRes)
        private$status <- private$gamsRet
        if(!identical(resVal, 0L)){
          if(identical(resVal, -100L)){
            private$status <- -100L
            flog.error("Fetching results timed out.")
          }else{
            flog.error(resVal)
            private$status <- -500L
          }
        }
      }else{
        private$wait <- bitwShiftL(2L, private$waitCnt)
        if(private$waitCnt < private$metadata$timeout){
          private$waitCnt <- private$waitCnt + 1L
        }else{
          private$status <- -404L
        }
      }
      return(private$status)
    }
    ret <- DELETE(paste0(private$metadata$url, "/jobs/", private$process, "/unread-logs"), 
                  add_headers(Authorization = private$authHeader,
                              Timestamp = as.character(Sys.time(), usetz = TRUE)),
                  timeout(2L))
    statusCode <- status_code(ret)
    if(identical(statusCode, 200L)){
      responseContent <- tryCatch({
        content(ret, type = "application/json", 
                encoding = "utf-8")
      }, error = function(e){
        private$status <- -404L
        return(-1L)
      })
      if(identical(responseContent, -1L)){
        return(private$status)
      }
      if(identical(responseContent$queue_finished, TRUE)){
        private$gamsRet <- responseContent$gams_return_code
        private$wait    <- 0L
        private$waitCnt <- 0L
        private$fRemoteRes  <- future({
          library(httr)
          private$readRemoteOutput()
        })
        private$status <- "d"
      }else{
        private$status <- NULL
      }
      private$log <- responseContent$message
      if(!identical(private$log, "")){
        private$updateLog <- private$updateLog + 1L
      }
      return(private$status)
    }else if(identical(statusCode, 308L)){
      # job finished, get full log
      ret <- private$getRemoteStatus(private$process)
      gamsRetCode <- content(ret)$gams_return_code
      
      if(is.null(gamsRetCode)){
        private$status <- -500L
      }else{
        private$status <- gamsRetCode
      }
    }else if(identical(statusCode, 403L)){
      private$wait <- bitwShiftL(2L, private$waitCnt)
      if(private$waitCnt < private$metadata$timeout){
        private$waitCnt <- private$waitCnt + 1L
      }else{
        private$status <- -404L
      }
    }else{
      private$status <- -500L
    }
    return(private$status)
  },
  readRemoteOutput = function(jID = NULL, workDir = NULL){
    if(is.null(jID)){
      jID <- private$process
    }
    if(is.null(workDir)){
      workDir <- private$workDir
    }
    if(!length(jID)){
      return("Process not started")
    }
    tmp <- tempfile(pattern="res_", fileext = ".zip")
    on.exit(unlink(tmp))
    timeout <- FALSE
    tryCatch(
      ret <- GET(url = paste0(private$metadata$url, "/jobs/", jID, "/result"), 
                 write_disk(tmp), 
                 add_headers(Authorization = private$authHeader,
                             Timestamp = as.character(Sys.time(), usetz = TRUE)),
                 timeout(100L)),
      error = function(e){
        timeout <<- TRUE
      })
    if(timeout){
      return(-100L)
    }
    for(text_entity in c(paste0(private$metadata$modelName, ".log"), private$metadata$text_entities)){
      tryCatch(private$readRemoteTextEntity(text_entity),
               error = function(e){
                 warning(sprintf("Problems fetching text entity: '%s'. Error message: '%s'.", 
                                 text_entity, conditionMessage(e)))
               })
    }
    
    if(identical(status_code(ret), 200L)){
      unzip(tmp, exdir = workDir)
    }else{
      return(content(ret)$message)
    }
    return(0L)
  },
  getRemoteStatus = function(jID){
    GET(paste0(private$metadata$url, "/jobs/", jID, "/status"),
        add_headers(Authorization = private$authHeader,
                    Timestamp = as.character(Sys.time(), usetz = TRUE)),
        timeout(2L))
  },
  interruptLocal = function(hardKill = FALSE, process = NULL){
    errMsg <- NULL
    
    if(is.R6(process)){
      tryCatch({
        if(hardKill){
          process$kill_tree()
        }else{
          process$signal(tools::SIGINT)
        }
      }, error= function(e){
        errMsg <<- "error"
        pID    <<- process$get_pid()
      })
    }else{
      errMsg <- "External process"
      pID <- suppressWarnings(as.integer(process))
      
      if(is.na(pID)){
        flog.error("Invalid process id: '%s'. Could not interrupt job", pID)
        return(0L)
      }
      if(!pidExists(pID)){
        return(0L)
      }
    }
    if(!is.null(errMsg)){
      errMsg <- NULL
      if(private$metadata$serverOS == 'windows'){
        tryCatch({
          processx::run(command = 'taskkill', args = c(if(hardKill) "/F", 
                                                       "/PID", 
                                                       pID, 
                                                       "/T"), 
                        windows_hide_window = TRUE, timeout = 10L)
        }, error = function(e){
          flog.error("Problems interrupting process with pid: %s. Error message: '%s'.",
                     pID, conditionMessage(e))
        })
      }else if(private$metadata$serverOS %in% c('linux', 'osx')){
        tryCatch({
          processx::run(command = 'kill', 
                        args = c(if(hardKill) "-SIGKILL" else "-SIGINT",
                                 -pID), timeout = 10L)
        }, error = function(e){
          flog.error("Problems interrupting process with pid: %s. Error message: '%s'.",
                     pID, conditionMessage(e))
        })
      }else{
        flog.error("Operating system: '%s' not supported.", private$metadata$serverOS)
      }
    }
    return(0L)
  },
  interruptRemote = function(hardKill = FALSE, process = NULL){
    if(!length(process)){
      return("Process not started")
    }
    private$validateAPIResponse(DELETE(
      url = paste0(private$metadata$url, "/jobs/", process), 
      body = list(hard_kill = hardKill),
      add_headers(Authorization = private$authHeader,
                  Timestamp = as.character(Sys.time(), usetz = TRUE)),
      timeout(2L)))
    
    return(0L)
  },
  isEmptyString = function(string){
    if(!length(string) || identical(string, ""))
      return(TRUE)
    return(FALSE)
  },
  buildAuthHeader = function(useTokenAuth){
    if(useTokenAuth){
      private$authHeader <- paste0("Bearer ", private$metadata$password)
      return(invisible(self))
    }
    private$authHeader <- paste0("Basic ", 
                                 base64_encode(charToRaw(
                                   paste0(private$metadata$username, 
                                          ":", private$metadata$password))))
    return(invisible(self))
  },
  saveLoginCredentials = function(){
    sessionToken <- private$validateAPIResponse(POST(
      url = paste0(url, "/auth/"), 
      add_headers(Authorization = private$authHeader,
                  Timestamp = as.character(Sys.time(), usetz = TRUE)), 
      timeout(2L)))$token
    
    private$metadata$password  <- sessionToken
    write_json(list(url = url,
                    username = username, 
                    password = sessionToken,
                    namespace = namespace,
                    reg = useRegistered), 
               private$metadata$rememberMeFileName, auto_unbox = TRUE)
    return(ininvisible(self))
  },
  initRun = function(sid){
    private$sid        <- sid
    private$log        <- character(1L)
    private$gamsRet    <- NULL
    private$fRemoteRes <- NULL
    private$fRemoteSub <- NULL
    private$jID        <- NULL
    private$hardKill   <- FALSE
    private$updateLog  <- 0L
    private$wait       <- 0L
    private$waitCnt    <- 0L
    
    return(invisible(self))
  },
  getGAMSRetCode = function(pID){
    if(private$remote){
      return(private$validateAPIResponse(
        private$getRemoteStatus(pID))$gams_return_code)
    }
    return("")
  },
  validateAPIResponse = function(response){
    ret <- tryCatch({
      content(response, 
              type = "application/json", 
              encoding = "utf-8")
    }, error = function(e){
      stop(404L, call. = FALSE)
    })
    if(status_code(response) >= 300L){
      stop(status_code(response), 
           call. = FALSE)
    }
    return(ret)
  }
))

progressBar <- function(session, type = c("down", "up")){
  type <- match.arg(type)
  
  httr:::request(options = list(
    noprogress = FALSE,
    progressfunction = progressBarMIRO(session, type)
  ))
}
progressBarMIRO <- function(session, type) {
  bar <- NULL
  
  show_progress <- function(down, up) {
    if (type == "down") {
      total <- down[[1]]
      now <- down[[2]]
    } else {
      total <- up[[1]]
      now <- up[[2]]
    }
    
    if (total == 0 && now == 0) {
      # Reset progress bar when seeing first byte
      bar <<- NULL
    } else if (total == 0) {
      
      # Can't automatically add newline on completion because there's no
      # way to tell when then the file has finished downloading
    } else {
      if (is.null(bar)) {
        bar <<- Progress$new(session, max = total)
        bar$set(message = paste0(if(type == "down") "Downloading" 
                                 else "Uploading",  "data"), value = 0)
        on.exit(bar$close())
      }
      bar$set(now)
      if (now == total) bar$close()
    }
    
    TRUE
  }
}
