Worker <- R6Class("Worker", public = list(
  initialize = function(metadata, remote){
    stopifnot(is.list(metadata), is.logical(remote), 
              identical(length(remote), 1L))
    private$remote   <- remote
    private$metadata <- metadata
  },
  setWorkDir = function(workDir){
    private$workDir <- workDir
  },
  run = function(inputData, pfFileContent = NULL){
    if(length(pfFileContent)){
      stopifnot(is.character(pfFileContent), length(pfFileContent) > 0L)
      private$pfFileContent <- pfFileContent
    }else{
      private$pfFileContent <- NULL
    }
    private$log     <- character(1L)
    private$gamsRet <- NULL
    private$fRemoteRes <- NULL
    private$wait    <- 0L
    private$waitCnt <- 0L
    
    if(private$remote){
      return(private$runRemote(inputData))
    }
    return(private$runLocal(inputData))
  },
  interrupt = function(){
    if(private$remote){
      return(private$interruptRemote())
    }
    return(private$interruptLocal())
  },
  pingProcess = function(){
    if(!length(private$process)){
      stop("Process not started", call. = FALSE)
    }
    if(is.integer(private$status)){
      return(private$updateLog)
    }
    if(inherits(private$process, "process")){
      return(private$pingLocalProcess())
    }
    return(private$pingRemoteProcess())
  },
  getReactiveLog = function(session){
    return(reactivePoll2(500, session, checkFunc = function(){
      self$pingProcess()
    }, valueFunc = function(){
      private$log
    }))
  },
  getReactiveStatus = function(session){
    return(reactivePoll2(1000, session, checkFunc = function(){
      private$status
    }, valueFunc = function(){
      private$status
    }))
  }
), private = list(
  remote = logical(1L),
  status = NULL, 
  metadata = NULL,
  inputData = NULL,
  log = character(1L),
  pfFileContent = NULL,
  process = NULL,
  workDir = NULL,
  updateLog = 1L,
  gamsRet = NULL,
  waitCnt = integer(1L),
  wait = integer(1L),
  fRemoteRes = NULL,
  runLocal = function(inputData){
    private$status  <- NULL
    inputData$writeCSV(private$workDir, delim = private$metadata$csvDelim)
    
    gamsArgs <- c(if(length(private$metadata$extraClArgs)) private$metadata$extraClArgs, 
                  paste0('idir1="', gmsFilePath(private$metadata$currentModelDir), '"'),
                  if(private$metadata$includeParentDir) paste0('idir2="', gmsFilePath(dirname(private$metadata$currentModelDir)), '"'), 
                  paste0('curdir="', private$workDir, '"'), "lo=4", paste0("execMode=", private$metadata$gamsExecMode), 
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
  runRemote = function(inputData){
    private$status  <- NA
    zipFilePath <- paste0(private$workDir, "data.zip")
    on.exit(unlink(zipFilePath))
    
    gamsArgs <- c(if(length(private$metadata$extraClArgs)) private$metadata$extraClArgs, 
                  paste0("execMode=", private$metadata$gamsExecMode), 
                  private$metadata$MIROSwitch)
    if(private$metadata$saveTraceFile){
      gamsArgs <- c(gamsArgs, paste0('trace="', tableNameTracePrefix, private$metadata$modelName, '.trc"'), "traceopt=3")
    }
    pfFilePath <- gmsFilePath(paste0(private$workDir, tolower(private$metadata$modelName), ".pf"))
    writeLines(c(private$pfFileContent, gamsArgs), pfFilePath)
    
    inputData$writeCSV(private$workDir, delim = private$metadata$csvDelim)$addFilePaths(pfFilePath)$compress(zipFilePath)
    
    ret <- POST(paste0(private$metadata$url, "/jobs"), encode = "multipart", 
                body = list(model = private$metadata$modelName, username = private$metadata$user,
                            use_pf_file = TRUE, text_entities = paste0(private$metadata$modelName, ".lst"),
                            stdout_filename = "log",
                            namespace = private$metadata$namespace,
                            data = upload_file(zipFilePath, 
                                               type = 'application/zip')),
                authenticate(private$metadata$user, private$metadata$password),
                add_headers(.headers = c("Timestamp" = as.character(Sys.time(), usetz = TRUE))),
                timeout(2L))
    
    if(identical(status_code(ret), 201L)){
      private$process <- content(ret)$token
    }else{
      stop(content(ret)$message, call. = FALSE)
    }

    return(self)
  },
  retrieveRemoteLog = function(){
    ret <- GET(paste0(private$metadata$url, "/jobs/", private$process, "/text-entity/log"),
               authenticate(private$metadata$user, private$metadata$password),
               add_headers(.headers = c("Timestamp" = as.character(Sys.time(), usetz = TRUE))),
               timeout(2L))
    if(!identical(status_code(ret), 200L)){
      stop(content(retFull)$message, call. = FALSE)
    }
    return(content(retFull)$message)
  },
  pingLocalProcess = function(){
    tryCatch(
      private$log <- private$process$read_output(),
    error = function(e){
      private$log <- ""
    })
    
    exitStatus  <- private$process$get_exit_status()
    if(!identical(private$log, "")){
      private$updateLog <- private$updateLog + 1L
    }
    if(length(exitStatus)){
      private$status <- exitStatus
    }
    return(private$updateLog)
  },
  pingRemoteProcess = function(){
    if(private$wait > 0L){
      private$wait <- private$wait - 1L
      return(private$updateLog)
    }
    if(length(private$gamsRet)){
      if(resolved(private$fRemoteRes)){
        private$status <- private$gamsRet
        if(!identical(value(private$fRemoteRes), 0L)){
          flog.error(value(private$fRemoteRes))
        }
      }else{
        private$wait <- bitwShiftL(2L, private$waitCnt)
        if(private$waitCnt < private$metadata$timeout){
          private$waitCnt <- private$waitCnt + 1L
        }else{
          private$status <- 100L
        }
      }
      return(private$updateLog)
    }
    ret <- DELETE(paste0(private$metadata$url, "/jobs/", private$process, "/unread-logs"), 
                  authenticate(private$metadata$user, private$metadata$password),
                  add_headers(.headers = c("Timestamp" = as.character(Sys.time(), usetz = TRUE))),
                  timeout(2L))
    statusCode <- status_code(ret)
    if(identical(statusCode, 200L)){
      responseContent <- content(ret)
      if(identical(responseContent$queue_finished, TRUE)){
        private$gamsRet <- responseContent$gams_return_code
        private$wait    <- 0L
        private$waitCnt <- 0L
        private$fRemoteRes  <- future({
          library(httr)
          private$readRemoteOutput()
          })
      }else{
        private$status <- NULL
      }
      private$log <- responseContent$message
      if(!identical(private$log, "")){
        private$updateLog <- private$updateLog + 1L
      }
      return(private$updateLog)
    }else if(identical(statusCode, 308L)){
      # job finished, get full log
      retContent <- content(ret)
      private$status <- retContent$gams_return_code
      return(private$updateLog)
    }else if(identical(statusCode, 403L)){
      private$wait <- bitwShiftL(2L, private$waitCnt)
      if(private$waitCnt < private$metadata$timeout){
        private$waitCnt <- private$waitCnt + 1L
      }else{
        private$status <- 100L
      }
      return(private$updateLog)
    }else{
      stop(content(ret)$message, call. = FALSE)
    }
  },
  readRemoteOutput = function(){
    if(!length(private$process)){
      return("Process not started")
    }
    
    tmp <- tempfile(pattern="res_", fileext = ".zip")
    on.exit(unlink(tmp))
    
    ret <- GET(url = paste0(private$metadata$url, "/jobs/", private$process, "/result"), 
               write_disk(tmp), authenticate(private$metadata$user, private$metadata$password),
               add_headers(.headers = c("Timestamp" = as.character(Sys.time(), usetz = TRUE))),
               timeout(2L))
    
    if(identical(status_code(ret), 200L)){
      unzip(tmp, exdir = private$workDir)
    }else{
      return(content(ret)$message)
    }
    return(0L)
  },
  interruptLocal = function(){
    if(!length(private$process)){
      return("Process not started")
    }
    errMsg <- NULL
    tryCatch({
      private$process$kill_tree()
    }, error= function(e){
      errMsg <<- "error"
    })
    if(!is.null(errMsg)){
      errMsg <- NULL
      if(private$metadata$serverOS == 'windows'){
        run(command = 'taskkill', args = c("/F", "/PID", private$process$get_pid(), "/T"), 
            windows_hide_window = TRUE)
      }else if(private$metadata$serverOS %in% c('linux', 'osx')){
        run(command = 'kill', args = c("-SIGKILL", -private$process$get_pid()))
      }else{
        stop(sprintf("Operating system: '%s' not supported.", private$metadata$serverOS), 
             call. = FALSE)
      }
    }
    return(0L)
  },
  interruptRemote = function(){
    if(!length(private$process)){
      return("Process not started")
    }
    
    ret <- DELETE(url = paste0(private$metadata$url, "/jobs/", private$process), 
                  authenticate(private$metadata$user, private$metadata$password),
                  add_headers(.headers = c("Timestamp" = as.character(Sys.time(), usetz = TRUE))),
                  timeout(2L))
    
    if(!identical(status_code(ret), 200L)){
      stop(sprintf("Problems interrupting remote process (status code: '%s'). Error message: '%s'",
                   status_code(ret), content(ret)$message), call. = FALSE)
    }
    return(0L)
  }
))
