Worker <- R6Class("Worker", public = list(
  initialize = function(metadata, workDir, method = c("local", "remote")){
    stopifnot(is.list(metadata), is.character(workDir), identical(length(workDir), 1L))
    private$method <- match.arg(method)
    private$workDir <- workDir
    private$metadata <- metadata
  },
  run = function(inputData, pfFileContent = NULL){
    if(length(pfFileContent)){
      stopifnot(is.character(pfFileContent), length(pfFileContent) > 0L)
      private$pfFileContent <- pfFileContent
    }else{
      private$pfFileContent <- NULL
    }
    private$log <- character(1L)
    private$status <- NULL
    
    if(identical(private$method, "local")){
      return(private$runLocal(inputData))
    }
    return(private$runRemote(inputData))
  },
  retrieveRemoteLog = function(){
    if(!length(private$process)){
      stop("Process not started", call. = FALSE)
    }
    if(length(private$status)){
      return(private$updateLog)
    }
    if(inherits(private$process, "process")){
      stop("Process is local", call. = FALSE)
    }
    ret <- DELETE(paste0(private$metadata$url, "/unread_logs/", private$process), timeout(2L))
    statusCode <- status_code(ret)
    if(identical(statusCode, 200L)){
      responseContent <- content(ret)
      if(identical(responseContent$queue_finished, TRUE)){
        private$status <- responseContent$gams_return_code
      }
      private$log <- responseContent$log
      if(!identical(responseContent$log, "")){
        private$updateLog <- private$updateLog + 1L
      }
      return(private$updateLog)
    }else if(identical(statusCode, 308L)){
      # job finished, get full log
      retContent <- content(ret)
      private$status <- retContent$gams_return_code
      #retFull <- GET(paste0(private$metadata$url, "/logs/", private$process))
      #if(identical(status_code(retFull), 200L)){
      #  retContent <- content(retFull)
      #  private$log <- responseContent$log
      #  return(nchar(private$log))
      #}else{
      #  stop(content(retFull)$message)
      #}
    }else if(identical(statusCode, 403L)){
      return(private$updateLog)
    }else{
      stop(content(ret)$message, call. = FALSE)
    }
  },
  getReactiveLog = function(session){
    if(!length(private$process)){
      stop("Process not started", call. = FALSE)
    }
    if(identical(private$method, "local")){
      return(reactiveFileReader2(300, session, file.path(private$workDir, 
                                                         paste0(private$metadata$modelName, ".log"))))
    }
    return(reactivePoll2(500, session, checkFunc = function(){
      self$retrieveRemoteLog()
    }, valueFunc = function(){
      private$log
    }))
  },
  getReactiveStatus = function(session){
    return(reactivePoll2(1000, session, checkFunc = function(){
      private$getStatus()
    }, valueFunc = function(){
      private$getStatus()
    }))
  },
  getMethod = function(){
    return(private$method)
  }
), private = list(
  method = character(1L),
  status = NULL, 
  metadata = NULL,
  inputData = NULL,
  log = character(1L),
  pfFileContent = NULL,
  process = NULL,
  workDir = NULL,
  updateLog = 1L,
  runLocal = function(inputData){
    
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
    
    private$process <- process$new(paste0(private$metadata$gamsSysDir, "gams"), args = c(private$metadata$modelGmsName, 
                                                                   "pf", pfFilePath), 
                                   stdout = paste0(private$workDir, private$metadata$modelName, ".log"), windows_hide_window = TRUE)
    return(self)
  },
  runRemote = function(inputData){
    zipFilePath <- paste0(private$workDir, "data.zip")
    gamsArgs <- c(if(length(private$metadata$extraClArgs)) private$metadata$extraClArgs, 
                  paste0("execMode=", private$metadata$gamsExecMode), 
                  private$metadata$MIROSwitch)
    pfFilePath <- gmsFilePath(paste0(private$workDir, tolower(private$metadata$modelName), ".pf"))
    writeLines(c(private$pfFileContent, gamsArgs), pfFilePath)
    
    inputData$writeCSV(private$workDir, delim = private$metadata$csvDelim)$addFilePaths(pfFilePath)$compress(zipFilePath)
    
    ret <- POST(paste0(private$metadata$url, "/jobs"), encode = "multipart", 
                body = list(model = private$metadata$modelName, username = private$metadata$user,
                            use_pf_file = TRUE, 
                            data = upload_file(zipFilePath, 
                                               type = 'application/zip')),
                timeout(2L))
    
    if(identical(status_code(ret), 201L)){
      private$process <- content(ret)$token
    }else{
      stop(content(ret)$message, call. = FALSE)
    }

    return(self)
  },
  getStatus = function(){
    if(!length(private$process)){
      return(NULL)
    }
    if(inherits(private$process, "process")){
      return(private$process$get_exit_status())
    }
    return(private$status)
  }
))