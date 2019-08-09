Worker <- R6Class("Worker", public = list(
  initialize = function(metadata, remote){
    stopifnot(is.list(metadata), is.logical(remote), 
              identical(length(remote), 1L))
    private$remote   <- remote
    private$metadata <- metadata
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
    private$metadata$username  <- username
    private$metadata$useRegistered <- useRegistered
    private$metadata$password  <- password
    private$buildAuthHeader(FALSE)
    
    if(rememberMeFlag){
      ret <- POST(url = paste0(url, "/auth/"), 
                  add_headers(Authorization = private$authHeader,
                              Timestamp = as.character(Sys.time(), usetz = TRUE)), 
                  timeout(2L))
      if(identical(status_code(ret), 200L)){
        sessionToken <- content(ret)$token
      }else if(status_code(ret) %in% c(401L, 403L)){
        stop(403L, call. = FALSE)
      }else{
        stop(sprintf("Problems retrieving token: Return code: %d, message: '%s'",
                     status_code(ret), content(ret)$message), call. = FALSE)
      }
      private$metadata$password  <- sessionToken
      write_json(list(url = url,
                      username = username, 
                      password = sessionToken,
                      namespace = namespace,
                      reg = useRegistered), 
                 private$metadata$rememberMeFileName, auto_unbox = TRUE)
    }
    private$metadata$namespace <- namespace
    ret <- GET(url = paste0(url, "/namespaces/", namespace, "/permissions/me"), 
                add_headers(Authorization = private$authHeader,
                            Timestamp = as.character(Sys.time(), usetz = TRUE)), 
                timeout(2L))
    if(identical(status_code(ret), 404L)){
      tryCatch(content(ret, type = 'application/json'),
               error = function(e){
                 stop(404L, call. = FALSE)
               })
      stop(400L, call. = FALSE)
    }
    if(identical(status_code(ret), 200L)){
      permissionLevel <- content(ret)$permission
      if(identical(useRegistered, TRUE)){
        if(permissionLevel < 5L)
          stop(403L, call. = FALSE)
        return(200L)
      }
      if(permissionLevel < 7L)
        stop(403L, call. = FALSE)
      return(200L)
    }else if(status_code(ret) %in% c(401L, 403L)){
      stop(401L, call. = FALSE)
    }else{
      tryCatch(content(ret, type = 'application/json'),
               error = function(e){
                 stop(404L, call. = FALSE)
               })
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
    if(private$remote &&
       !identical(private$metadata$noNeedCred, TRUE) &&
       any(vapply(
         c(private$metadata$url,
           private$metadata$password, 
           private$metadata$namespace), 
         private$isEmptyString, logical(1L), USE.NAMES = FALSE))){
      return(FALSE)
    }
    return(TRUE)
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
    private$fRemoteSub <- NULL
    private$wait    <- 0L
    private$waitCnt <- 0L
    
    if(private$remote){
      private$runRemote(inputData)
      return(0L)
    }
    private$runLocal(inputData)
    return(0L)
  },
  interrupt = function(){
    if(private$remote){
      return(private$interruptRemote())
    }
    return(private$interruptLocal())
  },
  pingProcess = function(){
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
  }
), private = list(
  remote = logical(1L),
  status = NULL, 
  metadata = NULL,
  inputData = NULL,
  log = character(1L),
  authHeader = character(1L),
  pfFileContent = NULL,
  process = NULL,
  workDir = NULL,
  updateLog = 1L,
  gamsRet = NULL,
  waitCnt = integer(1L),
  wait = integer(1L),
  fRemoteSub = NULL,
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
                          use_pf_file = TRUE, text_entities = paste0(metadata$modelName, ".lst"),
                          stdout_filename = "log",
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
        return(paste0("error:", status_code(ret), content(ret)$message))
      }
    }, globals = list(metadata = private$metadata, workDir = private$workDir,
                      pfFileContent = private$pfFileContent, inputData = inputData,
                      tableNameTracePrefix = tableNameTracePrefix, authHeader = private$authHeader,
                      gmsFilePath = gmsFilePath, DataInstance = DataInstance, 
                      isWindows = isWindows))
    return(self)
  },
  retrieveRemoteLog = function(){
    ret <- GET(paste0(private$metadata$url, "/jobs/", private$process, "/text-entity/log"),
               add_headers(Authorization = private$authHeader,
                           Timestamp = as.character(Sys.time(), usetz = TRUE)),
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
          flog.error(value(private$fRemoteSub))
          private$status <- -500L
        }else{
          private$process <- value(private$fRemoteSub)
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
        private$status <- private$gamsRet
        if(!identical(value(private$fRemoteRes), 0L)){
          flog.error(value(private$fRemoteRes))
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
      return(private$status)
    }else if(identical(statusCode, 308L)){
      # job finished, get full log
      retContent <- content(ret)
      private$status <- retContent$gams_return_code
      return(private$status)
    }else if(identical(statusCode, 403L)){
      private$wait <- bitwShiftL(2L, private$waitCnt)
      if(private$waitCnt < private$metadata$timeout){
        private$waitCnt <- private$waitCnt + 1L
      }else{
        private$status <- -404L
      }
      return(private$status)
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
               write_disk(tmp), 
               add_headers(Authorization = private$authHeader,
                           Timestamp = as.character(Sys.time(), usetz = TRUE)),
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
                  add_headers(Authorization = private$authHeader,
                              Timestamp = as.character(Sys.time(), usetz = TRUE)),
                  timeout(2L))
    
    if(!identical(status_code(ret), 200L)){
      stop(sprintf("Problems interrupting remote process (status code: '%s'). Error message: '%s'",
                   status_code(ret), content(ret)$message), call. = FALSE)
    }
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
                                 base64_encode(charToRaw(paste0(private$metadata$username, 
                                                                ":", private$metadata$password))))
    return(invisible(self))
  }
))
