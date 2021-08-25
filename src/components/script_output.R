ScriptOutput <- R6Class("ScriptOutput", public = list(
  initialize = function(session, workDir, config, errorMsg, gamsSysDir){
    private$session <- session
    private$config <- config$base
    private$hcConfig <- config$hcube
    private$errorMsg <- errorMsg
    private$workDir <- workDir
    if(!identical(.Platform$OS.type, "windows") &&
       length(gamsSysDir) && nchar(gamsSysDir) > 0L){
      # on Windows setting environment for local processes does not seem to work.
      # We have to rely on GAMS being in the PATH here.
      private$scriptEnv <- Sys.getenv()
      private$scriptEnv[["PATH"]] <- paste0(gamsSysDir, .Platform$path.sep,
                                            private$scriptEnv[["PATH"]])
    }
  },
  isRunning = function(scriptId = NULL){
    if(is.null(scriptId)){
      return(length(private$activeScripts) > 0L)
    }
    return(scriptId %in% names(private$activeScripts))
  },
  getRunningScriptIds = function(){
    return(names(private$activeScripts))
  },
  hasResults = function(scriptId = NULL){
    if(is.null(scriptId)){
      return(length(private$scriptResults) > 0L)
    }
    return(scriptId %in% names(private$scriptResults))
  },
  getResults = function(scriptId = NULL){
    if(is.null(scriptId)){
      if(length(private$scriptResults))
        return(tibble(id = names(private$scriptResults),
                      content = unlist(private$scriptResults,
                                       use.names = FALSE)))
      return(tibble(id = character(0L),
                    content = character(0L)))

    }
    return(private$scriptResults[[scriptId]])
  },
  clearContent = function(scriptIds = NULL, scenId = NULL){
    if(is.null(scriptIds)){
      scriptIds <- seq_along(private$config)
    }
    if(is.null(scenId)){
      for(scriptId in scriptIds){
        self$interrupt(private$config[[scriptId]]$id, suppressWarning = TRUE)
        hideEl(private$session, paste0("#scriptOutput_", scriptId, " .script-spinner"))
        hideEl(private$session, paste0("#scriptOutput_", scriptId, " .script-output"))
        showEl(private$session, paste0("#scriptOutput_", scriptId, " .out-no-data"))
      }
      private$scriptResults <- list()
      return(invisible(self))
    }
    for(scriptId in scriptIds){
      hideEl(private$session, paste0("#scenScript_", scenId, "_", scriptId))
      showEl(private$session, paste0("#scenScript_", scenId, "_", scriptId, "_noData"))
    }
    return(invisible(self))
  },
  sendContent = function(data, id, scenId = NULL,
                         hcube = FALSE, isError = FALSE, inModal = FALSE){
    if(inModal){
      hideEl(private$session, "#batchLoadAnalysisSpinner")
      addClassEl(private$session, "#batchLoadModal .modal-content", "modal-content-fullscreen")
      addClassEl(private$session, "#batchLoadModal", "modal-dialog-fullscreen")
      if(!isError){
        showEl(private$session, "#btDownloadBatchLoadScript")
      }
    }
    private$session$sendCustomMessage("gms-scriptExecuted",
                                      list(id = id,
                                           sid = scenId,
                                           data = data,
                                           hcube = hcube,
                                           isError = isError))
    if(hcube && !inModal){
      hideEl(private$session, "#analysisLoad")
    }
  },
  loadResultsBase = function(data, scenId = NULL){
    if(length(data) < 2L){
      flog.debug("Script results not loaded since no data was provided.")
      return()
    }
    idsToLoad  <- data[[1]]
    dataToLoad <- data[[2]]
    idsLoaded <- lapply(seq_along(private$config), function(id){
      config <- private$config[[id]]
      rowNo <- match(config$id, idsToLoad)
      if(is.na(rowNo)){
        self$clearContent(id, scenId)
        return(NULL)
      }
      if(is.null(scenId)){
        private$scriptResults[[config$id]] <- dataToLoad[rowNo]
        hideEl(private$session, paste0("#scriptOutput_", id, " .out-no-data"))
      }else{
        hideEl(private$session, paste0("#scenScript_", scenId, "_", id, "_noData"))
      }
      flog.trace("Script output of script: '%s' loaded.", config$id)
      outputToLoad <- dataToLoad[rowNo]
      if(identical(config$markdown, TRUE)){
        outputToLoad <- markdown(outputToLoad)
      }
      self$sendContent(outputToLoad, id, scenId)
      return(config$id)
    })
    if(is.null(scenId)){
      idsLoaded <- unlist(idsLoaded, use.names = FALSE)
      orphanedScriptResults <- match(idsToLoad, idsLoaded)
      if(any(is.na(orphanedScriptResults))){
        flog.info(paste0("The scenario you loaded includes orphaned script results.",
        " The next time you save the scenario, these scripts will be removed. Those orphaned scripts are: '%s'."),
        paste(orphanedScriptResults[is.na(orphanedScriptResults)], collapse = "', '"))
      }
    }
  },
  interrupt = function(scriptId = NULL, suppressWarning = FALSE){
    if(is.null(scriptId)){
      if(!length(private$activeScripts)){
        return(invisible(self))
      }
      scriptId <- names(private$activeScripts)[[1L]]
    }
    if(scriptId %in% names(private$activeScripts)){
      if(private$killScript(scriptId)){
        flog.debug("Script: '%s' was interrupted.", scriptId)
      }
      private$clearProcess(scriptId)
      return(invisible(self))
    }
    if(!suppressWarning){
      flog.warn("Script: '%s' could not be interrupted as it was not found in list of active scripts.",
                scriptId)
    }
    return(invisible(self))
  },
  run = function(id, hcube = FALSE, inModal = FALSE){
    stopifnot(is.integer(id), id > 0)

    if(hcube){
      configLocal <- private$hcConfig[[id]]
    }else{
      configLocal <- private$config[[id]]
    }

    scriptId <- configLocal$id
    if(scriptId %in% names(private$activeScripts)){
      flog.warn("Script: '%s' is already running!", scripId)
      return(invisible(self))
    }

    private$activeScripts[[scriptId]] <- process$new(configLocal$command,
                                                     configLocal$args,
                                               stdout = "|", stderr = "2>&1",
                                               wd = private$workDir,
                                               env = private$scriptEnv)

    private$activeScriptsTo[[scriptId]] <- if(length(configLocal$timeout))
      configLocal$timeout else -1L
    private$activeScriptsObs[[scriptId]] <- observe({
      if(private$activeScriptsTo[[scriptId]] == 0L){
        flog.info("Script: '%s' timed out.", scriptId)
        private$killScript(scriptId)
        self$sendContent(private$errorMsg$timeout, id, hcube = hcube, isError = TRUE,
                         inModal = inModal)
        private$clearProcess(scriptId)
      }else if(length(private$pingProcess(scriptId))){
        if(identical(private$pingProcess(scriptId), 0L)){
          flog.info("Script: '%s' terminated successfully.", scriptId)
          self$sendContent(self$readOutput(id, scriptId, hcube), id, hcube = hcube,
                           inModal = inModal)
        }else{
          stdout <- tryCatch({
            private$activeScripts[[scriptId]]$read_output()
          }, error = function(e){
            character(1L)
          })
          flog.warn("Script output: '%s' terminated with exit code: '%s'. Stdout/err: '%s'.",
                    scriptId, private$pingProcess(scriptId), stdout)
          self$sendContent(private$errorMsg$crash, id, hcube = hcube, isError = TRUE,
                           inModal = inModal)
        }
        private$clearProcess(scriptId)
      }else{
        flog.trace("Script: '%s' still running.", scriptId)
        if(private$activeScriptsTo[[scriptId]] > 0L){
          private$activeScriptsTo[[scriptId]] <- private$activeScriptsTo[[scriptId]] - 1L
        }
        invalidateLater(1000)
      }
    })
    return(invisible(self))
  },
  finalize = function(){
    try(lapply(private$activeScripts, function(script){
      script$kill()}), silent = TRUE)
  },
  readOutput = function(id, scriptId, hcube = FALSE){
    stopifnot(is.integer(id), id > 0,
              is.character(scriptId), length(scriptId) == 1L)
    if(hcube){
      outputFile <- private$hcConfig[[id]]$outputFile
      parseMarkdown <- identical(private$hcConfig[[id]]$markdown, TRUE)
    }else{
      outputFile <- private$config[[id]]$outputFile
      parseMarkdown <- identical(private$config[[id]]$markdown, TRUE)
    }
    return(tryCatch({
      private$scriptResults[[scriptId]] <- read_file(file.path(private$workDir,
                                                               outputFile))
      if(parseMarkdown){
        return(markdown(private$scriptResults[[scriptId]]))
      }
      return(private$scriptResults[[scriptId]])
    }, error = function(e){
      flog.error("Problems reading output file of script: '%s'. Error message: '%s'.",
                 id, conditionMessage(e))
      return(private$errorMsg$noOutput)
    }))
  }
), private = list(
  session = NULL,
  config = NULL,
  hcConfig = NULL,
  scriptResults = list(),
  workDir = character(0L),
  scriptEnv = NULL,
  activeScripts = list(),
  activeScriptsObs = list(),
  activeScriptsTo = list(),
  errorMsg = character(1L),
  clearProcess = function(scriptId){
    private$activeScripts[[scriptId]] <- NULL
    private$activeScriptsTo[[scriptId]] <- NULL
    private$activeScriptsObs[[scriptId]]$destroy()
    private$activeScriptsObs[[scriptId]] <- NULL
  },
  pingProcess = function(scriptId){
    if(scriptId %in% names(private$activeScripts)){
      return(private$activeScripts[[scriptId]]$get_exit_status())
    }
    return(0L)
  },
  killScript = function(scriptId){
    if(scriptId %in% names(private$activeScripts)){
      return(tryCatch({
        private$activeScripts[[scriptId]]$kill()
        return(TRUE)},
        error= function(e){
          flog.error("Problems terminating script: '%s'. Error message: '%s'.",
                     conditionMessage(e))
          return(FALSE)
        }))
    }
    return(FALSE)
  }
))
