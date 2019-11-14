ScriptOutput <- R6Class("ScriptOutput", public = list(
  initialize = function(session, workDir, config, errorMsg){
    private$session <- session
    private$config <- config$base
    private$hcConfig <- config$hcube
    private$errorMsg <- errorMsg
    private$workDir <- workDir
  },
  isRunning = function(id){
    scriptId <- paste0("script_", id)
    return(scriptId %in% names(private$activeScripts))
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
  sendContent = function(data, id, scenId = NULL){
    private$session$sendCustomMessage("gms-scriptExecuted", 
                                      list(id = id,
                                           sid = scenId,
                                           data = data))
  },
  loadResultsBase = function(data, scenId = NULL){
    idsToLoad  <- data[[1]]
    dataToLoad <- data[[2]]
    idsLoaded <- lapply(seq_along(private$config), function(id){
      config <- private$config[[id]]
      rowNo <- match(config$id, idsToLoad)
      if(is.na(rowNo))
        return(NULL)
      if(is.null(scenId)){
        private$scriptResults[[config$id]] <- dataToLoad[rowNo]
        hideEl(private$session, paste0("#scriptOutput_", id, " .out-no-data"))
      }else{
        hideEl(private$session, paste0("#scenScript_", scenId, "_", id, "_noData"))
      }
      flog.trace("Script output of script: '%s' loaded.", config$id)
      self$sendContent(dataToLoad[rowNo], id, scenId)
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
  interrupt = function(scriptId){
    if(scriptId %in% names(private$activeScripts)){
      private$clearProcess(scriptId)
      private$clearProcess(scriptId)
      if(private$killScript(scriptId)){
        flog.debug("Script: '%s' was interrupted.", scriptId)
      }
      return()
    }
    flog.warn("Script: '%s' could not be interrupted as it was not found in list of active scripts.",
              scriptId)
  },
  run = function(id, hcube = FALSE){
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
                                               wd = private$workDir)
    
    private$activeScriptsTo[[scriptId]] <- if(length(configLocal$timeout)) 
      configLocal$timeout else -1L
    private$activeScriptsObs[[scriptId]] <- observe({
      if(private$activeScriptsTo[[scriptId]] == 0L){
        flog.info("Script: '%s' timed out.", scriptId)
        private$killScript(scriptId)
        self$sendContent(private$errorMsg$timeout, id)
        private$clearProcess(scriptId)
      }else if(length(private$pingProcess(scriptId))){
        if(identical(private$pingProcess(scriptId), 0L)){
          flog.info("Script: '%s' terminated successfully.", scriptId)
          self$sendContent(self$readOutput(id, scriptId, hcube), id)
        }else{
          stdout <- tryCatch({
            private$activeScripts[[scriptId]]$read_output()
          }, error = function(e){
            character(1L)
          })
          flog.warn("Script output: '%s' terminated with exit code: '%s'. Stdout/err: '%s'.", 
                    scriptId, private$pingProcess(scriptId), stdout)
          self$sendContent(private$errorMsg$crash, id)
        }
        private$clearProcess(scriptId)
      }else{
        flog.debug("Script: '%s' still running.", scriptId)
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
    }else{
      outputFile <- private$config[[id]]$outputFile
    }
    return(tryCatch({
      private$scriptResults[[scriptId]] <- read_file(file.path(private$workDir, 
                                                               "scripts", outputFile))
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