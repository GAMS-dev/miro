MiroProc <- R6::R6Class("MiroProc", public = list(
  initialize = function(session){
    private$session <- session
    procEnv <- as.list(Sys.getenv())
    procEnv$MIRO_POPULATE_DB <- "true"

    private$procEnv <- procEnv

    if(!identical(Sys.getenv("SHINYPROXY_NOAUTH"), "true")){
      # if not NOAUTH mode, admin user = SHINYPROXY_USERNAME
      private$procEnv[["MIRO_ADMIN_USER"]] <- NULL
    }

    return(invisible(self))
  },
  getTablesToRemove = function(){
    return(private$tablesToRemove)
  },
  run = function(appId, modelName, miroVersion, appDir, dataDir, successCallback){
    private$tablesToRemove <- NULL
    private$errorRaised <- FALSE
    private$stdErr <- ""
    if(length(private$miroProc) &&
        private$miroProc$is_alive()){
        private$miroProc$kill()
    }
    if(length(private$procObs)){
        private$procObs$destroy()
    }
    procEnv <- private$procEnv
    procEnv$MIRO_VERSION_STRING <- miroVersion
    procEnv$MIRO_MODEL_PATH <- file.path(appDir, paste0(modelName, ".gms"))
    procEnv$MIRO_DATA_DIR <- dataDir

    private$miroProc <- processx::process$new("R", c("-e", 
        paste0("shiny::runApp('", MIRO_APP_PATH, "',port=3839,host='0.0.0.0')")),
        env = unlist(procEnv), wd = MIRO_APP_PATH, stderr = "|", stdout = "|")

    private$procObs <- observe({
        procExitStatus  <- private$miroProc$get_exit_status()
        outputLines <- tryCatch(private$miroProc$read_error_lines(), error = function(e){
            flog.warn("Problems fetching stderr from MIRO process. Error message: %s",
                conditionMessage(e))
            return("")
        })
        if(length(procExitStatus)){
            # somehow we have to call this twice in case process already finished..
            outputLines <- c(outputLines, private$miroProc$read_error_lines())
        }
        for(line in outputLines){
            private$stdErr <- paste(private$stdErr, line, sep = "\n")
            if(startsWith(line, 'merr:::')){
                flog.debug(paste0("MIRO error message received: ", line))
                error <- strsplit(trimws(line), ":::", fixed = TRUE)[[1]][-1]
                if(error[1] == '409'){
                   if(length(error) < 2){
                    flog.error('MIRO signalled that there are inconsistent tables but no data was provided.');
                    private$session$sendCustomMessage("onError", list(requestType = "addApp", message = "Internal error"))
                    private$procObs$destroy()
                    private$procObs <- NULL
                    return()
                   }
                   # split and decode base64 encoded table names
                   datasetsToRemove <- vapply(strsplit(error[2], ",", fixed = TRUE)[[1]], function(dataset){
                     return(rawToChar(jsonlite::base64_dec(dataset)))
                   }, character(1), USE.NAMES = FALSE)
                   flog.debug(paste0("Datasets to be removed are: ", paste(datasetsToRemove, collapse = "','")))

                   escapedAppId <- escapeAppIds(appId)[1]
                   private$tablesToRemove <- vapply(datasetsToRemove, function(el){
                    return(paste0(escapedAppId, "_", el))
                    }, character(1), USE.NAMES = FALSE)
                   flog.debug(paste0("Inconsistent tables to be removed are: ",
                    paste(private$tablesToRemove, collapse = "','")))

                   private$session$sendCustomMessage("onInconsistentDbTables", 
                    list(datasetsToRemove = I(datasetsToRemove)))
                   private$errorRaised <- TRUE
                   private$procObs$destroy()
                   private$procObs <- NULL
               }
            }else if(startsWith(line, 'mprog:::')){
                progress <- suppressWarnings(as.integer(substring(line, 9)))
                if(is.na(progress)){
                    flog.warn("Bad progress message received from MIRO: %s", line)
                }else{
                    private$session$sendCustomMessage("onAddAppProgress", 
                        if(progress >= 100) -1 else progress)
                }
            }
        }
        #flog.debug(private$miroProc$read_output())
        if(length(procExitStatus)){
            if(length(private$procObs)){
                private$procObs$destroy()
                private$procObs <- NULL
            }
            if(procExitStatus == 0){
                flog.debug('MIRO process finished successfully. Stderr: %s', private$stdErr)
                private$miroProc <- NULL
                successCallback()
            }else{
                if(private$errorRaised){
                    flog.debug("MIRO process finished with exit code: %s.",
                      as.character(procExitStatus))
                }else{
                    flog.warn("MIRO process finished with exit code: %s.",
                      as.character(procExitStatus))
                    flog.error('Unexpected error when starting MIRO process. Stderr: %s', private$stdErr)
                    private$session$sendCustomMessage("onError", list(requestType = "addApp", message = "Internal error."))
                }
                private$miroProc <- NULL
            }
        }else{
            invalidateLater(500, private$session)
        }
    })
    return(invisible(self))
  },
  finalize = function(){
    flog.debug("MiroProc: destructor called.")
    if(length(private$miroProc) &&
        private$miroProc$is_alive()){
        private$miroProc$kill()
    }
    if(length(private$procObs)){
        private$procObs$destroy()
    }
  }), private = list(
    session = NULL,
    procEnv = NULL,
    miroProc = NULL,
    procObs = NULL,
    stdErr = "",
    errorRaised = FALSE,
    tablesToRemove = NULL
  )
)
