MiroProc <- R6::R6Class("MiroProc", public = list(
  initialize = function(session){
    private$session <- session
    procEnv <- as.list(Sys.getenv())
    procEnv[["MIRO_DB_USERNAME"]] <- NULL
    procEnv[["MIRO_DB_PASSWORD"]] <- NULL
    procEnv$MIRO_POPULATE_DB <- "true"

    private$procEnv <- procEnv

    return(invisible(self))
  },
  getMigrationInfo = function(){
    return(private$migrationInfo)
  },
  setDbCredentials = function(username, password){
    private$procEnv[["MIRO_DB_USERNAME"]] <- username
    private$procEnv[["MIRO_DB_PASSWORD"]] <- password
    private$procEnv[["MIRO_DB_SCHEMA"]] <- username
    return(invisible(self))
  },
  run = function(appId, modelName, miroVersion, appDir, dataDir,
    progressSelector, successCallback, overwriteScen = TRUE, requestType = "addApp",
    migrationConfigPath = NULL, launchDbMigrationManager = NULL){
    private$errorRaised <- FALSE
    private$migrationInfo <- NULL
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
    procEnv$MIRO_MODEL_PATH <- file.path(appDir, modelName)
    procEnv$MIRO_DATA_DIR <- dataDir
    procEnv$MIRO_OVERWRITE_SCEN_IMPORT <- if(!identical(overwriteScen, TRUE)) "false" else "true"
    if(is.null(migrationConfigPath)){
      procEnv$MIRO_MIGRATION_CONFIG_PATH <- file.path(tempdir(check = TRUE), "mig_conf.json")
    }else{
      procEnv$MIRO_MIGRATION_CONFIG_PATH <- migrationConfigPath
      procEnv$MIRO_MIGRATE_DB <- "true"
    }
    flog.trace("Adding data for app: %s", appId)
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
                   flog.debug("MIRO signalled that database needs to be migrated. Waiting for user to migrate database.")
                   private$migrationInfo <- c(fromJSON(procEnv$MIRO_MIGRATION_CONFIG_PATH,
                                                        simplifyDataFrame = FALSE,
                                                        simplifyVector = FALSE),
                                              list(appId = appId, modelName = modelName,
                                                miroVersion = miroVersion, appDir = appDir,
                                                dataDir = dataDir))
                   private$session$sendCustomMessage("onHideAddAppProgress", list())
                   private$showMigrationModal(launchDbMigrationManager)
                   private$errorRaised <- TRUE
               }else if(error[1] == '418'){
                   flog.info("MIRO signalled that the scenario to import already exists.")
                   private$session$sendCustomMessage("onScenarioExists", error[2])
                   private$errorRaised <- TRUE
               }
            }else if(startsWith(line, 'mprog:::')){
                progress <- suppressWarnings(as.integer(substring(line, 9)))
                if(is.na(progress)){
                    flog.warn("Bad progress message received from MIRO: %s", line)
                }else{
                    private$session$sendCustomMessage("onProgress", 
                        list(selector = progressSelector,
                          progress = if(progress >= 100) -1 else progress))
                }
            }else if(startsWith(line, 'mmigprog:::')){
                progress <- suppressWarnings(as.integer(substring(line, 12)))
                if(is.na(progress)){
                    flog.warn("Bad migration progress message received from MIRO: %s", line)
                }else{
                    private$session$sendCustomMessage("onProgress", 
                        list(progress = if(progress >= 100) -1 else progress))
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
                    flog.error('Unexpected error when starting MIRO process. Stderr: %s',
                      private$stdErr)
                    private$session$sendCustomMessage("onError", list(requestType = requestType, message = "Internal error. Check log for more information."))
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
    migrationInfo = NULL,
    showMigrationModal = function(launchDbMigrationManager){
      showModal(modalDialog(
        title = "Database migration",
        tags$div(id = "migrationFormErrors", class = "gmsalert gmsalert-error",
                style = "white-space:pre-wrap;", lang$errMsg$unknownError),
        HTML(private$migrationInfo$uiContent),
        footer = actionButton("btCloseMigForm", lang$nav$migrationModule$btCancelMigration),
        size = "l"
      ), session = private$session)
      isolate({
        currentVal <- launchDbMigrationManager()
        launchDbMigrationManager(currentVal + 1L)
      })
    }
  )
)
