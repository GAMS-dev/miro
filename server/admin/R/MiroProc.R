MiroProc <- R6::R6Class("MiroProc", public = list(
  initialize = function(session) {
    private$session <- session
    procEnv <- as.list(Sys.getenv())
    procEnv[["MIRO_DB_USERNAME"]] <- NULL
    procEnv[["MIRO_DB_PASSWORD"]] <- NULL
    procEnv$MIRO_POPULATE_DB <- "true"

    private$procEnv <- procEnv

    return(invisible(self))
  },
  getMigrationInfo = function() {
    return(private$migrationInfo)
  },
  setDbCredentials = function(username, password) {
    private$procEnv[["MIRO_DB_USERNAME"]] <- username
    private$procEnv[["MIRO_DB_PASSWORD"]] <- password
    private$procEnv[["MIRO_DB_SCHEMA"]] <- username
    return(invisible(self))
  },
  run = function(appId, modelName, miroVersion, appDir, dataDir,
                 progressSelector, successCallback, overwriteScen = TRUE, requestType = "addApp",
                 migrationConfigPath = NULL, launchDbMigrationManager = NULL, additionalDataOnError = NULL,
                 parallelSessionId = NULL, newSession = FALSE) {
    private$errorRaised <- FALSE
    private$migrationInfo <- NULL
    private$stdErr <- ""
    if (length(parallelSessionId)) {
      stopifnot(is.character(parallelSessionId), identical(length(parallelSessionId), 1L))
      if (identical(newSession, TRUE) && length(private$parallelPidMap[[parallelSessionId]])) {
        # terminate remaining proceses of old session
        self$terminateParallelSession(parallelSessionId)
      }
      procId <- as.character(private$procCount)
      private$procCount <- private$procCount + 1L
      private$parallelPidMap[[parallelSessionId]] <- c(private$parallelPidMap[[parallelSessionId]], procId)
    } else {
      procId <- "main"
    }
    if (length(private$miroProc[[procId]]) &&
      private$miroProc[[procId]]$is_alive()) {
      flog.info("A process with pid: %s is still running. It will be killed.", procId)
      private$miroProc[[procId]]$kill()
    }
    if (length(private$procObs[[procId]])) {
      private$procObs[[procId]]$destroy()
    }
    procEnv <- private$procEnv
    procEnv$MIRO_VERSION_STRING <- miroVersion
    procEnv$MIRO_MODEL_PATH <- file.path(appDir, modelName)
    procEnv$MIRO_DATA_DIR <- dataDir
    if (identical(overwriteScen, NULL)) {
      procEnv$MIRO_OVERWRITE_SCEN_IMPORT <- "ask"
    } else if (identical(overwriteScen, TRUE)) {
      procEnv$MIRO_OVERWRITE_SCEN_IMPORT <- "true"
    } else {
      procEnv$MIRO_OVERWRITE_SCEN_IMPORT <- "false"
    }
    if (is.null(migrationConfigPath)) {
      procEnv$MIRO_MIGRATION_CONFIG_PATH <- file.path(tempdir(check = TRUE), "mig_conf.json")
    } else {
      procEnv$MIRO_MIGRATION_CONFIG_PATH <- migrationConfigPath
      procEnv$MIRO_MIGRATE_DB <- "true"
    }
    flog.trace("Adding data for app: %s", appId)
    private$miroProc[[procId]] <- processx::process$new("R", c(
      "--no-echo", "--no-restore", "--vanilla", "-e",
      paste0("shiny::runApp('", MIRO_APP_PATH, "',port=3839,host='0.0.0.0')")
    ),
    env = unlist(procEnv), wd = MIRO_APP_PATH, stderr = "|", stdout = "|"
    )

    private$procObs[[procId]] <- observe({
      procExitStatus <- private$miroProc[[procId]]$get_exit_status()
      procOutTmp <- private$miroProc[[procId]]$read_output()
      if (length(procOutTmp) && !identical(procOutTmp, "")) {
        flog.debug("%s: Stdout of add data process (pid: %s): %s", appId, procId, procOutTmp)
      }
      outputLines <- tryCatch(private$miroProc[[procId]]$read_error_lines(), error = function(e) {
        flog.warn(
          "Problems fetching stderr from MIRO process (pid: %s). Error message: %s",
          procId,
          conditionMessage(e)
        )
        return("")
      })
      if (length(procExitStatus)) {
        # somehow we have to call this twice in case process already finished..
        outputLines <- c(outputLines, private$miroProc[[procId]]$read_error_lines())
      }
      for (line in outputLines) {
        private$stdErr <- paste(private$stdErr, line, sep = "\n")
        if (startsWith(line, "merr:::")) {
          flog.debug(paste0("MIRO error message received: ", line))
          error <- strsplit(trimws(line), ":::", fixed = TRUE)[[1]][-1]
          if (error[1] == "409") {
            flog.debug("MIRO signalled that database needs to be migrated. Waiting for user to migrate database.")
            private$migrationInfo <- c(
              fromJSON(procEnv$MIRO_MIGRATION_CONFIG_PATH,
                simplifyDataFrame = FALSE,
                simplifyVector = FALSE
              ),
              list(
                appId = appId, modelName = modelName,
                miroVersion = miroVersion, appDir = appDir,
                dataDir = dataDir
              )
            )
            private$session$sendCustomMessage("onHideAddAppProgress", list())
            private$showMigrationModal(launchDbMigrationManager)
            private$errorRaised <- TRUE
          } else if (error[1] == "418") {
            flog.warn("MIRO signalled that the scenario to import already exists. This should not happen!")
          }
        } else if (startsWith(line, "mprog:::")) {
          progress <- suppressWarnings(as.integer(substring(line, 9)))
          if (is.na(progress)) {
            flog.warn("Bad progress message received from MIRO: %s", line)
          } else {
            if (length(progressSelector)) {
              private$session$sendCustomMessage(
                "onProgress",
                list(
                  selector = progressSelector,
                  progress = if (progress >= 100) -1 else progress
                )
              )
            }
          }
        } else if (startsWith(line, "mmigprog:::")) {
          progress <- suppressWarnings(as.integer(substring(line, 12)))
          if (is.na(progress)) {
            flog.warn("Bad migration progress message received from MIRO: %s", line)
          } else {
            private$session$sendCustomMessage(
              "onProgress",
              list(progress = if (progress >= 100) -1 else progress)
            )
          }
        }
      }
      # flog.debug(private$miroProc[[procId]]$read_output())
      if (length(procExitStatus)) {
        if (length(private$procObs[[procId]])) {
          private$procObs[[procId]]$destroy()
          private$procObs[[procId]] <- NULL
        }
        if (procExitStatus == 0) {
          flog.debug("MIRO process (pid: %s) finished successfully. Stderr: %s", procId, private$stdErr)
          private$miroProc[[procId]] <- NULL
          successCallback()
        } else {
          if (private$errorRaised) {
            flog.debug(
              "MIRO process (pid: %s) finished with exit code: %s.",
              procId,
              as.character(procExitStatus)
            )
          } else {
            flog.warn(
              "MIRO process (pid: %s) finished with exit code: %s.",
              procId,
              as.character(procExitStatus)
            )
            flog.error(
              "Unexpected error when starting MIRO process (pid: %s). Stderr: %s",
              procId,
              private$stdErr
            )
            if (length(additionalDataOnError)) {
              dataToSend <- additionalDataOnError
              dataToSend[["requestType"]] <- requestType
              dataToSend[["message"]] <- "Internal error. Check log for more information."
            } else {
              dataToSend <- list(requestType = requestType, message = "Internal error. Check log for more information.")
            }
            private$session$sendCustomMessage("onError", dataToSend)
          }
          private$miroProc[[procId]] <- NULL
        }
      } else {
        invalidateLater(500, private$session)
      }
    })
    return(invisible(self))
  },
  terminateParallelSession = function(parallelSessionId) {
    if (!length(private$parallelPidMap[[parallelSessionId]])) {
      return(invisible(self))
    }
    lapply(private$parallelPidMap[[parallelSessionId]], function(pid) {
      if (length(private$miroProc[[pid]]) && private$miroProc[[pid]]$is_alive()) {
        flog.info("Miro process (pid: %s) forcefully terminated.", pid)
        private$miroProc[[pid]]$kill()
      }
      if (length(private$procObs[[pid]])) {
        private$procObs[[pid]]$destroy()
      }
    })
    private$parallelPidMap[[parallelSessionId]] <- NULL
    return(invisible(self))
  },
  finalize = function() {
    flog.debug("MiroProc: destructor called.")
    if (length(private$miroProc)) {
      lapply(names(private$miroProc), function(pid) {
        if (private$miroProc[[pid]]$is_alive()) {
          flog.info("Miro process (pid: %s) forcefully terminated.", pid)
          private$miroProc[[pid]]$kill()
        }
      })
    }
    if (length(private$procObs)) {
      lapply(private$procObs, function(obs) {
        obs$destroy()
      })
    }
  }
), private = list(
  session = NULL,
  procEnv = NULL,
  miroProc = list(),
  procObs = list(),
  procCount = 1L,
  parallelPidMap = list(),
  stdErr = "",
  errorRaised = FALSE,
  migrationInfo = NULL,
  showMigrationModal = function(launchDbMigrationManager) {
    showModal(modalDialog(
      title = "Database migration",
      tags$div(
        id = "migrationFormErrors", class = "gmsalert gmsalert-error",
        style = "white-space:pre-wrap;", lang$errMsg$unknownError
      ),
      HTML(private$migrationInfo$uiContent),
      footer = actionButton("btCloseMigForm", lang$nav$migrationModule$btCancelMigration),
      size = "l"
    ), session = private$session)
    isolate({
      currentVal <- launchDbMigrationManager()
      launchDbMigrationManager(currentVal + 1L)
    })
  }
))
