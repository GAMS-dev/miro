# run GAMS
storeGAMSOutputFiles <- function(workDir) {
  if (config$activateModules$attachments &&
    !is.null(activeScen) &&
    (config$storeLogFilesDuration > 0L || length(config$outputAttachments))) {
    errMsg <- NULL
    tryCatch(
      {
        filesToStore <- character(0L)
        if (config$storeLogFilesDuration > 0L) {
          if (any(c(config$activateModules$logFile, config$activateModules$lstFile))) {
            filesToStore <- file.path(
              workDir,
              paste0(
                modelNameRaw,
                c(
                  if (config$activateModules$logFile) ".log",
                  if (config$activateModules$lstFile) ".lst"
                )
              )
            )
          }
          if (config$activateModules$miroLogFile) {
            filesToStore <- c(filesToStore, file.path(workDir, config$miroLogFile))
          }
        }

        enforceFileAccess <- rep.int(TRUE, length(filesToStore))
        fileAccessPerm <- rep.int(FALSE, length(filesToStore))

        fnOutputAttachments <- character()

        if (length(config$outputAttachments)) {
          fnOutputAttachments <- file.path(workDir, vapply(config$outputAttachments, "[[", character(1L),
            "filename",
            USE.NAMES = FALSE
          ))
          if (any(fnOutputAttachments %in% filesToStore)) {
            # overwrite settings of attachments
            # if they are also declared as output attachments
            filesNotInOA <- !filesToStore %in% fnOutputAttachments
            filesToStore <- filesToStore[filesNotInOA]
            enforceFileAccess <- enforceFileAccess[filesNotInOA]
            fileAccessPerm <- fileAccessPerm[filesNotInOA]
          }
          filesToStore <- c(filesToStore, fnOutputAttachments)
          enforceFileAccess <- c(enforceFileAccess, vapply(config$outputAttachments, function(el) {
            return(!isFALSE(el[["throwError"]]))
          }, logical(1L), USE.NAMES = FALSE))
          fileAccessPerm <- c(fileAccessPerm, vapply(config$outputAttachments, function(el) {
            return(isTRUE(el[["execPerm"]]))
          }, logical(1L), USE.NAMES = FALSE))
        }

        if (!length(filesToStore)) {
          return()
        }

        filesNoAccess <- file.access(filesToStore) == -1L
        if (any(filesNoAccess[enforceFileAccess])) {
          if (all(filesNoAccess[enforceFileAccess])) {
            stop("fileAccessException", call. = FALSE)
          }
          flog.info(
            "GAMS output files: '%s' could not be accessed. Check file permissions.",
            paste(filesToStore[filesNoAccess & enforceFileAccess], collapse = "', '")
          )
          errMsg <- sprintf(
            lang$errMsg$saveAttachments$noFileAccess,
            paste(basename(filesToStore[filesNoAccess & enforceFileAccess]), collapse = "', '")
          )
        }
        if (any(filesNoAccess)) {
          flog.debug(
            "File(s): '%s' were not added as attachment as they could not be accessed.",
            paste(filesToStore[filesNoAccess], collapse = "', '")
          )
        }
        filesToStore <- filesToStore[!filesNoAccess]
        fileAccessPerm <- fileAccessPerm[!filesNoAccess]
        filesTooLarge <- file.size(filesToStore) > attachMaxFileSize
        if (any(filesTooLarge)) {
          if (all(filesTooLarge)) {
            stop("fileSizeException", call. = FALSE)
          }
          flog.info(
            "GAMS output files: '%s' are too large. They will not be saved.",
            paste(filesToStore[filesTooLarge], collapse = "', '")
          )
          errMsg <- paste(errMsg, sprintf(
            lang$errMsg$saveAttachments$fileSizeExceeded,
            paste(basename(filesToStore[filesTooLarge]), collapse = "', '")
          ), sep = "\n")
        }
        attachments$add(
          session = NULL, filesToStore[!filesTooLarge], overwrite = TRUE,
          execPerm = fileAccessPerm[!filesTooLarge]
        )
        outputAttachmentsAdded <- filesToStore[!filesTooLarge] %in% fnOutputAttachments
        if (any(outputAttachmentsAdded)) {
          showNotification(sprintf(
            lang$nav$notificationOutputAttachmentAdded$desc,
            paste(basename(filesToStore[!filesTooLarge][outputAttachmentsAdded]), collapse = ", ")
          ))
        }
      },
      error_max_no = function(e) {
        flog.info("Maximum number of attachments (%s) has been exceeded.", attachMaxNo)
        errMsg <<- sprintf(
          lang$errMsg$saveAttachments$maxNoExceeded,
          attachMaxNo
        )
      },
      error = function(e) {
        switch(conditionMessage(e),
          fileAccessException = {
            flog.info("No GAMS output files could not be accessed. Check file permissions.")
            errMsg <<- sprintf(
              lang$errMsg$saveAttachments$noFileAccess,
              paste(basename(filesToStore), collapse = "', '")
            )
          },
          fileSizeException = {
            flog.info("All GAMS output files are too large to be saved.")
            errMsg <<- sprintf(
              lang$errMsg$saveAttachments$fileSizeExceeded,
              paste(basename(filesToStore), collapse = "', '")
            )
          },
          {
            flog.error(
              "Problems while trying to store GAMS output files in the database. Error message: '%s'.",
              conditionMessage(e)
            )
            errMsg <<- lang$errMsg$unknownError
          }
        )
      }
    )
    showErrorMsg(lang$errMsg$saveAttachments$title, errMsg)
  }
}
prepareModelRun <- function(async = FALSE) {
  prog <- Progress$new()
  on.exit(suppressWarnings(prog$close()))
  prog$set(message = lang$progressBar$prepRun$title, value = 0)

  prog$inc(amount = 0.5, detail = lang$progressBar$prepRun$sendInput)
  # save input data
  errMsgTmp <- NULL
  if (tryCatch(
    {
      dataTmp <- getInputDataFromSandbox()
      FALSE
    },
    no_data = function(e) {
      flog.error(conditionMessage(e))
      showErrorMsg(lang$errMsg$GAMSInput$title, conditionMessage(e))
      return(TRUE)
    },
    error = function(e) {
      flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
      showErrorMsg(lang$errMsg$GAMSInput$title, lang$errMsg$unknownError)
      return(TRUE)
    }
  )) {
    return()
  }
  scenData$loadSandbox(
    dataTmp, if (length(modelInFileNames)) modelInFileNames else character(),
    activeScen$getMetadataDf()
  )
  inputData <- InputDataInstance$new(
    fileExchange = config$fileExchange,
    gdxio = gdxio, csvDelim = config$csvDelim,
    activeScen = activeScen, attachments = attachments,
    views = views
  )
  lapply(seq_along(dataTmp), function(id) {
    # write compile time variable file and remove compile time variables from scalar dataset
    if (is.null(dataTmp[[id]])) {
      return()
    }
    if (identical(tolower(names(dataTmp)[[id]]), scalarsFileName)) {
      # scalars file exists, so remove compile time variables from it
      DDParIdx <- dataTmp[[id]][[1]] %in% DDPar
      GMSOptIdx <- dataTmp[[id]][[1]] %in% GMSOpt
      if (any(c(DDParIdx, GMSOptIdx))) {
        isClArg <- (DDParIdx | GMSOptIdx)
        inputData$pushClArgs(dataTmp[[id]][isClArg, ])
        # remove those rows from scalars file that are compile time variables
        inputData$push(names(dataTmp)[[id]], dataTmp[[id]][!isClArg, ])
      } else {
        inputData$push(names(dataTmp)[[id]], dataTmp[[id]])
      }
    } else if (identical(modelIn[[names(dataTmp)[[id]]]]$type, "dropdown") &&
      names(dataTmp)[[id]] %in% modelInTabularDataBase) {
      inputData$push(
        names(dataTmp)[[id]],
        ddToTibble(dataTmp[[id]][[1L]], modelIn[[names(dataTmp)[[id]]]])
      )
    } else {
      inputData$push(names(dataTmp)[[id]], dataTmp[[id]])
    }
  })
  if (is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))) {
    return(NULL)
  }
  tryCatch(
    {
      if (config$activateModules$attachments && attachAllowExec && !is.null(activeScen)) {
        prog$inc(amount = 0, detail = lang$progressBar$prepRun$downloadAttach)
        inputData$addFilePaths(attachments$download(workDir, allExecPerm = TRUE))
      }
      prog$close()
    },
    error = function(e) {
      errMsgTmp <<- lang$errMsg$gamsExec$desc
      flog.error("Attachment data could not be downloaded. Error message: %s.", conditionMessage(e))
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsgTmp))) {
    return(NULL)
  }
  return(inputData)
}
loadOutputData <- function() {
  storeGAMSOutputFiles(workDir)

  scenData$loadSandbox(loadScenData(
    metaData = modelOut, workDir = workDir,
    fileName = MIROGdxOutName,
    templates = modelOutTemplate,
    method = config$fileExchange,
    csvDelim = config$csvDelim
  )$tabular, names(modelOut))
  if (config$saveTraceFile) {
    tryCatch(
      {
        traceData <<- readTraceData(file.path(workDir, "_scenTrc.trc"))
      },
      error = function(e) {
        flog.info("Problems loading trace data. Error message: %s.", conditionMessage(e))
      }
    )
  }
  if (scenData$getSandboxHasOutputData(scriptOutput)) {
    noOutputData <<- FALSE
  } else {
    noOutputData <<- TRUE
  }
  tryCatch(
    worker$updateJobStatus(JOBSTATUSMAP["imported"]),
    error = function(e) {
      flog.warn(
        "Failed to update job status. Error message: '%s'.",
        conditionMessage(e)
      )
    }
  )
}
observeEvent(input$btLoadInconsistentOutput, {
  removeModal()

  errMsg <- NULL
  tryCatch(
    {
      loadOutputData()
    },
    error = function(e) {
      flog.error("Problems loading output data. Error message: %s.", conditionMessage(e))
      errMsg <<- lang$errMsg$readOutput$desc
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))) {
    return(htmltools::htmlEscape(statusText))
  }

  # select first tab in current run tabset
  switchTab(session, "output")
  updateTabsetPanel(session, "scenTabset",
    selected = "results.current"
  )
  renderOutputData()
  markUnsaved(markDirty = TRUE)
})
observeEvent(input$btSubmitJob, {
  flog.debug("Submit new asynchronous job button clicked.")
  jobNameTmp <- character(1L)
  if (!verifyCanSolve(async = TRUE, buttonId = "btSubmitJob")) {
    return()
  }
  if (length(activeScen)) {
    jobNameTmp <- activeScen$getScenName()
  }
  inputData <- prepareModelRun(async = TRUE)
  if (is.null(inputData)) {
    return(NULL)
  }
  worker$setInputData(inputData)
  tryCatch(
    {
      scenHashTmp <- inputData$generateScenHash()
      scenWithSameHash <- db$getScenWithSameHash(scenHashTmp)
      activeScen$setScenHash(scenHashTmp)
    },
    error = function(e) {
      flog.error(
        "Scenario hash could not be looked up. Error message: %s.",
        conditionMessage(e)
      )
      errMsg <<- lang$errMsg$unknownError
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))) {
    return(NULL)
  }
  instanceInfo <- worker$getInstanceInfo()
  showJobSubmissionDialog(jobNameTmp, scenWithSameHash, instanceInfo = instanceInfo)
  if (!length(instanceInfo)) {
    worker$fetchInstancesAsync(session, "selWorkerInstance", lang$nav$dialogJobSubmission$instanceDropdownCategories)
  }
})
observeEvent(virtualActionButton(input$btSubmitAsyncJob, rv$btSubmitAsyncJob), {
  flog.debug("Confirm new asynchronous job button clicked.")
  sid <- NULL
  jobName <- input$jobSubmissionName
  if (isBadScenName(jobName)) {
    showHideEl(session, "#jobSubmitBadName", 4000L)
    return()
  }
  if (length(activeScen) && length(activeScen$getSid())) {
    if (!activeScen$hasExecPerm()) {
      flog.error("User has no execute permission for this scenario. This looks like an attempt to tamper with the app!")
      return()
    }
    sid <- activeScen$getSid()
  }
  if (identical(worker$validateCredentials(), FALSE)) {
    flog.error("User has no valid credentials. This looks like an attempt to tamper with the app!")
    return(NULL)
  }
  solveOptions <- NULL
  instanceInfo <- worker$getInstanceInfo()
  if (length(instanceInfo) && identical(instanceInfo[["instancesSupported"]], TRUE)) {
    solveOptions <- list(selectedInstance = input$selWorkerInstance)
  }
  # submit job
  tryCatch(
    {
      disableEl(session, "#btSubmitAsyncJob")
      hideEl(session, "#jobSubmissionWrapper")
      showEl(session, "#jobSubmissionLoad")
      worker$runAsync(sid, name = jobName, solveOptions = solveOptions)
      showHideEl(session, "#jobSubmitSuccess", 2000)
      showQuotaWarnings(session, worker$getQuotaWarning())
      hideModal(session, 2L)
    },
    error = function(e) {
      errMsg <- conditionMessage(e)
      hideEl(session, "#btSubmitAsyncJob")
      hideModal(session, 6L)

      if (identical(errMsg, "404") || startsWith(errMsg, "Could not") ||
        startsWith(errMsg, "Timeout")) {
        flog.warn("Some problem occurred while executing job. Error message: '%s'.", errMsg)
        return(showEl(session, "#jobSubmitUnknownHost"))
      }

      if (identical(errMsg, "402")) {
        flog.info("Some problem occurred while executing job. Error message: '%s'.", errMsg)
        showQuotaWarnings(session, worker$getQuotaWarning())
        return(showElReplaceTxt(session, "#jobSubmitUnknownError", lang$errMsg$quotaWarning$quotaExceeded))
      }

      if (errMsg %in% c(401L, 403L)) {
        flog.info("Some problem occurred while executing job. Error message: '%s'.", errMsg)
        return(showEl(session, "#jobSubmitUnauthorized"))
      }
      flog.error("Some problem occurred while executing job. Error message: '%s'.", errMsg)

      showEl(session, "#jobSubmitUnknownError")
    },
    finally = {
      hideEl(session, "#jobSubmissionLoad")
    }
  )
})

if (config$activateModules$lstFile) {
  output$btDownloadLogFilesLst <- downloadHandler(paste0(modelNameRaw, ".lst"),
    content = function(file) {
      logPathTmp <- file.path(workDir, paste0(modelNameRaw, ".lst"))
      if (!file.exists(logPathTmp)[1]) {
        showNotification(lang$nav$notificationFileNotFound$lst, type = "error")
        return(write_file("", file))
      }
      tryCatch(file.copy(logPathTmp, file),
        error = function(e) {
          flog.warn("Lst file could not be copied. Error message: '%s'.", conditionMessage(e))
        }
      )
    }, contentType = "text/plain"
  )
  output$listFileContainer <- renderText({
    req(rv$refreshLogs)
    errMsg <- NULL
    tryCatch(
      {
        fileSize <- file.size(file.path(workDir, modelNameRaw %+% ".lst"))
        if (is.na(fileSize)) {
          stop("Could not access listing file", call. = FALSE)
        }
        if (fileSize > maxSizeToRead) {
          lang$errMsg$readLst$fileSize
        } else if (file.exists(file.path(workDir, modelNameRaw %+% ".lst"))) {
          read_file(file.path(workDir, modelNameRaw %+% ".lst"))
        } else {
          lang$errMsg$readLst$fileNotFound
        }
      },
      error = function(e) {
        flog.warn(
          "GAMS listing file could not be read (model: '%s'). Error message: %s.",
          modelName, conditionMessage(e)
        )
        showErrorMsg(lang$errMsg$readLst$title, errMsg)
        return("")
      }
    )
  })
}
if (config$activateModules$miroLogFile) {
  output$btDownloadLogFilesMiroLog <- downloadHandler(paste0(modelName, "-mirolog.log"),
    content = function(file) {
      logPathTmp <- file.path(workDir, config$miroLogFile)
      if (!file.exists(logPathTmp)[1]) {
        showNotification(lang$nav$notificationFileNotFound$mirolog, type = "error")
        return(write_file("", file))
      }
      tryCatch(file.copy(logPathTmp, file),
        error = function(e) {
          flog.warn("MIRO log file could not be copied. Error message: '%s'.", conditionMessage(e))
        }
      )
    }, contentType = "text/plain"
  )
  renderMiroLogContent <- function() {
    miroLogAnnotations <<- NULL
    miroLogContent <- ""
    miroLogPath <- file.path(workDir, config$miroLogFile)
    tryCatch(
      {
        if (file.exists(miroLogPath)[1]) {
          inputScalarsTmp <- NULL
          if (scalarsFileName %in% names(modelIn)) {
            inputScalarsTmp <- modelIn[[scalarsFileName]]$symnames
          }
          miroLogContent <- parseMiroLog(
            session, miroLogPath,
            names(modelIn), inputScalarsTmp
          )
          miroLogAnnotations <<- miroLogContent$annotations
          miroLogContent <- miroLogContent$content
        }
      },
      error = function(e) {
        flog.warn("MIRO log file could not be read. Error message: '%s'.", conditionMessage(e))
      }
    )
    return(HTML(paste(miroLogContent, collapse = "\n")))
  }
  if (config$activateModules$logFile) {
    output$miroLogContainer <- renderUI({
      req(rv$refreshLogs)
      return(renderMiroLogContent())
    })
  }
}

logFilePath <- NULL
if (config$activateModules$logFile ||
  config$activateModules$miroLogFile) {
  logfileObs <- NULL
  logfile <- NULL
  if (config$activateModules$logFile) {
    output$btDownloadLogFilesLog <- downloadHandler(paste0(modelNameRaw, ".log"),
      content = function(file) {
        logPathTmp <- file.path(workDir, paste0(modelNameRaw, ".log"))
        if (!file.exists(logPathTmp)[1]) {
          showNotification(lang$nav$notificationFileNotFound$log, type = "error")
          return(write_file("", file))
        }
        tryCatch(file.copy(logPathTmp, file),
          error = function(e) {
            flog.warn("Log file could not be copied. Error message: '%s'.", conditionMessage(e))
          }
        )
      }, contentType = "text/plain"
    )
    logFilePath <- file.path(workDir, modelNameRaw %+% ".log")
  } else {
    logFilePath <- file.path(workDir, config$miroLogFile)
  }

  logObs <- observe({
    req(rv$triggerAsyncProcObserver)

    logText <- NULL
    try(logText <- logfile(), silent = TRUE)

    if (!length(logText)) {
      return()
    }

    write_file(logText, logFilePath, append = TRUE)
    appendEl(session, "#logStatusContainer", logText,
      scroll = identical(isolate(input$logUpdate), TRUE)
    )
  })
}

output$modelStatus <- renderUI({
  req(rv$triggerAsyncProcObserver)

  currModelStat <- modelStatus()
  if (is.null(currModelStat)) {
    return(lang$nav$gamsModelStatus$exec)
  } else if (identical(currModelStat, "s")) {
    return(lang$nav$gamsModelStatus$submission)
  } else if (identical(currModelStat, "q")) {
    return(lang$nav$gamsModelStatus$queued)
  } else if (is.character(currModelStat) && startsWith(currModelStat, "q")) {
    return(sprintf(lang$nav$gamsModelStatus$queuedWithPosition, substring(currModelStat, 2L)))
  } else if (identical(currModelStat, "d")) {
    return(lang$nav$gamsModelStatus$collection)
  }
  modelStatusObs$destroy()
  modelStatus <<- NULL
  enableEl(session, "#btSolve")
  disableEl(session, "#btInterrupt")
  disableEl(session, "#btDetachCurrentJob")

  if (config$activateModules$logFile ||
    config$activateModules$miroLogFile) {
    logfileObs$destroy()
    logfileObs <- NULL
    logfile <- NULL
    if (config$activateModules$miroLogFile) {
      if (config$activateModules$logFile) {
        containerId <- "#miroLogContainer"
      } else {
        containerId <- "#logStatusContainer"
      }
      setContent(session, containerId, renderMiroLogContent())
    }
  }

  showQuotaWarnings(session, worker$getQuotaWarning())

  if (currModelStat < 0) {
    returnCodeText <- GAMSRCMAP[as.character(currModelStat)]
    if (is.na(returnCodeText)) {
      returnCodeText <- as.character(currModelStat)
    }
    statusText <- lang$nav$gamsModelStatus$error %+% returnCodeText
    flog.debug(
      "GAMS model was not solved successfully (model: '%s'). Model status: %s.",
      modelName, statusText
    )
    return(htmltools::htmlEscape(statusText))
  }
  isolate({
    if (is.null(rv$refreshLogs)) {
      rv$refreshLogs <- 1L
    } else {
      rv$refreshLogs <- rv$refreshLogs + 1L
    }
  })

  if (currModelStat != 0) {
    returnCodeText <- GAMSRCMAP[as.character(currModelStat)]
    if (is.na(returnCodeText)) {
      returnCodeText <- as.character(currModelStat)
    }
    statusText <- lang$nav$gamsModelStatus$error %+% returnCodeText
    if (config$activateModules$miroLogFile && length(miroLogAnnotations)) {
      session$sendCustomMessage("gms-showValidationErrors", miroLogAnnotations)
      valIdHead <- match(names(miroLogAnnotations)[[1L]], names(modelIn))
      if (length(valIdHead) && !is.na(valIdHead)) {
        valTabId <- 0L
        inputTabId <- tabSheetMap$input[[valIdHead]]
        updateTabsetPanel(session, "inputTabset", paste0("inputTabset_", inputTabId[1]))
        if (length(inputTabId) > 1L) {
          updateTabsetPanel(
            session, paste0("inputTabset", inputTabId[1]),
            paste0(
              "inputTabset", inputTabId[1], "_",
              inputTabId[2]
            )
          )
        }
      }
    }
    flog.debug(
      "GAMS model was not solved successfully (model: '%s'). Model status: %s.",
      modelName, statusText
    )
    tryCatch(
      worker$updateJobStatus(JOBSTATUSMAP["imported"]),
      error = function(e) {
        flog.warn(
          "Failed to update job status. Error message: '%s'.",
          conditionMessage(e)
        )
      }
    )
  } else {
    # run terminated successfully
    statusText <- lang$nav$gamsModelStatus$success

    if (inconsistentOutput) {
      showInconsistentOutputDialog()
      return(statusText)
    }

    errMsg <- NULL
    tryCatch(
      {
        loadOutputData()
      },
      error = function(e) {
        flog.error("Problems loading output data. Error message: %s.", conditionMessage(e))
        errMsg <<- lang$errMsg$readOutput$desc
      }
    )
    if (is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))) {
      return(htmltools::htmlEscape(statusText))
    }
    # select first tab in current run tabset
    switchTab(session, "output")
    updateTabsetPanel(session, "scenTabset",
      selected = "results.current"
    )
    isolate(renderOutputData())

    markUnsaved(markClean = TRUE)
  }
  return(statusText)
})
# refresh even when modelStatus message is hidden (i.e. user is on another tab)
outputOptions(output, "modelStatus", suspendWhenHidden = FALSE)

verifyCanSolve <- function(async = FALSE, buttonId = "btSolve", detachCurrentRun = FALSE) {
  if (!async) {
    if (detachCurrentRun) {
      removeModal()
    }
    if (length(modelStatus)) {
      if (detachCurrentRun) {
        modelStatusObs$destroy()
        modelStatus <<- NULL
        if (config$activateModules$logFile ||
          config$activateModules$miroLogFile) {
          logfileObs$destroy()
          logfileObs <- NULL
          logfile <- NULL
        }
      } else if (config$activateModules$remoteExecution) {
        if (is.null(worker$getJobId())) {
          flog.debug("Job still submitting. Cant detach..")
          showErrorMsg(
            lang$errMsg$jobRunning$title,
            lang$errMsg$jobRunning$descAsyncSubmitting
          )
          return(FALSE)
        }
        showJobRunningDialog()
        return(FALSE)
      } else {
        showErrorMsg(
          lang$errMsg$jobRunning$title,
          lang$errMsg$jobRunning$desc
        )
        return(FALSE)
      }
    }
  }
  if (length(unzipModelFilesProcess)) {
    if (length(unzipModelFilesProcess$get_exit_status())) {
      unzipModelFilesProcess <<- NULL
    } else {
      showErrorMsg(
        lang$errMsg$unzipProcessRunning$title,
        lang$errMsg$unzipProcessRunning$desc
      )
      return(FALSE)
    }
  }
  if (length(activeScen) && !activeScen$hasExecPerm()) {
    showErrorMsg(
      lang$nav$dialogNoExecPerm$title,
      lang$nav$dialogNoExecPerm$desc
    )
    flog.info("User has no execute permission for this scenario.")
    return(FALSE)
  }
  if (identical(worker$validateCredentials(), FALSE)) {
    showLoginDialog(
      cred = worker$getCredentials(),
      forwardOnSuccess = buttonId
    )
    return(FALSE)
  }
  return(TRUE)
}

runGAMSJob <- function() {
  clearLogs(session)
  # run GAMS
  errMsg <- NULL
  inconsistentOutput <<- FALSE
  tryCatch(
    {
      jobSid <- NULL
      if (length(activeScen) && length(activeScen$getSid())) {
        jobSid <- activeScen$getSid()
      }
      worker$run(jobSid, name = activeScen$getScenName())
    },
    error_duplicate_records = function(e) {
      flog.info("Problems writing GDX file. Duplicate records found: %s", conditionMessage(e))
      errMsg <<- conditionMessage(e)
    },
    error = function(e) {
      flog.error(
        "GAMS did not execute successfully. Error message: %s.",
        conditionMessage(e)
      )
      errMsg <<- lang$errMsg$gamsExec$desc
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))) {
    return(NULL)
  }
  if (config$activateModules$remoteExecution) {
    updateTabsetPanel(session, "jobListPanel", selected = "current")
  }
  if (config$activateModules$logFile) {
    updateTabsetPanel(session, "logFileTabsset", selected = "log")
  } else if (config$activateModules$miroLogFile) {
    updateTabsetPanel(session, "logFileTabsset", selected = "mirolog")
  }

  # activate Interrupt button as GAMS is running now
  updateActionButton(session, "btInterrupt", icon = character(0L))
  enableEl(session, "#btInterrupt")
  enableEl(session, "#btDetachCurrentJob")
  switchTab(session, "gamsinter")

  if (!is.null(logFilePath) &&
    !identical(unlink(logFilePath, force = TRUE), 0L)) {
    flog.warn("Could not remove log file: '%s'.", logFilePath)
  }

  errMsg <- NULL
  tryCatch(
    {
      modelStatusRE <- worker$getReactiveStatus(session)
      modelStatusObs <<- modelStatusRE$obs
      modelStatus <<- modelStatusRE$re
    },
    error = function(e) {
      flog.error(
        "GAMS status could not be retrieved. Error message: %s.",
        conditionMessage(e)
      )
      errMsg <<- lang$errMsg$readLog$desc
    }
  )

  if (is.null(showErrorMsg(lang$errMsg$readLog$title, errMsg))) {
    return()
  }

  tryCatch(
    {
      logRE <- worker$getReactiveLog(session)
      logfileObs <<- logRE$obs
      logfile <<- logRE$re
    },
    error = function(e) {
      flog.error(
        "GAMS log file could not be read (model: '%s'). Error message: %s.",
        modelName, conditionMessage(e)
      )
      errMsg <<- lang$errMsg$readLog$desc
    }
  )
  showErrorMsg(lang$errMsg$readLog$title, errMsg)

  if (is.null(rv$triggerAsyncProcObserver)) {
    rv$triggerAsyncProcObserver <- 1L
  } else {
    rv$triggerAsyncProcObserver <- rv$triggerAsyncProcObserver + 1L
  }
}

runNewSynchronousJob <- function(detachCurrentRun = FALSE) {
  if (!verifyCanSolve(detachCurrentRun = detachCurrentRun)) {
    return()
  }
  inputData <- prepareModelRun(async = FALSE)
  if (is.null(inputData)) {
    return(NULL)
  }
  worker$setInputData(inputData)
  tryCatch(
    {
      scenHashTmp <- inputData$generateScenHash()
      scenWithSameHash <- db$getScenWithSameHash(scenHashTmp)
      activeScen$setScenHash(scenHashTmp)
    },
    error = function(e) {
      flog.error(
        "Scenario hash could not be looked up. Error message: %s.",
        conditionMessage(e)
      )
      errMsg <<- lang$errMsg$unknownError
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))) {
    return(NULL)
  }
  if (length(scenWithSameHash) && nrow(scenWithSameHash) > 0L) {
    showHashExistsDialog(scenWithSameHash)
    return(NULL)
  }
  runGAMSJob()
}

observeEvent(virtualActionButton(input$btSolve, rv$btSolve), {
  flog.debug("Solve button clicked (model: '%s').", modelName)
  runNewSynchronousJob()
})
observeEvent(input$btRunNoCheckHash, {
  flog.debug("Solve button (no check scen hash) clicked (model: '%s').", modelName)
  if (!verifyCanSolve()) {
    return()
  }
  removeModal()
  runGAMSJob()
})
observeEvent(input$btInterrupt, {
  flog.debug("Button to interrupt model run clicked.")

  if (tryCatch(
    {
      worker$interrupt()
      FALSE
    },
    error = function(e) {
      flog.error("Problems interrupting the GAMS process. Error message: %s.", conditionMessage(e))
      showErrorMsg(lang$errMsg$gamsTerm$title, lang$errMsg$gamsTerm$desc)
      return(TRUE)
    }
  )) {
    return()
  }
  updateActionButton(session, "btInterrupt", icon = icon("skull"))
})
observeEvent(input$btSolveDetachCurrent, {
  flog.debug("Button to detach from current job to solve new job clicked.")
  runNewSynchronousJob(detachCurrentRun = TRUE)
})
observeEvent(input$btDetachCurrentJob, {
  flog.debug("Button to detach from current job clicked .")
  if (is.null(worker$getJobId())) {
    flog.debug("Job still submitting. Cant detach..")
    showNotification(lang$errMsg$jobRunning$descAsyncSubmittingDetach, type = "error", duration = 10L)
    return()
  }
  modelStatusObs$destroy()
  modelStatus <<- NULL
  if (config$activateModules$logFile ||
    config$activateModules$miroLogFile) {
    logfileObs$destroy()
    logfileObs <- NULL
    logfile <- NULL
  }
  clearLogs(session)
  enableEl(session, "#btSolve")
  disableEl(session, "#btInterrupt")
  disableEl(session, "#btDetachCurrentJob")
  showNotification(lang$nav$gams$boxGamsOutput$gamsOutputTabset$detachedInfoMsg)
})
if (!isShinyProxy && config$activateModules$remoteExecution) {
  observeEvent(input$btRemoteExecLogin, {
    flog.debug("Button to open login dialog clicked.")
    showLoginDialog(cred = worker$getCredentials())
  })
  observeEvent(input$btSaveCredentials, {
    flog.debug("Login button clicked.")
    tryCatch(
      {
        worker$login(
          url = input$remoteCredUrl,
          username = input$remoteCredUser,
          password = input$remoteCredPass,
          namespace = input$remoteCredNs,
          useRegistered = input$remoteCredReg,
          rememberMe = input$remoteCredRemember
        )
        connectionInfo <- worker$getCredentials()
        hideEl(session, "#btRemoteExecLogin")
        setAttributes(
          session, "#remoteExecLogoutDiv", "title",
          paste0(
            connectionInfo$user, "@", connectionInfo$url, " (",
            connectionInfo$ns, ")"
          )
        )
        showEl(session, "#remoteExecLogoutDiv")
        removeModal()
        if (input$btSaveCredentials %in% names(rv)) {
          rv[[input$btSaveCredentials]] <- rv[[input$btSaveCredentials]] + 1L
        }
      },
      error = function(e) {
        errMsg <- conditionMessage(e)
        flog.info("Problems logging in. Return code: %s", errMsg)
        if (identical(errMsg, "404") || startsWith(errMsg, "Could not") ||
          startsWith(errMsg, "Timeout")) {
          return(showHideEl(session, "#remoteLoginHostNotFound", 6000))
        }

        if (identical(errMsg, "444")) {
          return(showHideEl(session, "#remoteLoginNsNotFound", 6000))
        }

        if (identical(errMsg, "445")) {
          return(showHideEl(session, "#remoteLoginModelNotFound", 6000))
        }

        if (identical(errMsg, "400")) {
          return(showHideEl(session, "#remoteLoginNsNotFound", 6000))
        }

        if (identical(errMsg, "401")) {
          return(showHideEl(session, "#remoteLoginInvalidCred", 6000))
        }

        if (identical(errMsg, "403")) {
          return(showHideEl(session, "#remoteLoginInsuffPerm", 6000))
        }

        if (identical(errMsg, "426")) {
          return(showHideEl(session, "#remoteLoginInvalidProt", 6000))
        }

        return(showErrorMsg(lang$errMsg$fileWrite$title, lang$errMsg$unknownError))
      }
    )
  })
  observeEvent(input$btRemoteExecLogout, {
    removeModal()
    tryCatch(
      {
        worker$logout()
        showEl(session, "#btRemoteExecLogin")
        hideEl(session, "#remoteExecLogoutDiv")
      },
      error = function(e) {
        flog.error("Problems setting credentials: %s", conditionMessage(e))
        showErrorMsg(lang$errMsg$fileWrite$title, sprintf(
          lang$errMsg$fileWrite$desc,
          rememberMeFileName
        ))
      }
    )
  })
}
if (identical(config$activateModules$hcube, TRUE)) {
  hcubeBuilder <- NULL
  generateHcInterface <- function() {
    tagList(
      tags$div(
        class = "container-fluid hc-container",
        tags$div(
          class = "row",
          lapply(seq_along(config$hcModule$scalarsConfig), function(widgetId) {
            scalarConfig <- config$hcModule$scalarsConfig[[widgetId]]
            symId <- match(scalarConfig$name, names(modelIn))
            dependencyId <- match(scalarConfig$name, names(modelInWithDep))
            widgetlabel <- scalarConfig$label
            if (!is.null(scalarConfig$tooltip)) {
              widgetlabel <- widgetTooltip(widgetlabel, scalarConfig$tooltip, mobile = TRUE)
            }
            tags$div(class = "col-md-6 hc-widget-row", {
              if (identical(scalarConfig$type, "dropdown")) {
                if (identical(scalarConfig$baseType, "checkbox")) {
                  dropdownVal <- suppressWarnings(as.integer(isolate(input[[paste0("cb_", symId)]])))
                } else {
                  dropdownVal <- isolate(input[[paste0("dropdown_", symId)]])
                }
                if (!is.na(dependencyId)) {
                  scalarConfig$choices <- isolate(getData[[dependencyId]]())
                }
                selectInput(paste0("hcWidget_", widgetId),
                  label = widgetlabel,
                  choices = scalarConfig$choices,
                  selected = dropdownVal,
                  multiple = TRUE,
                  width = "100%"
                )
              } else if (identical(scalarConfig$type, "slider")) {
                sliderVal <- isolate(input[[paste0("slider_", symId)]])
                if (length(sliderVal)) {
                  sliderVal <- c(sliderVal, sliderVal)
                } else {
                  sliderVal <- c(scalarConfig$default, scalarConfig$default)
                }
                if (!is.na(dependencyId)) {
                  configTmp <- isolate(getData[[dependencyId]]())
                  scalarConfig$min <- configTmp$min
                  scalarConfig$max <- configTmp$max
                  scalarConfig$step <- configTmp$step
                }
                tags$div(
                  class = "row", style = "overflow:hidden;",
                  tags$div(
                    class = if (scalarConfig$single) "col-sm-10" else "col-sm-8",
                    sliderInput(paste0("hcWidget_", widgetId),
                      label = widgetlabel,
                      min = scalarConfig$min,
                      max = scalarConfig$max,
                      value = sliderVal,
                      step = scalarConfig$step,
                      ticks = scalarConfig$ticks,
                      width = "100%"
                    )
                  ),
                  if (scalarConfig$single) {
                    tags$div(
                      class = "col-sm-2",
                      numericInput(paste0("hcWidget_", widgetId, "_step"),
                        lang$nav$hcubeMode$stepsize,
                        scalarConfig$step,
                        min = scalarConfig$minStep,
                        width = "100%"
                      )
                    )
                  } else {
                    tagList(
                      tags$div(
                        class = "col-sm-2",
                        checkboxInput_MIRO(paste0("hcWidget_", widgetId, "_combinations"),
                          lang$nav$hcubeMode$sliderAllCombinations,
                          value = FALSE
                        )
                      ),
                      tags$div(
                        class = "col-sm-2",
                        conditionalPanel(
                          paste0("input.hcWidget_", widgetId, "_combinations===true"),
                          numericInput(paste0("hcWidget_", widgetId, "_step"),
                            lang$nav$hcubeMode$stepsize,
                            scalarConfig$step,
                            min = scalarConfig$minStep,
                            width = "100%"
                          )
                        )
                      )
                    )
                  }
                )
              } else {
                stop("HC widget type  not implemented (should never happen)", call. = FALSE)
              }
            })
          })
        )
      )
    )
  }
  noHcubeScen <- throttle(reactive({
    req(input$btSubmitHcJob)
    noScenTmp <- vapply(seq_along(config$hcModule$scalarsConfig), function(widgetId) {
      widgetVal <- input[[paste0("hcWidget_", widgetId)]]
      if (is.null(widgetVal)) {
        return(NA_integer_)
      }
      scalarConfig <- config$hcModule$scalarsConfig[[widgetId]]
      if (identical(scalarConfig$type, "slider")) {
        if (!scalarConfig$single) {
          if (!identical(input[[paste0("hcWidget_", widgetId, "_combinations")]], TRUE)) {
            hcubeBuilder$pushRange(
              paste0(scalarConfig$name, "_lo"),
              paste0(scalarConfig$name, "_up"), widgetVal
            )
            return(1L)
          }
        }

        stepSize <- input[[paste0("hcWidget_", widgetId, "_step")]]
        if (is.null(stepSize)) {
          return(NA_integer_)
        }
        if (!is.numeric(stepSize) || stepSize <= 0) {
          # non valid step size selected
          return(-1L)
        }
        hcRange <- floor((widgetVal[2] - widgetVal[1]) / stepSize) + 1

        if (length(scalarConfig$minStep)) {
          if (stepSize < scalarConfig$minStep) {
            return(-1L)
          }
        } else if (stepSize < scalarConfig$step) {
          return(-1L)
        }
        if (scalarConfig$single) {
          hcubeBuilder$push(scalarConfig$name, seq(widgetVal[1], widgetVal[2], stepSize))
          return(as.integer(hcRange))
        }
        # double slider all combinations
        hcubeBuilder$pushRange(paste0(scalarConfig$name, "_lo"),
          paste0(scalarConfig$name, "_up"),
          getCombinationsSlider(widgetVal[1], widgetVal[2], stepSize),
          allCombinations = TRUE
        )
        return(as.integer(hcRange * (hcRange + 1) / 2))
      } else {
        hcubeBuilder$push(scalarConfig$name, widgetVal, ddChoices = scalarConfig$choices)
        return(length(widgetVal))
      }
    }, integer(1L), USE.NAMES = FALSE)
    if (any(is.na(noScenTmp))) {
      return()
    }
    if (any(noScenTmp == -1L)) {
      return(-1L)
    }
    return(prod(noScenTmp))
  }), 1000L)
  noHcubeScenSolved <- throttle(reactive({
    req(rv$refreshHcubeHashes > 0L)
    if (!length(noHcubeScen()) || noHcubeScen() < 1L || noHcubeScen() > MAX_NO_HCUBE) {
      return(0L)
    }
    tryCatch(
      {
        scenHashes <- hcubeBuilder$generateScenHashes()
        return(db$getScenWithSameHash(scenHashes,
          limit = NULL, count = TRUE
        )[[1]][1])
      },
      error = function(e) {
        flog.error(
          "Scenario hashes could not be looked up. Error message: %s.",
          conditionMessage(e)
        )
        return(-1L)
      }
    )
  }), 1200L)
  output$newHcJobInfo <- renderText({
    noScenTmp <- noHcubeScen()
    disableEl(session, "#btSubmitHcJobConfirm")
    disableEl(session, "#btSubmitHcJobConfirmUnsolved")
    hideEl(session, "#btSubmitHcJobConfirmUnsolved")
    if (is.null(noScenTmp)) {
      return()
    }
    if (noScenTmp == -1) {
      showElReplaceTxt(
        session, "#newHcJobError",
        lang$nav$dialogHcube$badStepSizeDialog$desc
      )
    } else if (noScenTmp == 0) {
      showElReplaceTxt(
        session, "#newHcJobError",
        lang$nav$dialogHcube$noScenSelectedDialog$desc
      )
    } else if (noScenTmp > MAX_NO_HCUBE) {
      showElReplaceTxt(
        session, "#newHcJobError",
        sprintf(
          lang$nav$dialogHcube$exceedMaxNoDialog$desc,
          format(noScenTmp, big.mark = ","), format(MAX_NO_HCUBE, big.mark = ",")
        )
      )
    } else {
      enableEl(session, "#btSubmitHcJobConfirm")
      hideEl(session, "#newHcJobError")
    }
    if (noHcubeScenSolved() > 0L) {
      showEl(session, "#btSubmitHcJobConfirmUnsolved")
      if (noHcubeScenSolved() < noScenTmp) {
        enableEl(session, "#btSubmitHcJobConfirmUnsolved")
      }
    }
    sprintf(lang$nav$dialogHcube$desc, noScenTmp, noHcubeScenSolved())
  })
  observeEvent(input$btSubmitHcJob, {
    flog.trace("Submit new Hypercube job button clicked.")
    jobNameTmp <- character(1L)
    if (!verifyCanSolve(async = TRUE, buttonId = "btSubmitHcJob")) {
      return()
    }
    if (length(activeScen)) {
      jobNameTmp <- activeScen$getScenName()
    }
    inputData <- prepareModelRun(async = TRUE)
    if (is.null(inputData)) {
      return(NULL)
    }
    worker$setInputData(inputData)
    if (is.null(hcubeBuilder)) {
      hcubeBuilder <<- HcubeBuilder$new(inputData$getDataHashes(), inputData$getScalarData())
    } else {
      hcubeBuilder$setDataHashes(inputData$getDataHashes(), inputData$getScalarData())
    }
    rv$refreshHcubeHashes <- rv$refreshHcubeHashes + 1L

    instanceInfo <- worker$getInstanceInfo()

    showModal(modalDialog(
      class = "hc-modal",
      title = lang$nav$hcModule$submissionDialog$title,
      tags$div(
        id = "newHcJobError", class = "gmsalert gmsalert-error",
        style = "position: relative;max-width: unset;"
      ),
      tags$div(id = "newHcJobInfo", class = "shiny-text-output lead"),
      tags$div(
        class = "row hc-tags-row",
        tags$div(
          class = "col-md-6",
          selectizeInput("newHcubeTags", lang$nav$dialogHcube$newTags, c(),
            multiple = TRUE, width = "100%", options = list(
              "create" = TRUE,
              "persist" = FALSE
            )
          )
        ),
        tags$div(
          class = "col-md-6",
          if (!length(instanceInfo)) {
            tags$div(
              id = "selHcWorkerInstanceSpinner",
              style = "text-align:center;",
              tags$div(class = "space"),
              genSpinner(hidden = FALSE, absolute = FALSE, extraClasses = "gen-spinner-black")
            )
          },
          tags$div(
            id = "selHcWorkerInstanceWrapper",
            style = if (!length(instanceInfo) || !identical(instanceInfo[["instancesSupported"]], TRUE)) "display:none;",
            selectInput("selHcWorkerInstance",
              lang$nav$dialogJobSubmission$workerInstance,
              choices = instanceInfo$choices, selected = instanceInfo$selected,
              width = "100%"
            )
          )
        )
      ),
      tags$div(
        id = "newHcWrapper",
        genSpinner(absolute = FALSE)
      ),
      tags$div(
        class = "row hc-submit-row", style = "text-align:center;",
        actionButton("btSubmitHcJobConfirm", lang$nav$hcModule$submissionDialog$btSubmitAll,
          class = "bt-highlight-1 bt-gms-confirm bt-no-disable"
        ),
        actionButton("btSubmitHcJobConfirmUnsolved", lang$nav$hcModule$submissionDialog$btSubmitUnsolved,
          class = "bt-highlight-1 bt-gms-confirm bt-no-disable",
          style = "display:none;"
        )
      ),
      easyClose = FALSE,
      size = "l",
      footer = tagList(
        modalButton(lang$nav$hcModule$submissionDialog$btCancel)
      )
    ))

    if (!length(instanceInfo)) {
      worker$fetchInstancesAsync(session, "selHcWorkerInstance", lang$nav$dialogJobSubmission$instanceDropdownCategories)
    }

    emptyEl(session, "#newHcWrapper")
    tryCatch(
      {
        insertUI("#newHcWrapper", ui = generateHcInterface())
      },
      error = function(e) {
        flog.error(
          "Unexpected error while generating Hypercube job interface. Error message: '%s'",
          conditionMessage(e)
        )
        showElReplaceTxt(session, "#newHcJobError", lang$errMsg$unknownError)
      }
    )
  })
  observeEvent(input$btSubmitHcJobConfirmUnsolved, {
    flog.trace("Button to confirm submission of new Hypercube job (unsolved scenarios only) clicked.")
    if (!verifyCanSolve(async = TRUE, buttonId = "btSubmitHcJob")) {
      return()
    }
    if (!length(noHcubeScen()) || noHcubeScen() < 1L || noHcubeScen() > MAX_NO_HCUBE) {
      return()
    }
    if (tryCatch(
      {
        existingHashes <- db$getScenWithSameHash(hcubeBuilder$getScenHashes(),
          limit = NULL, count = FALSE, distinctHashes = TRUE
        )[[1]]
        FALSE
      },
      error = function(e) {
        flog.error(
          "Problems fetching existing hashes from database. Error message: %s",
          conditionMessage(e)
        )
        return(TRUE)
      }
    )) {
      return()
    }
    hcubeBuilder$removeScen(existingHashes)
    rv$submitHCJobConfirm <- rv$submitHCJobConfirm + 1L
  })
  observeEvent(input$btSubmitHcJobConfirm, {
    flog.trace("Button to confirm submission of new Hypercube job (all scenarios) clicked.")
    rv$submitHCJobConfirm <- rv$submitHCJobConfirm + 1L
  })
  observeEvent(rv$submitHCJobConfirm, {
    if (!verifyCanSolve(async = TRUE, buttonId = "btSubmitHcJob")) {
      return()
    }
    if (!length(noHcubeScen()) || noHcubeScen() < 1L || noHcubeScen() > MAX_NO_HCUBE) {
      return()
    }
    if (hcubeBuilder$getNoScen() < 1L) {
      flog.error("Hypercube submission button clicked without scenarios available in HcubeBuilder. This should never happen and is likely an attempt to tamper with the app!")
      showElReplaceTxt(session, "#newHcJobError", lang$errMsg$unknownError)
      return()
    }
    disableEl(session, "#btSubmitHcJobConfirmUnsolved")
    disableEl(session, "#btSubmitHcJobConfirm")
    hideEl(session, "#newHcJobError")
    tryCatch(
      {
        prog <- Progress$new()
        on.exit(prog$close(), add = TRUE)
        prog$set(message = lang$nav$dialogHcube$waitDialog$title, value = 0)
        currentMetaTmp <- activeScen$getMetadataInfo()
        attachmentsTmp <- Attachments$new(
          db, list(maxSize = attachMaxFileSize, maxNo = attachMaxNo),
          workDir,
          names(ioConfig$modelIn),
          names(ioConfig$modelOut),
          ioConfig$inputDsNamesBase
        )
        if (length(currentMetaTmp$attach) && length(config$outputAttachments)) {
          outputAttachNames <- vapply(config$outputAttachments, "[[", character(1L),
            "filename",
            USE.NAMES = FALSE
          )
          currentMetaTmp$attach$attachmentsToRemove <- c(
            currentMetaTmp$attach$attachmentsToRemove,
            outputAttachNames
          )
          localOutputAttach <- basename(currentMetaTmp$attach$localAttachments$filePaths) %in% outputAttachNames
          if (any(localOutputAttach)) {
            currentMetaTmp$attach$localAttachments$filePaths <- currentMetaTmp$attach$localAttachments$filePaths[!localOutputAttach]
            currentMetaTmp$attach$localAttachments$execPerm <- currentMetaTmp$attach$localAttachments$execPerm[!localOutputAttach]
            updatesOutputAttach <- currentMetaTmp$attach$attachmentsUpdateExec$name %in% outputAttachNames
            if (any(updatesOutputAttach)) {
              currentMetaTmp$attach$attachmentsUpdateExec$name <- currentMetaTmp$attach$attachmentsUpdateExec$name[!updatesOutputAttach]
              currentMetaTmp$attach$attachmentsUpdateExec$execPerm <- currentMetaTmp$attach$attachmentsUpdateExec$execPerm[!updatesOutputAttach]
            }
          }
        }
        hcJobConfig <- Scenario$new(
          db = db, sname = paste(activeScen$getScenName(), as.numeric(Sys.time())),
          tags = input$newHcubeTags, overwrite = FALSE,
          isNewScen = TRUE,
          duplicatedMetadata = currentMetaTmp,
          views = views, attachments = attachmentsTmp,
          scode = SCODEMAP[["hcube_inputs"]]
        )
        hcScalarsTmp <- hcubeBuilder$getHcubeScalars()
        hcJobConfig$save(scenData$get("sb", symNames = ioConfig$inputDsNames),
          msgProgress = lang$progressBar$saveScenDb
        )
        sid <- hcJobConfig$getSid()
        db$exportScenDataset(
          bind_cols(
            `_sid` = rep.int(sid, nrow(hcScalarsTmp)),
            hcScalarsTmp
          ),
          "_hcScalars"
        )
        hcJobConfig$finalize()
        solveOptions <- NULL
        instanceInfo <- worker$getInstanceInfo()
        if (length(instanceInfo) && identical(instanceInfo[["instancesSupported"]], TRUE)) {
          solveOptions <- list(selectedInstance = input$selHcWorkerInstance)
        }
        worker$runHcube(
          dynamicPar = hcubeBuilder,
          sid = sid, tags = input$newHcubeTags,
          solveOptions = solveOptions
        )
        prog$inc(amount = 1, detail = lang$nav$dialogHcube$waitDialog$desc)
        removeModal()
        showHideEl(session, "#hcubeSubmitSuccess", 2000)
        showQuotaWarnings(session, worker$getQuotaWarning())
      },
      error = function(e) {
        errMsg <- conditionMessage(e)
        if (identical(errMsg, "401")) {
          flog.info("Could not generate Hypercube job (unauthorized).")
          return(showElReplaceTxt(session, "#newHcJobError", lang$errMsg$sessionExpired$desc))
        }
        if (identical(errMsg, "402")) {
          flog.info("Could not generate Hypercube job (quota exceeded).")
          showQuotaWarnings(session, worker$getQuotaWarning())
          return(showElReplaceTxt(session, "#newHcJobError", lang$errMsg$quotaWarning$quotaExceeded))
        }
        flog.error(
          "Unexpected error while generating new Hypercube job. Error message: '%s'",
          errMsg
        )
        enableEl(session, "#btSubmitHcJobConfirm")
        hasUnsolvedHcScen <- noHcubeScenSolved() < noHcubeScen()
        if (identical(length(hasUnsolvedHcScen), 1L) && hasUnsolvedHcScen) {
          enableEl(session, "#btSubmitHcJobConfirmUnsolved")
        }
        showElReplaceTxt(session, "#newHcJobError", lang$errMsg$unknownError)
      }
    )
  })
}
