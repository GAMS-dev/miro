observeEvent(input$showJobLog, {
  flog.trace("Show job log button clicked.")
  if (identical(worker$validateCredentials(), FALSE)) {
    flog.debug("User is not logged in. Login dialog is opened.")
    return(showLoginDialog(cred = worker$getCredentials()))
  }
  asyncLogLoaded[] <<- FALSE
  showJobLogFileDialog(input$showJobLog)
})
observeEvent(
  {
    input$asyncLogFileTabsset
    input$showJobLog
  },
  {
    if (!length(input$asyncLogFileTabsset)) {
      return()
    }
    jID <- strsplit(input$asyncLogFileTabsset, "_", fixed = TRUE)[[1]]
    if (length(jID) < 2L) {
      flog.error("Log file could not be shown as no job ID could be identified. This looks like an attempt to tamper with the app!")
      return()
    }
    if (!identical(jID[2], input$showJobLog)) {
      return()
    }
    flog.debug("Log file for job: '%s' requested.", jID[2])
    fileType <- jID[[1L]]
    jID <- suppressWarnings(as.integer(jID[[2L]]))
    if (is.na(jID)) {
      flog.error("Log file could not be shown as no job ID could be identified. This looks like an attempt to tamper with the app!")
      return()
    }

    if (identical(fileType, "log")) {
      if (asyncLogLoaded[1L]) {
        flog.debug("Log file is already loaded. No reloading..")
        return()
      }
      asyncLogLoaded[1L] <<- TRUE
      fileToFetch <- paste0(modelNameRaw, ".log")
      containerID <- "#asyncLogContainer"
    } else if (identical(fileType, "listfile")) {
      if (asyncLogLoaded[2L]) {
        flog.debug("Listing file is already loaded. No reloading..")
        return()
      }
      asyncLogLoaded[2L] <<- TRUE
      fileToFetch <- paste0(modelNameRaw, ".lst")
      containerID <- "#asyncLstContainer"
    } else if (identical(fileType, "mirolog")) {
      if (!length(config$miroLogFile)) {
        flog.error("MIRO log file attempted to be fetched, but none is specified. This looks like an attempt to tamper with the app!")
        return()
      }
      if (asyncLogLoaded[3L]) {
        flog.debug("MIRO log file is already loaded. No reloading..")
        return()
      }
      asyncLogLoaded[3L] <<- TRUE
      fileToFetch <- config$miroLogFile
      containerID <- "#asyncMiroLogContainer"
    } else {
      flog.error("Log file type could not be identified. This looks like an attempt to tamper with the app!")
      return()
    }
    pID <- worker$getPid(jID)
    if (is.null(pID)) {
      flog.error("A job that user has no read permissions was attempted to be fetched. Job ID: '%s'.", jID)
      showHideEl(session, "#fetchJobsError")
      return(showElReplaceTxt(session, containerID, lang$errMsg$unknownError))
    }
    logContent <- tryCatch(
      {
        worker$readTextEntry(fileToFetch,
          pID,
          getSize = TRUE
        )
      },
      error = function(e) {
        statusCode <- conditionMessage(e)
        if (identical(statusCode, 404L)) {
          return(lang$nav$hcubeMode$showLogFileDialog$noContent)
        }
        flog.error(
          "Could not retrieve job log. Error message: '%s'.",
          statusCode
        )
        return(1L)
      }
    )
    if (is.integer(logContent)) {
      flog.info("Could not retrieve job log. Return code: '%s'.", logContent)
      if (identical(logContent, 401L) || identical(logContent, 403L)) {
        return(showElReplaceTxt(session, containerID, lang$nav$dialogRemoteLogin$insuffPerm))
      } else if (identical(logContent, 404L)) {
        return(showElReplaceTxt(session, containerID, lang$nav$dialogRemoteLogin$contentNotFoundError))
      } else if (identical(logContent, 413L)) {
        return(showElReplaceTxt(session, containerID, lang$errMsg$fileRead$tooLarge))
      } else {
        return(showElReplaceTxt(session, containerID, lang$errMsg$unknownError))
      }
    }
    return(session$sendCustomMessage(
      "gms-showLogContent",
      list(
        id = containerID,
        jID = jID,
        content = logContent[[1]],
        noChunks = logContent[[2]],
        type = fileType
      )
    ))
  }
)

observeEvent(input$loadTextEntryChunk, {
  fileType <- input$loadTextEntryChunk$type
  flog.debug("New text entry chunk requested.")
  if (identical(fileType, "log")) {
    fileToFetch <- paste0(modelNameRaw, ".log")
    containerID <- "#asyncLogContainer"
  } else if (identical(fileType, "listfile")) {
    fileToFetch <- paste0(modelNameRaw, ".lst")
    containerID <- "#asyncLstContainer"
  } else if (identical(fileType, "mirolog")) {
    if (!length(config$miroLogFile)) {
      flog.error("MIRO log file attempted to be fetched, but none is specified. This looks like an attempt to tamper with the app!")
      return()
    }
    fileToFetch <- config$miroLogFile
    containerID <- "#asyncMiroLogContainer"
  } else {
    flog.error("Log file type could not be identified. This looks like an attempt to tamper with the app!")
    return()
  }
  logContent <- tryCatch(
    {
      worker$readTextEntry(fileToFetch,
        worker$getPid(input$loadTextEntryChunk$jID),
        chunkNo = input$loadTextEntryChunk$chunkCount
      )
    },
    error = function(e) {
      flog.error(
        "Could not retrieve job log. Error message: '%s'.",
        conditionMessage(e)
      )
      return(1L)
    }
  )
  if (is.integer(logContent)) {
    flog.info("Could not retrieve job log. Return code: '%s'.", logContent)
    if (identical(logContent, 401L) || identical(logContent, 403L)) {
      return(showElReplaceTxt(session, containerID, lang$nav$dialogRemoteLogin$insuffPerm))
    } else if (identical(logContent, 404L)) {
      return(showElReplaceTxt(session, containerID, lang$nav$dialogRemoteLogin$contentNotFoundError))
    } else if (identical(logContent, 413L)) {
      return(showElReplaceTxt(session, containerID, lang$errMsg$fileRead$tooLarge))
    } else {
      return(showElReplaceTxt(session, containerID, lang$errMsg$unknownError))
    }
  }
  appendEl(session, containerID, logContent, triggerChange = TRUE)
})

observeEvent(input$importJob, {
  jobImportID <<- isolate(input$importJob)
  flog.trace("Import Job button clicked. Job ID: '%s'.", jobImportID)
  if (!is.integer(jobImportID) || length(jobImportID) != 1L) {
    flog.error("Invalid job ID: '%s'.", jobImportID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  if (identical(worker$validateCredentials(), FALSE)) {
    flog.debug("User is not logged in. Login dialog is opened.")
    return(showLoginDialog(cred = worker$getCredentials()))
  }
  if (!worker$getStatus(jobImportID) %in% c(JOBSTATUSMAP[["completed"]], JOBSTATUSMAP[["downloaded"]])) {
    flog.error(
      "Import button was clicked but job is not yet marked as 'completed' or 'downloaded' (Job ID: '%s'). The user probably tampered with the app.",
      jobImportID
    )
    showHideEl(session, "#fetchJobsError")
    return()
  }
  jobMeta <- worker$getInfoFromJobList(jobImportID)
  if (identical(jobMeta[["_scode"]][1], SCODEMAP[["hcube_jobconfig"]])) {
    return(importHcJob(worker$getJobResultsPath(jobImportID), jobMeta))
  }
  if (rv$unsavedFlag) {
    showRemoveScenDialog("importJobConfirm")
  } else {
    rv$importJobConfirm <- rv$importJobConfirm + 1L
  }
})

observeEvent(
  virtualActionButton(input$importJobNew, rv$importJobNew),
  {
    removeModal()
    errMsg <- NULL
    if (!worker$getStatus(jobImportID) %in% c(JOBSTATUSMAP[["completed"]], JOBSTATUSMAP[["downloaded"]])) {
      flog.error(
        "Import button was clicked but job is not yet marked as 'completed' or 'downloaded' (Job ID: '%s'). The user probably tampered with the app.",
        jobImportID
      )
      showHideEl(session, "#fetchJobsError")
      return()
    }
    tryCatch(
      {
        currentScenNameTmp <- activeScen$getScenName()
        activeScen <<- Scenario$new(
          db = db, sname = currentScenNameTmp,
          tags = scenTags,
          isNewScen = TRUE, views = views, attachments = attachments,
          rv = rv
        )
        scenTags <<- NULL
        rv$importJobConfirm <- rv$importJobConfirm + 1L
      },
      error = function(e) {
        flog.error("Some error occurred creating new scenario. Error message: %s.", conditionMessage(e))
        errMsg <<- lang$errMsg$saveScen$desc
      }
    )
    if (is.null(showErrorMsg(lang$errMsg$saveScen$title, errMsg))) {
      return(NULL)
    }
  }
)

observeEvent(virtualActionButton(
  input$importJobConfirm,
  rv$importJobConfirm
), {
  req(length(jobImportID) == 1L)
  removeModal()
  if (!worker$getStatus(jobImportID) %in% c(JOBSTATUSMAP[["completed"]], JOBSTATUSMAP[["downloaded"]])) {
    flog.error(
      "Import button was clicked but job is not yet marked as 'completed' or 'downloaded' (Job ID: '%s'). The user probably tampered with the app.",
      jobImportID
    )
    showHideEl(session, "#fetchJobsError")
    return()
  }
  resetWidgetsOnClose <<- FALSE
  if (!closeScenario()) {
    return()
  }
  newInputCount <- 0L
  errMsg <- NULL
  overwriteInput <- 1L
  scalarDataset <- NULL
  loadMode <- config$fileExchange
  datasetsToFetch <- names(modelIn)

  progress <- Progress$new()

  on.exit(progress$close())

  progress$set(
    message = lang$progressBar$importScen$title,
    detail = lang$progressBar$importScen$downloadResults,
    value = 0.1
  )

  tryCatch(
    {
      tmpdir <- worker$getJobResultsPath(jobImportID)
      on.exit(unlink(tmpdir), add = TRUE)
    },
    error = function(e) {
      flog.error(
        "Problems importing job. Error message: '%s'.",
        conditionMessage(e)
      )
      showHideEl(session, "#fetchJobsError")
    }
  )
  if (length(errMsg)) {
    return()
  }

  tryCatch(
    {
      loadMiroScenMeta(
        file.path(tmpdir, "_miro_ws_"),
        activeScen, attachments, views,
        names(modelIn)
      )
    },
    error = function(e) {
      flog.error(
        "Problems loading scenario metadata. Error message: '%s'.",
        conditionMessage(e)
      )
      showHideEl(session, "#fetchJobsError")
    }
  )
  if (length(errMsg)) {
    return()
  }

  loadModeWorkDir <- tmpdir
  loadModeFileName <- if (identical(config$fileExchange, "gdx")) MIROGdxInName else NULL
  dfClArgs <- NULL
  tryCatch(
    {
      progress$set(message = lang$progressBar$importScen$renderInput, value = 0.5)
      # load input data
      source("./modules/input_load.R", local = TRUE)
      if (!is.null(errMsg)) {
        return(NULL)
      }
      scenData$clearSandbox()
      scenData$loadSandbox(
        scenInputData, if (length(modelInFileNames)) modelInFileNames else character(),
        activeScen$getMetadataDf()
      )
      if (file.exists(file.path(tmpdir, MIROGdxOutName))) {
        scenData$loadSandbox(
          loadScenData(
            metaData = modelOut, workDir = tmpdir,
            fileName = MIROGdxOutName,
            templates = modelOutTemplate,
            method = config$fileExchange,
            csvDelim = config$csvDelim
          )$tabular,
          names(modelOut)
        )
      }
    },
    error = function(e) {
      flog.error("Problems reading job output data. Error message: '%s'.", conditionMessage(e))
      errMsg <<- lang$errMsg$readOutput$desc
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))) {
    return(NULL)
  }
  if (config$saveTraceFile) {
    tryCatch(
      {
        traceData <<- readTraceData(file.path(tmpdir, "_scenTrc.trc"))
      },
      error = function(e) {
        errMsg <<- conditionMessage(e)
        flog.info("Problems loading trace data. Error message: %s.", conditionMessage(e))
      }
    )
  }
  progress$set(message = lang$progressBar$importScen$renderOutput, value = 0.8)

  storeGAMSOutputFiles(tmpdir)

  # select first tab in current run tabset
  switchTab(session, "output")
  updateTabsetPanel(session, "scenTabset",
    selected = "results.current"
  )
  updateTabsetPanel(session, "outputTabset",
    selected = "outputTabset_1"
  )
  # rendering tables and graphs
  renderOutputData()

  tryCatch(
    {
      worker$updateJobStatus(
        JOBSTATUSMAP[["imported"]],
        jobImportID
      )
    },
    error = function(e) {
      flog.warn(
        "Problems updating job status. Error message: '%s'.",
        conditionMessage(e)
      )
    }
  )
  rv$jobListPanel <- rv$jobListPanel + 1L
  # mark scenario as unsaved
  markUnsaved(consistentOutput = TRUE)
})
