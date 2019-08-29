observeEvent(input$showJobLog, {
  flog.trace("Show job log button clicked.")
  
  pID <- worker$getPid(input$showJobLog)
  if(is.null(pID)){
    flog.error("A job that user has no read permissions was attempted to be fetched. Job ID: '%s'.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  
  logContent <- tryCatch({
    worker$readTextEntity(paste0(modelName, ".log"), 
                          pID)
  }, error = function(e){
    flog.error("Could not retrieve job log. Error message: '%s'.", 
               conditionMessage(e))
    return(1L)
  })
  if(is.integer(logContent)){
    flog.info("Could not retrieve job log. Return code: '%s'.", logContent)
    if(logContent == 401L || logContent == 403L){
      showHideEl(session, "#fetchJobsAccessDenied")
    }else{
      showHideEl(session, "#fetchJobsError")
    }
    return(NULL)
  }
  showJobLogFileDialog(logContent)
})

observeEvent(input$importJob, {
  jobImportID <<- isolate(input$importJob)
  flog.trace("Import Job button clicked. Job ID: '%s'.", jobImportID)
  if(!is.integer(jobImportID) || length(jobImportID) != 1L){
    flog.error("Invalid job ID: '%s'.", jobImportID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  if(!identical(worker$getStatus(jobImportID), JOBSTATUSMAP[['completed']])){
    flog.error("Import button was clicked but job is not yet marked as 'completed' (Job ID: '%s'). The user probably tampered with the app.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  
  if(rv$unsavedFlag){
    showRemoveScenDialog("importJobConfirm")
  }else{
    rv$importJobConfirm <- rv$importJobConfirm + 1L
  }
})

observeEvent(
  virtualActionButton(input$importJobNew, rv$importJobNew), {
    removeModal()
    errMsg <- NULL
    if(!identical(worker$getStatus(jobImportID), JOBSTATUSMAP[['completed']])){
      flog.error("Import button was clicked but job is not yet marked as 'completed' (Job ID: '%s'). The user probably tampered with the app.", jID)
      showHideEl(session, "#fetchJobsError")
    }
    tryCatch({
      activeScen <<- Scenario$new(db = db, sname = rv$activeSname, 
                                  tags = scenTags, overwrite = TRUE)
      scenTags   <<- NULL
      rv$importJobConfirm <- rv$importJobConfirm + 1L
    }, error = function(e){
      flog.error("Some error occurred creating new scenario. Error message: %s.", e)
      errMsg <<- lang$errMsg$saveScen$desc
    })
    if(is.null(showErrorMsg(lang$errMsg$saveScen$title, errMsg))){
      return(NULL)
    }
  })

observeEvent(virtualActionButton(
  input$importJobConfirm,
  rv$importJobConfirm), {
    req(length(jobImportID) == 1L)
    removeModal()
    if(!identical(worker$getStatus(jobImportID), JOBSTATUSMAP[['completed']])){
      flog.error("Import button was clicked but job is not yet marked as 'completed' (Job ID: '%s'). The user probably tampered with the app.", jID)
      showHideEl(session, "#fetchJobsError")
    }
    resetWidgetsOnClose <<- FALSE
    if(!closeScenario()){
      return()
    }
    rv$activeSname  <- worker$getJobName(jobImportID)
    newInputCount <- 0L
    errMsg <- NULL
    overwriteInput <- TRUE
    scalarDataset <- NULL
    loadMode <- "csv"
    datasetsToFetch <- names(modelIn)
    
    progress <- Progress$new()
    
    on.exit(progress$close())
    
    progress$set(message = lang$progressBar$importScen$title, 
                 detail = lang$progressBar$importScen$downloadResults, 
                 value = 0.1)
    
    tryCatch({
      tmpdir     <- worker$readOutput(worker$getPid(jobImportID))
      on.exit(unlink(tmpdir), add = TRUE)
    }, error = function(e){
      errMsg <<- conditionMessage(e)
      if(errMsg == 401L || errMsg == 403L){
        showLoginDialog(cred = worker$getCredentials(), 
                        forwardOnSuccess = "importJobConfirm")
        return()
      }
      flog.error("Problems loading job list from database. Error message: '%s'.", 
                 conditionMessage(e))
      showHideEl(session, "#fetchJobsError")
    })
    if(length(errMsg))
      return()
    
    loadModeWorkDir  <- tmpdir
    loadModeFileName <- NULL
    tryCatch({
      progress$set(message = lang$progressBar$importScen$renderInput, value = 0.5)
      # load input data 
      source("./modules/input_load.R", local = TRUE)
      if(!is.null(errMsg)){
        return(NULL)
      }
      scalarIdTmp <- match(scalarsFileName, 
                           tolower(names(scenInputData)))[[1L]]
      jobResults <- loadScenData(scalarsName = scalarsOutName, metaData = modelOut, workDir = tmpdir, 
                                 modelName = modelName, errMsg = lang$errMsg$GAMSOutput$badOutputData,
                                 scalarsFileHeaders = scalarsFileHeaders,
                                 templates = modelOutTemplate, method = "csv", csvDelim = config$csvDelim, 
                                 hiddenOutputScalars = config$hiddenOutputScalars)
    }, error = function(e){
      flog.error("Problems reading job output data. Error message: '%s'.", conditionMessage(e))
      errMsg <<- lang$errMsg$readOutput$desc
    })
    if(is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))){
      return(NULL)
    }
    if(!is.na(scalarIdTmp)){
      scalarData[["scen_1_"]] <<- scenInputData[[scalarIdTmp]]
    }else{
      scalarData[["scen_1_"]] <<- tibble()
    }
    if(!is.null(jobResults$scalar)){
      scalarData[["scen_1_"]] <<- bind_rows(jobResults$scalar, 
                                            scalarData[["scen_1_"]])
    }
    if(!is.null(jobResults$tabular)){
      scenData[["scen_1_"]] <<- jobResults$tabular
    }
    if(config$saveTraceFile){
      tryCatch({
        traceData <<- readTraceData(file.path(tmpdir, 
                                              paste0(tableNameTracePrefix,
                                                     modelName, ".trc")), 
                                    traceColNames)
      }, error = function(e){
        errMsg <<- conditionMessage(e)
        flog.info("Problems loading trace data. Error message: %s.", e)
      })
    }
    progress$set(message = lang$progressBar$importScen$renderOutput, value = 0.8)
    
    storeGAMSOutputFiles(tmpdir)
    
    #select first tab in current run tabset
    switchTab(session, "output")
    updateTabsetPanel(session, "scenTabset",
                      selected = "results.current")
    updateTabsetPanel(session, "contentCurrent",
                      selected = "contentCurrent_1")
    
    jobResults <- NULL
    # rendering tables and graphs
    renderOutputData()
    
    tryCatch({
      worker$updateJobStatus(JOBSTATUSMAP[['imported']], 
                             jobImportID)
    }, error = function(e){
      flog.warn("Problems updating job status. Error message: '%s'.", 
                conditionMessage(e))
    })
    rv$jobListPanel <- rv$jobListPanel + 1L
    # mark scenario as unsaved
    markUnsaved()
  })
