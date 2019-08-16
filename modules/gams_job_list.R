observe({
  rv$jobListPanel
  input$refreshActiveJobs
  if(input$jobListPanel != "joblist"){
    return()
  }
  showEl(session, "#jImport_load")
  isolate({
    flog.debug("Job list tab clicked. Job list is being reloaded")
    if(!worker$validateCredentials()){
      showLoginDialog(cred = worker$getCredentials(), forward = "jobListPanel")
      return(NULL)
    }
    
    jobList <- NULL
    redirect <- FALSE
    tryCatch({
      jobList <- worker$getJobList()
    }, error = function(e){
      errMsg <- conditionMessage(e)
      if(errMsg == 401L || errMsg == 403L){
        showLoginDialog(cred = worker$getCredentials(), forward = "jobListPanel")
        redirect <<- TRUE
        return()
      }
      flog.error("Problems loading job list from database. Error message: '%s'.", 
                 conditionMessage(e))
      showHideEl(session, "#fetchJobsError")
    })
    if(redirect)
      return()
    
    output$jImport_output <- renderUI(getHypercubeJobsTable(jobList, hcubeMode = FALSE))
    hideEl(session, "#jImport_load")
  })
})

observeEvent(input$btShowHistory, {
  flog.trace("Show job history button clicked.")
  jobList <- NULL
  err <- FALSE
  tryCatch({
    jobList <- worker$getJobList(jobHist = TRUE)
  }, error = function(e){
    if(errMsg == 401L || errMsg == 403L){
      showLoginDialog(cred = worker$getCredentials())
      err <<- TRUE
      return()
    }
    flog.error("Problems loading job list from database. Error message: '%s'.", 
               conditionMessage(e))
    showHideEl(session, "#fetchJobsError")
  })
  if(err)
    return()
  
  showJobHistoryDialog(jobList,  hcubeMode = FALSE)
})

observeEvent(input$importJob, {
  jID <- isolate(input$importJob)
  flog.trace("Import Job button clicked. Job ID: '%s'.", jID)
  if(!is.integer(jID) || length(jID) != 1L){
    flog.error("Invalid job ID: '%s'.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  errMsg <- NULL
  tryCatch({
    tmpdir <- worker$readOutput()
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
  if(!is.null(jobResults$scalar)){
    scalarData[["scen_1_"]] <<- jobResults$scalar
  }
  if(!is.null(jobResults$tabular)){
    scenData[["scen_1_"]] <<- jobResults$tabular
  }
  if(config$saveTraceFile){
    tryCatch({
      traceData <<- readTraceData(paste0(tmpdir, tableNameTracePrefix, modelName, ".trc"), 
                                  traceColNames)
    }, error = function(e){
      flog.info("Problems loading trace data. Error message: %s.", e)
    })
  }
  
  jobResults <- NULL
  # rendering tables and graphs
  renderOutputData()
  
  # mark scenario as unsaved
  markUnsaved()
})

observeEvent(input$showJobLog, {
  flog.debug("Show job log button clicked.")
  logContent <- tryCatch({
    pID <- worker$getPid(input$showJobLog)
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
