observeEvent(input$jobListPanel, {
  req(input$jobListPanel == "joblist")
  rv$jobListPanel <- rv$jobListPanel + 1L
})
observe({
  rv$jobListPanel
  input$refreshActiveJobs
  showEl(session, "#jImport_load")
  on.exit(hideEl(session, "#jImport_load"), add = TRUE)
  
  flog.trace("Refreshing job list..")
  isolate({
    flog.debug("Job list tab clicked. Job list is being reloaded")
    if(!worker$validateCredentials()){
      showLoginDialog(cred = worker$getCredentials(), forward = "jobListPanel")
      hideEl(session, "#jImport_load")
      return(NULL)
    }
    
    jobList <- NULL
    redirect <- FALSE
    tryCatch({
      jobList <- worker$getJobList()
      if(jobList$newCompleted){
        showNewCompletedJobsDialog(hcubeMode = config$activateModules$hcubeMode)
      }
      jobList <- jobList$jobList
    }, error = function(e){
      errMsg <- conditionMessage(e)
      
      if(errMsg == 404L || startsWith(errMsg, "Could not") || 
         startsWith(errMsg, "Timeout"))
        return(showHideEl(session, "#fetchJobsUnknownHost", 6000L))
      
      if(errMsg == 401L || errMsg == 403L){
        showLoginDialog(cred = worker$getCredentials(), forward = "jobListPanel")
        redirect <<- TRUE
        return(showHideEl(session, "#fetchJobsAccessDenied", 6000L))
      }
      flog.error("Problems loading job list from database. Error message: '%s'.", 
                 conditionMessage(e))
      showHideEl(session, "#fetchJobsError", 6000L)
    })
    if(redirect)
      return()
    
    output$jImport_output <- renderUI(
      getJobsTable(jobList, 
                   hcubeMode = config$activateModules$hcubeMode,
                   showLogFileDialog = any(config$activateModules$logFile, 
                                           config$activateModules$lstFile, 
                                           config$activateModules$miroLogFile)))
    hideEl(session, "#jImport_load")
  })
})

observeEvent(input$btShowHistory, {
  flog.trace("Show job history button clicked.")
  jobList <- NULL
  err <- FALSE
  tryCatch({
    jobList <- worker$getJobList(jobHist = TRUE)$jobList
  }, error = function(e){
    errMsg <- conditionMessage(e)
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
  
  showJobHistoryDialog(jobList,  hcubeMode = config$activateModules$hcubeMode)
})

observeEvent(input$discardJob, {
  err <- FALSE
  jID <- input$discardJob
  flog.trace("Discard job button clicked. Job ID: '%s'.", jID)
  if(is.null(worker$getPid(jID))){
    flog.error("A job that user has no read permissions was attempted to be fetched. Job ID: '%s'.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  tryCatch({
    worker$updateJobStatus(JOBSTATUSMAP[['discarded']], 
                           jID, 
                           tags = isolate(input[["jTag_" %+% jID]]))
    rv$jobListPanel <- rv$jobListPanel + 1L
  }, error = function(e){
    errMsg <- conditionMessage(e)
    if(errMsg == 401L || errMsg == 403L){
      showLoginDialog(cred = worker$getCredentials())
      err <<- TRUE
      return()
    }
    flog.error("Problems interrupting job: '%s'. Error message: '%s'.", jID, errMsg)
    showHideEl(session, "#fetchJobsError")
  })
  if(err)
    return()
})
