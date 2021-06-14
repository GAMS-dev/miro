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
        showNewCompletedJobsDialog(hcubeMode = LAUNCHHCUBEMODE)
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
                   hcubeMode = LAUNCHHCUBEMODE,
                   showLogFileDialog = any(config$activateModules$logFile, 
                                           config$activateModules$lstFile, 
                                           config$activateModules$miroLogFile)))
    hideEl(session, "#jImport_load")
  })
})

if(isTRUE(config$activateModules$remoteExecution)){
  observeEvent(input$downloadJobData, {
    jID <- isolate(input$downloadJobData)
    flog.trace("Download job data button clicked. Job ID: '%s'.", jID)
    if(!is.integer(jID) || length(jID) != 1L){
      flog.error("Invalid job ID: '%s'.", jID)
      return(showHideEl(session, "#fetchJobsError", 6000L))
    }
    if(length(worker$getActiveDownloads()) > 10L){
      flog.info("Can not download job as maximum number of: %d parallel downloads is reached.", 
                length(resDlIdx))
      return(showHideEl(session, "#fetchJobsMaxDownloads", 6000L))
    }
    res <- tryCatch(worker$getJobResults(jID),
                    error = function(e){
                      errMsg <- conditionMessage(e)
                      if(errMsg == 401L || errMsg == 403L){
                        showHideEl(session, "#fetchJobsAccessDenied", 6000L)
                        return()
                      }
                      if(errMsg == 404L){
                        showHideEl(session, "#fetchJobsJobNotFound", 6000L)
                        tryCatch(worker$updateJobStatus(JOBSTATUSMAP[['corrupted(noProcess)']], jID),
                                 error = function(e){
                                   flog.error("Problems updating job status of job ID: '%s'. Error message: '%s'.",
                                              jID, conditionMessage(e))
                                 })
                        rv$jobListPanel <- rv$jobListPanel + 1L
                        return()
                      }
                      flog.error("Problems initiating job results download of job ID: '%s'. Error message: '%s'.", 
                                 jID, errMsg)
                      showHideEl(session, "#fetchJobsError")
                    })
    if(identical(res, 100L)){
      activeDownloads <- worker$getActiveDownloads()
      session$sendCustomMessage("gms-markJobDownloadComplete", 
                                list(id = jID, 
                                     text = lang$nav$importJobsDialog$status$downloaded,
                                     triggerImport = identical(length(activeDownloads), 0L)))
      return()
    }else if(!identical(res, 5L)){
      return()
    }
    showEl(session, paste0("#jobImportDlProgressWrapper_", jID))
    
    if(!length(asyncResObs))
      asyncResObs <<- observe({
        invalidateLater(2000L, session)
        flog.debug("Download-results-observer triggered.")
        activeDownloads <- worker$getActiveDownloads()
        if(identical(length(activeDownloads), 0L) && 
           length(asyncResObs)){
          asyncResObs$destroy()
          asyncResObs <<- NULL
          return()
        }
        for(dlID in activeDownloads){
          dlProgress <- tryCatch(worker$getJobResults(dlID),
                                 error = function(e){
                                   errMsg <- conditionMessage(e)
                                   worker$removeActiveDownload(dlID)
                                   flog.error("Problems downloading job results. Error message: '%s'.",
                                              errMsg)
                                   showHideEl(session, "#fetchJobsError")
                                   return(-1L)
                                 })
          if(identical(dlProgress, -1L))
            next
          
          if(identical(dlProgress, 100L)){
            session$sendCustomMessage("gms-markJobDownloadComplete", 
                                      list(id = dlID, 
                                           text = lang$nav$importJobsDialog$status$downloaded,
                                           triggerImport = identical(length(activeDownloads), 1L)))
            return()
          }
          session$sendCustomMessage("gms-updateJobProgress", 
                                    list(id = paste0("#jobImportDlProgress_", dlID), 
                                         progress = list(noCompleted = dlProgress, 
                                                         noTotal = 100L)))
        }
      })
  })
}

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
  
  showJobHistoryDialog(jobList,  hcubeMode = LAUNCHHCUBEMODE)
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
                           tags = input[["jTag_" %+% jID]])
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
