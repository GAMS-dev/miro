# run the paver module (python)

traceFileDir <- paste0(workDir, "trace", .Platform$file.sep)
# paver solution files
paverFileDir <- paste0(workDir, "paver", .Platform$file.sep)

genPaverArgs <- function(traceFilenames){
  stopifnot(is.character(traceFilenames), length(traceFilenames) >= 1)
  c(file.path(getwd(), "tools", "paver", "paver.py"),
    traceFilenames, "--failtime", "3600", "--writehtml", paverFileDir , "--writeimg", paverFileDir, "--gmswebiter", gmswebiter)
}

observeEvent(input$btPaverConfig, {
  hideEl(session, "#btHcubeLoad")
  hideEl(session, "#hcubeLoadMethod")
  hideEl(session, "#btPaverConfig")
  hideEl(session, "#btHcubeDownload")
  # if already tracefiles in tracefiledir show deletion warning
  if(length(list.files(traceFileDir)) > 0){
    showEl(session, "#deleteTrace")
  }
})

gmswebiter <- 0
observeEvent(input$btPaver, {
  flog.debug("Run paver button clicked.")
  gmswebiter <<- gmswebiter + 1
  req(input$selPaverAttribs)
  noErr <- TRUE
  tryCatch({
    exceedsMaxNoSolvers <- hcubeLoad$exceedsMaxNoSolvers(rv$fetchedScenarios[rv$fetchedScenarios[[1]] %in% sidsToLoad, ,
                                                                             drop = FALSE], 
                                                         input$selPaverAttribs, maxSolversPaver, isolate(input$paverExclAttrib))
  }, error = function(e){
      noErr <<- FALSE
      flog.error("Problems identifying whether maximum number of solvers for paver is exceeded Error message: '%s'.", e)
      showHideEl(session, "#paverRunUnknownError", 6000L)
  })
  if(!noErr)
    return()
  
  if(exceedsMaxNoSolvers){
    showHideEl(session, "#configPaverMaxSolversErr", 4000L)
    return()
  }else{
    errMsg <- NULL
    paverDir <- paste0(workDir, "paver", .Platform$file.sep)
    tryCatch({
      if(dir.exists(traceFileDir)){
        unlink(file.path(traceFileDir,"*"), recursive = TRUE, force = TRUE)
      }else{
        dir.create(traceFileDir, showWarnings = FALSE)
      }
      if(dir.exists(paverDir)){
        unlink(file.path(paverDir,"*"), recursive = TRUE, force = TRUE)
        unlink(file.path(paverDir,"*.png"), recursive = TRUE, force = TRUE)
      }
      dir.create(paverDir, showWarnings = FALSE)
    }, error = function(e){
      flog.error("Problems creating temporary folder where trace files will be stored. Error message: '%s'.", e)
      errMsg <<- sprintf(lang$errMsg$fileWrite$desc, "./trace")
    })
    if(is.null(showErrorMsg(lang$errMsg$fileWrite$title, errMsg))){
      return()
    }
    noErr <- TRUE
    tryCatch(
      hcubeLoad$genPaverTraceFiles(traceFileDir, exclTraceCols)
      ,error = function(e){
        noErr <<- FALSE
        switch(conditionMessage(e),
               noTrc = {
                 showHideEl(session, "#paverRunNoTrc", 6000L)
               },
               {
                 showHideEl(session, "#paverRunUnknownError", 6000L)
               })
      })
    if(!noErr)
      return(invisible())
    traceFiles <- list.files(traceFileDir, pattern=".trc", full.names = TRUE)
  }
  addResourcePath("paver", paverDir)
  
  removeModal()
  if(!is.null(paver$get_exit_status())){
    flog.debug("Run Paver button clicked.")
    if(gmswebiter > 0){
      removeTab("tabs_paver_results", "stat_Status")
      removeTab("tabs_paver_results", "stat_Efficiency")
      removeTab("tabs_paver_results", "stat_SolutionQuality")
      removeTab("tabs_paver_results", "solvedata")
      removeTab("tabs_paver_results", "documentation")
    }
    
    hideEl(session, "#btLoadHcube")
    hideEl(session, "#paverFail")
    updateTabsetPanel(session, "tabs_paver_results", selected = "index")
    output$paverResults <- renderUI(character())
    showEl(session, "#paverLoad")
    hideEl(session, "#newPaverRunButton")
    enableEl(session, "#btPaverInterrupt")
    updateTabsetPanel(session, "sidebarMenuId", selected = "hcubeAnalyze")
    switchTab(session, "hcubeAna")
    
    errMsg <- NULL
    # run paver
    tryCatch({
      if(length(gamsSysDir) && nchar(gamsSysDir)){
        pyExec <- file.path(gamsSysDir, "GMSPython", "python")
      }else{
        if(isWindows())
          pyExec <- "python"
        else
          pyExec <- "python3"
      }
      
      paver <<- processx::process$new(pyExec, args = genPaverArgs(traceFiles), 
                                      windows_hide_window = TRUE,
                                      stdout = workDir %+% modelName %+% ".paverlog", stderr = "|")
      rm(pyExec)
    }, error = function(e) {
      errMsg <<- lang$errMsg$paverExec$desc
      flog.error("Paver did not execute successfully. Error message: %s.", e)
    })
    if(is.null(showErrorMsg(lang$errMsg$paverExec$title, errMsg))){
      return(NULL)
    }
    paverStatus <<- reactivePoll2(5000, session, checkFunc = function(){
      paver$get_exit_status()
    }, valueFunc = function(){
      paver$get_exit_status()
    })
    paverStatusObs <- paverStatus$obs
    paverStatus    <- paverStatus$re
    # include html files (seperate tabs)
    output$paverResults <- renderUI(
      if(!is.null(paverStatus())){
        paverStatusObs$destroy()
        paverStatus <- NULL
        if(paverStatus() == 0){
          hideEl(session, "#paverLoad")
          paverResultTabs <- c("index", "stat_Status", "stat_Efficiency", "stat_SolutionQuality", "solvedata", "documentation")
          lapply(2:length(paverResultTabs), function(i){
            insertTab("tabs_paver_results", target = paverResultTabs[i - 1L], position = "after",
                      tabPanel(paverResultTabs[i], value = paverResultTabs[i],
                               tags$div(id = "wrapper-" %+% paverResultTabs[i], 
                                        style = "overflow: auto; height: 75vh;",
                                        tryCatch(
                                          suppressWarnings(includeHTML(paste0(paverDir, .Platform$file.sep, 
                                                                              paverResultTabs[i], ".html"))),
                                          error = function(e){
                                            tags$div(class="errMsg", style="text-align:center;font-size:16px;margin-top:50px;",
                                                     lang$errMsg$paverFileLoad$desc)
                                            
                                          })
                               )
                      )
            ) 
          })
          return(includeHTML(paste0(paverDir, .Platform$file.sep, paverResultTabs[1], ".html")))
        }else{
          paverError <- paver$read_error()
          flog.error("Problems while running paver. Error message: '%s'.", paverError)
          hideEl(session, "#paverLoad")
          duplicatedInstances <- regmatches(paverError, regexpr('on instance [^>]*$', paverError))
          if(length(duplicatedInstances))
            showElReplaceTxt(session, "#paverFail", sprintf(lang$nav$hcubeAnalyze$duplicatesMsg, 
                                                            substr(duplicatedInstances, 13, 
                                                                   nchar(duplicatedInstances) - 3L)))
          else
            showEl(session, "#paverFail")
        }
      }
    )
  }else{
    showModal(modalDialog(
      title = lang$nav$dialogPaverInUse$title,
      sprintf(lang$nav$dialogPaverInUse$desc),
      footer = tagList(
        modalButton(lang$nav$dialogPaverInUse$closeButton)
      ),
      fade = TRUE, easyClose = FALSE
    ))
  }
})