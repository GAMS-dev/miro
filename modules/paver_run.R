# run the paver module (python)
paver <- NULL
traceFileDir <- file.path(workDir, "trace")
# paver solution files
paverFileDir <- file.path(workDir, "paver")

genPaverArgs <- function(traceFilenames, clArgs = NULL){
  stopifnot(is.character(traceFilenames), length(traceFilenames) >= 1)
  extraClArgs <- NULL
  if(length(clArgs)){
    stopifnot(is.character(clArgs))
    extraClArgs <- unlist(lapply(strsplit(clArgs, "( ?= ?)| "), function(el){
      el[1] <- tolower(gsub("--", "", el[1], fixed = TRUE)[1L])
      if(el[1] %in% c("zerogaptol", "nocheckinstanceattr", 
                      "numpts", "novirt", "refsolver", "nosortsolver",
                      "bw", "ccreltol", "ccabstol", "ccfeastol",
                      "ccopttol", "timeshift", "nodeshift", "mintime",
                      "failtime", "failnodes", "gaptol", "evalgap",
                      "filtertime", "filternodes", "timerelimpr",
                      "boundrelimpr", "ignoredualbounds", "eval", 
                      "extendedprofiles", "optfileisrunname")){
        return(c(paste0("--", el[1]), el[-1]))
      }else{
        flog.info("Ignored unknown paver option: '%s'.", el[1])
        return(NULL)
      }
    }), use.names = FALSE)
    if(length(extraClArgs)){
      flog.debug("Calling PAVER with extra arguments: '%s'.", paste(extraClArgs, collapse = " "))
    }
  }
    
  c(file.path(getwd(), "tools", "paver", "paver.py"),
    traceFilenames, if(length(extraClArgs)) extraClArgs, "--writehtml", paverFileDir , "--writeimg", paverFileDir, "--gmswebiter", gmswebiter)
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
  scenToFetch <- rv$fetchedScenarios[[1]] %in% sidsToLoad
  if(!any(scenToFetch)){
    flog.warn("Paver was attempted to be started while no scenarios were selected.")
    showHideEl(session, "#paverRunUnknownError", 6000L)
    return()
  }
  tryCatch({
    exceedsMaxNoSolvers <- hcubeLoad$exceedsMaxNoSolvers(rv$fetchedScenarios[scenToFetch, , drop = FALSE], 
                                                         input$selPaverAttribs, maxSolversPaver,
                                                         isolate(input$paverExclAttrib))
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
    paverDir <- paste0(workDir, "paver")
    paverClArgs <- isolate(input$paverClArgs)
    tryCatch({
      if(dir.exists(traceFileDir)){
        unlink(file.path(traceFileDir, "*"), recursive = TRUE, force = TRUE)
      }else{
        dir.create(traceFileDir, showWarnings = FALSE)
      }
      if(dir.exists(paverDir)){
        unlink(file.path(paverDir, "*"), recursive = TRUE, force = TRUE)
        unlink(file.path(paverDir, "*.png"), recursive = TRUE, force = TRUE)
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
                 flog.info("Unknown error exeuting Paver. Error message: '%s'.", e)
                 showHideEl(session, "#paverRunNoTrc", 6000L)
               },
               {
                 flog.error("Unknown error exeuting Paver. Error message: '%s'.", e)
                 showHideEl(session, "#paverRunUnknownError", 6000L)
               })
      })
    if(!noErr)
      return(invisible())
    traceFiles <- list.files(traceFileDir, pattern=".trc", full.names = TRUE)
  }
  addResourcePath("paver", paverDir)
  
  removeModal()
  if(is.null(paver) || !is.null(paver$get_exit_status())){
    flog.debug("Run Paver button clicked.")
    if(gmswebiter > 0){
      removeTab("tabs_paver_results", "tabs_paver_2")
      removeTab("tabs_paver_results", "tabs_paver_3")
      removeTab("tabs_paver_results", "tabs_paver_4")
      removeTab("tabs_paver_results", "tabs_paver_5")
      removeTab("tabs_paver_results", "tabs_paver_6")
    }
    
    hideEl(session, "#btLoadHcube")
    hideEl(session, "#paverFail")
    updateTabsetPanel(session, "tabs_paver_results", selected = "tabs_paver_1")
    output$paverResults <- renderUI(character())
    showEl(session, "#paverLoad")
    hideEl(session, "#newPaverRunButton")
    enableEl(session, "#btPaverInterrupt")
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
      
      paver <<- processx::process$new(pyExec, args = genPaverArgs(traceFiles, paverClArgs), 
                                      windows_hide_window = TRUE,
                                      stdout = file.path(workDir, modelName %+% ".paverlog"),
                                      stderr = "|")
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
    output$paverResults <- renderUI({
      if(is.null(paverStatus())){
        return(NULL)
      }
      paverStatusObs$destroy()
      paverStatus <- NULL
      if(paverStatus() == 0){
        hideEl(session, "#paverLoad")
        paverResultTabs     <- paste0("tabs_paver_", 1:6)
        paverResultFiles    <- c("index", "stat_Status", "stat_Efficiency", "stat_SolutionQuality", "solvedata", "documentation")
        paverResultTabNames <- c("Index", "Status", "Efficiency", "Solution Quality", "Solve data", "Documentation")
        lapply(2:length(paverResultTabs), function(i){
          insertTab("tabs_paver_results", target = paverResultTabs[i - 1L], position = "after",
                    tabPanel(paverResultTabNames[i], value = paverResultTabs[i],
                             tags$div(id = "wrapper-" %+% paverResultTabs[i], 
                                      style = "overflow: auto; height: 75vh;",
                                      tryCatch(
                                        suppressWarnings(includeHTML(paste0(paverDir, .Platform$file.sep, 
                                                                            paverResultFiles[i], ".html"))),
                                        error = function(e){
                                          tags$div(class="errMsg", style="text-align:center;font-size:16px;margin-top:50px;",
                                                   lang$errMsg$paverFileLoad$desc)
                                          
                                        })
                             )
                    )
          ) 
        })
        return(includeHTML(paste0(paverDir, .Platform$file.sep, paverResultFiles[1], ".html")))
      }
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
    })
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