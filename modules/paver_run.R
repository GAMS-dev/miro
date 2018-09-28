# run the paver module (python)

paver <- processx::process$new("python")
traceFileDir <- paste0(workDir, "trace", .Platform$file.sep)
# paver solution files
paverFileDir <- paste0(workDir, "paver", .Platform$file.sep)

genPaverArgs <- function(traceFilenames){
  stopifnot(is.character(traceFilenames), length(traceFilenames) >= 1)
  c(paste0(getwd(), .Platform$file.sep, "tools", .Platform$file.sep, "paver", .Platform$file.sep, "paver.py"), #, "--refsolver",  "Thorin"
    traceFilenames, "--failtime", "3600", "--writehtml", paverFileDir , "--writeimg", paverFileDir)
}

observeEvent(input$btPaverConfig, {
  shinyjs::show("configPaver")
  shinyjs::show("btPaver")
  hide("btBatchLoad")
  hide("btPaverConfig")
  # if already tracefiles in tracefiledir show deletion warning
  if(length(list.files(traceFileDir)) > 0){
    shinyjs::show("deleteTrace")
  }
})

observeEvent(input$btPaver, {
  req(input$selPaverAttribs)
  
  if(batchLoad$getNoSolvers(rv$fetchedScenarios[sidsToLoad, ], 
                            input$selPaverAttribs) > maxSolversPaver){
    shinyjs::show("configPaverMaxSolversErr")
    return()
  }else{
    errMsg <- NULL
    paverDir <- workDir %+% "paver"
    tryCatch({
      if(dir.exists(traceFileDir)){
        traceFiles <- list.files(traceFileDir, pattern=".trc", full.names = T)
        unlink(traceFiles, force = TRUE)
      }else{
        dir.create(traceFileDir, showWarnings = FALSE)
      }
      if(dir.exists(paverDir)){
        unlink(paverDir, recursive = TRUE, force = TRUE)
      }
      dir.create(paverDir, showWarnings = FALSE)
    }, error = function(e){
      flog.error("Problems creating temporary folder where trace files will be stored. Error message: '%s'.", e)
      errMsg <<- sprintf(lang$errMsg$fileWrite$desc, "./trace")
    })
    if(is.null(showErrorMsg(lang$errMsg$fileWrite$title, errMsg))){
      return()
    }
    
    batchLoad$genPaverTraceFiles(traceFileDir)
    traceFiles <- list.files(traceFileDir, pattern=".trc", full.names = T)
    #traceFiles <- c(paste0(traceFileDir,"test.trc"))
  }
  removeModal()
  if(!is.null(paver$get_exit_status())){
    flog.debug("Run Paver button clicked.")
    removeTab("tabs_paver_results", "stat_Status")
    removeTab("tabs_paver_results", "stat_Efficiency")
    removeTab("tabs_paver_results", "stat_SolutionQuality")
    removeTab("tabs_paver_results", "solvedata")
    removeTab("tabs_paver_results", "documentation")
    hide("btLoadBatch")
    hide("paver_fail")
    updateTabsetPanel(session, "tabs_paver_results", selected = "index")
    output$paverResults <- renderUI(character())
    shinyjs::show("paver_load")
    hide("newPaverRunButton")
    shinyjs::enable("btPaverInterrupt")
    updateTabsetPanel(session, "sidebarMenuId", selected = "batchAnalyze")
    
    if(!dir.exists(paverFileDir)){
      tryCatch({
        dir.create(paverFileDir, recursive = TRUE, showWarnings = FALSE)
      }, warning = function(w){
        errMsg <<- "Paver file directory for solution files could not be created. Check that you have sufficient read/write permissions."
      })
    }
    addResourcePath("paver", paverFileDir)

    errMsg <- NULL
    # run paver
    tryCatch({
      paver <<- processx::process$new("python", args = genPaverArgs(traceFiles), windows_hide_window = TRUE,
                                      stdout = workDir %+% modelName %+% ".paverlog", stderr = "|")
    }, error = function(e) {
      errMsg <<- lang$errMsg$paverExec$desc
      flog.error("Python/Paver did not execute successfully. Error message: %s.", e)
    })
    if(is.null(showErrorMsg(lang$errMsg$paverExec$title, errMsg))){
      return(NULL)
    }
    paverStatus <<- reactivePoll(5000, session, checkFunc = function(){
      paver$get_exit_status()
    }, valueFunc = function(){
      paver$get_exit_status()
    })
    # include html files (seperate tabs)
    output$paverResults <- renderUI(
      if(!is.null(paverStatus())){
        if(paverStatus() == 0){
          hide("paver_load")
          paverResultTabs <- c("index", "stat_Status", "stat_Efficiency", "stat_SolutionQuality", "solvedata", "documentation")
          lapply(2:length(paverResultTabs), function(i){
            insertTab("tabs_paver_results", target = paverResultTabs[i-1], position = "after",
                      tabPanel(paverResultTabs[i], value = paverResultTabs[i],
                               tags$div(id = "wrapper-" %+% paverResultTabs[i], 
                                        style = "overflow: auto; height: 75vh;",
                                        tryCatch(
                                          includeHTML(paste0(paverDir, .Platform$file.sep, 
                                                             paverResultTabs[i], ".html")),
                                          error = function(e){
                                            return()
                                          })
                               )
                      )
            ) 
          })
          return(includeHTML(paste0(paverDir, .Platform$file.sep, paverResultTabs[1], ".html")))
        }else{
          flog.error("Problems while running paver. Error message: '%s'.", paver$read_all_error())
          hide("paver_load")
          shinyjs::show("paver_fail")
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