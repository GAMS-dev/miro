# run the paver module (python)

# example trace files
traceFiles <- c("thorin.trc", "balin.trc")
paverFileDir <- paste0(workDir, "paver", .Platform$file.sep)

#run paver
paver <- processx::process$new("python")

genPaverArgs <- function(traceFilenames){
  stopifnot(is.character(traceFilenames), length(traceFilenames) >= 1)
  
  #  tracePath <- paste0(workDir, "paver", .Platform$file.sep, "traceFiles", .Platform$file.sep)
  tracePath <- paste0(getwd(), .Platform$file.sep, "tools", .Platform$file.sep, 
                      "paver", .Platform$file.sep, "examples", 
                      .Platform$file.sep, "miplib2010", .Platform$file.sep)
  c(paste0(getwd(), .Platform$file.sep, "tools", .Platform$file.sep, "paver", .Platform$file.sep, "paver.py"), 
    tracePath %+% traceFilenames,
    # solution files
    #paste0(getwd(), .Platform$file.sep, "tools", .Platform$file.sep, "paver_full", .Platform$file.sep, "solu", .Platform$file.sep, "miplib2010.solu"),
    # paver options
    "--failtime", "3600", "--writehtml", paste0(workDir, "paver") , "--writeimg", paverFileDir)
}
observeEvent(input$btPaverConfig, {
  shinyjs::show("configPaver")
  shinyjs::show("btPaver")
  hide("btPaverConfig")
})
observeEvent(input$btPaver, {
  req(input$selPaverAttribs)
  
  if(batchLoad$getNoSolvers(rv$fetchedScenarios[sidsToLoad, ], 
                            input$selPaverAttribs) > maxSolversPaver){
    shinyjs::show("configPaverMaxSolversErr")
    return()
  }else{
    traceFileDir <- paste0(workDir, "trace", .Platform$file.sep)
    errMsg <- NULL
    tryCatch({
      if(!dir.exists(traceFileDir)){
        dir.create(traceFileDir, showWarnings = FALSE)
      }
    }, error = function(e){
      flog.error("Problems creating temporary folder where trace files will be stored. Error message: '%s'.", e)
      errMsg <<- sprintf(lang$errMsg$fileWrite$desc, "./trace")
    })
    if(is.null(showErrorMsg(lang$errMsg$fileWrite$title, errMsg))){
      return()
    }
      
    batchLoad$genPaverTraceFiles(traceFileDir)
  }
  removeModal()
  if(!is.null(paver$get_exit_status())){
    flog.debug("Run Paver button clicked.")
    removeTab("tabs_paver_results", "stat_Status")
    removeTab("tabs_paver_results", "stat_Efficiency")
    removeTab("tabs_paver_results", "stat_SolutionQuality")
    removeTab("tabs_paver_results", "solvedata")
    removeTab("tabs_paver_results", "documentation")
    shinyjs::hide("btLoadBatch")
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
        errMsg <<- "Image file directory could not be created. Check that you have sufficient read/write permissions in www folder."
      })
    }
    addResourcePath("paver", paverFileDir)
  
    errMsg <- NULL
    # run Python/Paver
    tryCatch({
      paver <<- processx::process$new("python", args = genPaverArgs(traceFiles), windows_hide_window = TRUE)
      #, "--refsolver",  "Thorin"
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
    # include the html files into seperate tabs
    output$paverResults <- renderUI(
      if(!is.null(paverStatus()) && paverStatus() == 0){
        hide("paver_load")
        paverResultTabs <- c("index", "stat_Status", "stat_Efficiency", "stat_SolutionQuality", "solvedata", "documentation")
        lapply(2:length(paverResultTabs), function(i){
          insertTab("tabs_paver_results", target = paverResultTabs[i-1], position = "after",
                    tabPanel(paverResultTabs[i], value = paverResultTabs[i],
                             tags$div(style = "overflow: auto; height: 75vh;",
                                      includeHTML(paste0(workDir, "paver", .Platform$file.sep, paverResultTabs[i], ".html"))
                             )
                    )
          ) 
        })
        return(includeHTML(paste0(workDir, "paver", .Platform$file.sep, paverResultTabs[1], ".html")))
      }
    )
  }else{
    showModal(modalDialog(
      title = "Paver is already in use",
      sprintf("Please wait until the process is finished."),
      footer = tagList(
        modalButton("Close")
      ),
      fade = TRUE, easyClose = FALSE
    ))
  }
})