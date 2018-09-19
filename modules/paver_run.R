# run the paver module (python)

# example trace files
traceFiles <- c("gimli.trc", "bombur.trc", "thorin.trc", "bifur.trc", "balin.trc")

#run paver
paver <- NULL
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
    "--failtime", "3600", "--writehtml", paste0(workDir, "paver", .Platform$file.sep, "results"))
}
observeEvent(input$btPaver, {
  removeModal()
  flog.debug("Run Paver button clicked.")
  removeTab("tabs_paver_results", "documentation")
  removeTab("tabs_paver_results", "stat_Efficiency")
  removeTab("tabs_paver_results", "solvedata")
  shinyjs::hide("btLoadBatch")
  updateTabsetPanel(session, "tabs_paver_results", selected = "index")
  output$paverResults <- renderUI(character())
  shinyjs::show("paver_load")
  hide("btNewPaverRun")
  shinyjs::enable("btPaverInterrupt")
  updateTabsetPanel(session, "sidebar.menu", selected = "batchAnalyze")
  
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
      paverResultTabs <- c("index", "stat_Efficiency", "solvedata", "documentation")
      lapply(2:length(paverResultTabs), function(i){
           insertTab("tabs_paver_results", target = paverResultTabs[i-1], position = "after",
                    tabPanel(paverResultTabs[i], value = paverResultTabs[i],
                             tags$div(style = "overflow: auto; height: 75vh;",
                                      includeHTML(paste0(workDir, "paver", .Platform$file.sep, 
                                                         "results", .Platform$file.sep, paverResultTabs[i], ".html"))
                             )
                    )
          ) 
        })
      return(includeHTML(paste0(workDir, "paver", .Platform$file.sep, "results", .Platform$file.sep, paverResultTabs[1], ".html")))
    }
  )
})