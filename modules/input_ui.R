# generate modalDialog used to import input data
observeEvent(input$btImport, {
  flog.debug("%s: Import input data button clicked.", uid)
  #disable button animation
  shinyjs::removeClass("btImport", "glow-animation")
  isInSolveMode <<- TRUE
  if(config$activateModules$scenario){
    # fetch list of saved scenarios
    # only load single scenario as not in comparison mode
    errMsg <- NULL
    tryCatch({
      scenMetadata <<- db$fetchScenList(noBatch = TRUE)
    }, error = function(e){
      flog.error("Problems fetching list of saved scenarios from database. Error message: %s.", e)
      errMsg <<- sprintf(lang$errMsg$fetchScenData$desc, modelInAlias[i])
    })
    if(is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))){
      return(NULL)
    }
  }
  showLoadDataDialog(scenMetadata = scenMetadata, 
                     noDataInUI = is.null(isolate(rv$activeSname)))
  
  if(config$activateModules$scenario){
    if(identical(nrow(scenMetadata), 0L)){
      # no scenarios in database, so select local tab
      updateTabsetPanel(session, "tb_importData", selected = "tb_importData_local")
    }
    addClass("btSortTime", class = "scen-sort-by-selected")
  }
})
observeEvent(input$localInput$name, {
  flog.debug("A new input file with name: '%s' was uploaded.", 
             isolate(input$localInput$name))
  
  if(!is.null(isolate(input$localInput$name))){
    if(is.null(isolate(rv$activeSname))){
      updateTextInput(session, "local_newScenName", value = gsub("\\.[^\\.]+$", "", 
                                                                 isolate(input$localInput$name)))
    }
    enable("btCheckSnameLocal")
  }
})
observeEvent(input$btNewNameLocal, {
  flog.debug("Button to choose a different scenario name clicked.")
  hide("loadLocal_scenNameExists")
  shinyjs::show("loadLocal_content")
})