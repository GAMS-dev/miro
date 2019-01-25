# generate modalDialog used to import input data
observeEvent(input$btImport, {
  flog.debug("%s: Import input data button clicked.", uid)
  #disable button animation
  removeClassEl(session, "#btImport", "glow-animation")
  isInSolveMode <<- TRUE
  dbTagList     <- NULL
  if(config$activateModules$scenario){
    # fetch list of saved scenarios
    # only load single scenario as not in comparison mode
    errMsg <- NULL
    tryCatch({
      scenMetaDb       <<- db$fetchScenList(scode = if(config$activateModules$hcubeMode) 2L else 0L)
      dbTagList        <- csv2Vector(scenMetaDb[[stagIdentifier]])
      scenMetaDbSubset <<- scenMetaDb
    }, error = function(e){
      flog.error("Problems fetching list of saved scenarios from database. Error message: %s.", e)
      errMsg <<- sprintf(lang$errMsg$fetchScenData$desc, modelInAlias[i])
    })
    if(is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))){
      return(NULL)
    }
  }
  showLoadDataDialog(scenMetadata = scenMetaDb, 
                     noDataInUI = is.null(isolate(rv$activeSname)), dbTagList = dbTagList)
  
  if(config$activateModules$scenario){
    if(identical(nrow(scenMetaDb), 0L)){
      # no scenarios in database, so select local tab
      updateTabsetPanel(session, "tb_importData", selected = "tb_importData_local")
    }
    addClassEl(session, "#btSortTime", "scen-sort-by-selected")
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
    enableEl(session, "#btCheckSnameLocal")
  }
})
observeEvent(input$btNewNameLocal, {
  flog.debug("Button to choose a different scenario name clicked.")
  hideEl(session, "#loadLocal_scenNameExists")
  showEl(session, "#loadLocal_content")
})