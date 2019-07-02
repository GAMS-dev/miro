# generate modalDialog used to import input data
observeEvent(input$btImport, {
  flog.debug("%s: Import input data button clicked.", uid)
  #disable button animation
  removeClassEl(session, "#btImport", "glow-animation")
  isInSolveMode <<- TRUE
  dbTagList     <- NULL
  maxNoScenExceeded <- FALSE
  activeSid <- NULL
  scenListDb <- NULL
  if(config$activateModules$scenario){
    # fetch list of saved scenarios
    # only load single scenario as not in comparison mode
    errMsg <- NULL
    tryCatch({
      scenMetaDb       <<- db$fetchScenList(scode = if(config$activateModules$hcubeMode) -1L else 0L)
      dbTagList        <- csv2Vector(scenMetaDb[[stagIdentifier]])
    }, error = function(e){
      flog.error("Problems fetching list of saved scenarios from database. Error message: %s.", e)
      errMsg <<- sprintf(lang$errMsg$fetchScenData$desc, modelInAlias[i])
    })
    if(is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))){
      return(NULL)
    }
    if(length(scenMetaDb) && nrow(scenMetaDb) > maxNoScenToShow){
      scenMetaDbSubset <<- scenMetaDb[order(scenMetaDb[[stimeIdentifier]], 
                                            decreasing = TRUE), ][seq_len(maxNoScenToShow), ]
      maxNoScenExceeded <- TRUE
    }else{
      scenMetaDbSubset <<- scenMetaDb
      maxNoScenExceeded <- FALSE
    }
    if(!is.null(activeScen) && length(scenMetaDbSubset)){
      activeSid  <- activeScen$getSid()
      scenListDb <- db$formatScenList(scenMetaDbSubset[scenMetaDbSubset[[1L]] != activeSid, ], 
                                      stimeIdentifier, desc = TRUE)
    }else{
      scenListDb <- db$formatScenList(scenMetaDbSubset, 
                                      stimeIdentifier, desc = TRUE)
    }
  }
    
  showLoadDataDialog(scenListDb = scenListDb, 
                     noDataInUI = is.null(isolate(rv$activeSname)), dbTagList = dbTagList)
  if(maxNoScenExceeded)
    showHideEl(session, "#importScenMaxNoScen", 4000L)
  
  if(config$activateModules$scenario){
    if(!length(scenListDb)){
      # no scenarios in database, so select local tab
      updateTabsetPanel(session, "tb_importData", selected = "tb_importData_local")
    }
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