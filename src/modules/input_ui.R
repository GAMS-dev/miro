# generate modalDialog used to import input data
observeEvent(input$btImport, {
  flog.debug("Import input data button clicked.")
  isInSolveMode <<- TRUE
  dbTagList     <- NULL
  maxNoScenExceeded <- FALSE
  activeSid <- NULL
  scenListDb <- NULL
  # fetch list of saved scenarios
  # only load single scenario as not in comparison mode
  errMsg <- NULL
  tryCatch({
    scenMetaDb       <<- db$fetchScenList(scode = if(LAUNCHHCUBEMODE)
      SCODEMAP[['hcube_jobconfig']] else SCODEMAP[['scen']])
    dbTagList        <- csv2Vector(scenMetaDb[["_stag"]])
  }, error = function(e){
    flog.error("Problems fetching list of saved scenarios from database. Error message: %s.",
               conditionMessage(e))
    errMsg <<- sprintf(lang$errMsg$fetchScenData$desc, modelInAlias[i])
  })
  if(is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))){
    return(NULL)
  }
  if(length(scenMetaDb) && nrow(scenMetaDb) > maxNoScenToShow){
    scenMetaDbSubset <<- scenMetaDb[order(scenMetaDb[["_stime"]],
                                          decreasing = TRUE), ][seq_len(maxNoScenToShow), ]
    maxNoScenExceeded <- TRUE
  }else{
    scenMetaDbSubset <<- scenMetaDb
    maxNoScenExceeded <- FALSE
  }
  if(!is.null(activeScen) && length(activeScen$getSid()) && length(scenMetaDbSubset)){
    activeSid  <- activeScen$getSid()
    scenListDb <- formatScenList(scenMetaDbSubset[scenMetaDbSubset[[1L]] != activeSid, ],
                                 uid, "_stime", desc = TRUE)
  }else{
    scenListDb <- formatScenList(scenMetaDbSubset, uid,
                                 "_stime", desc = TRUE)
  }
  if(!LAUNCHHCUBEMODE){
    output$hcHashLookupResults <- renderUI(tags$div())
  }
  showLoadDataDialog(scenListDb = scenListDb, dbTagList = dbTagList,
                     selectLocalTab = identical(length(scenListDb), 0L))
  if(maxNoScenExceeded)
    showHideEl(session, "#importScenMaxNoScen", 4000L)
})
