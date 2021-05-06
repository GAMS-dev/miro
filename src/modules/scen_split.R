observeEvent(input$btSplitView, {
  switchCompareMode(session, input$btSplitView, numberScenTabs)
  if(identical(input$btSplitView, "tabView")){
    currentCompMode    <<-  "tab"
  }else if(identical(input$btSplitView, "pivotView")){
    currentCompMode    <<-  "pivot"
  }else{
    currentCompMode    <<-  "split"
  }
  if(isInCompareMode){
    rv$btCompareScen <- isolate(rv$btCompareScen + 1L)
  }
})
observeEvent(input$btScenSplit1_open, {
  flog.debug("Load Scenario button clicked (left box in split view).")
  loadInLeftBoxSplit <<- TRUE
  
  updateTabsetPanel(session, "contentScen_2", "contentScen_2_1")
  if(isGroupOfSheets[[1]]){
    updateTabsetPanel(session, "contentScen_2_1", "contentScen_2_1_1")
  }
  rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
})
observeEvent(input$btScenSplit2_open, {
  flog.debug("Load Scenario button clicked (right box in split view).")
  loadInLeftBoxSplit <<- FALSE
  
  updateTabsetPanel(session, "contentScen_3", "contentScen_3_1")
  if(isGroupOfSheets[[1]]){
    updateTabsetPanel(session, "contentScen_3_1", "contentScen_3_1_1")
  }
  rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
})
loadSandboxScen <- function(scenId){
  source("./modules/scen_save.R", local = TRUE)
  scenIdLongNew <- paste0("scen_", scenId, "_")
  
  # copy data over as scen_save.R stores data in active scenario (scen_1_), 
  # but here we need it in (scen_2_ or scen_3_ respectively)
  scenData[[scenIdLongNew]] <<- scenData[[scenIdLong]]
  scalarData[[scenIdLongNew]] <<- scalarData[[scenIdLong]]
  
  scenMetaData[[scenIdLongNew]] <<- db$getMetadata(sid = -19L, uid = uid, 
                                                   sname = paste(if(length(rv$activeSname))
                                                     rv$activeSname else lang$nav$dialogNewScen$newScenName,
                                                     lang$nav$scen$scenNameSandboxSuffix), 
                                                   stime = Sys.time(),
                                                   uidAlias = lang$nav$excelExport$metadataSheet$uid, 
                                                   snameAlias = lang$nav$excelExport$metadataSheet$sname, 
                                                   stimeAlias = lang$nav$excelExport$metadataSheet$stime)
  idxScalarOut <- dbSchema$getSymIdx(scalarsOutName)[[1]]
  idxScalarIn <- dbSchema$getSymIdx(scalarsFileName)[[1]]
  # load scalar data if available
  if(!is.na(idxScalarIn) && nrow(scenData[[scenIdLongNew]][[idxScalarIn]])){
    scalarData[[scenIdLongNew]]               <<- scenData[[scenIdLongNew]][[idxScalarIn]]
  }else{
    scalarData[[scenIdLongNew]]               <<- tibble()
  }
  if(!is.na(idxScalarOut) && nrow(scenData[[scenIdLongNew]][[idxScalarOut]])){
    # scalar data exists
    rowIdsToRemove                         <- tolower(scenData[[scenIdLongNew]][[idxScalarOut]][[1]]) %in% config$hiddenOutputScalars
    scalarData[[scenIdLongNew]]               <<- bind_rows(scalarData[[scenIdLongNew]], 
                                                            scenData[[scenIdLongNew]][[idxScalarOut]])
    scenData[[scenIdLongNew]][[idxScalarOut]] <<- scenData[[scenIdLongNew]][[idxScalarOut]][!rowIdsToRemove, ]
  }
  scenIdLong <- scenIdLongNew
  views$duplicateSandboxConf(scenId)
  try(source("./modules/scen_render.R", local = TRUE), silent = TRUE)
  # load script results
  if(length(config$scripts$base)){
    scriptOutput$loadResultsBase(scriptOutput$getResults(), 
                                 scenId = scenId)
  }
}
observeEvent(input$loadActiveScenSplitComp, {
  if(LAUNCHHCUBEMODE){
    flog.error("Load sandbox scenario button in split compare mode clicked while in Hypercube mode. This should never happen and is likely an attempt to tamper with the app!")
    return()
  }
  flog.debug("Load sandbox scenario to split comparison mode clicked ID: '%s'.", 
             isolate(input$loadActiveScenSplitComp))
  id <- suppressWarnings(as.integer(isolate(input$loadActiveScenSplitComp)))
  showEl(session, "#loading-screen")
  on.exit(hideEl(session, "#loading-screen"))
  if(identical(id, 2L)){
    loadInLeftBoxSplit <<- TRUE
    if(!compareModeTabsetGenerated[1]){
      compareModeTabsetGenerated[1] <<- TRUE
      insertUI("#scenSplit1_content", where = "afterBegin",
               generateScenarioTabsetSplit(2), immediate = TRUE)
    }
  }else if(identical(id, 3L)){
    loadInLeftBoxSplit <<- FALSE
    if(!compareModeTabsetGenerated[2]){
      compareModeTabsetGenerated[2] <<- TRUE
      insertUI("#scenSplit2_content", where = "afterBegin",
               generateScenarioTabsetSplit(3), immediate = TRUE)
    }
  }else{
    flog.error("Button ID (load active scenario to split comp) has invalid value: '%s'. This should never happen! 
               User most likely tried to tamper with the app.", isolate(input$loadActiveScenSplitComp))
    return()
  }
  updateTabsetPanel(session, paste0("contentScen_", id), 
                    paste0("contentScen_", id, "_1"))
  if(isGroupOfSheets[[1]]){
    updateTabsetPanel(session, paste0("contentScen_", id, "_1"), 
                      paste0("contentScen_", id, "_1_1"))
  }
  loadSandboxScen(id)
  showEl(session, paste0("#refreshSandbox_", id))
  sidsInSplitComp[id - 1L] <<- -19L
})

observeEvent(input$btScenSplit1_close, {
  flog.debug("%s: Close Scenario button clicked (left box in split view).", uid)
  
  if(identical(sidsInSplitComp[[1]], 0L)){
    return(NULL)
  }
  
  output$title_2                                     <- renderUI(character(0))
  scenIdLong                                         <- "scen_2_"
  scenData[[scenIdLong]]                             <<- list(NULL)
  scalarData[[scenIdLong]]                           <<- list(NULL)
  scenMetaData[[scenIdLong]]                         <<- list(NULL)
  sidsInSplitComp[1]                                 <<- 0L
  views$clearConf("2")
  
  # show button and hide content
  hideEl(session, "#refreshSandbox_2")
  hideEl(session, "#scenSplit1_content")
  showEl(session, "#scenSplit1_open")
})
observeEvent(input$btScenSplit2_close, {
  flog.debug("%s: Close Scenario button clicked (right box in split view).", uid)
  
  if(identical(sidsInSplitComp[[2]], 0L)){
    return(NULL)
  }
  
  output$title_3                                     <- renderUI(character(0))
  scenIdLong                                         <- "scen_3_"
  scenData[[scenIdLong]]                             <<- list(NULL)
  scalarData[[scenIdLong]]                           <<- list(NULL)
  scenMetaData[[scenIdLong]]                         <<- list(NULL)
  sidsInSplitComp[2]                                 <<- 0L
  views$clearConf("3")
  
  # show button and hide content
  hideEl(session, "#refreshSandbox_3")
  hideEl(session, "#scenSplit2_content")
  showEl(session, "#scenSplit2_open")
})