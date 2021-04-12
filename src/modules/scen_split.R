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
loadSandboxScen <- function(scenId, refresh = FALSE){
  if(tryCatch({
    scenData$loadSandbox(getInputDataFromSandbox(saveInputDb = TRUE),
                         modelInFileNames, activeScen$getMetadata())
    FALSE
  }, no_data = function(e){
    flog.error(conditionMessage(e))
    showErrorMsg(lang$errMsg$GAMSInput$title, conditionMessage(e))
    return(TRUE)
  }, error = function(e){
    flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
    showErrorMsg(lang$errMsg$GAMSInput$title, lang$errMsg$unknownError)
    return(TRUE)
  })){
    return()
  }
  scenData$load("sb", refId = tabIdToRef(scenId))
  renderScenInCompMode(scenId, refreshData = refresh)
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
    if(!dynamicUILoaded$compareModeTabsets[1]){
      dynamicUILoaded$compareModeTabsets[1] <<- TRUE
      insertUI("#scenSplit1_content", where = "afterBegin",
               generateScenarioTabsetSplit(2), immediate = TRUE)
    }
  }else if(identical(id, 3L)){
    loadInLeftBoxSplit <<- FALSE
    if(!dynamicUILoaded$compareModeTabsets[2]){
      dynamicUILoaded$compareModeTabsets[2] <<- TRUE
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
  loadSandboxScen(id, refresh = FALSE)
})

observeEvent(input$btScenSplit1_close, {
  flog.debug("Close Scenario button clicked (left box in split view).")

  dynamicUILoaded$compTabset[["tab_2"]][["content"]][] <<- FALSE
  views$clearConf("2")
  scenData$clear("cmpSplitL")
  
  # show button and hide content
  hideEl(session, "#cmpScenTitle_2")
  hideEl(session, "#scenSplit1_content")
  showEl(session, "#scenSplit1_open")
})
observeEvent(input$btScenSplit2_close, {
  flog.debug("Close Scenario button clicked (right box in split view).")
  
  dynamicUILoaded$compTabset[["tab_3"]][["content"]][] <<- FALSE
  views$clearConf("3")
  scenData$clear("cmpSplitR")
  
  # show button and hide content
  hideEl(session, "#cmpScenTitle_3")
  hideEl(session, "#scenSplit2_content")
  showEl(session, "#scenSplit2_open")
})