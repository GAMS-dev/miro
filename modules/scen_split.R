observeEvent(virtualActionButton(input$btSplitView, rv$btSplitView), {
  if(isInSplitView){
    if(numberScenTabs < 2){
      disableEl(session, "#btCompareScen")
    }
    showEl(session, "#scen-tab-view")
    hideEl(session, "#scen-split-view")
    updateActionButton(session, "btSplitView", label = lang$nav$sidebarButtons$splitView)
    isInSplitView <<- FALSE
  }else{
    # enable scenario comparison button
    enableEl(session, "#btCompareScen")
    showEl(session, "#scen-split-view")
    hideEl(session, "#scen-tab-view")
    updateActionButton(session, "btSplitView", label = lang$nav$sidebarButtons$tabView)
    isInSplitView <<- TRUE
  }
  if(isInCompareMode){
    rv$btCompareScen <- isolate(rv$btCompareScen + 1L)
  }
})
observeEvent(input$btScenSplit1_open, {
  flog.debug("Load Scenario button clicked (left box in split view).")
  loadInLeftBoxSplit <<- TRUE
  
  updateTabsetPanel(session, "contentScen_2", "contentScen_2_1")
  rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
})
observeEvent(input$btScenSplit2_open, {
  flog.debug("Load Scenario button clicked (right box in split view).")
  loadInLeftBoxSplit <<- FALSE
  
  updateTabsetPanel(session, "contentScen_3", "contentScen_3_1")
  rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
})
observeEvent(input$loadActiveScenSplitComp, {
  flog.debug("Load active scenario to split comparison mode pressed. ID: '%s'.", 
             isolate(input$loadActiveScenSplitComp))
  if(is.null(activeScen)){
    flog.debug("No scenario currently opened/active. Nothing to load.")
    return()
  }
  id <- suppressWarnings(as.integer(isolate(input$loadActiveScenSplitComp)))
  if(identical(id, 2L)){
    loadInLeftBoxSplit <<- TRUE
  }else if(identical(id, 3L)){
    loadInLeftBoxSplit <<- FALSE
  }else{
    flog.error("Button ID (load active scenario to split comp) has invalid value: '%s'. This should never happen! 
               User most likely tried to tamper with the app.", isolate(input$loadActiveScenSplitComp))
    return()
  }
  updateTabsetPanel(session, "contentScen_" %+% id, paste0("contentScen_", id, "_1"))
  
  sidsToLoad <<- list(activeScen$getSid())
  if(!length(scenMetaDb) || !sidsToLoad[[1]] %in% scenMetaDb[[1]]){
    scenMetaDb <<- db$fetchScenList(scode = 0L)
  }
  rv$btOverwriteScen <<- isolate(rv$btOverwriteScen + 1L)
})

observeEvent(input$btScenSplit1_close, {
  flog.debug("%s: Close Scenario button clicked (left box in split view).", uid)
  
  if(sidsInSplitComp[1] == 0){
    return(NULL)
  }
  
  output$title_2                                     <- renderText(character(0))
  scenIdLong                                         <- "scen_2_"
  scenData[[scenIdLong]]                             <<- list(NULL)
  scalarData[[scenIdLong]]                           <<- list(NULL)
  scenMetaData[[scenIdLong]]                         <<- list(NULL)
  sidsInSplitComp[1]                                 <<- 0L
  
  # show button and hide content
  hideEl(session, "#scenSplit1_content")
  showEl(session, "#scenSplit1_open")
})
observeEvent(input$btScenSplit2_close, {
  flog.debug("%s: Close Scenario button clicked (right box in split view).", uid)
  
  if(sidsInSplitComp[2] == 0){
    return(NULL)
  }
  
  output$title_3                                     <- renderText(character(0))
  scenIdLong                                         <- "scen_3_"
  scenData[[scenIdLong]]                             <<- list(NULL)
  scalarData[[scenIdLong]]                           <<- list(NULL)
  scenMetaData[[scenIdLong]]                         <<- list(NULL)
  sidsInSplitComp[2]                                 <<- 0L
  
  # show button and hide content
  hideEl(session, "#scenSplit2_content")
  showEl(session, "#scenSplit2_open")
})