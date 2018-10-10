observeEvent(virtualActionButton(input$btSplitView, rv$btSplitView), {
  if(isInSplitView){
    if(numberScenTabs < 2){
      disableEl(session, "#btCompareScen")
    }
    showEl(session, "#scenTabView")
    hideEl(session, "#scenSplitView")
    updateActionButton(session, "btSplitView", label = lang$nav$sidebarButtons$splitView)
    isInSplitView <<- FALSE
  }else{
    # enable scenario comparison button
    enableEl(session, "#btCompareScen")
    showEl(session, "#scenSplitView")
    hideEl(session, "#scenTabView")
    updateActionButton(session, "btSplitView", label = lang$nav$sidebarButtons$tabView)
    isInSplitView <<- TRUE
  }
  if(isInCompareMode){
    rv$btCompareScen <- isolate(rv$btCompareScen + 1L)
  }
})
observeEvent(input$btScenSplit1_open, {
  flog.debug("%s: Load Scenario button clicked (left box in split view).", uid)
  loadInLeftBoxSplit <<- TRUE
  
  updateTabsetPanel(session, "contentScen_2", "contentScen_2_1")
  if(is.null(isolate(rv$btLoadScen))){
    rv$btLoadScen <<- 1
  }else{
    rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
  }
})
observeEvent(input$btScenSplit2_open, {
  flog.debug("%s: Load Scenario button clicked (right box in split view).", uid)
  loadInLeftBoxSplit <<- FALSE
  
  updateTabsetPanel(session, "contentScen_3", "contentScen_3_1")
  if(is.null(isolate(rv$btLoadScen))){
    rv$btLoadScen <<- 1
  }else{
    rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
  }
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