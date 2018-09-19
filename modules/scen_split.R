observeEvent(virtualActionButton(input$btSplitView, rv$btSplitView), {
  if(isInSplitView){
    if(numberScenTabs < 2){
      shinyjs::disable("btCompareScen")
    }
    shinyjs::show("scenTabView")
    shinyjs::hide("scenSplitView")
    updateActionButton(session, "btSplitView", label = lang$nav$sidebarButtons$splitView)
    isInSplitView <<- FALSE
  }else{
    # enable scenario comparison button
    shinyjs::enable("btCompareScen")
    shinyjs::show("scenSplitView")
    shinyjs::hide("scenTabView")
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
  scen.str                                           <- "scen_2_"
  scenData[[scen.str]]                               <<- list(NULL)
  scalarData[[scen.str]]                             <<- list(NULL)
  scenMetaData[[scen.str]]                           <<- list(NULL)
  sidsInSplitComp[1]                                 <<- 0L
  
  # show button and hide content
  shinyjs::hide("scenSplit1_content")
  shinyjs::show("scenSplit1_open")
})
observeEvent(input$btScenSplit2_close, {
  flog.debug("%s: Close Scenario button clicked (right box in split view).", uid)
  
  if(sidsInSplitComp[2] == 0){
    return(NULL)
  }
  
  output$title_3                                     <- renderText(character(0))
  scen.str                                           <- "scen_3_"
  scenData[[scen.str]]                               <<- list(NULL)
  scalarData[[scen.str]]                             <<- list(NULL)
  scenMetaData[[scen.str]]                           <<- list(NULL)
  sidsInSplitComp[2]                                 <<- 0L
  
  # show button and hide content
  shinyjs::hide("scenSplit2_content")
  shinyjs::show("scenSplit2_open")
})