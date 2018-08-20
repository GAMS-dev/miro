observeEvent(input$btSplitView, {
  if(isolate(input$btSplitView)%%2 != 0){
    if(number.scen.tabs < 2){
      shinyjs::disable("btCompareScen")
    }
    scen.comp.mode <<- 1L
    shinyjs::show("scenSingleView")
    shinyjs::hide("scenSplitView")
    updateActionButton(session, "btSplitView", label = lang$nav$sidebarButtons$splitViewStart)
  }else{
    # enable scenario comparison button
    shinyjs::enable("btCompareScen")
    scen.comp.mode <<- 2L
    shinyjs::show("scenSplitView")
    shinyjs::hide("scenSingleView")
    updateActionButton(session, "btSplitView", label = lang$nav$sidebarButtons$splitViewStop)
  }
})
observeEvent(input$btScenSplit1_open, {
  flog.debug("%s: Load Scenario button clicked (left box in split view).", uid)
  scen.comp.mode <<- 2L
  
  updateTabsetPanel(session, "content.scen_2", "content.scen_2_1")
  if(is.null(isolate(rv$btLoadScen))){
    rv$btLoadScen <<- 1
  }else{
    rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
  }
})
observeEvent(input$btScenSplit2_open, {
  flog.debug("%s: Load Scenario button clicked (right box in split view).", uid)
  scen.comp.mode <<- 3L
  
  updateTabsetPanel(session, "content.scen_3", "content.scen_3_1")
  if(is.null(isolate(rv$btLoadScen))){
    rv$btLoadScen <<- 1
  }else{
    rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
  }
})
observeEvent(input$btScenSplit1_close, {
  flog.debug("%s: Close Scenario button clicked (left box in split view).", uid)
  
  if(sids.loaded.in.split.comp[1] == 0){
    return(NULL)
  }
  
  output$title_2                                     <- renderText(character(0))
  scen.str                                           <- "scen_2_"
  scenData[[scen.str]]                               <<- list(NULL)
  scalarData[[scen.str]]                             <<- list(NULL)
  scenMetaData[[scen.str]]                           <<- list(NULL)
  sids.loaded.in.split.comp[1]                       <<- 0L
  
  # show button and hide content
  shinyjs::hide("scenSplit1_content")
  shinyjs::show("scenSplit1_open")
})
observeEvent(input$btScenSplit2_close, {
  flog.debug("%s: Close Scenario button clicked (right box in split view).", uid)
  
  if(sids.loaded.in.split.comp[2] == 0){
    return(NULL)
  }
  
  output$title_3                                     <- renderText(character(0))
  scen.str                                           <- "scen_3_"
  scenData[[scen.str]]                               <<- list(NULL)
  scalarData[[scen.str]]                             <<- list(NULL)
  scenMetaData[[scen.str]]                           <<- list(NULL)
  sids.loaded.in.split.comp[2]                       <<- 0L
  
  # show button and hide content
  shinyjs::hide("scenSplit2_content")
  shinyjs::show("scenSplit2_open")
})