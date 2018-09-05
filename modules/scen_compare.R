# scenario comparison
observeEvent(input$btCompareScen,{
  if(isolate(input$btCompareScen)%%2 == 0){
    flog.debug("%s: Compare scenario button clicked (stop compare).", uid)
    updateActionButton(session, "btCompareScen", label = lang$nav$sidebarButtons$compareStart)
    if(scen.comp.mode < 2L){
      # single view compare mode
      lapply(seq_len(maxNumberScenarios + 3), function(i) obs.compare[[i]]$suspend())
    }else{
      # split view compare mode
      lapply(2:3, function(i) obs.compare[[i]]$suspend())
    }
    
    #obs.compare$suspend()
  }else{
    flog.debug("%s: Compare scenario button clicked (start compare).", uid)
    updateActionButton(session, "btCompareScen", label = lang$nav$sidebarButtons$compareStop)
    if(scen.comp.mode < 2L){
      # single view compare mode
      lapply(4:(maxNumberScenarios + 3), function(i) obs.compare[[i]]$resume())
    }else{
      # split view compare mode
      lapply(2:3, function(i) obs.compare[[i]]$resume())
    }
    
    #obs.compare$resume()
  }
})